;;; mini-modeline.el --- Display modeline in minibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  Kien Nguyen <kien.n.quang@gmail.com>
;; URL: https://github.com/kiennq/emacs-mini-modeline
;; Version: 0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "25.1") (dash "2.12.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display modeline in minibuffer.
;; With this we save one display line and also don't have to see redundant information.

;;; Code:

(require 'minibuffer)
(require 'dash)
(require 'frame)
(require 'timer)
(require 'face-remap)
(require 'cl-lib)

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup mini-modeline nil
  "Customizations for `mini-modeline'."
  :group 'minibuffer
  :prefix "mini-modeline-")

(defcustom mini-modeline--l-format nil
  "Left part of mini-modeline, same format with `mode-line-format'."
  :type `(repeat symbol)
  :group 'mini-modeline)

(defcustom mini-modeline--r-format '("%e" mode-line-front-space
                                     mode-line-buffer-identification
                                     ;; function name in [mule-info] could be long
                                     mode-line-mule-info
                                     mode-line-client
                                     mode-line-modified
                                     mode-line-remote
                                     mode-line-frame-identification
                                     " " mode-line-position " "
                                     (:eval (string-trim (format-mode-line mode-line-modes)))
                                     mode-line-misc-info)
  "Right part of mini-modeline, same format with `mode-line-format'."
  :type `(repeat symbol)
  :group 'mini-modeline)

(defcustom mini-modeline--face-attr `(:background ,(face-attribute 'mode-line :background))
  "Plist of face attribute/value pair for mini-modeline."
  :type '(plist)
  :group 'mini-modeline)

(defcustom mini-modeline--truncate-p nil
  "Truncates mini-modeline or not."
  :type 'boolean
  :group 'mini-modeline)

(defcustom mini-modeline--enhance-visual t
  "Enhance minibuffer and window's visibility."
  :type 'boolean
  :group 'mini-modeline)

(defface mini-modeline--mode-line
  '((((background light))
     :background "#55ced1" :height 0.14 :box nil)
    (t
     :background "#008b8b" :height 0.14 :box nil))
  "Modeline face for active window."
  :group 'mini-modeline)

(defface mini-modeline--mode-line-inactive
  '((((background light))
     :background "#dddddd" :height 0.1 :box nil)
    (t
     :background "#333333" :height 0.1 :box nil))
  "Modeline face for inactive window."
  :group 'mini-modeline)

(defvar-local mini-modeline--orig-mode-line mode-line-format)
(defvar mini-modeline--echo-keystrokes echo-keystrokes)
(defvar mini-modeline--orig-mode-line-remap
  (or (alist-get 'mode-line face-remapping-alist) 'mode-line))
(defvar mini-modeline--orig-mode-line-inactive-remap
  (or (alist-get 'mode-line-inactive face-remapping-alist) 'mode-line-inactive))

(defcustom mini-modeline--display-gui-line t
  "Display thin line at the bottom of the window."
  :type 'boolean
  :group 'mini-modeline)

(defcustom mini-modeline--right-padding 2
  "Padding to use in the right side.
Set this to the minimal value that doesn't cause truncation."
  :type 'integer
  :group 'mini-modeline)

;; perf
(defcustom mini-modeline-update-interval 0.1
  "The minimum interval to update mini-modeline."
  :type 'number
  :group 'mini-modeline)

(defvar mini-modeline--last-update-time (current-time))
(defvar mini-modeline--idle t
  "The state of current executed command.")

(defvar-local mini-modeline--face-cookie nil)
(defun mini-modeline--set-buffer-face ()
  "Set buffer default face for current buffer."
  (setq mini-modeline--face-cookie
        (face-remap-add-relative 'default mini-modeline--face-attr)))

(defvar message-log-flag nil)

(defun message-log (force &rest args)
  "Log message into message buffer with ARGS as same parameters in `message'."
  (when (or message-log-flag force)
    (save-excursion
      (with-current-buffer "*Messages*"
        (let ((inhibit-read-only t)
              (log-text (apply #'format args)))
          (goto-char (point-max))
          (insert log-text)
          (insert "\n")
          (notify "message:" log-text))))))

(defun message-log-flag-toggle ()
  (interactive)
  (setq message-log-flag (not message-log-flag)))

(defun message-log (force &rest args)
  "Log message into message buffer with ARGS as same parameters in `message'."
  (when (or message-log-flag force)
    (save-excursion
      (with-current-buffer "*Messages*"
        (let ((inhibit-read-only t)
              (log-text (apply #'format args)))
          (goto-char (point-max))
          (insert log-text)
          (insert "\n"))))))

(defun mini-modeline--show-buffers ()
  (interactive)
  (split-window-vertically)
  (switch-to-buffer " *Minibuf-0*")
  (call-interactively 'split-window-vertically)
  (call-interactively 'other-window)
  (switch-to-buffer " *Echo Area 0*")
  (call-interactively 'other-window))

(defsubst mini-modeline--overduep (since duration)
  "Check if time already pass DURATION from SINCE."
  (>= (float-time (time-since since)) duration))

(defsubst mini-modeline--lr-render (left right)
    "Render the LEFT and RIGHT part of mini-modeline."
    (let* ((left (or left ""))
           (right (or right ""))
           (frame-width (frame-width nil))
           (available-width (max (- frame-width
                                    (string-width left)
                                    mini-modeline--right-padding)
                                 0))
           (required-width (string-width right)))
      (when (> (string-width right) frame-width)
        (setq right (concat "[" (nth 1 (split-string right "\\[")))))
      (when (> (string-width right) frame-width)
        (setq right (nth 1 (split-string right "\\]"))))
      (if (< available-width required-width)
          (if mini-modeline--truncate-p
              (cons (format (format "%%s %%%d.%ds" available-width available-width) left right)
                    0)
            (cons
             (format (format "%%%d.%ds\n%%s" (- frame-width 1) (- frame-width 1)) right left)
             (ceiling (string-width left) frame-width)))
        (cons (format (format "%%s %%%ds" available-width) left right) 0))))

(defun mini-modeline--multi-lr-render (left right)
  "Render the LEFT and RIGHT part of mini-modeline with multiline supported.
Return value is (STRING . LINES)."
  (let* ((l (nreverse (split-string left "\n")))
         ;; right is a single line
         ;; (lines (max (length l) (length r)))
         (lines (length l))
         ;; right part is on an separate bottom line
         (result-lines 0)
         (extra-lines 0)
         re)
    (if (> lines 1)
        (let* ((root (frame-root-window nil))
               (max-lines (- (window-height root)
                             (window-min-size root)
                             1)))
          ;; (--dotimes lines
          ;;   (let ((lr (mini-modeline--lr-render (elt l it) (elt r it))))
          ;;     (setq re (nconc re `(,(car lr))))
          ;;     (setq extra-lines (+ extra-lines (cdr lr)))))
          (dolist (i l)
            (let ((first-i (equal 0 result-lines))
                  lr)
              (if first-i
                  (setq lr (mini-modeline--lr-render i right))
                (setq lr (mini-modeline--lr-render i "")))
              ;; lines before max-lines counted backwards are removed to fit the mini-window
              (if (> (+ result-lines 1 (cdr lr)) max-lines)
                  (cl-return re)
                (setq result-lines (+ result-lines 1 (cdr lr)))
                (setq re (nconc re
                                (if (and first-i (> (cdr lr) 0))
                                    (nreverse `(,(car lr)))
                                  `(,(car lr))))))))
          (setq re (nreverse re)))
      (let ((lr (mini-modeline--lr-render left right)))
        (setq re (nconc re `(,(car lr))))
        (setq result-lines (+ 1 (cdr lr)))))
    (cons (string-join re "\n") result-lines)))

(defvar mini-modeline--unprocessed-message '())

(defun mini-modeline--display (&optional force keep-msg)
  "Update mini-modeline."
  (when (and (or mini-modeline--idle force)
             (not (or (active-minibuffer-window)
                      (input-pending-p))))
    (save-match-data
      (condition-case err
          (cl-letf (((symbol-function 'completion-all-completions) #'ignore))
            (let* ((mini-modeline-window (minibuffer-window nil))
                   (mini-modeline-buffer (window-buffer mini-modeline-window)))
              (with-current-buffer mini-modeline-buffer
                (let (mini-modeline-content-left
                      mini-modeline-content)
                  ;; (message-log nil "mini-modeline--display, command state %s" mini-modeline--idle)
                  (setq mini-modeline-content-left (string-join mini-modeline--unprocessed-message "\n"))
                  (setq mini-modeline-content (mini-modeline--multi-lr-render
                                               (or mini-modeline-content-left (format-mode-line mini-modeline--l-format))
                                               (format-mode-line mini-modeline--r-format)))
                  (unless keep-msg
                    (setq mini-modeline--unprocessed-message '()))
                  (setq mini-modeline--last-update-time (current-time))
                  (setq mini-modeline--timer
                        (run-at-time 0.1 nil 'mini-modeline--set-minibuffer
                                     mini-modeline-content
                                     mini-modeline-window
                                     mini-modeline-buffer))))))
        ((error debug)
         (message-log t "mini-modeline: %s\n" err))))))

(defun mini-modeline--set-minibuffer (mini-modeline-content
                                      mini-modeline-window
                                      mini-modeline-buffer)
  (let* ((mini-modeline-content-height (cdr mini-modeline-content))
         (height-delta (- mini-modeline-content-height
                          (window-height mini-modeline-window)))
         ;; (height-delta-diff (- height-delta (window-max-delta mini-modeline-window)))
         (truncate-lines mini-modeline--truncate-p)
         (buffer-undo-list t)
         (inhibit-redisplay t)
         (inhibit-read-only t)
         ;; (auto-window-vscroll t)
         ;; (redisplay-adhoc-scroll-in-resize-mini-windows t)
         ;; (window-point-insertion-type t)
         ;; (max-mini-window-height 0.5)
         (cursor-in-echo-area t)
         (resize-mini-windows t))
    (with-current-buffer mini-modeline-buffer
      (erase-buffer)
      (insert (car mini-modeline-content))
      ;; (message-log nil "mini-modeline--set-minibuffer minibuffer height: %s, delta: %s"
      ;;                     (window-height mini-modeline-window) height-delta)
      ;; (when (> height-delta-diff 0)
      ;;   ;; (> mini-modeline-content-height (window-height mini-modeline-window)
      ;;   (delete-region (point-min) (progn (forward-line height-delta-diff) (point))))
      (when (> height-delta 0)
        ;; (window--resize-mini-window mini-modeline-window height-delta)
        (window-resize mini-modeline-window height-delta)))))

(setq inhibit-read-only t)

(defun mini-modeline--reroute-msg (func &rest args)
  "Reroute FUNC with ARGS that echo to echo area to place hodler."
  (let* ((inhibit-message t)
         (msg (apply func args)))
    ;; (replace-regexp-in-string "%" "%%" (substring msg 0 max-message-length))
    (unless (string-empty-p msg)
      ;; todo delete trailing spaces and blank lines
      (add-to-list 'mini-modeline--unprocessed-message msg t)
      ;; (message-log nil "Reroute message %s minibuffer active: %s input pending: %s"
      ;;              msg (active-minibuffer-window) (input-pending-p))
      (mini-modeline--display 'force t))
    msg))

(defmacro mini-modeline--wrap (func &rest body)
  "Add an advice around FUNC with name mini-modeline--%s.
BODY will be supplied with orig-func and args."
  (let ((name (intern (format "mini-modeline--%s" func))))
    `(advice-add #',func :around
                 (lambda (orig-func &rest args)
                   ,@body)
                 '((name . ,name)))))

(defsubst mini-modeline--pre-cmd ()
  "Pre command hook of mini-modeline."
  ;; Don't echo keystrokes when in middle of command
  (setq echo-keystrokes 0)
  (setq mini-modeline--idle nil))

(defsubst mini-modeline--post-cmd ()
  "Post command hook of mini-modeline."
  ;; (message-log t "post-cmd %s %s" (string-join mini-modeline--unprocessed-message "\n") (current-buffer))
  (setq mini-modeline--idle t)
  (mini-modeline--display)
  (setq echo-keystrokes mini-modeline--echo-keystrokes))

(defvar mini-modeline--orig-resize-mini-windows resize-mini-windows)
(defsubst mini-modeline--enter-minibuffer ()
  "`minibuffer-setup-hook' of mini-modeline."
  (when mini-modeline--enhance-visual
    (mini-modeline--set-buffer-face))
  (setq resize-mini-windows 'grow-only))

(defsubst mini-modeline--exit-minibuffer ()
  "`minibuffer-exit-hook' of mini-modeline."
  (setq resize-mini-windows mini-modeline--orig-resize-mini-windows)
  (mini-modeline--display))

(declare-function anzu--cons-mode-line "ext:anzu")
(declare-function anzu--reset-mode-line "ext:anzu")

(defun mini-modeline--enable ()
  "Enable `mini-modeline'."
  ;; Hide modeline for terminal, or use empty modeline for GUI.
  (setq-default mini-modeline--orig-mode-line mode-line-format)
  (setq-default mode-line-format (when (and mini-modeline--display-gui-line
                                            (display-graphic-p))
                                   '(" ")))
  ;; Do the same thing with opening buffers.
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (local-variable-p 'mode-line-format)
         (setq mini-modeline--orig-mode-line mode-line-format)
         (setq mode-line-format (when (and mini-modeline--display-gui-line
                                           (display-graphic-p))
                                  '(" "))))
       (when (and mini-modeline--enhance-visual
                  (or (minibufferp buf)
                      (string-prefix-p " *Echo Area" (buffer-name))))
         (mini-modeline--set-buffer-face))
       ;; Make the modeline in GUI a thin bar.
       (when (and mini-modeline--display-gui-line
                  (local-variable-p 'face-remapping-alist)
                  (display-graphic-p))
         (setf (alist-get 'mode-line face-remapping-alist)
               'mini-modeline--mode-line
               (alist-get 'mode-line-inactive face-remapping-alist)
               'mini-modeline--mode-line-inactive))))
   (buffer-list))

  ;; Make the modeline in GUI a thin bar.
  (when (and mini-modeline--display-gui-line
             (display-graphic-p))
    (let ((face-remaps (default-value 'face-remapping-alist)))
      (setf (alist-get 'mode-line face-remaps)
            'mini-modeline--mode-line
            (alist-get 'mode-line-inactive face-remaps)
            'mini-modeline--mode-line-inactive
            (default-value 'face-remapping-alist) face-remaps)))

  (setq resize-mini-windows mini-modeline--orig-resize-mini-windows)
  (redisplay)

  (defvar mini-modeline--timer nil)
  ;; (setq mini-modeline--timer (run-with-idle-timer 0.1 t #'mini-modeline--display))

  (setq message-original (symbol-function 'message))
  (advice-add #'message :around #'mini-modeline--reroute-msg)

  (add-hook 'focus-in-hook 'mini-modeline--display)
  (add-hook 'minibuffer-setup-hook #'mini-modeline--enter-minibuffer)
  (add-hook 'minibuffer-exit-hook #'mini-modeline--exit-minibuffer)
  (add-hook 'echo-area-clear-hook #'mini-modeline--exit-minibuffer)
  ;; (add-hook 'pre-redisplay-functions #'mini-modeline--display)
  (add-hook 'pre-command-hook #'mini-modeline--pre-cmd)
  (add-hook 'post-command-hook #'mini-modeline--post-cmd)

  ;; compatibility
  ;; anzu
  (mini-modeline--wrap
   anzu--cons-mode-line
   (let ((mode-line-format mini-modeline--r-format))
     (apply orig-func args)
     (setq mini-modeline--r-format mode-line-format)))
  (mini-modeline--wrap
   anzu--reset-mode-line
   (let ((mode-line-format mini-modeline--r-format))
     (apply orig-func args)
     (setq mini-modeline--r-format mode-line-format)))

  ;; read-key-sequence
  (mini-modeline--wrap
   read-key-sequence
   (progn
     (setq mini-modeline--idle nil)
     (apply orig-func args)))
  (mini-modeline--wrap
   read-key-sequence-vector
   (progn
     (setq mini-modeline--idle nil)
     (apply orig-func args))))

(defun mini-modeline--disable ()
  "Disable `mini-modeline'."
  (setq-default mode-line-format (default-value 'mini-modeline--orig-mode-line))
  (when (display-graphic-p)
    (let ((face-remaps (default-value 'face-remapping-alist)))
      (setf (alist-get 'mode-line face-remaps)
            mini-modeline--orig-mode-line-remap
            (alist-get 'mode-line-inactive face-remaps)
            mini-modeline--orig-mode-line-inactive-remap
            (default-value 'face-remapping-alist) face-remaps)))

  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (local-variable-p 'mode-line-format)
         (setq mode-line-format mini-modeline--orig-mode-line))
       (when mini-modeline--face-cookie
         (face-remap-remove-relative mini-modeline--face-cookie))
       (when (and (local-variable-p 'face-remapping-alist)
                  (display-graphic-p))
         (setf (alist-get 'mode-line face-remapping-alist)
               mini-modeline--orig-mode-line-remap
               (alist-get 'mode-line-inactive face-remapping-alist)
               mini-modeline--orig-mode-line-inactive-remap))))
   (buffer-list))

  (setq resize-mini-windows mini-modeline--orig-resize-mini-windows)
  (redisplay)
  ;; (when (timerp mini-modeline--timer) (cancel-timer mini-modeline--timer))
  ;; (funcall 'clear-minibuffer-message)
  (message nil)
  (advice-remove #'message #'mini-modeline--reroute-msg)

  (remove-hook 'focus-in-hook 'mini-modeline--display)
  (remove-hook 'minibuffer-setup-hook #'mini-modeline--enter-minibuffer)
  (remove-hook 'minibuffer-exit-hook #'mini-modeline--exit-minibuffer)
  (remove-hook 'echo-area-clear-hook #'mini-modeline--exit-minibuffer)
  ;; (remove-hook 'pre-redisplay-functions #'mini-modeline--display)
  (remove-hook 'pre-command-hook #'mini-modeline--pre-cmd)
  (remove-hook 'post-command-hook #'mini-modeline--post-cmd)

  ;; compatibility
  (advice-remove #'anzu--cons-mode-line 'mini-modeline--anzu--cons-mode-line)
  (advice-remove #'anzu--reset-mode-line 'mini-modeline--anzu--reset-mode-line)

  (advice-remove #'read-key-sequence 'mini-modeline--read-key-sequence)
  (advice-remove #'read-key-sequence-vector 'mini-modeline--read-key-sequence-vector))


;;;###autoload
(define-minor-mode mini-modeline-mode
  "Enable modeline in minibuffer."
  :init-value nil
  :global t
  :group 'mini-modeline
  :lighter " Minimode"
  (if mini-modeline-mode
      (mini-modeline--enable)
    (mini-modeline--disable)))

(provide 'mini-modeline)
;;; mini-modeline.el ends here
