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

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup mini-modeline nil
  "Customizations for `mini-modeline'."
  :group 'minibuffer
  :prefix "mini-modeline-")

;; Forward declaration for evil-mode-line-tag
(defvar evil-mode-line-tag)

(defcustom mini-modeline-l-format nil
  "Left part of mini-modeline, same format with `mode-line-format'."
  :type `(repeat symbol)
  :group 'mini-modeline)

(defcustom mini-modeline-r-format '("%e" mode-line-front-space
                                    mode-line-mule-info
                                    mode-line-client
                                    mode-line-modified
                                    mode-line-remote
                                    mode-line-frame-identification
                                    mode-line-buffer-identification
                                    " " mode-line-position " "
                                    evil-mode-line-tag
                                    (:eval (string-trim (format-mode-line mode-line-modes)))
                                    mode-line-misc-info)
  "Right part of mini-modeline, same format with `mode-line-format'."
  :type `(repeat symbol)
  :group 'mini-modeline)

(defcustom mini-modeline-face-attr `(:background ,(face-attribute 'mode-line :background))
  "Plist of face attribute/value pair for mini-modeline."
  :type '(plist)
  :group 'mini-modeline)

(defcustom mini-modeline-truncate-p t
  "Truncates mini-modeline or not."
  :type 'boolean
  :group 'mini-modeline)

(defcustom mini-modeline-enhance-visual t
  "Enhance minibuffer and window's visibility."
  :type 'boolean
  :group 'mini-modeline)

(defface mini-modeline-mode-line
  '((((background light))
     :background "#55ced1" :height 0.14 :box nil)
    (t
     :background "#008b8b" :height 0.14 :box nil))
  "Modeline face for active window."
  :group 'mini-modeline)

(defface mini-modeline-mode-line-inactive
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

(defcustom mini-modeline-echo-duration 2
  "Duration to keep display echo."
  :type 'number
  :group 'mini-modeline)

(defcustom mini-modeline-frame nil
  "Frame to display mini-modeline on.
Nil means current selected frame."
  :type 'sexp
  :group 'mini-modeline)

(defcustom mini-modeline-display-gui-line t
  "Display thin line at the bottom of the window."
  :type 'boolean
  :group 'mini-modeline)

(defcustom mini-modeline-right-padding 3
  "Padding to use in the right side.
Set this to the minimal value that doesn't cause truncation."
  :type 'integer
  :group 'mini-modeline)

(defcustom mini-modeline-echo-position "left"
  "Position to display echo area.
Set this to 'left' 'middle' 'right'."
  :type '(choice (const "left")
		 (const "middle")
		 (const "right"))
  :group 'mini-modeline)

(defvar mini-modeline--last-echoed nil)

(defvar mini-modeline--msg nil)
(defvar mini-modeline--msg-message nil
  "Store the string from `message'.")

;; perf
(defcustom mini-modeline-update-interval 0.1
  "The minimum interval to update mini-modeline."
  :type 'number
  :group 'mini-modeline)

(defvar mini-modeline--last-update (current-time))
(defvar mini-modeline--last-change-size (current-time))
(defvar mini-modeline--cache nil)
(defvar mini-modeline--command-state 'begin
  "The state of current executed command begin -> [exec exec-read] -> end.")

(defvar-local mini-modeline--face-cookie nil)
(defun mini-modeline--set-buffer-face ()
  "Set buffer default face for current buffer."
  (setq mini-modeline--face-cookie
        (face-remap-add-relative 'default mini-modeline-face-attr)))

(defun mini-modeline--log (&rest args)
  "Log message into message buffer with ARGS as same parameters in `message'."
  (save-excursion
    (with-current-buffer "*Messages*"
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (apply #'format args))))))

(defsubst mini-modeline--overduep (since duration)
  "Check if time already pass DURATION from SINCE."
  (>= (float-time (time-since since)) duration))

(defvar mini-modeline--minibuffer nil)
(defun mini-modeline-display (&optional arg)
  "Update mini-modeline.
When ARG is:
- `force', force update the minibuffer.
- `clear', clear the minibuffer.  This implies `force'."
  (save-match-data
    (condition-case err
        (cl-letf (((symbol-function 'completion-all-completions) #'ignore)
                  (l-fmt mini-modeline-l-format)
                  (r-fmt mini-modeline-r-format))
          (unless (or (active-minibuffer-window)
                      (input-pending-p))
            (setq mini-modeline--minibuffer
                  (window-buffer (minibuffer-window mini-modeline-frame)))
            (with-current-buffer mini-modeline--minibuffer
              (let ((truncate-lines mini-modeline-truncate-p)
                    (inhibit-read-only t)
                    (inhibit-redisplay t)
                    (buffer-undo-list t)
                    modeline-content)
                (when (or (memq arg '(force clear))
                          (mini-modeline--overduep mini-modeline--last-update
                                                   mini-modeline-update-interval))
                  (when-let ((msg (or mini-modeline--msg-message (current-message))))
                    ;; Clear echo area and start new timer for echo message
                    ;; (mini-modeline--log "msg: %s\n" msg)
                    ;; (mini-modeline--log "from: %s\n" mini-modeline--msg-message)
                    (message nil)
                    (setq mini-modeline--last-echoed (current-time))
                    ;; we proritize the message from `message'
                    ;; or the message when we're not in middle of a command running.
                    (when (or mini-modeline--msg-message
                              (eq mini-modeline--command-state 'begin))
                      (setq mini-modeline--command-state 'exec)
                      ;; Don't echo keystrokes when in middle of command
                      (setq echo-keystrokes 0))
                    (setq mini-modeline--msg msg))
                  ;; Reset echo message when timeout and not in middle of command
                  (when (and mini-modeline--msg
                             (not (memq mini-modeline--command-state '(exec exec-read)))
                             (mini-modeline--overduep mini-modeline--last-echoed
                                                      mini-modeline-echo-duration))
                    (setq mini-modeline--msg nil))
                  ;; Showing mini-modeline
                  (if (eq arg 'clear)
                      (setq modeline-content nil)
                    (setq modeline-content                        
			  (pcase mini-modeline-echo-position
			    ("left"
			     (mini-modeline--multi-lr-render
			      (if mini-modeline--msg
				  (format-mode-line '(:eval (mini-modeline-msg)))
				(format-mode-line l-fmt))
                              (format-mode-line r-fmt)))
			    ("middle"
			     (mini-modeline--multi-lr-render
			      (if mini-modeline--msg
				  (format-mode-line (append l-fmt '((:eval (mini-modeline-msg)))))
				(format-mode-line l-fmt))
                              (format-mode-line r-fmt)))
			    ("right"
			     (mini-modeline--multi-lr-render
			      (format-mode-line l-fmt)
			      (if mini-modeline--msg
				  (format-mode-line '(:eval (mini-modeline-msg)))
				(format-mode-line r-fmt))))))
                    (setq mini-modeline--last-update (current-time)))

                  ;; write to minibuffer
                  (unless (equal modeline-content
                                 mini-modeline--cache)
                    (setq mini-modeline--cache modeline-content)
                    (erase-buffer)
                    (when mini-modeline--cache
                      (let ((height-delta (- (cdr mini-modeline--cache)
                                             (window-height (minibuffer-window mini-modeline-frame))))
                            ;; ; let mini-modeline take control of mini-buffer size
                            (resize-mini-windows t))
                        (when (or (> height-delta 0)
                                  ;; this is to prevent window flashing for consecutive multi-line message
                                  (mini-modeline--overduep mini-modeline--last-change-size
                                                           mini-modeline-echo-duration))
                          (window-resize (minibuffer-window mini-modeline-frame) height-delta)
                          (setq mini-modeline--last-change-size (current-time)))
                        (insert (car mini-modeline--cache))))))))))
      ((error debug)
       (mini-modeline--log "mini-modeline: %s\n" err)))))

(defun mini-modeline-msg ()
  "Place holder to display echo area message."
  (when mini-modeline--msg
    (replace-regexp-in-string "%" "%%" mini-modeline--msg)))

(defsubst mini-modeline--lr-render (left right)
  "Render the LEFT and RIGHT part of mini-modeline."
  (let* ((left (or left ""))
         (right (or right ""))
         (available-width (max (- (frame-width mini-modeline-frame)
                                  (string-width left)
                                  mini-modeline-right-padding)
                               0))
         (required-width (string-width right)))
    ;; (mini-modeline--log "a:%s r:%s\n" available-width required-width)
    (if (< available-width required-width)
        (if mini-modeline-truncate-p
            (cons
             ;; Emacs 25 cannot use position format
             (format (format "%%s %%%d.%ds" available-width available-width) left right)
             0)
          (cons
           (let ((available-width (+ available-width (string-width left))))
             (format (format "%%0.%ds\n%%s" available-width) right left))
           (ceiling (string-width left) (frame-width mini-modeline-frame))))
      (cons (format (format "%%s %%%ds" available-width) left right) 0))))

(defun mini-modeline--multi-lr-render (left right)
  "Render the LEFT and RIGHT part of mini-modeline with multiline supported.
Return value is (STRING . LINES)."
  (let* ((l (split-string left "\n"))
         (r (split-string right "\n"))
         (lines (max (length l) (length r)))
         (extra-lines 0)
         re)
    (--dotimes lines
      (let ((lr (mini-modeline--lr-render (elt l it) (elt r it))))
        (setq re (nconc re `(,(car lr))))
        (setq extra-lines (+ extra-lines (cdr lr)))))
    (cons (string-join re "\n") (+ lines extra-lines))))

(defun mini-modeline--reroute-msg (func &rest args)
  "Reroute FUNC with ARGS that echo to echo area to place hodler."
  (if inhibit-message
      (apply func args)
    (let* ((inhibit-message t)
           (mini-modeline--msg-message (apply func args)))
      (mini-modeline-display 'force)
      mini-modeline--msg-message)))

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
  (setq mini-modeline--command-state 'begin))

(defsubst mini-modeline--post-cmd ()
  "Post command hook of mini-modeline."
  (setq mini-modeline--command-state 'end
        echo-keystrokes mini-modeline--echo-keystrokes))

(defvar mini-modeline--orig-resize-mini-windows resize-mini-windows)
(defsubst mini-modeline--enter-minibuffer ()
  "`minibuffer-setup-hook' of mini-modeline."
  (when mini-modeline-enhance-visual
    (mini-modeline--set-buffer-face))
  (setq resize-mini-windows 'grow-only))

(defsubst mini-modeline--exit-minibuffer ()
  "`minibuffer-exit-hook' of mini-modeline."
  (when mini-modeline-enhance-visual
    (with-current-buffer mini-modeline--minibuffer
      (mini-modeline--set-buffer-face)))
  (setq resize-mini-windows nil))

(declare-function anzu--cons-mode-line "ext:anzu")
(declare-function anzu--reset-mode-line "ext:anzu")

(defvar mini-modeline--timer nil)

(defun mini-modeline--enable ()
  "Enable `mini-modeline'."
  ;; Hide modeline for terminal, or use empty modeline for GUI.
  (setq-default mini-modeline--orig-mode-line mode-line-format)
  (setq-default mode-line-format (when (and mini-modeline-display-gui-line
                                            (display-graphic-p))
                                   '(" ")))
  ;; Do the same thing with opening buffers.
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (local-variable-p 'mode-line-format)
         (setq mini-modeline--orig-mode-line mode-line-format)
         (setq mode-line-format (when (and mini-modeline-display-gui-line
                                           (display-graphic-p))
                                  '(" "))))
       (when (and mini-modeline-enhance-visual
                  (or (minibufferp buf)
                      (string-prefix-p " *Echo Area" (buffer-name))))
         (mini-modeline--set-buffer-face))
       ;; Make the modeline in GUI a thin bar.
       (when (and mini-modeline-display-gui-line
                  (local-variable-p 'face-remapping-alist)
                  (display-graphic-p))
         (setf (alist-get 'mode-line face-remapping-alist)
               'mini-modeline-mode-line
               (alist-get 'mode-line-inactive face-remapping-alist)
               'mini-modeline-mode-line-inactive))))
   (buffer-list))

  ;; Make the modeline in GUI a thin bar.
  (when (and mini-modeline-display-gui-line
             (display-graphic-p))
    (let ((face-remaps (default-value 'face-remapping-alist)))
      (setf (alist-get 'mode-line face-remaps)
            'mini-modeline-mode-line
            (alist-get 'mode-line-inactive face-remaps)
            'mini-modeline-mode-line-inactive
            (default-value 'face-remapping-alist) face-remaps)))

  (setq mini-modeline--orig-resize-mini-windows resize-mini-windows)
  (setq resize-mini-windows nil)
  (redisplay)
  ;; (add-hook 'pre-redisplay-functions #'mini-modeline-display)
  (setq mini-modeline--timer (run-with-timer 0 0.1 #'mini-modeline-display))
  (advice-add #'message :around #'mini-modeline--reroute-msg)

  (add-hook 'minibuffer-setup-hook #'mini-modeline--enter-minibuffer)
  (add-hook 'minibuffer-exit-hook #'mini-modeline--exit-minibuffer)
  (add-hook 'pre-command-hook #'mini-modeline--pre-cmd)
  (add-hook 'post-command-hook #'mini-modeline--post-cmd)

  ;; compatibility
  ;; anzu
  (mini-modeline--wrap
   anzu--cons-mode-line
   (let ((mode-line-format mini-modeline-r-format))
     (apply orig-func args)
     (setq mini-modeline-r-format mode-line-format)))
  (mini-modeline--wrap
   anzu--reset-mode-line
   (let ((mode-line-format mini-modeline-r-format))
     (apply orig-func args)
     (setq mini-modeline-r-format mode-line-format)))

  ;; read-key-sequence
  (mini-modeline--wrap
   read-key-sequence
   (progn
     (setq mini-modeline--command-state 'exec-read)
     (apply orig-func args)))
  (mini-modeline--wrap
   read-key-sequence-vector
   (progn
     (setq mini-modeline--command-state 'exec-read)
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
  ;; (remove-hook 'post-command-hook #'mini-modeline-display)
  ;; (remove-hook 'pre-redisplay-functions #'mini-modeline-display)
  (when (timerp mini-modeline--timer) (cancel-timer mini-modeline--timer))
  (mini-modeline-display 'clear)
  (advice-remove #'message #'mini-modeline--reroute-msg)

  (remove-hook 'minibuffer-setup-hook #'mini-modeline--enter-minibuffer)
  (remove-hook 'minibuffer-exit-hook #'mini-modeline--exit-minibuffer)
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
