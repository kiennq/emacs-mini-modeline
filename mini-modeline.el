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

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup mini-modeline nil
  "Customizations for `mini-modeline'."
  :group 'minibuffer
  :prefix "mini-modeline-")

;; Forward declaration for evil-mode-line-tag
(defvar evil-mode-line-tag)

(defcustom mini-modeline-l-format '(:eval (mini-modeline-msg))
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
                                    mode-line-modes mode-line-misc-info
                                    mode-line-end-spaces)
  "Right part of mini-modeline, same format with `mode-line-format'."
  :type `(repeat symbol)
  :group 'mini-modeline)

(defcustom mini-modeline-color (face-background 'mode-line)
  "Background of mini-modeline.
Will be set if `mini-modeline-enhance-visual' is t."
  :type 'string
  :group 'mini-modeline)

(defcustom mini-modeline-truncate-p t
  "Truncates mini-modeline or not."
  :type 'boolean
  :group 'mini-modeline)

(defcustom mini-modeline-enhance-visual t
  "Enhance minibuffer and window's visibility."
  :type 'boolean
  :group 'mini-modeline)

(defvar mini-modeline--orig-mode-line mode-line-format)
(defvar mini-modeline--echo-keystrokes echo-keystrokes)

(defcustom mini-modeline-echo-duration 2
  "Duration to keep display echo."
  :type 'number
  :group 'mini-modeline)

(defcustom mini-modeline-frame nil
  "Frame to display mini-modeline on.
Nil means current selected frame."
  :type 'sexp
  :group 'mini-modeline)

(defcustom mini-modeline-right-padding 3
  "Padding to use in the right side.
Set this to the minimal value that doesn't cause truncation."
  :type 'integer
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

(defun mini-modeline--set-buffer-background ()
  "Set buffer background for current buffer."
  (when mini-modeline-color
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist
                 `(default (:background ,mini-modeline-color)))))

(defun mini-modeline--log (&rest args)
  "Log message into message buffer with ARGS as same parameters in `message'."
  (save-excursion
    (with-current-buffer "*Messages*"
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (apply #'format args))))))

(defmacro mini-modeline--overduep (since duration)
  "Check if time already pass DURATION from SINCE."
  `(>= (float-time (time-since ,since)) ,duration))

(defun mini-modeline-display (&optional arg)
  "Update mini-modeline.
When ARG is:
- `force', force update the minibuffer.
- `clear', clear the minibuffer.  This implies `force'."
  (save-match-data
    (condition-case err
        (cl-letf (((symbol-function 'completion-all-completions) #'ignore))
          (unless (or (active-minibuffer-window)
                      (input-pending-p))
            (with-current-buffer (window-buffer (minibuffer-window mini-modeline-frame))
              (let ((truncate-lines mini-modeline-truncate-p)
                    (inhibit-read-only t)
                    (inhibit-redisplay t)
                    (buffer-undo-list t))
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
                             (not (eq mini-modeline--command-state 'exec))
                             (mini-modeline--overduep mini-modeline--last-echoed
                                                      mini-modeline-echo-duration))
                    (setq mini-modeline--msg nil))
                  ;; Showing mini-modeline
                  (if (eq arg 'clear)
                      (setq mini-modeline--cache nil)
                    (setq mini-modeline--cache
                          (mini-modeline--multi-lr-render
                           (string-trim (format-mode-line mini-modeline-l-format))
                           (string-trim (format-mode-line mini-modeline-r-format))))
                    (setq mini-modeline--last-update (current-time)))

                  ;; write to minibuffer
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
                      (insert (car mini-modeline--cache)))))))))
      ((error debug)
       (mini-modeline--log "mini-modeline: %s\n" err)))))

(defun mini-modeline-msg ()
  "Place holder to display echo area message."
  `(format "%s" mini-modeline--msg))

(defsubst mini-modeline--lr-render (left right)
  "Render the LEFT and RIGHT part of mini-modeline."
  (let* ((left (or left ""))
         (right (or right ""))
         (available-width (max (- (frame-width mini-modeline-frame)
                                  (string-width left)
                                  mini-modeline-right-padding)
                               0))
         (required-width (string-width right)))
    (if (< available-width required-width)
        (if mini-modeline-truncate-p
            (cons
             (format (format "%%s %%%1$d.%1$ds" available-width) left right)
             0)
          (cons
           (format (format "%%%1$d.%1$ds\n%%s" (- (frame-width mini-modeline-frame)
                                                  mini-modeline-right-padding))
                   right left)
           1))
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
  (unless inhibit-message
    (let* ((inhibit-message t)
           (mini-modeline--msg-message (apply func args)))
      (mini-modeline-display 'force))))

(defun mini-modeline--window-divider (&optional reset)
  "Setup command `window-divider-mode' or RESET it."
  (if reset
      (window-divider-mode -1)
    (setq window-divider-default-places t
          window-divider-default-bottom-width 1
          window-divider-default-right-width 1)
    (window-divider-mode 1)))

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

(declare-function anzu--cons-mode-line "anzu")
(declare-function anzu--reset-mode-line "anzu")

(defun mini-modeline-enable ()
  "Enable `mini-modeline'."
  ;; Hide all modeline
  (setq-default mode-line-format nil)
  (mapc
   (lambda (buf)
     (unless (string-prefix-p " " (buffer-name buf))
       (with-current-buffer buf
         (setq mode-line-format nil)
         (if (and (minibufferp) mini-modeline-enhance-visual)
             (mini-modeline--set-buffer-background)))))
   (buffer-list))
  (redisplay)
  (setq resize-mini-windows t)
  (add-hook 'pre-redisplay-functions #'mini-modeline-display)
  (when mini-modeline-enhance-visual
    (add-hook 'minibuffer-setup-hook #'mini-modeline--set-buffer-background)
    ;; set up `window-divider-mode' for visibility
    (mini-modeline--window-divider))
  (advice-add #'message :around #'mini-modeline--reroute-msg)
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
     (apply orig-func args)))
  )

(defun mini-modeline-disable ()
  "Disable `mini-modeline'."
  (setq-default mode-line-format mini-modeline--orig-mode-line)
  (mapc
   (lambda (buf)
     (unless (string-prefix-p " " (buffer-name buf))
       (with-current-buffer buf
         (setq mode-line-format mini-modeline--orig-mode-line))))
   (buffer-list))
  (redisplay)
  ;; (remove-hook 'post-command-hook #'mini-modeline-display)
  (remove-hook 'pre-redisplay-functions #'mini-modeline-display)
  (mini-modeline-display 'clear)
  (when mini-modeline-enhance-visual
    (remove-hook 'minibuffer-setup-hook #'mini-modeline--set-buffer-background)
    (mini-modeline--window-divider 'reset))
  (advice-remove #'message #'mini-modeline--reroute-msg)
  (remove-hook 'pre-command-hook #'mini-modeline--pre-cmd)
  (remove-hook 'post-command-hook #'mini-modeline--post-cmd)

  ;; compatibility
  (advice-remove #'anzu--cons-mode-line 'mini-modeline--anzu--cons-mode-line)
  (advice-remove #'anzu--reset-mode-line 'mini-modeline--anzu--reset-mode-line)

  (advice-remove #'read-key-sequence 'mini-modeline--read-key-sequence)
  (advice-remove #'read-key-sequence-vector 'mini-modeline--read-key-sequence-vector)
  )

;;;###autoload
(define-minor-mode mini-modeline-mode
  "Enable modeline in minibuffer."
  :init-value nil
  :global t
  :group 'mini-modeline
  :lighter " Minimode"
  (if mini-modeline-mode
      (mini-modeline-enable)
    (mini-modeline-disable)))

(provide 'mini-modeline)
;;; mini-modeline.el ends here
