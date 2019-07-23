;;; mini-modeline.el --- Display modeline in minibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  Kien Nguyen <kien.n.quang@gmail.com>
;; Version: 0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "25.1") (dash "2.14.1"))

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
(require 'subr-x)
(require 'cl-macs)

(defgroup mini-modeline nil
  "Customizations for `mini-modeline'."
  :group 'minibuffer
  :prefix "mini-modeline-")

;; Forward declaration for evil-mode-line-tag
(unless (boundp 'evil-mode-line-tag)
  (defvar-local evil-mode-line-tag nil))

(defcustom l-mini-modeline-format '(:eval (mini-modeline-msg))
  "Left part of mini-modeline, same format with `mode-line-format'."
  :type `(repeat symbol)
  :group 'mini-modeline)

(defcustom r-mini-modeline-format '("%e" mode-line-front-space
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

(defcustom mini-modeline-format
  '((:eval (mini-modeline-lr-render
            (string-trim (format-mode-line l-mini-modeline-format))
            (string-trim (format-mode-line r-mini-modeline-format)))))
  "Analogous to `mode-line-format', but controls the minibuffer line."
  :type `(repeat symbol)
  :group 'mini-modeline)

(defcustom mini-modeline-color (face-background 'mode-line)
  "Background of mini-modeline.
Will be set if `mini-modeline-enhance-visual' is t."
  :type 'string
  :group 'mini-modeline)

(defcustom mini-modeline-truncate-p nil
  "Truncates mini-modeline or not."
  :type 'boolean
  :group 'mini-modeline)

(defcustom mini-modeline-enhance-visual t
  "Enhance minibuffer and window's visibility."
  :type 'boolean
  :group 'mini-modeline)

(defvar mini-modeline--minibuffer " *Minibuf-0*")

(defvar mini-modeline--orig-mode-line mode-line-format)

(defcustom mini-modeline-echo-duration 2
  "Duration to keep display echo."
  :type 'number
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

(defun mini-modeline--set-buffer-background ()
  (make-local-variable 'face-remapping-alist)
  (add-to-list 'face-remapping-alist
               `(default (:background ,mini-modeline-color))))

(defun mini-modeline--debug (&rest args)
  (save-excursion
    (with-current-buffer "*Messages*"
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (apply #'format args))
      (read-only-mode 1))))

(defun mini-modeline-display (&optional arg)
  "Update mini-modeline.
When ARG is:
- `force', force update the minibuffer.
- `clear', clear the minibuffer.  This implies `force'."
  (ignore-errors
    (when (or (memq arg '(force clear))
              (>= (float-time (time-since mini-modeline--last-update))
                  mini-modeline-update-interval))
      (setq mini-modeline--last-update (current-time))
      (cl-letf (((symbol-function 'completion-all-completions) #'ignore))
        (while-no-input
          (unless (active-minibuffer-window)
            (with-current-buffer mini-modeline--minibuffer
              (buffer-disable-undo)
              (let ((truncate-lines mini-modeline-truncate-p)
                    (msg (or mini-modeline--msg-message (current-message))))
                ;; Clear echo area and start new timer for echo message
                (when msg
                  ;; (mini-modeline--debug "msg: %s\n" msg)
                  ;; (mini-modeline--debug "from: %s\n" mini-modeline--msg-message)
                  (message nil)
                  (setq mini-modeline--last-echoed (current-time))
                  (setq mini-modeline--msg msg))
                ;; Reset echo message when timeout
                (when (and mini-modeline--msg
                           (>= (float-time (time-since mini-modeline--last-echoed))
                               mini-modeline-echo-duration))
                  (setq mini-modeline--msg nil))
                ;; Showing mini-modeline
                (erase-buffer)
                (unless (eq arg 'clear)
                  (insert (format-mode-line mini-modeline-format)))
                (goto-char (point-max))))))))))

(defun mini-modeline-msg ()
  "Place holder to display echo area message."
  mini-modeline--msg)

(defun mini-modeline-lr-render (left right)
  "Render the LEFT and RIGHT part of mini-modeline."
  (let* ((available-width (- (frame-width) (length left) 3)))
    (format (format "%%s %%%ds " available-width) left right)))

(defun mini-modeline--reroute-msg (func &rest args)
  "Reroute FUNC with ARGS that echo to echo area to place hodler."
  (unless inhibit-message
    (let ((inhibit-message t))
      (setq mini-modeline--msg-message (apply func args)))))

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
  (let* ((name (intern (format "mini-modeline--%s" (symbol-name func)))))
    `(advice-add #',func :around
                 (lambda (orig-func &rest args)
                   ,@body)
                 '((name . ,name)))))

(declare-function anzu--cons-mode-line "anzu")
(declare-function anzu--reset-mode-line "anzu")

;;;###autoload
(defun mini-modeline-enable ()
  "Enable `mini-modeline'."
  (interactive)
  ;; Hide all modeline
  (setq-default mode-line-format nil)
  (--each (buffer-list)
    (with-current-buffer it
      (setq mode-line-format nil)
      (if (and (minibufferp) mini-modeline-enhance-visual)
          (mini-modeline--set-buffer-background))))
  (redisplay)
  ;; (add-hook 'post-command-hook #'mini-modeline-display)
  (add-hook 'pre-redisplay-functions #'mini-modeline-display)
  (when mini-modeline-enhance-visual
    (add-hook 'minibuffer-setup-hook #'mini-modeline--set-buffer-background)
    ;; set up `window-divider-mode' for visibility
    (mini-modeline--window-divider))
  (advice-add #'message :around #'mini-modeline--reroute-msg)

  ;; compatibility
  (eval-after-load 'anzu
    (progn
      (mini-modeline--wrap
       anzu--cons-mode-line
       (let ((mode-line-format r-mini-modeline-format))
         (apply orig-func args)
         (setq r-mini-modeline-format mode-line-format)))
      (mini-modeline--wrap
       anzu--reset-mode-line
       (let ((mode-line-format r-mini-modeline-format))
         (apply orig-func args)
         (setq r-mini-modeline-format mode-line-format))))))

;;;###autoload
(defun mini-modeline-disable ()
  "Disable `mini-modeline'."
  (interactive)
  (setq-default mode-line-format mini-modeline--orig-mode-line)
  (--each (buffer-list)
    (with-current-buffer it
      (setq mode-line-format mini-modeline--orig-mode-line)))
  (redisplay)
  ;; (remove-hook 'post-command-hook #'mini-modeline-display)
  (remove-hook 'pre-redisplay-functions #'mini-modeline-display)
  (mini-modeline-display 'clear)
  (when mini-modeline-enhance-visual
    (remove-hook 'minibuffer-setup-hook #'mini-modeline--set-buffer-background)
    (mini-modeline--window-divider 'reset))
  (advice-remove #'message #'mini-modeline--reroute-msg)

  ;; compatibility
  (eval-after-load 'anzu
    (progn
      (advice-remove #'anzu--cons-mode-line 'mini-modeline--anzu--cons-mode-line)
      (advice-remove #'anzu--reset-mode-line 'mini-modeline--anzu--reset-mode-line))))

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
