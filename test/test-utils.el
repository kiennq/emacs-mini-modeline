;;; test-utils.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author:  Kien Nguyen <kien.n.quang <at> gmail.com>

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

;;

;;; Code:
(require 'bootstrap)
(require 'ert)

(ert-deftest mimo-byte-compilation-test ()
  (seq-doseq (library (-filter
                       (lambda (file)
                         (and (f-ext? file "el")
                              (not (string-match-p "test" file))))
                       (append (when (or load-file-name buffer-file-name)
                                 (f-files (f-parent (f-dirname (or load-file-name buffer-file-name)))))
                               (f-files default-directory))))
    (let ((byte-compile-error-on-warn t))
      (message "Testing file %s" library)
      (should (byte-compile-file (save-excursion
                                   (find-library library)
                                   (buffer-file-name)))))))

;;; test-utils.el ends here
