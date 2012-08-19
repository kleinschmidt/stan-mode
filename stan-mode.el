;;; stan-mode.el --- Major mode for editing STAN files

;; Copyright (C) 2012  Jeffrey Arnold

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>
;;; Commentary:

(require 'cc-mode)
(require 'font-lock)

;;
;; Customizable Variables
;;

(defun stan-mode-version ()
  "Currently package.el doesn't support prerelease version numbers."
  "0.0.1")

(defgroup stan nil
  "A Stan major mode."
  :group 'languages)

(defvar stan-mode-hook nil)

(defvar stan-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Stan major mode")

(defvar stan-comment-start "//" "Stan comment style to use")
(defvar stan-comment-end "" "Stan comment style to use")

(defvar stan-mode-abbrev-table nil
  "Abbrev table used in stan-mode buffers.")

(define-abbrev-table 'stan-mode-abbrev-table ())

;; Font-Locks

;; <- and ~ 
(defvar stan-assign-regexp 
  (regexp-opt '("<-" "~"))
  "Assigment operators")

(defvar stan-blocks-regexp
  "\\<\\(model\\|\\(transformed[ \t]+\\)?\\(data\\|parameters\\)\\|generated[ \t]+quantities\\)\\>"
  "Stan blocks.")

(defvar stan-types-regexp
  (regexp-opt 
   '("int" "real" "vector" "simplex" "ordered" "row_vector" "matrix" 
     "corr_matrix" "cov_matrix")
   'words)
  "Stan data types.")

(defvar stan-builtin-regexp
  (regexp-opt '("for" "in" "lp__" "T") 'words)
  "Stan keywords.")

(defvar stan-font-lock-keywords
  `((,stan-blocks-regexp . font-lock-keyword-face)
    (,stan-types-regexp . font-lock-type-face)
    (,stan-builtin-regexp . font-lock-keyword-face)
    (,stan-assign-regexp . font-lock-reference-face)))

;;; Define Syntax table
(setq stan-mode-syntax-table (make-syntax-table))
;; support #, //, and /* ... */ comments
;; see http://www.slac.stanford.edu/comp/unix/gnu-info/elisp_32.html
(modify-syntax-entry ?\/  ". 124b"  stan-mode-syntax-table)
(modify-syntax-entry ?*  ". 23"  stan-mode-syntax-table)
(modify-syntax-entry ?\n "> b"  stan-mode-syntax-table)
(modify-syntax-entry ?#  "< b"  stan-mode-syntax-table)
(modify-syntax-entry ?(  "()" stan-mode-syntax-table)
(modify-syntax-entry ?)  ")(" stan-mode-syntax-table)
(modify-syntax-entry ?{  "({" stan-mode-syntax-table)
(modify-syntax-entry ?}  "){" stan-mode-syntax-table)
(modify-syntax-entry ?[  "(]" stan-mode-syntax-table)
(modify-syntax-entry ?]  ")[" stan-mode-syntax-table)

;; Indenting

;; (defun stan-indent-region (start end &optional quiet)
;;   (funcall 'c-indent-region start end quiet))

;; (defun stan-indent-line ()
;;   (funcall 'c-indent-line))

;; 
;; Define Major Mode
;;
(define-derived-mode stan-mode fundamental-mode "Stan"
  "A major mode for editing Sample files."
  :syntax-table stan-mode-syntax-table

  ;; syntax highlighting
  (setq font-lock-defaults '((stan-font-lock-keywords)))

  ;; comments
  (setq mode-name "Stan")
  (make-local-variable 'comment-start)
  (setq comment-start stan-comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start stan-comment-end)
  ;; no tabs
  (setq indent-tabs-mode nil)
  ;; (setq c-syntatic-information t)
  ;; (make-local-variable 'indent-line-function)
  ;; (setq indent-line-function 'stan-indent-line)
  ;; (make-local-variable 'indent-region-function)
  ;; (setq indent-region-function 'stan-indent-region)
  )

(provide 'stan-mode)

;;; On Load
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stan\\'" . stan-mode))

;;; stan-mode.el ends here
