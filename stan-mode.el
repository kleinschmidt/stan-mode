;;; stan-mode.el --- Major mode for editing STAN files

;; Copyright (C) 2012  Jeffrey Arnold

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>
;; Keywords: languages

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
;;
;; Right not this is a basic mode that defines syntax highlighting,
;; and the syntax table.
;;
;; - imenu
;; 
;; TODO:
;; - command to compile .stan files into cpp
;; - use compile-mode, flymake or custom command to parse compilation
;;   files
;; - indentation
;; - forward-sexp, backward-sexp
;; - SMIE or semantic grammar
(require 'font-lock)

;;
;; Customizable Variables
;;

(setq stan-mode-version "0.0.1")
(defun stan-version ()
  "Show the `stan-mode' version in the echo area."
  (interactive)
  (message (concat "stan-mode version " stan-mode-version)))

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
     "corr_matrix" "cov_matrix" "positive_ordered")
   'symbols)
  "Stan data types.")

(defvar stan-builtin-regexp
  (regexp-opt '("for" "in" "lp__" "T" "print" "lower" "upper") 'symbols)
  "Stan keywords.")

(defvar stan-functions-regexp
  (regexp-opt
   '("abs" "int_step" "min" "max" "if_else" "step" "fabs" "fdim" "fmin" "fmax"
     "fmod" "floor" "ceil" "round" "trunc" "sqrt" "cbrt" "square" "exp" "exp2"
     "expm1" "log" "log2" "log10" "pow" "logit" "inv_logit" "inv_cloglog"
     "hypot" "cos" "sin" "tan" "acos" "asin" "atan" "atan2" "cosh" "sinh"
     "tanh" "acosh" "asinh" "atanh" "erf" "erfc" "Phi" "log_loss" "tgamma"
     "lgamma" "lmgamma" "lbeta" "binomial_coefficient_log" "fma" "multiply_log"
     "log1p" "log1m" "log1p_exp" "log_sum_exp" "rows" "cols" "dot_product"
     "prod" "mean" "variance" "sd" "diagonal" "diag_matrix" "col" "row"
     "softmax" "trace" "determinant" "inverse" "eigenvalue" "eigenvalues_sym"
     "cholesky" "singular_values" "log_normal_p" "normal_p" "exponential_p"
     "gamma_p" "weibull_p")
   'symbols)
  "List of Stan functions")

(defvar stan-distribution-list 
  '("bernoulli" "bernoulli_logit" "binomial" "beta_binomial" "hypergeometric"
    "categorical" "ordered_logistic" "neg_binomial" "poisson" "multinomial"
    "normal" "student_t" "cauchy" "double_exponential" "logistic"
    "lognormal" "chi_square" "inv_chi_square" "scaled_inv_chi_square"
    "exponential" "gamma" "inv_gamma" "weibull" "pareto" "beta" "uniform"
    "dirichlet" "multi_normal" "multi_normal_cholesky" "multi_student_t"
    "wishart" "inv_wishart" "lkj_cov" "lkj_corr_cholesky")
  "List of Stan distributions")

(defvar stan-distribution-regexp
  (regexp-opt (append stan-distribution-list
                      (mapcar (lambda (x) (format "log_%s" x))
                              stan-distribution-list))
              'symbols)
  "Regexp of Stan distributions")

(defvar stan-operators
  '("+" "-" "*" "/" ".*" "./" "\\")
  "List of Stan operators")

(defvar stan-font-lock-keywords
  `((,stan-blocks-regexp . font-lock-keyword-face)
    (,stan-types-regexp . font-lock-type-face)
    (,stan-builtin-regexp . font-lock-keyword-face)
    (,stan-assign-regexp . font-lock-reference-face)
    ;; faces used in ess-bugs
    (,stan-functions-regexp . font-lock-function-name-face)
    (,stan-distribution-regexp . font-lock-function-name-face)))

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
(modify-syntax-entry ?<  "." stan-mode-syntax-table)
(modify-syntax-entry ?>  "." stan-mode-syntax-table)
;; otherwise lower= will not highlight
(modify-syntax-entry ?=  "." stan-mode-syntax-table)
(modify-syntax-entry ?\" "\"" stan-mode-syntax-table)
;; cannot identify both <...> and <- 
;; (modify-syntax-entry ?<  "(>" stan-mode-syntax-table)
;; (modify-syntax-entry ?>  ")<" stan-mode-syntax-table)

;; Imenu tags 
(defvar stan-imenu-generic-expression
  `(("Variable" ,(concat stan-types-regexp "\\(<.*>\\|[.*]\\)?[ 	\n]*"
                         "\\([A-Za-z0-9_]+\\)") 3)))

;; Indenting
;; TODO:
;; Indentation notes
;; - Lines ending with ; are complete statements/expr
;; - If complete statement, indent at same level as previous 
;;   complete statement
;; - If not complete statement, then indent to
;;   - open ( or [
;;   - <-, ~ 
;;   - else last line
;; - If previous line ends in {, indent >>
;; - If previous line ends in }, indent <<
;; - If previous line begins with "for", indent >>

;; 
;; Define Major Mode
;;
(define-derived-mode stan-mode fundamental-mode "Stan"
  "A major mode for editing Stan files."
  :syntax-table stan-mode-syntax-table
  :abbrev-table stan-mode-abbrev-table
  :group 'languages

  ;; syntax highlighting
  (setq font-lock-defaults '((stan-font-lock-keywords)))

  ;; comments
  (setq mode-name "Stan")
  ;;(setq comment-start stan-comment-start)
  ;; (set (make-local-variable 'comment-start) "// ")
  ;; (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start) stan-comment-start)
  (set (make-local-variable 'comment-end) stan-comment-end)
  ;; no tabs
  (setq indent-tabs-mode nil)
  ;;
  (setq imenu-generic-expression stan-imenu-generic-expression)
  )

;;  Example error line
;; LOCATION:  file=input; line=2, column=10
;; (stan "LOCATION:[ 	]+file=input;[ 	]+line=\\([0-9]+\\),
;; column=\\([0-9]+\\)" nil 1 2)

(provide 'stan-mode)

;;; On Load
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stan\\'" . stan-mode))

;;; stan-mode.el ends here
