(require 'flymake)
(require 'stan-mode)

(defun flymake-stan-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (temp-output (temp-file))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "stanc" (list "-o" temp-output local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.stan$"
              flymake-stan-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

;; LOCATION:  file=input; line=2, column=10
(setq flymake-err-line-patterns
      '(("LOCATION:[ 	]+file=input;[ 	]+line=\\([0-9]+\\), column=\\([0-9]+\\)" nil 1 2 nil)) 
      flymake-err-line-patterns)

