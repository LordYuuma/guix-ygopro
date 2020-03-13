(define-module (ygopro packages)
  #:use-module ((gnu packages) #:prefix gnu:)
  #:export (search-patch search-patches))

(define (search-patch file-name)
  (parameterize ((gnu:%patch-path
                  (append
                   (map (lambda (directory)
                          (string-append directory "/ygopro/patches"))
                        %load-path)
                   (gnu:%patch-path))))
    (gnu:search-patch file-name)))

(define-syntax-rule (search-patches file-name ...)
  (list (search-patch file-name) ...))
