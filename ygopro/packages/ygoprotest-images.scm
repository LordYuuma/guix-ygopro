(define-module (ygopro packages ygoprotest-images)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix build-system copy)
  #:use-module (ygopro packages mycard))

(define-record-type* <ygoprotest-image-reference>
  ygoprotest-image-reference make-ygoprotest-image-reference
  ygoprotest-image-reference?
  (indirect-url ygoprotest-image-indirect-url)
  (database    ygoprotest-image-database))

(define* (ygoprotest-image-method uri hash-algo hash name
                                  #:key (system (%current-system))
                                  (guile (default-guile)))
  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-3))

  (define guile-sqlite3
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-sqlite3))

  (define db-package (ygoprotest-image-database uri))

  (define modules
    (source-module-closure '((guix build download)
                             (guix build utils)
                             (guix progress)
                             (json)
                             (sqlite3)
                             (srfi srfi-11)
                             (srfi srfi-26))))

  (define build
    (with-imported-modules modules
      (with-extensions (list guile-json guile-sqlite3)
        #~(begin
            (use-modules (guix build utils)
                         (guix build download)
                         (guix progress)
                         (ice-9 match)
                         (json)
                         (sqlite3)
                         (srfi srfi-11)
                         (srfi srfi-26)
                         (web uri))
            (mkdir-p (string-append #$output "/pics/field"))
            (define root
              ((compose
                (lambda (url)
                  (let ((suffix "/pics/%d.jpg"))
                    (and url (string-suffix? suffix url)
                         (string-drop-right url (string-length suffix)))))
                (cute assoc-ref <> "imageurl")
                (lambda (port len) (json->scm port))
                http-fetch
                string->uri)
               (getenv "indirect url")))

            (define (%fetch-pic fmt id)
              (false-if-exception
               (let* ((source (format #f fmt root id))
                      (uri (string->uri source))
                      (outfile (format #f fmt #$output id)))
                 (call-with-output-file outfile
                   (lambda (out)
                     (let-values (((in size) (http-fetch uri)))
                       (dump-port* in out
                                   #:reporter (progress-reporter/file
                                               outfile size))
                       (close-port in)
                       (close-port out)))))))
            (define fetch-pic (cute %fetch-pic "~a/pics/~a.jpg" <>))
            (define fetch-field (cute %fetch-pic "~a/pics/field/~a.png" <>))

            (for-each
             (lambda (db)
               (sqlite-fold
                (match-lambda*
                  ((#(id #x80002) seed)
                   (fetch-pic id)
                   (fetch-field id)
                   seed)
                  ((#(id type) seed)
                   (fetch-pic id)
                   seed))
                *unspecified*
                (sqlite-prepare
                 (sqlite-open db SQLITE_OPEN_READONLY)
                 "select id, type from datas")))
             (find-files '#$db-package ".*\\.cdb"))
            (newline)
            #t))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation name build
                      #:script-name "image-download"
                      #:env-vars `(("indirect url" .
                                    ,(ygoprotest-image-indirect-url uri)))
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")
                      #:system system
                      #:local-build? #t
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define-public (ygoprotest-images name db hash)
  (package
    (name (string-append "ygoprotest-images-" name))
    (version (package-version db))
    (source
     (origin
       (method ygoprotest-image-method)
       (uri (ygoprotest-image-reference
             (indirect-url (string-append "http://ygoprotest.appspot.com/"
                                          "ygopro_random_server?v=1034"))
             (database db)))
       (file-name (string-append name "-" version))
       (sha256 (base32 hash))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("pics" "share/ygopro/pics"))))
    (synopsis "Live images for YGOPro")
    (description "Card and field images for YGOPro.")
    (home-page "http://ygoprotest.appspot.com/")
    (license #f)))

(define-public ygoprotest-images-en
  (ygoprotest-images "en" ygopro-database-en
                     "022n6zkfi814hacpy5684653qsjz1pa567arid3r8g0b1f0lkcl7"))
