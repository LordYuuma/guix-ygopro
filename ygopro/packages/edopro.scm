(define-module (ygopro packages edopro)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ygopro packages)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages lua))

;; see <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=40066>
(define premake5
  (package
    (inherit premake4)
    (version "5.0.0-alpha14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/premake/premake-core/";
                                  "releases/download/v" version
                                  "/premake-" version "-src.zip"))
              (sha256
               (base32
                "0236s7bjvxf7x1l5faywmfzjywflpx42ngyhkn0mqqjnh54a97vw"))))
    (arguments
     (substitute-keyword-arguments (package-arguments premake4)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "../../bin/release/premake5"
                             (string-append (assoc-ref outputs "out") "/bin"))
               #t))))))
    (description "@code{premake5} is a command line utility that reads a
scripted definition of a software project and outputs @file{Makefile}s or
other lower-level build files.")))

(define-public edopro-core
  (let ((commit "b91379d2f9c47d5e59a8951feca226267a204c26")
        (revision "0"))
   (package
    (name "edopro-core")
    (version (git-version "6.1" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edo9300/ygopro-core.git")
             (commit commit)))
       (file-name (git-file-name "edopro-core" version))
       (sha256
        (base32
         "1zk6x77m6vwwnw0iy3vrp6dy59xfj2wgxvidr8rbnxyzsff95z5j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no `check' target
       #:make-flags `("CC=gcc" "--directory=build")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "interpreter.h"
               (("#include <l([a-z]+).h>" all lua-cont)
                (string-append "extern \"C\" {\n"
                               "#include <l" lua-cont ".h>\n"
                               "}")))
             #t))
         (delete 'bootstrap)
         (replace 'configure (lambda _ (invoke "premake5" "gmake")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include/ygopro-core")))
               (for-each (lambda (f) (install-file f inc))
                         (find-files "." ".*\\.h"))
               (install-file "bin/debug/libocgcore.so" lib)
               #t))))))
    (inputs
     `(("lua" ,lua)))
    (native-inputs
     `(("premake5" ,premake5)))
    (synopsis "Bleeding-edge fork of ygopro-core")
    (description
     "edopro-core is a bleeding-edge fork of ygopro-core with updates to
accommodate for new cards and features.  It is incompatible with forks not
derived from itself.")
    (home-page "https://github.com/edo9300/ygopro")
    (license license:agpl3+))))
