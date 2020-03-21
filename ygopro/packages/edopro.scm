(define-module (ygopro packages edopro)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ygopro packages)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (ygopro packages mycard))

(define-public edopro-core
  (package
   (name "edopro-core")
   (version "7.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/edo9300/ygopro-core.git")
           (commit (string-append "v" version))))
     (file-name (git-file-name "edopro-core" version))
     (sha256
      (base32
       "11jrg90f08j8p3gspgp0if3660g66qvcwqlj6wayj9fajbqmclng"))))
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
    (license license:agpl3+)))

(define nlohmann-json
  (package
    (name "nlohmann-json")
    (version "3.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nlohmann/json.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04rry1xzis71z5gj1ylcj8b4li5q18zxhcwaviwvi3hx0frzxl9w"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("single_include/nlohmann/json.hpp"
          "include/nlohmann/"))))
    (synopsis "Single-header JSON parser")
    (description "Nlohmann JSON is a parser, which aims for intuitive syntax,
trivial integration and 100% testing.")
    (home-page "https://github.com/nlohmann/json")
    (license license:expat)))

(define-public edopro
  (package
    (name "edopro")
    (version "37.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edo9300/ygopro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16kgm2vp6d1h04rw3sjrlgfqvj3i77czrjvg70xrzsh6f8n7hkm6"))
       (patches
        (search-patches
         "edopro-respect-YGOPRO_-_PATH.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no `check' target
       #:make-flags `("CC=gcc" "--directory=build")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (font (string-append
                           (assoc-ref inputs "font-google-noto")
                           "/share/fonts/truetype/NotoSans-Regular.ttf"))
                    (datadir (string-append out "/share/ygopro")))
               (rename-file "gframe/lzma/premake4.lua"
                            "gframe/lzma/premake5.lua")
               (substitute* (find-files "." ".*\\.(cpp|h)")
                 (("(ocgapi|progressivebuffer)\\.h" all lib)
                  (string-append "ygopro-core/" all)))
               (substitute* "gframe/premake5.lua"
                 (("/usr/include/freetype2")
                  (string-append (assoc-ref inputs "freetype")
                                 "/include/freetype2"))
                 (("/usr/include/irrlicht")
                  (string-append (assoc-ref inputs "irrlicht")
                                 "/include/irrlicht")))
               (substitute* "gframe/drawing.cpp"
                 (("draw2DRectangleClip") "draw2DRectangle")
                 (("nullptr,clip") "clip"))
               (substitute* "gframe/image_manager.cpp"
                 (("./textures/") (string-append datadir "/textures/")))
               (substitute* (find-files "gframe" ".*\\.cpp")
                 (("./config") (string-append out "/etc/ygopro")))
               (substitute* "config/system.conf"
                 (("textfont = .*$")
                  (string-append "textfont = " font " 12\n"))
                 (("numfont = .*$")
                  (string-append "numfont = " font "\n")))
               #t)))
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key configure-flags inputs #:allow-other-keys)
             (invoke "premake5" "gmake"
                     (string-append "--prebuilt-core="
                                    (assoc-ref inputs "edopro-core")
                                    "/lib"))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin"))
                    (datadir (string-append out "/share/ygopro")))
               (copy-recursively "config" (string-append out "/etc/ygopro"))
               (copy-recursively "textures"
                                 (string-append datadir "/textures"))
               (install-file "bin/debug/ygopro" bindir)
               #t))))))
    (inputs
     `(("curl" ,curl)
       ("font-google-noto" ,font-google-noto)
       ("freetype" ,freetype)
       ("fmt" ,fmt)
       ("glu" ,glu)
       ("irrlicht" ,irrlicht)
       ("libevent" ,libevent)
       ("libgit2" ,libgit2)
       ("lua" ,lua)
       ("mesa" ,mesa)
       ("nlohmann-json" ,nlohmann-json)
       ("sqlite" ,sqlite)
       ("premake5" ,premake5)
       ("edopro-core" ,edopro-core)))
    (native-search-paths
     (package-native-search-paths ygopro))
    (synopsis "Bleeding-edge fork of ygopro")
    (description
     "Edopro is a bleeding edge fork of the ygopro client.  Because it relies
on its own fork of the ygopro-core, it is incompatible with other clients not
built on top of that.")
    (home-page "https://github.com/mycard/ygopro")
    (license license:agpl3+)))

(define-public edopro-database
  (let ((commit "00175928c9669b1bef640fe56573f4d68b84928b")
        (revision "0"))
    (package
      (name "edopro-database")
      (version (git-version "0.2019.09.18" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YgoproStaff/live2019.git")
               (commit commit)))
         (file-name (git-file-name "edopro-database" version))
         (sha256
          (base32
           "0vkj63sjk6bcc1na7zns4sqiz2aharbs4ybzx4qpaclfd2133ga4"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("cards.cdb" "share/ygopro/data/")
           ("script" "share/ygopro/script"
            #:include-regexp (".*\\.lua"))
           ("lflist.conf" "share/ygopro/data/"))
         #:modules ((guix build copy-build-system)
                    (guix build utils)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
          (add-before 'install 'build-lflist
            (lambda _
              (call-with-output-file "lflist.conf"
                (lambda (port)
                  (for-each
                   (lambda (f)
                     (call-with-input-file f
                       (cute dump-port <> port)))
                   (find-files "lflists")))))))))
      (synopsis "Database and scripts for edopro")
      (description "Database and scripts compatible with edopro.")
      (home-page "https://github.com/YgoproStaff/live2019")
      (license license:agpl3+))))
