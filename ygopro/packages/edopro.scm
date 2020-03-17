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
    (version "37.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edo9300/ygopro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ksl1453cnm0mwqrf2kv00fm8jklnqzmgc407miskak5737ahybs"))
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
               (substitute* "gframe/game.cpp"
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
