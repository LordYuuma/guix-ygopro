(define-module (ygopro packages ignis)
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
           (url "https://github.com/ProjectIgnis/EDOPro-Core.git")
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
     "EDOPro-Core is a bleeding-edge fork of ygopro-core with updates to
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

(define irrlicht-for-edopro
  (let ((commit "e6d4e1e7b1904bb42b2fbb2aefa9a6bb04864565")
        (revision "1"))
   (package
    (inherit irrlicht)
    (name "irrlicht-for-edopro")
    (version (git-version "1.8.4" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edo9300/irrlicht1-8-4.git")
             (commit commit)))
       (sha256
        (base32
         "0h3jbs029z2hjhgybnfr0fzfcsjggrvi4a9g73psyy3yxxpb7c12")))))))

(define-public edopro
  (package
    (name "edopro")
    (version "38.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/EDOPro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z919wsq8459y1hqaxgc08bszbs70cr27w9pg06292dxmrjicrkp"))
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
             (invoke "premake5" "gmake" "--environment-paths"
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
       ("irrlicht" ,irrlicht-for-edopro)
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
     "EDOPro is a bleeding edge fork of the ygopro client.  Because it relies
on its own fork of the ygopro-core, it is incompatible with other clients not
built on top of that.")
    (home-page "https://github.com/ProjectIgnis/ygopro")
    (license license:agpl3+)))

(define-public ignis-database
  (let ((commit "ca705d8c4b071ae4621cdb12c382eecb7bfd24aa")
        (revision "0"))
    (package
      (name "ignis-database")
      (version (git-version (package-version edopro) revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ProjectIgnis/BabelCDB.git")
               (commit commit)))
         (file-name (git-file-name "ignis-database" version))
         (sha256
          (base32
           "0afa4wdwmyjpnb8yx9cabivb0r1jd5p8m7jdwi1vj8g034hkq9lp"))))
      (build-system copy-build-system)
      (outputs '("out" "pre-release" "rush" "skills" "unofficial"))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (define (install-with-output output install-plan)
                 ((assoc-ref %standard-phases 'install)
                  #:outputs `(("out" . ,(assoc-ref outputs output)))
                  #:install-plan install-plan))

               (install-with-output
                "out" `(("cards.cdb" "share/ygopro/data/")))
               (install-with-output
                "rush" `(("cards-rush.cdb" "share/ygopro/data/")))
               (install-with-output
                "skills" `(("cards-skills.cdb" "share/ygopro/data/")))
               (install-with-output
                "unofficial" `(("cards-unofficial.cdb" "share/ygopro/data/")))
               (install-with-output
                "pre-release" `(("." "share/ygopro/data/"
                                 #:include ("cards-rush-prerelease.cdb"
                                            "prerelease-cp20.cdb"
                                            "prerelease-rotd.cdb"))))
               #t))
           (add-after 'install 'install-lflists
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion (assoc-ref inputs "lflists")
                 (call-with-output-file (string-append
                                         (assoc-ref outputs "out")
                                         "/share/ygopro/data/lflist.conf")
                   (lambda (port)
                     (for-each
                      (lambda (f)
                        (call-with-input-file (string-append f ".lflist.conf")
                          (lambda (in)
                            (dump-port in port)
                            (close-port in))))
                      '("OCG-Korea" "OCG" "TCG" "Traditional" "World"))
                     (close-port port)))
                 (copy-file "Rush.lflist.conf"
                            (string-append (assoc-ref outputs "rush")
                                           "/share/ygopro/data/lflist.conf"))
                 (copy-file "Speed.lflist.conf"
                            (string-append (assoc-ref outputs "skills")
                                           "/share/ygopro/data/lflist.conf")))
               #t)))))
      (inputs
       `(("lflists"
          ,(origin
             (method git-fetch)
             (uri
              (git-reference
               (url "https://github.com/ProjectIgnis/LFLists")
               (commit "714fdb3334f3a05758888b0f3604ac54d4b95ed3")))
             (sha256
              (base32
               "06h8p5vjngim7f5k9wg8aff1w9sg3wfcafkx99693p387bq30ima"))))))
      (synopsis "Card databases for EDOPro")
      (description "Provides various card databases for EDOPro.")
      (home-page "https://github.com/ProjectIgnis/BabelCDB")
      (license #f))))

(define-public ignis-scripts
  (let ((commit "7b5620106c99b617152c630cf6bf53bfd61bf8b9")
        (revision "0"))
    (package
      (name "ignis-scripts")
      (version (git-version (package-version edopro) revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ProjectIgnis/CardScripts.git")
               (commit commit)))
         (file-name (git-file-name "ignis-scripts" version))
         (sha256
          (base32
           "1fg6rnnyplk0a6f4xlk5971i54b3v4w612k6w1v0zjsdrpl3r8a3"))))
      (build-system copy-build-system)
      (outputs '("out" "pre-release" "rush" "skill" "unofficial"))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (define (install-with-output output install-plan)
                 ((assoc-ref %standard-phases 'install)
                  #:outputs `(("out" . ,(assoc-ref outputs output)))
                  #:install-plan install-plan))

               (install-with-output
                "out" `(("official" "share/ygopro/script")
                        ("." "share/ygopro/script"
                         #:exclude
                         ("official" "pre-errata" "pre-release"
                          "rush" "skill" "unofficial"))))
               (install-with-output
                "rush" `(("rush" "share/ygopro/script")))
               (install-with-output
                "skill" `(("skill" "share/ygopro/script")))
               (install-with-output
                "unofficial" `(("unofficial" "share/ygopro/script")))
               (install-with-output
                "pre-release" `(("pre-errata" "share/ygopro/script")
                                ("pre-release" "share/ygopro/script")))
               #t)))))
      (synopsis "Card scripts for EDOPro")
      (description "Provides card scripts for EDOPro.")
      (home-page "https://github.com/ProjectIgnis/CardScripts")
      (license license:agpl3+))))
