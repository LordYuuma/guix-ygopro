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
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xiph)
  #:use-module (ygopro packages mycard))

(define-public edopro-core
  (package
   (name "edopro-core")
   (version "8.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ProjectIgnis/EDOPro-Core.git")
           (commit (string-append "v" version))))
     (file-name (git-file-name "edopro-core" version))
     (sha256
      (base32
       "04pjbjy1rms2m8ici9r0zls3y5plh6bc0ffk92dmbvgx4mwadz2y"))))
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
        (base32 "1w54h19xpw2mnmbrzcr0w0wh7dxafgl0mn908l32gyvfzn5cnhkb"))
       (patches
        (search-patches
         "edopro-respect-YGOPRO_-_PATH.patch"
         "edopro-respect-XDG-environment-variables.patch"
         "edopro-fix-strings.patch"
         "edopro-fix-advantage.patch"
         "edopro-hide-hands-and-loop-music.patch"
         "edopro-save-replay-on-connection-error.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "gframe/repo_manager.h"
             (("LIBGIT2_VER_MINOR==99")
              "LIBGIT2_VER_MAJOR>=1 || LIBGIT2_VER_MINOR>=99"))))))
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
                           "/share/fonts/opentype/NotoSansCJKjp-Regular.otf"))
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
                                 "/include/irrlicht"))
                 (("SDL2d") "SDL2"))
               (substitute* "gframe/drawing.cpp"
                 (("draw2DRectangleClip") "draw2DRectangle")
                 (("nullptr,clip") "clip"))
               (substitute* "gframe/game_config.cpp"
                 (("/etc/ygopro" all) (string-append out all))
                 (("/usr(/share/ygopro)" all rest) (string-append out rest)))
               (substitute* "gframe/logging.cpp"
                 (("error.log") "/dev/stderr"))
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
                     "--environment-paths" "--xdg-environment"
                     "--sound=sdl-mixer"
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
       ("edopro-core" ,edopro-core)
       ("font-google-noto" ,font-google-noto)
       ("freetype" ,freetype)
       ("fmt" ,fmt)
       ("glu" ,glu)
       ("irrlicht" ,irrlicht-for-edopro)
       ("libevent" ,libevent)
       ("libflac" ,flac)
       ("libgit2" ,libgit2)
       ("libvorbis" ,libvorbis)
       ("lua" ,lua)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("nlohmann-json" ,nlohmann-json)
       ("premake5" ,premake5)
       ("sdl2-mixer" ,(sdl-union (list sdl2 sdl2-mixer)))
       ("sqlite" ,sqlite)))
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
  (let ((commit "3e1787b344989cd49011425527f8f719b562d663")
        (revision "4"))
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
           "1cj92v36gyci7k3xx82jpap2yz9gccczl7fi76ggjq4fjs7d5msw"))))
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
                                            "prerelease.cdb"
                                            "prerelease-cp20.cdb"
                                            "prerelease-dp24.cdb"
                                            "prerelease-etco.cdb"
                                            "prerelease-rotd.cdb"
                                            "prerelease-sd39.cdb"))))
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
               (commit "061b753bc87c032bef4feb2953f92e802628c6e8")))
             (sha256
              (base32
               "0xm97lvr5cg4l8w2kv430dghyhhl0bk806niy5da7n4vsnz304as"))))))
      (synopsis "Card databases for EDOPro")
      (description "Provides various card databases for EDOPro.")
      (home-page "https://github.com/ProjectIgnis/BabelCDB")
      (license #f))))

(define-public ignis-scripts
  (let ((commit "addd2fbc7ae38a7744b948f54ac73d7537b8e769")
        (revision "4"))
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
           "14rp3d8814xdi8sm6kgi2kbk9h7bgnmz46md23rccws49n57f4k5"))))
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
