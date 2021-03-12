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
  #:use-module (gnu packages mono)
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
    (version "9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/EDOPro-Core.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "edopro-core" version))
       (sha256
        (base32
         "0cx8mmbg6ipr6w44dp52l6p3axpaszg3mz349qpbz4fpl21gwnmm"))))
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
  (let ((commit "333382b2fd495fac1161b8aac2aa7ccb13439684")
        (revision "5"))
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
           "0jic9ss7x250ng82vpnkkkfpx4n7z2jbhyhn72ll2zcj1zn7qdzp")))))))

(define-public edopro
  (package
    (name "edopro")
    (version "39.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/EDOPro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14zcqp4xq5n7h0rms8h4hvmg0q4vn57195smp711vdkir29wav11"))
       (patches
        (search-patches
         "edopro-fix-segfault.patch"
         "edopro-missing-config.patch"
         "edopro-utf8-source.patch"
         "edopro-respect-YGOPRO_-_PATH.patch"
         "edopro-respect-XDG-environment-variables.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "freetype")
           (substitute* "premake5.lua"
             (("include \"freetype\"") ""))
           (delete-file-recursively "sfAudio")
           #t))))
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
                 (("(/usr/include/freetype2|../freetype/include)")
                  (string-append (assoc-ref inputs "freetype")
                                 "/include/freetype2"))
                 (("/usr/include/irrlicht")
                  (string-append (assoc-ref inputs "irrlicht")
                                 "/include/irrlicht"))
                 (("SDL2d") "SDL2"))
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
         (add-after 'unpack 'ya-didnt-package-all-the-textures
           (lambda _
             (substitute* "gframe/image_manager.cpp"
               (("CHECK_RETURN\\(tSettings\\);") ""))
             #t))
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

(define-public ignis-database-baseline
  (package
   (name "ignis-database")
   (version "20201222")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ProjectIgnis/BabelCDB.git")
           (commit version)))
     (file-name (git-file-name "ignis-database" version))
     (sha256
      (base32
       "08hg9s7c6jmxw4i5d12w2xi4qmmc34y1n86449004i5j0ivhd59k"))))
   (build-system copy-build-system)
   (outputs '("out" "pre-release" "rush" "skills" "unofficial"))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-names
          (lambda* _
            (rename-file "Proxy_Horse.cdb" "prerelease-proxy-horse.cdb")
            #t))
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (define (install-with-output output install-plan)
              ((assoc-ref %standard-phases 'install)
               #:outputs `(("out" . ,(assoc-ref outputs output)))
               #:install-plan install-plan))

            (install-with-output
             "out"
             `(("." "share/ygopro/data/"
                #:include ("cards.cdb")
                #:include-regexp (".*-release\\.cdb"
                                  "(^|/)release.*\\.cdb"
                                  "fix(Misc|OT|Setcode|String)\\.cdb"))))
            (install-with-output
             "rush" `(("cards-rush.cdb" "share/ygopro/data/")))
            (install-with-output
             "skills" `(("cards-skills.cdb" "share/ygopro/data/")))
            (install-with-output
             "unofficial" `(("." "share/ygopro/data/"
                             #:include ("cards-unofficial.cdb"
                                        "cards-unofficial-new.cdb")
                             #:include-regexp (".*-unofficial\\.cdb"))))
            (install-with-output
             "pre-release" `(("." "share/ygopro/data/"
                              #:include ("cards-rush-prerelease.cdb")
                              #:include-regexp ("prerelease-.*\\.cdb"))))
            #t))
        (add-after 'install 'install-lflists
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (with-directory-excursion (assoc-ref inputs "lflists")
              (define (merge-lflists output lists)
                (call-with-output-file (string-append
                                        (assoc-ref outputs output)
                                        "/share/ygopro/data/lflist.conf")
                  (lambda (port)
                    (for-each
                     (lambda (f)
                       (call-with-input-file
                           (if (file-exists? f) f
                               (string-append f ".lflist.conf"))
                         (lambda (in)
                           (dump-port in port)
                           (close-port in))))
                     lists)
                    (close-port port))))

              (merge-lflists
               "out"
               '("OCG.Korea" "OCG" "TCG" "OCG.lflist.new.conf"
                 "Traditional" "World" "GOAT"))

              (merge-lflists
               "rush"
               '("Rush" "Rush-Prerelease"))

              (merge-lflists
               "skills"
               '("Speed")))
            #t)))))
   (inputs
    `(("lflists"
       ,(origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/ProjectIgnis/LFLists")
           (commit "e27a53f3d710f264642ab16f9deff8fa9503220c")))
         (sha256
          (base32
           "1k4rkr1bjypsxypakgc8yqdwn04gl3rnrfz0sbk2iz4a2ma4bz4z"))))
      ("sqlite" ,sqlite)))
   (synopsis "Card databases for EDOPro")
   (description "Provides various card databases for EDOPro.")
   (home-page "https://github.com/ProjectIgnis/BabelCDB")
   (license #f)))

(define-public ignis-database-nightly
  (let ((database-night "20210312")
        (database-commit "510e19244d015bb2102cfbb63e2e9da352633aea")
        (database-hash "1bz77apf3d45cbzicsdbx2sx04qqrpc2mnsf2pxlvi61d4ay4f6d")
        (lflists-commit "7cd70f732064f26a6ff44ebf281304cfc38d9311")
        (lflists-hash "0z2krvxhx1cii90bplsi9yw0bgfg9j92pmcsk0izrkfq347ly4bj"))
    (package
      (inherit ignis-database-baseline)
      (version (string-append database-night "-nightly"))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ProjectIgnis/BabelCDB.git")
               (commit database-commit)))
         (file-name (git-file-name "ignis-database" version))
         (sha256 (base32 database-hash))))
      (inputs
       `(("lflists"
          ,(origin
            (method git-fetch)
            (uri
             (git-reference
              (url "https://github.com/ProjectIgnis/LFLists")
              (commit lflists-commit)))
            (sha256 (base32 lflists-hash))))
         ,@(package-inputs ignis-database-baseline)))
      (outputs (cons "goat" (package-outputs ignis-database-baseline)))
      (arguments
       (substitute-keyword-arguments (package-arguments ignis-database-baseline)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'fix-names)
             (add-after 'install 'install-missing-cdbs
               (lambda* (#:key outputs #:allow-other-keys)
                 (define (install-with-output output install-plan)
                   ((assoc-ref %standard-phases 'install)
                    #:outputs `(("out" . ,(assoc-ref outputs output)))
                    #:install-plan install-plan))
                 (install-with-output
                  "goat" `(("goat-entries.cdb" "share/ygopro/data/")))
                 ;; baseline is missing plain prerelease.cdb
                 (install-with-output
                  "pre-release" `(("prerelease.cdb" "share/ygopro/data/")))
                 #t))
             (replace 'install-lflists
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (with-directory-excursion (assoc-ref inputs "lflists")
                   (define (merge-lflists output lists)
                     (call-with-output-file (string-append
                                             (assoc-ref outputs output)
                                             "/share/ygopro/data/lflist.conf")
                       (lambda (port)
                         (for-each
                          (lambda (f)
                            (call-with-input-file (string-append f ".lflist.conf")
                              (lambda (in)
                                (dump-port in port)
                                (close-port in))))
                          lists)
                         (close-port port))))

                   (merge-lflists
                    "out"
                    '("OCG" "OCG.Korea" "0TCG" "Traditional" "World"))
                   (merge-lflists "goat" '("GOAT"))
                   (merge-lflists "rush" '("Rush" "Rush-Prerelease"))
                   (merge-lflists "skills" '("Speed"))))))))))))

(define-public ignis-scripts-baseline
  (package
    (name "ignis-scripts")
    (version "20201222")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/CardScripts.git")
             (commit version)))
       (file-name (git-file-name "ignis-scripts" version))
       (sha256
        (base32
         "06rvfpsj4xfa2v2lhwh5icrqjwzh2pnkj3h9z459pc8l95y0l46g"))))
    (build-system copy-build-system)
    (outputs '("out" "pre-release" "pre-errata" "rush" "skill" "unofficial"))
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
              "pre-release" `(("pre-release" "share/ygopro/script")))
             (install-with-output
              "pre-errata" `(("pre-errata" "share/ygopro/script")))
             #t)))))
    (synopsis "Card scripts for EDOPro")
    (description "Provides card scripts for EDOPro.")
    (home-page "https://github.com/ProjectIgnis/CardScripts")
    (license license:agpl3+)))

(define-public ignis-scripts-nightly
  (let ((scripts-night "20210312")
        (scripts-commit "6c7c84fa93a7a89b5d7cefb595fa2c5d668f41d0")
        (scripts-hash "0985hv72d2iq3pxb1y4nyrrv6mc33vmfblmxdv21x19lc90ckcgv"))
    (package
      (inherit ignis-scripts-baseline)
      (version (string-append scripts-night "-nightly"))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ProjectIgnis/CardScripts.git")
               (commit scripts-commit)))
         (file-name (git-file-name "ignis-scripts" version))
         (sha256 (base32 scripts-hash)))))))

(define-public windbot-ignite
  (package
    (name "windbot-ignite")
    (version "20201222")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/windbot.git")
             (commit version)))
       (sha256
        (base32
         "0n2gwh0k2j4jwm715gqxc23inkh9g6jm5j84y5sg9z64sg0nbm6r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (add-after 'unpack 'patch-sources
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (share (string-append out "/share/windbot")))
               (substitute* (find-files "." ".*\\.cs")
                 ;; remap folders
                 (("\"(Dialogs|Decks)/\"" all folder)
                  (format #f "\"~a/~a/\"" share
                          (string-downcase folder)))
                 ;; Don't load extra executors
                 (("Directory.GetFiles\\(\"Executors\", .*\\);")
                  "{};"))
               #t)))
         (replace 'build
           (lambda _
             (invoke "xbuild" "WindBot.csproj")
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sqlite (string-append (assoc-ref inputs "sqlite") "/lib"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (share (string-append out "/share/windbot")))
               (with-directory-excursion "bin/Debug"
                 (copy-recursively "Decks" (string-append share "/decks"))
                 (copy-recursively "Dialogs" (string-append share "/dialogs"))
                 (install-file "bots.json" share)
                 (map (lambda (f) (install-file f lib))
                      (find-files "." ".*\\.(exe|dll)$")))
               (mkdir-p bin)
               (let ((windbot-wrapper (string-append bin "/windbot")))
                 (call-with-output-file windbot-wrapper
                   (lambda (port)
                     (format port "#!~a~%~a ~a/WindBot.exe DbPath=~a \"$@\""
                             (which "bash")
                             (which "mono")
                             lib
                             (string-append
                              (assoc-ref inputs "ignis-database")
                              "/share/ygopro/data/cards.cdb"))))
                 (chmod windbot-wrapper #o755)
                 (wrap-program windbot-wrapper
                   `("LD_LIBRARY_PATH" prefix (,sqlite))))
               #t))))))
    (inputs
     `(("sqlite" ,sqlite)
       ("ignis-database" ,ignis-database-baseline)))
    (native-inputs
     `(("mono" ,mono)))
    (synopsis "EDOPro-compatible bot")
    (description "EDOPro-compatible bot")
    (home-page "https://github.com/ProjectIgnis/windbot")
    (license license:agpl3+)))
