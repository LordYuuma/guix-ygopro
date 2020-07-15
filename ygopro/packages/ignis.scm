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
  (let ((revision "2")
        (commit "965cc7bdf7c99e81604fb6c046b3a327bb12027e"))
    (package
     (name "edopro-core")
     (version (git-version "8.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/EDOPro-Core.git")
             (commit commit)))
       (file-name (git-file-name "edopro-core" version))
       (sha256
        (base32
         "1c4sp64vrfwr9dhdbjq26m87gc70jbr2c1jva503fnrx0wzknk3n"))))
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

(define irrlicht-for-edopro
  (let ((commit "ece3bd7d3ed052028982b7c09da9761339482420")
        (revision "3"))
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
           "04ymj92qwb63h3da7gf1gspsh4zn9js4gy56qvgrw3lk6v303w49")))))))

(define-public edopro
  (package
    (name "edopro")
    (version "38.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/EDOPro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j0smmd3zpkw0shmq516p3b7hd23lpgbq7pm8gl9rqlqjgmidq4r"))
       (patches
        (search-patches
         "edopro-respect-YGOPRO_-_PATH.patch"
         "edopro-respect-XDG-environment-variables.patch"
         "edopro-fix-strings.patch"))))
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
  (let ((commit "ee92b22b1cccb9ce25f73cb3ee9a74f8313ceb6a")
        (revision "10"))
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
           "1n5n08zavjfm0llkc6jxs77wjm10f1gaibwki8lkh14s4jfy229s"))))
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
                "out"
                `(("." "share/ygopro/data/"
                   #:include ("cards.cdb" "release.cdb")
                   #:include-regexp ("fix(Misc|OT|Setcode|String)\\.cdb"))))
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
                          (call-with-input-file (string-append f ".lflist.conf")
                            (lambda (in)
                              (dump-port in port)
                              (close-port in))))
                        lists)
                       (close-port port))))

                 (merge-lflists
                  "out"
                  '("OCG.Korea" "OCG" "TCG" "Traditional" "World" "GOAT"))

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
               (commit "8664627f8ba3b097a25f9c8b5a40f9a0a21e3bcc")))
             (sha256
              (base32
               "167bmqkkvs7a115vblmb3zmsk4sd1mal6jwc1qmy59x1vhkjx3zy"))))
         ("sqlite" ,sqlite)))
      (synopsis "Card databases for EDOPro")
      (description "Provides various card databases for EDOPro.")
      (home-page "https://github.com/ProjectIgnis/BabelCDB")
      (license #f))))

(define-public ignis-scripts
  (let ((commit "144237e207cbd2cdde49f4b2cdcbb495c7f2974c")
        (revision "11"))
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
           "0anm588g7p0fwbdz3rxbx7d0kb1npk9gzc7s4wwvqyi1n73x3fpr"))))
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
      (license license:agpl3+))))

(define-public windbot-ignite
  (package
    (name "windbot-ignite")
    (version "20200715")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/windbot.git")
             (commit version)))
       (sha256
        (base32
         "0yk9122s3rrms2d7cn0hf3dylzijfz28v2k2hwlmi8drbrrnv387"))))
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
       ("ignis-database" ,ignis-database)))
    (native-inputs
     `(("mono" ,mono)))
    (synopsis "EDOPro-compatible bot")
    (description "EDOPro-compatible bot")
    (home-page "https://github.com/ProjectIgnis/windbot")
    (license license:agpl3+)))
