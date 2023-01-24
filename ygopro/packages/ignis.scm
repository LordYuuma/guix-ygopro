(define-module (ygopro packages ignis)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ygopro packages)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (ygopro packages mycard))

(define-public edopro-core
  (package
    (name "edopro-core")
    (version "10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edo9300/ygopro-core.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "edopro-core" version))
       (sha256
        (base32
         "08qlxq12i9a58lvh05lf1ygjj1qkbn608w3f9gkxi4sj5fk4y1rj"))))
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
    (inputs (list lua))
    (native-inputs (list premake5))
    (synopsis "Bleeding-edge fork of ygopro-core")
    (description
     "EDOPro-Core is a bleeding-edge fork of ygopro-core with updates to
accommodate for new cards and features.  It is incompatible with forks not
derived from itself.")
    (home-page "https://github.com/edo9300/ygopro")
    (license license:agpl3+)))

(define irrlicht-for-edopro
  (let ((commit "8864940ac07c69c87afafb4b44d2af2013867bd3")
        (revision "7"))
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
         (file-name (git-file-name "irrlicht-for-edopro" version))
         (sha256
          (base32
           "1pi0al08n8sl46v7pby6ydak0yrhr78x0hh1k73a9ykpz7p0b257"))
         (modules '((guix build utils)))
         (snippet #~(begin
                      (with-fluids ((%default-port-encoding #f))
                        (substitute* (find-files "source" "\\.(cpp|h)")
                          (("\"(w?glx?ext).h\"" all lib)
                           (string-append "<GL/" lib ".h>"))))
                      (delete-file "source/Irrlicht/glext.h")
                      (delete-file "source/Irrlicht/glxext.h")
                      (delete-file "source/Irrlicht/wglext.h")))))
      (arguments
       (substitute-keyword-arguments (package-arguments irrlicht)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'disable-dynamic-load
                (lambda* (#:key inputs #:allow-other-keys)
                  (with-fluids ((%default-port-encoding #f))
                    (substitute* (find-files "source" "\\.(cpp|h)")
                      (("defined\\(_IRR_X11_DYNAMIC_LOAD_\\)") "0")
                      (("#ifdef _IRR_X11_DYNAMIC_LOAD_") "#if 0"))
                    (substitute* "source/Irrlicht/CGLXManager.cpp"
                      (("LibGLX?") "nullptr")
                      (("dlsym\\(.*\\)") "nullptr")))))))))
      (inputs (modify-inputs (package-inputs irrlicht)
                (prepend wayland libxkbcommon))))))

(define edopro-assets
  (let ((version "40.0.0")
        (commit "712e4e3c205318699f0d4872cec3abd0be13b4ba"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/ProjectIgnis/Distribution")
            (commit commit)))
      (file-name (git-file-name "edopro-assets" version))
      (sha256
       (base32 "0plz4hb0gzng5jrn67nbyyjf7wlfcx31iywy51rxsg5hxr6ih5kw"))
      (modules '((guix build utils) (ice-9 ftw)))
      (snippet
       #~(begin
           (for-each (lambda (dir)
                       (unless (member dir '("." ".." "config" "textures"))
                         (delete-file-recursively dir)))
                     (scandir ".")))))))

(define-public edopro
  (package
    (name "edopro")
    (version "40.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edo9300/edopro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qgabk4as30b89lxakc0v7q4gjv2cdil81qx42h3ar0kz6mwgfnj"))
       (patches
        (search-patches
         "edopro-utf8-source.patch"
         "edopro-respect-YGOPRO_-_PATH.patch"
         "edopro-respect-XDG-environment-variables.patch"
         "edopro-drop-repo-manager.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "premake5.lua"
             (("include \"freetype\"") ""))
           (delete-file-recursively "sfAudio")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no `check' target
      #:make-flags #~(list "CC=gcc" "--directory=build")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'set-freetype2-includepath
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((dir (search-input-directory inputs "include/freetype2"))
                    (prepend (lambda (path dir)
                               (setenv path (string-append dir ":" (getenv path))))))
                (prepend "C_INCLUDE_PATH" dir)
                (prepend "CPLUS_INCLUDE_PATH" dir))))
          (add-after 'unpack 'patch-sources
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (font (search-input-file
                            inputs
                            "/share/fonts/opentype/NotoSansCJKjp-Regular.otf"))
                     (datadir (string-append out "/share/ygopro")))
                (rename-file "gframe/lzma/premake4.lua"
                             "gframe/lzma/premake5.lua")
                (substitute* (find-files "." ".*\\.(cpp|h)")
                  (("ocgapi\\.h" all)
                   (string-append "ygopro-core/" all)))
                (substitute* "gframe/premake5.lua"
                  (("(/usr/include/freetype2|../freetype/include)")
                   (dirname
                    (search-input-file inputs "include/freetype2/ft2build.h")))
                  (("/usr/include/irrlicht")
                   (dirname
                    (search-input-file inputs "include/irrlicht/irrlicht.h")))
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
                   (string-append "numfont = " font "\n"))))))
          (add-after 'unpack 'unpack-assets
            (lambda* (#:key inputs #:allow-other-keys)
              (install-file (search-input-file inputs "config/strings.conf")
                            "config")
              (copy-recursively (search-input-directory inputs "config/languages")
                                "config/languages")
              (copy-recursively (search-input-directory inputs "textures")
                                "textures")))
          (delete 'bootstrap)
          (replace 'configure
            (lambda* (#:key configure-flags inputs #:allow-other-keys)
              (invoke "premake5" "gmake"
                      "--environment-paths" "--xdg-environment"
                      "--sound=sdl-mixer"
                      (string-append
                       "--prebuilt-core="
                       (dirname
                        (search-input-file inputs "lib/libocgcore.so"))))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bindir (string-append out "/bin"))
                     (datadir (string-append out "/share/ygopro")))
                (copy-recursively "config" (string-append out "/etc/ygopro"))
                (copy-recursively "textures"
                                  (string-append datadir "/textures"))
                (for-each (lambda (bin) (install-file bin bindir))
                          (find-files "bin" "ygopro"))))))))
    (native-inputs (list unzip))
    (inputs (list curl
                  edopro-assets
                  edopro-core
                  flac
                  font-google-noto
                  freetype
                  fmt-7
                  glu
                  irrlicht-for-edopro
                  libevent
                  json-modern-cxx
                  libgit2
                  libvorbis
                  lua
                  mesa
                  mpg123
                  premake5
                  (sdl-union (list sdl2 sdl2-mixer))
                  sqlite))
    (native-search-paths (package-native-search-paths ygopro))
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
   (version "20220912")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ProjectIgnis/BabelCDB.git")
           (commit version)))
     (file-name (git-file-name "ignis-database" version))
     (sha256
      (base32
       "126sr1x84zrhkzzh582a88mgkikv4cr6fjmi01bm529594x75jcs"))))
   (build-system copy-build-system)
   (outputs '("out" "goat" "pre-release" "rush" "skills" "unofficial"))
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
                #:include ("cards.cdb")
                #:include-regexp (".*-release\\.cdb"
                                  "(^|/)release.*\\.cdb"
                                  "fix(Misc|OT|Setcode|String)\\.cdb"))))
            (install-with-output
             "rush" `(("cards-rush.cdb" "share/ygopro/data/")))
            (install-with-output
             "skills" `(("cards-skills.cdb" "share/ygopro/data/")))
            (install-with-output
             "goat" `(("goat-entries.cdb" "share/ygopro/data/")))
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
               '("OCG" "0TCG" "Traditional" "World"))

              (merge-lflists "goat" '("GOAT"))

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
           (commit "ae9307aef8116911ba6e72686ac9eb1a58849303")))
         (sha256
          (base32
           "1n3iwcmgkjjk9i6kzjc2xwp6wfhgqs5vrcy90ppbfjbaaglnqwqb"))))
      ("sqlite" ,sqlite)))
   (synopsis "Card databases for EDOPro")
   (description "Provides various card databases for EDOPro.")
   (home-page "https://github.com/ProjectIgnis/BabelCDB")
   (license #f)))

(define-public ignis-database-nightly
  (let ((database-night "20230123")
        (database-commit "b1ce8a65e2c47e5344d86718b1bbcbd70c49742f")
        (database-hash "14lwd8h87qgj4ippyy1krl7570y980rq98g90r2azjpncws3d3k7")
        (lflists-commit "52841a4126a4d9cfbde00eac893f65e2d3fa1b2c")
        (lflists-hash "0vrrhb4052dvz7m2yf8g80dmzpys0hc8hi811wp4xr2fir5l98vq"))
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
         ,@(package-inputs ignis-database-baseline))))))

(define-public ignis-scripts-baseline
  (package
    (name "ignis-scripts")
    (version "20220912")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProjectIgnis/CardScripts.git")
             (commit version)))
       (file-name (git-file-name "ignis-scripts" version))
       (sha256
        (base32
         "1crvs2k34v4wnkc1z4l82gl59lsq0zhpf1wxhn30lcjq40f681y3"))))
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
  (let ((scripts-night "20230124")
        (scripts-commit "11b828b868f92182bc4502907b4dc5b44f89865f")
        (scripts-hash "0xjf39chshawk0h7m3h3si8yslwkq0f6ln5mnf0f0gn6p86zwskv"))
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
