(define-module (ygopro packages mycard)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ygopro packages)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages sqlite))

(define (ygopro-core version commit hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/mycard/ygopro-core.git")
          (commit commit)))
    (file-name (git-file-name "ygopro-core" version))
    (sha256 (base32 hash))))

(define-public ygopro
  (package
    (name "ygopro")
    (version "1.035.0-2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mycard/ygopro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14iixsacc51v4ayriwdk3qblgxlf0bjyilbsn7gm6pihkd7jvv5l"))
       (modules '((guix build utils)))
       (patches
        (search-patches
         "ygopro-support-YGOPRO_DATA_PATH.patch"
         "ygopro-support-YGOPRO_SCRIPT_PATH.patch"
         "ygopro-support-YGOPRO_IMAGE_PATH.patch"))
       (snippet
        '(begin
           ;; Drop bundled lua
           (delete-file-recursively "lua")
           ;; fix premake4.lua
           (substitute* "premake4.lua"
             ((", \"LinkTimeOptimization\"")
              "")
             (("include \"lua\"")
              "includedirs { \"lua/include\" }"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no `check' target
       #:make-flags `("CC=gcc" "--directory=build")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'combine-and-patch-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (font (string-append
                           (assoc-ref inputs "font-google-noto")
                           "/share/fonts/truetype/NotoSans-Regular.ttf"))
                    (datadir (string-append out "/share/ygopro")))
               (copy-recursively (assoc-ref inputs "ygopro-core") "ocgcore")
               (substitute* "premake4.lua"
                 (("lua")
                  (assoc-ref inputs "lua")))
               (substitute* "gframe/premake4.lua"
                 (("/usr/include/freetype2")
                  (string-append (assoc-ref inputs "freetype")
                                 "/include/freetype2"))
                 (("/usr/include/irrlicht")
                  (string-append (assoc-ref inputs "irrlicht")
                                 "/include/irrlicht")))
               (substitute* "ocgcore/interpreter.h"
                 (("#include \"l([a-z]+).h\"" all lua-cont)
                  (string-append "extern \"C\" {\n"
                                 "#include <l" lua-cont ".h>\n"
                                 "}")))
               (substitute* "gframe/image_manager.cpp"
                 (("textures/") (string-append datadir "/textures/")))
               (substitute* "gframe/game.cpp"
                 (("fopen\\(\"system.conf\", \"r\"\\);" all)
                  (string-append all
                                 "\n\tif(!fp)\n\t\tfp = fopen(\"" datadir
                                 "/system.conf\", \"r\");")))
               (substitute* "system.conf"
                 (("textfont = .*$")
                  (string-append "textfont = " font " 12\n"))
                 (("numfont = .*$")
                  (string-append "numfont = " font "\n")))
               #t)))
         (delete 'bootstrap)
         (replace 'configure
           (lambda _
            (invoke "premake4" "gmake")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin"))
                    (datadir (string-append out "/share/ygopro")))
               (install-file "lflist.conf" (string-append datadir "/data"))
               (install-file "system.conf" datadir)
               (copy-recursively "textures"
                                 (string-append datadir "/textures"))
               (install-file "bin/debug/ygopro" bindir)
               #t))))))
    (inputs
     `(("font-google-noto" ,font-google-noto)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("irrlicht" ,irrlicht)
       ("libevent" ,libevent)
       ("lua" ,lua)
       ("mesa" ,mesa)
       ("sqlite" ,sqlite)
       ("premake" ,premake4)
       ("ygopro-core"
        ,(ygopro-core
          version "70d429495313513963ba4fbe9f892373585d27a2"
          "1pw8715pan2nj9cyr69zm6n7pli7wvf97xxna68khmz6jqaafjwz"))))
    (native-search-paths
     (list (search-path-specification
            (variable "YGOPRO_DATA_PATH")
            (files '("share/ygopro/data")))
           (search-path-specification
            (variable "YGOPRO_SCRIPT_PATH")
            (files '("share/ygopro/script")))
           (search-path-specification
            (variable "YGOPRO_IMAGE_PATH")
            (files '("share/ygopro/pics")))))
    (synopsis "Yu-Gi-Oh! script engine and sample GUI")
    (description
     "YGOPro is a lua-based engine for Yu-Gi-Oh! with full ruling support.")
    (home-page "https://github.com/mycard/ygopro")
    (license license:gpl2)))

(define-public ygopro-scripts
  (let ((commit "12feaa2622e33802667083a90b4a93356a739a5d")
        (revision "0"))
    (package
      (name "ygopro-scripts")
      (version (git-version (package-version ygopro)
                            revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mycard/ygopro-scripts.git")
               (commit commit)))
         (file-name (git-file-name "ygopro-scripts" version))
         (sha256
          (base32
           "1qln87n51kchgglakynhcgbs26q1gkr02m1v73559r2l50cbxsyj"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("." "share/ygopro/script/"))))
      (synopsis "Script files for YGOPro")
      (description "ygopro-scripts provides scripts needed by YGOPro.")
      (home-page "https://github.com/mycard/ygopro")
      (license #f))))

(define-public ygopro-database-en
  (let ((commit "57f19709cabdcc952d652e2c6fcba698fc87a972")
        (revision "0"))
    (package
      (name "ygopro-database-en")
      (version (git-version (package-version ygopro)
                            revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mycard/ygopro-database.git")
               (commit commit)))
         (file-name (git-file-name "ygopro-database" version))
         (sha256
          (base32
           "1yggymmvgq55m9f5x2wfiw94mkcbfskd7599r02hxldphgmnql1c"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("locales/en-US" "share/ygopro/data"))))
      (synopsis "English database for YGOPro")
      (description "ygopro-database-en provides an english card database
and other localization data needed by YGOPro.")
      (home-page "https://github.com/mycard/ygopro")
      (license #f))))
