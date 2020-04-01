(define-module (ygopro packages mycard)
  #:use-module (guix packages)
  #:use-module (guix download)
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

(define %ygopro-version "1.035.0-5")

(define-public ygopro-core
  (package
    (name "ygopro-core")
    (version %ygopro-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mycard/ygopro-core.git")
             (commit "caa7d62afc1052d441b647a1f1d187a38777bbfa")))
       (file-name (git-file-name "ygopro-core" %ygopro-version))
       (sha256
        (base32
         "15ydvlh2h6ha3xw21pl36gcn770zkh9v9yp2h1wg0m42h5jrxdq1"))
       (modules '((guix build utils)
                  (ice-9 textual-ports)))
       (snippet
        '(begin
           (with-output-to-file "-premake4.lua"
             (lambda ()
               (display "solution \"ygopro-core\"\n")
               (display "   location \"build\"\n")
               (display "   language \"C++\"\n")
               (display "   objdir \"obj\"\n")
               (display "   configurations { \"Debug\", \"Release\" }\n\n")
               (call-with-input-file "premake4.lua"
                 (lambda (in)
                   (let loop ((line (get-line in)))
                     (if (eof-object? line)
                         #t
                         (begin
                           (display "   ")
                           (display line)
                           (newline)
                           (loop (get-line in)))))))))
           (rename-file "-premake4.lua" "premake4.lua")
           (substitute* "premake4.lua"
             (("StaticLib")
              "SharedLib"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no `check' target
       #:make-flags `("CC=gcc" "--directory=build")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "interpreter.h"
               (("#include \"l([a-z]+).h\"" all lua-cont)
                (string-append "extern \"C\" {\n"
                               "#include <l" lua-cont ".h>\n"
                               "}")))
             #t))
         (delete 'bootstrap)
         (replace 'configure (lambda _ (invoke "premake4" "gmake")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include/ygopro-core")))
               (for-each (lambda (f) (install-file f inc))
                         (find-files "." ".*\\.h"))
               (install-file "libocgcore.so" lib)
               #t))))))
    (inputs
     `(("lua" ,lua)))
    (native-inputs
     `(("premake4" ,premake4)))
    (synopsis "Yu-Gi-Oh! script engine")
    (description
     "YGOPro-Core is a lua-based engine for Yu-Gi-Oh! with automatic ruling.")
    (home-page "https://github.com/mycard/ygopro")
    (license license:gpl2)))

(define-public ygopro
  (package
    (name "ygopro")
    (version %ygopro-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mycard/ygopro.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17qx87rqz3kqzcsw6j51fd0r9ls4iai9x4zbj04vhqwzkdg5akhz"))
       (modules '((guix build utils)))
       (patches
        (search-patches
         "ygopro-respect-YGOPRO_-_PATH.patch"
         "ygopro-respect-XDG-environment-variables.patch"))
       (snippet
        '(begin
           ;; Drop bundled lua
           (delete-file-recursively "lua")
           ;; fix premake4.lua
           (substitute* "premake4.lua"
             ((", \"LinkTimeOptimization\"")
              "")
             (("include \"ocgcore\"")
              "")
             (("include \"lua\"")
              ""))
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
                           "/share/fonts/truetype/NotoSans-Regular.ttf"))
                    (sysconfdir (string-append out "/etc/ygopro"))
                    (datadir (string-append out "/share/ygopro")))
               (substitute* "gframe/premake4.lua"
                 (("/usr/include/freetype2")
                  (string-append (assoc-ref inputs "freetype")
                                 "/include/freetype2"))
                 (("/usr/include/irrlicht")
                  (string-append (assoc-ref inputs "irrlicht")
                                 "/include/irrlicht")))
               (substitute* (find-files "gframe/" ".*\\.(h|cpp)")
                 (("\"\\.\\./ocgcore/([a-z_]+\\.h)\"" all header)
                  (string-append "<ygopro-core/" header ">")))
               (substitute* "gframe/game.h"
                 (("/etc/ygopro" all)
                  (string-append (assoc-ref outputs "out") all))
                 (("/usr(/share/ygopro)" all rest)
                  (string-append (assoc-ref outputs "out") rest)))
               (substitute* "system.conf"
                 (("textfont = .*$")
                  (string-append "textfont = " font " 12\n"))
                 (("numfont = .*$")
                  (string-append "numfont = " font "\n")))
               #t)))
         (delete 'bootstrap)
         (replace 'configure
           (lambda _
            (invoke "premake4" "--xdg-environment" "--environment-paths" "gmake")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin"))
                    (sysconfdir (string-append out "/etc/ygopro"))
                    (datadir (string-append out "/share/ygopro")))
               (install-file "lflist.conf" (string-append datadir "/data"))
               (install-file "system.conf" sysconfdir)
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
       ("ygopro-core" ,ygopro-core)))
    (native-inputs
     `(("premake4" ,premake4)))
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
    (synopsis "Yu-Gi-Oh! card game simulator")
    (description
     "YGOPro is a sample GUI for the YGPro-Core card game engine.")
    (home-page "https://github.com/mycard/ygopro")
    (license license:gpl2)))

(define-public ygopro-scripts
  (let ((commit "894fb6c7c0647ed0924b3e2006bc4a737a8376ca")
        (revision "0"))
    (package
      (name "ygopro-scripts")
      (version (git-version %ygopro-version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mycard/ygopro-scripts.git")
               (commit commit)))
         (file-name (git-file-name "ygopro-scripts" version))
         (sha256
          (base32
           "0l1fxc1arg6c9a44iwvxqn9mm335acsw9al8jgr6a5l13iyipnqi"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("." "share/ygopro/script/"
            #:include-regexp (".*\\.lua")))))
      (synopsis "Script files for YGOPro")
      (description "ygopro-scripts provides scripts needed by YGOPro.")
      (home-page "https://github.com/mycard/ygopro")
      (license license:gpl2))))

(define-public ygopro-database-en
  (let ((commit "852465b0900fd9b26ceee446c04e6f81b75ffe62")
        (revision "0"))
    (package
      (name "ygopro-database-en")
      (version (git-version %ygopro-version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mycard/ygopro-database.git")
               (commit commit)))
         (file-name (git-file-name "ygopro-database" version))
         (sha256
          (base32
           "1aanvb00n0h9wqbc40khwlxdhbvrnz1vwcg3iq2sgvaycd08lyc5"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("locales/en-US" "share/ygopro/data"))))
      (synopsis "English database for YGOPro")
      (description "ygopro-database-en provides an english card database
and other localization data needed by YGOPro.")
      (home-page "https://github.com/mycard/ygopro")
      (license #f))))

(define-public ygopro-database-ja
  (package
    (inherit ygopro-database-en)
    (name "ygopro-database-ja")
    (arguments
     `(#:install-plan
       `(("locales/ja-JP" "share/ygopro/data"))))
    (synopsis "Japanese database for YGOPro")
    (description "ygopro-database-en provides a japanese card database
and other localization data needed by YGOPro.")))

(define-public ygopro-database-zh
  (package
    (inherit ygopro-database-en)
    (name "ygopro-database-zh")
    (arguments
     `(#:install-plan
       `(("locales/zh-CN" "share/ygopro/data"))))
    (synopsis "Chinese database for YGOPro")
    (description "ygopro-database-en provides a chinese card database
and other localization data needed by YGOPro.")))

(define-public ygopro-images-field
  (let ((commit "8f042ddf7d9e214061a033f9602671d6806ff516")
        (revision "0"))
    (package
      (name "ygopro-images-field")
      (version (git-version %ygopro-version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mycard/ygopro-images-field.git")
               (commit commit)))
         (file-name (git-file-name "ygopro-images-field" version))
         (sha256
          (base32
           "15hpa8v13r5cva0dcvgm7l369zig9avvryq898aq67igwi516rg9"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("field" "share/ygopro/pics/"))))
      (synopsis "Field images for YGOPro")
      (description "ygopro-images-field provides large card images for
YGOPro, which are displayed across the board when a field spell is in play.")
      (home-page "https://github.com/mycard/ygopro")
      (license #f))))

(define-public (ygopro-images locale hash)
  (package
    (name (string-append "ygopro-images-" (string-take locale 2)))
    (version (package-version ygopro))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://cdn01.moecube.com/images/ygopro-images-"
                           locale ".zip"))
       (sha256 (base32 hash))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("." "share/ygopro/pics"))))
    (synopsis "Images for YGOPro")
    (description "Card and field images for YGOPro, snarfed directly from
Mycard's CI.")
    (home-page "https://github.com/mycard/ygopro")
    (license #f)))

(define-public ygopro-images-en
  (ygopro-images "en-US" "0f12zrslipdghvz4vg62a12mm2n6sx435r0zcww3n4lvx81h4mi1"))

(define-public ygopro-images-ja
  (ygopro-images "ja-JP" "12vbhbjq8khnzg2c85vi2l6lf5bqj6cy39qn8wkm0nwk23z4wikz"))

(define-public ygopro-images-zh
  (ygopro-images "zh-CN" "16yqwma1myf817j4qdwla5vdvan4vx1mzlcc3kc9bcngsw1yri6c"))
