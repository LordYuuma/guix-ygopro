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

(define %ygopro-version "1.035.1-3")

(define-public ygopro-core
  (package
    (name "ygopro-core")
    (version %ygopro-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mycard/ygopro-core.git")
             (commit "032b7b22e8b3021f0d67afbec7efb587556630e4")))
       (file-name (git-file-name "ygopro-core" %ygopro-version))
       (sha256
        (base32
         "1dzph0imww0mrdfz26h0q8f85w30i2s8pjxz4kwpp8vqp3ayjrf5"))
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
        (base32 "13pky5dbiq21ga5j1m5kp4l4fn6vrnfbya3ppf34wmc8vjvrj9mv"))
       (modules '((guix build utils)))
       (patches
        (search-patches
         "ygopro-respect-YGOPRO_-_PATH.patch"
         "ygopro-respect-XDG-environment-variables.patch"))
       (snippet
        '(begin
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
                           "/share/fonts/opentype/NotoSansCJKjp-Regular.otf"))
                    (sysconfdir (string-append out "/etc/ygopro"))
                    (datadir (string-append out "/share/ygopro")))
               (substitute* "gframe/premake4.lua"
                 (("/usr/include/freetype2")
                  (string-append (assoc-ref inputs "freetype")
                                 "/include/freetype2"))
                 (("/usr/include/irrlicht")
                  (string-append (assoc-ref inputs "irrlicht")
                                 "/include/irrlicht"))
                 (("lua5.3-c\\+\\+") "lua"))
               (substitute* (find-files "gframe/" ".*\\.(h|cpp)")
                 (("\"\\.\\./ocgcore/([a-z_]+\\.h)\"" all header)
                  (string-append "<ygopro-core/" header ">")))
               (substitute* "gframe/game.cpp"
                 (("error.log") "/dev/stderr"))
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
  (let ((commit "f0a30e61e84259d3b7704a3f3e4560a47a1a5cd8")
        (revision "1"))
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
           "1vnz2zw4j8fra45y8gn01lmvmms7y07fjw81csjwg1710gfw5pba"))))
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
  (let ((commit "0e13f008dbc2cf62b78fa0e6fb782380016480c3")
        (revision "1"))
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
           "1q30aqfjyh6m17pa71kmn455sv93kw43bg0pkcrr3pjq66qwnhm6"))))
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

;; these hashes change all the time, and also the download links can break too
;; I'd recommend using ygopro-images directly if you're writing a manifest
;; or finding alternative sources
(define-public ygopro-images-en
  (ygopro-images "en-US" "1pn7nk15nlk62zbic1rw64bwza7qsyg6f69wy60f86vvn8r98hsc"))

(define-public ygopro-images-ja
  (ygopro-images "ja-JP" "0ha518if5616cbnggcs4gxibrq03bnd3j4lj6qf58z0y1d6qks8s"))

(define-public ygopro-images-zh
  (ygopro-images "zh-CN" "0fakrcg11ri55k3qjl9wylf7lp8sk5qsx4rbkvvfbc1b524li904"))
