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

(define %ygopro-version "1.035.1-7")

(define-public ygopro-core
  (package
    (name "ygopro-core")
    (version %ygopro-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mycard/ygopro-core.git")
             (commit "b1f9ca9717308afcf7b688650e1e0a304a7691bc")))
       (file-name (git-file-name "ygopro-core" %ygopro-version))
       (sha256
        (base32
         "16nbc7574z8g5c2k6r787dhhbnc0bxpcfr7wqk521j3lxp47gpg0"))
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
        (base32 "18vijzi3iqyh0gl5jhi2ww88drd4bwfqk4hbp5hlgzpsw9fiqczc"))
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
  (let ((commit "80957f86e78cf6d83b9e40a848723f1c94304579")
        (revision "3"))
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
           "05qgc7asgkvdsn9p6jdvsi13d83n89yyngyx527hhspfk9xdapcb"))))
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
  (let ((commit "9b92da731795b3b36bbf066d25cd27abe28b9c79")
        (revision "3"))
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
           "18plfwwzhq7pn9jlya9bpad1qzb9hqpyj897wxwcnij4isfmj5jf"))))
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
  (ygopro-images "en-US" "0qgb856hcy2nvqplxpckif2mfyj4yikhfmhpi7zvz0ny8rv1d6kf"))

(define-public ygopro-images-ja
  (ygopro-images "ja-JP" "0ha518if5616cbnggcs4gxibrq03bnd3j4lj6qf58z0y1d6qks8s"))

(define-public ygopro-images-zh
  (ygopro-images "zh-CN" "0xq0b1zlq3ijy2gplpy5v1fiqcrczlp280j0k59vd82fq9gsks95"))
