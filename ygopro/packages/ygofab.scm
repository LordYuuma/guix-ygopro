(define-module (ygopro packages ygofab)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system font)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages sqlite))

(define (make-lua-i18n name lua)
  (package
    (name name)
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kikito/i18n.lua")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0sm9v1nsyn8z3kb0z45if7ig7bqvkry1y0gi70rl5gqqv62w8xi2"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("i18n"
          ,(string-append "share/lua/"
                          ,(version-major+minor (package-version lua))
                          "/i18n")))))
    (inputs `(("lua" ,lua-5.1)))
    (home-page "https://github.com/jonstoler/lua-toml")
    (synopsis "Internationalization library for Lua")
    (description "i18n is an internationalization library that handles
hierarchies of tags, accepts entries in several ways (one by one, in a table
or in a file) and implements a lot of pluralization rules, fallbacks,
and more.")
    (license license:expat)))

(define-public lua5.1-i18n (make-lua-i18n "lua5.1-i18n" lua-5.1))

(define-public (make-lua-path name lua)
  (package
    (name name)
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moteus/lua-path.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1v65rsakwi1n64v3ylpsyml9dg8kac8c1glnlng0ccrqnlw6jzka"))))
    (build-system cmake-build-system)
    (inputs
     `(("lua" ,lua)))
    (arguments
     `(#:tests? #f ; not interested in building luacov atm
       #:configure-flags
       (list (string-append "-DINSTALL_LMOD="
                            (assoc-ref %outputs "out")
                            "/share/lua/"
                            ,(version-major+minor (package-version lua))))))
    (home-page "https://github.com/moteus/lua-path")
    (synopsis "Path building for Lua")
    (description
     "lua-path brings a set of utility functions to deal with filenames in Lua.")
    (license license:expat)))

(define lua5.1-path (make-lua-path "lua5.1-path" lua-5.1))

(define (make-lua-sqlite name lua)
  (package
    (name name)
    (version "0.9.5")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri "http://lua.sqlite.org/index.cgi/zip/lsqlite3_fsl09y.zip")
       (sha256
        (base32
         "08mpcr26k59ny4qm902sl8jv8cbddmpc6ihj1hk5s1a0fsm0516k"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("lsqlite3.so"
          ,(string-append "lib/lua/"
                          ,(version-major+minor (package-version lua))
                          "/"))
         ("doc/" "share/doc/lua-sqlite")
         ("examples/" "share/doc/lua-sqlite/examples"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "lsqlite3_fsl09y")
             (invoke "ls")
             #t))
         (add-before 'install 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "gcc"
                     "-DLSQLITE_VERSION=\"0.9.5\""
                     "-fPIC"
                     "-lsqlite3" "-llua"
                     "-shared"
                     "lsqlite3.c"
                     "-o"
                     "lsqlite3.so"))))))
    (inputs
     `(("lua" ,lua)
       ("sqlite" ,sqlite)))
    (synopsis "Lua bindings to sqlite")
    (description
     "LuaSQLite3 provides a means to manipulate SQLite3 databases directly from
Lua.")
    (home-page "http://lua.sqlite.org")
    (license license:expat)))

(define-public lua5.1-sqlite (make-lua-sqlite "lua5.1-sqlite" lua-5.1))

(define (make-lua-toml name lua)
  (package
    (name name)
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jonstoler/lua-toml")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0lklhgs4n7gbgva5frs39240da1y4nwlx6yxaj3ix6r5lp9sh07b"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("toml.lua"
          ,(string-append "share/lua/"
                          ,(version-major+minor (package-version lua))
                          "/toml.lua")))))
    (inputs `(("lua" ,lua-5.1)))
    (home-page "https://github.com/jonstoler/lua-toml")
    (synopsis "Lua interface for TOML")
    (description "TOML is a minimalist configuration file format, that aims
to be easy to read and unambiguously map onto hash tables.  lua-toml provides
an interface to read/write TOML files.")
    (license license:expat)))

(define-public lua5.1-toml (make-lua-toml "lua5.1-toml" lua-5.1))

(define (make-lua-zlib name lua)
  (package
    (name "lua-zlib")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brimworks/lua-zlib.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1cv12s5c5lihmf3hb0rz05qf13yihy1bjpb7448v8mkiss6y1s5c"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DUSE_LUA_VERSION="
                            ,(version-major+minor (package-version lua)))
             (string-append "-DINSTALL_CMOD="
                            (assoc-ref %outputs "out")
                            "/lib/lua/"
                            ,(version-major+minor (package-version lua))))))
    (inputs
     `(("lua" ,lua)
       ("zlib" ,zlib)))
    (home-page "https://github.com/brimworks/lua-zlib")
    (synopsis "Lua bindings to zlib")
    (description #f)
    (license license:expat)))

(define lua5.1-zlib (make-lua-zlib "lua5.1-zlib" lua))

(define (make-lua-struct name lua)
  (package
    (name name)
    (version "0.3")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "http://www.inf.puc-rio.br/~roberto/struct/struct-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1xh70x9qj3gsc84jjb3ngwv2g4sdjyli2935h2qzhz7qi6hxss8n"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("struct.so"
          ,(string-append "lib/lua/"
                          ,(version-major+minor (package-version lua))
                          "/")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "make"
                     (string-append "LUADIR=" (assoc-ref inputs "lua")
                                    "/include"))))
         (add-after 'build 'check
           (lambda _
             (invoke "lua" "teststruct.lua"))))))
    (inputs
     `(("lua" ,lua)))
    (home-page "http://www.inf.puc-rio.br/~roberto/struct/")
    (synopsis "Working with C structs in Lua")
    (description
     "lua-struct provides basic facilities to convert Lua values to and from
C structs.  Its main functions are struct.pack, which packs multiple Lua values
into a struct-like string; and struct.unpack, which unpacks multiple Lua values
from a given struct-like string.")
    (license license:expat)))

(define lua5.1-struct (make-lua-struct "lua-struct" lua-5.1))

(define (make-lua-zipwriter name lua lua-zlib)
  (package
    (name "lua-zipwriter")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moteus/ZipWriter.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0rja2hpsf6iczhidywmvsrbgrmdqdx4nb7zszdlfpk07fxcpvizw"))))
    (build-system cmake-build-system)
    (inputs
     `(("lua" ,lua)))
    (propagated-inputs
     `(("lua-zlib" ,lua-zlib)))
    (arguments
     `(#:tests? #f ; not interested in building luacov atm
       #:configure-flags
       (list (string-append "-DINSTALL_LMOD="
                            (assoc-ref %outputs "out")
                            "/share/lua/"
                            ,(version-major+minor (package-version lua))))))
    (home-page "https://github.com/moteus/lua-zipwriter")
    (synopsis "Library for creating ZIP archives")
    (description
     "ZipWriter is a wrapper around lua-zlib, that makes archiving as easy
as writing to streams.")
    (license license:expat)))

(define lua5.1-zipwriter (make-lua-zipwriter "lua5.1-zipwriter" lua-5.1
                                             lua5.1-zlib))

(define (make-lua-vips name lua luajit)
  (let ((revision "9")
        (commit "7c0b5e3780f4f894b295de81cf09499d5e026372"))
   (package
    (name name)
    (version (git-version "1.1" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libvips/lua-vips.git")
             (commit commit)))
       (sha256
        (base32
         "18fqnnyxghcfgh5zs9a8k85cxfgqjqrr0skiw9qiaj8g8brjgh9n"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("src"
          ,(string-append "share/lua/"
                          ,(version-major+minor (package-version lua)))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* (find-files "src" ".*\\.lua")
              (("ffi.load\\(.*vips\"\\)")
               (format #f "ffi.load(\"~a/lib/libvips.so\")"
                       (assoc-ref inputs "vips")))))))))
    (inputs
     `(("lua" ,lua)
       ("luajit" ,luajit)
       ("vips" ,vips)))
    (home-page "https://github.com/libvips/lua-vips")
    (synopsis "Lua bindings for vips")
    (description "lua-vips is a Lua binding for the vips image processing
library using FFI and luajit.")
    (license license:expat))))

(define lua5.1-vips (make-lua-vips "lua5.1-vips" lua-5.1 luajit))

(define font-cinzel
  (let ((commit "dd598495b0fb2ad84270d5cc75d642d2f1e8eabf")
        (revision "0"))
   (package
    (name "font-cinzel")
    (version (git-version "1.001" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NDISCOVER/Cinzel")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0s39byijgcwpcph6gsgp1kx9afw7bih9lwwra80xs72yxv2x4ysp"))))
    (build-system font-build-system)
    (home-page "https://www.ndiscover.com")
    (synopsis "SmallCaps font inspired by Roman inscriptions")
    (description
     "Cinzel is a typeface inspired in first century roman inscriptions, and
based on classical proportions.  While it conveys all the ancient history of
the latin alphabet, it also merges a contemporary feel onto it.")
    (license license:silofl1.1))))

(define (make-ygo-fabrica font-packages font-replacements)
  (package
    (name "ygo-fabrica")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/piface314/ygo-fabrica.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1aj3slmhbq4gip8lqyj256vy5v91l6w96v0d0y7dpq0xcnxz51dw"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("bin" "bin")
         ("etc" "etc")
         ("lib"
          ,(string-append
            "share/lua/5.1/ygofab/lib"))
         ("scripts"
          ,(string-append
            "share/lua/5.1/ygofab/scripts"))
         ("res" "share/ygofab"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "scripts/config.lua"
               (("HOME") "INSTALL_HOME")
               (("local INSTALL_HOME = .*$")
                (format #f "local INSTALL_HOME = \"~a/etc/ygofab\"
local XDG_CONFIG_HOME = os.getenv(\"XDG_CONFIG_HOME\") or (os.getenv(\"HOME\")\
 .. \"/.config/\")"
                        (assoc-ref outputs "out")))
               (("global_cfg = load_file" all)
                (string-append all
                               "(XDG_CONFIG_HOME .. \"/ygofab/config.toml\")"
                               "or load_file")))
             (substitute* (find-files "scripts" ".*\\.lua")
               (("lsqlite3complete") "lsqlite3")
               (("prjoin\\(\"res\"") "prjoin(\"share\", \"ygofab\""))
             (substitute* "scripts/composer/type-writer.lua"
               (("justify = j,") ""))
             (substitute* "scripts/composer/layouts.lua"
               (("Fonts.path = .*") "")
               (("Fonts.get_family\\(\"([a-z_]+)\", \"([0-9.]+)\")" all idx size)
                (let* ((replacement
                        (or (assoc-ref (quote ,font-replacements) idx)
                            '()))
                       (size (* (or (assoc-ref replacement 'scale) 1)
                                (string->number size))))
                 (format #f "Fonts.get_family(\"~a\", \"~1$\")" idx size))))
             (substitute* "scripts/composer/fonts.lua"
               (("path = .*,") "")
               (("Fonts.path .. \"/\" ..") "")
               (("([a-z_]*) = \\{ file = \"(.*)\", family = \"(.*)\" \\}"
                 all idx file family)
                (let ((replacement (assoc-ref (quote ,font-replacements) idx))
                      (font (lambda (input file)
                              (string-append (assoc-ref inputs input)
                                             "/share/fonts/"
                                             file))))
                  (if replacement
                      (format #f "~a = { file = \"~a\", family = \"~a\" }"
                              idx (apply font (assoc-ref replacement 'file))
                              (or (assoc-ref replacement 'family) family))
                      (format #f "~a = { file = \"~a/~a/~a\", family = \"~a\"}"
                              idx (assoc-ref outputs "out")
                              "share/ygofab/composer/fonts" file
                              family)))))
             #t))
         (add-after 'patch-sources 'write-config
           (lambda _
             (mkdir-p "etc/ygofab")
             (with-output-to-file "etc/ygofab/config.toml"
               (lambda _
                 (format #t
                  "# Global configurations for YGOFabrica~&
                   # Define one or more game directories~&
                   [gamedir.main]~&
                   path = '''%s'''~&
                   default = true~&
                   # Define one or more picsets~&
                   [picset.regular]~&
                   mode = 'proxy'~&
                   size = '256x'~&
                   ext = 'jpg'~&
                   field = true~&
                   default = true~%")))))
         (add-after 'patch-sources 'wrap-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (define* (package-path pkg #:optional (version? #t))
               (string-append (assoc-ref inputs pkg) "/share/lua/5.1/?.lua"))
             (define (package-cpath pkg)
               (string-append (assoc-ref inputs pkg) "/lib/lua/5.1/?.so"))
             (mkdir-p "bin")
             (for-each
              (lambda (f)
                (with-output-to-file (string-append "bin/" f)
                  (lambda _
                    (format #t "#!~a~%" (which "bash"))
                    (format #t "export LUA_PATH=\"~a\"~%"
                            (string-join
                             (list
                              (string-append (assoc-ref outputs "out")
                                             "/share/lua/5.1/ygofab/?.lua")
                              (package-path "lua-path")
                              (package-path "lua-toml")
                              (package-path "lua-vips")
                              (package-path "lua-zipwriter"))
                             ";"))
                    (format #t "export LUA_CPATH=\"~a\"~%"
                            (string-join (list
                                          (package-cpath "lua-filesystem")
                                          (package-cpath "lua-sqlite")
                                          (package-cpath "lua-struct")
                                          (package-cpath "lua-zlib"))
                                         ";"))
                    (format #t "export YGOFAB_ROOT=\"~a\"~%"
                            (string-append (assoc-ref outputs "out")))
                    (format #t "exec ~a ~a/~a.lua \"$@\"~%"
                            (which "luajit") (assoc-ref outputs "out")
                            (string-append "share/lua/5.1/ygofab/scripts/" f))))
                (chmod (string-append "bin/" f) #o755))
              '("ygofab" "ygopic"))
             #t)))))
    (inputs
     `(,@font-packages
       ("lua" ,lua-5.1)
       ("luajit" ,luajit)
       ("lua-filesystem" ,lua5.1-filesystem)
       ("lua-path" ,lua5.1-path)
       ("lua-sqlite" ,lua5.1-sqlite)
       ("lua-struct" ,lua5.1-struct)
       ("lua-toml" ,lua5.1-toml)
       ("lua-vips" ,lua5.1-vips)
       ("lua-zipwriter" ,lua5.1-zipwriter)
       ("lua-zlib" ,lua5.1-zlib)))
    (home-page "https://github.com/piface314/ygo-fabrica")
    (synopsis "Project manager for EDOPro extensions")
    (description "YGOFabrica is an open source command line tool written in Lua,
using LuaJIT, to help Yu-Gi-Oh! card makers for the EDOPro game.  It helps the
process of managing extension packs: adding cards to the extension database,
generating card pics, etc.")
    (license license:expat)))

(define-public
  ygo-fabrica
  (make-ygo-fabrica
   `(("font-cinzel" ,font-cinzel)
     ("font-dejavu" ,font-dejavu))
   '(("card_name"
      (file "font-cinzel" "opentype/Cinzel-Bold.otf")
      (family . "Cinzel Bold")
      (scale . 0.9))
     ("monster_desc"
      (file "font-cinzel" "opentype/Cinzel-Black.otf")
      (family . "Cinzel Black")
      (scale . 0.9))
     ("values"
      (file "font-cinzel" "opentype/Cinzel-SemiBold.otf")
      (family . "Cinzel SemiBold")
      (scale . 0.85))
     ("signature"
      (file "font-dejavu" "truetype/DejaVuSerif.ttf")
      (family . "DejaVu Serif"))
     ("effect"
      (file "font-dejavu" "truetype/DejaVuSerifCondensed.ttf")
      (family . "DejaVu Serif"))
     ("flavor_text"
      (file "font-dejavu" "truetype/DejaVuSerifCondensed-Italic.ttf")
      (family . "DejaVu Serif")))))
