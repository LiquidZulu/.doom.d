;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;              ```-::::/sssssss/:::``
;;       .-oydmmMMMMMMMMMMMMMMMMMMMMMNmmyy+:`
;;      yMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNs                        -/ohhNNNdhhhyooo+::``
;;      `----++++++oyyyyydMMMMMMMMMMMMMMMMMMMM+                    /yNMMMMMMMMMMMMMMMMMMMMMdhs/-`
;;              .:++syyyyhMMMMMMMMMMMMMMMMMMMMN+                `omMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNo.
;;  `-/oyhhhhhNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMm              /NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNs.
;;-hNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMd.             dMMMMMMMMMMMMMMMMMMMMMMmo++++++yy+`
;;`dMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM+`              dMMMMMMMMMMMMMMMMMMMMMMMmhyyyyoy+-
;; `/oyhNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMo                mMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNd+//-
;;      ```--+sdMMMMMMMMMMMMMMMMMMMMMMMMMMMMmo                 mMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN
;;             `./hMMMMMMMMMMMMMMMMMMMMMNho:`                  /NMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
;;             `/dNMMMMMMMMMMMMMMMMMNho:``                      -yNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMdo-
;;              :ymMMMMMMMMMmddhys+-.`                            .+dMMMMMMMMMMMMMMMMMMMMMMMMMmh+:-`
;;                `.:/sss/-.```                                     `-ohmMMMMMMMMMMMMMMMMNho/-`
;;                                                                      `-sdMMMMMMMMMMMMN+`
;;                                                                         `--/++syyyyy+.
;;
;;
;;
;;
;;
;;                                        `.-----.``          ``
;;                                   `-+yhdNNNNNNNddh+++yyyyyyddyyyo-
;;                                   /MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMN-
;;                                   .hNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMo
;;                                     -oydmNNMMMMMMMMMMMMMMMMMMMMMMy`
;;                                         ..-+smMMMMMMMMMMMMMMMMNh:
;;                                              .+dNMMMMMMMMMMMMM:
;;                                                 ./yddddddddddd:
;;
;;
;;                                  ````````````           ``
;;                           `./ohhhmNNNNNNNNNNdhho/-`-:oyhdmhhs-`
;;                        :shdMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMdys/.`
;;                       `/+sssssssso///////sshmNMMMMMMMNNNmdmNMMMMMMMNh+`
;;                                              `:/oshs/.``` `./NMMMMMMMMNy`
;;                                                              `:+yyyy++ohN+
;;                                         ..:+/---/+ss++++++-`            ``
;;                                         `:hMMMMMMMMMMMNmso/`
;;                                            oNMMMMMNNs/`
;;                                              `-.::-`


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented

(org-babel-load-file
 (expand-file-name
  "README.org"
  "~/.doom.d"))
