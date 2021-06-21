;;; themes/doom-copper-theme.el -*- lexical-binding: t; -*-
(require 'doom-themes)

;;
(defgroup doom-copper-theme nil
  "Options for the `doom-copper' theme."
  :group 'doom-themes)

(defcustom doom-copper-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-copper-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-copper
  "A customised version of doom-laserwave, made for copper, the 'Circe opper'"

  ;; name        default   256       16
  ((bg         '("#3a1e16" nil       nil            ))
   (bg-alt     '("#331c15" nil       nil            ))
   (base0      '("#3a1e16" "black"   "black"        ))
   (base1      '("#30130a" "#222222" "brightblack"  ))
   (base2      '("#273133" "#222233" "brightblack"  ))
   (base3      '("#354948" "#333344" "brightblack"  ))
   (base4      '("#5b3322" "#444455" "brightblack"  ))
   (base5      '("#544863" "#554466" "brightblack"  ))
   (base6      '("#66421c" "#EE66BB" "brightblack"  ))
   (base7      '("#a05c20" "#998899" "brightblack"  ))
   (base8      '("#ECEFF4" "#EEEEFF" "white"        ))
   (fg-alt     '("#EEEEEE" "#EEEEEE" "brightwhite"  ))
   (fg         '("#FFFFFF" "#FFFFFF" "white"        ))

   (grey       base4)
   (red        '("#964C7B" "#964477" "red"              ))
   (orange     '("#FFB85B" "#FFBB55" "brightred"        ))
   (green      '("#74DFC4" "#77DDCC" "green"            ))
   (teal       '("#4D8079" "#448877" "brightgreen"      ))
   (yellow     '("#FFE261" "#FFEE66" "yellow"           ))
   (blue       '("#40B4C4" "#44BBCC" "brightblue"       ))
   (dark-blue  '("#336A79" "#336677" "blue"             ))
   (magenta    '("#EB64B9" "#EE66BB" "brightmagenta"    ))
   (violet     '("#B381C5" "#BB88CC" "magenta"          ))
   (cyan       '("#B4DCE7" "#BBDDEE" "brightcyan"       ))
   (dark-cyan  '("#6D7E8A" "#667788" "cyan"             ))
   (oxidised-0 '("#304347" "#304347" "cyan"             ))
   (oxidised-1 '("#005651" "#005651" "cyan"             ))
   (oxidised-2 '("#008070" "#008070" "cyan"             ))

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-cyan)
   (builtin        cyan)
   (comments       base7)
   (doc-comments   (doom-lighten bg-alt 0.25))
   (constants      orange)
   (functions      cyan)
   (keywords       (doom-darken orange 0.25))
   (methods        cyan)
   (operators      orange)
   (type           yellow)
   (strings        oxidised-2)
   (variables      fg)
   (numbers        orange)
   (region         `(,(doom-blend (car bg) (car oxidised-2) 0.8) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-copper-padded-modeline
      (if (integerp doom-copper-padded-modeline) doom-copper-padded-modeline 4)))

   (modeline-fg     orange)
   (modeline-fg-alt base6)
   (modeline-bg base6)
   (modeline-bg-inactive (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  ((lazy-highlight :background (doom-darken oxidised-2 0.4) :foreground fg)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground cyan)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground bg-alt)
   (mode-line-highlight :background orange :weight 'bold)
   ;;;; centaur-tabs
   (centaur-tabs-active-bar-face :background oxidised-2)
   (centaur-tabs-modified-marker-selected
    :inherit 'centaur-tabs-selected :foreground oxidised-1)
   (centaur-tabs-modified-marker-unselected
    :inherit 'centaur-tabs-unselected :foreground cyan)
   ;;;; company
   (company-box-background :foreground fg :background bg-alt)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background base6)
   (doom-modeline-info :inherit 'mode-line-emphasis)
   (doom-modeline-urgent :inherit 'mode-line-emphasis)
   (doom-modeline-warning :inherit 'mode-line-emphasis)
   (doom-modeline-debug :inherit 'mode-line-emphasis)
   (doom-modeline-buffer-minor-mode :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-project-dir :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-project-parent-dir :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-persp-name :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-file :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-modified :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-lsp-success :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :inherit 'mode-line-emphasis)
   (doom-modeline-evil-visual-state :foreground yellow)
   (doom-modeline-evil-replace-state :foreground orange)
   (doom-modeline-evil-operator-state :foreground teal)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background base2 :distant-foreground nil)
   ;;;; markdown-mode
   (markdown-header-delimiter-face :foreground base7)
   (markdown-metadata-key-face     :foreground base7)
   (markdown-list-face             :foreground base7)
   (markdown-link-face             :foreground cyan)
   (markdown-url-face              :inherit 'link :foreground fg :weight 'normal)
   (markdown-italic-face           :inherit 'italic :foreground yellow)
   (markdown-bold-face             :inherit 'bold :foreground yellow)
   (markdown-markup-face           :foreground base7)
   (markdown-gfm-checkbox-face :foreground cyan)
   ;;;; mic-paren
   (paren-face-match  :foreground yellow   :background (doom-darken bg 0.2) :weight 'ultra-bold)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))
   ;;;; org <built-in>
   ((org-block &override) :background (doom-darken bg 0.1))
   ((org-block-begin-line &override) :background (doom-darken bg 0.1))
   ((org-quote &override) :background (doom-darken bg 0.1))
   (org-hide :foreground hidden)
   ;;;; org-pomodoro
   (org-pomodoro-mode-line :inherit 'mode-line-emphasis :weight 'bold) ; unreadable otherwise
   (org-pomodoro-mode-line-overtime :inherit 'org-pomodoro-mode-line)
   (org-pomodoro-mode-line-break :inherit 'org-pomodoro-mode-line)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))))
