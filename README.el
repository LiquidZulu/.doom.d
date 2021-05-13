;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defmacro :function (&rest body)
  (if (->> body length (< 1))
      `(lambda () ,@body)
    (pcase (car body)
      ;; command symbol
      ((and v (pred commandp))
       `(lambda () (call-interactively (quote ,v))))
      ;; function symbol
      ((and v (pred symbolp))
       `(lambda () (,v)))
      ;; quoted command symbol
      ((and v (pred consp) (guard (eq 'quote (car v))) (pred commandp (cadr v)))
       `(lambda () (call-interactively ,v)))
      ;; quoted function symbol
      ((and v (pred consp) (guard (eq 'quote (car v))))
       `(lambda () (,(cadr v))))
      ;; body forms
      (_ `(lambda () ,@body) ))))

(defmacro :command (&rest body)
  (if (->> body length (< 1))
      `(lambda () (interactive) ,@body)
    (pcase (car body)
      ;; command symbol
      ((and v (pred commandp))
       `(lambda () (interactive) (call-interactively (quote ,v))))
      ;; function symbol
      ((and v (pred symbolp))
       `(lambda () (interactive) (,v)))
      ;; quoted command symbol
      ((and v (pred consp) (guard (eq 'quote (car v))) (pred commandp (cadr v)))
       `(lambda () (interactive) (call-interactively ,v)))
      ;; quoted function symbol
      ((and v (pred consp) (guard (eq 'quote (car v))))
       `(lambda () (interactive) (,(cadr v))))
      ;; body forms
      (_ `(lambda () (interactive) ,@body) ))))

(defmacro :after (package &rest body)
  "A simple wrapper around `with-eval-after-load'."
  (declare (indent defun))
  `(with-eval-after-load ',package ,@body))

(defmacro :hook (hook-name &rest body)
  "A simple wrapper around `add-hook'"
  (declare (indent defun))
  (let* ((hook-name (format "%s-hook" (symbol-name hook-name)))
         (hook-sym (intern hook-name))
         (first (car body))
         (local (eq :local first))
         (body (if local (cdr body) body))
         (first (car body))
         (body (if (consp first)
                   (if (eq (car first) 'quote)
                       first
                     `(lambda () ,@body))
                 `',first)))
    `(add-hook ',hook-sym ,body nil ,local)))

(defmacro :push (sym &rest body)
  (declare (indent defun))
  (if (consp body)
      `(setq ,sym (-snoc ,sym ,@body))
    `(add-to-list ,sym ,body)))

(defmacro :bind (key &rest body)
  (declare (indent defun))
  (pcase key
    ;; kbd string resolving symbol
    ((and k (pred symbolp) (pred boundp) (guard (stringp (eval key))))
     `(global-set-key (kbd ,(eval key)) ,(eval `(:command ,@body))))
    ;; partial mode symbol
    ((pred symbolp)
     (let ((mode (intern (format "%s-map" key)))
           (key (eval (car body)))
           (body (eval `(:command ,@(cdr body)))))
       `(define-key ,mode (kbd ,key) ,body)))
    ;; global binding
    (_ `(global-set-key (kbd ,key) ,(eval `(:command ,@body))))))

(setq user-full-name "LiquidZulu"
      user-mail-address "liquidzulu@pm.me")

(setq org-image-actual-width 500)

(setq
 doom-font      (font-spec :family "Mononoki" :size 24)
 doom-big-font  (font-spec :family "Mononoki" :size 36))

(setq org-export-headline-levels 512)

(setq org-directory "e:/emacs/documents/notes/org")

(setq org-startup-with-inline-images t)

(setq org-image-actual-width 500)

(setq display-line-numbers-type t)

(setq delete-by-moving-to-trash t)           ; Delete files to trash
(setq tab-width 4)                            ; Set width for tabs
(setq uniquify-buffer-name-style 'forward)    ; Uniquify buffer names
(setq window-combination-resize t)            ; take new window space from all other windows (not just current)
(setq x-stretch-cursor t)                    ; Stretch cursor to the glyph width

(setq undo-limit 80000000)                    ; Raise undo-limit to 80Mb
(setq evil-want-fine-undo t)                  ; By default while in insert all changes are one big blob. Be more granular
(setq auto-save-default t)                    ; Nobody likes to loose work, I certainly don't
(setq inhibit-compacting-font-caches t)       ; When there are lots of glyphs, keep them in memory
(setq truncate-string-ellipsis "…")          ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                  ; Replace selection when inserting text
(setq line-spacing 0.3)                    ; seems like a nice line spacing balance.

(setq auto-save-default t)
(setq auto-save-timeout 20)   ; every 20 secs
(setq auto-save-interval 20)  ; or every 20 keystrokes

(global-prettify-symbols-mode 1)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq rainbow-delimiters-depth-1-face "FF1D1A")
(setq rainbow-delimiters-depth-2-face "FF243A")
(setq rainbow-delimiters-depth-3-face "FF5D38")
(setq rainbow-delimiters-depth-4-face "FFC72E")
(setq rainbow-delimiters-depth-5-face "FFD724")
(setq rainbow-delimiters-depth-6-face "33FFEB")
(setq rainbow-delimiters-depth-7-face "75FFD6")
(setq rainbow-delimiters-depth-8-face "FFFB7A")
(setq rainbow-delimiters-depth-9-face "FFF1C7")

(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(:after xresources
  (set-face-foreground 'show-paren-match (theme-color 'green))
  (set-face-foreground 'show-paren-mismatch "#f00")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold))

(setq w32-apps-modifier 'hyper)
(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'hyper)

(map!
 "C-l"          #'beginning-of-line
 "C-u"          #'end-of-line
 "C-n"          #'backward-char
 "C-e"          #'forward-char

 "M-l"          #'previous-line
 "M-u"          #'next-line
 "M-n"          #'backward-word
 "M-e"          #'forward-word

 "C-M-s-l"      #'(lambda () (interactive) (previous-line) (beginning-of-line))
 "C-M-s-u"      #'(lambda () (interactive) (next-line)     (end-of-line))
 "C-M-s-n"      #'backward-paragraph
 "C-M-s-e"      #'forward-paragraph

 "C-;"          #'org-footnote-action

 "C-M-s-d"      #'centaur-tabs-backward
 "C-M-s-v"      #'centaur-tabs-forward
 "C-M-s-t"      #'centaur-tabs-select-beg-tab
 "C-M-s-g"      #'centaur-tabs-select-end-tab
 "C-M-s-k"      #'centaur-tabs--kill-this-buffer-dont-ask

 "C-x t t"      #'treemacs

 "C-c i i"      #'(lambda () (interactive) (insert "#+CAPTION:\n#+NAME:\n[[./images]]") (backward-char) (backward-char) "Insert image")  ; "insert image"

 "C-M-s-x r i"      #'org-toggle-inline-images  ; "render image"
 "C-M-s-x p p j a"  #'json-pretty-print-buffer-ordered
 "C-M-s-x p p j r"  #'json-pretty-print-ordered

 "C-M-s-<backspace>" #'(lambda () (interactive) (beginning-of-line) (org-delete-backward-char 1) (org-self-insert-command))

 "M-y" #'yank ; I keep accidently pressing this instead of C-y, and I hate it, it breaks everything

                                        ;"C-RET"    #'(lambda () (interactive) (+org/insert-item-below) (org-return))

 ;; "C-M-x f a"   ;#'helm-bibtex         ; "find article" : opens up helm bibtex for search.
 ;; "C-M-x o n"   ;#'org-noter           ; "org noter"  : opens up org noter in a headline
 ;; "C-M-x r c i" ;#'org-clock-in        ; "routine clock in" : clock in to a habit.
 ;; "C-M-x c b"   ;#'beacon-blink        ; "cursor blink" : makes the beacon-blink
 )

(if (eq initial-window-system 'x)       ; if started by emacs command or desktop file
    (toggle-frame-maximized))

(defun maximize-frame ()
  "Maximizes the active frame in Windows"
  (interactive)
  ;; Send a `WM_SYSCOMMAND' message to the active frame with the
  ;; `SC_MAXIMIZE' parameter.
  (when (eq system-type 'windows-nt)
    (w32-send-sys-command 61488)))
(add-hook 'window-setup-hook 'maximize-frame t)

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq
 org-css "file:///e:/emacs/documents/org-css/css/org.css")
(setq
 org-preamble (format
               "#+TITLE:\n#+AUTHOR:LiquidZulu\n#+BIBLIOGRAPHY:e:/Zotero/library.bib\n#+PANDOC_OPTIONS: csl:e:/Zotero/styles/australasian-physical-and-engineering-sciences-in-medicine.csl\n#+HTML_HEAD:<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\"/>\n/This file is best viewed in [[https://www.gnu.org/software/emacs/][emacs]]!/"
               org-css))

(add-hook 'find-file-hook
          (lambda ()
            (if
                (string=
                 (substring
                  (buffer-name)
                  (if (> (length (buffer-name)) 3) (- (length (buffer-name)) 3) 0)
                  nil)
                 "org")
                (if
                    (=
                     (buffer-size)
                     0)
                    ((lambda ()
                       (insert org-preamble)

                                        ; navigate point to end of #+TITLE:, doesnt work when launching from gitbash for some reason, point just moves right back down after doom does something
                       (goto-line 1)
                       (forward-word)
                       (forward-char)))))))

; ¯\_(ツ)_/¯
; TODO I think the relevant search term for #+FOO: is keyword but cant find any function that edits them nice and simple, if not ill need to search for it manually which will be a massive pain

(setq
 md-preamble
 "---\nslug:\ntitle:\nauthor: Liquidzulu\nauthor_title: Anarcho-Capitalist YouTuber\nauthor_url: https://www.youtube.com/channel/UCTf0py7ryuSldOsDm4abSsg\nauthor_image_url: https://yt3.ggpht.com/ytc/AAUvwngTBrwImrEHOckgvAV4I45tRm4-lPRC-X0KvsAT9w=s176-c-k-c0x00ffffff-no-rj\ntags: []\n---")

(add-hook 'find-file-hook
          (lambda ()
            (if
                (string=
                 (substring
                  (buffer-name)
                  (if (> (length (buffer-name)) 3) (- (length (buffer-name)) 3) 0)
                  nil)
                 "mdx")
                (if
                    (=
                     (buffer-size)
                     0)
                    ((lambda ()
                       (insert md-preamble)

                       (goto-line 2)
                       (forward-word)
                       (forward-char)))))))

;; splashcii
(defvar +fl/splashcii-query ""
  "The query to search on asciiur.com")

(defun +fl/splashcii ()
  (split-string (with-output-to-string
                  (call-process "splashcii" nil standard-output nil +fl/splashcii-query))
                "\n" t))

(defun +fl/doom-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          (+fl/splashcii))
    (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0) ?\n))))

;; override the first doom dashboard function
(setcar (nthcdr 0 +doom-dashboard-functions) #'+fl/doom-banner)

;; (setq +fl/splashcii-query "space")

(require 'ox-json)

                                        ;(add-hook 'org-mode-hook #'org-make-toc-mode) ; automatically update toc

(use-package! org-ref
    :after org
    :init
    ; code to run before loading org-ref
    :config
    ; code to run after loading org-ref
    )
(setq
      ;org-ref-notes-directory (concatenate 'string org-directory "/org-ref")
      org-ref-default-bibliography '("e:/Zotero/library.bib")
      org-ref-pdf-directory "e:/Zotero/pdfs")

(after! org
  (add-to-list 'org-capture-templates
               '(("a"               ; key
                  "Article"         ; name
                  entry             ; type
                  ;(file+headline (concatenate 'string org-directory "/foo.org) "Article")  ; target
                  "\* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"  ; template

                  :prepend t        ; properties
                  :empty-lines 1    ; properties
                  :created t        ; properties
))) )

(use-package! helm-bibtex
  :after org
  :init
  ; blah blah
  :config
  ;blah blah
  )

(setq bibtex-format-citation-functions
      '((org-mode . (lambda (x) (insert (concat
                                         "\\cite{"
                                         (mapconcat 'identity x ",")
                                         "}")) ""))))
(setq
      bibtex-completion-pdf-field "file"
      bibtex-completion-bibliography
      '("e:/Zotero/library.bib")
      bibtex-completion-library-path '("e:/Zotero/")
     ; bibtex-completion-notes-path "~/Dropbox/Org/references/articles.org"  ;; not needed anymore as I take notes in org-roam
      )
