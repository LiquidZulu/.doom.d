;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "LiquidZulu"
      user-mail-address "liquidzulu@pm.me")

(setq org-directory "e:/emacs/documents/notes/org")

(setq display-line-numbers-type t)

(setq-default
 delete-by-moving-to-trash t            ; Delete files to trash
 tab-width 4                            ; Set width for tabs
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 window-combination-resize t            ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                    ; Stretch cursor to the glyph width

(setq undo-limit 80000000               ; Raise undo-limit to 80Mb
      evil-want-fine-undo t             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t               ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t  ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")     ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)               ; Replace selection when inserting text
(setq line-spacing 0.3)                 ; seems like a nice line spacing balance.

(map!
    "C-l" #'beginning-of-line
    "C-u" #'end-of-line
    "C-n" #'backward-char
    "C-e" #'forward-char

    "M-l" #'previous-line
    "M-u" #'next-line
    "M-n" #'backward-word
    "M-e" #'forward-word

    ;; "C-M-x f a"   #'helm-bibtex         ; "find article" : opens up helm bibtex for search.
    ;; "C-M-x o n"   #'org-noter           ; "org noter"  : opens up org noter in a headline
    ;; "C-M-x r c i" #'org-clock-in        ; "routine clock in" : clock in to a habit.
    ;; "C-M-x c b"   #'beacon-blink        ; "cursor blink" : makes the beacon-blink
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
