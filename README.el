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

(setq org-directory "e:/emacs/documents/notes/org")

(setq display-line-numbers-type t)

(setq-default
    delete-by-moving-to-trash t            ; Delete files to trash
    tab-width 4                            ; Set width for tabs
    uniquify-buffer-name-style 'forward    ; Uniquify buffer names
    window-combination-resize t            ; take new window space from all other windows (not just current)
    x-stretch-cursor t)                    ; Stretch cursor to the glyph width

(setq undo-limit 80000000                  ; Raise undo-limit to 80Mb
    evil-want-fine-undo t                  ; By default while in insert all changes are one big blob. Be more granular
    auto-save-default t                    ; Nobody likes to loose work, I certainly don't
    inhibit-compacting-font-caches t       ; When there are lots of glyphs, keep them in memory
    truncate-string-ellipsis "…")          ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                  ; Replace selection when inserting text
(setq line-spacing 0.3)                    ; seems like a nice line spacing balance.

(setq
 auto-save-default t
 auto-save-timeout 20   ; every 20 secs
 auto-save-interval 20) ; or every 20 keystrokes

(global-prettify-symbols-mode 1)

(require 'color)
(defun gen-col-list (length s v &optional hval)
  (cl-flet ( (random-float () (/ (random 10000000000) 10000000000.0))
          (mod-float (f) (- f (ffloor f))) )
    (unless hval
      (setq hval (random-float)))
    (let ((golden-ratio-conjugate (/ (- (sqrt 5) 1) 2))
          (h hval)
          (current length)
          (ret-list '()))
      (while (> current 0)
        (setq ret-list
              (append ret-list
                      (list (apply 'color-rgb-to-hex (color-hsl-to-rgb h s v)))))
        (setq h (mod-float (+ h golden-ratio-conjugate)))
        (setq current (- current 1)))
      ret-list)))

(defun set-random-rainbow-colors (s l &optional h)
  ;; Output into message buffer in case you get a scheme you REALLY like.
  ;; (message "set-random-rainbow-colors %s" (list s l h))
  (interactive)
  (rainbow-delimiters-mode t)

  ;; Show mismatched braces in bright red.
  (set-face-background 'rainbow-delimiters-unmatched-face "red")

  ;; Rainbow delimiters based on golden ratio
  (let ( (colors (gen-col-list 9 s l h))
         (i 1) )
    (let ( (length (length colors)) )
      ;;(message (concat "i " (number-to-string i) " length " (number-to-string length)))
      (while (<= i length)
        (let ( (rainbow-var-name (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))
               (col (nth i colors)) )
          ;; (message (concat rainbow-var-name " => " col))
          (set-face-foreground (intern rainbow-var-name) col))
        (setq i (+ i 1))))))

(use-package rainbow-delimiters :commands rainbow-delimiters-mode :hook ...
  :init
  (setq rainbow-delimiters-max-face-count 16)
  (set-random-rainbow-colors 0.6 0.7 0.5)
  (:hook prog-mode 'rainbow-delimiters-mode))

(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(:after xresources
  (set-face-foreground 'show-paren-match (theme-color 'green))
  (set-face-foreground 'show-paren-mismatch "#f00")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold))

(map!
    "C-l"   #'beginning-of-line
    "C-u"   #'end-of-line
    "C-n"   #'backward-char
    "C-e"   #'forward-char

    "M-l"   #'previous-line
    "M-u"   #'next-line
    "M-n"   #'backward-word
    "M-e"   #'forward-word

    "M-C-l" #'(lambda () (interactive) (previous-line) (beginning-of-line))
    "C-M-u" #'(lambda () (interactive) (next-line)     (end-of-line))
    "C-M-n" #'backward-paragraph
    "C-M-e" #'forward-paragraph

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

(setq
 org-preamble (format
               "#+TITLE:\n#+AUTHOR:LiquidZulu\n#+BIBLIOGRAPHY:e:/Zotero/library.bib\n#+PANDOC_OPTIONS: csl:e:/Zotero/styles/australasian-physical-and-engineering-sciences-in-medicine.csl\n#+DATE:%s\n/This file is best viewed in [[https://www.gnu.org/software/emacs/][emacs]]!/"
               (format-time-string "%F %Z")))

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
