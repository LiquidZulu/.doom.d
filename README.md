
# Table of Contents

1.  [Introduction](#orgcae7782)
    1.  [Modifying this config](#orgb91cca2)
    2.  [Why Doom?](#orgbc40d76)
2.  [Lexical Binding](#orgbef10e9)
3.  [Helper Functions](#org251e46c)
    1.  [:function](#orge263645)
    2.  [:command](#orgf27bcf6)
    3.  [:after](#org4a1ec10)
    4.  [:hook](#orge14250a)
    5.  [:push](#orgf89ff43)
    6.  [:bind](#org7f32136)
4.  [Settings](#org65c9415)
    1.  [email](#org38e310b)
    2.  [org-directory](#org32b7f98)
    3.  [line number style](#orgb5dd6fb)
    4.  [various misc setings](#orgb23c266)
    5.  [Autosaves](#orgd922186)
    6.  [Prettify symbols](#org2baf5cf)
    7.  [Parenthesis Settings](#org382540c)
        1.  [Rainbow Delimiters](#orgc6d9247)
        2.  [Highlight Matching Bracket](#org6e55eea)
5.  [Key Mapping](#orge89dd12)
6.  [Maximising Frame on Windows](#org31bc569)
7.  [Only Show Encoding When Not UTF-8](#orgaf39ffb)
8.  [Add Default Preamble to New Orgmode Files](#org7532920)
9.  [Package Config](#org630de03)
    1.  [Splashcii](#org0bd8d10)
    2.  [org-ref](#org4b32f59)
    3.  [helm-bibtex](#orgbd8aa99)



<a id="orgcae7782"></a>

# Introduction

This is my config for [Doom Emacs](https://github.com/hlissner/doom-emacs)

> &#x2026;a configuration framework for GNU Emacs tailored for Emacs bankruptcy veterans who want less framework in their frameworks, a modicum of stability (and reproducibility) from their package manager, and the performance of a hand rolled config (or better). It can be a foundation for your own config or a resource for Emacs enthusiasts to learn more about our favorite operating system.


<a id="orgb91cca2"></a>

## Modifying this config

`config.org` contains a number of source blocks that may be modified at will, `config.el` will automatically load all elisp source contained in `README.org`.


<a id="orgbc40d76"></a>

## Why Doom?

You will notice from a perusing of `init.el` that I have disabled [Evil Mode](https://www.emacswiki.org/emacs/Evil), so I am clearly not on Doom for the vi bindings. I use Doom for the speed and to avoid bankruptcy &#x2014; vanilla bindings are good enough in most areas though I have modified the movement to make sense on Colemak-DHm.


<a id="orgbef10e9"></a>

# Lexical Binding

Enable Lexical Binding

    ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


<a id="org251e46c"></a>

# Helper Functions

I got these from [Dustin Lacewell&rsquo;s init.el](https://dustinlacewell.github.io/emacs.d/). I dont use most of these but its nice to have them if im going to be taxing things from his init.


<a id="orge263645"></a>

## :function

Make non-interactive functions out of forms or a symbol.

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


<a id="orgf27bcf6"></a>

## :command

Make interactive commands out of forms or a symbol.

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


<a id="org4a1ec10"></a>

## :after

Defer some forms until the given package is loaded.

    (defmacro :after (package &rest body)
      "A simple wrapper around `with-eval-after-load'."
      (declare (indent defun))
      `(with-eval-after-load ',package ,@body))


<a id="orge14250a"></a>

## :hook

Register some forms or a symbol with a hook.

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


<a id="orgf89ff43"></a>

## :push

A wrapper around add-to-list.

    (defmacro :push (sym &rest body)
      (declare (indent defun))
      (if (consp body)
          `(setq ,sym (-snoc ,sym ,@body))
        `(add-to-list ,sym ,body)))


<a id="org7f32136"></a>

## :bind

Bind some forms or a symbol to a key. (I use map! instead)

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


<a id="org65c9415"></a>

# Settings


<a id="org38e310b"></a>

## email

Email and username, some functionality uses this to identify you, e.g. GPG configuration, email clients, file templates and snippets.

    (setq user-full-name "LiquidZulu"
          user-mail-address "liquidzulu@pm.me")


<a id="org32b7f98"></a>

## org-directory

I don&rsquo;t want all of my orgfiles on my c drive, I like to keep them here

    (setq org-directory "e:/emacs/documents/notes/org")


<a id="orgb5dd6fb"></a>

## line number style

This determines the style of line numbers in effect. If set to `nil`, line numbers are disabled. For relative line numbers, set this to `relative`.

    (setq display-line-numbers-type t)


<a id="orgb23c266"></a>

## various misc setings

Blah

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


<a id="orgd922186"></a>

## Autosaves

    (setq
     auto-save-default t
     auto-save-timeout 20   ; every 20 secs
     auto-save-interval 20) ; or every 20 keystrokes


<a id="org2baf5cf"></a>

## Prettify symbols

    (global-prettify-symbols-mode 1)


<a id="org382540c"></a>

## Parenthesis Settings


<a id="orgc6d9247"></a>

### Rainbow Delimiters

Took this from ldle&rsquo;s init, found [here](https://dustinlacewell.github.io/emacs.d/).

> Colour parenthesis based on their depth, using the golden ratio (because why not).

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


<a id="org6e55eea"></a>

### Highlight Matching Bracket

    (require 'paren)
    (show-paren-mode 1)
    (setq show-paren-delay 0)
    (:after xresources
      (set-face-foreground 'show-paren-match (theme-color 'green))
      (set-face-foreground 'show-paren-mismatch "#f00")
      (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
      (set-face-attribute 'show-paren-mismatch nil :weight 'extra-bold))


<a id="orge89dd12"></a>

# Key Mapping

I use [Colemak-DHm](https://colemakmods.github.io/mod-dh/) &#x2013; my specific layout can be found [here](https://configure.ergodox-ez.com/ergodox-ez/layouts/BO06w/latest/0) &#x2013; so remapping of certain core keys, namely motion keys, is required for ergonomics and ease of use.

    (map!
        "C-l"   #'beginning-of-line
        "C-u"   #'end-of-line
        "C-n"   #'backward-char
        "C-e"   #'forward-char
    
        "M-l"   #'previous-line
        "M-u"   #'next-line
        "M-n"   #'backward-word
        "M-e"   #'forward-word
    
        "C-M-l" #'(lambda () (interactive) (previous-line) (beginning-of-line))
        "C-M-u" #'(lambda () (interactive) (next-line)     (end-of-line))
        "C-M-n" #'backward-paragraph
        "C-M-e" #'forward-paragraph
    
        ;; "C-M-x f a"   #'helm-bibtex         ; "find article" : opens up helm bibtex for search.
        ;; "C-M-x o n"   #'org-noter           ; "org noter"  : opens up org noter in a headline
        ;; "C-M-x r c i" #'org-clock-in        ; "routine clock in" : clock in to a habit.
        ;; "C-M-x c b"   #'beacon-blink        ; "cursor blink" : makes the beacon-blink
        )


<a id="org31bc569"></a>

# Maximising Frame on Windows

Pretty sure this is done by

    (if (eq initial-window-system 'x)       ; if started by emacs command or desktop file
        (toggle-frame-maximized))

on linux but adding this

    (defun maximize-frame ()
      "Maximizes the active frame in Windows"
      (interactive)
      ;; Send a `WM_SYSCOMMAND' message to the active frame with the
      ;; `SC_MAXIMIZE' parameter.
      (when (eq system-type 'windows-nt)
        (w32-send-sys-command 61488)))
    (add-hook 'window-setup-hook 'maximize-frame t)

makes it work on win 10.


<a id="orgaf39ffb"></a>

# Only Show Encoding When Not UTF-8

I basically only use UTF-8 so it takes up space for no reason most of the time.

    (defun doom-modeline-conditional-buffer-encoding ()
      (setq-local doom-modeline-buffer-encoding
                  (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                              (eq buffer-file-coding-system 'utf-8)))))
    (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


<a id="org7532920"></a>

# Add Default Preamble to New Orgmode Files

I dont like to type out all that crap on my own

    (setq
     org-preamble (concatenate
                   "#+TITLE:\n#+AUTHOR:LiquidZulu\n#+BIBLIOGRAPHY:e:/Zotero/library.bib\n#+PANDOC_OPTIONS: csl:e:/Zotero/styles/australasian-physical-and-engineering-sciences-in-medicine.csl\n#+DATE:"
                   (current-time-string)))
    
    (add-hook 'find-file-hook
              (lambda () (
                          (if
                              (=
                               (substring
                                (buffer-name)
                                (if (> (length (buffer-name)) 3) (- (length (buffer-name)) 3) 0)
                                nil)
                                 "org")
                              (if
                                  (=
                                   (buffer-size)
                                   0)
                                  (insert
                                   ))))))


<a id="org630de03"></a>

# Package Config


<a id="org0bd8d10"></a>

## Splashcii

Splashcii is a CLI program that generates random ASCII splashart, you need to download it [here](https://github.com/folke/splashcii) for this to work.

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


<a id="org4b32f59"></a>

## org-ref

org-ref is necessary for writing of any sort of academic material in emacs, makes citations a lot easier.

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


<a id="orgbd8aa99"></a>

## helm-bibtex

Helm BibTeX is a package that allows for searching through BibTeX bibliographies.

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

