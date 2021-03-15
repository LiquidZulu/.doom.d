
# Table of Contents

1.  [Introduction](#org996eeea)
    1.  [Modifying this config](#orgec51878)
    2.  [Why Doom?](#org01b3818)
2.  [Preamble](#org0abba66)
3.  [Key Mapping](#orga256c03)
4.  [Maximising Frame on Windows](#orga0e1808)
5.  [Only Show Encoding When Not UTF-8](#org449988d)
6.  [Package Config](#org451ebb2)
    1.  [Splashcii](#org2e44782)
    2.  [org-ref](#orgeb80f58)
    3.  [helm-bibtex](#orgb29c022)



<a id="org996eeea"></a>

# Introduction

This is my config for [Doom Emacs](https://github.com/hlissner/doom-emacs)

> &#x2026;a configuration framework for GNU Emacs tailored for Emacs bankruptcy veterans who want less framework in their frameworks, a modicum of stability (and reproducibility) from their package manager, and the performance of a hand rolled config (or better). It can be a foundation for your own config or a resource for Emacs enthusiasts to learn more about our favorite operating system.


<a id="orgec51878"></a>

## Modifying this config

`config.org` contains a number of source blocks that may be modified at will, `config.el` will automatically load all elisp source contained in `README.org`.


<a id="org01b3818"></a>

## Why Doom?

You will notice from a perusing of `init.el` that I have disabled [Evil Mode](https://www.emacswiki.org/emacs/Evil), so I am clearly not on Doom for the vi bindings. I use Doom for the speed and to avoid bankruptcy &#x2014; vanilla bindings are good enough in most areas though I have modified the movement to make sense on Colemak-DHm.


<a id="org0abba66"></a>

# Preamble

Enable Lexical Binding

    ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

Email and username, some functionality uses this to identify you, e.g. GPG configuration, email clients, file templates and snippets.

    (setq user-full-name "LiquidZulu"
          user-mail-address "liquidzulu@pm.me")

I don&rsquo;t want all of my orgfiles on my c drive, I like to keep them here

    (setq org-directory "e:/emacs/documents/notes/org")

This determines the style of line numbers in effect. If set to \`nil&rsquo;, line numbers are disabled. For relative line numbers, set this to \`relative&rsquo;.

    (setq display-line-numbers-type t)

Some settings

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


<a id="orga256c03"></a>

# Key Mapping

I use [Colemak-DHm](https://colemakmods.github.io/mod-dh/) &#x2013; my specific layout can be found [here](https://configure.ergodox-ez.com/ergodox-ez/layouts/BO06w/latest/0) &#x2013; so remapping of certain core keys, namely motion keys, is required for ergonomics and ease of use.

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


<a id="orga0e1808"></a>

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


<a id="org449988d"></a>

# Only Show Encoding When Not UTF-8

I basically only use UTF-8 so it takes up space for no reason most of the time.

    (defun doom-modeline-conditional-buffer-encoding ()
      (setq-local doom-modeline-buffer-encoding
                  (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                              (eq buffer-file-coding-system 'utf-8)))))
    (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)


<a id="org451ebb2"></a>

# Package Config


<a id="org2e44782"></a>

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


<a id="orgeb80f58"></a>

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


<a id="orgb29c022"></a>

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

