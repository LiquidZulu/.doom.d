;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun footnote-template ()
  (if (and
       (string=
        (substring
         (thing-at-point 'line t)
         0
         4)
        "[fn:")
       (string=
        (substring
         (thing-at-point 'line t)
         -3
         nil)
        "]\n")
       )
      (lambda () (interactive)
        (insert "[[][]] ([[][archived]])")
        (dotimes
            (i 19)
          (backward-char)))))

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

        (setq doom-theme 'doom-copper)

(custom-set-faces!
 '(rainbow-delimiters-depth-1-face :foreground "#FF5F5C")
 '(rainbow-delimiters-depth-2-face :foreground "#FFF1C7")
 '(rainbow-delimiters-depth-3-face :foreground "#5E807F")
 '(rainbow-delimiters-depth-4-face :foreground "#33FFEB")
 '(rainbow-delimiters-depth-5-face :foreground "#FF5D38")
 '(rainbow-delimiters-depth-6-face :foreground "#FFC72E")
 '(rainbow-delimiters-depth-7-face :foreground "#75FFD6")
 '(rainbow-delimiters-depth-8-face :foreground "#2996F5")
 '(rainbow-delimiters-depth-9-face :foreground "#FFFB7A")
 )

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
(setq zulu-scroll-amount 5)

(map!
 "C-l"          #'beginning-of-line
 "C-u"          #'end-of-line
 "C-n"          #'backward-char
 "C-e"          #'forward-char

 "M-l"          #'previous-line
 "M-u"          #'next-line
 "M-n"          #'backward-word
 "M-e"          #'forward-word

 "M-m"          #'(lambda () (interactive) (dotimes (i zulu-scroll-amount) (scroll-up-line)))
 "M-j"          #'(lambda () (interactive) (dotimes (i zulu-scroll-amount) (scroll-down-line)))

 "C-M-s-l"      #'(lambda () (interactive) (previous-line) (beginning-of-line))
 "C-M-s-u"      #'(lambda () (interactive) (next-line)     (end-of-line))
 "C-M-s-n"      #'backward-paragraph
 "C-M-s-e"      #'forward-paragraph

 "C-;"          #'(lambda () (interactive) (footnote-template) (org-footnote-action))

 "C-M-s-d"      #'centaur-tabs-backward
 "C-M-s-v"      #'centaur-tabs-forward
 "C-M-s-t"      #'centaur-tabs-select-beg-tab
 "C-M-s-g"      #'centaur-tabs-select-end-tab
 "C-M-s-k"      #'centaur-tabs--kill-this-buffer-dont-ask

 "C-x t t"      #'treemacs

 "C-c i i"      #'(lambda () (interactive) (insert "#+CAPTION:\n#+NAME:\n[[./images]]") (backward-char) (backward-char) "Insert image")  ; "insert image"
 "C-C i t r"    #'org-table-create-or-convert-from-region
 "C-C i t e"    #'org-table-create-with-table.el

 "C-M-s-x r i"      #'org-toggle-inline-images  ; "render image"
 "C-M-s-x p p j a"  #'json-pretty-print-buffer-ordered
 "C-M-s-x p p j r"  #'json-pretty-print-ordered

 "C-M-s-<backspace>" #'(lambda () (interactive) (beginning-of-line) (org-delete-backward-char 1) (org-self-insert-command))

 "C-M-s-b" #'ibuffer

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

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq
 org-css "file:///e:/emacs/documents/org-css/css/org.css")
(setq
 org-preamble (format
               "#+TITLE:\n#+AUTHOR:LiquidZulu\n#+BIBLIOGRAPHY:e:/Zotero/library.bib\n#+PANDOC_OPTIONS: csl:e:/Zotero/styles/australasian-physical-and-engineering-sciences-in-medicine.csl\n#+HTML_HEAD:<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\"/>\n#+OPTIONS: ^:{}\n#+begin_comment\n/This file is best viewed in [[https://www.gnu.org/software/emacs/][emacs]]!/\n#+end_comment"
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

(org-babel-do-load-languages
      'org-babel-load-languages
      '(
        (C . t)
        (js . t)))

(setq my-credentials-file "~/.private.el")

(defun freenode-password (server)
  (with-temp-buffer
    (insert-file-contents-literally my-credentials-file)
    (plist-get (read (buffer-string)) :freenode-password)))

(setq circe-network-options
      '(("Freenode" :host "chat.freenode.net" :port (6667 . 6697)
         :tls t
         :nick "LiquidZulu"
         :sasl-username "LiquidZulu"
         :sasl-password freenode-password
         :channels (
                    "#philosophy"
                    "#idleRPG"
                    "#physics"
                    "#science"
                    "#emacs"
                    "#"
                    )

         )))

(setq circe-use-cycle-completion t)

(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose
`circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-server-network)
            (throw 'return t))))))

(defun circe-maybe-connect (network)
  "Connect to NETWORK, but ask user for confirmation if it's
already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe-maybe-connect "Freenode"))

(setq circe-format-say "<{nick}> {body}")

(setq circe-format-self-say "<{nick}> {body}")

(setq circe-format-message "<{nick}> → <{chattarget}>: {body}")

(setq circe-format-self-message "<{nick}> → <{chattarget}>: {body}")

(setq circe-format-action "* <{nick}> {body}")

(setq circe-format-self-action "* <{nick}> {body}")

(setq circe-format-message-action "* <{nick}> {body}")

(setq circe-chat-buffer-name "{target}")

(setq circe-server-buffer-name "{host}:{port}")

(setq circe-format-notice "-{nick}- {body}")

(setq circe-format-server-notice "--SERVER-- {body}")

(setq circe-format-server-topic "*** Topic change by {nick} ({userhost}): {old-topic} → {new-topic} | {topic-diff}")

(setq circe-format-server-lurker-activity "*** First activity: {nick} joined {joindelta} ago ({jointime}).")

(setq circe-format-server-join "*** Join: {nick} ({userinfo})")

(setq circe-format-server-join-in-channel "*** Join: {nick} ({userinfo}) joined {channel}")

(setq circe-format-server-rejoin "*** Re-join: {nick} ({userinfo}), left {departuredelta} ago ({departuretime}).")

(setq circe-format-server-whois-idle-with-signon "*** {whois-nick} is {idle-duration} idle (signon on {signon-date}, {signon-ago} ago)")

(setq circe-format-server-whois-idle "*** {whois-nick} is {idle-duration} idle")

(setq circe-format-server-topic-time "*** Topic set by {setter} on {topic-date}, {topic-ago} ago")

(setq circe-format-server-topic-time-for-channel "*** Topic for {channel} set by {setter} on {topic-date}, {topic-ago} ago")

(setq circe-format-server-channel-creation-time "*** Channel {channel} created on {date}, {ago} ago")

(setq circe-format-server-ctcp "*** CTCP {command} request from {nick} ({userhost}) to {target}: {body}")

(setq circe-format-server-ctcp-ping "*** CTCP PING request from {nick} ({userhost}) to {target}: {body} ({ago} ago)")

(setq circe-format-server-ctcp-ping-reply "*** CTCP PING reply from {nick} ({userhost}) to {target}: {ago} ago ({body})")

(setq circe-format-server-netsplit "*** Netsplit: {split} (Use /WL to see who left)")

(setq circe-format-server-netmerge "*** Netmerge: {split}, split {ago} ago (Use /WL to see who's still missing)")

(setq circe-format-server-mode-change "*** Mode change: {change} on {target} by {setter} ({userhost})")

(setq circe-format-server-nick-change-self "*** Nick change: You are now known as {new-nick}")

(setq circe-format-server-nick-change "*** Nick change: {old-nick} ({userhost}) is now known as {new-nick}")

(setq circe-format-server-nick-regain "*** Nick regain: {old-nick} ({userhost}) is now known as {new-nick}")

(setq circe-format-server-part "*** Part: {nick} ({userhost}) left {channel}: {reason}")

(setq circe-format-server-quit-channel "*** Quit: {nick} ({userhost}) left {channel}: {reason}")

(setq circe-format-server-quit "*** Quit: {nick} ({userhost}) left IRC: {reason}")

(setq
 lui-time-stamp-position 'right-margin
 lui-time-stamp-format "%H:%M")

(add-hook 'lui-mode-hook 'my-circe-set-margin)
(defun my-circe-set-margin ()
  (setq right-margin-width 5))

(setq
 lui-time-stamp-position 'right-margin
 lui-fill-type nil)

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "    "))

(setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)

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

(defun zulu-open-directory-looper (list dir)
  "A helper function for zulu-open-directory containing the loop logic"
  (while list
    (dired (concat dir "/" (car list)))
    (setq list (cdr list))))
(defun zulu-open-directory (dir)
"Opens all level-1 subfolders in (dir) as dired buffers."
(zulu-open-directory-looper (directory-files dir) dir))

(irc)

(zulu-open-directory "e:/emacs/documents/youtube-scripts/scripts")
(zulu-open-directory "e:/emacs/documents/notes/org")
(zulu-open-directory "e:/emacs/documents/agenda")

(find-file "~/.doom.d/README.org")

(setq fancy-splash-image (expand-file-name "misc/splash-images/blackhole-lines.png" doom-private-dir))

(switch-to-buffer "*doom*")
