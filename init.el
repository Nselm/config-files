(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(desktop-save-mode)
(defun my/repeat (count)
  (interactive "p")
  (print (ring-ref evil-repeat-ring 0))
  (print (equal (ring-ref evil-repeat-ring 0) '([120])))
  (dotimes (i 10)
    (when (not (equal (ring-ref evil-repeat-ring i) '([120])))
      (evil-execute-repeat-info-with-count count
					   (ring-ref evil-repeat-ring i))
      (return))))

(use-package magit)
(server-start)

(defmacro show-if-interactive (code)
  "If function is called interactively, print result, then return, else return "
  (let ((symbol (gensym)))
    `(let ((,symbol ,code))
       (if (called-interactively-p 'any)
	   (print ,symbol)
	 ,symbol))))

(defun minor-mode-keymaps ()
  "Show only the names in minor-mode-map-alist"
  (interactive)
  (show-if-interactive (mapcar #'car minor-mode-map-alist)))

(defun find-pos (mode)
  "Find position of a mode in minor-mode-map-alist"
  (interactive "S")
  (show-if-interactive (position mode (minor-mode-keymaps))))

(defun find-precedence (mode-one mode-two)
  (interactive "S\nS")
  (let* ((all-modes (minor-mode-keymaps))
	 (one-pos (position mode-one all-modes))
	 (two-pos (position mode-two all-modes)))
    (print one-pos)
    (print two-pos)
    (if (> two-pos one-pos)
	(print (format "%S has higher precedence" mode-one))
      (print (format "%S has higher precedence") mode-two))))

;(use-package slime)
(defun test ()
  (interactive)
  (print (format "%S" '(hi there))))

;(use-package geiser-racket
;  :config (use-package quack)
;  (add-hook 'scheme-mode-hook 'geiser-mode)
;  (add-hook 'geiser-repl-mode-hook 'paredit-mode)
;  (setq geiser-debug-jump-to-debug-p nil)
;  )

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'paredit-mode)
  (add-hook 'racket-repl-mode-hook 'paredit-mode))
(use-package which-key)
(require 'slime-helper (expand-file-name "~/quicklisp/slime-helper.el"))

(defun require-own (name file)
  (require name (concat emacs-directory "/" file)))

(use-package crux)
					;(use-package company
					;  :config (add-hook 'after-init-hook 'company-tng-mode))

(setq inferior-lisp-program "sbcl") 
(setq python-shell-interpreter "/usr/bin/python3")

(setq tab-always-indent 'complete)
(setq emacs-config-file "~/.emacs.d/init.el")
(setq emacs-directory "~/.emacs.d")
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)



(use-package s)

(defun org-mode-block ()
  (interactive)
  (insert "#+begin_src\n\n#+end_src")
  (move-end-of-line -1)
  (evil-insert 1)
  (insert " "))

(defun search-markers ()
  (interactive)
  (re-search-forward "^\\([Qq]uestion\\|[Rr]emark\\|[Pp]roblem\\):?$"))

(defun remarks-format ()
  (interactive)
  (let ((delim "-+-+-+-+-+-+-+-+-+-"))
    (save-excursion
      (while (re-search-forward (concat "^" (regexp-opt (list delim)) "$"))
	(crux-kill-whole-line)))

    (save-excursion
      (beginning-of-buffer)
      (while (search-markers)
	(evil-open-above 1)
	(insert delim)
	(when
	    (re-search-forward "^$")
	  (insert delim)
	  (evil-open-below 1))))))






(defun org/opened-buffer-files ()
  (delq nil
	(mapcar (lambda (x)
		  (if (and (buffer-file-name x)
			   (string-match "\\.org$"
					 (buffer-file-name x)))
		      (buffer-file-name x)))
		(buffer-list))))



(use-package org-bullets
  :config
  (require-own 'choose-bullets "options.el")
  ;;(setq org-bullets-bullet-list (choose-bullet-elements 'circles))

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;;(setq org-bullets-face-name 'org-bullet-face))

;; Somebody should explain this snippet to me
(font-lock-add-keywords 'org-mode
			'(("^+\\([*]+\\) "
			   (0 (prog1 ()
				(compose-region (match-beginning 1)
						(math-end 1) "•"))))))

(setq org-directory (concat emacs-directory "/org.d"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("l" "Lisp Snippet" entry (file+headline "~/.emacs.d/org.d/notes.org" "Lisp")
	 "* Lisp %? \n#+begin_src lisp \n %i \n#+end_src\n")
	("s" "Code Snippet\n" entry
	 (file+headline "~/.emacs.d/org.d/notes.org" "Code Snippets")
	 "* Snippet \n#+begin_src %?\n %i \n#+end_src\n")
	("c" "Comment on text" entry
	 (file+headline "~/.emacs.d/org.d/notes.org" "Commentary")
	 "* %^{Enter Headline}: \n %^{Enter Comment} \n  %a")))

(define-minor-mode my/org-extension-mode
  "A few extensions to org-mode"
  :global t
  :lighter " org-ext"
  :keymap (let ((map (make-sparse-keymap)))
	    ;; Org-mode key bindings
	    (define-key map (kbd "C-c C-s") 'org-mode-block)
	    (define-key map (kbd "C-c l") #'org-store-link)
	    (define-key map (kbd "C-c a") #'org-agenda)
	    (define-key map (kbd "C-c c") #'org-capture)
	    (define-key map (kbd "C-c C-b") #'org-switchb)
	    map)

  (setq org-refile-targets '((org/opened-buffer-files :maxlevel . 9)))
  (setq my-org-mode-keywords
	'(("^[Pp]roblem:?$" . 'my/problem)
	  ("^[Qq]uestion:?$" . 'my/question)
	  ("^[Rr]emark:?$" . 'my/remark)))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (font-lock-add-keywords nil my-org-mode-keywords)))


  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
			       (lisp . t)
			       (c . t)
			       (shell . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-hide-emphasis-markers t))
(add-hook 'org-mode-hook
	  'my/org-extension-mode)


(define-minor-mode my/helm-mode
  "A minor mode to keep helm keybindings cleanly separated"
  :global t
  :lighter " my/helm"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-y") 'helm-show-kill-ring)
	    (define-key map (kbd "M-x") 'helm-M-x)
	    (define-key map (kbd "C-x b") 'helm-mini)
	    (define-key map (kbd "C-x C-f") 'helm-find-files)
	    (define-key map (kbd "C-x /") 'helm-find)
	    (define-key map (kbd "C-c h o") 'helm-occur)
	    (define-key map (kbd "C-c h a") 'helm-apropos)
	    (define-key map (kbd "C-c h r") 'helm-info-emacs)
	    (define-key map (kbd "C-c h p") 'helm-info-at-point)
	    (define-key map (kbd "C-c C-i") 'helm-info-elisp)
	    (define-key map (kbd "C-c h c") 'helm-lisp-completion-at-point)
	    (define-key map (kbd "C-c h g") 'helm-resume)
	    (define-key map (kbd "C-c h x") 'helm-regexp)
	    (define-key map (kbd "M-c") 'helm-comint-input-ring)
	    (define-key map (kbd "C-M-c") 'helm-comint-prompts)
	  

	    map)
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t
	helm-echo-input-in-header-line t)
  (setq completion-styles '(flex))
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t))

(define-abbrev-table 'html-mode-abbrev-table
  '(("htm" "<html>\n<head>\n<link rel=\"stylesheet\" src=____>\n<title></title>\n</head>\n<body>\n</body>\n</html>")))
(set-default 'abbrev-mode t)
;; Need to find out what this does exactly
(setq save-abbrevs nil)

(defun add-mode-hooks (mode hooks)
  (mapcar #'(lambda (hook)
	      (add-hook hook mode))
	  hook))

(use-package paredit
  :ensure t
  :config
  ;; Use the var paredit-commands to see all commands
  (global-set-key (kbd "M-k") 'paredit-splice-sexp-killing-backward)
  (global-set-key (kbd "M-S-k") 'paredit-splice-sexp-killing-forward)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'slime-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))


(setq use-package-always-ensure t)

(defun load-themes (theme-to-use)
  (use-package zenburn-theme
    :config
    (setq zenburn-use-variable-pitch t
	  zenburn-scale-org-headlines t))
  (use-package solarized-theme)
  (use-package color-theme-sanityinc-tomorrow)
  (use-package color-theme-sanityinc-solarized)
  (use-package leuven-theme)
  (load-theme theme-to-use t))


(load-themes 'zenburn)

(use-package rust-mode)

(use-package undo-tree
  :config
  (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode))

(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  (add-hook 'evil-mode-hook 'my/helm-mode)
  (add-hook 'paredit-mode-hook (lambda ()
				 (define-key evil-normal-state-map
				   (kbd "x") 'paredit-forward-delete)))

  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "+") 'enlarge-window)
  (define-key evil-normal-state-map (kbd "-") 'shrink-window)
  (define-key evil-normal-state-map (kbd "C--") 'shrink-window-horizontally)
  (define-key evil-normal-state-map (kbd "C-+") 'enlarge-window-horizontally)
  ;; Doesnt really work yet
  ;(define-key evil-normal-state-map (kbd ".") 'my/repeat)

  (define-key evil-insert-state-map (kbd "C-k") 'paredit-kill)
  ;; Ex-mode configurations
  (define-key evil-ex-map "b " 'helm-mini)


  (use-package helm
    :config
    (require 'helm-config)
    (helm-mode 1))
  (semantic-mode 1)
  (setq helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match    t)

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; Helm configurations
  (define-key helm-map (kbd "C-h") 'helm-next-source)
  (define-key helm-map (kbd "C-S-h") 'describe-key)
  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (define-key helm-map [escape] 'helm-keyboard-quit)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
    (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
    (define-key keymap (kbd "C-S-h") 'describe-key)))


(setq
 evil-search-module 'evil-search
 evil-want-minibuffer t
 evil-want-fine-undo t)

(define-key evil-motion-state-map (kbd "<SPC>") 'ignore)

(define-key evil-normal-state-map (kbd "C-S-j") 
  (lambda ()
    (interactive)
    (scroll-other-window 2)))

(define-key evil-normal-state-map (kbd "C-S-k") 
  (lambda ()
    (interactive)
    (scroll-other-window-down 2)))

(evil-set-initial-state 'Info-mode 'emacs)

(defun edit-atlas-source ()
  (interactive
   (find-file "/home/whysoserious/nyxt/src/nyxt/source")))
(defun edit-nyxt-config-file ()
  (interactive)
  (find-file "/home/whysoserious/.config/nyxt/init.lisp"))
(use-package evil-leader 
  :config 
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "." 'slime-edit-definition
    "a" 'edit-atlas-source
    "b" 'switch-to-buffer
    "c" 'close-window
    "e" 'edit-dotemacs
    "n" 'edit-nyxt-config-file
    "r" 'comint-history-isearch-backward-regexp
    "s" 'slime-selector
    "y" 'clipboard-yank
    "q" 'close-docs
    "w" 'other-window
    "d" 'delete-window
    "l" 'split-window-right
    "j" 'split-window-below
    "t" 'tjump
    "T" 'Tjump
    "f" 'fjump
    "F" 'Fjump
    ))





(defmacro wrap-new-buffer (fn)
  `(lambda ()
     (interactive)
     (split-window-below)
     (call-interactively ,fn)))

(defun my-helps ()
  "Provide additional help keybindings"
  (global-set-key (kbd "C-h C-l") (wrap-new-buffer #'find-library))
  (global-set-key (kbd "C-h C-f") (wrap-new-buffer #'find-function))
  (global-set-key (kbd "C-h C-v") (wrap-new-buffer #'find-variable))
  (global-set-key (kbd "C-h C-k") (wrap-new-buffer #'find-function-on-key))
  (global-set-key (kbd "C-h C-p") (wrap-new-buffer #'find-function-at-point))
  (global-set-key (kbd "C-h C-S-p") (wrap-new-buffer #'find-variable-at-point)))
(my-helps)


(defun change-on-name (name-list fn)
  "Apply fn to window that holds buffer of name"
  (let ((help-window nil)
	(windows (window-list)))
    (dotimes (i (length (window-list)))
      (when (member (buffer-name) name-list)
	(setf help-window (nth i windows)))
      (other-window 1))
    (if help-window
	(funcall fn help-window)
      (print (format "No %s window" name-list)))))

(defvar *annoying-buffers*
  '("*Help*" "*Warnings*"))

(defun close-docs ()
  (interactive)
  (change-on-name *annoying-buffers* #'quit-restore-window))

(defun edit-dotemacs ()
  (interactive)
  (find-file emacs-config-file))

(setq info-binding-list
      '(("j" . evil-next-line)
	("k" . evil-previous-line)
	("l" . evil-forward-char)
	("h" . evil-backward-char)
	("/" . evil-search-forward)))

(defun info-bindings ()
  "Add a few evil bindings to info mode"
  (mapcar #'(lambda (binding) (definfo (car binding) (cdr binding))) info-binding-list))

(defun definfo (key fn)
  (define-key Info-mode-map key fn))

(info-bindings)


					;(require 'package)
					;(add-to-list 'package-archives
					;             '("melpa" . "https://melpa.org/packages/") t)
					;(package-initialize)
					;(package-refresh-contents)

(defun close-window (num)
  (interactive "p")
  (if (<= num (length (window-list)))
      (delete-window (nth (1- num) (window-list)))
    (print (format "Invalid window num: %d. There are only %d windows" num (length (window-list))))))

(defun jump-cursor (jump-fn count)
  (funcall jump-fn count (char-after)))

(defun tjump (count)
  (interactive "p")
  ;; (insert (char-after))
  (jump-cursor #'evil-find-char-to count))

(defun fjump (count)
  (interactive "p")
  (jump-cursor #'evil-find-char count))

(defun Fjump (count)
  (interactive "p")
  (jump-cursor #'evil-find-char-backward count))

(defun Tjump (count)
  (interactive "p")
  (jump-cursor #'evil-find-char-to-backward count))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "9e3ea605c15dc6eb88c5ff33a82aed6a4d4e2b1126b251197ba55d6b86c610a1" default))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#efe5da4aafb2" "#cfc5e1add08c" "#fe53c9e7b34f" "#dbb6d3c3dcf4" "#e183dee1b053" "#f944cc6dae48" "#d360dac5e06a"))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#b3c34d" . 20)
     ("#6ccec0" . 30)
     ("#74adf5" . 50)
     ("#e1af4b" . 60)
     ("#fb7640" . 70)
     ("#ff699e" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(lsp-ui-doc-border "#586e75")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(racket-mode which-key quack paredit geiser-racket company org-bullets evil-collection evil-magit dash xwwp-follow-link-helm ivy helm-config helm color-theme-sanityinc-solarized leuven-theme color-theme-sanityinc-tomorrow solarized-theme zenburn-theme undo-tree magit tuareg org-roam elpher use-package evil-leader crux org rust-mode cl-lib sml-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:inherit sans-serif))))
 '(my-comments ((t (:background "darkseagreen2" :foreground "red" :underline t))))
 '(my/problem ((t (:foreground "dark red" :weight bold))))
 '(my/question ((t (:foreground "dark cyan" :slant italic))))
 '(my/remark ((t (:foreground "goldenrod"))))
 '(quack-pltish-comment-face ((t (:foreground "#5F7F5F"))))
 '(quack-pltish-selfeval-face ((t (:foreground "#CC9393")))))




(require-own 'medusa-mode "medusa.el")
(require-own 'init-windows "utils.el")
;;(init-windows)

;; Org configuration
(setq org-agenda-files
      '("~.emacs.d/org.d/tasks.org"))

(defun local-set-minor-mode-key (mode key def)
  "Overrides a minor mode keybinding for the local
   buffer, by creating or altering keymaps stored in buffer-local
   `minor-mode-overriding-map-alist'."
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist) 
                       map))))
    (define-key newmap key def)))

(defun actions ()
  "helm interface to my actions, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :sources `(((name . "Important files")
		    (candidates . (("emacs-notes" . "~/.emacs.d/org.d/emacs.org")
				   ("org-notes" . "~/.emacs.d/org.d/org.org")
				   ("todo-list" . "~/.emacs.d/org.d/todo.org")))
		    (action . (("Open" . (lambda (x) (find-file x)))))))))
