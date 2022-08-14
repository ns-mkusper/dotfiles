(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; remove window decoration
(set-frame-parameter nil 'undecorated t)

(setq initial-frame-alist '( (fullscreen . maximized)))
(setq default-frame-alist '( (fullscreen . fullheight)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
					  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(add-to-list 'exec-path "~/.local/bin/")

;; delete trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default save-place t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
    alist))


(defalias 'string-to-int 'string-to-number) ;;workaround for string-to-int problem
(setq backup-directory-alist '(("." . "~/.emacs_backups")))

;; Mac OS needed changes
(if (eq system-type 'darwin)

    ;; fix bad ssl
    (use-package gnutls
      :ensure t
      :config
      (add-to-list 'gnutls-trustfiles
		   (expand-file-name
		    "~/etc/tls/certificates/comodo.rsa.ca.intermediate.crt")))

  ;; get meta key working on Mac
  (setq-default mac-option-modifier 'meta)
  )

(require 'package)

(setq package-archives
      '(("celpa" . "https://celpa.conao3.com/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(require 'use-package)

;; Eldoc
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; General Settings
(setq inhibit-startup-message t)
(transient-mark-mode t)
(menu-bar-mode -1)
(setq compilation-window-height 45)
(global-set-key [f12] 'compile)
(mouse-avoidance-mode 'jump)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(line-number-mode 1)
(setq-default indicate-empty-lines t)
(column-number-mode 1)
(show-paren-mode t)
(auto-compression-mode 1)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(setq undo-limit 100000)
(setq fill-column 80)
(setq case-fold-search t)
(setq show-trailing-whitespace t)
(setq c-default-style "bsd") ;; Set BSD indent Style.
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq font-lock-maximum-decoration t)
(setq x-select-enable-clipboard t)
                                        ;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(display-time)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; QoL
;; resize windows with arrow keys
					; https://www.emacswiki.org/emacs/download/lazy-set-key.el
(require 'lazy-set-key)

(use-package windsize
  :ensure t
  :config
  (windsize-default-keybindings))
(require 'saveplace)

(use-package desktop
  :defer 2
  :config
  (setq desktop-restore-eager 5)
  (setq desktop-load-locked-desktop t)
  (desktop-save-mode +1))
;; Make Cursor more visible
(blink-cursor-mode 1)
(setq x-select-enable-clipboard t)
;; copy to x clipboard
(setq interprogram-paste-function 'x-selection-value)
;; Column indentation
(setq fci-rule-column 80)
(setq fci-rule-use-dashes t)
(setq fci-dash-pattern 0.5)
(setq fci-rule-width 1)
(setq fci-rule-color "grey40")
;; ignore errors in emacs
(setq debug-on-error nil)

;; highlight current lines
(global-hl-line-mode 1)
(set-face-background hl-line-face "grey20")


;; CUSTOM KEYS
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key [f2] (quote align-beginning-of-defun))
(global-set-key [f3] 'scroll-line-up)
(global-set-key [S-f8] 'nroff-fill-mode)
(global-set-key [f10] (quote align-end-of-defun))
(global-set-key [M-s] 'center-line)
(global-set-key "\M-\C-h" 'backward-kill-word)
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-R" 'replace-regexp)
(global-set-key "\M-G" 'goto-line)
(global-set-key "\M-h" 'help-command)

;;  Colors
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; fonts
(custom-set-faces
 '(default ((t (:family "Inconsolata" :foundry "nil" :slant normal :weight normal :height 181 :width normal)))))

;; make indentation more visible
(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "grey25")
  (set-face-background 'highlight-indentation-current-column-face "grey35"))

(use-package all-the-icons
  :ensure t)

;; Nuke whitespace
(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end nil)))

(global-set-key (kbd "C-x C-l") 'my-delete-leading-whitespace)

;; TODO: Split these to their own file?
;; modeline fix
(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))


(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))


;; Hooks
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'fci-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)

(use-package magit
  :commands (magit-status)
  :init (bind-key "C-x g" 'magit-status))

(use-package elpy
  :ensure t
  :bind ("M-." . elpy-goto-definition)
  :init
  (elpy-enable)

  (setq elpy-rpc-python-command "python")
  (setq python-shell-interpreter "python")
  (setq elpy-rpc-timeout 120)
  )

(setq smerge-command-prefix "\C-cv")


(use-package neotree
  :ensure t
  :bind ("<f8>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)

  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Every time when the neotree window is opened, let it find current file and jump to node
  (setq neo-smart-open t)

  ;; When running â€˜projectile-switch-projectâ€™ (C-c p p), â€˜neotreeâ€™ will change root automatically
  (setq projectile-switch-project-action 'neotree-projectile-action)

  ;; show hidden files
  (setq-default neo-show-hidden-files t))

;; helm was actually cool and better than ivy maybe I should update it
;; (require 'init-helm)


;;                                         ;open new buffer for all helm commands
;;                                         ;(setq helm-display-function 'helm-custom-display-buffer)
;;                                         ;(setq helm-show-completion-display-function #'helm-custom-display-buffer)
;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;       helm-display-buffer-reuse-frame nil
;;       helm-use-undecorated-frame-option t)
;; (setq helm-show-completion-display-function 'helm-display-buffer-in-own-frame
;;       helm-display-buffer-reuse-frame nil
;;       helm-use-undecorated-frame-optionrated-frame-option t)

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (
         ("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window)
  ;; make sure that swiper/ivy closes when expected to
;;         ("<ESC>" . minibuffer-keyboard-quit)
;;         ("C-g" . minibuffer-keyboard-quit)
         )
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  )

;; (use-package ivy-rich
;;   :after ivy
;;   :custom
;;   (ivy-virtual-abbreviate 'full
;;                           ivy-rich-switch-buffer-align-virtual-buffer t
;;                           ivy-rich-path-style 'abbrev)
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer
;;                                'ivy-rich-switch-buffer-transformer))

;; (use-package counsel
;;   :after ivy
;;   :config
;;   (counsel-mode)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; (use-package swiper
;;   :after ivy
;;   :bind (("S-C-s" . swiper)
;;          ("S-C-r" . swiper))
;;   ;; make sure that swiper/ivy closes when expected to
;;                                         ;(define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
;;                                         ;(define-key swiper-map (kbd "C-g") 'minibuffer-keyboard-quit)
;;   )

;; AUTO COMPLETE MODE
(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-modes 'sql-mode)
  (add-to-list 'ac-modes 'json-mode)
  (add-to-list 'ac-modes 'yaml-mode)
  (add-to-list 'ac-modes 'python-mode)
  (add-to-list 'ac-modes 'bash-mode)
  (global-auto-complete-mode t))


;; (require 'ac-helm)  ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-;") 'ac-complete-with-helm)
;; (define-key ac-complete-mode-map (kbd "C-;") 'ac-complete-with-helm)

;;; MaGIT -- Git integration with GNU/Emacs

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (global-magit-file-mode)
  )
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(gud-gdb-command-name "gdb --annotate=1")
;;  '(large-file-warning-threshold nil)
;;  '(package-selected-packages
;;    (quote
;;     (exec-path-from-shell use-package powerline highlight-indentation highlight-current-line helm-ls-git helm-c-yasnippet color-theme auto-complete))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package fill-column-indicator
  :ensure t)


(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
                                        ;(add-hook 'yaml-mode-hook 'highlight-indentation-mode)

(use-package pyenv-mode
  :ensure t)



(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)


;; Window dimming auto
;; (use-package solaire-mode
;;   :hook
;;   ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;   (minibuffer-setup . solaire-mode-in-minibuffer)
;;   :config
;;   (solaire-global-mode +1)
;;   (solaire-mode-swap-bg))
;; ORG MODE
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Code folding
(use-package origami
  :ensure t
  :config
  (global-origami-mode))

;; for git markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
