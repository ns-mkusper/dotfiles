(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; QoL
(require 'saveplace)
(require 'lazy-set-key)

;; remove window decoration
(set-frame-parameter nil 'undecorated t)

(setq initial-frame-alist '( (fullscreen . maximized)))
(setq default-frame-alist '( (fullscreen . fullheight)))

;; resize windows with arrow keys
(require 'windsize)
(windsize-default-keybindings)

;; get meta key working on Mac
(setq-default mac-option-modifier 'meta)

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

;; fix bad ssl
(require 'gnutls)
(add-to-list 'gnutls-trustfiles
             (expand-file-name
              "~/etc/tls/certificates/comodo.rsa.ca.intermediate.crt"))

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(require 'use-package)
(require 'all-the-icons)
;; (require 'python-config)

;; Eldoc
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; READ $PATH from system
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; General Settings
(setq inhibit-startup-message t)
(transient-mark-mode t)
                                        ;(menu-bar-mode -1)
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

;; Highlight line
(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "grey20")

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
(require 'doom-themes)

;; make indentation more visible
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "grey25")
(set-face-background 'highlight-indentation-current-column-face "grey35")

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(use-package all-the-icons)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)


;; Nuke whitespace
(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end nil)))

(global-set-key (kbd "C-x C-l") 'my-delete-leading-whitespace)

;; ;; Turn on project management.

;; (setq ede-locate-setup-options '(ede-locate-global ede-locate-locate ede-locate-base))
;; (global-ede-mode 1)
;; (global-srecode-minor-mode 1)

;; Kick off the semantic bovinator, function menu, C-warning mode, and flashing
;; brackets.
(require 'semantic)

(setq semantic-default-submodes (append semantic-default-submodes
                                        '(global-semantic-idle-local-symbol-highlight-mode
                                          global-semantic-idle-summary-mode
                                          global-semantic-idle-completions-mode
                                          global-semantic-decoration-mode
                                          global-semantic-highlight-func-mode
                                          global-semantic-stickyfunc-mode
                                          global-semantic-show-unmatched-syntax-mode
                                          global-semantic-mru-bookmark-mode)))

(setq semantic-decoration-styles '(("semantic-decoration-on-includes" . t)
                                   ("semantic-decoration-on-protected-members")
                                   ("semantic-decoration-on-private-members")))

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)



;; ;; CC-mde
(add-hook 'c-mode-common-hook '(lambda ()
                                 (setq ac-sources (append '(ac-source-semantic) ac-sources))
                                 (semantic-mode 1)))

                                        ;
;; ;; Some key bindings

(define-key semantic-mode-map (kbd "C-c , .") 'semantic-ia-fast-jump)
(define-key semantic-mode-map (kbd "C-c , P") 'semantic-analyze-proto-impl-toggle)
(define-key semantic-mode-map (kbd "C-c , h") 'semantic-decoration-include-visit)

;;                                         ;(global-ede-mode 1)



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


;;jump to function definition
(defun find-definition (arg)
  "Jump to the definition of the symbol, type or function at point.
  With prefix arg, find in other window."
  (interactive "P")
  (let* ((tag (or (semantic-idle-summary-current-symbol-info-context)
                  (semantic-idle-summary-current-symbol-info-brutish)
                  (error "No known tag at point")))
         (pos (or (semantic-tag-start tag)
                  (error "Tag definition not found")))
         (file (semantic-tag-file-name tag)))
    (if file
        (if arg (find-file-other-window file) (find-file file))
      (if arg (switch-to-buffer-other-window (current-buffer))))
    (push-mark)
    (goto-char pos)
    (end-of-line)
    )
  )


(setq powerline-image-apple-rgb t)


(require 'powerline)
(powerline-default-theme)

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

  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically
  (setq projectile-switch-project-action 'neotree-projectile-action)

  ;; show hidden files
  (setq-default neo-show-hidden-files t))

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

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

                                        ; === auto-complete ===
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'sql-mode)
(add-to-list 'ac-modes 'json-mode)
(add-to-list 'ac-modes 'yaml-mode)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'bash-mode)
(global-auto-complete-mode t)

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

(require 'fill-column-indicator)


(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
                                        ;(add-hook 'yaml-mode-hook 'highlight-indentation-mode)

(require 'pyenv-mode)



(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;; make sure that swiper/ivy closes when expected to
(define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
                                        ;(define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map (kbd "C-g") 'minibuffer-keyboard-quit)
                                        ;(define-key swiper-map (kbd "C-g") 'minibuffer-keyboard-quit)


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
(require 'origami)
(global-origami-mode)

;; for git markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
