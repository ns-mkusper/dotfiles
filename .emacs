(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; Regular ole bullshit
(require 'saveplace)
;; remove window decoration
(set-frame-parameter nil 'undecorated t)

(setq initial-frame-alist '( (fullscreen . maximized)))
(setq default-frame-alist '( (fullscreen . fullheight)))


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


                                        ;(require 'cl)
                                        ;(require 'ido)
                                        ;(ido-mode t)
;; init.els
(defalias 'string-to-int 'string-to-number) ;;workaround for string-to-int problem
;; fix bad ssl
(require 'gnutls)
(add-to-list 'gnutls-trustfiles
             (expand-file-name
              "~/etc/tls/certificates/comodo.rsa.ca.intermediate.crt"))

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(require 'use-package)

(require 'python-config)

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
(setq compilation-window-height 8)
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
(blink-cursor-mode 1)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)






(setq debug-on-error nil)

;; Highlight line!
(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "grey12")

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

;;  Colours
(require 'color-theme-hober2)
(require 'color-theme)
(color-theme-hober2)

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


;;                                         ;semantic
;; (require 'helm)
;; (require 'helm-config)

;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;       helm-ff-file-name-history-use-recentf t)

;; (helm-mode 1)

(setq powerline-image-apple-rgb t)


(require 'powerline)
(powerline-default-theme)

(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "olive drab")
(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)


(use-package magit
  :commands (magit-status)
  :init (bind-key "C-x g" 'magit-status))

(setq elpy-rpc-python-command "python3.7")

(require 'init-helm)


                                        ;open new buffer for all helm commands
                                        ;(setq helm-display-function 'helm-custom-display-buffer)
                                        ;(setq helm-show-completion-display-function #'helm-custom-display-buffer)
(setq helm-display-function 'helm-display-buffer-in-own-frame
      helm-display-buffer-reuse-frame nil
      helm-use-undecorated-frame-option t)
(setq helm-show-completion-display-function 'helm-display-buffer-in-own-frame
      helm-display-buffer-reuse-frame nil
      helm-use-undecorated-frame-optionrated-frame-option t)




; === auto-complete ===
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict") 
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'sql-mode)
(add-to-list 'ac-modes 'json-mode)
(add-to-list 'ac-modes 'yaml-mode)
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
