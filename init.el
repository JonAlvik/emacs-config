(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defconst demo-packages
  '(anzu
    company
    duplicate-thing
    ggtags
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    ;; function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
;    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    zygospore))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;(defvaralias 'c-basic-offset 'tab-width)

(global-linum-mode 1)            ; show line numbers
(setq linum-format "%4d \u2502") ; space and solid line after line numbers

(setq backup-directory-alist '(("." . "~/.emacs_backup"))) ; Save backups here
(setq auto-save-default nil)     ; stop creating autosave files

; less annoying yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; Nyan cat, showing position in buffer
(nyan-mode)

;(require 'multiple-cursors )
;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;(multiple-cursors-mode 1)

;;;
;;; Org Mode
;;;
(require 'org)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
;(setq-default major-mode 'org-mode) ;kinda annoying


;; wind move, move between windows with shift-arrow
;(when (fboundp 'windmove-default-keybindings)
;  (windmove-default-keybindings))

;;cpputils-cmake
;(add-to-list 'load-path "~/.emacs.d/plugins")
;(require 'cpputils-cmake)

(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))
;; OPTIONAL, somebody reported that they can use this package with Fortran
;(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
 '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
;; OPTIONAL, some users need specify extra flags forwarded to compiler
;(setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))


(require 'cc-mode)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;(delete 'company-semantic company-backends)

(require 'irony)
;(require 'company-irony)
;(eval-after-load 'company
;  '(add-to-list 'company-backends 'company-irony))

;(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony-c-headers))


;; Irony completion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)



(require 'company-clang)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-clang))


;; Flycheck
;(require 'flycheck)
;(add-hook 'c++-mode-hook 'flycheck-mode)
;(add-hook 'c-mode-hook 'flycheck-mode)
;(add-hook 'c++-mode-hook
;          (lambda () (setq flycheck-clang-language-standard "c++1y")))
;(add-hook 'c++-mode-hook
;          (lambda () (setq flycheck-clang-standard-library "libc++")))

;(eval-after-load 'flycheck
;    '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(setq c-default-style "linux"
      c-basic-offset 4)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;;helm
(require 'helm-config)
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-i") 'helm-swoop)

;; Package: projectile
(require 'projectile)
(projectile-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(require 'rtags)
;; (require 'company-rtags)
;; (setq rtags-completions-enabled t)
;; (eval-after-load 'company
;;   '(add-to-(list )
;;            'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)

(cmake-ide-setup)

(global-set-key (kbd "<f3>") 'rtags-find-symbol-at-point)
(global-set-key (kbd "<f4>") 'cmake-ide-compile)

(setq compilation-auto-jump-to-first-error t)

(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(org-agenda-files (quote ("~/todo.org")))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
