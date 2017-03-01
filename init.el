(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defconst demo-packages
  '(anzu
    company
    irony
    rtags
    cmake-ide
    duplicate-thing
    helm
    helm-projectile
    helm-swoop
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    yasnippet
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


(global-linum-mode 1)            ; show line numbers
(setq linum-format "%4d \u2502") ; space and solid line after line numbers

(setq backup-directory-alist '(("." . "~/.emacs_backup"))) ; Save backups here
(setq auto-save-default nil)     ; stop creating autosave files

; less annoying yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; Nyan cat, showing position in buffer
(nyan-mode)
(nyan-toggle-wavy-trail)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'irony)
(require 'company)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;;irony-mode's sbuffers by irony-mode's asynchronous function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony-c-headers))

(global-set-key (kbd "M-RET") 'company-complete)

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Flycheck
(require 'flycheck)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

; This gets reverted at startup, why?!
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

; company-yasnippet creates too much noise in company-complete
;; Package: yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-yasnippet))


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
(global-set-key (kbd "C-x f") 'helm-find-files)

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
(cmake-ide-setup)

;;
(global-set-key (kbd "<f3>") 'rtags-find-symbol-at-point)
(global-set-key (kbd "<f4>") 'cmake-ide-compile)

(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)

;; Control compilation window
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)


;; ansi colors in compile output
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-jedi
(require 'company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
