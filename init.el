(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

;(setq debug-on-error t)

; Print loading of packages
(setq use-package-verbose t)

(setq-default scroll-error-top-bottom t)

;(global-linum-mode 1)            ; show line numbers
;(setq linum-format "%4d \u2502") ; space and solid line after line numbers

(use-package nlinum
  :ensure t
  :init      (setq nlinum-format (quote " %5d "))
  :config
  (progn
    (add-hook 'prog-mode-hook 'nlinum-mode)))


(use-package nlinum-hl
  :ensure t
  :after nlinum
  :config
  (setq nlinum-highlight-current-line t))
;  (add-hook 'nlinum-mode-hook #'nlinum-hl-mode))


(setq backup-directory-alist '(("." . "~/.emacs_backup"))) ; Save backups here
(setq auto-save-default nil)     ; stop creating autosave files

; less annoying yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;Nyan cat, showing position in buffer
(use-package nyan-mode :ensure t
  :init
  (progn
    (nyan-mode)
    (nyan-toggle-wavy-trail)))

;(require 'multiple-cursors )
;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;(multiple-cursors-mode 1)

(use-package org
  :ensure t
  :config
  (progn
    (setq org-log-done t))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)))


;; wind move, move between windows with shift-arrow
;(when (fboundp 'windmove-default-keybindings)
;  (windmove-default-keybindings))

;;cpputils-cmake
;(add-to-list 'load-path "~/.emacs.d/plugins")
;(require 'cpputils-cmake)

; treat .h-files as c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
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


(use-package cc-mode
  :ensure t)

(use-package company
  :ensure t
  :bind (
	 ("M-RET" . company-complete))
  :config
  (progn
    (global-company-mode 1)
    ;(add-hook 'after-init-hook 'global-company-mode)
    (add-to-list 'company-backends 'company-irony)
    (add-to-list 'company-backends 'company-irony-c-headers))
  )


;; replace the `completion-at-point' and `complete-symbol' bindings in
;;irony-mode's sbuffers by irony-mode's asynchronous function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(defun my-irony-c++-mode-hook ()
  (setq irony-additional-clang-options '("-std=c++1y")))

(defun my-irony-c-mode-hook ()
  (setq irony-additional-clang-options '("-std=c99")))

(use-package irony
  :ensure t
  :config
  (progn
    (add-hook 'c-mode-common-hook 'irony-mode)
    (add-hook 'c++-mode-common-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'my-irony-c++-mode-hook)
    (add-hook 'c-mode-hook 'my-irony-c-mode-hook)
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (use-package company-irony
      :ensure t
      :config
      (progn
        (push 'company-irony company-backends)))))

;; (defun my-cmake-ide-hook ()
;;   (cmake-ide-load-db))

;; (use-package rtags
;;   :ensure t
;;   :after projectile
;;   :bind
;;   ("<f3>" . rtags-find-symbol-at-point))


;; (use-package cmake-ide
;;   :ensure t
;;   :after rtags
;;   :bind
;;   ("<f4>" . cmake-ide-compile)
;;   :config
;;   (progn
;;     (cmake-ide-setup)))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (add-hook 'c++-mode-hook 'flycheck-mode)
    (add-hook 'c-mode-hook 'flycheck-mode)
    ;(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++1y")))
    ;(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++1y")))
    ))



(defun flycheck-python-setup ()
  (flycheck-mode))
(add-hook 'python-mode-hook #'flycheck-python-setup)
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


(use-package clean-aindent-mode
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'clean-aindent-mode)))


(use-package dtrt-indent
  :ensure t
  :config
  (progn
    (dtrt-indent-mode 1)))


(use-package ws-butler
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'ws-butler-mode)))


; company-yasnippet creates too much noise in company-complete
;; Package: yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-yasnippet))


;(use-package smartparens
;  :ensure t
;  :config
;  (progn
;    (setq sp-base-key-bindings 'paredit)
;    (setq sp-autoskip-closing-pair 'always)
;    (setq sp-hybrid-kill-entire-symbol nil)
;    (sp-use-paredit-bindings)
;    (show-smartparens-global-mode +1)
;    (smartparens-global-mode 1)))

(use-package helm
  :ensure t
  :init (helm-mode 1)
  :config
  (progn
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x r b" . helm-bookmarks)
         ("M-i" . helm-swoop)
         ("C-x f" . helm-find-files)
         ("C-x C-f" . helm-find-files)))

(use-package projectile
  :ensure t
  :after helm
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-mode-line
         '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
    )
  :init
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :after projectile
  :init
  (progn
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)
    (setq projectile-indexing-method 'alien)))

(use-package zygospore
  :ensure t
  :bind
  (("C-x 1" . zygospore-toggle-delete-other-windows)))


(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)


;; Control compilation window
;; (defun my-compilation-hook ()
;;   (when (not (get-buffer-window "*compilation*"))
;;     (save-selected-window
;;       (save-excursion
;;         (let* ((w (split-window-vertically))
;;                (h (window-height w)))
;;           (select-window w)
;;           (switch-to-buffer "*compilation*")
;;           (shrink-window (- h compilation-window-height)))))))
;; (add-hook 'compilation-mode-hook 'my-compilation-hook)


;; ansi colors in compile output
(use-package ansi-color
  :ensure t)

(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)


(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package company-jedi
  :ensure t
  :bind
  (("<f3>" . jedi:goto-definition))
  :config
  (progn
    (add-hook 'python-mode-hook 'my/python-mode-hook)))

;; (use-package cyberpunk-theme
;;   :ensure t
;;   :config
;;   (progn
;;     (add-hook 'after-init-hook
;;           (lambda () (load-theme 'cyberpunk t)))))

(use-package material-theme
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook
          (lambda () (load-theme 'material t)))))



(defun my-c-mode-setup ()
  (message "my-c-mode-setup")
  (require 'rtags)
  (cmake-ide-setup)
  (global-set-key (kbd "<f3>") 'rtags-find-symbol-at-point)
  (global-set-key (kbd "<f4>") 'cmake-ide-compile))


(add-hook 'c-mode-common-hook 'my-c-mode-setup)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))


;; (defun shade-color (intensity)
;;   "compute new intensity of #rgb with alpha value for background"
;;   (interactive "nIntensity of the shade : ")
;;   (apply 'format "#%02x%02x%02x"
;;          (mapcar (lambda (x)
;;                    (if (> (lsh x -8) intensity)
;;                        (- (lsh x -8) intensity)
;;                      0))
;;                  (color-values (cdr (assoc 'background-color (frame-parameters)))))))

;; (use-package highline :ensure t
;;   :init
;;   (add-hook 'prog-mode-hook 'highline-mode)
;;   :config
;;   (progn
;;     (set-face-background 'highline-face (shade-color 09))))


(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "6c35ffc17f8288be4c7866deb7437e8af33cd09930e195738cdfef911ab77274" "235dc2dd925f492667232ead701c450d5c6fce978d5676e54ef9ca6dd37f6ceb" default)))
 '(hl-sexp-background-color "#1c1f26")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(org-agenda-files (quote ("~/todo.org")))
 '(safe-local-variable-values
   (quote
    ((cmake-ide-build-dir . "/home/jaa/projects/saas_hw/sw_repo_git/SW/FEE/build/make_fee")
     (cmake-ide-build-dir . "./build/make_fee")
     (cmake-ide-build-dir . "./build/make_test")
     (cmake-ide-build-dir . "../build/ROC/release/cmake")
     (setq irony-additional-clang-options
           (quote
            ("-std=c89")))
     (cmake-ide-build-dir . "/home/jaa/projects/saas_hw/sw_repo_git/SW/common/bootloader/u-boot")
     (cmake-ide-project-dir . "/home/jaa/projects/saas_hw/sw_repo_git/SW/common/bootloader/u-boot")
     (cmake-ide-build-dir . "/home/jaa/projects/saas_hw/sw_repo_git/SW/build/u-boot/saas_roc")
     (c++-mode
      ((setq irony-additional-clang-options
             (quote
              ("-std=c++11")))
       (flycheck-gcc-language-standard . "c++1y")
       (flycheck-clang-language-standard . "c++1y")))
     (cmake-ide-compile-command\. "/home/jaa/projects/saas_hw/sw_repo_git/SW/common/bootloader/u-boot/jalla.sh")
     (cmake-ide-build-command\. "/home/jaa/projects/saas_hw/sw_repo_git/SW/common/bootloader/u-boot/jalla.sh")
     (cmake-ide-project-dir . "/home/jaa/projects/saas_hw/sw_repo_git/SW/common/bootloader/u-boot/.")
     (setq irony-additional-clang-options
           (quote
            ("-std=c++11")))
     (irony-additional-clang-options . "-std=c++11")
     (irony-additional-clang-options . "-std=c++1y")
     (cmake-ide-build-dir . "../build"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
