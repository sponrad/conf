(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; packages to be installed
(ensure-package-installed
 'helm
 'magit
 'zenburn-theme
 'web-mode
 'projectile
 'helm-projectile
 'use-package
 's
 'dumb-jump
)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)

;; web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.djhtml?\\'" . web-mode)
         ("\\.tpl" . web-mode)
         ("\\.jsx" . web-mode))
  :config
  (defun m/current-buffer-django-p ()
    (save-excursion
      (search-forward-regexp "{% base\\|{% if\\|{% for\\|{% include\\|{% block\\|{% csrf_token %}\\|{% url\\|{{ "
                             nil
                             t)))
  (setq web-mode-engines-alist
        '(("django". "\\.djhtml")
          ("django" . m/current-buffer-django-p)
          ("php" . "\\.php")))
  (setq web-mode-content-types-alist
        '(("jsx"  . "\\.jsx")))
  (define-key web-mode-map (kbd "C-;") nil)
  (setq-default web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook (lambda () (electric-pair-local-mode 0))))

;; temp file save directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; (add-to-list 'load-path "/Users/conradframe/Documents/gdscript-mode")
;; (require 'gdscript-mode)

;; turn off bell
(setq ring-bell-function 'ignore)

;; magit bindings
(global-set-key (kbd "C-x g") 'magit-status)

;; helm bindings
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "s-g") 'helm-grep-do-git-grep)

;; projectile
(projectile-global-mode)
;;(define-key projectile-mode-map [?\s-g] 'projectile-grep)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(setq projectile-enable-caching t)

;; helm-projectile
(helm-projectile-on)

;; dumb-jump
(dumb-jump-mode)

;; theme
(load-theme 'zenburn t)
