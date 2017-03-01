(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; === CUSTOM CHECK FUNCTION ===
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (unless (package-installed-p package)
       (package-install package)))
     packages)
)

;; packages to be installed
(ensure-package-installed
 'helm
 'magit
 'zenburn-theme
 'web-mode
)

(tool-bar-mode -1)
(menu-bar-mode -1)

;; web mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

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

;; theme
(load-theme 'zenburn t)
