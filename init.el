;; Archive options -------------------------------------

;; allow emacs to use melpa archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Packages ---------------------------------------------

;;;; Add package directory to load path
(let ((default-directory  "~/.emacs.d/packages/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;;; load use-package
(eval-when-compile
  (require 'use-package))

;;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; cider
(use-package cider
  :ensure t
  :custom
  (cider-repl-display-help-banner nil))


;;; clojure-mode
(use-package clojure-mode
  :ensure t)

;;; projectile 
(use-package projectile
  :ensure t
  :init   (projectile-mode +1)
  :bind   (:map projectile-mode-map
                ("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

;;; paredit
(use-package paredit
  :ensure t
  :init (show-paren-mode 1)
  :commands (enable-paredit-mode)
  :hook ((clojure-mode emacs-lisp-mode) . enable-paredit-mode))

;;; rainbow-delimeters
(use-package rainbow-delimiters
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode) . rainbow-delimiters-mode))

;;; ivy
(use-package ivy
  :ensure t
  :init (ivy-mode +1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))


;;; undo-tree
;;; undo-tree is not on Melpa so it is downloaded into packages directory
(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1))

;;; smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

;;; ido
(use-package ido-completing-read+
  :ensure t
  :init
  (ido-mode +1)
  (ido-everywhere +1)
  :config (ido-ubiquitous-mode +1))

(use-package speed-type
  :ensure t
  :defines 'speed-type-gb-book-list
  :config
  (defvar my-gb-book-list
    '(1695  ; The Man Who Was Thursday
      1400  ; Great Expectations
      42324 ; Frankenstein; Or, The Modern Prometheus
      408   ; The Souls of Black Folk
      99    ; The Collected Articles of Fredrick Douglas
      19435 ; The Mule Bone
      43    ; The Strange Case of Dr. Jeckyll and Mr. Hyde
      36    ; War of the Worlds
      5230  ; The Invisible Man: A Grotesque Romance
      244   ; A Study in Scarlett
      120   ; Treasure island
      215   ; The Call of the Wild
      345   ; Dracula
      23    ; Narrative of the Life of Fredrerick Douglass
      19337 ; A Christmas Carol
      31516 ; The Eyes Have It
      2148  ; The Works of Edgar Allan Poe — Volume 2

      ))
  ;; (dolist (book my-book-list)
  ;;   (add-to-list 'speed-type-gb-book-list book))
      (setq speed-type-gb-book-list my-gb-book-list)
  )

;; Customizations folder
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "org-mode-customization")
(load "laf-customization")
(load "clojure-mode-customizations")


;;;; added by Custom ----------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(speed-type smex undo-tree ivy paredit projectile cider use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
