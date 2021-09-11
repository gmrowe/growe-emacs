;; Look and feel options -------------------------------

;;; start in misterioso theme by default
(load-theme 'misterioso t)

;;; remove toolbar
(tool-bar-mode -1)

;;; remove menubar
(menu-bar-mode -1)

;;; Customize the cursor - I like the bar
(blink-cursor-mode 1)
(setq-default cursor-type 'bar)
(set-cursor-color "#ffffff") ; Set cursor color to white

;;; turn off annoying beep
(setq visible-bell t)

;;; turn on column numbers
(setq column-number-mode +1)

;;; set startup frame size
(add-to-list 'default-frame-alist '(height . 64))
(add-to-list 'default-frame-alist '(width . 160))

(setq inhibit-startup-screen t)

;; highlights matching parens
(show-paren-mode +1)

;; tabs are evil!!!
(setq indent-tabs-mode nil)

;; comments
;; https://github.com/flyingmachine/emacs-for-clojure/blob/master/customizations/editing.el
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; fix weird os x kill error
;; https://github.com/flyingmachine/emacs-for-clojure/blob/master/customizations/editing.el
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; do not need to create lock-files
(setq create-lockfiles nil)

;; toggle fullscreen mode on mac
(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 
    (if (frame-parameter nil 'fullscreen)
      nil
      'fullboth)))

(define-key global-map [(alt return)] 
  'mac-toggle-max-window)
