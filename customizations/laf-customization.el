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
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

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

;; set keybinding for window-swapping 
(global-set-key (kbd "A-<left>") 'window-swap-states)


;; copy whole line emace
;; taken from: https://www.emacswiki.org/emacs/CopyingWholeLines#NewCommands
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "C-c w") 'copy-line)


;; Scroll output in compilation window
(setq compilation-scroll-output 'first-error)
