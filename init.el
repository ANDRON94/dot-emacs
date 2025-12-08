;; -*- lexical-binding: t; -*-

;;; My
;;;; Lisp
(defvar my-lisp-dir (locate-user-emacs-file "lisp/")
  "Where I store my libraries.")

(defvar my-var-dir (locate-user-emacs-file "var/")
  "Where I store my global state files.

State files contain unessential, non-portable, but persistent data which, if
lost won't cause breakage, but may be inconvenient as they cannot be
automatically regenerated or restored. For example, a recently-opened file list
is not essential, but losing it means losing this record, and restoring it
requires revisiting all those files.

Use this for: history, logs, user-saved data, autosaves/backup files, known
projects, recent files, bookmarks.")

(defvar my-cache-dir (expand-file-name "cache/" my-var-dir)
  "Where I store my global cache files.

Cache files represent unessential data that shouldn't be problematic when
deleted (besides, perhaps, a one-time performance hit), lack portability (and so
shouldn't be copied to other systems/configs), and are regenerated when needed,
without user input.

Some examples: images/data caches, elisp bytecode, natively compiled elisp,
session files, ELPA archives, authinfo files, org-persist, etc.")

(defvar my-etc-dir (locate-user-emacs-file "etc/")
  "Where I store my global data files.

Data files contain shared and long-lived data that Emacs, and their
packages require to function correctly or at all. Deleting them by hand will
cause breakage, and require user intervention (e.g. reinstall packages)
to restore.

Use this for: server binaries, package source, pulled module libraries,
autoloads/loaddefs, etc.")

(defun my-var (name)
  (expand-file-name name my-var-dir))

(defun my-etc (name)
  (expand-file-name name my-etc-dir))
;;;; Key Bindings
(defvar my-leader-key "SPC"
  "The leader prefix key.")

(defvar my-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states.")

(defvar my--leader-key-states '(normal visual motion))

(defvar my--leader-alt-key-states '(emacs insert))
;;;;; Global
(defvar-keymap my-buffer-map
  :doc "Keymap for operations on buffers: switch, close, revert, etc."
  "b" #'switch-to-buffer
  "d" #'kill-current-buffer
  "i" #'ibuffer
  "n" #'next-buffer
  "p" #'previous-buffer
  "r" #'revert-buffer)

(defvar-keymap my-file-map
  :doc "Keymap for operations on files: open file, open directory, open recently closed file, etc."
  "d" #'dired
  "f" #'find-file
  "r" #'recentf)

(defvar-keymap my-git-find-map
  :doc "Keymap for finding files with git (looking into history)."
  "f" #'magit-find-file)

(defvar-keymap my-git-map
  :doc "Keymap for working with git: status, blame, revert, log, etc."
  "B" #'magit-blame-addition
  "C" #'magit-clone
  "L" #'magit-log-buffer-file
  "[" #'diff-hl-previous-hunk
  "]" #'diff-hl-next-hunk
  "f" `("find" . ,my-git-find-map)
  "g" #'magit-status
  "r" #'diff-hl-revert-hunk)

(defvar-keymap my-help-bindings-map
  :doc "Keymap to get help for various key bindings."
  "b" #'describe-bindings)

(defvar-keymap my-help-map
  :doc "Keymap to get help regarding functions, variables, key bindings, etc."
  :keymap help-map
  "b" `("bindings" . ,my-help-bindings-map))

(defvar-keymap my-insert-map
  :doc "Keymap for inserting things into buffer."
  "y" #'yank-from-kill-ring
  "r" #'consult-register)

(defvar-keymap my-register-map
  :doc "Keymap for operations on registers: create, jump, etc."
  "SPC" #'point-to-register
  "b" #'bookmark-jump
  "i" #'insert-register
  "j" #'jump-to-register
  "m" #'bookmark-set
  "n" #'number-to-register
  "s" #'copy-to-register
  "w" #'window-configuration-to-register)

(defvar-keymap my-search-map
  :doc "Keymap for in buffer (and more) searching."
  "B" #'consult-line-multi
  "I" #'consult-imenu-multi
  "S" #'my-consult-line-thing-at-point
  "d" #'consult-ripgrep
  "f" #'consult-fd
  "i" #'consult-imenu
  "s" #'consult-line)

(defvar-keymap my-toggle-map
  :doc "Keymap for toggling things on and off."
  "c" #'global-display-fill-column-indicator-mode
  "r" #'read-only-mode
  "w" #'visual-line-mode)

(defvar-keymap my-window-maximize-map
  :doc "Keymap for maximizing windows: full, horizontal, vertical, etc."
  "m" #'delete-other-windows
  "s" #'ignore
  "v" #'ignore)

(defvar-keymap my-window-map
  :doc "Keymap for operations on windows: switch, close, split, etc."
  "d" #'evil-window-delete
  "h" #'evil-window-left
  "j" #'evil-window-down
  "k" #'evil-window-up
  "l" #'evil-window-right
  "m" `("maximize" . ,my-window-maximize-map)
  "s" #'evil-window-split
  "v" #'evil-window-vsplit)

(defvar-keymap my-leader-map
  :doc "Root keymap for all user-defined key bindings."
  "'" #'vertico-repeat
  "*" #'my-consult-ripgrep-thing-at-point
  "." #'find-file
  "/" #'consult-ripgrep
  ":" `("M-x" . ,#'execute-extended-command)
  "<" #'consult-buffer
  "RET" #'consult-bookmark
  "b" `("buffer" . ,my-buffer-map)
  "f" `("file" . ,my-file-map)
  "g" `("git" . ,my-git-map)
  "h" `("help" . ,my-help-map)
  "i" `("insert" . ,my-insert-map)
  "r" `("register" . ,my-register-map)
  "s" `("search" . ,my-search-map)
  "t" `("toggle" . ,my-toggle-map)
  "w" `("window" . ,my-window-map))
;;; Elpaca
(defvar elpaca-installer-version 0.11)

(defvar elpaca-directory (my-etc "elpaca/"))

(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))

(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode 1))

(setq use-package-hook-name-suffix nil)
;;; Evil
(defvar my--evil-mode-line-tag-placeholder ""
  "No-op variable just to place `evil-mode-line-tag' in the mode line.")

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  :config
  (setq evil-symbol-word-search t)
  (setq evil-mode-line-format '(after . my--evil-mode-line-tag-placeholder))
  (evil-mode 1)
  ;; Leader
  (evil-define-key my--leader-key-states 'global
    (kbd my-leader-key) my-leader-map)
  (evil-define-key my--leader-alt-key-states 'global
    (kbd my-leader-alt-key) my-leader-map)
  ;; Rest
  (evil-define-key my--leader-key-states 'global
    (kbd "g D") #'xref-find-references
    (kbd "s") #'evil-avy-goto-char-2
    (kbd "z N") #'widen
    (kbd "z n") #'narrow-to-region))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-key-blacklist `(,my-leader-key ,my-leader-alt-key))
  (evil-collection-init '(elpaca ibuffer magit outline xref)))
;;; UI/UX
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode -1)

(setq ring-bell-function #'ignore)

(setq use-short-answers t)
;; NOTE: By default, SPC = yes when `y-or-n-p' prompts you (and
;; `y-or-n-p-use-read-key' is off). This seems too easy to hit by accident,
;; especially with SPC as our default leader key.
(keymap-set y-or-n-p-map "SPC" nil)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq whitespace-style '(face tabs tab-mark trailing missing-newline-at-eof))
(defun my--prog-mode-whitespace-style ()
  (setq-local whitespace-style
              '(face tabs tab-mark trailing lines missing-newline-at-eof)))
(add-hook 'prog-mode-hook #'my--prog-mode-whitespace-style)
(setq whitespace-line-column nil)
(global-whitespace-mode 1)

(global-hl-line-mode 1)

(put 'narrow-to-region 'disabled nil)

(defun my--corfu-mode-completion-styles ()
  (setq-local completion-styles '(orderless-fast basic))
  (setq-local completion-category-overrides nil)
  (setq-local completion-category-defaults nil))

(use-package corfu
  :ensure t
  :hook ((corfu-mode-hook . my--corfu-mode-completion-styles)
         (prog-mode-hook . corfu-mode)
         (text-mode-hook . corfu-mode))
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator))
  :init
  (setq corfu-auto t))

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       (cons 'orderless-literal-prefix word)))

(use-package orderless
  :ensure t
  :config
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides
        '((file (styles basic orderless partial-completion)))))

(use-package transient
  :ensure t
  :init
  (setq transient-history-file (my-var "transient/history.el"))
  (setq transient-levels-file (my-var "transient/levels.el"))
  (setq transient-values-file (my-var "transient/values.el")))

(use-package vertico
  :ensure t
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :init
  (setq vertico-cycle t)
  (vertico-mode 1))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1))
;;;; Mode Line
(use-package evil-anzu
  :ensure t
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "black"
                      :weight 'normal)
  (global-anzu-mode 1))

(defsubst my--column-number-at-pos (&optional pos)
  (if pos
      (save-excursion
        (goto-char pos)
        (current-column))
    (current-column)))

(defun my--mode-line-region-info ()
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (mode-line-window-selected-p))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (let ((lines (count-lines beg (min end (point-max)))))
        (cond ((or (bound-and-true-p rectangle-mark-mode)
                   (and (bound-and-true-p evil-visual-selection)
                        (eq 'block evil-visual-selection)))
               (let ((cols (abs (- (my--column-number-at-pos end)
                                   (my--column-number-at-pos beg)))))
                 (format " %dx%dB" lines cols)))
              ((and (bound-and-true-p evil-visual-selection)
                    (eq evil-visual-selection 'line))
               (format " %dL" lines))
              (t
               (format " %dC" (- end beg))))))))

(defun my--mode-line-macro-recording ()
  (when (and (or defining-kbd-macro executing-kbd-macro)
             (mode-line-window-selected-p))
    (if (bound-and-true-p evil-this-macro)
        (format " M(%c)" evil-this-macro)
      " M()")))

(setq column-number-mode t)

(setq-default mode-line-format
              '(;; Errors
                "%e "
                ;; State
                "%+" my--evil-mode-line-tag-placeholder "%n"
                (:eval (my--mode-line-macro-recording))
                (:eval (my--mode-line-region-info))
                ;; Info
                " %o %l:%c %b "
                (:eval (symbol-name major-mode))))
;;; Editing
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 88)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
;;; Navigation
(use-package avy
  :ensure t
  :config
  (setq avy-background t)
  (setq avy-single-candidate-jump nil))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (my-var "bookmarks")))

(use-package consult
  :ensure t
  :config
  (defalias 'my-consult-line-thing-at-point #'consult-line)
  (consult-customize
   my-consult-line-thing-at-point
   :initial (thing-at-point 'symbol))
  (defalias 'my-consult-ripgrep-thing-at-point #'consult-ripgrep)
  (consult-customize
   my-consult-ripgrep-thing-at-point
   :initial (thing-at-point 'symbol)))

(use-package xref
  :ensure nil
  :config
  (setq xref-prompt-for-identifier nil)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))
;;; Saving
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq version-control t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq backup-directory-alist `(("." . ,(my-var "backup/"))))
(setq tramp-backup-directory-alist backup-directory-alist)

(setq auto-save-default t)
(setq auto-save-include-big-deletions t)
(setq auto-save-list-file-prefix (my-var "auto-save/"))
(setq tramp-auto-save-directory  (my-var "tramp/auto-save/"))

(use-package recentf
  :ensure nil
  :config
  (make-directory (my-var "recentf/") t)
  (setq recentf-auto-cleanup (if (daemonp) 300 'never))
  (setq recentf-save-file (my-var "recentf/history.el"))
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)
  (add-hook 'kill-emacs-hook #'recentf-cleanup))
;;; Organizer
(use-package outline
  :ensure nil
  :config
  (setq outline-minor-mode-cycle t))
;;; Git
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1))

(use-package magit
  :ensure t
  :after transient
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :bind (:map magit-mode-map
              ("SPC" . nil)
              :map magit-diff-mode-map
              ("SPC" . nil)))
;;; Footer
;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
