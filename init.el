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

(defun my-prog-mode-whitespace-style ()
  (setq-local whitespace-style
              '(face tabs tab-mark trailing lines missing-newline-at-eof)))
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
  ":" `("M-x" . ,#'execute-extended-command)
  "b" `("buffer" . ,my-buffer-map)
  "f" `("file" . ,my-file-map)
  "g" `("git" . ,my-git-map)
  "h" `("help" . ,my-help-map)
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
;;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  :config
  (setq evil-symbol-word-search t)
  (evil-mode 1)
  ;; Leader
  (evil-define-key my--leader-key-states 'global
    (kbd my-leader-key) my-leader-map)
  (evil-define-key my--leader-alt-key-states 'global
    (kbd my-leader-alt-key) my-leader-map))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-key-blacklist `(,my-leader-key ,my-leader-alt-key))
  (evil-collection-init '(elpaca magit)))
;;; UI/UX
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode -1)

(setq column-number-mode t)

(setq ring-bell-function #'ignore)

(setq use-short-answers t)
;; NOTE: By default, SPC = yes when `y-or-n-p' prompts you (and
;; `y-or-n-p-use-read-key' is off). This seems too easy to hit by accident,
;; especially with SPC as our default leader key.
(keymap-set y-or-n-p-map "SPC" nil)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq whitespace-style '(face tabs tab-mark trailing missing-newline-at-eof))
(add-hook 'prog-mode-hook #'my-prog-mode-whitespace-style)
(setq whitespace-line-column nil)
(global-whitespace-mode 1)

(use-package transient
  :ensure t
  :init
  (setq transient-history-file (my-var "transient/history.el"))
  (setq transient-levels-file (my-var "transient/levels.el"))
  (setq transient-values-file (my-var "transient/values.el")))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1))
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
  :bind (:map magit-mode-map
              ("SPC" . nil)
              :map magit-diff-mode-map
              ("SPC" . nil)))
;;; Footer
;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
