;; -*- lexical-binding: t; -*-

;;; My
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
  "h" `("help" . ,my-help-map)
  "w" `("window" . ,my-window-map))
;;; Elpaca
(defvar elpaca-installer-version 0.11)

(defvar elpaca-directory (expand-file-name "elpaca/" (locate-user-emacs-file "etc/")))

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
  (evil-collection-init '(elpaca)))
;;; UI/UX
(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1))
;;; Organizer
(use-package outline
  :ensure nil
  :config
  (setq outline-minor-mode-cycle t))
;;; Footer
;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
