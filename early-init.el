;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 16 1024 1024))

(setq package-enable-at-startup nil)

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
    (locate-user-emacs-file  "var/cache/eln-cache/")))
