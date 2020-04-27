;; Move all the customize stuff elsewhere to reduce clutter.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Switch to sane starting directory (no matter where the daemon
;; starts from)
(cd (expand-file-name "~"))

(add-to-list 'load-path "~/.emacs.d/own")
(add-to-list 'load-path "~/.emacs.d/external")
(add-to-list 'load-path "~/.emacs.d/external/use-package")

(require 'use-package)

;; Put temporary files in sane locations.
(use-package temp-config)

;; Allow the reset of setup to use elpa packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa stable" . "https://stable.melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(use-package elisp-utils)
(use-package evil-config)
(use-package text-misc)
(use-package text-config)
(use-package gui-config)
(use-package company-config)
(use-package clipboard-config)
(use-package lsp-config)
(use-package markdown-config)
(use-package magit-config)
(use-package uniquify)
(use-package projectile-config)
(use-package flycheck-config)
(use-package lisp-config)
(use-package python-config)
(use-package go-config)
(use-package rust-config)
(use-package yaml-config)
(use-package toml-mode)
(use-package js-config)
(use-package c-misc)
(use-package misc-misc)
(use-package insert-timestamp)
(use-package lua-mode)
(use-package org-config)
(use-package calc)
(use-package pastebinit)
(use-package dired-config)
(use-package gpg-config)
(use-package yasnippet-config)
(use-package csharp-config)
(use-package grep-config)
(use-package file-config)
(use-package misc-config)

(let ((site-lib (expand-file-name "~/.emacs.d/site.el")))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))


(use-package ffc-config)    ; load after site config

(server-start)

(use-package edit-server)
(edit-server-start)
