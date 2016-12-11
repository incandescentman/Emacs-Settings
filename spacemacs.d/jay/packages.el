;;; packages.el --- jay Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(setq
;; that's the variable
jay-packages
;; that's the value (it is a list of packages)
'(
  auto-capitalize
;;  auto-complete
  bongo
  buffer-stack
  caps-lock
  change-inner
  cheatsheet
  command-log-mode
  counsel
  crux
  cyberpunk-theme
  dired+
  dired-details+
;;  dired-hacks-utils
  dired-quick-sort
;;  dired-single
  dired-sort-menu
  discover-my-major
  expand-region
  fancy-narrow
  fastdef
  flyspell-lazy
  fountain-mode
  frame-cmds
  frame-restore
  fuzzy
  gist
  ;; graphviz-dot-mode
;;  helm
  helm-cmd-t
;;  helm-projectile
  ido-hacks
  imenu-list
  ;; js2
  key-chord
  ;; magit
;;  markdown-mode
  maxframe
  multicolumn
  multiple-cursors
  nm
;;  notmuch
  olivetti
  openwith
  org-bookmark-heading
  org-fstree
  ox-twbs
  palimpsest
  paredit
  peg
  point-stack
  polymode
  popup
  project-explorer
;;  projectile
  rainbow-mode
  recentf
  rspec-mode
  scratch
  scratch-message
  smex
  solarized-theme
  stripe-buffer
  sublime-themes
  tabbar
  tiny
  unfill
  wc-mode
  web-mode
  wrap-region
  writeroom-mode
  xah-replace-pairs
  xml-rpc
  zenburn-theme
  zone
yahoo-weather
wttrin
))



  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list."

(setq jay-excluded-packages
'(

  ;; "List of packages to exclude."
adaptive-wrap
  ))

;; For each package, define a function jay/init-<package-jay>
;;
;; (defun jay/init-my-package ()
;;   "Initialize my package"

(defun jay/init-autopair () (use-package autopair))
(defun jay/init-bongo () (use-package autopair))
(defun jay/init-cyberpunk-theme () (use-package cyberpunk-theme))
(defun jay/init-buffer-stack () (use-package buffer-stack))
(defun jay/init-org-fstree () (use-package org-fstree))
(defun jay/init-auto-capitalize () (use-package auto-capitalize))
;; (defun jay/init-edit-server () (use-package edit-server))
(defun jay/init-ido-hacks () (use-package ido-hacks))
(defun jay/init-openwith () (use-package openwith))
(defun jay/init-wc-mode () (use-package wc-mode))
(defun jay/init-frame-restore () (use-package frame-restore))
(defun jay/init-frame-cmds () (use-package frame-cmds))




(defun jay/init-popup () (use-package popup))
(defun jay/init-discover () (use-package discover))
(defun jay/init-engine-mode () (use-package engine-mode))
(defun jay/init-point-stack  () (use-package point-stack))
(defun jay/init-dired-details  () (use-package dired-details))
(defun jay/init-dired-sort-menu  () (use-package dired-sort-menu))
(defun jay/init-dired-details+  () (use-package dired-details+))
(defun jay/init-maxframe () (use-package maxframe))
(defun jay/init-palimpsest () (use-package palimpsest))
(defun jay/init-olivetti () (use-package olivetti))


(defun jay/init-recentf () (use-package recentf))
(defun jay/init-writeroom-mode () (use-package writeroom-mode))
(defun jay/init-key-chord () (use-package key-chord))

(defun jay/init-helm-cmd-t () (use-package helm-cmd-t))

(defun jay/init-w3m () (use-package w3m))


(defun jay/init-multiple-cursors () (use-package multiple-cursors))

(defun jay/init-xah-replace-pairs () (use-package xah-replace-pairs))

;; (defun jay/init-org-download () (use-package org-download))

(defun jay/init-gist () (use-package gist))

(defun jay/init-tiny () (use-package tiny))
(defun jay/init-imenu-list () (use-package imenu-list))
(defun jay/init-fountain-mode () (use-package fountain-mode))

(defun jay/init-stripe-buffer () (use-package stripe-buffer))


(defun jay/init-peg () (use-package peg))
(defun jay/init-nm () (use-package nm))

(defun jay/init-zone () (use-package zone))
;; (defun jay/init-direx () (use-package direx))
(defun jay/init-project-explorer () (use-package project-explorer))
(defun jay/init-discover-my-major () (use-package discover-my-major))

(defun jay/init-direx () (use-package direx))

;; (defun jay/init-org-vcard () (use-package org-vcard))

(defun jay/init-unfill() (use-package unfill))
;; (defun jay/init-swiper() (use-package swiper))

(defun jay/init-highlight-thing() (use-package highlight-thing))

(defun jay/init-wrap-region() (use-package wrap-region))
(defun jay/init-expand-region () (use-package expand-region))
;; (defun jay/init-graphviz-dot-mode () (use-package graphviz-dot-mode))


(defun jay/init-god-mode () (use-package god-mode))

(defun jay/init-multicolumn () (use-package multicolumn))


(defun jay/init-rainbow-mode () (use-package rainbow-mode))

(defun jay/init-change-inner () (use-package change-inner))

(defun jay/init-counsel () (use-package counsel))

(defun jay/init-fuzzy () (use-package fuzzy))
(defun jay/init-command-log-mode () (use-package command-log-mode))
(defun jay/init-ag () (use-package ag))

(defun jay/init-scratch () (use-package scratch))
(defun jay/init-ox-twbs () (use-package ox-twbs))
(defun jay/init-caps-lock () (use-package caps-lock))
(defun jay/init-dired+ () (use-package dired+))
;;(defun jay/init-org-pomodoro () (use-package org-pomodoro))


(defun jay/init-cheatsheet () (use-package cheatsheet))



(defun jay/init-scratch-message () (use-package scratch-message))
(defun jay/init-org-bookmark-heading () (use-package org-bookmark-heading))



(defun jay/init-tabbar () (use-package tabbar))

(defun jay/init-selected () (use-package selected))

(defun jay/init-fastdef () (use-package fastdef))
(defun jay/init-dired+ () (use-package dired+))
(defun jay/init-dired-details+ () (use-package dired-details+))

(defun jay/init-flyspell-lazy () (use-package flyspell-lazy))


(defun jay/init-crux () (use-package crux))

(defun jay/init-paredit () (use-package paredit))
(defun jay/init-web-mode () (use-package web-mode))

(defun jay/init-fancy-narrow () (use-package fancy-narrow))
(defun jay/init-polymode () (use-package polymode))
(defun jay/init-dired-quick-sort () (use-package dired-quick-sort))
(defun jay/init-rspec-mode () (use-package rspec-mode))
(defun jay/init-js2-mode () (use-package js2-mode))
(defun jay/init-yahoo-weather () (use-package yahoo-weather))

(defun jay/init-wttrin () (use-package wttrin))



;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
