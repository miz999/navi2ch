;;; navi2ch-thumbnail-internal.el --- thumbnail view for navi2ch -*- coding: utf-8-unix; -*-
;; Copyright (C) 2020 by Navi2ch Project

;; Authors: MIZUNUMA Yuto <mizmiz@users.sourceforge.net>
;; Keywords: network 2ch

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; サムネイルを表示する場合、新機能を使うか以前のを使うか選択するstub
;; navi2ch-thumbnail-curl-p

(provide 'navi2ch-thumbnail)

(defvar navi2ch-thumbnail-curl-p t "サムネイルを表示する場合、新機能を使うか以前のを使うか選択する")

(if navi2ch-thumbnail-curl-p
    (require 'navi2ch-thumbnail-new)
  (require 'navi2ch-thumbnail-old))

