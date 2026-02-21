;;; eaw.el --- Fix east asian ambiguous width issue for emacs -*- lexical-binding: t; -*-
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; URL: https://github.com/uwabami/locale-from-fontset
;; Version: 12
;; MIT License
(setq code-half '(
  #x101
))
(setq code-wide '(
  (#x231a . #x231b)
  (#x23e9 . #x23ec)
  #x23f0
  #x23f3
  (#x23fb . #x23fe)
  (#x25fd . #x25fe)
  (#x2614 . #x2615)
  #x2630
  (#x2648 . #x2653)
  #x2665
  #x267f
  #x2693
  #x26a1
  (#x26aa . #x26ab)
  (#x26bd . #x26be)
  (#x26c4 . #x26c5)
  #x26ce
  #x26d4
  #x26ea
  (#x26f2 . #x26f3)
  #x26f5
  #x26fa
  #x26fd
  #x2705
  (#x270a . #x270b)
  #x2728
  #x274c
  #x274e
  (#x2753 . #x2755)
  #x2757
  (#x276c . #x2771)
  (#x2795 . #x2797)
  #x27b0
  #x27bf
  (#x2b1b . #x2b1c)
  #x2b50
  #x2b55
  #x2b58
  (#xe000 . #xf8ff)
  #x1f004
  #x1f0cf
  #x1f18e
  (#x1f191 . #x1f19a)
  (#x1f1e6 . #x1f1ff)
  #x1f201
  #x1f21a
  #x1f22f
  (#x1f232 . #x1f236)
  (#x1f238 . #x1f23a)
  (#x1f250 . #x1f251)
  (#x1f300 . #x1f320)
  (#x1f32d . #x1f335)
  (#x1f337 . #x1f37c)
  (#x1f37e . #x1f393)
  (#x1f3a0 . #x1f3ca)
  (#x1f3cf . #x1f3d3)
  (#x1f3e0 . #x1f3f0)
  #x1f3f4
  (#x1f3f8 . #x1f43e)
  #x1f440
  (#x1f442 . #x1f4fc)
  (#x1f4ff . #x1f53d)
  (#x1f54b . #x1f54e)
  (#x1f550 . #x1f567)
  #x1f57a
  (#x1f595 . #x1f596)
  #x1f5a4
  (#x1f5fb . #x1f64f)
  (#x1f680 . #x1f6c5)
  #x1f6cc
  (#x1f6d0 . #x1f6d2)
  (#x1f6d5 . #x1f6d8)
  (#x1f6dc . #x1f6df)
  (#x1f6eb . #x1f6ec)
  (#x1f6f4 . #x1f6fc)
  (#x1f7e0 . #x1f7eb)
  #x1f7f0
  (#x1f90c . #x1f93a)
  (#x1f93c . #x1f945)
  (#x1f947 . #x1f9ff)
  (#x1fa70 . #x1fa7c)
  (#x1fa80 . #x1fa8a)
  (#x1fa8e . #x1fac6)
  #x1fac8
  (#x1facd . #x1fadc)
  (#x1fadf . #x1faea)
  (#x1faef . #x1faf8)
  (#xf0000 . #x10fffd)
))

;;;###autoload
(defun eaw-custominit ()
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (mapc (lambda (range) (set-char-table-range table range 1))
          code-half)
    (mapc (lambda (range) (set-char-table-range table range 2))
          code-wide)
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))


(provide 'eaw-custom)
;;; eaw.el ends here
