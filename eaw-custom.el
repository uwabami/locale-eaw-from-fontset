;;; eaw.el --- Fix east asian ambiguous width issue for emacs -*- lexical-binding: t; -*-
;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; URL: https://github.com/uwabami/locale-from-fontset
;; Package-Requires: ((emacs "23"))
;; Version: 12
;; MIT License
(setq code-half '(
  #x101
  #x111
  #x113
  #x11b
  (#x126 . #x127)
  #x12b
  (#x131 . #x133)
  #x138
  (#x13f . #x142)
  #x144
  (#x148 . #x14b)
  #x14d
  (#x152 . #x153)
  (#x166 . #x167)
  #x16b
  #x1ce
  #x1d0
  #x1d2
  #x1d4
  #x1d6
  #x1d8
  #x1da
  #x1dc
  #x251
  #x2c4
  #x2c7
  #x2cd
  #x2d0
  (#x2d8 . #x2db)
  #x2dd
  #x2df
  (#x391 . #x3a1)
  (#x3a3 . #x3a9)
  (#x3b1 . #x3c1)
  (#x3c3 . #x3c9)
  #x401
  (#x410 . #x44f)
  #x451
  #x2010
  (#x2013 . #x2016)
  (#x2018 . #x2019)
  (#x201c . #x201d)
  (#x2020 . #x2022)
  (#x2024 . #x2027)
  #x2030
  (#x2032 . #x2033)
  #x2035
  (#x203b . #x203c)
  #x203e
  #x2049
  #x2074
  #x207f
  (#x2081 . #x2084)
  #x20ac
  #x2103
  #x2109
  #x2113
  #x2116
  (#x2121 . #x2122)
  #x2126
  #x212b
  #x2139
  (#x2153 . #x2154)
  (#x215b . #x215e)
  (#x2160 . #x217e)
  (#x2180 . #x2182)
  #x2189
  (#x2190 . #x2199)
  (#x21a9 . #x21aa)
  (#x21b8 . #x21b9)
  #x21d2
  #x21d4
  #x21e7
  #x2200
  (#x2202 . #x2203)
  (#x2207 . #x2208)
  #x220b
  #x220f
  #x2211
  #x2215
  #x221a
  (#x221d . #x2220)
  #x2223
  (#x2227 . #x222c)
  #x222e
  (#x2234 . #x2237)
  (#x223c . #x223d)
  #x2248
  #x224c
  #x2252
  (#x2260 . #x2261)
  (#x2264 . #x2267)
  (#x226a . #x226b)
  (#x226e . #x226f)
  (#x2282 . #x2283)
  (#x2286 . #x2287)
  #x2295
  #x2299
  #x22a5
  #x22bf
  #x2312
  (#x231a . #x231b)
  #x2328
  #x23cf
  (#x23e9 . #x23f3)
  (#x23f8 . #x23fa)
  (#x23fd . #x23fe)
  (#x2460 . #x2490)
  (#x2493 . #x24b5)
  #x24c2
  (#x2500 . #x2615)
  (#x2618 . #x261e)
  (#x2620 . #x262f)
  (#x2631 . #x266a)
  #x266d
  (#x266f . #x2671)
  #x267b
  (#x267e . #x26ff)
  #x2702
  #x2705
  (#x2708 . #x270d)
  #x270f
  #x2712
  #x2714
  #x2716
  #x271d
  #x2721
  #x2728
  (#x2733 . #x2734)
  #x273d
  #x2744
  #x2747
  #x274c
  #x274e
  (#x2753 . #x2755)
  #x2757
  (#x2763 . #x2764)
  (#x276c . #x2771)
  (#x2780 . #x2793)
  (#x2795 . #x2797)
  #x27a1
  #x27b0
  #x27bf
  (#x2934 . #x2935)
  (#x2b05 . #x2b07)
  (#x2b1b . #x2b1c)
  #x2b50
  (#x2b55 . #x2b57)
  #x2b59
  #x3030
  #x303d
  (#x3248 . #x324f)
  #x3297
  #x3299
  (#xe000 . #xf8ff)
  #xfffd
  #x1f004
  #x1f0cf
  (#x1f100 . #x1f10a)
  #x1f10c
  (#x1f110 . #x1f12d)
  (#x1f130 . #x1f169)
  (#x1f170 . #x1f1ac)
  (#x1f1e6 . #x1f1ff)
  (#x1f201 . #x1f202)
  #x1f21a
  #x1f22f
  (#x1f232 . #x1f23a)
  (#x1f250 . #x1f251)
  (#x1f300 . #x1f321)
  (#x1f324 . #x1f393)
  (#x1f396 . #x1f397)
  (#x1f399 . #x1f39b)
  (#x1f39e . #x1f3f0)
  (#x1f3f3 . #x1f3f5)
  (#x1f3f7 . #x1f4fd)
  (#x1f4ff . #x1f53d)
  (#x1f549 . #x1f54e)
  (#x1f550 . #x1f567)
  (#x1f56f . #x1f570)
  (#x1f573 . #x1f57a)
  #x1f587
  (#x1f58a . #x1f58d)
  #x1f590
  (#x1f595 . #x1f596)
  (#x1f5a4 . #x1f5a5)
  #x1f5a8
  (#x1f5b1 . #x1f5b2)
  #x1f5bc
  (#x1f5c2 . #x1f5c4)
  (#x1f5d1 . #x1f5d3)
  (#x1f5dc . #x1f5de)
  #x1f5e1
  #x1f5e3
  #x1f5e8
  #x1f5ef
  #x1f5f3
  (#x1f5fa . #x1f64f)
  (#x1f680 . #x1f6c5)
  (#x1f6cb . #x1f6d2)
  (#x1f6d5 . #x1f6d8)
  (#x1f6dc . #x1f6e5)
  #x1f6e9
  (#x1f6eb . #x1f6ec)
  #x1f6f0
  (#x1f6f3 . #x1f6fc)
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
(setq code-wide '(
  #x261
  (#x2c9 . #x2cb)
  #x2105
  #x217f
  #x2225
  (#x23fb . #x23fc)
  (#x2491 . #x2492)
  (#x24b6 . #x24c1)
  (#x24c3 . #x24ff)
  (#x2616 . #x2617)
  #x261f
  #x2630
  (#x266b . #x266c)
  #x266e
  (#x2672 . #x267a)
  (#x267c . #x267d)
  (#x2776 . #x277f)
  #x2b58
))

;;;###autoload
(defun eaw-init ()
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
