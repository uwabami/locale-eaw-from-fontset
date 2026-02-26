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
  #x261
  #x2c4
  #x2c7
  (#x2c9 . #x2cb)
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
  (#x2013 . #x2016)
  (#x2018 . #x2019)
  (#x201c . #x201d)
  (#x2020 . #x2022)
  #x2024
  #x2026
  #x2030
  (#x2032 . #x2033)
  #x2035
  #x203e
  #x2074
  #x207f
  (#x2081 . #x2084)
  #x20ac
  #x2113
  #x2116
  #x2126
  (#x2153 . #x2154)
  (#x215b . #x215e)
  #x217c
  #x2181
  #x2189
  (#x2190 . #x2193)
  #x21b9
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
  #x2225
  (#x2227 . #x222c)
  #x222e
  (#x2234 . #x2237)
  (#x223c . #x223d)
  #x2248
  #x224c
  #x2252
  (#x2260 . #x2261)
  (#x2264 . #x2267)
  (#x226e . #x226f)
  (#x2282 . #x2283)
  (#x2286 . #x2287)
  #x2295
  #x2299
  #x22a5
  #x22bf
  (#x2500 . #x25a9)
  (#x25ac . #x25b5)
  (#x25b7 . #x25bf)
  (#x25c1 . #x25fa)
  #x25ff
  (#x2607 . #x2608)
  (#x260c . #x260d)
  #x2613
  (#x2619 . #x261b)
  #x2621
  (#x2624 . #x2625)
  (#x2627 . #x2628)
  #x262c
  (#x2631 . #x2637)
  (#x263d . #x263f)
  (#x2643 . #x2645)
  #x2647
  (#x2654 . #x265e)
  (#x266d . #x2671)
  (#x2680 . #x2691)
  #x2698
  (#x269e . #x269f)
  #x26a4
  #x26a9
  #x26ac
  (#x26b2 . #x26bc)
  (#x26bf . #x26c3)
  (#x26c6 . #x26c7)
  (#x26c9 . #x26cd)
  #x26d0
  #x26d2
  (#x26d5 . #x26e3)
  #x26e8
  (#x26fb . #x26fc)
  #x26ff
  (#x2b56 . #x2b57)
  #x2b59
  #xfffd
))
(setq code-wide '(
  #x2010
  #x2025
  #x2027
  (#x203b . #x203c)
  #x2049
  #x2103
  #x2105
  #x2109
  (#x2121 . #x2122)
  #x212b
  #x2139
  (#x2160 . #x217b)
  (#x217d . #x2180)
  #x2182
  (#x2194 . #x2199)
  (#x21a9 . #x21aa)
  #x21b8
  (#x226a . #x226b)
  #x2312
  (#x231a . #x231b)
  #x2328
  #x23cf
  (#x23e9 . #x23f3)
  (#x23f8 . #x23fe)
  (#x2460 . #x24ff)
  (#x25aa . #x25ab)
  #x25b6
  #x25c0
  (#x25fb . #x25fe)
  (#x2600 . #x2606)
  (#x2609 . #x260b)
  (#x260e . #x2612)
  (#x2614 . #x2618)
  (#x261c . #x2620)
  (#x2622 . #x2623)
  #x2626
  (#x2629 . #x262b)
  (#x262d . #x2630)
  (#x2638 . #x263c)
  (#x2640 . #x2642)
  #x2646
  (#x2648 . #x2653)
  (#x265f . #x266c)
  (#x2672 . #x267f)
  (#x2692 . #x2697)
  (#x2699 . #x269d)
  (#x26a0 . #x26a3)
  (#x26a5 . #x26a8)
  (#x26aa . #x26ab)
  (#x26ad . #x26b1)
  (#x26bd . #x26be)
  (#x26c4 . #x26c5)
  #x26c8
  (#x26ce . #x26cf)
  #x26d1
  (#x26d3 . #x26d4)
  (#x26e4 . #x26e7)
  (#x26e9 . #x26fa)
  (#x26fd . #x26fe)
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
  (#x2776 . #x2793)
  (#x2795 . #x2797)
  #x27a1
  #x27b0
  #x27bf
  (#x2934 . #x2935)
  (#x2b05 . #x2b07)
  (#x2b1b . #x2b1c)
  #x2b50
  #x2b55
  #x2b58
  #x3030
  #x303d
  (#x3248 . #x324f)
  #x3297
  #x3299
  (#xe000 . #xf8ff)
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
