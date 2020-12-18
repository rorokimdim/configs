;; Turn off some ui elements
(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))

(push '(tool-bar-lines . 0) default-frame-alist)

(push '(vertical-scroll-bars) default-frame-alist)
