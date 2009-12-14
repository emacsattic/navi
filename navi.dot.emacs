; Hey emacs!  -*-mode: Emacs-Lisp; coding: utf-8; tab-width: 2;-*- 
;@ navi.el

;(load-library "~/lib/emacs/navi.el")

; add other modes and regexps for navi.el.
; below, 
; (1) I added a regexp ';@.*' to Emacs-Lisp mode.
; (2) I wrote over the regexp of navi.el for C++ mode.
; (3) I added a new mode-name and regexp for editing xml.
;     Originaly, navi.el does not know how to get tags from a buffer of xml-mode.
;     But in navi-regexp-alist, you can add a key as modename, and a value 
;     as the regexp which navi.el use how to get tags.
;     In this case, I was editing a xml file having tags <section NNN> and <titile NNN>.

(setq navi-regexp-alist
  (append
    '(
      ("Emacs-Lisp" .   "^\\([ \t]*(\\(defun\\|defcustom\\|defvar\\|defgroup\\)[ \t]+.*\\|;@.*\\)$")
      ("C++/l" .
"\\(\
^#pragma[ \t]+mark[ \t]+[^\n]+\\|\
^using[ \t]+[^\n]+\\|\
^class[ \t]+[^\n]+\\|\
^namespace[ \t]+[^\n]+\\|\
^[^ \t\n#/{}\\(\\)]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\|\
^[\\+\\-]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n]*\
\\)$")
      ("xml"  . 
"^\\(\
[ \t]*<section.*\\|\
[ \t]*<title.*\\|\
<.+>.*\
\\)$")
      ) navi-regexp-alist ))

(setq navi-color-mode "yellow")  ;
(setq navi-color-file "green")  ; 
(setq navi-color-func "white")  ; 
(when window-system
  (setq navi-color-mode "red")(setq navi-color-file "green")(setq navi-color-func "black"))

(setq navi-listing-window-height      8)
(setq navi-listing-window-shrink-size 1)
(setq navi-source-window-scroll-size  1)

; keybinding
(global-set-key "\C-x\C-l" 'call-navi)
(defun call-navi ()
  (interactive)
  (navi (buffer-name))
  )
