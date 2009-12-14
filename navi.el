; Hey emacs! This file is -*-mode: Emacs-Lisp; tab-width: 4;-*- .

; navi.el : List function declaration and jump to it.
; Copyright (C) 1999, Sunagawa Youichi <suna@love.email.ne.jp>

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Function   : Subroutine Navigator mode
; Project    : MARUHA GEDON
;              (The worst jokes with the best technologies. since 1991.)
; SubProject : SUBROUTINE NAVIGEDON
; Version    : $Id: navi.el,v 1.43 2009/02/28 13:07:30 savage41 Exp $
; Author     : GEDON No.2 Sunagawa Youichi <suna@love.email.ne.jp>
; Maintainer : (Is there someone want to maintain this? Contact suna.)
; PatchAuthor: GEDON No.1 TKS <tks@love.email.ne.jp>
;            : Ikeda Takashi
;            : Masafumi Matsumura <mmatsu@roy.hi-ho.ne.jp>
; WhereToGet : http://www.ne.jp/asahi/love/suna/pub/soft/navi.el/index.html   ( japanese )
;              http://www.ne.jp/asahi/love/suna/pub/soft/navi.el/index-e.html ( english )
; Report     : Bugs, improvement, comment to navi.el, send e-mail to suna@love.email.ne.jp.
;              Since native language of me is Japanese and Klingon,
;              language of e-mail is prefferable in japanese or klingon. 
;              I can read and write english a little, e-mail in english is also acceptable.
;
;              I enlist you as a PatchAuthor in case your patch adapted to navi.el.
;              So, chose one you like to enlisted.
;              1) Name only.
;                 ex) Foo Var, Hoge Hoge,
;              2) Name and e-mail.
;                 ex) Foo Var <foo@var.var.var>, Hoge Hoge <hoge@mogomogo>
;              3) Other nickname.
;                 ex) Kenshirou, Mr Spock

; [navi-mode]
; 
; Like etag, navi.el parse source file with regular expression, then list and jump by function declaration.
; 
; navi.el depend on mode name.
; It must be one of "C++","C","Pascal","Java","Emacs-Lisp","Lisp Interaction","Perl","CPerl","TeX","Text","HTML","Ruby".
;
; navi.el positions source-window to specific function declaration with regular expression that selected in listing-window.
; So, if there are two functions same name in source-window, navi.el positions the first one.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; [how to customize your .emacs]
;
;1.default height of listing-window
;
;(setq navi-listing-window-height 10)
;
;2.key stroke to evoke navi-mode
;
;(global-set-key [f11] 	   'call-navi)
;(global-set-key "\C-x\C-l" 'call-navi)
;(defun call-navi ()
;  (interactive)
;  (navi (buffer-name))
;  )
;
;3.other values
; change these values in your .emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start

; ToDo
; get key-string to lower. and search-string to lower too, when do search in navi

; mode specific regular expressions.
(setq navi-regexp-alist
      '(
		("Assembler"        . "\\(^[ \t]*[^\n#/\\*=]+[0-9a-zA-Z_ \t\\*,\.:]+{[^\n;]*\\)$")
		("Autoconf"         . "^\\(^AC_DEFUN.*\\)$")
		("Basic"            . "^\\(\\([Pp][Rr][Ii][Vv][Aa][Tt][Ee]\\|[Pp][Uu][Bb][Ll][Ii][Cc]\\|[Ss][Uu][Bb]\\|[F][U][N][C][T][I][O][N]\\)[ \t]+.*\\)$")
		("C++"              . "\\(^@[^end \t]+[^\n]+\\|^#pragma[ \t]+mark[ \t]+[^\n]+\\|^[^ \t\n#/]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\)$")
		("C++/l"            . "\\(^@[^end \t]+[^\n]+\\|^#pragma[ \t]+mark[ \t]+[^\n]+\\|^[^ \t\n#/]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\)$")
        ("C"                . "\\(^@[^end \t]+[^\n]+\\|^#pragma[ \t]+mark[ \t]+[^\n]+\\|^[^ \t\n#/]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\)$")
		("C/l"              . "\\(^@[^end \t]+[^\n]+\\|^#pragma[ \t]+mark[ \t]+[^\n]+\\|^[^ \t\n#/]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\)$")
		("CPerl"            . "^\\([ \t]*sub[ \t]+.*\\)$")
		("Diff"             . "^\\(^[0-9*]+.*\\)$")
		("Emacs-Lisp"       . "^\\([ \t]*(defun[ \t]+.*\\)$")
		("Lisp Interaction" . "^\\([ \t]*(defun[ \t]+.*\\)$")
		("HTML"             . "^\\([ \t]*<[Hh][123456].*\\|[ \t]*<[Hh][Ee][Aa][Dd].*\\|[ \t]*<[Bb][Oo][Dd][Yy].*\\|[ \t]*<[Ff][Oo][Rr][Mm].*\\)$")
		("Java"             . "\\(^[ \t]*[^\n#/\\*=]+[0-9a-zA-Z_ \t\\*,\.()]+{[^\n;]*\\)$")
		("JavaScript"       . "\\(^@[^end \t]+[^\n]+\\|^function[ \t]*(.*\\|^[^ \t\n#/]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\)$")
		("m4"               . "^\\(^AC_DEFUN.*\\)$")
		("ObjC"             . "\\(^@[^end \t]+[^\n]+\\|^#pragma[ \t]+mark[ \t]+[^\n]+\\|^[^ \t\n#/]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\)$")
		("ObjC/l"           . "\\(^@[^end \t]+[^\n]+\\|^#pragma[ \t]+mark[ \t]+[^\n]+\\|^[^ \t\n#/]+[0-9a-zA-Z_ \t\\*\\&:]+([^\n;]*\\)$")
		("Pascal"           . "^\\([Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee].*\\)$")
		("Perl"             . "^\\([ \t]*sub[ \t]+.*\\)$")
		("Ruby"             . "^\\([ \t]*\\(class\\|module\\|def\\|alias\\)[ \t]+.*\\)$")
		("Text"             . "^\\([ \t]*[1234567890]+[\.]+.*\\)$")
		("TeX"              . "^\\(\\\\chapter.*\\|\\\\section.*\\|\\\\subsection.*\\|\\\\subsubsection.*\\)$")
        ))

; listing-window
(defvar navi-listing-window-height      5)
(defvar navi-listing-window-shrink-size 1)
(defvar navi-source-window-scroll-size  1)
(defvar navi-color-mode "black")
(defvar navi-color-file "blue")
(defvar navi-color-func "red")

;; end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Be careful when you modify these 3 values.

(defvar navi-listing-format        "%s:%s:%s\n")
(defvar navi-listing-clean-format  "%s:%s:")		; this is NOT regular expression.
(defvar navi-listing-search-exp "^[ ]*\\([^:]*\\)[ ]*:[ ]*\\([^: ]*\\)[ ]*:\\(.*\\)$")
;(setq navi-listing-search-exp "^[ ]*\\([^:]*\\)[ ]*:[ ]*\\([^: ]*\\)[ ]*:\\(.*\\)$")
					; (match-beginning nn) (match-end nn) , nn is 1-3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; do not modify these values.

(defvar navi-listing-window-name "*Subroutine Navigator*")	; buffer name of listing-window.
(defvar navi-listing-mode-name	 "Navi")		   			; mode name of navi-mode.
(defvar navi-search-mode-name "")							; mode name of source file.
(defvar navi-pair-alist nil)		; internal data. contains relationship of a file and a buffer.
(defvar navi-version-rcs "$Id: navi.el,v 1.43 2009/02/28 13:07:30 savage41 Exp $")
(defvar navi-version-deffile "1")	; version number of stored information of listing-window
(defvar navi-version-gedon   "1")	; "SUBROUTINE NAVIGEDON" is the name of navi.el in private project of suna.

(defvar navi-mode-map nil)
(if navi-mode-map
    ()
  (setq navi-mode-map (make-sparse-keymap))
  (define-key navi-mode-map "\C-n"	'navi-next			)
  (define-key navi-mode-map "\C-o"	'navi-open-other-window		)
  (define-key navi-mode-map "\C-m"	'navi-open-other-window		)
  (define-key navi-mode-map " "		'navi-open-other-window		)
  (define-key navi-mode-map "\C-p"	'navi-previous			)
  (define-key navi-mode-map "d"		'navi-listing-window-delete-line)
  (define-key navi-mode-map "D"		'navi-listing-window-delete-file)
  (define-key navi-mode-map "q"		'navi-quit			)
  (define-key navi-mode-map "e"		'navi-edit			)
  (define-key navi-mode-map "o"		'navi-edit-quit			)
  (define-key navi-mode-map "k"		'navi-scroll-down-other-window	)
  (define-key navi-mode-map "j"		'navi-scroll-up-other-window	)
  (define-key navi-mode-map "b"		'navi-scroll-down-other-window-page	)
  (define-key navi-mode-map "f"		'navi-scroll-up-other-window-page	)
  (define-key navi-mode-map "n"		'navi-next-show			)
  (define-key navi-mode-map "p"		'navi-previous-show		)
  (define-key navi-mode-map "h"		'navi-hide-window		)
  (define-key navi-mode-map "2"		'navi-open-current-window	)
  (define-key navi-mode-map "^"		'navi-window-height-up		)
  (define-key navi-mode-map "v"		'navi-window-height-down	)
  (define-key navi-mode-map "s"		'navi-listing-widow-sort	)
  (define-key navi-mode-map "r"		'navi-listing-widow-sort-reverse	)

  (define-key navi-mode-map "\C-x\C-f"	'navi-listing-widow-read-file	)
  (define-key navi-mode-map "\C-x\C-s"	'navi-listing-widow-save-file	)
;  (define-key navi-mode-map "1"		'delete-other-windows		)
  (define-key navi-mode-map "1"		'navi-delete-other-windows	)
  )

(defun navi-gedon-version ()
  (interactive)
  (message navi-version-gedon))

(defun navi-version ()
  (interactive)
  (message navi-version-rcs))

(defun navi-scroll-up-other-window-page ()
  "scroll up source window 1 page."
  (interactive)
  (save-restriction
   (save-excursion
     (select-window (next-window))
     (scroll-up)
     (select-window (previous-window)))))

(defun navi-scroll-down-other-window-page ()
  "scroll down source window 1 page."
  (interactive)
  (save-restriction
   (save-excursion
     (select-window (next-window))
     (scroll-down)
     (select-window (previous-window)))))

(defun navi-scroll-up-other-window ()
  "scroll up source window by specific lines."
  (interactive)
  (save-restriction
   (save-excursion
     (select-window (next-window))
     (scroll-up navi-source-window-scroll-size)
     (select-window (previous-window)))))

(defun navi-scroll-down-other-window ()
  "scroll down source window by specific lines."
  (interactive)
  (save-restriction
   (save-excursion
     (select-window (next-window))
     (scroll-down navi-source-window-scroll-size)
     (select-window (previous-window)))))

(defun navi-quit ()
  "delete listing-window."
  (interactive)
  ;(kill-buffer navi-listing-window-name)
  (kill-buffer (get-buffer (buffer-name)))
  (select-window (next-window))
  (delete-other-windows))

(defun navi-open-other-window ()
  "put specific function declaration to top of window in source-window."
  (interactive)
  (navi-open t))

(defun navi-open-current-window ()
  (interactive)
  (navi-open nil)
  ; let window hight to half.
  (let (naviHeight naviNewHeight naviFrameHeight)
    (setq naviFrameHeight (frame-height))
    (setq naviHeight (window-height))
    (setq naviNewHeight (- (/ naviFrameHeight 2) naviHeight))
    (shrink-window (- naviNewHeight) nil)))

(defun navi-open (navi-flag)
  (interactive)
  (let ((bufname "")(subname ""))
    (beginning-of-line)
    (if (re-search-forward navi-listing-search-exp (point-max) nil)
	(progn
	  (setq navi-search-mode-name (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq bufname               (buffer-substring (match-beginning 2) (match-end 2)))
	  (setq subname               (buffer-substring (match-beginning 3) (match-end 3)))
	  (beginning-of-line)
	  (cond ((or (equal navi-search-mode-name "C++")
			(equal navi-search-mode-name "C"))
		 (progn 
		   (navi-show-sub-other-window-c bufname (regexp-quote subname) navi-flag)))
		((progn
		  (navi-show-sub-other-window bufname (regexp-quote subname) navi-flag)))))
      (message "not found"))))

(defun navi-show-sub-other-window (bufname subname navi-flag)
  (save-restriction
   (save-excursion
     (if navi-flag
	 (progn
	   (if (eq (next-window) (selected-window))
	       (split-window (selected-window) navi-listing-window-height nil))
	   (select-window (next-window))))
     (if (not (get-buffer bufname))
	 (progn
	   (find-file-noselect (navi-assoc-get-value bufname navi-pair-alist))))
     (switch-to-buffer bufname)
     (goto-char (point-min))
;     (re-search-forward (concat "^[ \t]*" subname) (point-max) nil)
     (re-search-forward (concat "^" subname "$") (point-max) nil)
     (set-window-start nil (match-beginning 0))
     (goto-char (match-beginning 0))
     (if navi-flag
	 (select-window (previous-window))))))

(defun navi-show-sub-other-window-c (bufname subname navi-flag)
  "in case mode name is C or C++."
  (save-restriction
   (save-excursion
     (if navi-flag
	 (select-window (next-window)))
     (if (not (get-buffer bufname))
	 (progn
	   (find-file-noselect (navi-assoc-get-value bufname navi-pair-alist))))
     (switch-to-buffer bufname)
     (goto-char (point-min))
     (navi-show-sub-other-window-c-recurse subname)
     (set-window-start nil (match-beginning 0))
     (goto-char (match-beginning 0))
     (if navi-flag
	 (select-window (previous-window))))))

(defun navi-show-sub-other-window-c-recurse (subname)
  (let ((my-found-point)(my-end-point))
    (setq my-found-point (re-search-forward (concat "^" subname "$") (point-max) nil))
    (end-of-line)
  (setq my-end-point (point))
    (goto-char my-found-point)
    (if (re-search-forward ";" my-end-point t)
	(navi-show-sub-other-window-c-recurse subname))))

(defun navi-next ()
  (interactive)
  (forward-line))

(defun navi-next-show ()
  (interactive)
  (navi-next)
  (navi-open-other-window))

(defun navi-previous ()
  (interactive)
  (forward-line -1))

(defun navi-previous-show ()
  (interactive)
  (navi-previous)
  (navi-open-other-window))

(defun navi-edit ()
  (interactive)
  (navi-open-other-window)
  (select-window (next-window)))

(defun navi-edit-quit ()
  (interactive)
  (navi-open-other-window)
  (navi-quit))

(defun navi-mode ()
  "Subroutine Navigator
Navigate subroutines"
  (interactive)
  (kill-all-local-variables)
  (use-local-map navi-mode-map)
  (setq mode-name "Navi")
  (setq major-mode 'navi-mode)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)

  (setq buffer-file-name navi-listing-window-name)
  (setq buffer-read-only nil)
  (navi-listing-window-clear-internal-data (buffer-name))
  (navi-overlay-buffer)
  (set-buffer-modified-p nil)

  (run-hooks 'navi-mode-hook)
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun navi (b)
  "list function daclarlatin and jump to it."
  (interactive "b")
  (delete-other-windows)
  (split-window (selected-window) navi-listing-window-height nil)
  (select-window (next-window))
  (if (not (equal "Navi" (buffer-name (get-buffer b))))
      (save-restriction
		(save-excursion
		  (let ((outname navi-listing-window-name)
				(inname (buffer-name (get-buffer b)))
				(end (point-max))
				(start (point-min))
				(subexp ""))
			(setq navi-search-mode-name mode-name)
			(setq subexp (cdr (assoc-ignore-case mode-name navi-regexp-alist)))
			(if (equal nil subexp)
				(setq subexp (cdr (assoc-ignore-case "Text" navi-regexp-alist))))
			(navi-lines-clean-buffer navi-search-mode-name outname inname)
			(if (get-buffer outname)
				(display-buffer (get-buffer outname))
			  (display-buffer (get-buffer-create outname)))
			(goto-char start)
			(navi-overlay-make-faces)
			(while (re-search-forward subexp end t)
			  (navi-print inname outname (buffer-substring (match-beginning 1) (match-end 1))))
			(navi-set-alist)))))
  
  (select-window (next-window))
  (navi-mode)
  
; on mule2.3(emacs19) works fine , but on emacs20 it wouldn't worked.
; display-buffer effects windows size.
; examin window height of listing-window , and shrink it.
  (let ((naviHeight (window-height)))
    (if (> naviHeight navi-listing-window-height)
		(shrink-window (- naviHeight navi-listing-window-height) nil)))
  (navi-overlay-buffer)
  (goto-char (point-min)))

(defun navi-print (inname outname str)
  (save-restriction
    (save-excursion
      (set-buffer (get-buffer outname))
      (if buffer-read-only
	  (setq buffer-read-only nil))
      (insert (format navi-listing-format navi-search-mode-name inname str)))))

(defun navi-overlay-buffer ()
  "paint color on listing-window."
  (interactive)
  (if (functionp 'make-overlay)
	  (let (ov-mode ov-file ov-func 
					(start 0)
					(end   0)
					(bufname "")
					(subname "")
					(nbuf (get-buffer navi-listing-window-name)))
		(beginning-of-buffer)
		(while (re-search-forward navi-listing-search-exp (point-max) t)
		  (progn
			(setq ov-mode  (make-overlay (match-beginning 1) (match-end 1)))
			(setq ov-file  (make-overlay (match-beginning 2) (match-end 2)))
			(setq ov-func  (make-overlay (match-beginning 3) (match-end 3)))
			(overlay-put ov-mode 'face 'navi-mode)
			(overlay-put ov-file 'face 'navi-file)
			(overlay-put ov-func 'face 'navi-func))))))

(defun navi-overlay-make-faces ()
  (if (functionp 'make-overlay)
	  (progn
		; style of mode
		(make-face 'navi-mode)
		(copy-face 'bold 'navi-mode)
		(set-face-foreground  'navi-mode navi-color-mode)
		; style of file
		(make-face 'navi-file)
		(copy-face 'bold 'navi-file)
		(set-face-foreground  'navi-file navi-color-file)
		; style of function definition line
		(make-face 'navi-func)
		(copy-face 'bold 'navi-func)
		(set-face-foreground  'navi-func navi-color-func))))

(defun navi-lines-clean-buffer (modename bufname filename)
  "delete all lines of listing-window."
  (save-restriction
    (save-excursion
      (if (get-buffer bufname)
	  (progn
	    (set-buffer (get-buffer bufname)); bufname is allways "Subroutine Navigator",,,, don't mind.
	    (if buffer-read-only
		(setq buffer-read-only nil))
	    (goto-char (point-min))
	    (let ((searchStr (regexp-quote (format navi-listing-clean-format navi-search-mode-name filename ""))))
	      (while (re-search-forward searchStr end t)
		(save-excursion
		  (let ((end 0)(start 0))
		    (beginning-of-line)
		    (setq start (point))
		    (end-of-line)
		    (forward-char)
		    (setq end (point))
		    (delete-region start end)
		    (beginning-of-line)
		    (setq start (point))
		    (goto-char start))))))))))

; TKS.
(defun navi-hide-window ()
  (interactive)
  (enlarge-window (* -1 (+ (window-height) 1))))

; TKS.
(defun navi-listing-widow-sort ()
  "sort listing-window."
  (interactive)
  (save-excursion
    (setq buffer-read-only nil)
    (sort-lines nil (point-min) (point-max))
    (setq buffer-read-only t)
    (navi-overlay-buffer)))

(defun navi-listing-widow-sort-reverse ()
  "sort listing-window."
  (interactive)
  (save-excursion
    (setq buffer-read-only nil)
    (sort-lines t (point-min) (point-max))
    (setq buffer-read-only t)
    (navi-overlay-buffer)))

(defun navi-listing-widow-save-file ()
  "save listing-window to a file."
  (interactive)
  (if (equal navi-listing-mode-name mode-name)
      (progn
	(let ((filename nil))
	  (setq filename (read-file-name "Navi SaveFile:"))
	  (if filename
	      (progn
		(navi-assoc-expand navi-listing-window-name "navi-pair-alist" navi-pair-alist)
		(setq buffer-file-name navi-listing-window-name)
		(write-region (point-min) (point-max) filename)
		(navi-listing-window-clear-internal-data navi-listing-window-name)
		(set-buffer-modified-p nil)
		(message "wrote")
		)
	    (message "need filename."))))))


(defun navi-listing-widow-read-file ()
  "restore listing-window from a file."
  (interactive)
  (if (equal navi-listing-mode-name mode-name)
      (progn
	(let ((filename nil))
	  (setq filename (read-file-name "Navi ReadFile:"))
	  (if (file-exists-p filename)
	      (progn
		(setq buffer-read-only nil)
		(kill-region (point-min) (point-max))
		(insert-file-contents filename)
		(setq buffer-file-name navi-listing-window-name)
		(navi-listing-widow-read-file-parse-buffer)
		(navi-listing-window-clear-internal-data navi-listing-window-name)
		(set-buffer-modified-p nil)
		(setq buffer-read-only t)
		(navi-overlay-buffer)
		(goto-char (point-min))))))))

(defun navi-listing-widow-read-file-parse-buffer ()
  "only navi-listing-widow-read-file can call this function."
  (let ((myMode  "")(myName  "")(myValue "")(p3start nil)(p3end   nil))
    (goto-char (point-min))
    (setq navi-pair-alist nil)
    (while (re-search-forward navi-listing-search-exp (point-max) t)
	(progn
	  (setq myMode  (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq myName  (buffer-substring (match-beginning 2) (match-end 2)))
	  (setq myValue (buffer-substring (match-beginning 3) (match-end 3)))
	  (setq p3start (match-beginning 3))
	  (setq p3end (match-end 3))
	  (if (equal myMode "Navi")
	      (progn 
		(and (equal myName "navi-pair-alist")
		    (progn
		      (setq navi-pair-alist (navi-assoc-parse-add p3start p3end navi-pair-alist))))
		(and (equal myName "DefFileVersion")
		    (progn 
		      (setq subexp navi-search-exp-lisp)))))))))

(defun navi-assoc-parse-add (start end alist)
  "build internal information.
add a entry to assoc list.
format of data have to 'Navi:navi-parse-alist:key,value'.
returns assoc list."
  (let ((asKey "")(asValue ""))
    (save-excursion
      (goto-char start)
      (if (not (re-search-forward "\\(.+\\),\\(.+\\)" nil t))
          nil
        (if (not (= (match-end 2) end))
            nil
          (setq asKey (buffer-substring (match-beginning 1) (match-end 1)))
          (setq asValue (buffer-substring (match-beginning 2) (match-end 2)))
          (cons (cons asKey asValue) alist))))))

; TKS
; by "^","v", shrink widow size.
;;
;; windou-height
;;
(defvar navi-window-height-line 1)
(defun navi-window-height-up()
  (interactive)
  (if (one-window-p)
      nil
	(progn
	  (if (> (+ (window-height) navi-window-height-line navi-listing-window-height 2)
			 (frame-height))
		  nil  ;; (message "hogehoge")
		(enlarge-window navi-window-height-line)))))

(defun navi-window-height-down()
  (interactive)
  (if (one-window-p)
      nil
	(progn
	  (if (<= (- (window-height) navi-window-height-line) navi-listing-window-height)
		  nil  ;; (message "hogehoge")
		(shrink-window navi-window-height-line)))))

(defun navi-assoc-replace(key newval alist)
  (setq alist (delq (assoc key alist) alist))
  (setq alist (cons (cons key newval) alist)))

(defun navi-assoc-get-value(key alist)
  (cdr (assoc key alist)))

(defun navi-set-alist ()
  (let ((bufferName nil)(bufferFile nil))
    (setq bufferName (buffer-name))
    (setq bufferFile (buffer-file-name))
    ;(message (format "%s:%s" bufferName bufferFile))
    (setq navi-pair-alist (navi-assoc-replace bufferName bufferFile navi-pair-alist))))

(defun navi-listing-window-clear-internal-data (bufname)
  "delete internal information . mark is leading string 'Navi:'"
  (let ((myWriteOnly nil))
    (switch-to-buffer bufname)
    (setq myWriteOnly buffer-read-only)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward "^Navi:" (point-max) t)
      (save-excursion
	(let ((end 0)(start 0))
	  (beginning-of-line)
	  (setq start (point))
	  (end-of-line)
	  (forward-char)
	  (setq end (point))
	  (delete-region start end)
	  (goto-char (point-min)))))
    (setq buffer-read-only myWriteOnly)))

(defun navi-assoc-expand (bufname assocname assocvalue)
  "extract internal information , relation of a file and a buffer , to top of current window."
  (let ((alist assocvalue)(str nil)(myCar nil)(myCdr nil)(myWriteOnly nil))
    (setq myWriteOnly buffer-read-only)
    (setq buffer-read-only nil)
    ; clear version info
    (goto-char (point-min))
    (while (re-search-forward "^Navi:DefFileVersion" (point-max) t)
      (save-excursion
	(let ((end 0)(start 0))
	  (beginning-of-line)
	  (setq start (point))
	  (end-of-line)
	  (forward-char)
	  (setq end (point))
	  (delete-region start end)
	  (goto-char (point-min)))))
    (goto-char (point-min))
    (insert (format "Navi:DefFileVersion:%s\n" navi-version-deffile))
    (while (setq str (car alist))
      (setq alist (cdr alist))
      (setq myCar (car str))
      (setq myCdr (cdr str))
      (insert (format "Navi:%s:%s,%s\n" assocname myCar myCdr)))
    (setq buffer-read-only myWriteOnly)))


(defun navi-assoc-test (key)
  (interactive "sKey:")
  (message (navi-assoc-get-value key navi-pair-alist)))

(defun navi-listing-window-delete-line ()
  (interactive)
  (setq buffer-read-only nil)
  (let ((start)(end))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (delete-region start end))
  (setq buffer-read-only t))

(defun navi-listing-window-delete-file ()
  (interactive)
  (setq buffer-read-only nil)
  (let ((start)(end)(modename nil)(filename nil))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (if (re-search-forward navi-listing-search-exp (point-max) t)
	(progn
	  (setq modename (buffer-substring (match-beginning 1) (match-end 1)))
	  (setq filename (buffer-substring (match-beginning 2) (match-end 2)))))
    (and (not (equal modename nil))
	 (not (equal filename nil))
	 (progn
	   (navi-lines-clean-buffer modename (buffer-name) filename))))
  (setq buffer-read-only t))

(defun navi-delete-other-windows ()
  (interactive)
  (if (one-window-p)
      nil
	(delete-other-windows)))

(provide 'navi)
(run-hooks 'navi-load-hook)	; for your customizations
