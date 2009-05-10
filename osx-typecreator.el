;;; osx-typecreator.el --- Set OSX type and creator
;;
;; ~harley/share/emacs/pkg/osx/osx-typecreator.el ---
;;
;; $Id: osx-typecreator.el,v 1.9 2005/03/07 01:07:22 harley Exp $
;;

;; Author:    Harley Gorrell <harley@mahalito.net>
;; URL:       http://www.mahalito.net/~harley/elisp/osx-typecreator.el
;; License:   GPL v2
;; Keywords:  osx, type, creator

;;; Commentary:
;; * Sets the type and creator on OSX systems based on major-mode or
;;   buffer-file-name.
;; * Yea, I know that T&C info is being phased out of OSX,
;;   but the process isn't complete yet.
;; * Kinda odd that OSX is stripping out resouce forks while Linux,
;;   BSD and NFSv4 are in the process of adding extended attributes
;;   to their filesystems.

;;; Installation:
;;  Put this file somewhere in your load path and add the following to '~/.emacs':
;;   (when (eq system-type 'darwin)
;;     ;; Optionally you may want this line
;;     ;; (add-hook 'after-save-hook 'osx-tc-save-buffer-tc)
;;     (require 'osx-typecreator))
;;   ;; To add your own T&C info add lines like:
;;   (add-to-list 'osx-tc-defaults-by-mode '(foo-mode    "FOOT" "FOOC"))
;;   (add-to-list 'osx-tc-defaults-by-name '("\\.foo\\'" "FOOT" "FOOC"))

;;; History:
;;  2003-03-15 : Given to friends.
;;  2003-03-16 : Updated URL and contact info.

;;; Code:

(defvar osx-tc-defaults-by-mode
  ;; mode                 type   creator
  '((lisp-mode           "TEXT" "CCL2") ;; Macintosh Common Lisp
    (emacs-lisp-mode     "TEXT" "CCL2")
    (html-mode           "TEXT" "MOZZ") ;; Mozilla
    ;; (html-mode           "TEXT" "iCAB")
    ;; (html-mode           "TEXT" "MSIE")
    ;; uncommenting fundamental-mode might be too liberal
    ;; (fundamental-mode    ""     ""    ) ;; don't set tc
    ;; (fundamental-mode    "TEXT" "ttxt") ;; simple text
    ;; (fundamental-mode    "TEXT" "R*ch") ;; bbedit
    ;; (fundamental-mode    "TEXT" "MPW ") ;; MPW
    )
  "A mapping of the current `major-mode' to an OSX type and creator.
Types or creators of 'nil' or '\"\"' are not set.
The first match is used.
The major mode is checked before the filename.")

(defvar osx-tc-defaults-by-name
  '(("\\.te?xt"          "TEXT" "R*ch") ;; or one of the above creators
    ("\\.gif\\'"         "GIFf" "GKON")
    ("\\.png\\'"         "PNGf" "GKON")
    ("\\.mp3\\'"         "MP3 " ""    )
    ("\\.pdf\\'"         "PDF " ""    )
    ("\\.sit\\'"         "SITD" "SITx")
    ("\\.\\(cl\\|lisp\\)\\'" "TEXT" "CCL2")
    )
  "A mapping of the variable `buffer-file-name' to an OSX type and creator.
Sometimes you will be editing a binary file which doesn't really
have a mode.  Guess as to what the T&C should be from the name.
See `osx-tc-defaults-by-mode'.")

(defvar osx-setfile-program "SetFile"
  "The path to the SetFile program.
This is normally installed with the Developer Tools as /Developer/Tools/SetFile.")

(defvar osx-getfileinfo-program "GetFileInfo"
  "The path to the GetFileInfo program.
This is normally installed with the Developer Tools as /Developer/Tools/GetFileInfo.")

(defvar osx-tc-file-type nil
  "The osx file type for the current buffer.")
(make-variable-buffer-local 'osx-tc-file-type)

(defvar osx-tc-file-creator nil
  "The osx file creator for the current buffer.")
(make-variable-buffer-local 'osx-tc-file-creator)

;;;;;;;;;;

(defun osx-tc-get-tcinfo-by-name (filename)
  "Find the type and creator info by FILENAME."
  (let ((maplst osx-tc-defaults-by-name))
    (while (and maplst (not (string-match (caar maplst) filename)))
      (setq maplst (cdr maplst)) )
    (car maplst)))
;; (osx-tc-get-tcinfo-by-name "foo.gif")

(defun osx-tc-guess-file-type ()
  "Get the default type for this buffer."
  (cadr (or (assq major-mode osx-tc-defaults-by-mode)
            (osx-tc-get-tcinfo-by-name buffer-file-name) )))
;; (osx-tc-guess-file-type)

(defun osx-tc-guess-file-creator ()
  "Get the default type for this buffer."
  (caddr (or (assq major-mode osx-tc-defaults-by-mode)
             (osx-tc-get-tcinfo-by-name buffer-file-name))))
;; (osx-tc-guess-file-creator)

(defun osx-tc-buffer-type ()
  "Get or guess the the OSX file type of the buffer."
  (or osx-tc-file-type (osx-tc-guess-file-type)))

(defun osx-tc-buffer-creator ()
  "Get or guess the the OSX file creator of the buffer."
  (or osx-tc-file-creator (osx-tc-guess-file-creator)))

;;;;;;;;;;

(defun osx-tc-setfile (filename type creator)
  "For FILENAME set the TYPE and CREATOR ."
  (interactive "fFile: \nMType: \nMCreator: ")
  (let ((opt-c (if (and creator (not (string= creator "")))
                 (concat " -c '" creator "'") ))
        (opt-t (if (and type (not (string= type "")))
                 (concat " -t '" type "'") ))
        (fullname (expand-file-name filename)) )
    (if (or opt-c opt-t)
      (shell-command (concat osx-setfile-program opt-t opt-c
                             " '" fullname "'")) )))

(defun osx-tc-attribs-to-string (attribs)
  "Covert a list of ATTRIBS into a string."
  (apply 'concat
         (mapcar
          (lambda (a)
            (cond
             ((stringp a) a)
             ((symbolp a) (symbol-name a))
             (t (error "Not a string or a symbol"))))
          attribs)))
;; (osx-tc-attribs-to-string '("a" l))

(defun osx-tc-setfile-attributes (filename &rest attribs)
  "Set the ATTRIBS for FILENAME."
  (let ((opt-a (osx-tc-attribs-to-string attribs))
        (fullname (expand-file-name filename)) )
    (message "ATTRIB=%s" opt-a)
    (unless (string= opt-a "")
      (shell-command (concat osx-setfile-program " -a " opt-a 
                             " '" fullname "' ")) )))
;; (osx-tc-setfile-attributes "~/foo" 'l "s")

(defun osx-tc-save-buffer-tc ()
  "Save the OSX type and creator for the file visited by this buffer."
  (interactive)
  (if buffer-file-name
    (osx-tc-setfile buffer-file-name 
                    (osx-tc-buffer-type) (osx-tc-buffer-creator))
    (error "No file for this buffer") ))

(defun osx-tc-setfile-buffer ()
  "Prompt for and save the OSX type and creator for this buffer."
  (interactive)
  (let ((new-t (read-string "OSX type: "    osx-tc-file-type))
        (new-c (read-string "OSX creator: " osx-tc-file-creator)) )
    (setq osx-tc-file-type    new-t
          osx-tc-file-creator new-c)
    (osx-tc-save-buffer-tc)))


(defun osx-tc-getfileinfo (filename)
  "Display the output of GetFileInfo for FILENAME."
  (interactive "fGetFileInfo: ")
  (let ((buf (get-buffer-create " *GetFileInfo*"))
        (resize-mini-windows nil) )
    (save-excursion
      (switch-to-buffer-other-window buf)
      (erase-buffer)
      (let ((fullname (expand-file-name filename)))
        (shell-command (concat osx-getfileinfo-program
                               " '" fullname "' ") buf) ))))

(defun osx-tc-getfileinfo-buffer ()
  "Display the GetFileInfo output for this buffer."
  (interactive)
  (if buffer-file-name
    (osx-tc-getfileinfo buffer-file-name)
    (error "This buffer is not visiting a file")))

;;;;

(defun osx-unlock-buffer (arg)
  "Clear the 'lock' attribute for this buffer.
Lock the buffer when ARG is not nil."
  (interactive "P")
  (unless (buffer-file-name)
    (error "No filename for buffer"))
  (let ((opt-a (if arg "L" "l")))
    (osx-tc-setfile-attributes (buffer-file-name) opt-a)))

;;;;;;;;;;

;; Local Variables:
;; osx-tc-file-type:    "TEXT"
;; osx-tc-file-creator: "R*ch"
;; End:

(provide 'osx-typecreator)

;;; osx-typecreator.el ends here
