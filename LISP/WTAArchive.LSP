(defun TODAY  (/ d yr mo da)
	  (setq d  (rtos (getvar "CDATE") 2 6)
			  yr (substr d 1 4)
			  mo (substr d 5 2)
			  da (substr d 7 2)) ;_ end of setq
	  (strcat yr " " mo "-" da)) ; TODAY

(defun split (str delim / ptr lst)
	(while (setq ptr (vl-string-search delim str))
		(setq lst (cons (substr str 1 ptr) lst))
		(setq str (substr str (+ ptr 2)))
	)
	(reverse (cons str lst))
)
;(setq _WTAArchive_from nil)
;(setq _WTAArchive_to nil)

(defun FindBackward	 (file dir / dirL folder _f)
	(if (not dir) (setq dir (getvar "dwgprefix")))
	
	(setq dirL (split (vl-string-translate "/" "\\" dir) "\\"))
	; remove empty values
	(setq dirL (apply 'append (mapcar '(lambda (item) (if (/= "" item) (list item))) dirL)))
	;check for network path
	(if (= (substr dir 1 2) "\\\\")
		(setq dirL (append (list (strcat "\\\\" (car dirL))) (cdr dirL)))) ;add the backslash back to the first item
	;(print dir)
	;(print dirL)
		
	(setq _f nil)
	
	(while (> (length dirL) 0)
		
		;(print (mapcar '(lambda (x) (if (= x "") ("") (strcat x "\\")) ) dirL))
		(setq folder (apply 'strcat ;apply will 'apply' the said function to all of the items, so here will strcat all of the items of the array
						  ;mapcar will be run on all of the items individually, so we just add the backslash to each item, before strcat everything
						  (mapcar '(lambda (x) (if (= x "") "" (strcat x "\\"))  )
									 dirL)))
		;(print folder)
		(setq _f folder) ;(setq _f (strcat folder file))
		(if (/= nil (findfile (strcat _f file)))
			;if the file exists
			(setq dirL (list)) ;exit the loop
			;else continue to the next one
			(progn
				(setq dirL (reverse (cdr (reverse dirL)))) ;remove last item
				(setq _f nil)
			)
		);end if
	); end while
	;(print dirL)
	;(princ)
	_f
) ;_ end of defun

(defun c:WTAArchive	(/ _folder _settings f default lst i line var _f _to dirL defL)
	(defun *error*(msg) ; for exitting quietly
		(setq *error* nil)
		(princ)
	)
	(princ "----- WTA Archive -- v0.2 -- Voulz © 2015 -----")
	
	(setq _folder (getvar "dwgprefix"))
	(princ (strcat "\n Current File :    " _folder (getvar "dwgname")))

	(setq _settings (FindBackward "settings.archive" nil))
	(if _settings
		(princ (strcat "\n Current Settings File :    " _settings "settings.archive"))
		(progn
			(princ "\n --ERROR-- No Settings File Found !!\nPlease provide a 'settings.archive' file in one of the parent foler.")
		(princ "\n This file should contains data like \n    Model\\,Archive\\ \nwhere the 'Model\\' should be replaced with the working folder and the 'Archive\\' should be replaced to a relative path to the archive folder.") 
		(princ "\n To provide a default archive folder, leave the first path empty like\n    ,Archive\\")
			(exit)
		)
	)
	;(setq f (open "D:/0 - PROJECTS/298 - Aboitiz Lima/Work/settings.archive" "r"))
	
	(setq f (open (strcat _settings "settings.archive") "r"))
	
	(setq default nil)
	(setq lst nil)
	(setq i 0)
	(while (setq line (read-line f))
		(setq var (split line ","))
		(if (= "" (car var)) ; if first element empty, its default
			(setq default (cadr var)) ; car return first, cadr return 2nd
			(progn ;else add to the list
				;(setq lst (cons var lst))

				(setq dirL (strcat _settings (car var)) )
				;(print (substr (strcase _folder) 1 (strlen dirL)))
				(if (= (substr (strcase _folder) 1 (strlen dirL))  (strcase dirL))
					(setq lst (cons
								(list (strlen dirL) (car var) (strcat _settings (cadr var))  )
								lst ))
				)
				;(print (substr (strcase _folder) (1+ (strlen dirL))) )

				;the list is of the form (position of the folder name in the string, src folder,  dest folder)
;;;				(setq lst (cons
;;;								(list (vl-string-search (strcase (car var)) (strcase _folder)) (car var) (cadr var) )
;;;								lst ))
				
;;;				(setq lst (cons
;;;								(list (length (vl-remove-if '(lambda (a) (= a ""))  (split (vl-string-translate "/" "\\" (car var)) "\\")) )
;;;										(car var) (cadr var) )
;;;								lst ))
				;(print (vl-string-search (car var) (getvar "dwgprefix") ))
			)
		)
	)
	(close f)

	;(setq lst (reverse lst))
	(setq lst (vl-sort lst ;sort the list by the position where we found the src folder
			'(lambda (a b)
				(if (= (car a) (car b)) ;if same position, loof for the longest src folder (coz if longer, rule is more precise, so prioritary)
					(> (cadr a) (cadr b))
					(> (car a) (car b))
				)
			)
	))
	(setq lst (vl-remove-if-not '(lambda (a) (car a))  lst)) ;nil is false, so (car (nil)) is false so it will be removed
	(if default (setq lst (reverse (cons (list nil nil (strcat _settings default)) (reverse lst) )) )) ; if the default is set, add at the end of the list

	;(print lst)
	;(print default)
	
	(if (car lst) (progn ;if we have at least one rule
		(setq _f (car lst))
		;if this rule has a working folder defined, use the archive folder
		(setq _to (caddr _f) )
		(if (car _f)
			(princ (strcat "\n Archive Folder :    " _to))
			(princ (strcat "\n Default Archive Folder :    " _to))
		)
		(if (not (vl-file-directory-p _to)) ;if folder not existing
			(progn 
				(princ "\n --ERROR-- Archive folder not existing !!")
				(princ "\n Please make sure the Archive folder is already created.")
				(exit)
			);end else
		);end if
	)(progn ;if no default folder found
		(princ "\n --ERROR-- No Matching rule found in the Settings File !")
		(princ "\n Please provide an appropriate rule in your 'settings.archive' file and try again.")
		(princ "\n This file should contains data like \n    Model\\,Archive\\ \nwhere the 'Model\\' should be replaced with the working folder and the 'Archive\\' should be replaced to a relative path to the archive folder.") 
		(princ "\n To provide a default archive folder, leave the first path empty like\n    ,Archive\\")
		(exit)
	)); enf if (car lst)

	(_WTAArchive _to)

;;;	(while (not Done)
;;;		  (initget "F T R _From To Reset")
;;;		  (setq Choice (getkword "\nWTA Archive - Press enter to archive or [From/To/Reset]: "))
;;;		  (cond
;;;				 ((= Choice nil)
;;;				  		(princ "Gooooooooo")
;;;				  )
;;;				 ((= Choice "From")
;;;				  		(princ "From")
;;;				  )
;;;				 ((= Choice "To")
;;;				  		(princ "To")
;;;				  )
;;;				 ((= Choice "Reset")
;;;				  		(princ "Reset")
;;;				  )
;;;			)
;;;	)
;;;	(_WTAArchive _from _to)
) ;_ end of defun

(defun _WTAArchive (_to / _file _toFile _err) ;_from
	(setq _to (strcat _to (today)))
	(setq _file (strcat (getvar "dwgprefix") (getvar "dwgname")))
	(setq _toFile (strcat _to "\\" (getvar "dwgname")))
	(if (/= nil (findfile _toFile))
		(progn  ;if file exists, do we want to replace ?
			(initget "Y N  _Yes No")
			(if  (=	"Yes" (getkword 	"\n This file was already archived today. Do you want to replace it? [Yes/No]: <No> ") )
				(progn
					(if (vl-file-delete _toFile)
						(if (vl-file-copy _file _toFile)
							(princ (strcat "\n File Archived at : " _toFile))
							(princ "\n Error Copying file")
						)
						(princ "\n Error Deleting file. Might be already Open or Read-Only.")
					) ;_ end of if
				) ; end of replace file
				(princ (strcat "\n The file was not replaced at : " _toFile)) ;_ end of princ
			) ;replace the file ?
		) ;if _toFile exists
		(progn ;else create directory...
			(setq _err nil)
			(if (not (vl-file-directory-p _to)) (progn
				;(princ "\n Create directory...")
				(if (LM:MakeDirectory _to)
					(princ (strcat "\n Directory Created at : " _to))
					(setq _err (strcat " Error Creating Directory at : " _to))
				) ;_ end of if
			))
			;... and copy the file there
			(if (= _err nil)
				(progn
					(if (vl-file-copy _file _toFile)
						(princ (strcat "\n File Archived at : " _toFile))
						(princ "\n Error Copying file")
					) ;_ end of if
				)
				(princ _err) ;else
			)
		) ;_ end of progn
	) ; file already existing ?
	(princ)
) ;_WTAArchive

;;-------------------=={ Make Directory }==-------------------;;
;;                                                            ;;
;;  Creates a directory structure                             ;;
;;------------------------------------------------------------;;
;;  Author: Lee McDonnell, 2010                               ;;
;;                                                            ;;
;;  Copyright © 2010 by Lee McDonnell, All Rights Reserved.   ;;
;;  Contact: Lee Mac @ TheSwamp.org, CADTutor.net             ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  dir - the directory to create                             ;;
;;------------------------------------------------------------;;
;;  Returns:  T if directory creation is successful, else nil ;;
;;------------------------------------------------------------;;

(defun LM:MakeDirectory	 (dir / MakeDirectory folders)
	  ;; © Lee Mac 2010
	  ;; (LM:MakeDirectory "C:\\Folder\\Subfolder")
	  (vl-load-com)
	  (defun	MakeDirectory	(root folders)
			 (if folders
					((lambda	(dir)
							(vl-mkdir dir)
							(MakeDirectory dir (cdr folders))) ;_ end of lambda
						  (strcat root "\\" (car folders)))) ;_ end of if
			 ) ;_ end of defun
	  (if	(setq	folders (LM:str->lst
									 (vl-string-translate "/" "\\" dir)
									 "\\") ;_ end of LM:str->lst
					) ;_ end of setq
			 (MakeDirectory (car folders) (cdr folders))) ;_ end of if
	  (vl-file-directory-p dir)
) ;_ end of defun

;;-------------------=={ String -> List }==-------------------;;
;;                                                            ;;
;;  Separates a string into a list of strings using a         ;;
;;  specified delimiter string                                ;;
;;------------------------------------------------------------;;
;;  Author: Lee McDonnell, 2010                               ;;
;;                                                            ;;
;;  Copyright © 2010 by Lee McDonnell, All Rights Reserved.   ;;
;;  Contact: Lee Mac @ TheSwamp.org, CADTutor.net             ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  str - string to process                                   ;;
;;  del - delimiter by which to separate the string           ;;
;;------------------------------------------------------------;;
;;  Returns:  A list of strings                               ;;
;;------------------------------------------------------------;;

(defun LM:str->lst  (str del / pos)
	  ;; © Lee Mac 2010
	  (if	(setq pos (vl-string-search del str))
			 (cons (substr str 1 pos)
					 (LM:str->lst (substr str (+ pos 1 (strlen del))) del)) ;_ end of cons
			 (list str)) ;_ end of if
	  ) ;_ end of defun


;;;(defun getBase	 (path _from / folders found nFolder)
;;;	  (setq folders (LM:str->lst
;;;								(vl-string-translate "/" "\\" path)
;;;								"\\")) ;_ end of setq
;;;	  (setq found nil)
;;;	  (setq nFolder "")
;;;	  (foreach f  (reverse folders)
;;;			 (if (= found t)
;;;					(setq nFolder (strcat f "\\" nFolder))
;;;					(if (= f _from)
;;;						  (setq found t)) ;_ end of if
;;;	  ;(princ "not found;")
;;;					) ;_ end of if
;;;			 ) ;_ end of foreach
;;;	  (if	(= "" nFolder)
;;;			 nil
;;;			 nFolder) ;_ end of if
;;;) ;(getRoot (getvar "dwgprefix"))


	  ;(setq a (getRoot "M:\\298 - Aboitiz Lima\\Model\\Plop"))
	  ;(setq b (getRoot (getvar "dwgprefix")))
	  ;(setq _from "Work")
	  ;(setq _to (strcat "Work\\Archive\\" (today)))

	  ;(WTAarchive "Work" (strcat "Work\\Archive\\" (today)) )
(princ "Archive Loaded")
 ;|«Visual LISP© Format Options»
(72 3 3 0 T "end of " 60 9 1 0 0 T nil nil T)
;*** DO NOT add text below the comment! ***|;
