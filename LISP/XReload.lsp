(vl-load-com)
;; End Undo  -  Lee Mac
;; Closes an Undo Group. 
(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

(defun C:XReload ( / *error* saveVars restoreVars vars items it _name block err nb)
	(princ " :: XReload v0.1.2 - © Voulz\n")
	
;;;;; Error Handling ;;;;;
	(defun *error* ( msg )
		(restoreVars vars)
		(LM:endundo (vla-get-activedocument (vlax-get-acad-object)))
		(if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
			(princ (strcat "\nError: " msg))
		)
		(princ (strcat "\nError: " msg))
		(princ)
	);; -- *error* -- ;;
	
;;;;; Save the given Variables ;;;;;
	(defun saveVars ( var / result)
		(while (car var)
			(setq result (append result (list (cons (strcase (car var)) (getvar (car var)))))
					var (cdr var))
		)
		result
	);; -- saveVars -- ;;

;;;;; Restore the given Variables ;;;;;
	(defun restoreVars ( vars varname / it)
		(if varname ;if we provided a varname, find and reset the one that is matching
			(if (and (setq it (assoc (strcase varname) vars))
						(cdr it))
				(setvar (car it) (cdr it)))
			;else reset them all
			(while (setq it (car vars))
				(if (cdr it) ;if the value is not nil
					(setvar (car it) (cdr it)) )				
				(setq vars (cdr vars))
			)
		)
	);; -- restoreVars -- ;;
	
;;;;; Actual Reloading
	(setq vars (saveVars (list "VISRETAIN"))
			nb 0)
	(vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
	(initget "Y N  _Yes No")
	(if  (=	"No" (getkword "\n Retain Layer Overrides ? [Yes/No]: <Yes> ")) (setvar "VISRETAIN" 0))
	
	(setq items (ssget "I" '((0 . "INSERT"))))
	(while (> (sslength items) 0) (progn
		(setq it (ssname items 0)
			_name (vla-get-name (vlax-ename->vla-object it))
			block (vla-Item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) _name))
		(if (= :vlax-true (vla-get-isxref block))
			(if (vl-catch-all-error-p (setq err (vl-catch-all-apply 'vla-reload (list block))))
				(princ (strcat "\nError reloading XRef '" _name "': " (vl-catch-all-error-message err) ))
				(setq nb (1+ nb))
			)
		)
		(ssdel it items)
	)); end while
	
	(restoreVars vars nil)
	(sssetfirst nil nil)
	(vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
	(princ (strcat "\n " (itoa nb) " XRef" (if (> nb 1) "s" "") " reloaded."))
	(princ)
)

(princ "\n >> Loading: XReload  v0.1.2 - © Voulz")
