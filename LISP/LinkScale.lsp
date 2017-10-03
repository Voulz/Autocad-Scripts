(vl-load-com)

(defun C:LinkScale ( / items block vp err atts Choice Done appname attr pref suff new)

(while (not Done)
	(setq block nil vp nil err nil)
	(if (= 2 (sslength (setq items (ssget)))) (progn
		(while (> (sslength items) 0) (progn
			(setq new (ssname items 0)) ;the actual poly
			(print (cdr (assoc 0 (entget new))))
			(cond 
				((= (cdr (assoc 0 (entget new))) "INSERT")
				 	(print "is insert")
					(if (not block)
						 (setq block new)
						 (setq err T))
				 	(print block))
				((vlax-property-available-p (vlax-ename->vla-object new) 'CustomScale)
				 	(print "has custom scale")
					(if (not vp)
						 (setq vp new)
						 (setq err T))
				 	(print vp))
			)
			(ssdel new items)
		)); end while
		(if err
			(print "You need to select a block and a Viewport")
			(setq Done T) ;if we have one polyline and one Viewport
		)
	)(print "You need to select a block and a Viewport"))
);end while

(setq block (vlax-ename->vla-object block)
		atts (LM:vl-getattributes block)
		appname "Voulz:LinkScale"
		attr (getcfg (strcat "AppData/" appname "/Attribute"))
		pref (getcfg (strcat "AppData/" appname "/Prefix"))
		suff (getcfg (strcat "AppData/" appname "/Suffix"))
		Done nil)
	
(if (or (not attr) (= attr "")) (setq attr "VIEWPORTSCALE"))
(if (not pref) (setq pref ""))
(if (not suff) (setq suff " MTS"))
(print "here")
(while (not Done)
	(initget "Attribute pRefix Suffix")		
	(setq Choice (getkword (strcat "Enter an Option or press Enter to continue "
							"{\"" attr "\", \"" pref "\", \"" suff "\"}"
							"[Attribute/pRefix/Suffix]: ")))
	(cond					
		((= Choice nil) (setq Done T))
		((= Choice "Attribute")
			(while (not Done)
				(print "Attributes of the block :")(print)
				(foreach new atts (princ (strcat "  '" (car new) "'")))
				
				(setq new (getstring T (strcat "\nAttribute of the block to set: <" attr "> ")))
				(if (or (= new "") (= new " "))	(setq new attr))
				(if (not (assoc new atts))
					(print (strcat "The block doesn't have an attribute named '" new "'"))
					(setq Done T
							attr new)
				)
			)
			(setq Done nil)
		 	(setcfg (strcat "AppData/" appname "/Attribute") attr)
		)
		((= Choice "pRefix")
		 	(setq Done (getvar "DYNPROMPT"))	(setvar "DYNPROMPT" 0) ;needed to allow leading and trailing spaces
			(setq new (getstring T (strcat "\nPrefix" (if (not (= "" pref)) " (type space to remove the Prefix)" "") ": <" pref "> ")))
		 	(if (= new "") (setq new pref))
		 	(if (= new " ") (setq new ""))
		 	(setq pref new)
		 	(setcfg (strcat "AppData/" appname "/Prefix") pref)
		 	(setvar "DYNPROMPT" Done) (setq Done nil)
		)
		((= Choice "Suffix")
			(setq Done (getvar "DYNPROMPT"))	(setvar "DYNPROMPT" 0) ;needed to allow leading and trailing spaces
			(setq new (getstring T (strcat "\nSuffix" (if (not (= "" suff)) " (type space to remove the Suffix)" "") ": <" suff "> ")))
		 	(if (= new "") (setq new suff))
		 	(if (= new " ") (setq new ""))
		 	(setq suff new)
		 	(setcfg (strcat "AppData/" appname "/Suffix") suff)
		 	(setvar "DYNPROMPT" Done) (setq Done nil)
		)
	)
)
(if (not (assoc attr atts))
	(print (strcat "The block need to have an attribute named '" attr "' (case sensitive)"))
	(progn
		(LM:vl-setattributevalue block attr (strcat pref
						 		"%<\\AcObjProp Object(%<\\_ObjId "
						 		(itoa(vla-get-ObjectID (vlax-ename->vla-object vp)))
						 		">%).CustomScale \\f \"1:%lu2%ct1%qf2816\">%"
						 		suff))
		(command "REGEN")
		(print "--- Scale Linked !")
))
(princ)
)

;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [vla] VLA Block Reference Object
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

(defun LM:vl-setattributevalue ( blk tag val )
    (setq tag (strcase tag))
    (vl-some
       '(lambda ( att )
            (if (= tag (strcase (vla-get-tagstring att)))
                (progn (vla-put-textstring att val) val)
            )
        )
        (vlax-invoke blk 'getattributes)
    )
)

;; Get Attributes  -  Lee Mac
;; Returns an association list of attributes present in the supplied block.
;; blk - [vla] VLA Block Reference Object
;; Returns: [lst] Association list of ((<Tag> . <Value>) ... )

(defun LM:vl-getattributes ( blk )
    (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att)))
        (vlax-invoke blk 'getattributes)
    )
)

(print ">> LinkScale loaded.")
