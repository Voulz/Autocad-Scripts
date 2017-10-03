(vl-load-com)
;; End Undo  -  Lee Mac
;; Closes an Undo Group. 
(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

(defun C:LinkArea ( / *error* getFromSelection getUserOptions saveVars restoreVars createAreaString findFields AllOfProperty FieldCode
						 overrideText addText replaceText
						 items options txt blk att text obj fcode tmp)
	(princ " :: Link Area v0.3.1 - © Voulz\n")

;;;;; Error Handling ;;;;;
	(defun *error* ( msg )
		(LM:endundo (vla-get-activedocument (vlax-get-acad-object)))
		(if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
			(princ (strcat "\nError: " msg))
		)
		(princ (strcat "\nError: " msg))
		(princ)
	);; -- *error* -- ;;
	
;;;;; Prepare the Selection ;;;;;
	(defun getFromSelection ( / items it name block text area)
		(setq items (ssget))
		(while (> (sslength items) 0) (progn
			(setq it (ssname items 0)
					name (cdr (assoc 0 (entget it))))
			(cond 
				((= name "INSERT") ;if a block
					(setq block (append block (list it))) )
				((vlax-property-available-p (vlax-ename->vla-object it) 'Textstring) ;if has a Textstring
					(setq text (append text (list it))) )
				((vlax-property-available-p (vlax-ename->vla-object it) 'Area) ;if has area
					(setq area (append area (list it))) )
			)
			(ssdel it items)
		))
		(list	(cons 'block block)
				(cons 'text text)
				(cons 'area area))
	);; -- getFromSelection -- ;;
	
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
	
;;;;; get User Options ;;;;;
	(defun getUserOptions ( / *error* vars appname attr conv prec pref suff mode Done new)
		;; Error Handling ;;
		(defun *error* ( msg )
			(restoreVars vars nil)
			(LM:endundo (vla-get-activedocument (vlax-get-acad-object)))
			(if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
				(princ (strcat "\nError: " msg))
			)
			(princ)
		);; -- Error Handling -- ;;
		
		(setq vars (saveVars (list "unitmode" "DYNPROMPT"))
				appname "Voulz:LinkArea"
				attr (getcfg (strcat "AppData/" appname "/Attribute"))
				conv (getcfg (strcat "AppData/" appname "/ConversionFactor"))
				prec (getcfg (strcat "AppData/" appname "/Precision"))
				pref (getcfg (strcat "AppData/" appname "/Prefix"))
				suff (getcfg (strcat "AppData/" appname "/Suffix"))
				mode (getcfg (strcat "AppData/" appname "/Mode")) ;valid Modes: 'Override', 'Add', 'ReplaceOrOverride'
				Done nil)
		(if (or (not attr) (= attr "")) (setq attr "AREA"))
		(if (or (not conv) (= conv "")) (setq conv 1e-6) (setq conv (atof conv)))
		(if (or (not prec) (= prec "")) (setq prec 1) (setq prec (atoi prec)))
		(if (not pref) (setq pref ""))
		(if (not suff) (setq suff ""))
		(if (or (not mode) (= mode "")) (setq mode "ReplaceOrOverride"))
			
		(while (not Done)
			(initget "Attribute ConversionFactor Precision pRefix Suffix Mode")
			
			(setvar "unitmode" 1) ;needed to convert real to string with as few decimal as possible
			(setq new (strcat "Enter an Option "
									"{\"" attr "\", " (rtos conv 2 10) ", " (itoa prec) ", \"" pref "\", \"" suff  "\", \"" mode "\"}"
									"[Attribute/ConversionFactor/Precision/pRefix/Suffix/Mode]: "))
			(restoreVars vars "unitmode") (setq Done nil)
			
			(setq Choice (getkword new)) ;(print Choice)
			(cond					
				((not Choice) (setq Done T))
				((= Choice "Attribute")					
					(setq new (getstring T (strcat "\nAttribute of the block to set (case sensitive): <" attr "> ")))
					(if (or (= new "") (= new " "))	(setq new attr))
					(setq attr new)
				 	(setcfg (strcat "AppData/" appname "/Attribute") attr)
				)
				((= Choice "ConversionFactor")
				 	(setvar "unitmode" 1) ;needed to convert real to string with as few decimal as possible
				 	(setq new (getreal (strcat "\nConversion Factor: <" (rtos conv 2 10) "> ")))
				 	(if new (progn
						(setq conv new)
				 		(setcfg (strcat "AppData/" appname "/ConversionFactor") (rtos conv 2 10))
					))
				 	(restoreVars vars "unitmode")
				)
				((= Choice "Precision")
				 	(setq new (getint (strcat "\nPrecision (-1 for Document Precision): <" (itoa prec) "> ")))
				 	(if new (progn
						(if (< new 0) (setq new -1))
						(setq prec new)
				 		(setcfg (strcat "AppData/" appname "/Precision") (itoa prec))
					))
				)
				((= Choice "pRefix")
				 	(setvar "DYNPROMPT" 0) ;needed to allow leading and trailing spaces
					(setq new (getstring T (strcat "\nPrefix" (if (not (= "" pref)) " (type space to remove the Prefix)" "") ": <" pref "> ")))
				 	(if (= new "") (setq new pref))
				 	(if (= new " ") (setq new ""))
				 	(setq pref new)
				 	(setcfg (strcat "AppData/" appname "/Prefix") pref)
				 	(restoreVars vars "DYNPROMPT")
				)
				((= Choice "Suffix")
					(setvar "DYNPROMPT" 0) ;needed to allow leading and trailing spaces
					(setq new (getstring T (strcat "\nSuffix" (if (not (= "" suff)) " (type space to remove the Suffix)" "") ": <" suff "> ")))
				 	(if (= new "") (setq new suff))
				 	(if (= new " ") (setq new ""))
				 	(setq suff new)
				 	(setcfg (strcat "AppData/" appname "/Suffix") suff)
				 	(restoreVars vars "DYNPROMPT")
				)
				((= Choice "Mode")
					(setq Done nil)
					(while (not Done)
						(setq Done T)
						(initget "Override Add ReplaceOrOverride")
						(setq Choice (getkword (strcat "Mode [Override/Add/ReplaceOrOverride]: <" mode ">")))
						(cond
							((not Choice) (setq Done T))
							((= Choice "Override") (setq mode "Override"))
							((= Choice "Add") (setq mode "Add"))
							((= Choice "ReplaceOrOverride") (setq mode "ReplaceOrOverride"))
							(T (setq Done nil))
						)
					)
					(setcfg (strcat "AppData/" appname "/Mode") mode)
					(setq Done nil)
				); end of Choice
			); end of cond
		);end of while

		(restoreVars vars nil)
		(list	(cons 'attr attr)
				(cons 'conv conv)
				(cons 'prec prec)
				(cons 'pref pref)
				(cons 'suff suff)
				(cons 'mode mode))
	);; -- getUserOptions -- ;;

;;;;; create the Field String for the Area of the given objects ;;;;;
	(defun createAreaString ( items options / vars area endString txt)
		(setq vars (saveVars (list "unitmode"))
				area (cdr (assoc 'area items)))
		(setvar "unitmode" 1) ;needed to convert real to string with as few decimal as possible

		(setq endString (strcat " \\f \""
						 		"%lu2" ;decimals
						 		(if (not (< (cdr (assoc 'prec options)) 0))
									(strcat "%pr" (itoa (cdr (assoc 'prec options))))
									"")
						 		(if (/= (cdr (assoc 'conv options)) 1)
									(strcat "%ct8[" (rtos (cdr (assoc 'conv options)) 2 10) "]" )
									"")
						 		(if (or (/= (cdr (assoc 'pref options)) "")
										  (/= (cdr (assoc 'suff options)) ""))
									(strcat "%ps[" (cdr (assoc 'pref options)) "," (cdr (assoc 'suff options)) "]")
									"")
						 		"\">%"))
		
		(if (= (length area) 1)(progn
			(setq txt (strcat "%<\\AcObjProp.16.2 Object(%<\\_ObjId "
							 		(itoa (vla-get-ObjectID (vlax-ename->vla-object (car area))))
							 		">%).Area"
							 		endString))
		)(progn
			(setq txt "%<\\AcExpr (" )
			(while (car area)
				(setq txt (strcat txt
								 		"%<\\AcObjProp Object(%<\\_ObjId "
								 		(itoa(vla-get-ObjectID (vlax-ename->vla-object (car area))))
								 		">%).Area>%+"))
				(setq area (cdr area))
			)
			(setq txt (strcat (vl-string-right-trim "+" txt)
							 		")"
							 		endString))
		))
			
		(restoreVars vars nil)
		txt
	);; -- createAreaString -- ;;

;;;;; Create a hierarchical structure of the fields in the object ;;;;;
	(defun findFields ( enx / ent fld pos str lst chld tmp prev)
		(setq str (cdr (assoc 2 enx))
				prev nil
				child nil
				lst (list	(cons 'txt (if (= (cdr (assoc 1 enx)) "_text") str (strcat "%<" str ">%")))
						 		(cons 'type (cdr (assoc 1 enx)))
						 		(cons 'ent enx))
				tmp enx)
		
		(while (assoc 6 tmp)			
			(if (= (cdr (assoc 6 tmp)) "ObjectPropertyId")(progn
				;(print (assoc 0 (entget (cdr (assoc 331 tmp)))))
				;(print (itoa (vla-get-ObjectID (vlax-ename->vla-object (cdr (assoc 331 tmp))))))
				(setq lst (append	lst (list (cons 'id (cdr (assoc 331 tmp)))))) )
			)
			(if (= (cdr (assoc 6 tmp)) "ObjectPropertyName")
				(setq lst (append	lst (list (cons 'prop (cdr (assoc 1 tmp))))))
			)
			(setq tmp (cdr (member (assoc 6 tmp) tmp)))
		)
		(setq tmp (cdr (assoc 300 (member (assoc 7 enx) enx))))
		(if (and tmp (/= tmp ""))
			(setq lst (append	lst (list (cons 'format tmp)))) )
		
		(while (assoc 360 enx)
			(setq ent (assoc 360 enx)
					fld (entget (cdr ent))
					pos (vl-string-search "%<\\_FldIdx" str prev) )
			
			(setq chld (append chld (list (append
														(list (cons 'pos pos))
														(findFields fld)))))
			(setq enx (vl-remove (assoc 360 enx) enx))
			(setq prev (1+ pos))
		)
		(if chld
			(setq lst (append	lst (list (cons 'children chld)))) )
		lst
	);; -- findFields -- ;;

;;;;; Create a fieldcode for the object based on the findFields Output ;;;;;
	(defun FieldCode ( fields / i children child lst pos)
		(if (cdr (assoc 'override fields))
			(cdr (assoc 'override fields)) ;if it has an override, return the override
			(progn
				(if (setq children (cdr (assoc 'children fields))) (progn
					(setq i 0)
					(while (setq child (car children))
						(setq lst (append	lst (list (cons i (FieldCode child)))))
						(setq i (1+ i)
									children (cdr children))
					)
				))

				(if lst (progn ; if it has children, replace them in the string
					(setq str (cdr (assoc 'txt fields)))
					(while (car lst)
						(setq str (vl-string-subst (cdr (car lst))
										 					(strcat "%<\\_FldIdx " (itoa (car (car lst))) ">%")
										 					str)
								lst (cdr lst))
					)
					str
				)(progn
					(if (cdr (assoc 'id fields)) (progn ;if it has an ID, put it in the string
						(setq str (cdr (assoc 'txt fields))
								pos (vl-string-search "%<\\_ObjIdx" str))
						(strcat
							(substr str 1 (+ pos 9)) " "
							(itoa (vla-get-ObjectID (vlax-ename->vla-object (cdr (assoc 'id fields)))))
							(substr str (1+ (vl-string-search ">%" str pos)))
						)
					) (cdr (assoc 'txt fields))) ;if it doesn't have ID or Children, just output its text
				))
			);end of progn "else"
		)
	);; -- FieldCode -- ;;
	
;;;;; Check if the field or all its subfields are of the same property ;;;;;
	(defun AllOfProperty ( fields propName / continue children child )
		(setq continue T)
		;if it has a property name and it is not the right one, then return it
		(if (assoc 'prop fields) 
			(if (/= (strcase (cdr (assoc 'prop fields)))  (strcase propName))
					(setq continue nil))
			(progn
				(if (= "AcExpr" (cdr (assoc 'type fields)))(progn
					(setq children (cdr (assoc 'children fields)))
					(while (and
									 continue
									 (setq child (car children)))
						(setq continue (AllOfProperty child propName)
								children (cdr children))
					)
				)(setq continue nil))
			)
		)		
		continue
	);; -- AllOfProperty -- ;;

;;;;; Return a list of the attributes of a list of blocks matching the givien name ;;;;;
	(defun getAttributes ( blocks attName / b vla atts tmp)
		(setq atts nil)
		(while (setq b (car blocks))
			(setq vla (vlax-ename->vla-object b))
			(if (vlax-method-applicable-p vla 'getattributes)(progn
				(setq tmp (vlax-invoke vla 'getattributes)
						idx (vl-position (strcase attName) (mapcar 'strcase (mapcar 'vla-get-tagstring tmp))))
				(if idx (setq atts (append atts (list (vlax-vla-object->ename (nth idx tmp))))))
			))
			(setq blocks (cdr blocks))
		)
		atts
	);; -- getAttributes -- ;;

;;;;; Override the text of the entity ;;;;;
	(defun overrideText ( ent txt / vla)
		(setq vla (vlax-ename->vla-object ent))
		(vla-put-textstring vla "")
		(vla-put-textstring vla txt)
	);; -- overrideText -- ;;

;;;;; Add the text to the entity ;;;;;
	(defun addText ( ent txt / tmp fields fcode)
		(setq tmp (entget ent)
				tmp (cdr (assoc 360 tmp))
				tmp (dictsearch tmp "ACAD_FIELD"))
		(if tmp (progn ;if it has fields
			(setq tmp (dictsearch (cdr (assoc -1 tmp)) "TEXT")
					fields (findFields tmp)
					fcode (FieldCode fields))
		) (setq fcode (vla-get-textstring (vlax-ename->vla-object ent))) )
		(overrideText ent (strcat fcode txt))
	);; -- addText -- ;;

;;;;; Replace the text of the entity ;;;;;
	(defun replaceText ( ent txt propName / tmp fields fcode children i Done)
		(setq tmp (entget ent)
				tmp (cdr (assoc 360 tmp)) )
		(if (and tmp (setq tmp (dictsearch tmp "ACAD_FIELD")) ) (progn ;if it has fields
			(setq tmp (dictsearch (cdr (assoc -1 tmp)) "TEXT")
					fields (findFields tmp)
					children (cdr (assoc 'children fields)) ;get the children of the field.
					i 0
					Done nil)
			(while (and (not Done) (< i (length children)))
				(if (AllOfProperty (nth i children) propName) ;if all the property are of type "propName", then replace it
					(setq children (subst (append
													(list (cons 'override txt))
													(nth i children))
							  				(nth i children)
							  				children)
							Done T)
				)
				(setq i (1+ i))
			)
			(if Done ;reflect the changes
				(setq fields (subst (cons 'children children)
							  		(assoc 'children fields)
							  		fields)
						fcode (FieldCode fields))
				(setq fcode txt) ;else override
			)
		) (setq fcode txt) )
		(overrideText ent fcode)
	);; -- overrideText -- ;;
	
;;;;;; --------------------------------------------------------------------- ;;;;;;
;;;;;; ------------------ Actual Linking of the Area ----------------------- ;;;;;;
;;;;;; --------------------------------------------------------------------- ;;;;;;
	
	(setq items (getFromSelection))
	(if (and (cdr (assoc 'area items)) ;if there is at least one object with area
				(or (cdr (assoc 'text items)) ;and one text object
					 (cdr (assoc 'block items)) )); or one block object
		(progn
			(setq options (getUserOptions)
					txt (createAreaString items options)
					blk (cdr (assoc 'block items))
					att (getAttributes blk (cdr (assoc 'attr options)))
					text (cdr (assoc 'text items))
				 	obj att)
			(cond					
				((= "Override" (cdr (assoc 'mode options))) ;Override/Add/ReplaceOrOverride
					(vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))		 	
				 	(while (car obj)
						(overrideText (car obj) txt)
						(setq obj (cdr obj))
					)
					(setq obj text)
				 	(while (car obj)
						(overrideText (car obj) txt)
						(setq obj (cdr obj))
					)
					(setq tmp (getvar "cmdecho"))(setvar "cmdecho" 0)(command "regen")(setvar "cmdecho" tmp)
				 	(vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
					(princ "\n")
				)
				((= "Add" (cdr (assoc 'mode options)))
					(vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
				 	(while (car obj)
						(addText (car obj) txt)
						(setq obj (cdr obj))
					)
					(setq obj text)
				 	(while (car obj)
						(addText (car obj) txt)
						(setq obj (cdr obj))
					)
					(setq tmp (getvar "cmdecho"))(setvar "cmdecho" 0)(command "regen")(setvar "cmdecho" tmp)
				 	(vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
					(princ "\n")
				)
				(T
					(vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
				 	(while (car obj)
						(replaceText (car obj) txt "area")
						(setq obj (cdr obj))
					)
					(setq obj text)
				 	(while (car obj)
						(replaceText (car obj) txt "area")
						(setq obj (cdr obj))
					)
					(setq tmp (getvar "cmdecho"))(setvar "cmdecho" 0)(command "regen")(setvar "cmdecho" tmp)
				 	(vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
					(princ "\n")
				)
			);end of cond
			(if (< (length att) (length blk))
				(princ (strcat (itoa (- (length blk) (length att))) " object(s) discarded\n")) )
			(princ (strcat " --- Area of "
								(itoa (length (cdr (assoc 'area items)))) " object" (if (= (length (cdr (assoc 'area items))) 1) "" "s") " linked to "
								(if att (strcat (itoa (length att)) " block" (if (= (length att) 1) "" "s")) "")
								(if (and att text) " and " "")
								(if text (strcat (itoa (length text)) " text" (if (= (length text) 1) "" "s")) "")
								" !"))
		)
		(princ "\n  [ ERROR ] You need to select at least a Block or a MText and an object with an Area (Polyline, Hatch)")
	)
	(princ)
)

(princ "\n >> Loading: LinkArea  v0.3.1 - © Voulz")
