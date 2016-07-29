(vl-load-com)

(defun c:XXA (/ xrefs file)
	(if (setq xrefs (_xxaSelect)) (progn
		(if (and (setq file (getfiled "Select Reference File" "" "dwg" 0))
					(findfile file))
			(_XXA xrefs file))
	))
);end of XXA

(defun c:-XXA (/ xrefs file)
	(if (setq xrefs (_xxaSelect)) (progn
		(setq file nil)
		(while (not file)
			(setq file (getstring T "Enter Reference Filename: "))
			(if (not (findfile file)) (progn
				(print "File not found")
				(setq file nil)
			))
		)
		(_XXA xrefs file)
	))
);end of XXA

(defun _xxaSelect (/ first xrefs idx en)
	(setq first T
			xrefs nil)
	(while (not xrefs)
		(if first
			(setq xrefs (ssget "I" '((0 . "INSERT"))))
			(progn
				(setq xrefs (ssget '((0 . "INSERT"))))
				(setq first nil)
			))
		
		(if xrefs (progn
			(setq idx -1)
			(repeat (sslength xrefs)
				(setq en (ssname xrefs (setq idx (1+ idx))) )
				(if (not (vlax-property-available-p (vlax-ename->vla-object en) 'Path))	(progn
					;(command "._select" xrefs "_remove" en "")
					(ssdel en xrefs)
					(setq idx (1- idx))
				))
			)
			
			(if (= (sslength xrefs) 0)
				(setq xrefs nil)
				(if first
					(print (sslength xrefs))
					;(sssetfirst nil xrefs)
					;(command "._Pselect" xrefs "")
				)
				;(command "Select" xrefs "")
			)
		))
		(setq first nil)
	)
	
	(if xrefs (progn
		;(command "AI_DESELECT")
		;(command "._select" xrefs "")
	))
	xrefs
)

(defun GetClipBoundary (obj / xdict filter spatial elst ptlst)
	(if (= (type obj) 'ENAME)
		(setq obj (vlax-ename->vla-object obj))
	)
	(setq elst
		(vl-catch-all-apply
			'(lambda ()
				(setq xdict (vla-getextensiondictionary obj))
				(setq filter (vla-getobject xdict "ACAD_FILTER"))
				(setq spatial (vla-getobject filter "SPATIAL"))
				(entget (vlax-vla-object->ename spatial))
			)
		)
	)
	(if (not (vl-catch-all-error-p elst)) (progn
		;; if rectangular, 11 holds the origin, still looking for the ucs, somewhere inside the 40 (elements 13, 14 and 15 holds UCS x vector)
		(foreach x elst
			(if (eq 10 (car x))
				(setq ptlst (cons (cdr x) ptlst))
			)
		)
	))
	ptlst
) ;end

(defun _XXA (xrefs file / xr vla xclip insertedBlock npl oldPick) ;xrefs file 
			
	(command "_.undo" "_begin")
	(setq oldPick (getvar "PICKFIRST"))
	(setvar "PICKFIRST" 0)
	(command "_.UCS" "World")
	
	(while (> (sslength xrefs) 0) (progn
		(setq xr (ssname xrefs 0)
				vla (vlax-ename->vla-object xr)
				xclip (GetClipBoundary vla)
		)
		
		(setq insertedBlock (vla-AttachExternalReference
									  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object) ))
									  file
									  (vl-filename-base file)
									  (vlax-3d-point (vlax-get  vla 'insertionpoint))
									  (vlax-get  vla 'XScaleFactor)
									  (vlax-get  vla 'YScaleFactor)
									  (vlax-get  vla 'ZScaleFactor)
									  (vlax-get  vla 'rotation)
									  :vlax-false))
		
		(if (not (null xclip)) (progn
			(command "_.xclip" xr "" "_P")
			(setq npl (entlast))
			(command "_.xclip" (vlax-vla-object->ename insertedBlock) "" "n" "s" npl)
			(entdel npl)
			(setq npl nil)
		))

		(ssdel xr xrefs)
	)); end while

	(command "_.UCS" "Previous")
	(setvar "PICKFIRST" oldPick)
	(setq oldPick nil)
	(command "_.undo" "_end")

	(setq xrefs nil
			xr nil
			vla nil
			xclip nil
			file nil
			insertedBlock nil
			npl nil
			oldPick nil
	)
);enf od _XXA