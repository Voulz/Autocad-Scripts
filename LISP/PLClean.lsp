;;  PLClean.lsp [command name: PLCl to close closed PolyLines and weld overlapping vertices]
;;  Voulz, Dec 2015

; DO NOT USE PEDIT, DOESNT OWORK PROPERLY IF NOT VISIBLE ON SCREEN
; (setq coords (vlax-get (vlax-ename->vla-object (ssname (ssget "_A" '((0 . "LWPOLYLINE"))) 0)) 'coordinates))
; (vlax-put obj_vlax 'coordinates coords);replace the coordinate list
; (vlax-put obj_vlax 'closed 1)

; http://adndevblog.typepad.com/autocad/2013/02/change-a-specific-coordinate-in-a-lwpolyline-in-lisp.html
; check entget
; => (setq p (entget (ssname (ssget "I" '((0 . "LWPOLYLINE"))) 0)))
; http://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/remove-polyline-vertex/td-p/849526
; http://www.theswamp.org/index.php?topic=19865.55;wap2
(defun C:PLClean (/ cmde plset pl plverts start i p1 p2 nb nbtotal nbclosed nbpoly a ncoords change l)
	(defun *error*(msg) ; for exitting quietly
		(setq *error* nil)
		(princ)
	)
	
	(princ " ------- Polyline Cleaning -------\n      V 1.0      ©Voulz, 2015")
	
	(if (not (setq plset (ssget "I" '((0 . "LWPOLYLINE")) ))) (progn ; if no preselected polylines
		(initget "Y N _Yes No")
		(if (= (getkword "\n   No polyline selected. Run on all the model ? [Yes/No]: <No>")
				 "Yes") (progn
			(setq plset (ssget "_A" '((0 . "LWPOLYLINE"))))
		)(progn ; -- else
			(princ " --- Please select polylines before running the commnand.")
			(exit)
		))
	)(princ "\n")) ; -- (if (not (setq plset

	(princ (strcat " --- Start of Cleaning over " (itoa (sslength plset)) " Polyline(s)...\n"))
	
	(setq cmde (getvar 'cmdecho))
	(setvar 'cmdecho 0)
	
	(command "_.undo" "_begin")
	(setq nbclosed 0)
	(setq nb 0)
	(while (> (sslength plset) 0) (progn
		(setq pl (ssname plset 0)) ;the actual poly
		(setq l (PSimpleUser pl))
		(if (= (nth 3 l) T) (setq nbclosed (1+ nbclosed)))
		(setq nb (+ nb (nth 2 l)))
		;(mapcar '(lambda(x)(print (car x))(princ (cadr x))) (PSimpleUser pl))
		(ssdel (ssname plset 0) plset)
	)); end while
	(command "_.undo" "_end")
	(setvar 'cmdecho cmde)
	
	(princ (strcat " --- " (itoa nbclosed) " Polyline(s) closed and " (itoa nb) " Vertice(s) removed ---"))
	(princ)
); end defun




;;;=======================[ PSimple.lsp ]======================= 
;;; Author: Charles Alan Butler 
;;; Version:  1.7 Nov. 24, 2007
;;; Purpose: To remove unnecessary vertex from a pline
;;; Supports arcs and varying widths
;;;=============================================================
;; This version will remove the first vertex if it is colinear
;; and first & last arcs that have the same center

;;  command line entry, user selection set pick
(defun c:PSimple () (PSimpleUser nil)(princ))
(defun c:PSimpleV () ; Verbose version
  (mapcar '(lambda(x)(print (car x))(princ (cadr x))) (PSimpleUser nil))
  (princ)
)

;;  User interface Function
;;  flag = nil -> user selects a selection set
;;       = ENAME -> call the routine
;;       = OBJECT -> call the routine
;;       = True   -> User to select a single entity, repeats
(defun PSimpleUser (flag / ss ent)
  (cond
    ((null flag)    ; user selection set pick
     (prompt "\n Select polylines to remove extra vertex: ")
     (if (setq ss (ssget '((0 . "LWPOLYLINE"))))
       (PSimple ss)
     )
    )
    ;;  next two already have an object so pass to the main routine
    ((= (type flag) 'ENAME) (PSimple flag))
    ((= (type flag) 'VLA-object) (PSimple flag))
    (t  ; user single pick with repeat
       (while
         (setq ent (car (entsel "\n Select polyline to remove extra vertex: ")))
          (if (equal (assoc 0 (entget ent)) '(0 . "LWPOLYLINE"))
            (PSimple ent)
            (prompt "\nNot a LWPolyline, Try again.")
          )
       )
    )
  )
)





;;;=======================[ PSimple.lsp ]======================= 
;;; Author: Charles Alan Butler 
;;; Version:  1.7 Nov. 23, 2007
;;; Purpose: To remove unnecessary vertex from a pline
;;; Supports arcs and varying widths
;;;=============================================================
;; This version will remove the first vertex if it is colinear
;; and first & last arcs that have the same center
;; Open plines that have the same start & end point will be closed

;;  Argument: et
;;    may be an ename, Vla-Object, list of enames or
;;    a selection set
;;  Returns: a list, (ename message)
;;    Massage is number of vertex removed or error message string
;;    If a list or selection set a list of lists is returned
(defun PSimple (et / doc result Tan Replace BulgeCenter RemoveNlst ps1)
  (vl-load-com)

  (defun tan (a) (/ (sin a) (cos a)))

  (defun replace (lst i itm)
    (setq i (1+ i))
    (mapcar '(lambda (x) (if (zerop (setq i (1- i))) itm x)) lst)
  )

  
  ;;  CAB 11.16.07
  ;;  Remove based on pointer list
  (defun RemoveNlst (nlst lst)
    (setq i -1)
    (vl-remove-if  '(lambda (x) (not (null (vl-position (setq i (1+ i)) nlst)))) lst)
  )
  
  (defun BulgeCenter (bulge p1 p2 / delta chord radius center)
    (setq delta  (* (atan bulge) 4)
          chord  (distance p1 p2)
          radius (/ chord (sin (/ delta 2)) 2)
          center (polar p1 (+ (angle p1 p2) (/ (- pi delta) 2)) radius)
    )
  )

  ;;  Main function to remove vertex
  ;;  ent must be an ename of a LWPolyline
  (defun ps1 (ent /      aa     cpt    dir    doc    elst   hlst   Remove
                  idx    keep   len    newb   result vlst   x      closed
                  d10    d40    d41    d42    hlst   p1     p2     p3
                  plast  msg)
      ;;=====================================================
      (setq elst (entget ent)
            msg  "")
      (setq d10 (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) elst)))
      (if (> (length d10) 2)
        (progn
          ;;  seperate vertex data
          (setq d40 (vl-remove-if-not '(lambda (x) (= (car x) 40)) elst))
          (setq d41 (vl-remove-if-not '(lambda (x) (= (car x) 41)) elst))
          (setq d42 (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 42)) elst)))
          ;;  remove extra vertex from point list
          (setq plast (1- (length d10)))
          (setq p1 0  p2 1  p3 2)
          (if (and (not (setq closed (vlax-curve-isclosed ent)))
                   (equal (car d10) (last d10) 1e-6))
            (progn
              (setq Closed t ; close the pline
                    elst (subst (cons 70 (1+(cdr(assoc 70 elst))))(assoc 70 elst) elst)
                    msg  " Closed and")
              (if (and (not(zerop (nth plast d42)))(not(zerop (nth 0 d42))))
                (setq d10 (reverse(cdr(reverse d10)))
                      d40 (reverse(cdr(reverse d40)))
                      d41 (reverse(cdr(reverse d41)))
                      d42 (reverse(cdr(reverse d42)))
                      plast (1- plast)
                )
              )
            )
          )
          (setq idx -1)
          (while (<= (setq idx (1+ idx)) (if closed (+ plast 2) (- plast 2)))
            (cond
              ((and (or (equal (angle (nth p1 d10) (nth p2 d10))
                               (angle (nth p2 d10) (nth p3 d10)) 1e-6)
                        (equal (nth p1 d10) (nth p2 d10) 1e-6)
                        (equal (nth p2 d10) (nth p3 d10) 1e-6))
                    (zerop (nth p2 d42))
                    (or (= p1 plast)
                        (zerop (nth p1 d42)))
               )
               (setq remove (cons p2 remove)) ; build a pointer list
               (setq p2 (if (= p2 plast) 0 (1+ p2))
                     p3 (if (= p3 plast) 0 (1+ p3))
               )
              )
              ((and (not (zerop (nth p2 d42)))
                    (or closed (/= p1 plast))
                    (not (zerop (nth p1 d42))) ; got two arcs
                    (equal
                      (setq cpt (BulgeCenter (nth p1 d42) (nth p1 d10) (nth p2 d10)))
                      (BulgeCenter (nth p2 d42) (nth p2 d10) (nth p3 d10))
                      1e-4)
               )
               ;;  combine the arcs
               (setq aa   (+ (* 4 (atan (abs (nth p1 d42))))(* 4 (atan (abs (nth p2 d42)))))
                     newb (tan (/ aa 4.0))
               )
               (if (minusp (nth p1 d42))
                 (setq newb (- (abs newb)))
                 (setq newb (abs newb))
               )
               (setq remove (cons p2 remove)) ; build a pointer list
               (setq d42 (replace d42 p1 newb))
               (setq p2 (if (= p2 plast) 0 (1+ p2))
                     p3 (if (= p3 plast) 0 (1+ p3))
               )
              )
              (t
               (setq p1 p2
                     p2 (if (= p2 plast) 0 (1+ p2))
                     p3 (if (= p3 plast) 0 (1+ p3))
               )
              )
            )
          )
          (if remove
            (progn
              (setq count (length d10))
              ;; Rebuild the vertex data with pt, start & end width, bulge
              (setq d10 (RemoveNlst remove d10)
                    d40 (RemoveNlst remove d40)
                    d41 (RemoveNlst remove d41)
                    d42 (RemoveNlst remove d42)
              )
              (setq result (mapcar '(lambda(w x y z) (list(cons 10 w)
                                        x  y
                                        (cons 42 z))) d10 d40 d41 d42)
              )
              ;;  rebuild the entity data with new vertex data
              (setq hlst (vl-remove-if
                           '(lambda (x) (vl-position (car x) '(40 41 42 10))) elst)
              )
              (mapcar '(lambda (x) (setq hlst (append hlst x))) result)
              (setq hlst (subst (cons 90 (length result)) (assoc 90 hlst) hlst))
              (if (entmod hlst); return ename and number of vertex removed
                (list ent (strcat msg " Vertex removed " (itoa(- count (length d10)))) (- count (length d10)) (not (= msg "")) )
                (list ent " Error, may be on locked layer." 0 F)
              )
            )
            (list ent "Nothing to remove - no colinear vertex." 0 F)
          )
        )
        (list ent "Nothing to do - Only two vertex." 0 F)
      )
    )
  

  ;;  ========  S T A R T   H E R E  ===========
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (cond
    ((or (=(type et) 'ENAME)
         (and (=(type et) 'VLA-object)
              (setq et (vlax-vla-object->ename et))))
      (vla-startundomark doc)
      (setq result (ps1 et))
      (vla-endundomark doc)
     )
    ((= (type et) 'PICKSET)
      (vla-startundomark doc)
      (setq result (mapcar '(lambda(x) (ps1 x))
              (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))))
      (vla-endundomark doc)
    )
    ((listp et)
      (vla-startundomark doc)
      (setq result (mapcar '(lambda(x) (ps1 x)) et))
      (vla-endundomark doc)
    )
    ((setq result "PSimple Error - Wrong Data Type."))
  )
  result
)