(defun C:SetMText ( / items it name block text area)

	(setq items (ssget)
			text nil)
	(if (/= nil items) (progn
		(while (> (sslength items) 0) (progn
			(setq it (ssname items 0)
					vla (vlax-ename->vla-object it))
			(if (vlax-property-available-p vla 'Textstring) (progn ;if has a Textstring
				;(vla-put-textstring vla "")
				;(vla-put-textstring vla "plop")
				(setq text (append text (list vla)))
			))
			(ssdel it items)
		))
		(if (= nil text)
			(print "No MText Selected")
			(progn
				(setq str (getstring T "New Text: "))
				(while (/= nil (vl-string-search "/" str))
					(setq str (vl-string-subst "\\" "/" str))
				)
				(foreach vla text
					(progn
						(vla-put-textstring vla "")
						(vla-put-textstring vla str)
					)
				)
		))
	))
)
