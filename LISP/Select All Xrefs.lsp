(vl-load-com)
(defun C:SelectXRefS (/ xr_blk blk mSS ss)
	(setq xr_blk (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
			mSS (ssadd))

	(vlax-for blk xr_blk
		(if (= (vla-get-IsXRef blk) :vlax-true)(progn
			(setq ss (ssget "_X" (list
										'(0 . "INSERT")
										(cons 2 (vla-get-name blk))
										(cons 410
												(if (eq 1 (getvar 'CVPORT)) ; Current Space only
													(getvar 'CTAB)
													"Model"
												)
										) ))
				)
			(if (/= nil ss)
				(while (> (sslength ss) 0) (progn
					(ssadd (ssname ss 0) mSS)
					(ssdel (ssname ss 0) ss)
				)); end while
			)
	   ))
	)
	(sssetfirst nil mSS)
)
