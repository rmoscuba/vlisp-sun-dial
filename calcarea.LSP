;;; función RECORRER para ser llamada desde  AutoLisp para mostrar el    ;
;;; uso de las funciones de acceso a la BD de AutoCad                    ;
;;; Recorre toda la base de datos e imprime el nombre que la identifica  ;
;;;  © 2006                Rodolfo M Ortega Santiesteban                 ;
;;;  © 2006               rortega@vertice.cu, rmortega77@yahoo.es        ;

(vl-load-com)

(defun c:sumarea (/ sumaArea)
  (setq acadDocument (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq	modelSpace
	 (vla-get-ModelSpace acadDocument)
  )
  (terpri)
  (setq ent (ssget))
  (setq i 0)
  (while (< i (sslength ent))
    (setq VLAent (vla-handleToObject acadDocument
		   (cdr (assoc 5 (entget (ssname ent i))))
		 )
    )
    (cond
      ((vlax-property-available-p VLAent 'ObjectName)
       (princ (vlax-get-property VLAent 'ObjectName))
      )
    )
    (terpri)
    ;; (vlax-dump-object VLAent)
    (setq i (+ i 1))
  )
)






(defun recorrer	(/ ent)
  (terpri)
  (setq ent (ssget))
  (setq i 0)
  (while (< i (sslength ent))
     (print (ssname ent i))
     (vla-get-Area)
     (setq i (+ i 1))
  )
  (terpri)
)

