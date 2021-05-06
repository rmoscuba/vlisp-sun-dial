;;; =============================================================================;
;;;  � 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  � 2006               rortega@vertice.cu, rmortega77@yahoo.es        	;
;;;  � 2006  �` V�rtice� Empresa de Dise�o y Proyectos de Ingenier�a     	 ;
;;;                  Rutinas para el dibujo de un reloj de Sol           	 ;
;;; =============================================================================;

;;; ============================= Inicializaci�n ================================;

;;; ================================ Funciones ==================================;

;;; ============================== Procedimientos ===============================;

;;; =============================================================================;
;;; Funci�n rj:BorraSeguro borra los objetos que no se hayan borrado antes en la ;
;;; secuencia de reacciones. Se llama solo desde el evento :vlr-commandEnded	 ;
;;; =============================================================================;
;;; Se le pasa el nombre del comando terminado justo antes de ser llamada	 ;
;;; =============================================================================;
(defun rj:BorraSeguro (comandoActivo / Propietario trl)
  (if (not (equal
	     (strcase (substr comandoActivo 1 5))
	     "GRIP_"
	   )
      )
    (progn
      (if *Se-pueden-borrar*
	(foreach Elemento *Se-pueden-borrar*
	  (if (not (vlax-erased-p Elemento))
	    (vla-erase Elemento)
	  )
	)
      )
      (setq *Se-pueden-borrar* nil)

      (setq trl (assoc :VLR-OBJECT-REACTOR (vlr-reactors)))

      (if trl
	(setq trl (cdr trl))
      )
      (while trl
	(progn
	  (foreach Propietario *PropietarioReactoresARemover*
	    (if	Propietario
	      (vlr-owner-remove (car trl) Propietario)
	    )
	  )
	  (setq trl (cdr trl))
	)
      )

    )
  )
)

;;; =============================================================================;
;;; Funci�n rj:BorrarLineas borra todas las l�neas de sombra y nomon del reloj   ;
;;; =============================================================================;
;;; Se le pasa el puntero al reactor asociado al reloj, a partir del que se	 ;
;;; obtienen la lista asociada de l�neas a ser borradas.			 ;
;;; =============================================================================;
(defun rj:BorrarLineas (reactor / datosReactor tiles tile)
  (if (setq datosReactor (vlr-data reactor))
    (progn
      ;; Las l�neas de sombra guardadas en el reactor				 ;
      (setq tiles (cdr (assoc 100 datosReactor)))
      ;; Borra todas las l�neas de sombra					 ;
      (foreach tile tiles
	(if (and (null (member tile *Se-pueden-borrar*))
		 (not (vlax-erased-p tile))
	    )
	  (progn
	    (vla-put-visible tile 0)
	    (setq *Se-pueden-borrar* (cons tile *Se-pueden-borrar*))
	  )
	)
      )
      (vlr-data-set reactor nil)
    )
  )
)

;;; =============================================================================;
;;; Funci�n rj:comienzaComando se llama al comenzar cualquier reacci�n		 ;
;;; =============================================================================;
;;; Inicializamos todo para el tratamiento de la secuencia de eventos		 ;
;;; =============================================================================;
(defun rj:comienzaComando (reactor listaComandos)
  ;; Establecemos las variables globales a nil					 ;
  (setq	*reactoresABorrar*
	 nil
	*reactoresACambiar*
	 nil
  )
)

;;; =============================================================================;
;;; Funci�n rj:borranCirculoReloj evento si borran el c�rculo del reloj		 ;
;;; =============================================================================;
;;; Entonces guardamos el reactor en la lista *reactoresABorrar*		 ;
;;; =============================================================================;
(defun rj:borranCirculoReloj (circuloReloj reactor parameterList)
  (setq	*reactoresABorrar*	       (cons reactor *reactoresABorrar*)
	*PropietarioReactoresARemover* (cons circuloReloj
					     *PropietarioReactoresARemover*
				       )
  )
)

;;; =============================================================================;
;;; Funci�n rj:cambianCirculoReloj evento si cambian el c�rculo del reloj	 ;
;;; =============================================================================;
;;; Entonces guardamos el reactor en la lista *reactoresABorrar*		 ;
;;; =============================================================================;
(defun rj:cambianCirculoReloj (circuloReloj reactor parameterList)
  (setq	*reactoresACambiar* (cons reactor *reactoresACambiar*)
  )
)

;;; Hay que modificarla todav�a							 ;
;;; =============================================================================;
;;; Funci�n rj:terminaComando evento cunado termina la secuencia de eventos	 ;
;;; =============================================================================;
;;; Aqu� es donde respondemos una vez que el comando de usuario ha terminado	 ;
;;; =============================================================================;
(defun rj:terminaComando (reactor listaComandos	     /
				  objReactor	     reactorACambiar
				  datosReactor	     circuloACambiar
				  rj_datosReloj
				 )
  (cond
    ;; Se borr� el c�rculo o c�rculos  *reactoresABorrar*)			 ;
    ;; entonces, borrar todas las l�neas de sombra y el nomon			 ;
    (*reactoresABorrar*
     (foreach objReactor *reactoresABorrar*
       (rj:BorrarLineas objReactor)
     )
     (setq *reactoresABorrar* nil)
    )

    ;; Se cambi� el reloj (Move, Rotate, etc.)					 ;
    ;; entonces borrar las l�neas y el nomon					 ;
    ;; y dibujarlas de nuevo							 ;
    (*reactoresACambiar*
     (foreach reactorACambiar *reactoresACambiar*

       (progn ;; Tomamos los datos del reactor					 ;
	     (setq datosReactor
		    (vlr-data reactorACambiar)
	     )

	 (setq circuloACambiar (car (vlr-owners reactorACambiar)))

	 ;; Primero borramos todas las l�neas y el nomon del reloj		 ;
	 (rj:BorrarLineas reactorACambiar)

	 ;; Obtenemos el nuevo centro del c�rculo				 ;
	 (setq rCentro
		(vlax-safearray->list
		  (vlax-variant-value
		    (vla-get-center circuloACambiar)
		  )
		)
	 )

	 ;; Obtenemos el nuevo di�metro del c�rculo				 ;
	 (setq rDiametro (vla-get-diameter circuloACambiar))

	 ;; Restablecer los datos del reloj					 ;
	 (setq rj_datosReloj
		(list
		  (cons 3 (cdr (assoc 3 datosReactor)))
		  (cons 10 rCentro)
		  (cons 40 rDiametro)
		  (cons 42 (cdr (assoc 42 datosReactor)))
		  (cons 43 (cdr (assoc 43 datosReactor)))
		  (cons 44 (cdr (assoc 44 datosReactor)))
		  (cons 50 (cdr (assoc 50 datosReactor)))
		  ;;(cons 100 ListaLineas)
		  ;;(cons 102 Circulo)
		)
	 )

	 ;; Usamos ahora la lista rj_datosReloj, para redibujar y construir la	 ;
	 ;; lista de lineas y el nomon						 ;
	 (setq rj_datosReloj
		(append	rj_datosReloj
			(jr:dibujaLineas rj_datosReloj)
		)
	 )

	 ;; Asociamos los nuevos datos al reactor				 ;
	 (vlr-data-set reactorACambiar rj_datosReloj)
       )
     )
     ;; borramos las referencias a las varialbles temporales usadas		 ;
     (setq *circuloACambiar*     nil
	   *reactoresACambiar* nil
     )
    )
  )
  ;; borramos los elementos guardados en *Se-pueden-borrar*			 ;
  (rj:BorraSeguro (car listaComandos))
  (setq *PropietarioReactoresARemover* nil)
  (princ)
)


;;; =============================================================================;
;;; Funci�n rj:limpiaReactores restablece los reactores de autocad		 ;
;;; =============================================================================;
(defun rj:limpiaReactores ()
  (setq	*commandReactor* nil
	*DrawingReactor* nil
	)

  (mapcar 'vlr-remove-all
	  '(:VLR-AcDb-reactor		 :VLR-Editor-reactor
	    :VLR-Linker-reactor		 :VLR-Object-reactor
	    :VLR-Command-Reactor	 :VLR-DeepClone-Reactor
	    :VLR-DocManager-Reactor	 :VLR-DWG-Reactor
	    :VLR-DXF-Reactor		 :VLR-Editor-reactor
	    :VLR-Insert-Reactor		 :VLR-Linker-Reactor
	    :VLR-Lisp-Reactor		 :VLR-Miscellaneous-Reactor
	    :VLR-Mouse-Reactor		 :VLR-Object-Reactor
	    :VLR-SysVar-Reactor		 :VLR-Toolbar-Reactor
	    :VLR-Undo-Reactor		 :VLR-Wblock-Reactor
	    :VLR-Window-Reactor		 :VLR-XREF-Reactor
	    )
	  )
  )

;;; =============================================================================;
;;; Funci�n rj:limpiaTodosReactores restablece los reactores al cerrar AutoCad	 ;
;;; =============================================================================;
(defun rj:limpiaTodosReactores (reactor listaComandos)
  (terpri)
  (princ (list 'rj:limpiaReactores reactor listaComandos))
  (terpri)
  (princ (setq datosReactor (vlr-data reactor)))
  (terpri)
  (princ (list "Se ha ejecutado> " listaComandos))
  (rj:limpiaReactores)
)

;;; rmortega77@yahoo.es								 ;