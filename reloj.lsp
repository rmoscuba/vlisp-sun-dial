;;; =============================================================================;
;;;  � 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  � 2006               rortega@vertice.cu, rmortega77@yahoo.es        	;
;;;  � 2006  �` V�rtice� Empresa de Dise�o y Proyectos de Ingenier�a     	 ;
;;;                  Rutinas para el dibujo de un reloj de Sol           	 ;
;;; =============================================================================;

;;; ============================== Inicializaci�n ===============================;

;;; =============================== Funciones ===================================;

;;; ============================= Procedimientos ================================;

;;; =============================================================================;
;;; Funci�n jr:dibujaLineas dibujar� las l�neas de sombra del reloj y el nomon	 ;
;;; =============================================================================;
(defun jr:dibujaLineas	(rj_datos /	       cCentro
				  cRadio       rLatitud
				  rLongitud    rUsoRad
				  aPuntos      rOrient
				  AnguloSombra listaLineas
				  nomon	       matList
				  transMat
				 )
  (setq ;_ Sacar informaci�n de lista de asociaci�n rj_datos			 ;
    cCentro   (cdr (assoc 10 rj_datos))
    cRadio    (/ (cdr (assoc 40 rj_datos)) 2.0)
    rLatitud  (cdr (assoc 42 rj_datos))
    rLongitud (cdr (assoc 43 rj_datos))
    rUsoRad   (cdr (assoc 44 rj_datos))
    rOrient   (cdr (assoc 50 rj_datos))
  )
  ;; Se generan los puntos del c�rculo correspondiente a la sombra en cada hora	 ;
  ;; y se dibujan las l�neas correspondientes a las horas del d�a, guard�ndose	 ;
  ;; todas en la lista listaLineas para ser devueltas				 ;
  ;; hay que corregir seg�n la longitud del lugar y el uso horario		 ;
  (setq AnguloSombra -90)
  (while (<= AnguloSombra 90)
    (setq listaLineas
	   (append listaLineas
		   (cons (vla-AddLine
			   *EspacioDeTrabajo* ;_ Espacio de trabajo		 ;
			   (gp:list->variantArray cCentro) ;_ Centro del C�rculo ;
			   (gp:list->variantArray
			     (polar cCentro
				    (+ (rj:cuadranteh ;_ Angulo de sombra seg�n  ;
					 rLatitud ;_ la latitud, y la hora en 	 ;
					 (+ (Radianes<-Grados
					      AnguloSombra
					    )
					    (+ rLongitud rUsoRad)
					 )
				       )
				       rOrient
				    )
				    (* cRadio 0.85)
			     )
			   ) ;_ Radio del c�rculo				 ;
			 )
			 listaLineas
		   )
	   )
    )
    (setq AnguloSombra (+ AnguloSombra 15))
  )
  ;; Dibujo el nomon								 ;
  ;; primero una l�nea en la direcci�n Sur - Norte				 ;
  (setq
    nomon (vla-addline
	    *EspacioDeTrabajo*
	    (vlax-3d-point cCentro)
	    (vlax-3d-point
	      (polar cCentro rOrient cRadio)
	    )
	  )
  )
  ;; y lo roto rLatitud grados respecto a la direcci�n Este - Oeste		 ;
  (vla-Rotate3d
    nomon
    (vlax-3d-point cCentro)
    (vlax-3d-point
      (polar cCentro (+ (Radianes<-Grados -90) rOrient) cRadio)
    )
    rLatitud
  )
  (setq	listaLineas
	 (append listaLineas
		 (cons nomon
		       listaLineas
		 )
	 )
  )
  (list
    (cons 100 listaLineas)
  )  
)

;;; =============================================================================;
;;; Funci�n jr:dibujaCirculo dibujar� c�rculo del Reloj 			 ;
;;; =============================================================================;
(defun jr:dibujaCirculo	(rj_datos / cCentro cRadio)
  (setq ;_ Sacar informaci�n de lista de asociaci�n rj_datos			 ;
    cCentro   (cdr (assoc 10 rj_datos))
    cRadio    (/ (cdr (assoc 40 rj_datos)) 2.0)
  )
  (list
    (cons 102
	  ;; Plano del reloj							 ;
	  (vla-addCircle
	    *EspacioDeTrabajo* ;_ Espacio de trabajo				 ;
	    (vlax-3d-point cCentro) ;_ Centro del C�rculo			 ;
	    cRadio ;_ Radio del c�rculo						 ;
	  )
    )
  )  
)

;;; =============================================================================;
;;; c:reloj dibuja un reloj de sol						 ;
;;; =============================================================================;
;;;  rj_datosReloj guarda los datos de la figura en una lista con el formato: 	 ;
;;; 	(  3 . tipoDeReloj)							 ;
;;; 	( 10 . Centro)								 ;
;;; 	( 40 . Di�metro)							 ;
;;; 	( 42 . Latitud)								 ;
;;; 	( 43 . Longitud)							 ;
;;; 	( 44 . Uso_Horario_en_Radianes)						 ;
;;; 	( 50 . Orientaci�n_en_Radianes)						 ;
;;; 	(100 . ListaL�neas_y_nomon)						 ;
;;; 	(102 . C�rculo_Frontera)						 ;
;;; =============================================================================;
(defun c:reloj (/ rj_datosReloj rj_datosDialogo)
  ;; Deshabilita osnap								 ;
  (setvar "OSMODE" 0)
  ;; Eliminar reactor de commando						 ;
  (if *commandReactor*
    (progn
      (setq *commandReactor* nil)
      (vlr-remove-all :VLR-Command-Reactor)
    )
  )
  ;; Pide al usuario la localizaci�n del reloj, direcci�n norte-sur, y otros	 ;
  ;; parametros, continuar s�lo si se tiene una entrada v�lida			 ;
  (if (setq rj_datosReloj (rj:EntradaPunto))
    (if	(setq rj_datosDialogo (rj:EntradaDialogo))
      (progn
	;; En este punto se tiene una entrada v�lida de usuario			 ;
	;; A�adimos los datos devueltos por rj:EntradaDialogo a rj_datosReloj	 ;
	(setq rj_datosReloj (append rj_datosReloj rj_datosDialogo))
	;; dibujamos el c�rculo y lo a�adimos a la lista de datos del reloj	 ;
	(setq rj_datosReloj
	       (append rj_datosReloj
		       (jr:dibujaCirculo rj_datosReloj)
	       )
	)
	;; dibujamos las l�neas y las a�adimos a la lista de datos del reloj	 ;
	(setq rj_datosReloj
	       (append rj_datosReloj
		       (jr:dibujaLineas rj_datosReloj)
	       )
	)
	;; Adjuntamos los datos del reactor a un objeto, la funci�n		 ;
	;; vlr-object-reactor toma como argumentos los objetos, los datos, y las ;
	;; funciones a ser llamadas.						 ;
	(vlr-object-reactor
	  ;; El primer argumento es la lista de objetos asociados al reactor	 ;
	  (list (cdr (assoc 102 rj_datosReloj))) ;_ El c�rculo			 ;
	  ;; El segundo argumento son los datos que se quieren guardar		 ;
	  rj_datosReloj
	  ;; El tercero, la lista de pares (acci�n-usuario . funci�n-a-llamar)	 ;
	  '
	   (
	    ;; Si es modificado llamar a rj:cambianCirculoReloj			 ;
	    (:vlr-modified . rj:cambianCirculoReloj)
	    ;; Si es borrado llamar a rj:borranCirculoReloj			 ;
	    (:vlr-erased . rj:borranCirculoReloj)
	   )
	) ;_ fin de vlr-object-reactor, definici�n del reactor de objeto	 ;

	;; Registramos un reactor de comando para ajustar el c�rculo cuando el 	 ;
	;; cambio finalice							 ;
	(if (not *commandReactor*)
	  (setq	*commandReactor*
		 (VLR-Command-Reactor
		   nil ;_ En este caso no se asocia ning�n dato, ni objeto	 ;
		   '(
		     (:vlr-commandWillStart . rj:comienzaComando)
		     (:vlr-commandEnded . rj:terminaComando)
		    )
		 ) ;_ Fin de vlr-command-reactor, reactor de commando		 ;
	  )
	)
	;; Eliminamos todos los reactores una vez que cierren el dibujo	actual	 ;
	;; esto es muy importante!!!!!!!!! Sin esta notificaci�n AutoCad puede	 ;
	;; bloquearse al cerrar el dibujo.					 ;
	(if (not *DrawingReactor*)
	  (setq	*DrawingReactor*
		 (VLR-DWG-Reactor
		   nil ;_ En este caso tampoco se asocia ning�n dato, ni objeto	 ;
		   '((:vlr-beginClose . rj:limpiaTodosReactores)
		    )
		 ) ;_ fin de vlr-DWG-reactor, definici�n de reactor de dibujo	 ;
	  )
	)
      )
      (princ "\nFunci�n cancelada.")
    )
    (princ
      "\nInformaci�n incompleta, no se puede dibujar el c�rculo."
    )
  )
  (princ) ;_ termina elegantemente con una linea en blanco			 ;
)

;;; =============================================================================;
;;; Mostrar un mensaje para que el usuario sepa que comando ejecutar		 ;
;;; =============================================================================;

(princ
  (strcat
    "\n _________________________________________________________________"
    "\n             Rutinas para el dibujo de un reloj de Sol            "
    "\n          Escriba \"reloj\" para dibujar un reloj de sol          "
    "\n             Soporte: rmortega77@yahoo.es                         "
    )
)
(princ)  ;_ termina elegantemente con una linea en blanco			 ;

;;; rmortega77@yahoo.es								 ;