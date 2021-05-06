;;; =============================================================================;
;;;  © 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  © 2006               rortega@vertice.cu, rmortega77@yahoo.es        	;
;;;  © 2006  ´` Vértice® Empresa de Diseño y Proyectos de Ingeniería     	 ;
;;;                  Rutinas para el dibujo de un reloj de Sol           	 ;
;;; =============================================================================;

;;; ============================== Inicialización ===============================;

;;; =============================== Funciones ===================================;

;;; ============================= Procedimientos ================================;

;;; =============================================================================;
;;; Función jr:dibujaLineas dibujará las líneas de sombra del reloj y el nomon	 ;
;;; =============================================================================;
(defun jr:dibujaLineas	(rj_datos /	       cCentro
				  cRadio       rLatitud
				  rLongitud    rUsoRad
				  aPuntos      rOrient
				  AnguloSombra listaLineas
				  nomon	       matList
				  transMat
				 )
  (setq ;_ Sacar información de lista de asociación rj_datos			 ;
    cCentro   (cdr (assoc 10 rj_datos))
    cRadio    (/ (cdr (assoc 40 rj_datos)) 2.0)
    rLatitud  (cdr (assoc 42 rj_datos))
    rLongitud (cdr (assoc 43 rj_datos))
    rUsoRad   (cdr (assoc 44 rj_datos))
    rOrient   (cdr (assoc 50 rj_datos))
  )
  ;; Se generan los puntos del círculo correspondiente a la sombra en cada hora	 ;
  ;; y se dibujan las líneas correspondientes a las horas del día, guardándose	 ;
  ;; todas en la lista listaLineas para ser devueltas				 ;
  ;; hay que corregir según la longitud del lugar y el uso horario		 ;
  (setq AnguloSombra -90)
  (while (<= AnguloSombra 90)
    (setq listaLineas
	   (append listaLineas
		   (cons (vla-AddLine
			   *EspacioDeTrabajo* ;_ Espacio de trabajo		 ;
			   (gp:list->variantArray cCentro) ;_ Centro del Círculo ;
			   (gp:list->variantArray
			     (polar cCentro
				    (+ (rj:cuadranteh ;_ Angulo de sombra según  ;
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
			   ) ;_ Radio del círculo				 ;
			 )
			 listaLineas
		   )
	   )
    )
    (setq AnguloSombra (+ AnguloSombra 15))
  )
  ;; Dibujo el nomon								 ;
  ;; primero una línea en la dirección Sur - Norte				 ;
  (setq
    nomon (vla-addline
	    *EspacioDeTrabajo*
	    (vlax-3d-point cCentro)
	    (vlax-3d-point
	      (polar cCentro rOrient cRadio)
	    )
	  )
  )
  ;; y lo roto rLatitud grados respecto a la dirección Este - Oeste		 ;
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
;;; Función jr:dibujaCirculo dibujará círculo del Reloj 			 ;
;;; =============================================================================;
(defun jr:dibujaCirculo	(rj_datos / cCentro cRadio)
  (setq ;_ Sacar información de lista de asociación rj_datos			 ;
    cCentro   (cdr (assoc 10 rj_datos))
    cRadio    (/ (cdr (assoc 40 rj_datos)) 2.0)
  )
  (list
    (cons 102
	  ;; Plano del reloj							 ;
	  (vla-addCircle
	    *EspacioDeTrabajo* ;_ Espacio de trabajo				 ;
	    (vlax-3d-point cCentro) ;_ Centro del Círculo			 ;
	    cRadio ;_ Radio del círculo						 ;
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
;;; 	( 40 . Diámetro)							 ;
;;; 	( 42 . Latitud)								 ;
;;; 	( 43 . Longitud)							 ;
;;; 	( 44 . Uso_Horario_en_Radianes)						 ;
;;; 	( 50 . Orientación_en_Radianes)						 ;
;;; 	(100 . ListaLíneas_y_nomon)						 ;
;;; 	(102 . Círculo_Frontera)						 ;
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
  ;; Pide al usuario la localización del reloj, dirección norte-sur, y otros	 ;
  ;; parametros, continuar sólo si se tiene una entrada válida			 ;
  (if (setq rj_datosReloj (rj:EntradaPunto))
    (if	(setq rj_datosDialogo (rj:EntradaDialogo))
      (progn
	;; En este punto se tiene una entrada válida de usuario			 ;
	;; Añadimos los datos devueltos por rj:EntradaDialogo a rj_datosReloj	 ;
	(setq rj_datosReloj (append rj_datosReloj rj_datosDialogo))
	;; dibujamos el círculo y lo añadimos a la lista de datos del reloj	 ;
	(setq rj_datosReloj
	       (append rj_datosReloj
		       (jr:dibujaCirculo rj_datosReloj)
	       )
	)
	;; dibujamos las líneas y las añadimos a la lista de datos del reloj	 ;
	(setq rj_datosReloj
	       (append rj_datosReloj
		       (jr:dibujaLineas rj_datosReloj)
	       )
	)
	;; Adjuntamos los datos del reactor a un objeto, la función		 ;
	;; vlr-object-reactor toma como argumentos los objetos, los datos, y las ;
	;; funciones a ser llamadas.						 ;
	(vlr-object-reactor
	  ;; El primer argumento es la lista de objetos asociados al reactor	 ;
	  (list (cdr (assoc 102 rj_datosReloj))) ;_ El círculo			 ;
	  ;; El segundo argumento son los datos que se quieren guardar		 ;
	  rj_datosReloj
	  ;; El tercero, la lista de pares (acción-usuario . función-a-llamar)	 ;
	  '
	   (
	    ;; Si es modificado llamar a rj:cambianCirculoReloj			 ;
	    (:vlr-modified . rj:cambianCirculoReloj)
	    ;; Si es borrado llamar a rj:borranCirculoReloj			 ;
	    (:vlr-erased . rj:borranCirculoReloj)
	   )
	) ;_ fin de vlr-object-reactor, definición del reactor de objeto	 ;

	;; Registramos un reactor de comando para ajustar el círculo cuando el 	 ;
	;; cambio finalice							 ;
	(if (not *commandReactor*)
	  (setq	*commandReactor*
		 (VLR-Command-Reactor
		   nil ;_ En este caso no se asocia ningún dato, ni objeto	 ;
		   '(
		     (:vlr-commandWillStart . rj:comienzaComando)
		     (:vlr-commandEnded . rj:terminaComando)
		    )
		 ) ;_ Fin de vlr-command-reactor, reactor de commando		 ;
	  )
	)
	;; Eliminamos todos los reactores una vez que cierren el dibujo	actual	 ;
	;; esto es muy importante!!!!!!!!! Sin esta notificación AutoCad puede	 ;
	;; bloquearse al cerrar el dibujo.					 ;
	(if (not *DrawingReactor*)
	  (setq	*DrawingReactor*
		 (VLR-DWG-Reactor
		   nil ;_ En este caso tampoco se asocia ningún dato, ni objeto	 ;
		   '((:vlr-beginClose . rj:limpiaTodosReactores)
		    )
		 ) ;_ fin de vlr-DWG-reactor, definición de reactor de dibujo	 ;
	  )
	)
      )
      (princ "\nFunción cancelada.")
    )
    (princ
      "\nInformación incompleta, no se puede dibujar el círculo."
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