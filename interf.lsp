;;; =============================================================================;
;;;  © 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  © 2006               rortega@vertice.cu, rmortega77@yahoo.es        	;
;;;  © 2006  ´` Vértice® Empresa de Diseño y Proyectos de Ingeniería     	 ;
;;;                  Rutinas para el dibujo de un reloj de Sol           	 ;
;;; =============================================================================;

;;; ============================== Inicialización ===============================;

;;; =============================== Funciones ===================================;

;;; =============================================================================;
;;; Función rj:EntradaPunto obtendrá centro de reloj diámetro y orientación      ;
;;; =============================================================================;
;;;  Devuelve una lista que consiste en 					 ;
;;; (10 . Centro)								 ;
;;; (40 . Diámetro)								 ;
;;; (50 . Orientación)								 ;
;;; =============================================================================;
(defun rj:EntradaPunto (/ cCentro cPunto)
  (if (setq cCentro (getpoint "\nMarque el centro del reloj: "))
    (if	(setq cPunto (getpoint cCentro "\nMarque el Radio y orientación del reloj: "))
      (list ;_ Lista con datos del círculo					 ;
	(cons 10 cCentro) ;_ Centro 						 ;
	(cons 40 (* (distance cCentro cPunto) 2.0)) ;_ Diámetro			 ;
	(cons 50 (angle cCentro cPunto)) ;_ Ángulo o dirección del Reloj.	 ;
      )
    )
  )
)

;;; =============================================================================;
;;; Función rj:EntradaDialogo optendrá del usuario los parámetros del reloj	 ;
;;; =============================================================================;
;;;  Devuelve una lista que consiste en 					 ;
;;; (42 . Latitud)								 ;
;;; (43 . longitud)								 ;
;;; (44 . usoRadianes)								 ;
;;; (3 . tipoDeReloj)								 ;
;;; =============================================================================;
(defun rj:EntradaDialogo
			 (/		 dcl_id		dialogoCargado
			  mostrarDialogo tipoDeReloj	hizoClick
			  latitud	 longitud	resultado
			  usoSigno	 usoHora	usoMinutos
			  usoRadianes
			 )
  (setq	tipoDeReloj    "Horizontal"
	dialogoCargado T
	mostrarDialogo T
  )
  ;; Se carga el fichero dcl y chequea si se carga correctamente antes de seguir ;
  (if (= -1 (setq dcl_id (load_dialog "rjdialogo.dcl")))
    (progn
      ;; Hubo problemas cargando el diálogo. Se establece dialogoCargado a nil   ;
      (princ "\nNo se pudo cargar \"rjdialogo.dcl\".dcl")
      (setq dialogoCargado nil)
    )
  )
  ;; Se carga el diálogo en memoria 						 ;
  (if (and dialogoCargado
	   (not (new_dialog "rj_dialogoPrincipal" dcl_id))
      )
    (progn
      ;; Hubo un problema ...							 ;
      (princ "\nNo se pudo mostrar \"rj_dialogoPrincipal\"")
      (setq mostrarDialogo nil)
    )
  )
  (if (and dialogoCargado mostrarDialogo) ;_ Si se mostró correctamente		 ;
    (progn
      ;; Se establecen los valores de los componentes				 ;
      (set_tile "rj_lat" "20d53'")
      (set_tile "rj_lng" "76d15'")
      ;; Asiganar las funciones que se ejcutarán, a los botones			 ;
      (action_tile
	"rj_vrt"
	"(setq tipoDeReloj \"Vertical\")"
      )
      (action_tile
	"rj_hrz"
	"(setq tipoDeReloj \"Horizontal\")"
      )
      (action_tile
	"rj_ecu"
	"(setq tipoDeReloj \"Ecuatorial\")"
      )
      (action_tile
	"rj_lib"
	"(setq tipoDeReloj \"Libre\")"
      )
      (action_tile
	"Cancelar"
	"(done_dialog) (setq hizoClick nil)"
      )
      (action_tile
	"Aceptar"
	(strcat
	  ;; Leo los valores del dialogo					 ;
	  "(progn (setq latitud (angtof (get_tile \"rj_lat\")))"
	  "(setq longitud (angtof (get_tile \"rj_lng\")))"
	  "(setq usoSigno (get_tile \"rj_usg\"))"
	  "(setq usoHora (get_tile \"rj_uhr\"))"
	  "(setq usoMinutos (get_tile \"rj_umin\"))"
	  ;; Cierro el dialogo							 ;
	  "(done_dialog) (setq hizoClick T))"
	 )
      )
      (start_dialog)
      (unload_dialog dcl_id)
      (if hizoClick
	;; Build the resulting data
	(progn
	  (setq	usoRadianes
		 (Radianes<-Grados
		   (* (+
			(* (atof usoHora) 15)
			(cond
			  ((= usoMinutos "1") 7.5)
			  ((= usoMinutos "2") 11.25)
			  (t 0)
			)

		      )
		      (cond
			((= usoSigno "0") -1)
			((= usoSigno "1") 1)
			(t nil)
		      )
		   )
		 )
	  )
	  (setq	resultado (list
			    (cons 42 latitud)
			    (cons 43 longitud)
			    (cons 44 usoRadianes)
			    (cons 3 tipoDeReloj)
			  )
	  )
	)
      )
    )
  )
  resultado
)

;;; ============================= Procedimientos ================================;

;;; rmortega77@yahoo.es								 ;