;;; =============================================================================;
;;;  © 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  © 2006  ´` Vértice® Empresa de Diseño y Proyectos de Ingeniería     	 ;
;;;  © 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  © 2006               rortega@vertice.cu, rmortega77@yahoo.es        	;
;;;                  Rutinas para el dibujo de un reloj de Sol           	 ;
;;; =============================================================================;

;;; ============================== Inicialización ===============================;

;;; =============================================================================;
;;; Cargamos la funcionalidad de ActiveX. Si ya está cargado, no pasa nada.      ;
;;; =============================================================================;
(vl-load-com)
;;; =============================================================================;
;;; Definimos la variable global que apunta al espacio de trabajo dentro del     ;
;;; dibujo activo. 								 ;
;;; =============================================================================;
(setq *EspacioDeTrabajo*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

;;; =============================== Funciones ===================================;

;;; =============================================================================;
;;; Esta función calcula el ángulo de sombra para una hora H en			 ;
;;; una latitud l, para cuadrante de sombra horizontal				 ;
;;; (hay que corregir H según la longitud y el uso horario)			 ;
;;; =============================================================================;
(defun rj:cuadranteh (l H)
  (atan	(* (sin l) (/ (sin H) (cos H)))
  )
)

;;; =============================================================================;
;;; Esta función calcula el ángulo de sombra para una hora H en			 ;
;;; una latitud l, para cuadrante de sombra vertical				 ;
;;; (hay que corregir H según la longitud y el uso horario)			 ;
;;; =============================================================================;
(defun rj:cuadrantev (l H)
  (atan	(* (cos l) (/ (sin H) (cos H)))
  )
)

;;; =============================================================================;
;;; Convierte una lista en un arreglo de números reales				 ;
;;; =============================================================================;
(defun gp:list->variantArray (ptsList / arraySpace sArray)
  ;; Guarda espacio para un arreglo de números reales				 ;
  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble ;_ tipo de elemento					 ;
	   (cons 0
		 (- (length ptsList) 1)
	   ) ;_ Dimensión del arreglo						 ;
	 )
  )
  (setq sArray (vlax-safearray-fill arraySpace ptsList))
  (vlax-make-variant sArray) ;_ Devuelve el arreglo				 ;
)

;;; =============================================================================;
;;; Convierte grados en radianes						 ;
;;; =============================================================================;
(defun Radianes<-Grados	(parmGrados)
  (* pi (/ parmGrados 180.0))
)

;;; ============================= Procedimientos ================================;

;;; rmortega77@yahoo.es								 ;