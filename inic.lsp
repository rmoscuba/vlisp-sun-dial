;;; =============================================================================;
;;;  � 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  � 2006  �` V�rtice� Empresa de Dise�o y Proyectos de Ingenier�a     	 ;
;;;  � 2006                Rodolfo M Ortega Santiesteban                 	 ;
;;;  � 2006               rortega@vertice.cu, rmortega77@yahoo.es        	;
;;;                  Rutinas para el dibujo de un reloj de Sol           	 ;
;;; =============================================================================;

;;; ============================== Inicializaci�n ===============================;

;;; =============================================================================;
;;; Cargamos la funcionalidad de ActiveX. Si ya est� cargado, no pasa nada.      ;
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
;;; Esta funci�n calcula el �ngulo de sombra para una hora H en			 ;
;;; una latitud l, para cuadrante de sombra horizontal				 ;
;;; (hay que corregir H seg�n la longitud y el uso horario)			 ;
;;; =============================================================================;
(defun rj:cuadranteh (l H)
  (atan	(* (sin l) (/ (sin H) (cos H)))
  )
)

;;; =============================================================================;
;;; Esta funci�n calcula el �ngulo de sombra para una hora H en			 ;
;;; una latitud l, para cuadrante de sombra vertical				 ;
;;; (hay que corregir H seg�n la longitud y el uso horario)			 ;
;;; =============================================================================;
(defun rj:cuadrantev (l H)
  (atan	(* (cos l) (/ (sin H) (cos H)))
  )
)

;;; =============================================================================;
;;; Convierte una lista en un arreglo de n�meros reales				 ;
;;; =============================================================================;
(defun gp:list->variantArray (ptsList / arraySpace sArray)
  ;; Guarda espacio para un arreglo de n�meros reales				 ;
  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble ;_ tipo de elemento					 ;
	   (cons 0
		 (- (length ptsList) 1)
	   ) ;_ Dimensi�n del arreglo						 ;
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