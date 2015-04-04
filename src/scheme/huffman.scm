
					;--------------------------------------------------------------------------------;
					;						   		                 ;
					;	       	MODIFICACIÓN DEL ALGORITMO DE ORDENACIÓN POR MONTÍCULO		 ;
					;	            	PARA ORDENAR LA LISTA DE FRECUENCIAS			 ;
					;										 ;
					;--------------------------------------------------------------------------------;

;Intercambia los elementos de indice1 e indice2 de la lista
;Coge los indice1-1 primeros elementos, los junta con el elemento de indice2, después con los elementos del indice1 al indice2, estos con el de indice1 y todo ello con los elementos siguientes a al de indice2
;Hay que tener en cuenta que el primer elemento ocupa la posición 0, y así sucesivamente.
(define intercambia
  (lambda (lista indice1 indice2)
    (append 
     (list-head lista (- indice1 1)) 
     (cons 
      (list-ref lista (- indice2 1)) 
      (append 
       (list-tail (list-head lista (- indice2 1)) indice1) 
       (cons 
	(list-ref lista (- indice1 1)) 
	(list-tail lista indice2)
	)
       )
      )
     )
    )
  )

;Comprueba si el elemento a hundir es menor que alguno de sus hijos. Entonces comprueba cual de ellos es mayor y lo sustituye por el.
(define hundir
  (lambda (heap indice)
    (if (< (* 2 indice) (length heap));comprueba que tenga hijos
	(let (
	      (raiz (cadr (list-ref heap (- indice 1))))
	      (hijo1 (cadr (list-ref heap (- (* 2 indice) 1))))
	      (hijo2 (cadr (list-ref heap (* 2 indice))))
	      )
	  (if (or ;Los hijos estan en 2i y 2i+1, pero para acceder a ellos hay que restar 1 porque se empieza a contar por el 0
	       (< raiz hijo1) 
	       (< raiz hijo2)
	       )
	      (if (> hijo1 hijo2)
		  (hundir (intercambia heap indice (* 2 indice)) (* 2 indice))
		  (hundir (intercambia heap indice (+ (* 2 indice) 1)) (+ (* 2 indice) 1))
		  )
	      heap;Si ninguno de los dos es menor, lo devuelve como está
	      )
	  )
	heap
	)
    )
  )

(define crearMonticulo
  (lambda (lista)
    (letrec ((controlCrearMonticulo ;utilizado para sustituir al bucle for del algoritmo imperativo	
	      (lambda (lista indice)
		(if (>= indice 1)
		    (controlCrearMonticulo (hundir lista indice) (- indice 1))
		    lista
		    )
		))) 
      (controlCrearMonticulo lista (quotient (length lista) 2))
      )
    )
  )

(define ordMonticulo
  (lambda (lista)
    (if (<= (length lista) 1)
	lista
	(if (= (length lista) 2)
	    (if (< (cadr (list-ref lista 1)) (cadr (list-ref lista 0)))
		(intercambia lista 1 2)
		lista
		)
	    (if (= (length lista) 3) ;Si la lista es de menos de 4 elementos, el algoritmo no es suficiente para ordenarlos, asi que los ordeno comparandolos.
		(if (< (cadr (list-ref lista 1)) (cadr (list-ref lista 0)))
		    (ordMonticulo (intercambia lista 1 2))
		    (if (< (cadr (list-ref lista 2)) (cadr (list-ref lista 1)))
			(ordMonticulo (intercambia lista 2 3))
			lista
			)
		    )
		(letrec ((ordMonticuloConcat ;ordena un monticulo y lo concatena con la lista pasada. Funciona cuando el monticulo es de más de 3 elementos
			  (lambda (heap lista)
			    (if (= (length heap) 3); cuando quedan 3, estos están ordenados en orden descendente
				(append (reverse heap) lista)
				(ordMonticuloConcat 
				 (hundir
				  (append		;Si el montículo era (a,X,b), llama a hundir de (b,X)
				   (list-tail heap (- (length heap) 1));ultimo elemento
				   (sublist heap 1 (- (length heap) 1));sin el primer ni el ultimo elemento
				   )
				  1
				  )
				 (append (list-head heap 1) lista)
				 )
				)
			    )))
		  (ordMonticuloConcat (crearMonticulo lista) ())
		  )
		)
	    )
	)
    )
  )


					;--------------------------------------------------------------------------------;
					;                                       FIN                                      ;
					;						   		                 ;
					;	       	MODIFICACIÓN DEL ALGORITMO DE ORDENACIÓN POR MONTÍCULO		 ;
					;	            	PARA ORDENAR LA LISTA DE FRECUENCIAS			 ;
					;										 ;
					;--------------------------------------------------------------------------------;


;Calcula la frecuencia de los caracteres de la lista de entrada, lo concatena con la lista resultado y lo devuelve ordenado por frecuencias en orden ascendente.
(define CalcularFrecuencias
  (lambda (caracteres resultado)
    (if (null? caracteres)
	(ordMonticulo resultado) ;Ordena el resultado y lo devuelve.
	(CalcularFrecuencias 
	 (cdr caracteres)
	 (let ((presente (assq (car caracteres) resultado))) ;Presente es el par (Caracter Frecuencia) de resultado si está, y si no es #F.
	   (if presente   ; Comprimir recibe una cadena de caracteres, la pasa a lista de letras y la comprime con ComprimirHuffman.
(define Comprimir
	(lambda (cadena)
		(ComprimirHuffman (string->list cadena))
	)
)
	       (let* ((posicion (- (length resultado) (length (member presente resultado))))) ;Busca la posición que ocupa el caracter en la lista de frecuencias.
		 (append ;Incrementa en 1 la frecuencia de ese caracter.
		  (list-head resultado posicion)
		  (cons (list (car presente) (+ (cadr presente) 1)) (list-tail resultado (+ posicion 1)))
		  ) 
		 )
	       (append resultado (cons (list (car caracteres) 1) ())); Si el caracter no está en la lista, lo añade con frecuencia 1.
	       )
	   )
	 )
	)
    )
  )


;ListaNodosPeso es la lista devuelta por calcularFrecuencias.
;ListaNodosArbol es una lista de pares (ArbolHuffman Peso).
(define ConstruirArbolHuffman
  (lambda (listaNodosPeso listaNodosArbol)
    (if (<= (+ (length listaNodosPeso) (length listaNodosArbol)) 1)
	(if (= (length listaNodosPeso) 1)
	    (caar listaNodosPeso)
	    (caar listaNodosArbol); Cuando la suma de las longitudes de las dos listas es 1, en ListaNodosArbol solo queda el arbol Huffman buscado.
	    )
	(let* (
	       (menorPeso ;Función que devuelve el nodo de menor peso que hay en las 2 listas. Ambas están ordenadas de forma ascendente.
		(lambda (listaPesos1 listaPesos2)
		  (if (null? listaPesos1)
		      (car listaPesos2)
		      (if (null? listaPesos2)
			  (car listaPesos1)
			  (if (<= (cadar listaPesos1) (cadar listaPesos2))
			      (car listaPesos1)
			      (car listaPesos2)
			      )
			  )
		      )
		  )
		)
	       (insertaEnOrden ;Función que inserta el elemento en la lista en orden ascendente por peso.
		(lambda (lista elemento)
		  (letrec (
			   (insertar
			    (lambda (lista elemento posicion)
			      (if (= posicion 0)
				  (append lista (cons elemento ()))
;Como previsiblemente cada vez los elementos van a tener mayor peso, empiezo comparando por el final y cuando se encuentre uno de menor o igual peso, lo inserta detrás de el.
				  (if (<= (cadr (list-ref lista (- posicion 1))) (cadr elemento))
				      (append
				       (list-head lista posicion)
				       (cons
					elemento
					(list-tail lista posicion)
					)
				       )
				      (insertar lista elemento (- posicion 1))
				      )
				  )
			      )
			    )
			   )
		    
		    (insertar lista elemento (length lista))	   
		    )
		  )  
		)
	       (menorPeso1 (menorPeso listaNodosPeso listaNodosArbol)) ;El primer nodo de menor peso.
	       (nuevalistaNodosPeso ;La nueva listaNodosPeso. Si menorPeso1 pertenece a ella, lo quita.
		(if (not (member menorPeso1 listaNodosPeso))
		    listaNodosPeso
		    (list-tail listaNodosPeso 1)
		    )
		)
	       (nuevaListaNodosArbol ;La nueva listaNodosArbol. Si menorPeso1 pertenece a ella, lo quita.
		(if (not (member menorPeso1 listaNodosArbol))
		    listaNodosArbol
		    (list-tail listaNodosArbol 1)
		    )
		)
	       (menorPeso2 (menorPeso nuevaListaNodosPeso nuevaListaNodosArbol)) ;El segundo nodo de menor peso.
	       (nuevalistaNodosPeso ;La listaNodosPeso definitiva. Si menorPeso2 pertenece a ella, lo quita.
		(if (not (member menorPeso2 nuevalistaNodosPeso))
		    nuevalistaNodosPeso
		    (list-tail nuevalistaNodosPeso 1)
		    )
		)
	       (nuevaListaNodosArbol ;La listaNodosArbol definitiva. Si menorPeso2 pertenece a ella, lo quita y añade el nodo compuesto con menorPeso1 y menorPeso2 en orden.
		(insertaEnOrden
		 (if (not (member menorPeso2 nuevalistaNodosArbol))
		     nuevalistaNodosArbol
		     (list-tail nuevalistaNodosArbol 1)
		     )
		 (list
		  (list
		   (car menorPeso1)
		   (car menorPeso2)
		   )
		  (+ (cadr menorPeso1) (cadr menorPeso2))
		  )
		 )
		)
	       )
	  (ConstruirArbolHuffman nuevaListaNodosPeso nuevaListaNodosArbol)
	  )
	)
    )
  )

;A cada nodo del arbol le asigna un número, que será su código al pasarlo a bit-string.
;Sea N el número del nodo actual. 
;Si bajamos al hijo izquierdo, su número será N*2 (desplazamiento lógico a la izquierda),terminando en 0.
;Si bajamos al hijo derecho, su número será N*2+1 (desplazamiento lógico a la izquierda), terminando en 1.
;Para terminar, se representará en número con tantos bits como indica la profundidad del arbol.
(define ObtenerCodigosHuffman
  (lambda (arbolHuffman profundidad acumulado)
    (if (or (char? arbolHuffman) (symbol? arbolHuffman)) ;Si el arbol es un caracter, ha llegado a una hoja, así que devuelve su código. Compruebo que sea char o symbol para que funcione tanto con listas de letras como con listas de símbolos.
	(if (= profundidad 0) ;Si la hoja está en la raiz (solo hay un caracter)
	    (cons (cons arbolHuffman (cons (unsigned-integer->bit-string 1 acumulado) ())) ())	    
	    (cons (cons arbolHuffman (cons (unsigned-integer->bit-string profundidad acumulado) ())) ())
	    )
	(append ;Si no, junta los códigos obtenidos al bajar por la izquierda con los obtenidos al bajar por la derecha.
	 (ObtenerCodigosHuffman (car arbolHuffman) (+ profundidad 1) (* acumulado 2))
	 (ObtenerCodigosHuffman (cadr arbolHuffman) (+ profundidad 1) (+ (* acumulado 2) 1))
	 )
	)
    )
  )

;A comprimirHuffman hay que pasarle una lista de caracteres, por ejemplo (#\H #\o #\l #\a). Esto lo devuelve la funcion string->list, por ejemplo (string->list "Hola").
(define ComprimirHuffman
  (lambda (caracteres)
    (let* (
	   (arbolHuffman (ConstruirArbolHuffman (CalcularFrecuencias caracteres ()) ())) ;Obtiene el arbol Huffman.
	   (codigosHuffman (ObtenerCodigosHuffman arbolHuffman 0 0)) ;Obtiene la lista de códigos.
	   (codificacion ;La cadena codificada.
	    (letrec (
		     (codifica ;Función que codifica la entrada en función de los códigos.
		      (lambda (entrada codigos salida)
			(if (null? entrada)
			    salida ;Si la entrada está vacía, devuelve la salida.
			    (codifica ;Si no, en salida añade el código de la primera letra y llama a la función recursivamente.
			     (list-tail entrada 1)	
			     codigos
			     (append
			      salida
			      (cdr (assq (car entrada) codigos))
			      )
			     )
			    )
			)
		      )
		     )
	      (codifica caracteres codigosHuffman ())
	      )
	    )
	   )
      (list arbolHuffman codificacion) ;Devuelve la lista formada por el arbol huffman y la lista comprimida.
      )
    )
  )

;descomprimirHuffman devuelve una lista de caracteres, que puede ser convertida a string con list->string.
(define descomprimirHuffman
  (lambda (cadenaComprimida)
    (let* (
	   (arbolHuffman (car cadenaComprimida))
	   (codigosHuffmanFormat (map reverse (ObtenerCodigosHuffman arbolHuffman 0 0))) ;Da la vuelta a la lista de códigos Huffman, de forma que se puedan buscar fácilmente los códigos en ella gracias a la función assoc.
	   )
      (letrec (
	       (obtenerCadena
		(lambda (CodigosHuffmanFormat comprimido cadena)
		  (if (null? comprimido)
		      (reverse cadena) ;Si en la cadena comprimida no queda nada, las letras correspondientes están en cadena, pero de al revés. Esto lo hice así porque si no, cuando fuese a meter el primer elemento, haría (cons () #\letra) y eso devuelve (().#\letra).
		      (obtenerCadena CodigosHuffmanFormat (cdr comprimido) (cons (cadr (assoc (car comprimido) CodigosHuffmanFormat)) cadena))
		      )
		  )
		)
	       )
	(obtenerCadena CodigosHuffmanFormat (cadr cadenaComprimida) ())
	)
      )
    )
  )

;
;				PARA EL USO CON CADENAS DE CARACTERES
;
; Comprimir recibe una cadena de caracteres, la pasa a lista de letras y la comprime con ComprimirHuffman.
(define Comprimir
  (lambda (cadena)
    (ComprimirHuffman (string->list cadena))
    )
  )

; Descomprimir recive una cadena de letras comprimida, la descomprime y la pasa a String.
(define Descomprimir
  (lambda (cadenaComprimida)
    (list->string (DescomprimirHuffman cadenaComprimida))
    )
  )
