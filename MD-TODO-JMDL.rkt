#lang racket
(require racket/draw)
;(require racket)

;la negacion
;formato : (neg <proposicion>) --> proposicion
;proposito: calcular la negacion de una proposicion
;ejemplos y casos de prueba:
;> (neg #t)
;#f
;> (neg #f)
;#t
;>
(define neg
  (λ (p)
    (if p #f #t)))


;la conjuncion
;formato : (y <proposicion> <proposicion>) --> proposicion
;proposito: calcular la conjuncion de dos proposiciones
;ejemplos y casos de prueba:
;> (y #t #t)
;#t
;> (y #t #f)
;#f
;> (y #f #f)
;#f
;> (y #f #t)
;#f
;>
(define y
  (λ (p q)
    (if p q #f)))


;la disyuncion
;formato : (o <proposicion> <proposicion>) --> proposicion
;proposito: calcular la disyucion de dos proposiciones
;ejemplos y casos de prueba:
;> (o #t #t)
;#t
;> (o #t #f)
;#t
;> (o #f #t)
;#t
;> (o #f #f)
;#f
;>
(define o
  (λ (p q)
    (if p #t q)))


;la disyuncion exclusiva
;formato : (ox <proposicion> <proposicion>) --> proposicion
;proposito: calcular la disyuncion exclusiva de dos proposiciones
;ejemplos y casos de prueba:
;> (ox #t #t)
;#f
;> (ox #t #f)
;#t
;> (ox #f #t)
;#t
;> (ox #f #f)
;#f
;> 
(define ox
  (λ (p q)
    (if p (neg q) q)))


;la implicacion
;formato : (implicacion <proposicion> <proposicion>) --> proposicion
;proposito: calcular implicacion de dos proposiciones
;ejemplos y casos de prueba:
;> (implicacion #t #t)
;#t
;> (implicacion #t #f)
;#f
;> (implicacion #f #t)
;#t
;> (implicacion #f #f)
;#t
;>
(define implicacion
  (λ (p q)
    (if p q (neg p))))


;la doble implicacion
;formato : (doble-implicacion <proposicion> <proposicion>) --> proposicion
;proposito: calcular la doble implicacion de dos proposiciones
;ejemplos y casos de prueba:
;> (doble-implicacion #t #t)
;#t
;> (doble-implicacion #t #f)
;#f
;> (doble-implicacion #f #t)
;#f
;> (doble-implicacion #f #f)
;#t
;> 
(define doble-implicacion
  (λ (p q)
    (if p q (neg q))))


;cardinalidad
; formato: (cardinalidad <conjunto>) --> <numero>
; proposito: Determinar el numero de elementos de un conjunto
; ejemplos:
; > (cardinalidad ’(3 8 2 9))
; 4
; >
(define card-v2
      (λ (A res)
        (if (empty? A)
            res
            (card-v2 (cdr A) (+ res 1)))))

(define cardinalidad
  (λ (A)    
    (card-v2 A 0)))



;factorial 
; formato: (fact <numero>) --> <numero>
; proposito: calcular el factorial de un numero >= 1
; ejemplos:
;> (fact 9)
;362880
;> (fact 1)
;1
;> (fact 12)
;479001600
;>
(define fact-v2
      (λ (m res)
        (cond((= m 1) res)
             (else (fact-v2 (- m 1) (* res (- m 1)))))))

(define fact
  (λ (n)    
    (fact-v2 n n)))



;pertenencia 
; formato: (pertenece? <elemento> <conjunto>) --> <booleano>
; proposito: verificar si pertenece un elemento al conjunto devuelve #t o #f sea el caso
; ejemplos:
;> (pertenece? 'a '(a e i o u))
;#t
;> (pertenece? 34 '(12 23 35 1 0 9 78 65 13 25))
;#f
;>
;> (pertenece? '() '(1 2 3))
;#f
;>
(define pertenece?
  (λ (a A)
    (cond ((empty? A) #f)
          ((equal? a (car A)) #t)
          (else (pertenece? a (cdr A))))))


;conjunto?
; formato: (conjunto? <conjunto>) --> <booleano>
; proposito: verificar si la lista es cun conjunto si lo es devuelve #t, en otro caso devuelve #f
; ejemplos:
;> (conjunto? '(a d c a o))
;#f
;> (conjunto? '(a d c o))
;#t
;> (conjunto? '())
;#t
;>
(define conjunto?
  (λ (A)
    (if (empty? A) #t 
        (if (pertenece? (car A) (cdr A)) #f 
            (conjunto? (cdr A))))))


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tarea 08,09 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; (subconjunto? <Conjunto> <Conjunto>) --> <Booleano>

;; Produce #t si el conjunto A es subconjunto del conjunto B

;; (plantilla)
;; (define subconjunto?
;;   (λ (A B)
;;     (cond((empty? A) #t)
;;          ((.......))
;;       (....))))

;; (ejemplos)
;(check-expect (subconjunto? '() '(1 2 3)) #t)
;(check-expect (subconjunto? '(r) '(1 2 3)) #f)
;(check-expect (subconjunto? '(a b c d e r) '()) #f)

(define subconjunto?
  (λ (A B)
    (cond ((empty? A) #t)
          ((pertenece? (car A) B) (subconjunto? (cdr A) B))
          (else #f))))


;; (conjuntos-iguales? <Conjunto> <Conjunto>) --> <Booleano>

;; Produce #t si el cuanjunto A y el conjunto B son iguales

;; (plantilla)
;; (define conjuntos-iguales?
;;   (λ (A B)
;;     (cond((y subconjunto? A B)...))
;;       (....))))

;; (ejemplos)
;(check-expect (conjuntos-iguales? '() '(a s e r)) #f)
;(check-expect (conjuntos-iguales? '(a s e r) '(a s e r)) #t)
;(check-expect (conjuntos-iguales? '(a s e r) '()) #f)

(define conjuntos-iguales?
  (λ (A B)
    (y (subconjunto? A B)
       (subconjunto? B A))))


;; (subconjunto-propio? <Conjunto> <Conjunto>) --> <Booleano>

;; Produce #t si el cuanjunto A es subconjunto de B y el conjunto A es diferente al conjunto B

;; (plantilla)
;; (define subconjunto-propio?
;;   (λ (A B)
;;     (cond((y (subconjunto? A B)...))
;;       (else ...))))

;; (ejemplos)
;(check-expect (subconjunto-propio? '() '(a b c d e f)) #t)
;(check-expect (subconjunto-propio? '(a b) '(a b c d e f)) #t)
;(check-expect (subconjunto-propio? '(a b c d f g) '(e f b c g a)) #f)
;(check-expect (subconjunto-propio? '(a b c e f g) '(a b c d e f)) #f)

(define subconjunto-propio?
  (λ (A B)
    (cond((y (subconjunto? A B) (if (conjuntos-iguales? A B) #f #t)) #t)
         (else #f))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;cunatificador universal

;; (paraTodo <Predicado> <Dominio>) --> <Booleano>

;; Dado un predicado <P> un dominio <A> devuelve #t si cada elemento del conjunto cumple con el predicado <P>

;; (plantilla)
;;(define paraTodo
;;  (λ (P A)
;;    (cond ((empty? A) #t)
;;          ((P (car A)) .....)
;;          (... #f))))
;; (ejemplos)


(define paraTodo
  (λ (P A)
    (cond ((empty? A) #t)
          ((P (car A)) (paraTodo P (cdr A)))
          (else #f))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::.

;;cuantificador existencial

;; (existeUn <Predicado> <Dominio>) --> <Booleano>

;;Dado un redicado <P> y un dominio <A> devuelve #t si al menos un elemento cumple con el  dominio <A>

;;(plantilla)
;;(define existeUn?
;;  (λ (P A)
;;    (cond ((empty? A) ...)
;;          ((P (car A)) ...)
;;          (....))))
;;(ejemplos)
;(check-expect (existeUn? (λ (x) (> x 2)) '(0 2 3 4 5)) #t)
;(check-expect (existeUn? (λ (x) (> x 3)) '(0 -2 -3 -4 5)) #t)
;(check-expect (existeUn? (λ (x) (> x 10)) '(7 -1 -3 9 5)) #f)

(define existeUn?
  (λ (P A)
    (cond ((empty? A) #f)
          ((P (car A)) #t)
          (else (existeUn? P (cdr A))))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; Formato:: agregar: <elemento> <conjunto> --> <conjunto>
;; Objetivo:: crear un conjunto con los elementos del conjunto dado junto con el nuevo elemento
;; Ejemplos:
;; > (agregar 3 ’(2 4 6 8 10))
;; ’(3 2 4 6 8 10)
;; > (agregar 3 ’())
;; ’(3)
;; > (agregar ’() ’())
;; ’(())
(define agregar
  (λ (e C)
    (if (pertenece? e C)
        C
        (cons e C))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;; Formato: agregar : conjunto x conjunto -> conjunto
;; Proposito: construir un conjunto, haciendo la union de ellos
;; Ejemplos y casos de prueba:
;; > (union ’() ’(2 4 6 8 10))
;; ’(2 4 6 8 10)
;; > (union ’(5) ’(2 4 6 8 10))
;; ’(5 2 4 6 8 10)
;; > (union 3 ’())
;; ’(3)
;; > (union ’() ’())
;; ’()
(define union
  (λ (A B)
    (if (empty? A)
        B
        (union (cdr A) (agregar (car A) B)))))

;;%%%%%%%%%%%%%%%%%%%%%%%%%Tarea 10%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; (interseccion-auxilia <conjunto> <conjunto> <conjunto>) --> <conjunto>
;;
;; Proposito: Construir un conjunto, haciendo la intersccion de los primeros dos conjuntos
;;(plantilla)
;;(define interseccion-auxiliar
;;  (λ(A B C)
;;    (cond((empty? B) ...)
;;         ((empty? A) C)
;;         ((.... (car A) B) .....))
;;         (...(interseccion-auxiliar ....))) 

;; Ejemplos y casos de prueba:
;(check-expect (interseccion-auxiliar '() '(2 4 6 8 10) '()) '())
;(check-expect (interseccion-auxiliar '(2 4 6 8 10) '(11 12 13 14 15) '()) '())
;(check-expect (interseccion-auxiliar '(2 4 6 8 10) '(2 3 4 5) '()) '(4 2))


(define interseccion-auxiliar
  (λ(A B C)
    (cond((empty? B) '())
         ((empty? A) C)
         ((pertenece? (car A) B) (interseccion-auxiliar (cdr A) B (agregar (car A) C)))
         (else (interseccion-auxiliar (cdr A) B C))))) 

;;(interseccion <conjunto> <conjunto>) --> <conjunto>
;; Proposito: Construir un conjunto, haciendo la intersccion de los primeros dos conjuntos
;;ejemplos y casos de prueba
;(check-expect (interseccion '() '(2 4 6 8 10)) '())
;(check-expect (interseccion '(2 4 6 8 10) '(11 12 13 14 15)) '())
;(check-expect (interseccion '(2 4 6 8 10) '(2 3 4 5)) '(4 2))
(define interseccion
  (λ (A B)
    (interseccion-auxiliar A B '())))


;;==================== Tarea 11,12,13,14,15,16 ==================

; (diferencia <conjunto> <conjunto>) --> <conjunto>
;;
;; Proposito: Construir un conjunto, haciendo la diferencia entre los primeros dos conjuntos
;;
;;(plantilla)
;;(define diferencia
;;  (λ (A B)
;;   (diferencia-auxiliar A B '())))

;;(ejemplos)
;(check-expect (diferencia '(1 2 3 4) '()) '(4 3 2 1))
;(check-expect (diferencia '() '()) '())
;(check-expect (diferencia '(a b c d 1) '(e f r t 1 a)) '(d c b))
;(check-expect (diferencia-auxiliar '(1 2 3 4) '() '()) '(4 3 2 1))

(define diferencia-auxiliar
  (λ (A B C)
    (cond((empty? A) C)
         ((pertenece? (car A) B) (diferencia-auxiliar (cdr A) B C))
         (else (diferencia-auxiliar (cdr A) B (agregar (car A) C))))))
 
;; > (diferencia '(1 2 3 4) '(a b c 2 4))
;; '(3 1)
(define diferencia
  (λ (A B)
    (diferencia-auxiliar A B '())))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;(diferencia-simetrica <conjunto> <conjunto>)--> <conjunto>
;;
;; Proposito: Construir un conjunto, haciendo la diferencia simetrica entre dos conjuntos
;;
;; Ejemplos y casos de prueba:
;; > (diferencia-simetrica '(1 2 3 4) '(a b c 2 4))
;; '(1 3 c b a)
;; > (diferencia-simetrica '(a b c 2 4) '(1 2 3 4))
;; '(a b c 3 1)
;; > (diferencia-simetrica ’(a b c 2 4) '())
;; '(a b c 2 4)
;; > (diferencia-simetrica '(1 2 3 4) '())
;; '(1 2 3 4)
;; > (diferencia-simetrica '() '())
;; '()
;; >
(define diferencia-simetrica
  (λ (A B)
    (union (diferencia A B)
           (diferencia B A))))



;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define AA '(20 16 1 31 5 7 25 22 38 11 22 27 21 10 24 19 23 3 22 25))
(define BB '(15 32 21 10 16 14 29 30 9 18 7 8 20 24 27 17 18 22 5 36))

;:::::::::::::::::::::::::::::
;(diferencia BB AA) -> alumnos que cursan MD pero no estan en 4to ciclo
;(diferencia AA BB) -> alunos de 4to cuclo pero no estan en MD
;(interseccion AA BB) -> alumnos que estan en MD y cursan 4to ciclo
;;::::::::::::::::::::::::::::

;;1.1.- conjunto compuesto por los alumnos de cuarto ciclo que se han
;matriculado en MD

;;>>solo los que cursan MD y no estan
(define EE (diferencia (interseccion AA BB) (union (diferencia AA BB) (diferencia BB AA))))
;;'(20 16 5 7 22 27 21 10 24)


;;1.2.- conjunto compuesto por los alumnos de cuarto ciclo que no cursan MD

;;>>solamente los alumnos que cursan 4to pero no estan en MD
(define DD (interseccion AA (union (diferencia AA BB) (diferencia BB AA))))
;;'(3 23 19 11 38 25 31 1)

;;1.3 conjunto compuesto por los alumnos que o bien cursan el cuarto ciclo o que 
;se han matriculado en MD

;;>>son los alumnos que cursan MD o estan en 4to pero no en ambos
(define CC (diferencia BB (diferencia AA (union AA BB))))
;;'(36 5 22 17 27 24 20 8 7 18 9 30 29 14 16 10 21 32 15)

;1.4 conjunto compuesto por los alumnos que obien no cursan el cuarto ciclo o no se han matriculado en MD

;;>>es el conjunto vacio porque tanto como el conjunto AA como el conjunto BB estan en 4to y cursan MD, solo los que no
;esten en 4to y no cursen MD.
(define FF (interseccion (interseccion AA BB) (interseccion (diferencia AA BB) (diferencia BB AA))))
;;'()

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;(tu-numero <conjunto> <conjunto> <conjunto>)--> (numero >= 0 o <= 5)
;; Proposito: devuelve un numero si cumple con alguna de las condiciones

;;(platilla)
;;(define tu-numero
;;   (λ (A B C)
;;       (...))

;;(ejemplos)
;(check-expect  (tu-numero '(1 2 3 4) '() '(4 3 2 1)) 1)
;(check-expect  (tu-numero '() '() '()) 1)

(define tu-numero
  (λ (A B C)
    (cond((subconjunto? (union A B) (union A (union B C))) 1)
         ((subconjunto? (interseccion A (interseccion B C)) (interseccion A B)) 2)
         ((subconjunto? (diferencia (diferencia A B) C) (diferencia A C)) 3)
         ((equal? (interseccion (diferencia A C) (diferencia C B)) '()) 4)
         ((equal? (union (diferencia B A) (diferencia C A)) (diferencia (union B C) A)) 5)
         (else 0))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;(similitud-conjuntos <conjunto> <conjunto>)--> <real>
;; Proposito: devuelve que tan similares son los conjuntos

;;(platilla)
;;(define similitud-conjuntos
;;   (λ (A B)
;;       (cond((...)))))

;;(ejemplos)
;(check-expect (similitud-conjuntos '() '()) 1)
;(check-expect  (similitud-conjuntos '(a b) '(a b s)) (/ 2 3))
;(check-expect  (similitud-conjuntos '(a b) '()) 0)
;(check-expect (similitud-conjuntos '(a b 2 10 9 h y) '(3 b 4 w e r t 5 6)) (/ 1 15))

(define similitud-conjuntos
 (λ (A B)
   (cond((y (empty? A) (empty? B)) 1)
        (else (/ (cardinalidad (interseccion A B)) (cardinalidad (union A B)))))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;(union-generalizada <LitaDe/Conjunto>)--> <Conjunto>
;; Proposito: calcular la union generalizada de la lista de conjunto

;;(platilla)
;;(define union-generalizada
;;   (λ (A)
;;       (union-generalizada-auxiliar ...)))

;;(ejemplos)
;(check-expect (union-generalizada '((1 2 5 6) (3 6 9) (2 4 6) (9 7 5))) '(7 4 9 3 6 5 2 1))
;(check-expect (union-generalizada '((1) () (2) (1 2 3 9 7 5))) '(5 7 9 3 2 1))
;(check-expect (union-generalizada '()) '())
;(check-expect (union-generalizada '(() (8) (a b) ())) '(b a 8))

(define union-generalizada-auxiliar
  (λ (A B)
    (cond((empty? A) B)
         (else (union-generalizada-auxiliar (cdr A) (union (car A) B))))))


(define union-generalizada
  (λ (A) (union-generalizada-auxiliar A '())))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;(interseccion-generalizada <LitaDe/Conjunto>)--> <Conjunto>
;; Proposito: calcular la interseccion generalizada de la lista de conjunto

;;(platilla)
;;(define interseccion-generalizada
;;   (λ (A)
;;       (if (empty? A)'() ...)))

;;(ejemplos)
;(check-expect (interseccion-generalizada '((1 2) (2 4 1 3) (8 1 9 2 6))) '(2 1))
;(check-expect (interseccion-generalizada '((1 2) (2 4 1 3) (8 9 6))) '())
;(check-expect (interseccion-generalizada '((2 d w e r) (2 w n) (m w n y) (0 9 w))) '(w))

(define interseccion-generalizada-auxiliar
  (λ (A B)
    (cond((empty? A) B)
         (else (interseccion-generalizada-auxiliar (cdr A) (interseccion (car A) B))))))

(define interseccion-generalizada
  (λ (A)
    (if (empty? A) '()
        (interseccion-generalizada-auxiliar A (car A)))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;(LConjuntos <entero>) --> <listaDe/conjunto>
;;Proposito  recibe un numero entero no negativo devuelve una lista de conjuntos

;;(platilla)
;;(define LConjuntos
;;  (λ(n)
;;    (contruye-numeros ....)))

;;(ejemplos)
;>(LConjuntos 0)
;'((0))
;>(LConjuntos 1)
;'((0) (0 1))
;>(LConjuntos 2)
;'((0) (0 1) (0 1 2))
;>(LConjuntos 10)
;'((0) (0 1) (0 1 2) (0 1 2 3) (0 1 2 3 4) (0 1 2 3 4 5) (0 1 2 3 4 5 6) (0 1 2 3 4 5 6 7) (0 1 2 3 4 5 6 7 8) (0 1 2 3 4 5 6 7 8 9) (0 1 2 3 4 5 6 7 8 9 10))

(define contruye-numeros
  (λ(n res)
    (cond((equal? n 0)  (cons (build-list 1 values) res))
          (else (contruye-numeros (- n 1) (cons (build-list (+ n 1) values) res))))))
     
(define LConjuntos
  (λ(n)
    (contruye-numeros n '())))

;;===================== Tarea 17,18 ================================

;::::::::::::::::::::::::::::T17::::::::::::::::::::::::::::::::

; (conjunto-potencia <conjunto>) --> <listaDe/conjunto>
; Proposito recibe un conjunto en forma de lista, como ’(a b c)
; devuelve una lista de conjuntos, como ’((c b a) (c b) (c a) (c) (b a) (b) (a) ())
;ejemplo
;> (conjunto-potencia '(3 4 5))
;'((4 3) (4) () (3) (5 3) (5) (5 4) (5 4 3))
;> 

;;agrega a tdos los subconjuntos un elemento
(define agregar-a-todos
  (λ (e Lc)
    (union Lc (map (λ (Lc) (agregar e Lc)) Lc))))

;;recibe un conjunto y resto donde se guardara el resultado
(define conjunto-potencia-aux
  (λ (A res)
    (cond((empty? A) res)
         (else (conjunto-potencia-aux (cdr A) (agregar-a-todos (car A) res)))))) 
          

(define conjunto-potencia
  (λ (A)
    (conjunto-potencia-aux A '(()))))

;::::::::::::::::::::::::::T18::::::::::::::::::::::::::::::::::::::::::::

; (cubrimiento? <listaDe/conjunto> <conjunto>)--> <Booleano>
; Proposito: recibe una lista de conjuntos LC, como ’((c) (b a) (b) (a)) y un conjunto A
; devuelve #t si la lista de conjuntos LC es un cubrimiento para el conjunto A dado
; o #f si no es un cubrimiento
;ejemplos

;> (cubrimiento? '((a b) (c d) (a d)) '(a b c d))
;#t
;> (cubrimiento? '((a b) (c d) (a d)) '(a b c d e))
;#f
;> (cubrimiento? '((a 3) (c d) (a d)) '(a b 5 3 e))
;#f
;>
(define cubrimiento?
  (λ (LC A)
    (conjuntos-iguales? (union-generalizada LC) A)))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (particion? <listaDe/conjunto> <conjunto>)--> <Booleano>
; recibe una lista de conjuntos LC, como ’((c) (b a) (b) (a)) y un conjunto A
; devuelve #t si la lista de conjuntos LC es una particion para el conjunto A dado
; o #f si no es una particion
;
;ejemplos
;> (particion? '((a b) (c d)) '(a b c d))
;#t
;> (particion? '((a b) (c d) (a d)) '(a b c d))
;#f
;>
(define particion-aux
  (λ (LC res)
    (cond((empty? LC) res)
         ((empty? (cdr LC)) res)
         (else (particion-aux (cdr LC) (append (interseccion (car LC) (car (cdr LC))) res)))))) 

(define particion?
  (λ (LC A)
     (y (cubrimiento? LC A) (conjuntos-iguales? (particion-aux LC '()) '()))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::.

;=====================================================================
;; Formato:: crear-par: <elemento> <elemento> --> <par>
;; Objetivo:: crear un par con los elementos dados
;; Ejemplos:
;; > (crear-par 2 3)
;; ’(2 3)
;; > (crear-par 3 2)
;; ’(3 2)
;; >

(define crear-par-aux
  (λ (a b)
    (if (pair? b) (cons a b)
        (cons a (list b)))))

(define crear-par
  (λ (a b)
    (crear-par-aux a b)))
;    (if (pair? b) (cons a b)
;        (cons a b))))



;;;metodos accesores de los alementos del par

;; Formato:: primero-de-par: <par> --> <elemento>
;; Objetivo:: Obtener la primera entrada de un par
;; Ejemplos:
;; > (primero-de-par (crear-par 2 3))
;; 2
;; > (primero-de-par ’(3 2))
;; 3
;; >
(define primero-de-par
  (λ (p)
    (car p)))

;; Formato:: segundo-de-par: <par> --> <elemento>
;; Objetivo:: Obtener la segunda entrada de un par
;; Ejemplos:
;; > (segundo-de-par (crear-par 2 3))
;; 3
;; > (segundo-de-par ’(3 2))
;; 2
;;>
(define segundo-de-par
  (λ (p)
    (car (cdr p))))


;=============================Tarea 19=========================

;::::::::::::::::::::::::::::::::T19:::::::::::::::::::::::::::::::

;;(producto-cartesiano <LitaDe/Conjunto>) --> <ListaDeTuplas>
;; Proposito: calcular el producto cartesiano de la lista de conjunto

;;(platilla)
;;(define producto-cartesiano
;;   (λ LC
;;       (...))

;;(ejemplos)
;>(producto-cartesiano '(1 2 3))
;'((1) (2) (3))
;> (producto-cartesiano '(1 2 3) '(a b c))
;'((1 a) (2 a) (3 a) (1 b) (2 b) (3 b) (1 c) (2 c) (3 c))

(define agrega-por-derecha
  (λ (e C)
    (append C (list e))))

(define agrega-a-todos-aux 
  (λ(C e)
    (map (λ (c) (agrega-por-derecha e c)) C)))

(define agrega-a-todos*
  (λ(res LC)
    (apply append
            (map (λ(e) (agrega-a-todos-aux res e)) LC))))

(define producto-cartesiano-aux 
  (λ (res . LC)
    (cond ((empty? LC) res)
          (else (apply producto-cartesiano-aux  (cons (agrega-a-todos* res (primero-de-par LC)) (cdr LC)))))))

(define producto-cartesiano
  (λ LC
    (apply producto-cartesiano-aux (cons '(()) LC))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;======================Tarea 20,21,22,23,24,25==========================

;:::::::::::::::::::::::::::::T20::::::::::::::::::::::::::::::::::::::::::

;(dominio <Relacion> <Conjunto> <Conjunto>) --> <Conjunto>
;; Proposito: calcular el dominio de una relacion R 

;;(platilla)
;;(define dominio
;;   (λ (R A B)
;;       (...))
;;ejemplos
;> (dominio '((1 2) (2 3) (3 4)) '(1 2 3 4) '(1 2 3 4))
;'(3 2 1)
;> (dominio '((1 2) (2 3) (3 4)) '(1 2 3 4) '(1 2 3 4))
;'(3 2 1)
;> (dominio '((1 2) (2 3) (3 4) (4 1)) '(1 2 3) '(1 2 3 4))
;'(3 2 1)
;> (dominio '((1 2) (2 3) (3 4) (4 1)) '(1 2 3 4) '())
;'()
;> 

;;busca conjuntos iguales
(define busca-igual
  (λ (C R)
    (cond((empty? R) '(()()))
         ((equal? C (car R)) C)
         (else (busca-igual C (cdr R))))))

;agrega a la lista si esta en el dominio
(define agregar*
  (λ (e C)
    (cond((pertenece? e C)  C)
         ((equal? e '()) C)
         (else (cons e C)))))

;busca el dominio  
(define dominio*
  (λ (R A res)
    (cond((empty? A)  res)
         (else (dominio* R (cdr A) (agregar* (car (busca-igual (car A) R)) res))))))

(define dominio
  (λ (R A B)
  (dominio* R (producto-cartesiano A B) '())))

;::::::::::::::::::::T21:::::::::::::::::::::::::::

;(codominio <Relacion> <Conjunto> <Conjunto>) --> <Conjunto>
;; Proposito: calcular el codominio de una relacion R 

;;(platilla)
;;(define codominio
;;   (λ (R A B)
;;       (...))

;;ejemplos
;> (codominio '((1 2) (2 3) (3 4) (4 1)) '(1 2 3 4) '(1 2 3 4))
;'(1 2 3 4)
;> (codominio '((1 2) (2 3) (3 4) (4 1)) '(1 2 3 4) '(1 2 3 4))
;'(1 2 3 4)
;> (codominio '((1 2) (2 3) (3 4) (4 1)) '(1 2 3 4) '())
;'()
;> 
(define codominio
  (λ (R A B)
    B))

;:::::::::::::::::::::T22:::::::::::::::::::::::::::::::

;(rango <Relacion> <Conjunto> <Conjunto>) --> <Conjunto>
;; Proposito: calcular el rango de la Relacion R

;;(platilla)
;;(define rango
;;   (λ (R A B)
;;       (...))

;(ejemplos)
;> (rango '((1 2) (1 3) (4 1) (4 2)) '(1 2 3 4) '(1 2 3 4 5 6))
;'(3 2 1)
;> (rango '((1 2) (1 3)) '(1 2 3) '(1 2 3 4 5 6))
;'(3 2)
;> (rango '((1 2) (1 3) (2 5)) '(1) '(1 2 3 4 5 6))
;'(3 2)
;> (rango '((1 2) (1 3) (2 5)) '(4) '(1 2 3 4 5 6))
;'()
;> 

(define rango*
  (λ (R A res)
    (cond((empty? A)  res)
         (else (rango* R (cdr A) (agregar* (car (cdr (busca-igual (car A) R))) res))))))

(define rango
  (λ (R A B)
    (rango* R (producto-cartesiano A B) '())))

;::::::::::::::::::::::::::T23:::::::::::::::::::::::::::::::::

;(imagen <e> <Relacion> <Conjunto> <Conjunto>) --> <Conjunto>
;proposito: calcular la imagen de un elemento del dominio en una relacion

;(plantilla)
;(define imagen
;  (λ (a R A B)
;    (cond(...)
;         (else (...)))))

;ejemplos
;> (imagen 5 '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 5) (5 4)) '(1 2 3 4 5) '(1 2 3 4 5))
;'(2 5 4)
;> (imagen 8 '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 5) (5 4)) '(1 2 3 4 5) '(1 2 3 4 5))
;'()
;> (imagen 2 '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 5) (5 4)) '(1 2 3 4 5) '(1 2 3 4 5))
;'(3 5)
;>

(define imagen-aux
  (λ (a R A B res)
    (cond((empty? R) (reverse res))
          ((equal? a (car (car R))) (imagen-aux a (cdr R) A B (agregar (car (cdr (car R))) res)))
         (else (imagen-aux a (cdr R) A B res)))))
   
     
(define imagen
  (λ (a R A B)
    (cond((neg (pertenece? a A)) '())
         (else (imagen-aux a R A B '())))))

;:::::::::::::::::::::::::::::::T24:::::::::::::::::::::::::::::::::::::::::::::::::::::

;(imagen* <Conjunto> <Relacion> <Conjunto> <Conjunto>) --> <Conjunto>
;proposito: calcular la imagen de un conjunto en una relacion R

;(plantilla)

;(define imagen*
;  (λ (C R A B)
;    (union-generalizada...)))


;(ejemplos)
;> (imagen* '(5 2) '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 5) (5 4)) '(1 2 3 4 5) '(1 2 3 4 5))
;'(3 4 5 2)
;> (imagen* '() '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 5) (5 4)) '(1 2 3 4 5) '(1 2 3 4 5))
;'()
;> (imagen* '(1 2 3 4 5) '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 5) (5 4)) '(1 2 3 4 5) '(1 2 3 4 5))
;'(4 5 3 2)
;> 
(define imagen*
  (λ (C R A B)
    (union-generalizada (map (λ (e) (imagen e R A B)) C))))

;::::::::::::::::::::::::::::::::::T25::::::::::::::::::::::::::::::::::::::

;=======================LEE UNA RELACION DE UN ARCHIVO===========================
;::::::::::::::::::::EJEMPLO::::::::::::::::::::::::::::::
;;(lee-relacion "r01.rl") ;<--ESPECIFICAR EL ARCHIVO (LEER SU USO)
;'((1 1) (1 3) (1 5) (2 1) (2 2) (2 4) (3 3) (3 4) (3 5) (4 1) (4 2))
;>
;::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;crea una nueva lista segun una lista de ceros y uno
;ejemplo
;(crea-nueva-lista '(1 2 3 4 5) '(0 1 0 1 1) '())
;>'(2 4 5)
(define crea-nueva-lista
  (λ (A C res)
    (cond((empty? A) (reverse res))
         ((equal? (car C) 1) (crea-nueva-lista (cdr A) (cdr C) (cons (car A) res)))
         (else (crea-nueva-lista (cdr A) (cdr C) res)))))


;;crea par con un elemento y una lista
;ejemplo
;(crea-relacion* '1 '(2 4 5) '())
;>'((1 2) (1 4) (1 5))
(define crea-relacion*
  (λ (e B res)
    (cond((empty? B) (reverse res))
         (else (crea-relacion* e (cdr B) (cons (list e (car B)) res))))))

;;crea una R relacion con dos conjuntos A y B y una lista de una tabla de relacion
;
(define crea-relacion
  (λ (A B LC res)
    (cond((empty? LC) res)
         (else (crea-relacion (cdr A) B (cdr LC) (append res (crea-relacion* (car A) (crea-nueva-lista B (car LC) '()) '())))))))


;convierte cada cadena en numero
(define extrae-numero*
  (λ (cad res)
    (cond((empty? cad) res)
         (else (extrae-numero* (cdr cad) (append res (list (string->number (car cad)))))))))
     
(define extrae-numero
  (λ (cad)
    (extrae-numero* (string-split (string-append cad "\r\n\t"))'())))


;recibe el archivo a leer y un res donde guardara el resultado
(define lee-relacion*
  (λ (archivo res)
    (define linea (read-line archivo))
    (cond ((eof-object? linea) (close-input-port archivo) (map extrae-numero  res))
          (else (lee-relacion* archivo (append res (list linea)))))))

;;lee un archivo con el dominio codominio y la tabla de relaciones en 0s y 1s y regresa una relacion R
;ejemplo del contenido del archivo
;------------
;|1 2 3 4   | -->dominio
;|1 2 3 4 5 | -->codominio
;|1 0 1 0 1 | [
;|1 1 0 1 0 |   TABLA DE RELACION
;|0 0 1 1 1 |
;|1 1 0 0 0 | ]
;------------
;(especificar nombre y extencion)

;;(lee-relacion "r01.rl")
;'((1 1) (1 3) (1 5) (2 1) (2 2) (2 4) (3 3) (3 4) (3 5) (4 1) (4 2))
;>
(define lee-relacion
  (λ (path)
    (define archivo (open-input-file path))
    (define resultado (lee-relacion* archivo '()))
    (crea-relacion (car resultado) (car (cdr resultado)) (cdr (cdr resultado)) '())))
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;======================================Tarea 26============================================
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(define vacio '())
(define conjunto-vacio '())
(define relacion-vacia '())
(define conjunto-vacio? empty?)
(define relacion-vacia? empty?)

;;(reflexiva?: <listaDe/tuplas> <conjunto>)--> <booleano>
;;proposito: saber si una relacion es reflexiva
;; devuelve: un <booleano> es un valor de verdad. #t si la relacion es reflexiva, #f de otro modo
;;
;;ejemplos
;;> (reflexiva? '((1 1) (2 2) (3 3) (4 4) (5 5) (1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 4)) '(1 2 3 4 5))
;;#t
;;> (reflexiva? '((1 1) (2 2) (3 3) (2 3) (4 5) (5 5) ) '(1 2 3 4 5))
;;#f
;;> (reflexiva? '((1 1) (2 2) (3 3) (2 3) (4 5) (5 5) ) '())
;;#t
;;>

(define reflexiva?
  (λ (R A)
    (andmap (λ (a) (pertenece? (list (car A) (car A)) R)) (dominio R A A))))
;    (cond((y (conjunto-vacio? A) (relacion-vacia? R)) #t)
;         ((relacion-vacia? R) #f)
;         ((conjunto-vacio? A) #t)
;         ((pertenece? (crear-par (car A) (car A)) R) (reflexiva? R (cdr A)))
;         (else (reflexiva? (cdr R) A)))))
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;(irreflexiva?: <listaDe/tuplas> <conjunto>)--> <booleano>
;;proposito: saber si una relacion es irreflexiva
;; devuelve: un <booleano> es un valor de verdad. #t si la relacion es irreflexiva, #f de otro modo
;;
;;ejemplos
;> (irreflexiva? '((1 1) (2 2) (3 3) (4 4) (5 5) (1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 4)) '(1 2 3 4 5))
;#f
;> (irreflexiva? '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 4)) '(1 2 3 4 5))
;#t
;> (irreflexiva? '((1 2) (2 3) (3 4) (4 3) (5 2) (2 5) (5 4) (1 1)) '(1 2 3 4 5))
;#f
;> (irreflexiva? '((1 2) (2 3) (3 2) (1 3) (4 2) (2 1) (3 4) (1 2)) '(1 2 3 4))
;#t
;>

(define irreflexiva?
  (λ (R A)
    (cond((y (conjunto-vacio? A) (relacion-vacia? R)) #t)
         ((relacion-vacia? R) #f)
         ((conjunto-vacio? A) #t)
         ((pertenece? (list (car A) (car A)) R) #f)
         (else (irreflexiva? R (cdr A))))))
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;=================================TAREA 27,28,29,30,31=============================

;(par-inverso <par/a1 a2>)--><par/a2 a1>
;proposito invertir el par, devuelve el par invertido <a1 a2> --> <a2 a1>

;ejemplos
;> (par-inverso '(a b))
;'(b a)
;> (par-inverso '(10 0))
;'(0 10)
;> (par-inverso '(() 1))
;'(1 ())
;>
(define par-inverso
  (λ (p)
    (list (cadr p) (car p))))

;:::::::::::::::::::::::::T27::::::::::::::::::::::::::::::::::::

;(simetrica? <listaDe/tuplas> <Dominio>) --> <Booleano>

;proposito: devuelve #t si la <listaDe/tuplas>/(relacion) es simetrica 

;ejemplos
;> (simetrica? '((1 2) (1 3) (1 1) (2 2) (2 1) (3 1) (3 3)) '(1 2 3))
;#t
;> (simetrica? '((1 2) (1 1) (2 2) (3 1) (3 3)) '(1 2 3))
;#f
;> (simetrica? '((1 2) (1 1) (3 1) (3 3)) '(1 2 3))
;#f
;>

(define simetrica?
  (λ (R A)
    (paraTodo (λ (a) (paraTodo (λ (b) (doble-implicacion (pertenece? (list a b)  R) (pertenece? (list b a) R))) A)) A)))

;:::::::::::::::::::::::::::::T28:::::::::::::::::::::::::::::::::::::::::::::::

;(asimetrica? <ListaDe/tuplas> <Dominio>) --> <Booleano>

;proposito: devuelve #t si la <listaDe/tuplas>/(relacion) es asimetrica
 
;ejemplos
;> (asimetrica? '((1 2) (1 3) (1 1) (2 2) (2 1) (3 1) (3 3)) '(1 2 3))
;#f
;> (asimetrica? '((1 2) (1 3) (3 2)) '(1 2 3))
;#t
;> (asimetrica? '((1 2) (1 3) (3 2) (2 3)) '(1 2 3))
;#f
;>
(define asimetrica?
  (λ (R A)
    (paraTodo (λ (a) (paraTodo (λ (b) (implicacion (pertenece? (list a b)  R) (neg (pertenece? (list b a) R)))) A)) A)))

;::::::::::::::::::::::::::::::::T29:::::::::::::::::::::::::::::::::::::::::::::

;(antisimetrica? <ListaDe/tuplas> <Dominio>) --> <Booleano>

;proposito: devuelve #t si la <listaDe/tuplas>/(relacion) es antisimetrica?
  

;ejemplo
;> (antisimetrica? '((1 1)) '(1 2 3))
;#t
;> (antisimetrica? '((1 1) (2 2) (3 3)) '(1 2 3))
;#t
;> (antisimetrica? '((1 1) (2 2) (3 3) (1 2) (2 1)) '(1 2 3))
;#f
;> 
(define antisimetrica?
  (λ (R A)
   (paraTodo (λ (a) (paraTodo (λ (b) (implicacion (y (pertenece? (list a b)  R) (pertenece? (list b a) R)) (equal? a b))) A)) A)))

;:::::::::::::::::::::::::::::::::T30:::::::::::::::::::::::::::::::::::::::::::

;(transitiva? <ListaDe/tuplas> <Dominio>) --> <Booleano>

;proposito: devuelve #t si la <listaDe/tuplas>/(relacion) es transitiva?

;ejemplos
;> (transitiva? '((1 2) (2 3) (1 3) (3 1) (2 1) (1 1) (2 2)) '(1 2 3))
;#f
;> (transitiva? '((1 2) (2 3) (1 3) (3 1) (2 1) (1 1) (2 2) (3 3)) '(1 2 3))
;#f
;> (transitiva? '((1 2) (2 3) (3 2) (1 3) (3 1) (2 1) (1 1) (2 2) (3 3)) '(1 2 3))
;#t
;>
(define transitiva?
  (λ (R A)
    (paraTodo 
     (λ (a)
       (paraTodo 
        (λ (b)
          (paraTodo 
           (λ (c)
             (implicacion (y (pertenece? (list a b) R) (pertenece? (list b c) R)) (pertenece? (list a c) R)))
           A))
        A))
     A)))

;:::::::::::::::::::::::::::::TEX:::::::::::::::::::::::::::::::::::::::::::.

;(equivalencia?: <listaDe/tuplas> <conjunto>) --> <booleano>

; recibe: a) una relacion, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion;
; devuelve: un <booleano> es un valor de verdad. #t si la relacion es una relacion de equivalencia,
;#f de otro modo


;ejemplos
;> (equivalencia? '((1 1) (2 2) (3 3) (2 3) (3 2)) '(1 2 3))
;#t
;> (equivalencia? '((1 1) (2 1) (3 3) (2 3) (3 2)) '(1 2 3))
;#f
;> (equivalencia? '((1 1) (3 3) (2 3) (3 2)) '(1 2 3))
;#f
;>
(define equivalencia?
  (λ (R A)
    (and (reflexiva? R A)
         (simetrica? R A)
         (transitiva? R A))))

;::::::::::::::::::::::::::::::T31::::::::::::::::::::::::::::::::::::::::::

;(clase-equivalencia <elemento> <listaDe/tuplas> <conjunto> <conjunto>) --> <conjunto>

;proposito: devuelve un conjunto que se relaciona con el elemento y que pertenece a la relacion

;ejemplo
;> (clase-equivalencia 3 '((1 1) (2 2) (3 3) (2 3) (3 2)) '(1 2 3))
;'(2 3)
;> (clase-equivalencia 1 '((1 1) (2 2) (3 3) (2 3) (3 2)) '(1 2 3))
;'(1)
;> (clase-equivalencia 2 '((1 1) (2 2) (3 3) (2 3) (3 2)) '(1 2 3))
;'(2 3)
;> 
(define clase-equivalencia*
  (λ (a R A res)
    (cond((empty? A) res)
         ((pertenece? (list a (car A)) R) (clase-equivalencia* a R (cdr A) (append res (list (car A)))))
         (else (clase-equivalencia* a R (cdr A) res)))))
    

(define clase-equivalencia
  (λ (a R A)
    (cond((neg (equivalencia? R A)) 'NoEsEquivalencia)
         ((empty? A) '())
         (else (clase-equivalencia* a R A '())))))
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (relacion-complemento <listaDe/tuplas> <conjunto> <conjunto>) --> <conjunto>
; recibe: a) una relacion, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion
;         c) El conjunto codominio de la relacion
; devuelve: una tupla con tres elementos: 
;         1) <listaDe/tuplas> de elementos del producto cartesiano, que no pertenecen a la relacion
;         2) <conjunto> el dominio de la relacion complemento
;         3) <conjunto> el codominio de la relacion complemento
;ejemplo
;> (relacion-complemento '((1 a) (2 a) (3 b) (2 c) (3 a)) '(1 2 3) '(a b c))
;'(((1 b) (2 b) (1 c) (3 c)) (1 2 3) (a b c))
;> 
(define relacion-complemento
  (λ (R A B)
    (list (filter-not (λ (p) (pertenece? p R))
                      (producto-cartesiano A B))
          A ; --- el dominio
          B))) ;--- el codominio
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;===============================TAREA 32,33==============================

;::::::::::::::::::::::::::T32::::::::::::::::::::::::::::::::

; (relacion-inversa <listaDe/tuplas> <conjunto> <conjunto>) --> <listaDe/tuplas> <conjunto> <conjunto>
; recibe: a) una relacion, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion
;         c) El conjunto codominio de la relacion
;devuelve: una tupla con tres elementos: 
;        1) <listaDe/tuplas> de elementos del producto cartesiano,que no pertenecen a la relacion
;        2) <conjunto> el dominio de la relacion complemento
;        3) <conjunto> el codominio de la relacion complemento
;ejemplo

;> (relacion-inversa '((1 a) (2 a) (3 b) (2 c) (3 a)) '(1 2 3) '(a b c))
;'(((a 1) (a 2) (c 2) (a 3) (b 3)) (a b c) (1 2 3))

;> (relacion-inversa '((1 w) (2 w) (3 t) (2 q) (3 r)) '(1 2 3) '(q r t))
;'(((q 2) (r 3) (t 3)) (q r t) (1 2 3))
;>
(define relacion-inversa
  (λ (R A B)
    (list (filter (λ (p) (pertenece? (par-inverso p) R)) (producto-cartesiano B A)) B A)))

;::::::::::::::::::::::::::::T33:::::::::::::::::::::::::::::::::

; (relacion-composicion: <listaDe/tuplas> <conjunto> <conjunto>
;<listaDe/tuplas> <conjunto> <conjunto>) --> <listaDe/tuplas> <conjunto> <conjunto>
; recibe: a) una relacion R, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion R
;         c) El conjunto codominio de la relacio R
;         d) una relacion S, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         e) El conjunto dominio de la relacion S
;         f) El conjunto codominio de la relacion S
; devuelve: una lista con los tres elementos de la nueva relacion composicion
;         1) La lista de pares que conforman la composicion
;         2) El dominio
;         3) El codominio

;ejemplo
; >(relacion-composicion '((1 a) (2 c) (3 d) (4 a)) '(1 2 3 4) '(a b c d) '((a w) (a z) (c x) (d z)) '(d a c b) '(w x y z))
;'(((4 z) (1 z) (3 z) (2 x) (4 w) (1 w)) (1 2 3 4) (w x y z))
;
;>(relacion-composicion '((1 a) (2 b) (2 a) (3 c)) '(1 2 3) '(a b c) '((a x) (c y)) '(c b a) '(x y z))
;'(((3 y) (2 x) (1 x)) (1 2 3) (x y z))

;une pares corespondiente a la relacion 
(define unElemento
  (λ (R p res)
    (cond ((empty? R) res)
          ((equal? (car (cdr (car R))) (car p)) (unElemento (cdr R) p (cons (list (car (car R)) (car (cdr p))) res)))
          (else (unElemento (cdr R) p  res)))))

 ;auxiliar de composicion     
(define relacion-composicion*
  (λ (R S res)
    (cond((empty? S) res)
         (else (relacion-composicion* R (cdr S) (append (unElemento R (car S) '()) res))))))
          
(define relacion-composicion
  (λ (R A B S AB C)
    (cond((subconjunto? B AB) ;tienen que ser iguales el codominio de R y el dominio de S
          (list (relacion-composicion* (filter (λ (p) (pertenece? p R)) (producto-cartesiano A B))
                                 (filter (λ (w) (pertenece? w S)) (producto-cartesiano AB C)) '()) A C))
          (else '()))))          

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (es-funcion? <listaDe/tuplas> <conjunto> <conjunto>) --> <booleano>
; recibe: a) una relacion, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion
;         c) El conjunto codominio de la relacion
; devuelve: Un valor de verdad. #t si la relacion dada es una funcion, y #f si no lo es
(define es-funcion?
  (λ (R A B)
    (andmap (λ (a) (= 1 (cardinalidad (imagen a R A B)))) (dominio R A B))))
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;=============================Tarea 34,35,36=================================

;::::::::::::::::::::::::::::T34::::::::::::::::::::::::::::::::::::::::: 

;(funcion-definida <listaDe/tuplas> <conjunto> <conjunto>) --> <entero>
; recibe: a) una funcion f, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion f
;         c) El conjunto codominio de la relacio f
;devuelve: un numero entre 0, 1 o 2 de acuerdo con los siguientes casos
;         0 si la relacion no es una funcion
;         1 si la funcion esta completamente definida
;         2 si la funcion es parcialmente definida
;ejemplo
;> (funcion-definida '((1 1) (2 2) (3 3) (4 1)) '(1 2 3 4) '(1 2 3))
;1
;> (funcion-definida '((1 1) (2 2) (3 3)) '(1 2 3 4) '(1 2 3))
;2
;> (funcion-definida '((1 1) (2 2) (3 3) (3 1)) '(1 2 3 4) '(1 2 3))
;0

(define funcion-definida
  (λ (f A B)
    (cond((neg (es-funcion? f A B)) 0)
         ((neg (existeUn? (λ (a) (= 0 (cardinalidad (imagen a f A B)))) A)) 1)
         (else 2))))
                                    
;::::::::::::::::::::::::::::::T35::::::::::::::::::::::::::::::::::::::::

;(funcion-sobre? <listaDe/tuplas> <conjunto> <conjunto>) --> <entero>
; recibe: a) una funcion f, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion f
;         c) El conjunto codominio de la relacio f
; devuelve: un valor de verdad. #t si la funcion es sobreyectiva y #f si no loes
;ejemplos
;> (funcion-sobre? '((1 2) (2 3) (3 3) (4 1)) '(1 2 3 4) '(1 2 3 4))
;#f
;> (funcion-sobre? '((1 2) (2 3) (3 1) (4 4)) '(1 2 3 4) '(1 2 3 4))
;#t
;> (funcion-sobre? '((1 2) (2 3) (3 1) (4 1)) '(1 2 3 4) '(1 2 3 4))
;#f
;>
(define funcion-sobre?
  (λ (f A B)
    (cond((conjuntos-iguales? (rango f A B) B) #t)
         (else #f))))
    
;:::::::::::::::::::::::::::T36:::::::::::::::::::::::::::::::::::::::

;(funcion-1a1? <listaDe/tuplas> <conjunto> <conjunto>) --> <entero>
; recibe: a) una funcion f, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion f
;         c) El conjunto codominio de la relacio f
; devuelve: un valor de verdad. #t si la funcion es sobreyectiva y #f si no loes
;ejemplos
;> (funcion-1a1? '((1 2) (2 3) (3 1) (4 4)) '(1 2 3 4) '(1 2 3 4))
;#t
;> (funcion-1a1? '((1 2) (2 3) (3 3) (4 4)) '(1 2 3 4) '(1 2 3 4))
;#f
;> 
(define funcion-1a1?
  (λ (f A B)
    (cond((paraTodo
           (λ (a) (paraTodo 
                   (λ (b) 
                     (implicacion (subconjunto? (imagen a f A B) (imagen b f A B)) (equal? a b)))
                   A)) 
           A) #t)
         (else #f))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(funcion-biyectiva? <listaDe/tuplas> <conjunto> <conjunto>) --> <booleano>
; recibe: a) una relacion, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion
;         c) El conjunto codominio de la relacion
; devuelve: Un valor de verdad. #t si la relacion dada es una funcion biyectiva, y #f si no lo es
;ejemplos
;> (funcion-biyectiva? '((1 2) (2 3) (3 1) (4 4) (4 1)) '(1 2 3 4) '(1 2 3 4))
;#f
;> (funcion-biyectiva? '((1 2) (2 3) (3 1) (4 4)) '(1 2 3 4) '(1 2 3 4))
;#t
;> (funcion-biyectiva? '((1 2) (2 3) (3 1) (4 4) (2 2)) '(1 2 3 4) '(1 2 3 4))
;#f
;>
(define funcion-biyectiva?
  (λ (f A B)
   (y (funcion-sobre? f A B)
      (funcion-1a1? f A B))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;=============================TAREAS 37,38==============================

;::::::::::::::::::::::::::T37:::::::::::::::::::::::::::::::::::::

; (subconjunto->decimal <conjunto> <conjunto>) --> <entero>
; recibe: a) un subconjunto, dado como una lista de la forma ’(b a)
;         b) El conjunto del cual se toman los subconjuntos, como ’(a b c)
; devuelve: un numero entero que le corresponde al subconjunto dado

;ejemplo
;> (subconjunto->decimal '(b) '(c b a))
;2
;> (subconjunto->decimal '(a) '(c b a))
;1
;> (subconjunto->decimal '(c) '(c b a))
;4
;> (subconjunto->decimal '(c b) '(c b a))
;6

(define suma-binario ;suma la lista y devuelve un entero
  (λ (L res n)
    (cond((empty? L) res)
         ((= (car L) 1) (suma-binario (cdr L) (+ (expt 2 n) res) (+ n 1)))
         (else (suma-binario (cdr L) res (+ n 1))))))

(define subconjunto->decimal* 
  (λ (S A res)
    (cond((empty? A) (suma-binario res 0 0))
         ((pertenece? (car A) S) (subconjunto->decimal* S (cdr A) (append (list 1) res)))
         (else (subconjunto->decimal* S (cdr A) (append (list 0) res))))))
          
(define subconjunto->decimal
  (λ (S A)
    (cond((empty? S) 0)
         (else (subconjunto->decimal* S A '())))))

;::::::::::::::::::::::::::::::::::::::::T38::::::::::::::::::::::::::::::::::::::::::::::::::::

;(nCr <Conjunto> <numero>) -->(ListaDe/Conjuntos)
;recibe: a) un conjunto, dado como una lista de la forma ’(1 2 3)
;        b) un numero con el cual se haran las combinaciones 
; devuelve: las combinaciones de n elementos
;combinaciones

;ejemplos
;> (rcombinaciones '(a1 a2 a3 a4 a5) 3)
;'((a3 a4 a5) (a2 a4 a5) (a1 a4 a5) (a2 a3 a5) (a1 a3 a5) (a1 a2 a5) (a2 a3 a4) (a1 a3 a4) (a1 a2 a4) (a1 a2 a3))
;> (rcombinaciones '(1 2 3) 3)
;'((1 2 3))
;> (rcombinaciones '(1 2 3) 0)
;'(())
;> (rcombinaciones '(1 2 3) 1)
;'((3) (2) (1))
;>

;buca conjunto repetido
(define repetido?
  (λ (c L)
    (cond((empty? L) #f)
         ((conjuntos-iguales? c (car L)) #t)
         (else (repetido? c (cdr L))))))

;elimina los conjuntos repetidos        
(define elimina-repetidos
  (λ (L res)
    (cond((empty? L) res)
         ((repetido? (car L) (cdr L)) (elimina-repetidos (cdr L) res))
         (else (elimina-repetidos (cdr L) (cons (car L) res))))))

(define rcombinaciones*
  (λ (N r res x)
    (cond((= 0 r) (elimina-repetidos res '()))
         (else (rcombinaciones* N (- r 1) 
                     (filter (λ (C) (> (cardinalidad C) x)) 
                             (apply append (map (λ (c) (map (λ (a) (agregar a c)) N)) res))) (+ x 1))))))         
(define rcombinaciones 
 (λ (N r)
   (rcombinaciones* N r '(()) 0)))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;=============================TAREAS 39 y 40================================

;:::::::::::::::::::::::::::::::::T39:::::::::::::::::::::::::::::::::::::::

; (funcion-invertible? <listaDe/tuplas> <conjunto> <conjunto>) --> <booleano>
; recibe: a) una funcion f, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la relacion f
;         c) El conjunto codominio de la relacio f
; devuelve: un valor de verdad. #t si la funcion es invertible y #f si no lo es

;ejemplo

;> (funcion-invertible? '((1 2) (2 1) (3 3)) '(1 2 3) '(1 2 3))
;#t
;> (funcion-invertible? '((1 2) (2 1) (3 2)) '(1 2 3) '(1 2 3))
;#f
;> (funcion-invertible? '((2 2) (2 1) (3 1)) '(1 2 3) '(1 2 3))
;#f
;> (funcion-invertible? '((2 2) (1 1) (3 3)) '(1 2 3) '(1 2 3))
;#t

(define funcion-invertible?
  (λ (f A B)
    (es-funcion? (car (relacion-inversa f A B)) B A)))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (funcion-permutacion? <listaDe/tuplas> <conjunto> <conjunto>) --> <booleano>
; recibe: a) una relacion, con forma de lista de tuplas. p.ej. ’((1 2) (2 1))
;         b) El conjunto dominio de la funcion
;         c) El conjunto codominio de la funcion
; devuelve: Un valor de verdad. #t si la funcion dada es una permutacion, y #f si no lo es

;ejemplos
;> (funcion-permutacion? '((1 1) (2 2) (3 3)) '(1 2 3) '(1 2 3))
;#t
;> (funcion-permutacion? '((1 1) (2 1) (3 3)) '(1 2 3) '(1 2 3))
;#f
;> (funcion-permutacion? '((1 1) (2 1) (3 3)) '(1 2 3) '(1 2 3 4))
;#f
;>
(define funcion-permutacion?
  (λ (f A B)
    (y (funcion-biyectiva? f A B)
       (conjuntos-iguales? A B))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (! <entero-no-negativo>) --> <entero-no-negativo>
; recibe: <n> un numero entero no negativo y opcionalmente r, un numero entero no negativo.
; si no se proporciona <r>, su valor es <n>
; devuelve: el factorial de los ultimos <r> numeros (empezando desde 1)

;ejemplos
;> (! 5)
;120
;> (! 10 4)
;5040
;> (! 9)
;362880
;>
(define !
  (λ (n [r n])
    (define !-aux
      (λ (n r res)
        (if (= r 0)
            res
            (!-aux (- n 1) (- r 1) (* n res)))))
    (!-aux n r 1)))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::.::::::::

; (p <entero-positivo> <entero-positivo>) --> <entero-positivo>
; recibe: a) Un numero entero positivo, que representa la cardinalidad del conjunto
;         b) Un numero entero que representa el numero de elementos en cada permutacion
; devuelve: Un numero entero positivo que significa la cantidad de permutaciones

;ejemplos
;> (p 4 4)
;24
;> (p 3 2)
;6
;> (p 8 3)
;336
(define p
  (λ (n r)
    (! n r)))

;:::::::::::::::::::::::::::::::::::T40:::::::::::::::::::::::::::::::::::::::

; (permutaciones <conjunto>) --> <listaDe/permutaciones>
; recibe: a) Un conjunto A
; devuelve: Un conjunto de todas las permutaciones del conjunto dado

;ejemplos
;> (permutaciones '())
;'(())
;> (permutaciones '(a))
;'((a))
;> (permutaciones '(g t y))
;'((t y g) (y t g) (g y t) (y g t) (g t y) (t g y))

(define relaciona;relaciono cada elemento de B con uno de A
  (λ (A B res)
    (cond((empty? A) res)
         ((list? (car A)) (relaciona (cdr A) B 
                                     (append (map (λ (e) (agregar e (car A))) 
                                                  (filter-not (λ (a) (pertenece? a (car A))) B)) res)))
         (else (relaciona (cdr A) B 
                          (append (map (λ (e) (agregar e (list (car A)))) 
                                                 (filter-not (λ (a) (pertenece? a (list (car A)))) B)) res))))))

;recibe el conjunto A y el conjunto B y un numero
(define permutaciones*
  (λ (A B n)
    (cond((o (= n 0) (= n 1)) (list A)) ;si no hay elemento o si hay uno         
         ((> n 2) (permutaciones* (relaciona A B '()) B (- n 1)))
         (else (relaciona A B '())))))

(define permutaciones
  (λ (A)
    (permutaciones* A A (cardinalidad A))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;=================================TAREA 41==================================

;:::::::::::::::::::::::::::::::T41::::::::::::::::::::::::::::::::::::::::

; (despachar <funcion> <funcion>) --> (<funcion>)
; recibe dos funciones y regresa una funcion que recibe un parametro
;> (define f (despachar (λ (x) (* x 10)) (λ (x) (* x 2))))
;> f
;#<procedure:...Definiciones.rkt:1672:4>
;> (f 10)
;'(100 20)
(define despachar
  (λ (f g)
    (λ (a)
      (list (f a) (g a)))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (aplicacion-paralela <funcion> <funcion>) --> (<funcion>)
; recibe dos funciones y regresa una funcion que recibe dos parametros

;> (define G (aplicacion-paralela (λ (x) (* x 10)) (λ (x) (* x 2))))
;> G
;#<procedure:...Definiciones.rkt:1681:4>
;> (G 5 10)
;'(50 20)
;>
(define aplicacion-paralela
  (λ (f g)
    (λ (a b)
      (list (f a) (g b)))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;==============================================================================
;==============================#3rp=GRAFOS#====================================

;====================================T42 y T43 ================================

;se crea la estructura de datos nodo& con dos campos uno de informacion y otro para saber si el nodo fue explorado

(struct nodo& (info expl dist f c) #:mutable)

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(crear-nodo <cualquiercosa>) --> <nodo&>
;recibe: cualquier cosa
;regresa: un nodo

;ejemplos

;> (crear-nodo 1)
;#<nodo&>
;> (crear-nodo 'b)
;#<nodo&>
;> (crear-nodo 123)
;#<nodo&>
;>
(define crear-nodo
  (λ (inf)
   (nodo& inf #f +inf.0 +inf.0 '())))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(nodo-info <nodo&>) --> <infomacion del nodo>
;recibe: nodo&, si no es un nodo es un error
;regresa: info del nodo&

;ejemplos

;> (define n1 (crear-nodo 3))
;> n1
;#<nodo&>
;> (nodo-info n1)
;3
;> (define n2 (crear-nodo 'px))
;> n2
;#<nodo&>
;> (nodo-info n2)
;'px
;> 
(define nodo-info
  (λ (nodo)
    (if (nodo&? nodo) (nodo&-info nodo) 'ERR-NoNodo)))

;describe una lista de nodos
(define nodos-describir
  (λ (N)
    (map (λ (n) (nodo-info n)) N)))

;describe una lista de nodos
(define nodos-describir*
  (λ (N)
    (map (λ (n) (list (nodo-info n) (nodo-explorado? n) (nodo-info-dist n) (nodos-describir (nodo-camino n)))) N)))
;modifico el valor de vardad del nodo a #f o #t ya sea el caso
(define nodo-marcar
  (λ (nodo bool)
    (set-nodo&-expl! nodo bool)))

;pregunto si el nodo fue explorado
(define nodo-explorado?
  (λ (nodo)
    (nodo&-expl nodo)))

;modifica la distancia del nodo
(define nodo-dist!
  (λ (nodo d)
    (set-nodo&-dist! nodo d)))

;devuelve la informacion de la distancia
(define nodo-info-dist
  (λ (nodo)
    (nodo&-dist nodo)))

;retorna el camino del nodo 
(define nodo-camino
  (λ (nodo)
    (if (nodo&? nodo) (nodo&-c nodo) 'ERR-NoNodo)))

;modifica el camino del nodo
(define nodo-camino!
  (λ (nodo cam)
    (if (nodo&? nodo) (set-nodo&-c! nodo (append (nodo-camino nodo) cam)) 'ERR-NoNodo)))

;modifica el tiempo de finalizacion
(define nodo-tf!
  (λ (nodo f)
    (set-nodo&-f! nodo f)))

;devuelve el tiempo de finalizacion
(define nodo-tf
  (λ (nodo)
    (if (nodo&? nodo) (nodo&-f nodo) 'ERR-NoNodo)))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(nodos-iguales? <nodo&> <nodo&>) --> <Boolean>
;recibe: dos nodos de tipo nodo&
;regresa: #t si los nodos tienen la misma informacion, #f en caso contrario

;ejemplos

;> (define n1 (crear-nodo 1))
;> (define n2 (crear-nodo 2))
;> (define n3 (crear-nodo 1))
;> (nodos-iguales? n1 n2)
;#f
;> (nodos-iguales? n1 n3)
;#t
;>
(define nodos-iguales?
  (λ (na nb)
    (equal? (nodo-info na) (nodo-info nb))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;se crea la estructura de datos arista& con dos campos, nodo inicial y nodo final

(struct arista& (ni nf dis) #:mutable)
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(crear-arista <nodo&><nodo&>) --> <arista&>
;recibe: un nodo& inicial y un nodo& final
;regresa: una arista&

;ejemplo

;> (define n1 (crear-nodo 3))
;> (define n2 (crear-nodo 2))
;> (crear-arista n1 n2)
;#<arista&>
;>
(define crear-arista
  (λ (nodoi nodof)
    (arista& nodoi nodof +inf.0)))

;::::::::::::::::::::::::T42::::::::::::::::::::::::::::::::::::

;(arista-nodo-inicial <arista&>) --> <nodo&>
;recibe: una estrura de datos arista&
;devuelve: una estructura de datos nodo&

;ejemplo

;> (define n1 (crear-nodo 3))
;> (define n2 (crear-nodo 2))
;> (define a1 (crear-arista n1 n2))
;> (arista-nodo-inicial a1)
;#<nodo&>
;> (nodo-info (arista-nodo-inicial a1))
;3
(define arista-nodo-inicial
  (λ (arista)
    (if (arista&? arista) (arista&-ni arista) 'ERR-NoArista)))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;arista-nodo-final: <arista&> --> <nodo&>
;recibe: una estrura de datos arista&
;devuelve: una estructura de datos nodo&

;ejemplo

;> (define n1 (crear-nodo 3))
;> (define n2 (crear-nodo 2))
;> (define a1 (crear-arista n1 n2))
;> (arista-nodo-final a1)
;#<nodo&>
;>
;> (nodo-info (arista-nodo-final a1))
;2
(define arista-nodo-final
  (λ (arista)
    (if (arista&? arista) (arista&-nf arista) 'ERR-NoArista)))

;modifica la distancia de la arista
(define arista-dis! 
  (λ (arista d)
    (set-arista&-dis! arista d)))

;retorna la distancia de la arista
(define arista-info-dis 
  (λ (arista)
     (if (arista&? arista) (arista&-dis arista) 'ERR-NoArista)))


;describe una lista de aristas
(define aristas-describir
  (λ (A)
    (map (λ (a) (crear-par-aux (nodo-info (arista-nodo-inicial a)) (nodo-info (arista-nodo-final a)))) A)))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(aristas-iguales? <arista&> <arista&>) --> <booleano>
;determina si dos aristas son iguales.
;recibe: a) una arista&;
;        b) una arista&
;devuele: falso o verdadero.

;ejemplo

;> (define n1 (crear-nodo 1))
;> (define n2 (crear-nodo 2))
;> (define n3 (crear-nodo 1))
;> (define n4 (crear-nodo 2))
;> (define a1 (crear-arista n1 n2))
;> (define a2 (crear-arista n3 n4))
;> (aristas-iguales? a1 a2)
;#t
;> 
(define aristas-iguales?
  (λ (aristaA aristaB)
    (and (nodos-iguales? (arista-nodo-inicial aristaA)
                         (arista-nodo-inicial aristaB))
         (nodos-iguales? (arista-nodo-final aristaA)
                         (arista-nodo-final aristaB)))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;se crea la estructura de datos grafo& con dos campos, conjunto de aristas& y conjunto de nodos&
(struct grafo& (N A) #:mutable)
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(crear-grafo <conjuntoDearista&> <conjuntoDenodo&>) --> <grafo&>
;recibe: a) conjunto de arista&;
;        b) conjunto de nodo&
;devuele: una estructura de tipo grafo&.

;ejemplo

;(define n1 (crear-nodo 1))
;(define n2 (crear-nodo 2))
;(define n3 (crear-nodo 1))
;(define n4 (crear-nodo 3))
;(define a1 (crear-arista n1 n2))
;(define a2 (crear-arista n3 n4))
;(define Gr01 (crear-grafo (list n1 n2 n3) (list a1 a2)))
;> Gr01
;#<grafo&>
(define crear-grafo
  (λ (conjunto-Nodos conjunto-Aristas)
    (grafo& conjunto-Nodos conjunto-Aristas)))

;:::::::::::::::::::::::::::T43::::::::::::::::::::::::::::::::::::::

;(grafo-nodos <grafo&>) --> <listaDe/nodo&>
;recibe: una estructura de datos grafo&
;devuelve: un conjunto de nodo&

;ejemplo

;> (define n1 (crear-nodo 1))
;> (define n2 (crear-nodo 2))
;> (define n3 (crear-nodo 1))
;> (define n4 (crear-nodo 3))
;> (define a1 (crear-arista n1 n2))
;> (define a2 (crear-arista n3 n4))
;> (define Gr01 (crear-grafo (list n1 n2 n3) (list a1 a2)))
;> Gr01
;#<grafo&>
;> (grafo-nodos Gr01)
;'(#<nodo&> #<nodo&> #<nodo&>)
(define grafo-nodos
  (λ (grafo)
    (if (grafo&? grafo)  (grafo&-N grafo) 'ERR-NoGrafo)))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(grafo-aristas <grafo&>) --> <listaDe/arista&>
;recibe: una estructura de datos grafo&
;devuelve: un conjunto de arista&

;ejemplo

;> (define n1 (crear-nodo 1))
;> (define n2 (crear-nodo 2))
;> (define n3 (crear-nodo 1))
;> (define n4 (crear-nodo 3))
;> (define a1 (crear-arista n1 n2))
;> (define a2 (crear-arista n3 n4))
;> (define Gr01 (crear-grafo (list n1 n2 n3) (list a1 a2)))
;> Gr01
;#<grafo&>
;> (grafo-aristas Gr01)
;'(#<arista&> #<arista&>)
(define grafo-aristas
  (λ (grafo)
    (if (grafo&? grafo)  (grafo&-A grafo) 'ERR-NoGrafo)))

;===================================T44-T45======================================

;::::::::::::::::::::::::::::::::::T44::::::::::::::::::::::::::::::::::::::::::::::::::

; (grafo-describir <grafo&>) --> <listaDe <listaDe/nodos> <listaDe/aristas>>
; recibe: una estructura de datos de tipo <grafo&>
; devuelve: una lista que contiene el conjunto de nodos (no son estructuras de datos)
;y una lista de pares ordenados, sus aristas (no son Est. Dat.)

;ejemplo

;; Definimos los nodos
;(define n1 (crear-nodo 1))
;(define n2 (crear-nodo 2))
;(define n3 (crear-nodo 3))
;(define n4 (crear-nodo 4))
;; y el conjunto de nodos
;(define N (list n1 n2 n3 n4))
;; creamos las aristas
;(define a1 (crear-arista n1 n2))
;(define a2 (crear-arista n1 n3))
;(define a3 (crear-arista n2 n3))
;(define a4 (crear-arista n3 n2))
;(define a5 (crear-arista n3 n3))
;(define a6 (crear-arista n4 n3))
;; y el conjunto de aristas
;(define A (list a1 a2 a3 a4 a5 a6))
;; Ahora creamos el grafo
;(define Gr02 (crear-grafo N A))

;;> (grafo-describir Gr02)
;;'((1 2 3 4) ((1 2) (1 3) (2 3) (3 2) (3 3) (4 3)))
;;> 
(define grafo-describir
  (λ (G)
    (list (map nodo-info (grafo-nodos G)) 
          (map (λ (arista) 
                 (list (nodo-info (arista-nodo-inicial arista))
                       (nodo-info (arista-nodo-final arista))))
               (grafo-aristas G)))))

(define grafo-describir*
  (λ (G)
    (list (map nodo-info (grafo-nodos G)) 
          (map (λ (arista) 
                 (list (nodo-info (arista-nodo-inicial arista))
                       (nodo-info (arista-nodo-final arista)) 
                       (arista-info-dis arista)))
               (grafo-aristas G)))))

;::::::::::::::::::::::::::::::T45::::::::::::::::::::::::::::::::::::::::::::::::
;::::::::::::::::LEER EL GRAFO DE UN ARCHIVO:::::::::::::::::::


;::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::;creo una lista de nodos y una lista de aristas

;busca un nodo que tenga la informacion que recibe
(define busca-nodoX
  (λ (a Lnodo)
    (cond((equal? (nodo-info (car Lnodo)) a) (car Lnodo))
         (else (busca-nodoX a (cdr Lnodo))))))

;auxiliar de crear-LA
(define crear-LA-aux1
  (λ (N n nodosX)
    (crear-arista N (busca-nodoX n nodosX))))

;auxiliarde crear-LA
(define crear-LA-aux
  (λ (N LN res nodosX)
    (cond((empty? LN) res)
         (else (crear-LA-aux N (cdr LN) (cons (crear-LA-aux1 N (car LN) nodosX) res) nodosX)))))        

;crea una lista de aristas
(define crear-LA
  (λ (Lnodos LC res nodosX)
    (cond((empty? Lnodos) (reverse res))         
         (else (crear-LA (cdr Lnodos) (cdr LC) (crear-LA-aux (car Lnodos) (cdr (car LC)) res nodosX) nodosX)))))

;lee todo el archivo
;ejemplo del contenido del archivo
;|----------| 
;|1 2 3 4   | 
;|2 2 3     | 
;|3 1 4     | 
;|4 1       | ;<----"sin salto de linea al final de la ultima lista"
;|----------|
(define leer
  (λ (flujo res)    
      (define linea (read-line flujo))      
        (cond((eof-object? linea) (close-input-port flujo) (map extrae-numero res))
             (else (leer flujo (append res (list linea)))))))

;> ejemplo de pruba (leer "g01.dat")
;'((1 2 3 4) (2 2 3) (3 1 4) (4 1)) ;se crea lista de listas
;-------------------------------------------------------------------

;leer-archivo: <string> --> <grafo&>
;recibe: un string con el nombre del archivo de texto plano
;devuelve: una estructura de datos <grafo&>

;ejemplo
;> (leer-grafo "grafo.dat")
;#<grafo&>
;>
;> (grafo-describir (leer-grafo "grafo.dat"))
;'((1 2 3 4 5 6 7 8) ((1 2) (1 3) (1 8) (2 1) (2 7) (3 5) (4 5) (5 7) (6 5) (7 4) (7 3) (8 6) (8 7)))
;>

;lee el grafo de forma de adyacencia
(define leer-grafo
  (λ (path)
    (let* ([archivo (open-input-file path)]
           [adya (leer archivo '())] ;se crea listas de listas
           [nodosX (map (λ (N) (crear-nodo (car N))) adya)]
           [aristasX (crear-LA nodosX adya '() nodosX)])
      (crear-grafo nodosX aristasX)))) ; se crea el grafo

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;crea aristas apartide de una tripleta '(1 2 3) tomando en cuenta que el ultimo es el valor de la arista en distancia
(define crear-arista+dis
  (λ (tripleta LN)
    (let* ([arista (crear-arista (busca-nodoX (car tripleta) LN) (busca-nodoX (cadr tripleta) LN))])
      (arista-dis! arista (caddr tripleta))
      arista)))

;crea tipletas con los nodos y la distancia entre ellos
(define lista-triple
  (λ (LtX)
    (define lista-aux
      (λ (Lt res)
        (cond ((empty? Lt) (map (λ (l) (cons (car LtX) l)) res))  
              (else (lista-aux (cddr Lt) (append res (list (list (car Lt) (cadr Lt)))))))))
    (lista-aux (cdr LtX) '())))

;lee un texto plano que representa un grafo ponderado, con distancia de las aristas
(define leer-grafo-ponderado
  (λ (path)
    (let* ([archivo (open-input-file path)]
           [adya (leer archivo '())]
           [nodosX (map (λ (N) (crear-nodo (car N))) adya)]
           [tripleta (apply append (map (λ (ly) (lista-triple ly)) adya))]
           [aristasX (map (λ (a) (crear-arista+dis a nodosX)) tripleta)])
      (crear-grafo nodosX aristasX))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;========================================T46========================================

;:::::::::::::::::::::::::::::::::::::::::T46::::::::::::::::::::::::::::::::::

;realiza una lista que corresponde al primer nodo con su correspondiente relacion
;recibe un nodo, la lista de pares que son las aristas y una lista '();
;es auxiliar a crea-lista
(define une
  (λ (e A res)
    (cond ((empty? A) (list (cons e res)))
          ((equal? e (car (car A))) (une e (cdr A) (agrega-por-derecha (cadr (car A)) res)))
          (else (une e (cdr A) res)))))

;crea una lista  de lista que sera la lista de adyacencia
;recibe una lista de nodos, la lista de aristas que son pares y una lista '()
(define crea-lista
  (λ (N A res)
    (cond((empty? N) res)
         (else (crea-lista (cdr N) A (append res (une (car N) A '())))))))

;escribe en el archivo la fila correspondiente a la primera lista de la lista de adyacencia
;recibe una lista y el archivo en donde escribira
;auxiliar a escribe
(define escribe-aux
  (λ (Lt archivo)
    (cond ((= (length Lt) 1) (display (string-append (number->string (car Lt)) "\n") archivo)) ;si solo queda un elemento
          (else (display (string-append (number->string (car Lt)) " ") archivo) (escribe-aux (cdr Lt) archivo)))))

;escribe toda la lista de adyacencia en un archivo
;recibe una lista de listas que corresponde a la lista de adyacencia y un archivo para escribir en el.
;regresa #t si se escribio con exito
(define escribe
  (λ (Lt archivo)
    (cond ((empty? Lt) (close-output-port archivo) #t)
          (else (escribe-aux (car Lt) archivo) (escribe (cdr Lt) archivo)))))

;(escribir-grafo <grafo&> <string>) --><Boolean>
;recibe: a) una estructura de tipo grafo&
;        b) un estring que corresponde al nombre del archivo con el que se guardara
;regresa: un archivo en el "directorio actual de este archivo" con la informacion de las relaciones del grafo
;
;ejemplo
;> (escribir-grafo (leer-grafo "grafo.dat") "test.dat")
;#t
;> 
(define escribir-grafo
  (λ (grafo nombre)
    (let* ([lista (grafo-describir grafo)]
           [nodos (car lista)]
           [aristas (cadr lista)]
           [adya (crea-lista nodos aristas '())]
           [salida (open-output-file nombre #:exists 'replace)])
           (escribe adya salida))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;###########################PINTA MI GRAFO############################################

;(pinta-grafo <Grafo> <nombre-para-guardar>) --> <imagen.jpg>
;ejemplo
;(pinta-grafo (leer-grafo "grafo.dat") "g06.jpg")
(define  pinta-grafo*
  (λ (LP salida nombre)
    (cond((empty? LP) (display (string-append "\tlabel=\"" nombre "\"\n}") salida) (close-output-port salida)
                      (if (system (string-append "dot -Tpng grafo.dot -o" (string-append nombre".png"))) 
                          (make-object bitmap% (string-append nombre".png") 'png) 'Not-Image))         
         (else (display (string-append "\t" (number->string (car (car LP))) " -> " (number->string (car (cdr (car LP)))) "\n") salida) 
               (pinta-grafo* (cdr LP) salida nombre)))))
     
(define pinta-grafo
  (λ (grafo nombre)
    (let ([LP (car (cdr (grafo-describir grafo)))]
           [salida (open-output-file "grafo.dot" #:exists 'replace)])
      (display "digraph G \n{ \n\tnode [shape=circle];\n\tlayout=circo;\n\tsize=\"3,3;\"\n" salida);
      (pinta-grafo* LP salida nombre))))

;######################################PINTA-GRAFO-PONDERADO#########################

;(pinta-grafoP <Grafo> <nombre-para-guardar>) --> <imagen.jpg>
;ejemplo
;(pinta-grafoP (leer-grafo "grafoP.dat") "g06.jpg")
(define  pinta-grafop*
  (λ (LP salida nombre)
    (cond((empty? LP) (display (string-append "\tlabel=" nombre "\n}") salida) (close-output-port salida)
                      (if (system (string-append "dot -Tpng grafo.dot -o" (string-append nombre".png"))) 
                          (make-object bitmap% (string-append nombre".png") 'png) 'Not-Image))         
         (else (display (string-append "\t" (number->string (car (car LP))) " -> " (number->string (car (cdr (car LP))))
                                       "[fontcolor=blue label="(number->string (car (cddr (car LP)))) "]" "\n") salida) 
               (pinta-grafop* (cdr LP) salida nombre)))))
     
(define pinta-grafop
  (λ (grafo nombre)
    (let ([LP (car (cdr (grafo-describir* grafo)))]
           [salida (open-output-file "grafo.dot" #:exists 'replace)])
      (display "digraph G \n{ \n\tnode [shape=circle];\n\tlayout=\"circo\";\n\tsize=\"3;\"\n" salida);
      (pinta-grafop* LP salida nombre))))

;###################################################################################

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; nodo-adyacente?: <nodo&> <nodo&> <grafo&> --> <booleano>
; Requiere: a) un nodo, supuestamente el inicial
             ;b) un nodo, supuestamente el final.
             ;c) un grafo
; Devuelve: #t si el nodo inicial es adyacente al nodo final propuesto. #f en otro caso
(define nodo-adyacente?
  (λ (ni nf G)
    (ormap (λ (e) (aristas-iguales? e (crear-arista ni nf))) (grafo-aristas G))))

; nodos-adyacentes: <grafo&> <nodo&> --> <listaDe/nodo&>
; Requiere: a) el nodo inicial
             ;b) un grafo
; Devuelve: la lista de nodos que son adyacentes al nodo propuesto

(define nodos-adyacentes
  (λ (ni G)
    (filter (λ (nf) (nodo-adyacente? ni nf G)) (grafo-nodos G))))

;===================================T47,T48====================================

;::::::::::::::::::::::::::::::::T47::::::::::::::::::::::::::::::::::::::::::

;devuelve si una arista& pertenece aun conjunto de aritas
(define pertenece-arista?
  (λ (a A)
    (cond((empty? A) #f)
          ((aristas-iguales? a (car A)) #t)
          (else (pertenece-arista? a (cdr A))))))

;crea realciones ;una lista de pares
(define crea-funcion
  (λ (A B res)
    (cond ((empty? B) (reverse res))
          (else (crea-funcion (cdr A) (cdr B) (cons (crear-par-aux (car A) (car B)) res))))))

;auxiliar a crea biyecciones; fuciones biyectivas
(define crea-CBiyectiva
  (λ (A B res)
    (cond ((empty? B) res)
          (else (crea-CBiyectiva A (cdr B) (cons (list (crea-funcion A (car B) '()) A (car B)) res))))))

;crea todas las funciones biyectivas recibe dos listas de nodos del mismo tamaño
(define crea-biyecciones
  (λ (A B)
    (crea-CBiyectiva A (permutaciones B) '())))

;devuelve el primer nodo de la arista
;recibe una arista&
(define 1de
  (λ (A)
    (arista-nodo-inicial A)))

;devuelve el segundo nodo de la arista
;recibe una arista&
(define 2de
  (λ (A)
    (arista-nodo-final A)))

;recibe dos grafo G y H pero no compra los tamaños de las lista de los nodos
(define grafos-isomorfos-aux?
  (λ (G H)
    (let ([CB (crea-biyecciones (car (grafo-describir G)) (car (grafo-describir H)))]
           [Ag (grafo-aristas G)]  ;;aristas del grafo G
           [Bh (grafo-aristas H)]) ;;aristas del grafo H
           (ormap
            (λ (f)
              (andmap
               (λ (a)
                 ;(printf "~s~n" (imagen (nodo-info (1de a)) (car f) (cadr f) (caddr f)))
                 (let ([na (crear-nodo (car (imagen (nodo-info (1de a)) (car f) (cadr f) (caddr f))))] ;;nodo&
                        [nb (crear-nodo (car (imagen (nodo-info (2de a)) (car f) (cadr f) (caddr f))))]) ;;nodo&
                   (pertenece-arista? (crear-arista na nb) Bh)))
               Ag))
            CB))))

;(grafos-isomorfos? <grafo&> <grafo&>) --><Boolean>
;recibe: a) un grafo G (una estructura de tipo grafo&)
;        b) un grafo G (una estructura de tipo grafo&)
;regresa: #t si G y H son grafos isomorfos, #f en otro caso
;
;ejemplo
;G y H son grafos leidos de un archivo de texto
;> (grafos-isomorfos? (leer-grafo "G.dat") (leer-grafo "H.dat"))
;#f
;> 
(define grafos-isomorfos?
  (λ (G H)
    (if (= (cardinalidad (car (grafo-describir G))) (cardinalidad (car (grafo-describir H))))
        (grafos-isomorfos-aux? G H)
        #f)))

;::::::::::::::::::::::::::::::::::T48:::::::::::::::::::::::::::::::::::::::::

;(nodo-grado-entrada <nodo&> <grafo&>) --> <entero>
;recibe: a) un nodo
;        b) una estructura de datos grafo&
;devuelve: un numero. El grado de entrada del nodo en el grafo

(define nodo-grado-entrada
  (λ (n G)
    (cardinalidad (filter (λ (p) (equal? (nodo-info n) (car (cdr p)))) (car (cdr (grafo-describir G)))))))


;(nodo-grado-salida <nodo&> <grafo&>) --> <entero>
;recibe: a) un nodo
;        b) una estructura de datos grafo&
;devuelve: un numero. El grado de salida del nodo en el grafo

(define nodo-grado-salida
  (λ (n G)
    (cardinalidad (nodos-adyacentes n G))))

;(nodo-grado <nodo&> <grafo&>) --> <entero>
;recibe: a) un nodo
;        b) una estructura de datos grafo&
;devuelve: un numero. El grado del nodo en el grafo

(define nodo-grado
  (λ (n G)
    (+ (nodo-grado-entrada n G) (nodo-grado-salida n G))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;===========================================T49,T50============================================
;::::::::::::::::::::::::::::::::::::T49:::::::::::::::::::::::::::::::::::::::::::

;(grafo-compleo? <grafo&>) --> <boolean>
;recibe: a) una estructura grafo&        
;devuelve: #t si el grafo es completo, #f en otro caso

;ejemplo, "el ejemplo se realizo
;con un grafo leido de un archivo *.dat"

;>(grafo-completo? (leer-grafo "K5.dat")
;#t
;> (grafo-completo? (leer-grafo "K5a.dat"))
;#f
(define grafo-completo?
  (λ (G)
    (let* ([Ng (nodos-describir (grafo-nodos G))]
           [Ag (aristas-describir (grafo-aristas G))])
      (y (simetrica? Ag Ng) (irreflexiva? Ag Ng)))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(nodos-inducidos <grafo&> <subconjunto de "arista&" del grafo&>)--> <lista/De <info-nodos>>
;recibe: a) una estructura grafo&
;        b) un subconjunto de arista& del grafo& "a)"
;devuelve: los nodos inducidos del grafo& por el conjunto de aristas

;ejemplo
;> (nodos-inducidos G (list a1 a3 a5 a9))
;'(1 2 3 4)
(define nodos-inducidos
  (λ (G A)
    (let ([Ng (car (grafo-describir G))])
    (filter (λ (n) (existeUn? (λ (a) (o (= n (nodo-info (1de a))) (= n (nodo-info (2de a))))) A)) Ng))))
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(aristas-inducidas <grafo&> <subconjunto de "nodo&" del grafo&>)--> <lista/DePares>
;recibe: a) una estructura grafo&
;        b) un subconjunto de nodo& del grafo& "a)"
;devuelve: las aristas inducidas del grafo& por el conjunto de nodos

;ejemplo
;> (aristas-inducidas G (list n1 n3))
;'((1 3) (3 1))
;> 
(define aristas-inducidas
  (λ (G N)
    (let ([Ag (cadr (grafo-describir G))]
          [Np (nodos-describir N)])
    (filter (λ (a) (subconjunto? a Np)) Ag))))

;::::::::::::::::::::::::::::::::::::T50:::::::::::::::::::::::::::::::::::::::

;(subgrafo? <grafo&> <grafo&>) --> <Boolean>
;recibe: a) una estructura grafo& 
;        b) otra estructura grafo&
;devuele: #t si el grafo& de "a)" es un subgrafo del grafo& de "b)"

;ejemplo, "el ejemplo se realizo
;con un grafo leido de un archivo *.dat"

;> (subgrafo? (leer-grafo "G.dat") (leer-grafo "H.dat"))
;#f
(define subgrafo?
  (λ (G H)
    (let* ([N1 (nodos-describir (grafo-nodos G))]
           [A1 (aristas-describir (grafo-aristas G))]
           [N2 (nodos-describir (grafo-nodos H))]
           [A2 (aristas-describir (grafo-aristas H))])
      (cond ((y (subconjunto? N1 N2) (conjuntos-iguales? A1 (aristas-inducidas H (grafo-nodos G)))) #t)
            ((y (subconjunto? A1 A2) (conjuntos-iguales? N1 (nodos-inducidos H (grafo-aristas G)))) #t)
            (else #f)))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(union-grafo <grafo&> <grafo&>) --> <grafo&>
;recibe: a) una estructura grafo&
;        b) otra estructura grafo&
;devuelve: la union del primer y segundo grafo

(define union-grafo
  (λ (G H)
    (let* ([Ng (nodos-describir (grafo-nodos G))]
           [Nh (nodos-describir (grafo-nodos H))]
           [Ag (aristas-describir (grafo-aristas G))]
           [Ah (aristas-describir (grafo-aristas H))])
      (let* ([Ngh (union Ng Nh)]
             [Agh (union Ag Ah)]
             [nodos (map (λ (n) (crear-nodo n)) Ngh)]
             [aristas (map (λ (a) (crear-arista (crear-nodo (car a)) (crear-nodo (cadr a)))) Agh)])
        (crear-grafo nodos aristas)))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;====================================T51,52,53,54,55=======================================
;::::::::::::todos los ejemplos fueron leidos de un archivo *.dat con formato de lista de adyacencia::::::::::::::::

;:::::::::::::::::::::::::::::::::::::T51::::::::::::::::::::::::::::::::::::::::::::::

;auxiliar a grafo-bipartita? 
;crea todas las biarticione del grafo G, recibe una lista de informacion de nodos del grafo
(define crea-biparticiones
  (λ (Lt)
    (let ([c (filter (λ (e) (neg (equal? e '()))) (conjunto-potencia (cdr Lt)))])
      (map (λ (cv) (list cv (union (list (car Lt)) (diferencia Lt cv)))) c))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(grafo-bipartita? <grafo&>) --> <Boolean>
;recibe: a) una estructura grafo&       
;devuelve: #t si el grafo es bipartita #f si no lo es

;ejemplo, "el ejemplo se realizo
;con un grafo leido de un archivo *.dat"

;> (grafo-bipartita? (leer-grafo "bi.dat"))
;#t
;>
(define grafo-bipartita?
  (λ (G)
    (let* ([Bp (crea-biparticiones (nodos-describir (grafo-nodos G)))]
           [Ag (aristas-describir (grafo-aristas G))])
      (existeUn? (λ (b) 
          (paraTodo (λ (a) 
              (ox (y (pertenece? (car a) (car b)) (pertenece? (cadr a) (cadr b)))
                  (y (pertenece? (car a) (cadr b)) (pertenece? (cadr a) (car b))))) Ag)) Bp))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
; (pertenece-nodo? <nodo&> <listaDe/nodo&>)--> <Boolean>
; Recibe; a) Un nodo
;         b) una lista de nodos
; devuelve: #t si el nodo a pertenece a la lista de nodos b
(define pertenece-nodo?
  (λ (a A)
    (cond ((empty? A) #f)
          ((nodos-iguales? a (car A)) #t)
          (else (pertenece-nodo? a (cdr A))))))

;::::::::::::::::::::::::::::::::::::T52::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo; construir un paseo unitario con el nodo inicial <v0> en un grafo <G>
; (grafo-paseo-unitario <nodo&> <grafo&>)--> <listaDe/nodo&>
; Recibe; a) Un nodo, el nodo inicial <v0>
;         b) Un grafo <G>
; devuelve: la lista initaria (<v0>).

;ejemplo
;> (define pas (grafo-paseo-unitario (crear-nodo 1) (leer-grafo "paseo.dat")))
;> (nodos-describir pas)
;'(1)
;> pas
;'(#<nodo&>)
;> 
(define grafo-paseo-unitario
  (λ (v0 G)
    (if (pertenece-nodo? v0 (grafo-nodos G)) (list v0) 'No-Existe)))

;::::::::::::::::::::::::::::::::::::::::T53::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Determinar si una lista de nodos es un paseo en un grafo dado
; (paseo-por-nodos?  <listaDe/nodo&> <grafo&>)--> <booleano>
; recibe; a) Una secuencia de nodos <W>
;         b) Un grafo <G>
; devuelve: #t si la lista de nodos dada es un paseo; #f en otro caso

;ejemplo
;(paseo-por-nodos? (list  (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))
;>#t
(define paseo-por-nodos?
  (λ (w G)
    (cond ((empty? w) #f)
          ((equal? '() (cdr w)) #t)
          ((nodo-adyacente? (car w) (cadr w) G) (paseo-por-nodos? (cdr w) G))
          (else #f))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Determinar si una lista de aristas es un paseo en un grafo dado
; (paseo-por-aristas?  <listaDe/nodo&> <grafo&>)--> <booleano>
; recibe; a) Una secuencia de Aristas <E>
;         b) Un grafo <G>
; devuelve: #t si la lista de aristas dada es un paseo; #f en otro caso

;ejemplo
;> (paseo-por-aristas? (list (crear-arista (crear-nodo 1) (crear-nodo 2)) 
;                            (crear-arista (crear-nodo 3) (crear-nodo 4))) 
;                      (leer-grafo "paseo.dat"))
;
;#f
(define paseo-por-aristas?
  (λ (e G)
    (cond ((empty? e) #f)
          ((equal? '() (cdr e)) #t)
          ((nodos-iguales? (2de (car e)) (1de (cadr e))) (paseo-por-aristas? (cdr e) G))
          (else #f))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Determina el nodo que es el inicio de un paseo
; (paseo-inicio  <listaDe/nodo&> <grafo&>) --> <nodo&>
; recibe:  a) Una secuencia de Nodos
;          b) Un grafo <G>
; devuelve: <nodo&>

; ejemplos
;> (paseo-inicio (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))
;#<nodo&>
;> (nodos-describir (list (paseo-inicio (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))))
;'(1)
(define paseo-inicio
  (λ (W G)
    (if (paseo-por-nodos? W G)
        (car W)
        'NoDef)))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Determina el nodo que es el final de un paseo
; (paseo-final <listaDe/nodo&> <grafo&>) --> <nodo&>
; recibe: a) Una secuencia de Nodos
;         b) Un grafo <G>
; devuelve: <nodo&>

; ejemplos
;> (paseo-final (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))
;#<nodo&>
;> (nodos-describir (list (paseo-final (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))))
;'(3)
;>
(define paseo-final
  (λ (W G)
    (if (paseo-por-nodos? W G)
        (last W)
        'NoDef)))

;::::::::::::::::::::::::::::::::::::::T54::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: si la <listaDe/nodo&> es un paseo ciclo
; (paseo-ciclo? <listaDe/nodo&> <grafo&>) --> <booleano>
; Recibe: Un paseo <W> y El grafo <G> que es la base de los paseos
; Devuelve: #t si <W> es un ciclo, y #f si <W> no es un ciclo.

;ejemplo
;> (paseo-ciclo? (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))
;#t
(define paseo-ciclo?
  (λ (w G)
    (nodos-iguales? (paseo-inicio w G) (paseo-final w G))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Calcular el paseo inverso un paseo en un grafo dado
; (paseo-inverso <listaDe/nodo&> <grafo&>) --> <listaDe/nodo&>
; recibe; a) Un paseo en forma de una secuencia de nodos <W> 
;         b) Un grafo <G>
; devuelve: Un paseo en forma de una secuencia de nodos <W-inverso>

;ejemplo
;> (paseo-inverso (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))
;'(#<nodo&> #<nodo&> #<nodo&>)
;(nodos-describir (paseo-inverso (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat")))
;'(3 2 1)
(define paseo-inverso
  (λ (w G)
    (if (y (paseo-por-nodos? w G) (paseo-por-nodos? (reverse w) G)) (reverse w) '())))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo : Anexar dos paseos
; (grafo-anexar-paseos <listaDe/nodo&> <listaDe/nodo&> <grafo&>) --> <listaDe/nodo&>
; Recibe: a) Un paseo <W1>, 
;         b) Un paseo <W2> 
;         c) un grafo <G> que es la base de los paseos
; Devuelve: Un nuevo paseo que empieza en el inicio del paseo A
; y termina en el final del paseo B <w10,w11,..,w1k,w20,w21,...,w2l>

;ejemplo
;> (nodos-describir (grafo-anexar-paseo (list (crear-nodo 1) (crear-nodo 2)) 
;                                       (list (crear-nodo 2) (crear-nodo 3)) 
;                                       (leer-grafo "paseo.dat")))
;'(1 2 3)
(define grafo-anexar-paseo
  (λ (w1 w2 G)
    (if (y (paseo-por-nodos? w1 G) (paseo-por-nodos? w2 G))
          (if (o (nodos-iguales? (paseo-final w1 G) (paseo-inicio w2 G))
               (nodo-adyacente? (paseo-final w1 G) (paseo-inicio w2 G) G))
            (append w1 (cdr w2)) 'NoseUnen)
          'NoPaseo)))

;::::::::::::::::::::::::::::::::::::::::T55::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Anexar todos los nodos adyacentes a un paseo, generando una coleccion de paseos
; (grafo-paseo+1 <listaDe/nodo&> <grafo&>) --> <listaDe/ <listaDe/nodo&>>
; Recibe: a) Un paseo en forma de una lista de nodos (W)
;         b) Un grafo, que es la base de los paseos (G)
; Devueve: Una lista de todos los paseos, iniciando con el paseo dado y terminando
; en cada uno de los nodos adyacentes al final del paseo original

;ejemplo
;> (grafo-paseo+1 (list (crear-nodo 1) (crear-nodo 2) (crear-nodo 3)) (leer-grafo "paseo.dat"))
;'((#<nodo&> #<nodo&> #<nodo&> #<nodo&>) (#<nodo&> #<nodo&> #<nodo&> #<nodo&>) (#<nodo&> #<nodo&> #<nodo&> #<nodo&>))
;> 
(define grafo-paseo+1
  (λ (w G)
    (map (λ (wi) (append w (grafo-paseo-unitario wi G))) (nodos-adyacentes (paseo-final w G) G))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Anexar todos los nodos adyacentes a una familia de paseos.
; (grafo-paseos+1 <listaDe/paseo> <grafo&>) --> <listaDe/paseo>
; Recibe: a) Una lista paseos, en laforma de lista de listas de nodos (W*)
;         b) El grafo base (G)
; Devuelve: Una lista de paseos, generando para cada paseo, una lista de paseos al anexar un nodo ms.

;ejemplo
;> (grafo-paseos+1 (list (list (crear-nodo 1) (crear-nodo 2)) (list (crear-nodo 2) (crear-nodo 5))) (leer-grafo "paseo.dat"))
;'((#<nodo&> #<nodo&> #<nodo&>) (#<nodo&> #<nodo&> #<nodo&>) (#<nodo&> #<nodo&> #<nodo&>) (#<nodo&> #<nodo&> #<nodo&>))
(define grafo-paseos+1
  (λ (w* G)
    (apply append (map (λ (wi) (grafo-paseo+1 wi G)) w*))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;grafo-k-paseos* ----> auxiliar a grafo-k-paseos

; Recibe: a) Un numero entero no negativo <k>
;         b) El nodo inicial <n>
;         c) Un grafo <G>
;         d) una lista paseo W

(define grafo-k-paseos*
  (λ (k v G PI)
    (cond((= k 0) PI)
         (else (grafo-k-paseos* (- k 1) v G  (grafo-paseos+1 PI G))))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Calcular los paseos de longitud k, de un grafo G
; (grafo-k-paseos <numero> <grafo&>) --> <listaDe <listaDe/nodo&>>
; Recibe: a) Un numero entero no negativo <k>
;         b) El nodo inicial <n>
;         c) Un grafo <G>
; Devuelve: una lista de paseos, todos ellos de longitud <k>

;ejemplo
;(grafo-k-paseos 2 (crear-nodo 1) (leer-grafo "paseo.dat"))
;'((#<nodo&> #<nodo&> #<nodo&>)
;  (#<nodo&> #<nodo&> #<nodo&>)
;  (#<nodo&> #<nodo&> #<nodo&>)
;  (#<nodo&> #<nodo&> #<nodo&>)
;  (#<nodo&> #<nodo&> #<nodo&>)
;  (#<nodo&> #<nodo&> #<nodo&>)
;  (#<nodo&> #<nodo&> #<nodo&>)
;  (#<nodo&> #<nodo&> #<nodo&>))
(define grafo-k-paseos
  (λ (k v G)
    (grafo-k-paseos* k v G (list (grafo-paseo-unitario v G)))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;============================================T56===========================================

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;verifica si hay nodos repetidos en los caminos
(define sin-repetidos?
  (λ (W)
    (= (cardinalidad W) (cardinalidad (remove-duplicates W)))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Determina el conjunto de los nodos es un camino
; (camino?  <listaDe/nodo&> <grafo&>) --> <Boolean>
; recibe; a) una lista de nodos
;         c) el grafo base <G>
;devuelve: #t si la lista de nodos es un camino, #f en otro caso

;ejemplo

;> (camino? (list (crear-nodo 2) (crear-nodo 3) (crear-nodo 5)) (leer-grafo "paseo.dat"))
;#t
;> 
(define camino?
  (λ (W G)
    (y (paseo-por-nodos? W G) (sin-repetidos? (nodos-describir W)))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Determina el conjunto de los k-caminos con inicio en un nodo <v> de un grafo <G>
; (grafo-k-caminos  <numero> <nodo&> <grafo&>) --> <listaDe/ <listaDe/nodo&>>
; recibe; a) un numero entero no negativo, <k> que es la longitud de los caminos
;         b) el nodo inicial <v>
;         c) el grafo base <G>
;devuelve: una lista de caminos < W1 W2 ... >

;ejemplo

;> (define c-3 (grafo-k-caminos 3 (busca-nodoX 2 (grafo-nodos (leer-grafo "paseo.dat")))  (leer-grafo "paseo.dat")))
;> (map (λ (c) (nodos-describir c)) c-3)
;'((2 3 5 2) (2 3 5 6) (2 5 2 3) (2 5 6 4) (2 5 6 7))
;> 
(define grafo-k-caminos
  (λ (k v G)
    (filter (λ (W) (camino? W G)) (grafo-k-paseos k v G))))

;::::::::::::::::::::::::::::::::::::::T56::::::::::::::::::::::::::::::::::::::::::::

; objetivo: obtener una lista con todos los caminos hamiltonianos de un grafo <G>
; (grafo-caminos-hamiltonianos <grafo&>) --> <listaDe/ <listaDe/nodo&>>
; recibe; a) el grafo base <G>
;devuelve: una lista de caminos hamiltonianos < W1 W2 ... >

;ejemplo
;> (map (λ (c) (nodos-describir c)) (grafo-caminos-hamiltonianos (leer-grafo "ha2.dat")))
;'((1 2 3 4 5)
;  (1 2 5 3 4)
;  (1 2 5 4 3)
;  (2 3 4 5 1)
;  (2 5 4 3 1)
;  (3 1 2 5 4)
;  (3 4 5 1 2)
;  (4 3 1 2 5)
;  (4 5 1 2 3)
;  (4 5 3 1 2)
;  (5 1 2 3 4)
;  (5 4 3 1 2))
(define grafo-caminos-hamiltonianos
  (λ (G)
    (let ([lg (- (cardinalidad (grafo-nodos G)) 1)])
      (apply append (map (λ (n) (grafo-k-caminos lg n G)) (grafo-nodos G))))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: calcular los ciclos hamiltonianos de un grafo <G>
; (grafo-ciclos-hamiltonianos <grafo&>) --> <listaDe/ <listaDe/nodo&>>
; recibe; a) el grafo base <G>
;devuelve: una lista de ciclos hamiltonianos < W1 W2 ... >

;ejemplo

;> (map (λ (c) (nodos-describir c)) (grafo-ciclos-hamiltonianos (leer-grafo "ha2.dat")))
;'((1 2 3 4 5 1)
;  (1 2 5 4 3 1)
;  (2 3 4 5 1 2)
;  (2 5 4 3 1 2)
;  (3 1 2 5 4 3)
;  (3 4 5 1 2 3)
;  (4 3 1 2 5 4)
;  (4 5 1 2 3 4)
;  (5 1 2 3 4 5)
;  (5 4 3 1 2 5))
;> 
(define grafo-ciclos-hamiltonianos
  (λ (G)
    (let ([ciclos (apply append (map (λ (w) (grafo-paseo+1 w G)) (grafo-caminos-hamiltonianos G)))])
      (filter (λ (C) (nodos-iguales? (car C) (last C))) ciclos))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;===========================================T57=============================================

;::::::::::::::::::::::::::::::::::::::::T57:::::::::::::::::::::::::::::::::::::::::::

; procedimiento auxiliar
; (bfs-aux <grafo&> <Cola> <lista>) --> <listaDe/nodo&>>
(define bfs-aux
  (λ (G Q BFS)
    (cond ((empty? Q) BFS)
          ((nodo-explorado? (car Q)) (bfs-aux G (cdr Q) BFS))
          (else (begin                  
                  (nodo-marcar (car Q) #t)                  
                  (bfs-aux G (append Q (nodos-adyacentes (car Q) G))
                             (agrega-por-derecha (car Q) BFS)))))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: enlista los nodos del grafo en orden BFS
; (bfs <grafo&> <nodo&>) --> <listaDe/nodo&>>
; recibe; a) el grafo base <G>
;         b) el nodo inicial para el recorrido
;devuelve: una lista los nodos del grafo en orden BFS <nodo& nodo&,...>

;ejemplo
;> (bfs (leer-grafo "paseo.dat") (busca-nodoX 1 (grafo-nodos (leer-grafo "paseo.dat"))))
;'(#<nodo&> #<nodo&> #<nodo&> #<nodo&> #<nodo&> #<nodo&> #<nodo&>)
;> 
;> (nodos-describir (bfs (leer-grafo "paseo.dat") (busca-nodoX 1 (grafo-nodos (leer-grafo "paseo.dat")))))
;'(1 2 5 7 3 6 4)
(define bfs
  (λ (G v1)
    (bfs-aux G (list v1) '())))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;==================================T58,T59,T60=======================================

;:::::::::::::::::::::::::::::::::::::::::T58:::::::::::::::::::::::::::::::::::::::::

; procedimiento auxiliar
; (dfs-aux <grafo&> <Cola> <lista>) --> <listaDe/nodo&>>

(define dfs-aux
  (λ (G S DFS)
    (cond ((empty? S) DFS)
          ((nodo-explorado? (car S)) (dfs-aux G (cdr S) DFS))
          (else (begin
                   (nodo-marcar (car S) #t)
                   (dfs-aux G (append (nodos-adyacentes (car S) G) S) (agrega-por-derecha (car S) DFS)))))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: enlista los nodos del grafo en orden DFS
; (dfs <grafo&> <nodo&>) --> <listaDe/nodo&>>
; recibe; a) el grafo base <G>
;         b) el nodo inicial para el recorrido
;devuelve: una lista los nodos del grafo en orden DFS <nodo& nodo&,...>

;ejemplo

;(define G (leer-grafo "g19.dat"))
;> (nodos-describir (dfs G (busca-nodoX 1 (grafo-nodos G))))
;'(1 4 2 5 8 6 3 7 9)
;>
(define dfs
  (λ (G v1)
    (dfs-aux G (list v1) '())))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (grafo-nodirigido? <grafo&>) --> <Boolean>
; recibe; a) el grafo base <G>;        
;devuelve: #t si el grafo es un grafo no dirigido y #f en otro caso

;ejemplo

;> (grafo-nodirigido? (leer-grafo "g19.dat"))
;#t
;>
(define grafo-nodirigido?
  (λ (G)
    (simetrica? (cadr (grafo-describir G)) (car (grafo-describir G))))) 

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Enlistar las componentes conexas de un grafo no dirigido
; formato: gcomp-aux: <grafo&> <listaDe/nodo&> <lista/De/nodo&> --> <listaDe/ <listaDe/nodo&>>
; recibe; Un grafo <G>; la lista de nodos en el grafo <N>;
;y la lista de las componentes conectadas <c-conexas> inicialmente vacia.
; devuelve: una lista con las componentes conexas del grafo <<va...> <vb....> ....>

(define gcomp-aux
  (λ (G N c-conexas)
    (cond ((empty? N) c-conexas)
          (else (let* ([c (bfs G (car N))]
                      [n (diferencia N c)])
                  (gcomp-aux G n (agregar c c-conexas)))))))

;::::::::::::::::::::::::::::::::::::T59::::::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Enlistar las componentes conexas de un grafo no dirigido
; (grafo-componentes-nd <grafo&>) --> <listaDe/ <listaDe/nodo&>>
; recibe; Un grafo <G>
; devuelve: una lista con las componentes conexas del grafo <<va...> <vb....> ....>

;ejemplo

;> (map (λ (n) (nodos-describir n)) (grafo-componentes-nd (leer-grafo "g17.dat")))
;'((2 4) (10 6 8) (1 3 5 7 9))

;> (map (λ (n) (nodos-describir n)) (grafo-componentes-nd (leer-grafo "g19.dat")))
;'((1 4 8 9 2 6 7 5 3))
(define grafo-componentes-nd
  (λ (G)
    (if (grafo-nodirigido? G)
        (gcomp-aux G (grafo-nodos G) '()) 'Err.NoDet)))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (inicializar-grafo <grafo&>)
; recibe; a) el grafo base <G>;        
; inicializa en grafo a su estado inicial cuando fue creado

(define inicializar-grafo
  (λ (G)
    (begin
      (for-each (λ (n) (nodo-dist! n +inf.0)) (grafo-nodos G))
      (for-each (λ (n) (nodo-tf! n +inf.0)) (grafo-nodos G))
      (for-each (λ (n) (nodo-camino! n '())) (grafo-nodos G))
      (for-each (λ (n) (nodo-marcar n #f)) (grafo-nodos G)))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; (aumentar-distancia-nodos <listaDe/nodo&> <entero>)
; recibe; a) lista de nodos
;         b) una distancia la cual se le aumentara al nodo
;aumentar la distancia  de todos los nodos

(define aumentar-distancia-nodos
  (λ (Ln d)
    (begin
      (for-each (λ (n) (nodo-dist! n 0)) Ln)
      (for-each (λ (n) (nodo-dist! n (+ 1 d (nodo-info-dist n)))) Ln))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; objetivo: Calcular la distancia entre dos nodos de un grafo
; (dis-bfs-aux: <grafo&> <nodo&> <listaDe/nodo&>) --> <numero>
; recibe; Un grafo <G>;
;         el nodo final <nf>
;         una cola <Q> inicialmente con el nodo inicial;
; devuelve: un numero no negativo que es la distancia de un nodo a otro, de acuerdo con la longitud del camino mas corto

(define dis-bfs-aux
  (λ (G nf Q)
    (cond ((empty? Q) (nodo-info-dist (car Q)))
          ((nodos-iguales? (car Q) nf) (nodo-info-dist (car Q)))
          ((nodo-explorado? (car Q)) (dis-bfs-aux G nf (cdr Q)))
          (else (begin
                  (let* ([n (car Q)]
                         [nad (nodos-adyacentes n G)])
                  (nodo-marcar n #t)
                  (aumentar-distancia-nodos nad (nodo-info-dist n))
                  (dis-bfs-aux G nf (cdr (append Q nad)))))))))

;::::::::::::::::::::::::::::::::::::::::T60:::::::::::::::::::::::::::::::::::::::

; objetivo: Calcular la distancia entre dos nodos de un grafo
; (dis-bfs <grafo&> <nodo&> <nodo&>)--> <numero>
; recibe; a) Un grafo <G>;
;         b) el nodo incial <ni>
;         c) el nodo final <nf>
; devuelve: un numero no negativo que es la distancia de un nodo a otro

;ejemplos
;> (define G (leer-grafo "g19.dat"))
;> (dis-bfs G (busca-nodoX 3 (grafo-nodos G)) (busca-nodoX 6 (grafo-nodos G)))
;1
;> (inicializar-grafo G)
;> (dis-bfs G (busca-nodoX 3 (grafo-nodos G)) (busca-nodoX 8 (grafo-nodos G)))
;4
;> 
(define dis-bfs
  (λ (G ni nf)
    (inicializar-grafo G)
    (nodo-dist! ni 0)
    (dis-bfs-aux G nf (list ni))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;============================================T61,62======================================

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;aumenta el tiempo de una lista de nodos
;es un metodo auxiliar para el dfs-interno
(define aumenta-tiempo
  (λ (Lt t res)
    (cond ((empty? Lt) res)          
          (else (begin
                  (nodo-tf! (car Lt) t)
                  (aumenta-tiempo (cdr Lt) (- t 1) (append res (list (car Lt)))))))))

;:::::::::::::::::::::::::::::::::::::::T61:::::::::::::::::::::::::::::::::::::::::::::

; objetivo: Obtener la familia de nodos alcanzables de todo el grafo
; (dfs-completo: <grafo&> [OP<listaDe/nodo&>]) --> <listaDe/<listaDe/nodo&>>
; recibe; Un grafo <G>;
; opcionalmente una lista de nodos, ubicados en unorden arbitrario <N>
; devuelve: una lista de listas de nodos, cada sublista con los nodos accesibles desde
; un nodo tomado como lider.

;ejemplo
;> (define G20 (leer-grafo "g20.dat"))
;> (define completo (dfs-completo G20))
;> (map (λ (n) (nodos-describir n)) completo)
;'((1 4 5 2 3 8 9) (6 10 7 11))

(define dfs-completo
  (λ (G #:Nodos [N (grafo-nodos G)])
    (define dfs-interno
      (λ (N dfs-orden)
        (if (empty? N) dfs-orden
            (let* ([comp (dfs G (car N))]                   
                   [dif (diferencia (nodos-describir N) (nodos-describir comp))]                   
                   [orden (append dfs-orden '())]
                   [orden (append orden (list comp))]
                   [t (cardinalidad (append* orden))]
                   [comp (aumenta-tiempo comp  t '())])
              (dfs-interno (filter (λ (n) (pertenece? (nodo-info n) dif)) N) (append dfs-orden (list comp)))))))
    (inicializar-grafo G)
    (dfs-interno N '())))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;crea aristas desde de una lista de pares que corresponde a las arsitas
;recibe la lista de aristas y la lista de nodos
(define crea-arista+
  (λ (ab LN)
    (crear-arista (busca-nodoX (car ab) LN) (busca-nodoX (cadr ab) LN))))

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;(grafo-inverso <grafo&>) --> <grafo&>/inverso
;recibe un grafo
;devuelve en grafo inverso del grafo dado
(define grafo-inverso
  (λ (G)
    (let* ([aristas (aristas-describir (grafo-aristas G))]
           [aristaInV (map (λ (a) (reverse a)) aristas)]
           [nodosX (grafo-nodos G)]
           [aristasX (map (λ (a) (crea-arista+ a nodosX)) aristaInV)])
      (crear-grafo nodosX aristasX)))) 

;:::::::::::::::::::::::::::::::::::::T62:::::::::::::::::::::::::::::::::::::::::::::::

; obtener las componentes fuertemente conexas de un grafo dirigido
; (scc-kosaraju <grafo&>) --> <listaDe <listaDe nodo&>>
; recibe un grafo <G>
; devuelve una lista con las componentes fuertemente conexas del grafo G

;ejemplo
;> (define G20 (leer-grafo "g20.dat"))
;> (define scc (scc-kosaraju G20))
;> (map (λ (n) (nodos-describir n)) scc)
;'((2 3 8) (5) (11 10 7 6) (1 4 9))

(define scc-kosaraju
  (λ (G)
    (let* ([Grev (grafo-inverso G)]
           [LN1 (apply append (dfs-completo Grev))]
           [LN2 (sort LN1 (λ (x y) (> (nodo-tf x) (nodo-tf y))))])
      (dfs-completo G #:Nodos LN2))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;calcula la longitud de un camino

(define longitud-camino
  (λ (G W)
    (if (paseo-por-aristas? W G) (apply + (map (λ (a) (arista-info-dis a)) W)) 'NoEsUnPaseo)))

;::::::::::::::::::::::::::::::::::::::T63 :::::::::::::::::::::::::::::::::::::::::::::::::::::

;; Actualiza la distancia mas corta desde un nodo a todos los dems. se debe tener un grafo
;; etiquetado, porque se consideran las etiquetas como referente para las distancias
;; FORMATO: (dijkstra #<grafo&> #<nodo-inicial&>) -> #<grafo&>
;  recibe:a) un grafo <G>
;         b) un nodos innicial del grafo <G>
;devuelve: un <grafo&>

; ejemplo

;> (define Gp04 (leer-grafo-ponderado "gp04.dat"))
;> (define dsk (dijkstra Gp04 (busca-nodoX 1 (grafo-nodos Gp04))))
;> (nodos-describir* (grafo-nodos dsk))
;'((1 #t 0 ()) (2 #t 2 (2)) (3 #t 3 (3)) (4 #t 4 (4 3)) (5 #t 7 (5 4 3)) (6 #t 6 (6 4 3)) (7 #t 12 (7 5 4 3)))
;> 
(define dijkstra
  (λ (G ni)
    (define djk-aux
      (λ (Gr Q)
        (cond ((empty? Q) Gr)
              ((nodo-explorado? (car Q)) (djk-aux Gr (cdr Q)))
              (else (let* ([aristas (filter (λ (a)(= (nodo-info (1de a)) (nodo-info (car Q)))) (grafo-aristas Gr))])
                      (nodo-marcar (car Q) #t)                      
                      (for-each
                       (λ (a)                         
                         (cond ((< (+ (nodo-info-dist (1de a)) (arista-info-dis a)) (nodo-info-dist (2de a)))
                                (nodo-dist! (2de a) (+ (nodo-info-dist (1de a)) (arista-info-dis a)))                                
                                (nodo-camino! (2de a) (apply append (list (list (2de a)) (nodo-camino (1de a))))))))
                       aristas)
                      (djk-aux Gr (cdr (append Q (nodos-adyacentes (car Q) Gr)))))))))
    (inicializar-grafo G)
    (nodo-dist! ni 0)
    (djk-aux G (list ni))))

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::