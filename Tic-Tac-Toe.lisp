;TIC-TAC-TOE

;Funcion que crea una lista que representa los nueve cuadros dentro del tablero del tic-tac-toe.
;Un cero signiﬁca que la posición está vacía
;1 signiﬁca que lo llena un O; 
;10 signiﬁca que es rellenado por una X

(defun Crear_tablero () (list 'Tablero 0 0 0 0 0 0 0 0 0))
 
;Funcion que convierte un valor numerico a letra dentro de el tablero
;equal->compara valores
;Regresa el numero convertido a letra

(defun Convertir_a_letrar (Numero) 
    (cond ((equal Numero 5) "O") 
        ((equal Numero 3) "X") 
        ((equal Numero 0) " "))) 
        
;Funciones que imprimen tablero 

;Funcion que imprime todo el tablero llamando a la que imprime solo un renglon tres veces
;format t escribe una cadena en pantalla
;~% -> Nueva linea
;nth -> te permite ingresar al valor correspondiente a partir de una posicion y la lista

(defun Imprimir_Tablero (Tablero) 
    (format t "~%") 
    (Imprimir_Renglon 
        (nth 1 Tablero) (nth 2 Tablero) (nth 3 Tablero)) 
    (format t "~% -----------") 
    (Imprimir_Renglon 
        (nth 4 Tablero) (nth 5 Tablero) (nth 6 Tablero)) 
    (format t "~% -----------") 
    (Imprimir_Renglon 
        (nth 7 Tablero) (nth 8 Tablero) (nth 9 Tablero)) 
    (format t "~%"))

;Funcion que imprime solo un renglon y cambia el valor de el numero por una letra
;~A Recibe un argumento de caracter ASCII

(defun Imprimir_Renglon (x y z) 
    (format t "~& ~A | ~A | ~A" 
        (Convertir_a_letrar x) 
        (Convertir_a_letrar y) 
        (Convertir_a_letrar z))) 

;Definimos los valores para COMPUTADORA Y USUARIO
;Asignacion de valores como variables

(setf *Computadora* 3)
(setf *Usuario* 5) 

;Funcion que permite modificar el tablero ingresando un valor en posicion 
;->Jugador puede ser "computadora" o "Usuario"
;Regresa el tablero nuevo despues de asignarle la posision al jugador que quiere hacer el movimiento 

(defun Hacer_Movimiento (Jugador Posicion Tablero) 
    (setf (nth Posicion Tablero) Jugador) 
        Tablero)
    
;Variable global para las posibles combinaciones para hacer tres X o O seguidas

(setf *Tripletas* 
    '((1 2 3) (4 5 6) (7 8 9) ;Tripletas Horizontales. 
      (1 4 7) (2 5 8) (3 6 9) ;Tripletas Verticales. 
      (1 5 9) (3 5 7))) ;Tripletas Diagonales. 

;Funcion que regresa la suma de las posiciones en las tripletas
;Le pasan una Tripleta, analiza cada una dentro del tablero y regresa la suma de los valores que se encuentren dentro de las casillas

(defun Suma_de_Tripletas (Tablero Tripleta) 
    (+ (nth (first Tripleta) Tablero) 
        (nth (second Tripleta) Tablero) 
        (nth (third Tripleta) Tablero))) 

;Funcion que regresa una lista de las sumas de las posiciones en Tripletas
;->mapcar regresa una lista despues de hacer una funcion primero al CAR y luego a los CDR de las listas
;->Lambda es una funcion anonima, en este caso asocia a la variable Tripleta el valor que resulte de la suma de tripletas
(defun Hacer_Sumas (Tablero) 
    (mapcar #'(lambda (Tripleta)
        (Suma_de_Tripletas Tablero Tripleta)) *Tripletas*))

;Funcion que comprueba si un jugador ya a ganado a traves de la multiplicacion de los numero asignados por jugador
;Si la multiplicacion de el valor del jugador * 3 esta dentro de la lista de sumas significa que ya gano
;->let asocia a la variable Sumas el valor que resulte de "Hacer_Sumas"
;Se compara si la multiplicacion * 3 de los valores es miembro de "Sumas"

(defun Jugador-Ganador (Tablero) 
    (let ((Sumas (Hacer_Sumas Tablero))) 
        (or (member (* 3 *Computadora*) Sumas)     
            (member (* 3 *Usuario*) Sumas))))

;Funcion que ofrece al usuario ir primero dependiendo del caso llama a una funcion diferente 
;Pregunta al usuario si quiere ir primero
(defun Tic-Tac-Toe () 
    (if (y-or-n-p "Quieres tirar primero? ") 
        (Movimiento_Usuario (Crear_tablero)) 
        (Movimiento_Computadora (Crear_tablero))))

;Funcion que pide al usuario que escriba un movimiento y verifica que sea legal
;Existen dos casos en los que ya no se llama a Movimiento_Computadora Cuando gana y cuando hay un empate
;Let le asigna a Posicion el valor que resulte de "movimiento_legal" (regresa una posicion) y a new-Tablero lo que resulte de "Hacer_Movimiento" (Regresa un tablero)
;Se imprime el tablero nuevo que resulta de hacer el movimiento
;Se tiene una condicion, si el jugador ya gano o si ya no hay lugares disponibles (empate)
;Actualiza el plano y llama a Movimiento_Computadora con el nuevo tablero

(defun Movimiento_Usuario (Tablero) 
    (let* ((Posicion (Movimiento_Legal Tablero)) 
        (new-Tablero (Hacer_Movimiento *Usuario* Posicion Tablero))) 
        (Imprimir_Tablero new-Tablero) 
        (cond ((Jugador-Ganador new-Tablero) 
            (format t "~&HAZ GANADO!")) 
        ((Tablero_Lleno new-Tablero) 
            (format t "~&HEMOS EMPATADO")) 
        (t (Movimiento_Computadora new-Tablero)))))

;Funcion que determina si un movimiento es legal comprobandolo si esta dentro de 1 y 9 (casillas)
;Y si aun quedan casillas vacias 
;No sale de la funcion hasta que un movimiento sea legal 
;->Read detiene la ejecuciuon de la funcion hasta que se introduce un dato
;->Let le asigna a "Posicion" el valor que se introdujo
;->Se verifica si la posicion que se introfujo es valida
;->intergetp verifica si lo que se introdujo es un numero
;->Si el movimiento es bueno let le asigna la "Posicion" a t

(defun Movimiento_Legal (Tablero) 
    (format t "~&TU MOVIMIENTO: ") 
    (let ((Posicion (read)))       
        (cond ((not (and (integerp Posicion) 
            (<= 1 Posicion 9))) 
        (format t "~&MOVIMIENTO INVALIDO") 
        (Movimiento_Legal Tablero)) 
        ((not (zerop (nth Posicion Tablero))) 
        (format t "~&ESA CASILLA ESTA OCUPADA") 
        (Movimiento_Legal Tablero)) 
        (t Posicion))))
        
;Funcion que comprueba si aun hay espacios vacios en el tablero
;Si ya no hay 0 en el tablero significa que esta lleno

(defun Tablero_Lleno (Tablero) 
    (not (member 0 Tablero)))
    
;Funcion que determina el movimiento de la Computadora
;->Let le asigna a "El mejor movimiento" lo que resulte de "escoger_el_mejor_movimiento" (una posicion y el nombre de la estrategia)
;La posicion y la estrategia que se uso esto para saber en que desicion tomo la computadora
;->Let le asigna a posicion el primer elemento de la lista que resulto de "El_mejor_movimiento" en este caso es la posicion
;->Let le asigna a Estrategia el segundo elemento de la lista que resulto de "El mejor movimiento" en este caso es la  descripcion de la estrategia 
;->Let le asigna a new-Tablero lo que resulte de "Hacer_movimiento" en este caso es un tablero modificado
;->Se imprime el movimiento de la computadora, la estrategia y el nuevo tablero modificado
;->Comprueba si gano o empato con el tiro que acaba de hacer 
;-> Si no, llama a un movimiento de usuario

(defun Movimiento_Computadora (Tablero) 
    (let* ((El_Mejor_Movimiento (Escoger_El_Mejor_Movimiento Tablero)) 
        (Posicion (first El_Mejor_Movimiento)) 
        (Estrategia (second El_Mejor_Movimiento)) 
        (new-Tablero (Hacer_Movimiento *Computadora* Posicion Tablero)))
        (format t "~&Mi Movimiento: ~S" Posicion) 
        (format t "~&Estrategia que use: ~A~%" Estrategia) 
        (Imprimir_Tablero new-Tablero) 
        (cond ((Jugador-Ganador new-Tablero) 
            (format t "~&TE GANE!"))        
        ((Tablero_Lleno new-Tablero) 
            (format t "~&HEMOS EMPATADO")) 
        (t (Movimiento_Usuario new-Tablero)))))


;Funcion que determina si la computadora, busca ganar, empatar o solo tener un movimiento aleatorio
;llama a 3 funciones para saber que movimiento hacer 
(defun Escoger_El_Mejor_Movimiento (Tablero) 
    (or (Tres_En_Un_Renglon Tablero) 
        (Bloquear_Usuario Tablero) 
        (Movimiento_Aleatorio Tablero)))

;Funcion que regresa un movimiento aletorio compuesto de una lista con la posicion y la estrategia
(defun Movimiento_Aleatorio (Tablero)
    (list (Movimiento_Aleatorio_Vacio Tablero)
    "Movimiento Aletorio"))

;Funcion que crea un movimiento aletorio
;-Let le asigna a Posicion lo que resulte de un random entre 1 y 9
;Comprueba si la posicion aletoria esta desocupada (0) verificandolo dentro de el tablero
;-Si no es asi regresa a la funcion hasta que se encuentre una casilla disponible
;-Si es correcto, regresa la posicion

(defun Movimiento_Aleatorio_Vacio (Tablero)
    (let ((Posicion (+ 1 (random 9))))
    (if (zerop (nth Posicion Tablero))
    Posicion
    (Movimiento_Aleatorio_Vacio Tablero))))

;Funcion que determina si puede completar una trifecta de simbolos
;Let le asigna a Posicion lo que resulte de Ganar o Bloquear (Posicion)
;Llama a ganar o bloquear con el Tablero y la multiplicacion * 2 de el valor que tenga el usuario Para comprobar que la multiplicacion este dentro de la lista de las sumas
;Regresa una lista con la posicion y la estrategia 

(defun Tres_En_Un_Renglon (Tablero) 
    (let ((Posicion (Ganar_O_Bloquear Tablero  (* 2 *Computadora*)))) 
        (and Posicion (list Posicion "Tres En Un Renglon")))) 

;Funcion que determina si puede bloquear
;Let le asigna a Posicion lo que resulte de Ganar o Bloquear (Posicion)
;Llama a ganar o bloquear con el Tablero y la multiplicacion * 2 de el valor que tenga el usuario Para comprobar que la multiplicacion este dentro de la lista de las sumas
;Regresa una lista con la posicion y la estrategia 

(defun Bloquear_Usuario (Tablero) 
    (let ((Posicion (Ganar_O_Bloquear Tablero (* 2 *Usuario*)))) 
        (and Posicion (list Posicion "Bloquear Usuario"))))

;Funcion que determina si puede empatar o ganar
;Recibe un tablero y  la multiplicacion que va a buscar
;Let asigna a Tripleta lo que resulte de buscar el resultado en la suma de las tripletas
;Si se encuentra llama a Encontrar_Posicion_vacia con la Tripleta para saber que casilla esta vacia
;-> When evalua que la tripleta y la posicion 

(defun Ganar_O_Bloquear (Tablero Resultado) 
    (let ((Tripleta (find-if #'(lambda (trip) 
            (equal (Suma_de_Tripletas Tablero trip) Resultado)) 
                *Tripletas*))) 
                (when Tripleta (Encontrar_Posicion_Vacia Tablero  Tripleta))))


;Funcion que determina si hay una Posicion sola para poder tirar
;Entra el tablero y la tripleta en donde buscar si hay una casilla vacia
;Verifica si hay un 0 dentro de la tripleta

 (defun Encontrar_Posicion_Vacia (Tablero Tripleta) 
    (find-if #'(lambda (Posicion) 
        (zerop (nth Posicion Tablero))) Tripleta))
            