;;;; board-builder.lisp
;;;; Fun��es de constru��o dos v�rios tabuleiros
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

(defun empty-board (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro vazio: 0 em todas as c�lulas"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun start-board ()
  (board-g)
)

(defun board-a ()
  "Retorna o tabuleiro do problema a do enunciado"
  '((0 0 0 0 0 2)
    (0 0 0 0 4 0))
)

(defun board-b ()
  "Retorna o tabuleiro do problema b do enunciado"
  (make-list 2 :initial-element (make-list 6 :initial-element '2))
)

(defun board-c ()
  "Retorna o tabuleiro do problema c do enunciado"
  '((0 3 0 3 0 3)
    (3 0 3 0 3 0))
)

(defun board-d ()
  "Retorna o tabuleiro do problema d do enunciado"
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
)

(defun board-e ()
  "Retorna o tabuleiro do problema e do enunciado"
  '((2 4 6 8 10 12)
    (12 10 8 6 4 2))
)

(defun board-f ()
  "Retorna o tabuleiro do problema f do enunciado"
  '((48 0 0 0 0 0)
    (0 0 0 0 0 48))
)

(defun board-g ()
  "Retorna o tabuleiro do problema g do enunciado"
  (make-list 2 :initial-element (make-list 6 :initial-element '8))
)

(defun print-board (board) 
  "Prints a pretty version of the board"
  (format t "(~A ~% ~A)" (first board) (second board))
)

;;;; board-handler.lisp
;;;; Fun��es de intera��o com o tabuleiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

(defun valid-cell (index) 
  "Fun��o auxiliar que valida se o index da c�lula est� v�lido (entre 0 e 5).
   Retorna T se sim, nil caso contr�rio"
  (AND (>= index 0) (<= index 5))
)

(defun valid-row (index) 
  "Fun��o auxiliar que valida se o index da linha est� v�lido (0 ou 1) 
   Retorna T se sim, nil caso contr�rio"
  (OR (= index 0) (= index 1))
)

(defun get-row (index board)
  "Valida o {index} e devolve a linha do {board} correspondente ao {index} ou nil se o {index} for inv�lido"
  (cond 
        ((not (valid-row index)) nil)
        ((= index 0) (first board))
        (t (second board)))
)

(defun get-cell (rowIndex cellIndex board)
  "Valida o {rowIndex} e {cellIndex} e devolvee a c�lula do {board} correspondente 
   ao [rowIndex][cellIndex] ou nil se algum dos indexes for inv�lido"
  (cond 
     ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)
     (t (nth cellIndex (get-row rowIndex board))))
)


(defun board-emptyp (board) 
  "Verifica se todos os �tomos da lista s�o 0. Retorna T se sim, nil caso contr�rio"
  (eval 
   (cons 'and 
         (mapcar 
             #'(lambda (tabuleiro &aux (row
                                          (eval 
                                           (cons 'and 
                                                 (mapcar #'(lambda (linha &aux (emptyCell (= linha 0))) emptyCell) tabuleiro)
                                           )
                                          )
                                       )
                       ) 
                       row
               )
             board
         )
    ))
)

(defun replace-position (cellIndex row &optional (value 0))
  "Valida se {row} e {cellIndex} s�o v�lidos. 
   Se sim, substitui o valor na [row][cellIndex] com {value} e retorna uma nova linha
   Caso contr�rio devolve nil"
  (cond
    ((null row) ())
    ((not (valid-cell cellIndex)) nil)
    ((eq cellIndex 0) (cons value (rest row)))
    (t (cons(first row) (replace-position (- cellIndex 1) (rest row) value))))
)

(defun update-board (rowIndex cellIndex board &optional (value 0))
  "Valida se {rowIndex}, {cellIndex} e {board} s�o v�lidos.
  Se sim, atualiza a posi��o [rowIndex][colIndex] do {board} com o {value} recebido e retorna o novo tabuleiro
  Caso cotnr�rio retorna nil"
  (cond 
     ((null board) nil)

     ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)

     ((= rowIndex 0)  
        (cons 
           (replace-position cellIndex (get-row rowIndex board) value) 
           (list (second board))
        ))

     (t (cons 
           (first board) 
           (list (replace-position cellIndex (get-row rowIndex board) value))
        )))
)

(defun add-position (rowIndex cellIndex board) 
  "Valida se {rowIndex}, {cellIndex} e {board} s�o v�lidos.
  Se sim, incrementa o valor na posi��o [rowIndex[cellIndex]] do {board} por 1
  Caso contr�rio, retorna nil"
  (cond
     ((null board) ())
     ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)    
     (t (+ 1 (get-cell rowIndex cellIndex board)))
  )
)


(defun get-next-cell (rowIndex cellIndex isFirstCall)
  "Calcula e retorna o index da pr�xima coluna com base no index da coluna e linha atual"
  (cond 
    ((AND (> cellIndex 0) (= rowIndex 0) (NOT isFirstCall)) (- cellIndex 1))
    ((AND (< cellIndex 5) (= rowIndex 1) (NOT isFirstCall)) (1+ cellIndex))
    (t cellIndex)
  )
)

(defun get-next-row (rowIndex cellIndex isFirstCall)
  "Calcula e retorna o index da pr�xima linha com base no index da coluna e linha atual"
  (cond 
    ((AND (= cellIndex 0) (= rowIndex 0) (NOT isFirstCall)) (1+ rowIndex))
    ((AND (= cellIndex 5) (= rowIndex 1) (NOT isFirstCall)) (- rowIndex 1))
    (t rowIndex)
  )
)

(defun allocate-pieces (numPieces rowIndex colIndex &optional (board (start-board)) (isFirstCall nil))
  "Valida {rowIndex}, {colIndex} e {board}. Se n�o forem v�lidos, retorna nil
   Se forem, chama percorre o {board} recursivamente com base no {numPieces} e 
   em cada posi��o incrementa um valor. Na posi��o inicial, deixa um valor de 0.
   No final, retorna o {board} com os novos valores"
  (let* (
          (nextCell (get-next-cell rowIndex colIndex isFirstCall)) 
          (nextRow (get-next-row rowIndex colIndex isFirstCall))
          (value (cond (isFirstCall 0)
                       (t (add-position nextRow nextCell board))))
        )

    (cond
      ((null board) nil)
      ((not (and (valid-cell colIndex) (valid-row rowIndex))) nil)
      ((>= numPieces 0)
         (allocate-pieces 
             (- numPieces 1)
             nextRow
             nextCell
             (update-board 
                 nextRow 
                 nextCell
                 board
                 value
             )
         )
      ) 
      (t board)
   )
  ) 
)


(defun rowsMissing(r c)
    (cond 
        ((and (= r 0) (= c 0) 1))
        ((and (= r 1) (= c 5) 0))
        (t r)
    )
)
(defun columnsM(r c)
    (cond 
        ((and (= r 0) (> c 0) (- c 1) ))
        ((and (= r 1) (< c 5) (+ c 1) ))
        (t c)
    )
)
;; prolem need to return (list r c) but as list on end 
(defun distribuir-pecas (n r c &optional (board (start-board)))
    (
     cond 
        ((= 0 n) nil)
        (
        (cons  
            (list r c) 
            (distribuir-pecas (- n 1) (rowsMissing r c) (columnsM r c) board ) 
        ))   
    )
)
;;(print (fuck_its 15 1 1))

;;(print (list (list 1 1 ) (list 2 1 ) ))


(defun operador(rowIndex cellIndex board)
    (sub-operador
         (distribuir-pecas 
            (get-cell rowIndex cellIndex board )
            rowIndex 
            cellIndex
         )
         board
    )
)

(defun sub-operador(data board)
    (cond 
        ((eq nil data)  board)
        (
            t
            (
                sub-operador 
                (cdr data)
                (update-board (first (car data)) (second  (car data)) board 
                    (add-position (first  (car data)) (second  (car data)) board)
                )   
            )
            
        )
        
    )
)



;;; Ideia - 
;;; 
;;;
;;; Problema das vasilhas
;;; variaveis de teste e operadores
(defun no-teste ()
"Define um no teste do problema da vasilhas em que A=2, B=2, profundidade=0 e pai=NIL"
 (list (board-d) 0 nil))

(defun operadores ()
 "Cria uma lista com todos os operadores do problema das vasilhas."
 (list '(1 1)  '(1 1) '(1 1) '(1 1) '(1 1))

;;; Construtor
(defun create-no (board &optional (g 0) (pai nil))
  (list board g pai)
)

;;; Metodos seletores
;; no-estado
;; teste: (no-estado (no-teste))
;; resultado: (2 2)
(defun no-state(no)
    (cond 
    ((eq nil no) nil)
    (t (car no))
    )
)


;; no-profundidade
;; teste: (no-profundidade (no-teste))
;; resultado: 0
(defun no-profundidade(no)
    (second no)
)

;; no-pai
;; teste: (no-pai (no-teste))
;; resultado: NIL
(defun no-pai(no)
    (third no)
)

(defun no-solucaop(a)
    (cond 
        ((= 1 (first (no-estado a))) t)
        ((= 1 (second (no-estado a))) t)
        (t nil)
    )
)
(print (no-solucaop (no-teste)))
;;; sucessores
(defun sucessores-aux(no function)
    (cria-no (funcall function (no-estado no)) (+ 1 (no-profundidade no)) no)
)   

(defun sucessores(no lop alg pmax)
    (cond 
        ((and (equal alg 'dfs) (>= (no-profundidade no) pmax) ) nil)
        (t (mapcar #'(lambda (op) (sucessores-aux no op)) lop))
    )
)
(print (sucessores (no-teste) (operadores) 'dfs 2))
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
;; teste: (sucessores (no-teste) (operadores) 'dfs 2)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
(defun abertos-bfs(abertos nosucessores)
    (append abertos nosucessores)
)
(defun abertos-dfs(abertos nosucessores)
    (append nosucessores abertos)
)
(defun no-existep(no fechados)
   (eval (cons 'or (mapcar #'(lambda (nof) (equal (no-estado no) (no-estado nof)) fechados))))
)
;;; Algoritmos
;; procura na largura
;; teste: (bfs (no-teste) 'no-solucaop 'sucessores (operadores))
;; resultado: ((3 1) 1 ((2 2) 0 NIL))
(defun bfs (no &optional( abertos (list no)) (fechados nil) )
    (cond 
        ((no-solucaop (car abertos)) (car abertos))
        ((null abertos) nil)    
        (t 
     (bfs nil (abertos-bfs (cdr abertos) (apply #'append (mapcar #'(lambda (nos) (if (no-existep nos fechados) nil)
      (list nos)
        (sucessores (car abertos) (operadores) 'bfs nil))
        )
        )) 
        (cons (car abertos) fechados)
    )
    ))
)
(print (bfs (no-teste)))
;; procura na profundidade
;; teste: (dfs (no-teste) 'no-solucaop 'sucessores (operadores) 10)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))

