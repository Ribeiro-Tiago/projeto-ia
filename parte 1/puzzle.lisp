;;;; board-handler.lisp
;;;; Fun��es de intera��o com o tabuleiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;; Board builder functions 
(defun empty-board (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro vazio: 0 em todas as c�lulas"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun build-board (lista)
  "Constr�i um tabuleiro com base na lista recebida. H� de ser constru�do com base em ficheiro"
  (let ((linha1 (first lista))
        (linha2 (second lista)))

    (cond ((or (null lista1) (null lista2)) nil)
          ((or (not (= (lenght-list lista1) 2)) (not (= (length-list lista2) 2))) nil)
          (t lista)))
)

(defun start-board ()
  "Retorna o tabuleiro inicial default"
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


;;; Board builder functions
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
  "Valida o {index} e devolvee a linha do {board} correspondente ao {index} ou nil se o {index} for inv�lido"
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
    ((= cellIndex 0) (cons value (rest row)))
    (t (cons(first row) (replace-position (- cellIndex 1) (rest row) value))))
)

(defun update-board (rowIndex cellIndex board &optional (value 0))
  "Valida se {rowIndex}, {cellIndex} e {board} s�o v�lidos.
  Se sim, atualiza a posi��o [rowIndex][cellIndex] do {board} com o {value} recebido e retorna o novo tabuleiro
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

(defun validate-final-position (initRowIndex finalBoard)
  "Valida a posi��o final, se for a posi��o 1 3 ou 5 da linha contr�ria � inicial
   retira essas \"captura\" essas pe�as"
  (let* (
         (endRowIndex (first finalBoard))
         (isSameRow (= endRowIndex initRowIndex))
         (endCellIndex (second finalBoard))
         (board (third finalBoard))
         (piecesLastPos (get-cell endRowIndex endCellIndex board)))
    
    (cond ((AND (null isSameRow) (OR (= piecesLastPos 1) (= piecesLastPos 3) (= piecesLastPos 5)))
             (update-board 
                 endRowIndex 
                 endCellIndex
                 board
                 0
             ))
          (t board)))
)

(defun allocate-pieces (rowIndex cellIndex &optional (board (start-board)))
  "Valida {rowIndex}, {cellIndex} e {board}. Se n�o forem v�lidos, retorna nil
   Se forem, allocate-pieces-aux para percorrer o {board} com, enviando o n�mero de pe�as 
   existentes na [rowIndex[cellIndex]]
   Quando acaba de percorrer, chama a validate-final-position to check if the final position
   has 1, 3 or 5 pieces and to remove them if that's the case"
  (let ((numPieces (get-cell rowIndex cellIndex board)))
    
    (cond
      ((null board) nil)
      ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)
      ((= numPieces 0) board)
      (t (validate-final-position
            rowIndex 
            (allocate-pieces-aux numPieces rowIndex cellIndex board (list rowIndex cellIndex) T)))
    )
  )
)

(defun allocate-pieces-aux (numPieces rowIndex cellIndex board initPos &optional (isFirstCall nil))
  "Enquanto houver {numPieces} para distribuir, percorre o {board} incrementando uma pe�a a cada
   posi��o. Se passar pela posi��o inicial, passa � pr�xima"
  (let* (
          (nextCell (get-next-cell rowIndex cellIndex isFirstCall)) 
          (nextRow (get-next-row rowIndex cellIndex isFirstCall))
          (value (cond (isFirstCall 0)
                       (t (add-position nextRow nextCell board))))
        )

    (cond
      ((>= numPieces 0)
         (allocate-pieces-aux
             (- numPieces 1)
             nextRow
             nextCell
             (update-board 
                 nextRow 
                 nextCell
                 board
                 value
             )
             initPos
         )
      ) 
      (t (list rowIndex cellIndex board))
   )
 )
)