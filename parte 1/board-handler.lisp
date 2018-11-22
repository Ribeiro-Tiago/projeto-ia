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
