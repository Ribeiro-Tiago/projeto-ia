;;;; board-handler.lisp
;;;; Funcoes de manipulacao do tabuleiro
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;; Board builder functions 
(defun empty-board (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro vazio: 0 em todas as celulas"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun print-board (index board) 
  "Retorna uma versao bonificada do tabuleiro com index atras"
  (format t "~% ~% ~d - ~A ~%     ~A" index (first board) (second board))
)


;;; Board handler functions
(defun valid-cell (index) 
  "Funcao auxiliar que valida se o index da celula esta valido (entre 0 e 5). Retorna T se sim, nil caso contrario"
  (AND (>= index 0) (<= index 5))
)

(defun valid-row (index) 
  "Funcao auxiliar que valida se o index da linha esta valido (0 ou 1). Retorna T se sim, nil caso contrario"
  (OR (= index 0) (= index 1))
)

(defun get-row (index board)
  "Valida o {index} e devolvee a linha do {board} correspondente ao {index} ou nil se o {index} for invalido"
  (cond 
        ((not (valid-row index)) nil)
        ((= index 0) (first board))
        (t (second board)))
)

(defun get-cell (rowIndex cellIndex board)
  "Valida o {rowIndex} e {cellIndex} e devolvee a celula do {board} correspondente ao [rowIndex][cellIndex] ou nil se algum dos indexes for invalido"
  (cond 
     ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)
     (t (nth cellIndex (get-row rowIndex board))))
)


(defun board-emptyp (board) 
  "Verifica se todos os atomos da lista sao 0. Retorna T se sim, nil caso contrario"
  (cond ((= (+ (apply '+ (first board)) (apply '+ (second board))) 0)))
)

(defun replace-position (cellIndex row &optional (value 0))
  "Valida se {row} e {cellIndex} sao validos. Se sim, substitui o valor na [row][cellIndex] com {value} e retorna uma nova linha. Caso contrario deaolve nil"
  (cond
    ((null row) ())
    ((not (valid-cell cellIndex)) nil)
    ((= cellIndex 0) (cons value (rest row)))
    (t (cons(first row) (replace-position (- cellIndex 1) (rest row) value))))
)

(defun update-board (rowIndex cellIndex board &optional (value 0))
  "Valida se {rowIndex}, {cellIndex} e {board} sao validos. Se sim, atualiza a posicao [rowIndex][cellIndex] do {board} com o {value} recebido e retorna o novo tabuleiro. Caso contrario retorna nil"
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
  "Valida se {rowIndex}, {cellIndex} e {board} sao validos. Se sim, incrementa o valor na posicao [rowIndex[cellIndex]] do {board} por 1. Caso contrario, retorna nil"
  (cond
     ((null board) ())
     ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)    
     (t (+ 1 (get-cell rowIndex cellIndex board)))
  )
)


(defun get-next-cell (rowIndex cellIndex)
  "Calcula e retorna o index da proxima coluna com base no index da coluna e linha atual"
  (cond 
    ((AND (> cellIndex 0) (= rowIndex 0)) (- cellIndex 1))
    ((AND (< cellIndex 5) (= rowIndex 1)) (1+ cellIndex))
    (t cellIndex)
  )
)

(defun get-next-row (rowIndex cellIndex)
  "Calcula e retorna o index da proxima linha com base no index da coluna e linha atual"
  (cond 
    ((AND (= cellIndex 0) (= rowIndex 0)) (1+ rowIndex))
    ((AND (= cellIndex 5) (= rowIndex 1)) (- rowIndex 1))
    (t rowIndex)
  )
)

(defun allocate-pieces (rowIndex cellIndex board)
  "Valida {rowIndex}, {cellIndex} e {board}. Se nao forem validos, retorna nil. Se forem, allocate-pieces-aux para percorrer o {board} com, enviando o numero de pecas existentes na [rowIndex[cellIndex]]. Quando acaba de percorrer, chama a validate-final-position to check if the final position has 1, 3 or 5 pieces and to remove them if that's the case"
  (let ((numPieces (get-cell rowIndex cellIndex board)))
    
    (cond
      ((null board) nil)
      ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)
      ((= numPieces 0) board)
      (t (validate-final-position 
                   rowIndex 
                   (allocate-pieces-aux numPieces rowIndex cellIndex board (list rowIndex cellIndex) T))
      ))
  )
)

(defun allocate-pieces-aux (numPieces rowIndex cellIndex board initPos &optional (isFirstCall nil))
  "Enquanto houver {numPieces} para distribuir, percorre o {board} incrementando uma peca a cada posicao. Se passar pela posicao inicial, passa a proxima"
  (let* (
          (nextCell (cond (isFirstCall cellIndex) ; so avanca o cellIndex se nao for a primeira chamada
                          (t (get-next-cell rowIndex cellIndex))))
          (nextRow (cond (isFirstCall rowIndex)
                         (t (get-next-row rowIndex cellIndex)))) ; o mesmo para o rowIndex
          (value (cond (isFirstCall 0)
                       (t (add-position nextRow nextCell board))))
        )

    (cond
      ((> numPieces 0)
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
      (t  (list rowIndex cellIndex board))
   )
 )
)


(defun validate-final-position (initRowIndex finalBoard)
  "Valida o valor da posicao final, se o valor for 1, 3 ou 5 \"captura\" essas pecas"
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

(defun is-move-validp (rowIndex cellIndex board)
  "Verifica se o valor da [rowIndex[cellIndex]] e 0. Se for 0 nao ha jogada a fazer entao e considerada invalida"
  (cond ((null board) nil)
        ((not (and (valid-cell cellIndex) (valid-row rowIndex))) t)
        ((= (get-cell rowIndex cellIndex board) 0) nil)
        (t t))
)
