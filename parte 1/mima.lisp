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
        ((not (and (valid-cell cellIndex) (valid-row rowIndex))) nil)
        ((= (get-cell rowIndex cellIndex board) 0) nil)
        (t t))
)

;;;; procura.lisp
;;;; Implementacao dos algoritmos de procura em espaco de estados
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;;;;;;;;;;;; Construtor ;;;;;;;;;;;;;;;
;; custo = f | heuristica = g
(defun create-node (board heuristica custo &optional (depth 0) (parent nil))
  "Construtor do no das arvores para os algoritmos"
  (list board heuristica custo depth parent)
)


;;;;;;;;;;;;;;; getters ;;;;;;;;;;;;;;;

;; Test: (get-node-state (teste))
;; Result: ((8 8 8 8 8 8) (8 8 8 8 8 8))
(defun get-node-state (node)
  "Devolve o estado (o tabuleiro) do problema neste no"
  (first node)
)

;; Teste: (get-node-heuristic (teste))
;; Result: heuristica-default
(defun get-node-heuristic (node)
  "Retorna a heuristica que e usada para calcular o custo do no"
  (second node)
)

;; Teste: (get-node-cost (teste))
;; Result: 0
(defun get-node-cost (node)
  "Retorna o custo do no"
  (third node)
)

;; Teste: (get-node-depth (teste))
;; Result: 0
(defun get-node-depth (node)
  "Retorna a profundidade do {node} recebido"
  (fourth node)
)

;; Teste: (get-node-parent (teste))
;; Result: NIL
(defun get-node-parent (node)
  "Retorna o no pai de {node}"
  (fifth node)
)

;; Test: (node-solutionp (teste))
;; Result: nil
(defun node-solutionp (node) 
  "Verifica se o {node} e um no solucao"
  (cond ((board-emptyp (get-node-state node))))
)


;;;;;;;;;;;;;;; Funcoes auxiliares aos algos ;;;;;;;;;;;;;;;

(defun shortest-cost-sort-compare (a b)
  "Funcao de comparacao de custo mais baixo para o sort"
  (< (get-node-cost a) (get-node-cost b))
)

;; teste: (get-node-in-abertos (get-node-state (teste3)) (list (teste3)))
;; result: ((((8 0 0 0 0 2) (0 0 0 0 4 0)) heuristica-default 0 0 NIL) 0)
;; returns: (node indexInAbertos)
(defun get-node-in-abertos (board abertos &optional (index 0))
  "Verifica se {node} encontra-se na lista dos {fechados}"
  (let ((curr-item (first abertos)))

    (cond ((null abertos) nil)
          ((equal board (get-node-state curr-item)) (list curr-item index))
          (t (get-node-in-abertos board (rest abertos)))))
)

;; teste: (sucessores-a* (teste3) (list (teste3)))
;; result: (((((0 0 0 0 0 3) (1 1 1 1 5 1)) heuristica-default 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) (((8 0 0 0 0 3) (0 0 0 0 0 1)) heuristica-default 11 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) (((8 0 0 0 1 0) (0 0 0 0 4 0)) heuristica-default 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL))) 3)
;; returns: (novaListaAbertos numNodesGerados)
(defun sucessores-a* (node abertos isFirstCall &optional (sucs '()) (rowIndex 0) (cellIndex 0))
  "Percorre as posicoes todas do estado do {node} e gera os seus nos sucessores"
  (cond ((AND (= rowIndex 0) (= cellIndex 0) (not isFirstCall)) (build-end-sucs-list sucs abertos)) ; deu a volta toda
        (t (let* ((result-sucs (sucessores-aux-a* rowIndex cellIndex node abertos)) ; Devolve (listaAbertos sucessor)

                 (nextRow (get-next-row rowIndex cellIndex))

                 (nextCell (get-next-cell rowIndex cellIndex))
                 
                 (newSucs (cond ((null (first result-sucs)) sucs)
                                (t (append sucs (list (first result-sucs)))))))

             (sucessores-a* node (list (first result-sucs)) nil newSucs nextRow nextCell))))
)

(defun build-end-sucs-list (sucs abertos)
  "Constroi a lista retornada no final da funcaoo sucessores-a*"
  (cond ((OR (null abertos) (null (first abertos))) (list sucs (list-length sucs)))
        (t (list (append sucs abertos) (list-length sucs)))) 
)

;; teste: (sucessores-aux-a* 0 0 (teste3) '())
;; result: ((((0 0 0 0 0 3) (1 1 1 1 5 1)) heuristica-default 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) 0)
;; returns: (listaAbertos numNodesGerados) 
(defun sucessores-aux-a* (rowIndex cellIndex parentNode abertos)
  "Verifica se a posicao [rowIndex[cellIndex]] e valida, se for expande esse no, gerando o novo tabuleiro deopis dessa jogada e criando um novo no. Senao passa a frente"
  (let ((board (get-node-state parentNode)))
    (cond ((is-move-validp rowIndex cellIndex board) ; so geramos sucessores se for uma casa com valor > 0
        (let* ((newBoard (allocate-pieces rowIndex cellIndex board))

               (depth (1+ (get-node-depth parentNode)))

               (heuristic (get-node-heuristic parentNode))

               (value (+ (call-heuristic heuristic newBoard parentNode) depth))

               (oldNode (get-node-in-abertos newBoard abertos))

               (newNode (create-node newBoard heuristic value depth parentNode)))

           (cond ((not (first oldNode)) (cons newNode '(0))) ; nao esta em abertos
                 
                 (t (cond ((> value (get-node-cost (first oldNode))); esta em abertos, vamos comparar valores e substituir se no gerado for superior
                          (cons newNode (replace-nth-in-list abertos (second oldNode) (first oldNode))))))
           )
        )
    ))
  )
)


;; teste: (replace-nth-in-list '(3 3 3) 1 5)
;; result: (3 5 3)
(defun replace-nth-in-list (list n elem)
  "Substitui o elemento na posicao {n} da {list} pelo {elem} recebido"
  (cond
    ((null list) list)
    ((= n 0) (cons elem (rest list)))
    (t (cons (first list) (replace-nth-in-list (rest list) (- n 1) elem))))
)

;; teste: (no-existe-fechadosp (no-teste) nil)
;; result: T 
(defun no-existe-fechadosp(no fechados)
  (cond
    ((null fechados) nil)
    (t (eval (cons 'or (mapcar #'(lambda (nof)
                                    (equal (get-node-state no) (get-node-state nof))) fechados))))
  )
)

;; sucessores
;; teste: (sucessores (no-teste) 'bfs nil)
;; result: ((((5 0 0 1 1 1) (0 0 0 0 0 0)) 1 (((5 0 0 0 0 0) (0 0 0 0 0 5)) HEURISTICA-DEFAULT 0 0 NIL) 0 NIL) (((0 0 0 0 0 0) (1 1 1 0 0 5)) 1 (((5 0 0 0 0 0) (0 0 0 0 0 5)) HEURISTICA-DEFAULT 0 0 NIL) 0 NIL))
(defun sucessores(no alg pmax &optional (jogador 0) )
    (cond 
        ((and (equal alg 'dfs) (>= (get-node-depth no) pmax) ) nil)
        (t
            (mapcar #'(lambda (board) (create-node board nil nil (1+ (get-node-depth no)) no))
                     (sucessores-aux jogador 0 (get-node-state no)) )
        )
    )
)

;; teste:  (sucessores-aux 0 0 '((5 0 0 1 1 1) (0 0 0 0 0 0)))
;; result: (((0 0 0 1 1 1) (1 1 1 0 0 0))) 
;; returns: (listaSucessoroers listaSucessoroers) 
(defun sucessores-aux(rowIndex cellIndex board &aux (nextCell (get-next-cellV1 rowIndex cellIndex)))
    (cond 
        ( (null nextCell) (list (allocate-pieces rowIndex cellIndex board) ))
        ( (not (is-move-validp rowIndex cellIndex board)) (sucessores-aux rowIndex nextCell board ) )
        ( (columnsValid rowIndex cellIndex) 
            (append (sucessores-aux rowIndex nextCell board) (list (allocate-pieces rowIndex cellIndex board) ))
        )
    )
)

;; teste: (columnsValid 0 0)
;; result: nil
;; returns: (bolean numNodesGerados) 
(defun columnsValid(row column)
  (cond 
        ((and (= row 0) (>= column 0) (<= column 5)) t )
        ((and (= row 1) (<= column 5) (>= column 0)) t )
        (t nil)
  )
)
;; teste: (sucessores-aux-a* 0 0 (teste3) '())
;; result: ((((0 0 0 0 0 3) (1 1 1 1 5 1)) heuristica-default 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) 0)
;; returns: (listaAbertos numNodesGerados) 
(defun get-next-cellV1 (rowIndex cellIndex)
  "Calcula e retorna o index da proxima coluna com base no index da coluna e linha atual"
  (cond 
    ((eq nil cellIndex) nil)
    ((AND (> cellIndex 0) (= rowIndex 0) ) (- cellIndex 1))
    ((AND (< cellIndex 5) (= rowIndex 1) ) (1+ cellIndex))
    (t nil)
  )
)

(defun teste2 ()
  "Funcao que cria no inicial dum tabuleiro aleatorio para testar"
  (create-node '((3 0 1 5 1 1) (4 0 0 1 5 0)) "f" 0)
)

;;(print (sucessores-aux 0 0 '((5 0 0 1 1 1) (0 0 0 0 0 0))))
;;(print (allocate-pieces 0 0 '((5 0 0 1 1 1)
;;                              (0 0 0 0 0 0))))

;;(print (sucessores (teste2) 'dfs 2 0))
;;;;;;;;;;;;;;; HEURISTICAS ;;;;;;;;;;;;;;;

;; verifica qual a heuristica a usar e chama-a com os respetivos argumentos
(defun call-heuristic (heuristica board parentNode)
  "Como as heuristicas tem argumentos diferentes, esta funcao ve qual a heuristica que o node esta a usar e chama a respetiva funcao com os argumentos corretos"
  (cond ((string-equal heuristica 'heuristica-default) (heuristica-default board parentNode))
        (t (heuristica-extra board 0 0 0 t)))
)

(defun heuristica-default (board node)
  "Calcula a heuristica predefinida (fornecida pelo enunciado)"
  (let ((newBoardValue (board-value board)))
    (- newBoardValue (- (board-value (get-node-state node)) newBoardValue)))
)

;; teste: (board-value (get-node-state (teste)))
;; result: 96
(defun board-value (board) 
  "Funcao auxiliar a heuristica default. Calcula o valor total (soma do valor de cada posicao) do tabuleiro recebido"
  (+ (apply '+ (first board)) (apply '+ (second board)))
)


(defun heuristica-extra (board numJogadasPossiveis rowIndex cellIndex &optional (isFirstCall nil))
  "Calcula o numero de jogadas validas no tabuleiro"
  (cond ((AND (not isFirstCall) (= rowIndex 0) (= cellIndex 0)) numJogadasPossiveis)
        (t (let* ((nextRow (get-next-row rowIndex cellIndex))

                 (nextCell (get-next-cell rowIndex cellIndex)))

             (cond ((is-move-validp nextRow nextCell board) (heuristica-extra board (1+ numJogadasPossiveis) nextRow nextCell))
                   (t (heuristica-extra board numJogadasPossiveis nextRow nextCell))))))
)

;;;;;;;;;;;;;;; Algos ;;;;;;;;;;;;;;;

;; returns (nosExpandidos nosGerados penetrancia, fatorRamificacao, noSolucao)
(defun a* (starter-node depth &optional (abertos (list starter-node)) (fechados nil) (nodes-expandidos 0) (nodes-gerados 0))
  "Algoritmo de procura em espaco de estados A*"
  (cond ((null abertos) nil)
        (t 
           (let ((currNode (first abertos)))
             
             ; nao vale a pena gerar os sucessores se este for no solucao
             (cond ((node-solutionp currNode) (list nodes-gerados
                                                     nodes-expandidos
                                                     (penetrancia (get-node-depth currNode) nodes-gerados)
                                                     (fator-ramificacao (get-node-depth currNode) nodes-gerados)
                                                     currNode)) 

                   ; nao e solucao, vamos continuar
                   (t (let* ((newFechados (append fechados (list currNode)))
                  
                             (sucsGerados (sucessores-a* currNode (rest abertos) t))

                             (newAbertos (sort (first sucsGerados) 'shortest-cost-sort-compare)))

                    (a* (first newAbertos) depth newAbertos newFechados (1+ nodes-expandidos) (+ nodes-gerados (second sucsGerados))))))
           )
       )
  )
)

;; procura em largura - BFS
;; teste: (bfs (no-teste) 
;; resultado: (((0 0 0 0 0 0) (0 0 0 0 0 0)) 10 (((0 0 0 0 0 1) (0 0 0 0 0 0)) 9 (((0 0 0 0 1 1) (0 0 0 0 0 0)) 8 (((0 0 0 1 1 1) (0 0 0 0 0 0)) 7 (((0 0 1 1 1 1) (0 0 0 0 0 0)) 6 (((0 0 1 1 1 1) (1 0 0 0 0 0)) 5 (((0 0 1 1 1 1) (1 1 0 0 0 0)) 4 (((0 0 1 1 1 1) (1 1 1 0 0 0)) 3 (((0 0 1 1 1 1) (1 1 1 1 0 0)) 2 (((5 0 1 1 1 1) (0 0 0 0 0 0)) 1 (((5 0 0 0 0 0) (0 0 0 0 0 5)) 0 NIL)))))))))))
;; returns (nosExpandidos nosGerados penetrancia, fatorRamificacao, noSolucao)
(defun bfs (no depth &optional (abertos (list no)) (fechados nil) (nos-gerados 0) (nos-expandidos 0))
  "Funcao que implementa o algoritmo \"Breadth first serac\""
  (let ((noAtual (first abertos))) 
    (cond 
        ((null abertos) nil)
          ((no-existe-fechadosp noAtual fechados) (dfs nil depth (cdr abertos) fechados))
          ((node-solutionp noAtual) (list nos-gerados
                                          nos-expandidos
                                          (penetrancia (get-node-depth noAtual) nos-gerados)
                                          (fator-ramificacao (get-node-depth noAtual) nos-gerados)
                                          noAtual))
        (t (let* ((sucsGerados (sucessores (car abertos) 'bfs depth))
             
                  (novosAbertos (append (cdr abertos) (remove-rep sucsGerados abertos fechados ))))

             (bfs nil depth novosAbertos (cons noAtual fechados) (+ nos-gerados (list-length sucsGerados)) (1+ nos-expandidos))))
    )
  )
)
(defun changePlayer(jogador)
(- 1 jogador)
)
;; procura em profundidade DFS
;; teste: (dfs (no-teste) 
;; resultado: (((0 0 0 0 0 0) (0 0 0 0 0 0)) 10 (((0 0 0 0 0 1) (0 0 0 0 0 0)) 9 (((0 0 0 0 1 1) (0 0 0 0 0 0)) 8 (((0 0 0 1 1 1) (0 0 0 0 0 0)) 7 (((0 0 1 1 1 1) (0 0 0 0 0 0)) 6 (((0 0 1 1 1 1) (1 0 0 0 0 0)) 5 (((0 0 1 1 1 1) (1 1 0 0 0 0)) 4 (((0 0 1 1 1 1) (1 1 1 0 0 0)) 3 (((0 0 1 1 1 1) (1 1 1 1 0 0)) 2 (((5 0 1 1 1 1) (0 0 0 0 0 0)) 1 (((5 0 0 0 0 0) (0 0 0 0 0 5)) 0 NIL)))))))))))
(defun dfs (no depth &optional(abertos (list no)) (fechados nil) (nos-gerados 0) (nos-expandidos 0) (jogador 0))
      (print "2")
    (print abertos)
  (print "1")
  "Funcao que implementa o algortimo \"Depth first search\""
  (let ((noAtual (first abertos))) 
    (cond 
        ((null abertos) nil)
        ((node-solutionp noAtual) (list nos-gerados
                                        nos-expandidos
                                        (penetrancia (get-node-depth noAtual) nos-gerados)
                                        (fator-ramificacao (get-node-depth noAtual) nos-gerados)
                                        noAtual))
        (t (let* ((sucsGerados (sucessores noAtual 'dfs depth (changePlayer jogador))) 
                  (novosAbertos (append (remove-rep sucsGerados abertos fechados) (cdr abertos))))(print novosAbertos)
             (dfs nil depth novosAbertos (cons noAtual fechados) (+ nos-gerados (list-length sucsGerados)) (1+ nos-expandidos) (changePlayer jogador) ) ))
    )
  )
)


(defun remove-rep(sucessores abertos fechados)
  "Remove os sucessores que existerem em abertos ou fechados"
  (apply #'append (mapcar #'(lambda (s) (cond ((no-existe-fechadosp s fechados)  nil)
                                              ((no-existe-fechadosp s abertos)  nil)
                                              (t (list s))
                                        )  
                            ) sucessores)
  )
)

;;;;; Avaliacao de eficiecia ;;;;;
;; penetrancia
(defun penetrancia (depth generated-nodes)
  "Funcao que permite calcular a penetrancia de um algoritmo"
  (cond
     ((not (zerop depth)) (float (/ depth generated-nodes)))
     (t 0)
  )
)

;; fator de ramificacao
(defun fator-ramificacao(depth nos-gerados &optional (limite-inferior 1) (limite-superior most-positive-fixnum) (margem-erro 0.00001) (limite-medio (/ (+ limite-inferior limite-superior) 2)))
  "Funcao que permite calcular o fator de ramificacao para um no. Aplica o metodo da bissecao para o calculo desta funcao com uma margem de erro de 0.00001"
  (cond
   ((< (- limite-superior limite-inferior) margem-erro) (float limite-medio)) ;Se a diferenca entre os limites for inferior a margem de erro entao podemos assumir com precisao que o fator de ramificacao encontra-se entre estes dois limites

   ((> (- (polinomial depth limite-medio) nos-gerados) margem-erro) (fator-ramificacao depth nos-gerados limite-inferior limite-medio margem-erro))  ;Caso o valor esteja mais perto do limite-inferior, diminui-se o limite superior

   (T (float (fator-ramificacao depth nos-gerados limite-medio limite-superior margem-erro))) ;Caso o valor esteja mais perto do limite-superior, aumenta-se o limite inferior

  )
)

;; polinomial
(defun polinomial (grau polinomio)
  "Funcao que implementa o calculo de uma funcao polinomial"
    (cond
     ((= grau 1) polinomio)
     (t (+ (expt polinomio grau) (polinomial (1- grau) polinomio)))
    )
)


(defun teste (no depth &optional(abertos (list no)) (fechados nil) (nos-gerados 0) (nos-expandidos 0) (jogador 0))
  "Funcao que implementa o algortimo \"Depth first search\""
  (let ((noAtual (first abertos))) 
    (cond
        ((= 0 depth) abertos)
        ((null abertos) nil)
        ((node-solutionp noAtual) (list nos-gerados
                                        nos-expandidos
                                        (penetrancia (get-node-depth noAtual) nos-gerados)
                                        (fator-ramificacao (get-node-depth noAtual) nos-gerados)
                                        noAtual))
        (t (let* ((sucsGerados (sucessores noAtual 'dfs depth (changePlayer jogador))) 
                  (novosAbertos (append (remove-rep sucsGerados abertos fechados) (cdr abertos))))(print novosAbertos)
             (dfs nil depth novosAbertos (cons noAtual fechados) (+ nos-gerados (list-length sucsGerados)) (1+ nos-expandidos) (changePlayer jogador) ) ))
    )
  )
)

(defun sucessoresMiniMax(no alg &optional (jogador 0) )
    
            (mapcar #'(lambda (board) (create-node board nil nil (1+ (get-node-depth no)) no))
                     (sucessores-auxB jogador (ab jogador) (get-node-state no)) )
)
(defun ab(a)
 (cond ((= 0 a) 5)
    (t 0)
 )
)
(defun sucessores-auxB(rowIndex cellIndex board &aux (nextCell (get-next-cellV1 rowIndex cellIndex)))
    (cond 
         ( (null cellIndex) nil)
        ( (not (is-move-validp rowIndex cellIndex board)) (sucessores-auxB rowIndex nextCell board ))
        ( (columnsValid rowIndex cellIndex) 
            (append (sucessores-auxB rowIndex nextCell board) 
                (list (allocate-pieces rowIndex cellIndex board) )
            )
        )
    )
)
(defun aval (no jogador &aux (row (get-row jogador (get-node-state no))) )
    (let
    (
    (valorA 
    
    (apply #'+ 
    (append (get-row 0 (get-node-state no)) (get-row 1 (get-node-state no)) ) ))
    (valorb 
    (apply #'+  
        (append
            (get-row 0 (get-node-state(get-node-parent no)))
            (get-row 1 (get-node-state (get-node-parent no)))
        )
    )
    )
    )
    
    (- valorB valorA)
    )
)
(defun riberionaomedeixajogarM(elemA elemB )
    
    (> (second elemA) (second elemB))
)

(defun riberionaomedeixajogarL(elemA elemB )

    (< (second elemA) (second elemB))
)
(defun minimax(no mDepth jogador)
    (cond 
        ((= 0 mDepth) (list no (aval no jogador)) )
        ( (= 1 jogador)
           (first (sort (mapcar  #'(lambda (board) 
                (minimax board (- mDepth 1) (changePlayer jogador))
            )
                (sucessoresMiniMax no 'dfs  jogador)
            )  'riberionaomedeixajogarL) )
        )
        ( (= 0 jogador)
         (first    (sort (mapcar  #'(lambda (board) 
                (minimax board (- mDepth 1) (changePlayer jogador))
            )
                (sucessoresMiniMax no 'dfs jogador)
            ) 'riberionaomedeixajogarM) )
        )
    )
)

(print (minimax (teste2) 6 0))