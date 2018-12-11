 ;;;; board-handler.lisp
;;;; Implementacao dos algoritmos de procura em espa�o de estados
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;; Construtor ;;;;;
;; custo = f | heuristica = g
(defun create-node (board heuristica custo &optional (depth 0) (parent nil))
  "Construtor do n� das �rvores para os algoritmos"
  (list board heuristica custo depth parent)
)


;;;;; getters ;;;;;

;; Test: (node-state (create-node (empty-board)))
;; Result: ((0 0 0 0 0 0) (0 0 0 0 0 0))
(defun get-node-state (node)
  "Devolve o estado (o tabuleiro) do problema neste no"
  (first node)
)

;; Teste: (node-heuristic (create-node (empty-board)))
;; Result: 0
; num_pe�as_por_capturar - num_pe�as_capturadas
(defun get-node-heuristic (node)
  "Retorna a heuristica que � usada para calcular o custo do no"
  (second node)
)

;; Teste: (node-heuristic (create-node (empty-board)))
;; Result: 0
(defun get-node-cost (node)
  "Retorna o custo do no"
  (third node)
)

;; Teste: (node-depth (create-node (empty-board)))
;; Result: 0
(defun get-node-depth (node)
  "Retorna a profundidade do {node} recebido"
  (fourth node)
)

;; Teste: (node-parent (create-node (empty-board)))
;; Result: NIL
(defun get-node-parent (node)
  "Retorna o n� pai de {node}"
  (fifth node)
)

;; Test: (node-solutionop (create-node (empty-board)))
;; Result: T
(defun node-solutionop (node) 
  "Verifica se o {node} e um no solucao"
  (cond ((board-emptyp (get-node-state node))))
)


;;;;; Fun��es auxiliares aos algos ;;;;;
(defun shortest-cost-sort-compare (a b)
  "Fun��o de compara��o de custo mais baixo para o sort"
  (< (nth 2 a) (nth 2 b))
)

(defun get-node-in-abertos (board abertos &optional (index 0))
  "Verifica se {node} encontra-se na lista dos {fechados}"
  (let ((curr-item (first abertos)))

    (cond ((null abertos) nil)
          ((equal board (get-node-state curr-item)) (list curr-item index))
          (t (get-node-in-abertos board (rest abertos)))))
)

;; teste: (sucessores (teste3) (list (teste3)))
;; result: (((((0 0 0 0 0 3) (1 1 1 1 5 1)) CALC-HEURISTICA 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) (((8 0 0 0 0 3) (0 0 0 0 0 1)) CALC-HEURISTICA 11 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) (((8 0 0 0 1 0) (0 0 0 0 4 0)) CALC-HEURISTICA 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL))) 3)
;; returns: (novaListaAbertos numNodesGerados)
(defun sucessores (node abertos &optional (sucs '()) (rowIndex 0) (cellIndex 0))
  "Percorre as posi��es todas do estado do {node} e gera os seus n�s sucessores"
  (cond ((AND (= rowIndex 0) (= cellIndex 1)) (list (append sucs abertos) (list-length sucs))) ; deu a volta toda
        (t (let* ((result-sucs (sucessores-aux rowIndex cellIndex node abertos)) ; Devolve (listaAbertos sucessor)

                 (nextRow (get-next-row rowIndex cellIndex))

                 (nextCell (get-next-cell rowIndex cellIndex))
                 
                 (newSucs (cond ((null (first result-sucs)) sucs)
                                (t (append sucs (list (first result-sucs)))))))

             (sucessores node (list (first result-sucs)) newSucs nextRow nextCell))))
)

;; teste: (sucessores-aux 0 0 (teste3) '())
;; result: ((((0 0 0 0 0 3) (1 1 1 1 5 1)) CALC-HEURISTICA 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) 0)
;; returns: (listaAbertos numNodesGerados) 
(defun sucessores-aux (rowIndex cellIndex parentNode abertos)
  "Verifica se a posi��o [rowIndex[cellIndex]] � valida, se for expande esse n�,
   gerando o novo tabuleiro deopis dessa jogada e criando um novo n�. Sen�o passa � frente"
  (let ((board (get-node-state parentNode)))

    (cond ((is-move-validp rowIndex cellIndex board) ; s� geramos sucessores s� for uma casa com valor > 0

        (let* ((newBoard (allocate-pieces rowIndex cellIndex board))

               (depth (1+ (get-node-depth parentNode)))

               (value (+ (calc-heuristica newBoard parentNode) depth))

               (oldNode (get-node-in-abertos newBoard abertos))

               (newNode (create-node newBoard 'calc-heuristica value depth parentNode)))

           (cond ((not (first oldNode)) (cons newNode '(0))) ; n�o est� em abertos
                 (t (cond ((> value (get-node-cost (first oldNode))); t� em abertos, vamos comparar valores e substituir se no gerado for superior
                          (cons newNode (replace-nth-in-list abertos (second oldNode) (first oldNode))))))
           )
        )
    ))
  )
)

(defun replace-nth-in-list (list n elem)
  "Substitui o elemento na posi��o {n} da {list} pelo {eleme} recebido"
  (cond
    ((null list) list)
    ((= n 0) (cons elem (rest list)))
    (t (cons (first list) (replace-nth-in-list (rest list) (- n 1) elem))))
)


(defun board-value (board) 
  "Calcula o valor total (soma do valor de cada posi��o) do tabuleiro recebido"
  (+ (apply '+ (first board)) (apply '+ (second board)))
)

(defun calc-heuristica (board node)
  "Calcula a heuristica predefinida"
  (let ((newBoardValue (board-value board)))
    (- newBoardValue (- (board-value (get-node-state node)) newBoardValue)))
)


;;;;; Algos ;;;;;
(defun a* (starter-node &optional (abertos (list starter-node)) (fechados nil) (nodes-expandidos 0) (nodes-gerados 0))
  "Algoritmo de procura em espaco de estados A*"

  (cond ((null abertos) nil)
        (t 
           (let* ((currNode (first abertos))
                  
                  (newFechados (append fechados (list currNode)))
                  
                  (sucsGerados (sucessores currNode (rest abertos)))

                  (newAbertos (first sucsGerados)))
             
             (cond ((node-solutionop currNode) (list nodes-expandidos nodes-gerados currNode))
                    (t (a* (first newAbertos) newAbertos newFechados (1+ nodes-expandidos) (+ nodes-gerados (second sucsGerados))))))
       )
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