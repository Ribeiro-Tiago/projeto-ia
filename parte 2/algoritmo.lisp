;;;; algoritmo.lisp
;;;; Implementacao do alfabeta
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;;;;;;;;;;;; variaveis globais ;;;;;;;;;;;;;;;
(defvar *hashTable* (make-hash-table))

;;;;;;;;;;;;;;; Construtor ;;;;;;;;;;;;;;;
;; custo = f | heuristica = g
(defun create-node (board custo &optional (depth 0) (parent nil))
  "Construtor do no das arvores para os algoritmos"
  (list board custo depth parent)
)


;;;;;;;;;;;;;;; getters ;;;;;;;;;;;;;;;

;; Test: (get-node-state (teste))
;; Result: ((8 8 8 8 8 8) (8 8 8 8 8 8))
(defun get-node-state (node)
  "Devolve o estado (o tabuleiro) do problema neste no"
  (first node)
)

;; Teste: (get-node-cost (teste))
;; Result: 0
(defun get-node-cost (node)
  "Retorna o custo do no"
  (second node)
)

;; Teste: (get-node-depth (teste))
;; Result: 0
(defun get-node-depth (node)
  "Retorna a profundidade do {node} recebido"
  (third node)
)

;; Teste: (get-node-parent (teste))
;; Result: NIL
(defun get-node-parent (node)
  "Retorna o no pai de {node}"
  (fourth node)
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

(defun sucessores (node abertos isFirstCall &optional (sucs '()) (rowIndex 0) (cellIndex 0))
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

(defun sucessores-aux (rowIndex cellIndex parentNode abertos)
  "Verifica se a posicao [rowIndex[cellIndex]] e valida, se for expande esse no, gerando o novo tabuleiro deopis dessa jogada e criando um novo no. Senao passa a frente"
  (let ((board (get-node-state parentNode)))
    (cond ((move-validp rowIndex cellIndex board) ; so geramos sucessores se for uma casa com valor > 0
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


;;;;;;;;;;;;;;; ALFABETA ;;;;;;;;;;;;;;;

(defun alfabeta ()
  
)


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


;;;;;;;;;;;;;;; MEMOIZACAO ;;;;;;;;;;;;;;;
(defun memoizacao (node)
  "Funcao que verifica se ja existe um resultado alfabeta para o no passado na hash table, caso exista devolve-o, caso nao exista calcula o seu valor, retorna-o e insere-o na hash table"
  (let ((nodeTable (get-hash-node node)))
    (cond ((null nodeTable) ;; calcula  
           (let ((newNode (alfabeta)))
             (setf (get-hash-node node) newNode)
             newNode
             ))
          (t nodeTable)))
)

(defun get-hash-node (node) 
  "Funcao que vai buscar o no a hash table"
  (gethash (hash-node node) *hashTable*)
)

(defun hash-node (no)
  "Funcao que converte o estado e as pecas dos jogadores de um no numa string para que possa ser usada como key na hash table de memoizacao"
  (concatenate 'string (to-string (get-estado-no no)) (to-string (get-pecas-jogador1-no no)) (to-string (get-pecas-jogador2-no no)))
)

(defun to-string (item)
  (format nil "~S" item)
)
