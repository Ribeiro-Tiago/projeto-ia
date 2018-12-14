;;;; procura.lisp
;;;; Implementacao dos algoritmos de procura em espaco de estados
;;;; Disciplina de IA - 2018 / 2019
;;;; Autor: Tiago Alves & Tiago Ribeiro

;;;;; Construtor ;;;;;
;; custo = f | heuristica = g
(defun create-node (board heuristica custo &optional (depth 0) (parent nil))
  "Construtor do nó das árvores para os algoritmos"
  (list board heuristica custo depth parent)
)


;;;;; getters ;;;;;

;; Test: (get-node-state (teste))
;; Result: ((8 8 8 8 8 8) (8 8 8 8 8 8))
(defun get-node-state (node)
  "Devolve o estado (o tabuleiro) do problema neste no"
  (first node)
)

;; Teste: (get-node-heuristic (teste))
;; Result: heuristica-default
(defun get-node-heuristic (node)
  "Retorna a heuristica que é usada para calcular o custo do no"
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

;; Test: (node-solutionop (teste))
;; Result: nil
(defun node-solutionop (node) 
  "Verifica se o {node} e um no solucao"
  (cond ((board-emptyp (get-node-state node))))
)


;;;;; Funções auxiliares aos algos ;;;;;
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

;; teste: (sucessores (teste3) (list (teste3)))
;; result: (((((0 0 0 0 0 3) (1 1 1 1 5 1)) heuristica-default 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) (((8 0 0 0 0 3) (0 0 0 0 0 1)) heuristica-default 11 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) (((8 0 0 0 1 0) (0 0 0 0 4 0)) heuristica-default 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL))) 3)
;; returns: (novaListaAbertos numNodesGerados)
(defun sucessores (node abertos isFirstCall &optional (sucs '()) (rowIndex 0) (cellIndex 0))
  "Percorre as posicoes todas do estado do {node} e gera os seus nos sucessores"
  (cond ((AND (= rowIndex 0) (= cellIndex 0) (not isFirstCall)) (build-end-sucs-list sucs abertos)) ; deu a volta toda
        (t (let* ((result-sucs (sucessores-aux rowIndex cellIndex node abertos)) ; Devolve (listaAbertos sucessor)

                 (nextRow (get-next-row rowIndex cellIndex))

                 (nextCell (get-next-cell rowIndex cellIndex))
                 
                 (newSucs (cond ((null (first result-sucs)) sucs)
                                (t (append sucs (list (first result-sucs)))))))

             (sucessores node (list (first result-sucs)) nil newSucs nextRow nextCell))))
)

(defun build-end-sucs-list (sucs abertos)
  "Constrói a lista retornada no final da função sucessores"
  (cond ((OR (null abertos) (null (first abertos))) (list sucs (list-length sucs)))
        (t (list (append sucs abertos) (list-length sucs)))) 
)

;; teste: (sucessores-aux 0 0 (teste3) '())
;; result: ((((0 0 0 0 0 3) (1 1 1 1 5 1)) heuristica-default 13 1 (((8 0 0 0 0 2) (0 0 0 0 4 0)) 14 14 0 NIL)) 0)
;; returns: (listaAbertos numNodesGerados) 
(defun sucessores-aux (rowIndex cellIndex parentNode abertos)
  "Verifica se a posicao [rowIndex[cellIndex]] e valida, se for expande esse no,
   gerando o novo tabuleiro deopis dessa jogada e criando um novo no. Senao passa a frente"
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

;;;;; HEURISTICAS ;;;;;

;; verifica qual a heuristica a usar e chama-a com os respetivos argumentos
(defun call-heuristic (heuristica board parentNode)
  "Como as heuristicas têm argumentos diferentes, esta função vê qual é a heuristica que o node está a usar e chama a respetiva função com os argumentso corretos"
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
  "Função auxiliar à heuristica default. Calcula o valor total (soma do valor de cada posicao) do tabuleiro recebido"
  (+ (apply '+ (first board)) (apply '+ (second board)))
)


(defun heuristica-extra (board numJogadasPossiveis rowIndex cellIndex &optional (isFirstCall nil))
  "Calcula o número de jogadas validas no tabuleiro"
  (cond ((AND (not isFirstCall) (= rowIndex 0) (= cellIndex 0)) numJogadasPossiveis)
        (t (let* ((nextRow (get-next-row rowIndex cellIndex))

                 (nextCell (get-next-cell rowIndex cellIndex)))

             (cond ((is-move-validp nextRow nextCell board) (heuristica-extra board (1+ numJogadasPossiveis) nextRow nextCell))
                   (t (heuristica-extra board numJogadasPossiveis nextRow nextCell))))))
)

;;;;; Algos ;;;;;
;; returns (nosExpandidos nosGerados penetrancia, fatorRamificacao, noSolucao)
(defun a* (starter-node &optional (abertos (list starter-node)) (fechados nil) (nodes-expandidos 0) (nodes-gerados 0))
  "Algoritmo de procura em espaco de estados A*"
  (cond ((null abertos) nil)
        (t 
           (let ((currNode (first abertos)))
             
             ; nao vale a pena gerar os sucessores se este for no solução
             (cond ((node-solutionop currNode) (list nodes-gerados
                                                     nodes-expandidos
                                                     (penetrancia (get-node-depth currNode) nodes-gerados)
                                                     (fator-ramificacao (get-node-depth currNode) nodes-gerados)
                                                     currNode)) 

                   ; nao e solucao, vamos continuar
                   (t (let* ((newFechados (append fechados (list currNode)))
                  
                             (sucsGerados (sucessores currNode (rest abertos) t))

                             (newAbertos (sort (first sucsGerados) 'shortest-cost-sort-compare)))

                    (a* (first newAbertos) newAbertos newFechados (1+ nodes-expandidos) (+ nodes-gerados (second sucsGerados))))))
           )
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