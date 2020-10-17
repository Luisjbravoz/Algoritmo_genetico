#lang racket
(require framework)
(require plot)

;; PROJECT: GENETIC ALGORITHM
;; LUIS J. BRAVO ZÚÑIGA.
;; THIS PROJECT CONSIST OF APPROXIMATING AN ARBITRARY FUNCTION, USING A GENETIC ALGORITHM.
;; TAKE INTO CONSIDERATION:
;; 1. THIS PROGRAM WORK WITH FUNCTIONS OF TWO VARIABLES.
;; 2. FUNCTIONS ARE COMPOSED BY THIS OPERATORS: +, -, /, *, ^ and log.
;; 3. FILES NAMES MUST TO BE: fx.txt fy.txt fz.txt
;; 4. PROBABILITY OF MUTATION IS 10%.
;; 5. (MAIN <NUMBER OF INDIVIDUALS> <NUMBER OF GENERATIONS>)


;; 1. FUNCTIONS FOR READ FILE.

;; INPUT: EMPTY.
;; OUTPUT: A LIST WITH FILE CONTENT
;; DESCRIPTION: READ A TXT FILE.
(define read-x
 (call-with-input-file "fx.txt"
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x ) '() (cons x (f (read p))))))))

(define read-y
 (call-with-input-file "fy.txt"
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x ) '() (cons x (f (read p))))))))

(define read-z
 (call-with-input-file "fz.txt"
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x ) '() (cons x (f (read p))))))))
		  

;; 2. FUNCTIONS FOR VALIDATE OPERATORS.

;; INPUT: TWO REAL NUMBERS.
;; OUTPUT: ONE REAL NUMBER.
;; DESCRIPTION: DIVIDE.
(define div
  (lambda (numerator denominator)
    (cond
      ((zero? denominator) +inf.0)
      (else (/ numerator denominator)))))

;; INPUT: ONE REAL NUMBER
;; OUTPUT: ONE REAL NUMBER.
;; DESCRIPTION: QUADRATIC. 
(define cuad
  (lambda (x)
    (* x x)))

;; INPUT: ONE REAL NUMBER AND ONE POSITIVE INTEGER.
;; OUTPUT: ONE REAL NUMBER.
;; DESCRIPTION: EMPOWERMENT OF REAL NUMBER AND POSITIVE INTEGER.
(define empowerment
  (lambda (base exp)
    (cond ((zero? exp) 1)
          ((even? exp) (cuad (empowerment base (quotient exp 2))))
          (else (* base (cuad (empowerment base (quotient (- exp 1) 2))))))))

;; INPUT: TWO REAL NUMBER.
;; OUTPUT: ONE REAL NUMBER.
;; DESCRIPTION: EMPOWERMENT ANY EXPRESION.
(define emp
  (lambda (base exp)
    (cond
      ((and (zero? base) (zero? exp)) +inf.0)
      (else (expt base exp)))))

;; INPUT: ONE POSITIVE INTEGER.
;; OUTPUT: ONE REAL NUMBER.
;; DESCRIPTION: LOG BASE 10.
(define loga
  (lambda (base argument)
    (cond
      ((> 2 base) +inf.0)
      ((zero? argument) +inf.0)
      (else (div (log argument) (log base))))))

  
;; 3. FUNCTIONS FOR RANDOM.

;; INPUT: EMPTY.
;; OUTPUT: OPERATOR.
;; DESCRIPTION: RANDOMLY GET AN OPERATOR.
(define random-operator
  (lambda ()
      (list-ref '(+ - / * expt log) (random 6))))

;; INPUT: EMPTY. 
;; OUTPUT: SYMBOL.
;; DESCRIPTION: RANDOMLY GET AN SYMBOL.
(define random-symbol
  (lambda ()
    (aux-random-symbol (random 3))))

(define aux-random-symbol
  (lambda (n)
    (cond
      ((= 0 n) 'x)
      ((= 1 n) 'y)
      (else (+ 1 (random 100))))))


;; 4. FUNCTIONS FOR GENERATE INDIVIDUALS

;; INPUT: EMPTY. 
;; OUTPUT: BOOLEAN VALUE.
;; DESCRIPTION: DETERMINE RANDOMLY IF IS A LEAF.
(define leaf?
  (lambda ()
    (< (random 100) 60)))

;; INPUT: EMPTY.   
;; OUTPUT: 
;; DESCRIPTION: GENERATE INDIVIDUAL.
(define branch
  (lambda ()
    (cond
      ((leaf?) (random-symbol))
          (else (generate-individual)))))

;; INPUT: EMPTY.   
;; OUTPUT: INDIVIDUAL.
;; DESCRIPTION: GENERATE INDIVIDUAL.
(define generate-individual
  (lambda ()
    (list (random-operator) (branch) (branch))))
	
	
;; 5. FUNCTIONS FOR GENERATE POPULATION

;; INPUT: ONE INTEGER NUMBER.  
;; OUTPUT: POPULATION.
;; DESCRIPTION: GENERATE POPULATION.
(define generate-population
  (lambda (n)
    (cond
      ((and (integer? n) (positive? n)) (aux-generate-population '() n))
      (else (display "ARGUMENT MUST TO BE ONE POSITIVE INTEGER NUMBER!")))))

(define aux-generate-population
  (lambda (population n)
    (cond
      ((zero? n) population)
      (else (aux-generate-population (cons (generate-individual) population) (- n 1)))))) 


;; 6. FITNESS FUNCTION: WHO IS BETTER?

;; INPUT: INDIVIDUAL AND TWO REAL NUMBERS.
;; OUTPUT: ONE REAL NUMBER, RESULT OF EVALUATE THIS INDIVIDUAL.
;; DESCRIPTION: REPLACES "X" AND "Y" FOR NUMBERS AND EVALUATE THE INDIVIDUAL.
(define replace-and-evaluate
  (lambda (individual x-value y-value)
    (eval (list (cons 'lambda (cons '(x y) (list individual))) x-value y-value))))
	
;; INPUT: POPULATION AND TWO REAL NUMBERS.
;; OUTPUT: A LIST OF REAL NUMBERS RESULT OF EVALUATE INDIVIDUALS.
;; DESCRIPTION: TAKE ONE BY ONE THE INDIVIDUALS OF THE POPULATION AND EVALUATE THEM.
(define evaluate-population
  (lambda (population x-value y-value)
    (map (lambda (individual) 
	           (with-handlers ([exn:fail:contract:divide-by-zero?
                            (lambda (exn) (random 100000))])
               (replace-and-evaluate individual x-value y-value))) population)))
			 
;; INPUT: A LIST OF RESULT TO VALUATE THE POPULATION AND ONE REAL NUMBER (VALUE Z) 
;; OUTPUT: A LIST OF REAL NUMBERS RESULT OF THE DIFFERENCE BETWEEN THE RESULT OF EVALUATE ONE BY ONE THE INDIVIDUALS AND Z-VALUE.
;; DESCRIPTION: CALCULATE THE DIFFERENCE BETWEEN THE RESULT OF EVALUATE ONE BY ONE THE INDIVIDUALS AND Z-VALUE.
(define difference-points
  (lambda (population-valuate z-value)
    (map (lambda (z)
           (cond
             ((equal? z +inf.0) +inf.0)
             ((real? z) (abs (- z z-value)))
             (else +inf.0)))
         population-valuate)))

;; INPUT: POPULATION. 
;; OUTPUT: A MATRIX OF REAL NUMBERS RESULTS OF THE POPULATION EVALUATED IN MANY POINTS.
;; DESCRIPTION: EVALUATE THE POPULATION IN MANY POINTS.
(define multiple-evaluate-population
  (lambda (population)
    (map (lambda (x-value y-value z-value) (difference-points (evaluate-population population x-value y-value) z-value)) read-x read-y read-z)))

;; INPUT: POPULATION.  
;; OUTPUT: A LIST OF TOTALS PER FUNCTION.
;; DESCRIPTION: EVALUATE THE POPULATION AND SUM THE RESULTS.
(define fitness-function
  (lambda (population)
    (apply map + (multiple-evaluate-population population))))


;; 7. CROSSBREEDS AND MUTATION.

;; INPUT: TWO INDIVIDUALS.
;; OUTPUT: ONE INDIVIDUALS.
;; DESCRIPTION: CROSSBREEAD THE INDIVIDUALS.
(define crossbreead
  (lambda (ind-1 ind-2)
    (cond
      ((mutation?) (list (car ind-1) (generate-individual) (caddr ind-2)))
      (else (list (car ind-1) (cadr ind-1) (caddr ind-2))))))
	  
;; INPUT: EMPTY.
;; OUTPUT: BOOLEAN VALUE.
;; DESCRIPTION: DETERMINE IF IS TIME OF MUTATION OR NOT.
(define mutation?
  (lambda ()
    (<= (random 101) 11)))


;; 8. NEXT GENERATION AND ELITISM.

;; INPUT: POPULATION AND LIST OF RESULTS OF INDIVIDUALS AFTER BEING EVALUATED.
;; OUTPUT: A MATRIX OF INDIVIDUALS AND THEY PROBABILITY TO BE SELECTED FOR CROSSBREEDS.
;; DESCRIPTION: COMPUTE THE PROBABILITY OF ALL INDIVIDUALS TO BE SELECTED FOR CROSSBREEDS.
(define probability
  (lambda (population results)
    (compute-probability-one-by-one population (compute-probability results) 0)))
	
;; INPUT: POPULATION, LIST OF PROBABILITIES AND ZERO.
;; OUTPUT: A MATRIX OF INDIVIDUALS AND THEY PROBABILITY TO BE SELECTED FOR CROSSBREEDS.
;; DESCRIPTION: COMPUTE THE PROBABILITY OF INDIVIDUALS OF POPULATION ONE BY ONE.
(define compute-probability-one-by-one
  (lambda (population probabilities acumulate)
    (cond
      ((null? (cdr probabilities)) (list (list (car population) (+ acumulate (car probabilities)))))
      (else (cons (list (car population) (+ acumulate (car probabilities))) (compute-probability-one-by-one (cdr population) (cdr probabilities) (+ acumulate (car probabilities))))))))

;; INPUT: LIST OF RESULTS OF INDIVIDUALS AFTER BEING EVALUATED.
;; OUTPUT: LIST OF PROBABILITIES OF INDIVIDUALS.
;; DESCRIPTION: CALCULATE THE PROBABILITIES OF INDIVIDUALS.
(define compute-probability
  (lambda (results)
    (map (lambda (individual) (* 10 (- 1 (/ individual (apply + results)))))  results)))

;; INPUT: A MATRIX OF INDIVIDUALS AND THEY PROBABILITY TO BE SELECTED FOR CROSSBREEDS AND RANDOM INTEGER NUMBER.
;; OUTPUT: A INDIVIDUAL.
;; DESCRIPTION: CHOOSE AN INDIVIDUAL BASED ON HIS PROBABILITY.
(define who-will-be-selected
  (lambda (matrix-inv-prob random-number)
    (cond ((null? (cdr matrix-inv-prob)) (caar matrix-inv-prob))
          ((< random-number (car(cdar matrix-inv-prob))) (caar matrix-inv-prob))
          (else (who-will-be-selected (cdr matrix-inv-prob) random-number)))))

;; INPUT: TWO INTEGERS NUMBERS (LENGTH OF POPULATION) AND  A MATRIX OF INDIVIDUALS AND THEY PROBABILITY TO BE SELECTED.
;; OUTPUT: NEXT GENERATION.
;; DESCRIPTION: GENERATE NEXT GENERATION.
(define next-generation
  (lambda (length-1 length-2 matrix-inv-prob)
    (cond
      ((zero? length-1) '())
      (else (append (list (crossbreead
                           (who-will-be-selected matrix-inv-prob (random (* 10 (- length-2 1))))
                           (who-will-be-selected matrix-inv-prob (random (* 10 (- length-2 1))))))
                    (next-generation (- length-1 1) length-2 matrix-inv-prob))))))


;; 9. FUNCTIONS FOR FIND AND GRAPH THE BEST INDIVIDUAL.

;; INPUT: TWO INTEGERS NUMBERS. NUMBER OF INDIVIDUALS AND NUMBER OF GENERATION.
;; OUTPUT: GRAPH OF THE BETTER INDIVIDUAL FOR EACH GENERATION.
;; DESCRIPTION: MAIN FUNCTION.
(define main
  (lambda (n k)
    (cond
      ((and (integer-and-positive? n) (integer-and-positive? k)) (compute-generation (generate-population n) k))
      (else (display "ARGUMENTS MUST TO BE ONE POSITIVE INTEGER NUMBER!")))))

;; INPUT: POPULATION AND NUMBER OF GENERATIONS.
;; OUTPUT: LIST OF ALL THE GRAPHS OF ALL GENERATIONS.
;; DESCRIPTION: SELECT THE BEST INDIVIDUAL FROM EACH GENERATION.
(define compute-generation
  (lambda (population generation-n)
    (show 1 population)
    (aux-compute-generation (compute-population population) generation-n '() 2)))

;; INPUT: LIST OF THE GRAPH OF THE BEST INDIVIDUAL, NUMBER OF GENERATIONS, EMPTY LIST AND TWO.
;; OUTPUT: LIST OF ALL THE GRAPHS OF ALL GENERATIONS.
;; DESCRIPTION: SELECT THE BEST INDIVIDUAL FROM EACH GENERATION.
(define aux-compute-generation
  (lambda (list-graphs generation-n all-graphs acumulate)
    (cond ((zero? generation-n) all-graphs)
          (else (show acumulate (cadr list-graphs))
                (aux-compute-generation (compute-population (cadr list-graphs)) (- generation-n 1) (cons (car list-graphs) all-graphs) (+ acumulate 1))))))

;; INPUT: POPULATION.
;; OUTPUT: LIST WITH THE GRAPH OF THE BEST INDIVIDUAL OF THIS GENERATION.
;; DESCRIPTION: COMPUTE THE POPULATION.
(define compute-population
  (lambda (population)
    (graph-and-next-generation (probability population (fitness-function population)))))

;; INPUT: A MATRIX OF INDIVIDUALS AND THEY PROBABILITY.
;; OUTPUT: LIST WITH GRAPHICS AND NEXT GENERATION.
;; DESCRIPTION: ADD TO LIST THE GRAPH OF THE BEST INDIVIDUAL IN THIS GENERATION AND DEFINE NEXT GENERATION.
(define graph-and-next-generation
  (lambda (matrix-inv-prob)
    (list (graph-function (find-best matrix-inv-prob '() 0 0) read-x read-y read-z)
    (next-generation (length matrix-inv-prob) (length matrix-inv-prob) matrix-inv-prob))))

;; INPUT: A MATRIX OF INDIVIDUALS AND THEY PROBABILITY, EMPTY LIST AND TWO ZEROS. 
;; OUTPUT: BEST INDIVIDUAL OF THE GENERATION.
;; DESCRIPTION: FIND BEST INDIVIDUAL OF THE GENERATION.
(define find-best
  (lambda (matrix-inv-prob best-inv acum-points difference)
    (cond ((null? matrix-inv-prob) best-inv)
          ((> (- (cadar matrix-inv-prob) difference) acum-points) (find-best (cdr matrix-inv-prob) (caar matrix-inv-prob) (- (cadar matrix-inv-prob) difference) (cadar matrix-inv-prob)))
          (else (find-best (cdr matrix-inv-prob) best-inv acum-points (cadar matrix-inv-prob))))))

;; INPUT: ONE INDIVIDUAL, LIST OF X, Y AND Z VALUES.
;; OUTPUT: ONE GRAPHIC.
;; DESCRIPTION: GRAPH TO AN INDIVIDUAL.
(define graph-function
  (lambda (individual x-list-values y-list-values z-list-values)
    (plot3d (list (surface3d (eval (car (pre-graph-function individual))) (- 100) 100 (- 100) 100 #:label (string-join (map ~a individual) " ") )
                  (points3d (map vector x-list-values y-list-values z-list-values) #:color 3 #:sym 'fullcircle5)))))

;; INPUT: ONE INDIVIDUAL.
;; OUTPUT: ONE REAL NUMBER, RESULT OF EVALUATE THIS INDIVIDUAL.
;; DESCRIPTION: PREPARE AN INDIVIDUAL FOR BEING GRAPHICATED.
(define pre-graph-function
  (lambda (individual)
    (list (cons 'lambda (cons '(x y) (list individual))))))


;; 10. FUNCTIONS FOR VALIDATION DATA.

;; INPUT: ONE NUMBER.
;; OUTPUT: BOOLEAN VALUE.
;; DESCRIPTION: CHECK IF NUMBER IS INTEGER AND POSITIVE.
(define integer-and-positive?
  (lambda (number)
    (and (integer? number) (positive? number))))


;; 11. FUNCTIONS FOR SHOW DATA.

;; INPUT: ONE INTEGER NUMBER (NUMBER OF GENERATION) AND POPULATION.
;; OUTPUT: GENERATION: (ARG #1) POPULATION: (ARG #2)
;; DESCRIPTION: WRITE IN CONSOLE. 
(define show
  (lambda (n population)
    (display (string-append "\nGENERATION: " (number->string n) "\nPOPULATION:\n"))
    (map (lambda (element) (display element) (display "\n")) population)))