(import (chicken random)
        (chicken sort)
        (srfi 1)
        test
        test-generative
        (slib wt-tree))

(define elt-bound (expt 10 12))

(define size-bound 256)

;; test-generative's scoping is very simple, so some test groups
;; contain embedded loops.  Multiply this by test-generative's
;; iteration count to get the number of times these embedded tests
;; are run.
(define embedded-trial-iterations 10)

(define (random-nat-elt)
  (pseudo-random-integer elt-bound))

(define (random-nonzero-size)
  (let loop ((k (pseudo-random-integer size-bound)))
    (if (zero? k)
        (loop (pseudo-random-integer size-bound))
        k)))

;; These may contain duplicates.
(define (make-random-nat-alist size)
  (list-tabulate size
                 (lambda (_)
                   (cons (random-nat-elt) (random-nat-elt)))))

;; Super inefficient.
(define (remove-key-dups ps)
  (if (null? ps)
      ps
      (let ((key (caar ps)))
        (cons (car ps)
              (remove-key-dups
               (remove (lambda (p) (eqv? key (car p)))
                       (cdr ps)))))))

(define (make-random-wt-tree size)
  (alist->wt-tree number-wt-type
                  (make-random-nat-alist size)))

(define (sort-num-alist ps)
  (sort ps (lambda (p q) (< (car p) (car q)))))

(test-group "Basic wt-trees"
  (test-assert (wt-tree/empty? (make-wt-tree number-wt-type)))

  (test-generative ((tree (lambda () (make-random-wt-tree 10))))
    (test-assert (not (wt-tree/empty? tree)))
    (test-assert (wt-tree? tree)))

  (test-generative ((x random-nat-elt))
    (test 1 (wt-tree/size (singleton-wt-tree number-wt-type x x)))
    (test 1 (wt-tree/size
             (wt-tree/add (make-wt-tree number-wt-type) x x))))

  ;; This uses a very low bound, since using something big will
  ;; eat your RAM quickly.
  (test-generative ((ps (lambda ()
                          (remove-key-dups
                           (make-random-nat-alist 32)))))
    (test (length ps)
          (wt-tree/size (alist->wt-tree number-wt-type ps))))

  (let ((empty (make-wt-tree number-wt-type)))
    (test-generative ((x random-nat-elt)
                      (t (lambda () (make-random-wt-tree size-bound))))
      (test #t (wt-tree/member? x (wt-tree/add empty x x)))
      (test #f (wt-tree/member? x empty))
      (test #t (wt-tree/member? x (wt-tree/add t x x)))
      (test #f (wt-tree/member? x (wt-tree/delete t x)))))

  (let ((empty (make-wt-tree number-wt-type)))
    (test-generative ((x random-nat-elt)
                      (ps (lambda ()
                            (remove-key-dups
                             (make-random-nat-alist 32)))))
      (test x (wt-tree/lookup (wt-tree/add empty x x) x #f))
      (let ((tree (alist->wt-tree number-wt-type ps)))
        (test-assert "wt-tree/lookup all elements"
          (equal? (map cdr ps)
                  (map (lambda (p)
                         (wt-tree/lookup tree (car p) #f))
                       ps)))
        (test 'frob
              (wt-tree/lookup (wt-tree/delete tree x)
                              x
                              'frob)))))
  )

(test-group "Advanced wt-trees"
  (test-generative ((ps (lambda () (make-random-nat-alist size-bound)))
                    (x random-nat-elt))
    (let ((tree (alist->wt-tree number-wt-type ps)))
      (receive (in out) (partition (lambda (p) (< (car p) x)) ps)
        (let ((t (wt-tree/split< tree x)))
          (test "members of split< trees"
                #t
                (every (lambda (p) (wt-tree/member? (car p) t)) in))
          (test "non-members of split< trees"
                #f
                (any (lambda (p) (wt-tree/member? (car p) t)) out))))

      (receive (in out) (partition (lambda (p) (> (car p) x)) ps)
        (let ((t (wt-tree/split> tree x)))
          (test "members of split> trees"
                #t
                (every (lambda (p) (wt-tree/member? (car p) t)) in))
          (test "non-members of split> trees"
                #f
                (any (lambda (p) (wt-tree/member? (car p) t)) out))))))

  (test-generative ((ps1 (lambda () (make-random-nat-alist size-bound)))
                    (ps2 (lambda () (make-random-nat-alist size-bound))))
    (let ((tree1 (alist->wt-tree number-wt-type ps1))
          (tree2 (alist->wt-tree number-wt-type ps2)))
      ;; union
      (let ((tu (wt-tree/union tree1 tree2)))
        (test "members of union trees"
              #t
              (and (every (lambda (p) (wt-tree/member? (car p) tu)) ps1)
                   (every (lambda (p) (wt-tree/member? (car p) tu)) ps2))))
      ;; intersection
      (let ((ti (wt-tree/intersection tree1 tree2))
            (psi (lset-intersection (lambda (p q)
                                      (= (car p) (car q)))
                                    ps1
                                    ps2)))
        (test "members of intersection trees"
              #t
              (every (lambda (p) (wt-tree/member? (car p) ti)) psi)))
      ;; difference
      (let ((ti (wt-tree/difference tree1 tree2))
            (psd (lset-difference (lambda (p q)
                                    (= (car p) (car q)))
                                  ps1
                                  ps2)))
        (test "members of difference trees"
              #t
              (every (lambda (p) (wt-tree/member? (car p) ti)) psd)))))

  ;; subset? and set-equal?
  (let ((empty (make-wt-tree number-wt-type)))
    (test-generative ((ps1 (lambda ()
                             (make-random-nat-alist size-bound)))
                      (ps2-temp (lambda ()
                                  (make-random-nat-alist size-bound))))
      (let ((t1 (alist->wt-tree number-wt-type ps1))
            (t-sub (alist->wt-tree number-wt-type
                                   (take ps1 (quotient (length ps1) 2))))
            (t-disj (alist->wt-tree
                     number-wt-type
                     (remove (lambda (p) (assv (car p) ps1))
                         ps2-temp))))
        (test #t (wt-tree/subset? t1 t1))
        (test #t (wt-tree/subset? t-sub t1))
        (test #t (wt-tree/subset? empty t1))
        (test "wt-tree/subset? of disjoint trees"
              #t
              (or (= 0 (wt-tree/size t1) (wt-tree/size t-disj))
                  (not (wt-tree/subset? t1 t-disj))))
        (test #t (wt-tree/subset? t1 (wt-tree/union t1 t-disj)))
        (test #t (wt-tree/set-equal? t1 t1))
        (test #f (wt-tree/set-equal? t1 empty))
        (test "wt-tree/set-equal? of disjoint trees"
              #t
              (or (= 0 (wt-tree/size t1) (wt-tree/size t-disj))
                  (not (wt-tree/set-equal? t1 t-disj)))))))
  )

;; TODO: More tests for wt-tree/for-each.
(test-group "iteration"
  (test-generative ((ps (lambda ()
                          (remove-key-dups
                           (make-random-nat-alist size-bound)))))
    (let ((t (alist->wt-tree number-wt-type ps)))
      (test "wt-tree/fold empty"
            'z
            (wt-tree/fold append 'z (make-wt-tree number-wt-type)))
      ; bignums!
      (test "wt-tree/fold non-empty"
            #t
            (= (fold (lambda (p k) (+ (car p) (cdr p) k)) 0 ps)
               (wt-tree/fold + 0 t)))
      (test "wt-tree/fold alist conv"
            #t
            (lset= equal?
                   ps
                   (wt-tree/fold (lambda (k v qs) (cons (cons k v) qs))
                                 '()
                                 t)))
      (let ((n 0))
        (test "wt-tree/for-each counting"
              (wt-tree/size t)
              (begin
               (wt-tree/for-each (lambda (_k _v) (set! n (+ n 1))) t)
               n)))))
  )

(test-group "index and rank"
  (test-generative ((size random-nonzero-size))
    (let* ((ps (sort-num-alist
                (remove-key-dups
                 (make-random-nat-alist size))))
           (t (alist->wt-tree number-wt-type ps))
           (min-p (car ps)))
      (test "wt-tree/min" (car min-p) (wt-tree/min t))
      (test "wt-tree/min-datum" (cdr min-p) (wt-tree/min-datum t))
      (test "wt-tree/min-pair" min-p (wt-tree/min-pair t))

      (test "wt-tree/delete-min no min"
            #f
            (wt-tree/member? (car min-p) (wt-tree/delete-min t)))
      (test "wt-tree/delete-min rest"
            #t
            (every (lambda (p)
                     (or (= (car p) (car min-p)) ; min assoc deleted
                         (= (cdr p) (wt-tree/lookup t (car p) #f))))
                   ps))

      (test "wt-tree/index"
            'success
            (let loop ((i 0) (k (pseudo-random-integer size)))
              (cond ((> i embedded-trial-iterations) 'success)
                    ((= (car (list-ref ps k)) (wt-tree/index t k))
                     (loop (+ i 1) (pseudo-random-integer size)))
                    (else 'failure))))

      (test "wt-tree/index-datum"
            'success
            (let loop ((i 0) (k (pseudo-random-integer size)))
              (cond ((> i embedded-trial-iterations) 'success)
                    ((= (cdr (list-ref ps k)) (wt-tree/index-datum t k))
                     (loop (+ i 1) (pseudo-random-integer size)))
                    (else 'failure))))

      (test "wt-tree/index-pair"
            'success
            (let loop ((i 0) (k (pseudo-random-integer size)))
              (cond ((> i embedded-trial-iterations) 'success)
                    ((equal? (list-ref ps k) (wt-tree/index-pair t k))
                     (loop (+ i 1) (pseudo-random-integer size)))
                    (else 'failure))))

      (test "wt-tree/rank 1"
            'success
            (let loop ((i 0) (k (pseudo-random-integer size)))
              (cond ((> i embedded-trial-iterations) 'success)
                    ((= k (wt-tree/rank t (car (list-ref ps k))))
                     (loop (+ i 1) (pseudo-random-integer size)))
                    (else 'failure))))

      (test "wt-tree/rank 2" #f (wt-tree/rank t (* size 2)))
  )))

(test-group "destructive ops"
  (test "wt-tree/add!"
        23
        (let ((t (alist->wt-tree number-wt-type
                                 (make-random-nat-alist
                                  (random-nonzero-size)))))
          (wt-tree/add! t 10 23)
          (wt-tree/lookup t 10 #f)))

  (test "wt-tree/add! size delta"
        1
        (let* ((t (alist->wt-tree number-wt-type
                                  (make-random-nat-alist
                                   (random-nonzero-size))))
               (old-size (wt-tree/size t)))
          (wt-tree/add! t 10 23)
          (- (wt-tree/size t) old-size)))

  (test "wt-tree/delete! 1"
        #t
        (let ((t (alist->wt-tree number-wt-type '((10 . 23)))))
          (wt-tree/delete! t 10)
          (wt-tree/empty? t)))

  (test "wt-tree/delete! 2"
        #f
        (let ((t (alist->wt-tree number-wt-type
                                 (make-random-nat-alist
                                  (random-nonzero-size)))))
          (wt-tree/delete! t 10)
          (wt-tree/member? 10 t)))

  (test "wt-tree/delete! size delta"
        1
        (let* ((t-temp (alist->wt-tree number-wt-type
                                       (make-random-nat-alist
                                        (random-nonzero-size))))
               (t (wt-tree/add t-temp 10 23))
               (old-size (wt-tree/size t)))
          (wt-tree/delete! t 10)
          (- old-size (wt-tree/size t))))

  (test "wt-tree/delete-min! 1"
        #t
        (let ((t (alist->wt-tree number-wt-type '((10 . 23)))))
          (wt-tree/delete-min! t)
          (wt-tree/empty? t)))

  (test "wt-tree/delete-min! 2"
        #f
        (let* ((t (alist->wt-tree number-wt-type
                                  (make-random-nat-alist
                                   (random-nonzero-size))))
               (min-key (wt-tree/min t)))
          (wt-tree/delete-min! t)
          (wt-tree/member? min-key t)))

  (test "wt-tree/delete-min! size delta"
        1
        (let* ((t (alist->wt-tree number-wt-type
                                  (make-random-nat-alist
                                   (random-nonzero-size))))
               (old-size (wt-tree/size t)))
          (wt-tree/delete-min! t)
          (- old-size (wt-tree/size t))))
  )

(test-exit)
