(import (chicken random)
        (srfi 1)
        test
        test-generative
        (slib wt-tree))

(define elt-bound (expt 10 12))

(define size-bound 256)

(define (random-nat-elt)
  (pseudo-random-integer elt-bound))

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

(test-group "Basic wt-trees"
  (test-assert (wt-tree/empty? (make-wt-tree number-wt-type)))

  (test-generative ((tree (lambda () (make-random-wt-tree 10))))
    (test-assert (not (wt-tree/empty? tree))))

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
                       ps))))))
  )

(test-exit)