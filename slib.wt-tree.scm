(module (slib wt-tree)
  (make-wt-tree-type
   wt-tree-type?
   number-wt-type
   string-wt-type
   make-wt-tree
   singleton-wt-tree
   alist->wt-tree
   wt-tree?
   wt-tree/empty?
   wt-tree/size
   wt-tree/add
   wt-tree/delete
   wt-tree/add!
   wt-tree/delete!
   wt-tree/member?
   wt-tree/lookup
   wt-tree/split<
   wt-tree/split>
   wt-tree/union
   wt-tree/union-merge
   wt-tree/intersection
   wt-tree/difference
   wt-tree/subset?
   wt-tree/set-equal?
   wt-tree/fold
   wt-tree/for-each
   wt-tree/index
   wt-tree/index-datum
   wt-tree/index-pair
   wt-tree/rank
   wt-tree/min
   wt-tree/min-datum
   wt-tree/min-pair
   wt-tree/delete-min
   wt-tree/delete-min!
   )

  (import scheme
          (chicken base)
          (chicken condition)
          (chicken fixnum)
          (chicken type)
          typed-records
          )

  (define (make-type-condition loc msg . args)
    (make-composite-condition
     (make-property-condition 'exn
      'location loc
      'message msg
      'arguments args)
     (make-property-condition 'type)))

  (define (make-bounds-condition loc msg . args)
    (make-composite-condition
     (make-property-condition 'exn
      'location loc
      'message msg
      'arguments args)
     (make-property-condition 'bounds)))

  (define-syntax assert-type
    (syntax-rules ()
      ((assert-type loc expr . args)
       (unless expr
         (abort
          (make-type-condition loc
                               "assertion violation: type check failed"
                               'expr
                               . args))))))

  (include "wt-tree-impl.scm")
  )
