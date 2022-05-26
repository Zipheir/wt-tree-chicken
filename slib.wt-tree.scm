(module slib-wt-tree
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
          (chicken fixnum)
          (chicken type)
          typed-records
          )

  (include "wt-tree-impl.scm")
  )
