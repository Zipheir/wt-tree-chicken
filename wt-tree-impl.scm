;;; "wttree.scm" Weight balanced trees			-*-Scheme-*-
;;;
;;; $ I d : wttree.scm,v 1.10 1999/01/02 06:19:10 cph Exp $
;;;
;;; Copyright (c) 1993-1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Copyright (c) 1993-1994 Stephen Adams
;;;
;;; References:
;;;
;;;   Stephen Adams, Implemeting Sets Efficiently in a Functional
;;;      Language, CSTR 92-10, Department of Electronics and Computer
;;;      Science, University of Southampton, 1992
;;;
;;;
;;; Packaged for R7RS Scheme by Peter Lane, 2017
;;; Ported to CHICKEN 5 Scheme by Wolfgang Corcoran-Mathe 2022
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Weight Balanced Binary Trees
;;
;;
;;
;;  This file has been modified from the MIT-Scheme library version to
;;  make it more standard. The main changes are
;;
;;   . The whole thing has been put in a LET as R4RS Scheme has no module
;;     system.
;;   . The MIT-Scheme define structure operations have been written out by
;;     hand.
;;
;;  It has been tested on MIT-Scheme, scheme48 and scm4e1
;;
;;  If your system has a compiler and you want this code to run fast, you
;;  should do whatever is necessary to inline all of the structure accessors.
;;
;;  This is MIT-Scheme's way of saying that +, car etc should all be inlined.
;;
;;(declare (usual-integrations))

;;; Utility

(: exact-natural? (* -> boolean))
(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

;;; Conditions and exceptions

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

;;;
;;; Interface to this package.
;;;
;;; ONLY these procedures (and TEST at the end of the file) will be
;;; (re)defined in your system.
;;;

(define fix:fixnum? fixnum?)
(define fix:+ fx+)
(define fix:- fx-)
(define fix:< fx<)
(define fix:<= fx<=)
(define fix:> fx>)
(define fix:* fx*)

;;  A TREE-TYPE is a collection of those procedures that depend on the
;;  ordering relation.

(define-record-type tree-type
  (%make-tree-type key<?       alist->tree
                   add         insert!
                   delete      delete!
                   member?     lookup
                   split-lt    split-gt
                   union       union-merge
                   intersection difference
                   subset?     rank        )
  wt-tree-type?
  (key<? tree-type/key<?)
  (alist->tree tree-type/alist->tree)
  (add tree-type/add)
  (insert! tree-type/insert!)
  (delete tree-type/delete)
  (delete! tree-type/delete!)
  (member? tree-type/member?)
  (lookup tree-type/lookup)
  (split-lt tree-type/split-lt)
  (split-gt tree-type/split-gt)
  (union tree-type/union)
  (union-merge tree-type/union-merge)
  (intersection tree-type/intersection)
  (difference tree-type/difference)
  (subset? tree-type/subset?)
  (rank tree-type/rank))

(define-type wt-tree-type (struct tree-type))

;;  User level tree representation.
;;
;;  WT-TREE is a wrapper for trees of nodes.

(define-type node (struct node))

(define-record-type wt-tree
  (%make-wt-tree type root)
  wt-tree?
  (type tree/type : wt-tree-type)
  (root tree/root set-tree/root! : (or symbol node)))

(define-type wt-tree (struct wt-tree))

;;  Nodes are the thing from which the real trees are built.  There are
;;  lots of these and the uninquisitive user will never see them, so
;;  they are represented as untagged to save the slot that would be
;;  used for tagging structures.
;;  In MIT-Scheme these were all DEFINE-INTEGRABLE

(define-record-type node
  (make-node k v l r w)
  node?
  (k node/k : *)
  (v node/v : *)
  (l node/l : (or symbol node))
  (r node/r : (or symbol node))
  (w node/w : integer))

(define empty  'empty)
(define (empty? x) (eq? x 'empty))

(define (node/size node)
  (if (empty? node) 0  (node/w node)))

(define (node/singleton k v) (make-node k v empty empty 1))

(define (with-n-node node receiver)
  (receiver (node/k node) (node/v node) (node/l node) (node/r node)))

;;
;;  Constructors for building node trees of various complexity
;;

(define (n-join k v l r)
  (make-node k v l r (fix:+ 1 (fix:+ (node/size l) (node/size r)))))

(define (single-l a_k a_v x r)
  (with-n-node r
               (lambda (b_k b_v y z) (n-join b_k b_v (n-join a_k a_v x y) z))))

(define (double-l a_k a_v x r)
  (with-n-node r
               (lambda (c_k c_v r_l z)
                 (with-n-node r_l
                              (lambda (b_k b_v y1 y2)
                                (n-join b_k b_v
                                        (n-join a_k a_v x y1)
                                        (n-join c_k c_v y2 z)))))))

(define (single-r b_k b_v l z)
  (with-n-node l
               (lambda (a_k a_v x y) (n-join a_k a_v x (n-join b_k b_v y z)))))

(define (double-r c_k c_v l z)
  (with-n-node l
               (lambda (a_k a_v x l_r)
                 (with-n-node l_r
                              (lambda (b_k b_v y1 y2)
                                (n-join b_k b_v
                                        (n-join a_k a_v x y1)
                                        (n-join c_k c_v y2 z)))))))

;; (define-integrable wt-tree-delta 3)
(define wt-tree-delta 3)
(define wt-tree-gamma 2)

(define (t-join k v l r)
  (define (simple-join) (n-join k v l r))
  (let ((l_n (fix:+ (node/size l) 1))
        (r_n (fix:+ (node/size r) 1)))
    (cond ((fix:> r_n (fix:* wt-tree-delta l_n))
           ;; right is too big
           (let ((r_l_n (fix:+ (node/size (node/l r)) 1))
                 (r_r_n (fix:+ (node/size (node/r r)) 1)))
             (if (fix:< r_l_n (fix:* wt-tree-gamma r_r_n))
               (single-l k v l r)
               (double-l k v l r))))
          ((fix:> l_n (fix:* wt-tree-delta r_n))
           ;; left is too big
           (let ((l_l_n (fix:+ (node/size (node/l l)) 1))
                 (l_r_n (fix:+ (node/size (node/r l)) 1)))
             (if (fix:< l_r_n (fix:* wt-tree-gamma l_l_n))
               (single-r k v l r)
               (double-r k v l r))))
          (else
            (simple-join)))))

;;
;;  Node tree procedures that are independent of key<?
;;

(define (node/min node)
  (cond  ((empty? node)          (error:empty 'min))
         ((empty? (node/l node)) node)
         (else                   (node/min (node/l node)))))

(define (node/delmin node)
  (cond ((empty? node)           (error:empty 'delmin))
        ((empty? (node/l node))  (node/r node))
        (else   (t-join (node/k node) (node/v node)
                        (node/delmin (node/l node)) (node/r node)))))

(define (node/concat2 node1 node2)
  (cond ((empty? node1)   node2)
        ((empty? node2)   node1)
        (else
          (let ((min-node (node/min node2)))
            (t-join (node/k min-node) (node/v min-node)
                    node1 (node/delmin node2))))))

(define (node/inorder-fold procedure base node)
  (define (fold base node)
    (if (empty? node)
      base
      (with-n-node node
                   (lambda (k v l r)
                     (fold (procedure k v (fold base r)) l)))))
  (fold base node))

(define (node/for-each procedure node)
  (if (not (empty? node))
    (with-n-node node
                 (lambda (k v l r)
                   (node/for-each procedure l)
                   (procedure k v)
                   (node/for-each procedure r)))))

(define (node/height node)
  (if (empty? node)
    0
    (+ 1 (max (node/height (node/l node))
              (node/height (node/r node))))))

(define (node/index node index)
  (define (loop node index)
    (let ((size_l  (node/size (node/l node))))
      (cond ((fix:< index size_l)  (loop (node/l node) index))
            ((fix:> index size_l)  (loop (node/r node)
                                         (fix:- index (fix:+ 1 size_l))))
            (else                  node))))
  (let ((bound  (node/size node)))
    (if (or (< index 0)
            (>= index bound)
            (not (fix:fixnum? index)))
      (abort
       (make-bounds-condition 'node/index
                              "argument out of bounds"
                              index))
      (loop node index))))

(define (error:empty owner)
  (error owner "Operation requires non-empty tree"))

(define (local:make-wt-tree-type key<?)

  (define (key>? x y)  (key<? y x))

  (define (node/find k node)
    (let search ((node node) (best #f))
      (cond ((empty? node)
             (if (or (not best)
                     (key<? (node/k best) k))
                 #f
                 best))
            ((key<? k (node/k node))
             (search (node/l node) best))
            (else
             (search (node/r node) node)))))

  (define (node/rank k node rank)
    (cond ((empty? node)             #f)
          ((key<? k (node/k node))  (node/rank k (node/l node) rank))
          ((key>? k (node/k node))
           (node/rank k (node/r node)
                      (fix:+ 1 (fix:+ rank (node/size (node/l node))))))
          (else (fix:+ rank (node/size (node/l node))))))

  (define (node/add node k v)
    (if (empty? node)
      (node/singleton k v)
      (with-n-node node
                   (lambda (key val l r)
                     (cond ((key<? k key)   (t-join key val (node/add l k v) r))
                           ((key<? key k)   (t-join key val l (node/add r k v)))
                           (else            (n-join key v   l r)))))))

  (define (node/delete x node)
    (if (empty? node)
      empty
      (with-n-node node
                   (lambda (key val l r)
                     (cond ((key<? x key)   (t-join key val (node/delete x l) r))
                           ((key<? key x)   (t-join key val l (node/delete x r)))
                           (else            (node/concat2 l r)))))))

  (define (node/concat tree1 tree2)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
            (let ((min-node (node/min tree2)))
              (node/concat3 (node/k min-node) (node/v min-node) tree1
                            (node/delmin tree2))))))

  (define (node/concat3 k v l r)
    (cond ((empty? l) (node/add r k v))
          ((empty? r) (node/add l k v))
          (else
            (let ((n1 (fix:+ (node/size l) 1))
                  (n2 (fix:+ (node/size r) 1)))
              (cond ((fix:< (fix:* wt-tree-delta n1) n2)
                     (with-n-node r
                                  (lambda (k2 v2 l2 r2)
                                    (t-join k2 v2 (node/concat3 k v l l2) r2))))
                    ((fix:< (fix:* wt-tree-delta n2) n1)
                     (with-n-node l
                                  (lambda (k1 v1 l1 r1)
                                    (t-join k1 v1 l1 (node/concat3 k v r1 r)))))
                    (else
                      (n-join k v l r)))))))

  (define (node/split-lt node x)
    (cond ((empty? node)  empty)
          ((key<? x (node/k node))
           (node/split-lt (node/l node) x))
          ((key<? (node/k node) x)
           (node/concat3 (node/k node) (node/v node) (node/l node)
                         (node/split-lt (node/r node) x)))
          (else (node/l node))))

  (define (node/split-gt node x)
    (cond ((empty? node)  empty)
          ((key<? (node/k node) x)
           (node/split-gt (node/r node) x))
          ((key<? x (node/k node))
           (node/concat3 (node/k node) (node/v node)
                         (node/split-gt (node/l node) x) (node/r node)))
          (else (node/r node))))

  (define (node/union tree1 tree2)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
            (with-n-node tree2
                         (lambda (ak av l r)
                           (let ((l1  (node/split-lt tree1 ak))
                                 (r1  (node/split-gt tree1 ak)))
                             (node/concat3 ak av (node/union l1 l) (node/union r1 r))))))))

  (define (node/union-merge tree1 tree2 merge)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
            (with-n-node tree2
                         (lambda (ak av l r)
                           (let* ((node1  (node/find ak tree1))
                                  (l1     (node/split-lt tree1 ak))
                                  (r1     (node/split-gt tree1 ak))
                                  (value  (if node1
                                            (merge ak av (node/v node1))
                                            av)))
                             (node/concat3 ak value
                                           (node/union-merge l1 l merge)
                                           (node/union-merge r1 r merge))))))))

  (define (node/difference tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   tree1)
          (else
            (with-n-node tree2
                         (lambda (ak av l r)
                           (let ((l1  (node/split-lt tree1 ak))
                                 (r1  (node/split-gt tree1 ak)))
                             av
                             (node/concat (node/difference l1 l)
                                          (node/difference r1 r))))))))

  (define (node/intersection tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   empty)
          (else
            (with-n-node tree2
                         (lambda (ak av l r)
                           (let ((l1  (node/split-lt tree1 ak))
                                 (r1  (node/split-gt tree1 ak)))
                             (if (node/find ak tree1)
                               (node/concat3 ak av (node/intersection l1 l)
                                             (node/intersection r1 r))
                               (node/concat (node/intersection l1 l)
                                            (node/intersection r1 r)))))))))

  (define (node/subset? tree1 tree2)
    (or (empty? tree1)
        (and (fix:<= (node/size tree1) (node/size tree2))
             (with-n-node tree1
                          (lambda (k v l r)
                            ; v is ignored
                            (cond ((key<? k (node/k tree2))
                                   (and (node/subset? l (node/l tree2))
                                        (node/find k tree2)
                                        (node/subset? r tree2)))
                                  ((key>? k (node/k tree2))
                                   (and (node/subset? r (node/r tree2))
                                        (node/find k tree2)
                                        (node/subset? l tree2)))
                                  (else
                                    (and (node/subset? l (node/l tree2))
                                         (node/subset? r (node/r tree2))))))))))


  ;;; Tree interface: stripping off or injecting the tree types

  (define (tree/map-add tree k v)
    (%make-wt-tree (tree/type tree)
                   (node/add (tree/root tree) k v)))

  (define (tree/insert! tree k v)
    (set-tree/root! tree (node/add (tree/root tree) k v)))

  (define (tree/delete tree k)
    (%make-wt-tree (tree/type tree)
                   (node/delete k (tree/root tree))))

  (define (tree/delete! tree k)
    (set-tree/root! tree (node/delete k (tree/root tree))))

  (define (tree/split-lt tree key)
    (%make-wt-tree (tree/type tree)
                   (node/split-lt (tree/root tree) key)))

  (define (tree/split-gt tree key)
    (%make-wt-tree (tree/type tree)
                   (node/split-gt (tree/root tree) key)))

  (define (tree/union tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/union (tree/root tree1) (tree/root tree2))))

  (define (tree/union-merge tree1 tree2 merge)
    (%make-wt-tree (tree/type tree1)
                   (node/union-merge (tree/root tree1) (tree/root tree2)
                                     merge)))

  (define (tree/intersection tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/intersection (tree/root tree1) (tree/root tree2))))

  (define (tree/difference tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/difference (tree/root tree1) (tree/root tree2))))

  (define (tree/subset? tree1 tree2)
    (node/subset? (tree/root tree1) (tree/root tree2)))

  (define (alist->tree alist)
    (define (loop alist node)
      (cond ((null? alist)  node)
            ((pair? alist)  (loop (cdr alist)
                                  (node/add node (caar alist) (cdar alist))))
            (else
              (abort
               (make-type-condition 'alist->tree
                                    "wrong type argument"
                                    alist)))))
    (%make-wt-tree my-type (loop alist empty)))

  (define (tree/get tree key default)
    (let ((node  (node/find key (tree/root tree))))
      (if node
        (node/v node)
        default)))

  (define (tree/rank tree key)  (node/rank key (tree/root tree) 0))

  (define (tree/member? key tree)
    (and (node/find key (tree/root tree))
         #t))

  (define my-type #F)

  (set! my-type
    (%make-tree-type
      key<?                        ;  key<?
      alist->tree                  ;  alist->tree
      tree/map-add                 ;  add
      tree/insert!                 ;  insert!
      tree/delete                  ;  delete
      tree/delete!                 ;  delete!
      tree/member?                 ;  member?
      tree/get                     ;  lookup
      tree/split-lt                ;  split-lt
      tree/split-gt                ;  split-gt
      tree/union                   ;  union
      tree/union-merge             ;  union-merge
      tree/intersection            ;  intersection
      tree/difference              ;  difference
      tree/subset?                 ;  subset?
      tree/rank                    ;  rank
      ))

  my-type)

;;; Type checking forms

(define-syntax guarantee-tree
  (syntax-rules ()
    ((guarantee-tree tree procedure)
     (assert-type procedure (wt-tree? tree) tree))))

(define-syntax guarantee-tree-type
  (syntax-rules ()
    ((guarantee-tree-type type procedure)
     (assert-type procedure (wt-tree-type? type) type))))

(define-syntax guarantee-compatible-trees
  (syntax-rules ()
    ((guarantee-compatible-trees tree1 tree2 procedure)
     (begin
      (guarantee-tree tree1 procedure)
      (guarantee-tree tree2 procedure)
      (unless (eq? (tree/type tree1) (tree/type tree2))
        (abort
         (make-type-condition procedure
                              "trees have incompatible types"
                              tree1
                              tree2
                              (tree/type tree1)
                              (tree/type tree2))))))))

(define (valid? tree)
  (let ((root (tree/root tree)))
    (and (balanced? root)
         (ordered? root))))

(define (balanced? n)
  (define (isBalanced a b)
    (let ((x (fix:+ (node/size a) 1))
          (y (fix:+ (node/size b) 1)))
      (fix:<= y (fix:* wt-tree-delta x))))
  (or (empty? n)
      (let ((l (node/l n))
            (r (node/r n)))
        (and (isBalanced l r) (isBalanced r l)
             (balanced? l) (balanced? r)))))

(define (ordered? n)
  (define (isOrdered lo hi m)
    (or (empty? m)
        (let ((k (node/k m))
              (l (node/l m))
              (r (node/r m)))
          (and (lo k) (hi k)
               (isOrdered lo (lambda (x) (< x k)) l)
               (isOrdered (lambda (x) (< k x)) hi r)))))
  (isOrdered (lambda (x) #t) (lambda (x) #t) n))

;;;______________________________________________________________________
;;;
;;;  Export interface
;;;
(: make-wt-tree-type (procedure -> wt-tree-type))
(define make-wt-tree-type local:make-wt-tree-type)

(: make-wt-tree (wt-tree-type -> wt-tree))
(define make-wt-tree
  (lambda (tree-type)
    (%make-wt-tree tree-type empty)))

(: singleton-wt-tree (wt-tree-type * * -> wt-tree))
(define singleton-wt-tree
  (lambda (type key value)
    (guarantee-tree-type type 'singleton-wt-tree)
    (%make-wt-tree type (node/singleton key value))))

(: alist-wt-tree (wt-tree-type list -> wt-tree))
(define alist->wt-tree
  (lambda (type alist)
    (guarantee-tree-type type 'alist->wt-tree)
    (assert-type 'alist->wt-tree
                 (or (pair? alist) (null? alist)))
    ((tree-type/alist->tree type) alist)))

(: wt-tree/empty? (wt-tree -> boolean))
(define wt-tree/empty?
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/empty?)
    (empty? (tree/root tree))))

(: wt-tree/size (wt-tree -> integer))
(define wt-tree/size
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/size)
    (node/size (tree/root tree))))

(: wt-tree/add (wt-tree * * -> wt-tree))
(define wt-tree/add
  (lambda (tree key datum)
    (guarantee-tree tree 'wt-tree/add)
    ((tree-type/add (tree/type tree)) tree key datum)))

(: wt-tree/delete (wt-tree * -> wt-tree))
(define wt-tree/delete
  (lambda (tree key)
    (guarantee-tree tree 'wt-tree/delete)
    ((tree-type/delete (tree/type tree)) tree key)))

(: wt-tree/add! (wt-tree * * -> undefined))
(define wt-tree/add!
  (lambda (tree key datum)
    (guarantee-tree tree 'wt-tree/add!)
    ((tree-type/insert! (tree/type tree)) tree key datum)))

(: wt-tree/delete! (wt-tree * -> undefined))
(define wt-tree/delete!
  (lambda (tree key)
    (guarantee-tree tree 'wt-tree/delete!)
    ((tree-type/delete! (tree/type tree)) tree key)))

(: wt-tree/member? (* wt-tree -> boolean))
(define wt-tree/member?
  (lambda (key tree)
    (guarantee-tree tree 'wt-tree/member?)
    ((tree-type/member? (tree/type tree)) key tree)))

(: wt-tree/lookup (wt-tree * * -> *))
(define wt-tree/lookup
  (lambda (tree key default)
    (guarantee-tree tree 'wt-tree/lookup)
    ((tree-type/lookup (tree/type tree)) tree key default)))

(: wt-tree/split< (wt-tree * -> wt-tree))
(define wt-tree/split<
  (lambda (tree key)
    (guarantee-tree tree 'wt-tree/split<)
    ((tree-type/split-lt (tree/type tree)) tree key)))

(: wt-tree/split> (wt-tree * -> wt-tree))
(define wt-tree/split>
  (lambda (tree key)
    (guarantee-tree tree 'wt-tree/split>)
    ((tree-type/split-gt (tree/type tree)) tree key)))

(: wt-tree/union (wt-tree wt-tree -> wt-tree))
(define wt-tree/union
  (lambda (tree1 tree2)
    (guarantee-compatible-trees tree1 tree2 'wt-tree/union)
    ((tree-type/union (tree/type tree1)) tree1 tree2)))

(: wt-tree/union-merge (wt-tree wt-tree (* * * -> *) -> wt-tree))
(define wt-tree/union-merge
  (lambda (tree1 tree2 merge)
    (guarantee-compatible-trees tree1 tree2 'wt-tree/union-merge)
    ((tree-type/union-merge (tree/type tree1)) tree1 tree2 merge)))

(: wt-tree/intersection (wt-tree wt-tree -> wt-tree))
(define wt-tree/intersection
  (lambda (tree1 tree2)
    (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)
    ((tree-type/intersection (tree/type tree1)) tree1 tree2)))

(: wt-tree/difference (wt-tree wt-tree -> wt-tree))
(define wt-tree/difference
  (lambda (tree1 tree2)
    (guarantee-compatible-trees tree1 tree2 'wt-tree/difference)
    ((tree-type/difference (tree/type tree1)) tree1 tree2)))

(: wt-tree/subset? (wt-tree wt-tree -> boolean))
(define wt-tree/subset?
  (lambda (tree1 tree2)
    (guarantee-compatible-trees tree1 tree2 'wt-tree/subset?)
    ((tree-type/subset? (tree/type tree1)) tree1 tree2)))

(: wt-tree/set-equal? (wt-tree wt-tree -> boolean))
(define wt-tree/set-equal?
  (lambda (tree1 tree2)
    (and (wt-tree/subset? tree1 tree2)
         (wt-tree/subset? tree2 tree1))))

(: wt-tree/fold ((* * * -> *) * wt-tree -> *))
(define wt-tree/fold
  (lambda (combiner-key-datum-result init tree)
    (guarantee-tree tree 'wt-tree/fold)
    (assert-type 'wt-tree/fold (procedure? combiner-key-datum-result))
    (node/inorder-fold combiner-key-datum-result
                       init
                       (tree/root tree))))

(: wt-tree/for-each ((* * -> *) wt-tree -> undefined))
(define wt-tree/for-each
  (lambda (action-key-datum tree)
    (guarantee-tree tree 'wt-tree/for-each)
    (assert-type 'wt-tree/for-each (procedure? action-key-datum))
    (node/for-each action-key-datum (tree/root tree))))

(: wt-tree/index (wt-tree integer -> *))
(define wt-tree/index
  (lambda (tree index)
    (guarantee-tree tree 'wt-tree/index)
    (assert-type 'wt-tree/index (exact-natural? index))
    (let ((node  (node/index (tree/root tree) index)))
      (and node (node/k node)))))

(: wt-tree/index-datum (wt-tree integer -> *))
(define wt-tree/index-datum
  (lambda (tree index)
    (guarantee-tree tree 'wt-tree/index-datum)
    (assert-type 'wt-tree/index-datum (exact-natural? index))
    (let ((node  (node/index (tree/root tree) index)))
      (and node (node/v node)))))

(: wt-tree/index-pair (wt-tree integer -> pair))
(define wt-tree/index-pair
  (lambda (tree index)
    (guarantee-tree tree 'wt-tree/index-pair)
    (assert-type 'wt-tree/index-pair (exact-natural? index))
    (let ((node  (node/index (tree/root tree) index)))
      (and node (cons (node/k node) (node/v node))))))

(: wt-tree/rank (wt-tree * -> (or integer false)))
(define wt-tree/rank
  (lambda (tree key)
    (guarantee-tree tree 'wt-tree/rank)
    ((tree-type/rank (tree/type tree)) tree key)))

(: wt-tree/min (wt-tree -> *))
(define wt-tree/min
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/min)
    (node/k (node/min (tree/root tree)))))

(: wt-tree/min-datum (wt-tree -> *))
(define wt-tree/min-datum
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/min-datum)
    (node/v (node/min (tree/root tree)))))

(: wt-tree/min (wt-tree -> pair))
(define wt-tree/min-pair
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/min-pair)
    (let ((node  (node/min (tree/root tree))))
      (cons (node/k node) (node/v node)))))

(: wt-tree/delete-min (wt-tree -> wt-tree))
(define wt-tree/delete-min
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/delete-min)
    (%make-wt-tree (tree/type tree)
                   (node/delmin (tree/root tree)))))

(: wt-tree/delete-min! (wt-tree -> undefined))
(define wt-tree/delete-min!
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/delete-min!)
    (set-tree/root! tree (node/delmin (tree/root tree)))))

;; < is a lexpr. Many compilers can open-code < so the lambda is faster
;; than passing <.
(: number-wt-type wt-tree-type)
(define number-wt-type (local:make-wt-tree-type  (lambda (u v) (< u v))))
(: string-wt-type wt-tree-type)
(define string-wt-type (local:make-wt-tree-type  string<?))

(define wt-tree/valid?
  (lambda (tree)
    (guarantee-tree tree 'wt-tree/valid?)
    (valid? tree)))
