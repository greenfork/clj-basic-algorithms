(ns clj-basic-algorithms.binary-search-tree.core)

;;; Tree structure
;;;
;;; The tree is a plain list. Every node is a list of 2 elements. Elements can
;;; consist of any nested structures.
;;;
;;; The tree is structured in a way that the first element represents the
;;; value at that node, the second element is the left child and the third
;;; element is the right child. This keeps the tree structure as a plain data
;;; which is very common in a functional programming paradigm.

;; bstl-int -- tree implementation with integers as leaf nodes and singly
;; linked list as a main data structure
(def bstl-int '(4 ((2 (1 3)) 5)))

;; bstl-arr -- tree implementation with lists with key and nils as leaf nodes
;; and singly linked list as a main data structure
(def bstl-arr '(4 (2 (1 nil nil) (3 nil nil)) (5 nil nil)))

;; bstv-int -- tree implementation with integers as leaf nodes and vector as a
;; main data structure
(def bstv-int [4 [[2 [1 3]] 5]])

;; bstv-arr -- tree implementation with vectors with key and nils as leaf
;; nodes and vector as a main data structure
(def bstv-arr [4 [2 [1 nil nil] [3 nil nil]] [5 nil nil]])

;; bst-rec -- tree implementation using records aka Java classes; it is a
;; deviation from the plain list structure
(defrecord BST [key left right])
(def bst-rec (BST. 4
                    (BST. 2
                           (BST. 1 nil nil)
                           (BST. 3 nil nil))
                    (BST. 5 nil nil)))

;;;
;;;     4
;;;    / \
;;;   2   5
;;;  / \
;;; 1   3
;;;
