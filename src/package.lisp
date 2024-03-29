
;;;;
;;;; Copyright 2007 Andrew Baine 
;;;; 
;;;; Licensed under the Apache License, Version 2.0 (the "License"); 
;;;; you may not use this file except in compliance with the License. 
;;;; You may obtain a copy of the License at 
;;;; 
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;; 
;;;; Unless required by applicable law or agreed to in writing, software 
;;;; distributed under the License is distributed on an "AS IS" BASIS, 
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
;;;; See the License for the specific language governing permissions and 
;;;; limitations under the License.
;;;; 

(in-package :cl-user)

(defpackage :funds
  (:use :cl)
  (:export :make-avl-tree
	   :make-binary-tree

	   :bt-key
	   :bt-value

	   :tree-insert
	   :tree-remove
	   :tree-find
	   :tree-empty-p
	   :tree-height
	   :tree-weight
	   :tree-as-alist
	   :tree-count
	   :tree-count-if
	   :map-tree

	   :make-heap
	   :heap-empty-p
	   :heap-first
	   :heap-insert
	   :heap-remove

	   :make-queue
	   :queue-empty-p
	   :enqueue
	   :dequeue
	   :queue-first
	   :queue-size
	   :queue-as-list
	   :queue-count
	   :queue-count-if
	   :map-queue

	   :make-stack
	   :stack-empty-p
	   :stack-push
	   :stack-top
	   :stack-size
	   :stack-from-list
	   :stack-as-list
	   :stack-count
	   :stack-count-if
	   :map-stack

	   :make-dictionary
	   :dictionary-add
	   :dictionary-remove
	   :dictionary-lookup
	   :dictionary-as-alist
	   :dictionary-from-alist

	   :make-f-array
	   :f-array-elt
	   :f-array-replace
	   :f-array-size
	   :f-array-as-list
	   :f-array-count
	   :f-array-count-if
	   :map-f-array))
