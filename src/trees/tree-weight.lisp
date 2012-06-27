
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

(in-package :funds)

(defgeneric tree-weight (tree)
  (:documentation
"The number of nodes in the given tree."))

(defmethod tree-weight ((tree leaf))
  0)

(defmethod tree-weight ((tree binary-tree))
  (+ 1 (tree-weight (bt-left tree))
     (tree-weight (bt-right tree))))

