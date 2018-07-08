
(in-package :cl-user)

(defpackage TR
  (:nicknames :trie)
  (:use cl pod-utils)
  (:export #:clear-all-trie-dbs
	   #:clear-tries
	   #:db-predicates
	   #:ensure-trie-db
	   #:find-db
	   #:lt-query
	   #:qvar-versions
	   #:show-db
	   #:subst-bindings 
	   #:trie-add 
	   #:trie-query 
	   #:trie-query-all 
	   #:trie-query-next
	   #:trie-remove 
	   #:with-literal 
	   #:with-trie-db
	   #:write-db))





