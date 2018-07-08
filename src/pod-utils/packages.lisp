
(in-package :cl-user)

;;; POD FIX THIS! Must re-evaluate ~/projects/moss/source/utils/project-utils.lisp
;;; after reloading this file!

(defpackage pod-utils
  (:nicknames :pod)
  (:use :cl)
  ;(:shadowing-import-from #:str)
  (:export 
   ;; from utils.lisp
   #:*null-stream* 
   #:aand
   #:aif
   #:app-redirect
   #:awhen
   #:awhile
   #:base-namespace
   #:basic-ascii-string
   #:basic-ascii-string-file
   #:before 
   #:breadth-first-search 
   #:break-line-at 
   #:c-name2lisp 
   #:chop 
   #:clear-memoize 
   #:combinations 
;   #:copy-file 
   #:date-to-utime
   #:dash-to-camel
   #:dbind 
   #:debug-memo 
   #:declare-ignore 
   #:decode-time-interval 
   #:define-constant 
   #:definition
   #:defmemo 
   #:defmemo!
   #:defmemo-equal 
   #:defun-memoize 
   #:depth-first-search 
   #:depth-search-tracking
   #:duplicate 
   #:elip
   #:equiv-classes
   #:fail 
   #:file-size 
   #:find-non-ascii
   #:find2 
   #:flatten 
   #:format-to-size
   #:found-at-pos
   #:gather-duplicates
   #:gethash-inv
   #:group 
   #:ht2list 
   #:if-bind 
   #:intersect-predicates 
   #:it
   #:kintern 
   #:last1 
   #:lisp-name2c 
   #:load-ht 
   #:longer 
   #:lpath
   #:*lpath-ht*
   #:lpath-init
   #:mac 
   #:mac2 
   #:macroexpand-all 
   #:map-in 
   #:mapappend 
   #:mapnconc 
   #:memo 
   #:memoize 
   #:mklist 
   #:mvb 
   #:mvs 
   #:name2initials 
   #:new-reslist 
   #:new-uuid
   #:now 
   #:ordinal
   #:pairs 
   #:pprint-symbols 
   #:pprint-without-strings 
   #:prepend 
   #:prune 
   #:pushnew-last
   #:push-last
   #:read-string-to-list 
   #:reinit-singleton 
   #:remove-extra-spaces 
   #:reslist-arr 
   #:reslist-fillptr 
   #:reslist-pop 
   #:reslist-push 
   #:reuse-cons 
   #:session-vo-class
   #:setx 
   #:set-p
   #:set-search-path
   #:shadow-for-model
   #:sidebox-menu
   #:single-p 
   #:singleton 
   #:sintern 
   #:split 
   #:split-if 
   #:strcat 
   #:strcat* 
   #:strcat+
   #:string-integer-p 
   #:substitute-string 
   #:substring 
   #:system-add-memoized-fn 
   #:system-clear-memoized-fns 
   #:system-forget-memoized-fns 
   #:system-list-memoized-fns 
   #:the-instance
   #:tree-search 
   #:tree-search-path 
   #:uappend
   #:ulist
   #:unique
   #:update 
   #:usr-bin-diff
   #:usr-bin-file 
   #:usr-bin-xmllint
   #:utils-parse-error
   #:utime-to-date
   #:vars 
   #:vector-count-if 
   #:when-bind 
   #:when-bind* 
   #:with-gensyms 
   #:with-package-renamed
   #:with-stack-size
   ;; from debugging.lisp
   #:*debugging* 
   #:*dbg-tags* 
   #:*debug-stream* 
   #:clear-debugging 
   #:dbg-funcall 
   #:dbg-message dbg-pprint
   #:full-debugging 
   #:get-debugging 
   #:if-debugging 
   #:show-debugging 
   #:set-debugging
   #:when-debugging 
   #:with-debugging))

(defpackage :user-system
  (:use :cl :asdf :pod-utils))

