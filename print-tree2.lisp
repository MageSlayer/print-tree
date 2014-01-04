;;(declaim (optimize (debug 3)))

;;=================================params=================================================
(defparameter *spacing* 2)		   ; number of symbols between graphical objects
(defparameter *subnodes-text* "subnode")   ; text to show on sub-nodes
;;=================================params=================================================

(defun list? (x)
  (cond ((listp x) t)
	((numberp x) nil)
	((stringp x) nil)
	(t (assert nil nil "Objects other than numbers, strings and lists are not supported. Sorry..."))
	))

(defun sum(x)
  (reduce #'+ x :initial-value 0)
  )

(defun tostr (x)
  (princ-to-string x))

(defun objtxt-width (x)
  (length (tostr x)))

(defun level-flatten (node)
  (mapcar #'(lambda(x)
	      (if (list? x)
		  (car x)
		  x))
	  node)
  )

(defun level-width-with-spacing (level)
  (let ((spaces (* (- (length level) 1) *spacing*)))
    (+ spaces (sum level))
    ))

(defun node-describe (node)
  (let* ((vals (level-flatten node))
	 (val-widths (mapcar #'objtxt-width vals))
	 (widths
	  (mapcar #'(lambda(x)
		      (if (list? x)
			  (max (objtxt-width (car x)) (node-describe (cdr x))) ;; sub-node width
			  (objtxt-width x)))
		  node))
	 )

    (values (level-width-with-spacing widths) vals val-widths widths)
    ))

(defstruct treeitem
  level	     ; item level in tree
  val	     ; text value
  val-width  ; value width in symbols
  width	     ; item width (without spacing)

  parent     ; parent treeitem
  pos        ; actual print position after printing
  relpos     ; print position relative to parent center
  total-width ; total tree width
  )

(defun tree-transform (node level parent)
  ;; transforms hier-node into flat tree items

  ;; process whole tree in depth and left-to-right mode
  ;; it is done to sort result and be ready for level-by-level printing.

  (let ((result '())
	(curlevel '())
	(relx 0)
	(first-level-scalar (and (= level 0)
				 (not (list? (car node)))))
	)

    (multiple-value-bind (total-width vals val-widths widths) (node-describe node)
      ;; process this level

      ;; special processing on first node. It will become a single root.
      (if first-level-scalar
	  (progn
	    ;; position root node at the center
	    (setq relx (div2 total-width))

	    ;; leave only one value in lists
	     (setq vals (list (car vals)))
	     (setq val-widths (list (car val-widths)))
	     (setq widths (list (car widths)))
	    ))

      ;; store current sub-level (level-branch) into results
      (setq curlevel
	    (mapcar #'(lambda(val val-width width)
			(let* ((offset-to-center (- (div2 width) (div2 val-width)))
			       (item
				(make-treeitem :level level
					       :val (tostr val)
					       :val-width val-width
					       :width width
					       :pos nil
					       :total-width total-width
					       :relpos (+ offset-to-center (- relx (div2 total-width)))
					       :parent parent)))

			  (setq relx (+ relx width *spacing*))
			  item)
			)
		    vals val-widths widths))

      (setq result
	    (append result curlevel))

      ;; append next level nodes
      (mapcar #'(lambda(x parent)
		  (if (list? x)
		      (nconc result (tree-transform (cdr x) (+ level 1) parent))))
	      node curlevel)
      )

    ;; add rest elements on level 0 as children
    (if first-level-scalar
    	(setq result
    	      (append result (tree-transform (cdr node) (+ level 1) (car curlevel)))))

    result
    )
  )

(defun padding (c)
  (make-string c :initial-element #\Space)
  )

(defun print-into-x (buf x txt)
  (assert (< (length buf) (+ x 1)) nil "Invalid operation" )
  (concatenate 'string buf (padding (- (+ x 1) (length buf))) txt)
  )

(defun max-level-find (levels)
  (loop for e in levels
     maximize (treeitem-level e))
  )

(defun level-collect (levels level)
  ;; collects all nodes on a given level

  (loop for curnode in levels
     when (= (treeitem-level curnode) level)
     collect curnode))

(defun print-tree (tree)
  (let* ((levels (tree-transform tree 0 nil))
	 (max-level (max-level-find levels))
	 (level-txt "")
	 (level-edges "")
	 (level nil)
	 )

    ;; iterate over tree level starting from top
    (loop for curlevel from 0 to max-level
       do
	 (setq level-txt "")
	 (setq level-edges "")
	 (setq level (level-collect levels curlevel))

	 ;; accumulate current level nodes/edges
	 (loop for curnode in level
	    do
	      (let* ((relpos (treeitem-relpos curnode))
		     (val (treeitem-val curnode))
		     (total-width (treeitem-total-width curnode))
		     (parent (treeitem-parent curnode))

		     ;; parent left coordinate
		     (parent-pos (if (not parent) (div2 total-width) (treeitem-pos parent)))
		     ;; parent width of its value text only
		     (parent-val-width (if (not parent) 0 (treeitem-val-width parent)))

		     (parent-center (+ parent-pos (div2 parent-val-width)))
		     (curpos (+ parent-center relpos))
		     )

		;; print nodes into current line
		(setq level-txt
		      (print-into-x level-txt curpos val))

		;; if top level, print node edges into current line
		(if parent
		    (setq level-edges
			  (print-into-x level-edges curpos (if (= curpos parent-center) "|"
							       (if (< curpos parent-center) "/" "\\")))))

		(setf (treeitem-pos curnode) curpos)                          ;; save node position in case if it is a parent
		)
	      )

       ;; draw edges if present
	 (if (> (length level-edges) 0)
	     (write-line level-edges))

       ;; draw nodes
	 (write-line level-txt)
	 )
    ))


;; ================text cases=================================================
(print-tree '("A" ("BC" "D") "EFG"))
(print-tree '(("A" "AA") ("BC" "D") "EFG"))
(print-tree (list 1 2 3 (list 20 30 (list 10 20 30) (list 10 20 30)) "234" "Привет"))

;;             1
;; 2   3      20       234   Привет
;;         /  |   \
;;       30  10    10
;;          / \    /  \
;;         20 30   20 30

;; TODO
;;-make macro nappend instead of nconc+append

;; ================text cases=================================================
