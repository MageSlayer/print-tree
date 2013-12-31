;;=================================types=================================================
(defstruct point2d
  x y
  )

(defstruct rect2d
  lefttop rightbottom
  )

(defstruct line2d
  start finish
  )

(defstruct textbox
  rect
  text
  )

(defstruct text-and-dims
  width height
  text
  )
;;=================================types==================================================

;;=================================params=================================================
(defparameter *spacing* 2)		   ; number of symbols between graphical objects
(defparameter *subnodes-text* "subnode")   ; text to show on sub-nodes
;;=================================params=================================================


;;=================================utility functions======================================
(defun make-rect2d-empty ()
  (make-rect2d :lefttop nil :rightbottom nil)
  )

(defun rect2d-left (r)
  (let ((lt (rect2d-lefttop r)))
    (and lt (point2d-x lt)))
  )

(defun rect2d-top (r)
  (let ((lt (rect2d-lefttop r)))
    (and lt (point2d-y lt)))
  )

(defun rect2d-right (r)
  (let ((rb (rect2d-rightbottom r)))
    (and rb (point2d-x rb)))
  )

(defun rect2d-bottom (r)
  (let ((rb (rect2d-rightbottom r)))
    (and rb (point2d-y rb)))
  )

(defun rect2d-width (r)
  (+ 1 (- (rect2d-right r) (rect2d-left r)))
  )

(defun rect2d-height (r)
  (+ 1 (- (rect2d-bottom r) (rect2d-top r)))
  )

(defun line2d-coords (l)
  (values (point2d-x (line2d-start l))
	  (point2d-y (line2d-start l))
	  (point2d-x (line2d-finish l))
	  (point2d-y (line2d-finish l))
	  )
  )

(defun line (x1 y1 x2 y2)
  (make-line2d :start  (make-point2d :x x1 :y y1)
	       :finish (make-point2d :x x2 :y y2))
  )

(defun bounds (left top right bottom)
  (make-rect2d :lefttop (make-point2d :x left :y top)
	       :rightbottom (make-point2d :x right :y bottom)
	       )
  )

(defun bounds-points (r)
  ;; returns 4 rect points
  (list (rect2d-lefttop r)
	(make-point2d :x (rect2d-right r) :y (rect2d-top r))
	(rect2d-rightbottom r)
	(make-point2d :x (rect2d-left r) :y (rect2d-bottom r))
	)
  )

(defun bounds-dims (left top width height)
  (bounds left top (+ left (- width 1)) (+ top (- height 1)))
  )

(defun min-ex (x1 x2)
  ;; an ordinary min function
  ;; except it allows x1 to be nil, in this case result = x2
  (if (not x1)
      x2
      (min x1 x2))
  )

(defun max-ex (x1 x2)
  ;; an ordinary max function
  ;; except it allows x1 to be nil, in this case result = x2
  (if (not x1)
      x2
      (max x1 x2))
  )

(defun div2 (x)
  (truncate (/ x 2))
  )

(defun box-expand-point (r p)
  ;; builds bounding box for r (rect2d) and p (point2d)

  (let ((rx1 (rect2d-left r))
	(ry1 (rect2d-top r))
	(rx2 (rect2d-right r))
	(ry2 (rect2d-bottom r))
	(px (point2d-x p))
	(py (point2d-y p))
	)
    (bounds (min-ex rx1 px)
	    (min-ex ry1 py)
	    (max-ex rx2 px)
	    (max-ex ry2 py)))
  )

(defun box-expand (r1 r2)
  ;; builds bounding box for r1 and r2 rects
  (let ((r2points (bounds-points r2)))
    (reduce #'box-expand-point r2points :initial-value r1 ))
  )

;;================================Generics=====================================
(defgeneric move (obj dx dy))       ;; generic "move object" function
(defgeneric render (obj s))	    ;; generic "render object on surface" function
(defgeneric bounding-box (obj))	    ;; generic "determine bounding box" function
;;================================Generics=====================================

;;=============================================================================
;; Coordinates
;;  ---> X
;; |
;; |
;; V Y
;;=============================================================================

(defun textbox-dims (text)
  ;; calculates text box dimensions
  ;; [1] - width
  ;; [2] - height

  ;; calculates text rendered as following
  ;;
  ;; |----------|
  ;; |text123456|
  ;; |----------|

  (let
      ((width (+ 2 (length text)))
       (height 3)			;; let's hardcore it for now.
       )
    (values width height))
  )

(defun make-textbox-dims (text)
  (multiple-value-bind (w h) (textbox-dims text)
    (make-text-and-dims
     :width w
     :height h
     :text text))
  )

(defun textbox-bounds (left top text)
  (multiple-value-bind (w h) (textbox-dims text)
    (bounds-dims left top w h)))

(defmethod move ((box textbox) dx dy)
  (let ((r (textbox-rect box)))
    (make-textbox
     :text (textbox-text box)
     :rect (bounds
	    (+ (rect2d-left r) dx)
	    (+ (rect2d-top r) dy)
	    (+ (rect2d-right r) dx)
	    (+ (rect2d-bottom r) dy)
	    )))
  )

(defmethod move ((l line2d) dx dy)
  (multiple-value-bind (x1 y1 x2 y2) (line2d-coords l)
    (line (+ x1 dx)
	  (+ y1 dy)
	  (+ x2 dx)
	  (+ y2 dy)
	)))

(defmethod bounding-box ((obj textbox))
  (textbox-rect obj)
  )

(defmethod bounding-box ((obj line2d))
  (make-rect2d :lefttop (line2d-start obj)
	       :rightbottom (line2d-finish obj))
  )
;;=================================utility functions======================================

;;=================================graphics functions=====================================
(defun surface-create (width height)
  ;; creates a surface of size width x height to render objects on
  ;; a surface, in fact, is just an array of strings (rectangular shape).
  (let ((s (make-array height)))
    (loop for y from 0 to (- height 1)
	 do
	 (setf (aref s y) (make-string width :initial-element #\Space))
	 )
    s)
  )

(defun surface-put! (s x y c)
  ;; puts a symbol c into position (x;y) on surface s
  ;; side-effects!!!

  (setf (char (aref s y) x) c)
  )

(defun surface-render-horz-line! (s x1 x2 y)
  ;; renders a horizontal line
  ;; can draw both from left to right and from right to left
  (loop for x from x1 to x2
     do
       (surface-put! s x y #\─))
  (loop for x from x1 downto x2
     do
       (surface-put! s x y #\─))
  )

(defun surface-render-vert-line! (s x y1 y2)
  ;; renders a vertical line
  ;; can draw both from top to bottom and from bottom to top
  (loop for y from y1 to y2
     do
       (surface-put! s x y #\│))
  (loop for y from y1 downto y2
     do
       (surface-put! s x y #\│))
  )

(defun surface-render-box! (s r)
  ;; renders box (rect2d) on surface s

  (surface-render-horz-line! s (rect2d-left r) (rect2d-right r) (rect2d-top r))
  (surface-render-horz-line! s (rect2d-left r) (rect2d-right r) (rect2d-bottom r))
  (surface-render-vert-line! s (rect2d-left r) (rect2d-top r) (rect2d-bottom r))
  (surface-render-vert-line! s (rect2d-right r) (rect2d-top r) (rect2d-bottom r))

  ;; some fancy corners :)
  (surface-put! s (rect2d-left r) (rect2d-top r) #\┌)
  (surface-put! s (rect2d-right r) (rect2d-top r) #\┐)
  (surface-put! s (rect2d-left r) (rect2d-bottom r) #\└)
  (surface-put! s (rect2d-right r) (rect2d-bottom r) #\┘)
  )

(defun surface-render-line! (s l)
  ;; renders line l (line2d) on surface s
  ;; only vertical or horizontal lines are expected

  (multiple-value-bind (x1 y1 x2 y2) (line2d-coords l)
    (if (= x1 x2)
	;; vertical line
	(surface-render-vert-line! s x1 y1 y2)
	(if (= y1 y2)
	    ;; horizontal line
	    (surface-render-horz-line! s x1 x2 y1)
	    (assert nil)))
    ))

(defun surface-render-text! (s x y text)
  (loop for c across text
     do
       (progn
	 (surface-put! s x y c)
	 (incf x)
	 ))
  )

(defun surface-render-textbox! (s tb)
  (let ((r (textbox-rect tb)))
    (surface-render-box! s r)
    (surface-render-text! s (+ (rect2d-left r) 1) (+ (rect2d-top r) 1) (textbox-text tb))
    )
  )

(defun surface-print (s)
  ;; prints surface to stdout
  (loop for line across s
     do
       (write-line line))
  )
;;=================================graphics functions=====================================

(defmethod render ((obj textbox) s)
  (surface-render-textbox! s obj)
  )

(defmethod render ((obj line2d) s)
  (surface-render-line! s obj)
  )

(defun box-tree-collect (tree)
  ;; creates a tree of text boxes to render
  (mapcar #'(lambda (x)
	      (if (atom x)
		  (make-textbox-dims (princ-to-string x))
		  (box-tree-collect x)
		  ))
	  tree)
  )

(defun make-subnodes-textbox-dims ()
  (make-textbox-dims *subnodes-text*)
  )

(defun make-subnode-box (x y)
  (let ((tb (make-subnodes-textbox-dims)))
    (to-textbox x y tb))
  )

(defun node-dims (node)
  ;; calculates horizontal node total width and height

  (let ((w 0)
	(h 0)
	(curw nil)
	(dx nil)
	(offsets '())			;; accumulates x offsets of boxes/edges.
	(half-real-widths '())		;; half of real width of each box.
	(half-widths '())		;; accumulates half-widths of each box.
	)
    (mapcar #'(lambda (x)
		(let ((box
		       (if (listp x)
			   (make-subnodes-textbox-dims) ;; special placeholder for sub-nodes
			   x)))

		  (setq curw (text-and-dims-width box)) ;; box width.
		  (setq h (max h (text-and-dims-height box))) ;; box height.
		  )

		;;calculate half-width of node itself
		(setq half-real-widths (cons (div2 curw) half-real-widths)) ;; appends in reverse order!

		;; to avoid overlapping nodes, let's expand width for the entire sub-tree
		(if (listp x)
		    (let ((subtree-width (node-dims x))) ;; extract just first value (width)
		      (setq curw (max curw subtree-width)))
		    )

		;; accumulate total node width
		(incf w
		      (if (= w 0)
			  curw
			  (+ *spacing* curw)))

		;; calculate offset to the next node element
		(setq dx (+ curw *spacing*))
		(setq offsets (cons dx offsets)) ;; appends in reverse order!

		;;calculate half-width (for edges placement)
		(setq half-widths (cons (div2 curw) half-widths)) ;; appends in reverse order!
		)
	    node)
    (values w h (reverse offsets) (reverse half-widths) (reverse half-real-widths))
    )
  )

(defun to-textbox (x y o)
  ;; converts "content" textbox into "graphical" textbox
  (make-textbox
   :rect (bounds-dims x y (text-and-dims-width o) (text-and-dims-height o))
   :text (text-and-dims-text o))
  )

(defun xy-tree-collect (tree rootx rooty)
  ;; position list boxes relatively to each other
  ;; x,y defines root node coordinates
  ;; returns a flat list of objects to be rendered

  (let ((result '()))
    (multiple-value-bind (node-w node-h node-dx half-widths half-real-widths) (node-dims tree)
      (let ((curx (- rootx (div2 node-w)))  ;; calculate total node width and use it as a center relative to parent x
	    (cury rooty)
	    )
	;; underline node level
	(push (line curx (+ cury node-h) (+ curx (- node-w 1)) (+ cury node-h)) result)

	(mapcar #'(lambda (o)
		    (if (listp o)
			(let ((newroot-y (+ cury node-h (+ (* 2 *spacing*) 1))) ;; make enough space for node levels.
			      (newroot-x (+ curx (car half-widths)))
			      (subnode-x (- (+ curx (car half-widths))
					    (car half-real-widths)))
			      )

			  ;; insert sub-node object
			  (push (make-subnode-box subnode-x cury) result)

			  ;; insert edge object
			  (push (line newroot-x (+ rooty node-h) newroot-x (- newroot-y 1)) result)

			  ;; move to sub-nodes
			  (nconc result
				 (xy-tree-collect o newroot-x newroot-y)))

			;; position one single box
			(push (to-textbox curx cury o) result)
			)

		    (incf curx (car node-dx))
		    (setq node-dx (cdr node-dx))
		    (setq half-widths (cdr half-widths))
		    (setq half-real-widths (cdr half-real-widths))
		    )
		tree)
	))
    result
    ))

(defun bounding-box-all (obj)
  ;; find bounding box for a obj list

  (let ((bb (make-rect2d-empty)))
    (mapcar #'(lambda (o)
		(setq bb (box-expand bb (bounding-box o)))
		) obj)
    bb)
  )

(defun xy-reposition (obj)
  ;; after initial tree construction, xy coordinates are "raw"
  ;; in sense that they can span (-inf;+inf)
  ;; for rendering they are required to be in [0;+inf) range

  (let* ((box (bounding-box-all obj)) ;; find bounding box
	 (dx (- (rect2d-left box)))
	 (dy (- (rect2d-top box)))
	 )

    ;; move all objects by (dx;dy) vector
    (mapcar #'(lambda (o)
		(move o dx dy))
	    obj))
  )

(defun render-all (obj s)
  (mapcar #'(lambda (o)
	      (render o s))
	  obj)
  )

(defun print-tree (tree)
  ;; renders tree into stdout

  (let* ((box-tree (box-tree-collect tree))        ;; let's transform lisp tree into tree of text boxes
	 (xy-obj (xy-tree-collect box-tree 0 0))   ;; position objects relatively to each other
	 (xy-obj (xy-reposition xy-obj))           ;; move to positive coordinates
	 (xy-box (bounding-box-all xy-obj))	   ;; find total bounding box
	 (surface (surface-create
		   (rect2d-width xy-box)
		   (rect2d-height xy-box)))        ;; create rendering surface of size enough for rendering

	)
    (render-all xy-obj surface)	                   ;; renders the entire object list onto surface
    (surface-print surface)			   ;; writes surface to stdout
    )
 )

;; ================text cases=================================================
;;(bounds-points (bounds 0 0 10 20))
;;(box-expand (bounds 0 -25 100 20)(bounds -10 0 80 40))
;;(setq s (surface-create 30 30))
;;(surface-render-line! s (line 0 29 29 0))
;;(surface-put! s 9 9 #\y)
;;(surface-put! s 0 0 #\y)
;;(surface-render-horz-line! s 0 9 3)
;;(surface-render-vert-line! s 0 0 9)
;;(surface-render-box! s (bounds 0 0 9 9))
;;(surface-render-text! s 1 1 "абвг")
;;(surface-print s)

;;(print-tree '("A" ("BC" "D") "EFG"))
(print-tree (list 1 2 3 (list 20 30 (list 10 20 30) (list 10 20 30)) "234" "Привет"))
;; TODO

;; ================text cases=================================================
