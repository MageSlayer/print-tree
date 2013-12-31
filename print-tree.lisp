;;=================================types=================================================
(defstruct point2d
  x y
  )

(defstruct rect2d
  lefttop rightbottom
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
(defparameter *spacing* 2)		; number of symbols between graphical objects
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

(defun textbox-move (box dx dy)
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
  (loop for x from x1 to x2
     do
       (surface-put! s x y #\─))
  )

(defun surface-render-vert-line! (s x y1 y2)
  (loop for y from y1 to y2
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

(defun box-tree-collect (tree)
  ;; creates a tree of text boxes to render
  (mapcar #'(lambda (x)
	      (if (atom x)
		  (make-textbox-dims (princ-to-string x))
		  (box-tree-collect x)
		  ))
	  tree)
  )

(defun node-dims (node)
  ;; calculates horizontal node total width and height

  (let ((w 0)
	(h 0)
	(curw nil)
	(dx nil)
	(offsets '())			;; accumulates x offsets of boxes/edges.
	)
    (mapcar #'(lambda (x)
		(if (listp x)
		    (setq curw 1) ;; egde width is hardcoded to be 1 symbol. Height is not influenced by sub-nodes.

		    (progn
		      (setq curw (text-and-dims-width x)) ;; box width.
		      (setq h (max h (text-and-dims-height x))) ;; box height.
		      )
		    )

		;; accumulate total node width
		(incf w
		      (if (= w 0)
			  curw
			  (+ *spacing* curw)))

		;; calculate offset to the next node element
		(setq dx (+ curw *spacing*))
		(setq offsets (cons dx offsets)) ;; appends in reverse order!
		)
	    node)
    (values w (+ h *spacing*) (reverse offsets))
    )
  )

(defun xy-tree-collect (tree x y)
  ;; position list boxes relatively to each other
  ;; x,y defines root node coordinates

  (multiple-value-bind (node-w node-h node-dx) (node-dims tree)
    (let ((curx (- x (truncate (/ node-w 2))))  ;; calculate total node width and use it as a center relative to parent x
	  (cury (+ y *spacing*))
	  )
      (mapcar #'(lambda (o)
		  (let ((new-node
			 (if (listp o)
			     ;; move to sub-nodes
			     (xy-tree-collect o curx (+ cury node-h))

			     ;; position one single box
			     (make-textbox
			      :rect (bounds-dims curx cury (text-and-dims-width o) (text-and-dims-height o))
			      :text (text-and-dims-text o))
			     )))

		    (incf curx (car node-dx))
		    (setq node-dx (cdr node-dx))
		    new-node
		    )
		  )
	      tree)))
  )

(defun bounding-box (tree)
  ;; find bounding box for a tree

  (let ((bb (make-rect2d-empty)))
    (mapcar #'(lambda (o)
		(if (listp o)
		    (setq bb (box-expand bb (bounding-box o))) ;; move to sub-nodes
		    (setq bb (box-expand bb (textbox-rect o)))
		  )
		) tree)
    bb)
  )

(defun move-tree (tree dx dy)
  ;; moves tree by (dx;dy) vector

  (mapcar #'(lambda (o)
	      (if (listp o)
		  (move-tree o dx dy) ;; move to sub-nodes
		  (textbox-move o dx dy)
		  )
	      ) tree)
  )

(defun xy-reposition (tree)
  ;; after initial tree construction, xy coordinates are "raw"
  ;; in sense that they can span (-inf;+inf)
  ;; for rendering they are required to be in [0;+inf) range

  (let* ((box (bounding-box tree)) ;; find tree bounding box
	 (dx (- (rect2d-left box)))
	 (dy (- (rect2d-top box)))
	 )
    (move-tree tree dx dy)) ;; move by (dx;dy) vector
  )

(defun tree-render (tree s)
  (mapcar #'(lambda (o)
	      (if (listp o)
		  (tree-render o s) ;; move to sub-nodes
		  (surface-render-textbox! s o)
		  )
	      ) tree)
  )

(defun print-tree (tree)
  ;; renders tree into stdout


  (let* ((box-tree (box-tree-collect tree))        ;; let's transform lisp tree into tree of text boxes
	 (xy-tree (xy-tree-collect box-tree 0 0))  ;; position boxes relatively to each other
	 (xy-tree (xy-reposition xy-tree))         ;; move to positive coordinates
	 (xy-box (bounding-box xy-tree))	   ;; find total bounding box
	 (surface (surface-create
		   (rect2d-width xy-box)
		   (rect2d-height xy-box)))        ;; create rendering surface of size enough for rendering

	)
    (tree-render xy-tree surface)	           ;; renders the entire tree onto surface
    (surface-print surface)			   ;; writes surface to stdout
    )
 )

;; ================text cases=================================================
;;(bounds-points (bounds 0 0 10 20))
;;(box-expand (bounds 0 -25 100 20)(bounds -10 0 80 40))
;;(setq s (surface-create 10 10))
;;(surface-put! s 9 9 #\y)
;;(surface-put! s 0 0 #\y)
;;(surface-render-horz-line! s 0 9 3)
;;(surface-render-vert-line! s 0 0 9)
;;(surface-render-box! s (bounds 0 0 9 9))
;;(surface-render-text! s 1 1 "абвг")
;;(surface-print s)

(print-tree (list 1 2 3 (list 20 30) "234" "Привет"))

;; TODO

;; ================text cases=================================================
