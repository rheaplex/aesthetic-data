

(defun degrees-to-radians (degrees)
   (let ((x (if (floatp degrees) degrees (float degrees pi))))
     (* x (/ (float pi x) 180))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *svg-stream* t)

(defmethod svg-header (width height &key (to *svg-stream*))
  "Write the start of the SVG file."
  (format to "<?xml version=\"1.0\" standalone=\"no\"?>~%")
  (format to "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~%")
  (format to "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~%")
  (format to "<svg width=\"~dpx\" height=\"~dpx\" viewBox=\"0 0 ~d ~d\"~%"
          width height width height)
  (format to "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">~%"))

(defmethod svg-footer (&key (to *svg-stream*))
  "Write the end of the svg file."
  (format to "</svg>~%"))

(defmethod svg-rgb (r g b)
  (format nil "#~X~X~X" (floor r) (floor g) (floor b)))

(defmethod svg-path-tag-start (&key (to *svg-stream*))
  "Write the start of the path tag."
  (format to "<path"))

(defmethod svg-path-tag-end (&key (to *svg-stream*))
  "Write the start of the path tag."
  (format to " />~%"))

(defmethod svg-path-d-start (&key (to *svg-stream*))
  "Write the start of the path d."
  (format to " d=\""))

(defmethod svg-path-d-end (&key (to *svg-stream*))
  "Write the start of the path d."
  (format to "\""))

(defmethod svg-fill (r g b &key (to *svg-stream*))
  "Write the fill property."
  (format to " fill=\"~a\" " (svg-rgb r g b)))

(defmethod svg-stroke (r g b &key (to *svg-stream*))
  "Write the stroke property."
  (format to " stroke=\"~a\" " (svg-rgb r g b)))

(defmethod svg-close-path (&key (to *svg-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to " z"))

(defmethod svg-moveto (x y &key (to *svg-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to " M ~,3F ~,3F" x y))

(defmethod svg-lineto (x y &key (to *svg-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to " L ~,3F ~,3F" x y))

(defmethod svg-circle-arc (radius to-x to-y large-arc sweep 
			   &key (to *svg-stream*))
  "Draw a small circular arc of given radius from current point to to-x, to-y."
  (format to " A ~,3F ~,3F 0.0 ~d ~d ~,3F ~,3F" 
	  radius radius large-arc sweep to-x to-y))

(defmethod svg-rectfill (x y width height r g b &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
          "<rect x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\" fill=\"~a\" />~%"
          x y width height (svg-rgb r g b)))

(defmethod svg-rectstroke (x y width height r g b
                             &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
   "<rect x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\" stroke=\"~a\" />~%"
   x y width height (svg-rgb r g b)))

(defmethod svg-circlefill (x y radius  r g b &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
          "<circle cx=\"~F\" cy=\"~F\" r=\"~F\" fill=\"~a\" />~%"
          x y radius (svg-rgb r g b)))

(defmethod svg-circlestroke (x y radius r g b
                             &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
   "<circle cx=\"~F\" cy=\"~F\" r=\"~F\" stroke=\"~a\" />~%"
   x y radius (svg-rgb r g b)))

(defmethod svg-translate-group (x y
				&key (to *svg-stream*))
  "Adds a group with a translate transform."
  (format to "<g transform=\"translate(~,3F ~,3F)\">~%" x y))

(defmethod svg-rotate-group (degrees x y
				&key (to *svg-stream*))
  "Adds a group with a rotate transform."
  (format to "<g transform=\"rotate(~,3F ~,3F ~,3F)\">~%" degrees x y))

(defmethod svg-close-group (&key (to *svg-stream*))
  "Adds a group close tag."
  (format to "</g>~%"))

(defmacro with-svg-file ((path width height) &body body)
  ;; Use gensym, save & restore *svg-stream*
  `(with-open-file (ps ,path :direction :output :if-exists :supersede)
     (setf *svg-stream* ps)
     (svg-header ,width ,height :to ps)
     ,@body
     (svg-footer :to ps)
     (namestring ps)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotate a point about the origin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-point (x y degrees)
  "Rotate the point x y degrees degrees around the origin."
  (let ((radians (- (degrees-to-radians degrees))))
    (values (- (* x (cos radians)) (* y (sin radians)))
	    (- (* x (sin radians)) (* y (cos radians))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converting from rgb colour percentages to hex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %percentage-to-255 (/ 255.0 100.0))

(defun percentage-to-255 (percentage)
  (* %percentage-to-255 percentage))

(defun rgb-to-255 (rgb)
  (list (percentage-to-255 (first rgb))
	(percentage-to-255 (second rgb))
	(percentage-to-255 (third rgb))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw colours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *square-size* 10.0)

(defun draw-colour-square (x y col)
  (svg-rectfill x y 
		*square-size* *square-size*
		(percentage-to-255 (first col)) 
		(percentage-to-255 (second col)) 
		(percentage-to-255 (third col))))

(defun draw-colour-lists-grid (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (loop for y below (length cols)
	   for col in cols
	   do (draw-colour-square (* x *square-size*) 
				  (* y *square-size*) 
				  col))))

(defparameter *square-skip* (* *square-size* 3))

(defun draw-colour-lists-radial-grid (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (progn
	  (svg-rotate-group (* x (/ 360.0 (length colour-lists))) 0 0)
	  (loop for y below (length cols)
	     for col in cols
	     ;; -0.5 centres the square on the y axis for rotation
	     do (draw-colour-square 
		 (* *square-size* -0.5) 
		 (+ (* y *square-size*) *square-skip*) 
		 col))
	  (svg-close-group))))


(defparameter *bar-width* 10.0)
(defparameter *bar-height* 50.0)

(defun draw-colour-bar (x y height col)
  (svg-rectfill x y 
		*bar-width* height
		(percentage-to-255 (first col)) 
		(percentage-to-255 (second col)) 
		(percentage-to-255 (third col))))

(defun draw-colour-lists-bars (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (loop for y below (length cols)
	   for col in cols
	   do (draw-colour-bar (* x *bar-width*)
			       (* y (/ *bar-height* (length cols))) 
			       (/ *bar-height* (length cols)) col))))

(defparameter *bar-skip* (* *square-size* 3))

(defun draw-colour-lists-radial-bars (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (progn
	  (svg-rotate-group (* x (/ 360.0 (length colour-lists))) 0 0)
	  (loop for y below (length cols)
	     for col in cols
	     ;; -0.5 centres the bar on the y axis for rotation
	     do (draw-colour-bar
		 (* *square-size* -0.5) 
		 (+ (* y (/ *bar-height* (length cols)) ) *bar-skip*) 
		 (/ *bar-height* (length cols)) 
		 col))
	  (svg-close-group))))


(defparameter *circle-radius* 10.0)
(defparameter *circle-cell* 30.0)

(defun draw-colour-circle (x y col)
  (svg-circlefill (* x *circle-cell*) (* y *circle-cell*) 
		   *circle-radius*
		   (percentage-to-255 (first col)) 
		   (percentage-to-255 (second col)) 
		   (percentage-to-255 (third col))))

(defun draw-colour-lists-circles (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (loop for y below (length cols)
	   for col in cols
	   do (draw-colour-circle x y col))))

(defparameter *circle-skip* 3)

(defun draw-colour-lists-radial-circles (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (progn
	  (svg-rotate-group (* x (/ 360.0 (length colour-lists))) 0 0)
	  (loop for y below (length cols)
	     for col in cols
	     do (draw-colour-circle 0 (+ y *circle-skip*) col))
	  (svg-close-group))))


(defparameter *rosette-segment-degrees-padding* 0.0)
(defparameter *rosette-total-height* 100.0)

(defmethod draw-rosette-segment (centre-x centre-y inner-radius outer-radius 
			      degrees col)
  "Draw a ring segment."
  (svg-path-tag-start)
  (svg-path-d-start)
  ;; bottom left
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y inner-radius)
					   (- (/ degrees 2.0)))
    (svg-moveto x y))
  ;; top left
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y outer-radius)
					   (- (/ degrees 2.0)))
    (svg-lineto x y))
  ;; top right
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y outer-radius)
					   (/ degrees 2.0))
    (svg-lineto x y))
  ;; bottom right
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y inner-radius)
					   (/ degrees 2.0))
    (svg-lineto x y))
  ;; join back to bottom left
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y inner-radius)
					   (- (/ degrees 2.0)))
    (svg-lineto x y))
  (svg-close-path)
  (svg-path-d-end)
  (svg-fill (percentage-to-255 (first col)) 
	    (percentage-to-255 (second col)) 
	    (percentage-to-255 (third col)))
  (svg-path-tag-end))

(defun draw-colour-lists-rosette (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (progn
	  (svg-rotate-group (* x (/ 360.0 (length colour-lists))) 0 0)
	  (loop for y below (length cols)
	   for col in cols
	     do (draw-rosette-segment 0.0 0.0
				      (* y (/ *rosette-total-height* 
					      (length cols)))
				      (* (+ y 1) 
					 (/ *rosette-total-height* 
					    (length cols)))
				      (- (/ 360.0 (length colour-lists)) 
					 *rosette-segment-degrees-padding*) 
				 col))
	  (svg-close-group))))


(defparameter *ring-segment-degrees-padding* 0.0)
(defparameter *ring-total-height* 100.0)

(defmethod draw-ring-segment (centre-x centre-y inner-radius outer-radius 
			      degrees col)
  "Draw a ring segment."
  (svg-path-tag-start)
  (svg-path-d-start)
  ;; bottom left
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y inner-radius)
					   (- (/ degrees 2.0)))
    (svg-moveto x y))
  ;; top left
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y outer-radius)
					   (- (/ degrees 2.0)))
    (svg-lineto x y))
  ;; top right
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y outer-radius)
					   (/ degrees 2.0))
    (svg-circle-arc outer-radius x y 0 1))
  ;; bottom right
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y inner-radius)
					   (/ degrees 2.0))
    (svg-lineto x y))
  ;; join back to bottom left
  (multiple-value-bind (x y) (rotate-point centre-x 
					   (+ centre-y inner-radius)
					   (- (/ degrees 2.0)))
    (svg-circle-arc inner-radius x y 0 0))
  (svg-close-path)
  (svg-path-d-end)
  (svg-fill (percentage-to-255 (first col)) 
	    (percentage-to-255 (second col)) 
	    (percentage-to-255 (third col)))
  (svg-path-tag-end))

(defun draw-colour-lists-rings (colour-lists) 
  (loop for x below (length colour-lists)
     for cols in colour-lists
     do (progn
	  (svg-rotate-group (* x (/ 360.0 (length colour-lists))) 0 0)
	  (loop for y below (length cols)
	   for col in cols
	   do (draw-ring-segment 0.0 0.0
				 (* y (/ *ring-total-height* (length cols)))
				 (* (+ y 1) 
				    (/ *ring-total-height* (length cols)))
				 (- (/ 360.0 (length colour-lists)) 
				    *ring-segment-degrees-padding*) 
				 col))
	  (svg-close-group))))



;; Studio, 8am to 11pm, Tuesday 24th June 2008

(defparameter *colours-list* 
'(((18.0346 25.7801 26.0975)
   (55.9777 65.0675 64.0803) 
   (40.3952 50.7362 47.7638) 
   (44.892 57.3144 56.1624) 
   (61.3443 72.9976 75.967)  )
((71.8074 73.0632 73.0266) (70.0008 74.8302 76.4706) (20.235 29.4896 31.8883) (33.3684 60.5356 65.0477) (35.552 46.5217 50.8446)  )
((72.0989 73.0602 73.0678) (70.5196 74.5998 76.3943) (23.9902 31.3298 33.3761) (36.2905 59.9557 64.6494) (40.5127 46.7628 50.7942)  )
((72.0363 73.022 73.0159) (70.6813 74.5953 76.2783) (23.9536 30.8949 32.9885) (35.6634 59.9603 64.6403) (40.3616 46.6194 50.7195)  )
((71.4931 72.6543 72.4285) (70.985 74.8257 75.6207) (21.912 27.924 29.3416) (38.1155 48.0461 49.9397) (30.7729 60.5341 65.2857)  )
((72.1187 73.0159 72.9915) (70.8248 74.6181 76.3363) (25.2201 32.2301 34.0108) (34.5342 60.5325 65.0324) (41.7762 46.7659 50.3777)  )
((72.1065 73.0159 72.9915) (71.0094 74.6029 76.1425) (24.0635 31.3298 33.2204) (36.3882 60.0153 64.448) (41.5015 46.7353 50.28)  )
((72.1279 73.019 72.9824) (71.0567 74.5647 76.112) (24.8142 31.9402 33.3608) (36.3531 60.0793 64.3046) (42.7176 47.0298 49.8634)  )
((72.2957 73.0602 73.0755) (71.0994 74.4366 76.2585) (26.8818 33.1991 35.0286) (40.3677 59.6262 64.0467) (42.8351 46.2623 50.4768)  )
((71.6228 72.6894 72.5887) (71.0277 74.7982 75.9319) (22.7237 27.892 30.2708) (39.7635 47.2129 50.4753) (31.1635 60.4471 65.4231)  )
((72.3369 73.1014 73.138) (70.8324 74.3618 76.1929) (27.5959 34.3526 35.9838) (40.3098 59.646 63.9368) (41.7868 46.2486 50.6706)  )
((65.4719 69.4408 68.246) (20.1038 26.07 26.6285) (70.5119 74.0887 75.5871) (33.7346 48.3345 50.8003) (35.5917 57.5402 61.651)  )
((45.4337 57.3022 59.3256) (11.8196 20.4395 23.4943) (33.7652 41.7182 42.1805) (28.5588 56.7468 66.3432) (41.3443 53.8628 57.026)  )
((6.59953 8.33295 8.61372) (19.0585 25.4124 24.1566) (20 26.4057 25.6214)  )
((10.4738 9.60098 7.07561) (28.3024 41.3519 39.2401) (14.4625 25.5375 24.0742) (17.0275 30.3517 30.9728) (37.1832 53.8323 50.5165)  )
((10.4005 9.55673 7.05425) 
 (25.9312 37.3037 40.9796) 
 (14.1176 25.359 23.7903) 
 (11.5633 22.7543 26.6667) 
 (15.5779 27.9286 30.4707)  )))

(defun grid-svg ()
  (with-svg-file ("./squares.svg" 500 500)
    (draw-colour-lists-grid *colours-list*)))

(defun bars-svg ()
  (with-svg-file ("./bars.svg" 500 500)
    (draw-colour-lists-bars *colours-list*)))

(defun circles-svg ()
  (with-svg-file ("./circles.svg" 500 500)
    (draw-colour-lists-circles *colours-list*)))


(defun radial-grid-svg ()
  (with-svg-file ("./radial-grid.svg" 500 500)
    (svg-translate-group 250 250)
    (draw-colour-lists-radial-grid *colours-list*)
    (svg-close-group)))

(defun radial-bars-svg ()
  (with-svg-file ("./radial-bars.svg" 500 500)
    (svg-translate-group 250 250)
    (draw-colour-lists-radial-bars *colours-list*)
    (svg-close-group)))

(defun radial-circles-svg ()
  (with-svg-file ("./radial-circles.svg" 500 500)
    (svg-translate-group 250 250)
    (draw-colour-lists-radial-circles *colours-list*)
    (svg-close-group)))


(defun rosette-svg ()
  (with-svg-file ("./rosette.svg" 500 500)
    (svg-translate-group 250 250)
    (draw-colour-lists-rings *colours-list*)
    (svg-close-group)))

(defun rings-svg ()
  (with-svg-file ("./rings.svg" 500 500)
    (svg-translate-group 250 250)
    (draw-colour-lists-rings *colours-list*)
    (svg-close-group)))