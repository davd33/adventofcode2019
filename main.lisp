;; SOME UTILITIES

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

;; DECEMBER 2ND - PART 1

(defun int-code-computer (program)
  (loop for i below (length program) by 4
        when (not (= 99 (nth i program)))
          do (let* ((pos0 (nth i program))
                    (pos1 (nth (1+ i) program))
                    (pos2 (nth (+ 2 i) program))
                    (storepos (nth (+ 3 i) program))
                    (pos1val (when pos1 (nth pos1 program)))
                    (pos2val (when pos2 (nth pos2 program))))
               (cond ((= pos0 1) (setf (nth storepos program) (+ pos1val pos2val)))
                     ((= pos0 2) (setf (nth storepos program) (* pos1val pos2val)))
                     (t (return)))))
  program)

(defun dec2-part1 ()
  (with-open-file (input "./dec2.input")
    (let ((code-to-be-run (read input)))
      (int-code-computer code-to-be-run))))

;; DEC 2ND - PART 2

(defun dec2-part2 ()
  (with-open-file (input2 "./dec2.2.input")
    (let ((read-list (read input2)))
      (loop for noun to 99
            do
               (loop for verb to 99
                     do
                        (let* ((copied-list (copy-list read-list))
                               (program (append (list (first copied-list)
                                                      noun
                                                      verb)
                                                (cdddr copied-list)))
                               (result (first (int-code-computer program))))
                          (when (= result 19690720)
                            (print (+ verb (* 100 noun))))))))))

;; DECEMBER 3RD

(defun move-str->list (move-str)
  "Split the move \"L377\" to a list '(\"L\" 377)"
  (when move-str
    (list (elt move-str 0) (parse-integer (subseq move-str 1)))))

(defun next-point (from direction distance)
  (destructuring-bind (from-x from-y) from
    (cond ((char= direction #\U) (list from-x (+ distance from-y)))
          ((char= direction #\R) (list (+ distance from-x) from-y))
          ((char= direction #\D) (list from-x (- from-y distance)))
          ((char= direction #\L) (list (- from-x distance) from-y)))))

(defun coordinates-of (path)
  "Calculates coordinates of a given path, translating the U,R,L,D notation
   to an X/Y notation. The central port coordinate being (:x 0 :y 0).
   The returned notation is a list of points, that is a list of tuples."
  (loop for move in path
        for (direction distance) = (move-str->list move)
        for current-point = '(0 0) then current-point
        collect (setf current-point (next-point current-point direction distance))))

(defun to-lines (points)
  "From a list of points, creates a list of pairs of them."
  (let ((lines (loop for p on points
                     for cnt to (length points) by 2
                     collect (subseq p 0 2))))
    (append `(((0 0) ,(caar lines))) lines)))

(defun line-type-of (line)
  (destructuring-bind ((x1 y1) (x2 y2)) line
    (cond ((= x1 x2) :vertical)
          ((= y1 y2) :horizontal)
          (t :diagonal))))

(defgeneric generate-points-for-line (line-type line)
  (:documentation
   "Return a list with the line's beginning and end points together with all
   intermediate points. Only integers.
   Lines are always straight! No diagonal managed here."))

(defmethod generate-points-for-line ((line-type (eql :horizontal)) line)
  (destructuring-bind ((x1 y1) (x2 _)) line
    (if (< x1 x2)
        (loop for i from x1 upto x2
              collect (list i y1))
        (loop for i from x1 downto x2
              collect (list i y1)))))

(defmethod generate-points-for-line ((line-type (eql :vertical)) line)
  (destructuring-bind ((x1 y1) (_ y2)) line
    (if (< y1 y2)
        (loop for i from y1 upto y2
              collect (list x1 i))
        (loop for i from y1 downto y2
              collect (list x1 i)))))

(defun point= (p1 p2)
  "Returns T if the two points are the same."
  (and (= (first p1) (first p2))
       (= (second p1) (second p2))))

(defun intersection-lines (l1 l2)
  "Goes through both lines, stopping at the end of the shortest, searching for an
   equal point (the intersection) which is then returned."
  (let ((l1points (generate-points-for-line (line-type-of l1) l1))
        (l2points (generate-points-for-line (line-type-of l2) l2)))
    (intersection l1points l2points
                  :test
                  #'point=)))

(defun distance (from to)
  "The Manhattan distance between points from and to."
  (destructuring-bind ((x1 y1) (x2 y2)) (list from to)
    (+ (abs (- y2 y1)) (abs (- x2 x1)))))

(defun line-desc (line)
  "Return the line's beginning and end points."
  (list (first line)
        (car (last line))))

(defun parallel-lines (l1 l2)
  "Returns T if l1 and l2 are parallel lines."
  (eql (line-type-of (line-desc l1))
       (line-type-of (line-desc l2))))

(defun get-all-intersections (path-a path-b)
  "From two lists of points (paths), return the intersections, that is all the points
   that are found in both paths."
  (let ((lines-a (to-lines (coordinates-of path-a)))
        (lines-b (to-lines (coordinates-of path-b))))
    (rest
     (remove-if #'null
                (loop for la in lines-a
                      append (loop for lb in lines-b
                                   append (when (not (parallel-lines la lb))
                                            (intersection-lines la lb))))))))

(defun shortest-distance-to-ori (points)
  "From the given lists of points, return the last shortest to point (0 0)."
  (loop for p in points
        minimize (distance '(0 0) p)))

(defun dec3-part1 ()
  (with-open-file (input "./dec3.input")
    (let ((path-a (read input))
          (path-b (read input)))
      (shortest-distance-to-ori (get-all-intersections path-a path-b)))))

;; DEC 3RD PART 2

(defun generate-points-for-lines (lines)
  "Generate complete lines for all the given lines."
  (loop for l in lines
        collect (generate-points-for-line (line-type-of l) l)))

(defun count-steps-to-point (wire point)
  "Calculates the number of steps (or points) from the central point
   to the given point of the wire.
   A wire is a list of complete lines. A complete line is a list of points
   containing all the points from the beginning to the end of the line."
  (let ((flatten-wire (loop for l in wire
                            for i to (length wire)
                            append (if (= i (1- (length wire)))
                                       l
                                       (subseq l 0 (1- (length l)))))))
    (loop for p in flatten-wire
          for i = 0 then (1+ i)
          when (point= point p)
            return i)))

(defun minimize-wire-steps (path-a path-b)
  (let* ((wire-a (generate-points-for-lines
                  (to-lines (coordinates-of path-a))))
         (wire-b (generate-points-for-lines
                  (to-lines (coordinates-of path-b))))
         (intersections (get-all-intersections path-a path-b)))
    (loop for i in intersections
          minimize (loop for w in (list wire-a wire-b)
                         sum (let ((a (count-steps-to-point w i)))
                               a)))))

(defvar simple-path-a '("R8" "U5" "L5" "D3"))
(defvar simple-path-b '("U7" "R6" "D4" "L4"))

(defvar example-path-a '("R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"))
(defvar example-path-b '("U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"))

(defvar example2-path-a '("R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"))
(defvar example2-path-b '("U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"))

(defun dec3-part2 ()
  (with-open-file (input "./dec3.2.input")
    (let ((path-a (read input))
          (path-b (read input)))
      (minimize-wire-steps path-a path-b))))

;; DAY 4 - PART 1

(define-condition passwd-is-not-a-string-error (error)
  ((text :initarg :text :reader text)))

(defun password-ok (passwd)
  "Return T if the password meets the following criteria:
    - 6-digit number
    - 2 adjacent digits are the same
    - going from left to right, the digits never decrease"
  (let ((failing-reason :none))
    (if (stringp passwd)
        (or (and (progn
                   (setf failing-reason :6-digit)
                   (= 6 (length passwd)))
                 (progn
                   (setf failing-reason :only-2-in-a-row)
                   (loop
                     for last-c = nil then c
                     for had-two-adjacents-once = nil
                       then had-two-adjacents-once

                     for c across passwd
                     for i to (length passwd)

                     for cnt-in-a-row = 1
                       then (if (eql last-c c)
                                (1+ cnt-in-a-row)
                                (progn
                                  (when (= 2 cnt-in-a-row)
                                    (setf had-two-adjacents-once t))
                                  1))

                     for two-adjacents-digits = (= 2 cnt-in-a-row)

                     when (= i (1- (length passwd)))
                       return (or had-two-adjacents-once
                                  two-adjacents-digits)))
                 (progn
                   (setf failing-reason :increasing-numbers)
                   (let ((number-list (map 'list
                                           (compose #'parse-integer #'string)
                                           passwd)))
                     (loop
                       for last-c = nil then c
                       for c in number-list
                       for has-decreased = (or (and (not (null last-c))
                                                    (> last-c c))
                                               has-decreased)
                       finally (return (not has-decreased))))))
            (values nil failing-reason))
        (error 'passwd-is-not-a-string-error
               :text "The passwd argument must be a string."))))

(defmacro test-password-ok (str expected)
  `(multiple-value-bind (pok reason) (password-ok ,str)
     (format t "~&~a ~a ~a"
             ,str
             (or (and (eql ,expected pok)
                      "--")
                 "KO")
             reason)))

(defun dec4-part1 ()
  (loop for i from 240920 upto 789857
        count (password-ok (format nil "~d" i))))

(defun dec4-part2 ()
  (test-password-ok "111122" t)
  (test-password-ok "112233" t)
  (test-password-ok "123444" nil)
  (test-password-ok "123449" t)
  (test-password-ok "111111" nil)
  (test-password-ok "223450" nil)
  (test-password-ok "123789" nil)
  (dec4-part1))

