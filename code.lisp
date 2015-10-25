;------CONFIG------
(defparameter *nr-of-objects* 6)
(defparameter *pruning-frequency* 20)
(defparameter *pruning-used-treshold* 10)
(defparameter *pruning-success-treshold* 0.2)


(defun parse-number (str)
  (with-input-from-string (in str)
                          (read in)))

(defun same-elements (first second)
  (and (= (length first) (length second)) (loop for el in first do
                                                (when (not (member el second)) (return nil))
                                                finally (return t))))

;Source: my solutions of the Lisp exercises
(defun tree-combinations (trees)
  (labels ((iter (result current)
                 (if (null current)
                     (cdr (sort (copy-list result) #'< :key #'length)) ;sort by length: try combinations with least number of trees first
                     (iter (append (mapcar (lambda (i) (cons (car current) i)) result) result) (cdr current)))))
          (iter '(nil) trees)))

;Todo: write iteratively?
(defun tree-nodes-combinations (lists)
  (if (car lists)
      (mapcan (lambda (inner-val)
                (mapcar (lambda (outer-val)
                          (cons outer-val
                                inner-val))
                        (car lists)))
              (tree-nodes-combinations (cdr lists)))
      (list nil)))

(defparameter *sensory-channels* '(X Y WIDTH HEIGHT GRAYSCALE))

(defstruct (object
             (:constructor create-object (x y width height grayscale)))
  x y width height grayscale)

(defun process-object-string (string)
  (loop for start = 0 then (1+ stop)
        for stop = (position #\, string :start start)
        collecting (parse-number (subseq string start stop)) into object-list
        until (null stop)
        finally (return (apply #'create-object (append (subseq object-list 0 2) (subseq object-list 3 6))))))

(let ((in (open "~/MA1-AI/Artificial Intelligence Programming Paradigms/Assignment1/object-features.txt" :if-does-not-exist nil)))
  (when in
    (read-line in nil)
    (defparameter *objects* (loop repeat *nr-of-objects*
                                  collect (process-object-string (read-line in nil)))
      (close in))))

(defstruct node
  (schannel nil :type symbol)
  (regionstart 0 :type number)
  (regionend 1 :type number)
  (success 0 :type number)
  (used 0 :type number)
  (age 0 :type number)
  (left)
  (right))

(defun split-node (node)
  (let* ((schannel (node-schannel node))
         (regionstart (node-regionstart node))
         (regionend (node-regionend node))
         (left (make-node :schannel schannel :regionstart regionstart :regionend (/ (+ regionstart regionend) 2)))
         (right (make-node :schannel schannel :regionstart (/ (+ regionstart regionend) 2) :regionend regionend)))
    (setf (node-left node) left)
    (setf (node-right node) right)))

(defparameter *trees* (loop for schannel in *sensory-channels*
                            collect (make-node :schannel schannel)))

(defun filter-objects (filter-nodes objects)
  (loop for node in filter-nodes
        for start = (node-regionstart node)
        for end = (node-regionend node)
        for channel = (node-schannel node)
        do (setf objects (loop for obj in objects
                               when (and (>= (slot-value obj channel) start) (< (slot-value obj channel) end))
                               collect obj))
        finally (return objects)
        ))

(defun random-expand (tree)
  (if (and (not (null (node-left tree))) (not (null (node-right tree)))) ;we are not at a leaf yet
      (if (= (random 2) 0) ;randomly go to the left or right child
          (random-expand (node-left tree))
          (random-expand (node-right tree)))
      (split-node tree))) ;we are at a leaf: split it

(defun same-values (objects schannel)
  (let ((first-value (slot-value (car objects) schannel)))
    (loop for obj in objects do
          (when (not (= (slot-value obj schannel) first-value))
            (return nil))
          finally (return t))))

(defun random-element (l)
  (nth (random (length l)) l))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
                           (with-slots (schannel regionstart regionend left right) object
                                       ;(format stream "N[~d, ~d] ~% /  \\ ~% ~a  ~a" regionstart regionend left right))))
                                       (format stream "~s[~d, ~d] " schannel regionstart regionend left right))))

(defstruct agent
  (games-played 0 :type number)
  (objects)
  (trees))

(defparameter *agent* (make-agent :objects *objects* :trees *trees*))

(defun saliency-filter (topic objects trees treshold)
  (loop for channel in *sensory-channels*
        summing 1 into ctr
        for saliency = (loop for obj in objects
                             collect (abs (- (slot-value topic channel) (slot-value obj channel))) into diffs
                             finally (return (apply #'min diffs)))
        when (> saliency treshold)
        collect  (nth (- ctr 1) trees)))

(defun context-scaling (objects)
  (loop for channel in (butlast *sensory-channels*) do ;Butlast: don't apply context-scaling to grayscale, values are important
        (let* ((values (loop for obj in objects collect (slot-value obj channel)))
               (min (apply #'min values))
               (max (apply #'max values))
               (diff (- max min)))
          (loop for obj in objects do
                (setf (slot-value obj channel) (/ (- (slot-value obj channel) min) diff))))
        finally (return objects)))

(defun increment-age (trees)
  (loop while (not (null trees)) do
        (let ((node (pop trees)))
          (setf (node-age node) (+ 1 (node-age node)))
          (when (and (node-left node) (node-right node))
            (push (node-left node) trees)
            (push (node-right node) trees)))))

(defun trees-pruning (trees)
  (defun walk-tree (tree)
    (if (or (null (node-left tree)) (null (node-right tree)))
        (and (> (node-used tree) *pruning-used-treshold*) (< (/ (node-success tree) (node-used tree)) *pruning-success-treshold*))
        (let ((left (walk-tree (node-left tree)))
              (right (walk-tree (node-right tree))))
          (if (and left right)
              (progn (format t "Removing children of ~a" tree) (setf (node-left tree) nil) (setf (node-right tree) nil) (and (> (node-used tree) 5) (< (/ (node-success tree) (node-used tree)) 0.2)))
              nil))))
  (loop for tree in trees do
        (walk-tree tree)))

(defun play-game (agent)
  (setf (agent-games-played agent) (+ 1 (agent-games-played agent)))
  (when (= (mod (agent-games-played agent) *pruning-frequency*) 0)
          (trees-pruning (agent-trees agent)))
  (increment-age (copy-list (agent-trees agent)))
  (let* ((objects-copy (loop for obj in (agent-objects agent) collect (copy-object obj)))
         (objects-scaled (context-scaling objects-copy))
         (topic (random-element (agent-objects agent)))
         (topic-scaled (nth (position topic (agent-objects agent)) objects-scaled))
         (trees (saliency-filter topic (remove topic (agent-objects agent)) (agent-trees agent) 0)))
    ; (format t "Value for ~s channel of topic: ~d~%" (node-schannel tree) (slot-value topic (node-schannel tree)))
    (defun try-node (node objects path)
      (let* ((filtered-objects (filter-objects (list node) objects))
             (new-path (cons (cons node filtered-objects) path)))
        (setf (node-used node) (+ 1 (node-used node)))
        (cond
          ((or (null (node-left node)) (null (node-right node))) (reverse new-path))
          ((< (slot-value topic-scaled (node-schannel node)) (/ (+ (node-regionstart node) (node-regionend node)) 2)) (try-node (node-left node) filtered-objects new-path))
          (t (try-node (node-right node) filtered-objects new-path)))))
    (let* ((tree-paths (loop for tree in trees collect (try-node tree objects-scaled '())))
           (combinations (tree-combinations tree-paths)))
      (loop for combination in combinations
            for tree-nodes-lists = (tree-nodes-combinations combination)
            for result = (loop for nodes-combination in tree-nodes-lists
                                      for filtered = (reduce #'intersection (mapcar #'cdr nodes-combination))
                                      when (and (= (length filtered) 1) (eq (car filtered) topic-scaled)) do
                                              (let ((nodes (mapcar #'car nodes-combination)))
                                                (loop for node in nodes do (setf (node-success node) (+ 1 (node-success node))))
                                                (return nodes))
                                      finally (return nil))
            when result do
              ;(format t "Found it, combination: ~a. Topic was: ~a~%" result topic-scaled)
              (return result)
            finally (random-expand (random-element (agent-trees agent)))))))

;collect (progn (loop for node in result do (setf (node-success node) (+ 1 (node-success node)))) t) into results

;Todo: apply context-scaling: first copy objects
; combine multiple trees (not just leafs)
; tree pruning