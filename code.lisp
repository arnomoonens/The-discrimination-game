;------CONFIG------
(defparameter *nr-of-objects* 6)
(defparameter *pruning-frequency* 20)
(defparameter *pruning-used-treshold* 10)
(defparameter *pruning-success-treshold* 0.2)
(defparameter *saliency-treshold* 0)


(defun parse-number (str)
  (with-input-from-string (in str)
                          (read in)))

(defun random-element (l)
  (nth (random (length l)) l))

(defun max-depth (tree)
  (if (or (null (node-left tree)) (null (node-right tree)))
      0
      (max (+ 1 (max-depth (node-left tree))) (+ 1 (max-depth (node-right tree))))))

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

(defun get-random-objects (n)
  (let ((in (open "~/MA1-AI/Artificial Intelligence Programming Paradigms/Assignment1/object-features.txt" :if-does-not-exist nil)))
  (when in
    (read-line in nil)
    (defparameter *objects* (loop for line = (read-line in nil)
                                                  until (eq line nil)
                                                   collect (process-object-string line)))
      (close in)))
(loop repeat n collect (random-element *objects*)))


(defstruct node
  (channel nil :type symbol)
  (regionstart 0 :type number)
  (regionend 1 :type number)
  (success 0 :type number)
  (used 0 :type number)
  (age 0 :type number)
  (left)
  (right))

(defun split-node (node)
  (let* ((channel (node-channel node))
         (regionstart (node-regionstart node))
         (regionend (node-regionend node))
         (left (make-node :channel channel :regionstart regionstart :regionend (/ (+ regionstart regionend) 2)))
         (right (make-node :channel channel :regionstart (/ (+ regionstart regionend) 2) :regionend regionend)))
    (setf (node-left node) left)
    (setf (node-right node) right)))

(defun filter-objects (filter-nodes objects)
  (loop for node in filter-nodes
        for start = (node-regionstart node)
        for end = (node-regionend node)
        for channel = (node-channel node)
        do (setf objects (loop for obj in objects
                               when (and (>= (slot-value obj channel) start) (<= (slot-value obj channel) end))
                               collect obj))
        finally (return objects)))

(defun random-expand (tree)
  (if (and (not (null (node-left tree))) (not (null (node-right tree)))) ;we are not at a leaf yet
      (if (= (random 2) 0) ;randomly go to the left or right child
          (random-expand (node-left tree))
          (random-expand (node-right tree)))
      (split-node tree))) ;we are at a leaf: split it

(defun same-values (objects channel)
  (let ((first-value (slot-value (car objects) channel)))
    (loop for obj in objects do
          (when (not (= (slot-value obj channel) first-value))
            (return nil))
          finally (return t))))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
                           (with-slots (channel regionstart regionend left right) object
                                       (format stream "[~s ~d-~d] " channel (float regionstart) (float regionend)))))

(defstruct agent
  (games-played 0 :type number)
  (repertoire-size (length *sensory-channels*) :type number)
  (objects (get-random-objects *nr-of-objects*) :type cons)
  (trees (loop for channel in *sensory-channels* collect (make-node :channel channel)) :type cons))

(defparameter *agent* (make-agent))

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
                (setf (slot-value obj channel) (/ (- (slot-value obj channel) min) diff)))); (X - MIN) / (MAX - MIN)
        finally (return objects)))

(defun increment-age (trees)
  (loop while (not (null trees)) do
        (let ((node (pop trees)))
          (setf (node-age node) (+ 1 (node-age node)))
          (when (and (node-left node) (node-right node))
            (push (node-left node) trees)
            (push (node-right node) trees)))))

(defun trees-pruning (trees)
  (let ((removed-nodes 0))
    (defun walk-tree (tree)
    (if (or (null (node-left tree)) (null (node-right tree)));Check if node may be removed only if it is a leaf node (i.e. has no children)
        (and (> (node-used tree) *pruning-used-treshold*) (< (/ (node-success tree) (node-used tree)) *pruning-success-treshold*)) ;Node may be removed if it is already used for a few times but hasn't been very successful
        (let ((left (walk-tree (node-left tree)))
              (right (walk-tree (node-right tree))))
          (if (and left right) ;If the left and right child may be removed: remove them execute walk-tree function on same node (which will go to the 'then' branch of the first 'if')
              (progn (format t "Removing children of ~a" tree) (setf (node-left tree) nil) (setf (node-right tree) nil) (setf removed-nodes (+ 2 removed-nodes)) (walk-tree tree))
              nil))))
  (loop for tree in trees do ;Check the tree of every sensory channel
        (walk-tree tree))
  removed-nodes))

(defun play-game (agent)
  (setf (agent-games-played agent) (+ 1 (agent-games-played agent)))
  (when (= (mod (agent-games-played agent) *pruning-frequency*) 0)
          (setf (agent-repertoire-size agent) (- (agent-repertoire-size agent) (trees-pruning (agent-trees agent)))))
  (increment-age (copy-list (agent-trees agent)))
  (let* ((objects-copy (loop for obj in (agent-objects agent) collect (copy-object obj)))
         (objects-scaled (context-scaling objects-copy))
         (topic (random-element (agent-objects agent)))
         (topic-scaled (nth (position topic (agent-objects agent)) objects-scaled))
         (trees (saliency-filter topic (remove topic (agent-objects agent)) (agent-trees agent) *saliency-treshold*)))
    ; (format t "Value for ~s channel of topic: ~d~%" (node-channel tree) (slot-value topic (node-channel tree)))
    (defun try-node (node objects path)
      (let* ((filtered-objects (filter-objects (list node) objects))
             (new-path (cons (cons node filtered-objects) path))); Each element of a path is a cons of the visited node and the objects left after filtering using that node
        (setf (node-used node) (+ 1 (node-used node)))
        (cond
          ((or (null (node-left node)) (null (node-right node))) (reverse new-path))
          ((<= (slot-value topic-scaled (node-channel node)) (/ (+ (node-regionstart node) (node-regionend node)) 2)) (try-node (node-left node) filtered-objects new-path))
          (t (try-node (node-right node) filtered-objects new-path)))))
    (let* ((tree-paths (loop for tree in trees collect (try-node tree objects-scaled '())))
           (combinations (tree-combinations tree-paths))) ;Combinations of trees (sensory channels), e.g. X and WIDTH, only GRAYSCALE, all of them together
      (loop for combination in combinations
            for tree-nodes-lists = (tree-nodes-combinations combination) ;make combinations of nodes from those trees, e.g.: {[X 0.0-0.5] [Y 0.5-1.0]}
            for result = (loop for nodes-combination in tree-nodes-lists
                                      for filtered = (reduce #'intersection (mapcar #'cdr nodes-combination));Filter using combinations of nodes: intersection of objects left after filtering using the nodes
                                      when (and (= (length filtered) 1) (eq (car filtered) topic-scaled)) do ;Only the topic is left after filtering
                                              (let ((nodes (mapcar #'car nodes-combination)))
                                                (loop for node in nodes do (setf (node-success node) (+ 1 (node-success node)))) ;Increase success of all nodes used for discrimination
                                                (return nodes));Discriminative combination found: stop looping
                                      finally (return nil)) ;No discriminative combination of nodes found
            when result do
              ;(format t "Found it, combination: ~a. Topic was: ~a~%" result topic-scaled)
              (return (values (agent-repertoire-size agent) result)) ;Result from inner-loop: stop outer loop as well
            finally (progn (random-expand (random-element (agent-trees agent))) (return (values (setf (agent-repertoire-size agent) (+ 2 (agent-repertoire-size agent))) nil))))))) ;No results from all the inner loops: randomly expand a random tree

;collect (progn (loop for node in result do (setf (node-success node) (+ 1 (node-success node)))) t) into results

(loop repeat 20
      for (sizepoint successpoint) =  (loop repeat 25
                    for (size result) = (multiple-value-list (PLAY-GAME *agent*))
                    collect result into results
                    finally (return (list size (/ (count-if-not #'null results) 25.0))))
      summing 25 into xaxis
      collect (list xaxis sizepoint) into sizes
      collect (list xaxis successpoint) into successes
      finally (progn (loop for xy in sizes do (format t "(~d, ~d)~%" (car xy) (cadr xy))) (loop for xy in successes do (format t "(~d, ~d)~%" (car xy) (cadr xy)))))

