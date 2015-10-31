(defun parse-number (str)
  (with-input-from-string (in str)
                          (read in)))

(defun split-in-parts (l n)
  (loop until (>= start (length l))
        for start = 0 then (+ start n)
        for stop = n then (+ stop n)
        collect (subseq l start (min (length l) stop))))

(defun random-element (l)
  (nth (random (length l)) l))

(defun max-depth (tree)
  (if (or (null (node-left tree)) (null (node-right tree)))
      0
      (max (+ 1 (max-depth (node-left tree)))
           (+ 1 (max-depth (node-right tree))))))

;Can be removed? Same as (not (mismatch first second))
(defun same-elements (first second)
  (and (= (length first) (length second)) (loop for el in first do
                                                (when (not (member el second))
                                                  (return nil))
                                                finally (return t))))

;Source: my solutions of the Lisp exercises
(defun tree-combinations (trees)
  (labels ((iter (result current)
                 (if (null current)
                     (cdr (sort (copy-list result) #'< :key #'length)) ;sort by length: try combinations with least number of trees first
                     (iter (append (mapcar (lambda (i) (cons (car current) i)) result)
                                   result) (cdr current)))))
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
      '(nil)))

(defparameter *sensory-channels* '(X Y WIDTH HEIGHT GRAYSCALE))

(defstruct (object
             (:constructor create-object (x y width height grayscale)))
  x y width height grayscale)

(defun process-object-string (string)
  (loop for start = 0 then (1+ stop)
        for stop = (position #\, string :start start)
        collecting (parse-number (subseq string start stop)) into object-list
        until (null stop)
        finally (return (apply #'create-object (append (subseq object-list 0 2)
                                                       (subseq object-list 3 6))))))

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

(defun mean-success (nodes)
  (if (null nodes)
      0
      (/ (reduce #'+ nodes :key #'node-success) (length nodes))))

(defun split-node (node)
  (let* ((channel (node-channel node))
         (regionstart (node-regionstart node))
         (regionend (node-regionend node))
         (half (/ (+ regionstart regionend) 2))
         (left (make-node :channel channel :regionstart regionstart :regionend half))
         (right (make-node :channel channel :regionstart half :regionend regionend)))
    (setf (node-left node) left)
    (setf (node-right node) right)))

(defun filter-objects (filter-nodes objects)
  (loop for node in filter-nodes
        for start = (node-regionstart node)
        for end = (node-regionend node)
        for channel = (node-channel node)
        do (setf objects (loop for obj in objects
                               when (and (>= (slot-value obj channel) start)
                                         (<= (slot-value obj channel) end))
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
  (objects)
  (trees (loop for channel in *sensory-channels* collect (make-node :channel channel)) :type cons))

(defun saliency-filter (topic objects trees treshold)
  (loop for channel in *sensory-channels*
        summing 1 into ctr
        for saliency = (loop for obj in objects
                             collect (abs (- (slot-value topic channel) (slot-value obj channel))) into diffs
                             finally (return (apply #'min diffs)))
        when (> saliency treshold)
        collect  (nth (- ctr 1) trees)))

(defun get-saliency (topic objects trees)
  (loop for channel in *sensory-channels*
        collect (loop for obj in objects
                      collect (abs (- (slot-value topic channel) (slot-value obj channel))) into diffs
                      finally (return (apply #'min diffs)))))

(defun sort-by-saliency (topic objects trees)
  (let ((saliency-values (loop for channel in *sensory-channels*
                               collect (loop for obj in objects
                                             collect (abs (- (slot-value topic channel) (slot-value obj channel))) into diffs
                                             finally (return (cons channel (apply #'min diffs)))))))
    (sort saliency-values #'> :key #'cdr)))

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

(defun trees-pruning (trees pruning-used-treshold pruning-success-treshold)
  (let ((removed-nodes 0))
    (defun walk-tree (tree)
      (if (or (null (node-left tree)) (null (node-right tree)));Check if node may be removed only if it is a leaf node (i.e. has no children)
          (if (> (node-used tree) pruning-used-treshold)
              (< (/ (node-success tree) (node-used tree)) pruning-success-treshold)
              (and (or (= (node-used tree) 0)
                       (< (/ (node-success tree) (node-used tree)) pruning-success-treshold))
                   (> (node-age tree) 50))) ;Node may be removed if it is already used for a few times but hasn't been very successful
          (let ((left (walk-tree (node-left tree)))
                (right (walk-tree (node-right tree))))
            (if (and left right) ;If the left and right child may be removed: remove them execute walk-tree function on same node (which will go to the 'then' branch of the first 'if')
                (progn (format t "Pruning children of ~a~%" tree)
                       (setf (node-left tree) nil)
                       (setf (node-right tree) nil)
                       (setf removed-nodes (+ 2 removed-nodes))
                       (walk-tree tree))
                nil))))
    (loop for tree in trees do ;Check the tree of every sensory channel
          (walk-tree tree))
    removed-nodes))

(defun play-n-games (agent n pruning-frequency pruning-used-treshold pruning-success-treshold saliency-treshold)
  (let* ((objects-copy (loop for obj in (agent-objects agent) collect (copy-object obj)))
         (objects-scaled (context-scaling objects-copy)))
    (defun try-node (topic-scaled node objects path)
      (let* ((filtered-objects (filter-objects (list node) objects))
             (new-path (cons (cons node filtered-objects) path))); Each element of a path is a cons of the visited node and the objects left after filtering using that node
        (cond
          ((or (null (node-left node)) (null (node-right node))) (reverse new-path))
          ((< (slot-value topic-scaled (node-channel node))
              (/ (+ (node-regionstart node) (node-regionend node)) 2)) (try-node topic-scaled (node-left node) filtered-objects new-path))
          (t (try-node topic-scaled (node-right node) filtered-objects new-path)))))
    (loop repeat n
          for result = (let* ((topic (random-element (agent-objects agent)))
                              (topic-scaled (nth (position topic (agent-objects agent)) objects-scaled))
                              (trees (saliency-filter topic
                                                      (remove topic (agent-objects agent))
                                                      (agent-trees agent)
                                                      saliency-treshold))
                              (tree-paths (loop for tree in trees collect (try-node topic-scaled
                                                                                    tree
                                                                                    objects-scaled
                                                                                    '())))
                              (combinations (tree-combinations tree-paths))) ;Combinations of trees (sensory channels), e.g. X and WIDTH, only GRAYSCALE, all of them together
                         (setf (agent-games-played agent) (+ 1 (agent-games-played agent)))
                         (when (= (mod (agent-games-played agent) pruning-frequency) 0)
                           (setf (agent-repertoire-size agent) (- (agent-repertoire-size agent)
                                                                  (trees-pruning (agent-trees agent)
                                                                                 pruning-used-treshold
                                                                                 pruning-success-treshold))))
                         (increment-age (copy-list (agent-trees agent)))
                         (loop for combination in combinations
                               for tree-nodes-lists = (tree-nodes-combinations combination) ;make combinations of nodes from those trees, e.g.: {[X 0.0-0.5] [Y 0.5-1.0]}
                               for result =  (loop for nodes-combination in tree-nodes-lists
                                             for filtered = (reduce #'intersection (mapcar #'cdr nodes-combination));Filter using combinations of nodes: intersection of objects left after filtering using the nodes
                                             for nodes = (mapcar #'car nodes-combination)
                                             do (loop for node in nodes do (setf (node-used node) (+ 1 (node-used node))));Increase used counter of nodes in combination
                                             when (and (= (length filtered) 1) (eq (car filtered) topic-scaled)) do ;Only the topic is left after filtering
                                                (loop for node in nodes do (setf (node-success node) (+ 1 (node-success node)))) ;Increase success of all nodes used for discrimination
                                                (return nodes);Discriminative combination found: stop looping
                                             finally (return nil)) ;No discriminative combination of nodes found
                               when result
                                  collect result into results
                               finally (if (not (null results))
                                           (let ((most-successful (reduce (lambda (x y) (if (>= (mean-success x) (mean-success y));Keep the combination of nodes that was on average the most successful in the past
                                                                                            x
                                                                                            y))
                                                                          results)))
                                             (return (list (agent-repertoire-size agent) most-successful)))
                                           (progn (random-expand (random-element (agent-trees agent)))
                                                  (return (list (setf (agent-repertoire-size agent) (+ 2 (agent-repertoire-size agent)))
                                                                nil))))))
          collect result into results
          finally (return results)))) ;No results from all the inner loops: randomly expand a random tree

(defparameter *agent* (make-agent :objects (get-random-objects 6)))

(defun reset-agent (agent)
  (setf (agent-repertoire-size agent) (length *sensory-channels*))
  (setf (agent-games-played agent) 0)
  (setf (agent-trees agent) (loop for channel in *sensory-channels* collect (make-node :channel channel))))

(let* ((average-of-n-games 25)
       (nr-of-groups 40)
       (pruning-frequency 25)
       (pruning-used-treshold 25)
       (pruning-success-treshold 0.2)
       (saliency-treshold 0)
       (results (play-n-games *agent* (* nr-of-groups average-of-n-games) pruning-frequency pruning-used-treshold pruning-success-treshold saliency-treshold))
       (results-split (split-in-parts results average-of-n-games))
       (results-reduced (mapcar (lambda (results-part)
                                  (append (list (caar (last results-part)))
                                          (list (/ (count-if-not #'null results-part :key #'cadr) (float average-of-n-games))))) results-split))
       (my-stream (open (concatenate 'string "~/Dropbox/aipp1/csv/res"
                                     (write-to-string average-of-n-games) "-"
                                     (write-to-string nr-of-groups) "-"
                                     (write-to-string pruning-frequency) "-"
                                     (write-to-string pruning-used-treshold) "-"
                                     (write-to-string pruning-success-treshold) "-"
                                     (write-to-string saliency-treshold)
                                     ".csv")
                        :if-does-not-exist :create
                        :direction :output
                        :if-exists :overwrite)))
  (write-line (format nil "~{~a~^, ~}" '(games size success)) my-stream)
  (write-line (format nil "~{~a~^, ~}" '(0 5 0.0)) my-stream)
  (loop for result in results-reduced
        summing average-of-n-games into xaxis
        do
        (write-line (format nil "~{~a~^, ~}" (append (list xaxis) result)) my-stream))
  (close my-stream))
