(defparameter *nr-of-objects* 6)

(defun parse-number (str)
  (with-input-from-string (in str)
                          (read in)))

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

(defun filter-objects (filter-node objects)
  (let ((start (node-regionstart filter-node))
        (end (node-regionend filter-node))
        (schannel (node-schannel filter-node)))
    (loop for obj in objects
          when (and (>= (slot-value obj schannel) start) (< (slot-value obj schannel) end))
          collect obj)))

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

;geeft die met meeste saliency terug
; (defun most-salient (topic objects)
;   (loop for channel in *sensory-channels*
;         collect (loop for obj in objects
;                       collect (abs (- (slot-value topic channel) (slot-value obj channel))) into diffs
;                       finally (return (apply #'min diffs))) into channel-diffs
;         finally (return (nth (position (apply #'max channel-diffs) channel-diffs) *sensory-channels*))))

(defun saliency-filter (topic objects trees treshold)
  (loop for channel in *sensory-channels*
        summing 1 into ctr
        for saliency = (loop for obj in objects
                      collect (abs (- (slot-value topic channel) (slot-value obj channel))) into diffs
                      finally (return (apply #'min diffs)))
        when (> saliency treshold)
        	collect  (nth (- ctr 1) trees)))

(defun context-scaling (objects)
  (loop for channel in *sensory-channels* do
        (let* ((values (loop for obj in objects collect (slot-value obj channel)))
               (min (apply #'min values))
               (max (apply #'max values))
               (diff (- max min)))
          (loop for obj in objects do
                (setf (slot-value obj channel) (/ (- (slot-value obj channel) min) diff))))))

(defun increment-age (trees)
  (loop while (not (null trees)) do
         (let ((node (pop trees)))
           (setf (node-age node) (+ 1 (node-age node)))
           (when (and (node-left node) (node-right node))
               (push (node-left node) trees)
               (push (node-right node) trees)))))

(defun play-game (agent)
  (increment-age (copy-list (agent-trees agent)))
  (let* ((objects-copy (loop for obj in (agent-objects agent) collect (copy-object obj)))
         (topic (random-element (agent-objects agent)))
         (trees (saliency-filter topic (remove topic (agent-objects agent)) (agent-trees agent) 0)))
    (context-scaling objects-copy)
    ; (format t "Value for ~s channel of topic: ~d~%" (node-schannel tree) (slot-value topic (node-schannel tree)))
    (defun try-node (node objects path)
      (let ((filtered-objects (filter-objects node objects))
            (new-path (cons node path)))
        ;(format t "New path: ~a, filtered-objects: ~d~%" new-path (length filtered-objects))
        (setf (node-used node) (+ 1 (node-used node)))
        (cond ((null filtered-objects) nil)
              ((and (= (length filtered-objects) 1) (eq (car filtered-objects) topic)) new-path)
              ((same-values filtered-objects (node-schannel node)) nil)
              ((or (null (node-left node)) (null (node-right node))) nil)
              ((< (slot-value topic (node-schannel node)) (/ (+ (node-regionstart node) (node-regionend node)) 2)) (try-node (node-left node) filtered-objects new-path))
              (t (try-node (node-right node) filtered-objects new-path)))))
        (loop for tree in (agent-trees agent)
              for result = (try-node tree (agent-objects agent) '())
              if result
              	collect (progn (loop for node in result do (setf (node-success node) (+ 1 (node-success node)))) t) into results
               else
               	collect nil into results
               finally (let ((got-success (reduce (lambda (x y) (or x y)) results)))
                         (if got-success
                             (format t "Got success! List: ~a~%" results)
                             (progn (format t "No successes, expanding randomly~%") (random-expand (random-element (agent-trees agent)))))))))


;Todo: apply context-scaling: first copy objects
; combine multiple trees (not just leafs)
; tree pruning