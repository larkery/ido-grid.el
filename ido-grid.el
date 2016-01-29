(defcustom ido-grid-column-padding 3
  "How many columns of padding to put between items"
  :type 'integer)

(defface ido-grid-match '((t (:background "#778899"))) "match face 1")
(defface ido-grid-match-1 '((t (:background "#5f9ea0" :weight bold))) "match face 2")
(defface ido-grid-match-2 '((t (:background "#8a2be2"))) "match face 3")
(defface ido-grid-match-3 '((t (:background "#8b3a3a" :weight bold))) "match face 4")

(defcustom ido-grid-match-faces '(ido-grid-match-1
                                  ido-grid-match-2
                                  ido-grid-match-3)
  "Faces to use for parts of matches")

(defvar ido-grid--coff 0)
(defvar ido-grid--crow 0)
(defvar ido-grid--ccol 0)

;;; Drawing

(defmacro ido-grid--name (item row col)
  `(let* ((name (substring (ido-name ,item) 0))
          (name (cond ((not name) "<nil>")
                      ((zerop (length name)) "<empty>")
                      (t name)))
          (l (length name)))

     (add-face-text-property 0 l standard-height nil name)

     (if (and (= ido-grid--crow ,row)
              (= ido-grid--ccol ,col))
         (add-face-text-property 0 l 'ido-first-match nil name)
       (when (ido-final-slash name)
         (add-face-text-property 0 l 'ido-subdir nil name)))

     ;; decorate match groups with the features

     (when (string-match decoration-regexp name)
       (ignore-errors
         ;; try and match each group in case it's a regex with groups
         (add-face-text-property (match-beginning 0)
                                 (match-end 0)
                                 'ido-grid-match
                                 nil name)
         (let ((group 1))
           (while (match-beginning group)
             (add-face-text-property (match-beginning group)
                                     (match-end group)
                                     (pop faces)
                                     nil name)
             (setq faces (or faces ido-grid-match-faces))
             (cl-incf group))
           )))
     name
     ))

(defmacro ido-grid--reset ()
  `(setq ido-grid--crow 0
          ido-grid--ccol 0
          ido-grid--coff 0))

(defmacro ido-grid--length (x)
  `(length ,x)) ;; TODO may need to strip invisbles

(defun ido-grid--grid (decoration-regexp items width rows &optional cols)
  "Generate the grid for ITEMS fitting into WIDTH text and ROWS lines with max COLS columns"

  ;; we definitely can't highlight a row we can't see
  (when (>= ido-grid--crow rows)
    (ido-grid--reset))

  (save-match-data
    (with-temp-buffer
     (unless (= 1 rows)
       (insert (make-string (- rows 1) ?\n)))
     (goto-char (point-min))
     (let ((faces ido-grid-match-faces)
           (standard-height `(:height ,(face-attribute 'default :height nil t)))
           (indent-tabs nil) (tab-width 1)
           (row 0) (column 0)
           (column-max 0)
           (target-column 2)
           (name-buffer (make-vector rows nil))
           item item-0-location)
       (while items
         (setq item (pop items))

         ;; store the items up
         (let* ((ti (ido-grid--name item row column))
                (tl (ido-grid--length ti)))
           ;; get location of zeroth item
           (when (and (zerop row) (zerop column))
             (setf item-0-location (cons target-column
                                         (+ target-column tl))))
           (aset name-buffer row ti)
           (setq row (1+ row)
                 column-max (max column-max tl)))

         ;; emit them into the temporary buffer
         (when (or (not items)
                   (= row rows))
           (let ((new-target (+ column-max
                                target-column
                                ido-grid-column-padding)))
             (if (and (> column 1)
                      (> new-target width))
                 (setq items nil) ;; die
               (progn ;; do the thing
                 (goto-char (point-min))
                 (end-of-line)
                 (dotimes (row row)
                   (insert " ")
                   (move-to-column target-column t)
                   (insert (aref name-buffer row))
                   (end-of-line 2))
                 (setq target-column new-target
                       row 0
                       column (1+ column))
                 (if (and cols (>= column cols))
                     (setq items nil))
                 )))

           ))
       ;; now we have the grid, we want to ensure something got the highlight face

       (when (and (>= ido-grid--ccol column)
                  item-0-location)
         ;; we went out of bounds
         (ido-grid--reset)
         (add-face-text-property (car item-0-location)
                                 (cdr item-0-location)
                                 'ido-first-match)))
     (goto-char (point-min))
     (insert "\n")
     (buffer-string))))

(defun ido-grid--completions (name)
  ;; handle no-match here

  (let ((ido-matches ido-grid--matches))
    (or (unless ido-matches
          (cond (ido-show-confirm-message  " [Confirm]")
                (ido-directory-nonreadable " [Not readable]")
                (ido-directory-too-big     " [Too big]")
                (ido-report-no-match       " [No match]")
                (t "")))

        (when ido-incomplete-regexp
          (concat " " (let ((name (substring (ido-name (car ido-matches)) 0)))
                        (add-face-text-property 0 (length name) 'ido-incomplete-regexp nil name)
                        name)))

        (when (not (cdr ido-matches))
          (let ((standard-height `(:height ,(face-attribute 'default :height nil t)))
                (name (substring (ido-name (car ido-matches)) 0)))
            (add-face-text-property 0 (length name) standard-height nil name)
            (add-face-text-property 0 (length name) 'ido-only-match nil name)
            (concat "\n  " name)))

        (ido-grid--grid (if ido-enable-regexp
                            ido-text
                          (regexp-quote name))
                        ido-matches
                        (- (window-body-width (minibuffer-window)) 1)
                        5 )
        ""))

  )

;;; Return value and offset

;; this is the awful bit where we change the match list up to a rotation

(defvar ido-grid--matches ())

(defun ido-grid--same-matches (x y)
  (when (equal (length x) (length y))
    (let ((a (car x))
          (y2 y))
      ;; find where y2 overlaps x
      (while (and y2
                  (not (equal a
                              (car y2))))
        (setq y2 (cdr y2)))
      (when y2 ;; if nil, a is not in y
        (while (and x
                    (equal (car x)
                           (car y2)))
          (setq x (cdr x)
                y2 (or (cdr y2) y)))
        (not x)))))

(defun ido-grid--rotate-matches-to (new-head)
  (let ((new-tail ido-grid-mode-rotated-matches))
    (while new-tail
      (setq new-tail
            (if (equal new-head
                       (cadr new-tail))
                (progn
                  (setq new-head (cdr new-tail))
                  (setcdr new-tail nil)
                  (setq ido-grid-mode-rotated-matches
                        (nconc new-head ido-grid-mode-rotated-matches))
                  nil)
              (cdr new-tail))))))

(defun ido-grid--output-matches ()
  "copy of matches with selected match at head"
  (let ((ido-grid--matches (copy-sequence ido-grid--matches)))
    (ido-grid--rotate-matches-to (ido-grid--selected-match))
    ido-grid--matches))

(defun ido-grid--selected-match ()
  (nth ido-grid--coff ido-grid--matches))

(defun ido-grid--set-matches (original &rest rest)
  (let ((might-change-something (or ido-rescan ido-use-merged-list))
        (might-merge-list ido-use-merged-list)
        (result-of-original (apply original rest)))

    (when might-change-something
      (if (ido-grid--same-matches ido-matches ido-grid--matches)
          (if (and might-merge-list
                   (not (eq (car ido-matches)
                            (ido-grid--selected-match))))
              (setq ido-matches (ido-grid--output-matches)))
        (setq ido-grid--matches (copy-sequence ido-matches)))
      )))

;;; Keys and movement

(defun ido-grid--move (dr dc)
  ;; this is all annoying
  )

;;; Setup and hooks

(defun ido-grid--setup ()
  (ido-grid--reset)

  )

(defun ido-grid-enable ()
  (advice-add 'ido-completions :override #'ido-grid--completions)
  (advice-add 'ido-set-matches :around #'ido-grid--set-matches '(:depth -50))
  (add-hook 'ido-setup-hook #'ido-grid--setup))

(with-current-buffer "asdfasdf"
  (erase-buffer)
  (insert (ido-grid--grid
           "one"
           '("one" "two") 100 5)))
