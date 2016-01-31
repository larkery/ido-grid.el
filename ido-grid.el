(defcustom ido-grid-functions-using-matches
  '(ido-kill-buffer-at-head
    ido-delete-file-at-head
    ido-exit-minibuffer)
  "What functions need `ido-matches' to look right before they are called"
  :type 'hook)

(defcustom ido-grid-column-padding 3
  "How many columns of padding to put between items"
  :type 'integer)

(defcustom ido-grid-rows 0.15
  "How many rows to show. If a float, is a proportion of the frame height."
  :type 'number)

(defcustom ido-grid-start-small t
  "Whether to start ido-grid in a small size")

(defcustom ido-grid-max-columns nil
  "Max column count (or nil)")

(defcustom ido-grid-vertical-commands ()
  "Commands which will be advised to show in a vertical list"
  :type 'hook)

(defface ido-grid-common-match '((t (:inherit shadow))) "face for match prefix")
(defface ido-grid-match '((t (:inherit underline))) "match face 1")
(defface ido-grid-match-1 '((t (:background "#104e8b" :weight bold))) "match face 2")
(defface ido-grid-match-2 '((t (:background "#2e8b57" :weight bold))) "match face 3")
(defface ido-grid-match-3 '((t (:background "#8b8b00" :weight bold))) "match face 4")

(defcustom ido-grid-match-faces '(ido-grid-match-1
                                  ido-grid-match-2
                                  ido-grid-match-3)
  "Faces to use for parts of matches")

(defvar ido-grid--selection nil)
(defvar ido-grid--selection-offset 0)
(defvar ido-grid--cells 0)
(defvar ido-grid--match-count 0)

(defvar ido-grid--rows 0)
(defvar ido-grid--cols 0)

(defvar ido-grid--is-small nil)

;;; Drawing

(defmacro ido-grid--name (item)
  `(let* ((name (substring (ido-name ,item) 0))
          (faces ido-grid-match-faces)
          (name (cond ((not name) "<nil>")
                      ((zerop (length name)) "<empty>")
                      (t name)))
          (l (length name)))

     (add-face-text-property 0 l standard-height nil name)

     (if (eq item ido-grid--selection)
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
             (setq group (1+ group)))
           (when (= group 1)
             (add-face-text-property (match-beginning 0)
                                     (match-end 0)
                                     (car faces)
                                     nil name))
           )))
     name
     ))

(defmacro ido-grid--length (x)
  `(length ,x)) ;; TODO may need to strip invisbles

;;;; The main grid drawing code

(defun ido-grid--grid (decoration-regexp items width rows &optional cols)
  "Generate the grid for ITEMS fitting into WIDTH text and ROWS lines with max COLS columns"
  (save-match-data
    (with-temp-buffer
     (unless (= 1 rows) (insert (make-string (- rows 1) ?\n)))
     (goto-char (point-min))
     (let ((original-items items)
           (standard-height `(:height ,(face-attribute 'default :height nil t)))
           (indent-tabs nil) (tab-width 1)
           (row 0) (column 0)
           (column-max 0)
           (target-column 1)
           (name-buffer (make-vector rows nil))
           seen-selection
           selection-in-column
           item)
       (while items
         (setq item (pop items))
         (if (eq item ido-grid--selection)
             (setq selection-in-column t))

         ;; store the items up
         (let* ((ti (ido-grid--name item))
                (tl (ido-grid--length ti)))
           (aset name-buffer row ti)
           (setq row (1+ row)
                 column-max (max column-max tl)))

         ;; emit them into the temporary buffer
         (when (or (not items)
                   (= row rows))
           (let ((new-target (+ column-max
                                target-column
                                ido-grid-column-padding)))
             (if (and (> column 0)
                      (> new-target width))
                 (setq items nil) ;; die
               (progn ;; do the thing
                 (setq seen-selection (or seen-selection selection-in-column))
                 (goto-char (point-min))
                 (end-of-line)
                 (dotimes (row row)
                   (insert " ")
                   (move-to-column target-column t)
                   (insert (aref name-buffer row))
                   (end-of-line 2))
                 (setq target-column new-target
                       row 0
                       column-max 0
                       column (1+ column))
                 (if (and cols (>= column cols))
                     (setq items nil))
                 )))

           ))

       ;; now we have the grid, we want to ensure something got the highlight face
       (setq ido-grid--cols column)
       (setq ido-grid--rows (if (= column 1) (min ido-grid--match-count rows) rows))
       (setq ido-grid--cells (min (* ido-grid--rows ido-grid--cols)
                                  ido-grid--match-count))

       (if seen-selection
            (progn
              (goto-char (point-min))
              (insert "\n")
              (buffer-string))
          (progn
            (ido-grid--column-shift 1)
            (ido-grid--grid decoration-regexp ido-grid--matches width rows cols))
          )))))

;;;; the completion code

(defun ido-grid--completions (name)
  ;; handle no-match here

  (let ((ido-matches ido-grid--matches))
    (setq ido-grid--match-count (length ido-matches))
    (concat
     (if (and (stringp ido-common-match-string)
              (> (length ido-common-match-string)
                 (length name)))
         (let ((x (substring ido-common-match-string (length name))))
           (add-face-text-property
            0 (length x)
            'ido-grid-common-match nil x)
           x)
       "")
     (format " (%d)" ido-grid--match-count)
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

         ;; todo make tall?
         (when (not (cdr ido-matches))
           (let ((standard-height `(:height ,(face-attribute 'default :height nil t)))
                 (name (substring (ido-name (car ido-matches)) 0)))
             (add-face-text-property 0 (length name) standard-height nil name)
             (add-face-text-property 0 (length name) 'ido-only-match nil name)
             (concat "\n " name)))

         (ido-grid--grid (if ido-enable-regexp
                             ido-text
                           (regexp-quote name))
                         ido-matches
                         (- (window-body-width (minibuffer-window)) 1)
                         (if ido-grid--is-small 1
                           (if (floatp ido-grid-rows)
                               (max 1 (round (* ido-grid-rows
                                                (frame-height))))
                             ido-grid-rows))
                         ido-grid-max-columns)
         "")))

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

(defun ido-grid--rotate (matches new-head)
  (let ((new-tail matches))
    (while new-tail
      (setq new-tail
            (if (equal new-head
                       (cadr new-tail))
                (progn
                  (setq new-head (cdr new-tail))
                  (setcdr new-tail nil)
                  (setq matches
                        (nconc new-head matches))
                  nil)
              (cdr new-tail))))
    matches))

(defun ido-grid--output-matches ()
  "copy of matches with selected match at head"
  (if (eq (car ido-grid--matches) ido-grid--selection)
      ido-grid--matches
    (ido-grid--rotate (copy-sequence ido-grid--matches)
                      ido-grid--selection)))


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
        (setq ido-grid--matches (copy-sequence ido-matches)
              ido-grid--selection (car ido-grid--matches)
              ido-grid--selection-offset 0))
      )))

;;; Keys and movement

(defun ido-grid--column-shift (n)
  (setq ido-grid--matches (ido-grid--rotate
                           ido-grid--matches
                           (if (< n 0)
                               (nth (+ ido-grid--match-count (* ido-grid--rows n))
                                    ido-grid--matches)
                             (nth (* ido-grid--rows n) ido-grid--matches)))

        ido-grid--selection-offset (- ido-grid--selection-offset
                                      (* ido-grid--rows n))))

(defun ido-grid--select (offset)
  (setq ido-grid--selection-offset (+ ido-grid--selection-offset offset))

  (if (> ido-grid--match-count ido-grid--cells)
      (cond ((< ido-grid--selection-offset 0)
             (ido-grid--column-shift (/ offset ido-grid--rows)))
            ((>= ido-grid--selection-offset ido-grid--cells)
             (ido-grid--column-shift (/ offset ido-grid--rows))))


    (when (not (< -1 ido-grid--selection-offset ido-grid--cells))
      (setq ido-grid--selection-offset (% ido-grid--selection-offset ido-grid--cells))
      (if (< ido-grid--selection-offset 0) (setq ido-grid--selection-offset (+ ido-grid--selection-offset
                                                                               ido-grid--cells)))))

  (setq ido-grid--selection (nth ido-grid--selection-offset ido-grid--matches)))

(defun ido-grid-right ()
  (interactive)
  (ido-grid--select ido-grid--rows))

(defun ido-grid-left ()
  (interactive)
  (ido-grid--select (- ido-grid--rows)))

(defun ido-grid-up ()
  (interactive)
  (ido-grid--select -1))

(defun ido-grid-down ()
  (interactive)
  (ido-grid--select 1))

(defun ido-grid-cannot-complete ()
  (interactive)
  (if (and ido-grid--is-small
           (< ido-grid--cells ido-grid--match-count))
      (setq ido-grid--is-small nil)
    (ido-grid-down)))

(defun ido-grid-expand ()
  (interactive)
  (setq ido-grid--is-small nil))

;;; Setup and hooks

(defun ido-grid--modify-matches (o &rest args)
  (setq ido-matches (ido-grid--output-matches))
  (apply o args))

(defun ido-grid--setup ()
  (setq ido-grid--is-small ido-grid-start-small
        ido-grid--selection (car ido-grid--matches)
        ido-grid--selection-offset 0
        ido-grid--nitems 0
        ido-grid--citems 0
        ido-grid--nrows 0
        ido-grid--ncols 0)

  (define-key ido-completion-map (kbd "<right>") #'ido-grid-right)
  (define-key ido-completion-map (kbd "<left>")  #'ido-grid-left)
  (define-key ido-completion-map (kbd "<up>")    #'ido-grid-up)
  (define-key ido-completion-map (kbd "<down>")  #'ido-grid-down)
  (define-key ido-completion-map (kbd "C-p")    #'ido-grid-up)
  (define-key ido-completion-map (kbd "C-n")  #'ido-grid-down))

(defun ido-grid--vertical (o &rest args)
  (let ((ido-grid-start-small nil)
        (ido-grid-max-columns 1))
    (apply o args)))

(defun ido-grid-enable ()
  (interactive)
  (advice-add 'ido-completions :override #'ido-grid--completions)
  (advice-add 'ido-set-matches :around #'ido-grid--set-matches '(:depth -50))

  (dolist (fn ido-grid-vertical-commands)
    (advice-add fn :around #'ido-grid--vertical))

  (dolist (fn ido-grid-functions-using-matches)
    (advice-add fn :around #'ido-grid--modify-matches))

  (add-hook 'ido-setup-hook #'ido-grid--setup))

(defun ido-grid-disable ()
  (interactive)
  (advice-remove 'ido-completions #'ido-grid--completions)
  (advice-remove 'ido-set-matches #'ido-grid--set-matches)

  (dolist (fn ido-grid-vertical-commands)
    (advice-remove fn #'ido-grid--vertical))

  (dolist (fn ido-grid-functions-using-matches)
    (advice-remove fn #'ido-grid--modify-matches))

  (remove-hook 'ido-setup-hook #'ido-grid--setup))
