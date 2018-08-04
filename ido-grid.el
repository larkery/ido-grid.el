;;; ido-grid.el --- Ido candidate grid

;; Copyright (C) 2016 Tom Hinton

;; Author: T. G. Hinton <ido-grid@larkery.com>
;; Version: 1
;; Keywords: ido
;; URL: http://github.com/larkery/ido-grid.el

;;; Commentary

;; A simpler version of ido-grid-mode.el, which only does one kind of layout.

;;; Code:

;;;; variables from ido

(eval-when-compile
  ;; this is to make compile be quiet
  (defvar ido-show-confirm-message)
  (defvar ido-directory-too-big)
  (defvar ido-directory-nonreadable)
  (defvar ido-use-merged-list))

;;;; debug

(defmacro ido-grid--log-clear ()
   ;; `(with-current-buffer (get-buffer-create "*ido-grid-log*")
   ;;    (erase-buffer))
  )
(defmacro ido-grid--log (&rest args)
    ;; `(with-current-buffer (get-buffer-create "*ido-grid-log*")
    ;;        (insert (apply #'format (list ,@args)))
    ;;        (insert "\n")
    ;;        (goto-char (point-max)))
  )

;;;; our variables

(defgroup ido-grid nil
  "Displays ido prospects in a grid in the minibuffer"
  :group 'ido)

;;;###autoload
(defcustom ido-grid-enabled nil
  "Display ido prospects in a grid?"
  :type 'boolean
  :group 'ido-grid
  :require 'ido-grid
  :set (lambda (s v) (set-default s v)
         ;; do not invoke if not autoloaded yet?
         (when (featurep 'ido-grid)
           (if v (ido-grid-enable) (ido-grid-disable)))))

(defcustom ido-grid-functions-using-matches
  '(ido-kill-buffer-at-head
    ido-delete-file-at-head
    ido-exit-minibuffer)
  "What functions need `ido-matches' to look right before they are called"
  :type 'hook
  :group 'ido-grid)

(defcustom ido-grid-indent 1
  "How many columns indent on the left."
  :type 'integer
  :group 'ido-grid)

(defcustom ido-grid-column-padding 3
  "How many columns of padding to put between items."
  :type 'integer
  :group 'ido-grid)

(defcustom ido-grid-rows 6
  "How many rows to show."
  :type '(choice :tag "Number of rows"
                 (integer :tag "Exactly")
                 (float :tag "Proportion of frame"))
  :group 'ido-grid)

(defcustom ido-grid-start-small nil
  "Whether to start ido-grid in a small size by default.
When the grid is small, the up or down arrows will make it bigger."
  :group 'ido-grid)

(defcustom ido-grid-max-columns nil
  "How many columns to show."
  :type '(choice :tag "Columns"
                 (integer :tag "Maximum of")
                 (const :tag "As many as fit" nil))
  :group 'ido-grid)

(defun ido-grid--generic-advice (o &rest args)
  (let* ((cmd (assoc this-command ido-grid-special-commands))
         (ido-grid-rows (if cmd (nth 1 cmd) ido-grid-rows))
         (ido-grid-max-columns (if cmd (nth 2 cmd) ido-grid-max-columns))
         (ido-grid-start-small (if cmd (nth 3 cmd) ido-grid-start-small))
         (ido-grid-indent (if cmd (nth 4 cmd) ido-grid-indent)))
    (apply o args)))

;;;###autoload
(defun ido-grid--custom-advice (sym new-value)
  (ido-grid-change-advice new-value
                          (if (boundp 'ido-grid-special-commands)
                               ido-grid-special-commands nil))
  (set-default sym new-value))

(defun ido-grid-change-advice (new &optional old)
  (dolist (c old) (advice-remove (car c) #'ido-grid--generic-advice))
  (dolist (c new) (advice-add (car c) :around #'ido-grid--generic-advice)))

(defcustom ido-grid-special-commands ()
  "Special rules for some commands.
If you want some commands to pop-up differently (e.g. in a vertical list or horizontal row),
You can configure that in here; each entry is a command, and then alternative bindings for the layout variables."
  :set 'ido-grid--custom-advice
  :type '(repeat
          (list
           (function :tag "Command name")
           (choice :tag "Number of rows"
                   (integer :tag "Exactly")
                   (float :tag "Proportion of frame"))
           (choice :tag "Columns"
                   (integer :tag "Maximum of")
                   (const :tag "As many as fit" nil))
           (boolean :tag "Start small")
           (integer :tag "Indent")))
  :group 'ido-grid)

(defface ido-grid-common-match '((t (:inherit shadow))) "Face for the common prefix (text that is inserted if you press tab)"
  :group 'ido-grid)
(defface ido-grid-match '((t (:inherit underline))) "Face for the whole matching part of a candidate"
  :group 'ido-grid)
(defface ido-grid-match-1 '((t (:background "#104e8b" :weight bold))) "Face for first, fourth, ... match group"
  :group 'ido-grid)
(defface ido-grid-match-2 '((t (:background "#2e8b57" :weight bold))) "Face for second, fifth, ... match group"
  :group 'ido-grid)
(defface ido-grid-match-3 '((t (:background "#8b8b00" :weight bold))) "Face for third, sixth, ... match group"
  :group 'ido-grid)

(defcustom ido-grid-match-faces '(ido-grid-match-1
                                  ido-grid-match-2
                                  ido-grid-match-3)
  "Faces to use for parts of matches (usually these are the faces above - match groups cycle through this list.)"
  :type '(repeat face)
  :group 'ido-grid)

(defcustom ido-grid-bind-keys t
  "Bind the direction keys and C-n/C-p and `ido-cannot-complete-command'.
If you don't like these bindings, you can add a function to `ido-setup-hook' to bind the commands yourself.
See `ido-grid-up', `ido-grid-down', `ido-grid-left', `ido-grid-right' etc."
  :group 'ido-grid
  :type 'boolean)

(defvar ido-grid--selection nil)
(defvar ido-grid--selection-offset 0)
(defvar ido-grid--cells 0)
(defvar ido-grid--match-count 0)
(defvar ido-grid--text "")

(defvar ido-grid--rows 0)
(defvar ido-grid--cols 0)

(defvar ido-grid--is-small nil)
(defvar ido-grid--max-rows 0)

;;; Drawing

(defmacro ido-grid--name (item)
  `(let* ((name (substring (ido-name ,item) 0))
          (faces ido-grid-match-faces)
          (name (cond ((not name) "<nil>")
                      ((zerop (length name)) "<empty>")
                      (t name)))
          (l (length name)))

     (if (eq item ido-grid--selection)
         (add-face-text-property 0 l (if (= 1 ido-grid--match-count)
                                         'ido-only-match
                                       'ido-first-match) nil name)
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

(defun ido-grid--grid (ido-grid--selection
                       decoration-regexp
                       items
                       width
                       rows
                       &optional cols)
  "Generate the grid for ITEMS fitting into WIDTH text and ROWS lines with max COLS columns"
  (save-match-data
    (with-temp-buffer
     (unless (= 1 rows) (insert (make-string (- rows 1) ?\n)))
     (goto-char (point-min))
     (let ((original-items items)
           (standard-height `(:height ,(face-attribute 'default :height nil t)))
           (row 0) (column 0)
           (column-max 0)
           (target-column ido-grid-indent)
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
           (let ((trunc nil)
                 (new-target (+ column-max
                                target-column
                                1 ido-grid-column-padding)))
             (if (and (> column 0)
                      (> new-target width))
                 (setq items nil) ;; die
               (progn ;; do the thing
                 (setq trunc (> new-target width)) ;; if there is a single column, trim it
                 (setq seen-selection (or seen-selection selection-in-column))
                 (goto-char (point-min))
                 (end-of-line)

                 (dotimes (row row)
                   (unless (zerop column) (insert " "))
                   (move-to-column target-column t)
                   (insert (aref name-buffer row))
                   (when trunc
                     (let ((p (point)))
                       (move-to-column width)
                       (when (> p (point))
                         (delete-region (- (point) 1) p)
                         (insert "→"))))
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
              (add-face-text-property (point-min)
                                      (point-max)
                                      standard-height nil)
              (buffer-string))
         nil
          )))))


(defun ido-grid--effective-rows ()
  "Work out how many rows to allow for the grid at the moment"
  (cond
   (ido-grid--is-small 1)
   ((floatp ido-grid--max-rows)
    (max 1 (round (* ido-grid-rows
                     (frame-height)))))
   (t ido-grid--max-rows)))

(defun ido-grid--grid-ensure-visible ()
  "Generate the grid, shifting enough columns to ensure the current item is visible"
  (let (grid (shift 0))
    (while (not grid)
      (setq grid
            (ido-grid--grid ido-grid--selection
                            (if ido-enable-regexp ido-text (regexp-quote name))
                            ido-grid--matches
                            (- (window-body-width (minibuffer-window)) 1)
                            (ido-grid--effective-rows)
                            ido-grid-max-columns))
      (unless grid
        (ido-grid--shift ido-grid--rows t)))
    grid))

;;;; the completion code
(defun ido-grid--completions (name)
  ;; handle no-match here

  (let ((ido-matches ido-grid--matches))
    (setq ido-grid--cells 1)

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

        (let* ((grid (ido-grid--grid-ensure-visible)))
          (concat (if (and (stringp ido-common-match-string)
                           (> (length ido-common-match-string)
                              (length name)))
                      (let ((x (substring ido-common-match-string (length name))))
                        (add-face-text-property
                         0 (length x)
                         'ido-grid-common-match nil x)
                        x)
                    "")

                  (if (> (- ido-grid--match-count ido-grid--cells) 0)
                      (format " (%d more)%s"
                              (- ido-grid--match-count ido-grid--cells)
                              (if ido-grid--is-small " ↓" ""))
                    "")

                  grid)))))


;;; Return value and offset

;; this is the awful bit where we change the match list up to a rotation

(defvar ido-grid--matches ())

(defun ido-grid--matches-differ-from (x y y2)
  (while (and x (equal (car x) (car y2)))
      (setq x (cdr x)
            y2 (or (cdr y2) y)))
  x)

(defun ido-grid--same-matches (x y)
  (when (equal (length x) (length y))
    (ido-grid--log "ido-grid--same-matches length unchanged %d"
                   (length x))
    (let ((a (car x))
          (y2 y)
          (not-finished t))
      ;; find where y2 overlaps x
      ;; except, annoyingly there may be more than one place where they overlap.
      ;; thanks, python.el.
      (while (and not-finished y2)
        ;; scan y2 until it matches
        (while (and y2 (not (equal a (car y2))))
          (setq y2 (cdr y2)))
        (setq not-finished (ido-grid--matches-differ-from x y y2)
              y2 (cdr y2)))
      (not not-finished)
      )))

(defun ido-grid--rotate (matches new-head)
  (ido-grid--log "ido-grid--rotate to %s" new-head)
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
  (ido-grid--log "ido-grid--set-matches invoked")
  (let ((might-change-something (or ido-rescan ido-use-merged-list))
        (might-merge-list ido-use-merged-list)
        (result-of-original (apply original rest)))

    (when might-change-something
      (ido-grid--log "ido-grid--set-matches checking for change")
      ;; Changing ido-text might change faces of matches (flx-ido case)
      (if (and (string-equal ido-text ido-grid--text)
               (ido-grid--same-matches ido-matches ido-grid--matches))
          (progn
            (ido-grid--log "ido-grid--set-matches no changes")
            (ido-grid--log "ido-grid--set-matches %s %s %s"
                           might-merge-list
                           (car ido-matches)
                           ido-grid--selection)

            (when (and might-merge-list
                       (not (eq (car ido-matches)
                                ido-grid--selection)))
              (ido-grid--log "ido-grid--set-matches changing ido matches")
              (setq ido-matches (ido-grid--output-matches))))
        (progn
          (ido-grid--log "ido-grid--set-matches matches changed")
          (setq ido-grid--matches (copy-sequence ido-matches)
                ido-grid--match-count (length ido-matches)
                ido-grid--selection (car ido-grid--matches)
                ido-grid--selection-offset 0
                ido-grid--text ido-text)))
      ))

  (unless (and
           (< ido-grid--selection-offset ido-grid--match-count)
           (eq (nth ido-grid--selection-offset ido-grid--matches)
               ido-grid--selection))
    (setq ido-grid--selection (car ido-grid--matches)
          ido-grid--selection-offset 0)))

;;; Keys and movement

(defun ido-grid--+% (a b m)
  (let ((r (% (+ a b) m)))
    (if (< r 0) (+ r m) r)))

(defun ido-grid--shift (n &optional keep-offset)
  (ido-grid--log "ido-grid--shift %d %s" n keep-offset)
  (if (= ido-grid--cells ido-grid--match-count)
      ;; it does all fit but it may not be a rectangle.
      (progn
        (ido-grid--log "ido-grid--shift not rotating %d %d" ido-grid--cells ido-grid--match-count)
        (unless keep-offset
         (let ((new-offset (+ ido-grid--selection-offset n)))
           (setq new-offset
                 (cond
                  ((>= new-offset ido-grid--cells)
                   (% (+ 1 (% ido-grid--selection-offset ido-grid--rows)) ido-grid--rows))

                  ((< new-offset 0)
                   ;; we went off the left hand side
                   ;; we want to go up a row, and to the rightmost
                   ;; column in that row

                   (let ((r (+ (% (- (% ido-grid--selection-offset ido-grid--rows) 1)
                                  ido-grid--rows) ;; this is the row-offset
                               ;; we also need column index
                               ;; there are cells/rows columns
                               ;; but sometimes a bit less than that
                               ;; in the event that we have a remaindery bit.
                               (* ido-grid--rows (/ ido-grid--cells ido-grid--rows)))))
                     (if (>= r ido-grid--cells)
                         (- r ido-grid--rows)
                       r)))

                  (t new-offset)
                  ))

           (setq ido-grid--selection-offset new-offset
                 ido-grid--selection (nth new-offset ido-grid--matches)))))

    ;; it doesn't all fit, so we can always make a rectangle?
    (let* ((new-offset (ido-grid--+% ido-grid--selection-offset n
                                     ido-grid--match-count))
           (new-head (if (< n 0)
                         (- ido-grid--match-count ido-grid--rows)
                       ido-grid--rows)))

      (ido-grid--log "ido-grid--shift rotating %d %d %d %d"
                     ido-grid--selection-offset
                     new-offset
                     ido-grid--cells
                     new-head)
      ;; check if this offset is displayed on screen - if not we need it to be displayed
      ;; however, the thing to be selected is different - it should be the top of a row
      (when (>= new-offset ido-grid--cells)
        (setq ido-grid--matches
              (ido-grid--rotate ido-grid--matches
                                (nth new-head ido-grid--matches))
              new-offset (- new-offset new-head)))
      (ido-grid--log "ido-grid--shift rotated to %s" (car ido-grid--matches))
      (unless keep-offset
        (setq ido-grid--selection-offset new-offset
              ido-grid--selection (nth new-offset ido-grid--matches)
              )))))

(defun ido-grid-right ()
  (interactive)
  (ido-grid--shift ido-grid--rows))

(defun ido-grid-left ()
  (interactive)
  (ido-grid--shift (- ido-grid--rows)))

(defun ido-grid-up ()
  (interactive)
  (ido-grid--shift -1))

(defun ido-grid-down ()
  (interactive)
  (ido-grid--shift 1))

(defun ido-grid-down-or-expand ()
  (interactive)
  (if ido-grid--is-small
      (setq ido-grid--is-small nil)
    (ido-grid-down)))

(defun ido-grid-up-or-expand ()
  (interactive)
  (if ido-grid--is-small
      (setq ido-grid--is-small nil)
    (ido-grid-up)))

(defun ido-grid-expand ()
  (interactive)
  (setq ido-grid--is-small nil))

;;; Setup and hooks

(defun ido-grid--modify-matches (o &rest args)
  (setq ido-matches (ido-grid--output-matches))
  (apply o args))

(defvar ido-grid--prior-ccc nil)

(defun ido-grid-display-more-rows ()
  (interactive)
  (setq ido-grid--is-small nil
        ido-grid--max-rows
        (+ ido-grid--max-rows
           (if (floatp ido-grid--max-rows)
               1.0
             ido-grid-rows))))

(defun ido-grid--setup-minibuffer ()
  "Setup the minibuffer height in grid"

  (let ((inhibit-message t))
    (setq-local max-mini-window-height (max max-mini-window-height (1+ ido-grid-rows)))
    (setq-local resize-mini-windows t)))

(defun ido-grid--setup ()
  (ido-grid--log-clear)
  (ido-grid--log "ido-grid--setup %s" ido-cur-item)
  (setq ido-grid--is-small ido-grid-start-small

        ido-grid--max-rows ido-grid-rows

        ido-grid--selection nil
        ido-grid--selection-offset 0)

  (when ido-grid-bind-keys
    (setq ido-grid--prior-ccc ido-cannot-complete-command
          ido-cannot-complete-command #'ido-grid-down)
    (define-key ido-completion-map (kbd "<right>") #'ido-grid-right)
    (define-key ido-completion-map (kbd "<left>")  #'ido-grid-left)
    (define-key ido-completion-map (kbd "<up>")    #'ido-grid-up-or-expand)
    (define-key ido-completion-map (kbd "<down>")  #'ido-grid-down-or-expand)
    (define-key ido-completion-map (kbd "C-<up>")  #'ido-grid-display-more-rows)
    (define-key ido-completion-map (kbd "C-p")     #'ido-grid-up)
    (define-key ido-completion-map (kbd "C-n")     #'ido-grid-down)))

;;;###autoload
(defun ido-grid-enable ()
  (interactive)

  (make-local-variable 'max-mini-window-height)

  (advice-add 'ido-completions :override #'ido-grid--completions)
  (advice-add 'ido-set-matches :around #'ido-grid--set-matches '(:depth -50))

  (dolist (fn ido-grid-functions-using-matches)
    (advice-add fn :around #'ido-grid--modify-matches))

  (add-hook 'ido-setup-hook #'ido-grid--setup)
  (add-hook 'ido-minibuffer-setup-hook #'ido-grid--setup-minibuffer))


;;;###autoload
(defun ido-grid-disable ()
  (interactive)

  (setq ido-cannot-complete-command (or ido-grid--prior-ccc
                                        ido-cannot-complete-command)
        ido-grid--prior-ccc nil)

  (advice-remove 'ido-completions #'ido-grid--completions)
  (advice-remove 'ido-set-matches #'ido-grid--set-matches)

  (dolist (c ido-grid-special-commands)
    (advice-remove (car c) 'ido-grid--generic-advice))

  (dolist (fn ido-grid-functions-using-matches)
    (advice-remove fn #'ido-grid--modify-matches))

  (remove-hook 'ido-setup-hook #'ido-grid--setup)
  (remove-hook 'ido-minibuffer-setup-hook #'ido-grid--setup-minibuffer))

(when ido-grid-enabled (ido-grid-enable))

(provide 'ido-grid)

;;; ido-grid.el ends here
