(require 'ido-grid)

(ert-deftest
    ido-grid-one-column ()
  "A simple single column test"

  (let ((items '("one" "two" "three" "four" "five"))
        (ido-grid-indent 0))
    (should
     (equal
      (substring-no-properties
       
       (ido-grid--grid
        (car items)
        ""
        items
        10
        5))
      (concat "\n"
              (mapconcat
               (lambda (x) (concat x " "))
               items
               "\n")))
     )))

(ert-deftest
    ido-grid-columns-fit ()
  "Next column is removed"

  (let ((items '("one" "two" "three" "four" "five"
                 "seven" "eight" "nine" "ten"))
        (ido-grid-indent 0))
    (should
     (equal
      (substring-no-properties
       
       (ido-grid--grid
        (car items)
        ""
        items
        10
        5))
      (concat "\n"
              (mapconcat
               (lambda (x) (concat x " "))
               (butlast items 4)
               "\n")))
     )))


(ert-deftest
    ido-grid-columns-fit ()
  "Next column is removed"

  (let ((items '("one" "two" "three" "four" "five"
                 "six" "seven" "eight" "nine" "ten"))
        (ido-grid-indent 0)
        (ido-grid-column-padding 0))
    (should
     (equal
      (substring-no-properties
       
       (ido-grid--grid
        (car items)
        ""
        items
        100
        5))

      "
one   six
two   seven
three eight
four  nine
five  ten"
      )
     )))

