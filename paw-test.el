;;; pen/pen-test.el -*- lexical-binding: t; -*-


(-difference
 '("world" "good")
 '("world" "hello")
 )


(-difference
 '("world" "hello")
 '("world")
 )

(cl-set-difference
 (cl-loop for item in (pen-db-sql [:select word :from items :order :by ROWID]) collect
          (car item))
 (cl-loop for entry in (pen-parse-json json) collect
          (alist-get 'word entry))
 :test #'equal)

(pen-parse-json (json-read-file pen-json-file ))

(let ( (entries '(((word . "HELLO") (exp . "GOOD"))
                  ((word . "WORLD") (exp . "MEN"))

                  )) )
  (cl-loop for entry in entries collect

           (cl-map 'array #'identity (list (alist-get 'word entry) 0))
           )
  )


(pen-db-insert '(((word . "TEST") (exp . "GOOD"))
                  ((word . "TEST2") (exp . "GOOD2"))))
(pen-db-delete ["TEST" "TEST2" ])
(pen-db-delete ["insert" "pen" ])

(pen-db-online "join")
(pen-db-online [ "join" "clamp" ])

(message "%s" [ "join" "clamp" ])

(pen-db-sql '[:select [ word note ] :from status :where (= origin_path "/Users/damonchan/org/Doc/Calibre/Elad Elrom/The Blockchain Developer (672)/The Blockchain Developer - Elad Elrom.epub")])

(benchmark 1 '(pen))

(benchmark 1 '(nov-goto-document 9) )
(benchmark 1 '(nov-render-document) )

(benchmark 1 '(pen-annotation-mode 1))

(benchmark 1 '(pen-view-note))
