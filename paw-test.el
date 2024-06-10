;;; paw-test.el -*- lexical-binding: t; -*-


(-difference
 '("world" "good")
 '("world" "hello")
 )


(-difference
 '("world" "hello")
 '("world")
 )

(cl-set-difference
 (cl-loop for item in (paw-db-sql [:select word :from items :order :by ROWID]) collect
          (car item))
 (cl-loop for entry in (paw-parse-json json) collect
          (alist-get 'word entry))
 :test #'equal)

(paw-parse-json (json-read-file paw-json-file ))

(let ( (entries '(((word . "HELLO") (exp . "GOOD"))
                  ((word . "WORLD") (exp . "MEN"))

                  )) )
  (cl-loop for entry in entries collect

           (cl-map 'array #'identity (list (alist-get 'word entry) 0))
           )
  )


(paw-db-insert '(((word . "TEST") (exp . "GOOD"))
                  ((word . "TEST2") (exp . "GOOD2"))))
(paw-db-delete ["TEST" "TEST2" ])
(paw-db-delete ["insert" "paw" ])

(paw-db-online "join")
(paw-db-online [ "join" "clamp" ])

(message "%s" [ "join" "clamp" ])

(paw-db-sql '[:select [ word note ] :from status :where (= origin_path "/Users/damonchan/org/Doc/Calibre/Elad Elrom/The Blockchain Developer (672)/The Blockchain Developer - Elad Elrom.epub")])

(benchmark 1 '(paw))

(benchmark 1 '(nov-goto-document 9) )
(benchmark 1 '(nov-render-document) )

(benchmark 1 '(paw-annotation-mode 1))

(benchmark 1 '(paw-view-note))
