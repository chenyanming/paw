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

(benchmark 1 '(paw-view-note-current-thing))

"¿Qué hora es?"

"Buenos días."


"Lontemps je me suis couché de bonne heure.Parfois,à peine ma bougie éteinte, mes yeux se fermaient si vite que je je n’avait pas le temps de me dire:“je m’endors”Et, une demi-heure après,la pensée qu’il étaits temps de chercher le sommeil m’éveillait."

"Die „Albertina“ ist die Bibliothek der Universität Leipzig. Das Haus in der Beethoven-straße 6 ist alt, aber die Bibliothek ist sehr modern. Viele Studentinnen und Studenten arbeiten in den Lesesälen in der ersten Etage."


(defun paw-test-language()
  (assert (string= (paw-check-language "A wiki (/ˈwɪki/ ⓘ WI-kee) is a form of online hypertext publication that is collaboratively edited and managed by its own audience directly through a web browser.") "en") t "Wrong Langauge")
  (assert (string= (paw-check-language "대한의사협회(의협)의 집단 휴진에 불참하는 의사들이 속속 나오고 있다. 분만병의원협회, 대한아동병원협회에 이어 각 대학병원 뇌전증 전문 교수들이 모인 거점 뇌전증지원병원 협의체 역시 18일로 예정된 의협의 집단 휴진에 불  참한다.") "ko") t "Wrong Langauge")
  (assert (string= (paw-check-language "「不論人生或商場，各種情形都需要蒐集對的情報、進行建設性的思考，並做出正確決斷。本書提供一個從思考到決策的完整框架，可以幫助讀者避免受到常見盲點的影響，客觀做出正確的決定，提高人生和商場上各種試煉的打擊率，非常值得一讀。」") "zh") t "Wrong Langauge")
  (assert (string= (paw-check-language "距盐城湖一小时车程外，是犹他州一个名为伊甸的小镇，小镇上有一座名叫Powder的山。山上绿树成荫，白雪皑皑。2013年，一群20多岁的有志青年怀揣着400万美元来到这里，买下了1万英亩的土地。他们打算把这里建成一个生态住宅区，以吸引一些成功的企业家。") "zh") t "Wrong Langauge")
  )


(elp-instrument-function 'paw-view-note-current-thing)
(elp-instrument-function 'paw-focus-view-note-process-sentinel-english)
(elp-instrument-function 'paw-focus-find-current-thing-segment-english)
(elp-instrument-function 'paw-view-notes)
(elp-instrument-function 'paw-insert-note)
(elp-instrument-function 'paw-focus-find-unknown-words-sentinel-english)
(elp-reset-all)
(elp-results)

(elp-restore-all)



paw-focus-view-note-process-sentinel-english  1           33.963411668  33.963411668
paw-view-note-current-thing                   1           3.061395557   3.061395557
