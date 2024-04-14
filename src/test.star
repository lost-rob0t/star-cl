((-> (create document :id "2") (create document :id "2") "Test relation!")
 (create person :fname "john" :lname "doe" :bio "male"  :dataset "starintel"))


; note that a simple select could be the same as create but it will just make a query that
; takes all the key pairs, this could be used for mango queries or maybe ORM!

; Another idea for a dsl is zmq
; (define-zeromq (:type 'pub :listen "tcp://*:5000")
;     ())
