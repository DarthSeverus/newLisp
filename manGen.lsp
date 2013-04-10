;; manGen.lsp - A script to grab the man pages from a txt file and put them into a db
;; Written by Sven Severini (2013)
;; Contact: http://www.contactify.com/26a0f
;;

;; I used the part of a text file from newlisp manual with the definitions, created with dump function of w3m browser 
;; First entry "!" will not be in db, must be added manually
;; Some articles include different functions, this must be corrected manually - like +, - must become two entrys, so that (man "+") works!  
;; The article about regex ist not working, db can't be loaded if it is into it


#!/usr/bin/env newlisp
(set-locale "C")

(setq text (read-file "/home/yourusername/textfile_of_definitions"))
(setq parsed (parse text "\n\n\n\n"))

; create headlines and print them for control
(dolist (temp parsed)
		(if (> (length temp) 2)
			(and
				(push ((parse temp "\n") 0) headlines -1)
				(println ((parse temp "\n") 0))
				; (sleep 500)
			)
		)
)

; clean headlines of extras like "utf8" and/or "!"
(dolist (temp headlines) 
		(when (and (ends-with temp " utf8") (not(ends-with temp " ! utf8")))  (push (chop temp 5) headlines2 -1))
		(when 	   (ends-with temp " !")							  		  (push (chop temp 2) headlines2 -1))
		(when 	   (ends-with temp " ! utf8") 							 	  (push (chop temp 7) headlines2 -1))
		(when (and (not (ends-with temp " !")) (not(ends-with temp " utf8"))) (push temp headlines2 -1))
		(inc hl-counter)
)

; create db
(new Tree 'man-content)
(setq counter 0)
(dolist (temp (1 headlines2))
	(eval (man-content temp (string (char 123) (parsed (+ counter 1) (char 125)))))
	(inc counter)
)

; testing
(dolist (temp headlines2) (if (nil? (eval-string (string {(man-content "} temp {")}))) (push temp failed -1)))

; function must be copied into man.lsp, which must then be loaded by users
(define (man input) (println (man-content input)) (setq output-man ""))


 
