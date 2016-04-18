;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Group Name: Bunny Onion          Date: April 15, 2016
;;;; Members: Jacob Dalton, Lauren Ogawa, Michele Takasato
;;;; Course: ICS313                   Assignment: 6 (Project)
;;;; File name: <BunnyOnionProject.lisp>
;;;;
;;;; The Wizard's World
;;;; The following code is from the wizards_part1.lisp and 
;;;; wizards_part1.lisp code.
;;;; It has been commented and modified so that there is a new
;;;; location and item in the game. It has been further modified
;;;; so that there is a mulit-part object, with at least one part
;;;; in every area.
;;;;
;;;; Ignore this. This is for my personal use to load the file easily.
;;;; (load "BunnyOnionProject.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             ????                                 ;;;;
;;;; Don't know if we need this section                               ;;;;
;;;; Inital value to supress warning of defconstant being overwriten. ;;;;
;;;; Basically a warning that my name is being rewritten.             ;;;;
;;;; I got help with how to resolve the issue at the following site:  ;;;;
;;;; http://www.clisp.org/impnotes/defconstant.html                   ;;;;
;;;;(setf *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* 't)        ;;;;
;;;;                                                                  ;;;;
;;;; Globally defined variables                                       ;;;;
;;;;(defconstant +ID+ "Jacob Dalton, Lauren Ogawa, Michele Takasato") ;;;;
;;;;                                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; These are the nodes of the places the user can visit. This just
;;; gives a short description to the user so that they know where
;;; they are in the game. It also may give some information about
;;; what is around them, possibly for a more immersive play?
(defparameter *nodes* 
  ;; Basically a list of places and a short blurb about that area.
  ;; rooms for the first floor
  '((room1-A2 (You are in a room filled with books with papers littering the floor. You wonder what could have happened here.))
  (room1-B2 (You are in a small dimly lit room. Nothing catches your eye in particular.))
  (room1-C2 (You are in a dark room with al the draperies closed. You pull them aside and peer out the window. You see another tower.))           
  (room1-C4 (You are in a well lit room. You look out the window to see another tower with dark windows.))
  (room1-D4 (you are in a room with a hole in the floor.))
  (room1-E4 (you are in a room filled with portraits... none of them look familiar.))

  ;; rooms for the second floow
  (room2-A1 (you are in a very dark room with a strange smell coming from somewhere. You inch forward. The hair rises on the back of your neck as your foot hits something.))
  (room2-A2 (you are in a dimly lit room. The air is very still.))
  (room2-A4 (you are in a kitchen.))
  (room2-B1 (you are in some sort of dungeon.))
  (room2-C1 (you are in a room with many doors.))
  (room2-D1 (you are in the room with a furnace but there is no fire. ))
  (room2-E3 (you find yourself in a room glowing with treasures. You realize you dont have pockets...))


  ;; hallway nodes for the first floor
  (hall1-A1 (You are facing a dragon which is blocking your only exit.))
  (hall1-B1 (you are at the beginning of a long hallway. You can feel the dragons eyes on you.))
  (hall1-C1 (you are in the middle of a long hallway. You look out the windows to see a lake that is glittering in the sunlight.))
  (hall1-D1 (you are at the end of the hallway... but at the beginning of another.))
  (hall1-D2 (you find yourself getting tired of hallways...))
  (hall1-D3 (You are in a hallway. You feel like you have been going in circles.))


  ;; hallway nodes for the second floor
  (hall2-B2 (You find yourself in a hallway.))
  (hall2-B3 (You are in a hall. You feel the air getting cooler.))
  (hall2-C2 (You are in a hall with many doors.))
  (hall2-D2 (You are in a dimly lit hallway.))
  (hall2-D3 (You find yourself in a short hallway.))
  (hall2-D4 (You are at the beginning of another hallway. You wonder what happened to the original ladder that was here.))
  )) 

;;; This is the function that prints out a description of the current
;;; location using the one of the nodes above.
(defun describe-location (location nodes)
	(cadr (assoc location nodes)))

;;; This is a list telling us what is connected to what. This is
;;; basically shaping the "room/space" of each area.
(defparameter *edges* 
	;; A list defining where the user is currently at and what areas they can reach,
	;; where that area can be reached and how to get there. (i.e. if the user is in 
	;; the attic, they can reach the living room downstairs via a ladder.)
	
	;;; This is the first floor

	;; Hallway
	'((hall1-A1 (hall1-B1 south hall))
		(hall1-B1 (hall1-A1 north hall) (hall1-C1 south hall))
		;;contains the rope
		(hall1-C1 (hall1-B1 north hall) (hall1-D1 south hall))
		(hall1-D1 (hall1-C1 north hall) (hall1-D2 east hall))
		;;contains the saw
		(hall1-D2 (hall1-D1 west hall) (hall1-D3 east hall))
		(hall1-D3 (hall1-D2 west hall) (room1-D4 east hall))
		
		;; First room, locked need key
		(room1-C2 (hall1-C1 west door) (room1-B2 north door))
		(room1-B2 (room1-C2 south door) (room1-A2 north door))
		;;contains room2-E3 key/key3
		(room1-A2 (room1-B2 south door))
		
		;; Not sure how to allow user to jump from the first floor to the second...
		;; End of the first floor and the room to get to the second floor via ladder
		(room1-D4 (hall1-D3 west door) (room1-C4 north door) (room1-E4 south door)) 

		;;contains glue
		(room1-C4 (room1-D4 south door))
		;;contains board
		(room1-E4 (room1-D4 north door))
		
		;;; This is the second floor

		;; Second floor hallway
		(hall2-D4 (hall2-D3 west hall))
		(hall2-D3 (hall2-D4 east hall) (hall2-D2 west hall))
		(hall2-D2 (hall2-D3 east hall) (hall2-C2 north hall))
		(hall2-C2 (hall2-D2 south hall) (hall2-B2 north hall) (room2-C1 west hall))
		(hall2-B2 (hall2-C2 south hall) (hall2-B3 east hall) (room2-A2 north hall))
		(hall2-B3 (hall2-B2 west hall) (hall2-B4 east hall))
		(hall2-B4 (hall2-B3 west hall) (room2-A4 north door))
		
		;; Small closet room, also locked, contains hall1-A1 key/key4
	;;;;(room2-E3 (hall2-D3 north door))
		
		;; Next room contains a key and trap object... I think it was weapons to
		;; "slay" the dragon
		(room2-C1 (hall2-C2 east door) (room2-D1 south door) (room2-B1 north door))
		;;contains weapons
		(room2-D1 (room2-C1 north door))
		;;contains hall2-E3 key/key1
		(room2-B1 (room2-C1 south door))

		;; This is a smaller room on the second floor, contains a key
		(room2-A2 (hall2-B2 south door) (room2-A1 west door))
		;;contains hall1-C2 key/key2
		(room2-A1 (room2-A2 east door))

		;; The salad
	;;;;(room2-A4 (hall2-B4 south door))
		))

;;; This function puts the information in *edges* into basic sentences
;;; to tell the user what, where, and how to reach other areas in the
;;; game.
(defun describe-path (edge)
	`(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;; This function appends all the areas that can be reached from the
;;; current location.
(defun describe-paths (location edges)
	(apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;; Create all the objects that the user can pick up
;;; Added a multi-part object to go fishing, because I imagine that a wizard
;;; lives somewhere that is not easily accessed by others and would therefore
;;; have to procure his own food. This wizard is probably also bored and needed
;;; a hobby.
;;; Added parts of a coffee maker so the wizard can take some coffee while fishing.
(defparameter *objects* '(key1 key2 key3 key4 rope board saw magical-glue salad weapons))

;;; Where each object can be found

(defparameter *object-locations* 
	'((rope hall1-C1)
		(saw hall1-D2)
		(magical-glue room1-C4)
		(board room1-E4)
		(weapons room2-D1)
		(key1 room2-B1)     ;; key for 
		(key2 room2-A1)     ;; key for hall1-C1 to enter room1-C2
		(key3 room1-A2)     ;; key for hall2-B4 to enter room2-A4
		(salad room2-A4)    
		(key4 room2-E3)))   ;; key for hall1-A1 to exit game

;;; This function gives a list of the visible objects at a current
;;; location.
(defun objects-at (loc objs obj-loc)
	(labels ((is-at (obj)
    (eq (cadr (assoc obj obj-loc)) loc)))                 ; Check if the object is at the current location
  (remove-if-not #'is-at objs)))                        ; Remove objects not at the current location

;;; This function describes where an item is at the current location.
(defun describe-objects (loc objs obj-loc)
	(labels ((describe-obj (obj)
		`(you see a ,obj on the floor.)))
   ;; If there is more than one object at the current location, we want the
   ;; the user to be able to know about that too!
   (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;; The user starts out in hall1-A1 every time.
(defparameter *location* 'hall1-A1)

;;; This function basically compiles all the information from previous
;;; functions. This tells the user all they need to know about the
;;; current location.
(defun look ()
	(append
    (describe-location *location* *nodes*)                  ; Description of the current location.
    (describe-paths *location* *edges*)                   ; Where they can go from this location.
    (describe-objects *location* *objects* *object-locations*)))      ; What objects or items are at this location

;;; This function basically moves the user to the area they wish to move
;;; to given a direction.
(defun walk (direction)
  (labels ((correct-way (edge)                        ; Check to see if the path exists
  	(eq (cadr edge) direction)))
  (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
    ;; Tell the user that they can't go that way if that path does not
    ;; exist.
    (if next                                ; If they can go...
      (progn (setf *location* (car next))                 ; Move to the desired location.
        (look))                             ; Give them a descriptions of what's there.
      '(you cannot go that way.)))))                    ; Else they can't go that way.

;;; This function allows the user to pick up an object at the current location.
(defun pickup (object)
	(cond ((member object (objects-at *location* *objects* *object-locations*))
		(push (list object 'body) *object-locations*)
    ;; Let the user know if they've successfully picked up an item or not.
    `(you are now carrying the ,object))
	(t '(you cannot get that.))))

;;; This functions gives a list of items/objects that they have picked up.
(defun inventory ()
	(cons 'items- (objects-at 'body *objects* *object-locations*)))

;;; This function tells the user whether they have a specific item or not.
(defun have (object)
	(member object (cdr (inventory))))

;;;; The Wizard's World Part 2
;;;;
;;;; Comment these sections and test the following code.
;;;; Can you break it? What tests will you run to find out?

;;; This function REPL, basically it reads, evaluates, and prints in a
;;; loop.
;;; Basically this calls the other functions below it so we can play 
;;; the game without having to type parentheses or quotes a.k.a.
;;; better game play.
(defun game-repl ()
    (let ((cmd (game-read)))                          ; Reads the game
        (unless (eq (car cmd) 'quit)                      ; End the game by typing in quit
            (game-print (game-eval cmd))                    ; This evaluates the commands and prints the appropriates response(s)
            (game-repl))))                            ; Loop so you can continue playing.

;;; This function reads the users input and removes the need to type
;;; parentheses or quotes.
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))   ; Read the user's input and put's it within parentheses
         (flet ((quote-it (x)                           ; I believe this lets you quote things without having to type it
         	(list 'quote x)))
         (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;; These are the possible commands that the user is allowed to use.
;;; This prevents the user from using other commands that we don't
;;; want them to use.
;;; Added more commands (help h ?) that the user is able to use.
(defparameter *allowed-commands* '(look walk pickup inventory help h cut unlock eat saw glue ?))

;;; This function evaluates the input from the user and checks if it's
;;; in the list of allowed commands. Else it prints out a comment
;;; telling the user that they basically can't use that command because
;;; it's not in the approved list of commmands they can use.
(defun game-eval (sexp)

  ;; Since the function help exists, we only need to check for h and ?
  ;; This statement checks if the command typed in is h or ? and then
  ;; calls the help function
  ;; Originally this worked...then when I'm putting together my output
  ;; file it doesn't work... so back to the original plan of two other
  ;; functions tahat call the help function
  ;(if (or (eq sexp "h") (eq sexp "?"))
    ;(eval help)                              ; If the command is h or ? then call the help function

  (if (member (car sexp) *allowed-commands*)                  ; Check if it's within the allowed parameters
    (eval sexp)                               ; Evaluates the command
    '(i do not know that command.)))                    ; Return this if it's not within the parameters

;;; This function helps the game-print function with editing the
;;; responses printed to the user.
(defun tweak-text (lst caps lit)
	(when lst
		(let ((item (car lst))
			(rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))       ; Appropriate spacing
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))    ; Is it a ! ? or . for proper punctuation
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))        ; Change to uppercase
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))     ; Change to lowercase

;;; This function prints the responses to the users commands after editing
;;; them so that it looks better when playing the program.
;;; This gives it a better overall look when playing.
(defun game-print (lst)
	(princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
	(fresh-line))


;;;; The Wizard's World Part 3
;;;; Creating SPELS in Wizard's World
;;;; This is from the code titled "wizard_special_actions.lisp"
;;;; I believe that this code came from the Land of Lisp book and as such
;;;; I do not own any of it.

;;; This macro allows the user to do certain actions with the
;;; in-game objects
(defmacro game-action (command subj obj place &body body)
	`(progn (defun ,command (subject object)
		(if (and (eq *location* ',place)
			(eq subject ',subj)
			(eq object ',obj)
			(have ',subj))
		,@body
		'(i cant ,command like that.)))
  ;; Make sure that each action's command is added to the list
  ;; of actions the user can use.
  (pushnew ',command *allowed-commands*)))


;;;This macro allows the user to do cetrain actions with
;;;a single object versus the previous macro with
;;;two different objects.
(defmacro game-use (command obj place &body body)
	`(progn (defun ,command (object)
		(if (and (eq *location* ',place)
			(eq object ',obj)
			(have ',obj))
		,@body
		'(i cant ,command like that.)))
  ;; Make sure that each action's command is added to the list
  ;; of actions the user can use.
  (pushnew ',command *allowed-commands*)))

;;;; parameters to check if door is unlocked
(defparameter *unlocked1* nil)
(defparameter *unlocked2* nil)
(defparameter *unlocked3* nil)
(defparameter *unlocked4* nil)
(defparameter *eaten* nil)

;;;; action to eat salad
(game-action eat salad myself room2-A4
	(if (and (have 'salad) (not *eaten*))
    ;; Make sure that the game knows completed
    ;; 
    (progn (setf *eaten* 't)
    	'(you have eaten the salad))
    '(you either do not have the salad or are not in room2-A4.)))
    
;;;; Action that tries to "slay" the dragon
;;;;needs testing
(game-action slay weapons dragon hall1-A1
	(cond 
	(not(have 'weapons) '(you do not have weapons to slay the dragon with!))
	(t '(you try to slay the dragon but he is grumpy! He is too powerful and you lose. The end.)
    )))

;;;; action to unlock door at hall2-D3  room2-b1
(game-action unlock key1 lock1 hall2-D3
	(if (and (have 'key1) (not *unlocked1*))
    ;; Make sure that the game knows completed
    ;; 
    (progn (setf *unlocked1* 't)
    	'(the door to room2-E3 has been unlocked)
         (pushnew '(hall1-C1 (hall1-B1 north hall) (hall1-D1 south hall) (room1-C2 east door)) *edges*)
        )
    '(you do not have key1.)))

;;;; action to unlock door at hall1-C1
(game-action unlock key2 lock2 hall1-C1
	(if (and (have 'key2) (not *unlocked2*))
    ;; Make sure that the game knows completed
    ;; 
    (progn (setf *unlocked2* 't)
    	'(the door to room1-C2 has been unlocked))
    '(you do not have key2.)))

;;;; action to unlock door at hall2-B4
(game-action unlock key3 lock3 hall2-B4
	(if (and (have 'key3) (not *unlocked3*))
    ;; Make sure that the game knows completed
    ;; 
    (progn (setf *unlocked3* 't)
    	'(the door to room2-A4 has been unlocked))
    '(you do not have key3.)))

;;;; action to unlock door at hall1-A1
(game-action unlock key4 lock4 hall1-A1
	(if (and (have 'key4) (not *unlocked4*))
    ;; Make sure that the game knows completed
    ;; 
    (progn (setf *unlocked4* 't)
    	'(The exit has been unlocked.))
    '(you do not have key4.)))

;;; Basically check if the rope has been cut into two pieces
(defparameter *two-ropes* nil)

;;; Action that cuts the rope using the saw
(game-action cut saw rope room1-D4
	(if (and (have 'rope) (not *two-ropes*))
    ;; Make sure that the game knows completed
    ;; the cutting and tells the user too
    (progn (setf *two-ropes* 't)
    	'(the rope is now cut into two pieces.))
;;        (push (list 'two-ropes 'body) *object-locations*))
    '(you do not have a rope or a saw.)))

;;; Basically check if the board has been cut into multiple pieces
(defparameter *board-pieces* nil)

;;; Action that cuts the board into pieces
(game-action saw saw board room1-D4
	(if (and (have 'board) (not *board-pieces*))
    ;; Make sure that the game knows completed
    ;; the cutting and tells the user too
    (progn (setf *board-pieces* 't)
    	'(the board is now cut into multiple pieces.))
    '(you do not have a board or a saw.)))

;;; Basically check if the ladder has been created
(defparameter *ladder* nil)

;;; Not sure it I can have two items in the have....will have to test
;;; Error problem
;;; Action that creates the ladder
(game-action create rope board room1-D4
    (cond 
        ( (equal *ladder* t)
            '(you already made the ladder))
        ((not (equal *two-ropes* t))
            '(you did not cut the rope))
        ((not (equal *board-pieces* t))
            '(you did not cut the board))
        (t (progn (setf *ladder* 't)
            '(you created a ladder.)))))

;;; This is to put the ladder in place and create a new path.
(game-use glue magical-glue room1-D4
    (cond
         ((not (equal *ladder* t))
          '(you did not make the ladder yet.))
         ((not (have 'magical-glue))
          '(you do not have the magical-glue.))
    ;; Make sure that the game knows completed
    ;; the cutting and tells the user too
    (t (progn (new-path room1-D4 down hall2-D4 ladder)
           (new-path hall2-D4 up room1-D4 ladder)
    	    '(success)))))

;;; This function helps the user by giving them a list of commands that they
;;; are allowed to use. h and ? are other commmands that the user can use to
;;; call this function.
(defun help ()
	(terpri)
  ;; Using the game-print function to make sure that everything prints out
  ;; nicely. Tried to get rid of the printed nil, but couldn't...
  (game-print '(please enter quit or one of the following commands)) 
  (game-print `( ,@*allowed-commands*))
  (terpri))

;;; Originally the two functions below were axed when I thought I figured
;;; out a simpler way of calling the help function when given h or ? as a 
;;; command.
;;; All these functions do is call the help function...
(defun h ()
	(help))

(defun ? ()
	(help))

;;;; Macro's to make creating the game easier
;;;; Used the following site to look up how the push function works
;;;; http://clhs.lisp.se/Body/m_push.htm
;;;; (It's really useful for adding items to the front of the list)

;;; This macro makes adding new objects to the game easier. 
(defmacro new-object (object location)
	`(cond
    ;; Make sure that it doesn't already exist in the game.
    ;; If it exists let the user know
    ((member ',object *objects*)
    	'(the ,object already exists.))
    ;; Make sure that the location where you want to create
    ;; and put the object exists too
    ;; Had to re-look up assoc. Used the following site:
    ;; http://clhs.lisp.se/Body/f_assocc.htm
    ((not (assoc ',location *nodes*))
    	'(the ,location does not exist.))
    ;; If the object doesn't yet exist and the location
    ;; exists create the new object in the location
    ;; Let the user know that the object was created
    ((push ',object *objects*)
    	(push '(,object ,location) *object-locations*)
    	'(the ,object was added to the ,location ))))

;;; This macro makes adding a new location to the game easier.
(defmacro new-location (location &body body)
	`(cond
    ;; Check to make sure that the location doesn't already
    ;; exist and let the user know if it does.
    ((assoc ',location *nodes*)
    	'(I'm sorry but that location already exists.))
    ;; Make sure that there is a description to go with the
    ;; new location so that we can immerse ourselves in the 
    ;; game play
    ((null ',body)
    	'(You need a description of the new location.))
    ;; If the location doesn't yet exist add it to the 
    ;; nodes and edges parameters
    ;; Also let the user know that the location has 
    ;; been added
    ((push '(,location (,@body)) *nodes*)
    	(push '(,location) *edges*)
    	'(the ,location location was added.))))

;;; This macro creates a path from one location to another.
;;; I know we were supposed to be able to make 2-way paths and 1-way paths...
;;; Sadly I was only able to make it create a single path at a time. So if the
;;; user wants a 2-way path they have to do this macro twice.
(defmacro new-path (start direction destination via)
	`(cond
    ;; Make sure both locations exist and if not let the user know.
    ((not (or (assoc ',start *nodes*) (assoc ',destination *nodes*)))
    	'(I'm sorry but one or both of the locations do not exist.))
    
    ;; Make sure that the path they want to create doesn't already exist
    ;; Used the following site for the code below since assoc wasn't cutting
    ;; it for making sure the path didn't already exist.
    ;; http://www2.hawaii.edu/~jgarces/ICS313/jgarces5/jgarces5.lisp
    ((member ',direction (mapcar #'cadr (cdr (assoc ',start *edges*))))
    	'(I'm sorry but that path already exists.))

    ;; If both locations exist and a path doesn't already exist between them
    ;; create a path.
    ((nconc (assoc ',start *edges*) (list (list ',destination ',direction ',via)))
    	'(the new via was added.))

    ;; Probably needs some sort of loop to change the direction to the opposite one
    ;; but I wasn't too sure how to make it optional so that the user has to the 
    ;; option of creating a 1-way or 2-way path. Perhaps a new parameter passed through
    ;; the macro that checks for 1 or 2 and then branch off into everything else?
    ;((nconc (assoc ',destination *edges*) (list (list ',start ',direction ',via)))
    ; '(the new via was added.))
    ))
