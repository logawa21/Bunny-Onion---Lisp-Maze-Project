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
  '((room1-A2 (you are in ....))        ; The Living Room
  (room1-B2 (you are in ....))              ; The Garden
  (room1-C2 (you are in ....))           ; The Attic
  (room1-C4 (you are in ....))
  (room1-D4 (you are in ....))
  (room1-E4 (you are in ....))

  ;; rooms for the second floow
  (room2-A1 (you are in the room))
  (room2-A2 (you are in the room))
  (room2-A4 (you are in the room))
  (room2-B1 (you are in the room))
  (room2-C1 (you are in the room))
  (room2-D1 (you are in the room))
  (room2-E3 (you are in the room))

  ;; hallway nodes for the first floor
  (hall1-A1 (you are in the hallway))
  (hall1-B1 (you are in the hallway))
  (hall1-C1 (you are in the hallway))
  (hall1-D1 (you are in the hallway))
  (hall1-D2 (you are in the hallway))
  (hall1-D3 (you are in the hallway))

  ;; hallway nodes for the second floor
  
  ))               ; The Kitchen

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
	'((hall1-A1 (hall1-1B south hall))
		(hall1-B1 (hall1-A1 north hall) (hall1-C1 south hall))
		(hall1-C1 (hall1-B1 north hall) (hall1-D1 south hall) (room1-C2 east door))
		(hall1-D1 (hall1-C1 north hall) (hall1-D2 east hall))
		(hall1-D2 (hall1-D1 west hall) (hall1-D3 east hall))
		(hall1-D3 (hall1-D2 west hall) (room1-D4 east hall))
		
		;; First room, locked need key
		(room1-C2 (hall1-C1 west door) (room1-B2 north door))
		(room1-B2 (room1-C2 south door) (room1-A2 north door))
		(room1-A2 (room1-B2 south door))
		
		;; Not sure how to allow user to jump from the first floor to the second...
		;; End of the first floor and the room to get to the second floor via ladder
		(room1-D4 (hall1-D3 west door) (room1-C4 north door) (room1-E4 south door) (hall2-D4 downstairs ladder))
		(room1-C4 (room1-D4 north door))
		(room1-E4 (room1-D4 south door))
		
		;;; This is the second floor

		;; Second floor hallway
		(hall2-D4 (hall2-D3 west hall))
		(hall2-D3 (hall2-D4 east hall) (hall2-D2 west hall) (room2-E3 south door))
		(hall2-D2 (hall2-D3 east hall) (hall2-C2 north hall))
		(hall2-C2 (hall2-D2 south hall) (hall2-B2 north hall) (room2-C1 west hall))
		(hall2-B2 (hall2-C2 south hall) (hall2-B3 east hall) (room2-A2 north hall))
		(hall2-B3 (hall2-B2 west hall) (hall2-B4 east hall))
		(hall2-B4 (hall2-B3 west hall) (room2-A4 north door) (room2-A4 north hall))
		
		;; Small closet room, also locked
		(room2-E3 (hall2-D3 north door))
		
		;; Next room contains a key and trap object... I think it was weapons to
		;; "slay" the dragon
		(room2-C1 (hall2-C2 east door) (room2-D1 south door) (room2-B1 north door))
		(room2-D1 (room2-C1 north door))
		(room2-B1 (room2-C1 south door))

		;; This is a smaller room on the second floor, contains a key
		(room2-A2 (hall2-B2 south door) (room2-A1 west door))
		(room2-A1 (room2-A2 east door))

		;; The salad
		(room2-A4 (hall2-B4 south door))
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
;;; Added a multi-part object to go fishing

;;; Add a coffee maker parts?
(defparameter *object-locations* 
	'((whiskey living-room)
		(bucket living-room)
		(fishing-rod living-room)
		(coffee-pot living-room)
		(fishing-reel attic)
		(coffee-filter attic)
		(chain garden)
		(frog garden)
		(fishing-hook garden)
		(coffee-brew-basket garden)
		(water kitchen)
		(fishing-line kitchen)
		(coffee-grounds kitchen)))

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

;;; The user starts out in the living room every time.
(defparameter *location* 'living-room)

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
(defparameter *allowed-commands* '(look walk pickup inventory help h ?))

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

;;; Basically check if the chain and bucker have been put
;;; together or not.
(defparameter *chain-welded* nil)

;;; Action that welds the chain and bucket together
(game-action weld chain bucket attic
	(if (and (have 'bucket) (not *chain-welded*))
    ;; Make sure that the game knows completed
    ;; the welding and tells the user too
    (progn (setf *chain-welded* 't)
    	'(the chain is now securely welded to the bucket.))
    '(you do not have a bucket.)))

;;; Basically check if the bucket if full of water or not.
(defparameter *bucket-filled* nil)

;;; Action to get water from the well in the garden
(game-action dunk bucket well garden
	(if *chain-welded* 
		(progn (setf *bucket-filled* 't)
			'(the bucket is now full of water))
		'(the water level is too low to reach.)))

;;; Action to wake up the wizard
(game-action splash bucket wizard living-room
	(cond ((not *bucket-filled*) '(the bucket has nothing in it.))
    ;; Lose the game if you picked up the frog
    ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
    	he is so upset he banishes you to the 
    	netherworlds- you lose! the end.))
    ;; If you didn't touch the wizard's frog you get a donut.
    ;; Basically "win" the game.
    (t '(the wizard awakens from his slumber and greets you warmly. 
    	he hands you the magic low-carb donut- you win! the end.))))

;;;; New action to put together my multi-part object

;;; Created a new macro action because we needed a list of the
;;; multi-part object and I couldn't figure out how to make
;;; game-action accept a list... so I basically created a duplicate
;;; macro that can take a list.
;;; I'm also pretty sure there's an easier way to do this, but I'm
;;; also feeling lazy...
;;; This is basically the same as the game-action macro above but
;;; slightly edited
(defmacro game-action-multi (command obj place &body body)
	`(progn (defun ,command (object)
    ;; So this is where I really changed things. Make
    ;; sure that the second arg is a list. Doesn't matter
    ;; how many items as long as it's a list
    (if (and (eq *location* ',place)
    	(subsetp ,obj object)
      (listp object))         ; Make sure that the second argument is a list.
    ,@body
    ;; Make sure that the user knows what went wrong so that
    ;; they can make changes.
    '(i cant ,command like that.
    	The argument after ,command should be a list.)))
	(pushnew ',command *allowed-commands*)))

;;; Basically check if the fishing rod has been put together or not.
(defparameter *fishing-rod-built* nil)

;;; Put together the fishing rod so that they user can now fish...in the
;;; well? I probably should have created another area to allow the user
;;; to actually fish but again too lazy. So I guess they just have to fish
;;; in the well. Although there are probably alternative uses for a fishing
;;; rod.
(game-action-multi build '(fishing-rod fishing-reel fishing-hook fishing-line) garden
  ;; Check if you have all the items needed, if you don't tell the user
  ;; what they are missing.
  (cond 
  	((eq *fishing-rod-built* t)
  		(game-print '(you put the fishing rod together already.)))
  	((not (have 'fishing-rod))
  		(game-print '(you do not have a fishing rod.)))
  	((not (have 'fishing-reel))
  		(game-print '(you do not have a fishing reel.)))
  	((not (have 'fishing-hook))
  		(game-print '(you do not have a fishing hook.)))
  	((not (have 'fishing-line))
  		(game-print '(you do not have a fishing line.)))

    ;; If they have everything build the fishing rod
    (t (progn (setf *fishing-rod-built* 't)
      ;; Remove all the parts of the fishing rod
      (setf *objects* (remove 'fishing-reel *objects*))
      (setf *objects* (remove 'fishing-hook *objects*))
      (setf *objects* (remove 'fishing-line *objects*))
      '(the fishing rod is now put together.
      	You can now go fishing for food.)))))

;;; Basically check if the coffee maker has been put together or not.
(defparameter *make-coffee* nil)

;;; Put together the coffee maker to make some coffee.
(game-action-multi make '(coffee-pot coffee-filter coffee-brew-basket water coffee-grounds) kitchen
  ;; Check if you have all the items needed, if you don't tell the user
  ;; what they are missing.
  (cond 
  	((eq *make-coffee* t)
  		(game-print '(you put the coffee maker together already.)))
  	((not (have 'coffee-pot))
  		(game-print '(you do not have a coffee pot.)))
  	((not (have 'coffee-filter))
  		(game-print '(you do not have a coffee filter.)))
  	((not (have 'coffee-brew-basket))
  		(game-print '(you do not have a coffee brew basket.)))
  	((not (have 'water))
  		(game-print '(you do not have water.)))
  	((not (have 'coffee-grounds))
  		(game-print '(you do not have coffee grounds.)))
    ;; If they have everything build the coffee maker
    (t (progn (setf *make-coffee* 't)

      ;; Create the coffee maker object and make sure that its
      ;; in the inventory
      ;; Single cup of coffee the user will ever have in this
      ;; game.
      (new-object coffee kitchen)
      (pickup 'coffee)

      ;; Remove all the parts of the coffee maker
      (setf *objects* (remove 'coffee-pot *objects*))
      (setf *objects* (remove 'coffee-filter *objects*))
      (setf *objects* (remove 'coffee-brew-basket *objects*))
      (setf *objects* (remove 'water *objects*))
      (setf *objects* (remove 'coffee-grounds *objects*))
      
      '(you can now have a cup of coffee.
      	this is the only cup of coffee you can have.)))))

;;; Was going to separate the action of putting the coffee maker
;;; together and brewing a cup of coffee but then I would have to
;;; take into account that you could probably pour more coffee and
;;; that seemed unnecessary so now the user can only have that single
;;; cup of coffee.
;(defparameter *brew-coffee* nil)

;(game-action brew coffee coffee-maker kitchen
;   (if *brew-coffee* 
;     (progn (setf *brew-coffee* 't)
;     '(the coffee is now made))))

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
