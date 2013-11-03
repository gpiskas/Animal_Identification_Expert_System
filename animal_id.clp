;;
;;  Animal Identification Expert System
;;  Copyright (C) 2013  George Piskas
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;;
;;  Contact: geopiskas@gmail.com
;;

;;; This is the template definition for the animal. The fact has the following form:
;;; (animal (name ?STRING) (kind ?STRING) (size ?NUMBER) (colors $?SYMBOL) (spots SYMBOL) (body $?SYMBOL) (environment $?SYMBOL) (particularity $?SYMBOL) )
;;; It is better (visually) explained in the analysis pdf
(deftemplate animal "Template holding the characteristics of an animal."
	;;; Type checking is applied on every field.
	;;; Some fields have a number of allowed values for safety.

	;;; Name, kind and size. All three are required fields (default ?NONE). Size is strictly a number(float or integer) in *cm*.
	(multislot name (type STRING) (default ?NONE))
	(multislot kind (type STRING) (default ?NONE))
	(slot size (type NUMBER) (default ?NONE))
	
	;;; Descripion in three seperate parts. Color is required, spots and body are optional.
	(multislot colors (type SYMBOL) (default ?NONE)
		(allowed-symbols brown yellow black grey green red white blue)
	)
	(slot spots (type SYMBOL) (default false) (allowed-symbols false true))
	(multislot body (type SYMBOL) (default ?DERIVE)
		(allowed-symbols nil big-eyes colored-belly hairy strip long concave spikes sharp tentacles tail mustache)
	)
	
	;;; Environment, required field.
	(multislot environment (type SYMBOL) (default ?NONE)
		(allowed-symbols water beach cave dry bush forest mud crops underwater rock mountain leafs cavity)
	)
	
	;;; Particularities, optional field.
	(multislot particularity (type SYMBOL) (default ?DERIVE)
		(allowed-symbols nil descendants-color can-dive smallest-mammal dangerous can-swim infectious cultivable night-movement chameleon jumps-flies)
	)
)

	
(deffacts animal-database "Adding our animal database to the facts."
	;;; Monk Seal
	(animal (name "Monk Seal")
			(kind "Monachus monachus")
			(size 3.8)
			(colors brown grey white)
			(spots true)
			(body colored-belly)
			(environment water beach cave)
			(particularity descendants-color can-dive)
	)
	;;; Mouse
	(animal (name "Mouse")
			(kind "Suncus etruscus")
			(size 4.5)
			(colors brown grey)
			(body big-eyes mustache)
			(environment dry bush)
			(particularity smallest-mammal)
	)	
	;;; Wild Boar
	(animal (name "Wild Boar")
			(kind "Sus scrofa")
			(size 180)
			(colors brown grey)
			(body hairy)
			(environment forest mud)
			(particularity descendants-color dangerous)
	)		
	;;; Brown Hare
	(animal (name "Brown Hare")
			(kind "Lepus europaeus")
			(size 65)
			(colors brown)
			(body strip)
			(environment crops forest)
			(particularity can-swim)
	)	
	;;; Sea Urchin
	(animal (name "Sea Urchin")
			(kind "Centrostephanus longispinus")
			(size 6)
			(colors red white)
			(body long concave spikes sharp)
			(environment underwater cave)
			(particularity infectious)
	)		
	;;; Mediterranean Sea Mussel
	(animal (name "Mediterranean Sea Mussel")
			(kind "Mytilus galloprovincialis")
			(size 15)
			(colors blue brown)
			(environment rock water)
			(particularity cultivable)
	)	
	;;; Fire Salamander
	(animal (name "Fire Salamander")
			(kind "Salamandra salamandra")
			(size 28)
			(colors yellow black)
			(spots true)
			(environment forest mountain)
			(particularity night-movement)
	)
	;;; Octopus
	(animal (name "Octopus")
			(kind "Octopus vulgaris")
			(size 100)
			(colors grey green)
			(body tentacles)
			(environment cave cavity)
			(particularity chameleon)
	)	
	;;; Grasshopper
	(animal (name "Grasshopper")
			(kind "Acrida mediterranea")
			(size 7.5)
			(colors green)
			(body big-eyes)
			(environment bush)
			(particularity jumps-flies)
	)		
	;;; Ladybird
	(animal (name "Ladybird")
			(kind "Occinella septempunctat")
			(size 0.8)
			(colors red)
			(spots true)
			(environment leafs)
	)	
	;;; Southeuropean Scorpion
	(animal (name "Southeuropean Scorpion")
			(kind "Euscorpius flavicaudis")
			(size 3.5)
			(colors brown yellow)
			(body tail)
			(environment rock dry)
			(particularity dangerous)
	)
	;;; Spider
	(animal (name "Spider")
			(kind "Oecobius annulipes")
			(size 0.2)
			(colors yellow brown red)
			(body colored-belly)
			(environment rock)
	)		
)


;;; A global variable that holds the number of avaliable animals.
(defglobal ?*counter* = 12)


;;; The variable above is being modified with this function each time we exclude an animal from
;;; the possible solutions. (minusOne) decreases the global counter by one.
(deffunction minusOne ()
	(bind ?*counter* (- ?*counter* 1))
)


;;; This function is used for every question made to the user.
;;; The question that is printed to the user is broken into three arguments (?qBEG ?qMID ?qEND) for flexibility, as we may need to include a printable in the middle.
;;; The argument $?allowed-values is a list that holds the allowed values that the program accepts.
;;; If the user enters a non-acceptable value, the program asks the question again until the answer is valid.
(deffunction ask-question (?qBEG ?qMID ?qEND $?allowed-values)
	(printout t ?qBEG ?qMID ?qEND)
	(bind ?answer (read))
	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer))
	)
	(while (not (member ?answer ?allowed-values)) do
		(printout t ?qBEG ?qMID ?qEND)
		(bind ?answer (read))
		(if (lexemep ?answer)
			then (bind ?answer (lowcase ?answer)))
	)
?answer)


;;; The first main question made to the user. We ask for the size of the animal, expecting tiny, small or big as the answer.
;;; The result is stored as the following fact: (theScale ?size)
(defrule mainQuestion-Size
	?x <- (initial-fact)
	=>
	(retract ?x)
	(bind ?size (ask-question "### Is the animal tiny ( <4cm ) , small ( 4cm - 20cm ) or big ( >20cm )? (tiny,small,big) ### " "" "" tiny small big))
	(assert (theScale ?size))	
)


;;; Given that the fact (theScale ?size) exists, this rule gets triggered.
;;; This rule filters the animals by size, and deletes those that are not in the given scale-group.
;;; In addition, every time we retract an animal, we substract one from the global variable ?*counter* calling the (minusOne) function.
(defrule filterBy-Size
	(theScale ?s)
	?ani <- (animal (size ?size))
	=>
	(if (eq ?s tiny)
		then (if (> ?size 4) then (retract ?ani) (minusOne))
	else (if (eq ?s small)
		    then (if (or (<= ?size 4) (>= ?size 20)) then (retract ?ani) (minusOne))
		 )
	else (if (eq ?s big)
		    then (if (< ?size 20) then (retract ?ani) (minusOne))
		 )
	)
)


;;; The second main question is about the color of the animal. The user can type any color from the acceptable list in the parentheses.
;;; The result is stored as the following fact: (theColor ?color)
(defrule mainQuestion-Color
	(theScale ?s)
	=>
	(bind ?color (ask-question "### What color is it? (brown yellow black grey green red white blue) ### " "" "" brown yellow black grey green red white blue))
	(assert (theColor ?color))
)	


;;; Given that the fact (theColor ?color) exists, this rule gets triggered. It is very similar to the filterBy-Scale rule.
;;; This rule filters the animals by color, and deletes those that do not have this color in the list of colors.
;;; In addition, every time we retract an animal, we substract one from the global variable ?*counter* calling the (minusOne) function.
(defrule filterBy-Color
	(theColor ?c)
	?ani <- (animal (colors $?colors))
	=>
	(if (not (member$ ?c $?colors))
		then (retract ?ani) (minusOne)
	)
)


;;; After the Scale and Color filtering process, we check the global variable ?*counter*.
;;; If we have 1 animal left, this is the result and we assert (found true) in order to trigger the success print rule below.
;;; If we have 0 animals left, we assert (found false) as there are no animals that passed the filtering.
;;; If we got more than one, we need more facts to reach a conclusion, so we assert (needMoreFacts ?scale ?color) for the program to continue asking.
(defrule postFilteringEvaluation
    ?scale <- (theScale ?s)
	?color <- (theColor ?c)
	=>
	(retract ?scale ?color)
	(if (eq ?*counter* 1)
		then (assert (found true))
	else (if (eq ?*counter* 0)
			then (assert (found false))
		 ) 
	else (if (> ?*counter* 1)
			then (assert (needMoreFacts ?s ?c))
		 ) 
	)	
)	


;;; Given the fact (needMoreFacts ?s ?c) where ?s is the scale and ?c is the color the user has asked, we ask a more specific question about the animal
;;; that we are searching. We then assert a fact in the following form: (ask X Y) where X is a field of the animal template and Y is what we are searching in X.
;;; This rule is based on the table that is included in the analysis pdf.
(defrule needMoreFacts
	?q <-(needMoreFacts ?s ?c)
	=>
	(retract ?q)
	(if (and (eq ?s tiny) (eq ?c brown))
		then (assert (ask spots true))
	)
	(if (and (eq ?s tiny) (eq ?c yellow))
		then (assert (ask body tail))
	)
	(if (and (eq ?s tiny) (eq ?c red))
		then (assert (ask environment leafs))
	)
	(if (and (eq ?s small) (eq ?c brown))
		then (assert (ask body mustache))
	)
	(if (and (eq ?s big) (eq ?c brown))
		then (assert (ask environment mud))
	)
	(if (and (eq ?s big) (eq ?c grey))
		then (assert (ask body tentacles))
	)
)


;;; Based on the assert that was made on the rule above, we ask a specific question about the body of the animal.
;;; According to the analysis table in the pdf, there will be only two possible choises when the question is about the body of the animal.
;;; We pick those two facts, make sure they are different and ask the body question. 
;;; Then, based on the user's answer, we retract one of them and we have reached a solution. We then assert the fact (found true).
(defrule askBody
	?q <-(ask body ?ans)
	?ani1 <- (animal (body $?content1))
	(test (member$ ?ans $?content1))
	?ani2 <- (animal (body $?content2))
	(test (neq ?ani1 ?ani2))
	=>
	(retract ?q)
	(bind ?a (ask-question "### Has the animal got " ?ans " on it's body? (yes/no) ### " yes no))
	(if (eq ?a yes)
		then (retract ?ani2) (minusOne)
		else (retract ?ani1) (minusOne)
	)
	(if (eq ?*counter* 1)
		then (assert (found true))
	)
)


;;; This rule follows the same idea as the rule above. Two possible animals, one gets filtered and we got a solution.
(defrule askenvironment
	?q <-(ask environment ?ans)
	?ani1 <- (animal (environment $?content1))
	(test (member$ ?ans $?content1))
	?ani2 <- (animal (environment $?content2))
	(test (neq ?ani1 ?ani2))
	=>
	(retract ?q)
	(bind ?a (ask-question "### Does the animal live in an environment that contains " ?ans "? (yes/no) ### " yes no))
	(if (eq ?a yes)
		then (retract ?ani2) (minusOne)
		else (retract ?ani1) (minusOne)
	)
	(if (eq ?*counter* 1)
		then (assert (found true))
	)
)

;;; This rule follows the same idea as the rule above, but instead of two animals we have three.
;;; We ask if the animal has spots. Based on the analysis table, If the answer is yes, we get a solution and we assert (found true).
;;; Otherwise, answering no, we get two possible animals and we need to ask one more question. Thus, we assert (ask particularity dangerous)
;;; in order to trigger the rule that follows.
(defrule askSpots
	?q <-(ask spots true)
	?ani1 <- (animal (spots true))
	?ani2 <- (animal (spots false))
	?ani3 <- (animal (spots false))
	(test (neq ?ani2 ?ani3))
	=>
	(retract ?q)
	(bind ?a (ask-question "### Does the animal have " spots "? (yes/no) ### " yes no))
	(if (eq ?a yes)
		then (retract ?ani2) (minusOne) (retract ?ani3) (minusOne)
		else (retract ?ani1) (minusOne) (assert (ask particularity dangerous))
	)
	(if (eq ?*counter* 1)
		then (assert (found true))
	)
)

;;; This rule gets triggered if the previous one (askSpots) was triggered and the fact (ask particularity dangerous) is present.
;;; Other than that, it follows the same idea as the rules above where we only have two possible animals.
;;; One of them gets filtered out and we got a solution, asserting (found true).
(defrule askParticularity
	?q <-(ask particularity ?ans)
	?ani1 <- (animal (particularity $?content1))
	(test (member$ ?ans $?content1))
	?ani2 <- (animal (particularity $?content2))
	(test (neq ?ani1 ?ani2))
	=>
	(retract ?q)
	(bind ?a (ask-question "### Is the animal " dangerous "? (yes/no) ### " yes no))
	(if (eq ?a yes)
		then (retract ?ani2) (minusOne)
		else (retract ?ani1) (minusOne)
	)
	(if (eq ?*counter* 1)
		then (assert (found true))
	)
)

;;; If the fact (found true) is present, it means that we have only one (animal) fact in memory, thus we have concluded to
;;; the one the user searches for. We assign this animal to the variable ?ani and print it's name and kind to the user.
(defrule matchFound
	?f <- (found true)
	?ani <- (animal (name ?n) (kind ?k))
	=>
	(retract ?f ?ani)
	(printout t "*********************" crlf)
	(printout t "* Animal found!" crlf)
	(printout t "* Name: " ?n crlf)
	(printout t "* Kind: " ?k crlf)
	(printout t "*********************" crlf)
)

;;; Just like the rule above, if the fact (found false) is present, we have no (animal) facts in memory. This means we have
;;; no results with the given criteria. We then print the failure to the user.
(defrule matchNotFound
	?f <- (found false)
	=>
	(retract ?f)
	(printout t "*********************" crlf)
	(printout t "* No animals match your description!" crlf)
	(printout t "*********************" crlf)
)