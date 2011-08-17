(* Author:      Joel Verhagen
 * Date:        2011-08-03
 * Description: A SuDoku solver written in SML of New Jersey.
 *)

(* if an unexpected character is provided in a string to square list conversion *)
exception InvalidValue;

(* if there is a contradiction in a puzzle *)
exception PuzzleContradiction;

(* representing what the contents of a square are. either can be colored in or a list of possible colors *)
datatype SquareValue = Colored of int | Blank of int list;

(* a list of Squares is what I used to internally represent a SuDoku puzzle. Each square can either be colors (with a specific color) or Blank (with a list of possible values) *)
datatype Square = Square of SquareValue * int * int;

(* takes a string representing a puzzle and strips out all characters used for formatting and normalizes different characters used for representing a blank square in the puzzle *)
fun formatPuzzleString input =
    let
        fun stripChars (#"\n"::tail) = stripChars(tail) (* exclude formatting characters *)
          | stripChars (#"\r"::tail) = stripChars(tail)
          | stripChars (#"+"::tail) = stripChars(tail)
          | stripChars (#"-"::tail) = stripChars(tail)
          | stripChars (#"|"::tail) = stripChars(tail)
          | stripChars (#"0"::tail) = #" "::stripChars(tail) (* convert various forms of a blank square to a space *)
          | stripChars (#"."::tail) = #" "::stripChars(tail)
          | stripChars (#","::tail) = #" "::stripChars(tail)
          | stripChars (#"*"::tail) = #" "::stripChars(tail)
          | stripChars (#"?"::tail) = #" "::stripChars(tail)
          | stripChars (head::tail) = head::stripChars(tail) (* pass everything else through *)
          | stripChars ([]) = []
    in
        String.implode(stripChars(String.explode(input)))
    end;

(* returns an int list from minimum to maximum inclusive on both ends *)
fun range minimum maximum =
    if minimum > maximum
        then []
    else
        minimum::(range (minimum + 1) maximum);

(* takes a string and turns in into a square list (my internal representation of a SuDoku puzzle) *)
fun stringToSquareList input =
    let
        val charList = String.explode(formatPuzzleString(input))
		
		(* used for string to Square list conversion *)
		fun characterToInt input =
			let
				fun removeIntOption (SOME inputInt) = inputInt
				  | removeIntOption NONE = raise InvalidValue
			in
				removeIntOption (Int.fromString(String.implode([input])))
			end;
		
		(* takes the index (0 - 80) of a square a converts it to x, y coordinates *)
		fun indexToCoordinates index = (index mod 9, index div 9);
		
        fun generateSquare(#" ", (x, y)) = Square(Blank(range 1 9), x, y)
          | generateSquare(character, (x, y)) = Square(Colored(characterToInt(character)), x, y)

        fun numerateChars (head::tail, currentIndex) = generateSquare(head, indexToCoordinates(currentIndex))::numerateChars(tail, currentIndex + 1)
          | numerateChars ([], currentIndex) = []
    in
        numerateChars(charList, 0)
    end;

(* my implementation of quicksort generalized to any datatype by passing a comparator function *)
fun quicksort comparator inputList =
    let
        fun quicksortBase ([]) = [] (* return an empty list if the input is empty *)
          | quicksortBase ([something]) = [something] (* if the list size is 1, then just return it because no sorting is needed *)
          | quicksortBase (head::tail) = (* use the first element as the pivot, which is fine *)
            let
                fun partitioner check = comparator(check, head)
                val (leftList, rightRight) = List.partition partitioner tail (* do the partition step based on the given comparison function *)
            in
                quicksortBase leftList @ head :: quicksortBase rightRight (* recurse quicksort then concatenate the lists (putting the pivot in the right place) *)
            end
    in
        (* kick off the quicksort operation on the given inputList *)
        quicksortBase inputList
    end;

(* a comparator used with quicksort to sort a square list by x, y coordinate *)
fun squareComparator (Square (aValue, aX, aY), Square (bValue, bX, bY)) =
    if bY > aY
        then true
    else if bY = aY andalso bX > aX
        then true
    else
        false;

(* converts a square list to string. has pretty formatting characters like borders between cubes and new lines! *)
fun squareListToString input =
    let
		(* used for string to Square list conversion *)
		fun intToCharacter input = List.hd(String.explode(Int.toString(input)));
		
        fun squareListToCharList ((Square(Colored (value), x, y))::tail) = intToCharacter(value)::squareListToCharList(tail)
          | squareListToCharList ((Square(Blank (intList), x, y))::tail) = #" "::squareListToCharList(tail)
          | squareListToCharList ([]) = []

        fun makeGrid (head::tail, currentIndex) =
            let
                fun getCharListForIndex (currentChar, currentIndex) =
                    if currentIndex mod 9 = 0 (* are we at the beginning of the line *)
                        then if currentIndex = 0 (* are we on the first index? *)
                            then (String.explode "\n+---+---+---+\n|") @ [currentChar]
                        else
                            [#"|", currentChar]

                    else if currentIndex mod 9 mod 3 = 2 (* are we at the end of a block - horizontally ? *)
                        then if currentIndex mod 9 = 8 (* are we at the end of a line? *)
                            then if currentIndex mod 27 = 26 (* are we at the end of a block - vertically? *)
                                then currentChar::(String.explode("|\n+---+---+---+\n"))
                            else
                                [currentChar, #"|", #"\n"]
                        else
                            [currentChar, #"|"]
                    else
                        [currentChar]
                val currentChars = getCharListForIndex (head, currentIndex)
            in
                currentChars @ makeGrid(tail, currentIndex + 1)
            end
          | makeGrid ([], currentIndex) = []
    in
        String.implode(makeGrid(squareListToCharList(quicksort squareComparator input), 0))
    end;

(* returns an int * int indicating the cube coordinates of the given square coordinates.*)
fun getCubeCoordinatesBySquareCoordinates (x, y) = (x div 3, y div 3);

(* returns an int * int indicating the cube coordinates of the given index.  *)
fun getCubeCoordinatesByCubeIndex index = (index mod 3, index div 3);
fun getCubeIndexBySquareCoordinates (x, y) = (x div 3) + (3 * (y div 3))

(* get the squares from the provided squareList of the column, row, or cube with the given index *)
fun getColumn squareList input = List.filter (fn (Square(value, x, y)) => x = input) squareList;
fun getRow squareList input = List.filter (fn (Square(value, x, y)) => y = input) squareList;
fun getCube squareList input = List.filter (fn (Square(value, x, y)) => getCubeCoordinatesBySquareCoordinates(x, y) = getCubeCoordinatesByCubeIndex(input)) squareList;

fun getColoredValues ((Square(Colored (value), x, y))::tail) = value::(getColoredValues tail)
  | getColoredValues ((Square(Blank (intList), x, y))::tail) = getColoredValues tail
  | getColoredValues ([]) = [];

(* takes the union of two lists (such that only unique elements of the second element are added to the first) *)
infix U
fun listA U listB =
    let
        (* filter out all elements of listB that exist in listA *)
        val uniqueElements = List.filter (fn elementB => not (List.exists (fn elementA => elementA = elementB) listA)) listB
    in
        listA @ uniqueElements
    end;

(* takes the difference of two lists (such that any elements in listA that are also in listB are removed from listA before returning listA back) *)
infix \
fun listA \ listB =
    let
        val difference = List.filter (fn elementA => not (List.exists (fn elementB => elementA = elementB) listB)) listA
    in
        difference
    end;

(* returns only the unique elements of a list *)
fun unique (head::tail) =
    if List.exists (fn element => head = element) tail
        then unique tail
    else
        head::unique tail
  | unique ([]) = [];

(* returns true if the provided square list is a valid (does not test for completion) *)
fun isValid squareList =
    let
        (* take the provided function to get an area of the square list and make sure all of the colors are unique in that area *)
        fun validateArea areaFunction index =
            let
                val colors = getColoredValues(areaFunction squareList index)
            in
                colors = unique(colors)
            end

        fun myAnd (a, b) = a andalso b
        fun allTrue input = List.foldl myAnd true input

        val areaFunctions = [getColumn, getRow, getCube]
        val indices = range 0 8

        (* make sure all color values are unique on all rows, columns and squares *)
        val successTable = List.map (fn areaFunction => List.map (fn index => validateArea areaFunction index) indices) areaFunctions
    in
        (* collapse all booleans with andalso. If it evaluates to false, there there is atleast one invalid row, column or cube *)
        allTrue (List.map allTrue successTable)
    end;

(* follows the cancelling out possibilities rules to fill in as many squares as possible (no guessing involved here) *)
fun reduce squareList =
    let
        fun basicPossibilityReduceStep squareList =
            let
                (* list of all of the colors in each column, row, and square *)
                val columnColoredList = List.map (fn columnIndex => getColoredValues(getColumn squareList columnIndex)) (range 0 8)
                val rowColordList = List.map (fn rowIndex => getColoredValues(getRow squareList rowIndex)) (range 0 8)
                val cubeColoredList = List.map (fn cubeIndex => getColoredValues(getCube squareList cubeIndex)) (range 0 8)

                (* removes impossible values from candidate lists by column, row, and cube *)
                fun basicPossibilityReduce ([]) = []
                  | basicPossibilityReduce ((Square(Blank (oldPossibleColors), x, y))::tail) =
                    let
                        val columnColors = List.nth (columnColoredList, x)
                        val rowColors = List.nth (rowColordList, y)
                        val cubeColors = List.nth (cubeColoredList, getCubeIndexBySquareCoordinates(x, y))

                        val usedColors = columnColors U rowColors U cubeColors

                        val newPossibleColors = oldPossibleColors \ usedColors
                        val newPossibleColorsCount = List.length newPossibleColors
                    in
                        if newPossibleColorsCount = 0
                            then raise PuzzleContradiction
                        else if newPossibleColorsCount = 1
                            then Square(Colored(List.hd newPossibleColors), x, y)::basicPossibilityReduce tail
                        else
                            Square(Blank(newPossibleColors), x, y)::basicPossibilityReduce tail
                    end
                  | basicPossibilityReduce (head::tail) = head::basicPossibilityReduce tail
            in
                basicPossibilityReduce(squareList)
            end;

        fun uniqueCandidateReduceStep squareList =
            let
                (* takes a list of squares (a row, column, or cube) and returns a list of unique candidates *)
                fun getUniqueCandidates squareList =
                    let
                        (* count the number of instances of value input *)
                        fun count input value =
                            let
                                fun incrementIfEqual (head::tail) count =
                                        if head = value
                                            then incrementIfEqual tail (count + 1)
                                        else
                                            incrementIfEqual tail count
                                  | incrementIfEqual ([]) count = count
                            in
                                incrementIfEqual input 0
                            end

                        (* get all candidates in the given square list (typically constituting a row, column or square *)
                        fun getCandidates ((Square(Colored (value), x, y))::tail) = getCandidates(tail)
                          | getCandidates ((Square(Blank (intList), x, y))::tail) = intList @ getCandidates(tail)
                          | getCandidates ([]) = [];

                        val candidateList = getCandidates squareList
                    in
                        (* remove all candidates that have more than 1 instance *)
                        List.filter (fn candidate => count candidateList candidate = 1) candidateList
                    end

                (* lists of all of the unique values in each column, row, and square *)
                val columnUniqueList = List.map (fn columnIndex => getUniqueCandidates(getColumn squareList columnIndex)) (range 0 8)
                val rowUniqueList = List.map (fn rowIndex => getUniqueCandidates(getRow squareList rowIndex)) (range 0 8)
                val cubeUniqueList = List.map (fn cubeIndex => getUniqueCandidates(getCube squareList cubeIndex)) (range 0 8)

                (* finds a unique candidates in all columns, rows, and cubes then fills in squares with that information *)
                fun uniqueCandidateReduce ([]) = []
                  | uniqueCandidateReduce ((Square(Blank (oldPossibleColors), x, y))::tail) =
                    let
                        val head = Square(Blank (oldPossibleColors), x, y)

                        val columnUniqueColors = List.nth (columnUniqueList, x)
                        val rowUniqueColors = List.nth (rowUniqueList, y)
                        val cubeUniqueColors = List.nth (cubeUniqueList, getCubeIndexBySquareCoordinates(x, y))
                        val uniqueColors = columnUniqueColors U rowUniqueColors U cubeUniqueColors

                        val uniqueColorMatches = List.filter (fn oldPossibleColor => List.exists (fn uniqueColor => uniqueColor = oldPossibleColor) uniqueColors) oldPossibleColors
                        val uniqueColorMatchesCount = List.length uniqueColorMatches
                    in
                        if uniqueColorMatchesCount >= 1
                            then Square(Colored(List.hd uniqueColorMatches), x, y)::uniqueCandidateReduce tail
                        else
                            head::uniqueCandidateReduce tail
                    end
                  | uniqueCandidateReduce (head::tail) = head::uniqueCandidateReduce tail
            in
                uniqueCandidateReduce(squareList)
            end;

        val basicPossibilityReduced = basicPossibilityReduceStep(squareList)
        val uniqueCandidateReduced = uniqueCandidateReduceStep(squareList)
    in
        if basicPossibilityReduced = squareList
            then if uniqueCandidateReduced = squareList
                then squareList
            else
                reduce uniqueCandidateReduced
        else
            reduce basicPossibilityReduced
    end;

(* returns true only if the puzzle is solved. This does not test the validity of the solution *)
fun isSolved ((Square(Blank (intList), x, y))::tail) = false
  | isSolved (head::tail) = isSolved tail
  | isSolved ([]) = true;

(* takes a square list and creates new copies of the square list where the provided blank is replaced with all possible values *)
fun generateGuesses (squareList, (Square(Blank (intList), x, y))) =
    let
        (* takes a square list and a color and replaces the blank at x and y with color *)
        fun generateGuess ((Square(Blank (intList), currentX, currentY))::tail, color) =
            let
                val head = (Square(Blank (intList), currentX, currentY))
            in
                if currentX = x andalso currentY = y
                    then Square(Colored(color), x, y)::tail
                else
                    head::generateGuess(tail, color)
            end
          | generateGuess (head::tail, color) = head::generateGuess(tail, color)
          | generateGuess ([], color) = []
    in
        List.map (fn color => generateGuess(squareList, color)) intList
    end
  | generateGuesses (squareList, _) = [squareList];

(* returns the next blank in the square list *)
fun getNextBlank ((Square(Blank (intList), x, y))::tail) = [Square(Blank (intList), x, y)]
  | getNextBlank (head::tail) = getNextBlank tail
  | getNextBlank ([]) = [];

(* takes a square list and generates all possible guesses from the next open blank *)
fun guessStep squareList =
    let
        (* determines whether the provided square list is valid or not by trying a reduce, and checking whether it is valid *)
        fun isValidGuess squareList = isValid(reduce squareList) handle PuzzleContradiction => false

        val nextBlankList = getNextBlank squareList;
        val nextBlankListCount = List.length nextBlankList;

    in
        if nextBlankListCount = 0 (* if there are no more blanks, then just return back the squarelist itself, since we have found a valid solution *)
            then [squareList]
        else (* otherwise, remove all invalid guesses that we have generated *)
            List.map reduce (List.filter (fn guess => isValidGuess(guess)) (generateGuesses(squareList, List.hd nextBlankList)))
    end;

(* takes a square list list and returns all solutions, by means of guessing when necessary and using reduce rules when possible *)
fun guess (squareList::tail) =
    let
        val guesses = guessStep(squareList)
    in
        if [squareList] = guesses
            then guesses @ guess(tail)
        else
            guess(guesses @ tail)
    end
  | guess([]) = [];

(* takes a square list and returns a list of all possible solutions by trying to reduce first then guessing if the reduce did not solve the puzzle *)
fun solve squareListList =
    let
        fun solveOne squareList =
            let
                val reduced = reduce squareList
            in
                if isSolved reduced
                    then [reduced]
                else
                    guess([reduced])
            end
    in
        List.foldl (op @) [] (List.map solveOne squareListList)
    end;

(* This puzzle has 5 solutions. *)
(* val puzzle = "8  6  9 5             2 31   7318 6 24     73           279 1  5   8  36  3      "; *)
val puzzle = squareListToString(stringToSquareList(" 482 9 5 9 76 38  5   7    4 2   7   6 7 8 3   5   9 8    2   9  64 15 2 5 9 617 "));
(print puzzle);

val solution = List.map squareListToString (solve([stringToSquareList(puzzle)]));

(print (List.hd solution));