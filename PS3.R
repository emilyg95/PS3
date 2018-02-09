door1 = 2 ## creates object door = 2 to indicate candidate chose number 2

class(door1) = "door" ## defines class of object door1 as "door"

door1 ## test

PlayGame = function(door){ ## defines existence of PlayGame method for class door
  UseMethod("PlayGame", door)
}

PlayGame.door = function(door){ ## creates a function using PlayGame method for class door
  car = ceiling(runif(1, 0, 3)) ## creates object "car" to indicate the number of the winning door; runif generates a random number between 0 and 3 and ceiling rounds up to the largest integer value ensuring the result will be a whole number from 1 through 3
  if (door == car){ ## if then statement which prints congratulations if the number door of the input the candidate chose and the number of the winning door are equal
    return("Congratulations! You Win")
  }
  else { ## prints sorry you lose if the numbers are not equal
    return("Sorry :( You Lose")
}
}

PlayGame(door1) ## test


setClass(Class = "door", ## creates a new class "door"
         representation = representation(
           x = "numeric" ## indicates 1 input x which must be numeric
         ),
         prototype = prototype(
           x = c() ## indicates x will be a list
         )
)

new("door") ## check

setValidity("door", function(object){ ## creates function to check validity of objects of class door
  test1 = object == 1
  test2 = object == 2
  test3 = object == 3
  if (!test1 & !test2 & !test3){ ## if then statement which checks to see if the input is equal to 1 2 or 3 and rejects if not
    return("object is not a valid value")
  }
}
)

generic = function(object = "door"){ ## creates interior of generic function PlayGame for class door
  standardGeneric("PlayGame")
}

setGeneric("PlayGame", generic) ## sets the generic as the one I just created

