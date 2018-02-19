door1 = 2 ## creates object door1 equal to 2 to indicate candidate chose number 2

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

PlayGame.door(door1) ## test


setClass(Class = "door", ## creates a new class "door"
         representation = representation(
           x = "numeric" ## indicates 1 input x which must be numeric
         ),
         prototype = prototype(
           x = c() ## indicates x will be a vector
         )
)

new("door") ## check

setValidity("door", function(object){ ## creates function to check validity of objects of class door
  test1 = object@x == 1
  test2 = object@x == 2
  test3 = object@x == 3
  if (!test1 & !test2 & !test3){ ## if then statement which checks to see if the input is equal to 1 2 or 3 and rejects if not
    return("object is not a valid value")
  }
}
)

new("door", x = 5) ## check to confirm error if x != 1 2 or 3 
new("door", x = 2)

generic = function(object = "door"){ ## creates interior function for setGeneric, which defines the function as PlayGame and its generic input as of class door
  standardGeneric("PlayGame")
}

setGeneric("PlayGame", generic) ## sets the generic of function PlayGame as the function I just created

setMethod("PlayGame", "door",
          function(object){ ## sets the method of the PlayGame function for class door with the same commands as the function I created in s3
            car = ceiling(runif(1, 0, 3))
            if (object == car){
              return("Congratulations! You Win")
            }
            else { ## prints sorry you lose if the numbers are not equal
              return("Sorry :( You Lose")
          }}
            )

door2 = 3 ## creates new object door2 equal to 3

class(door2) = "door" ## makes door2 of class door

PlayGame(door2) ## test

door3 = 5

class(door3) = "door" ## why does this still work?

PlayGame(door3)


## Activity 5:

createstudent = function(name){ ## function to create a student
  courage = ceiling(runif(1, 0, 100)) ## runif selects continuous values between the specified numbers, here one value between zero and one hundred
  intelligence = ceiling(runif(1, 0, 100)) ## my values will actually be whole numbers between one and one hundred because I force it to round up to the nearest integer using ceiling
  ambition = ceiling(runif(1, 0, 100)) ## I run this command four times and give each output a label corresponding to a house
  effort = ceiling(runif(1, 0, 100))
  return(structure(list("name" = name, "courage" = courage, "intelligence" = intelligence, "ambition" = ambition, "effort" = effort), class = "student"
  ))} ## the function will return a list with the name of the student, and each attribute and it's value

Emily = createstudent("Emily")

class(Emily)

sort.student = function(student, x){ ## creates a function to sort the student into a house which will take in the arguments "student" and "x" which will be a 4x4 matrix as specified in the question
  a = c(student[[2]], student[[3]], student[[4]], student[[5]]) ## a vector of the student's four values
  vector_a = x%*%a ## multiplies the matrix x by the vector a
  rank_a = rank(vector_a, ties.method = "first") ## creates a new vector that is a rank order list of the values in vector_a with 4 being the largest and 1 being the smallest
  if (rank_a[[1]] == 4){ ## if else statement that checks the placement of the number 4 in rank_a and returns a house depending on where it is
    print("GRYFFINDOR")}
  else if (rank_a[[2]] == 4){
    print("SLYTHERIN")}
  else if (rank_a[[3]] == 4){
    print("RAVENCLAW")}
  else if (rank_a[[4]] == 4){
    print("HUFFLEPUFF")}
}

vec_x = ceiling(runif(16, 1, 100))
matrix_X = matrix(vec_x, nrow = 4, ncol = 4)

sort.student(Emily, matrix_X) ## it works!

sort.student2 = function(student, x){ ## creates a function to sort the student into a house which will take in the arguments "student" and "x" which will be a 4x4 matrix as specified in the question
  a = c(student[[2]], student[[3]], student[[4]], student[[5]]) ## a vector of the student's four values
  vector_a = x%*%a ## multiplies the matrix x by the vector a
  rank_a = rank(vector_a, ties.method = "first") ## creates a new vector that is a rank order list of the values in vector_a with 4 being the largest and 1 being the smallest
  if (rank_a[[1]] == 4){ ## if else statement that checks the placement of the number 4 in rank_a and appends a new class to the student indicating the correct house depending on where it is
    class(student) = append(class(student), "GRYFFINDOR") ## uses the class setting function to append a second class to the student labeled depending on the corresponding house of the ranking 
    assign(student$name, student, envir = .GlobalEnv) ## creates a new variable replacing the input for student with a new object including the appended class 
    return(student) ## returns the new object
    }
  else if (rank_a[[2]] == 4){
    class(student) = append(class(student), "SLYTHERIN") 
    assign(student$name, student, envir = .GlobalEnv)
    return(student)
    }
  else if (rank_a[[3]] == 4){
    class(student) = append(class(student), "RAVENCLAW")
    assign(student$name, student, envir = .GlobalEnv)
    return(student)
    }
  else if (rank_a[[4]] == 4){
    class(student) = append(class(student), "HUFFLEPUFF") 
    assign(student$name, student, envir = .GlobalEnv)
    return(student)
    }
}

sort.student2(Emily, matrix_X)

class(Emily)

Emily = createstudent("Emily")
sort.student2(Emily, matrix_X) ## Hufflepuff

Professor_Montgomery = createstudent("Professor_Montgomery")
sort.student2(Professor_Montgomery, matrix_X) ## Ravenclaw

Min_Hee = createstudent("Min_Hee")
sort.student2(Min_Hee, matrix_X) ## Gryffindor

Arya = createstudent("Arya")
sort.student2(Arya, matrix_X) ## Slytherin

## the houses probably won't turn out the same when you rerun it but I just wanted to make someone in each house for fun

Gryffindor_Tower = new.env()
Black_Lake = new.env()
Ravenclaw_Tower = new.env()
Basement = new.env() ## creates new environments named after the 4 Houses' common rooms

curfew = function(student){
  return(student)
}

curfew(Emily)
curfew(Professor_Montgomery)
curfew(Min_Hee)
curfew(Arya)
## check

curfew.GRYFFINDOR = function(student){ ## creates a new function for the class GRYFFINDOR
  assign(student$name, student, envir = Gryffindor_Tower) ## assigns the student to the new environment Gryffindor_Tower
  ls(Gryffindor_Tower) ## returns a list of the objects in the environment Gryffindor_Tower
}

curfew.GRYFFINDOR(Min_Hee) ## check

curfew.SLYTHERIN = function(student){
  assign(student$name, student, envir = Black_Lake)
  ls(Black_Lake)
}

curfew.SLYTHERIN(Arya)

curfew.RAVENCLAW = function(student){
  assign(student$name, student, envir = Ravenclaw_Tower)
  ls(Ravenclaw_Tower)
}

curfew.RAVENCLAW(Professor_Montgomery)

curfew.HUFFLEPUFF = function(student){
  assign(student$name, student, envir = Basement)
  ls(Basement)
}

curfew.HUFFLEPUFF(Emily)
