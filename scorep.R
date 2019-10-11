## My third R function
## Score reporter - input marks (0-100) to return a report on score description and pass/fail

## input marks and return score value
scogen <- function(marks){
  if(marks >= 81 & marks <= 100)
    score <- 5
  if(marks >= 61 & marks <= 80)
    score <- 4
  if(marks >= 41 & marks <= 60)
    score <- 3
  if(marks >= 21 & marks <= 40)
    score <- 2
  if(marks >= 0 & marks <= 20)
    score <- 1
  return(score)
}

## input score and return description and pass/fail
scodes <- function(score){
  if(!is.numeric(score))
    stop("'score' must be numeric")
  if(score < 1 | score > 5)
    stop("'score' only accepts input between 1-5")
  switch(score,
         '1' = print("Very poor"),
         '2' = print("Poor"),
         '3' = print("Average"),
         '4' = print("Good"),
         '5' = print("Very good"))
  if (score >2){
    print("Pass")
  } else
    print("Fail")
}

## input marks and return description and pass/fail
scorep <- function(marks){
  scocat <- scogen(marks)
  result <- scodes(scocat)
  return(result)
}

## Example

scorep(63) 
