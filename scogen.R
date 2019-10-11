## My second R function
## Score generator - converts marks (0-100) to a score (1-5)

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


## Examples

scogen(82)
scogen(81)
scogen(80)
scogen(45)
scogen(21)
scogen(20)
scogen(19)
scogen(0)
