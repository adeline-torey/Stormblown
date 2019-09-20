## My first R function
# Quality report

qreport <- function(score){
  switch(score,
         '1' = print("Very poor"),
         '2' = print("Poor"),
         '3' = print("Average"),
         '4' = print("Good"),
         '5' = print("Very good"),
         cat(red("Invalid input. Accepts input between 1-5"))
  )
  if ( score >2){
    print("Pass")
  } else
    cat(red("Fail"))
}

## to run function

score <- 1

qreport(score)

## improvements:
### pass/fail condition to display error message for values other than 1-5
