## My first R function
# Quality report

qreport <- function(score){
  if(!is.numeric(score))
    stop("'score' must be numeric")
  if(score < 1 | score > 5)
    stop("'score' only accepts input between 1-5")
  switch(score,
         '1' = print("Very poor"),
         '2' = print("Poor"),
         '3' = print("Average"),
         '4' = print("Good"),
         '5' = print("Very good")
  )
  if (score >2){
    message("Pass")
  } else
    message("Fail")
}

## Example

score <- 3 #integer between 1-5

qreport(score)
