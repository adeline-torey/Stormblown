## Digital fortune teller game
## inspired by the paper fortune teller game and interactive MS-Dos games I used to play and fortune cookies
## Just for fun!! :)
## fortune cookies sayings sources: 
## https://www.wishesmessagessayings.com/fortune-cookie-sayings.html
## https://www.funny-jokes-quotes-sayings.com/funny-fortune-cookie-sayings.html


## prompt

fortel <- function(){
  print("Ask a question")
  message("blue, red, green or yellow")
  my.col <- readline(prompt = "Choose a colour to get the answer ")
  pikcol(my.col)
  my.num <- readline(prompt = "Choose a number to get the answer ")
  piknum(my.num)
}

## pick color
pikcol <- function(colour){
  switch(colour,
         "blue" = message("B..L..U..E.."),
         "red" = message("R..E..D.."),
         "green" = message("G..R..E..E..N.."),
         "yellow" = message("Y..E..L..L..O..W.."))
  
  
  #random sampling to simulate spelling out the colours so that x or y is selected randomly
  x <- "1 or 2 or 5 or 6 "
  y <- "3 or 4 or 7 or 8 "
  num.options <- c(x, y)
  num <- message(sample(num.options, size = 1, replace = TRUE))
}


## function to return fortune based on selected number
piknum <- function(number){
  switch(number,
         '1' = message("Life is full of twists. Stay flexible."),
         '2' = message("Be a snowflake. They are all unique and beautiful in their own way."),
         '3' = message("There's no receipt for your time. You can't be refunded if you don't spend it wisely."),
         '4' = message("Two days from now, tomorrow will be yesterday."),
         '5' = message("You are cleverly disguised as responsible adult."),
         '6' = message("Now is the time to try something new."),
         '7' = message("If you're happy, you're successful."),
         '8' = message("You will soon have an out of money experience."))
}      


## example (type answers in console)

fortel()


