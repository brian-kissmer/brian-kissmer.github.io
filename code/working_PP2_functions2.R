#First, create a custom function and assigning it to the object JacobAllen. I created custom arguments for game number, my moves, and opponents moves for the game.
#Set all of these arguments to an empty value NA to later be defined when the game is played.

JacobAllen <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  
  #To create strategy for the games I used if statements with the if function which will execute a command based off of of the conditions in the parentheses are true.
  #For the next 4 strategic rules using if statements, I used the same code to apply the same rule for games 4 through 7. 
  #The rule is that for each 4 games, if the opponent's move for the last game was cooperation (1), a random number (move) is drawn from the sample 0 and 1 (cooperate for defect).
  #I used indexing to define the opponents last move and the sample function to draw the random move from the two.
  
  if (GameNum == 4 && OpMoves[GameNum - 1] == 1) {
    return(sample(c(0,1),1))
  }
  
  if (GameNum == 5 && OpMoves[GameNum - 1] == 1) {
    return(sample(c(0,1),1))
  }
  
  if (GameNum == 6 && OpMoves[GameNum - 1] == 1) {
    return(sample(c(0,1),1))
  }
  
  if (GameNum == 7 && OpMoves[GameNum - 1] == 1) {
    return(sample(c(0,1),1))
  }
  
  #This strategy and statement uses indexing to take the sum of the value of the opponents moves from the last two games (GameNum - 1 and -2). If that value equals 1,
  #the output to the function will be returned as 0 (defect) which is coded with the return funtion. In other words, if the opponent cooperates the last two moves, I will defect.
  
  if (sum(OpMoves[GameNum - 1], OpMoves[GameNum - 2] == 2)) {
    return(0)
  }
  
  #The next rule simply makes my move a cooperate (1) through game 3 (for games less than or equal to 3).
  
  if (GameNum <= 3) {
    return(0) 
  }
  
  #This rule is for games 10 through 20, if the opponents moves add up to 5, or opponent has cooperated up to 5 times, I will defect, or the funtion returns a 0.
  #These multiple conditions were completed by using two and codes to link each condition. 
  if (GameNum >= 10 && GameNum <= 20 && sum(OpMoves) <= 5) {
    return (0)
  }
  
  #This next 5 moves are the same as well for games 8, 9, 12, 13, and 17.
  #It is for each of those game numbers, a 0 or 1 will be returned from the binomial distribution. 
  #The probability for that draw is the sum of the opponents total moves divided by the game number subtracted from 1. That means that on each of those games, if the opponent has cooperated more, 
  #I will be more likely to defect because the equation will result in a smaller probability of success, or a 1.
  
  if (GameNum == 8) {
    return(rbinom(n = 1, size = 1, prob = (1-(sum(OpMoves)/GameNum))))
  }
  
  if (GameNum == 9) {
    return(rbinom(n = 1, size = 1, prob = (1-(sum(OpMoves)/GameNum))))
  }
  
  if (GameNum == 12) {
    return(rbinom(n = 1, size = 1, prob = (1-(sum(OpMoves)/GameNum))))
  }
  
  if (GameNum == 13) {
    return(rbinom(n = 1, size = 1, prob = (1-(sum(OpMoves)/GameNum))))
  }
  
  if (GameNum == 17) {
    return(rbinom(n = 1, size = 1, prob = (1-(sum(OpMoves)/GameNum))))
  }
  
  #This rule simply states if none of these conditions above are met and the sum of the opponents moves is greater than 16, or the opponent has cooperated 16 times, I will defect and be returned a 0. 
  
  if (sum(OpMoves) > 16) {
    return(0)
  }
  
  #This next rule codes that if none of the above conditions have been met and it is game 18 or greater, I will be returned a 1 if the proportion of the sum of all the opponent's game moves to the game number is 
  #higher than 0.5, I will cooperate (returned a 1). I used the "and" codes && to create the two conditions and the equation of total (sum) of opponent's moves divided by the game number to tell me if they have been cooperating for more than 50% of the game thus far. 
  #I used if else statement to make it so if the same proportion was less than 50% (the previous conditions weren't met) or they cooperated less than half the times thus far, I will defect (be returned a 0). I used the same code but just changed the code to less than instead of greater than.
  
  if (GameNum >=18 && (sum(OpMoves)/GameNum) > 0.5 ) {
    return(1)
  }  else if (GameNum >=18 && (sum(OpMoves)/GameNum) < 0.5 ) {
    return(0)
  }
  
  #For this rule, if it is games 1 through 14 and the sum of my moves is greater than or equal to the current game number minus 5, I will defect.
  #I used this code to try to make it so if I was cooperating a majority of the time (I have only defected 0 to 5 times in the game up to game 14), I would defect. This is if the conditons above weren't applied and is why it is at the end. 
  
  if (GameNum > 15 && sum(MyMoves) >= (GameNum - 5)) {
    return(0)
  }
  
  #In the end, if none of these were able to apply, my default strategy is to cooperate
  #Overall, my strategy is really random and heavily based off of the opponents moves. I don't want my opponent to be able to predict my moves.
  return(1)
}#reference - 1= cooperate, 0= Defect 
#First set up the custom function
KayleeAllen <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  if (GameNum == 1) {return(0) 
    # start the game by defecting because there is a higher chance of getting more points that way
  }else if (mean(OpMoves) < .5) {return (0) 
    #if my opponent has put down more 0s than 1s, return with a 0
  }else if (mean(OpMoves) > .5) {return (1)
    #if my opponent put down more 1s than 0s, return with 1
  }else {return (1)
    #if they put down the same amount of each, put down a 1
  }
}

#Programing Project 2: Game Theory

KenadeeAllred<- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  #Strategy: My stragey is to coorperate for the first game gaurenteed.This would allow for my opponent to start off trusting me. 
  if(GameNum==1){
    return(1)
  } #I then want to continue to cooperate until my opponent deflects once in the previous round.
  else if(OpMoves[GameNum-1]==1){ 
    return(1)
    #My response will now mimic the response of the opponent from the play before.
  } else if (OpMoves[GameNum-1]==0){
    return(0)
  } #After that I will continue to deflect or return based on my opponents moves until I hit round 25. 
  #If my random number gets above 25, the idea is that this would be a good time to play it safe and just cooperate.
  else if (GameNum >=25){
    return (1)
  } #Finally if we make it to 35 rounds, I want to risk a lot and just deflect and hope to make lots of points that round. 
  else if (GameNum >= 35){
    return (0)
  }
}

TannerBanham <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  
  # I will always cooperate the first round
  if (GameNum == 1) {
    return(1)
  }
  
  # creating a Boolean to check if my opponent has ever defected
  defected = FALSE
  
  # if my opponent has ever defected, I will track that 
  if (any(OpMoves == 0)) {
    defected = TRUE
  }
  
  # I will also defect near the end of the game
  if (GameNum >= 14) {
    return(0)
  }
  
  # always defect if opponent defected
  if (defected) {
    return(0)
  }
  
  # otherwise, I will always cooperate
  return(1)
}


ethanbardsley <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  if (GameNum == 1) {
    return(1)  
    # Cooperate in the first game
  } else {
    if (tail(MyMoves, 1) == 1 & tail(OpMoves, 1) == 1) {
      return(1)  
      #Tail function views the value in the MYMoves and OpMoves vector. 
      #If we both cooperated in last game then I will continue to cooperate. 
    } else if (tail(MyMoves, 1) == 0 & tail(OpMoves, 1) == 0) {
      return(0)  
      #If we both defected in the last move I will continue to defect becuase I assume they will. 
    } else if (tail(MyMoves, 1) == 1 & tail(OpMoves, 1) == 0) {
      return(0) 
      #If I cooperated and they defected in the last move then I will retaliate and defect.
    } else if (tail(MyMoves, 1) == 0 & tail(OpMoves, 1) == 1) {
      return(1)  
      #If I defected and they cooperated in last move then I will cooperate.
    }
  }
}#Task 1---------
#initial set up of variables in function for use in game
brevanbarton <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) { 
  #Code for the first few rounds. I started by making a vector which stores my opponents last move (OpMoves) and their second to last move (OpMoves-1). This will remeber the opponents last two moves. 
  o.m <- c(OpMoves[c(length(OpMoves-1),length(OpMoves))])
  #Below is code for the first two rounds. For the first response I decided to cooperate and for the second round I decided to be greedy because why not ¯\_(ツ)_/¯
  if(GameNum == 1) { 
    return(1)
  } else if(GameNum == 2) { 
    return(0)
  } else if (GameNum >= 3 & sum(o.m) == 0) {
    return(0) #In the line above I am setting up defalt code for the remainder of the game. If the game number is greater or equal to 3 and the sum of my opponents last two moves is 0, then I also defect. If not then I return 1. 
  } else {
    return(1) 
  }
} 

#I used chatgpt to help me understand several of the functions and improve my 
#code 
KaeganBurtenshaw <- function(GameNum = 1, MyMoves = NA, OpMoves = NA, RetaliationCount = 0) {
# First round I always cooperate
if (GameNum == 1) {
  return(1)
}

# On subsequent rounds, observe the opponent's last move
if (GameNum > 1) {
  OpPrev <- tail(OpMoves, n = 1)  # Get the opponent's last move
  Mymoves3 <- tail(MyMoves, n = 3)  # Get my last three moves (if available)
  # If the opponent cooperated (1) in the last round
  if (OpPrev == 1) {
    # Check how many times we have cooperated in the last three moves
    if (length(Mymoves3) < 3 || sum(Mymoves3 == 1) < 3) {
      return(1)  # Cooperate if fewer than 3 cooperation
    } else {return(0)  # Defect after cooperating three times
    }
  } else {
    # If the opponent defected last time
    RetaliationCount <- RetaliationCount + 1
    if (RetaliationCount < 4) {
      return(0)  # Defect for the first several rounds of retaliation
    } else {
      # After four rounds of retaliation, continue to defect
      if (OpPrev == 1) {
        return(1)  # Return to cooperation if opponent cooperates
      } else {
        return(0)  # Continue to defect otherwise
      }
    }
  }
}
}
LaytonJohnBurtenshaw <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  # Start with cooperation always
  response <- 1  # The one means cooperate
  
  # On the first round, we always cooperate
  if (GameNum == 1) {
    return(response)  # Cooperating on the first move
  }
  
  #  This counts how many times the enemy has defected or cooperated
  opponent_defects <- sum(OpMoves == 0, na.rm = TRUE)
  opponent_cooperates <- sum(OpMoves == 1, na.rm = TRUE)
  
  # This will make decisions based on the opponent's previous behavior
  if (opponent_defects > opponent_cooperates) {
    # If the opponent has defected more than cooperated
    # Check back to what they did in the last round
    if (GameNum > 1 && OpMoves[GameNum - 1] == 0) {
      # Even if they defected last time, we will give them another chance to cooperate
      response <- 1  # So we will be forgiving and cooperate again
    } else {
      #  If they’re defecting a lot, then we will start defecting to protect ourselves
      response <- 0  # Defect to protect ourselves
    }
  } else {
    # If the enemy has cooperated more often than not
    response <- 1  # We keep cooperating to build trust
  }
  
  # For fun and to make the strategy a bit unpredictabe we introduce a 10% chance just to randomly defect
  if (runif(1) < 0.1) {  #  This is the 10% chance to randomly defect
    response <- 0  # Occasionally throwing in a defect to keep them on their toes
  }
  
  return(response)  # Returning to our decision for this round
}


#To start off, we create a function with my name (Ron Esplin) as
#the name of the function as was described in the project outline.
#Next I created some objects "X" and "Y" to represent my moves and
#my opponents previous moves to be used later on in the function.
#This will sum all of the previous moves to give a rough estimate of
#has been happening in the game or the amount of cooperations and 
#defects.
RonEsplin <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  X <- sum(MyMoves)
  Y <- sum(OpMoves)
  #For the next three "If" functions, the function will look at the 
  #game number to decide what to do. If it is game number 1, then it
  #will return a 1. If it is game number 7, then it will return a 0.
  #If it is game number 14, then it will return a 1.
  if (GameNum==1)
    return(1)
  if (GameNum==7)
    return(0)
  if (GameNum==14)
    return(1)
  #Next the function will look at the opponents and my previous moves.
  #If "X" is equal to "Y" (which means that we have done the same 
  #amount of cooperations and defects), then function will return a zero
  #and defect. This will be checked and done unless it is game number
  # 1,7,or 14 as that part of the function will be ran first. 
  if (X==Y)
    return(0)
  #For the next two if functions the main "RonEsplin" function will check
  #what the opponent did in the previous game to make decide on its output.
  #In the first if function, if the game is between game number 5 and game 
  #number 9 (including both of those because it is less/greater than or 
  #equal to), then the game will check the opponents move on the previous 
  #game. If the opponent cooperated (had a 1), then it will return a 1.
  #For the second if function the main function will check the opponents 
  #previous moves if the current game number is between 18 and 22. If the 
  #opponent cooperated (had a 1), then the main function will return a 0 
  #and defect.
  if (OpMoves [GameNum -1] == 1 & GameNum >=5 & GameNum <= 9)
    return(1)
  if (OpMoves [GameNum -1] == 1 & GameNum >=18 & GameNum <= 22)
    return(0)
  #For the next if function if the game number is between 2 and 7 (but
  #not including 7 this time as it is just the less than sign), then the 
  #main function will return a random number from a sample contain 0 and 1.
  #This means that a random choice will be made on these games.
  if (GameNum >=2 & GameNum < 7)
    return(sample(c(0,1),1))
  #For this if function if the game number is between 8 and 14 but not
  #including game 14, then an rbinom function will be run to determine the
  #output. This binomial will generate one value and one attempt or coin flip
  #will be made. The probability of success or getting a one is 0.75 which means
  #that a return of 1 is more likely for these games.
  if (GameNum >=8 & GameNum < 14)
    return(rbinom(n=1, size=1, prob=0.75))
  #For this if function, it will be pretty similar to the last one. However, the
  #main function will run this between games 15 and 21 but not 21. The only other
  #difference is that the probability will be changed to 0.5 so that a random 
  #or 50/50 chance for a 1 or a 0 to be returned will occur.
  if (GameNum >= 15 & GameNum < 21)
    return(rbinom(n=1, size=1, prob= 0.5))
  #This if function is also similar to the last two. The main differences is that
  #it will take game numbers 21 to 30 (but not 30) and the probability of a
  #success or a 1 is less likely. The probability is at 0.3 so a return of 0
  #or a defect is more likely.
  if (GameNum >=21 & GameNum < 30)
    return(rbinom (n=1, size=1, prob=0.3))
  #This if function will run if none of the other if requirements have been
  #met and the game number is between 30 and 36 including 36. If "X" is 
  # greater than "Y" (meaning that my function has chosen to cooperate more
  #than my opponent has), then a return of 0 will occur. 
  if (GameNum >=30 & GameNum <=36 & X>Y)
    return(0)
  #This part of the function means that if none of the requirements or if 
  #functions are met, then a one will be returned. The "else" just means that
  # for everything else, then something will occur (in this case, a return of 1).
  else 
    return(1)
}




MiaHammond <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  ##The code will provide a random 0 or 1 for the first 5 runs of the game.
  if (GameNum <= 5){
    return (rbinom (n = 1, size = 1, prob= 0.5))
    ##The code will return a 0 if the OpMoves are less than or equal to 1 for games 7 through 12.
  } else if (GameNum > 6 && GameNum < 13 && sum(OpMoves, na.rm = TRUE) <= 1) {
    return(0)
    ##The code will return a 0 for game 6.
  } else if (GameNum == 6){
    return (0)
    ##The code will return a 1 for game 13.
  } else if (GameNum == 13){
    return (1)
    ##The code will return a random 0 or 1 for game 15.
  } else if (GameNum == 15){
    return (rbinom (n = 1, size = 1, prob= 0.5))
    ##The code will return a 0 for any game greater than 15. 
  } else if (GameNum > 15){
    return (0)
    ##The code will return a 1 for any game not covered by the previous code. 
  } else {
    return (1)
  }
}#This game strategy is called Forgive but don't Forget. 
#This line of code sets the function to my name and the variables that are used as inputs in the function.

RUSSELLHANSON<-function(GameNum=1,MyMoves=NA,OpMoves=NA){
  #In this first line of conditions, we check to see if the game number is the first 4 games, or every fifth game after game 8; if one of these games, automatic cooperate.
  #This is part of the strategy. 
  #If there was no breaks, the function would continue to defect until the game ends. 
  #Here is the forgiveness, a break in the defecting to give the opponent a chance to change.
  if(GameNum %in% c(1,2,3,4,8,13,18,23,28,33,38,43,48,53,58)){
    return(1)
  }
  #Here is the "Don't forget" part of function. 
  #In the next two lines of code, we check if either 3 moves back or 3 and 4 of my moves back was a defect, we automatically defect. 
  #It will continue a pattern because we are always checking 3 moves back after that and defecting if 0, and not stop till the "forgiveness". 
  if(MyMoves[GameNum-4]==0 & MyMoves[GameNum-3]==0){
    return(0)
  }
  if(MyMoves[GameNum-3]==0){
    return(0)
  }
  #These last two lines of code will check the code to see if there is initial compliance in the last move. If not, it will defect. This leaves a trail of 0 my moves that with earlier code will read to see if we should come back to punish the opponent and defect.
  if(OpMoves[GameNum-1]==1){
    return(1)
  }
  return(0)
}

EllieHartman<-function(GameNum = 1, MyMoves = NA, OpMoves = NA){
  #For the first 8 rounds, I will cooperate in order to build trust.
  if(GameNum<=8){
    return(1)
    #For the round 30 to the end of the game, I will only defect to try and gain more points as a 
    #last effort
  }else if(GameNum>=30){
    return(0)
    #For games between 5 and 30 I will base my move off of my opponents previous move, so if they are 
    #likely to cooperate I will cooperate and if they are likely to defect I will defect
  }else if(sum(OpMoves[GameNum-1]==1)){
    return(1)
  }else if(sum(OpMoves[GameNum-1]==0)){
    return(0)
  }
}


#My strategy is going to just repeat back what they did but add random switch ups
connerhilton <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  
  #These three probs will randomly defect, randomly cooperate, and randomly cooperate every once in a while adding unpredicability
  # 20% chance to cooperate after opponent defects
  forgiveness.prob <- 0.2
  # 10% chance to randomly defect
  random.defect.prob <- 0.1 
  #10% chance to randomly cooperate
  random.coop.prob <- 0.1   
  
  #Start off on a good note w/ cooperation
  if (GameNum == 1) {
    return(1)
  }
  
  # Random chance to defect, introducing unpredictability
  defect <- runif(1)
  if (defect < random.defect.prob) {
    return(0)  
  }
  
  #Random chance to cooperate even if opponent defected to stay unpredictable
  coop <- runif(1)
  if (coop < random.coop.prob) {
    return(1)
  }
  
  #Repeat whatever they did
  if (OpMoves[GameNum - 1] == 1) {
    return(1)  #Cooperate if they did 
  } else {
    #If the opponent defected, decide if I will forgive
    forgive <- runif(1)
    if (forgive < forgiveness.prob) {
      return(1)  # Forgive
    } else {
      return(0)  #still defect
    }
  }
}




#my function
emilyjustus <- function(GameNum = 1, MyMoves = NA, OpMoves = NA){
  
  #if game number is less than or equal to 8 it will randomly choose defect or cooperate  
  if (GameNum <= 8)
    return(rbinom(n=1, size=1, prob=0.5))
  
  #after 8 games my next move will equal whatever my opponent did in the last round
  else if (OpMoves[GameNum-1]==1) 
    return(1)
  else if (OpMoves[GameNum-1]==0) 
    return (0)
  
  #if game number goes to 27 or above then i will defect every time until the game is over hehe
  if (GameNum >= 27) 
    return (0)}


EmilyKuehnl <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  #I want to fake generosity by cooperating the first 4 games. False sense of
  #security.
  if (GameNum <= 4) {
    return(1)
  }
  #Mostly cooperate, but occasionally defect to introduce uncertainty over my 
  #previous niceness.
  else if(GameNum > 4 && GameNum < 15) {
    return(rbinom(1, 1, 0.9)) 
  }
  #If the game number is between 15 and 23, I will cooperate or defect based on 
  #the ratio of the sum of my opponents move throughout the games.
  else if(GameNum > 15 && GameNum < 23) {
    return(rbinom(n=1, size=1, prob=sum(OpMoves/GameNum)))
  }
  #For games between 23 and 35, I want to cooperate or defect based on my 
  #opponents previous move... so I will match their move. Just to weird them out 
  #that I am copying them.
  else if(GameNum > 23 && GameNum < 35){
    return(OpMoves[GameNum-1])  
  }
  #For all games after 35, cooperate.
  else if (GameNum > 35){
    return(1)
  }
  #Then, if my opponents previous move was cooperation, I want to defect. Just
  #because I feel like breaking their trust.
  else if (OpMoves[GameNum-1]==1){
    return(0)
  }
  #If my opponents previous move was to defect, I also want to defect. Because if 
  #they do me dirty I want to do them dirty back.
  else if(OpMoves[GameNum-1]==0){
    return(0)
  }
}




makinleylarsen <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  
  # my default move will be a random move. 
  random_move <- rbinom(1, 1, 0.2) # 20% chance randomly defect or 80% to cooperate
  
  if (GameNum == 1) {
    return(0) # For round 1, I'll always start by defecting, because I think most people will start with cooperating. 
  }
  
  # Count opponent's defections
  opdefects <- sum(OpMoves == 0)
  
  # if opponent defected last round, I'll defect as well, but delayed by a round to potentially throw them off instead of immediately retaliating. 
  if (GameNum > 2) {
    if (OpMoves[GameNum - 1] == 0) {
      if (OpMoves[GameNum - 2] == 0) {
        return(0)
      }
    }
  }
  
  # For my next stratefy, I'll first check if GameNum is 5, 10, 15, etc. using a loop
  is_multiple_of_5 <- FALSE
  for (i in 1:200) {
    if (GameNum == i * 5) {
      is_multiple_of_5 <- TRUE
    }
  }
  
  # Every 5th round I'll do the opposite of the move I did in the previous round to add another layer of unpredictability
  if (is_multiple_of_5) {
    return(ifelse(MyMoves[GameNum - 1] == 1, 0, 1)) 
  }
  
  # If the opponent has mostly cooperated, I'll mix in more defections to take advantage of them
  if (opdefects < GameNum / 2) {
    return(ifelse(runif(1) < 0.3, 0, 1)) # 30% chance to defect if they mostly cooperate
  }
  
  # If none of these conditions apply, default with the random move specified at the beginning
  return(random_move)
}#We start by naming the function that we are building with my First and Last Name.
AlexLemon <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  #My first condition is set for the first three games. The output for the first three games will be 1.
  if (GameNum <= 3) {
    return(1)
    #The second condition is for game number 4. This will output a 0.
  } else if (GameNum == 4) {
    return(0)
    #The next condition is for Games 5-7. This will output a 0 if the opponent has output a one in no more than 3 rounds.
  } else if (GameNum > 4 && GameNum < 8 && sum(OpMoves) <= 3) {
    return(0)
    #This condition is for Game 8. This will output a 0 if the random binomial sample with a probability of 0.5 results in at least 30 successes in 50 trials.
  } else if (GameNum == 8 && sum(rbinom(50, 1, 0.5)) >= 30) {
    return(0)
    #This condition is for Games 9-20. It will output a 0 if the opponent has output a 1 more often than a 0.
  } else if (GameNum > 8 && GameNum <= 20 && sum(OpMoves)/GameNum >= 0.5) {
    return(0)
    #This condition is more or less random for game 21. I just made up a dbinom function that will most likely result in a success, outputting a 1.
  } else if (GameNum == 21 && dbinom(25, 26, prob = sum(OpMoves)/GameNum) >= 0.6) {
    return(1)
    #This condition is for any game number larger than 22. This will just look at the previous move by the opponent and if they were cooperative last time, we will take from them.
  } else if (GameNum >= 22 && length(OpMoves) >= GameNum && OpMoves[GameNum - 1] == 1) {
    return(0)
  } else {
    #This makes the default barring none of the other conditions are met in the code to 1.
    return(1)
  }
}


SydneyLinsenmann <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  if (GameNum == 1) {
    return(rbinom(1,1,0.5)) # Return a random move
  }
  #Random move for the first game
  if (OpMoves[GameNum-1]==1) {
    return(0) # If opponent's last move was 1, return 0
  } else {
    return(1) # Otherwise, return 1
  }
}



makenzielong <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) { 
  
  # ON THE FIRST GAME I CHOOSE TO COOPERATE
  
  if (GameNum == 1) {
    return(1)
  }
  
  # ON THE SECOND GAME I CHOOSE TO DEFECT
  
  if (GameNum == 2) {
    return(0)
  }
  
  # CALCULATING THE SUM OF OPPONENT'S MOVES 
  # IF THE SUM IS GREATER THAN OR EQUAL TO ONE, I WILL COOPERATE
  
  if (sum(OpMoves, na.rm = TRUE) >= 1) {
    return(1)
  }
  
  # IF GAME NUMBER IS LESS THAN OR EQUAL TO 3 I WILL COOPERATE, BUT IF THE 
  # GAME NUMBER IS EXACTLY 5 I WILL DEFECT
  
  if (GameNum <= 3) {
    return(1)
  } else if (GameNum == 5) {
    return(0)
  }
  
  # EVEN OR ODD GAMENUM
  
  # USING %% TO DETERMINE IF THE GAME NUMBER IS EVEN OR ODD
  
  if (GameNum %% 2 == 0) {
    
    # IF GAME NUMBER IS EVEN AND OPPONENT'S MOVES ARE GREATER THAN ONE
    
    if (sum(OpMoves == 1, na.rm = TRUE) > 1) {
      return(1)  # I WILL COOPERATE
    } else {
      return(0)  # I WILL DEFECT
    }
  } else {
    
    # IF GAME NUMBER IS ODD AND OPPONENT HAS DEFECTED
    
    if (sum(OpMoves == 0, na.rm = TRUE) > 0) {
      return(0)  # I WILL DEFECT
    } else {
      return(1)  # I WILL COOPERATE
    }
  }
  # FOR GAME NUMBERS 7 - 10
  
  if (GameNum >= 7 && GameNum <= 10) {
    if (sum(OpMoves, na.rm = TRUE) < 3) {
      return(1)  #I WILL COOPERATATE IF THEY HAVE DEFECTED LESS THAN 3 TIMES
    } else {    
      return(0)  # IF THEY HAVE DEFECTED 3 OR MORE TIMES I WILL DEFECT
    }
  }
  
  #  FOR GAME NUMBERS 11 - 20
  
  if (GameNum >= 11 && GameNum <= 20) {
    if (mean(OpMoves, na.rm = TRUE) < 0.5) {
      return(1)   # I WILL COOPERATE IF OPMOVES AVERAGE IS LESS THAN 0.5
    } else {       
      return(0)  # I WILL DEFECT IF IT IS MORE THAN 0.5
    }
  }
  
  #  FOR GAME NUMBERS 21 - 30
  
  if (GameNum >= 21 && GameNum <= 30) {
    if (sum(OpMoves == 1, na.rm = TRUE) < 2) {
      return(1)  # I WILL COOPERATE IF OP HAS COOPERATED LESS THAN TWO TIMES
    } else { 
      return(0)  # I WILL DEFEECT IF OP COOPERATED TWO TIMES OR MORE
    }
  }
  
  #  FOR GAME NUMBERS 31 - 40
  
  if (GameNum >= 31) {
    if (sum(OpMoves == 0, na.rm = TRUE) > 3) {
      return(0)    # I WILL DEFECT IF OP DEFECTED MORE THAN 3 TIMES  
    } else {   
      return(1)  # I WILL COOPERATE IF OP DEFECTED 3 TIMES OR LESS
    }
  }
  
  # RANDOM DECISION BASED ON OPPONENT'S MOVES
  
  if (rbinom(n = 1, size = 1, prob = sum(OpMoves, na.rm = TRUE) / GameNum)) {
    return(1)
  }
}

AdamMerrill <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  
  # cooperate on first seven turns
  if(GameNum <= 7) {
    return(1)
  }
  # Betray on last round (will happen rarely if at all because of variable game lengths)
  else if(GameNum == 40) {
    if(OpMoves[39] == 1) {
      print("You've been betrayed, nerd. B)")
    }
    return(0)
  }
  # if opponent defected on last two moves, defect
  else if(OpMoves[GameNum - 2] == 0 && OpMoves[GameNum - 1] == 0) {
    if(GameNum == 8) {
      print("I HAVE BEEN BETRAYED! :(")
    }
    return(0)
  }
  # if opponent defected 50% or more of the time in the last 5 rounds, defect
  else if(mean(OpMoves[(GameNum-5):(GameNum-1)]) <= 0.5){
    return(0)
  }
  else {
    return(1)
  }
}




EthanRico <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  # Payoff matrix
  payoff_matrix <- matrix(c(-5, 2, -4, 1), nrow = 2, dimnames = list(c("Cooperate", "Defect"), c("Cooperate", "Defect")))
  
  # Fitness calculation
  fitness_boost <- 1
  fitness_cost_defect <- -4
  fitness_cost_cooperate <- -5
  fitness_benefit_defect <- 2
  
  # Calculate fitness based on previous moves
  my_fitness <- sum(payoff_matrix[1, ] * MyMoves) + sum(payoff_matrix[2, ] * OpMoves)
  
  # Implement simple evolutionary strategy
  if (length(MyMoves) < 2) {
    # Start with random choice
    next_move <- sample(c(0, 1), 1)
  } else {
    # Analyze opponent's behavior
    opponent_coops <- sum(OpMoves[-length(OpMoves)])
    opponent_defects <- length(OpMoves) - opponent_coops
    
    # Simple learning rule
    if (opponent_coops > opponent_defects) {
      # If opponent cooperates more, increase chance of cooperation
      next_move <- sample(c(0, 1), 1, prob = c(0.3, 0.7))
    } else {
      # Otherwise, increase chance of defection
      next_move <- sample(c(0, 1), 1, prob = c(0.7, 0.3))
    }
    
    # Adjust based on fitness
    if (!is.na(my_fitness)) {
      next_move <- sample(c(0, 1), 1, prob = c(0.4, 0.6))
    }
  }
  
  return(next_move)
}


AbbySherman <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  #First two games will output 1 always.
  if (GameNum <=2){
    return(1) 
    #Game 3 will output 0 always
  } else if (GameNum==3){
    return(0)
    # Check if OpMoves is NA or contains only NA values
  } else if (all(is.na(OpMoves))) {
    return(1)
    # if the mean of the opponents games is greater than or equal to 0.6 it will output 1. 
  } else if (mean(OpMoves,na.rm = TRUE) >=0.6){
    return(1)
    #if the mean of the opponents games is lower than 0.6 it will output 0.
  } else if (mean(OpMoves,na.rm = TRUE) <0.6){
    return(0)
    # default action
  } else {
    return (1)
  }
}




#Used ChatGPT to figure out how to do the "or" part of my first if. 
JaydenThompson <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  if (GameNum == 1 | GameNum == 5 | GameNum == 7) { 
    return(0)
  } else { 
    if (OpMoves[length(OpMoves)] == 1){
      return(1)
    }
    return(0)  
  }
}





EthonVanNoy <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  ## For the first six games return 1
  if (GameNum <= 6){
    return(1)
  } ##After the first six games check the last five games and if every answer
  ##is one, return 0
  else if ((sum(OpMoves[(GameNum-5):(GameNum-1)]) / 5) == 1){
    return(0)
  } ##After the first six games if they haven't played five continuous 1s,
  ##check the probability of Op playing 1s. If the frequency of Op playing 1s
  ##is less than .6 return 0. If greater than or equal to .6, return 0
  else if ((sum(OpMoves[1:(GameNum-1)]) / (GameNum-1)) < .6){
    return(0)
  } else if ((sum(OpMoves[1:(GameNum-1)]) / (GameNum-1)) >= .6){
    return(1)
  } ##If all else fails, return 0
  else {
    return(0)
  }
}
##Programming Project 2 - Prisoners Dilemma
AnnieWatson <- function(GameNum =1, Mymoves = NA, Opmoves=NA) {
  # Calculate the average of the opponent's moves and define as sum1
  sum1 <- sum(Opmoves)/GameNum
  
  #Cooperate in the first 3 games 
  if (GameNum < 4) {
    return(1)
    ##Deflect in games 4 through 9
  } else if (GameNum >= 4 && GameNum <= 9) {
    return(0)
    ##In games 10:16 cooperate if opponent cooperates more than half the time.
  } else if (GameNum >= 10 && GameNum <= 16) {
    if (sum1 >.5) {
      return(1)
    } else {
      return(0)
    } 
    ##for all other games cooperate 80% of the time and deflect 20%.
  } else {
    biasedrando <- runif(1) #Generate a number btwn 0 and 1
    if (biasedrando <.8) {
      return(1)
    } else {
      return(0)
    }
  }
}


# Project 2 ----
# Creating my function given the parameters stated in the assignment
JoshWesthora <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
  
  # Deciding to either cooperate (1) or defect (0) in the first round
  if (GameNum == 1) {
    return(0)  # Some men just want to see the world burn. (I don't trust my opponent)
  }
  
  # If it isn't the first round, I will calculate the opponent's previous moves
  OpD <- sum(OpMoves == 0) #How many times they defected
  OpC <- sum(OpMoves == 1) #How many times they cooperated
  
  # Deciding to either defect or cooperate based on the amounts of times they cooperate/defect
  if (OpD > OpC) {
    return(0)  # If they have defected more than they have cooperated, I will defect
  } else if (OpD == OpC) {
    return(0)  # If they have equal numbers of defects as cooperations, I will defect because that's a little shady
  } else {
    return(1)  # If they have not defected more than they have cooperated, I will trust them and cooperate
  } 
}


LilyYoung<-function(GameNum=1, MyMoves=NA, OpMoves=NA){
  #Cooperate on the first game
  if (GameNum==1){
    return(1)
    #After game 1, if my opponent cooperated
    #in their previous game, I will defect
  } else if (OpMoves[GameNum-1]==1){
    return(0)
    #If my opponent defected in their previous
    #game, I will cooperate
  } else if (OpMoves[GameNum-1]==0){
    return(1)
    #From game 12 on, I will cooperate
  } else if (GameNum>=12){
    return(1)
  }
}
#I know that this method isn't likely to 
#let me win, but I do think it will be interesting
#to see how this holds up against other code
#Especially code that really takes into account
#my previous moves.









#####Known strategies
#For the sake of time I had ChatGPT whip up some code for some well-known 
#strategies for this type of game. Try running the implementation code 
#with our code as well as these to see how they fared against your own!
# Tit-for-Tat Strategy
TitForTat <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
    if (GameNum == 1) {
        return(1)  # Start by cooperating
    } else {
        return(OpMoves[GameNum - 1])  # Mirror opponent's last move
    }
}

# Tit-for-Two-Tats Strategy
TitForTwoTats <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
    if (GameNum == 1) {
        return(1)  # Start by cooperating
    } else if (GameNum == 2) {
        return(1)  # Cooperate for at least two rounds
    } else if (OpMoves[GameNum - 1] == 0 && OpMoves[GameNum - 2] == 0) {
        return(0)  # Defect if opponent defected twice in a row
    } else {
        return(1)  # Otherwise, cooperate
    }
}

# Probabilistic Strategy
ProbabilisticStrategy <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
    if (GameNum == 1) {
        return(1)  # Start by cooperating
    } else {
        defect_rate <- sum(OpMoves == 0) / length(OpMoves)
        if (defect_rate > 0.5) {
            return(0)  # Defect if opponent defected > 50% of the time
        } else {
            return(1)  # Cooperate otherwise
        }
    }
}

# Grim Trigger Strategy
GrimTrigger <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
    if (GameNum == 1) {
        return(1)  # Start by cooperating
    } else if (0 %in% OpMoves) {
        return(0)  # Defect forever if opponent defected once
    } else {
        return(1)  # Otherwise, cooperate
    }
}

# Win-Stay, Lose-Shift Strategy
WinStayLoseShift <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
    if (GameNum == 1) {
        return(1)  # Start by cooperating
    } else {
        last_payoff <- ifelse(MyMoves[GameNum - 1] == 1, 
                              ifelse(OpMoves[GameNum - 1] == 1, 1, -5), 
                              ifelse(OpMoves[GameNum - 1] == 1, 2, -4))
        if (last_payoff > 0) {
            return(MyMoves[GameNum - 1])  # Repeat last move if payoff was positive
        } else {
            return(1 - MyMoves[GameNum - 1])  # Switch move if payoff was negative
        }
    }
}

# All-Cooperate Strategy
AllCooperate <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
    return(1)  # Always cooperate
}

# All-Defect Strategy
AllDefect <- function(GameNum = 1, MyMoves = NA, OpMoves = NA) {
    return(0)  # Always defect
}











