#This is a simulation of how likely it is to win the lottery as well as the powerball. On average, there is a 1 in 292 million chance
#of winning the powerball. Therefore, it's unlikely the simulation will return a powerball winner unless you run this about 300,000,000 
# times. Which, I don't recommend as it would take hours or possibly days for your computer to run. Instead, you can un-commment-out and
#run the lines in the 'for-loop' to rig the system and pick a winning ticket, forcing a match. This is done just to confirm the 
# simulator works.




#the range of possible number picked for each lotto number 
vec<- c(1:69)
#range of possible powerball numbers picked
pb_vec<- c(1:26)
#Selects 5 numbers without replacement. These are the winning lottery numbers a ticket has to much completely to win.
lotto_pick_win<-sample(vec, 5, replace = FALSE)
#Winning powerball number you have to match in addition to the lottery numbers to get the powerball.
powerball_num_win<- sample(pb_vec, 1)
winningtickets<- 0
jackpots<- 0
#The number of lottery tickets sold. The more you put, the longer the script takes to run. Can take days if set to realistic ticket sales.
total_tix_sold<-30000


for (i in 1:total_tix_sold)
{
  #use following line to test function is working (simulates your ticket matching the winning lotto numbers)
  #ticket<-lotto_pick_win
  #if you un-comment-out the above line, comment-out the below line where each lottery ticket is picked 'fairly' (randomly).
  ticket<-sample(vec, 5, replace = FALSE)
  #use following line to test function is working (simulates matching the powerball number as well = jackpot!)
  #pb<- powerball_num_win
  #if you un-comment-out the above line, comment-out the below line where each powerball number is picked 'fairly' (randomly).
  pb<- sample(pb_vec, 1)
  
  #The next few lines check each ticket to see if they match the winning numbers and powerball number.
  if(isTRUE(all.equal(ticket, lotto_pick_win)))
  {
    winningtickets<- winningtickets +1
    
  }
  if(isTRUE(all.equal(ticket, lotto_pick_win)) & (pb == powerball_num_win) )
  {
    jackpots<- jackpots +1
  }
}

#these next lines calculate and state your probabilty of winning both the lotto as well as the powerball 
#(matching lotto numbers and powerball number). If you cheated above and 'rigged' the system by un-commenting-out
# the 'cheat' lines, these numbers should indicate all tickets sold won.
chance_jackpot<- jackpots / total_tix_sold 
chance_win <- winningtickets / total_tix_sold 

chance_win
chance_jackpot

#This just says the number of winning tickets and powerball winners
winningtickets  
jackpots

