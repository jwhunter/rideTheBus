#ride the bus game sim
#uses optimal naivitee

rideTheBus <- function(deck, id) {
    fullDeck <- data.frame(
        val = rep(2:14, 4),
        name = rep(c(2:10, "J", "Q", "K", "A"), 4),
        suit = c(rep("H", 13), rep("D", 13), rep("C", 13), rep("S", 13)),
        color = c(rep("red", 26), rep("black", 26))
    )
    
    #this will keep track of the actions
    console <- c()
    
    #let's keep the following stats per player turn
    r1g <- 0 #r1 guesses
    r1l <- 0 #r1 losses
    r2g <- 0 #r2 guesses
    r2l <- 0 #r2 losses
    r2b <- 0 #r2 busts
    r3g <- 0 #r3 guesses
    r3l <- 0 #r3 losses
    r3b <- 0 #r3 busts
    r4g <- 0 #r4 guesses
    r4l <- 0 #r4 losses
    #number of cards used / guesses made
    
    state <- 1
    #1: red or black
    #2: above or below
    #3: between or outside
    #4: suit
    #5: win
    
    r1Fallback <- sample(c("red", "black"), 1)
    #good theory would say you only ever guess one color...
    r2Fallback <- sample(c("above", "below"), 1)
    #one "above or below" when uncertain...
    r4Fallback <- sample(c("H", "D", "C", "S"), 1)
    #or one suit
    
    
    while(state < 5) {
        #play until you get off the bus
        if (state == 1) {
            #red or black? ----

            console <- c(console, paste0("Round 1: Player chooses ", r1Fallback, "."))
            
            if (nrow(deck) == 1) {
                #the guess is already chosen in r1Fallback, so we pick the top card
                deck <- fullDeck[sample(1:52), ]
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            } else {
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            }
            
            console <- c(console, paste0("Round 1: The ", top$name, " of ", top$suit, " is drawn, a ", top$color, " card."))
            
            r1g <- r1g + 1
            #count the round 1 guess
            
            if (r1Fallback == top$color) {
                #if we guessed correctly
                state <- 2 #advance
                
                console <- c(console, paste0("Round 1: The logic deems this as a WIN for round 1. Progress to round 2."))
            } else {
                r1l <- r1l + 1
                #count the round 1 loss
                
                console <- c(console, paste0("Round 1: The logic deems this as a LOSS for round 1. Stay at round 1."))
            }
            
        } else if (state == 2) {
            #above or below? ----
            
            #first we generate a guess
            if (top$val %in% 2:7) {
                r2guess <- "above"
            } else if (top$val %in% 9:14) {
                r2guess <- "below"
            } else {
                r2guess <- r2Fallback
            }
            
            console <- c(console, paste0("Round 2: Player chooses ", r2guess, "."))
            
            r2g <- r2g + 1
            #count the guess
            
            memory <- top
            #remember what the top card is
            
            #draw the card
            if (nrow(deck) == 1) {
                deck <- fullDeck[sample(1:52), ]
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            } else {
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            }
            
            console <- c(console, paste0("Round 2: The prevous card was a ", memory$name, " and the top card is a ", top$name, "."))
            
            if (top$val == memory$val) {
                #check for bust first, since it encompasses the other logic
                r2b <- r2b + 1
                state <- 1
                
                console <- c(console, paste0("Round 2: The logic deems this as a BUST for round 2. Go back to round 1."))
            } else if (r2guess == "above") {
                #if we chose above
                if (top$val > memory$val) {
                    #if the top card is higher than the previous card
                    state <- 3
                    
                    console <- c(console, paste0("Round 2: The logic deems this as a WIN for round 2. Progress to round 3."))
                } else {
                    #if the top card is lower than the previous card
                    r2l <- r2l + 1
                    state <- 1
                    
                    console <- c(console, paste0("Round 2: The logic deems this as a LOSS for round 2. Go back to round 1."))
                }
            } else {
                #if we chose below
                if (top$val < memory$val) {
                    #if the top card is lower than the previous card
                    state <- 3
                    
                    console <- c(console, paste0("Round 2: The logic deems this as a WIN for round 2. Progress to round 3."))
                } else {
                    #if the top card is higher than the previous card
                    r2l <- r2l + 1
                    state <- 1
                    
                    console <- c(console, paste0("Round 2: The logic deems this as a LOSS for round 2. Go back to round 1."))
                }
            }
        } else if (state == 3) {
            #between or outside? ----
            
            #first we generate a guess
            if (abs(top$val - memory$val) <= 6) {
                #if there's 6 or fewer between the two cards, pick outside
                r3guess <- "outside"
            } else {
                r3guess <- "between"
            }
            
            console <- c(console, paste0("Round 3: Player chooses ", r3guess, "."))
            
            r3g <- r3g + 1
            #count the guess
            
            memory2 <- top
            #remember this card as well
            
            #draw the card
            if (nrow(deck) == 1) {
                deck <- fullDeck[sample(1:52), ]
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            } else {
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            }
            
            console <- c(console, paste0("Round 3: The boundaries are a ", memory$name, " and a ", memory2$name, ". A ", top$name, " was drawn."))
            
            if (top$val == memory$val |
                top$val == memory2$val) {
                #check for bust first, since it encompasses the other logic
                r3b <- r3b + 1
                state <- 1
                
                console <- c(console, paste0("Round 3: The logic deems this as a BUST for round 3. Go back to round 1."))
            } else if (r3guess == "between") {
                #if we chose between
                if (top$val %in% memory$val:memory2$val) {
                    #if the top card is between the last two cards
                    state <- 4
                    
                    console <- c(console, paste0("Round 3: The logic deems this as a WIN for round 3. Progress to round 4."))
                } else {
                    #if not between
                    r3l <- r3l + 1
                    state <- 1
                    
                    console <- c(console, paste0("Round 3: The logic deems this as a LOSS for round 3. Go back to round 1."))
                }
            } else {
                #if we chose outside
                if (top$val %in% memory$val:memory2$val) {
                    #if the top card is between
                    r3l <- r3l + 1
                    state <- 1
                    
                    console <- c(console, paste0("Round 3: The logic deems this as a LOSS for round 3. Go back to round 1."))
                } else {
                    state <- 4
                    
                    console <- c(console, paste0("Round 3: The logic deems this as a WIN for round 3. Progress to round 4."))
                }
            }
        } else if (state == 4) {
            #suit? ----
            
            console <- c(console, paste0("Round 4: Player chooses ", r4Fallback ,"."))
            
            if (nrow(deck) == 1) {
                #the guess is already chosen in r4Fallback, so we pick the top card
                deck <- fullDeck[sample(1:52), ]
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            } else {
                top <- deck[1, ]
                deck <- deck[2:nrow(deck), ]
            }
            
            console <- c(console, paste0("Round 4: The ", top$name, " of ", top$suit, " is drawn."))
            
            r4g <- r4g + 1
            #count the round 1 guess
            
            if (r4Fallback == top$suit) {
                #if we guessed correctly
                state <- 5 #get off the bus!
                
                console <- c(console, paste0("Round 4: The logic deems this as a WIN for round 4. Off the bus!"))
            } else {
                r4l <- r4l + 1
                #count the round 4 loss
                state <- 1
                
                console <- c(console, paste0("Round 4: The logic deems this as a LOSS for round 4. Go back to round 1."))
            }
        }
    }
    
    return(list(
        data.frame(
            r1g = r1g,
            r1l = r1l,
            r2g = r2g,
            r2l = r2l,
            r2b = r2b,
            r3g = r3g,
            r3l = r3l,
            r3b = r3b,
            r4g = r4g,
            r4l = r4l
        ),
        deck,
        data.frame(
            id = id,
            messages = console
        )
    ))
}