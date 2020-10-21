#main simulation loop
library(svMisc)

#number of rids of the bus
n <- 10000

fullDeck <- data.frame(
    val = rep(2:14, 4),
    name = rep(c(2:10, "J", "Q", "K", "A"), 4),
    suit = c(rep("H", 13), rep("D", 13), rep("C", 13), rep("S", 13)),
    color = c(rep("red", 26), rep("black", 26))
)

deck <- fullDeck[sample(1:52),]

records <- data.frame(
    r1g = as.numeric(),
    r1l = as.numeric(),
    r2g = as.numeric(),
    r2l = as.numeric(),
    r2b = as.numeric(),
    r3g = as.numeric(),
    r3l = as.numeric(),
    r3b = as.numeric(),
    r4g = as.numeric(),
    r4l = as.numeric()
)

console <- data.frame(
    id = as.numeric(),
    messages = as.character()
)

for(i in 1:n){
    if(i %% 100 == 0){
        message(paste("i = ", i))
    }
    
    round <- rideTheBus(deck, i)
    
    r <- round[[1]]
    deck <- round[[2]]
    c <- round[[3]]
    
    records <- rbind(records, r)
    # uncomment this line if you want to collect the 
        # console messages
    # console <- rbind(console, c)
    
    gc()
}