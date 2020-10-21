library(dplyr)

#let's calculate some quick columns
    #points is the sum of losses across all simulated rounds
    #weightedPoints weieghts 2 points for r2 losses, 3 for r3, etc. if that's how you play
    #loss rates are the losses divided by the total guesses per round
records <- records %>% rowwise() %>% mutate(busts = sum(r2b, r3b),
                                 points = sum(r1l, r2l, r3l, r4l),
                                 weightedPoints = sum(r1l, r2l * 2, r3l * 3, r4l * 4),
                                 r1LossRate = r1l / r1g,
                                 r2LossRate = r2l / r2g,
                                 r3LossRate = r3l / r3g,
                                 r4LossRate = r4l / r4g) %>% ungroup()

#quick summary
summary(records$busts)

#quick summary by round
summary(records %>% select(r1LossRate, r2LossRate, r3LossRate, r4LossRate))
#with histograms
par(mfrow = c(2, 2))
hist(records$r1LossRate)
hist(records$r2LossRate)
hist(records$r3LossRate)
hist(records$r4LossRate)
#slightly interesting, I am surprised to see the heavy weight at 0 losses in r3

#the big question!
#how many points should you expect per ride of the bus?
par(mfrow = c(1, 1))
hist(records$points)

#how often can we expect to get between 1 and 5 points?
nrow(records %>% filter(points %in% 1:5))

#what percentile would having 4 busts in one round be?
dist <- ecdf(records$busts)
dist(4)

#do these results match up to intuition?
#we'd think that r1 would have a .5 loss rate on the whole
sum(records$r1l) / sum(records$r1g)
#probability quickhand fails me, but the ~.22 loss rate seems right
sum(records$r2l) / sum(records$r2g)
sum(records$r3l) / sum(records$r3g)
#most importantly, we need this to be .75
sum(records$r4l) / sum(records$r4g)
#which it is!
