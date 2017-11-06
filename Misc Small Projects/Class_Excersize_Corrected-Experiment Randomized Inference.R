city.names <- c("A", "B", "C", "D", "E", "F", "G", "H")
observed.turnout = c(17, 21, 13, 12, 26, 32, 48, 45)

observed.diffmeans <- mean(observed.turnout[c(2,4,6,8)]) - 
  mean(observed.turnout[c(1,3,5,7)])

print(observed.diffmeans)

bar <- data.frame(city.names, observed.turnout)

foo[sample(3:4)]

# Assignment function
assignment <- function(foo) {
  # Four coin flips, establishing random assignment
  assig        <- foo[sample(1:2),]
  assig[3:4,]  <- foo[sample(3:4),]
  assig[5:6,]  <- foo[sample(5:6),]
  assig[7:8,]  <- foo[sample(7:8),]
  
  treatment.group   <- assig[c(1,3,5,7),]
  control.group     <- assig[c(2,4,6,8),]
  
  
  return(mean(treatment.group[,2]) - mean(control.group[,2]))
}

# Iterating the Assignment function
iter.RI <- function(iterations = 1000) {
  for (i in 1:iterations) 
  {storage.vector[i] <- assignment(bar)
  }
  return(storage.vector)
}

storage.vector <- NULL
results <- iter.RI()


# Exploring the results

quantile(results, prob = c(0.025, 0.975))
quantile(results, prob = c(0.65, 0.7, , 0.75, 0.8, 0.85))

length(unique(results))

hist(results)

#plot the distribution of potential treatment effects if the null hypothesis was true
plot(density(results))
abline(v = -4, lwd = 2, col = "red") # not necessarily 14
abline(v = 4, lwd = 2, col = "red") # not necessarily 14


