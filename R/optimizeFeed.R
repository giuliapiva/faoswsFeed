## This function optimizes the amount of feed (in metric tonnes) of each commodity such that 
## energy and protein requiremetns are both fulfilled using a linear program

library(lpSolve)

optimizeFeed <- function(x, y) { 
  #subset the data for one year in one point in time
  allocation <- subset(availabilityDemand, geographicAreM49 == x & timepointYears == y)
  
# set up objective
objective <- rep(1, length(allocation$geographicAreM49))

# find minimum and maximum feed amounts (corridor of feasible solutions)
allocation[, min := felse(energyBaseFeed < proteinBaseFeed, energyBaseFeed, proteinBaseFeed)]
allocation[, max := felse(energyBaseFeed > proteinBaseFeed, energyBaseFeed, proteinBaseFeed)]                     

# define feed excesses
excessiveEnergyDemand = unique(allocation$residualEnergyDemand) - sum(alllocation$min * allocation$energyContent)
excessiveProteinDemand = unique(allocation$residualProteinDemand) - sum(allocation$min * allocation$proteinContent)

#set up matrix of constants
matrixMinimum = diag(1, nrow=length(allocation[, geographicAreaM49]))
matrixConstants = rbind(allocation[, energyContent], allocation[, proteinContent], matrixMinimum)

# define direction of optimazation 
constantDirection = c(">=", ">=" , rep("<=", length(allocation[, geographicAreaM49])))

# condition constants
feedConstants = c(excessiveEnergyDemand, excessiveProteinDemand, allocation$max - allocation$min )

# Solve the mimimazation problem and save the solution
addFeed = lp(direction = "min", 
              objective.in = objective, 
              const.matrix = matrixConstants, 
              const.dir = constantDirection, 
              const.rhs = feedConstants)$solution

# Join solutions with the main data
addFeed <- data.table(addfeed=addfeed)
allocatedFeed <- cbind(allocation, addfeed)

# establish final amount of allocate Feed
allocatedFeed[, allocatedFeed :=  min + addFeed]

# report back selected columns
allocatedFeed[, .(geographicAreaM49, measuredItemCPC, timePointYears, allocatedFeed)]

}


