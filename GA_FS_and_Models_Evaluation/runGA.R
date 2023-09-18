library(GA)

#This function is used by the GA to compute or report the statistics of your interest after every generation.
#This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  # gaMonitor(obj)                      #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter                      #get the current iternation/generation number 
  if (iter <= maxGenerations){          #some error checking
    fitness <- obj@fitness              #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.    
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}

runGA <- function(noRuns = 30){
  print("Setting the parameters \n")
    maxGenerations <<- 30    #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 5
    pcrossover = 0.5
    pmutation = 0.01
    type = "binary"
    #crossover = 
    #data <- getData()
    data <- ga_traindata
    xx <<- runGA_traindata
    yy <<- runGA_testdata
    
    fitness = feature_selection_random_forest              #fitness function defined in feature-selection.R
    #fitness = feature_selection_logistic_regression
    #fitness = feature_selection_KNN
    #fitness = feature_selection_ANN
    #fitness = feature_selection_XGBoost
    #fitness = feature_selection_DT
    
  #Set up what stats you wish to note. 
  print("Setting up the stats \n")  
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  print("Starting GA \n")
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    GA <- ga(type="binary", fitness = fitness,nBits = ncol(xx), 
               seed=i, popSize = popSize, 
               pcrossover = pcrossover, pmutation = pmutation, 
               maxiter = maxGenerations, monitor= monitor)
    
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
}

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}