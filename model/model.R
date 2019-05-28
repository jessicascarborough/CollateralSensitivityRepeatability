# install.packages("binaryLogic")
library(binaryLogic)

install.packages("compositions")


# Helper Functions -------------------------------------------------------------

convert_IntToGenotype <- function(anInt, padding){
  # Converts an integer to a genotype by taking the binary value 
  # and padding to the left by 0s
  bin <- as.binary(anInt + 2**padding)
  return(bin[2:length(bin)])
}

convert_GenotypeToInt <- function(x) {
  return(as.integer(x))
}

calc_hammingDistance <- function(genotype1, genotype2){
  if (length(genotype1)!=length(genotype2)){
    stop("Lengths of genotype1 and genotype2 must match.")
  }
  return(sum(c(genotype1)!=c(genotype2)))
}

get_oneStepNeighbors <- function(genotype){
  result <- list()
  for (i in 1:length(genotype)){
    temp_genotype <- genotype
    temp_genotype[i] <- !temp_genotype[i]
    result[[i]] <- temp_genotype
  }
  return(result) 
}



# Fitness Landscape Class ------------------------------------------------------

fitness_landscape <- function(landscape_values, name = NULL) {
  genotype <- get_fit
  value <- list(name = name,
    landscape_values = landscape_values, 
    genotype = genotype)
  attr(value, "class") <- "fitness_landscape"
  value
}


test_landscape <- fitness_landscape(c(1.851, 2.082, 1.948, 2.434, 2.024, 2.198, 2.033, 0.034, 
                         1.57, 2.165, 0.051, 0.083, 2.186, 2.322, 0.088, 2.821), 
                       "Ampicillin")




# Create Fitness Landscapes

AMP <- FitnessLandscape(c(1.851, 2.082, 1.948, 2.434, 2.024, 2.198, 2.033, 0.034, 
                         1.57, 2.165, 0.051, 0.083, 2.186, 2.322, 0.088, 2.821), 
                        "Ampicillin")
AM  <- FitnessLandscape(c(1.778, 1.782, 2.042, 1.752, 1.448, 1.544, 1.184, 0.063, 
                         1.72, 2.008, 1.799, 2.005, 1.557, 2.247, 1.768, 2.047), 
                        "Amoxicillin")
CEC	<- FitnessLandscape(c(2.258, 1.996, 2.151, 2.648, 2.396, 1.846, 2.23, 0.214, 
                         0.234, 0.172, 2.242, 0.093, 2.15, 0.095, 2.64, 0.516), 
                        "Cefaclor")
CTX <- FitnessLandscape(c(0.16, 0.085, 1.936, 2.348, 1.653, 0.138, 2.295, 2.269, 
                         0.185, 0.14, 1.969, 0.203, 0.225, 0.092, 0.119, 2.412), 
                        "Cefotaxime")
ZOX <- FitnessLandscape(c(0.993, 0.805, 2.069, 2.683, 1.698, 2.01, 2.138, 2.688, 
                         1.106, 1.171, 1.894, 0.681, 1.116, 1.105, 1.103, 2.591), 
                        "Ceftizoxime")
CXM <- FitnessLandscape(c(1.748, 1.7, 2.07, 1.938, 2.94, 2.173, 2.918, 3.272, 0.423, 
                         1.578, 1.911, 2.754, 2.024, 1.678, 1.591, 2.923), 
                        "Cefuroxime")
CRO <- FitnessLandscape(c(1.092, 0.287, 2.554, 3.042, 2.88, 0.656, 2.732, 0.436, 
                         0.83, 0.54, 3.173, 1.153, 1.407, 0.751, 2.74, 3.227), 
                        "Ceftriaxone")
AMC <- FitnessLandscape(c(1.435, 1.573, 1.061, 1.457, 1.672, 1.625, 0.073, 0.068, 
                         1.417, 1.351, 1.538, 1.59, 1.377, 1.914, 1.307, 1.728), 
                        "Amoxicillin+Clav")
CAZ <- FitnessLandscape(c(2.134, 2.656, 2.618, 2.688, 2.042, 2.756, 2.924, 0.251, 
                         0.288, 0.576, 1.604, 1.378, 2.63, 2.677, 2.893, 2.563), 
                        "Ceftazidime")
CTT <- FitnessLandscape(c(2.125, 1.922, 2.804, 0.588, 3.291, 2.888, 3.082, 3.508, 
                         3.238, 2.966, 2.883, 0.89, 0.546, 3.181, 3.193, 2.543), 
                        "Cefotetan")
SAM <- FitnessLandscape(c(1.879, 2.533, 0.133, 0.094, 2.456, 2.437, 0.083, 0.094, 
                         2.198, 2.57, 2.308, 2.886, 2.504, 3.002, 2.528, 3.453), 
                        "Ampicillin+Sulbactam")
CPR <- FitnessLandscape(c(1.743, 1.662, 1.763, 1.785, 2.018, 2.05, 2.042, 0.218, 
                         1.553, 0.256, 0.165, 0.221, 0.223, 0.239, 1.811, 0.288), 
                        "Cefprozil")
CPD <- FitnessLandscape(c(0.595, 0.245, 2.604, 3.043, 1.761, 1.471, 2.91, 3.096, 
                         0.432, 0.388, 2.651, 1.103, 0.638, 0.986, 0.963, 3.268), 
                        "Cefpodoxime")
TZP <- FitnessLandscape(c(2.679, 2.906, 2.427, 0.141, 3.038, 3.309, 2.528, 0.143, 
                         2.709, 2.5, 0.172, 0.093, 2.453, 2.739, 0.609, 0.171), 
                        "Piperacillin+Tazobactam")
FEP <- FitnessLandscape(c(2.59, 2.572, 2.393, 2.832, 2.44, 2.808, 2.652, 0.611, 
                         2.067, 2.446, 2.957, 2.633, 2.735, 2.863, 2.796, 3.203), 
                        "Cefepime")