install.packages("binaryLogic")
library(binaryLogic)



# Helper Functions -------------------------------------------------------------


convert_IntToGenotype <- function(anInt, padding){
  # Converts an integer to a genotype by taking the binary value 
  # and padding to the left by 0s
  bin <- as.binary(anInt + 2**padding)
  return(bin[2:length(bin)])
}


bitsToInt<-function(x) {
  # x <- as.vector(x)
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), x)), "integer")
}

a <- convert_IntToGenotype(30, 5)
bitsToInt(a)

?packBits

?rep

as.logical(as.vector(a))

32-length(a)%%32
a

rep(FALSE, 32-length(a)%%32)
