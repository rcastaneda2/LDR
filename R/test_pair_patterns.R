#setwd("C:/Users/Ruben/Desktop/Data for Example/IQ1/")
tdata<- read.csv("C:/Users/nz491/Desktop/02 Model Building Example/Data for Example/IQ1/data.csv")

library(mirt)
library(metafor)

#Pull out items
data<-tdata[,1:25]

#score items as binary
data <- matrix(as.numeric(data == 10),ncol = 25, byrow = F)
dat <- as.data.frame(data)

#given dat, sus, and logic for complex
test_pair_patterns <- function(dat,sus, complex = F){

  N = dim(dat)[2]

  #First we need to apply a 2PL model to the data
  res<-mirt(dat,1,'2PL')

  #Now we obtain the Q3 values of our data.
  #This will give us an mirt object with the Q3 values
  resids<-residuals(res,type="Q3")

  #Now we convert that object into a matrix format.
  #Since we have 25 items we know that it will be a 25*25 matrix with 1's on the diagonal.
  q3<-matrix(resids,ncol=N,nrow=N)

  #Extract upper diagonal of matrix
  newq3 <-matrix(c(q3[upper.tri(q3,diag=FALSE)]),nrow=1)

  #Convert the upper diagonal of Q3 values into a vector for analysis
  newq3 <-as.vector(newq3)

  #Apply fishers r to z transformation to the Q3 values.
  q3= .5*log((1+newq3)/(1-newq3))

  #Compute variances for each effect size
  k<-length(q3)
  obs<-length(tdata[,1])
  v<- rep(1/(obs-3),k)

  #Create moderator that groups items 4, 12 and 13.
  mod <- make_a_vector(N = N, sus = c(4,12,13),complex = F,pattern = "triangle")$mod

  #Now that we have our r-to-z transformed Q3 values, their respective variances and the moderator that
  #models the dependent relationship between our 3 items we can estimate the model using the rma() function
  ld<-rma(q3~mod,v)

  #Insepct results.
  return(summary(ld))

}

test_pair_patterns(dat,c(4,12,13), complex = F)
