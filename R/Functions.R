#------------------addLevel Function----------------------

#Fonction to add new level in factor variable
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}



#------------------Rescale Function----------------------

#Function to rescale proba
rescalex <- function(a,b,data){
  step1 <- data-min(data,na.rm=T)
  step2 <- b-a
  step3 <- max(data,na.rm=T)-min(data,na.rm=T)
  step4 <- (step1*step2)/step3
  res <- step4 + a
  return(res)}
