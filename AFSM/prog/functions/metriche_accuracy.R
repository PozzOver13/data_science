# metriche per test classificazione

prec_zero <- function(act,pred){  
  tble = table(act,pred)
  round(tble[1,1]/(tble[1,1]+tble[2,1]),4)
}

prec_one <- function(act,pred){ 
  tble = table(act,pred)
  round(tble[2,2]/(tble[2,2]+tble[1,2]),4) 
}

recl_zero <- function(act,pred){
  tble = table(act,pred)
  round(tble[1,1]/(tble[1,1]+tble[1,2]),4)
}

recl_one <- function(act,pred){ 
  tble = table(act,pred)
  round( tble[2,2]/(tble[2,2]+tble[2,1]),4)
}

accrcy <- function(act,pred){ 
  tble = table(act,pred)
  round((tble[1,1]+tble[2,2])/sum(tble),4)
}

