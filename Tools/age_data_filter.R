age_data_filter<-function(data, threshold_young, subset){
  if(subset=="young"){
    data<-data[firm_age<=threshold_young]
  }else{
    if(subset=="mature"){
      data<-data[firm_age>threshold_young]
    }else{
      if(subset=="all"){
        data<-data
      }else{
        stop("subset not well-defined")
      }
    }
  }
  return(data)
}

