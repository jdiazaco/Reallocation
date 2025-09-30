add_stars<-function(estimate, p_value){
  stars<-ifelse(p_value<=0.01, "***",
                ifelse(p_value<=0.05, "**",
                       ifelse(p_value<0.1, "*", "")))
  return(paste0(round(estimate, 4), stars))
}
