output_dir_creator<-function(output_dir, temp=F, sub_folder=NULL){
  
  # If directories are going to be created dynamically, temp should be true
  if(temp){
    if(!dir.exists(paste0(output_dir_og, sub_folder, "/"))){
      output_dir<<-paste0(output_dir_og, sub_folder, "/")
      temp<-paste0(output_dir_og, sub_folder, "/")
      dir.create(temp)
    }else{
      output_dir<<-paste0(output_dir_og, sub_folder, "/")
    }
    
  }else{
    # Create output folder if it does not exist
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
    
    # Create the test folder if we're in trials
    if(!final){
      output_dir<-paste0(output_dir, "test/")
      if(!dir.exists(output_dir)){
        dir.create(output_dir)
      }
    }
  }
}

