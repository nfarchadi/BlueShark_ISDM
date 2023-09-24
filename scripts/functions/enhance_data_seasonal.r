# function to enhance spatial data

enhance_data_seasonal<-function(input_df, env_dir, add_error = FALSE){
  qter <-unique(input_df$quarter) #get unique quarter from df
  enhanced_df<-data.frame()#need a empty df

  #for loop that subsets input df daily 
  #then enhances that data specific to the same quarter of a raster file
  #rbinds it all at the end
  for (i in 1:length(qter)){ 
    qter_subset <- filter(input_df, 
                        input_df$quarter == qter[i])
    
    qter_subset<-qter_subset[order(qter_subset$pres_abs, decreasing = TRUE),] # this just makes sure everything is in order when we extract
    
    if(!is(qter_subset, "sf")){
      qter_subset<-sf::st_as_sf(qter_subset, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    }
    
    #ym <- as.Date(qter[i]) %>% format("%Y-%m") 
    #bring in the same env quarter
    env_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".grd"), pattern = paste0("Q",qter[i]), value = TRUE)
    
    if(length(env_file_list)==0){
      next
    }
    
    print(paste0("Q",qter[i]))
    
    env_qter_stack<-stack(env_file_list)
    names(env_qter_stack) <- names(env_qter_stack) %>% stringr::str_replace("Q.","")
    
    # when want to incorporate error in the extracting environmental data
    if(add_error == TRUE){
      # need to extract first for presence data as this has the error
      qter_presence <- qter_subset %>% filter(pres_abs == 1) %>% as.data.frame()
      pres.env <- data.frame()
      
      for (j in 1:nrow(qter_presence)){

        error_grid <- raster::extent(qter_presence[j,"lon"] - qter_presence[j,"longitudeError"],
                                                    qter_presence[j,"lon"] + qter_presence[j,"longitudeError"],
                                                    qter_presence[j,"lat"] - qter_presence[j,"latitudeError"],
                                                    qter_presence[j,"lat"] + qter_presence[j,"latitudeError"])
        
       
        if(!(error_grid[1] == error_grid[2] & error_grid[3] == error_grid[4])){
          pres.env_mean <- raster::extract(env_qter_stack, error_grid) %>% as.data.frame() %>% 
                          summarise(across(everything(), mean, na.rm = TRUE))
        } else {
          pres.env_mean <- raster::extract(env_qter_stack, qter_presence[j,] %>% 
                                           sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) %>% 
                            as.data.frame()
        }             
        
        pres.env <- rbind(pres.env, pres.env_mean)
        }
     
      # now exact for absence data which has no error
      qter_absence <- qter_subset %>% filter(pres_abs == 0)
      
      abs.env <- raster::extract(env_qter_stack, qter_absence)
      
      # combine
      pts.env <- rbind(pres.env, abs.env)
      
    } else {
      pts.env <- raster::extract(env_qter_stack,qter_subset)
    }
    
    qter.pts.env<-cbind(sf::st_coordinates(qter_subset),qter_subset,pts.env)%>% dplyr::select(-c("geometry")) %>% mutate(id=qter_subset$id)
    
    enhanced_df<-rbind(enhanced_df,qter.pts.env)
  }
  return(enhanced_df) #returns the fully enhanced df
}

