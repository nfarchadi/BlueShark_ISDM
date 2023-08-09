# function to enhance spatial data

enhance_data<-function(input_df, env_dir, add_error = FALSE){
  yearmon <-unique(input_df$year_mon) #get unique year_mon from df
  enhanced_df<-data.frame()#need a empty df

  #for loop that subsets input df daily 
  #then enhances that data specific to the same year_mon of a raster file
  #rbinds it all at the end
  for (i in 1:length(yearmon)){ 
    yearmon_subset <- filter(input_df, 
                        input_df$year_mon == yearmon[i])
    
    yearmon_subset<-yearmon_subset[order(yearmon_subset$pres_abs, decreasing = TRUE),] # this just makes sure everything is in order when we extract
    
    if(!is(yearmon_subset, "sf")){
      yearmon_subset<-sf::st_as_sf(yearmon_subset, coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    }
    
    ym <- as.Date(yearmon[i]) %>% format("%Y-%m") 
    #bring in the same env year_mon
    env_file_list <- grep(list.files(path = env_dir, full.names = TRUE, pattern = ".grd"), pattern = ym, value = TRUE)
    
    if(length(env_file_list)==0){
      next
    }
    
    print(yearmon[i])
    
    env_yearmon_stack<-stack(env_file_list)
    
    # when want to incorporate error in the extracting environmental data
    if(add_error == TRUE){
      # need to extract first for presence data as this has the error
      yearmon_presence <- yearmon_subset %>% filter(pres_abs == 1) %>% as.data.frame()
      pres.env <- data.frame()
      
      for (j in 1:nrow(yearmon_presence)){

        error_grid <- raster::extent(yearmon_presence[j,"lon"] - yearmon_presence[j,"longitudeError"],
                                                    yearmon_presence[j,"lon"] + yearmon_presence[j,"longitudeError"],
                                                    yearmon_presence[j,"lat"] - yearmon_presence[j,"latitudeError"],
                                                    yearmon_presence[j,"lat"] + yearmon_presence[j,"latitudeError"])
        
       
        if(!(error_grid[1] == error_grid[2] & error_grid[3] == error_grid[4])){
          pres.env_mean <- raster::extract(env_yearmon_stack, error_grid) %>% as.data.frame() %>% 
                          summarise(across(everything(), mean, na.rm = TRUE))
        } else {
          pres.env_mean <- raster::extract(env_yearmon_stack, yearmon_presence[j,] %>% 
                                           sf::st_as_sf(coords = c("lon","lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) %>% 
                            as.data.frame()
        }             
        
        pres.env <- rbind(pres.env, pres.env_mean)
        }
     
      # now exact for absence data which has no error
      yearmon_absence <- yearmon_subset %>% filter(pres_abs == 0)
      
      abs.env <- raster::extract(env_yearmon_stack, yearmon_absence)
      
      # combine
      pts.env <- rbind(pres.env, abs.env)
      
    } else {
      pts.env <- raster::extract(env_yearmon_stack,yearmon_subset)
    }
    
    yearmon.pts.env<-cbind(sf::st_coordinates(yearmon_subset),yearmon_subset,pts.env)%>% dplyr::select(-c("geometry")) %>% mutate(id=yearmon_subset$id)
    
    enhanced_df<-rbind(enhanced_df,yearmon.pts.env)
  }
  return(enhanced_df) #returns the fully enhanced df
}

