# function to extract environmental data from locations for LGCP model
f.covar <- function(loc, env) {
    
  # make sure loc and env are SpatVector and SpatRaster data
  if(!inherits(loc,"SpatVector")){
      loc <- terra::vect(loc)
  }
  
  if(!inherits(env,"SpatRaster")){
      env <- terra::rast(env)
  }
  
  # make sure they have the same coordinate reference system (CRS)
  # important for the bru_fill_missing function below
  if(!identical(crs(loc),crs(env))){
      crs(loc)<-crs(env)
  }
    
  # Extract environmental data from locations
  v <- terra::extract(env, loc, ID = FALSE) %>% dplyr::pull(1)
  
  # if there are NA's this will computes nearest-available-value imputation for missing values in space
  if (any(is.na(v))) {
      v <- inlabru:::bru_fill_missing(env, loc, v)
    }
  return(v)
}
