collinearity <- function(covariates) {
  # input example:   collinearity(covariates = data_observer[ , c("sst","ssh","chl","bathy")]) # just call covariates by their col names and the function does the rest
  ## function written by MCA
  
  ## if NA values present, can give spurious results
  if (nrow(na.omit(covariates)) < nrow(covariates)){
    warning('NA values present in covariates. Using na.omit() to remove them.')
    print(paste0('Input covariates contained ', nrow(covariates), ' rows of data.'))
    covariates <- na.omit(covariates)
    print(paste0('Output covariates, with NA removed, contained ', nrow(covariates), ' rows of data.'))
  }
  
  correlations <- cor(covariates)
  dimlength <- nrow(covariates)
  diags <- seq(1, dimlength ^ 2, by = (dimlength + 1))
  colinears <- which(abs(correlations) > 0.7 & upper.tri(correlations, diag = FALSE) == TRUE)
  
  if (length(colinears) != 0){
    for (i in 1:length(colinears)){
      ind <- which(correlations == correlations[colinears[i]] & upper.tri(correlations, diag = FALSE) == TRUE, arr.ind = TRUE)
      print(paste(rownames(correlations)[ind[1]], colnames(correlations)[ind[2]], sep = ", "))
    }
  } else {print("No pairwise comparisons with |r| > 0.70")}
}