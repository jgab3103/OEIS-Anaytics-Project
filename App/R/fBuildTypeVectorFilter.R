 fBuildTypeVectorFilter <- function(pcontacttracingFilter, pcasemanagementFilter, potherredploymentFilter) {
  
   # Purpose of this function is obtain the values used to filter the Type column based on the user inputs from the checkboxInput
   
   # Build vector of dummy variables. Dummy variables change depending on the checkboxInput
   TypeDummyVector0 <- c("Contact tracing" =  base::as.numeric(pcontacttracingFilter), 
                         "Case management" = base::as.numeric(pcasemanagementFilter), 
                         "Other redeployment" = base::as.numeric(potherredploymentFilter))
   
   # Keep only where the dummy = 1
   TypeDummySubsetVector0 <- base::names(TypeDummyVector0[base::grep(TypeDummyVector0, pattern = 1)])
   
}