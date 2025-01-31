#'Function to write age and length composition data from \code{atlantisom} to Stock Synthesis 3.30. Note: every time this function is called, it overwrites the age and length composition data currently in the \code{ss_data_list} object.
#'@export
#'@param ss_data_list the list object containing SS data
#'@param comp_matrix a list, where each list item is the matrix of composition data (age or length)
#'@param data_rows a list, where each entry is a vector corresponding to the year column of the composition data. For length and age composition data, this is the 1:nyears repped each row for the number of length or age bins. For conditional age-at-length composition data, this is nyears:lengthbins .
#'@param sampling_month a list where each list item is a vector of the sampling month for each composition data type
#'@param data_type a vector of strings, each entry needs to be either "agecomp" or "lengthcomp"
#'@param fleet_number a vector of numbers, each entry corresponds to which fleet the comp data are from
#'@param bins a list item, each item is a vector specifying either the age or length bins
#'@param caal_bool a vector of length \code{ss_data_list} with boolean values for whether the data is conditional age-at-length
#'@return modified ss_data_list that includes added composition data

#' HAP - 2024 - The following script was edited by Holly Ann Perryman to get updated CalCurr and SAR SS files to run

SS_write_comps <- function(ss_data_list,
                           comp_matrix,
                           data_rows,
                           sampling_month,
                           data_type,
                           fleet_number,
                           bins,
                           caal_bool){

  #Check if age and length comp are in the data_type vector and remove all rows if so
  if("agecomp" %in% data_type){
    # HAP - changed this to only take the first 9 columns and thus dropping all a# columns to account for age bins in atlantis differing from the dat file
    ss_data_list$agecomp <- ss_data_list$agecomp[0,c(1:9)]
    age_comp_ind <- which(data_type=="agecomp")
  }

  if("lencomp" %in% data_type){
    # HAP - new column names
    ss_data_list$lencomp <- ss_data_list$lencomp[0,c("year","month","fleet","sex","part","Nsamp")] # c("Yr","Seas","FltSvy","Gender","Part","Nsamp") # month
    len_comp_ind <- which(data_type=="lencomp")
  }

  #Initialize local variables
  k_agecomp <- k_lencomp <- 1
  start_year <- ss_data_list$styr

  for(i in 1:length(comp_matrix)){
    # if(i==3){
    #   browser()
    # }
    k <- switch(data_type[i], "lencomp"=k_lencomp, "agecomp"=k_agecomp)
    # HAP - new column names
    indices <- (k:(k+length(data_rows[[i]])-1))
    ss_data_list[[data_type[i]]][indices,"year"]  <- data_rows[[i]]
    ss_data_list[[data_type[i]]][indices,"month"] <- sampling_month[[i]]
    ss_data_list[[data_type[i]]][indices,"fleet"] <- rep(fleet_number[i], length(data_rows[[i]]))
    ss_data_list[[data_type[i]]][indices,"sex"]   <- rep(0, length(data_rows[[i]]))
    ss_data_list[[data_type[i]]][indices,"part"]  <- rep(0,length(data_rows[[i]]))
    ss_data_list[[data_type[i]]][indices,"Nsamp"] <- comp_matrix[[i]][,"nsamp"]
    type_prefix <- switch(data_type[i],"agecomp"="a","lencomp"="l")

    if(data_type[i]=="agecomp"){
      if(caal_bool[i]){
        ss_data_list[[data_type[i]]][indices,"Lbin_lo"] <- comp_matrix[[i]][,"lower.bins"]
        ss_data_list[[data_type[i]]][indices,"Lbin_hi"] <- comp_matrix[[i]][,"upper.bins"]
      } else{
        ss_data_list[[data_type[i]]][indices,"Lbin_lo"] <- rep(-1,length(data_rows[[i]]))
        ss_data_list[[data_type[i]]][indices,"Lbin_hi"] <- rep(-1,length(data_rows[[i]]))
      }
      ss_data_list[[data_type[i]]][indices, "ageerr"] <- rep(1,length(data_rows[[i]])) # HAP - new column name
    }

    if(data_type[[i]]=="lencomp"){
      if(indices[1]==1){
        ss_data_list[[data_type[i]]][, paste(type_prefix,as.character(bins[[i]]), sep="")] <- 0
      }
    }
    # HAP - I had to edit the following line as it was making lencomp data char, rather than num or int, which was giving me issues
    ss_data_list[[data_type[i]]][indices,paste(type_prefix,as.character(bins[[i]]), sep="")] <- comp_matrix[[i]][,as.character(bins[[i]]), with = FALSE]

    # HAP - have to check for NAs, likely due to mismatching bins across fleets, and turn NAs to zeros
    ss_data_list[[data_type[i]]][is.na(ss_data_list[[data_type[i]]])] <- 0

    if(data_type[i]=="agecomp"){
      k_agecomp <- k_agecomp+length(data_rows[[i]])
    } else{
      k_lencomp <- k_lencomp+length(data_rows[[i]])
    }
  }

  return(ss_data_list)
}

