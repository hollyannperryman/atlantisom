

##' @title Biomass for age groups
##' @param fgs data from the groups.csv file
##' @param nc_out_path ncdf atlantis' output file
##' @return dataframe: time polygon biomass agecl species
##' @author Demiurgo (ReactiveAtlantis)
##' @editor Holly Perryman - Oct. 2024

calc_bio_age <- function(fgs,
                         nc_out_path,
                         bboxes)
{
  # # --- FOR DEBUGGING
  # fgs = fgs
  # nc_out_path = file.path(dir, nc_out)
  # bboxes = boxes
  # # ---

  # subset group csv data based on verts (groups with _ResN, _StructN, and _Nums data)
  fgs_age <- fgs[fgs$NumCohorts  > 1 & !fgs$InvertType %in%
                   c('PWN', 'PRAWNS', 'PRAWN', 'CEP', 'MOB_EP_OTHER', 'SEAGRASS', 'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE'), ]
  # extract nc data
  nc.out <- ncdf4::nc_open(nc_out_path)
  Time <- nc.out$dim$t$vals / (60*60*24) # get time, convert from seconds to days
  #* HAP, functions in atlantisom expect Time to be the number of time steps and not the days
  Time <- Time / (Time[2] - Time[1])
  # convertion metrics
  mg2t   <- 0.00000002  # mgC converted to wet weight in tonnes = 20/1000000000
  x.cn   <- 5.7   	    # Redfield ratio of C:N 5.7
  # define placeholder for function output data
  func_out_bio <- NULL
  # enter loop over fids
  for(index_fgs in 1 : nrow(fgs_age)){ # looping through fids --- for debugging: index_fgs = 2
    # message to user
    print(paste0(round((index_fgs / nrow(fgs_age)) * 100,2),"% complete computing biomass - ",
                 "processing group ",
                 fgs_age[index_fgs,5]))
    # define placeholder for output age-based output data
    cohort <- NULL
    # enter loop over cohorts to compute biomass
    for(coh in 1 : fgs_age[index_fgs, 'NumCohorts']){ # looping through index_fgs groups --- for debugging: coh = 1
      # get name for nc output
      name.fg <- paste0(fgs_age$Name[index_fgs], coh)
      # get biomass based on the nc output and convert units
      b.coh   <- (ncdf4::ncvar_get(nc.out, paste0(name.fg, '_ResN'))  +
                    ncdf4::ncvar_get(nc.out, paste0(name.fg, '_StructN')))  *
        ncdf4::ncvar_get(nc.out, paste0(name.fg, '_Nums')) * mg2t * x.cn
      #*
      #* HAP, I am not sure if this needs layer data or not
      #*
      #* (1) provide layer data
      #*
      # # convert to data frame
      # b.coh <- cbind(expand.grid(layer = 1:dim(b.coh)[1],
      #                            polygon = 1:dim(b.coh)[2],
      #                            time = 1:dim(b.coh)[3]),
      #                atoutput = as.vector(b.coh)) # --- head(b.coh)
      # #* Drop boundary boxes - HAP, I am having issues with NAs with sample_survey_biomass() due to NAs in boundary boxes
      # b.coh <- b.coh[which(!(b.coh$polygon %in% bboxes)),]
      # # fix time to Time
      # b.coh$time <- Time
      # # add column for age cohort
      # b.coh$agecl <- coh
      # # store data
      # cohort  <- rbind(cohort, b.coh);remove(b.coh) # head(cohort)
      #*
      #* (2) collapse by layer
      #*
      # collapse by depth, leaving time:polygon
      b.coh <- apply(b.coh, 2, colSums, na.rm = TRUE)
      # convert to data frame
      b.coh <- cbind(expand.grid(time = 1:dim(b.coh)[1],
                                 polygon = 1:dim(b.coh)[2]),
                     atoutput = as.vector(b.coh)) # HAP, the header needs to be atoutput as other atlantisom functions expect this
      #* HAP, drop boundary boxes - I am having issues with NAs with sample_survey_biomass() due to NAs in boundary boxes
      b.coh <- b.coh[which(!(b.coh$polygon %in% bboxes)),]
      # fix time to Time
      b.coh$time <- Time
      # add column for age cohort
      b.coh$agecl <- coh
      # store data
      cohort  <- rbind(cohort, b.coh);remove(b.coh) # head(cohort)
    }
    # generate function output
    cohort$species <- paste0(as.character(fgs_age$Name[index_fgs]))
    #
    func_out_bio <- rbind(func_out_bio, cohort)
  }
  return(as.data.frame(func_out_bio))
}
