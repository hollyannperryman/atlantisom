#' Load Atlantis catch tons by fleet and polygon outputfiles (netcdf)
#'
#' This function loads Atlantis CATCH outputfile (netcdf) and
#' converts them to a \code{data.frame}.
#' @template dir
#' @template file_nc
#' @template bps
#' @template fgs
#' @template biolprm
#' @template select_groups
#' @param select_variable A character value specifying which variable to return.
#'   loaded. Only one variable of the options available (i.e., \code{c(
#'   "Catch", "Discard")
#'   }) can be loaded at a time.
#' @param check_acronyms A logical, specifying if selected groups are active
#'   in the model run. This is used in automated runs.
#'   All groups are passed when plotting is called via batch-file,
#'   (which cannot be changed easily) this will result in errors if some
#'   groups are not active in the model run.
#'   By default this argument is \code{TRUE}.
#' @template bboxes
#' @template verbose
#' @family load functions
#' @return A \code{data.frame} in long format with the following coumn names:
#'   Species, timestep, polygon, agecl, and atoutput (i.e., variable).
#'
#' @keywords gen
#' @author Alexander Keth, modified by Sarah Gaichas
#'
#' HAP - 2024 - The following script was edited by Holly Ann Perryman to get updated CalCurr and SAR SS files to run
#'
#' @importFrom magrittr %>%
#' @export

load_nc_catchtons <- function(dir = getwd(),
                              file_nc,
                              file_fish,
                              bps,
                              fgs,
                              biolprm,
                              select_groups,
                              select_variable,
                              # = c("Catch", "Discard"), # HAP I removed this as I kept getting an error...
                              check_acronyms = TRUE,
                              bboxes = c(0),
                              verbose = FALSE) {
  # NOTE: The extraction procedure may look a bit complex...
  # A different approach would be to
  # create a dataframe for each variable (e.g. GroupAge_Nums)
  # and combine all dataframes at the end.
  # This alternative approach requires a lot more storage
  # and the code wouldn't be vectorized.

  # Check input of the nc file
  if (tail(strsplit(file_nc, "\\.")[[1]], 1) != "nc") {
    stop("The argument for file_nc,", file_nc, "does not end in nc")
  }
  if (is.null(dir)) {
    file.nc <- file_nc
  } else {
    file.nc <- file.path(dir, file_nc)
  }

  # Check input of select_variable as only one value is allowed
  select_variable <- match.arg(select_variable, choices = c("Catch", "Discard"), several.ok = FALSE) # HAP - this fixed an error I was encountering

  # select_variable gets renamed for fishery data, save input
  input_select_variable <- select_variable

  # select_groups get renamed to code, save input
  select_groups_name <- select_groups

  # Check input structure!
  if (check_acronyms) {
    active_groups <- as.vector(subset(fgs, IsTurnedOn == 1)$Name)
    inactive_groups <- select_groups[which(
      !is.element(select_groups, active_groups))]
    if (length(inactive_groups) >= 1) {
      select_groups <- select_groups[!is.element(select_groups, inactive_groups)]
      warning(paste(paste("Some selected groups are not active in the model run.",
        "Check 'IsTurnedOn' in fgs\n"),
        paste(inactive_groups, collapse = "\n")))
    }
    if (all(!is.element(select_groups, active_groups))) {
      stop(paste("None of the species selected are active in the model run.",
        "Check spelling and Check 'IsTurnedOn' in fgs"))
    }
  }

  # Deal with file structures

  # Load ATLANTIS output!
  at_out <- RNetCDF::open.nc(con = file.nc)
  on.exit(RNetCDF::close.nc(at_out))

  if (select_variable != "N" & all(is.element(select_groups, bps))) {
    stop("The only output for Biomasspools is N.")
  } else{
    print(paste("Read", file.nc, "successfully"))
  }

  # Get info from netcdf file! (Filestructure and all variable names)
  var_names_ncdf <- sapply(seq_len(RNetCDF::file.inq.nc(at_out)$nvars - 1),
    function(x) RNetCDF::var.inq.nc(at_out, x)$name)
  n_timesteps <- RNetCDF::dim.inq.nc(at_out, 0)$length
  n_boxes     <- RNetCDF::dim.inq.nc(at_out, 1)$length
  n_layers    <- RNetCDF::dim.inq.nc(at_out, 2)$length
  # layer data
  num_layers <- RNetCDF::var.get.nc(ncfile = at_out, variable = "numlayers")[, 1]
  num_layers <- num_layers + ifelse(num_layers == 0, 0, 1)

  # Extract data from the ncdf file
  # Create a vector of all potential variable names
  # Only use names which are available in the ncdf-file as an
  # extraction of missing variables is not possible
  # Create vector of available species at the end using search_clean
  # This is needed to create species-names later on

  # To get what I hope is catch output in tons,
  # keep variable names with CODES instead of species names

  # get code matching species name and use as select groups
  select_groups <- fgs$Code[which(fgs$Name %in% select_groups_name)]

  # and _FC with a number corresponding to the fishery in the Fisheries.csv

  if(select_variable %in% c("Catch", "Discard")){
    #input_select_variable <- select_variable
    #read in fleet names
    fleetnames <- atlantisom::load_fisheries(dir = dir, file_fish = file_fish)
    #select_variable <- concatenate select_variable "_" each fleet name
    svfish <- c()
    for(i in select_variable){
      sv <- paste0(i, "_FC", fleetnames$Index)
      svfish <- c(svfish, sv)
    }
    select_variable <- svfish
  }

  # To make the creation of variables as robust as possible
  # we introduce different combinations of groups, variable, and cohort
  # Only combinations present in the ncdf are used later on
  # Loop over select_groups to use the same ordering

  search <- list()
  for (i in seq_along(select_groups)) {
    search[[i]] <- c(
      unlist(paste0(select_groups[i], select_variable)),  # GroupVariable
      unlist(paste(select_groups[i], select_variable,
                   sep = "_"))                           # Group_Variable
    )
    search[[i]] <- search[[i]][is.element(search[[i]], var_names_ncdf)]
    search[[i]] <- unique(search[[i]])
  }
  search_clean <- do.call(c, search)
  # If the combination of select_groups and select_variable ends up not being found.
  if (length(search_clean) == 0) return(0)
  # get nc data for search_clean
  at_data <- lapply(search_clean, RNetCDF::var.get.nc, ncfile = at_out)

  #* HAP - to split at_data into agecl data, we need to also get data for FID#_Catch variables
  search2 <- list()
  if (input_select_variable %in% "Discard") {input_select_variable<-"Discards"} # Note, the nc var is Discards - not Discard
  for (i in seq_along(select_groups)) {
    search2[[i]] <- c(unlist(paste0(fgs[which(fgs$Code %in% select_groups[i]),"Name"], 1:fgs[which(fgs$Code %in% select_groups),"NumCohorts"][i], "_", input_select_variable)))
    search2[[i]] <- search2[[i]][is.element(search2[[i]], var_names_ncdf)]
    search2[[i]] <- unique(search2[[i]])
  }
  if (input_select_variable %in% "Discards") {input_select_variable<-"Discard"} # swap parameter from Discards to Discard as to presurve the functionality of the rest of the script
  search_clean2 <- do.call(c, search2)
  # get nc data for search_clean
  at_data_agecl <- lapply(search_clean2, RNetCDF::var.get.nc, ncfile = at_out)# NOTE, this data is in NUMBERS

  # Get final species and number of ageclasses per species
  # final_species <- select_groups[sapply(lapply(select_groups, grepl, x = search_clean), any)] #
  #* HAP
  #* in the above, I think there is a bug when setting final_species
  #* when testing current files from the CalCurr model, this is script is
  #* selecting FIDs that actually do not have "_Catch_" data being reported out
  #* from the nc file. I think some of the Codes are flagging multiple groups,
  #* for example, FPL flags both FPL and PL as being caught but PL is not
  #* actually caught.
  #*
  #* To fix this and get the function to run, I used the following script
  #* instead of the above
  # Extract the prefixes from search_clean by splitting at the first underscore
  final_species <- unique(sapply(strsplit(search_clean, "_"), `[[`, 1))

  # # Get final fleets for full age structure fishery output--wrong order
  # if(input_select_variable %in% c("Catch", "Discard")){
  #   final_fleet <- fleetnames$Code[sapply(
  #     lapply(select_variable, grepl, x = search_clean), any)]
  # }

  # Create an array of layerids.
  # Every entry in the array indicates if a layer is present (= 1) or not (= 0).
  # Boxes without layers (= islands) have only 0s as id,
  # used later on to remove data from non-existent layers!
  # By default output should be 0 in those layers.
  # Layers in boundary boxes are set to 0 if bboxes is anything other than NULL!
  # Applying a boolean array to an array results in a vector!
  for (i in seq_along(num_layers)) {
    if (i == 1) layerid <- array(dim = c(n_layers, n_boxes))
    if (num_layers[i] == 0) {
      layerid[, i] <- 0
    } else {
      if (!is.null(bboxes) & is.element((i - 1), bboxes)) {
        layerid[, i] <- 0
      } else {
        layerid[, i] <- c(rep(0, times = n_layers - num_layers[i]),
                          rep(1, times = num_layers[i]))
      }
    }
  }

  # Create vectors for polygons and layers
  # Each vector has the length equal to one time-step
  # All data from islands and non-existent layers is removed
  # Therefore the length of these
  # vectors is equal for each extracted variable
  boxes <- 0:(n_boxes - 1)
  # Remove islands and boundary boxes
  island_ids <- num_layers == 0
  if (!is.null(bboxes)) {
    boundary_ids <- is.element(boxes, bboxes)
    island_ids <- island_ids | boundary_ids
  }
  boxes <- boxes[!island_ids]
  num_layers <- num_layers[!island_ids]

  polygons <- rep(boxes, times = num_layers)
  layers <- sapply(num_layers[num_layers != 0] - 2,
    function(x) c(seq(x, from = 0, by = 1), n_layers - 1))
  if (any(sapply(layers, length) != num_layers[num_layers != 0])) {
    stop("Number of layers incorrect.")
  }
  layers <- do.call(c, layers)
  if (length(polygons) != length(layers)) {
    stop("Number of polygons and layers do not match.")
  }

  # In the following section the data is transformed to a long dataframe
  # I haven't found any solution to vectorize the creation of the dataframe
  # columns (species, age, polygons,...)
  # when data from 2d and 3d arrays
  # (e.g. select_variable = "N" all biomasspools are only present in the
  # sediment layer.) are read in simultaneously.
  # Therefore the current "messy" solution splits the data
  # in 2 subpopulations: 2d-data and 3d-data
  at_data3d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 3)]
  at_data2d <- at_data[which(sapply(at_data, function(x) length(dim(x))) == 2)]
  # HAP - not sure if this is necessary but doing the same for the at_data_agecl data
  at_data2d_agecl <- at_data_agecl[which(sapply(at_data_agecl, function(x) length(dim(x))) == 2)]

  int_fs <- final_species
  #int_fa <- rep(1, length(final_species))
  #* HAP -
  #* In my work, I realized I am not getting agecl data from this in that all the values in agecl
  #* in the resulting datasets were 1. First, I tried swapping the 1 in the above for int_fa with 10 and
  #* it appeared to work (ie the function runs) but catchtons data no longer matched catch.txt data
  #* After a bit more digging this is when I realized that the data in the nc file being used here does
  #* not have agecl data. The dime of FID_Catch_FC# is boxes:time.
  #* However, nc data FID#_Catch does of agecl data, and is also as dim boxes_time.
  #* This is when I decided to adjust this script to collect this data to compute proportions for
  #* splitting FID_Catch_FC# into agecl.
  #* To make sure we are getting the correct max age, I used the following instead for int_fa
  int_fa <- fgs[which(fgs$Code %in% final_species),"NumCohorts"]

  if(input_select_variable %in% c("Catch", "Discard")){
    # how many unique fleets per select_group in search_clean?
    # lookup of species and fleet names
    splitfleets <- search_clean %>%
      stringr::str_replace(paste0("_",input_select_variable,"_FC"), "-")
    sp_fleet <- as.data.frame(unique(splitfleets)) %>%
      dplyr::rename(spfleet = "unique(splitfleets)") %>%
      tidyr::separate(spfleet, c("species", "fleet"), sep = "-")
    # lookup of number of fleets per species
    sp_nfleet <- as.data.frame(table(sp_fleet$species)) %>%
      dplyr::rename(species = Var1, nfleet = Freq)
    final_fleet <- sp_nfleet[match(final_species, sp_nfleet$species),]
    # for indexing fleets
    int_ff <- final_fleet$nfleet
  }

  # 3d should not apply here
  # if (length(at_data3d) >= 1) {
  #   # Remove biomasspools if selected variable is "N"!
  #   if (input_select_variable == "N") {
  #     int_fs <- final_species[!is.element(final_species, bps)]
  #     int_fa <- final_agecl[!is.element(final_species, bps)]
  #     # Note this only works if age-structured vertebrates have 10 ageclasses
  #     # there is no "N" output in annage files so has no impact
  #     int_fa[int_fa == 10] <- 1
  #   }
  #   for (i in seq_along(at_data3d)) {# for loop over all variables
  #     if (i == 1) result3d <- list()
  #     for (j in 1:n_timesteps) {# loop over timesteps
  #       if (j == 1) values <- array(dim = c(length(layers), n_timesteps))
  #       values[, j] <- at_data3d[[i]][,, j][layerid == 1]
  #     }
  #     result3d[[i]] <- as.vector(values)
  #   }
  #   result3d <- data.frame(
  #     species = unlist(sapply(
  #       X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = FALSE,
  #       USE.NAMES = FALSE),
  #       FUN = rep, each = length(layers) * n_timesteps, simplify = FALSE)),
  #     agecl = unlist(sapply(
  #       X = sapply(X = int_fa, FUN = seq, from = 1, by = 1, simplify = FALSE,
  #       USE.NAMES = FALSE),
  #       FUN = rep, each = length(layers) * n_timesteps, simplify = FALSE)),
  #     polygon = unlist(sapply(
  #       X = n_timesteps * int_fa, FUN = rep, x = polygons, simplify = F,
  #       USE.NAMES = FALSE)),
  #     layer = unlist(sapply(
  #       X = n_timesteps * int_fa, FUN = rep, x = layers, simplify = FALSE,
  #       USE.NAMES = FALSE)),
  #     time = unlist(sapply(
  #       X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1), each = length(layers)),
  #       simplify = FALSE, USE.NAMES = FALSE)),
  #     atoutput = do.call(c, result3d),
  #     stringsAsFactors = FALSE)
  # }

  if (length(at_data2d) >= 1) {
    # # Only select biomasspools if selected variable is "N"!
    # if (input_select_variable == "N") {
    #   int_fs <- final_species[is.element(final_species, bps)]
    #   int_fa <- final_agecl[is.element(final_species, bps)]
    # }
    # # age-structured invert groups are combined in ncdf file!
    # if (input_select_variable == "Grazing") int_fa <- 1

    #* HAP - this for loop was modified in order to use at_data2d_agecl data to split at_data2d by agecl
    index <- rep(1:length(int_fs), each = int_ff[1])
    for (i in seq_along(at_data2d)) {# for loop over all variables! i=a=j=1
      if (i == 1) result2d <- list() # define an empty list to insert vectorized data
      # for variable i, use at_data2d_agecl data to split at_data2d by agecl
      if (i <= length(fleetnames$Index)) {proportions_agecl_s <- lapply(at_data2d_agecl[c(1:int_fa[index[i]])], function(x) x / Reduce("+", at_data2d_agecl[c(1:int_fa[index[i]])]))
      } else {proportions_agecl_s <- lapply(at_data2d_agecl[c(int_fa[index[i]-1]+(c(1:int_fa[index[i]])))], function(x) x / Reduce("+", at_data2d_agecl[c(int_fa[index[i]-1]+(c(1:int_fa[index[i]])))]))
      }
      scaled_proportions_i <- lapply(proportions_agecl_s, function(x) {
        x[is.nan(x)] <- 0  # Replace NaN with 0
        x * at_data2d[[i]]
      })
      for (a in seq_along(scaled_proportions_i)) {
        if (a == 1) values2 <- array(dim = c(length(scaled_proportions_i), length(boxes), n_timesteps)) # define an empty array
        for (j in 1:n_timesteps) {# loop over timesteps
          values2[a, , j] <- scaled_proportions_i[[a]][, j][boxes + 1] # select data for element i and time j, dropping boxes that are not in model domain, and insert into defined array
        }
      }
      result2d[[i]] <- as.vector(values2)
    }

    # Order of the data in value column = "atoutput".
    # 1. species  --> rep each with the number of
    #                 ageclasses * fleets and n_timesteps * boxes
    # 2. age      --> rep each (1:maxage for each species) with n_timesteps * boxes
    # 3. timestep --> rep each timestep (1:n_timesteps)
    #                 with the number of boxes and final_agecl
    #                 (num ages per species)
    # 4. polygon  --> rep boxes times n_timesteps * final_agecl
    #                 (num ages per species)
    # 5. fleet    -->

    # if(input_select_variable %in% c("Nums", "Weight")){
    #
    # result2d <- data.frame(species = unlist(sapply(
    #   X = mapply(FUN = rep, x = int_fs, each = int_fa, SIMPLIFY = FALSE,
    #              USE.NAMES = FALSE),
    #   FUN = rep, each = length(boxes) * n_timesteps, simplify = FALSE)),
    #   agecl = unlist(sapply(X = sapply(X = int_fa, FUN = seq, from = 1,
    #                                    by = 1, simplify = FALSE, USE.NAMES = FALSE),
    #                         FUN = rep, each = length(boxes) * n_timesteps, simplify = FALSE)),
    #   polygon = unlist(sapply(X = n_timesteps * int_fa,
    #                           FUN = rep, x = boxes, simplify = FALSE, USE.NAMES = FALSE)),
    #   time = unlist(sapply(X = int_fa, FUN = rep, x = rep(0:(n_timesteps - 1),
    #                                                       each = length(boxes)), simplify = FALSE, USE.NAMES = FALSE)),
    #   atoutput = do.call(c, result2d),
    #   stringsAsFactors = F)
    #   if (select_variable == "N") result2d$layer <- n_layers - 1
    # }

    #should now work properly for multiple fleets
    if(input_select_variable %in% c("Catch", "Discard")){
      # result2d <- data.frame(species = unlist(sapply(X = mapply(FUN = rep, x = int_fs,
      #              each = (int_fa * int_ff),SIMPLIFY = FALSE,USE.NAMES = FALSE),
      #   FUN = rep, each = length(boxes) * n_timesteps, simplify = FALSE)),
      #   agecl = unlist(sapply(X = sapply(X = rep(int_fa, int_ff), FUN = seq, from = 1,
      #                                    by = 1, simplify = FALSE, USE.NAMES = FALSE),
      #                         FUN = rep, each = (length(boxes) * n_timesteps), simplify = FALSE)),
      #   polygon = unlist(sapply(X = n_timesteps * int_fa * int_ff,
      #                           FUN = rep, x = boxes, simplify = FALSE, USE.NAMES = FALSE)),
      #   fleet = unlist(sapply(X = mapply(FUN = rep, x = sp_fleet$fleet,
      #                                    each = (rep(int_fa, int_ff)),SIMPLIFY = FALSE,USE.NAMES = FALSE),
      #     FUN = rep, each = (length(boxes) * n_timesteps), simplify = FALSE)),
      #   time = unlist(sapply(X = int_fa * int_ff , FUN = rep, x = rep(0:(n_timesteps - 1),
      #                                                       each = length(boxes)), simplify = FALSE, USE.NAMES = FALSE)),
      #   atoutput = do.call(c, result2d),
      #   stringsAsFactors = F)
      #* HAP:
      #* above is the old way of preparing function output. This had to be adjusted to account for the agecl data
      #* With the agecl data being integrated, the vectorization of the data changed, making the above store the
      #* result2d incorrectly. Below now properly stores the vectorized data, well it does with my case study.
      #* of course folks should test and check to make sure this works for them too...
      result2d <- data.frame(species = unlist(Map(function(x, reps) rep(x, reps), int_fs, int_ff * n_timesteps * length(boxes) * int_fa), use.names = FALSE),
                             agecl   = unlist(sapply(X = rep(int_fa),
                                                     FUN = function(x) rep(seq(from = 1, by = 1, length.out = 10), times = (length(boxes) * n_timesteps * int_ff[1])),
                                                     simplify = FALSE, USE.NAMES = FALSE)),
                             polygon = unlist(lapply(lapply(int_fa, function(n) rep(boxes, each = n)),
                                                     function(x) rep(x, times = n_timesteps * int_ff[1]))),
                             fleet   = unlist(Map(function(seq_item, reps) rep(seq_item, each = reps),
                                                  lapply(int_ff, function(x) seq(1, x)),  int_fa * length(boxes) * n_timesteps)),
                             time    =  unlist(lapply(lapply(int_fa * length(boxes), function(n) rep(0:(n_timesteps - 1), each = n)),
                                                      function(x) rep(x, times = int_ff[1]))),
                             atoutput = do.call(c, result2d),
                             stringsAsFactors = F)

      #if (select_variable == "N") result2d$layer <- n_layers - 1
    }

  }

  # Combine dataframes if necessary!
  # if (all(sapply(lapply(at_data, dim), length) == 3) & input_select_variable != "N") {
  #   result <- result3d
  # }
  # HAP - I changed this in an attempt to reduce the use of background storage
  if (all(sapply(lapply(at_data, dim), length) == 2) & input_select_variable != "N") {
    return(result2d)
  } else if (input_select_variable == "N") {
    if (length(at_data2d) >= 1 & length(at_data3d) == 0) result <- result2d
    if (length(at_data2d) == 0 & length(at_data3d) >= 1) result <- result3d
    if (length(at_data2d) >= 1 & length(at_data3d) >= 1) {
      result <- rbind(result2d, result3d)
      return(result)
    }
  }

  # # Remove min_pools if existent (well, there always are min pools... ;)).
  # min_pools <- is.element(result$atoutput, c(0, 1e-08, 1e-16))
  # if (length(min_pools) > 0) {
  #   # exclude 1st timestep and sediment layer from calculation
  #   print_min_pools <- sum(min_pools) -
  #     length(result[min_pools & result$time == 1, 1]) -
  #     length(result[min_pools & result$time > 1 & result$layer == 7, 1])
  #   if (print_min_pools > 0 & verbose) {
  #     warning(paste0(round(print_min_pools/dim(result)[1] * 100),
  #       "% of ", select_variable, " are true min-pools (0, 1e-08, 1e-16)"))
  #   }
  #   result <- result[!min_pools, ]
  # }

  # Remove non-existent layers.
  # WARNING: Biomass is build up (very few) in sediment layer for
  # NON sediment groups (e.g. baleen whales)
  # Therefore, I subset all data from that layer for non biomass groups and
  # groups which cannot penetrate into the sediment!
  # UPDATE: Doesn't work with layers as species are not distributed through
  # the whole water column and do not appear in
  # every polygon.

  # Sum up N for invert cohorts if invert cohorts are present!
  # NOTE: invert cohorts of size 10 are not considered!
  # if (input_select_variable == "N" & any(final_agecl != 10 & final_agecl > 1)) {
  #   result <- result %>%
  #     dplyr::group_by(polygon, layer, species, time) %>%
  #     dplyr::summarise(atoutput = sum(atoutput))
  # }

  # return(result)
}
