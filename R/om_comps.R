#'Generate composition data from atlantisom
#'
#'#'@description A wrapper function to create survey and fishery compositional data for assessment input.
#'Takes the output of \code{om_species}. Wrapper can generate replicates. Saves output as .rds
#'Results for more than one survey are generated with multiple survey config files and
#'saved as separate .rds files.
#'@param usersurvey survey config file in format of /config/usersurvey.R
#'@param userfishery fishery config file in format of /config/fisherycensus.R
#'@param omlist_ss output of \code{om_species}
#'@param n_reps number of replicate age, length, and weight-at-age compositions to be generated
#'@template save
#'@return Returns list objects containing dataframes of survey catch, length, and weight at age class and age:
#' \itemize{
#'  \item{survObsAgeComp, list of replicate dataframes of observed survey age comp (n fish)}
#'  \item{survObsLenComp, list of replicate dataframes of observed survey length comp (n fish)}
#'  \item{survObsWtAtAge, list of replicate dataframes of observed survey weight at age (avg wt)}
#'  \item{fishObsAgeComp, list of replicate dataframes of observed fishery age comp (n fish)}
#'  \item{fishObsLenComp, list of replicate dataframes of observed fishery length comp (n fish)}
#'  \item{fishObsWtAtAge, list of replicate dataframes of observed fishery weight at age (avg wt)}
#'  \item{survObsFullAgeComp, list of replicate dataframes of observed survey annual age comp (n fish)}
#'  \item{fishObsFullAgeComp, list of replicate dataframes of observed fishery age comp (n fish)}
#'  \item{survObsFullWtAtAge, list of replicate dataframes of observed survey weight at annual age (avg wt)}
#'  \item{fishObsFullWtAtAge, list of replicate dataframes of observed fishery weight at annual age (avg wt)}
#' },
#'
#'@export
#'
#'@family wrapper functions
#'@author Sarah Gaichas
#'
#'@examples
#'\dontrun{
#' # assuming CC3om is output of om_init(here("config/CC3config.r"))
#' # and CC3om_sardine <- om_species(c("Pacific_sardine"), CC3om)
#'
#'CC3om_sard_comp <- om_comps(usersurvey = here("config/usersurvey.R"),
#'     userfishery = here("config/fisherycensus.R"),
#'     omlist_ss = CC3om_sardine,
#'     n_reps = 1,
#'     save = TRUE)
#'
#'}
om_comps <- function(usersurvey = usersurvey_file,
                     userfishery = userfishery_file,
                     omlist_ss,
                     n_reps = n_reps,
                     save = TRUE){

  #one script for dimension parameters to be used in multiple functions
  source("config/omdimensions.R", local = TRUE)

  # user options for survey--default is a census with mid-year sample
  # allows multiple surveys
  age_comp_datas <- list()
  survObsLenComps <- list()
  survObsWtAtAges <- list()

  for (s in usersurvey)
  {
    source(s, local = TRUE)

    #numbers based fishery independent survey for age and length comps
    # same user specifications as indices
    survey_N <- atlantisom::create_survey(dat = omlist_ss$truenums_ss,
                                          time = survtime,
                                          species = survspp,
                                          boxes = survboxes,
                                          effic = surveffic,
                                          selex = survselex.agecl)

    #Sample fish for age composition
    # if we want replicates for obs error this sample function will generate them
    age_comp_data <- list()
    for(i in 1:n_reps){
      age_comp_data[[i]] <- atlantisom::sample_fish(survey_N, surveffN)
    }

    # save age comps
    if(save){
      saveRDS(age_comp_data, file.path(d.name, paste0(scenario.name, "_",
                                                      survey.name, "survObsAgeComp.rds")))
    }

    #weights needed for weight at age and length comp calcs
    # aggregate true resn per survey design
    survey_aggresn <- atlantisom::aggregateDensityData(dat = omlist_ss$trueresn_ss,
                                                       time = survtime,
                                                       species = survspp,
                                                       boxes = survboxes)

    # aggregate true structn per survey design
    survey_aggstructn <- atlantisom::aggregateDensityData(dat = omlist_ss$truestructn_ss,
                                                          time = survtime,
                                                          species = survspp,
                                                          boxes = survboxes)

    #dont sample these, just aggregate them using median
    structnss <- atlantisom::sample_fish(survey_aggstructn, surveffN, sample = FALSE)

    resnss <- atlantisom::sample_fish(survey_aggresn, surveffN, sample = FALSE)

    #this is all input into the length function, replicates follow age comp reps
    #  separating the length comps from the weight at age here
    survey_lenwt <- list()
    survObsLenComp <- list()
    survObsWtAtAge <- list()

    for(i in 1:n_reps){
      survey_lenwt[[i]] <- atlantisom::calc_age2length(structn = structnss,
                                                       resn = resnss,
                                                       nums = age_comp_data[[i]],
                                                       biolprm = omlist_ss$biol,
                                                       fgs = omlist_ss$funct.group_ss,
                                                       maxbin = maxbin,
                                                       CVlenage = lenage_cv,
                                                       remove.zeroes=TRUE)

      survObsLenComp[[i]] <- survey_lenwt[[i]]$natlength
      survObsWtAtAge[[i]] <- survey_lenwt[[i]]$muweight
    }

    if(save){
      saveRDS(survObsLenComp, file.path(d.name, paste0(scenario.name, "_",
                                                       survey.name, "survObsLenComp.rds")))
      saveRDS(survObsWtAtAge, file.path(d.name, paste0(scenario.name, "_",
                                                       survey.name, "survObsWtAtAge.rds")))
    }
    # add each survey to master list objects for survey data
    age_comp_datas[[survey.name]] <- age_comp_data
    survObsLenComps[[survey.name]] <- survObsLenComp
    survObsWtAtAges[[survey.name]] <- survObsWtAtAge

  }

  #now do fishery comps
  # user options for fishery--default is a census with mid-year sample
  # 2023 update, can now get fleet specific catch by polygon
  # user options for fishery--default is a census in all areas for all fleets
  # allows multiple fisheries
  catch_age_comps <- list()
  fishObsLenComps <- list()
  fishObsWtAtAges <- list()

  for(f in userfishery){

    source(f, local = TRUE)

    # 2023 update: we can now subset fishery catch from CATCH.nc using fleet output
    # create_fishery_subset as currently written aggregates across fleets and polygons
    # changed so that fleets to be aggregated together are specified in the userfishery file

    # however truecatchnum does not have fleets so needs to be null here, no fleet info
    # SO, no length comps by fleet! ugh. can get true age comps by fleet though

    # With current setup, if user specifies multiple fishery files, they will get
    # catch index by fleet
    # true age by fleet
    # but the length comps will have all fleet lengths for the specified fleet times and boxes

    #fishery catch at age each observed timestep summed over observed polygons
    # catch at age by area and timestep
    catch_numbers <-  atlantisom::create_fishery_subset(dat = omlist_ss$truecatchnum_ss,
                                                        time = fishtime,
                                                        fleets = NULL,
                                                        species = survspp,
                                                        boxes = fishboxes)

    # if we want replicates for obs error this sample function will generate them
    catch_age_comp <- list()
    for(i in 1:n_reps){
      catch_age_comp[[i]] <- atlantisom::sample_fish(catch_numbers, fisheffN)
    }

    # save fishery age comps
    if(save){
      saveRDS(catch_age_comp, file.path(d.name, paste0(scenario.name, "_",
                                                       fishery.name, "fishObsAgeComp.rds")))
    }

    #Get catch weights for length comp calc
    # aggregate true resn per fishery subset design
    catch_aggresnss <- atlantisom::aggregateDensityData(dat = omlist_ss$trueresn_ss,
                                                        time = fishtime,
                                                        species = survspp,
                                                        boxes = fishboxes)

    # aggregate true structn fishery subsetdesign
    catch_aggstructnss <- atlantisom::aggregateDensityData(dat = omlist_ss$truestructn_ss,
                                                           time = fishtime,
                                                           species = survspp,
                                                           boxes = fishboxes)

    #dont sample these, just aggregate them using median
    catch_structnss <- atlantisom::sample_fish(catch_aggstructnss, fisheffN, sample = FALSE)

    catch_resnss <- atlantisom::sample_fish(catch_aggresnss, fisheffN, sample = FALSE)

    # these fishery lengths and weight at age are each output timestep
    #same structure as above for surveys, replicates follow age comp reps
    #  separating the length comps from the weight at age here
    fishery_lenwt <- list()
    fishObsLenComp <- list()
    fishObsWtAtAge <- list()

    for(i in 1:n_reps){
      fishery_lenwt[[i]] <- atlantisom::calc_age2length(structn = catch_structnss,
                                                        resn = catch_resnss,
                                                        nums = catch_age_comp[[i]],
                                                        biolprm = omlist_ss$biol,
                                                        fgs = omlist_ss$funct.group_ss,
                                                        maxbin = maxbin,
                                                        CVlenage = lenage_cv,
                                                        remove.zeroes=TRUE)

      fishObsLenComp[[i]] <- fishery_lenwt[[i]]$natlength
      fishObsWtAtAge[[i]] <- fishery_lenwt[[i]]$muweight
    }

    if(save){
      saveRDS(fishObsLenComp, file.path(d.name, paste0(scenario.name, "_",
                                                       fishery.name, "fishObsLenComp.rds")))
      saveRDS(fishObsWtAtAge, file.path(d.name, paste0(scenario.name, "_",
                                                       fishery.name, "fishObsWtAtAge.rds")))
    }

    # add each survey to master list objects for survey data
    catch_age_comps[[fishery.name]] <- catch_age_comp
    fishObsLenComps[[fishery.name]] <- fishObsLenComp
    fishObsWtAtAges[[fishery.name]] <- fishObsWtAtAge

  }

  if(!is.null(omlist_ss$truenumsage_ss)){
    #numbers based fishery independent survey for age and length comps
    #allows for mulitple surveys
    annage_comp_datas <- list()

    for (s in usersurvey)
    {
      source(s, local = TRUE)

      # same user specifications as indices
      survey_annageN <- atlantisom::create_survey(dat = omlist_ss$truenumsage_ss,
                                                  time = survtime,
                                                  species = survspp,
                                                  boxes = survboxes,
                                                  effic = surveffic,
                                                  selex = survselex)
      #Sample fish for age composition
      # if we want replicates for obs error this sample function will generate them
      annage_comp_data <- list()
      for(i in 1:n_reps){
        annage_comp_data[[i]] <- atlantisom::sample_fish(survey_annageN, surveffN)
      }

      # save survey annual age comps
      if(save){
        saveRDS(annage_comp_data, file.path(d.name, paste0(scenario.name, "_",
                                                           survey.name, "survObsFullAgeComp.rds")))
      }
      annage_comp_datas[[survey.name]] <- annage_comp_data
    }

  }else{annage_comp_datas <- NULL}

  if(!is.null(omlist_ss$truecatchage_ss)){
    #fishery catch at age each observed timestep summed over observed polygons
    # there is a fleet variable in this dataset so can get catch at age by fleet
    # catch at age by area and timestep
    #allows for mulitple fisheries
    catch_annage_comps <- list()

    for (f in userfishery)
    {
      source(f, local = TRUE)

      catch_annagenumbers <-  atlantisom::create_fishery_subset(dat = omlist_ss$truecatchage_ss,
                                                                time = fishtime,
                                                                fleets = fishfleets,
                                                                species = survspp,
                                                                boxes = fishboxes)

      # if we want replicates for obs error this sample function will generate them
      # WARNING THIS AGGREGATES ACROSS FLEETS
      # TODO: need to change sample_fish to fix
      # SKG June 2023, with fleets defined in the input, the output aggregates only
      # selected fleets so I think this is ok to get annual age comp by user defined fleet
      catch_annage_comp <- list()
      for(i in 1:n_reps){
        catch_annage_comp[[i]] <- atlantisom::sample_fish(catch_annagenumbers, fisheffN)
      }

      # save fishery annual age comps
      if(save){
        saveRDS(catch_annage_comp, file.path(d.name, paste0(scenario.name,"_",
                                                            fishery.name, "fishObsFullAgeComp.rds")))
      }
      catch_annage_comps[[fishery.name]] <- catch_annage_comp
    }

  }else{catch_annage_comps <- NULL}

  # call interpolate weight at age function to get survObsFullWtAtAge
  if(!is.null(omlist_ss$truenumsage_ss)){
    interp_survWtAtAges <- list()
    for (s in usersurvey)
    {
      source(s, local = TRUE)

      interp_survWtAtAge <- list()
      for(i in 1:n_reps){
        interp_survWtAtAge[[i]] <- calc_avgwtstage2age(wtagecl = survObsWtAtAges[[survey.name]][[i]],
                                                       annages = omlist_ss$truenumsage_ss,
                                                       fgs = omlist_ss$funct.group_ss)
      }
      if(save){
        saveRDS(interp_survWtAtAge, file.path(d.name, paste0(scenario.name, "_",
                                                             survey.name, "survObsFullWtAtAge.rds")))
      }
      interp_survWtAtAges[[survey.name]] <- interp_survWtAtAge
    }
  }else{interp_survWtAtAges <- NULL}

  # do we want fishery average weight at true age too? why not
  # call interpolate weight at age function to get fishObsFullWtAtAge
  # WARNING currently aggregates out fleet info, but no fleets in aggregate wtage
  # June 2023
  # This will produce separate output files with fleets defined in separate fishery
  # config files areas and times but with problem notes above for lengths
  if(!is.null(omlist_ss$truecatchage_ss)){
    interp_fishWtAtAges <- list()
    for (f in userfishery)
    {
      source(f, local = TRUE)

      interp_fishWtAtAge <- list()
      for(i in 1:n_reps){
        interp_fishWtAtAge[[i]] <- calc_avgwtstage2age(wtagecl = fishObsWtAtAge[[i]],
                                                       annages = omlist_ss$truecatchage_ss,
                                                       fgs = omlist_ss$funct.group_ss)
      }
      if(save){
        saveRDS(interp_fishWtAtAge, file.path(d.name, paste0(scenario.name,"_",
                                                             fishery.name, "fishObsFullWtAtAge.rds")))
      }
      interp_fishWtAtAges[[fishery.name]] <- interp_fishWtAtAge
    }
  }else{interp_fishWtAtAge <- NULL}



  comps <- list("survObsAgeComp" = age_comp_datas,
                "survObsLenComp" = survObsLenComps,
                "survObsWtAtAge" = survObsWtAtAges,
                "fishObsAgeComp" = catch_age_comps,
                "fishObsLenComp" = fishObsLenComps,
                "fishObsWtAtAge" = fishObsWtAtAges,
                "survObsFullAgeComp" = annage_comp_datas,
                "fishObsFullAgeComp" = catch_annage_comps,
                "survObsFullWtAtAge" = interp_survWtAtAges,
                "fishObsFullWtAtAge" = interp_fishWtAtAges
                )

  return(comps)
}
