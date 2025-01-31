
#* Script: Pass Atlantis Output Data to SS3 input Files
#*
#* author: Holly Perryman (2024)
#* This script is based on atlantisom examples:
#*    (i.a) overview:
#*     https://sgaichas.github.io/poseidon-dev/atlantisom_landingpage.html
#*    (i.b) Get true biomass, abundance, age composition, length composition, weight at age, fishery catch, fishery catch at age, fishery length composition, and fishery weight age age for a “sardine-like species”
#*     https://sgaichas.github.io/poseidon-dev/FullSardineTruthEx.html
#*    (i.c) Format these outputs and get other life history parameters for input into a stock assessment model (Stock Synthesis, using r4ss)
#*     https://sgaichas.github.io/poseidon-dev/CreateStockSynthesis.html
#*    (i.d) Get true and observed input data, format inputs, and run the assessment model
#*     https://sgaichas.github.io/poseidon-dev/SardinesHakeatlantisom2SStest.html
#*     https://sgaichas.github.io/poseidon-dev/Sardines_atlantisom2SS_demo.html
#*    (i.e) in progress: compare assessment results with truth
#*     https://sgaichas.github.io/poseidon-dev/SkillAssessInit.html
#*    (i.f) Simplified dataset extraction with wrapper functions
#*     https://sgaichas.github.io/poseidon-dev/NOBAcod.html
#*
#* The following script it organized into several sections:
#*      0. Preparation - initialization for the script
#*      I. Derive "Data" from Atlantis to give to SS3
#*         a) derive ecological data (fishery independent survey)
#*         b) derive catch data (fishery dependent survey)
#*      II. Write data for Stock Synthesis
#*         a) write data to the dat file (HAP: this is working)
#*         b) write data to the ctl file (HAP: this is under development)
#*      III. Run Stock Synthesis
#*

#* ----------------------------
#* ---------------------------- 0. Preparation
#* ----------------------------
#*
#* a. define libraries for this script
#* b. directories and names for Atlantis files (SS3 files prep is in Sec. II)
#* c. define functional group(s) of focus

# --- (a) libraries for this script

library(tidyr)
require(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(ggforce)
library(ggthemes)
#library(atlantisom) # HAP - loading the functions that I have edited

setwd("/Users/hollyperryman/Documents/atlantisom-master/R")
files.sources = list.files()
sapply(files.sources, source)
setwd("/Users/hollyperryman/Documents/atlantisom-master")

# --- (b.0) directories and file names for Atlantis files

# directories with Atlantis inputs and outputs:
dir.inputs  <- "/Users/hollyperryman/Documents/CalCurrent/AtlantisCalCurr/AtlantisCalCurrV4_0400"
dir.outputs <- paste0(dir.inputs,"/outputFolder")
#dir.outputs <- paste0(dir.inputs,"/outputFolder_73_73")
# Atlantis input file names
in.init.conditions  <- "CalCurrentV4_Biol_8Aug2023.nc"
in.groups           <- "CalCurrentV4Groups_May2023.csv"
in.fisheries        <- "CalCurrentV4Fisheries.csv"
in.biol.prm         <- "CalCurrentV4_Biol_Aug2023.prm"
# Atlantis output file names
#scenario.name        <- "CCV4"
in.bgm                <- "CalCurrentV3_utm_testvertmix.bgm"
in.run                <- "CalCurrentV4_run_Dec2022.xml"
out.biomindx          <- "BiomIndx.txt"
out.catch             <- "Catch.txt"
# out.catchperfishery <- "CatchPerFishery.txt"
# out.dietcheck       <- "DietCheck.txt"
out.yoy               <- "YOY.txt"
# out.nc              <- ".nc"
# out.catch.nc        <- "CATCH.nc"
# out.prod.nc         <- "PROD.nc"
# out.annagebio.nc    <- "ANNAGEBIO.nc"
# out.annagecatch.nc  <- "ANNAGECATCH.nc"
# NOTE: The script will also call the log.txt file indirectly

# --- (b.1) TRANSFER necessary Atlantis inputs to Atlantis output folder

#* the atlantisom functions expect needed input and output files are in the same directory
#* In the event the Atlantis user saves outputs to an output folder, I have script here
#* to put the needed input files into the current output folder being processed.
file.copy(list.files(path = dir.inputs, full.names = TRUE, recursive = F, pattern = in.init.conditions),dir.outputs)
file.copy(list.files(path = dir.inputs, full.names = TRUE, recursive = F, pattern = in.groups),         dir.outputs)
file.copy(list.files(path = dir.inputs, full.names = TRUE, recursive = F, pattern = in.fisheries),      dir.outputs)
file.copy(list.files(path = dir.inputs, full.names = TRUE, recursive = F, pattern = in.biol.prm),       dir.outputs)

# --- (b.2) create a version of in.groups with the correct headers and use that file

#* the atlantisom functions assume certain headers in the groups.csv input file.
#* This code fixes this for our case, but each user of the script may need to
#* adjust this accordingly.

temp.groups <- utils::read.csv(paste0(dir.outputs,"/",in.groups)); head(temp.groups)
names(temp.groups)[which(names(temp.groups)=="GroupType")]  <- 'Code'
write.csv(temp.groups,paste0(dir.outputs,"/","CalCurrGroupsForR.csv"))
in.groups <- "CalCurrGroupsForR.csv"
remove(temp.groups)

# --- (c) functional group(s) of focus

# NOTE, while you can gather Atlantis data for all functional groups, this can
# require a lot of time to process the script and a lot of R environmental space. If
# one does not need info. for all Atlantis functional groups, it is best to trim it
# down to just the one group of focus or a small selection of groups - whichever is
# more appropriate.

# species of SS3 model (atlantis functional group name) + scenario save name
species_ss <- c("Pacific_sardine"); save.name.scenario <- "atlantisom_CCV4"

# group(s) for data extraction from atlantis (atlantis functional group name(s)) + name for saving Rdata
#species_truth <- c("Pacific_sardine","Mesopel_M_Fish"); save.name.truth <- "SardHake"
species_truth <- c("Pacific_sardine"); save.name.truth <- "SAR"

#* ----------------------------
#* ---------------------------- I. Derive "Data" from Atlantis to give to SS3
#* ----------------------------    a) derive ecological data (fishery independent survey)
#* ----------------------------
#*
#* This will take a few sections of script:
#*         (A) loading Atlantis ecological output (get truth)
#*         (B) create ecological observations from Atlantis outputs (fishery independent survey data)
#*         (C) sample a biomass index of abundance from an Atlantis scenario
#*            -- note, in this section we will pause to make a graph comparing biomass time series from Atlantis output (truth) to that from the developed survey

#* -------------- (A) loading Atlantis ecological output (get truth)
#*
#* this calls for the use of the atlantisom function run_truth()
#* This function needs the following:
#*
#* scenario      - based on the changes I made to the function, this is now the name used when saving the RData
#* dir           - directory with atlantis outputs
#* file_fgs      - name of groups.csv file (or the groups.csv with headers reformatted for atlantisom)
#* file_bgm      - name of bgm file
#* select_groups - vector of atlantis function groups (names) to collect data on
#* file_init     - name of initial conditions nc file
#* file_biolprm  - name of biology.prm file
#* file_runprm   - name of run file (the xml file which is saved in the outputs folder)
#* file_fish     - name of the fisheries.csv file
#*
#* see ?run_truth to get more info.

# (all inputs are defined in the preparation section, see above)

#* HAP - I had to make edits to the function to get this to run with updated frameworks --- source("R/run_truth.R")

#* Check to see if you already have truth .RData for the current save.name.scenario and save.name.truth
#* if not, execute run_truth() - if so, load that .RData
if(!file.exists(file.path(dir.outputs, paste0(save.name.scenario,"_",save.name.truth,"_run_truth.RData")))){
  #Store all loaded results into an R object
  print("executing run_truth() to get truth data")
  truth <- run_truth(scenario      = paste0(save.name.scenario,"_",save.name.truth),
                     dir           = dir.outputs,
                     file_fgs      = in.groups,
                     file_bgm      = in.bgm,
                     select_groups = species_truth,
                     file_init     = in.init.conditions,
                     file_biolprm  = in.biol.prm,
                     file_runprm   = in.run,
                     file_fish     = in.fisheries)
  # HAP - after running this I get the following warnings:
  #   1: In `[<-.data.frame`(`*tmp*`, , 3:(maxmat + 2), value = list(`1` = c("0.1",  :
  #   provided 13 variables to replace 10 variables
  #   2: In load_biolprm(dir = dir, file_biolprm = file_biolprm) :
  #   NAs introduced by coercion
  #   3: `gather_()` was deprecated in tidyr 1.2.0.
  #   ℹ Please use `gather()` instead.
  #   Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
} else {
  print("reading in saved truth data")
  truth <- local({
    load(file.path(dir.outputs, paste0(save.name.scenario, "_", save.name.truth, "_run_truth.RData")))
    result
  })
}
#* Returns a list object with a variety of information (see truth object in Global Environment for more details)

#* -------------- (B) create ecological observations from Atlantis outputs (fishery independent survey data)
#*
#* calls for using the function create_survey(), this function needs the following:
#*
#* dat     - A data.frame of numbers at age
#* time    - The timing of the survey (a vector indicating specific time steps, which are typically associated with years) i.e., seq(365,10*3650,365) would be an annual survey for 10 years
#* species - The species to sample in the survey (a vector)
#* boxes   - A matrix with two columns: 1) polygon: box ID's that are sampled 2) survArea: area sampled in that box
#* effic   - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species efficiency:
#* selex   - Selectivity at age. A dataframe defining selectivity at age for each species.
#*
#* see ?create_survey to get more info.
#*
#* Notes: Specify our survey sampling. This could come from other information such as
#* the overlap of actual survey stations with OM polygons, experiments
#* evaluating survey selectivity and efficiency, actual sample-based survey
#* cv, etc.

#* HAP - in this application, the dat file for the SS3 model has 6 fleets (see SS.datfile later in the script):
#*      1 - MexCal_S1 (fishery)
#*      2 - MexCal_S2 (fishery)
#*      3 - PNW (fishery)
#*      4 - AT_survey (survey)
#*      5 - DEPM (survey)
#*      6 - TEP_all (all survey)
#* HAP - 01.2025 - Call with IK and RW: RW suggested to keep AT_survey but drop DEPM and TEP_all
#*
#* Fleets 4-6 are surveys, thus we will go through this function once for each of the three surveys.
#*
#* note, catch data will need to be split by seasons, however this does not seem to be necessary for the
#* CPUE or Length/Age data from the surveys.

#* -----
#* ----- creating ecological observations for the AT_survey survey
#* -----

# (a) A data.frame of numbers at age

# You can get this from run_truth() via numbers output (head(truth$nums)) (see above for the use of this function)

# (b) timing of the survey

# truth$nums$time provides time step index (0,1,2,3,...), so we need to determine the index values that
# corresponds to the years:months we want to sample

# get timing info from Atlantis run file
load_runpar <- load_runprm(dir.outputs, in.run)
# frequency of Atlantis output throughout the year
ts_stepperyr <- if(load_runpar$outputstepunit == "days") 365 / load_runpar$toutinc # 5, ie atlantis reports output 5 times a year

# looking at the OG dat file, AT_survey samples in season 10 and/or 1. To start,
# I am going to sample every year at season 10. However, it looks like we can play with this based
# on what we want to assume for this survey.
ts_range_ATsurvey <- c(273:303) # assuming a year with 365 days, these day correspond to season 10

# determine the first reporting record that is associated to the sample range
ts_sampinyr <- seq(0,ts_stepperyr)[which((seq(0,ts_stepperyr) * load_runpar$toutinc) %in% ts_range_ATsurvey)]
# define vector of ts indices of sampling
ts_sampling_ATsurvey <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends

# (c) species to sample, atlantis functional group "name". This can be just the assessed species or a list of species, eg: c("Pacific_sardine","Mesopel_M_Fish")

# defined above as species_ss

# (d) box ID's that are sampled

# load bgm data, see ?load_box for more info.
load_boxes <- load_box(dir.outputs, in.bgm)

# HAP, need to determine the appropriate polygons for this survey!
# for now, going to just assume all polygons

# select Atlantis polygon IDs for sampling
boxes_sampling_ATsurvey <- c(0:(load_boxes$nbox - 1)) # HAP, this is box IDs 0,1,... which does not line up with unique(truth$biomass_ages$polygon) which is 1,2,... when specifying specific polygons should confirm that the correct ones are being selected...

# (e) Efficiency of the survey

# In this context, "efficiency" is typically used to describe the proportion of available fish caught by the survey gear (gear efficiency).
# Values range from 0 to 1; 1 = all means all fish in the survey area are detected or caught (perfect efficiency), < 1 means only a fraction of the fish are detected or caught.
#* HAP: in the atlantisom examples, this was 1 for the perfect survey example,
#* https://github.com/r4atlantis/atlantisom/blob/master/config/census_spec.R
#* and 0.5 for the other survey example.
#* https://sgaichas.github.io/poseidon-dev/Sardines_atlantisom2SS_demo.html
efficiency_ATsurvey <- data.frame(species    = species_ss,
                                  efficiency = 1
) # Note, this is the format that the function is expecting
# HAP - 12.2024. convo with RW and IK: Lets start with the perfect setting then
# see if we want to make this more complex, Hake it might not be 1 ...

# (f) Selectivity at age

# age classes of your species
age_classes <- 1:10 # HAP, I think this is the Atlantis age classes as I tried 1:12 and it still produced 1:10 output
# selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1, length(age_classes)) # Assume uniform selectivity
# prep data for function expectations
selectivity_ATsurvey <- data.frame(species = rep(species_ss, length(age_classes)),
                                   agecl   = age_classes,
                                   selex   = sel_by_age)

# (---) Create survey observations from Atlantis output

survey_numbers_ATsurvey <- create_survey(dat     = truth$nums,
                                         time    = ts_sampling_ATsurvey,
                                         species = species_ss,
                                         boxes   = boxes_sampling_ATsurvey,
                                         effic   = efficiency_ATsurvey,
                                         selex   = selectivity_ATsurvey);head(survey_numbers_ATsurvey)

# #* NOTE, in the example code, they also use biomass data ($biomass_ages (biomass for age structured groups))
# survey_biomass_ATsurvey <- create_survey(dat     = truth$biomass_ages,
#                                          time    = ts_sampling_ATsurvey,
#                                          species = species_ss,
#                                          boxes   = boxes_sampling_ATsurvey,
#                                          effic   = efficiency_ATsurvey,
#                                          selex   = selectivity_ATsurvey);head(survey_biomass_ATsurvey)
# #* but, I am not sure if that is just showcasing an example or if the function can actually be used this way...

#* -----
#* ----- creating ecological observations for the DEPM survey
#* -----

# (a) A data.frame of numbers at age

# You can get this from run_truth() via numbers output (head(truth$nums)) (see above for the use of this function)

# (b) timing of the survey

# truth$nums$time provides time step index (0,1,2,3,...), so we need to determine the index values that
# corresponds to the years:months we want to sample

# get timing info from Atlantis run file
load_runpar <- load_runprm(dir.outputs, in.run)
# frequency of Atlantis output throughout the year
ts_stepperyr <- if(load_runpar$outputstepunit == "days") 365 / load_runpar$toutinc # 5, ie atlantis reports output 5 times a year

# looking at the OG dat file, DEPM samples in season 10.
ts_range_DEPM <- c(273:303) # assuming a year with 365 days, these day correspond to season 10

# determine the first reporting record that is associated to the sample range
ts_sampinyr <- seq(0,ts_stepperyr)[which((seq(0,ts_stepperyr) * load_runpar$toutinc) %in% ts_range_DEPM)]
# define vector of ts indices of sampling
ts_sampling_DEPM <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends

# (c) species to sample, atlantis functional group "name". This can be just the assessed species or a list of species, eg: c("Pacific_sardine","Mesopel_M_Fish")

# defined above as species_ss

# (d) box ID's that are sampled

# load bgm data, see ?load_box for more info.
load_boxes <- load_box(dir.outputs, in.bgm)

# HAP, need to determine the appropriate polygons for this survey!
# for now, going to just assume all polygons

# select Atlantis polygon IDs for sampling
boxes_sampling_DEPM <- c(0:(load_boxes$nbox - 1)) # HAP, this is box IDs 0,1,... which does not line up with unique(truth$biomass_ages$polygon) which is 1,2,... when specifying specific polygons should confirm that the correct ones are being selected...

# (e) Efficiency of the survey

# In this context, "efficiency" is typically used to describe the proportion of available fish caught by the survey gear (gear efficiency).
# Values range from 0 to 1; 1 = all means all fish in the survey area are detected or caught (perfect efficiency), < 1 means only a fraction of the fish are detected or caught.
#* HAP: in the atlantisom examples, this was 1 for the perfect survey example,
#* https://github.com/r4atlantis/atlantisom/blob/master/config/census_spec.R
#* and 0.5 for the other survey example.
#* https://sgaichas.github.io/poseidon-dev/Sardines_atlantisom2SS_demo.html
efficiency_DEPM <- data.frame(species    = species_ss,
                              efficiency = 1
) # Note, this is the format that the function is expecting
# HAP - 12.2024. convo with RW and IK: Lets start with the perfect setting then
# see if we want to make this more complex, Hake it might not be 1 ...

# (f) Selectivity at age

# age classes of your species
age_classes <- 1:10 # HAP, I think this is the Atlantis age classes as I tried 1:12 and it still produced 1:10 output
# selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1, length(age_classes)) # Assume uniform selectivity
# prep data for function expectations
selectivity_DEPM <- data.frame(species = rep(species_ss, length(age_classes)),
                               agecl   = age_classes,
                               selex   = sel_by_age)

# (---) Create survey observations from Atlantis output

survey_numbers_DEPM <- create_survey(dat     = truth$nums,
                                     time    = ts_sampling_DEPM,
                                     species = species_ss,
                                     boxes   = boxes_sampling_DEPM,
                                     effic   = efficiency_DEPM,
                                     selex   = selectivity_DEPM);head(survey_numbers_DEPM)

# #* NOTE, in the example code, they also use biomass data ($biomass_ages (biomass for age structured groups))
# survey_biomass_DEPM <- create_survey(dat     = truth$biomass_ages,
#                                          time    = ts_sampling_DEPM,
#                                          species = species_ss,
#                                          boxes   = boxes_sampling_DEPM,
#                                          effic   = efficiency_DEPM,
#                                          selex   = selectivity_DEPM);head(survey_biomass_DEPM)
# #* but, I am not sure if that is just showcasing an example or if the function can actually be used this way...

#* -----
#* ----- creating ecological observations for the TEPall survey
#* -----

# (a) A data.frame of numbers at age

# You can get this from run_truth() via numbers output (head(truth$nums)) (see above for the use of this function)

# (b) timing of the survey

# truth$nums$time provides time step index (0,1,2,3,...), so we need to determine the index values that
# corresponds to the years:months we want to sample

# get timing info from Atlantis run file
load_runpar <- load_runprm(dir.outputs, in.run)
# frequency of Atlantis output throughout the year
ts_stepperyr <- if(load_runpar$outputstepunit == "days") 365 / load_runpar$toutinc # 5, ie atlantis reports output 5 times a year

# looking at the OG dat file, TEPall samples in season 10
ts_range_TEPall <- c(273:303) # assuming a year with 365 days, these day correspond to season 10

# determine the first reporting record that is associated to the sample range
ts_sampinyr <- seq(0,ts_stepperyr)[which((seq(0,ts_stepperyr) * load_runpar$toutinc) %in% ts_range_TEPall)]
# define vector of ts indices of sampling
ts_sampling_TEPall <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends

# (c) species to sample, atlantis functional group "name". This can be just the assessed species or a list of species, eg: c("Pacific_sardine","Mesopel_M_Fish")

# defined above as species_ss

# (d) box ID's that are sampled

# load bgm data, see ?load_box for more info.
load_boxes <- load_box(dir.outputs, in.bgm)

# HAP, need to determine the appropriate polygons for this survey!
# for now, going to just assume all polygons

# select Atlantis polygon IDs for sampling
boxes_sampling_TEPall <- c(0:(load_boxes$nbox - 1)) # HAP, this is box IDs 0,1,... which does not line up with unique(truth$biomass_ages$polygon) which is 1,2,... when specifying specific polygons should confirm that the correct ones are being selected...

# (e) Efficiency of the survey

# In this context, "efficiency" is typically used to describe the proportion of available fish caught by the survey gear (gear efficiency).
# Values range from 0 to 1; 1 = all means all fish in the survey area are detected or caught (perfect efficiency), < 1 means only a fraction of the fish are detected or caught.
#* HAP: in the atlantisom examples, this was 1 for the perfect survey example,
#* https://github.com/r4atlantis/atlantisom/blob/master/config/census_spec.R
#* and 0.5 for the other survey example.
#* https://sgaichas.github.io/poseidon-dev/Sardines_atlantisom2SS_demo.html
efficiency_TEPall <- data.frame(species    = species_ss,
                                efficiency = 1
) # Note, this is the format that the function is expecting
# HAP - 12.2024. convo with RW and IK: Lets start with the perfect setting then
# see if we want to make this more complex, Hake it might not be 1 ...

# (f) Selectivity at age

# age classes of your species
age_classes <- 1:10 # HAP, I think this is the Atlantis age classes as I tried 1:12 and it still produced 1:10 output
# selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1, length(age_classes)) # Assume uniform selectivity
# prep data for function expectations
selectivity_TEPall <- data.frame(species = rep(species_ss, length(age_classes)),
                                 agecl   = age_classes,
                                 selex   = sel_by_age)

# (---) Create survey observations from Atlantis output

survey_numbers_TEPall <- create_survey(dat     = truth$nums,
                                       time    = ts_sampling_TEPall,
                                       species = species_ss,
                                       boxes   = boxes_sampling_TEPall,
                                       effic   = efficiency_TEPall,
                                       selex   = selectivity_TEPall);head(survey_numbers_TEPall)

# #* NOTE, in the example code, they also use biomass data ($biomass_ages (biomass for age structured groups))
# survey_biomass_TEPall <- create_survey(dat     = truth$biomass_ages,
#                                          time    = ts_sampling_TEPall,
#                                          species = species_ss,
#                                          boxes   = boxes_sampling_TEPall,
#                                          effic   = efficiency_TEPall,
#                                          selex   = selectivity_TEPall);head(survey_biomass_TEPall)
# #* but, I am not sure if that is just showcasing an example or if the function can actually be used this way...

#* -------------- (C) sample a biomass index of abundance from an Atlantis scenario
#*
#* This call for using the function sample_survey_biomass(). This function needs the following:
#*
#* dat     - A data.frame of numbers at age
#* cv      - Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
#* wtAtAge - Weight-at-age by species. a matrix with columns: species, agecl, time (optional), wtAtAge (kg)
#*
#* see ?sample_survey_biomass to get more info.
#*
#* We will go through this function once for each of the three surveys identified in the OG dat file

#* -----
#* ----- sample a biomass index of abundance from AT_survey survey
#* -----

# (a) A data.frame of numbers at age

# You get this data from the output of create_survey() (see survey_numbers_ATsurvey)

# (b) Coefficient of variation for the entire species specific biomass

cv_ATsurvey <- data.frame(species = species_ss,
                          cv      = 0.25 # 0.1
)
# Note, this is the format of this data that the function is expecting

# (3) Weight-at-age by species

#* this calls for using calc_age2length() to calculate length from ages
#*
#* needed for function:
#* structn  - A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* resn     - A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* nums     - A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.
#* biolprm  - A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.
#* fgs      - A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".
#* CVlenage - The variability in length at age (same for all species) The default value is 0.1.
#*
#* see ?calc_age2length to get more info.

#* HAP - Jan 27 2024 - call with IK, RW and SG: we may need to dig a little bit into the calc_age2length function:
#*
#* https://github.com/r4atlantis/atlantisom/blob/master/R/calc_age2length.R
#* https://github.com/r4atlantis/atlantisom/blob/1e5d32854b417a0d1650d45a4a4f41041f08594c/R/calc_age2length.R#L145
#*
#* Specifically, we may need to adjust the computations of the length bins:
#* (i) some of my earlier work with this script indicated issues with capturing smaller length bins
#* (ii) we may need to make adjustments so that we have length bin increments of .5, not 1
#*

# (i)  A data.frame containing the structural nitrogen data

# First, use aggregateDensityData() to aggregate and select density data from Atlantis output --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, survey_eco_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_ss
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
survey_structn_aggr_ATsurvey <- aggregateDensityData(dat     = truth$structn,
                                                     time    = ts_sampling_ATsurvey,
                                                     species = species_ss,
                                                     boxes   = boxes_sampling_ATsurvey); head(survey_structn_aggr_ATsurvey)

# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

# first, define effN for each species in a format that the function expects
# HAP, for this walk through we only have one species of focus thus only one effN needed
effN_ATsurvey <- data.frame(species = species_ss,
                            effN    = rep(100, # note, the example used these values also: 1000, 1e+8
                                          length(species_ss)))
# next, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_structn_sample_ATsurvey <- sample_fish(dat    = survey_structn_aggr_ATsurvey,
                                              effN   = effN_ATsurvey,
                                              sample = FALSE);head(survey_structn_sample_ATsurvey)

# (ii) A data.frame containing the reserve nitrogen data

# First, use aggregateDensityData() to aggregate and select density data from Atlantis output --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, survey_eco_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_ss
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
#*
survey_resn_aggr_ATsurvey <- aggregateDensityData(dat     = truth$resn,
                                                  time    = ts_sampling_ATsurvey,
                                                  species = species_ss,
                                                  boxes   = boxes_sampling_ATsurvey); head(survey_resn_aggr_ATsurvey)
# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#*
# Here, effN has already been defined (see above, effN_ATsurvey), so can go straight into creating composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_resn_sample_ATsurvey <- sample_fish(dat    = survey_resn_aggr_ATsurvey,
                                           effN   = effN_ATsurvey,
                                           sample = FALSE);head(survey_resn_sample_ATsurvey)

# (iii) A data.frame containing the numbers composition data

# we can do this by using sample_fish() to sample numbers-at-age (survey_numbers, see above) and create composition data --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#
# Here, effN has already been defined (see above, survey_effN)
# so we can go straight to creating numbers composition data
# NOTE!! Here we ARE sampling thus set sample to TRUE (default)
survey_age_comp_data_ATsurvey <- sample_fish(dat  = survey_numbers_ATsurvey,
                                             effN = effN_ATsurvey); head(survey_age_comp_data_ATsurvey)

# (iv) biolprm data stored in data from run_truth() (truth$biolprm)

# (v) fgs data, stored in data from run_truth() (truth$fgs)

# (vi) variability in length at age (lenage saved in CVs, defined above)
#* in example, for the perfect survey::: CVs <- list("lenage"=0, "fishery"=0, "survey"=0),
#* in example, for the other survey::: CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)
ss_var_lenatage <- 0.1

# (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
ss_maxbin <- 40 #  # HAP, the example used this 200

# (***) run calc_age2length to extract length composition data
survey_age2length_ATsurvey <- calc_age2length(structn  = survey_structn_sample_ATsurvey,
                                              resn     = survey_resn_sample_ATsurvey,
                                              nums     = survey_age_comp_data_ATsurvey,
                                              biolprm  = truth$biolprm,
                                              fgs      = truth$fgs,
                                              maxbin   = ss_maxbin, # Not in OG example, but shouldn't it be especially since it is used when catch data is passed through the function (see below)
                                              CVlenage = ss_var_lenatage)
#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(survey_age2length_ATsurvey$mulen)
head(survey_age2length_ATsurvey$muweight)
head(survey_age2length_ATsurvey$natlength) # unique(survey_age2length_ATsurvey$natlength$lower.bins); length(unique(survey_age2length_ATsurvey$natlength$lower.bins))

#* Lastly, we need to restructure the mean weight-at-age (muweight) data for sample_survey_biomass()
survey_age2length_ATsurvey_wtAtAge <- survey_age2length_ATsurvey$muweight %>%
  select(species, agecl, time, wtAtAge = atoutput) %>%
  mutate(wtAtAge = wtAtAge/1000) # HAP, converting wtAtAge to kgs for sample_survey_biomass()

# (---) run sample_survey_biomass() to sample a biomass index of abundance from an atlantis scenario

# sample_survey_biomass(), Sample a biomass index of abundance from an atlantis scenario
# this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# data    = A data.frame of numbers at age
# cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
# wtAtAge = Weight-at-age by species. a matrix with columns: species, agecl, time (optional), wtAtAge (kg)
survey_observed_biomass_ATsurvey <- sample_survey_biomass(dat     = survey_numbers_ATsurvey,
                                                          cv      = cv_ATsurvey,
                                                          wtAtAge = survey_age2length_ATsurvey_wtAtAge); head(survey_observed_biomass_ATsurvey)

# # Note, the example code also showcases sample_survey_numbers
# # as it is currently not being used in this script, this is commented out but
# # something to consider in case this data is needed for the dat file
# #
# # sample_survey_numbers(), Sample a numbers index of abundance from an atlantis scenario
# # this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# # data    = A data.frame of numbers at age
# # cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
# survey_observed_numbers_ATsurvey  <- sample_survey_numbers(dat     = survey_numbers_ATsurvey,
#                                                            cv      = cv_ATsurvey); head(survey_observed_numbers_ATsurvey)

#* -----
#* ----- creating ecological observations for the DEPM survey
#* -----

# (a) A data.frame of numbers at age

# You get this data from the output of create_survey() (see survey_numbers_DEPM)

# (b) Coefficient of variation for the entire species specific biomass

cv_DEPM <- data.frame(species = species_ss,
                      cv      = 0.25 # 0.1
)
# Note, this is the format of this data that the function is expecting

# (3) Weight-at-age by species

#* this calls for using calc_age2length() to calculate length from ages
#*
#* needed for function:
#* structn  - A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* resn     - A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* nums     - A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.
#* biolprm  - A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.
#* fgs      - A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".
#* CVlenage - The variability in length at age (same for all species) The default value is 0.1.
#*
#* see ?calc_age2length to get more info.

# (i)  A data.frame containing the structural nitrogen data

# First, use aggregateDensityData() to aggregate and select density data from Atlantis output --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, survey_eco_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_ss
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
survey_structn_aggr_DEPM <- aggregateDensityData(dat     = truth$structn,
                                                 time    = ts_sampling_DEPM,
                                                 species = species_ss,
                                                 boxes   = boxes_sampling_DEPM); head(survey_structn_aggr_DEPM)

# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

# first, define effN for each species in a format that the function expects
# HAP, for this walk through we only have one species of focus thus only one effN needed
effN_DEPM <- data.frame(species = species_ss,
                        effN    = rep(100, # note, the example used these values also: 1000, 1e+8
                                      length(species_ss)))
# next, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_structn_sample_DEPM <- sample_fish(dat    = survey_structn_aggr_DEPM,
                                          effN   = effN_DEPM,
                                          sample = FALSE);head(survey_structn_sample_DEPM)

# (ii) A data.frame containing the reserve nitrogen data

# First, use aggregateDensityData() to aggregate and select density data from Atlantis output --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, survey_eco_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_ss
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
#*
survey_resn_aggr_DEPM <- aggregateDensityData(dat     = truth$resn,
                                              time    = ts_sampling_DEPM,
                                              species = species_ss,
                                              boxes   = boxes_sampling_DEPM); head(survey_resn_aggr_DEPM)
# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#*
# Here, effN has already been defined (see above, effN_DEPM), so can go straight into creating composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_resn_sample_DEPM <- sample_fish(dat    = survey_resn_aggr_DEPM,
                                       effN   = effN_DEPM,
                                       sample = FALSE);head(survey_resn_sample_DEPM)

# (iii) A data.frame containing the numbers composition data

# we can do this by using sample_fish() to sample numbers-at-age (survey_numbers, see above) and create composition data --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#
# Here, effN has already been defined (see above, survey_effN)
# so we can go straight to creating numbers composition data
# NOTE!! Here we ARE sampling thus set sample to TRUE (default)
survey_age_comp_data_DEPM <- sample_fish(dat  = survey_numbers_DEPM,
                                         effN = effN_DEPM); head(survey_age_comp_data_DEPM)

# (iv) biolprm data stored in data from run_truth() (truth$biolprm)

# (v) fgs data, stored in data from run_truth() (truth$fgs)

# (vi) variability in length at age (lenage saved in CVs, defined above)
#* in example, for the perfect survey::: CVs <- list("lenage"=0, "fishery"=0, "survey"=0),
#* in example, for the other survey::: CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)
ss_var_lenatage <- 0.1

# (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
ss_maxbin <- 40 #  # HAP, the example used this 200

# (***) run calc_age2length to extract length composition data
survey_age2length_DEPM <- calc_age2length(structn  = survey_structn_sample_DEPM,
                                          resn     = survey_resn_sample_DEPM,
                                          nums     = survey_age_comp_data_DEPM,
                                          biolprm  = truth$biolprm,
                                          fgs      = truth$fgs,
                                          maxbin   = ss_maxbin, # Not in OG example, but shouldn't it be especially since it is used when catch data is passed through the function (see below)
                                          CVlenage = ss_var_lenatage)
#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(survey_age2length_DEPM$mulen)
head(survey_age2length_DEPM$muweight)
head(survey_age2length_DEPM$natlength) # unique(survey_age2length_DEPM$natlength$lower.bins); length(unique(survey_age2length_DEPM$natlength$lower.bins))

#* Lastly, we need to restructure the mean weight-at-age (muweight) data for sample_survey_biomass()
survey_age2length_DEPM_wtAtAge <- survey_age2length_DEPM$muweight %>%
  select(species, agecl, time, wtAtAge = atoutput) %>%
  mutate(wtAtAge = wtAtAge/1000) # HAP, converting wtAtAge to kgs for sample_survey_biomass()

# (---) run sample_survey_biomass() to sample a biomass index of abundance from an atlantis scenario

# sample_survey_biomass(), Sample a biomass index of abundance from an atlantis scenario
# this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# data    = A data.frame of numbers at age
# cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
# wtAtAge = Weight-at-age by species. a matrix with columns: species, agecl, time (optional), wtAtAge (kg)
survey_observed_biomass_DEPM <- sample_survey_biomass(dat     = survey_numbers_DEPM,
                                                      cv      = cv_DEPM,
                                                      wtAtAge = survey_age2length_DEPM_wtAtAge); head(survey_observed_biomass_DEPM)

# # Note, the example code also showcases sample_survey_numbers
# # as it is currently not being used in this script, this is commented out but
# # something to consider in case this data is needed for the dat file
# #
# # sample_survey_numbers(), Sample a numbers index of abundance from an atlantis scenario
# # this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# # data    = A data.frame of numbers at age
# # cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
# survey_observed_numbers_DEPM  <- sample_survey_numbers(dat     = survey_numbers_DEPM,
#                                                            cv  = cv_DEPM); head(survey_observed_numbers_DEPM)

#* -----
#* ----- creating ecological observations for the TEPall survey
#* -----

# (a) A data.frame of numbers at age

# You get this data from the output of create_survey() (see survey_numbers_TEPall)

# (b) Coefficient of variation for the entire species specific biomass

cv_TEPall <- data.frame(species = species_ss,
                        cv      = 0.25 # 0.1
)
# Note, this is the format of this data that the function is expecting

# (3) Weight-at-age by species

#* this calls for using calc_age2length() to calculate length from ages
#*
#* needed for function:
#* structn  - A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* resn     - A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* nums     - A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.
#* biolprm  - A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.
#* fgs      - A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".
#* CVlenage - The variability in length at age (same for all species) The default value is 0.1.
#*
#* see ?calc_age2length to get more info.

# (i)  A data.frame containing the structural nitrogen data

# First, use aggregateDensityData() to aggregate and select density data from Atlantis output --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, survey_eco_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_ss
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
survey_structn_aggr_TEPall <- aggregateDensityData(dat     = truth$structn,
                                                   time    = ts_sampling_TEPall,
                                                   species = species_ss,
                                                   boxes   = boxes_sampling_TEPall); head(survey_structn_aggr_TEPall)

# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

# first, define effN for each species in a format that the function expects
# HAP, for this walk through we only have one species of focus thus only one effN needed
effN_TEPall <- data.frame(species = species_ss,
                          effN    = rep(100, # note, the example used these values also: 1000, 1e+8
                                        length(species_ss)))
# next, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_structn_sample_TEPall <- sample_fish(dat    = survey_structn_aggr_TEPall,
                                            effN   = effN_TEPall,
                                            sample = FALSE);head(survey_structn_sample_TEPall)

# (ii) A data.frame containing the reserve nitrogen data

# First, use aggregateDensityData() to aggregate and select density data from Atlantis output --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, survey_eco_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_ss
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
#*
survey_resn_aggr_TEPall <- aggregateDensityData(dat     = truth$resn,
                                                time    = ts_sampling_TEPall,
                                                species = species_ss,
                                                boxes   = boxes_sampling_TEPall); head(survey_resn_aggr_TEPall)
# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#*
# Here, effN has already been defined (see above, effN_TEPall), so can go straight into creating composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_resn_sample_TEPall <- sample_fish(dat    = survey_resn_aggr_TEPall,
                                         effN   = effN_TEPall,
                                         sample = FALSE);head(survey_resn_sample_TEPall)

# (iii) A data.frame containing the numbers composition data

# we can do this by using sample_fish() to sample numbers-at-age (survey_numbers, see above) and create composition data --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#
# Here, effN has already been defined (see above, survey_effN)
# so we can go straight to creating numbers composition data
# NOTE!! Here we ARE sampling thus set sample to TRUE (default)
survey_age_comp_data_TEPall <- sample_fish(dat  = survey_numbers_TEPall,
                                           effN = effN_TEPall); head(survey_age_comp_data_TEPall)

# (iv) biolprm data stored in data from run_truth() (truth$biolprm)

# (v) fgs data, stored in data from run_truth() (truth$fgs)

# (vi) variability in length at age (lenage saved in CVs, defined above)
#* in example, for the perfect survey::: CVs <- list("lenage"=0, "fishery"=0, "survey"=0),
#* in example, for the other survey::: CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)
ss_var_lenatage <- 0.1

# (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
ss_maxbin <- 40 #  # HAP, the example used this 200

# (***) run calc_age2length to extract length composition data
survey_age2length_TEPall <- calc_age2length(structn  = survey_structn_sample_TEPall,
                                            resn     = survey_resn_sample_TEPall,
                                            nums     = survey_age_comp_data_TEPall,
                                            biolprm  = truth$biolprm,
                                            fgs      = truth$fgs,
                                            maxbin   = ss_maxbin, # Not in OG example, but shouldn't it be especially since it is used when catch data is passed through the function (see below)
                                            CVlenage = ss_var_lenatage)
#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(survey_age2length_TEPall$mulen)
head(survey_age2length_TEPall$muweight)
head(survey_age2length_TEPall$natlength) # unique(survey_age2length_TEPall$natlength$lower.bins); length(unique(survey_age2length_TEPall$natlength$lower.bins))

#* Lastly, we need to restructure the mean weight-at-age (muweight) data for sample_survey_biomass()
survey_age2length_TEPall_wtAtAge <- survey_age2length_TEPall$muweight %>%
  select(species, agecl, time, wtAtAge = atoutput) %>%
  mutate(wtAtAge = wtAtAge/1000) # HAP, converting wtAtAge to kgs for sample_survey_biomass()

# (---) run sample_survey_biomass() to sample a biomass index of abundance from an atlantis scenario

# sample_survey_biomass(), Sample a biomass index of abundance from an atlantis scenario
# this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# data    = A data.frame of numbers at age
# cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
# wtAtAge = Weight-at-age by species. a matrix with columns: species, agecl, time (optional), wtAtAge (kg)
survey_observed_biomass_TEPall <- sample_survey_biomass(dat     = survey_numbers_TEPall,
                                                        cv      = cv_TEPall,
                                                        wtAtAge = survey_age2length_TEPall_wtAtAge); head(survey_observed_biomass_TEPall)

# # Note, the example code also showcases sample_survey_numbers
# # as it is currently not being used in this script, this is commented out but
# # something to consider in case this data is needed for the dat file
# #
# # sample_survey_numbers(), Sample a numbers index of abundance from an atlantis scenario
# # this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# # data    = A data.frame of numbers at age
# # cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
# survey_observed_numbers_TEPall  <- sample_survey_numbers(dat   = survey_numbers_TEPall,
#                                                            cv  = cv_TEPall); head(survey_observed_numbers_TEPall)

#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#* PAUSE
#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#*
#* now that we have an observation of biomass, lets take a break and compare this
#* against (i) true data and (ii) a perfect survey

#* Comparing our (census) survey based on true biomass from above with the
#* Atlantis output file “[modelscenario]BiomIndx.txt” should give us a perfect
#* match. Note that the our (census) survey may have more sampling in time than
#* the Atlantis output file.

# (1) subset sample survey biomass data by group of focus

forplot_surATsurvey_obsB_ss <- survey_observed_biomass_ATsurvey[survey_observed_biomass_ATsurvey$species == species_ss,]
forplot_surDEPM_obsB_ss     <- survey_observed_biomass_DEPM[survey_observed_biomass_DEPM$species == species_ss,]
forplot_surTEPall_obsB_ss   <- survey_observed_biomass_TEPall[survey_observed_biomass_TEPall$species == species_ss,]

# (2) get true biomass data from atlantis biomass txt output file

# read Atlantis output files
forplot.biomindx <- read.table(file.path(dir.outputs, list.files(path = dir.outputs, pattern = out.biomindx)[1]), header=T)
# lookup the matching names, put in time, species, biomass column format
load_groups <- load_fgs(dir = dir.outputs,file_fgs = in.groups)
# prep biom.txt data for plotting
forplot.biomindx.tidy <- forplot.biomindx %>%
  select(Time:DIN) %>%
  rename(!!!setNames(as.list(as.character(load_groups$Code)), load_groups$Name)) %>%
  gather(species, biomass, -Time) %>%
  filter(species %in% c(species_ss))

# (2) lets get biomass estmation from a PERFECT survey

# perfect survey
forplot_biomass_perfectobs <- create_survey(dat     = truth$biomass_ages,
                                            time    = c(0:(load_runpar$tstop / load_runpar$outputstep)), # all time steps sampled
                                            species = species_ss,
                                            boxes   = c(0:(load_boxes$nbox - 1)),
                                            effic   = data.frame(species    = species_ss,
                                                                 efficiency = rep(1.0,length(species_ss))),
                                            selex   = data.frame(species = rep(species_ss, each=10),
                                                                 agecl   = rep(c(1:10),length(species_ss)),
                                                                 selex   = rep(1.0,length(species_ss)*10)));head(forplot_biomass_perfectobs)
# perfect sample
forplot_biomass_perfectsample <- sample_survey_biomass(dat     = forplot_biomass_perfectobs,
                                                       cv      = data.frame(species = species_ss,
                                                                            cv      = rep(0.0,length(species_ss))),
                                                       wtAtAge = data.frame(species = rep(species_ss, each=10),
                                                                            agecl   = rep(c(1:10), length(species_ss)),
                                                                            wtAtAge = rep(1.0, length(species_ss)*10)));head(forplot_biomass_perfectsample)
# select focal group
forplot_biomass_perfectsample_ss <- forplot_biomass_perfectsample[forplot_biomass_perfectsample$species == species_ss,];head(forplot_biomass_perfectsample_ss)

# (3) plot

ggplot() +
  # plot the data from the biomindx.txt output file
  geom_point(data=forplot.biomindx.tidy, aes(x=Time/365,y=biomass/1,
                                             color="True B, biom.txt output"), alpha = 10/10) +
  # plot the data from a PERFECT survey (made just above for this comparison)
  geom_line(data=forplot_biomass_perfectsample_ss, aes(x=time/ts_stepperyr,y=atoutput*1000, # convert from kg to tonnes
                                                       color="perfect survey census B"), alpha = 10/10) +
  # plot the data from the survey we executed above
  geom_line(data=forplot_surATsurvey_obsB_ss, aes(x=time/ts_stepperyr,y=atoutput/1,
                                                  color="ATsurvey sample B from nums * wtAtAge"), alpha = 10/10) +
  geom_line(data=forplot_surDEPM_obsB_ss, aes(x=time/ts_stepperyr,y=atoutput/1,
                                              color="DEPM sample B from nums * wtAtAge"), alpha = 10/10) +
  geom_line(data=forplot_surTEPall_obsB_ss, aes(x=time/ts_stepperyr,y=atoutput/1,
                                                color="TEPall sample B from nums * wtAtAge"), alpha = 10/10) +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="",
       x = "time (years)",
       y = "biomass (tonnes)") +
  facet_wrap(~species, scales="free")

#* --------------
#* --------------
#* END PAUSE
#* --------------
#* --------------

#* ----------------------------
#* ---------------------------- I. Derive "Data" from Atlantis to give to SS3
#* ----------------------------    b) derive catch data (fishery dependent survey)
#* ----------------------------
#*
#* This will take a few sections of script:
#*         (A) loading Atlantis catch output (load catch)
#*         (B) Create a subset of the fishery observations for an Atlantis scenario
#*         (C) generate size composition data from fishery information
#*            -- note, in this section we will pause to make some graphs with the processed catch data

#* -------------- (A) loading Atlantis catch output (load catch)
#*
#* Loading this data calls for using the function load_catch() --- ?load_catch
#*
#* This function needs the following:
#*
#* dir        - The directory of the atlantis model output, where the default is getwd().
#* file_catch - A character value specifying the name of the text file that contains total annual catch in tonnes output for an Atlantis scenario. An example entry would be "outputSETAScatch.txt".
#* fgs        - A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".

# (a) directory to atlantis outputs

# defined above, dir.outputs

# (b) the name of the text file

file.catch.txt <- list.files(path = dir.outputs, pattern = out.catch)[1]

# (c) A data frame created by load_fgs

load_groups <- load_fgs(dir = dir.outputs,file_fgs = in.groups)

# (---) Load Atlantis catch data

load_catch_txt <- load_catch(dir        = dir.outputs,
                             file_catch = file.catch.txt,
                             fgs        = load_groups);head(load_catch_txt)
#* After much testing, the most reliable source for Atlantis catch in biomass is catch.txt.
#* Using the catch.txt file means that true total catch weight through atlantisom is
#* currently available only on an annual and full-model (all polygons) scale.
#* However, one can also find catch data in truth output:
head(truth$catch)     # numbers
head(truth$catchtons) # tonnes
head(truth$disctons)  # tonnes

#* Seeing that this info is partitioned by data like polygon and fleet, it would
#* be great to use this. Lets check if the sardine data reported in load_catch_txt
#* is the same as the sardine data reported in truth$catchtons

# isolate load_catch_txt for focal group in SS3 model
load_catch_txt_ss <- load_catch_txt[load_catch_txt$species == species_ss,] # sum(load_catch_txt_ss$atoutput)
# Aggregating data from truth$catchtons
truth_catchtons_agg <- aggregate(atoutput ~ species + time, data = truth$catchtons[truth$catchtons$species == "SAR",],
                                 sum);truth_catchtons_agg$species <- load_catch_txt_ss$species
# view
ggplot() +
  geom_line(data=load_catch_txt_ss, aes(x=time/365,y=atoutput, color="a. catch.txt"),
            alpha = 10/10) +
  geom_line(data=truth_catchtons_agg, aes(x=time,y=atoutput, color="b. truth$catchtons"), # truth$catch looks to be numbers
            alpha = 10/10, linetype = "dashed") +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="")

#* The two data sets match! Thus, we can move forward using truth$catchtons so we can partitioned the catch data as we need to

#* Now that catch has been loaded, we can define the catches for the fleets in SS3.
#* For this application this entails:
#*    (a) defining the Atlantis fleets that make up the SS3 fleets
#*    (b) defining the seasonal split in catches
#*
#* (a) defining the Atlantis fleets that make up the SS3 fleets
#*
#* For the application being developed herein, the dat file for the SS3 model has 6 fleets (see SS.datfile later in the script):
#*      1 - MexCal_S1 (fishery)
#*      2 - MexCal_S2 (fishery)
#*      3 - PNW (fishery)
#*      4 - AT_survey (survey)
#*      5 - DEPM (survey)
#*      6 - TEP_all (all survey)
#*
#* Fleets 1 - 3 are fishing fleets.
#*
#* Ultimately, we will need to separate CalCurr catch data based on the fleets represented in the fleet info passed to SS3
#* 19.12,2024, talked with IK
#* From IK:
#* Wait I see this: https://www.pcouncil.org/documents/2023/03/2022-pacific-sardine-stock-assessment-update.pdf/
#* The projection model included sardine NSP landings (metric tons) from six major fishing regions:
#* Ensenada, Mexico (ENS), southern California (SCA), central California (CCA), Oregon (OR),
#* Washington (WA), and British Columbia, Canada (BC). Catch data for the fisheries off ENS, SCA,
#* and CCA were pooled into a single “MexCal” fleet, and catch data from OR, WA, and BC were
#* combined and treated as a single “PacNW” fleet in the model. The sardine model is based on a
#* July-June model year, with two semester-based seasons per year (S1-July to December and S2-
#* January to June).
#*
#* IK recommended the following fleet split:

fleets_index_MexCal <- c(2,3,4,5,6,32)
fleets_index_PacNW  <- c(7,8,29,30,31,33)

#* isolate data for these fleets

# MexCal catch
catch_tons_ss_MexCal <- truth$catchtons  %>%
  filter(species == load_groups[which(load_groups$Name %in% species_ss),"Code"] # select data for SS3 spp (note, this data has group code, not name)
  ) %>%
  filter(fleet %in% as.character(fleets_index_MexCal) # select data from CalCur fleets that correspond to this fleet
  );head(catch_tons_ss_MexCal)

# PacNW catch
catch_tons_ss_PacNW <- truth$catchtons  %>%
  filter(species == load_groups[which(load_groups$Name %in% species_ss),"Code"] # select data for SS3 spp (note, this data has group code, not name)
  ) %>%
  filter(fleet %in% as.character(fleets_index_PacNW) # select data from CalCur fleets that correspond to this fleet
  );head(catch_tons_ss_PacNW)

#* (b) defining the seasonal split in catches
#*
#* as stated above:
#* The sardine model is based on a July-June model year, with two semester-based seasons
#* per year (S1-July to December and S2-January to June).
#*
#* thus, ultimately, we will need to separate CalCurr catch data based on these seasons

catch.season.1 <- c(7:12)
catch.season.2 <- c(1:6)

#* HAP
#* right now, the catch data is only reported annually from the model thus I do not have seasonal data,
#* truth$catchtons$time
#* so this will need to be adjusted in order to properly split catches seasonally for the SS3 dat file
#* but, we can make a rough estimate to develop the script
#* Looking at the data in RWs dat file it looks like
#* about 80/20 split between S2/S1 for MexCal
#* about 60/40 split between S2/S1 for PacNW

# MexCal catch

# summarize data by time
catch_tons_ss_MexCal_agg <- catch_tons_ss_MexCal %>%
  group_by(time, species) %>%
  summarize(total_atoutput = sum(atoutput, na.rm = TRUE), .groups = "drop");head(catch_tons_ss_MexCal_agg)
# apply seasonal split to data
catch_tons_ss_MexCal_agg <- catch_tons_ss_MexCal_agg %>%
  # Expand data to add a 'season' column with values 1 and 2
  tidyr::expand_grid(season = c(1, 2)) %>%
  # Modify the 'total_atoutput' column based on 'season'
  mutate(
    total_atoutput = ifelse(season == 1, 0.2 * total_atoutput,
                            0.8 * total_atoutput),
  );head(catch_tons_ss_MexCal_agg)

# PacNW catch

# summarize data by time
catch_tons_ss_PacNW_agg <- catch_tons_ss_PacNW %>%
  group_by(time, species) %>%
  summarize(total_atoutput = sum(atoutput, na.rm = TRUE), .groups = "drop");head(catch_tons_ss_PacNW_agg)
# apply seasonal split to data
# HAP, I do not have the seasonal split data yet so I am just assuming 60/40 split between S2/S1 based on OG dat file
catch_tons_ss_PacNW_agg <- catch_tons_ss_PacNW_agg %>%
  # Expand data to add a 'season' column with values 1 and 2
  tidyr::expand_grid(season = c(1, 2)) %>%
  # Modify the 'total_atoutput' column based on 'season'
  mutate(
    total_atoutput = ifelse(season == 1, 0.4 * total_atoutput,
                            0.6 * total_atoutput),
  );head(catch_tons_ss_PacNW_agg)

#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#* PAUSE
#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#*
#* Lets take a pause and visualize these data splits

# isolate catch txt data (true, total catch) for focal group in SS3 model
forplot_truecatch_ss <- load_catch_txt[load_catch_txt$species == species_ss,]
forplot_truecatch_ss$species <- "Pacific sardine (tonnes)"

# aggregate fleet specific catch data
forplot_catch_tons_ss_MexCalS1_agg <- aggregate(total_atoutput ~ species + time, data = catch_tons_ss_MexCal_agg[which(catch_tons_ss_MexCal_agg$season == 1),], sum);head(forplot_catch_tons_ss_MexCalS1_agg)
forplot_catch_tons_ss_MexCalS1_agg$species <- "Pacific sardine (tonnes)"

forplot_catch_tons_ss_MexCalS2_agg <- aggregate(total_atoutput ~ species + time, data = catch_tons_ss_MexCal_agg[which(catch_tons_ss_MexCal_agg$season == 2),], sum);head(forplot_catch_tons_ss_MexCalS2_agg)
forplot_catch_tons_ss_MexCalS2_agg$species <- "Pacific sardine (tonnes)"

forplot_catch_tons_ss_PacNW_agg <- aggregate(total_atoutput ~ species + time, data = catch_tons_ss_PacNW_agg, sum);head(forplot_catch_tons_ss_PacNW_agg)
forplot_catch_tons_ss_PacNW_agg$species <- "Pacific sardine (tonnes)"

# any catch data missed by the fleet selection?
forplot_catch_tons_ss_missed <- truth$catchtons %>%
  filter(species == load_groups[which(load_groups$Name %in% species_ss),"Code"]) %>%
  filter(!(fleet %in% as.character(c(fleets_index_MexCal,fleets_index_PacNW)))) %>%
  group_by(time, species) %>%
  summarize(total_atoutput = sum(atoutput, na.rm = TRUE), .groups = "drop");head(forplot_catch_tons_ss_missed)
forplot_catch_tons_ss_missed$species <- "Pacific sardine (tonnes)"

# View
ggplot() +
  geom_line(data=forplot_truecatch_ss, aes(x=time/365,y=atoutput, color="a. total catch"), alpha = 10/10) +
  geom_line(data=forplot_catch_tons_ss_MexCalS1_agg, aes(x=time,y=total_atoutput, color="b. MexCalS1 catch"), alpha = 10/10) +
  geom_line(data=forplot_catch_tons_ss_MexCalS2_agg, aes(x=time,y=total_atoutput, color="c. MexCalS2 catch"), alpha = 10/10) +
  geom_line(data=forplot_catch_tons_ss_PacNW_agg, aes(x=time,y=total_atoutput, color="d. PacNW catch"), alpha = 10/10) +
  geom_line(data=forplot_catch_tons_ss_missed, aes(x=time,y=total_atoutput, color="e. not included"), alpha = 10/10) +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="")+
  facet_wrap(~species, scales="free")

# HAP, doing a VERY quick look at the data, it looks like fleets
# 35, 36, 46, 48, 49, 52
# have reasonable large catches of SAR as well

#* 01.2025 - Call with IK and RW - RW brought up some points with this plot.
#* First, MexCal fleets should be landing more SAR than the PacNW,
#* Second, not ALL SAR catch will be included with our fleets so that is okay
#* Third, we may been to revisit the fleet allocations as although we will not
#* grab all SAR catch with our fleet selections, there is still quite a lot of
#* being missed

#* --------------
#* --------------
#* END PAUSE
#* --------------
#* --------------

#* -------------- (B) Create a subset of the fishery observations for an Atlantis scenario.
#*
#* This calls for using the function create_fishery_subset(). This function needs the following:
#*
#* dat     - A data.frame of numbers at age
#* time    - The timing of the survey (a vector indicating specific time steps, which are typically associated with years) i.e., seq(365,10*3650,365) would be an annual survey for 10 years
#* fleets  - which fleet indices to aggregate in the output (NULL aggregates all fleets)
#* species - The species to sample in the survey (a vector)
#* boxes   - A vector of box numbers
#*
#* see ?create_fishery_subset to get more info.

#* HAP
#* Before diving into running create_fishery_subset for each of our three fleets, we need to
#* do some prep work. Because we are sub-setting the data be fleets, we run into a little bump.
#* In the OG script, the truth$catch data is passed for the dat argument (A data.frame of numbers
#* at age), but that data does not have a fleet column. Thus, when we try to run the
#* create_fishery_subset function with a vector of fleets, it errors out. Previously, we did not
#* have an issue because we set the fleets argument to NULL thus aggregating fleets.
#*
#* I believe we need to proceed with truth$catch rather than truth$catchtons, which does have
#* fleet info., because later function expect number-at-age data.
#*
#* So, to move forward with using the create_fishery_subset function,
#* I have manipulated truth$catch data to incorporate a fleet column.
#*
#* To do this, I made a general assumption that the fleet proportions in truth$catchtons
#* will be enough to split the truth$catch data by fleets.

# Step 1: Calculate proportional breakdown in catchtons by fleet
# note, I tried using create_fishery_subset to make this df but the function drops the fleets column
# - subset catchtons by FID
temp_catchtons_fishery_subset <- truth$catchtons[which(truth$catchtons$species %in% "SAR"),]
temp_catchtons_fishery_subset$species <- species_ss # rename so spp names in both dfs match
head(temp_catchtons_fishery_subset)

# - Calculate proportional breakdown
temp_catchtons_fishery_subset_prop <- temp_catchtons_fishery_subset %>%
  group_by(species, agecl, polygon, time) %>%
  mutate(proportion = atoutput / sum(atoutput, na.rm = TRUE)) %>%
  filter(proportion >= 0) %>% # Keep only rows with non-NAN values
  ungroup();head(temp_catchtons_fishery_subset_prop)

# Step 2: Join proportional breakdown with truth$catch
# - subset truth$catch by FID
temp_catch_fishery_subset <- truth$catch[which(truth$catch$species %in% species_ss),];head(temp_catch_fishery_subset)
# - Join proportional breakdown
truth_catch_w_fleets <- temp_catch_fishery_subset %>%
  left_join(temp_catchtons_fishery_subset_prop,
            by = c("species", "agecl", "polygon", "time")) %>%
  mutate(atoutput = atoutput.x * proportion) %>%
  filter(atoutput > 0)
head(truth_catch_w_fleets)
#* here, atoutput.x is the original value in truth$catch and atoutput.y the original value in truth$catchtons used
#* to compute (fleet) proportion and atoutput is the the original value in truth$catch with proportion applied

# Step 3: get this dataset into a format expected by create_fishery_subset
head(truth$catch)
truth_catch_w_fleets <- truth_catch_w_fleets %>%
  select(species, agecl, polygon, time, fleet, atoutput) # Keep only relevant columns

# Step 4: View the result and purge temp data
head(truth_catch_w_fleets)
#* here, atoutput.x is the original value in truth$catch and atoutput is that value split by the fleets
remove(temp_catchtons_fishery_subset,temp_catchtons_fishery_subset_prop,temp_catch_fishery_subset)

#* -----
#* ----- creating fishery observations for the MexCal_S1 fishery
#* -----

# (a) A data.frame of numbers at age

# You can get this from run_truth() via catch output (truth$catch) (see above)
# This data has been adjusted to account for fleets (truth_catch_w_fleets) (see above)
head(truth_catch_w_fleets)
truth_catch_w_fleets_MexCalS1 <- truth_catch_w_fleets %>%
  filter(fleet %in% fleets_index_MexCal) %>%
  mutate(atoutput = atoutput * 0.2)

#* (b) The timing of the survey

# get timing info from Atlantis run file
load_runpar <- load_runprm(dir.outputs, in.run)
# frequency of Atlantis catch output throughout the year (e.g., a result of 5 means atlantis reports output 5 times a year)
tsf_stepperyr <- if(load_runpar$outputstepunit=="days") 365 / load_runpar$toutfinc
# NOTE, this can differ from the ecological output: load_runpar$toutfinc VS load_runpar$toutinc

# HAP:
# looking at the OG dat file, MexCal_S1 fishery samples len and age data during month 4, however
# the stock assessment says that this (sub) fleet represents earlier in the year: catch.season.1
# In a call, RW mentioned that the SS models runs JUL to JUN across calendar year, not JAN - DEC
# this may have something to do with this difference.
# Thus, here, I am going to stick with stock assessment split and say this is sampling in month 10
ts_range_MexCalS1 <- c(274:304) # days of the year associated to month 10

# # determine time of year of the output reporting that is associated to the survey sample range
# ts_sampinyr <- seq(0,tsf_stepperyr)[which((seq(0,tsf_stepperyr) * load_runpar$toutinc) %in% ts_range_MexCal)]
# # define vector of ts indices of sampling
# ts_sampling_MexCal <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends
#
# HAP, at the moment catch output in atlantis is annual, so we will just sample all annual data
ts_sampling_MexCalS1 <- seq(1, ((load_runpar$tstop / 365)), by = tsf_stepperyr) # for ecological data we subtract 1 so we sample just before the simulation output ends, but do not need to do that here atm as data is annual

# (c) which fleet indices to aggregate in the output (NULL aggregates all fleets)

fleets_index_MexCal

# NOTE, NULL aggregates all fleets: fishery_subset_fleets <- NULL

#* HAP - to get create_fishery_subset to run I set fleets to NULL
#* I am not sure what fleets need to be, but it looks like a vector a fleet names
#* the issue is truth$catch, in the code for create_fishery_subset
#* https://github.com/r4atlantis/atlantisom/blob/master/R/create_fishery_subset.R
#* it looks like truth$catch needs to have a column for "fleets" ??? and right now
#* my data does not have that. Thus, I think I need to go back to the function
#* that builds the truth data and see why it is missing that info.

# (d) The species to sample in the survey (a vector)

species_ss

# (e) A vector of box numbers

# note, we have previously loaded box info: see load_boxes
# for now, we are going to select all polygons:
boxes_sampling_MexCalS1 <- c(0:(load_boxes$nbox - 1))
# HAP note,
# this is based on the example OG example scripts. These values are box IDs 0,1,...
# which does not line up with unique(truth$biomass_ages$polygon) which is 1,2,...
# Thus, if we start sub-setting by polygons, we should double check it is working correctly...

# (---) create fishery subset -- ?create_fishery_subset

catch_fishery_subset_MexCalS1 <- create_fishery_subset(dat     = truth_catch_w_fleets_MexCalS1, # numbers at age data
                                                       time    = ts_sampling_MexCalS1,
                                                       fleets  = fleets_index_MexCal,
                                                       species = species_ss,
                                                       boxes   = boxes_sampling_MexCalS1);head(catch_fishery_subset_MexCalS1)

#* -----
#* ----- creating fishery observations for the MexCal_S2 fishery
#* -----

# (a) A data.frame of numbers at age

# You can get this from run_truth() via catch output (truth$catch) (see above)
# This data has been adjusted to account for fleets (truth_catch_w_fleets) (see above)
head(truth_catch_w_fleets)
truth_catch_w_fleets_MexCalS2 <- truth_catch_w_fleets %>%
  filter(fleet %in% fleets_index_MexCal) %>%
  mutate(atoutput = atoutput * 0.8)

#* (b) The timing of the survey

# get timing info from Atlantis run file
load_runpar <- load_runprm(dir.outputs, in.run)
# frequency of Atlantis catch output throughout the year (e.g., a result of 5 means atlantis reports output 5 times a year)
tsf_stepperyr <- if(load_runpar$outputstepunit=="days") 365 / load_runpar$toutfinc
# NOTE, this can differ from the ecological output: load_runpar$toutfinc VS load_runpar$toutinc

# HAP:
# looking at the OG dat file, MexCal_S2 fishery samples len and age data during month 10, however
# the stock assessment says that this (sub) fleet represents earlier in the year: catch.season.2
# In a call, RW mentioned that the SS models runs JUL to JUN across calendar year, not JAN - DEC
# this may have something to do with this difference.
# Thus, here, I am going to stick with stock assessment split and say this is sampling in month 4
ts_range_MexCalS2 <- c(91:120) # days of the year associated to month 4

# # determine time of year of the output reporting that is associated to the survey sample range
# ts_sampinyr <- seq(0,tsf_stepperyr)[which((seq(0,tsf_stepperyr) * load_runpar$toutinc) %in% ts_range_MexCal)]
# # define vector of ts indices of sampling
# ts_sampling_MexCalS2 <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends
#
# HAP, at the moment catch output in atlantis is annual, so we will just sample all annual data
ts_sampling_MexCalS2 <- seq(1, ((load_runpar$tstop / 365)), by = tsf_stepperyr) # for ecological data we subtract 1 so we sample just before the simulation output ends, but do not need to do that here atm as data is annual

# (c) which fleet indices to aggregate in the output (NULL aggregates all fleets)

fleets_index_MexCal

# NOTE, NULL aggregates all fleets: fishery_subset_fleets <- NULL

#* HAP - to get create_fishery_subset to run I set fleets to NULL
#* I am not sure what fleets need to be, but it looks like a vector a fleet names
#* the issue is truth$catch, in the code for create_fishery_subset
#* https://github.com/r4atlantis/atlantisom/blob/master/R/create_fishery_subset.R
#* it looks like truth$catch needs to have a column for "fleets" ??? and right now
#* my data does not have that. Thus, I think I need to go back to the function
#* that builds the truth data and see why it is missing that info.

# (d) The species to sample in the survey (a vector)

species_ss

# (e) A vector of box numbers

# note, we have previously loaded box info: see load_boxes
# for now, we are going to select all polygons:
boxes_sampling_MexCalS2 <- c(0:(load_boxes$nbox - 1))
# HAP note,
# this is based on the example OG example scripts. These values are box IDs 0,1,...
# which does not line up with unique(truth$biomass_ages$polygon) which is 1,2,...
# Thus, if we start sub-setting by polygons, we should double check it is working correctly...

# (---) create fishery subset -- ?create_fishery_subset

catch_fishery_subset_MexCalS2 <- create_fishery_subset(dat     = truth_catch_w_fleets_MexCalS2, # numbers at age data
                                                       time    = ts_sampling_MexCalS2,
                                                       fleets  = fleets_index_MexCal,
                                                       species = species_ss,
                                                       boxes   = boxes_sampling_MexCalS2);head(catch_fishery_subset_MexCalS2)

#* -----
#* ----- creating fishery observations for the PNW fishery
#* -----

# (a) A data.frame of numbers at age

# You can get this from run_truth() via catch output (truth$catch) (see above)
# This data has been adjusted to account for fleets (truth_catch_w_fleets) (see above)

#* (b) The timing of the survey

# get timing info from Atlantis run file
load_runpar <- load_runprm(dir.outputs, in.run)
# frequency of Atlantis catch output throughout the year (e.g., a result of 5 means atlantis reports output 5 times a year)
tsf_stepperyr <- if(load_runpar$outputstepunit=="days") 365 / load_runpar$toutfinc
# NOTE, this can differ from the ecological output: load_runpar$toutfinc VS load_runpar$toutinc

# looking at the OG dat file, PNW fishery samples len and age data during month 4.
ts_range_PNW <- c(91:120) # days of the year associated to month 4

# # determine time of year of the output reporting that is associated to the survey sample range
# ts_sampinyr <- seq(0,tsf_stepperyr)[which((seq(0,tsf_stepperyr) * load_runpar$toutinc) %in% ts_range_PNW)]
# # define vector of ts indices of sampling
# tsf_sampling_PNW <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends
#
# HAP, at the moment catch output in atlantis is annual, so we will just sample all annual data
tsf_sampling_PNW <- seq(1, ((load_runpar$tstop / 365)), by = tsf_stepperyr) # for ecological data we subtract 1 so we sample just before the simulation output ends, but do not need to do that here atm as data is annual

# (c) which fleet indices to aggregate in the output (NULL aggregates all fleets)

fleets_index_PacNW

# NOTE, NULL aggregates all fleets: fishery_subset_fleets <- NULL

#* HAP - to get create_fishery_subset to run I set fleets to NULL
#* I am not sure what fleets need to be, but it looks like a vector a fleet names
#* the issue is truth$catch, in the code for create_fishery_subset
#* https://github.com/r4atlantis/atlantisom/blob/master/R/create_fishery_subset.R
#* it looks like truth$catch needs to have a column for "fleets" ??? and right now
#* my data does not have that. Thus, I think I need to go back to the function
#* that builds the truth data and see why it is missing that info.

# (d) The species to sample in the survey (a vector)

species_ss

# (e) A vector of box numbers

# note, we have previously loaded box info: see load_boxes
# for now, we are going to select all polygons:
boxes_sampling_PNW <- c(0:(load_boxes$nbox - 1))
# HAP note,
# this is based on the example OG example scripts. These values are box IDs 0,1,...
# which does not line up with unique(truth$biomass_ages$polygon) which is 1,2,...
# Thus, if we start sub-setting by polygons, we should double check it is working correctly...

# (---) create fishery subset -- ?create_fishery_subset
catch_fishery_subset_PNW <- create_fishery_subset(dat     = truth_catch_w_fleets, # numbers at age data
                                                  time    = tsf_sampling_PNW,
                                                  fleets  = fleets_index_PacNW,
                                                  species = species_ss,
                                                  boxes   = boxes_sampling_PNW);head(catch_fishery_subset_PNW)

#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#* PAUSE
#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#*
#* Lets check the results from using create_fishery_subset by comparing the results (nums)
#* to the catch trends from catchtons (tonnes)
#*

#* ----- fishery observations for the MexCalS1 fishery

# isolate total catch from fleet (tonnes)
forplot_catch_tons_ss_MexCalS1_agg <- aggregate(total_atoutput ~ species + time, data = catch_tons_ss_MexCal_agg[which(catch_tons_ss_MexCal_agg$season == 1),],
                                                sum);head(forplot_catch_tons_ss_MexCalS1_agg)
forplot_catch_tons_ss_MexCalS1_agg$species <- "Pacific sardine (tonnes)"
# aggregate data from create_fishery_subset for fleet (nums)
forplot_catch_fishery_subset_MexCalS1 <- aggregate(atoutput ~ species + time, data = catch_fishery_subset_MexCalS1, sum);head(forplot_catch_fishery_subset_MexCalS1) # units here is nums, not kg or tonnes
forplot_catch_fishery_subset_MexCalS1$species <- "Pacific sardine (nums)"
# Viewing the data
ggplot() +
  geom_line(data=forplot_catch_tons_ss_MexCalS1_agg, aes(x=time,y=total_atoutput, color="MexCalS1 catch"), alpha = 10/10) +
  geom_line(data=forplot_catch_fishery_subset_MexCalS1, aes(x=time,y=atoutput, color="MexCalS1 create_fishery_subset"),alpha = 10/10, linetype = "dashed") +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="")+
  facet_wrap(~species, scales="free")

#* ----- fishery observations for the MexCalS2 fishery

# isolate total catch from fleet (tonnes)
forplot_catch_tons_ss_MexCalS2_agg <- aggregate(total_atoutput ~ species + time, data = catch_tons_ss_MexCal_agg[which(catch_tons_ss_MexCal_agg$season == 2),],
                                                sum);head(forplot_catch_tons_ss_MexCalS2_agg)
forplot_catch_tons_ss_MexCalS2_agg$species <- "Pacific sardine (tonnes)"
# aggregate data from create_fishery_subset for fleet (nums)
forplot_catch_fishery_subset_MexCalS2 <- aggregate(atoutput ~ species + time, data = catch_fishery_subset_MexCalS2, sum);head(forplot_catch_fishery_subset_MexCalS2) # units here is nums, not kg or tonnes
forplot_catch_fishery_subset_MexCalS2$species <- "Pacific sardine (nums)"
# Viewing the data
ggplot() +
  geom_line(data=forplot_catch_tons_ss_MexCalS2_agg, aes(x=time,y=total_atoutput, color="MexCalS2 catch"), alpha = 10/10) +
  geom_line(data=forplot_catch_fishery_subset_MexCalS2, aes(x=time,y=atoutput, color="MexCalS2 create_fishery_subset"),alpha = 10/10, linetype = "dashed") +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="")+
  facet_wrap(~species, scales="free")

#* ----- fishery observations for the PNW fishery

# isolate total catch from fleet (tonnes)
forplot_catch_tons_ss_PacNW_agg <- aggregate(total_atoutput ~ species + time, data = catch_tons_ss_PacNW_agg, sum);head(forplot_catch_tons_ss_PacNW_agg)
forplot_catch_tons_ss_PacNW_agg$species <- "Pacific sardine (tonnes)"
# aggregate data from create_fishery_subset for fleet (nums)
forplot_catch_fishery_subset_PNW <- aggregate(atoutput ~ species + time, data = catch_fishery_subset_PNW, sum);head(forplot_catch_fishery_subset_PNW) # units here is nums, not kg or tonnes
forplot_catch_fishery_subset_PNW$species <- "Pacific sardine (nums)"
# Viewing the data
ggplot() +
  geom_line(data=forplot_catch_tons_ss_PacNW_agg, aes(x=time,y=total_atoutput, color="PacNW catch"), alpha = 10/10) +
  geom_line(data=forplot_catch_fishery_subset_PNW, aes(x=time,y=atoutput, color="PacNW create_fishery_subset"),alpha = 10/10, linetype = "dashed") +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="")+
  facet_wrap(~species, scales="free")

#* --------------
#* --------------
#* END PAUSE
#* --------------
#* --------------

#* -------------- (C) generate size composition data from fishery information
#*
#* We can use calc_age2length to calculate length composition in 1 cm bins from Atlantis output (st)age data.
#* Uses numbers at age, plus weight at age, and weight-length relationships to generate size comps.
#*
#* needed for function:
#* structn  - A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* resn     - A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.
#* nums     - A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.
#* biolprm  - A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.
#* fgs      - A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".
#* CVlenage - The variability in length at age (same for all species) The default value is 0.1.
#*
#* see ?calc_age2length to get more info.

#* -----
#* ----- creating fishery observations for the MexCalS1 fishery
#* -----

# (a) A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

#* FIRST,
#* use aggregateDensityData to aggregate true structn per survey or fishery subset design --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, tsf_sampling_FLEET
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes

#* (1) A data.frame of density data (structn, resn) from reading in the Atlantis output
head(truth$structn)

#* (2) A single value or a vector of time steps for a survey

#* we are NOT going to pass tsf_sampling_MexCalS1 here!!
#* structn and resn data are reported on the ecological output frequency (load_runpar$toutinc),
#* and recall this freq can be different from the fishery output freq (load_runpar$toutfinc).
#* Thus, here we need to define a sampling time vector for this fishery based on the
#* ecological output increments.

#* Since the catch data is currently annual, in order to get the target function to produce
#* ouptput for each entry (ie year) in the numbers data, I had to define a ts vector based
#* on the ecological ts output units in which the values correspond to the annual outputs
ts_sampling_MexCalS1 <- c(1:(load_runpar$tstop / load_runpar$outputstep))[which((c(1:(load_runpar$tstop / load_runpar$outputstep)) * load_runpar$toutinc / 365) %% 1 == 0)]

# # We have already loaded the run file and defined frequency of Atlantis ecological output throughout the year
# load_runpar
# ts_stepperyr
#
# # looking at the OG dat file, MexCalS1 fishery samples len and age data during month 4.
# ts_range_MexCalS1 <- c(91:120) # days of the year associated to month 4
# ts_range_MexCalS1 <- c(146) # HAP!! The current output freq does not report in this range...
#
# # determine time of year of the output reporting that is associated to the survey sample range
# ts_sampinyr <- seq(0,ts_stepperyr)[which((seq(0,ts_stepperyr) * load_runpar$toutinc) %in% ts_range_MexCalS1)]
# # define vector of ts indices of sampling
# ts_sampling_MexCalS1 <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends

#* (3) A vector of character values, where each entry is a species name that is sampled in the survey
species_ss

#* (4) A vector of polygons where the survey samples within each specified polygon
boxes_sampling_MexCalS1

#* (***) run function
catch_structn_aggr_MexCalS1 <- aggregateDensityData(dat     = truth$structn,
                                                    time    = ts_sampling_MexCalS1 ,
                                                    species = species_ss,
                                                    boxes   = boxes_sampling_MexCalS1);head(catch_structn_aggr_MexCalS1)

#* THEN,
#* use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

#* (1) A data.frame of numbers at age (output from create_survey())
head(catch_structn_aggr_MexCalS1)

#* (2) Efficiency for each species
effN_MexCalS1 <- data.frame(species = species_ss,
                            effN    = rep(200, # HAP, was testing other values: 50, 200, 1000
                                          length(species_ss)))

#* (***) run function, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_structn_sample_MexCalS1 <- sample_fish(dat    = catch_structn_aggr_MexCalS1,
                                             effN   = effN_MexCalS1,
                                             sample = FALSE);head(catch_structn_sample_MexCalS1)

# (b) A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

#* FIRST,
#* use aggregateDensityData to aggregate true resn per survey or fishery subset design --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, tsf_sampling_FLEET
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes

#* (1) A data.frame of density data (structn, resn) from reading in the Atlantis output
head(truth$resn)

#* (2) A single value or a vector of time steps for a survey
ts_sampling_MexCalS1

#* (3) A vector of character values, where each entry is a species name that is sampled in the survey
species_ss

#* (4) A vector of polygons where the survey samples within each specified polygon
boxes_sampling_MexCalS1

#* (***) run function
catch_resn_aggr_MexCalS1 <- aggregateDensityData(dat     = truth$resn,
                                                 time    = ts_sampling_MexCalS1 ,
                                                 species = species_ss,
                                                 boxes   = boxes_sampling_MexCalS1);head(catch_resn_aggr_MexCalS1)

#* THEN,
#* use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

#* (1) A data.frame of numbers at age (output from create_survey())
head(catch_resn_aggr_MexCalS1)

#* (2) Efficiency for each species
effN_MexCalS1

#* (***) run function, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_resn_sample_MexCalS1 <- sample_fish(dat    = catch_resn_aggr_MexCalS1,
                                          effN   = effN_MexCalS1,
                                          sample = FALSE);head(catch_resn_sample_MexCalS1)

# (c) A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.

# we can do this by using sample_fish() to sample numbers-at-age (catch_fishery_subset, see above) and create composition data --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (catch_fishery_subset, see above)
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
# NOTE!! you ARE sampling, thus do NOT set sample (default is TRUE)

# Here, effN has already been defined (see above, catch_effN), so can go straight into sampling data from create_fishery_subset()
catch_age_comp_data_MexCalS1 <- sample_fish(dat  = catch_fishery_subset_MexCalS1,
                                            effN = effN_MexCalS1); head(catch_age_comp_data_MexCalS1)

# (d) A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.
# stored in truth, truth$biolprm

# (e) A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".
# stored in truth, truth$fgs

# (f) The variability in length at age (same for all species) The default value is 0.1.
# recall, this is stored in ss_var_lenatage

# # (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
ss_maxbin

# (---) Length sample with user specified max length bin

#*!!! Before moving forward, we are going to convert the time in one of our input data sets.
#* when we apply the calc_age2length function, it expects time in all datasets to be of the same units.
#* So, we could convert the time in the nums data:
# catch_age_comp_data_MexCalS1$time <- catch_age_comp_data_MexCalS1$time / load_runpar$toutinc * 365
#* or we could convert the time in the nitrogen datasets
catch_structn_sample_MexCalS1$time <- catch_structn_sample_MexCalS1$time * load_runpar$toutinc / 365
catch_resn_sample_MexCalS1$time    <- catch_resn_sample_MexCalS1$time * load_runpar$toutinc / 365
# Here, I opted to do the later

catch_age2length_MexCalS1 <- calc_age2length(structn  = catch_structn_sample_MexCalS1,
                                             resn     = catch_resn_sample_MexCalS1,
                                             nums     = catch_age_comp_data_MexCalS1,
                                             biolprm  = truth$biolprm,
                                             fgs      = truth$fgs,
                                             maxbin   = ss_maxbin, # HAP, should this be the same as what we are using when passing survey data??
                                             CVlenage = ss_var_lenatage);head(catch_age2length_MexCalS1)
#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(catch_age2length_MexCalS1$mulen)
head(catch_age2length_MexCalS1$muweight)
head(catch_age2length_MexCalS1$natlength) # unique(catch_age2length_MexCalS1$natlength$lower.bins);length(unique(catch_age2length_MexCalS1$natlength$lower.bins))

# # NOTE: if sampling more species than the data files may be quite large, making it ideal to save
# # and load RDS files rather than re-executing functions.
# saveRDS(catch_age2length, file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))
# catch_age2length <- readRDS(file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))

#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#* PAUSE
#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#*
#* overview some of the results from calc_age2length()

#* Plot true catch at age:
catageplot <-
  ggplot(catch_age_comp_data_MexCalS1, aes(x=agecl, y=atoutput)) +
  geom_point() +
  theme_tufte() +
  labs(subtitle = paste("", catch_age_comp_data_MexCalS1$species));catageplot
# you can isolate by time
catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 1, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 2, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 3, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 4, scales="free")

#* Histogram of length of catch
lfplot <- ggplot(catch_age2length_MexCalS1$natlength, aes(upper.bins)) +
  geom_bar(aes(weight = atoutput)) +
  theme_tufte() +
  labs(subtitle = paste("",catch_age2length_MexCalS1$species));lfplot
# you can isolate by time
lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 1, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 2, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 3, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 4, scales="free_y")

#* Fishery weight at (st)age:
wageplot <- ggplot(catch_age2length_MexCalS1$muweight, aes(agecl, atoutput)) +
  geom_point(aes(colour = time)) +
  theme_tufte() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limits=factor(c(1:10))) +
  xlab("age class") +
  ylab("average individual weight (g)") +
  labs(subtitle = "") +
  facet_wrap(c("species"), scales="free_y");wageplot

#* Change in wt at (st)age in the fishery over the last 10 years
wtage_annsurv <- catch_age2length_MexCalS1$muweight %>%
  filter(time %in% c(77:87))
# reverse to show agecl time series of wt
wageplot <- ggplot(wtage_annsurv, aes(time, atoutput)) +
  geom_line(aes(colour = factor(agecl))) +
  theme_tufte() +
  theme(legend.position = "bottom") +
  xlab("time (year)") +
  ylab("average individual weight (g)") +
  labs(subtitle = paste0("last 10 years")) +
  facet_wrap(c("species"), scales="free_y");wageplot

#* --------------
#* --------------
#* END PAUSE
#* --------------
#* --------------

#* -----
#* ----- creating fishery observations for the MexCalS2 fishery
#* -----

# (a) A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

#* FIRST,
#* use aggregateDensityData to aggregate true structn per survey or fishery subset design --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, tsf_sampling_FLEET
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes

#* (1) A data.frame of density data (structn, resn) from reading in the Atlantis output
head(truth$structn)

#* (2) A single value or a vector of time steps for a survey

#* we are NOT going to pass tsf_sampling_MexCalS2 here!!
#* structn and resn data are reported on the ecological output frequency (load_runpar$toutinc),
#* and recall this freq can be different from the fishery output freq (load_runpar$toutfinc).
#* Thus, here we need to define a sampling time vector for this fishery based on the
#* ecological output increments.

#* Since the catch data is currently annual, in order to get the target function to produce
#* ouptput for each entry (ie year) in the numbers data, I had to define a ts vector based
#* on the ecological ts output units in which the values correspond to the annual outputs
ts_sampling_MexCalS2 <- c(1:(load_runpar$tstop / load_runpar$outputstep))[which((c(1:(load_runpar$tstop / load_runpar$outputstep)) * load_runpar$toutinc / 365) %% 1 == 0)]

# # We have already loaded the run file and defined frequency of Atlantis ecological output throughout the year
# load_runpar
# ts_stepperyr
#
# # looking at the OG dat file, MexCalS2 fishery samples len and age data during month 4.
# ts_range_MexCalS2 <- c(91:120) # days of the year associated to month 4
# ts_range_MexCalS2 <- c(146) # HAP!! The current output freq does not report in this range...
#
# # determine time of year of the output reporting that is associated to the survey sample range
# ts_sampinyr <- seq(0,ts_stepperyr)[which((seq(0,ts_stepperyr) * load_runpar$toutinc) %in% ts_range_MexCalS2)]
# # define vector of ts indices of sampling
# ts_sampling_MexCalS2 <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends

#* (3) A vector of character values, where each entry is a species name that is sampled in the survey
species_ss

#* (4) A vector of polygons where the survey samples within each specified polygon
boxes_sampling_MexCalS2

#* (***) run function
catch_structn_aggr_MexCalS2 <- aggregateDensityData(dat     = truth$structn,
                                                    time    = ts_sampling_MexCalS2 ,
                                                    species = species_ss,
                                                    boxes   = boxes_sampling_MexCalS2);head(catch_structn_aggr_MexCalS2)

#* THEN,
#* use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

#* (1) A data.frame of numbers at age (output from create_survey())
head(catch_structn_aggr_MexCalS2)

#* (2) Efficiency for each species
effN_MexCalS2 <- data.frame(species = species_ss,
                            effN    = rep(200, # HAP, was testing other values: 50, 200, 1000
                                          length(species_ss)))

#* (***) run function, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_structn_sample_MexCalS2 <- sample_fish(dat    = catch_structn_aggr_MexCalS2,
                                             effN   = effN_MexCalS2,
                                             sample = FALSE);head(catch_structn_sample_MexCalS2)

# (b) A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

#* FIRST,
#* use aggregateDensityData to aggregate true resn per survey or fishery subset design --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, tsf_sampling_FLEET
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes

#* (1) A data.frame of density data (structn, resn) from reading in the Atlantis output
head(truth$resn)

#* (2) A single value or a vector of time steps for a survey
ts_sampling_MexCalS2

#* (3) A vector of character values, where each entry is a species name that is sampled in the survey
species_ss

#* (4) A vector of polygons where the survey samples within each specified polygon
boxes_sampling_MexCalS2

#* (***) run function
catch_resn_aggr_MexCalS2 <- aggregateDensityData(dat     = truth$resn,
                                                 time    = ts_sampling_MexCalS2 ,
                                                 species = species_ss,
                                                 boxes   = boxes_sampling_MexCalS2);head(catch_resn_aggr_MexCalS2)

#* THEN,
#* use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

#* (1) A data.frame of numbers at age (output from create_survey())
head(catch_resn_aggr_MexCalS2)

#* (2) Efficiency for each species
effN_MexCalS2

#* (***) run function, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_resn_sample_MexCalS2 <- sample_fish(dat    = catch_resn_aggr_MexCalS2,
                                          effN   = effN_MexCalS2,
                                          sample = FALSE);head(catch_resn_sample_MexCalS2)

# (c) A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.

# we can do this by using sample_fish() to sample numbers-at-age (catch_fishery_subset, see above) and create composition data --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (catch_fishery_subset, see above)
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
# NOTE!! you ARE sampling, thus do NOT set sample (default is TRUE)

# Here, effN has already been defined (see above, catch_effN), so can go straight into sampling data from create_fishery_subset()
catch_age_comp_data_MexCalS2 <- sample_fish(dat  = catch_fishery_subset_MexCalS2,
                                            effN = effN_MexCalS2); head(catch_age_comp_data_MexCalS2)

# (d) A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.
# stored in truth, truth$biolprm

# (e) A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".
# stored in truth, truth$fgs

# (f) The variability in length at age (same for all species) The default value is 0.1.
# recall, this is stored in ss_var_lenatage

# # (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
ss_maxbin

# (---) Length sample with user specified max length bin

#*!!! Before moving forward, we are going to convert the time in one of our input data sets.
#* when we apply the calc_age2length function, it expects time in all datasets to be of the same units.
#* So, we could convert the time in the nums data:
# catch_age_comp_data_MexCalS2$time <- catch_age_comp_data_MexCalS2$time / load_runpar$toutinc * 365
#* or we could convert the time in the nitrogen datasets
catch_structn_sample_MexCalS2$time <- catch_structn_sample_MexCalS2$time * load_runpar$toutinc / 365
catch_resn_sample_MexCalS2$time    <- catch_resn_sample_MexCalS2$time * load_runpar$toutinc / 365
# Here, I opted to do the later

catch_age2length_MexCalS2 <- calc_age2length(structn  = catch_structn_sample_MexCalS2,
                                             resn     = catch_resn_sample_MexCalS2,
                                             nums     = catch_age_comp_data_MexCalS2,
                                             biolprm  = truth$biolprm,
                                             fgs      = truth$fgs,
                                             maxbin   = ss_maxbin, # HAP, should this be the same as what we are using when passing survey data??
                                             CVlenage = ss_var_lenatage);head(catch_age2length_MexCalS2)
#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(catch_age2length_MexCalS2$mulen)
head(catch_age2length_MexCalS2$muweight)
head(catch_age2length_MexCalS2$natlength) # unique(catch_age2length_MexCalS2$natlength$lower.bins);length(unique(catch_age2length_MexCalS2$natlength$lower.bins))

# # NOTE: if sampling more species than the data files may be quite large, making it ideal to save
# # and load RDS files rather than re-executing functions.
# saveRDS(catch_age2length, file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))
# catch_age2length <- readRDS(file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))

#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#* PAUSE
#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#*
#* overview some of the results from calc_age2length()

#* Plot true catch at age:
catageplot <-
  ggplot(catch_age_comp_data_MexCalS2, aes(x=agecl, y=atoutput)) +
  geom_point() +
  theme_tufte() +
  labs(subtitle = paste("", catch_age_comp_data_MexCalS2$species));catageplot
# you can isolate by time
catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 1, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 2, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 3, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 4, scales="free")

#* Histogram of length of catch
lfplot <- ggplot(catch_age2length_MexCalS2$natlength, aes(upper.bins)) +
  geom_bar(aes(weight = atoutput)) +
  theme_tufte() +
  labs(subtitle = paste("",catch_age2length_MexCalS2$species));lfplot
# you can isolate by time
lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 1, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 2, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 3, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 4, scales="free_y")

#* Fishery weight at (st)age:
wageplot <- ggplot(catch_age2length_MexCalS2$muweight, aes(agecl, atoutput)) +
  geom_point(aes(colour = time)) +
  theme_tufte() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limits=factor(c(1:10))) +
  xlab("age class") +
  ylab("average individual weight (g)") +
  labs(subtitle = "") +
  facet_wrap(c("species"), scales="free_y");wageplot

#* Change in wt at (st)age in the fishery over the last 10 years
wtage_annsurv <- catch_age2length_MexCalS2$muweight %>%
  filter(time %in% c(77:87))
# reverse to show agecl time series of wt
wageplot <- ggplot(wtage_annsurv, aes(time, atoutput)) +
  geom_line(aes(colour = factor(agecl))) +
  theme_tufte() +
  theme(legend.position = "bottom") +
  xlab("time (year)") +
  ylab("average individual weight (g)") +
  labs(subtitle = paste0("last 10 years")) +
  facet_wrap(c("species"), scales="free_y");wageplot

#* --------------
#* --------------
#* END PAUSE
#* --------------
#* --------------

#* -----
#* ----- creating fishery observations for the PNW fishery
#* -----

# (a) A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

#* FIRST,
#* use aggregateDensityData to aggregate true structn per survey or fishery subset design --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, tsf_sampling_FLEET
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes

#* (1) A data.frame of density data (structn, resn) from reading in the Atlantis output
head(truth$structn)

#* (2) A single value or a vector of time steps for a survey

#* we are NOT going to pass tsf_sampling_PNW here!!
#* structn and resn data are reported on the ecological output frequency (load_runpar$toutinc),
#* and recall this freq can be different from the fishery output freq (load_runpar$toutfinc).
#* Thus, here we need to define a sampling time vector for this fishery based on the
#* ecological output increments.

#* Since the catch data is currently annual, in order to get the target function to produce
#* ouptput for each entry (ie year) in the numbers data, I had to define a ts vector based
#* on the ecological ts output units in which the values correspond to the annual outputs
ts_sampling_PNW <- c(1:(load_runpar$tstop / load_runpar$outputstep))[which((c(1:(load_runpar$tstop / load_runpar$outputstep)) * load_runpar$toutinc / 365) %% 1 == 0)]

# # We have already loaded the run file and defined frequency of Atlantis ecological output throughout the year
# load_runpar
# ts_stepperyr
#
# # looking at the OG dat file, PNW fishery samples len and age data during month 4.
# ts_range_PNW <- c(91:120) # days of the year associated to month 4
# ts_range_PNW <- c(146) # HAP!! The current output freq does not report in this range...
#
# # determine time of year of the output reporting that is associated to the survey sample range
# ts_sampinyr <- seq(0,ts_stepperyr)[which((seq(0,ts_stepperyr) * load_runpar$toutinc) %in% ts_range_PNW)]
# # define vector of ts indices of sampling
# ts_sampling_PNW <- seq(ts_sampinyr, ((load_runpar$tstop / load_runpar$outputstep) - 1), by = ts_stepperyr) # subract 1 so we sample just before the simulation output ends

#* (3) A vector of character values, where each entry is a species name that is sampled in the survey
species_ss

#* (4) A vector of polygons where the survey samples within each specified polygon
boxes_sampling_PNW

#* (***) run function
catch_structn_aggr_PNW <- aggregateDensityData(dat     = truth$structn,
                                               time    = ts_sampling_PNW ,
                                               species = species_ss,
                                               boxes   = boxes_sampling_PNW);head(catch_structn_aggr_PNW)

#* THEN,
#* use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

#* (1) A data.frame of numbers at age (output from create_survey())
head(catch_structn_aggr_PNW)

#* (2) Efficiency for each species
effN_PNW <- data.frame(species = species_ss,
                       effN    = rep(200, # HAP, was testing other values: 50, 200, 1000
                                     length(species_ss)))

#* (***) run function, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_structn_sample_PNW <- sample_fish(dat    = catch_structn_aggr_PNW,
                                        effN   = effN_PNW,
                                        sample = FALSE);head(catch_structn_sample_PNW)

# (b) A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

#* FIRST,
#* use aggregateDensityData to aggregate true resn per survey or fishery subset design --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, tsf_sampling_FLEET
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes

#* (1) A data.frame of density data (structn, resn) from reading in the Atlantis output
head(truth$resn)

#* (2) A single value or a vector of time steps for a survey
ts_sampling_PNW

#* (3) A vector of character values, where each entry is a species name that is sampled in the survey
species_ss

#* (4) A vector of polygons where the survey samples within each specified polygon
boxes_sampling_PNW

#* (***) run function
catch_resn_aggr_PNW <- aggregateDensityData(dat     = truth$resn,
                                            time    = ts_sampling_PNW ,
                                            species = species_ss,
                                            boxes   = boxes_sampling_PNW);head(catch_resn_aggr_PNW)

#* THEN,
#* use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

#* (1) A data.frame of numbers at age (output from create_survey())
head(catch_resn_aggr_PNW)

#* (2) Efficiency for each species
effN_PNW

#* (***) run function, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_resn_sample_PNW <- sample_fish(dat    = catch_resn_aggr_PNW,
                                     effN   = effN_PNW,
                                     sample = FALSE);head(catch_resn_sample_PNW)

# (c) A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.

# we can do this by using sample_fish() to sample numbers-at-age (catch_fishery_subset, see above) and create composition data --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (catch_fishery_subset, see above)
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
# NOTE!! you ARE sampling, thus do NOT set sample (default is TRUE)

# Here, effN has already been defined (see above, catch_effN), so can go straight into sampling data from create_fishery_subset()
catch_age_comp_data_PNW <- sample_fish(dat  = catch_fishery_subset_PNW,
                                       effN = effN_PNW); head(catch_age_comp_data_PNW)

# (d) A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.
# stored in truth, truth$biolprm

# (e) A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".
# stored in truth, truth$fgs

# (f) The variability in length at age (same for all species) The default value is 0.1.
# recall, this is stored in ss_var_lenatage

# # (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
ss_maxbin

# (---) Length sample with user specified max length bin

#*!!! Before moving forward, we are going to convert the time in one of our input data sets.
#* when we apply the calc_age2length function, it expects time in all datasets to be of the same units.
#* So, we could convert the time in the nums data:
# catch_age_comp_data_PNW$time <- catch_age_comp_data_PNW$time / load_runpar$toutinc * 365
#* or we could convert the time in the nitrogen datasets
catch_structn_sample_PNW$time <- catch_structn_sample_PNW$time * load_runpar$toutinc / 365
catch_resn_sample_PNW$time    <- catch_resn_sample_PNW$time * load_runpar$toutinc / 365
# Here, I opted to do the later

catch_age2length_PNW <- calc_age2length(structn  = catch_structn_sample_PNW,
                                        resn     = catch_resn_sample_PNW,
                                        nums     = catch_age_comp_data_PNW,
                                        biolprm  = truth$biolprm,
                                        fgs      = truth$fgs,
                                        maxbin   = ss_maxbin, # HAP, should this be the same as what we are using when passing survey data??
                                        CVlenage = ss_var_lenatage);head(catch_age2length_PNW)
#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(catch_age2length_PNW$mulen)
head(catch_age2length_PNW$muweight)
head(catch_age2length_PNW$natlength) # unique(catch_age2length_PNW$natlength$lower.bins);length(unique(catch_age2length_PNW$natlength$lower.bins))

# # NOTE: if sampling more species than the data files may be quite large, making it ideal to save
# # and load RDS files rather than re-executing functions.
# saveRDS(catch_age2length, file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))
# catch_age2length <- readRDS(file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))

#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#* PAUSE
#* ----------------------------------------------------------------------
#* ----------------------------------------------------------------------
#*
#* overview some of the results from calc_age2length()

#* Plot true catch at age:
catageplot <-
  ggplot(catch_age_comp_data_PNW, aes(x=agecl, y=atoutput)) +
  geom_point() +
  theme_tufte() +
  labs(subtitle = paste("", catch_age_comp_data_PNW$species));catageplot
# you can isolate by time
catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 1, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 2, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 3, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 4, scales="free")


#* Histogram of length of catch
lfplot <- ggplot(catch_age2length_PNW$natlength, aes(upper.bins)) +
  geom_bar(aes(weight = atoutput)) +
  theme_tufte() +
  labs(subtitle = paste("",catch_age2length_PNW$species));lfplot
# you can isolate by time
lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 1, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 2, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 3, scales="free_y")
# lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 4, scales="free_y")

#* Fishery weight at (st)age:
wageplot <- ggplot(catch_age2length_PNW$muweight, aes(agecl, atoutput)) +
  geom_point(aes(colour = time)) +
  theme_tufte() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limits=factor(c(1:10))) +
  xlab("age class") +
  ylab("average individual weight (g)") +
  labs(subtitle = "") +
  facet_wrap(c("species"), scales="free_y");wageplot

#* Change in wt at (st)age in the fishery over the last 10 years
wtage_annsurv <- catch_age2length_PNW$muweight %>%
  filter(time %in% c(77:87))
# reverse to show agecl time series of wt
wageplot <- ggplot(wtage_annsurv, aes(time, atoutput)) +
  geom_line(aes(colour = factor(agecl))) +
  theme_tufte() +
  theme(legend.position = "bottom") +
  xlab("time (year)") +
  ylab("average individual weight (g)") +
  labs(subtitle = paste0("last 10 years")) +
  facet_wrap(c("species"), scales="free_y");wageplot

#* --------------
#* --------------
#* END PAUSE
#* --------------
#* --------------

#* ----------------------------
#* ---------------------------- II. Write data for Stock Synthesis
#* ----------------------------    a) write data to the dat file
#* ----------------------------
#*
#* This will take a few sections of script:
#*         (A) using SS_readdat_3.30 to upload a .dat file into R
#*         (B) using SS_write_ts to overwrite the CPUE from Atlantis data (atlantisom) into a Stock Synthesis 3.3 estimation model (the .dat file)
#*              - This preps atlantis catch data to be inserted as ss _Catch data, and atlantis CPUE data to be inserted as SS _CPUE_data data
#*         (C) using SS_write_comps to overwrite age and length composition data from from Atlantis data (atlantisom) into a Stock Synthesis 3.3 estimation model (the .dat file)
#*         (D) using SS_writedat_3.30 to save the .dat file
#*

#* -------------- (0) preparation

# (a) libraries
#* To read in the Stock Synthesis files, first ensure you have the latest
#* version of r4ss. You will need to ensure the relevant Stock Synthesis files
#* are stored in the inst/extdata/ folder, with each species example in its
#* own folder.
#* HAP, I had some issues at first as SS3 was just recently updated at the time
#* of my start and r4ss had yet to catch up. But, IK reached out to Ian and
#* he guided us to here
#* https://r4ss.github.io/r4ss/
#install.packages("pak")
#pak::pkg_install("r4ss/r4ss")
library(r4ss)

# (b) directory to SS model
dir_model_SS <- "/Users/hollyperryman/Documents/CalCurrent/SS3/Sardine/SardineMSE-main/scenarioModels/start2001/constGrowthMidSteepNewSelex_OM_HAPtest/"

# (c) Name of SS data files to write data to
name_datfile <- "data.ss"

# (d) years
# the SS3 dat file for our application has calendar years, but atlantis tracks simulation years
years.calendar.simulation <- data.frame(yr_sim = c(0:(load_runpar$tstop / 365)),
                                        yr_ss = c(1932:2019))

#* -------------- (A) using SS_readdat_3.30 to upload a .dat file into R
#*
#* atlantisom provides data to SS3 by uploading a working .dat file and swapping out data
#* with the data developed herein based on Atlantis output. Thus, first, a .dat file needs
#* to be upload for editing.
#* This is done through r4ss via SS_readdat_3.30 (Read Stock Synthesis (version 3.30) data file into list object in R)

SS.datfile <- r4ss::SS_readdat_3.30(paste0(dir_model_SS, "data_OG.ss"))
# HAP, I created a copy of the OG dat file as to not overwrite and lose it. I am reading that OG file in here.

#* -------------- (B) using SS_write_ts to overwrite the CPUE from Atlantis data (atlantisom) into a Stock Synthesis 3.3 estimation model (the .dat file)
#*
#* HAP notes from debugging this script:
#* - It does not appear that SS_write_ts replaces the start or end year in the .dat file. You may need to do this manually
#* if you are providing a time series that differs from the original .dat file that you are overwriting.
#* - Kinda obvious but make sure all your time series match up in length.
#* - when I was debugging my CVs were set to the perfect example (0) and that made SS3 crash. Once I set them to 0.01 it was fine.
#*
#* This calls for using the function SS_write_ts() to overwrite catch and CPUE data in the uploaded .dat data
#* with data developed based on Atlantis data.
#*
#* This function needs the following:
#* ss_data_list   - the list of the SS .dat file that #'comes from SS_readdat (data from r4ss::SS_readdat_3.30)
#* ts_data        - (not listed in R documentation)
#* CVs            - a vector of numbers representing the CV around the index
#* data_years     - a vector, where each item is the length of the index
#* sampling_month - a list, where each item is a vector representig the month of sampling for each index
#* units          - a vector where each entry must be one of "numbers" or "biomass"
#* data_type      - a vector with length = length(ts_data), each entry must be either "CPUE" or "catch"
#* fleets         - a list of vectors, in each vector each entry is the fleet number
#*
#* see ?SS_write_ts to get more info.

#* (a) the list of the SS .dat file that #'comes from SS_readdat

# This was uploaded above, SS.datfile

#* (b) - (h):
#* each item in these lists and vectors corresponds to individual fleets.
#*

#*
#* Fleet 1 - MexCal_S1
#*
# (d) - (e), years and season vectors (must be the same length)
# years of data for SS model
flt1.years <- c(2001:2019) # atlantis data is in simulation years (0,1,2..) but this allows you to insert years (2001,2002,...) into the dat file
# seasons of data for SS model
flt1.seasons <- c(1,2)
# define season vector for function
flt1.ts.season <- rep(flt1.seasons, length(flt1.years))
# define year vector based on season vector
flt1.ts.yrs <- rep(flt1.years, each = length(flt1.seasons))
# (b) time series data vector corresponding to fleet (must be the same length as time:season vectors)
# this is a fishery so we are loading catch data
# Note, the OG script grabs data from load_catch_txt
# but since we need to subset by fleet (and season) we can use the data saved in truth$catchtons
# filter out appropriate catch data
flt1.data <- catch_tons_ss_MexCal_agg  %>%
  filter((time) %in% tail(unique(truth$catchtons$time),length(flt1.years)) # selecting the final simulation years
  );head(flt1.data)
# Select season 1 data
flt1.data <- flt1.data %>%
  # Modify the 'total_atoutput' column based on 'season'
  mutate(
    total_atoutput = ifelse(season == 1, total_atoutput, 0),
  );head(flt1.data)
# define ts data  for function
flt1.ts.data <- flt1.data$total_atoutput
# (c) CV
flt1.ts.cv    <- 0.05 # from RWs dat file
# (f) units, "numbers" or "biomass"
flt1.ts.units <- "biomass"
# (g) data type, "CPUE" or "catch"
flt1.ts.type  <- "catch"
# (h) fleet number
flt1.num   <- 1

#*
#* Fleet 2 - MexCal_S2
#*
# (d) - (e), years and season vectors (must be the same length)
# years of data for SS model
flt2.years <- c(2001:2019) # atlantis data is in simulation years (0,1,2..) but this allows you to insert years (2001,2002,...) into the dat file
# seasons of data for SS model
flt2.seasons <- c(1,2)
# define season vector for function
flt2.ts.season <- rep(flt2.seasons, length(flt2.years))
# define year vector based on season vector
flt2.ts.yrs <- rep(flt2.years, each = length(flt2.seasons))
# (b) time series data vector corresponding to fleet (must be the same length as time:season vectors)
# this is a fishery so we are loading catch data
# Note, the OG script grabs data from load_catch_txt
# but since we need to subset by fleet (and season) we can use the data saved in truth$catchtons
# filter out appropriate catch data
flt2.data <- catch_tons_ss_MexCal_agg  %>%
  filter((time) %in% tail(unique(truth$catchtons$time),length(flt2.years)) # selecting the final simulation years
  );head(flt2.data)
# Select season 2 data
flt2.data <- flt2.data %>%
  # Modify the 'total_atoutput' column based on 'season'
  mutate(
    total_atoutput = ifelse(season == 2, total_atoutput, 0),
  );head(flt2.data)
# define ts data  for function
flt2.ts.data <- flt2.data$total_atoutput
# (c) CV
flt2.ts.cv    <- 0.05 # from RWs dat file
# (f) units, "numbers" or "biomass"
flt2.ts.units <- "biomass"
# (g) data type, "CPUE" or "catch"
flt2.ts.type  <- "catch"
# (h) fleet number
flt2.num   <- 2

#*
#* Fleet 3 - PNW
#*
# (d) - (e), years and season vectors (must be the same length)
# years of data for SS model
flt3.years <- c(2001:2019) # atlantis data is in simulation years (0,1,2..) but this allows you to insert years (2001,2002,...) into the dat file
# seasons of data for SS model
flt3.seasons <- c(1,2)
# define season vector for function
flt3.ts.season <- rep(flt3.seasons, length(flt3.years))
# define year vector based on season vector
flt3.ts.yrs <- rep(flt3.years, each = length(flt3.seasons))
# (b) time series data vector corresponding to fleet (must be the same length as time:season vectors)
# this is a fishery so we are loading catch data
# Note, the OG script grabs data from load_catch_txt
# but since we need to subset by fleet (and season) we can use the data saved in truth$catchtons
# filter out appropriate catch data
flt3.data <- catch_tons_ss_PacNW_agg  %>%
  filter((time) %in% tail(unique(truth$catchtons$time),length(flt3.years)) # selecting the final simulation years
  );head(flt3.data)
# define ts data  for function
flt3.ts.data <- flt3.data$total_atoutput
# (c) CV
flt3.ts.cv    <- 0.05 # from RWs dat file
# (f) units, "numbers" or "biomass"
flt3.ts.units <- "biomass"
# (g) data type, "CPUE" or "catch"
flt3.ts.type  <- "catch"
# (h) fleet number
flt3.num   <- 3

#*
#* Fleet 4 - AT_Survey (Acoustic Trawl)
#*
# (d) - (e), years and season vectors (must be the same length)
# years of data for SS model
flt4.years <- c(2005:2019) # atlantis data is in simulation years (0,1,2..) but this allows you to insert years (2001,2002,...) into the dat file
# seasons of data for SS model
flt4.seasons <- c(10) # Recall, we are assuming just season 10 atm based on OG dat file
# define season vector for function
flt4.ts.season <- rep(flt4.seasons, length(flt4.years))
# define year vector based on season vector
flt4.ts.yrs <- rep(flt4.years, each = length(flt4.seasons))
# (b) time series data vector corresponding to fleet (must be the same length as time:season vectors)
# this is a survey so we are loading observed biomass data
flt4.data <- survey_observed_biomass_ATsurvey  %>%
  filter(species == species_ss) %>%
  filter((((time - ts_sampinyr) / ts_stepperyr) + 1) %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt4.years),"yr_sim"]);head(flt4.data)
# summarize data by time
flt4.data <- flt4.data %>%
  group_by(species, time) %>%
  summarize(total_atoutput = sum(atoutput, na.rm = TRUE), .groups = "drop");head(flt4.data)
# define ts data  for function
flt4.ts.data <- flt4.data$total_atoutput
# (c) CV
flt4.ts.cv <- 0.25 # from RWs dat file
# (f) units, "numbers" or "biomass"
flt4.ts.units <- "biomass"
# (g) data type, "CPUE" or "catch"
flt4.ts.type <- "CPUE"
# (h) fleet number
flt4.num <- 4

#*
#* Fleet 5 - DEPM (Daily egg production method)
#*
# (d) - (e), years and season vectors (must be the same length)
# years of data for SS model
flt5.years <- c(2003:2014) # atlantis data is in simulation years (0,1,2..) but this allows you to insert years (2001,2002,...) into the dat file
# seasons of data for SS model
flt5.seasons <- c(10) # Recall, we are assuming just season 10 atm based on OG dat file
# define season vector for function
flt5.ts.season <- rep(flt5.seasons, length(flt5.years))
# define year vector based on season vector
flt5.ts.yrs <- rep(flt5.years, each = length(flt5.seasons))
# (b) time series data vector corresponding to fleet (must be the same length as time:season vectors)
# this is a survey so we are loading observed biomass data
flt5.data <- survey_observed_biomass_DEPM  %>%
  filter(species == species_ss) %>%
  filter((((time - ts_sampinyr) / ts_stepperyr) + 1) %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt5.years),"yr_sim"]);head(flt5.data)
# summarize data by time
flt5.data <- flt5.data %>%
  group_by(species, time) %>%
  summarize(total_atoutput = sum(atoutput, na.rm = TRUE), .groups = "drop");head(flt5.data)
# define ts data  for function
flt5.ts.data <- flt5.data$total_atoutput
# (c) CV
flt5.ts.cv    <- 0.25 # from RWs dat file
# (f) units, "numbers" or "biomass"
flt5.ts.units <- "biomass"
# (g) data type, "CPUE" or "catch"
flt5.ts.type  <- "CPUE"
# (h) fleet number
flt5.num   <- 5

#*
#* Fleet 4 - TEP_all (Total Egg Production)
#*
# (d) - (e), years and season vectors (must be the same length)
# years of data for SS model
flt6.years <- c(2001:2014) # atlantis data is in simulation years (0,1,2..) but this allows you to insert years (2001,2002,...) into the dat file
# seasons of data for SS model
flt6.seasons <- c(10) # Recall, we are assuming just season 10 atm based on OG dat file
# define season vector for function
flt6.ts.season <- rep(flt6.seasons, length(flt6.years))
# define year vector based on season vector
flt6.ts.yrs <- rep(flt6.years, each = length(flt6.seasons))
# (b) time series data vector corresponding to fleet (must be the same length as time:season vectors)
# this is a survey so we are loading observed biomass data
flt6.data <- survey_observed_biomass_TEPall  %>%
  filter(species == species_ss) %>%
  filter((((time - ts_sampinyr) / ts_stepperyr) + 1) %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt6.years),"yr_sim"]);head(flt6.data)
# summarize data by time
flt6.data <- flt6.data %>%
  group_by(species, time) %>%
  summarize(total_atoutput = sum(atoutput, na.rm = TRUE), .groups = "drop");head(flt6.data)
# define ts data  for function
flt6.ts.data <- flt6.data$total_atoutput
# (c) CV
flt6.ts.cv <- 0.25 # from RWs dat file
# (f) units, "numbers" or "biomass"
flt6.ts.units <- "biomass"
# (g) data type, "CPUE" or "catch"
flt6.ts.type <- "CPUE"
# (h) fleet number
flt6.num <- 6

#*
#* build lists and vectors for SS_write_ts()
#*

# (b) time series data corresponding to fleet
SS_write_ts_data <- list(flt1.ts.data,
                         flt2.ts.data,
                         flt3.ts.data,
                         flt4.ts.data,
                         flt5.ts.data,
                         flt6.ts.data
)
# (c) vector, survey and fishery cvs
SS_write_cvs <- c(flt1.ts.cv,
                  flt2.ts.cv,
                  flt3.ts.cv,
                  flt4.ts.cv,
                  flt5.ts.cv,
                  flt6.ts.cv
)
# (d) a LIST of vectors, where each item is the length of the index
SS_write_years <- list(flt1.ts.yrs,
                       flt2.ts.yrs,
                       flt3.ts.yrs,
                       flt4.ts.yrs,
                       flt5.ts.yrs,
                       flt6.ts.yrs
)
# (e) list, item is a vector representing the month of sampling for each index
SS_write_sampling <- list(flt1.ts.season,
                          flt2.ts.season,
                          flt3.ts.season,
                          flt4.ts.season,
                          flt5.ts.season,
                          flt6.ts.season
)
# (f) a vector where each entry must be one of "numbers" or "biomass"
SS_write_units <- c(flt1.ts.units,
                    flt2.ts.units,
                    flt3.ts.units,
                    flt4.ts.units,
                    flt5.ts.units,
                    flt6.ts.units
)
# (g) a vector with length = length(data.ts), each entry must be either "CPUE" or "catch"
SS_write_types <- c(flt1.ts.type,
                    flt2.ts.type,
                    flt3.ts.type,
                    flt4.ts.type,
                    flt5.ts.type,
                    flt6.ts.type
)
# (h) a list of vectors, in each vector each entry is the fleet number
SS_write_fleets <- c(flt1.num,
                     flt2.num,
                     flt3.num,
                     flt4.num,
                     flt5.num,
                     flt6.num
)

#* (---) call SS_write_ts

#* HAP - I had to make edits to SS_write_ts() to mae it work --- source("SS_write_ts.R")
SS.datfile.write <- SS_write_ts(ss_data_list   = SS.datfile,
                                ts_data        = SS_write_ts_data,
                                CVs            = SS_write_cvs,
                                data_years     = SS_write_years,
                                sampling_month = SS_write_sampling,
                                units          = SS_write_units,
                                data_type      = SS_write_types,
                                fleets         = SS_write_fleets
)

#* check out some of the outputs from this:
SS.datfile.write
SS.datfile.write$CPUE
SS.datfile.write$catch

#* HAP:
#* when debugging this script for your application, here is a good moment to
#* create the dat file with the Atlantis catch and CPUT data to see if SS will
#* run it before inserting the Atlantis length and age data:
# SS_writedat_3.30(SS.datfile.write,
#                  outfile   = paste0(dir_model_SS, name_datfile),
#                  overwrite = TRUE)
# r4ss::run(dir             = dir_model_SS,
#           exe             = "ss3_osx_arm64_3_30_23",
#           skipfinished    = F,
#           #extras          = "-nohess", #HAP debugging the failed to invert Hessian
#           show_in_console = T)

#*
#* Manually change some aspects of the SS.datfile before creating new dat file
#*
#* depending on the differences between the Atlantis data and the data that was
#* in the OG dat file, you may need to manually change additional items in the
#* dat file. Such as styr (start year) or endyr (end year) (see example code below).
#* This would be a good moment to pause and do this before moving on to inserting
#* Atlantis length and age data into the dat file data.

# # Change start year
# SS.datfile.write$styr <- years.data.for.SS[1]
# # Change end year
# SS.datfile.write$endyr <- years.data.for.SS[length(years.data.for.SS)]

#* -------------- (C) using SS_write_comps to overwrite age and length composition data from from Atlantis data (atlantisom) into a Stock Synthesis 3.3 estimation model (the .dat file)
#*
#* This calls for using the function SS_write_comps()
#*
#* This function continues processing data uploaded from a working .dat file and swaps out data with information collated
#* from an Atlantis run via atlantisom
#*
#* This function needs the following:
#* ss_data_list	  - the list object containing SS data
#* comp_matrix    - a list, where each list item is the matrix of composition data (age or length)
#* data_rows      - a list, where each entry is a vector corresponding to the year column of the composition data
#* sampling_month - a list where each list item is a vector of the sampling month for each composition data type
#* data_type      - a vector of strings, each entry needs to be either "agecomp" or "lencomp"
#* fleet_number   - a vector of numbers, each entry corresponds to which fleet the comp data are from
#* bins           - a list item, each item is a vector specifying either the age or length bins
#* caal_bool      - a vector of length ss_data_list with boolean values for whether the data is conditional age-at-length
#*
#* see ?SS_write_comps to get more info.
#*

#* (a) the list object containing SS data (output from r4ss::SS_readdat_3.30 or output from SS_write_ts)

#* as we already processed r4ss::SS_readdat_3.30 output via SS_write_ts, we will use SS_write_ts output

#* (b) - (h):
#* are a series of lists and vectors where each element corresponds to data called in comp_matrix
#* Here, rather than going through (b) - (h) individually, I am going to build the info for each dataset
#* to be called in comp_matrix then merge it all together before passing the info to SS_write_comps
#*
#* Here, we are going to pass four datasets to SS_write_comps via comp_matrix:
#*     (i.a) conditional age-at-length (CAAL) from survey
#*     (i.b) length composition data from survey
#*     (i.c) age composition data from catch
#*     (i.d) length composition data from catch
#*
#* Note, you do NOT have to provide data in this order, but whatever order you use must be consistant
#* across (b) - (h)

#* Looking at RWs file we get length and age data from fleets 1,2,3,4
#*      1 - MexCal_S1 (fishery)
#*      2 - MexCal_S2 (fishery)
#*      3 - PNW (fishery)
#*      4 - AT_survey (survey)

#* -----
#* ----- (1) MexCal_S1 (fishery)
#* -----

#*
#* (a) length composition data
#*

# recall, previously we applied calc_age2length to calculate length composition in 1 cm bins from Atlantis catch outputs (catch_age2length):
# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type lencomp
flt1.lencomp.data <- reformat_compositions(catch_age2length_MexCalS1$natlength,
                                           round.places = 0,
                                           comp_type="lencomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
flt1.lencomp.data <- flt1.lencomp.data  %>%
  filter(time %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt1.years),"yr_sim"]);head(flt1.lencomp.data)
# year column of the composition data (data for data_rows)
flt1.lencomp.years <- years.calendar.simulation[which(years.calendar.simulation$yr_sim %in% flt1.lencomp.data$time),"yr_ss"]
# sampling month for composition data (data for sampling_month)
flt1.lencomp.month <- rep(4, # HAP, value from RWs OG dat file
                          length(flt1.lencomp.years))
# data type, either "agecomp" or "lencomp" (data for data_type)
flt1.lencomp.type <- "lencomp"
# fleet type of data (data for fleet_number)
flt1.num
# age or length bins (data for bins)
# currently we are working with lencomp data so we will provide the length bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
length_bins <- as.integer(names(flt1.lencomp.data));length_bins <- length_bins[!is.na(length_bins)]
flt1.lencomp.bins <- length_bins
# boolean values for whether the data is conditional age-at-length
flt1.lencomp.bool <- FALSE

#*
#* (b) age composition data from catch
#*

# recall, previously we applied sample_fish to sample data from numbers-at-age catch data and create composition data (catch_age_comp_data)
# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type agecomp
flt1.agecomp.data <- reformat_compositions(catch_age_comp_data_MexCalS1,
                                           comp_type="agecomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
flt1.agecomp.data <- flt1.agecomp.data  %>%
  filter(time %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt1.years),"yr_sim"]);head(flt1.agecomp.data)
# year column of the composition data (data for data_rows)
flt1.agecomp.years <- years.calendar.simulation[which(years.calendar.simulation$yr_sim %in% flt1.agecomp.data$time),"yr_ss"]
# sampling month for composition data (data for sampling_month)
flt1.agecomp.month <- rep(4, # HAP, value from RWs OG dat file
                          length(flt1.agecomp.years))
# data type, either "agecomp" or "lengthcomp" (data for data_type)
flt1.agecomp.type <- "agecomp"
# fleet type of data (data for fleet_number)
flt1.num
# age or length bins of the data
# currently we are working with agecomp data so we will provide the age bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
age_bins <- as.integer(names(flt1.agecomp.data));age_bins <- age_bins[!is.na(age_bins)]
flt1.agecomp.bins <- age_bins
# boolean values for whether the data is conditional age-at-length
flt1.agecomp.bool <- FALSE

#* -----
#* ----- (2) MexCal_S2 (fishery)
#* -----

# recall, previously we applied calc_age2length to calculate length composition in 1 cm bins from Atlantis catch outputs (catch_age2length):
# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type lencomp
flt2.lencomp.data <- reformat_compositions(catch_age2length_MexCalS2$natlength,
                                           round.places = 0,
                                           comp_type="lencomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
flt2.lencomp.data <- flt2.lencomp.data  %>%
  filter(time %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt2.years),"yr_sim"]);head(flt2.lencomp.data)
# year column of the composition data (data for data_rows)
flt2.lencomp.years <- years.calendar.simulation[which(years.calendar.simulation$yr_sim %in% flt2.lencomp.data$time),"yr_ss"]
# sampling month for composition data (data for sampling_month)
flt2.lencomp.month <- rep(4, # HAP, value from RWs OG dat file
                          length(flt2.lencomp.years))
# data type, either "agecomp" or "lencomp" (data for data_type)
flt2.lencomp.type <- "lencomp"
# fleet type of data (data for fleet_number)
flt2.num
# age or length bins (data for bins)
# currently we are working with lencomp data so we will provide the length bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
length_bins <- as.integer(names(flt2.lencomp.data));length_bins <- length_bins[!is.na(length_bins)]
flt2.lencomp.bins <- length_bins
# boolean values for whether the data is conditional age-at-length
flt2.lencomp.bool <- FALSE

#*
#* (b) age composition data from catch
#*

# recall, previously we applied sample_fish to sample data from numbers-at-age catch data and create composition data (catch_age_comp_data)
# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type agecomp
flt2.agecomp.data <- reformat_compositions(catch_age_comp_data_MexCalS2,
                                           comp_type="agecomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
flt2.agecomp.data <- flt2.agecomp.data  %>%
  filter(time %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt2.years),"yr_sim"]);head(flt2.agecomp.data)
# year column of the composition data (data for data_rows)
flt2.agecomp.years <- years.calendar.simulation[which(years.calendar.simulation$yr_sim %in% flt2.agecomp.data$time),"yr_ss"]
# sampling month for composition data (data for sampling_month)
flt2.agecomp.month <- rep(4, # HAP, value from RWs OG dat file
                          length(flt2.agecomp.years))
# data type, either "agecomp" or "lengthcomp" (data for data_type)
flt2.agecomp.type <- "agecomp"
# fleet type of data (data for fleet_number)
flt2.num
# age or length bins of the data
# currently we are working with agecomp data so we will provide the age bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
age_bins <- as.integer(names(flt2.agecomp.data));age_bins <- age_bins[!is.na(age_bins)]
flt2.agecomp.bins <- age_bins
# boolean values for whether the data is conditional age-at-length
flt2.agecomp.bool <- FALSE

#* -----
#* ----- (3) PNW (fishery)
#* -----

#*
#* (a) length composition data from catch
#*

# recall, previously we applied calc_age2length to calculate length composition in 1 cm bins from Atlantis catch outputs (catch_age2length):
# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type lencomp
flt3.lencomp.data <- reformat_compositions(catch_age2length_PNW$natlength,
                                           round.places = 0,
                                           comp_type="lencomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
flt3.lencomp.data <- flt3.lencomp.data  %>%
  filter(time %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt3.years),"yr_sim"]);head(flt3.lencomp.data)
# year column of the composition data (data for data_rows)
flt3.lencomp.years <- years.calendar.simulation[which(years.calendar.simulation$yr_sim %in% flt3.lencomp.data$time),"yr_ss"]
# sampling month for composition data (data for sampling_month)
flt3.lencomp.month <- rep(4, # HAP, value from RWs OG dat file
                          length(flt3.lencomp.years))
# data type, either "agecomp" or "lencomp" (data for data_type)
flt3.lencomp.type <- "lencomp"
# fleet type of data (data for fleet_number)
flt3.num
# age or length bins (data for bins)
# currently we are working with lencomp data so we will provide the length bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
length_bins <- as.integer(names(flt3.lencomp.data));length_bins <- length_bins[!is.na(length_bins)]
flt3.lencomp.bins <- length_bins
# boolean values for whether the data is conditional age-at-length
flt3.lencomp.bool <- FALSE

#*
#* (b) age composition data from catch
#*

# recall, previously we applied sample_fish to sample data from numbers-at-age catch data and create composition data (catch_age_comp_data)
# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type agecomp
flt3.agecomp.data <- reformat_compositions(catch_age_comp_data_PNW,
                                           comp_type="agecomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
flt3.agecomp.data <- flt3.agecomp.data  %>%
  filter(time %in% years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt3.years),"yr_sim"]);head(flt3.agecomp.data)
# year column of the composition data (data for data_rows)
flt3.agecomp.years <- years.calendar.simulation[which(years.calendar.simulation$yr_sim %in% flt3.agecomp.data$time),"yr_ss"]
# sampling month for composition data (data for sampling_month)
flt3.agecomp.month <- rep(4, # HAP, value from RWs OG dat file
                          length(flt3.agecomp.years))
# data type, either "agecomp" or "lengthcomp" (data for data_type)
flt3.agecomp.type <- "agecomp"
# fleet type of data (data for fleet_number)
flt3.num
# age or length bins of the data
# currently we are working with agecomp data so we will provide the age bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
age_bins <- as.integer(names(flt3.agecomp.data));age_bins <- age_bins[!is.na(age_bins)]
flt3.agecomp.bins <- age_bins
# boolean values for whether the data is conditional age-at-length
flt3.agecomp.bool <- FALSE

#* -----
#* ----- (4) AT_survey (survey)
#* -----

#*
#* (a) length composition data
#*

#* recall, previously we ran calc_age2length to calculate length composition in 1 cm bins from Atlantis
#* ecological output (see survey_age2length above) and catch output
#* This generated lists containing three data.frames:
#*    mulen (mean length at age),
#*    muweight (mean weight at age), and
#*    natlength (numbers at length)

# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type lencomp
flt4.lencomp.data <- reformat_compositions(survey_age2length_ATsurvey$natlength,
                                           round.places = 0,
                                           comp_type="lencomp")
# isolate the temporal data to be sent to SS (data for comp_matrix); NOTE! Survey time data is in ts, not years!
flt4.lencomp.data <- filter(flt4.lencomp.data,
                            (((time - ts_sampinyr) / ts_stepperyr) + 1) %in%
                              years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt4.years),"yr_sim"]);head(flt4.lencomp.data)
# year column of the composition data (data for data_rows); NOTE! Survey time data needs to be converted to desired year data for dat file
flt4.lencomp.years <- years.calendar.simulation$yr_ss[match((((flt4.lencomp.data$time - ts_sampinyr) / ts_stepperyr) + 1),
                                                            years.calendar.simulation$yr_sim)]
# sampling month for composition data (data for sampling_month)
flt4.lencomp.month <- rep(1, # HAP, value in RWs OG dat file
                          length(flt4.lencomp.data$time))
# data type, either "agecomp" or "lencomp" (data for data_type)
flt4.lencomp.type <- "lencomp"
# fleet type of data (data for fleet_number)
flt4.num
# age or length bins (data for bins)
# currently we are working with lencomp data so we will provide the length bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
flt4.lencomp.bins <- as.integer(names(flt4.lencomp.data));flt4.lencomp.bins <- flt4.lencomp.bins[!is.na(flt4.lencomp.bins)]
# boolean values for whether the data is conditional age-at-length
flt4.lencomp.bool <- FALSE

#*
#* (b) conditional age-at-length (CAAL)
#*

#* recall, previously we ran calc_age2length to calculate length composition in 1 cm bins from Atlantis
#* ecological output (see survey_age2length above) and catch output
#* This generated lists containing three data.frames:
#*    mulen (mean length at age),
#*    muweight (mean weight at age), and
#*    natlength (numbers at length)

# use ?reformat_compositions() to reformat composition data for input in to Stock Synthesis with comp_type caalcomp
flt4.caalcomp.data <- reformat_compositions(survey_age2length_ATsurvey$natlength, # numbers at length output from calc_age2length on ecological data
                                            round.places = 4,
                                            comp_type = "caalcomp")
# isolate the temporal data to be sent to SS (data for comp_matrix); NOTE! Survey time data is in ts, not years!
flt4.caalcomp.data <- filter(flt4.caalcomp.data,
                             (((time - ts_sampinyr) / ts_stepperyr) + 1) %in%
                               years.calendar.simulation[which(years.calendar.simulation$yr_ss %in% flt4.years),"yr_sim"]);head(flt4.caalcomp.data)

# year column of the composition data (data for data_rows); NOTE! Survey time data needs to be converted the year info you want in the dat file
flt4.caalcomp.years <- years.calendar.simulation$yr_ss[match((((flt4.caalcomp.data$time - ts_sampinyr) / ts_stepperyr) + 1),
                                                             years.calendar.simulation$yr_sim)]
# sampling month for composition data (data for sampling_month)
flt4.caalcomp.month <- rep(1, # using the value in RWs OG dat file
                           length(flt4.caalcomp.data$time))
# data type, either "agecomp" or "lengthcomp" (data for data_type)
flt4.caalcomp.type <- "agecomp"
# fleet type of data (data for fleet_number)
flt4.num
# age or length bins of the data
# currently we are working with agecomp data so we will provide the age bins
# we can get length bins for this data by looking at the df names (will get NAs but that is okay)
age_bins <- as.integer(names(flt4.caalcomp.data));age_bins <- age_bins[!is.na(age_bins)]
flt4.caalcomp.bins <- age_bins
# boolean values for whether the data is conditional age-at-length
flt4.caalcomp.bool <- TRUE

#*
#* Now that we have defined all needed info from each dataset for items (b) - (h), we can build the lists and vectors to
#* pass to the function
#*

# (b) a list, where each entry is a vector corresponding to the year column of the composition data
SS_write_comps_data <- list(flt1.lencomp.data,
                            flt2.lencomp.data,
                            flt3.lencomp.data,
                            flt4.lencomp.data,
                            flt1.agecomp.data,
                            flt2.agecomp.data,
                            flt3.agecomp.data,
                            flt4.caalcomp.data)

# (c) a list, where each entry is a vector corresponding to the year column of the composition data
SS_write_comps_years <- list(flt1.lencomp.years,
                             flt2.lencomp.years,
                             flt3.lencomp.years,
                             flt4.lencomp.years,
                             flt1.agecomp.years,
                             flt2.agecomp.years,
                             flt3.agecomp.years,
                             flt4.caalcomp.years)

# (d) a list where each list item is a vector of the sampling month for each composition data type
SS_write_comps_months <- list(flt1.lencomp.month,
                              flt2.lencomp.month,
                              flt3.lencomp.month,
                              flt4.lencomp.month,
                              flt1.agecomp.month,
                              flt2.agecomp.month,
                              flt3.agecomp.month,
                              flt4.caalcomp.month)

# (e) a vector of strings, each entry needs to be either "agecomp" or "lengthcomp"
SS_write_comps_types <- c(flt1.lencomp.type,
                          flt2.lencomp.type,
                          flt3.lencomp.type,
                          flt4.lencomp.type,
                          flt1.agecomp.type,
                          flt2.agecomp.type,
                          flt3.agecomp.type,
                          flt4.caalcomp.type)

# (f) a vector of numbers, each entry corresponds to which fleet the comp data are from
SS_write_comps_fleets <- c(flt1.num,
                           flt2.num,
                           flt3.num,
                           flt4.num,
                           flt1.num,
                           flt2.num,
                           flt3.num,
                           flt4.num)

# (g) a list item, each item is a vector specifying either the age or length bins
SS_write_comps_bins <- list(flt1.lencomp.bins,
                            flt2.lencomp.bins,
                            flt3.lencomp.bins,
                            flt4.lencomp.bins,
                            flt1.agecomp.bins,
                            flt2.agecomp.bins,
                            flt3.agecomp.bins,
                            flt4.caalcomp.bins)

# (h) a vector of length ss_data_list with boolean values for whether the data is conditional age-at-length
SS_write_comps_bool <- c(flt1.lencomp.bool,
                         flt2.lencomp.bool,
                         flt3.lencomp.bool,
                         flt4.lencomp.bool,
                         flt1.agecomp.bool,
                         flt2.agecomp.bool,
                         flt3.agecomp.bool,
                         flt4.caalcomp.bool)

#* --- call SS_write_comps
#*
#* Write age and length composition data from atlantisom to Stock Synthesis 3.30
#* Note: every time this function is called, it overwrites the age and length composition data currently in the ss_data_list object.

#* HAP - I had to make edits to SS_write_ts() to mae it work --- source("SS_write_comps.R")
SS.datfile.write.comps <- SS_write_comps(ss_data_list   = SS.datfile.write,
                                         comp_matrix    = SS_write_comps_data,
                                         data_rows      = SS_write_comps_years,
                                         sampling_month = SS_write_comps_months,
                                         data_type      = SS_write_comps_types,
                                         fleet_number   = SS_write_comps_fleets,
                                         bins           = SS_write_comps_bins,
                                         caal_bool      = SS_write_comps_bool
)

#*
#* Manually change some aspects of the SS.datfile before creating new dat file
#*
#* Some of these items are necessary (eg changing bins to match those of Atlantis)
#* however some of these were just in the OG sardine example but may not be needed
#* for our project. It would be worth while to think about this section of script
#* to determine what is/is not necessary.

# Change length bin structure to match atlantis data
SS.datfile.write.comps$lbin_vector   <- length_bins
# Get correct number of length bins
SS.datfile.write.comps$N_lbins       <- length(length_bins)
# Change age bin structure to match atlantis data
SS.datfile.write.comps$agebin_vector <- age_bins
# Get correct number of age bins
SS.datfile.write.comps$N_agebins     <- length(age_bins)

#* HAP: In the OG sardine example, they set the lbin_method:
# Set lbin_method to 1 - this makes the population length bins match the data bins
# When lbin_method==1, we just comment out the binwidth, minimum, and maximum size arguments since they aren't used
SS.datfile.write.comps$lbin_method  <- 1
SS.datfile.write.comps$binwidth     <- "#"
SS.datfile.write.comps$minimum_size <- "#"
SS.datfile.write.comps$maximum_size <- "#"
# HAP: I had a quick chat with SG about this and it sounds like this was specific for the example
# at the time but not a requirement for atlantisom. Thus, we should consider whether or not we actually
# want to do this or comment it out.

#* HAP: In the OG sardine example, they set the minimum sample size:
# #Change minimum sample size to 0.001 for CAAL data (SS won't let it go lower than this)
#SS.datfile.write.comps$age_info$minsamplesize <- rep(0.001,2) # length needs to match the number of fleets
# HAP: I commented this out for testing this script, but we should consider whether or not
# this is needed and if so how to appropriate set it.

#* -------------- (D) using SS_writedat_3.30 to save the .dat file

SS_writedat_3.30(SS.datfile.write.comps,
                 outfile   = paste0(dir_model_SS, name_datfile),
                 overwrite = TRUE)

#* ----------------------------
#* ---------------------------- II. Write data for Stock Synthesis
#* ----------------------------    b) write data to the ctl file
#* ----------------------------
#*
#* In the OG example scripts, there was some code attempting to work with the ctl file
#* I started debugging some of this (commented out below, the rest of it is at the
#* end of this script file) but I did not have the time to complete.
#*
#* On Jan 27, 2025, I had a chat with IK, RW and SG and it was mentioned that for the
#* project we may need to make some edits to the ctl file, eg
#* Time block, Start and end years, Starting parameters may need to be pulled from Atlantis
#* This would be a good spot to do it: after editing the dat file be before running SS
#*

# #* ----- debugging
# #*
# #*
# #*
#
# # get ctl data
# name_ctlfile <-  "sardEM_3_3.ctl"
# SS.ctlfile <- r4ss::SS_readctl_3.30(paste0(dir_model_SS, name_ctlfile))
#
# # get bio prm data
# load_biol <- load_biolprm(dir          = dir.outputs,
#                           file_biolprm = in.biol.prm)
#
# # get M est data
#
# # (a) get file name for yoy output:
#
# file_yoy_txt <- list.files(path = dir.outputs, pattern = out.yoy)[1]
# #* User manual: this is the Biomass (tonnes) of recruits per year (which is the same as per
# #* spawning event, because a group only spawns once per year) summed over the total
# #* model domain. Snapshot at the time of spawning, regardless of when the output
# #* is written out.
# # load yoy out data
# YOY <- atlantisom::load_yoy(dir.outputs, file_yoy_txt)
# # subset by ss species
# YOY_ss <- YOY %>%
#   select(Time, "SAR.0")
#
# # (b)
#
# truenums_ss <- truth$nums[truth$nums$species==species_ss,]
#
# # (---) run calc_Z
#
# names(load_biol$kwsr)[1] <- names(load_biol$kwrr)[1] <- "1" # need to fix this so the calc_Z function will run
#
# fullresZ <- calc_Z(yoy     = YOY_ss,
#                    nums    = truenums_ss,
#                    fgs     = truth$fgs,
#                    biolprm = load_biol,
#                    toutinc = load_runpar$toutinc)
#
# # get mean wt for age classes
# meanwt_spp <- survey_age2length$muweight %>%
#   filter((((time - survey_time_first) / ts_stepperyr) + 1) > 30) %>%
#   group_by(agecl) %>%
#   summarize(meanwt = mean(atoutput))
#
# #three dimensional vector with column names agecl, meanln, and cvln giving mean body length and cv of body length at age
#
# meanlen_spp <- survey_age2length$mulen %>%
#   filter((((time - survey_time_first) / ts_stepperyr) + 1) > 30) %>%
#   group_by(agecl) %>%
#   summarize(meanln = mean(atoutput),
#             cvln = (sd(atoutput)/mean(atoutput))*100)
#
# #* Function to pull relevant life history parameters from atlantis to write to a Stock Synthesis control and/or data file.
# SS.ctlfile.biol <- SS_write_biol(ctl_obj        = SS.ctlfile,
#                                  biolprm_object = load_biol,
#                                  species_code   = "SAR",
#                                  M_est          = mean(fullresZ$atoutput),
#                                  wtsage         = meanwt_spp,
#                                  lensage        = meanlen_spp)
# #* Error in SS_write_biol(ctl_obj = SS.ctlfile, biolprm_object = load_biol,  :
# #* could not find function "SS_write_biol"

#* ----------------------------
#* ---------------------------- III. Run Stock Synthesis
#* ----------------------------

#* -------------- Preparation

# --- (a) libraries
#library(r4ss) #* HAP, should already be loaded from previous section

# --- (b) ss exe file, remember should be ss directory
ss_exe <- "ss3_osx_arm64_3_30_23"

#* -------------- Run SS
r4ss::run(dir             = dir_model_SS,
          exe             = ss_exe,
          skipfinished    = F,
          #extras          = "-nohess", #HAP used to debug a failed to invert Hessian
          show_in_console = T)

#* -------------- SS outputs
#* https://sgaichas.github.io/poseidon-dev/SkillAssessInit.html
#* create a list object for the output from Stock Synthesis -- ?SS_output
SSout_list <- r4ss::SS_output(dir        = dir_model_SS,
                              verbose    = TRUE,
                              printstats = TRUE,
                              covar      = FALSE)

#* plot many quantities related to output from Stock Synthesis -- ?SS_plots
SS_plots(SSout_list)

#* -------------- How does SS3 ESTIMATED biomass compare with TRUE biomass?

#* get TRUE biomass
#* recall, previously we ran sample_survey_biomass() to sample a biomass index of abundance from an atlantis scenario
#* we subset that output by the ss species

forplot_surATsurvey_obsB_ss <- survey_observed_biomass_ATsurvey %>%
  filter(species == species_ss) %>%
  mutate(
    transformed_time = (((time - ts_sampinyr) / ts_stepperyr) + 1)  # Transform the 'time' column
  ) %>%
  left_join(
    years.calendar.simulation,
    by = c("transformed_time" = "yr_sim")  # Join on the transformed time
  );head(forplot_surATsurvey_obsB_ss)

forplot_surDEPM_obsB_ss     <- survey_observed_biomass_DEPM %>%
  filter(species == species_ss) %>%
  mutate(
    transformed_time = (((time - ts_sampinyr) / ts_stepperyr) + 1)  # Transform the 'time' column
  ) %>%
  left_join(
    years.calendar.simulation,
    by = c("transformed_time" = "yr_sim")  # Join on the transformed time
  );head(forplot_surDEPM_obsB_ss)

forplot_surTEPall_obsB_ss   <-  survey_observed_biomass_TEPall %>%
  filter(species == species_ss) %>%
  mutate(
    transformed_time = (((time - ts_sampinyr) / ts_stepperyr) + 1)  # Transform the 'time' column
  ) %>%
  left_join(
    years.calendar.simulation,
    by = c("transformed_time" = "yr_sim")  # Join on the transformed time
  );head(forplot_surTEPall_obsB_ss)

#* Make plot of est bio V tru bio

ggplot() +
  geom_line(data = forplot_surATsurvey_obsB_ss,
            aes(x = yr_ss, y = atoutput, color = "B ATsurvey"), alpha = 10/10) +
  geom_line(data = forplot_surDEPM_obsB_ss,
            aes(x = yr_ss, y = atoutput, color = "B DEPM"), alpha = 10/10) +
  geom_line(data = forplot_surTEPall_obsB_ss,
            aes(x = yr_ss, y = atoutput, color = "B TEPall"), alpha = 10/10) +
  geom_point(data = SSout_list$timeseries,
             aes(x = Yr, y = Bio_all, color = as.factor(Seas)), alpha = 10/10) +
  # HAP: note: SSout_list$timeseries is split by Seas thus there are two data points per Yr
  theme_tufte() +
  theme(legend.position = "top") +
  labs(color = "Legend: SS Seas") +  # Adjust legend title
  facet_wrap(~species, scales = "free")
#* HAP - 01.2025 - The trends look similar but Bio_all is higher earlier in the timeseries then sharply drops
#* maybe there is something that needs to be adjusted in the SS model inputs to help the initial trends be closer
#* to the data?

#* -------------- How about recruitment?

# get file name for yoy output:
file_yoy_txt <- list.files(path = dir.outputs, pattern = out.yoy)[1]
#* User manual: this is the Biomass (tonnes) of recruits per year (which is the same as per
#* spawning event, because a group only spawns once per year) summed over the total
#* model domain. Snapshot at the time of spawning, regardless of when the output
#* is written out.
# load yoy out data
YOY <- atlantisom::load_yoy(dir.outputs, file_yoy_txt)
# get code for ss species
code_ss <- load_groups$Code[which(load_groups$Name == species_ss)]
# isolate yoy data for ss species
YOY_ss <- YOY %>%
  select(Time, paste0(code_ss, ".0"))
#* HAP,
#* in the example I think they are converting yoy in order to compare to rec from SS3:

# define conversion, mg carbon converted to wet weight in tonnes
k_wetdry <- truth$biolprm$kgw2d / 1000000000
# get recruitment (numbers)
# WARNING only works for CCA because YOY.txt rows 2:end are already in numbers
# and we are merging out the incorrect and irrelevant YOY row 1 (Time=0)
# also hardcoded for sardine example
true_rec <- YOY_ss %>%
  mutate(yr = as.integer(round(YOY_ss$Time/365))) %>%
  mutate(recnums = SAR.0/k_wetdry/truth$biolprm$redfieldcn) %>% # HAP, this is from the example
  mutate(rectonnes = SAR.0) %>%
  filter(yr>0) %>%
  left_join(
    years.calendar.simulation,
    by = c("yr" = "yr_sim")  # Join on the transformed time
  )
# # plot
# ggplot() +
#   geom_line(data = true_rec,
#             aes(x = yr,
#                 y = recnums, color="True R"),
#             alpha = 10/10) +
#   geom_point(data = SSout_list$timeseries,
#              aes(x = Yr,
#                  y = (Recruit_0), color = "SS3 Est R"), alpha = 10/10) +
#   theme_tufte() +
#   theme(legend.position = "top") +
#   labs(colour="")
#*
#* HAP: to not fall into the conversion pit and just focus on patterns,
#* I just plotted the two trends on different y axis:

library(ggthemes)

# Normalize the secondary axis data
SSout_list$timeseries$scaled_recruit <- SSout_list$timeseries$Recruit_0 / max(SSout_list$timeseries$Recruit_0) * max(true_rec$rectonnes)

# Plot
ggplot() +
  geom_line(data = true_rec,
            aes(x = yr_ss,
                y = rectonnes, color="True R"),
            alpha = 10/10) +
  geom_point(data = SSout_list$timeseries, #HAP, I noticed that in some yrs values are in Seas 1 and other yrs values are in Seas 2
             aes(x = Yr,
                 y = scaled_recruit,
                 color = "SS3 Est R"), alpha = 10/10) +
  # HAP: note: SSout_list$timeseries is split by Seas thus there are two data points per Yr
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="") +
  scale_y_continuous(
    name = "True R (tonnes)",
    sec.axis = sec_axis(~ . * (max(SSout_list$timeseries$Recruit_0) / max(true_rec$recnums)),
                        name = "SS3 Est R (numbers)")
  )
#* HAP - 01.2025 - magnitudes are similar but there appears to be a declining trend in SS output
#* might be tied to the very sharp drop in Bio_all over time??

# HAP - I saved to data so you can load my working environment and see what it looks like:
# save.image("~/Documents/atlantisom-master/atlantisom_exSardine_6fleets.RData")

#*
#* Everything below this point was being debugged and not yet integrated into the main script
#*
#*
#*
#*
#*
#*
#*
#*
#*
#*
#*

#*
#* Coming up with dynamic life history parameters (partial from Christine)
#*
#* Below we get the weight-length relationship parameters from atlantis,
#* calculate h and R0 from Atlantis α and β parameters, use the function to
#* back out natural mortality (M) from Atlantis, and use the survey
#* length-at-age from Atlantis to estimate a growth curve.
#*
#* The following is under construction from CreateStockSynthesis.Rmd and not
#* evaluated. Existing control file was modified by hand to make the number
#* of recruitment settlement assignments 1 instead of two, with GPattern month
#* area age vector 1 1 1 1.

# Get biological parameters

# #Load needed inputs for biological parameters
# source(here("config/CC3Config.R"))
# # HAP, did this up top

# biological parameters
biolprm <- truth$biolprm
# functional groups
fgs     <- truth$fgs

li_a_use <- biolprm$wl[match(fgs$Code[match(species_ss,fgs$Name)],biolprm$wl[, 1]), 2]/1000
li_b_use <- biolprm$wl[match(fgs$Code[match(species_ss,fgs$Name)],biolprm$wl[, 1]), 3]


len_nonburn <- survey_age2length$mulen %>%
  filter((((time - survey_time_first) / ts_stepperyr) + 1) > 30)


plot(len_nonburn$atoutput~len_nonburn$agecl, col=len_nonburn$time)


length.data <- data.frame("Year"=(len_nonburn$time-survey_sample_time)/timestep+1, length=len_nonburn$atoutput, Weight=NA, Sex="Female", age=as.integer(len_nonburn$agecl))

runprm <- load_runprm(dir = dir.outputs, file_runprm = in.run) # note, the run.xml file is in the outputs folder
# read YOY output file
YOY <- load_yoy(dir.outputs, list.files(path = dir.outputs, pattern = out.yoy)[1])
# subset data by focal species
truenums_ss <- truth$nums[truth$nums$species==species_ss,] # HAP, had to update from ex as ex had "results" and I think that was "truth"...
index.yoy <- paste0(fgs[which(fgs$Name == species_ss),"Code"],".0") # HAP, to help automate code a bit, get the index for yoy data based off what is in "species"
YOY_ss <- YOY %>%
  select(Time, index.yoy)
# uses atlantisom to Calculate total mortality for age structured groups (focal group)
#* HAP, I needed to change the headers of the following as the function expects these names:
names(biolprm$kwsr)[1] <- "1"; names(biolprm$kwrr)[1] <- "1"
fullresZ <- calc_Z(yoy     = YOY_ss,
                   nums    = truenums_ss,
                   fgs     = fgs,
                   biolprm = biolprm,
                   toutinc = runprm$toutinc)
# get mean wt for age classes
meanwt_spp <- survey_age2length$muweight %>%
  filter((((time - survey_time_first) / ts_stepperyr) + 1) > 30) %>%
  group_by(agecl) %>%
  summarize(meanwt = mean(atoutput))

# read ctl file (defined above)
name_ctlfile <- "sardEM_3_3.ctl"

sardine.ctl <- r4ss::SS_readctl_3.30(paste0(dir_model_SS, name_ctlfile))
#* HAP, before I had issues getting this line to run as I was missing the data_echo.ss_new file.
#* I had to get my hands on a sardine ss3 model and get it to run on my machine via the updated
#* ss3 framework. This took a bit. IKs model was a tad outdated. Luckily, RWs model ran on my machine
#* and I was able to get that echo file. Now, this line of code runs.

# #* Write life history parameters -- This calls for using the function SS_write_biol()
# #*
# #* Function to pull relevant life history parameters from atlantis to write to a Stock Synthesis control and/or data file.
# #*
# #* This function needs the following:
# #* ctl_obj	      - list that is returned from r4ss::read_ctl() that contains the control file
# #* biolprm_object - list with a number of atlantis biological parameters
# #* species_code   - the three-letter species code of the fish you are creating a model for
# #* M_est          - scalar value for M of the species (default = NULL)
# #* wtsage         - two dimensional vector with column names agecl and meanwt giving mean body weight at age in grams
# #* lenage         - three dimensional vector with column names agecl, meanln, and cvln giving mean body length and cv of body length at age
# #* lenwt_a = NULL
# #* lenwt_b = NULL
# #*
# #* see ?SS_write_biol to get more info.
#
# sardine.ctl.new <- SS_write_biol(ctl_obj        = sardine.ctl,
#                                  biolprm_object = biolprm,
#                                  species_code   = fgs[which(fgs$Name == species_ss),"Code"],
#                                  M_est          = mean(fullresZ$atoutput),
#                                  wtsage         = meanwt_spp
# )

#maturity ogive
#check what R0 is

len_nonburn <- ss_length_stdsurv$mulen %>%
  filter(time > burnin*timestep)
plot(len_nonburn$atoutput~len_nonburn$agecl, col=len_nonburn$time)

length.data <- data.frame("Year"=(len_nonburn$time-survey_sample_time)/timestep+1, length=len_nonburn$atoutput, Weight=NA, Sex="Female", age=as.integer(len_nonburn$agecl))

#require(bbmle)
#vb_estimates <-sample_fit_vbgf(length.data, 25, 45, 0.4,
#  0.1, 0.1, 20, 40, 0.05, 0.01, 0.01,
#  30, 50, 0.6, 0.3, 0.3, 0.5, 10)
