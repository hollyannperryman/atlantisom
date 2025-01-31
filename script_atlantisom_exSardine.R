
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
#* Notes: Specify our survey sampling. This could come from other information such as
#* the overlap of actual survey stations with OM polygons, experiments
#* evaluating survey selectivity and efficiency, actual sample-based survey
#* cv, etc.
#*
#* This calls for using the function create_survey(). This function needs the following:
#*
#* dat     - A data.frame of numbers at age
#* time    - The timing of the survey (a vector indicating specific time steps, which are typically associated with years) i.e., seq(365,10*3650,365) would be an annual survey for 10 years
#* species - The species to sample in the survey (a vector)
#* boxes   - A matrix with two columns: 1) polygon: box ID's that are sampled 2) survArea: area sampled in that box
#* effic   - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species efficiency:
#* selex   - Selectivity at age. A dataframe defining selectivity at age for each species.
#*
#* see ?create_survey to get more info.

# (1) A data.frame of numbers at age

# You can get this from run_truth() via numbers output (truth$nums) (see above for the use of this function)

# HAP, the OG example also ran create_survey with bio data
# You can get this from run_truth() via bio output (truth$biomass_ages) (see above for the use of this function)

# (2) timing of the survey

# get timing info from atlantis run file
load_runpar <- load_runprm(dir.outputs, in.run)
# number of output time steps
ts_total <- load_runpar$tstop / load_runpar$outputstep
# length of the simulation (years)
ts_nyears <- load_runpar$tstop / load_runpar$toutfinc
# number of days in between data reported
ts_stepperyr <- if(load_runpar$outputstepunit == "days") 365 / load_runpar$toutinc
# mid point in between touninc
ts_midptyr <- round(median(seq(0,ts_stepperyr)))
# define first timestep to sample
survey_time_first <- ts_midptyr # 3rd timestep simulates a summer survey, changed to 2
# define last timestep to sample
survey_time_last <- ts_total - 1 # 495
# define frequency of sample
survey_time_frequency <- ts_stepperyr # 5
# define vector of indices of survey times to pull
survey_time <- seq(survey_time_first, survey_time_last, by = survey_time_frequency)

# (3) species to sample, atlantis functional group "name". This can be just the assessed species or a list of species, eg: c("Pacific_sardine","Mesopel_M_Fish")

# defined above as species_truth

# (4) box ID's that are sampled

# load bgm data, see ?load_box for more info.
load_boxes <- load_box(dir.outputs, in.bgm)
# select atlatis polygon IDs for sampling
survey_boxes <- c(0:(load_boxes$nbox - 1)) # note, this is box IDs 0,1,... which does not line up with unique(truth$biomass_ages$polygon) which is 1,2,...

# (5) Efficiency of the survey

# In this context, "efficiency" is typically used to describe the proportion of available fish caught by the survey gear (gear efficiency).
# Values range from 0 to 1; 1 = all means all fish in the survey area are detected or caught (perfect efficiency), < 1 means only a fraction of the fish are detected or caught.
#* HAP: in the atlantisom examples, this was 1 for the perfect survey example,
#* https://github.com/r4atlantis/atlantisom/blob/master/config/census_spec.R
#* and 0.5 for the other survey example.
#* https://sgaichas.github.io/poseidon-dev/Sardines_atlantisom2SS_demo.html
survey_eff <- 1 # Lets start with the perfect then see if we want to make this more complex, Hake it might not be 1
# prep data for function expectations
survey_efficiency <- data.frame(species    = species_truth,
                                efficiency = survey_eff)

# (6) Selectivity at age

# age classes of your species
age_classes <- 1:10 # HAP, I think this is the Atlantis age classes as I tried 1:12 and it still produced 1:10 output
# selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1,
                  length(age_classes)) # Assume uniform selectivity, same as selex1 here
# prep data for function expectations
survey_selectivity <- data.frame(species = rep(species_truth, length(age_classes)),
                                 agecl   = age_classes,
                                 selex   = sel_by_age)

# (***) Create survey observations from Atlantis output

survey_numbers <- create_survey(dat     = truth$nums,
                                time    = survey_time,
                                species = species_truth,
                                boxes   = survey_boxes,
                                effic   = survey_efficiency,
                                selex   = survey_selectivity);head(survey_numbers)

#* NOTE, in the example code, they also use biomass data ($biomass_ages (biomass for age structured groups))
survey_biomass <- create_survey(dat     = truth$biomass_ages,
                                time    = survey_time,
                                species = species_truth,
                                boxes   = survey_boxes,
                                effic   = survey_efficiency,
                                selex   = survey_selectivity);head(survey_biomass)
#* but, I am not sure if that is just showcasing an example or if the function can actually be used this way...


#* -------------- (C) sample a biomass index of abundance from an Atlantis scenario
#*
#* This call for using the function sample_survey_biomass(). This function needs the following:
#*
#* dat     - A data.frame of numbers at age
#* cv      - Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
#* wtAtAge - Weight-at-age by species. a matrix with columns: species, agecl, time (optional), wtAtAge (kg)
#*
#* see ?sample_survey_biomass to get more info.

# (1) A data.frame of numbers at age

# You can get this data from the output of create_survey() (see above for the use of this function)

# (2) Coefficient of variation for the entire species specific biomass
#* in example, for the perfect survey::: CVs <- list("lenage"=0, "fishery"=0, "survey"=0),
#* in example, for the other survey::: CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)
cv_survey <- 0.1
# data for function:
survey_cv <- data.frame(species = species_truth,
                        cv      = cv_survey)

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
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_truth
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
survey_structn_aggr <- aggregateDensityData(dat     = truth$structn,
                                            time    = survey_time,
                                            species = species_truth,
                                            boxes   = survey_boxes); head(survey_structn_aggr)
# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.

# Here, we need to first define effN for each species:
# HAP, for this walk through we only have one species of focus thus only one effN needed
sample_effN <- 1000 # note, the example used these values also: 1000, 1e+8
# then make the list for the function
survey_effN <- data.frame(species = species_truth,
                          effN    = rep(sample_effN, length(species_truth)))
# now, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_structn_sample <- sample_fish(dat    = survey_structn_aggr,
                                     effN   = survey_effN,
                                     sample = FALSE);head(survey_structn_sample)

# (ii) A data.frame containing the reserve nitrogen data

# First, use aggregateDensityData() to aggregate and select density data from Atlantis output --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, survey_eco_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, species_truth
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, survey_eco_box
#*
survey_resn_aggr <- aggregateDensityData(dat     = truth$resn,
                                         time    = survey_time,
                                         species = species_truth,
                                         boxes   = survey_boxes); head(survey_resn_aggr)
# Next, use sample_fish() to create composition data based on info from aggregateDensityData() --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#*
# Here, effN has already been defined (see above, survey_effN), so can go straight into creating composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
survey_resn_sample <- sample_fish(dat    = survey_resn_aggr,
                                  effN   = survey_effN,
                                  sample = FALSE);head(survey_resn_sample)

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
survey_age_comp_data <- sample_fish(dat  = survey_numbers,
                                    effN = survey_effN); head(survey_age_comp_data)

# (iv) biolprm data stored in data from run_truth() (truth$biolprm)

# (v) fgs data, stored in data from run_truth() (truth$fgs)

# (vi) variability in length at age (lenage saved in CVs, defined above)
#* in example, for the perfect survey::: CVs <- list("lenage"=0, "fishery"=0, "survey"=0),
#* in example, for the other survey::: CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)
var_lenatage <- 0.14

# (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
maxbin_ss <- 46 #  # HAP, the example used this 200

# (---) run calc_age2length to extract length composition data
survey_age2length <- calc_age2length(structn  = survey_structn_sample,
                                     resn     = survey_resn_sample,
                                     nums     = survey_age_comp_data,
                                     biolprm  = truth$biolprm,
                                     fgs      = truth$fgs,
                                     maxbin   = maxbin_ss, # Not in OG example, but shouldn't it be especially since it is used when catch data is passed through the function (see below)
                                     CVlenage = var_lenatage
);unique(survey_age2length$natlength$lower.bins); length(unique(survey_age2length$natlength$lower.bins))
#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(survey_age2length$mulen)
head(survey_age2length$muweight)
head(survey_age2length$natlength) # unique(survey_age2length$natlength$lower.bins); length(unique(survey_age2length$natlength$lower.bins))

#* ---- pause and plot the length distribution
survey_age2length_natleng_ss <- survey_age2length$natlength %>%
  filter(species == species_truth)

ggplot(survey_age2length_natleng_ss, aes(upper.bins)) +
  geom_bar(aes(weight = atoutput)) +
  theme_tufte() +
  labs(subtitle = paste("",survey_age2length_natleng_ss$species))
#* ----

#* Lastly, we need to restructure the mean weight-at-age (muweight) data for sample_survey_biomass()
survey_age2length_wtAtAge <- survey_age2length$muweight %>%
  select(species, agecl, time, wtAtAge = atoutput) %>%
  mutate(wtAtAge = wtAtAge/1000) # HAP, converting wtAtAge to kgs for sample_survey_biomass()

# (***) run sample_survey_biomass() to sample a biomass index of abundance from an atlantis scenario

# sample_survey_biomass(), Sample a biomass index of abundance from an atlantis scenario
# this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# data    = A data.frame of numbers at age
# cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
# wtAtAge = Weight-at-age by species. a matrix with columns: species, agecl, time (optional), wtAtAge (kg)
survey_observed_biomass <- sample_survey_biomass(dat     = survey_numbers,
                                                 cv      = survey_cv,
                                                 wtAtAge = survey_age2length_wtAtAge); head(survey_observed_biomass)

# Note, the example code also showcases sample_survey_numbers
#
# sample_survey_numbers(), Sample a numbers index of abundance from an atlantis scenario
# this takes numbers-at-age data from an Atlantis scenario where the data was read in from Atlantis output
# data    = A data.frame of numbers at age
# cv      = Coefficient of variation for the entire species specific biomass a matrix with columns: species, cv
survey_observed_numbers  <- sample_survey_numbers(dat     = survey_numbers,
                                                  cv      = survey_cv); head(survey_observed_numbers)


#* --------------
#* PAUSE
#* --------------
#* now that we have an observation of biomass, lets take a break and compare this against true data

#* Comparing our (census) survey based on true biomass from above with the
#* Atlantis output file “[modelscenario]BiomIndx.txt” should give us a perfect
#* match. Note that the our (census) survey may have more sampling in time than
#* the Atlantis output file.


# (1) subset sample survey biomass data by group of focus

forplot_survey_obsB_ss <- survey_observed_biomass[survey_observed_biomass$species == species_truth,]

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
  filter(species %in% c(species_truth))

# (3) lets get biomass estmation from a PERFECT survey

# perfect survey
forplot_biomass_perfectobs <- create_survey(dat     = truth$biomass_ages,
                                            time    = c(0:ts_total), # all time steps sampled
                                            species = species_truth,
                                            boxes   = survey_boxes,
                                            effic   = data.frame(species    = species_truth,
                                                                 efficiency = rep(1.0,length(species_truth))),
                                            selex   = data.frame(species = rep(species_truth, each=10),
                                                                 agecl   = rep(c(1:10),length(species_truth)),
                                                                 selex   = rep(1.0,length(species_truth)*10)));head(forplot_biomass_perfectobs)
# perfect sample
forplot_biomass_perfectsample <- sample_survey_biomass(dat     = forplot_biomass_perfectobs,
                                                       cv      = data.frame(species = species_truth,
                                                                            cv      = rep(0.0,length(species_truth))),
                                                       wtAtAge = data.frame(species = rep(species_truth, each=10),
                                                                            agecl   = rep(c(1:10), length(species_truth)),
                                                                            wtAtAge = rep(1.0, length(species_truth)*10)));head(forplot_biomass_perfectsample)
# select focal group
forplot_biomass_perfectsample_ss <- forplot_biomass_perfectsample[forplot_biomass_perfectsample$species == species_truth,];head(forplot_biomass_perfectsample_ss)

# (3) plot

ggplot() +
  # plot the data from the biomindx.txt output file
  geom_point(data=forplot.biomindx.tidy, aes(x=Time/365,y=biomass/1,
                                             color="True B, biom.txt output"), alpha = 10/10) +
  # plot the data from a PERFECT survey (made just above for this comparison)
  geom_line(data=forplot_biomass_perfectsample_ss, aes(x=time/ts_stepperyr,y=atoutput*1000, # convert from kg to tonnes
                                                       color="perfect survey census B"), alpha = 10/10) +
  # plot the data from the survey we executed above
  geom_point(data=forplot_survey_obsB_ss, aes(x=time/ts_stepperyr,y=atoutput/1,
                                              color="survey sample B from nums * wtAtAge"), alpha = 10/10) +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="",
       x = "time (years)",
       y = "biomass (tonnes)") +
  facet_wrap(~species, scales="free")

#* --------------
#* END PAUSE
#* --------------


#* -------------- (D) loading Atlantis catch output (load catch)
#*
#* After much testing, the most reliable source for Atlantis catch in biomass is catch.txt.
#* Using the catch.txt file means that true total catch weight through atlantisom is
#* currently available only on an annual and full-model (all polygons) scale.
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

file_catch_txt <- list.files(path = dir.outputs, pattern = out.catch)[1]

# (c) A data frame created by load_fgs

# defined above, load_groups

# -- Load Atlantis catch data

load_catch_txt <- load_catch(dir        = dir.outputs,
                             file_catch = file_catch_txt,
                             fgs        = load_groups);head(load_catch_txt)
# HAP,I am not 100% sure this is necessary. The example uses this infor in SS_write_ts so I will need to double back and confirm once I finsh debugging


#* -------------- (E) create fishery observations from Atlantis outputs (fishery dependent survey data)
#*
#* We can sample fishery catch for lengths, ages, and weights. Here, we use create_fishery_subset
#* on the numbers output of run_truth to create the survey census of age composition.
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

# (a) directory to atlantis outputs

# You can get this from run_truth() via catch output (truth$catch) (see above)

#* (b) The timing of the survey
#* Timesteps may be different for fishery output than for the ecological output. The run parameter
#* file will tell us what time units we are dealing with:
fstepperyr <- if(load_runpar$outputstepunit=="days") 365/load_runpar$toutfinc
#* Now that we know that, we can determine fishing years for selection, assuming fishing output is
#* annual and years index 1,2,3 etc
#* HAP, although I trimmed up the code from the OG examples, I tried to use some of the
#* same definitions to make comparison easier
#*
# fish_sample_full <- unique(load_catch_txt$time/365)
# fish_times       <- fish_sample_full[c((0*fstepperyr):(nyears*fstepperyr))]
# fish_years       <- unique(floor(fish_times/fstepperyr) + 1)
fishery_subset_time <- c(1:ts_nyears)*fstepperyr

# (c) which fleet indices to aggregate in the output (NULL aggregates all fleets)

fishery_subset_fleets <- NULL
#* HAP - to get create_fishery_subset to run I set fleets to NULL
#* I am not sure what fleets need to be, but it looks like a vector a fleet names
#* the issue is truth$catch, in the code for create_fishery_subset
#* https://github.com/r4atlantis/atlantisom/blob/master/R/create_fishery_subset.R
#* it looks like truth$catch needs to have a column for "fleets" ??? and right now
#* my data does not have that. Thus, I think I need to go back to the function
#* that builds the truth data and see why it is missing that info.

# (d) The species to sample in the survey (a vector)

fishery_subset_species <- species_truth # HAP just doing SAR here rather than the spp processed in to get truth

# (e) A vector of box numbers

fishery_subset_boxes <- survey_boxes

# -- create fishery subset -- ?create_fishery_subset
catch_fishery_subset <- create_fishery_subset(dat     = truth$catch, #numbers at age, thus reading truth$catch here
                                              time    = fishery_subset_time,
                                              fleets  = fishery_subset_fleets,
                                              species = fishery_subset_species,
                                              boxes   = fishery_subset_boxes);head(catch_fishery_subset)

#* ---------- take a pause and make some images
#*
# isolate data for focal group in SS3 model
truecatch_ss <- load_catch_txt[load_catch_txt$species == species_truth,]
# Aggregating the data
catch_aggregated <- aggregate(atoutput ~ species + time, data = catch_fishery_subset[catch_fishery_subset$species == species_truth,],
                              sum);head(catch_aggregated) # units here is nums, not kg or tonnes
catch_aggregated$species <- "Pacific_sardine (nums)"
truth_catchtons_agg <- aggregate(atoutput ~ species + time, data = truth$catchtons[truth$catchtons$species == "SAR",],
                                 sum);head(truth_catchtons_agg)
truth_catchtons_agg$species <- truecatch_ss$species
# Viewing the aggregated data
plotcatch <- ggplot() +
  geom_line(data=truecatch_ss, aes(x=time/365,y=atoutput, color="a. true catch bio"),
            alpha = 10/10) +
  geom_line(data=truth_catchtons_agg, aes(x=time,y=atoutput, color="b. truth$catchtons"), # truth$catch looks to be numbers
            alpha = 10/10, linetype = "dashed") +
  geom_line(data=catch_aggregated, aes(x=time,y=atoutput, color="c. create_fishery_subset"),
            alpha = 10/10, linetype = "dotted") +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="")+
  facet_wrap(~species, scales="free");plotcatch
#* ----------

#* -------------- (F) generate size composition data from fishery information
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

# (a) A data.frame containing the structural nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

# first, use aggregateDensityData to aggregate true structn per survey or fishery subsetdesign --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, fishery_subset_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes
catch_structn_aggr <- aggregateDensityData(dat     = truth$structn,
                                           time    = survey_time, #fishery_subset_time,
                                           species = fishery_subset_species,
                                           boxes   = fishery_subset_boxes);head(catch_structn_aggr)
# then, use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
# Here, we need to first define effN for each species:
# HAP, for this walk through we only have one species of focus thus only one effN needed
fish_effN <- 1000 # https://sgaichas.github.io/poseidon-dev/Sardines_atlantisom2SS_demo.html
# HAP, was testing other values: 50, 200
# then make the list for the function
catch_effN <- data.frame(species = fishery_subset_species,
                         effN    = rep(fish_effN,
                                       length(fishery_subset_species)))
# now, create composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_structn_sample <- sample_fish(dat    = catch_structn_aggr,
                                    effN   = catch_effN,
                                    sample = FALSE);head(catch_structn_sample)

# (b) A data.frame containing the reserve nitrogen per functional-group ("species") per timestep, ageclass, layer and polygon.

# first, use aggregateDensityData to aggregate true structn per survey or fishery subsetdesign --- ?aggregateDensityData
#* needed for function:
#* dat     - A data.frame of density data (structn, resn) from reading in the Atlantis output ----  stored in truth
#* time    - A single value or a vector of time steps for a survey. Time steps are specified as integers --- see timing of survey defined above, fishery_subset_time
#* species - A vector of character values, where each entry is a species name that is sampled in the survey --- see the sample species defined above, fishery_subset_species
#* boxes   - A vector of polygons where the survey samples within each specified polygon --- see box ID's that are sampled defined above, fishery_subset_boxes
catch_resn_aggr <- aggregateDensityData(dat     = truth$resn,
                                        time    = survey_time, #fishery_subset_time,
                                        species = fishery_subset_species,
                                        boxes   = fishery_subset_boxes);head(catch_resn_aggr)
# then, use sample_fish on info from aggregateDensityData to aggregate using median (NOT sample, ie set sample to FALSE) --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (output from create_survey())
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#* sample - Logical asking whether to apply multinomial sampling using effN. Setting to false results in simple aggregation of atoutput to annual age class values. The default value is TRUE.
#*
# Here, effN has already been defined (see above, catch_effN), so can go straight into creating composition data based on info from aggregateDensityData
# NOTE!! you are not sampling, thus set sample to FALSE
catch_resn_sample <- sample_fish(dat    = catch_resn_aggr,
                                 effN   = catch_effN,
                                 sample = FALSE);head(catch_structn_sample)

# (c) A data.frame containing the numbers per functional-group ("species") per timesetp, ageclass, layer and polygon.

# we can do this by using sample_fish() to sample numbers-at-age (catch_fishery_subset, see above) and create composition data --- ?sample_fish
#* needed for function:
#* dat  - A data.frame of numbers at age (catch_fishery_subset, see above)
#* effN - Efficiency for each species: a matrix with nrow=length(species). Columns: species: the species name. Matches names in species effN: the effective N for each species (effective sample size)
#
# Here, effN has already been defined (see above, catch_effN), so can go straight into sampling data from create_fishery_subset()
# NOTE!! you ARE sampling, thus do NOT set sample (default is TRUE)
catch_age_comp_data <- sample_fish(dat  = catch_fishery_subset,
                                   effN = catch_effN); head(catch_age_comp_data)

# (d) A list of biological parameters available from the [...]_Biol.prm file, as read in by load_biolprm.

# stored in truth, truth$biolprm

# (e) A data frame created by load_fgs that reads in the csv file containing functional group names, usually "functionalGroups.csv".

# stored in truth, truth$fgs

# (f) The variability in length at age (same for all species) The default value is 0.1.

# recall, this is stored in var_lenatage

# # (g) (optional) - maxbin - The upper length (cm) bin applied to all species sampled. The default value is 150.
# maxbin_ss <- 46 #  # HAP, the example used this 200

# -- Length sample with user specified max length bin

#* HAP, I think we need to eight convert the N data time or num data time
catch_age_comp_data$time <- catch_structn_sample$time

catch_age2length <- calc_age2length(structn  = catch_structn_sample,
                                    resn     = catch_resn_sample,
                                    nums     = catch_age_comp_data,
                                    biolprm  = truth$biolprm,
                                    fgs      = truth$fgs,
                                    maxbin   = maxbin_ss, # HAP, should this be the same as what we are using when passing survey data??
                                    CVlenage = var_lenatage
);unique(catch_age2length$natlength$lower.bins);length(unique(catch_age2length$natlength$lower.bins))

#* NOTE, the output here is A list containing three data.frames:
#* mulen (mean length at age),
#* muweight (mean weight at age), and
#* natlength (numbers at length).
#* natlength is in the same format as other dataframes in the atlantisom package except has two
#* additional columns to include the length bin information.
head(catch_age2length$mulen)
head(catch_age2length$muweight)
head(catch_age2length$natlength) # unique(catch_age2length$natlength$lower.bins);length(unique(catch_age2length$natlength$lower.bins))

# # NOTE: if sampling more species than the data files may be quite large, making it ideal to save
# # and load RDS files rather than re-executing functions.
# saveRDS(catch_age2length, file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))
# catch_age2length <- readRDS(file.path(dir.outputs, paste0(save.name,"_Sar", "catch_age2length.rds")))


#* --------------
#* PAUSE
#* --------------

#*
#* true fishery catch over time
#*

# isolate data for focal group in SS3 model
truecatch_ss <- load_catch_txt[load_catch_txt$species == species_truth,]

plotcatch <- ggplot() +
  geom_line(data=truecatch_ss, aes(x=time/365,y=atoutput, color="true catch bio"),
            alpha = 10/10) +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="")+
  facet_wrap(~species, scales="free");plotcatch

#*
#* Plot true catch at age:
#*

catch_age_comp_data_ss <- catch_age_comp_data %>%
  filter(species == species_truth)

catageplot <-
  ggplot(catch_age_comp_data_ss, aes(x=agecl, y=atoutput)) +
  geom_point() +
  theme_tufte() +
  labs(subtitle = paste("", catch_age_comp_data_ss$species));catageplot

# you can isolate by time
catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 1, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 2, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 3, scales="free")
# catageplot + facet_wrap_paginate(~time, ncol=3, nrow = 3, page = 4, scales="free")

#*
#* We should get the upper end of anything with a 200cm max length bin.
#* Sardine catch lengths:
#*

catch_age2length_natleng_ss <- catch_age2length$natlength %>%
  filter(species == species_truth)

lfplot <- ggplot(catch_age2length_natleng_ss, aes(upper.bins)) +
  geom_bar(aes(weight = atoutput)) +
  theme_tufte() +
  labs(subtitle = paste("",catch_age2length_natleng_ss$species));lfplot

# you can isolate by time
lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 1, scales="free_y")
lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 2, scales="free_y")
lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 3, scales="free_y")
lfplot + facet_wrap_paginate(~time, ncol=4, nrow = 4, page = 4, scales="free_y")

#*
#* Fishery weight at (st)age:
#*

wageplot <- ggplot(catch_age2length$muweight, aes(agecl, atoutput)) +
  geom_point(aes(colour = time)) +
  theme_tufte() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limits=factor(c(1:10))) +
  xlab("age class") +
  ylab("average individual weight (g)") +
  labs(subtitle = "") +
  facet_wrap(c("species"), scales="free_y");wageplot

#* Change in wt at (st)age in the fishery over the last 10 years

wtage_annsurv <- catch_age2length$muweight %>%
  filter(time %in% c(382:432))

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
#* END PAUSE
#* --------------




#* ----------------------------
#* ---------------------------- II. Write data for Stock Synthesis
#* ----------------------------
#*
#* This will take a few sections of script:
#*         (A) using SS_readdat_3.30 to upload a .dat file into R
#*         (B) using SS_write_ts to overwrite the CPUE from Atlantis data (atlantisom) into a Stock Synthesis 3.3 estimation model (the .dat file)
#*         (C) using SS_write_comps to overwrite age and length composition data from from Atlantis data (atlantisom) into a Stock Synthesis 3.3 estimation model (the .dat file)
#*         (D) using SS_writedat_3.30 to save the .dat file
#*

#* (0) ----- preparation

# (a) directory to SS model
dir_model_SS <- "/Users/hollyperryman/Documents/CalCurrent/SS3/Sardine/AB_Lmax25_v2_ABinControl_Mlower/"

# (b) Name of SS data files to write data to
name_datfile <- "sardEM_3_3.dat"

# (c) libraries
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
library(r4ss) #* HAP, I now have version 1.50.0. Hopefully that will work with SS 3.30.23

# (f) time specifications of Atlantis outputs to be put into SS

# debugging!!
years.data.for.SS <- c(30:79) # HAP, this is the same as the OG dat file, we need to do some debugging if we are to change this
#years.data.for.SS <- c(1:79)

# # number of years of data to pull (nyears), # HAP, this is defined below when we read in outputs to give this info
# # initialization period of Atlantis simulation (ie, burnin)
# burnin <- 30  # HAP, note - I think this is only important when writing data to SS


#* -------------- (A) using SS_readdat_3.30 to upload a .dat file into R
#*
#* atlantisom provides data to SS3 by uploading a working .dat file and swapping out data
#* with the data developed herein based on Atlantis output. Thus, first, a .dat file needs
#* to be upload for editing.
#* This is done through r4ss via SS_readdat_3.30 (Read Stock Synthesis (version 3.30) data file into list object in R)

SS.datfile <- r4ss::SS_readdat_3.30(paste0(dir_model_SS, "sardEM_3_3_OG.dat"))
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

#* (b) This argument was not described in the R documentation (?SS_write_ts)
#* looking at the example it appears to be a list
#* list(observed biomass for species ss through X years (ie, info from sample_survey_biomass()),
#*      catch for species ss through X years (ie, info from load_catch()))
#*

# subset sample_survey_biomass() output by ss species and temporal range to be put into the .dat file
survey_observed_biomass_ss <- survey_observed_biomass %>%
  filter(species == species_truth) %>%
  filter((((time - survey_time_first) / ts_stepperyr) + 1) %in% years.data.for.SS) # REMEMBER THESE ARE TIME STEPS

# subset load_catch() output by ss species and temporal range to be put into the .dat file
catch_ss <- load_catch_txt  %>%
  filter(species == species_truth) %>%
  filter((time / 365) %in% years.data.for.SS) # REMEMBER THESE ARE DAYS

# build list for SS_write_ts()
SS_write_ts_data <- list(survey_observed_biomass_ss$atoutput,
                         catch_ss$atoutput)

# (c) vector, suvey and fishery cvs

# use the CVs which were define above
# recall cv_survey
cv_fishery <- 0.01
# define vector for function
SS_write_cvs <- c(cv_survey,
                  cv_fishery)

# (d) a LIST of vectors, where each item is the length of the index

# # from example:
# data.years <- list((survey_observed_biomass$time[fish_years]-survey_sample_time)/timestep+1,
#                    fish_years)

# debuggin:
SS_write_years <- list(years.data.for.SS,
                       years.data.for.SS)

# (e) list, item is a vector representing the month of sampling for each index

# define the month of survey and fishing
# HAP, these come from the atlantisom example
survey_month  <- 7
fishing_month <- 1

# build info to pass to function
SS_write_sampling <- list(rep(survey_month, length(years.data.for.SS)),
                          rep(fishing_month, length(years.data.for.SS)))

# (f) a vector where each entry must be one of "numbers" or "biomass"

SS_write_units <- c("biomass","biomass")

# (g) a vector with length = length(data.ts), each entry must be either "CPUE" or "catch"

SS_write_types <- c("CPUE","catch") # "CPUE" or "catch"

# (h) a list of vectors, in each vector each entry is the fleet number

SS_write_fleets <- c(2,1) # in each vector each entry is the fleet number

#* -- call SS_write_ts

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

#*
#* (i.a) conditional age-at-length (CAAL) from survey
#*

#* recall, previously we ran calc_age2length to calculate length composition in 1 cm bins from Atlantis
#* ecological output (see survey_age2length above) and catch output
#* This generated lists containing three data.frames:
#*    mulen (mean length at age),
#*    muweight (mean weight at age), and
#*    natlength (numbers at length)

# use ?reformat_compositions() to reformat compositional data for input in to Stock Synthesis with comp_type caalcomp
survey_age2length_caalcomp <- reformat_compositions(survey_age2length$natlength, # numbers at length output from calc_age2length on ecological data
                                                    round.places = 4,
                                                    comp_type = "caalcomp");unique(survey_age2length_caalcomp$time)
# isolate the temporal data to be sent to SS (data for comp_matrix)
a.composition.data <- filter(survey_age2length_caalcomp, (((time - survey_time_first) / ts_stepperyr) + 1) %in% years.data.for.SS) # NOTE!! Survey time data is in ts, not years!
# year column of the composition data (data for data_rows)
a.year.col.data <- (((a.composition.data$time - survey_time_first) / ts_stepperyr) + 1) # NOTE!! Survey time data needs to be converted from ts to year
# sampling month for composition data (data for sampling_month)
a.sampling.month <- rep(survey_month, length(a.composition.data$time))
# data type, either "agecomp" or "lengthcomp" (data for data_type)
a.dat.type <- "agecomp"
# fleet type of data (data for fleet_number)
a.fleet.num <- 2 # HAP, I am guessing that 2 corresponds to survey based on previous examples...
# age or length bins (data for bins)
# currently we are working with agecomp data so we will provide the age bins.
# define the age bins, which can be done through the SS.datfile.write$agecomp data
# HAP, first, I am not sure why the ex code is not just grabbing data from SS.datfile.write$agebin_vector
# HAP, second, I am not sure why we are not grabbing the info from the comp data itself. Here they are the same, will they always be the same?
age_bin_names <- names(SS.datfile.write$agecomp)[10:length(names(SS.datfile.write$agecomp))]
age_bins      <- sub("a","",age_bin_names)
# define data to pass to function
a.bins <- age_bins
# boolean values for whether the data is conditional age-at-length
a.bool <- TRUE

#*
#* (i.b) length composition data from survey
#*

#* recall, previously we ran calc_age2length to calculate length composition in 1 cm bins from Atlantis
#* ecological output (see survey_age2length above) and catch output
#* This generated lists containing three data.frames:
#*    mulen (mean length at age),
#*    muweight (mean weight at age), and
#*    natlength (numbers at length)

# use ?reformat_compositions() to reformat compositional data for input in to Stock Synthesis with comp_type lencomp
survey_age2length_lencomp <- reformat_compositions(survey_age2length$natlength,
                                                   round.places = 0,
                                                   comp_type="lencomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
b.composition.data <- filter(survey_age2length_lencomp, (((time - survey_time_first) / ts_stepperyr) + 1) %in% years.data.for.SS) # NOTE!! Survey time data is in ts, not years!
# year column of the composition data (data for data_rows)
b.year.col.data <- (((b.composition.data$time - survey_time_first) / ts_stepperyr) + 1) # NOTE!! Survey time data needs to be converted from ts to year
# sampling month for composition data (data for sampling_month)
b.sampling.month <- rep(survey_month, length(b.composition.data$time))
# data type, either "agecomp" or "lencomp" (data for data_type)
b.dat.type <- "lencomp"
# fleet type of data (data for fleet_number)
b.fleet.num <- 2 # HAP, I am guessing that 2 corresponds to survey based on previous examples...
# age or length bins (data for bins)
# currently we are working with lencomp data so we will provide the length bins.
# define the length bins, which can be done with the b.composition.data data
length_bins <- as.integer(names(b.composition.data));length_bins <- length_bins[!is.na(length_bins)]
# define data to pass to function
b.bins <- length_bins
# boolean values for whether the data is conditional age-at-length
b.bool <- FALSE


#* (i.c) age composition data from catch
# recall, previously we applied sample_fish to sample data from numbers-at-age catch data and create composition data (catch_age_comp_data)
# use ?reformat_compositions() to reformat compositional data for input in to Stock Synthesis with comp_type agecomp
catch_age_comp_data_agecomp <- reformat_compositions(catch_age_comp_data,
                                                     comp_type="agecomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
c.composition.data <- filter(catch_age_comp_data_agecomp, (((time - survey_time_first) / ts_stepperyr) + 1) %in% years.data.for.SS) # NOTE!! Survey time data is in ts, not years!
# year column of the composition data (data for data_rows)
c.year.col.data <- (((c.composition.data$time - survey_time_first) / ts_stepperyr) + 1) # NOTE!! Survey time data needs to be converted from ts to year
# sampling month for composition data (data for sampling_month)
c.sampling.month <- rep(fishing_month, length(c.composition.data$time))
# data type, either "agecomp" or "lengthcomp" (data for data_type)
c.dat.type <- "agecomp"
# fleet type of data (data for fleet_number)
c.fleet.num <- 1 # HAP, I am guessing that 1 corresponds to fishery data based on previous examples...
# age or length bins (data for bins)
# HAP, we hav defined the age bins above. Like the example I am working from, I am going to use
# those values here.
c.bins <- age_bins
# boolean values for whether the data is conditional age-at-length
c.bool <- FALSE


# (i.d) length composition data from catch
# recall, previously we applied calc_age2length to calculate length composition in 1 cm bins from Atlantis catch outputs (catch_age2length):
# use ?reformat_compositions() to reformat compositional data for input in to Stock Synthesis with comp_type lencomp
catch_age2length_lencomp <- reformat_compositions(catch_age2length$natlength,
                                                  round.places = 0,
                                                  comp_type="lencomp")
# isolate the temporal data to be sent to SS (data for comp_matrix)
d.composition.data <- filter(catch_age2length_lencomp, (((time - survey_time_first) / ts_stepperyr) + 1) %in% years.data.for.SS) # NOTE!! Survey time data is in ts, not years!
# year column of the composition data (data for data_rows)
d.year.col.data <- (((d.composition.data$time - survey_time_first) / ts_stepperyr) + 1) # NOTE!! Survey time data needs to be converted from ts to year
# sampling month for composition data (data for sampling_month)
d.sampling.month <- rep(fishing_month, length(d.composition.data$time))
# data type, either "agecomp" or "lencomp" (data for data_type)
d.dat.type <- "lencomp"
# fleet type of data (data for fleet_number)
d.fleet.num <- 1 # HAP, I am guessing that 1 corresponds to fishery data based on previous examples...
# age or length bins (data for bins)
# HAP, not here I am getting length_bins from the survey data and using it for both the survey and catch data, like
# in the example, and I checked and they do match. My question is, will that always be the case?
d.bins <- length_bins
# boolean values for whether the data is conditional age-at-length
d.bool <- FALSE

#*
#* Now that we have defined all needed info from each dataset for items (b) - (h), we can build the lists and vectors to
#* pass to the function
#*

# (b) a list, where each entry is a vector corresponding to the year column of the composition data
SS_write_comps_data <- list(a.composition.data,
                            b.composition.data,
                            c.composition.data,
                            d.composition.data)

# (c) a list, where each entry is a vector corresponding to the year column of the composition data
SS_write_comps_years <- list(a.year.col.data,
                             b.year.col.data,
                             c.year.col.data,
                             d.year.col.data)

# (d) a list where each list item is a vector of the sampling month for each composition data type
SS_write_comps_months <- list(a.sampling.month,
                              b.sampling.month,
                              c.sampling.month,
                              d.sampling.month)

# (e) a vector of strings, each entry needs to be either "agecomp" or "lengthcomp"
SS_write_comps_types <- c(a.dat.type,
                          b.dat.type,
                          c.dat.type,
                          d.dat.type)

# (f) a vector of numbers, each entry corresponds to which fleet the comp data are from
SS_write_comps_fleets <- c(a.fleet.num,
                           b.fleet.num,
                           c.fleet.num,
                           d.fleet.num)

# (g) a list item, each item is a vector specifying either the age or length bins
SS_write_comps_bins <- list(a.bins,
                            b.bins,
                            c.bins,
                            d.bins)

#* HAP, there is lbin data in SS.datfile.write. I suppose we want to insert the lbin info developed with
#* the Atlantis data, or should we make adjustments to working with the Atlantis data so they reflect the
#* lbin info in the SS3 model?

# (h) a vector of length ss_data_list with boolean values for whether the data is conditional age-at-length
SS_write_comps_bool <- c(a.bool,
                         b.bool,
                         c.bool,
                         d.bool)

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
#* ---------------------------- III. Run Stock Synthesis
#* ----------------------------

#* -------------- Preparation

# --- (a) libraries
library(r4ss)

# --- (b) ss exe file, remember should be ss directory
ss_exe <- "ss3_osx_arm64_3_30_23"

#* -------------- Run SS
r4ss::run(dir             = dir_model_SS,
          exe             = ss_exe,
          skipfinished    = F,
          #extras          = "-nohess", #HAP debugging the failed to invert Hessian
          show_in_console = T)

#* -------------- SS outputs
#* https://sgaichas.github.io/poseidon-dev/SkillAssessInit.html
#* create a list object for the output from Stock Synthesis -- ?SS_output
SSout_list <- r4ss::SS_output(dir        = dir_model_SS,
                              verbose    = TRUE,
                              printstats = TRUE,
                              covar      = FALSE)

#* -------------- SS output plots
#* plot many quantities related to output from Stock Synthesis -- ?SS_plots
SS_plots(SSout_list)

#* -------------- How does SS3 ESTIMATED biomass compare with TRUE biomass?

#* get TRUE biomass
#* recall, previously we ran sample_survey_biomass() to sample a biomass index of abundance from an atlantis scenario
#* we subset that output by the ss species
survey_observed_biomass_ss <- survey_observed_biomass %>%
  filter(species==species_truth)

#* Make plot of est bio V tru bio
ggplot() +
  geom_line(data = survey_observed_biomass_ss,
            aes(x = (((time - survey_time_first) / ts_stepperyr) + 1),
                y = atoutput,
                color = "True B"), alpha = 10/10) +
  geom_point(data = SSout_list$timeseries,
             aes(x = Yr,
                 y = Bio_all,
                 color="SS3 Est B"), alpha = 10/10) +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="") +
  facet_wrap(~species, scales="free")
#* HAP - there are two data points at the start of the SS3 Est B data that are MUCH larger than the rest of the timeseries. Maybe
#* this can be fixed by adjusting another input for SS??

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
code_ss <- load_groups$Code[which(load_groups$Name == species_truth)]
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
  mutate(yr = as.integer(round(YOY_ss$Time)/365)) %>%
  mutate(recnums = SAR.0/k_wetdry/truth$biolprm$redfieldcn) %>% # HAP, this is from the example
  mutate(rectonnes = SAR.0) %>%
  filter(yr>0)
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
#* However, to not fall into the conversion pit and just focus on patterns,
#* I just plotted the two trends on different y axis:

library(ggthemes)

# Normalize the secondary axis data
SSout_list$timeseries$scaled_recruit <- SSout_list$timeseries$Recruit_0 / max(SSout_list$timeseries$Recruit_0) * max(true_rec$rectonnes)

# Plot
ggplot() +
  geom_line(data = true_rec,
            aes(x = yr,
                y = rectonnes, color="True R"),
            alpha = 10/10) +
  geom_line(data = SSout_list$timeseries,
            aes(x = Yr,
                y = scaled_recruit, color = "SS3 Est R"), alpha = 10/10) +
  theme_tufte() +
  theme(legend.position = "top") +
  labs(colour="") +
  scale_y_continuous(
    name = "True R (tonnes)",
    sec.axis = sec_axis(~ . * (max(SSout_list$timeseries$Recruit_0) / max(true_rec$recnums)),
                        name = "SS3 Est R (numbers)")
  )
# HAP - in our example, this is lining up pretty well!!

# HAP - I saved to data so you can load my working environment and see what it looks like:
# save.image("~/Documents/atlantisom-master/atlantisom_exSardine.RData")

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
#*
#* Coming up with dynamic life history parameters (partial from Christine)
#*
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

li_a_use <- biolprm$wl[match(fgs$Code[match(species_truth,fgs$Name)],biolprm$wl[, 1]), 2]/1000
li_b_use <- biolprm$wl[match(fgs$Code[match(species_truth,fgs$Name)],biolprm$wl[, 1]), 3]


len_nonburn <- survey_age2length$mulen %>%
  filter((((time - survey_time_first) / ts_stepperyr) + 1) > 30)


plot(len_nonburn$atoutput~len_nonburn$agecl, col=len_nonburn$time)


length.data <- data.frame("Year"=(len_nonburn$time-survey_sample_time)/timestep+1, length=len_nonburn$atoutput, Weight=NA, Sex="Female", age=as.integer(len_nonburn$agecl))


runprm <- load_runprm(dir = dir.outputs, file_runprm = in.run) # note, the run.xml file is in the outputs folder
# read YOY output file
YOY <- load_yoy(dir.outputs, list.files(path = dir.outputs, pattern = out.yoy)[1])
# subset data by focal species
truenums_ss <- truth$nums[truth$nums$species==species_truth,] # HAP, had to update from ex as ex had "results" and I think that was "truth"...
index.yoy <- paste0(fgs[which(fgs$Name == species_truth),"Code"],".0") # HAP, to help automate code a bit, get the index for yoy data based off what is in "species"
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
#                                  species_code   = fgs[which(fgs$Name == species_truth),"Code"],
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
