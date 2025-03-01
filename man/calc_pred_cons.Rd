% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_pred_cons.R
\name{calc_pred_cons}
\alias{calc_pred_cons}
\title{Calculate eaten total biomass in tonnes for each functional group.}
\usage{
calc_pred_cons(eat, grazing, vol, biolprm, runprm)
}
\arguments{
\item{eat}{A \code{data.frame} containing the consumed biomass in mg N for
each functional group, time, ageclass, and polygon.
The \code{data.frame} must originate from \code{\link{load_nc}}
using \code{select_variable = "Eat"}.}

\item{grazing}{A \code{data.frame} containing the consumed biomass in mg N for
each functional group, time, ageclass, and polygon.
The \code{data.frame} must originate from \code{\link{load_nc}}
using \code{select_variable = "Grazing"}.}

\item{vol}{A \code{data.frame} containing the volume for
each time, polygon, and layer.
The \code{data.frame} must originate from \code{\link{load_nc_physics}}
using \code{physic_variables = "volume"}.}

\item{biolprm}{A list of biological parameters available from the
\code{[...]_Biol.prm} file, as read in by \code{\link{load_biolprm}}.}
}
\value{
A code{data.frame} in tidy format with the following columns:
  species, agecl, time, polygon, atoutput (bio_eaten)
}
\description{
Function to calculate the eaten total biomass in tonnes:
"consumption" for each functional group
per time, polygon, and ageclass.
}
\examples{
d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
fgs <- load_fgs(d, "Functional_groups.csv")
bps <- load_bps(dir = d, fgs = "Functional_groups.csv",
  file_init = "Initial_condition.nc")
runprm <- load_runprm(d, "Run_settings.xml")
boxes <- get_boundary(load_box(dir = d, file_bgm = "Geography.bgm"))
groups <- load_fgs(dir = d, "Functional_groups.csv")
groups <- groups[groups$IsTurnedOn > 0, "Name"]
eat <- load_nc(dir = d, file_nc = "outputsPROD.nc",
  bps = bps, fgs = fgs, select_groups = groups,
  select_variable = "Eat", check_acronyms = TRUE,
  bboxes = boxes)
grazing <- load_nc(dir = d, file_nc = "outputsPROD.nc",
  bps = bps, fgs = fgs, select_groups = groups,
  select_variable = "Grazing", check_acronyms = TRUE,
  bboxes = boxes)
vol <- load_nc_physics(dir = d, file_nc = "outputs.nc",
  physic_variables = "volume", aggregate_layers = FALSE,
  bboxes = boxes)
biolprm <- load_biolprm(dir = d,
  file_biolprm = "Biology.prm")
runprm <- load_runprm(dir = d, file_runprm = "Run_settings.xml")
calcs <- calc_pred_cons(eat = eat, grazing = grazing,
  vol = vol, biolprm = biolprm, runprm = runprm)
rm(calcs)

}
\seealso{
Other calc functions: 
\code{\link{calc_Z}()},
\code{\link{calc_age2length}()},
\code{\link{calc_avgwtstage2age}()},
\code{\link{calc_biomass_age}()},
\code{\link{calc_biomass_pool}()},
\code{\link{calc_pred_diet}()},
\code{\link{calc_stage2age}()},
\code{\link{calc_timestep2time}()}
}
\author{
Alexander Keth, Sarah Gaichas
}
\concept{calc functions}
