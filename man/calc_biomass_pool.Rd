% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_biomass_pool.R
\name{calc_biomass_pool}
\alias{calc_biomass_pool}
\title{Calculate biomass in t per non-age-structured groups (biomass pools) per polygon and time.}
\usage{
calc_biomass_pool(pooln, vol, area, fgs, biolprm)
}
\arguments{
\item{pooln}{A \code{data.frame} containing the Nitrogen per functional-group
("species") per timestep, layer and polygon. Ageclass is always = 1.
The \code{data.frame} has to originate from \code{\link{load_nc}} using
\code{select_variable = "N"}.}

\item{vol}{A \code{data.frame} containing the volume for
each time, polygon, and layer.
The \code{data.frame} must originate from \code{\link{load_nc_physics}}
using \code{physic_variables = "volume"}.}

\item{area}{A \code{data.frame} containing the area for
each polygon.
The \code{data.frame} must originate from \code{\link{load_boxarea}}.}

\item{fgs}{A data frame created by \code{\link{load_fgs}} that
reads in the csv file containing functional group names, usually
\code{"functionalGroups.csv"}.}

\item{biolprm}{A list of biological parameters available from the
\code{[...]_Biol.prm} file, as read in by \code{\link{load_biolprm}}.}
}
\value{
A \code{data.frame} in tidy format with the following columns:
  species, agecl=1, time, polygon and atoutput.
  Atoutput is the total biomass in t.
  At this point biomass was aggregated (sum) per layer or area for benthos!
}
\description{
Calculate biomass in t per non-age-structured groups (biomass pools) per polygon and time.
}
\details{
This functions converts the ATLANTIS output to a \code{data.frame}
  which can be processed in R.
}
\examples{
# Set up the example with input files
d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
fgs <- load_fgs(d, "Functional_groups.csv")
bps <- load_bps(dir = d, fgs = "Functional_groups.csv",
  file_init = "Initial_condition.nc")
runprm <- load_runprm(d, "Run_settings.xml")
biolprm <- load_biolprm(dir = d,
  file_biolprm = "Biology.prm")
boxes <- get_boundary(load_box(dir = d, file_bgm = "Geography.bgm"))

# Get the pool values
pooln <- load_nc(dir = d, file_nc = "outputs.nc",
  bps = bps, fgs = fgs, select_groups = fgs[fgs$IsTurnedOn > 0, "Name"],
  select_variable = "N", check_acronyms = TRUE, bboxes = boxes)

biomasspools <- calc_biomass_pool(pooln = pooln, biolprm = biolprm)

}
\seealso{
Other calc functions: 
\code{\link{calc_Z}()},
\code{\link{calc_age2length}()},
\code{\link{calc_avgwtstage2age}()},
\code{\link{calc_biomass_age}()},
\code{\link{calc_pred_cons}()},
\code{\link{calc_pred_diet}()},
\code{\link{calc_stage2age}()},
\code{\link{calc_timestep2time}()}
}
\author{
Alexander Keth modified by Sarah Gaichas
}
\concept{calc functions}
\keyword{gen}
