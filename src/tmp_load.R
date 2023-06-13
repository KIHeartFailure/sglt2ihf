
# run 03-06
ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = FALSE,
  munging = FALSE
)

load(paste0(datapath, "rsdata_rs.RData"))
load(paste0(datapath, "rawData_sosdors.RData"))
load(paste0(datapath, "rawData_scb.RData"))

source("./munge/03-endtime.R")
source("./munge/04-selection_swedehf.R")
source("./munge/05-child_scb.R")
source("./munge/06-lisa_scb.R")

save(
  file = "./data/tmprsdata1.RData",
  list = c("rsdata", "flow")
)

# run 07

ProjectTemplate::reload.project()
load(file = "./data/tmprsdata1.RData")
load(paste0(datapath, "/rawData_sossv.RData"))

load(paste0(datapath, "/rawData_sosov.RData"))

source("./munge/07-prep_nprdata.R")

save(file = paste0("./data/patreg.RData"), list = c("patreg"))

# run 08-09
ProjectTemplate::reload.project()

load(file = "./data/tmprsdata1.RData")
load(paste0("./data/patreg.RData"))

source("./munge/08-outcom_sos.R")
source("./munge/09-charlsoncomorbindex_sos.R")

# run 10
source("./munge/10-recat.R")

# run 11
source("./munge/11-create_subset.R")

save(
  file = "./data/tmprsdata2.RData",
  list = c("rsdata", "flow", "outcommeta", "ccimeta")
)

# run 12
ProjectTemplate::reload.project()

load("./data/tmprsdata2.RData")
load("./data/lmsglt2.RData")

source("./munge/12-lmvar.R")

save(
  file = "./data/tmprsdata3.RData",
  list = c("rsdata", "flow", "outcommeta", "ccimeta", "metalm", "overtime", "overtime_subset")
)

# run 02, 13
ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = FALSE,
  munging = FALSE
)

meta_variables <- read.xlsx("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/metadata/meta_variables.xlsx",
  sheet = "Sheet 1"
)

load("./data/tmprsdata3.RData")

source("./munge/02-vars.R")
source("./munge/13-impute.R")


# save

ProjectTemplate::cache("meta_variables")

ProjectTemplate::cache("flow")
ProjectTemplate::cache("rsdata")
ProjectTemplate::cache("imprsdata")
ProjectTemplate::cache("imprsdata_subset")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("outcommeta")
ProjectTemplate::cache("ccimeta")

ProjectTemplate::cache("overtime")
ProjectTemplate::cache("overtime_subset")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
