
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
source("./munge/05-countryofbirth_child_scb.R")
source("./munge/06-lisa_scb.R")

save(
  file = "./data/tmprsdata1.RData",
  list = c("rsdata", "flow")
)


# run 07-08
ProjectTemplate::reload.project()

load(file = "./data/tmprsdata1.RData")
load(paste0(datapath, "patregrsdata.RData"))

source("./munge/07-outcom_sos.R")
source("./munge/08-charlsoncomorbindex_sos.R")

# run 09
source("./munge/09-recat.R")

save(
  file = "./data/tmprsdata2.RData",
  list = c("rsdata", "flow", "outcommeta", "ccimeta")
)

# run 10
ProjectTemplate::reload.project()

load("./data/tmprsdata2.RData")
load(paste0(datapath, "lmswedehf.RData"))

lmsel <- lmswedehf %>%
  mutate(atcneed = stringr::str_detect(ATC, "^A10")) %>%
  filter(
    ANTAL >= 0,
    atcneed
  )

rm(lmswedehf)

source("./munge/10-lmvar.R")

save(
  file = "./data/tmprsdata3.RData",
  list = c("rsdata", "flow", "outcommeta", "ccimeta", "metalm", "overtime")
)

# run 02, 11
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
source("./munge/11-impute.R")


# save

ProjectTemplate::cache("meta_variables")

ProjectTemplate::cache("flow")
ProjectTemplate::cache("rsdata")
ProjectTemplate::cache("imprsdata")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("outcommeta")
ProjectTemplate::cache("ccimeta")

ProjectTemplate::cache("overtime")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
