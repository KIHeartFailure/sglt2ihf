
meta_variables <- read.xlsx("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/metadata/meta_variables.xlsx", sheet = "Sheet 1")

load(paste0(datapath, "rsdata_rs.RData"))
load(paste0(datapath, "rawData_scb.RData"))
load(paste0(datapath, "rawData_sosdors.RData"))
load(paste0(datapath, "patregrsdata.RData"))
load(paste0(datapath, "lmswedehf.RData"))

lmsel <- lmswedehf %>%
  mutate(atcneed = stringr::str_detect(ATC, "^A10")) %>%
  filter(
    ANTAL >= 0,
    atcneed
  )

rm(lmswedehf)
