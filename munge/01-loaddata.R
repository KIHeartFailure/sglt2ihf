
meta_variables <- read.xlsx("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/metadata/meta_variables.xlsx", sheet = "Sheet 1")

load("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/data/v401/rsdata401.RData")

load("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/data/20220908/patregrsdata.RData")

load("C:/Users/Lina/STATISTIK/Projects/20210525_shfdb4/dm/data/20220908/lmswedehf.RData")

lmsel <- lmswedehf %>%
  mutate(atcneed = stringr::str_detect(ATC, "^A10")) %>%
  filter(
    ANTAL >= 0,
    atcneed
  )

rm(lmswedehf)
