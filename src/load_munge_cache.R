# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = TRUE,
  munging = TRUE
)

ProjectTemplate::cache("meta_variables")

ProjectTemplate::cache("flow")
ProjectTemplate::cache("rsdata")
ProjectTemplate::cache("imprsdata")
ProjectTemplate::cache("matchrsdata")

ProjectTemplate::cache("rsdata_rec")
ProjectTemplate::cache("matchrsdata_rec")

ProjectTemplate::cache("matchcross_deathcvhosphf")
ProjectTemplate::cache("matchcross_deathcv")
ProjectTemplate::cache("matchcross_hosphf")
ProjectTemplate::cache("matchcross_death")
ProjectTemplate::cache("matchcross_hosprenal")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("matchingn")
ProjectTemplate::cache("overtime")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("stratavars")
ProjectTemplate::cache("modvarsstrata")