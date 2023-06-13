# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = FALSE,
  munging = TRUE
)

ProjectTemplate::cache("meta_variables")

ProjectTemplate::cache("flow")
ProjectTemplate::cache("rsdata")
ProjectTemplate::cache("imprsdata")
ProjectTemplate::cache("imprsdata_subset")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("overtime")
ProjectTemplate::cache("overtime_subset")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")