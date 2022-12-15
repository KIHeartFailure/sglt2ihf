
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

# Get map data ------------------------------------------------------------

swedenmap <- raster::getData("GADM", country = "SWE", level = 1)

saveRDS(swedenmap, file = "./data/swedenmap.rds")


# LM data -----------------------------------------------------------------

lmsglt2 <- haven::read_sas("./raw-data/lmsglt2.sas7bdat")
save(
  file = "./data/lmsglt2.RData",
  list = c(
    "lmsglt2"
  )
)
