
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

# Get map data ------------------------------------------------------------

swedenmap <- raster::getData("GADM", country = "SWE", level = 1)

saveRDS(swedenmap, file = "./data/swedenmap.rds")