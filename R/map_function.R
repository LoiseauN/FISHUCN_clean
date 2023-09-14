

#Add graticules function 

geom_graticules <- function(crs) {
  
  grat <- sf::st_graticule(crs = crs, 
                           lon = seq(-180, 180, by = 40), 
                           lat = seq( -90,  90, by = 30))
  
  geom_sf(data = grat, color = "#bebebe", size = 0.1)
}



st_mapframe <- function(meridians = seq(-180, 180, 40), 
                        parallels = seq(-90, 90, 30), 
                        crs = get_proj4("rob")) {
  
  if (is.null(crs)) {
    crs <- get_proj4(options()$"proj")
  }
  
  #if (!is.character(crs) || length(crs) != 1) {
  #  stop("Argument 'crs' must be a character of length 1")
  #}
  
  
  ## Store user options ----
  
  warn <- options()$"warn"
  on.exit(options(warn = warn))
  
  
  ## Disable warnings ----
  
  options(warn = -1)
  
  
  ## Create bounding box ----
  
  bbox <- c("xmin" = min(meridians), "ymin" = min(parallels), 
            "xmax" = max(meridians), "ymax" = max(parallels))
  
  bbox <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(bbox)))
  bbox <- sf::st_set_crs(bbox, 4326)
  
  bbox <- as(bbox, "Spatial")
  
  
  ## Create graticules ----
  
  grid <- sp::gridlines(bbox, easts  = meridians, norths = parallels)
  
  
  # Transform graticules from SpatialLines to a data table ----
  
  grid_dt <- ggplot2::map_data(sp::SpatialLinesDataFrame(
    sl = grid, data = data.frame(1:length(grid)), match.ID = FALSE))
  
  
  ## Project coordinates ----
  
  grid_dt$"X" <- rgdal::project(cbind(grid_dt$"long", grid_dt$"lat"),
                                proj = crs)[ , 1]
  
  grid_dt$"Y" <- rgdal::project(cbind(grid_dt$"long", grid_dt$"lat"),
                                proj = crs)[ , 2]
  
  
  ## Keep only the box ----
  
  sub_set <- which(
    (grid_dt$"long" %in% c(-180, 180) & grid_dt$"region" == "NS") | 
      (grid_dt$"long" %in% c(-180, 180) & grid_dt$"lat" %in% c(-90,90) & 
         grid_dt$"region" == "EW"))
  
  grid_dt[sub_set, ]
}

geom_mapframe <- function(crs, linetype = "solid", colour = "black", 
                          size = 0.4) {
  
  bbox <- st_mapframe(crs = crs)
  
  geom_path(data = bbox, aes(x = X, y = Y, group = group), 
            linetype = linetype, colour = colour, size = size)
}
