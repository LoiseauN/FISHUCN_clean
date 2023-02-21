get_phylopic_image <- function(uuid, size = 512) {
  
  
  ## Check args ----
  
  if (missing(uuid)) {
    stop("You must provide an UUID (argument 'uuid')", call. = FALSE)
  }
  
  if (!is.character(uuid) || length(uuid) != 1) {
    stop("Argument 'uuid' must be a character of length 1", call. = FALSE)
  }
  
  if (!is.vector(size) || length(size) != 1) {
    stop("Argument 'size' must be a vector ('character' or 'integer') of ", 
         "length 1", call. = FALSE)
  }
  
  
  ## Send request ----
  
  base_url <- "https://api.phylopic.org/images/"
  
  page <- httr::GET(paste0(base_url, uuid))
  
  if (page$"status_code" != 200) {
    stop("The resource cannot be found", call. = FALSE)
  }
  
  
  ## Extract response ----
  
  content <- httr::content(page, as = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content)
  content <- content[[1]]
  
  images <- content$"rasterFiles"
  
  
  ## Get available sizes ----
  
  sizes <- unlist(lapply(strsplit(images$"sizes", "x"), function(x) x[1]))
  
  if (!(size %in% sizes)) {
    stop("Size not found. Available sizes are: '", 
         paste(sizes, collapse = "', '"), "'.", call. = FALSE)
  }
  
  
  ## Get image link ----
  
  link <- images[which(sizes == size), "href"]
  
  
  ## Download png ----
  
  filename <- paste0(tempfile(), ".png")
  utils::download.file(url = link, destfile = filename, quiet = TRUE)
  
  
  ## Import raster ----
  
  img <- png::readPNG(filename, native = FALSE)
  
  
  ## Convert to RGBA (without background color) ----
  
  if (dim(img)[3] == 2) {
    
    img_rgb <- array(1, dim = c(dim(img)[1], dim(img)[2], 4))
    
    img_rgb[ , , 1] <- ifelse(img[ , , 2] == 0, 1, 0) # red
    img_rgb[ , , 2] <- ifelse(img[ , , 2] == 0, 1, 0) # green
    img_rgb[ , , 3] <- ifelse(img[ , , 2] == 0, 1, 0) # blue
    img_rgb[ , , 4] <- ifelse(img[ , , 2] == 0, 0, 1) # alpha
    
  } else {
    
    img_rgb <- img
  }
  
  img_rgb
}