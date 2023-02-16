base_url <- function() "https://api.phylopic.org/images/"

get_phylopic_image <- function(uuid, size = "512") {
  
  if (missing(uuid)) {
    stop("You must provide an uuid", .call = FALSE)
  }
  
  if (!is.character(uuid) || length(uuid) != 1) {
    stop("Argument 'uuid' must be a character of length 1", .call = FALSE)
  }
  
  page <- httr::GET(paste0(base_url(), uuid))
  
  if (page$"status_code" != 200) {
    stop("Unable to reach the page", .call = FALSE)
  }
  
  content <- httr::content(page, as = "text")
  content <- jsonlite::fromJSON(content)
  content <- content[[1]]
  
  images <- content$"rasterFiles"
  
  
  ## Get available sizes ----
  
  sizes <- unlist(lapply(strsplit(images$"sizes", "x"), function(x) x[1]))
  
  if (!(size %in% sizes)) {
    stop("Unavailable size", .call = FALSE)
  }
  
  
  ## Get image link ----
  
  pos <- which(sizes == size)
  
  link <- images[pos, "href"]
  
  filename <- tempfile()
  invisible(utils::download.file(url = link, destfile = filename, quiet = TRUE))
  
  png::readPNG(filename)
}


mammals_pic    <- get_phylopic_image("8cad2b22-30d3-4cbd-86a3-a6d2d004b201", 
                                     size = "512")

birds_pic      <- get_phylopic_image("34d9872c-b7d0-416f-8ac6-1f9f952982c8", 
                                     size = "512")

fish_pic       <- get_phylopic_image("86c40d81-2613-4bb4-ad57-fb460be56ae5", 
                                     size = "512")

amphibians_pic <- get_phylopic_image("cd0cdc36-ecfa-414f-af87-1b5e0ec0c69b", 
                                     size = "512")

reptile_pic    <- get_phylopic_image("bf7d9c5f-83c0-435a-b09f-dc6111ece257", 
                                     size = "512")

