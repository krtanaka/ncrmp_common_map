select_region <- function(region_name) {
  result <- switch(
    region_name,
    "S.MARIAN" = list(islands = c("gua", "rot", "sai", "tin", "agu"), region = "S.MARIAN"),
    "N.MARIAN" = list(islands = c("agr", "ala", "ana", "asc", "gug", "fdp", "mau", "sar", "sup", "pag", "zea"), region = "N.MARIAN"),
    "SAMOA" = list(islands = c("ofu", "ros", "swa", "tau", "tut"), region = "SAMOA"),
    "PRIAs" = list(islands = c("bak", "how", "jar", "joh", "kin", "pal", "wak"), region = "PRIAs"),
    "MHI" = list(islands = c("haw", "kah", "kal", "kau", "lan", "mai", "mol", "nii", "oah"), region = "MHI"),
    "NWHI" = list(islands = c("ffs", "kur", "lay", "lis", "mar", "mid", "phr"), region = "NWHI"),
    stop("Invalid region name. Please specify a valid region.")
  )
  
  # Return the result
  return(result)
  cat("Region =", result[[1]], "\nIslands =", result[[2]], "\n")
  
}


library(geosphere)
remove_close_points <- function(data, threshold = 100) {
  keep <- rep(TRUE, nrow(data))
  for (i in 1:(nrow(data) - 1)) {
    if (keep[i]) {
      dists <- distm(data[i, c("longitude", "latitude")], data[(i+1):nrow(data), c("longitude", "latitude")], fun = distHaversine)
      close_points <- which(dists < threshold)
      if (length(close_points) > 0) {
        keep[(i + close_points)] <- FALSE
      }
    }
  }
  data[keep, ]
}

# Function to generate new unique IDs
# Define a function to generate unique IDs starting from a smaller number
generate_new_ids <- function(original_ids, existing_ids) {
  max_id <- max(original_ids) # Find the largest number in the original IDs
  new_ids <- seq(max_id + 1, max_id + length(original_ids)) # Generate new IDs
  replacement_ids <- setdiff(new_ids, existing_ids) # Remove conflicts from new IDs
  
  # Replace conflicts
  updated_ids <- sapply(original_ids, function(x) {
    if (x %in% existing_ids) {
      replacement <- replacement_ids[1]
      replacement_ids <<- replacement_ids[-1] # Remove used ID
      return(replacement)
    } else {
      return(x)
    }
  })
  
  return(sort(updated_ids))
}
