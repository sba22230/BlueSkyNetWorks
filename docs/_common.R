# _common.R
library(knitr)
# Load the here package
library(here)
# Drop-in replacement: always returns parent of here() root
here_parent <- function(...) {
  # Get the current here() root
  root <- here::here()
  
  # Go one level up
  parent_root <- normalizePath(file.path(root, ".."), winslash = "/", mustWork = FALSE)
  
  # Append any extra path components passed in (...)
  if (nargs() > 0) {
    return(file.path(parent_root, ...))
  } else {
    return(parent_root)
  }
}

# Always set root.dir to the Quarto project root
# normalizePath() ensures a clean, absolute path
workingDir <- normalizePath(here_parent())

opts_chunk$set(root.dir = workingDir)

# Optional: confirm during render
message("knitr root.dir set to: ", opts_chunk$get("root.dir"))




# --- Examples ---

# Just the parent root
#here_parent()
#> "/path/to/project"

# Path to a file in the parent root
#here_parent("myfile.csv")
#> "/path/to/project/myfile.csv"

# Path to a subfolder in the parent root
#here_parent("data", "dataset.csv")
#> "/path/to/project/data/dataset.csv"
