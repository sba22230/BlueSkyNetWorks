# _common.R
# Load knitr
library(knitr)
library(here)
# Return a path relative to the project root detected by here.
here_parent <- function(...) {
  root <- here::here()
  if (!file.exists(file.path(root, "0_functions.R"))) {
    root <- file.path(root, "..")
  }
  normalizePath(file.path(root, ...), winslash = "/", mustWork = FALSE)
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
