## Project-level R startup for VS Code R sessions
## Ensure the VS Code-R init script gets sourced when opening R in this workspace.
if (
  interactive() &&
    Sys.getenv("RSTUDIO") == "" &&
    Sys.getenv("TERM_PROGRAM") == "vscode"
) {
  vsc_init <- file.path(Sys.getenv("USERPROFILE"), ".vscode-R", "init.R")
  if (file.exists(vsc_init)) {
    try(source(vsc_init), silent = TRUE)
  } else {
    message("VS Code R init script not found: ", vsc_init)
  }
}
