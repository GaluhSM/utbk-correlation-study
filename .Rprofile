source("renv/activate.R")

if (interactive()) {
  clean_tex <- function(name = "main") {
    exts <- c("aux", "bcf", "bbl", "blg", "run.xml", "log", "out", "toc", "lof", "lot", "synctex.gz")
    files <- paste0(name, ".", exts)
    file.remove(files[file.exists(files)])
  }
}
