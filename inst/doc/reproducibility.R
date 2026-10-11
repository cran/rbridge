## ----guide, echo=FALSE, results='asis'----------------------------------------
guide <- readLines("../inst/doc/reproducibility.md", warn = FALSE)
knitr::asis_output(paste(guide[-1L], collapse = "\n"))

