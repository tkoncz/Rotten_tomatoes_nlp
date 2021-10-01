source("setup_tools.R")
suppressPackageStartupMessages(sapply(
    parseRequirements(),
    function(x) library(x[1], character.only = TRUE)
))
invisible(sapply(list.files("R", full.names = TRUE, pattern = "*.R"), source))
