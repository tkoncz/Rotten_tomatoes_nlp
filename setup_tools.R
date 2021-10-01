installMissingPackages <- function() {
    missingPackages <- getMissingPackages()
    install.packages(missingPackages)
}

getMissingPackages <- function() {
    Filter(
        x = parseRequirements(),
        f = function(x) !(x[1] %in% installed.packages()[,"Package"])
    )
}

parseRequirements <- function() {
    lines <- trimws(dropComments(readLines("packages.txt")))
    nonemptyLines <- lines[lines != ""]
    nonemptyLines
}

dropComments <- function(str) gsub("(#.*$)", "", str)
