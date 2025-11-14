intertomain <- function(interNameList, p = NULL) {
    ### convert an interaction name list into the corresponding main effect for
    ### enforcing strong hierachy.
    if (length(interNameList) == 0) {
        return(integer())
    }
    if (is.null(p)) {
        idx <- unlist(lapply(interNameList, function(interName) {
            as.numeric(strsplit(interName, "X")[[1]][2:3])
        }))
        p <- max(idx, na.rm = TRUE)
    }
    mainInd <- rep(0, p)
    for (i in seq_along(interNameList)) {
        interName <- interNameList[[i]]
        pair <- as.numeric(strsplit(interName, "X")[[1]][2:3])
        mainInd[pair[1]] <- 1
        mainInd[pair[2]] <- 1
    }
    which(mainInd == 1)
}
