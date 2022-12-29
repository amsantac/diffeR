composite <- function (comp, ref, factor){

    nRefCats <- length(unique(values(ref)))
    nCompCats <- length(unique(values(comp)))

    if (nCompCats != nRefCats) 
        stop("set of unique pixel values in input rasters must be equal")

    conf.matrix <- matrix(nrow = nCompCats, ncol = nRefCats)

    compAg <- memberships(comp, fact = factor)
    refAg <- memberships(ref, fact = factor)

    weights <- comp - comp + 1

    wghtAg <- aggregate(weights, fact = factor, fun = sum)

    sumDiagPix <- 0
    for (i in 1:nlyr(refAg)) {
        min.ii <- min(compAg[[i]], refAg[[i]])
        sumDiagPix <- sumDiagPix + min.ii
    }
    i = 1; j = 2
    for (i in 1:nlyr(compAg)) {
        minsi <- min(compAg[[i]], refAg[[i]])
        for (j in 1:nlyr(refAg)) {
            minsj <- min(compAg[[j]], refAg[[j]])
            Aij <- wghtAg * ((compAg[[i]] - minsi) * (refAg[[j]] - minsj))/(1 - sumDiagPix)
            sum.Aij <- global(Aij, fun = sum, na.rm = TRUE)[1, 1]
            conf.matrix[i, j] <- sum.Aij/(global(wghtAg, fun = sum, na.rm = TRUE)[1, 1])
        }
    }
    for (i in 1:nlyr(refAg)) {
        min.ii <- min(compAg[[i]], refAg[[i]]) * wghtAg
        sum.min.ii <- global(min.ii, fun = sum, na.rm = TRUE)[1, 1]
        conf.matrix[i, i] <- sum.min.ii/(global(wghtAg, fun = sum, na.rm = TRUE)[1, 1])
    }
    colnames(conf.matrix) <- paste("ref.", unique(values(ref)), sep = "")
    rownames(conf.matrix) <- paste("comp.", unique(values(comp)), sep = "")
    return(conf.matrix)
}
