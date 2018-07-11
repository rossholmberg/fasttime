print.times <- function(x, ...) {
    xo <- x
    ## print whole days (no fraction) as regular integers
    if(all(is.na(x)) || any(x[!is.na(x)] >= 1))
        cat("Time in days:\n")
    att <- attributes(x)
    nas <- is.na(x)
    att$class <- att$format <- att$origin <- NULL
    ## <NOTE>
    ## DJ's design is that
    ##   times greater than 1 day  should format like numerics
    ## To change this (e.g., have times(1.5) format as 36:00:00), simply
    ## comment the code below, and make the corresponding change in
    ## print.times().
    days <- abs(floor(x))
    if(any(days[!nas] > 0)) {
        attributes(x) <- att
        return(format(x))
    }
    ## </NOTE>
    x <- as.numeric(x)
    sec <- round(24 * 3600 * abs(x))
    ms <- round(((x * 86400) %% 1) * 1000)
    hh <- sec %/% 3600
    mm <- (sec - hh * 3600) %/% 60
    ss <- trunc(sec - hh * 3600 - 60 * mm)
    hh <- formatC( hh, width = 2, flag = "0" )
    mm <- formatC( mm, width = 2, flag = "0" )
    ss <- formatC( ss, width = 2, flag = "0" )
    ms <- formatC( ms, width = 3, flag = "0" )
    ms <- sub( "0+$", "", ms )
    out <- ifelse( ms > 0,
                   paste0( hh, ":", mm, ":", ss, ".", ms ),
                   paste0( hh, ":", mm, ":", ss )
    )
    if(any(x[!nas] < 0))
        out <- paste(ifelse(x < 0, "-", " "), out, sep = "")
    out[nas] <- NA
    out[x == Inf] <- "Inf"
    out[x ==  - Inf] <- "-Inf"
    attributes(out) <- att
    x <- out
    NextMethod("print", quote = FALSE)
    invisible(xo)
}

format.times <- function(x, ...) {
    
}
