print.ftime <- function(x, ...) {
    
    # make a copy of the input to return at the end
    xo <- x
    
    # read input attributes
    att <- attributes(x)
    
    # remove some attributes
    att$class <- att$format <- att$origin <- NULL
    
    # make sure the input is numeric
    if( !is.numeric(x) ) {
        x <- as.numeric(x)
    }

    # get the number of seconds since midnight
    sec <- round(86400 * abs(x))
    
    # use that to get other values
    ms <- round((x - sec) * 1000)
    hh <- sec %/% 3600
    mm <- (sec - hh * 3600) %/% 60
    ss <- trunc(sec - hh * 3600 - 60 * mm)
    
    # Add leading zeros as necessary
    hh <- formatC( hh, width = 2, flag = "0" )
    mm <- formatC( mm, width = 2, flag = "0" )
    ss <- formatC( ss, width = 2, flag = "0" )
    ms <- formatC( ms, width = 3, flag = "0" )
    
    # remove trailing zeros from milliseconds
    ms <- sub( "0+$", "", ms )
    
    # add formatting, including milliseconds if appropriate
    out <- ifelse( ms > 0,
                   paste0( hh, ":", mm, ":", ss, ".", ms ),
                   paste0( hh, ":", mm, ":", ss )
    )
    
    # print as negative time if input was negative
    if(any(x[!nas] < 0))
        out <- paste(ifelse(x < 0, "-", " "), out, sep = "")
    
    # fill nas where appropriate
    out[nas] <- NA
    
    # fill infinite values where appropriate
    out[x == Inf] <- "Inf"
    out[x ==  - Inf] <- "-Inf"
    
    # redefine attributes
    attributes(out) <- att
    
    x <- out
    NextMethod("print", quote = FALSE)
    
    # invisibly return the input object
    invisible(xo)
}
