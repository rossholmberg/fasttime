ftime <- function(x, required.components = 3L) {
    if( !is.character(x) ) {
        x <- as.character(x)
    }
    
    out <- .Call(parse_time, x, required.components)
    attr(out, "class") <- c( "ftime", "times" )
    attr(out, "format") <- "h:m:s"
    out
    
}

