
fposx <- function(x, required.components = 3L) {

    if( is.character( x ) ) {
        out <- .POSIXct( xx = .Call(parse_ts,
                                    x,
                                    required.components),
                         tz = "UTC" )
    } else if( is.factor( x ) ) {
        out <- .POSIXct( xx = .Call(parse_ts,
                                    as.character(x),
                                    required.components),
                         tz = "UTC" )
    } else if( inherits( x, "POSIXct" ) ) {
        if( attr( x, "tzone" ) == "UTC" ) {
            out <- x
        } else if( attr( x, "tzone" ) == "GMT" ) {
            out <- x
            attr( out, "tzone" ) <- "UTC"
            warning( "`tzone` attribute changed from GMT to UTC. This should not cause any shift." )
        } else {
            stop( "To convert to fposx, the timezone MUST be UTC." )
        }
    } else if( is.numeric( x ) ) {
        out <- as.POSIXct.numeric( xx = x,
                                   tz = "UTC",
                                   origin = "1970-01-01 00:00:00" )
    } else {
        stop( "Don't know how to convert to fposx" )
    }
    
    attr( out, "class" ) <- c( "fposx", class( out ) )
    
    out

}
