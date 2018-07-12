
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
    } else if( is.numeric( x ) ) {
        out <- .POSIXct( xx = x, tz = "UTC" )
    } else {
        stop( "Don't know how to convert to fposx" )
    }
    
    attr( out, "class" ) <- c( "fposx", class( out ) )
    
    out

}
