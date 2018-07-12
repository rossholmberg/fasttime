
as.POSIXct.fposx <- function( x, tz ) {
    
    # remove the fposx class attribute
    attr( x, "class" ) <- class( x )[ class(x) != "fposx" ]
    
    # modify the timezone attribute if requested
    if( !missing( tz ) ) {
        attr( x, "tzone" ) <- tz
    }
    
    x
    
}
