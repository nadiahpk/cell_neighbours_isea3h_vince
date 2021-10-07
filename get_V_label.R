# just a quick function for turning a row with elements h1, h2, h3
# and v1, v2, v3 into a string that can be used for printing


get_V_label <- function( v1, v2, v3, h1, h2, h3 ) {

    v <- c( v1, v2, v3 )
    h <- c( h1, h2, h3 )
    l <- paste( c( paste(sort(v),collapse=","), '\n', paste(h[order(v)],collapse=",") ), collapse="")


    return( l )
}
