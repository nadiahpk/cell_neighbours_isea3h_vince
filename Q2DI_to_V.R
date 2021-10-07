#' @title Quad number and (i,j) coordinates to Vince-like index
#'
#' @description Converts the index from quad number and (i,j) 
#'  coordinates to a system based on Vince (2006, J. Vis. Commun. 
#'  Image R).
#'
#' @param q2di List with named elements: quad, i, and j.
#'
#' @details TODO
#'
#' @param TODO
#'
#' @export

Q2DI_to_V <- function(q2di, res=NULL){


    # check that the user gave us lists with the required elements and turn q2di into dataframe
    # ---

    if ( !exists('quad', where=q2di) ) {
        stop( "Input list needs an element called quad" )
    }
    if ( !exists('i', where=q2di) ) {
        stop( "Input list needs an element called i" )
    }
    if ( !exists('j', where=q2di) ) {
        stop( "Input list needs an element called j" )
    }

    # todo-check lengths

    # turn q2di into a dataframe
    df <- data.frame(q2di)
    df$seqnum <- 1:nrow(df)     # need to store these because the merge later will mess up the order


    # obtain the max index in the Vince system
    # ---

    if ( is.null(res) ){
        M <- max(q2di$j)+1
    } else {
        M <- 3^( (res+1) %/% 2 )
    }


    # define the dataframe that maps from quad number 
    # (1-10 inclusive) and triangle_type ("u" for upper or "l" for 
    # lower) to the ordered triple of Hamiltonian-numbered vertices
    # ---
    
    # quad number from dggridr
    quad     <- c(  1,   2,   3,   4,   5,   6,   7,   8,   9,  10,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10)

    # whether we're in the upper part of the quad or the lower part
    tri_type <- c('u', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'l', 'l', 'l', 'l', 'l', 'l', 'l', 'l', 'l', 'l')

    # the Hamiltonian-numbered vertices of the triangle formed of the upper or lower part of the quad
    v1       <- c(  1,   2,   3,  11,  10,   5,   4,   7,   8,   9,   1,   2,   3,  11,  10,   5,   4,   7,   8,   9)
    v2       <- c(  2,   3,  11,  10,   1,   4,   7,   8,   9,   5,   2,   3,  11,  10,   1,   4,   7,   8,   9,   5)
    v3       <- c(  0,   0,   0,   0,   0,   2,   3,  11,  10,   1,   5,   4,   7,   8,   9,   6,   6,   6,   6,   6)

    map <- data.frame( quad, tri_type, v1, v2, v3 )


    # merge the Hamiltonian-numbered vertices of the triangles into df
    # ---

    # first, ID each cell as a member of either an upper or lower triangle
    df$tri_type <- ifelse( df$i <= df$j, 'u', 'l' )

    # perform the merge with the mapping, leaving NAs for those that don't get a match
    df <- merge(df, map, by.x=c('quad', 'tri_type'), by.y=c('quad','tri_type'), all=TRUE)


    # sort out the edges
    # ---

    # those who have i==0 or j==0 have their v2 set to zero
    df[which( df$i == 0 | df$j == 0 ),]$v2 <- NA

    # those who have i==j have their v3 set to zero
    df[which( df$i == df$j ),]$v3 <- NA
    

    # fix up the north and south poles, they are special cases in dggridr indexing scheme
    # ---

    # north pole, having (q,i,j) = (0,0,0)
    df[which(df$quad == 0 & df$i == 0 & df$j == 0),]$v1 <- 0

    # south pole, having (q,i,j) = (11,0,0)
    df[which(df$quad == 11 & df$i == 0 & df$j == 0),]$v1 <- 6


    # allocate the v1, v2, v3 according to the Vince scheme
    # ---

    df$h3 <- abs(df$j-df$i)
    df$h2 <- ifelse( df$tri_type == 'u', df$i, df$j )
    df$h1 <- M - df$h2 - df$h3


    # return dataframe in order according to seqnum
    # ---

    df <- df[order(df$seqnum),] 
    rownames(df) <- c()
    df$seqnum <- NULL

    # sort in ascending vertex order
    # ---

    df_sorted <- df

    for ( i in 1:nrow(df) ) {

        row <- df[i,]

        v <- c(row$v1, row$v2, row$v3)
        h <- c(row$h1, row$h2, row$h3)

        h <- h[order(v)]
        v <- v[order(v)]

        df_sorted[i,]$v1 <- v[1]; df_sorted[i,]$v2 <- v[2]; df_sorted[i,]$v3 <- v[3]
        df_sorted[i,]$h1 <- h[1]; df_sorted[i,]$h2 <- h[2]; df_sorted[i,]$h3 <- h[3]

    }

    return( df_sorted )

}
