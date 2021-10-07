
find_opposite_vertices <- function(edge){

    v1 <- edge[1]   # treat this vertex as the focal vertex
    v2 <- edge[2]

    # neighbourhood vertex operations depend on focal vertex's type
    if ( v1 %% 2 == 0 ) {
        s <- c( 10, 1, 2, 3, 11 )
    } else {
        s <- c( 1, 4, 8, 9, 11 )
    }

    # which operation in s do we use to get from v1 to v2?
    s_op <- (v2-v1) %% 12
    s_idx <- which( s == s_op )

    # the opposite vertices are plus 1 and minus 1 operations
    o1 <- ( v1 + s[ s_idx %% 5 + 1 ] ) %% 12
    o2 <- ( v1 + s[ (s_idx-2) %% 5 + 1 ] ) %% 12

    return( c(o1,o2) )

}

# find the neighbours of a cell given the centre cell (h1, h2, h3, v1, v2, v3) and the resolution
find_neighbours <- function(res, focal_v1, focal_v2, focal_v3, focal_h1, focal_h2, focal_h3){

    # calculate the max index in the hexagons
    M <- 3^as.integer((res+1)/2)

    s_0 <- c( 10, 1, 2, 3, 11 ) # even
    s_1 <- c( 1, 4, 8, 9, 11 )  # odd


    # prepare a space to put the neighbours
    neighs <- data.frame (matrix(ncol=6,nrow=0) ); colnames(neighs) <- c('v1', 'v2', 'v3', 'h1', 'h2', 'h3')

    # find neighbours
    # ===

    if ( ( focal_h3 == 0 ) & ( focal_h2 == 0 ) ) { 
        
        # pentagon rules
        # ---

        if ( focal_v1 %% 2 == 0 ) { s_v <- s_0 } else { s_v <- s_1 }

        if (res %% 2 == 0) {

            # hexagons are always on an edge with a neighbouring vertex

            h <- c(M-1, 1, 0)

            v2s <- (focal_v1+s_v) %% 12   # a list of adjacent vertices

            for (v2 in v2s) {

                v <- c(focal_v1, v2, NA) # NA bc on edge

                # append to the neighbours dataframe
                df <- data.frame(matrix( c(v,h), nrow=1 ))
                colnames(df) <- colnames(neighs)
                neighs <- rbind(neighs, df)

            }

        } else { # if focal_v1 is odd

            # hexagons will be non-edge, need to find neighbouring faces

            h <- c(M-2, 1, 1)

            for (i in 1:length(s_v) ){

                # get the pair of neighbouring vertices
                v2 <- (focal_v1 + s_v[i]) %% 12
                v3 <- ( focal_v1 + s_v[ i %% 5 + 1 ] ) %% 12 # (tru_idx + 1) %% 5 + 1
                v <- c(focal_v1, v2, v3)

                # append to the neighbours dataframe
                df <- data.frame(matrix( c(v,h), nrow=1 ))
                colnames(df) <- colnames(neighs)
                neighs <- rbind(neighs, df)

            }

        } # if (focal_v1 %% 2 == 0) {

    } else if ( focal_h3 == 0 ) { # nb ordering so always last one alone 0
        
        # 0-edge rules
        # ---

        v3s <- find_opposite_vertices( c(focal_v1, focal_v2) ) # find opposite vertices of v1, v2 edge, this will be the new v3

        # vertex neighbour rules that get applied twice, once for each new v3
        if ( res %% 2 == 0 ){ # even rules for operations on the cells

            h_opsM <- matrix( c( -1,  0, +1,  0, -1, +1), ncol=3, byrow=T )

        } else { # odd rules for operations on the cells

            h_opsM <- matrix( c( -2, +1, +1, +1, -2, +1, -1, -1, +2 ), ncol=3, byrow=T )

        }

        # apply rules once to each v3
        for (v3 in v3s) {

            for (i in 1:nrow(h_opsM)){

                h_ops <- h_opsM[i,]

                v <- c( focal_v1, focal_v2, v3 )
                h <- c( focal_h1, focal_h2, focal_h3 ) + h_ops

                # append to the neighbours dataframe
                df <- data.frame(matrix( c(v,h), nrow=1 ))
                colnames(df) <- colnames(neighs)
                neighs <- rbind(neighs, df)

            }

        }

        # the neighbouring edges that get applied only once each
        if ( res %% 2 == 0 ) { 

            v <- c( focal_v1, focal_v2, focal_v3 )

            h <- c( focal_h1, focal_h2, focal_h3 ) + c( +1, -1, 0)

            # append to the neighbours dataframe
            df <- data.frame(matrix( c(v,h), nrow=1 ))
            colnames(df) <- colnames(neighs)
            neighs <- rbind(neighs, df)

            h <- c( focal_h1, focal_h2, focal_h3 ) + c( -1, +1, 0)

            # append to the neighbours dataframe
            df <- data.frame(matrix( c(v,h), nrow=1 ))
            colnames(df) <- colnames(neighs)
            neighs <- rbind(neighs, df)

        } 

    } else { # probably a face neighbourhood

        # general rules
        # ---

        # vertex neighbour rules
        if ( res %% 2 == 0 ){ # even rules for operations on the cells

            h_opsM <- matrix( c( +1, -1,  0, -1, +1,  0, +1,  0, -1, -1,  0, +1,  0, +1, -1,  0, -1, +1), nrow=6, ncol=3, byrow=T )

        } else { # odd rules for operations on the cells

            h_opsM <- matrix( c( +2, -1, -1, -2, +1, +1, -1, +2, -1, +1, -2, +1, -1, -1, +2, +1, +1, -2), nrow=6, ncol=3, byrow=T )

        }

        # find neighbours for each row
        for (i in 1:nrow(h_opsM)){

            h_ops <- h_opsM[i,]

            v <- c( focal_v1, focal_v2, focal_v3 )
            h <- c( focal_h1, focal_h2, focal_h3 ) + h_ops

            # for edge crossings in odd resolution
            if ( any(h<0) ){

                lost_v_idx <- which(h<0)    # which axis vertex gets swapped
                lost_v <- v[lost_v_idx]

                edge_idxs <- which(h>=0)    # the crossing edge
                edge <- v[edge_idxs]

                # what vertex replaces lost vertex?
                opp_vs <- find_opposite_vertices( edge )
                new_v <- opp_vs[ which(opp_vs != lost_v) ]

                # construct the new v
                v[lost_v_idx] <- new_v

                # revert to old h
                h <- c( focal_h1, focal_h2, focal_h3 )

            }

            # append to the neighbours dataframe
            df <- data.frame(matrix( c(v,h), nrow=1 ))
            colnames(df) <- colnames(neighs)
            neighs <- rbind(neighs, df)

        }
    }

    # order neighbour indices and removed unused axis vertices so indices are unique
    # ===

    for ( i in 1:nrow(neighs) ){

        neigh <- neighs[i,]

        h <- c(neigh$h1, neigh$h2, neigh$h3)
        v <- c(neigh$v1, neigh$v2, neigh$v3)

        # any hexagon index that's equal to a zero has its vertex index set to NA
        if ( any(h==0) ){ v[ which(h==0) ] <- NA }

        # order the h's in ascending order wrt v
        h <- h[order(v)]
        v <- v[order(v)]

        # assign it back into neighs in correct order
        neighs[i,]$v1 <- v[1]; neighs[i,]$v2 <- v[2]; neighs[i,]$v3 <- v[3]
        neighs[i,]$h1 <- h[1]; neighs[i,]$h2 <- h[2]; neighs[i,]$h3 <- h[3]

    }


    return( neighs )

}
