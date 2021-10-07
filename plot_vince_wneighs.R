# the purpose of this function is to quickly plot a Vince-indexed grid
# and maybe I'll add neighbourhoods next

plot_vince <- function(res, neighs_in_red=NULL){

    # for now - hardcode in some seqnums to find the neighbours of
    # ---

    neighs_in_red <- c(56, 202, 241, 147, 123) # for resolution three

    #                  pent, edg niegh pent, edge, mid, edge, upper,
    #neighs_in_red <- c( 326,            659,  700, 602,  200,   181, 278, 412)

    # obtain the basic grid info
    # ---

    # create the grid
    dggs <- dgconstruct(res=res, pole_lat_deg = 90, pole_lon_deg = 0) 
    nocells <- dgmaxcell(dggs)
    q2di <- dgSEQNUM_to_Q2DI(dggs,1:nocells)

    # convert the Q2DI indexing to Vince and get the labels
    df_v <- Q2DI_to_V(q2di, res)
    label <- mapply( get_V_label, df_v$v1, df_v$v2, df_v$v3, df_v$h1, df_v$h2, df_v$h3 )
    qgrid <- data.frame(label)

    # get the lat-long so we can plot it properly
    cellcentres <- dgSEQNUM_to_GEO(dggs, 1:nocells)
    qgrid$cent_long <- cellcentres$lon_deg
    qgrid$cent_lat <- cellcentres$lat_deg


    # find the neighbours
    # ---

    red_grid_idxs <- c() # where I'll store their indices

    for (i in neighs_in_red) {

        neighs_i <- find_neighbours(res, df_v[i,]$v1, df_v[i,]$v2, df_v[i,]$v3, df_v[i,]$h1, df_v[i,]$h2, df_v[i,]$h3)

        for ( j in 1:length(neighs_i) ) {

            neigh <- neighs_i[j,]
            new_red <- as.numeric(rownames(subset(df_v, match(v1, neigh$v1) & match(v2, neigh$v2) & match(v3, neigh$v3) & match(h1, neigh$h1) & match(h2, neigh$h2) & match(h3, neigh$h3) )))
            red_grid_idxs <- union(red_grid_idxs, new_red)

        }

    }

    red_grid <- dgcellstogrid(dggs, red_grid_idxs, frame=T, wrapcells=T)

    # the boundries of the polygons for each cell
    black_grid_idxs <- setdiff(1:nocells, red_grid_idxs)
    black_grid <- dgcellstogrid(dggs, black_grid_idxs, frame=T, wrapcells=T)


    # plot the basic grid
    # ---

    # map of the world to put under it
    countries <- map_data("world")

    # creat plot
    p <- ggplot() +
         geom_path   (data=black_grid, aes(x=long, y=lat, group=group), color="black") +         # draw each hexagon
         geom_path   (data=red_grid,   aes(x=long, y=lat, group=group), size=2, color="red") +
         geom_polygon(data=countries,  aes(x=long, y=lat, group=group), color="blue", fill=NA) + # draw country boundaries
         geom_label  (data=qgrid,      aes(x=cent_long, y=cent_lat, label=label)) +              # draw label at cell centres
         geom_text() +
         coord_equal()

    # send plot to pdf
    
    # sort out plot size
    if (res == 2) { 
        plot_size <- 20 
    } else if (res == 4) {
        plot_size <- 50 
    } else {
        plot_size <- 15*res # just a guess
    }

    file_name <- paste('res_', as.character(res), '_vince.pdf', sep="")
    pdf(file_name, width=plot_size, height=plot_size)
    print(p)
    dev.off()

    return( df_v )
}

