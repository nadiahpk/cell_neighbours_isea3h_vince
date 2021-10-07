# Background

In order to model a spatially discretised process on the whole Earth, we need a discrete global grid system (DGGS). Ideally, it would divide the globe into a tessellation of some regular polygon, like a hexagon, with equal-distant centres so that the neighbourhood of a grid cell is well defined. Unfortunately, it's not mathematically possible to tessellate a sphere in this desirable way, and finding the best DGGS is a research area of its own.

One of the most popular discrete global grid systems, that comes closest to possessing the ideal properties, is the Icosahedral Snyder Equal Area Aperture 3 Hexagonal Grid (ISEA3H). Its base polyhedron is the icosahedron, with its triangular faces partitioned into hexagons, and a pentagon at each vertex. 

The general method for projecting the grid onto the Earth's surface is the Inverse Snyder Equal-Area Projection. The algorithm is quite involved, but fortunately, software exists to implement it and create the ISEA3H grid. DGGRID is a C++ command-line application that can generate a variety of icosahedral grids, including ISEA3H. A convenient R front-end for interacting with DGGRID has been written, dggridR.

# This code

The purpose of this code is to work with dggridR to quickly find the neighbours of each ISEA3H cell using a new indexing scheme. The scheme is adpated from Vince (2006). The code is at the just-get-something-working stage, and no attempt has been made to improveits efficiency.

For more details, see [this blog post](https://nadiah.org/ "nadiah.org")

# Quick start guide

## Install dggridR

The dggridR package has since been removed from CRAN, so you will need to install from the archive.
First, make sure you have its dependencey rgdal and devtools
```
    sudo apt-get install r-cran-rgdal
    sudo apt-get install r-cran-devtools
```

Then
```
    > library('devtools')
    Loading required package: usethis
    > install_url('https://cran.r-project.org/src/contrib/Archive/dggridR/dggridR_2.0.4.tar.gz')
    # Chose Update All when prompted
```

## Plot a resolution 3 grid with some example neighbourhoods

The following steps should reproduce the pdf in the archive
```
    > library(dplyr)
    > library(dggridR)
    > source("Q2DI_to_V.R")
    > source("get_V_label.R")
    > source('find_neighbours_V.R')
    > source('plot_vince_wneighs.R')
    > plot_vince(3) # 3 refers to a resolution 3 grid

```

# Reference

Vince, A. (2006). Indexing the aperture 3 hexagonal discrete global grid, Journal of Visual Communication and Image Representation 17(6): 1227–1236.

