## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = identical(tolower(Sys.getenv("NOT_CRAN")), "true"),
  out.width = "100%"
)

# Initialize variables to prevent "object not found" error when eval=FALSE
time_diff <- "XX"
memo_diff <- "XX"

# CRAN OMP THREAD LIMIT to avoid CRAN NOTE
Sys.setenv(OMP_THREAD_LIMIT = 2)

## ----message = FALSE, warning=FALSE-------------------------------------------
library(duckspatial)
library(bench)
library(dplyr)
library(sf)
library(lwgeom)
library(ggplot2)
options(scipen = 999)

# read polygons data
countries_sf <- sf::st_read(system.file("spatial/countries.geojson", package = "duckspatial"))

# generate random points
set.seed(42)

## create points data
n = 10e4
points_sf_100k <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),  
    y = runif(n, min = -90, max = 90)
    ) |> 
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

n = 10e5
points_sf_1mi <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),  
    y = runif(n, min = -90, max = 90)
    ) |> 
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

# n = 10e6
# points_sf_10mi <- data.frame(
#     id = 1:n,
#     x = runif(n, min = -180, max = 180),  
#     y = runif(n, min = -90, max = 90)
#     ) |> 
#     sf::st_as_sf(coords = c("x", "y"), crs = 4326)


## ----message = FALSE----------------------------------------------------------
run_benchmark <- function(points_sf){
    
    temp_bench <- bench::mark(
        iterations = 1, 
        check = FALSE, 
        duckspatial = duckspatial::ddbs_join(
            x = points_sf, 
            y = countries_sf, 
            join = "within"),
        
        sf = sf::st_join(
            x = points_sf, 
            y = countries_sf, 
            join = sf::st_within)
        )
    
    temp_bench$n <- nrow(points_sf)
    temp_bench$pkg <- c("duckspatial", "sf")
    
    return(temp_bench)
}

# From 100K points to 1 million and 10 million points
df_bench_join <- lapply(
    X = list(points_sf_100k, points_sf_1mi),
    FUN = run_benchmark
    ) |> 
    dplyr::bind_rows()


# calculate difference in performance
temp <- df_bench_join |> 
    filter(n == 10e5)

memo_diff <- round(as.numeric(temp$mem_alloc[2] / temp$mem_alloc[1]),1)
time_diff <- (1 - round(as.numeric(temp$median[1] / temp$median[2]),2))*100

## ----warning=FALSE------------------------------------------------------------
ggplot(data = df_bench_join) +
    geom_point(size =3, aes(x= mem_alloc, y = median, color = pkg, 
                    shape = format(n, big.mark = ".")
                    )) +
    labs(color= "Package", shape = "Data size",
         y = "Computation time (seconds)",
         x = "Memory allocated") +
    theme_minimal()



## ----message = FALSE----------------------------------------------------------
run_benchmark <- function(points_sf){
    
    temp_bench <- bench::mark(
        iterations = 1, 
        check = FALSE, 
        duckspatial = duckspatial::ddbs_filter(
            x = points_sf, 
            y = countries_sf),
        
        sf = sf::st_filter(
            x = points_sf, 
            y = countries_sf)
        )
    
    temp_bench$n <- nrow(points_sf)
    temp_bench$pkg <- c("duckspatial", "sf")
    
    return(temp_bench)
}


# From 100K points to 1 million and 10 million points
df_bench_filter <- lapply(
    X = list(points_sf_100k, points_sf_1mi),
    FUN = run_benchmark
    ) |> 
    dplyr::bind_rows()


# calculate difference in performance
temp <- df_bench_filter |> 
    filter(n == 10e5)

memo_diff <- round(as.numeric(temp$mem_alloc[2] / temp$mem_alloc[1]),1)
time_diff <- (1 - round(as.numeric(temp$median[1] / temp$median[2]),2))*100

## ----warning=FALSE------------------------------------------------------------
ggplot(data = df_bench_filter) +
    geom_point(size =3, aes(x= mem_alloc, y = median, color = pkg, 
                    shape = format(n, big.mark = ".")
                    )) +
    labs(color= "Package", shape = "Data size",
         y = "Computation time (seconds)",
         x = "Memory allocated") +
    theme_minimal()



## ----message = FALSE----------------------------------------------------------
# Turn on S2 (Spherical geometry)
sf::sf_use_s2(TRUE)

run_benchmark <- function(n){
    
    set.seed(42)

    ## create points data
    points_sf <- data.frame(
        id = 1:n,
        x = runif(n, min = -180, max = 180),  
        y = runif(n, min = -90, max = 90)
        ) |> 
        sf::st_as_sf(coords = c("x", "y"), crs = 4326)
    
    temp_bench <- bench::mark(
        iterations = 1, 
        check = FALSE, 
        duckspatial = duckspatial::ddbs_distance(
            x = points_sf, 
            y = points_sf, 
            dist_type = "haversine"),
        
        sf = sf::st_distance(
            x = points_sf, 
            y = points_sf, 
            which = "Great Circle")
        )
    
    temp_bench$n <- nrow(points_sf)
    temp_bench$pkg <- c("duckspatial", "sf")
    
    return(temp_bench)
}


# From 100K points to 1 million and 10 million points
df_bench_distance <- lapply(
    X = c(500, 1000, 10000),
    FUN = run_benchmark
    ) |> 
    dplyr::bind_rows()


# calculate difference in performance
temp <- df_bench_distance |> 
    filter(n == 10000)

memo_diff <- round(as.numeric(temp$mem_alloc[1]) / as.numeric(temp$mem_alloc[2]) ,1)
time_diff <- (1 - round(as.numeric(temp$median[1] / temp$median[2]),2))*100

## ----warning=FALSE------------------------------------------------------------
ggplot(data = df_bench_distance) +
    geom_point(size =3, aes(x= mem_alloc, y = median, color = pkg, 
                    shape = format(n, big.mark = ".")
                    )) +
    labs(color= "Package", shape = "Data size",
         y = "Computation time (seconds)",
         x = "Memory allocated") +
    theme_minimal()



