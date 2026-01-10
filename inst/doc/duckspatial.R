## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = identical(tolower(Sys.getenv("NOT_CRAN")), "true"),
  out.width = "100%"
)

# CRAN OMP THREAD LIMIT to avoid CRAN NOTE
Sys.setenv(OMP_THREAD_LIMIT = 2)

## ----message=FALSE------------------------------------------------------------
library(duckspatial)
library(sf)

# polygons
countries_sf  <- sf::st_read(
    system.file("spatial/countries.geojson",  package = "duckspatial"),
    quiet = TRUE
    )

# create random points
set.seed(42)
n <- 10000
points_sf <- data.frame(
  id = 1:n,
  x  = runif(n, min = -180, max = 180),
  y  = runif(n, min =  -90, max =  90)
) |>
  sf::st_as_sf(coords = c("x","y"), crs = 4326)


## ----message=FALSE------------------------------------------------------------
result_sf <- ddbs_join(
  x = points_sf,
  y = countries_sf,
  join = "intersects"
)

head(result_sf)

## ----message=FALSE------------------------------------------------------------
# create duckdb con and install / load spatial extension
conn <- duckspatial::ddbs_create_conn()

## ----message=FALSE------------------------------------------------------------
ddbs_join(
    conn = conn,
    x = points_sf,
    y = countries_sf,
    join = "intersects", 
    name = "points_in_countries_tbl"
)


## ----message=FALSE------------------------------------------------------------
tbl <- ddbs_read_vector(
    conn = conn,
    name = "points_in_countries_tbl"
    )

head(tbl)


## ----message=FALSE------------------------------------------------------------
# write `sf` objects as tables to duckdb
duckspatial::ddbs_write_vector(
    conn = conn, 
    data = countries_sf, 
    name = "countries"
    )

duckspatial::ddbs_write_vector(
    conn = conn, 
    data = points_sf, 
    name = "points"
    )


## ----message=FALSE------------------------------------------------------------
result_sf <- ddbs_join(
  conn = conn,
  x = "points",
  y = "countries",
  join = "intersects"
  )


## ----message=FALSE------------------------------------------------------------
ddbs_join(
  conn = conn,
  x = "points",
  y = "countries",
  join = "intersects", 
  name = "points_in_countries_tbl", 
  overwrite = TRUE
  )


# and read the table to memory as sf
# tbl <- ddbs_read_vector(
#     conn = conn,
#     name = "points_in_countries_tbl"
#     )


