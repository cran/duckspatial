## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = identical(tolower(Sys.getenv("NOT_CRAN")), "true"),
  out.width = "100%"
)

# CRAN OMP THREAD LIMIT to avoid CRAN NOTE
Sys.setenv(OMP_THREAD_LIMIT = 2)

## -----------------------------------------------------------------------------
library(duckspatial)
# library(mapview)
library(sf)

# polygons
countries_sf  <- sf::st_read(
    system.file("spatial/countries.geojson",  package = "duckspatial"),
    quiet = TRUE
    )

# random points
set.seed(42)
n <- 10000
points_sf <- data.frame(
  id = 1:n,
  x  = runif(n, min = -180, max = 180),
  y  = runif(n, min =  -90, max =  90)
) |>
  sf::st_as_sf(coords = c("x","y"), crs = 4326)


## ----message=FALSE------------------------------------------------------------
out_sf1 <- ddbs_join(
  x    = points_sf,
  y    = countries_sf,
  join = "within"
)

# quick peek
# mapview(out_sf1, zcol="NAME_ENGL")


## -----------------------------------------------------------------------------
# create a fresh DuckDB connection
conn <- duckspatial::ddbs_create_conn()


## ----message=FALSE------------------------------------------------------------
# write data to DuckDB
ddbs_write_vector(conn, points_sf,   "points",    overwrite = TRUE)
ddbs_write_vector(conn, countries_sf, "countries", overwrite = TRUE)

# spatial join inside DuckDB; result returned as sf
out_sf2 <- ddbs_join(
  conn,
  x    = "points",
  y    = "countries",
  join = "within"
)

## ----message=FALSE------------------------------------------------------------
ddbs_join(
    conn = conn,
    x = "points",
    y = "countries",
    join = "within",
    name = "points_in_countries",
    overwrite = TRUE
)

# use the result in SQL (or read back as sf later)
# DBI::dbReadTable(conn, "points_in_countries") |>
#     sf::st_as_sf(wkt = 'geometry') |> 
#     head()


## -----------------------------------------------------------------------------
duckdb::dbDisconnect(conn)

