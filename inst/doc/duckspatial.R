## -----------------------------------------------------------------------------
#| include: false

# Limit threads to avoid a CRAN NOTE
Sys.setenv(OMP_THREAD_LIMIT = 2)


## -----------------------------------------------------------------------------
#| label: setup
#| message: false
#| warning: false

library(duckdb)
library(duckspatial)
library(dplyr)
library(sf)


## -----------------------------------------------------------------------------
countries_ddbs <- ddbs_open_dataset(
  system.file(
    "spatial/countries.geojson",
    package = "duckspatial"
  )
)

print(countries_ddbs)


## -----------------------------------------------------------------------------
## read with sf as usual
countries_sf <- read_sf(
  system.file(
    "spatial/countries.geojson",
    package = "duckspatial"
  )
)

## push into DuckDB
countries_ddbs <- as_duckspatial_df(countries_sf)

class(countries_ddbs)


## -----------------------------------------------------------------------------
countries_ddbs |>
  ddbs_is_valid() |>
  filter(!is_valid)


## -----------------------------------------------------------------------------
world_ddbs <- countries_ddbs |>
  ddbs_make_valid() |>
  ddbs_union()

print(world_ddbs)


## -----------------------------------------------------------------------------
world_sf <- world_ddbs |>
  ddbs_collect()

print(world_sf)


## -----------------------------------------------------------------------------
plot(world_sf)


## -----------------------------------------------------------------------------
conn <- ddbs_create_conn()


## -----------------------------------------------------------------------------
conn <- ddbs_create_conn(
  threads         = 2,
  memory_limit_gb = 8
)


## -----------------------------------------------------------------------------
ddbs_write_table(conn, countries_sf, name = "countries")


## -----------------------------------------------------------------------------
ddbs_list_tables(conn)


## -----------------------------------------------------------------------------
ddbs_is_valid("countries", conn = conn) |>
  filter(!is_valid)


## -----------------------------------------------------------------------------
ddbs_make_valid("countries", conn = conn, name = "countries_valid")
ddbs_union("countries_valid", conn = conn, name = "world")


## -----------------------------------------------------------------------------
ddbs_read_table(conn, "world") |>
  plot()


## -----------------------------------------------------------------------------
ddbs_stop_conn(conn)


## -----------------------------------------------------------------------------
#| eval: false

# conn <- ddbs_create_conn("my_database.duckdb")


## -----------------------------------------------------------------------------
#| eval: false

# ## open persistent connection
# conn <- ddbs_create_conn("my_database.duckdb")
# 
# ## do all processing with duckspatial_df objects
# world_ddbs <- ddbs_open_dataset(
#     system.file("spatial/countries.geojson", package = "duckspatial")
#   ) |>
#   ddbs_make_valid() |>
#   ddbs_union()
# 
# ## write only the final result to the persistent database
# ddbs_write_table(conn, world_ddbs, name = "world")
# 
# ## close — "my_database.duckdb" will persist on disk
# ddbs_stop_conn(conn)

