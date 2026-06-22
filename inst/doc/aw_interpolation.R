## -----------------------------------------------------------------------------
#| include: false

# CRAN OMP THREAD LIMIT to avoid CRAN NOTE
Sys.setenv(OMP_THREAD_LIMIT = 2)


## -----------------------------------------------------------------------------
#| label: setup
#| message: false
#| warning: false
library(duckspatial)
library(sf)

# 1. Load Source Data (NC Counties)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# 2. Transform to projected CRS (Albers) for accurate area calculations
nc <- st_transform(nc, 5070)

# 3. Create a Target Grid
grid <- st_make_grid(nc, n = c(10, 5)) |> st_as_sf()

# 4. Create Unique IDs (Required for interpolation)
nc$source_id <- 1:nrow(nc)
grid$target_id <- 1:nrow(grid)


## -----------------------------------------------------------------------------
# Interpolate Total Births (Extensive)
res_extensive <- ddbs_interpolate_aw(
  target = grid,
  source = nc,
  tid = "target_id",
  sid = "source_id",
  extensive = "BIR74",
  weight = "total",
  mode = "sf"
)


## -----------------------------------------------------------------------------
orig_sum <- sum(nc$BIR74)
new_sum  <- sum(res_extensive$BIR74, na.rm = TRUE)

sprintf("Original: %s | Interpolated: %s", orig_sum, round(new_sum, 1))


## -----------------------------------------------------------------------------
# Interpolate 'BIR74' treating it as an intensive variable (e.g. density assumption)
res_intensive <- ddbs_interpolate_aw(
  target = grid,
  source = nc,
  tid = "target_id",
  sid = "source_id",
  intensive = "BIR74", # Treated as density here
  weight = "sum",      # Standard behavior for intensive vars
  mode = "sf"
)


## ----fig.height=5, fig.width=7------------------------------------------------
# Combine for plotting
plot_data <- res_extensive[, "BIR74"]
names(plot_data)[1] <- "Extensive_Count"
plot_data$Intensive_Value <- res_intensive$BIR74

plot(plot_data[c("Extensive_Count", "Intensive_Value")], 
     main = "Interpolation Methods Comparison",
     border = "grey90",
     key.pos = 4)


## -----------------------------------------------------------------------------
# Return a standard data.frame/tibble without geometry
res_tbl <- ddbs_interpolate_aw(
  target = grid,
  source = nc,
  tid = "target_id",
  sid = "source_id",
  extensive = "BIR74"
) |> 
  ddbs_collect(as = "tibble")

head(res_tbl)


## -----------------------------------------------------------------------------
# Create connection
conn <- ddbs_create_conn()

# Write layers to DuckDB
ddbs_write_vector(conn, nc, "nc_table", overwrite = TRUE)
ddbs_write_vector(conn, grid, "grid_table", overwrite = TRUE)


## -----------------------------------------------------------------------------
# Run interpolation and save to new table 'nc_grid_births'
ddbs_interpolate_aw(
  conn = conn,
  target = "grid_table",
  source = "nc_table",
  tid = "target_id",
  sid = "source_id",
  extensive = "BIR74",
  weight = "total",
  name = "nc_grid_births", # <--- Writes to DB
  overwrite = TRUE
)

# Verify the table was created
DBI::dbListTables(conn)


## -----------------------------------------------------------------------------
# Read the result back from the database
final_sf <- ddbs_read_vector(conn, "nc_grid_births")

head(final_sf)


## -----------------------------------------------------------------------------
ddbs_interpolate_aw(
  conn = conn,
  target = "grid_table",
  source = "nc_table",
  tid = "target_id",
  sid = "source_id",
  extensive = "BIR74",
  weight = "total",
  name = "nc_grid_births", # <--- Writes to DB
  overwrite = TRUE,
  mode = "tibble"
)


## -----------------------------------------------------------------------------
as_duckspatial_df("nc_grid_births", conn)


## -----------------------------------------------------------------------------
duckdb::dbDisconnect(conn)

