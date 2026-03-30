## -----------------------------------------------------------------------------
#| include: false

# Limit threads to avoid a CRAN NOTE
Sys.setenv(OMP_THREAD_LIMIT = 2)


## -----------------------------------------------------------------------------
#| echo: false

## Internal packages
library(patchwork)

## Functions used in the vignette
ggplot_benchmark <- function(
  data, 
  log = FALSE,
  show.legend = TRUE,
  ...
  ) {

    ## Generate base plot
    ggplot(
      data = data, 
      aes(
        x     = factor(format(n, big.mark = ".")),
        y     = if (log) median else as.numeric(median),
        color = pkg,
        group = pkg
    )) +
      geom_line(
        linewidth = 0.7,
        linetype  = "dashed",
        alpha     = 0.6,
        show.legend = show.legend
      ) +
      geom_point(
        aes(size = as.numeric(mem_alloc)),
        alpha = 0.9,
        show.legend = show.legend
      ) +
      scale_size_binned(
        name   = "Memory allocated",
        labels = scales::label_bytes(),
        n.breaks = 4,
        range = c(1, 8)
      ) +
      scale_color_brewer(palette = "Set1") +
      labs(
        color    = "Package",
        x        = "Data size (rows)",
        y        = "Computation time",
        ...
      ) +
      theme_minimal(base_size = 13) +
      theme(
        # legend.position  = "top",
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold")
      )
}

ggplot_assemble <- function(plot1, plot2, fun_name) {

  plot1 +
    plot2 +
    plot_annotation(
      title    = paste("Benchmark Comparison for", fun_name),
      subtitle = "Execution time vs. data size",
      theme    = theme(
        plot.title = element_text(face = "bold", hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        text = element_text(size = rel(3.5))
      )
    )

}


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| code-fold: true
#| code-summary: "Set-up"

# Load necessary packages
library(duckspatial)
library(bench)
library(dplyr)
library(sf)
library(ggplot2)
options(scipen = 999)

# Function to generate random points
make_points <- function(n_points) {
    points_df <- data.frame(
      id = 1:n_points,
      x = runif(n_points, min = -180, max = 180),
      y = runif(n_points, min = -90, max = 90),
      value = rnorm(n_points, mean = 100, sd = 15),
      category = sample(c("A", "B", "C", "D"), n_points, replace = TRUE)
  ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)
}

# Generate datasets of different sizes
withr::with_seed(27, {
  points_sf_100k <- make_points(1e5)
  points_sf_1mi  <- make_points(1e6)
  points_sf_3mi  <- make_points(3e6)
})

# Generate polygons
# Create large polygon dataset (e.g., administrative regions, zones, etc.)
n_polygons <- 10000
polygons_list <- vector("list", n_polygons)

for(i in 1:n_polygons) {
  # Random center point with buffer from edges
  center_x <- runif(1, min = -170, max = 170)
  center_y <- runif(1, min = -80, max = 80)
  
  # Create simple rectangular polygons to avoid geometry issues
  width <- runif(1, min = 0.5, max = 3)
  height <- runif(1, min = 0.5, max = 3)
  
  # Create rectangle coordinates (must be closed: first point = last point)
  x_coords <- c(
    center_x - width/2,
    center_x + width/2,
    center_x + width/2,
    center_x - width/2,
    center_x - width/2  # Close the polygon
  )
  
  y_coords <- c(
    center_y - height/2,
    center_y - height/2,
    center_y + height/2,
    center_y + height/2,
    center_y - height/2  # Close the polygon
  )
  
  # Create polygon matrix
  coords <- cbind(x_coords, y_coords)
  
  # Create polygon (wrapped in list as required by st_polygon)
  polygons_list[[i]] <- st_polygon(list(coords))
}

polygons_sf <- st_sf(
  poly_id    = 1:n_polygons,
  region     = sample(c("North", "South", "East", "West"), n_polygons, replace = TRUE),
  population = sample(1000:1000000, n_polygons, replace = TRUE),
  geometry   = st_sfc(polygons_list, crs = 4326)
)


## -----------------------------------------------------------------------------
#| message: false
#| code-fold: true
#| code-summary: "Benchmark code - ddbs_join"

# Helper to run the benchmark
run_join_benchmark <- function(points_sf) {
  temp <- bench::mark(
    iterations  = 3,
    check       = FALSE,
    duckspatial = ddbs_join(points_sf, polygons_sf, join = "within"),
    sf          = st_join(points_sf, polygons_sf, join = st_within)
  )
  temp$n   <- nrow(points_sf)
  temp$pkg <- c("duckspatial", "sf")
  temp
}

# Run the benchmark
df_bench_join <- lapply(
  X   = list(points_sf_100k, points_sf_1mi, points_sf_3mi),
  FUN = run_join_benchmark
) |>
  dplyr::bind_rows()


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false

# Id to store the figures, saving the older ones
id_output <- "v1.5.1"

# Generate the plots
gg_join <- ggplot_benchmark(
  data     = df_bench_join,
  log      = FALSE,
  show.legend = T,
  subtitle = "A) Normal scale"
)

gg_join_log <- ggplot_benchmark(
  data     = df_bench_join,
  log      = TRUE,
  show.legend = F,
  subtitle = "B) Log scale"
)

# Assemble them in a single plot
ggplot_assemble(
  plot1 = gg_join,
  plot2 = gg_join_log,
  fun_name = "ddbs_join()"
)

# Export it
ggsave(
  filename = paste0("man/figures/bench/bench-st-join-", id_output, ".png"),
  height   = 15,
  width    = 30,
  units    = "cm"
)


## -----------------------------------------------------------------------------
#| message: false
#| code-fold: true
#| code-summary: "Benchmark code - ddbs_filter"

# Helper to run the benchmark
run_filter_benchmark <- function(points_sf) {
  temp <- bench::mark(
    iterations  = 3,
    check       = FALSE,
    duckspatial = ddbs_filter(points_sf, polygons_sf),
    sf          = st_filter(points_sf, polygons_sf)
  )
  temp$n   <- nrow(points_sf)
  temp$pkg <- c("duckspatial", "sf")
  temp
}

# Run the benchmark
df_bench_filter <- lapply(
  X   = list(points_sf_100k, points_sf_1mi, points_sf_3mi),
  FUN = run_filter_benchmark
) |>
  dplyr::bind_rows()


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false

# Generate the plots
gg_filter <- ggplot_benchmark(
  data     = df_bench_filter,
  log      = FALSE,
  show.legend = T,
  subtitle = "A) Normal scale"
)

gg_filter_log <- ggplot_benchmark(
  data     = df_bench_filter,
  log      = TRUE,
  show.legend = F,
  subtitle = "B) Log scale"
)

# Assemble them in a single plot
ggplot_assemble(
  plot1 = gg_filter,
  plot2 = gg_filter_log,
  fun_name = "ddbs_filter()"
)

# Export it
ggsave(
  filename = paste0("man/figures/bench/bench-st-filter-", id_output, ".png"),
  height   = 15,
  width    = 30,
  units    = "cm"
)


## -----------------------------------------------------------------------------
#| message: false
#| code-fold: true
#| code-summary: "Benchmark code - ddbs_distance"

# Helper to run the benchmark
run_distance_benchmark <- function(n) {

  points_sf <- withr::with_seed(27, make_points(n))

  temp <- bench::mark(
    iterations  = 1,
    check       = FALSE,
    duckspatial = ddbs_distance(points_sf, points_sf),
    sf          = st_distance(points_sf, points_sf)
  )
  temp$n   <- n
  temp$pkg <- c("duckspatial", "sf")
  temp
}

df_bench_distance <- lapply(
  X   = c(1000, 5000, 10000),
  FUN = run_distance_benchmark
) |>
  dplyr::bind_rows()


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false

# Generate the plots
gg_distance <- ggplot_benchmark(
  data     = df_bench_distance,
  log      = FALSE,
  show.legend = T,
  subtitle = "A) Normal scale"
)

gg_distance_log <- ggplot_benchmark(
  data     = df_bench_distance,
  log      = TRUE,
  show.legend = F,
  subtitle = "B) Log scale"
)

# Assemble them in a single plot
ggplot_assemble(
  plot1 = gg_distance,
  plot2 = gg_distance_log,
  fun_name = "ddbs_distance()"
)

# Export it
ggsave(
  filename = paste0("man/figures/bench/bench-st-distance-", id_output, ".png"),
  height   = 15,
  width    = 30,
  units    = "cm"
)


## -----------------------------------------------------------------------------
#| message: false
#| code-fold: true
#| code-summary: "Benchmark code - ddbs_union_agg"

# Helper to run the benchmark
run_union_benchmark <- function(points_sf) {
  temp <- bench::mark(
    iterations  = 3,
    check       = FALSE,
    duckspatial = ddbs_union_agg(points_sf, by = "category"),
    sf          = points_sf |> 
      group_by(category) |> 
      summarise(geometry = st_union(geometry))
  )
  temp$n   <- nrow(points_sf)
  temp$pkg <- c("duckspatial", "sf")
  temp
}

# Run the benchmark
df_bench_union <- lapply(
  X   = list(points_sf_100k, points_sf_1mi, points_sf_3mi),
  FUN = run_union_benchmark
) |>
  dplyr::bind_rows()


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false

# Generate the plots
gg_union_agg <- ggplot_benchmark(
  data     = df_bench_union,
  log      = FALSE,
  show.legend = T,
  subtitle = "A) Normal scale"
)

gg_union_agg_log <- ggplot_benchmark(
  data     = df_bench_union,
  log      = TRUE,
  show.legend = F,
  subtitle = "B) Log scale"
)

# Assemble them in a single plot
ggplot_assemble(
  plot1 = gg_union_agg,
  plot2 = gg_union_agg_log,
  fun_name = "ddbs_union_agg()"
)

# Export it
ggsave(
  filename = paste0("man/figures/bench/bench-st-dissolve-", id_output, ".png"),
  height   = 15,
  width    = 30,
  units    = "cm"
)


## -----------------------------------------------------------------------------
#| message: false
#| code-fold: true
#| code-summary: "Benchmark code - ddbs_intersects"

# Helper to run the benchmark
run_predicate_benchmark <- function(points_sf) {
  temp <- bench::mark(
    iterations  = 1,
    check       = FALSE,
    duckspatial = ddbs_intersects(points_sf, polygons_sf),
    sf          = st_intersects(points_sf, polygons_sf)
  )
  temp$n   <- nrow(points_sf)
  temp$pkg <- c("duckspatial", "sf")
  temp
}

# Run the benchmark
df_bench_predicate <- lapply(
  X   = list(points_sf_100k, points_sf_1mi, points_sf_3mi),
  FUN = run_predicate_benchmark
) |>
  dplyr::bind_rows()


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false

# Generate the plots
gg_predicate <- ggplot_benchmark(
  data     = df_bench_predicate,
  log      = FALSE,
  show.legend = T,
  subtitle = "A) Normal scale"
)

gg_predicate_log <- ggplot_benchmark(
  data     = df_bench_predicate,
  log      = TRUE,
  show.legend = F,
  subtitle = "B) Log scale"
)

# Assemble them in a single plot
ggplot_assemble(
  plot1 = gg_predicate,
  plot2 = gg_predicate_log,
  fun_name = "ddbs_intersects()"
)

# Export it
ggsave(
  filename = paste0("man/figures/bench/bench-st-intersects-", id_output, ".png"),
  height   = 15,
  width    = 30,
  units    = "cm"
)

