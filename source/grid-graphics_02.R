
library(tidyverse)

Rcpp::sourceCpp(here::here("source", "stoneskip.cpp"))

fractals <- list(
  ambient::billow,
  ambient::fbm,
  ambient::ridged
)

generators <- list(
  ambient::gen_simplex,
  ambient::gen_worley
)

sample_list <- function(...) {
  (sample(...))[[1]]
}

skip_matrix <- function(seed = 1, grains = 100, shades = 1000, iterations = sample(x = 50:200, size = 1)) {

  set.seed(seed)

  # create the base image using ambient -------------------------------------

  grid <- ambient::long_grid(
    x = seq(0, 1, length.out = grains),
    y = seq(0, 1, length.out = grains)
  )

  base <- ambient::fracture(
    noise     = sample_list(x = generators, size = 1),
    fractal   = sample_list(x = fractals, size = 1),
    octaves   = sample(x = 1:10, size = 1),
    frequency = sample(x = 1:10, size = 1),
    value     = "distance2",
    seed      = seed,
    x         = grid$x,
    y         = grid$y
  )

  # run cellular automaton over base image ----------------------------------

  input <- round(ambient::normalise(base, to = c(1, shades)))
  input <- matrix(as.integer(input), grains, grains)
  output <- skip_stone(input, iterations)
  return(output)

}


construct_tibble <- function(mat) {

  # use the row and column names to represent co-ordinates
  rownames(mat) <- paste0("y", nrow(mat):1) # <- flip y
  colnames(mat) <- paste0("x", 1:ncol(mat))

  # convert to tibble
  tbl <- mat %>%
    as.data.frame() %>%
    rownames_to_column("y") %>%
    as_tibble()

  # reshape
  tbl <- tbl %>%
    pivot_longer(
      cols = starts_with("x"),
      names_to = "x",
      values_to = "shade"
    )

  # tidy
  tbl <- tbl %>%
    arrange(x, y) %>%
    mutate(
      x = x %>% str_remove_all("x") %>% as.numeric(),
      y = y %>% str_remove_all("y") %>% as.numeric(),
      id = row_number()
    )

  return(tbl)
}


annotate_tibble <- function(tbl, x_shift, y_shift, shade_shift) {
  tbl %>%
    mutate(
      id = 1,
      seed = sample(1000, 1),
      ind = row_number(),
      type = "stoneskip",
      x = x_shift + (x / max(x)),
      y = y_shift + (y / max(y)),
      size = (shade %% 10),
      shade = shade + shade_shift
    )
}


sample_shades <- function(n = 1) {
  canva1 <- sample(ggthemes::canva_palettes, 1)[[1]]
  canva2 <- sample(ggthemes::canva_palettes, 1)[[1]]
  canva <- c(canva1, "white", "white", "white", "white", canva2)
  return(canva)
}

specify_plot <- function(dat) {

  shades <- sample_shades()
  bg <- "white"

  pic <- ggplot(dat) +
    geom_tile(
      mapping = aes(
        x = x,
        y = y,
        fill = shade
      ),
      show.legend = FALSE
    ) +
    scale_x_continuous(name = NULL, expand = c(0,0), breaks = NULL) +
    scale_y_continuous(name = NULL, expand = c(0,0), breaks = NULL) +
    scale_size_identity() +
    scale_colour_gradientn(colours = shades) +
    scale_fill_gradientn(colours = shades) +
    theme_void() +
    theme(plot.background = element_rect(fill = bg)) +
    NULL

  return(pic)
}

render_plot <- function(pic, seed) {
  inches <- 40/3
  for(pixels in c(800, 4000)) {
    output <- here::here(
      "docs", pixels, "02", paste0("grid_sys02_seed", seed, ".png")
    )
    ggsave(
      filename = output,
      plot = pic,
      width = inches,
      height = inches,
      dpi = pixels / inches
    )
  }
}


paint_cell <- function(x_shift, y_shift, shade_shift, seed, grains = 20) {
  skip_matrix(seed = seed, grains = grains) %>%
    construct_tibble() %>%
    annotate_tibble(x_shift, y_shift, shade_shift)
}


paint_grid <- function(seed) {

  set.seed(seed)

  grid <- skip_matrix(seed = seed, grains = 20, iterations = 15) %>%
    construct_tibble() %>%
    transmute(
      x_shift = x - 1,
      y_shift = y - 1,
      shade_shift = shade * 10,
      seed = sample(1000, n())
    )

  dat <- pmap_dfr(grid, paint_cell) %>%
    mutate(
      x = x / max(x),
      y = y / max(y),
      time = 10,
      size = 1
    )

  dat %>%
    specify_plot() %>%
    render_plot(seed)
}



# run it ------------------------------------------------------------------

seeds <- c(
  1101, 1120, 1127, 1132,
  1142, 1143, 1146, 1148,
  1149, 1164, 1167, 1170,
  1181, 1188, 1198, 1199
)

for(seed in seeds) {
  cat("seed:", seed, "\n")
  paint_grid(seed)
}




