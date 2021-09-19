
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

skip_matrix <- function(seed = 1) {

  set.seed(seed)

  # grid size and number of shades are fixed
  grains <- 200
  shades <- 1000

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

  iterations <- sample(x = 50:200, size = 1)
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

annotate_tibble <- function(tbl, x_shift, y_shift) {
  tbl %>%
    mutate(
      id = 1,
      seed = sample(1000, 1),
      ind = row_number(),
      type = "stoneskip",
      x = x_shift + (x / max(x)),
      y = y_shift + (y / max(y)),
      size = (shade %% 10),
      shade = shade + seed
    )
}

sample_shades <- function(n = 1) {
  canva1 <- sample(ggthemes::canva_palettes, 1)[[1]]
  canva2 <- sample(ggthemes::canva_palettes, 1)[[1]]
  canva <- c(canva1, canva2[1:2])
  return(sample(canva1))
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
      "docs", pixels, "01", paste0("grid_sys01_seed", seed, ".png")
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

paint_cell <- function(x_shift, y_shift, seed) {
  seed %>%
    skip_matrix() %>%
    construct_tibble() %>%
    annotate_tibble(x_shift, y_shift)
}

paint_grid <- function(seed) {

  set.seed(seed)

  vals <- (0:4)
  grid <- expand_grid(x_shift = vals, y_shift = vals) %>%
    mutate(seed = sample(1000, n()))

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
  1002, 1004, 1038, 1056,
  1057, 1060, 1066, 1067,
  1074, 1075, 1076, 1078,
  1079, 1082, 1094, 1097
)

for(seed in seeds) {
  cat("seed:", seed, "\n")
  paint_grid(seed)
}




