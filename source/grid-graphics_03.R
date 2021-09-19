
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
  canva <- sample(ggthemes::canva_palettes, 1)[[1]]
  shade <- c(canva, "white", "white")
  return(shade)
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
      "docs", pixels, "03", paste0("grid_sys03_seed", seed, ".png")
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
  grains <- 25

  grid <- subdivide_matrix(seed = seed, grains = grains, iterations = 7) %>%
    construct_tibble() %>%
    transmute(
      x_shift = x - 1,
      y_shift = y - 1,
      shade_shift = shade * 3,
      seed = sample(1000, n())
    )

  dat <- pmap_dfr(grid, paint_cell, grains = grains) %>%
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


subdivide_matrix <- function(seed, grains, iterations) {

  set.seed(seed)

  split_rectangles <- function(left, right, bottom, top) {
    vertically <- runif(1) < .5

    if(vertically == TRUE) {
      if(top - bottom < 2) {
        return(tibble(
          left = left,
          right = right,
          bottom = bottom,
          top = top
        ))
      }
      insert <- sample((bottom + 1):top, 1)
      return(tibble(
        left = c(left, left),
        right = c(right, right),
        bottom = c(bottom, insert),
        top = c(insert - 1, top)
      ))
    }

    if(vertically == FALSE) {
      if(right - left < 2) {
        return(tibble(
          left = left,
          right = right,
          bottom = bottom,
          top = top
        ))
      }
      insert <- sample((left + 1):right, 1)
      return(tibble(
        left = c(left, insert),
        right = c(insert - 1, right),
        bottom = c(bottom, bottom),
        top = c(top, top)
      ))
    }
  }

  tbl <- tibble(
    left = 1,
    right = grains,
    bottom = 1,
    top = grains
  )

  for(it in 1:iterations) {
    tbl <- pmap_dfr(tbl, split_rectangles)
  }
  tbl <- mutate(tbl, shade = sample(1000, n()))

  mat <- matrix(0, nrow = grains, ncol = grains)
  for(i in 1:nrow(tbl)) {
    with(tbl[i, ], mat[left:right, bottom:top] <<- shade) # OMG WHHHHHHHY this code is awful???????
  }
  return(mat)
}



# run it ------------------------------------------------------------------

seeds <- c(
  1201, 1209, 1220, 1226,
  1233, 1238, 1242, 1246,
  1252, 1266, 1274, 1279,
  1284, 1286, 1288, 1295
)

for(seed in seeds) {
  cat("seed:", seed, "\n")
  paint_grid(seed)
}




