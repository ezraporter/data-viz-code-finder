library(rlang)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(purrr)
library(parsermd)

process_article <- function(rmd_path, dictionary = NULL) {
  message("processing ", rmd_path)
  html_path <- replace_extension(rmd_path, "Rmd", "html")
  article_name <- strip_extension(basename(rmd_path), "Rmd")
  
  # Fix parsing errors
  rmd <- read_file(rmd_path) |>
    str_replace("```\\{r global options", "```{r global-options") |>
    str_replace_all("\u00A0", " ")
  
  parsed <- parse_rmd(rmd)
  
  out <- parsed |>
    rmd_select(has_type("rmd_chunk")) |>
    as_tibble() |>
    update_chunk_labels() |>
    add_png_paths(html_path) |>
    mutate(
      code = map_chr(ast, \(x) paste(rmd_node_code(x), collapse = "\n")),
      functions = map(code, extract_calls),
      #functions = map(code, safely(extract_calls)),
      function_labels = map(functions, \(x) intersect(x, dictionary %||% x)),
      article = article_name
    ) |>
    select(article, chunk_name = label, png_path, code, functions, function_labels)
  
  out$png_path <- map_chr(out$png_path, copy_pngs, article_name = article_name)
  out
}

update_chunk_labels <- function(data) {
  data |>
    # knitr assigns the ith unnamed chunk the label "unnamed-chunk-[i]"
    mutate(
      label = if_else(label == "", paste0("unnamed-chunk-", row_number()), label),
      .by = label
    ) |>
    # knitr appends an index to ensure each chunk label is unique
    mutate(
      label = paste(label, row_number(), sep = "-"),
      .by = label
    )
}

add_png_paths <- function(data, html_path) {
  png_paths <- extract_png_paths(html_path)
  
  left_join(data, png_paths, by = "label")
}

extract_png_paths <- function(path) {
  file_name <- strip_extension(basename(path), "html")
  dir_name <- dirname(path)
  
  pattern <- paste0(file_name, "_files/figure-html/.+\\.png")
  content <- read_file(path)
  
  png_paths <- str_extract_all(content, pattern)
  # Make paths relative to working dir
  png_paths <- file.path(dir_name, png_paths[[1]])
  
  tibble(png_path = png_paths) |>
    mutate(label = strip_extension(basename(png_path), "png"))
}

extract_calls <- function(code) {
  parse_exprs(code) |>
    flat_map_chr(extract_calls_rec) |>
    unique()
}

extract_calls_rec <- function(x) {
  # Base case
  if (is_syntactic_literal(x) || is.symbol(x)) {
    return(character(0))
  }
  
  # Recursive cases
  
  if (is.call(x)) {
    # If its a call return the name of the function and check for calls at the next layer
    # call_name() will only handle "simple" calls and misses R6 methods
    # call_name fails for formulas so handle that manually
    fn_name <- if (is_formula(x)) "~" else call_name(x)
    the_rest <- flat_map_chr(as.list(x), extract_calls_rec)
    return(c(fn_name, the_rest))
  }
  
  if (is.pairlist(x)) {
    # If its a pairlist check all the args for calls
    return(flat_map_chr(as.list(x), extract_calls_rec))
  }
  
  stop(typeof(x), " is not recognized")
}

copy_pngs <- function(path, article_name) {
  if (is.na(path)) {
    return(path)
  }
  to_path <- file.path("images", article_name, basename(path))
  if (!any(file.exists(path, to_path))) {
    message(path, " not found")
    return(NA)
  }
  if (file.exists(to_path)) {
    message(to_path, " exists")
    return(to_path)
  }
  message("copying ", path, " to ", to_path)
  if (!dir.exists(dirname(to_path))) {
    dir.create(dirname(to_path), recursive = TRUE)
  }
  file.copy(path, to_path)
  to_path
}

## Utils

flat_map_chr <- function(.x, .f, ...) {
  list_c(map(.x, .f, ...), ptype = character(0))
}

replace_extension <- function(path, from, to) {
  pattern <- paste0("\\.", from, "$")
  replacement <- paste0(".", to)
  str_replace(path, pattern, replacement)
}

strip_extension <- function(path, extension) {
  pattern <- paste0("\\.", extension, "$")
  str_remove(path, pattern)
}

# Main

dir_name <- "R-graph-gallery"

rmd_paths <- list.files(dir_name, pattern = "\\.Rmd$")
rmd_paths <- file.path(dir_name, rmd_paths)

dictionary <- c(
  "hexbin",
  "geom_line",
  "geom_ribbon",
  "corrgram",
  "ggcorr",
  "ggpairs",
  "lines",
  "rasterImage",
  "geom_rect",
  "coord_polar",
  "geom_label",
  "geom_text",
  "polygon",
  "doughnut",
  "pie",
  "geom_density",
  "facet_wrap",
  "geom_area",
  "radarchart",
  "coord_circle",
  "text",
  "geom_polygon",
  "geom_path",
  "geom_point",
  "choroLayer",
  "layoutLayer",
  "propSymbolsLayer",
  "geom_histogram",
  "abline",
  "geom_segment",
  "grid.arrange",
  "stat_density2d",
  "clplot",
  "levelplot",
  "panel.2dsmoother",
  "barplot",
  "heatmap",
  "geom_bar",
  "boxplot",
  "facet_grid",
  "geom_hline",
  "geom_vline",
  "treemap",
  "geom_boxplot",
  "stat_summary",
  "geom_density_ridges",
  "geom_density_ridges_gradient",
  "geom_bin2d",
  "geom_rug",
  "geom_tile",
  "circleProgressiveLayout",
  "circleLayoutVertices",
  "ggraph",
  "geom_node_point",
  "geom_conn_bundle",
  "get_con",
  "geom_node_text",
  "geom_node_circle",
  "geom_edge_diagonal",
  "geom_node_tile",
  "geom_node_arc_bar",
  "geom_edge_link",
  "geom_node_label",
  "cartogram",
  "rect.dendrogram",
  "colored_bars",
  "geom_errorbar",
  "geom_crossbar",
  "geom_linerange",
  "geom_pointrange",
  "arrows",
  "rect",
  "points",
  "geom_jitter",
  "geom_violin",
  "parcoord",
  "vioplot",
  "jitter",
  "plotcorr",
  "scatterplotMatrix",
  "geom_text_repel",
  "ggparcoord",
  "geom_col",
  "geom_richtext",
  "geom_shadowtext",
  "grid.lines",
  "grid.rect",
  "grid.text",
  "gghighlight",
  "geom_curve",
  "geom_smooth","geom_stream",
  "geom_textbox",
  "ggdraw",
  "draw_image",
  "stat_difference",
  "facet_geo",
  "xyplot",
  "xyTable",
  "arrow",
  "hist"
)

## Process
articles <- map(rmd_paths, safely(process_article), dictionary = dictionary) |>
  transpose()

result <- articles |>
  pluck("result") |>
  bind_rows()

errors <- articles |>
  pluck("error") |>
  set_names(rmd_paths) |>
  compact()

## Check results

all_functions <- result |>
  filter(!is.na(png_path)) |>
  pull(functions) |>
  list_c(ptype = character(0)) |>
  unique()

not_in_dict <- setdiff(all_functions, dictionary)

cat("Found", length(all_functions), "possible labels")
cat(length(not_in_dict), "not in dictionary")
not_in_dict

## Errors
cat(length(errors), "failed articles")
map(errors, "message")

## Save output

result |>
  filter(!is.na(png_path)) |>
  mutate(
    across(
      where(is.list),
      \(x) map_chr(x, paste, collapse = ",")
    )
  ) |>
  write_csv("graph-gallery.csv", na = "")

message("Data written to graph-gallery.csv")

write_rds(result, "full-graph-gallery.rds")

message("Full data written to graph-gallery.csv")
