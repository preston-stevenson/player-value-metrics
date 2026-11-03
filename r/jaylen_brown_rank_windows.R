# jaylen_brown_rank_windows.R
# ---------------------------------------------------------------
# For each of the five player-value steps (RAPM, box-only, 11-year
# decayed RAPM, prior-informed Bayesian RAPM, recency-weighted
# Bayesian RAPM), builds a small gt leaderboard table (9 rows)
# centered on Jaylen Brown, so you can see exactly who sits around
# him at each step's rank. Each table auto-prints to the RStudio
# Viewer when you run its block, and the last section saves all five
# as PNGs straight to the Desktop.
#
# Ranks use standard "competition" ranking (ties share the lower
# rank number, next distinct value picks up after the tie count) via
# dplyr::min_rank() -- the same convention used everywhere else in
# this project's reporting, not the fractional "average" rank R's
# base rank() or pandas' default would otherwise give you.
#
# All dplyr verbs are called as dplyr::verb() rather than bare verb()
# -- something else in past R sessions was shadowing dplyr::arrange(),
# so this doesn't rely on load order / library(tidyverse) attachment
# position at all.
#
# On the border: gt's own table-outline/border options (opt_table_
# outline(), table.border.*) go through CSS border-collapse, which
# resolves which of several competing lines actually gets drawn at a
# shared edge using its own width/style/source-priority rules. Three
# different attempts at tuning that (hidden styles, explicit cell
# borders, wider widths) each fixed one edge and broke another, which
# means the approach itself was the problem, not the specific values.
# So: the gt table below has NO outline at all, and the border is
# instead added as a real, uniform, pixel-level border on the saved
# PNG directly (via the `magick` package, in the save loop at the
# bottom), completely outside gt/CSS. That step also fixes the output
# size deterministically instead of via webshot zoom/font tuning.
#
# Run this from an R session with its working directory set to
# PlayerValueMetrics/ (open the project in RStudio and run it from
# there), since paths below are relative to that folder's data/.
# ---------------------------------------------------------------

library(tidyverse)
library(gt)

DATA_DIR  <- "data"
PLAYER    <- "Jaylen Brown"
WINDOW    <- 9   # total rows shown; PLAYER sits in the middle when possible

PAGE_BG         <- "#ffffff"  # table card background
HIGHLIGHT_COLOR <- "#E3EAD9"  # pale sage/forest green for PLAYER's row
ACCENT_COLOR    <- "#3A5A40"  # forest green used for the saved PNG's border

# Returns `window` rows centered on `player`, ranked by `value_col`
# descending. Clamps at either end of the leaderboard so the window
# never runs off the top or bottom of the pool.
rank_window <- function(df, value_col, name_col = "PLAYER_NAME",
                         player = PLAYER, window = WINDOW) {
  ranked <- df %>%
    dplyr::arrange(dplyr::desc(.data[[value_col]])) %>%
    dplyr::mutate(
      Rank = dplyr::min_rank(dplyr::desc(.data[[value_col]])),
      Pos  = dplyr::row_number()
    )

  target_pos <- ranked %>%
    dplyr::filter(.data[[name_col]] == player) %>%
    dplyr::pull(Pos)
  if (length(target_pos) == 0) {
    stop(paste0("'", player, "' not found in this leaderboard. ",
                "Check the name spelling / that this player qualified for this metric's pool."))
  }

  half  <- (window - 1) %/% 2
  start <- max(1, target_pos - half)
  end   <- min(nrow(ranked), start + window - 1)
  start <- max(1, end - window + 1)  # re-clamp if the window hit the bottom edge

  ranked %>% dplyr::slice(start:end)
}

# Turns a rank_window() result (already select()'d down to the columns
# you want shown) into a styled gt table: white card, bold title and
# column headers, PLAYER's row highlighted. No outer border here on
# purpose -- see the header comment; that's added to the saved PNG
# instead. `label_overrides` is an optional named character vector of
# column_name = "label" pairs (e.g. c(RAPM__Off = "Off", RAPM__Def =
# "Def")) for relabeling specific columns without touching the others.
style_gt <- function(window_df, title, numeric_cols,
                      label_overrides = NULL, player = PLAYER) {
  tbl <- window_df %>%
    gt::gt() %>%
    gt::tab_header(title = title) %>%
    gt::cols_label(PLAYER_NAME = "Player") %>%
    gt::fmt_number(columns = dplyr::all_of(numeric_cols), decimals = 2) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = HIGHLIGHT_COLOR),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(rows = PLAYER_NAME == player)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold", size = gt::px(15)),
      locations = gt::cells_title(groups = "title")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_options(
      table.font.size = gt::px(11),
      table.font.names = c("Helvetica Neue", "Helvetica", "Arial", "sans-serif"),
      heading.align = "left",
      table.background.color = PAGE_BG,
      heading.background.color = PAGE_BG,
      column_labels.background.color = PAGE_BG,
      row.striping.background_color = PAGE_BG,
      table_body.border.top.style = "hidden",       # avoids a doubled line under the column headers
      table_body.border.bottom.style = "hidden",    # fine to hide now -- no outline competing with it
      column_labels.border.top.style = "hidden",    # avoids a doubled line above the column headers
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#dddddd"
    )

  if (!is.null(label_overrides)) {
    tbl <- tbl %>% gt::cols_label(.list = as.list(label_overrides))
  }
  tbl
}

# ---- RAPM: Plain single-season RAPM (2025-26) -------------------
single <- readr::read_csv(file.path(DATA_DIR, "single_year_rapm_2015_2025.csv"),
                           show_col_types = FALSE) %>%
  dplyr::filter(Season == 2025)

m1_window <- rank_window(single, "RAPM") %>%
  dplyr::select(Rank, PLAYER_NAME, RAPM, RAPM__Off, RAPM__Def)

m1_tbl <- style_gt(
  m1_window,
  title = "RAPM",
  numeric_cols = c("RAPM", "RAPM__Off", "RAPM__Def"),
  label_overrides = c(RAPM__Off = "Off", RAPM__Def = "Def")
)
m1_tbl

# ---- Box Score Only: box-score-only (SBPM) (2025-26) ------------
box <- readr::read_csv(file.path(DATA_DIR, "box_only_leaderboard.csv"),
                        show_col_types = FALSE) %>%
  dplyr::filter(Season == 2025)

m2_window <- rank_window(box, "BoxOnlyValue") %>%
  dplyr::select(Rank, PLAYER_NAME, BoxOnlyValue)

m2_tbl <- style_gt(
  m2_window,
  title = "Box Score Only",
  numeric_cols = c("BoxOnlyValue"),
  label_overrides = c(BoxOnlyValue = "Value")
)
m2_tbl

# ---- 11 Year-Decayed RAPM (no box prior yet) --------------------
decayed <- readr::read_csv(file.path(DATA_DIR, "rapm_decayed_2025.csv"),
                            show_col_types = FALSE)

active_ids <- single %>% dplyr::pull(playerId) %>% unique()
decayed_active <- decayed %>% dplyr::filter(playerId %in% active_ids)

m3_window <- rank_window(decayed_active, "RAPM_decayed") %>%
  dplyr::select(Rank, PLAYER_NAME, RAPM_decayed, RAPM_decayed__Off, RAPM_decayed__Def)

m3_tbl <- style_gt(
  m3_window,
  title = "11 Year-Decayed RAPM",
  numeric_cols = c("RAPM_decayed", "RAPM_decayed__Off", "RAPM_decayed__Def"),
  label_overrides = c(RAPM_decayed = "Value", RAPM_decayed__Off = "Off", RAPM_decayed__Def = "Def")
)
m3_tbl

# ---- Prior-informed (Bayesian) RAPM ------------------------------
prior <- readr::read_csv(file.path(DATA_DIR, "rapm_prior_informed_2025.csv"),
                          show_col_types = FALSE)

m4_window <- rank_window(prior, "RAPM_prior_informed") %>%
  dplyr::select(Rank, PLAYER_NAME, RAPM_prior_informed,
                RAPM_prior_informed__Off, RAPM_prior_informed__Def)

m4_tbl <- style_gt(
  m4_window,
  title = "Prior-informed (Bayesian) RAPM",
  numeric_cols = c("RAPM_prior_informed", "RAPM_prior_informed__Off", "RAPM_prior_informed__Def"),
  label_overrides = c(RAPM_prior_informed = "Value", RAPM_prior_informed__Off = "Off", RAPM_prior_informed__Def = "Def")
)
m4_tbl

# ---- Recency-weighted, prior-informed RAPM (final) ---------------
souped <- readr::read_csv(file.path(DATA_DIR, "rapm_souped_up_2025.csv"),
                           show_col_types = FALSE)

souped_active <- souped %>% dplyr::filter(playerId %in% active_ids)

m5_window <- rank_window(souped_active, "RAPM_souped_up") %>%
  dplyr::select(Rank, PLAYER_NAME, RAPM_souped_up,
                RAPM_souped_up__Off, RAPM_souped_up__Def)

m5_tbl <- style_gt(
  m5_window,
  title = "Recency-weighted, prior-informed RAPM",
  numeric_cols = c("RAPM_souped_up", "RAPM_souped_up__Off", "RAPM_souped_up__Def"),
  label_overrides = c(RAPM_souped_up = "Value", RAPM_souped_up__Off = "Off", RAPM_souped_up__Def = "Def")
)
m5_tbl

# ---- Save all five tables as PNGs to the Desktop -----------------
# Two-step process per table, each step doing one job:
#   1. gt::gtsave() renders the table (no border) through a headless
#      browser at zoom = 2 -- a sharp, oversized raw capture. Its
#      exact pixel dimensions don't matter here, since step 2 resizes
#      it to a fixed target anyway.
#   2. magick resizes that raw capture down to exactly FINAL_WIDTH
#      pixels wide (high-quality resampling -- downsizing a 2x source
#      is what actually produces a crisp result, not a bigger zoom
#      number) and adds a real BORDER_WIDTH-px solid border in
#      ACCENT_COLOR on all four sides, uniformly, as actual pixels --
#      not a CSS property that has to survive browser rendering.
#
# One-time setup if you don't already have these:
#   install.packages("webshot2")
#   install.packages("magick")   # wraps ImageMagick; on macOS, if you
#                                 # don't already have ImageMagick itself,
#                                 # `brew install imagemagick` first.

library(magick)

OUTPUT_DIR   <- path.expand("~/Desktop")
FINAL_WIDTH  <- 400   # total output width in px, border included (~57% of the original 700)
BORDER_WIDTH <- 3     # px, each side

tables_to_save <- list(
  "jaylen_brown_rapm.png"                   = m1_tbl,
  "jaylen_brown_box_score_only.png"         = m2_tbl,
  "jaylen_brown_11yr_decayed_rapm.png"      = m3_tbl,
  "jaylen_brown_prior_informed_rapm.png"    = m4_tbl,
  "jaylen_brown_recency_weighted_rapm.png"  = m5_tbl
)

for (fname in names(tables_to_save)) {
  out_path <- file.path(OUTPUT_DIR, fname)
  raw_path <- tempfile(fileext = ".png")

  gt::gtsave(tables_to_save[[fname]], filename = raw_path,
             zoom = 2, expand = 0, vwidth = 1600, vheight = 1400, delay = 1)

  content_width <- FINAL_WIDTH - 2 * BORDER_WIDTH
  img <- magick::image_read(raw_path) %>%
    magick::image_resize(paste0(content_width, "x")) %>%
    magick::image_border(color = ACCENT_COLOR,
                          geometry = paste0(BORDER_WIDTH, "x", BORDER_WIDTH))

  magick::image_write(img, path = out_path)
  unlink(raw_path)
  cat("Saved:", out_path, "\n")
}
