# ── Brand palette function — returns a function(n) like scales::hue_pal() ──────

#' @param type  "categorical" (default) — hue-distinct colours for discrete series
#'              "sequential"  — single-hue lightness ramp (for continuous fills)
#'              "diverging"   — teal ↔ navy through light centre
#' @param direction  1 = natural order, -1 = reverse
#' @param na_colour  colour for NA values

brand_pal <- function(
    type      = c("categorical", "sequential", "diverging"),
    direction = 1,
    na_colour = "#d0d3d4"
) {
  type <- match.arg(type)

  # ── Anchor colours (original 7 in hue order) ─────────────────────────────
  anchors <- c(
    "#3db28c",   # teal green      H162
    "#7db356",   # sage green      H96   [added]
    "#f98e2b",   # amber orange    H33
    "#a84c6f",   # raspberry       H337  [added]
    "#475da7",   # slate blue      H228
    "#29235c",   # deep navy       H247
    "#addad9"    # ice teal        H181  (lighter, used for n > 7)
  )

  anchors <- c(
    "#f98e2b",   # amber       H33   — start: warm anchor
    "#475da7",   # slate blue  H228  — ~180° away, maximum contrast
    "#3db28c",   # teal green  H162  — splits the green-blue gap
    "#a84c6f",   # raspberry   H337  — splits the warm gap
    "#7db356",   # sage green  H96   — fills green quarter
    "#29235c",   # deep navy   H247  — darker blue variant
    "#addad9"    # ice teal    H181  — light, used last
  )

  # ── Interpolate up to max_n colours by linearly blending adjacent anchors ─
  max_n <- 14L

  expanded <- local({
    out <- character(length(anchors) * 2)
    hex_to_rgb <- function(h) {
      c(strtoi(substr(h,2,3),16),
        strtoi(substr(h,4,5),16),
        strtoi(substr(h,6,7),16))
    }
    rgb_to_hex <- function(rgb) {
      sprintf("#%02x%02x%02x",
              as.integer(round(rgb[1])),
              as.integer(round(rgb[2])),
              as.integer(round(rgb[3])))
    }
    n <- length(anchors)
    idx <- 1L
    for (i in seq_len(n)) {
      out[idx] <- anchors[i]; idx <- idx + 1L
      j <- if (i == n) 1L else i + 1L
      mid <- (hex_to_rgb(anchors[i]) + hex_to_rgb(anchors[j])) / 2
      out[idx] <- rgb_to_hex(mid); idx <- idx + 1L
    }
    out[seq_len(max_n)]
  })

  # ── Sequential ramp: light tint → teal green ────────────────────────────
  sequential_ramp <- colorRampPalette(c("#e8f7f2", "#3db28c", "#1a6b54"))

  # ── Diverging ramp: teal ↔ silver ↔ navy ────────────────────────────────
  diverging_ramp <- colorRampPalette(c("#3db28c", "#f0f2f4", "#29235c"))

  # ── Return the palette function ──────────────────────────────────────────
  function(n) {
    if (n < 1) stop("`n` must be >= 1")

    cols <- switch(type,
                   categorical = {
                     if (n > max_n) {
                       warning(sprintf(
                         "brand_pal(): only %d distinct colours defined; recycling for n = %d.\n",
                         max_n, n
                       ))
                     }
                     expanded[((seq_len(n) - 1L) %% max_n) + 1L]
                   },
                   sequential = sequential_ramp(n),
                   diverging  = diverging_ramp(n)
    )

    if (direction == -1) cols <- rev(cols)
    cols
  }
}


# ── Drop-in scale functions for ggplot2 ──────────────────────────────────────

scale_colour_brand <- function(type = "categorical", direction = 1, ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette    = brand_pal(type = type, direction = direction),
    na.value   = "#d0d3d4",
    ...
  )
}

scale_fill_brand <- function(type = "categorical", direction = 1, ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette    = brand_pal(type = type, direction = direction),
    na.value   = "#d0d3d4",
    ...
  )
}

# Continuous variants
scale_colour_brand_c <- function(type = "sequential", direction = 1, ...) {
  ggplot2::scale_colour_gradientn(
    colours = brand_pal(type = type, direction = direction)(256),
    na.value = "#d0d3d4",
    ...
  )
}

scale_fill_brand_c <- function(type = "sequential", direction = 1, ...) {
  ggplot2::scale_fill_gradientn(
    colours = brand_pal(type = type, direction = direction)(256),
    na.value = "#d0d3d4",
    ...
  )
}

anchors <- c(
  "#3db28c", "#f98e2b",   # amber       H33   — start: warm anchor
  "#475da7",   # slate blue  H228  — ~180° away, maximum contrast
  "#a84c6f",   # raspberry   H337  — splits the warm gap
  "#7db356",   # sage green  H96   — fills green quarter
  "#29235c",   # deep navy   H247  — darker blue variant
  "#addad9"    # ice teal    H181  — light, used last
)

hue_order <- function(hex) {
  hex_to_hue <- function(h) {
    r <- strtoi(substr(h, 2, 3), 16) / 255
    g <- strtoi(substr(h, 4, 5), 16) / 255
    b <- strtoi(substr(h, 6, 7), 16) / 255
    max_c <- max(r, g, b)
    min_c <- min(r, g, b)
    if (max_c == min_c) return(0)
    d <- max_c - min_c
    hue <- switch(which.max(c(r, g, b)),
                  `1` = (g - b) / d + ifelse(g < b, 6, 0),
                  `2` = (b - r) / d + 2,
                  `3` = (r - g) / d + 4
    )
    (hue / 6) * 360
  }
  hues <- vapply(hex, hex_to_hue, numeric(1))
  hex[order(hues)]
}
x <- hue_order(anchors)
x <- c(x, x)
id <- which(x == anchors[1])
x <- x[id[1]:(id[2]-1)]

colorRampPalette(colors = x)(2)

"#3DB28C"
"#3DB28C" "#7DB356"
"#3DB28C" "#29235C" "#7DB356"
"#3DB28C" "#475DA7" "#A84C6F" "#7DB356"
"#3DB28C" "#799BC0" "#29235C" "#D06D4C" "#7DB356"
"#3DB28C" "#98C0CF" "#3A4589" "#753B67" "#E88038" "#7DB356"
"#3DB28C" "#ADDAD9" "#475DA7" "#29235C" "#A84C6F" "#F98E2B" "#7DB356"
"#3DB28C" "#9DD4CE" "#6480B5" "#353B7C" "#5F3464" "#BF5E5B" "#E79331" "#7DB356"
"#3DB28C" "#91D0C5" "#799BC0" "#3F4E94" "#29235C" "#88416A" "#D06D4C" "#D99735" "#7DB356"
"#3DB28C" "#87CCBF" "#8BB0C8" "#475DA7" "#333675" "#533062" "#A84C6F" "#DD7741" "#CF9A39" "#7DB356"

"#3db28c" "#addad9" "#475da7" "#29235c" "#a84c6f" "#f98e2b" "#7db356" "#3db28c"

"#F8766D" "#00BFC4"
"#F8766D" "#00BA38" "#619CFF"
"#F8766D" "#7CAE00" "#00BFC4" "#C77CFF"
"#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"
"#F8766D" "#B79F00" "#00BA38" "#00BFC4" "#619CFF" "#F564E3"
"#F8766D" "#C49A00" "#53B400" "#00C094" "#00B6EB" "#A58AFF" "#FB61D7"
"#F8766D" "#CD9600" "#7CAE00" "#00BE67" "#00BFC4" "#00A9FF" "#C77CFF" "#FF61CC"
"#F8766D" "#D39200" "#93AA00" "#00BA38" "#00C19F" "#00B9E3" "#619CFF" "#DB72FB" "#FF61C3"
"#F8766D" "#D89000" "#A3A500" "#39B600" "#00BF7D" "#00BFC4" "#00B0F6" "#9590FF" "#E76BF3" "#FF62BC"

