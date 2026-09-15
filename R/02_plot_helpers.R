# ============================================================================
# 02_plot_helpers.R
# Shared visual language: one theme, one fixed colour palette, and reusable
# ggplot2 building blocks so every chart in the dashboard looks consistent.
# (Applies the module's visual-analytics design principles: consistency,
#  restrained colour, clear labels, reduced clutter, colourblind-safe hues.)
# ============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# --- Fixed colour mapping for the five discovery groups ----------------------
# Same colour means the same method on EVERY tab. Colourblind-considered.
# Okabe-Ito (2008) colourblind-safe qualitative palette. CVD simulation confirms
# all five method pairs stay distinguishable (worst-case deltaE ~16 under
# deuteranopia); replaces an earlier set that had a deuteranopia blue/green clash.
METHOD_COLOURS <- c(
  "Transit"         = "#0072B2",  # blue
  "Radial Velocity" = "#E69F00",  # orange
  "Microlensing"    = "#009E73",  # bluish green
  "Imaging"         = "#CC79A7",  # reddish purple
  "Other"           = "#666666"   # grey
)

# Wrapped over two lines: the filter rail leaves some charts in ~460px columns,
# where a 67-character caption overflows the panel. Wrapping is width-independent,
# unlike shrinking the type.
SOURCE_CAPTION <- paste("Source: NASA Exoplanet Archive, PSCompPars;",
                        "downloaded 21 July 2026", sep = "\n")

# Wrap chart subtitles to a fixed measure so they never clip in a narrow column.
# ggplot does not reflow text, so this is done explicitly at build time.
wrap_sub <- function(x, width = 52) paste(strwrap(x, width = width), collapse = "\n")

# Cache-busting version, included in EVERY bindCache() key. Chart appearance/code
# is not part of the automatic cache key (only data inputs are), so a chart-code
# or palette change can otherwise serve a stale cached image at an unchanged
# filter state. Bump this string whenever a chart builder's output changes.
#   v1 -> initial cached charts
#   v2 -> size-mix rebuilt as single-series labelled bar + sequential SIZE_RAMP
#   v3 -> facility bar re-pointed off Transit blue to BAR_PRIMARY (Role 3 indigo)
#   v4 -> new Host stars tab (star-temp histogram + system-multiplicity bar)
#   v5 -> Planet-diversity restructure + Earth-context layout (chart heights changed)
#   v6 -> host-star temp histogram now per-star + capped x-axis (J2/J3)
#   v7 -> chart text renders in Inter; subtitles/caption/axis labels wrapped to fit
#   v8 -> all 10 charts interactive via as_interactive() (plotly, curated toolbar)
#   v9 -> axes unlocked so every chart shows the same 4 buttons; legend/subtitle fits
#   v10 -> full default plotly modebar restored on all 10 charts
#   v11 -> modebar hidden until hover, trimmed to the 7 navigation tools
#   v12 -> autosize + responsive so charts fill their card width
#   v13 -> ResizeObserver refits charts on any container resize (tab/rail/window)
#   v14 -> force-fit module: re-fit on render/tab/rail/window + post-transition pass
#   v15 -> width-guarded self-correcting re-fit (body-class observer, retry window)
#   v16 -> restore autosize instead of Plots.resize (which pins width + kills autosize)
#   v17 -> Host-stars label headroom: correct histogram peak + top-only y expansion
#   v18 -> Sun marker becomes a segment so its label sits above it (no line-through)
#   v19 -> bar value labels offset in DATA space (ggplotly drops vjust/hjust)
#   v24 -> clearer chart-specific empty states for narrow filter selections
#   v25 -> responsive axis-title wrapping, headroom, and shared chart typography
#   v26 -> fixed three-column facet geometry and added vertical row spacing
#   v27 -> Detection-bias facets use WebGL and no longer build unused hover text
#   v28 -> Detection-bias facet rows spaced so strip labels never overlap panels
CACHE_VERSION <- "v28"

# Sequential single-hue ramp for the ORDINAL size variable (Earth-size -> Giant).
# Shade encodes size order: palest for Earth-size, darkening to the deepest for
# Giant, whose darkest step is the UI accent hue (#3B4E8C). One graduating indigo
# hue (a sequential palette, per the Visual Analytics lecture) so it is never
# confused with the categorical five-method palette. Ordered lightest -> darkest.
SIZE_RAMP <- c(
  "Earth-size (<1.25 R Earth)"    = "#DFE3F0",
  "Super-Earth (1.25-<2 R Earth)" = "#B7BFDD",
  "Sub-Neptune (2-<4 R Earth)"    = "#8A93C4",
  "Neptune-size (4-<6 R Earth)"   = "#5E69A8",
  "Giant (>=6 R Earth)"           = "#3B4E8C"
)

# Single-series quantitative fill (Role 3) — a neutral brand colour, deliberately
# NOT a method hue, so a plain count bar reads as "a data bar", never as Transit.
# Mid-indigo from the same family as the size ramp (COLOUR_SYSTEM.md Role 3).
BAR_PRIMARY <- "#5E69A8"

# Discrete-axis labels that append the plotted n per group, so every box/panel
# honestly reports how many planets it summarises (chart-craft standard §5).
# Method names are broken at spaces as well ("Radial Velocity" -> two lines) so
# five groups still fit without colliding in a narrow column.
group_n_labels <- function(groups) {
  tb <- table(groups)
  function(lv) paste0(gsub(" ", "\n", lv), "\n(n=",
                      format(as.integer(tb[lv]), big.mark = ",", trim = TRUE), ")")
}

# Appended to a chart subtitle when the plotted sample is small enough that
# patterns may not be meaningful (chart-craft standard §5: "too few points").
few_points_note <- function(n, threshold = 30) {
  if (n > 0 && n < threshold)
    sprintf(" — few data points (n=%s); read with caution", comma(n))
  else ""
}

# plotly builds its legend from traces and silently drops any method with no
# points, so a filtered plotly scatter can show fewer than five legend entries.
# This returns one invisible seed point per absent method (placed at the data
# centroid so axes/counts are unaffected; rendered at size ~0 so it never shows),
# which restores a consistent five-method legend. Used with itemsizing="constant"
# in the render so every legend swatch is the same visible size.
seed_missing_methods <- function(d, xvar, yvar) {
  missing <- setdiff(levels(d$discovery_group), as.character(unique(d$discovery_group)))
  if (length(missing) == 0) return(NULL)
  data.frame(
    sx = median(d[[xvar]], na.rm = TRUE),
    sy = median(d[[yvar]], na.rm = TRUE),
    discovery_group = factor(missing, levels = levels(d$discovery_group))
  )
}

# --- One theme for the whole app --------------------------------------------
# base_family: charts render in Inter — the same face as the UI — via the TTFs
# bundled in www/fonts/ and registered with sysfonts/showtext at startup in
# app.R, so plot text matches the interface instead of the device's generic
# sans. ggplot2 >= 4.0 resolves geom_text/geom_label `family` from the theme, so
# this one setting also covers in-chart value labels, the Earth/Sun markers and
# the signature-chart callout. Falls back to the device sans if the font is
# unavailable (CHART_FONT is "" in that case).
theme_exo <- function(base_size = 13,
                      base_family = if (exists("CHART_FONT")) CHART_FONT else "Inter") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(colour = "grey30", size = base_size - 1),
      plot.caption  = element_text(colour = "grey45", size = base_size - 3),
      axis.title    = element_text(colour = "grey20"),
      legend.position = "bottom",
      legend.title  = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin   = margin(10, 14, 10, 10)
    )
}

# ----------------------------------------------------------------------------
# as_interactive(): the SINGLE shared plotly treatment every chart passes
# through, so all ten charts behave identically.
#
# It exists because ggplotly() alone is not enough for this dashboard:
#   1. ggplotly DROPS labs(subtitle) and labs(caption). Those carry the valid-n
#      count and the data source, which every chart must keep, so they are
#      re-attached as paper-space annotations. They are APPENDED, never assigned:
#      ggplotly stores axis titles and facet strip labels as annotations too, so
#      overwriting the list would silently delete them.
#   2. plotly text is rendered by the browser, not by showtext, so the Inter
#      stack has to be set explicitly on the layout, the axes, the annotations
#      AND the hover label (hover is styled separately from everything else).
#   3. The modebar is hidden until the pointer is over a chart
#      (displayModeBar = "hover"), so the default view stays clean, and is
#      trimmed to the recognisable navigation tools: download, zoom, pan,
#      zoom in, zoom out, fit and reset. The selection tools, the spike-line
#      and hover-mode toggles and the Plotly logo are removed. The same config
#      is applied to all ten charts, so the toolbar never varies between them.
#
# Axes are left zoomable everywhere. Honest baselines are protected at the
# source instead: the bar and histogram scales start at zero by default (the
# ggplot scale expansions), and zooming is a reversible user action that always
# resets, rather than a state the reader can be silently landed in.
# ----------------------------------------------------------------------------
PLOTLY_FONT <- "Inter, system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"

# Removed from the modebar: the two selection tools (we never act on a selection)
# and the spike-line / hover-mode toggles (display switches, not navigation).
PLOTLY_STRIP <- c("select2d", "lasso2d", "toggleSpikelines",
                  "hoverClosestCartesian", "hoverCompareCartesian")

as_interactive <- function(p, filename = "chart", webgl = FALSE, tooltip = "text") {
  # Capture before ggplotly discards them.
  sub <- p$labels$subtitle
  cap <- p$labels$caption
  to_html  <- function(x) gsub("\n", "<br>", x)
  n_lines  <- function(x) if (is.null(x)) 0L else length(strsplit(x, "\n")[[1]])

  b <- plotly::plotly_build(plotly::ggplotly(p, tooltip = tooltip))

  # WebGL only for the two dense scatters; the aggregated charts are small and
  # stay SVG. ggplotly sets `hoveron`, which scattergl rejects with a warning.
  if (webgl) {
    b$x$data <- lapply(b$x$data, function(tr) { tr$hoveron <- NULL; tr })
    b <- plotly::plotly_build(plotly::toWebGL(b))
  }

  # Axis typography; covers facet axes (xaxis2, yaxis2, ...). Ranges are left
  # unlocked so every chart offers the same zoom/pan/reset tools.
  #
  # Long axis titles need special handling in Plotly. A y-axis title is rotated
  # vertically, so a long one can be taller than a compact dashboard card and be
  # clipped at the top or bottom. This was most visible for "Planet mass or
  # minimum mass (Earth masses)", but the same risk exists for temperature and
  # host-star labels. Wrap the title in the Plotly layout itself, rather than
  # shortening the scientific label, and give the chart a predictable left
  # margin. The result is responsive across the full-width and half-width cards.
  long_y_title <- FALSE
  for (nm in grep("^[xy]axis", names(b$x$layout), value = TRUE)) {
    axis <- b$x$layout[[nm]]
    title <- axis$title
    title_text <- if (is.list(title)) title$text else title
    if (length(title_text) == 1L && !is.na(title_text) && nzchar(title_text)) {
      # Convert any existing line break to a space before re-wrapping, then use
      # an HTML break because Plotly annotations and axis titles render HTML.
      plain_title <- gsub("<br\\s*/?>|\\n", " ", title_text, perl = TRUE)
      is_y <- grepl("^yaxis", nm)
      wrap_width <- if (is_y) 28 else 36
      wrapped_title <- paste(strwrap(plain_title, width = wrap_width), collapse = "<br>")
      if (is.list(title)) {
        b$x$layout[[nm]]$title$text <- wrapped_title
      } else {
        b$x$layout[[nm]]$title <- list(text = wrapped_title)
      }
      if (is_y && nchar(plain_title) > 30) long_y_title <- TRUE
      title_size <- if (is_y && nchar(plain_title) > 30) 12 else 13
      b$x$layout[[nm]]$titlefont <- list(family = PLOTLY_FONT,
                                          size = title_size, color = "#333333")
    }
    b$x$layout[[nm]]$fixedrange <- FALSE
    b$x$layout[[nm]]$tickfont  <- list(family = PLOTLY_FONT, size = 12, color = "#4D4D4D")
  }

  # Re-attach the subtitle and the source caption, BOTH in the top band:
  # caption right-aligned on its own line, subtitle left-aligned beneath it.
  #
  # Why the top and not the conventional bottom-right: plotly annotations only
  # accept yref "paper" or an axis (yref = "container" exists for the legend
  # from plotly.js 2.24, but not for annotations), so a bottom caption has to be
  # offset by a pixel guess against the axis ticks, the axis title and the
  # legend. ggplotly does not expose those heights reliably, and the guess broke
  # as soon as a chart had a legend or a two-line axis title — the caption
  # landed on top of them. Anchoring both lines to the top is deterministic at
  # any chart width or height, and they sit on separate rows so they can never
  # collide with each other. Keeping the caption INSIDE the figure (rather than
  # as HTML beside it) also means the downloaded PNG carries its own source
  # attribution.
  n_sub <- n_lines(sub); n_cap <- n_lines(cap)
  sub_px <- if (n_sub > 0) n_sub * 18 + 12 else 0
  cap_px <- if (n_cap > 0) n_cap * 12 + 8 else 0   # tracks the 9px caption
  has_legend <- isTRUE(b$x$layout$showlegend)
  # One row of 12px entries plus breathing room. Was 64 when the legend also
  # carried a stacked title; keeping 64 would leave dead space where it used to sit.
  leg_px <- if (has_legend) 36 else 0
  # Faceted charts draw their strip labels AT paper y = 1 — the same anchor the
  # subtitle uses — so the top band has to clear the strip boxes as well.
  has_facets <- any(grepl("^xaxis[0-9]", names(b$x$layout)))

  # ggplotly sizes each grey strip RECTANGLE from ONE line of strip text plus the
  # theme margin, and never counts the second line: measured 18.60px = 10.6px of
  # text + 8px of margin, and adding a second line left it at 18.60px while the
  # label itself needed ~23px. So a two-line strip label ("Method" / "(n=...)")
  # overflowed its own box at EVERY width. Raising strip.text's margin does grow
  # the rectangle, but only by guessing a number that happens to fit — it does not
  # track the label. Instead, measure the label and size the box to it.
  # Strip annotations are the only ones ggplotly anchors with yanchor = "bottom"
  # (axis titles use "top"/"center"), which is what identifies them here.
  strip_px <- 0
  if (has_facets) {
    ann <- b$x$layout$annotations
    si  <- which(vapply(ann, function(a)
      identical(a$yanchor, "bottom") && !is.null(a$text) && nzchar(a$text),
      logical(1)))
    if (length(si)) {
      fs    <- max(vapply(ann[si], function(a) a$font$size %||% 11, numeric(1)))
      nline <- max(vapply(ann[si], function(a)
                          length(strsplit(a$text, "<br\\s*/?>")[[1]]), integer(1)))
      pad   <- 5
      box_h <- nline * fs * 1.25 + 2 * pad
      # Lift the text off the panel edge so it is padded inside the taller box.
      for (i in si) b$x$layout$annotations[[i]]$yshift <- pad
      b$x$layout$shapes <- lapply(b$x$layout$shapes, function(sh) {
        if (identical(sh$ysizemode, "pixel") && !is.null(sh$fillcolor) &&
            grepl("^rgba\\(217,217,217", sh$fillcolor)) sh$y1 <- box_h
        sh
      })
      strip_px <- box_h
    }
  }

  m <- b$x$layout$margin
  if (is.null(m)) m <- list(t = 30, b = 40, l = 60, r = 20)
  # Preserve enough room for y-axis ticks and the wrapped title. This is a
  # minimum, so charts with naturally larger ggplot margins keep those values.
  # It also keeps the long-title treatment aligned across cards.
  m$l <- max(m$l %||% 60, if (long_y_title) 82 else 70)
  m$r <- max(m$r %||% 20, 20)

  extra <- list()
  if (n_sub > 0) extra <- c(extra, list(list(
    text = to_html(sub), x = 0, xref = "paper", xanchor = "left",
    y = 1, yref = "paper", yanchor = "bottom", yshift = 12 + strip_px,
    align = "left", showarrow = FALSE,
    font = list(family = PLOTLY_FONT, size = 12.5, color = "#4D4D4D"))))
  if (n_cap > 0) extra <- c(extra, list(list(
    text = to_html(cap), x = 1, xref = "paper", xanchor = "right",
    y = 1, yref = "paper", yanchor = "bottom", yshift = sub_px + 10 + strip_px,
    align = "right", showarrow = FALSE,
    # Recedes by SIZE, not by colour: #737373 on white is 4.74:1, just over the
    # WCAG AA 4.5:1 floor for small text (QA_CHECKLIST §6). Lightening further
    # would have failed AA, so the caption is shrunk instead of greyed out.
    font = list(family = PLOTLY_FONT, size = 9, color = "#737373"))))
  # APPEND — ggplotly keeps axis titles and facet strips in this same list.
  b$x$layout$annotations <- c(b$x$layout$annotations, extra)

  # Top margin holds both lines; the bottom only has to clear the legend band.
  m$t <- (m$t %||% 30) + sub_px + cap_px + strip_px
  m$b <- (m$b %||% 40) + leg_px

  # A bottom legend is also pinned to the container, sitting just above the
  # caption, so it can never land on top of the x-axis title.
  leg <- list(font = list(family = PLOTLY_FONT, size = 12))
  if (has_legend) {
    # No title block any more (the scales set name = NULL) — just the centred
    # row of named swatches, pinned to the container bottom.
    leg <- c(leg, list(orientation = "h", x = 0.5, xanchor = "center",
                       y = 0, yref = "container", yanchor = "bottom",
                       yshift = cap_px + 4, itemsizing = "constant"))
  }

  b <- plotly::layout(b,
    # Fill the card: without autosize plotly renders at its default ~640px and
    # leaves an empty band to the right of the wider (col-8) charts.
    autosize   = TRUE,
    margin     = m,
    font       = list(family = PLOTLY_FONT, size = 13, color = "#141A24"),
    hoverlabel = list(font = list(family = PLOTLY_FONT, size = 12.5),
                      bgcolor = "#FFFFFF", bordercolor = "#E6E9EF"),
    legend     = leg
  )

  plotly::config(b,
    responsive = TRUE,                # re-lay out when the container resizes
    displayModeBar = "hover",         # hidden until the chart is hovered
    displaylogo = FALSE,
    # Leaves: toImage, zoom2d, pan2d, zoomIn2d, zoomOut2d, autoScale2d,
    # resetScale2d -> download, zoom, pan, zoom in, zoom out, fit, reset.
    modeBarButtonsToRemove = PLOTLY_STRIP,
    toImageButtonOptions = list(format = "png", filename = filename,
                                scale = 2, width = 1200, height = 750))
}

# Friendly empty state for renderPlotly. renderPlot could lean on
# validate(need()), but a plotly output needs its own blank figure carrying the
# message, otherwise an empty selection surfaces as an error.
empty_plotly <- function(msg = "No planets match the current filters. Widen the year range, select more methods, or try another facility.") {
  # An explicit empty scatter trace: plot_ly() with no trace at all warns
  # ("No trace type specified"), which would breach the clean-console rule
  # every time a filter selection came back empty.
  plotly::plot_ly(x = numeric(0), y = numeric(0),
                  type = "scatter", mode = "markers", hoverinfo = "skip") |>
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      margin = list(t = 20, b = 20, l = 20, r = 20),
      annotations = list(list(
        text = gsub("\n", "<br>", msg), x = 0.5, y = 0.5,
        xref = "paper", yref = "paper", showarrow = FALSE, align = "center",
        font = list(family = PLOTLY_FONT, size = 14, color = "#5B6472")))) |>
    plotly::config(displayModeBar = FALSE)
}

# Reusable colour scales keyed to the discovery groups
# name = NULL, so ggplotly renders no legend title. With the legend centred
# (x = 0.5) plotly anchors the title to the legend's LEFT edge, so a title sat
# far left above centred swatches and read as a misalignment. The five NAMED
# swatches are self-documenting, and each tab's copy already frames colour as
# the discovery method.
scale_colour_method <- function(...) {
  scale_colour_manual(values = METHOD_COLOURS, drop = FALSE, name = NULL, ...)
}
scale_fill_method <- function(...) {
  scale_fill_manual(values = METHOD_COLOURS, drop = FALSE, name = NULL, ...)
}

# ----------------------------------------------------------------------------
# Chart builders. Each takes an already-filtered data frame and returns a
# ggplot. Plot-specific complete cases are applied INSIDE each builder, and
# the number displayed is reported in the subtitle.
# ----------------------------------------------------------------------------

# Discovery timeline: planets per year, stacked by method group.
# mode = "annual" or "cumulative".
plot_timeline <- function(df, mode = "annual") {
  d <- df[!is.na(df$disc_year), ]
  counts <- as.data.frame(table(disc_year = d$disc_year, discovery_group = d$discovery_group))
  counts$disc_year <- as.integer(as.character(counts$disc_year))
  if (mode == "cumulative") {
    counts <- counts[order(counts$disc_year), ]
    counts <- do.call(rbind, lapply(split(counts, counts$discovery_group), function(g) {
      g$Freq <- cumsum(g$Freq); g
    }))
    ylab <- "Cumulative confirmed planets"
    subtitle <- sprintf("Cumulative totals; %s planets in current selection", comma(nrow(d)))
  } else {
    ylab <- "Confirmed planets"
    subtitle <- sprintf("Annual counts; %s planets in current selection", comma(nrow(d)))
  }
  counts$tip <- paste0(counts$discovery_group, " — ", comma(counts$Freq),
                       " planets in ", counts$disc_year)
  ggplot(counts, aes(disc_year, Freq, fill = discovery_group, text = tip)) +
    geom_col(width = 0.9) +
    scale_fill_method() +
    scale_y_continuous(labels = comma) +
    labs(
      subtitle = subtitle,
      x = "Discovery year", y = ylab, caption = SOURCE_CAPTION
    ) +
    theme_exo()
}

# Top-N facilities bar chart. metric = "count" or "share".
plot_facilities <- function(df, top_n = 10, metric = "count") {
  d <- df[!is.na(df$disc_facility), ]
  tab <- sort(table(d$disc_facility), decreasing = TRUE)
  tab <- tab[tab > 0]
  keep <- names(tab)[seq_len(min(top_n, length(tab)))]
  d$fac <- ifelse(as.character(d$disc_facility) %in% keep,
                  as.character(d$disc_facility), "Other facilities")
  ct <- as.data.frame(table(fac = d$fac))
  ct$fac <- factor(ct$fac, levels = ct$fac[order(ct$Freq)])
  ct$val <- if (metric == "share") 100 * ct$Freq / sum(ct$Freq) else ct$Freq
  xlab <- if (metric == "share") "Share of selected planets (%)" else "Number of planets"
  ct$tip <- paste0(ct$fac, " — ", comma(ct$Freq), " planets",
                   if (metric == "share") paste0(" (", round(ct$val, 1), "%)") else "")
  ggplot(ct, aes(val, fac, text = tip)) +
    geom_col(fill = BAR_PRIMARY) +
    scale_x_continuous(labels = if (metric == "share") label_number(suffix = "%") else comma) +
    labs(
      subtitle = wrap_sub("Remaining facilities grouped as \"Other facilities\"", width = 30),
      x = xlab, y = NULL, caption = SOURCE_CAPTION
    ) +
    theme_exo()
}

# Size-mix bar: one bar per size band, counts labelled at the bar end.
# Single series (no method stacking, no legend) shaded by a sequential indigo
# ramp keyed to size order, so the question "which sizes dominate this
# selection?" and the ordinal progression both read at a glance.
# Bands read top-to-bottom in natural size order: Earth-size -> Giant.
plot_size_mix <- function(df) {
  d <- df[!is.na(df$planet_size_group), ]
  lv <- levels(d$planet_size_group)
  ct <- as.data.frame(table(size = d$planet_size_group), stringsAsFactors = FALSE)
  ct$size <- factor(ct$size, levels = lv)   # keeps empty bands on the axis
  ct$tip <- paste0(sub("\\s*\\(.*$", "", ct$size), " — ", comma(ct$Freq), " planets")
  # Same ggplotly limitation as the multiplicity chart: hjust is dropped and the
  # label is centred on the bar end, so the count must be offset in DATA space.
  ct$lab_y <- ct$Freq + max(ct$Freq) * 0.075
  ggplot(ct, aes(size, Freq, fill = size, text = tip)) +
    geom_col(width = 0.72) +
    # Sequential shade encodes size order; y-axis labels name the bands, so no
    # legend is needed. Value labels sit OUTSIDE the bar end (grey25 on the panel
    # background), so they stay readable even against the darkest Giant bar.
    scale_fill_manual(values = SIZE_RAMP, guide = "none") +
    geom_text(aes(y = lab_y, label = comma(Freq)), size = 3.4, colour = "grey25") +
    # rev() puts Earth-size at the top once coord_flip() is applied; the label
    # function strips the "(... R Earth)" range, which moves to the subtitle.
    scale_x_discrete(limits = rev(lv),
                     labels = function(x) sub("\\s*\\(.*$", "", x)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                       breaks = breaks_extended(4), labels = comma) +
    coord_flip() +
    labs(
      # wrap_sub rather than hand-placed \n: the manual 63-character band line
      # overflowed the card on every narrow layout (measured +189px at 1000px
      # rail-open). 48 keeps it inside the panel, which starts after the y-margin.
      subtitle = wrap_sub(sprintf(paste0(
        "%s planets with a reported radius%s. ",
        "Bands by radius (Earth radii): Earth <1.25, Super-Earth 1.25-2, ",
        "Sub-Neptune 2-4, Neptune 4-6, Giant 6+"),
        comma(nrow(d)), few_points_note(nrow(d))), width = 48),
      x = NULL, y = "Planets", caption = SOURCE_CAPTION
    ) +
    theme_exo(11) +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),   # no gridlines along the bands
          plot.margin = margin(10, 16, 10, 14))
}

# The Sun's effective temperature (K) — reference marker for the host-star chart.
SUN_TEFF <- 5772

# Host-star temperature histogram. Single series (Role 3 indigo, not a method
# hue) with a dashed Sun reference line. Counted PER STAR (one row per hostname),
# not per planet, so multi-planet systems are not over-weighted. The x-axis is
# capped to a sensible host-star range (rows are kept, only the view is limited).
STAR_TEFF_VIEW <- c(2000, 12000)

plot_star_temp <- function(df) {
  d <- df[!is.na(df$st_teff) & df$st_teff > 0, ]
  d <- d[!duplicated(d$hostname), ]                 # one row per host star
  # Tallest bin, so the Sun label can sit at a finite height in the headroom.
  # The breaks must be CENTRED on multiples of the binwidth (hence the -125
  # half-bin offset), because that is how geom_histogram(binwidth = 250) bins.
  # Aligning them to 0 instead under-counted the tallest bin by ~8% (757 vs 822),
  # which put the label BELOW the tallest bar and on top of the data.
  brks <- seq(floor(min(d$st_teff) / 250) * 250 - 125, max(d$st_teff) + 250, by = 250)
  peak <- max(table(cut(d$st_teff, breaks = brks, include.lowest = TRUE)))
  n_beyond <- sum(d$st_teff < STAR_TEFF_VIEW[1] | d$st_teff > STAR_TEFF_VIEW[2])
  beyond_note <- if (n_beyond > 0) " A few hotter stars lie beyond this range." else ""
  ggplot(d, aes(st_teff)) +
    # fixed binwidth (not a fixed bin count) so the capped view stays fine-grained
    geom_histogram(binwidth = 250, fill = BAR_PRIMARY, colour = "white", linewidth = 0.15) +
    # The Sun marker is a SEGMENT, not geom_vline: a full-height rule reaches the
    # top of the panel, leaving nowhere for its label except beside it - and a
    # sideways nudge cannot be made safe, because ggplotly ignores geom_text's
    # hjust (it centres the text on x) and the label's width in DATA units grows
    # as the card narrows (~1,150 K at a wide card). Stopping the rule just above
    # the bars and centring the label above it is width-independent: the label
    # can never touch the rule or a bar at any card size.
    geom_segment(data = data.frame(x = SUN_TEFF, y = 0, yend = peak * 1.02),
                 aes(x = x, xend = x, y = y, yend = yend), inherit.aes = FALSE,
                 linetype = "dashed", colour = "grey40") +
    # Finite y (not Inf): plotly places Inf outside the visible range, which
    # silently dropped this label from the interactive chart.
    geom_text(data = data.frame(x = SUN_TEFF, y = peak * 1.16, lab = "Sun (reference)"),
              aes(x, y, label = lab), inherit.aes = FALSE,
              colour = "grey30", size = 3, fontface = "bold", vjust = 1) +
    # Explicit breaks that stop short of the capped view, so the last tick label
    # sits inside the panel instead of half-overflowing the right edge.
    scale_x_continuous(labels = comma, breaks = seq(2000, 11000, by = 3000)) +
    # Top-only headroom for the Sun label; baseline stays pinned at 0 so bar
    # heights remain honest.
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
    coord_cartesian(xlim = STAR_TEFF_VIEW) +        # limit view, keep all rows
    labs(
      subtitle = wrap_sub(sprintf(
        "%s host stars with a reported temperature; dashed line = Sun (~5,772 K).%s%s",
        comma(nrow(d)), beyond_note, few_points_note(nrow(d)))),
      x = "Host-star effective temperature (K)", y = "Host stars",
      caption = SOURCE_CAPTION
    ) +
    theme_exo()
}

# Multi-planet systems: host systems by number of known planets. Deduplicate to
# one row per host first (count each system once), then tabulate sy_pnum.
# Single series (Role 3 indigo), counts labelled at the bar ends.
plot_system_multiplicity <- function(df) {
  d <- df[!is.na(df$sy_pnum), ]
  d <- d[!duplicated(d$hostname), ]                 # one row per system
  ct <- as.data.frame(table(pnum = d$sy_pnum), stringsAsFactors = FALSE)
  ct$pnum <- factor(as.integer(ct$pnum),
                    levels = sort(unique(as.integer(ct$pnum))))
  ct$tip <- paste0(comma(ct$Freq), " systems with ", ct$pnum,
                   ifelse(as.integer(as.character(ct$pnum)) == 1, " known planet", " known planets"))
  # Lift each value label ABOVE its bar in DATA space. ggplotly silently DROPS
  # geom_text's vjust (and hjust) and centres the label on the data point, so a
  # vjust nudge leaves every number straddling its own bar top - no amount of
  # axis headroom can fix that, because the label tracks the bar. Offsetting y
  # is the only placement ggplotly preserves.
  lab_gap <- max(ct$Freq) * 0.075
  ct$lab_y <- ct$Freq + lab_gap
  ggplot(ct, aes(pnum, Freq, text = tip)) +
    geom_col(fill = BAR_PRIMARY, width = 0.8) +
    geom_text(aes(y = lab_y, label = comma(Freq)), size = 3.2, colour = "grey25") +
    # Top-only headroom for the lifted label; baseline stays pinned at 0 so bar
    # comparisons remain honest.
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.14))) +
    labs(
      subtitle = wrap_sub(sprintf(
        "%s host systems in the current selection; each system counted once%s",
        comma(nrow(d)), few_points_note(nrow(d)))),
      x = "Known planets in the system", y = "Host systems",
      caption = SOURCE_CAPTION
    ) +
    theme_exo() +
    theme(panel.grid.major.x = element_blank())
}

# Signature scatter: planet radius vs orbital period, both log scales.
# show_earth adds Earth reference lines (context only).
plot_radius_period <- function(df, show_earth = TRUE) {
  d <- df[!is.na(df$pl_rade) & !is.na(df$pl_orbper) &
          df$pl_rade > 0 & df$pl_orbper > 0, ]
  p <- ggplot(d, aes(pl_orbper, pl_rade, colour = discovery_group,
                     text = paste0(
                       pl_name, " (", hostname, ")",
                       "<br>Radius: ", round(pl_rade, 2), " R Earth",
                       "<br>Orbital period: ", round(pl_orbper, 2), " days",
                       "<br>Method: ", discoverymethod,
                       "<br>Year: ", disc_year))) +
    # Alpha 0.55 / size 1.7: at 0.30 on 1.3px the method hues read washed out.
    # Tuned against the dense Transit cluster so the colours are solid without
    # merging into one blob. METHOD_COLOURS hex values are untouched.
    geom_point(alpha = 0.55, size = 1.7)
  seed <- seed_missing_methods(d, "pl_orbper", "pl_rade")
  if (!is.null(seed)) {
    p <- p + geom_point(data = seed, aes(sx, sy, colour = discovery_group),
                        alpha = 0.55, size = 1e-3, inherit.aes = FALSE, show.legend = TRUE)
  }
  earth_note <- ""
  if (show_earth) {
    p <- p +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      geom_vline(xintercept = 365.25, linetype = "dashed", colour = "grey40") +
      # Offset in DATA space (multiplicative, because both axes are log): with
      # hjust/vjust dropped by ggplotly the label would be centred exactly on the
      # crossing of the two dashed Earth lines and sit on top of them.
      geom_text(data = data.frame(pl_orbper = 365.25 * 2.6, pl_rade = 1 * 1.55,
                                  lab = "Earth"),
                aes(pl_orbper, pl_rade, label = lab), inherit.aes = FALSE,
                colour = "grey30", size = 3, fontface = "bold")
    earth_note <- " Dashed lines mark Earth (radius 1 R Earth, period 365 days)."
  }
  # Cautious callout naming the most visible structure (publication treatment).
  # geom_text (not geom_label) because plotly renders text but not label boxes.
  # Only drawn when planets actually occupy that region, so it never labels empty space.
  hot_jup <- d[d$pl_orbper < 10 & d$pl_rade > 6, ]
  if (nrow(hot_jup) >= 30) {
    # Centred, in the sparse upper band (y=24 sits above 99.9% of points; x=300
    # is deep interior on the log-x range 0.11-4e8), so no character is clipped
    # at any window width.
    p <- p +
      geom_text(data = data.frame(pl_orbper = 300, pl_rade = 24,
                                   lab = "Large planets on tight orbits"),
                aes(pl_orbper, pl_rade, label = lab), inherit.aes = FALSE,
                colour = "grey25", fontface = "bold", size = 3.2, hjust = 0.5)
  }
  p +
    scale_x_log10(labels = comma) +
    scale_y_log10(labels = comma) +
    scale_colour_method() +
    labs(
      # wrap_sub, like the other builders: plotly annotations do NOT auto-wrap,
      # so an unwrapped sprintf ran straight past the right edge of the card.
      subtitle = wrap_sub(sprintf("%s planets with reported radius and period; both axes log scale.%s%s",
                                  comma(nrow(d)), earth_note, few_points_note(nrow(d))), width = 76),
      x = "Orbital period (days)",
      y = "Planet radius (Earth radii)",
      caption = SOURCE_CAPTION
    ) +
    theme_exo()
}

# Distribution of radius OR mass by method group (boxplot, log y).
plot_distribution <- function(df, variable = c("pl_rade", "pl_bmasse")) {
  variable <- match.arg(variable)
  d <- df[!is.na(df[[variable]]) & df[[variable]] > 0, ]
  ylab <- if (variable == "pl_rade") "Planet radius (Earth radii)"
          else "Planet mass or minimum mass (Earth masses)"
  ggplot(d, aes(discovery_group, .data[[variable]], fill = discovery_group)) +
    geom_boxplot(alpha = 0.85, outlier.alpha = 0.15) +
    scale_x_discrete(labels = group_n_labels(d$discovery_group)) +
    scale_y_log10(labels = comma) +
    scale_fill_method() +
    labs(
      subtitle = wrap_sub(sprintf("%s planets with a valid value; log scale%s",
                                  comma(nrow(d)), few_points_note(nrow(d)))),
      x = NULL, y = ylab, caption = SOURCE_CAPTION
    ) +
    theme_exo() +
    theme(legend.position = "none", axis.text.x = element_text(size = 9))
}

# Detection-bias facets: radius vs period, one panel per selected method,
# identical axes so the comparison is honest.
plot_bias_facets <- function(df) {
  d <- df[!is.na(df$pl_rade) & !is.na(df$pl_orbper) &
          df$pl_rade > 0 & df$pl_orbper > 0, ]
  fac_n <- table(d$discovery_group)
  # Count on its OWN line, and trim = TRUE: format() pads a vector to a common
  # width, which produced "(n=   12)" — the padding widened the widest strip and
  # read as a typo. Two short lines fit a narrow panel where one long line cannot.
  fac_labs <- setNames(
    paste0(names(fac_n), "\n(n=",
           format(as.integer(fac_n), big.mark = ",", trim = TRUE), ")"),
    names(fac_n))
  # The Detection bias output deliberately has no hover labels: labels were
  # disabled after ggplotly() was built because they could cover a neighbouring
  # facet. Do not construct thousands of unused HTML strings here. Keeping the
  # chart free of a text aesthetic substantially reduces conversion time and
  # the size of the object sent to the browser.
  ggplot(d, aes(pl_orbper, pl_rade, colour = discovery_group)) +
    geom_point(alpha = 0.55, size = 1.5) +
    # Keep the five-method arrangement deterministic. The explicit three-column
    # layout prevents the responsive Plotly conversion from pushing a panel
    # outside the visible chart at narrower card widths.
    facet_wrap(~ discovery_group, ncol = 3,
               labeller = as_labeller(fac_labs)) +
    # Compact tick labels: five panels share a narrow column, where full
    # thousands-separated numbers ("10,000,000") collide along the x-axis.
    scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
    scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
    scale_colour_method() +
    labs(
      subtitle = wrap_sub(sprintf("%s planets; identical log axes across panels for honest comparison%s",
                                  comma(nrow(d)), few_points_note(nrow(d))), width = 38),
      x = "Orbital period (days)",
      y = "Planet radius (Earth radii)",
      caption = SOURCE_CAPTION
    ) +
    theme_exo() +
    # Strips sit side by side in a narrow column: 8pt text plus real horizontal
    # and vertical gaps keep each label inside its own strip. The vertical gap
    # is important because Plotly otherwise places the Imaging/Other strips
    # directly against the Transit/Radial Velocity panels below them.
    theme(legend.position = "none",
          strip.text = element_text(size = 8, lineheight = 1.05,
                                    margin = margin(3, 2, 3, 2)),
          panel.spacing.x = unit(6, "pt"),
          # Vertical gap between the two facet rows must exceed the strip
          # label box (two lines: "Method"/"(n=...)" ~ 37px after ggplotly),
          # or the Imaging/Other headers overlap the Transit/RV panels above.
          panel.spacing.y = unit(26, "pt"))
}

# Orbital-period distribution by method (boxplot, log y), n shown per box.
plot_period_by_method <- function(df) {
  d <- df[!is.na(df$pl_orbper) & df$pl_orbper > 0, ]
  ggplot(d, aes(discovery_group, pl_orbper, fill = discovery_group)) +
    geom_boxplot(alpha = 0.85, outlier.alpha = 0.12) +
    # Two lines only ("Method" / "(n=...)"), NOT the shared group_n_labels(),
    # which also breaks "Radial Velocity" across lines — a third line stacks too
    # deep once the labels are angled. Angling is what actually buys the room:
    # rotated labels need only their height, not their width, so five names fit
    # side by side however narrow the card gets.
    scale_x_discrete(labels = function(lv) {
      tb <- table(d$discovery_group)
      paste0(lv, "\n(n=", format(as.integer(tb[lv]), big.mark = ",", trim = TRUE), ")")
    }) +
    scale_y_log10(labels = comma) +
    scale_fill_method() +
    labs(
      subtitle = wrap_sub(sprintf("%s planets with a valid orbital period; log scale%s",
                                  comma(nrow(d)), few_points_note(nrow(d))), width = 38),
      x = NULL, y = "Orbital period (days)", caption = SOURCE_CAPTION
    ) +
    theme_exo() +
    theme(legend.position = "none",
          # 45 deg, not 30: at 30 the five labels still collided once the card
          # fell to ~410px (measured, 4 overlapping pairs). Steepening trades
          # horizontal footprint for vertical, which is what a narrow card has.
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
}

# Earth-context scatter: equilibrium temperature vs radius, with Earth lines.
plot_earth_context <- function(df) {
  d <- df[!is.na(df$pl_rade) & !is.na(df$pl_eqt) &
          df$pl_rade > 0 & df$pl_eqt > 0, ]
  p <- ggplot(d, aes(pl_eqt, pl_rade, colour = discovery_group,
                text = paste0(pl_name, " (", hostname, ")",
                  "<br>Radius: ", round(pl_rade, 2), " R Earth",
                  "<br>Est. equilibrium temp: ", round(pl_eqt), " K",
                  "<br>Distance: ", round(sy_dist, 1), " pc"))) +
    geom_point(alpha = 0.55, size = 1.8)
  seed <- seed_missing_methods(d, "pl_eqt", "pl_rade")
  if (!is.null(seed)) {
    p <- p + geom_point(data = seed, aes(sx, sy, colour = discovery_group),
                        alpha = 0.55, size = 1e-3, inherit.aes = FALSE, show.legend = TRUE)
  }
  p +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_vline(xintercept = 255, linetype = "dashed", colour = "grey40") +
    scale_colour_method() +
    labs(
      subtitle = wrap_sub(sprintf(
        "%s planets match the current filters. Dashed lines mark Earth (radius 1 R Earth, ~255 K equilibrium temperature) — context only, not a habitability rule.%s",
        comma(nrow(d)), few_points_note(nrow(d))), width = 76),
      x = "Estimated equilibrium temperature (K)",
      y = "Planet radius (Earth radii)",
      caption = SOURCE_CAPTION
    ) +
    theme_exo()
}
