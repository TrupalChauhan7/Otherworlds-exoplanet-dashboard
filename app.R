# ============================================================================
# Beyond Our Solar System
# How discovery methods shape our view of confirmed exoplanets
#
# DSA8045 Applied Analytics - Assignment 2 (interactive R Shiny dashboard)
# Data: NASA Exoplanet Archive, Planetary Systems Composite Parameters
#       (PSCompPars), downloaded 21 July 2026. DOI 10.26133/NEA13
#
# UI shell: navbarPage (top navigation) + a unified, collapsible left filter
# rail that pushes content (never overlays it), so a control and the chart it
# drives stay co-visible.
#
# Charts: all ten are plotly figures produced by the shared as_interactive()
# helper in R/02_plot_helpers.R, which gives every chart the same curated
# toolbar with seven navigation tools, Inter text, hover styling, and the
# re-attached subtitle and source caption.
#
# Run:  open this folder in RStudio and click "Run App", or
#       shiny::runApp()  from this directory.
# ============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(plotly)
  library(DT)
})

# --- Chart font: render ggplot text in Inter, matching the UI -----------------
# Without this, every ggplot renders in the graphics device's generic sans
# (DejaVu/Arial) while the interface is in Inter, so charts visibly do not match
# the app around them. Data-visualisation practice is for chart text to use the
# same sans body face as the UI.
#
# The TTFs are BUNDLED in www/fonts/ (Inter v4.1, SIL OFL) and registered
# locally, deliberately NOT fetched from Google Fonts at runtime: chart text must
# render identically offline, on shinyapps.io, and in the static PDF export.
# The UI uses the same local files through CSS @font-face declarations; charts
# use these files through sysfonts and showtext.
#
# ggplot2 >= 4.0 resolves geom_text/geom_label `family` via from_theme(), so
# setting base_family on theme_exo() also covers in-chart value labels and
# callouts - no thematic/geom-default patching is required. See R/02_plot_helpers.R.
INTER_DIR <- file.path("www", "fonts")
INTER_OK <- file.exists(file.path(INTER_DIR, "Inter-Regular.ttf"))
if (INTER_OK) {
  if (!"Inter" %in% sysfonts::font_families()) {
    sysfonts::font_add(
      "Inter",
      regular    = file.path(INTER_DIR, "Inter-Regular.ttf"),
      bold       = file.path(INTER_DIR, "Inter-Bold.ttf"),
      italic     = file.path(INTER_DIR, "Inter-Italic.ttf"),
      bolditalic = file.path(INTER_DIR, "Inter-Italic.ttf")
    )
  }
  showtext::showtext_auto()
  # Match the rendering DPI to Shiny's renderPlot device so text keeps its
  # intended point size (verified: label widths identical to the pre-Inter build).
  showtext::showtext_opts(dpi = 96)
} else {
  warning("www/fonts/Inter-*.ttf not found - charts will fall back to the device sans font.")
}
# Charts fall back to the device sans if the bundled files are ever missing.
CHART_FONT <- if (INTER_OK) "Inter" else ""
# Browser-rendered Plotly text is configured in R/02_plot_helpers.R so every
# chart uses one shared font stack.

# --- Load helpers & (re)build data -------------------------------------------
# 01 is idempotent: it rebuilds the processed file only when that file is
# missing or older than the raw export, so this is silent on a normal launch
# (and no longer rebuilds every time Shiny auto-loads the R/ folder).
source(file.path("R", "01_prepare_data.R"))
source(file.path("R", "02_plot_helpers.R"))
source(file.path("R", "03_text_helpers.R"))

# bindCache store: an explicit in-memory cache. It is created fresh every time
# this file is (re-)sourced — i.e. on every app start/reload — so a restart also
# clears all cached renders. Combined with CACHE_VERSION in each cache key, a
# chart-code change can never surface a stale cached image.
shinyOptions(cache = cachem::cache_mem())

processed_path <- file.path("data", "processed", "exoplanets_dashboard.csv")

METHOD_LEVELS <- c("Transit", "Radial Velocity", "Microlensing", "Imaging", "Other")
SIZE_LEVELS <- c("Earth-size (<1.25 R Earth)", "Super-Earth (1.25-<2 R Earth)",
                 "Sub-Neptune (2-<4 R Earth)", "Neptune-size (4-<6 R Earth)",
                 "Giant (>=6 R Earth)")

exo <- read_csv(processed_path, show_col_types = FALSE) |>
  mutate(
    discovery_group   = factor(discovery_group, levels = METHOD_LEVELS),
    planet_size_group = factor(planet_size_group, levels = SIZE_LEVELS),
    disc_facility     = as.factor(disc_facility)
  )

# --- Startup constants -------------------------------------------------------
YEAR_MIN <- min(exo$disc_year, na.rm = TRUE)
YEAR_MAX <- max(exo$disc_year, na.rm = TRUE)
FACILITY_CHOICES <- names(sort(table(exo$disc_facility), decreasing = TRUE))
TOTAL_PLANETS <- nrow(exo)   # denominator for the Earth-context selectivity bar

RAD_MAX <- ceiling(max(exo$pl_rade, na.rm = TRUE))
EQT_MAX <- ceiling(max(exo$pl_eqt, na.rm = TRUE))
DIST_MAX <- ceiling(max(exo$sy_dist, na.rm = TRUE))

# The visible defaults are named once so the UI, reset button, and guided
# behaviour cannot drift apart during later maintenance.
DEFAULT_RADIUS_RANGE <- c(0, 4)
DEFAULT_EQT_RANGE    <- c(0, 1000)
DEFAULT_MAX_DIST     <- DIST_MAX

# --- Reactive proportion bar (replaces the stat-card rows on all three tabs) --
# One visual grammar, three questions: what is this selection MADE OF (Discovery),
# how are systems SPLIT (Host stars), how SELECTIVE are the filters (Earth
# context). A composition is a shape before it is a number, so a bar answers it
# faster than three isolated figures — and unlike KPI cards it moves, which makes
# the effect of a filter legible rather than something to be remembered.
#
# Plain HTML/CSS, no plotly: chrome-level furniture, so it costs no render/cache
# cycle and animates natively. `counts` is a NAMED vector in display order and
# `colours` supplies a hex per name, so the caller owns the palette — method hues
# on Discovery, the indigo sequential family elsewhere, never mixed.
# Picks the in-bar label ink by MEASURING both candidates against the segment
# and taking the higher WCAG contrast ratio — not by a luminance threshold.
# A threshold looked fine until it was measured: it put white on the Radial
# Velocity orange (#E69F00) at 2.25:1, well under the 4.5:1 floor, because that
# hue sits just on the wrong side of any cutoff you pick. Measuring instead of
# guessing keeps every segment legible across both palettes this bar uses.
relative_luminance <- function(hex) {
  v   <- grDevices::col2rgb(hex)[, 1] / 255
  lin <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055) ^ 2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin)
}
contrast_ratio <- function(a, b) {
  la <- relative_luminance(a); lb <- relative_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}
label_ink <- function(hex) {
  if (contrast_ratio("#141A24", hex) >= contrast_ratio("#FFFFFF", hex)) "#141A24" else "#FFFFFF"
}

share_bar <- function(bar_id, counts, colours, min_label_share = 6) {
  total <- sum(counts)
  share <- 100 * as.numeric(counts) / total
  pct   <- round(share)
  nms   <- names(counts)
  lab   <- sprintf("%s \u2014 %s (%d%%)", nms,
                   format(as.integer(counts), big.mark = ",", trim = TRUE), pct)

  segs <- lapply(seq_along(nms), function(i) {
    div(class = "msb-seg",
        `data-method` = nms[i],
        `data-w` = sprintf("%.4f%%", share[i]),
        style = sprintf("width:0%%;background:%s;", unname(colours[nms[i]])),
        title = lab[i], role = "img", `aria-label` = lab[i],
        # Only label a segment wide enough to hold the text.
        if (share[i] >= min_label_share) {
          ink <- label_ink(unname(colours[nms[i]]))
          span(class = if (ink == "#FFFFFF") "msb-pct" else "msb-pct msb-pct-dark",
               style = sprintf("color:%s;", ink), paste0(pct[i], "%"))
        } else NULL)
  })
  keys <- lapply(seq_along(nms), function(i) {
    span(class = "msb-key",
         span(class = "msb-dot", style = sprintf("background:%s;", unname(colours[nms[i]]))),
         span(class = "msb-name", nms[i]),
         span(class = "msb-share", paste0(pct[i], "%")))
  })
  tagList(
    # The bar is aria-hidden; THIS is what a screen reader reads, so the mix is
    # available as a sentence rather than as five unlabelled divs.
    span(class = "sr-only",
         paste0("Proportions in the current selection: ",
                paste(sprintf("%s %d%%", nms, pct), collapse = ", "), ".")),
    div(class = "msb-bar", `data-bar` = bar_id, `aria-hidden` = "true", segs),
    div(class = "msb-legend", keys)
  )
}


# --- Content card (replaces shinydashboard box(); same visual language) -------
# div.exo-card with a titled header and a padded body. Styled in custom.css.
card <- function(title = NULL, ...) {
  div(class = "exo-card",
      if (!is.null(title)) tags$h3(class = "exo-card-title", title),
      div(class = "exo-card-body", ...))
}

# --- Progressive-disclosure "what's this?" expander (plain HTML, no libraries) -
# Keeps scientific jargon off the screen until a novice asks for it.
whatis <- function(summary_text, explanation) {
  tags$details(class = "whatis",
    tags$summary(summary_text),
    div(class = "whatis-body", explanation))
}

# --- Brand wordmark: inline orbit glyph + "OTHERWORLDS" ----------------------
# Simple SVG orbit (ellipse) with a planet dot and a central star; strokes/fills
# are set to --hero-text in CSS (.brand-orbit) to keep token discipline.
brand_wordmark <- span(class = "brand-wordmark",
  tags$svg(class = "brand-orbit", viewBox = "0 0 24 24", width = "18", height = "18",
           `aria-hidden` = "true", focusable = "false",
    # horizontal orbit + central star + a planet dot offset on the rim, so it
    # reads unmistakably as an orbit (not a "prohibited"/null symbol)
    tags$ellipse(cx = "12", cy = "12", rx = "9", ry = "4.6", fill = "none",
                 `stroke-width` = "1.5"),
    tags$circle(cx = "12", cy = "12", r = "1.3"),
    tags$circle(cx = "19", cy = "9", r = "2.2")),
  tags$span(class = "brand-text", "OTHERWORLDS")
)

# --- Unified collapsible left filter rail ------------------------------------
# WHY A DOCKED LEFT RAIL RATHER THAN AN OVERLAY DRAWER:
# The filters previously opened as a right-hand overlay that sat ON TOP of the
# very chart the user was adjusting. That breaks direct manipulation
# (Shneiderman): a control and the result it affects must be CO-VISIBLE, so the
# effect of a change is seen immediately rather than after dismissing a panel.
# A persistent left sidebar is the established pattern for data-heavy apps that
# are filtered frequently. Our three global filters (year, method, facility)
# cascade into ALL FIVE analytical tabs — they feed filtered_data(), and
# earth_data() is built on top of filtered_data() — so a single global rail is
# scope-correct rather than per-tab. It stays collapsible so the user can
# reclaim the full width when reading a chart.
#
# ARCHITECTURE: one shared rail instance lives in navbarPage(header = ...), so
# every input stays mounted across tab switches. Collapsing is purely visual
# (a CSS transform on the rail + a left margin on the content) — the inputs are
# never unmounted, which is what lets Home's guided-entry buttons preset
# year/method from another tab and preserves era/reset state across
# collapse, expand and navigation.
ANALYTICAL_TABS <- c("discovery", "diversity", "hoststars", "bias", "earth")
ANALYTICAL_TABS_JS <- "['discovery','diversity','hoststars','bias','earth'].indexOf(input.nav) > -1"

# Navbar "Filters" button — now a collapse/expand toggle for the rail.
filter_toggle <- conditionalPanel(
  condition = ANALYTICAL_TABS_JS,
  class = "filter-toggle-wrap",
  actionButton("toggle_filters", "Filters", icon = icon("filter"),
               class = "btn-filter-toggle",
               `aria-expanded` = "true",
               `aria-controls` = "filter-rail")
)

filter_rail <- conditionalPanel(
  # Present only on the analytical tabs; Home and Conclusions stay full-width.
  condition = ANALYTICAL_TABS_JS,
  class = "filter-rail-wrap",
  # No floating reopen affordance: the navbar "Filters" button is the SINGLE
  # toggle (it stays visible while collapsed), so nothing can overlap the page
  # heading and there is no duplicate control to reason about.
  div(class = "filter-rail", id = "filter-rail",
      role = "region", `aria-label` = "Global filters",
      div(class = "rail-header",
          span(class = "rail-title", "Filters"),
          actionButton("close_filters", label = NULL, icon = icon("angles-left"),
                       class = "rail-collapse", `aria-label` = "Collapse filters")),
      div(class = "rail-body",
        sliderInput("year_range", "Discovery year",
                    min = YEAR_MIN, max = YEAR_MAX,
                    value = c(YEAR_MIN, YEAR_MAX), sep = "", width = "100%"),
        checkboxGroupInput("methods", "Discovery method",
                           choices = METHOD_LEVELS, selected = METHOD_LEVELS),
        selectizeInput("facility", "Facility (search; blank = all)",
                       choices = FACILITY_CHOICES, selected = NULL,
                       multiple = TRUE, width = "100%",
                       options = list(
                         placeholder = "All facilities",
                         plugins = list("remove_button")
                       )),
        tags$hr(class = "rail-divider"),
        tags$small(class = "rail-label", "Quick eras"),
        div(class = "era-group",
            actionButton("era_early",  "Early",  class = "btn-xs"),
            actionButton("era_kepler", "Kepler", class = "btn-xs"),
            actionButton("era_tess",   "TESS+",  class = "btn-xs"),
            actionButton("era_all",    "All",    class = "btn-xs")),
        actionButton("reset", "Reset filters", icon = icon("undo"),
                     class = "btn-sm btn-warning rail-reset"),

        # Tab-scoped filters: the Earth-context sliders live in the same rail so
        # there is never a second filter panel. IDs are unchanged, so
        # earth_data() is untouched.
        conditionalPanel(
          condition = "input.nav == 'earth'",
          tags$hr(class = "rail-divider"),
          tags$h3(class = "rail-subhead", "Earth-context filters"),
          sliderInput("radius_range", "Planet radius (Earth radii)",
                      min = 0, max = RAD_MAX, value = DEFAULT_RADIUS_RANGE, width = "100%"),
          sliderInput("eqt_range", "Estimated equilibrium temperature (K)",
                      min = 0, max = EQT_MAX, value = DEFAULT_EQT_RANGE, width = "100%"),
          sliderInput("max_dist", "Maximum system distance (parsecs)",
                      min = 0, max = DIST_MAX, value = DEFAULT_MAX_DIST, width = "100%"),
          whatis("What is equilibrium temperature?",
                 paste("Estimated equilibrium temperature is a model estimate of how warm a",
                       "planet would be from its star's light alone, assuming no atmosphere.",
                       "It is not a measured surface temperature and says nothing about",
                       "habitability.")),
          whatis("What is a parsec?",
                 "A parsec is a distance unit used in astronomy — about 3.26 light-years."),
          # This status is visually hidden but gives assistive technology and
          # keyboard users a plain-English description of the active selection.
          div(id = "filter-status", class = "sr-only", role = "status",
              `aria-live` = "polite", textOutput("filter_status", inline = TRUE)),
          div(id = "filter-help", class = "sr-only",
              span(id = "filter-help-year",
                   "Discovery year limits the catalogue by the year each planet was discovered."),
              span(id = "filter-help-methods",
                   "Discovery method keeps planets found by the selected detection techniques."),
              span(id = "filter-help-facility",
                   "Facility keeps planets recorded by the selected observatories or missions. Use the x beside a selected facility to remove only that facility."),
              span(id = "filter-help-radius",
                   "Planet radius filters size relative to Earth."),
              span(id = "filter-help-eqt",
                   "Estimated equilibrium temperature is a model estimate, not a measured surface temperature."),
              span(id = "filter-help-distance",
                   "Maximum system distance filters the host-star distance in parsecs."))
        )
      )
  )
)

# Keeps every plotly chart fitted to its container in every state.
#
# ROOT CAUSE (measured, not assumed): plotly.js implements `responsive: true` by
# calling Plots.resize(), and Plots.resize WRITES an explicit layout.width and
# sets autosize:false. From then on the chart is pinned at whatever width it was
# measured at, and Plots.resize() is a permanent no-op because it only acts when
# autosize is true. The Discovery charts get measured during the brief moment
# after the pane is shown but before the filter rail's body class arrives from
# the server, so they were pinned at the rail-CLOSED width (1216px at a 1920px
# viewport) and stayed 200px wider than their card forever after.
#
# Calling Plots.resize() again cannot fix this - verified. The cure is to RESTORE
# autosize and clear the pinned width, which makes plotly re-fit to the
# container and keeps it fitting afterwards.
#
# The guard compares the drawn width with the container and relayouts only on a
# real mismatch, so repeat passes are free: no redraw churn and no flicker while
# dragging a filter. Deliberately no custom ResizeObserver calling Plots.resize -
# that is what pinned the width in the first place.
plotly_resize_script <- tags$script(HTML(
  "(function(){
     function start(){
       if(!window.jQuery || !document.body) return;
       function fitOne(gd){
         if(!gd || gd.offsetParent === null) return;     // hidden tab: width 0
         if(!gd._fullLayout) return;                     // not drawn yet
         var want = Math.round(gd.clientWidth);
         var have = Math.round(gd._fullLayout.width || -1);
         if(want > 0 && Math.abs(want - have) > 1){
           try { Plotly.relayout(gd, {autosize: true, width: null, height: null}); }
           catch(e) {}
         }
       }
       function fitAll(){
         if(!window.Plotly) return;
         document.querySelectorAll('.js-plotly-plot').forEach(fitOne);
       }
       // The container settles asynchronously (a 150ms margin transition plus a
       // server round-trip for the rail class), so re-check over a short window
       // rather than guessing one delay; the mismatch guard makes extras free.
       var timers = [];
       function schedule(){
         timers.forEach(window.clearTimeout); timers = [];
         window.requestAnimationFrame(fitAll);
         [120, 320, 700].forEach(function(d){ timers.push(window.setTimeout(fitAll, d)); });
       }
       $(document).on('shiny:idle', schedule);            // after outputs render
       $(document).on('shown.bs.tab', schedule);          // navbarPage tab shown (BS3)
       $(document).on('click', '#toggle_filters, #close_filters', schedule);
       $(window).on('resize', schedule);
       // This script is placed in the navbar header, which can be evaluated
       // before the document body exists. Register the observer only after the DOM is ready.
       if(window.MutationObserver){
         new MutationObserver(schedule).observe(document.body,
           { attributes: true, attributeFilter: ['class'] });
       }
       schedule();
     }
     if(document.readyState === 'loading'){
       document.addEventListener('DOMContentLoaded', start);
     } else {
       start();
     }
   })();"
))

# Layout plumbing. The server is the single source of truth for two body classes:
#   .has-filter-rail  -> this tab has the rail (content is pushed right)
#   .rail-collapsed   -> rail slid off-canvas (content returns to full width)
# Also keeps aria-expanded truthful on both toggles, collapses on Esc, and
# defaults to COLLAPSED on narrow viewports (where pushing would crush charts).
rail_script <- tags$script(HTML(
  "Shiny.addCustomMessageHandler('railState', function(msg){
     var b = document.body;
     b.classList.toggle('has-filter-rail', !!msg.rail);
     b.classList.toggle('rail-collapsed', !!msg.collapsed);
     // The navbar button is the single toggle; keep its pressed state truthful
     // however the rail closes (button, the rail's chevron, or a tab switch).
     var expanded = (!!msg.rail && !msg.collapsed) ? 'true' : 'false';
     var el = document.getElementById('toggle_filters');
     if (el) el.setAttribute('aria-expanded', expanded);
   });
   $(document).on('shiny:connected', function(){
     if (window.innerWidth < 900) {
       Shiny.setInputValue('rail_narrow_init', 1, {priority: 'event'});
     }
   });
   document.addEventListener('keydown', function(e){
     if (e.key !== 'Escape') return;
     var b = document.body;
     if (b.classList.contains('has-filter-rail') && !b.classList.contains('rail-collapsed')) {
       var el = document.getElementById('close_filters');
       if (el) el.click();
     }
   });
   // Mobile: the Bootstrap navbar menu does not auto-collapse when a tab
   // link is tapped, so it would sit over the page content. Close it on
   // tab selection, and also when the Filters rail opens, so the two
   // overlays never stack on a phone.
   $(document).on('click', '.navbar-collapse .navbar-nav a, #toggle_filters', function(){
     var c = document.querySelector('.navbar-collapse');
     if (c && c.classList.contains('in')) { $(c).collapse('hide'); }
   });"
))

# Add non-invasive hover guidance and programmatic descriptions to the existing
# controls. The visible layout, labels, screenshots, and default state remain
# unchanged, while a first-time or assistive-technology user gets extra context.
filter_hint_script <- tags$script(HTML(
  "(function(){
     function start(){
       if(!window.jQuery || !document.body) return;
       var hints = {
         year_range: ['Discovery year limits the catalogue by the year each planet was discovered.', 'filter-help-year'],
         methods: ['Discovery method keeps planets found by the selected detection techniques.', 'filter-help-methods'],
         facility: ['Facility keeps planets recorded by the selected observatories or missions. Use the x beside a selected facility to remove only that facility.', 'filter-help-facility'],
         radius_range: ['Planet radius filters size relative to Earth.', 'filter-help-radius'],
         eqt_range: ['Estimated equilibrium temperature is a model estimate, not a measured surface temperature.', 'filter-help-eqt'],
         max_dist: ['Maximum system distance filters the host-star distance in parsecs.', 'filter-help-distance']
       };
       var queued = false;
       function schedule(){
         if (queued) return;
         queued = true;
         window.requestAnimationFrame(function(){
           queued = false;
           Object.keys(hints).forEach(function(id){
             var el = document.getElementById(id);
             if (!el) return;
             var root = el.closest('.form-group') || el;
             root.setAttribute('title', hints[id][0]);
             root.setAttribute('aria-describedby', hints[id][1]);
           });
         });
       }
       $(document).on('shiny:connected', schedule);
       // Observe only the filter rail. Watching the whole body caused every
       // Plotly redraw to schedule another pass through all filter controls.
       if (window.MutationObserver) {
         var rail = document.getElementById('filter-rail');
         if (rail) {
           new MutationObserver(schedule).observe(rail, {childList: true, subtree: true});
         }
       }
       schedule();
     }
     if(document.readyState === 'loading'){
       document.addEventListener('DOMContentLoaded', start);
     } else {
       start();
     }
   })();"
))

# renderUI REPLACES its subtree, so freshly mounted segments have no previous
# width to transition from — a CSS transition alone would never fire and the bar
# would snap. This observer remembers the last width per method, paints new
# segments at that old width, forces a reflow, then sets the real width on the
# next frame, so the browser genuinely animates from the previous mix to the new
# one. Widths travel in data-w; the inline style starts at 0 for a first-load
# grow-in. Under prefers-reduced-motion the CSS transition is off, so this just
# assigns the final width and nothing moves.
share_bar_script <- tags$script(HTML(
  "(function(){
     var last = {};
     function paint(){
       var bars = document.querySelectorAll('.msb-bar');
       if (!bars.length) return;
       bars.forEach(function(bar){
         // Key the cache per BAR as well as per segment: three bars now share
         // this code and two of them could use the same segment name.
         var id = bar.getAttribute('data-bar') || '';
         var segs = bar.querySelectorAll('.msb-seg');
         segs.forEach(function(el){
           var k = id + '|' + el.getAttribute('data-method');
           if (last[k] !== undefined) el.style.width = last[k];
         });
         void bar.offsetWidth;                                   // reflow
         requestAnimationFrame(function(){
           segs.forEach(function(el){
             var k = id + '|' + el.getAttribute('data-method');
             var w = el.getAttribute('data-w');
             el.style.width = w;
             last[k] = w;
           });
         });
       });
     }
     function arm(){
       // The three bars live on different tabs and are mounted/replaced
       // independently as Shiny re-renders each uiOutput. Observe only their
       // output containers, not the complete tab-content subtree, because every
       // Plotly DOM update would otherwise invoke paint().
       var roots = document.querySelectorAll(
         '#method_share_bar, #hs_share_bar, #earth_share_bar'
       );
       if (!roots.length) {
         window.setTimeout(arm, 100);
         return;
       }
       roots.forEach(function(root){
         new MutationObserver(paint).observe(root, {childList: true, subtree: true});
       });
       paint();
       return true;
     }
     $(document).on('shiny:connected', arm);
   })();"
))

# ============================================================================
# UI
# ============================================================================
ui <- navbarPage(
  title = brand_wordmark,
  windowTitle = "OTHERWORLDS",
  id = "nav",
  collapsible = TRUE,
  position = "fixed-top",
  header = tagList(
    tags$head(includeCSS(file.path("www", "custom.css")), plotly_resize_script,
              share_bar_script, filter_hint_script),
    filter_toggle,
    filter_rail,
    rail_script
  ),

  # ---- Home / cosmic hero + guided entry (no ribbon) ----------------------
  tabPanel(
    "Home", value = "home",
    div(class = "cosmic-hero",
        div(class = "hero-eyebrow", "Beyond Our Solar System"),
        h1(class = "hero-title",
           "We've confirmed thousands of worlds — but what we see is shaped by how we look."),
        p(class = "hero-sub",
          "An interactive tour of every confirmed planet beyond our Solar System, built from ",
          "NASA's official Exoplanet Archive. It explores not just what we have found, but how ",
          "our detection methods shape the catalogue we are able to see."),
        div(class = "hero-statline", uiOutput("hero_stat", inline = TRUE)),
        div(class = "hero-actions",
            actionButton("go_discovery", "How discovery exploded", class = "guided-btn"),
            actionButton("go_bias",      "Compare detection methods", class = "guided-btn"),
            actionButton("go_earth",     "Explore Earth-context worlds", class = "guided-btn")
        ),
        div(class = "hero-note",
            "Source: NASA Exoplanet Archive, Planetary Systems Composite Parameters (PSCompPars), ",
            "downloaded 21 July 2026. This dashboard makes no claim about habitability or life.")
    )
  ),

  # ---- Discovery story (full-width) ---------------------------------------
  tabPanel(
    "Discovery story", value = "discovery",
    div(class = "tab-headline",
        "Confirmed discoveries exploded after 2009, when space telescopes began ",
        "surveying thousands of stars at once."),
    div(class = "tab-notice",
        "What to notice: the timeline climbs from a handful of planets a year to hundreds — ",
        "and the colours show which method drove each surge."),
    # The prime slot: one reactive finding rather than four KPI cards. The old
    # cards were low-entropy — "Leading method"/"Leading facility" read Transit
    # and Kepler in almost every filter state, and the two counts sat isolated so
    # planets-per-star had to be worked out by the reader. This band states the
    # relationship AND leads with the tab's thesis. It is the same reactive text
    # that used to sit in the "What changed?" box, promoted rather than rebuilt.
    # Prime slot: the method mix as a shape you can see, plus one thesis line.
    # The bar is plain HTML/CSS (no plotly) — it is chrome-level furniture, not a
    # tenth chart, so it costs no render/cache cycle and animates natively.
    div(class = "headline-finding",
        uiOutput("method_share_bar"),
        div(class = "headline-caption", textOutput("insight_text"))),
    fluidRow(
      column(8, card("How has the confirmed catalogue grown over time?",
        radioButtons("timeline_mode", NULL, inline = TRUE,
                     choices = c("Annual" = "annual", "Cumulative" = "cumulative")),
        plotlyOutput("p_timeline", height = 360),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "each bar is one year; its height is how many planets were confirmed then, ",
            "split by how they were found."))),
      column(4, card("How planets are found",
        tags$ul(
          tags$li(tags$b("Transit:"), " a small, regular dip in a star's brightness as a planet passes in front of it."),
          tags$li(tags$b("Radial velocity:"), " a tiny wobble in the star caused by an orbiting planet's gravity."),
          tags$li(tags$b("Microlensing / imaging:"), " rarer techniques for special cases.")
        ),
        tags$small(tags$em("Each method is sensitive to a different kind of planet."))))
    ),
    # Full width now that "What changed?" has moved to the band above: the
    # horizontal facility bars read better wide, and the caveat that used to sit
    # beside them belongs with the finding, not with one chart.
    fluidRow(
      column(12, card("Which facilities have confirmed the most planets?",
        radioButtons("facility_metric", NULL, inline = TRUE,
                     choices = c("Count" = "count", "Share" = "share")),
        plotlyOutput("p_facilities", height = 340),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "which observatories and missions confirmed the most planets in your current selection.")))
    )
  ),

  # ---- Planet diversity (full-width; each control sits above its chart) ----
  tabPanel(
    "Planet diversity", value = "diversity",
    div(class = "tab-headline",
        "Confirmed worlds range from smaller than Earth to larger than Jupiter — ",
        "and the detection methods cluster in different regions."),
    div(class = "tab-notice",
        "What to notice: most points pile up at short orbital periods (left) — ",
        "that is where today's surveys are most sensitive, not where planets truly concentrate."),
    # No sidebar: the signature scatter is full-width with its control inline
    # above it (Row 1); the two supporting charts sit balanced below (Row 2),
    # each with the control that drives it directly above the chart.
    fluidRow(
      column(12, card("How do planet size and orbital period relate?",
        div(class = "inline-control",
            checkboxInput("show_earth", "Show Earth reference lines", value = TRUE)),
        plotlyOutput("p_radius_period", height = 460),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "every dot is a planet; left-right is how long its year is, up-down is its size, ",
            "and colour is how it was found. Hover any dot to inspect it."),
        whatis("What is a log scale?",
               paste("A log scale compresses very large ranges so each step multiplies",
                     "(1, 10, 100, 1,000, ...). Orbital periods here span from about 0.09",
                     "days to hundreds of millions of days, so a normal scale would squash",
                     "almost every planet into one corner.")),
        whatis("What are Earth radii?",
               paste("Planet size is measured in Earth radii: 1 means Earth-sized,",
                     "11 is roughly Jupiter-sized. It compares each planet's width to Earth's."))))
    ),
    fluidRow(
      column(6,
        card("Size mix in selection",
          plotlyOutput("p_sizemix", height = 340),
          tags$small(class = "help-text",
            "Size bands are a visual grouping based on reported radius, not a claim about composition.")),
        card("Typical values (selection)",
          tableOutput("t_medians"),
          tags$small(class = "help-text",
            "Each figure uses only the planets with that value reported (plot-specific complete cases)."))),
      column(6, card("How does each method's haul of planets differ?",
        div(class = "inline-control",
            radioButtons("dist_var", "Show distribution of:", inline = TRUE,
                         choices = c("Planet radius" = "pl_rade",
                                     "Mass or minimum mass" = "pl_bmasse"))),
        plotlyOutput("p_distribution", height = 340),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "the spread of sizes (or masses) each method tends to find — the box covers the middle half."),
        whatis("What is radial velocity?",
               paste("Radial velocity finds a planet from the tiny back-and-forth wobble",
                     "its gravity causes in the host star. It usually yields a minimum mass,",
                     "which is why mass is labelled 'mass or minimum mass' here.")),
        whatis("Why 'mass or minimum mass'?",
               paste("For many planets we only know a lower bound on the mass (the true",
                     "value depends on the orbit's tilt, which is often unknown), so the",
                     "figure shown may be a minimum, not the exact mass."))))
    )
  ),

  # ---- Host stars & systems (full-width) ----------------------------------
  tabPanel(
    "Host stars", value = "hoststars",
    div(class = "tab-headline",
        "The planets we've found orbit a wide range of stars — and usually one at a time."),
    div(class = "tab-notice",
        "What to notice: most host-star temperatures cluster near the Sun's, and most ",
        "confirmed systems have just a single known planet — the smaller and lighter ones are the easiest to miss."),
    # Same treatment as Discovery, tab-specific question: how are systems split?
    # This is the clean single/multi PROPORTION; the multiplicity chart below
    # shows the full 1-8 distribution — a different read, not a duplicate.
    div(class = "headline-finding",
        uiOutput("hs_share_bar"),
        div(class = "headline-caption", textOutput("hs_caption"))),
    fluidRow(
      column(7, card("What kinds of stars host these planets?",
        plotlyOutput("p_star_temp", height = 360),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "each bar counts planets whose host star falls in that temperature range; ",
            "the dashed line marks our Sun for comparison."),
        whatis("What is host-star temperature?",
               paste("The effective temperature of the star a planet orbits, in kelvin.",
                     "Hotter stars are bluer, cooler stars redder; our Sun is about 5,772 K.")))),
      column(5, card("How many planets do these systems have?",
        plotlyOutput("p_system_mult", height = 360),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "each system is counted once; the bars show how many have 1, 2, 3 or more known planets — ",
            "most have just one found so far.")))
    )
  ),

  # ---- Detection bias (full-width) ----------------------------------------
  tabPanel(
    "Detection bias", value = "bias",
    div(class = "tab-headline",
        "Each detection method reveals a different slice of the same universe."),
    div(class = "tab-notice",
        "What to notice: the empty regions in each panel are what that method cannot detect — ",
        "not places where planets are absent."),
    fluidRow(
      column(12, card("Interpretation guidance",
        checkboxInput("show_interpretation", "Show interpretation guidance", value = TRUE),
        conditionalPanel(
          condition = "input.show_interpretation == true",
          div(class = "warn-box", textOutput("bias_text")))))
    ),
    fluidRow(
      column(7, card("What can each method actually see?",
        plotlyOutput("p_bias_facets", height = 500),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "the same size-vs-orbit chart split by method, on identical axes, ",
            "so you can compare what each method reaches."))),
      column(5, card("How do orbital periods differ by method?",
        plotlyOutput("p_period_method", height = 500),
        div(class = "chart-takeaway",
            tags$b("In plain terms: "),
            "higher boxes mean longer years; each method favours a different range.")))
    )
  ),

  # ---- Earth context (full-width; its filters live in the global left rail) -
  tabPanel(
    "Earth context", value = "earth",
    div(class = "tab-headline",
        "A few confirmed worlds are near Earth's size — but temperature alone ",
        "says nothing about habitability."),
    div(class = "tab-notice",
        "What to notice: adjust size, temperature and distance to find matches, ",
        "then read the caveat below before drawing any conclusion."),
    div(class = "disclaimer",
        tags$b("Important: "),
        "Estimated equilibrium temperature is a simplified model, not a measured surface ",
        "temperature. These filters do not establish an atmosphere, liquid water, ",
        "habitability, or life."),
    # The radius / temperature / distance sliders now live in the global left
    # rail (under "Earth-context filters"), so this tab has no second filter
    # panel and its content runs full width beside the rail.
    # Selectivity, not composition: how much of the full catalogue survives the
    # current filters. It shrinks visibly as the sliders tighten, which is the
    # honest counterweight to a filter that can feel like "finding" planets.
    div(class = "headline-finding",
        uiOutput("earth_share_bar"),
        div(class = "headline-caption", textOutput("earth_caption_text"))),
    fluidRow(
      column(12,
        card("How do these worlds compare on size and estimated temperature?",
          plotlyOutput("p_earth", height = 460),
          div(class = "chart-takeaway",
              tags$b("In plain terms: "),
              "size versus estimated temperature; the dashed lines mark Earth for scale only, ",
              "not as a habitability threshold.")),
        card("Matching planets",
          div(class = "table-actions",
              downloadButton("dl_earth", "Download matching planets (CSV)",
                             class = "btn-sm btn-warning")),
          DTOutput("t_earth")))
    )
  ),

  # ---- Conclusions (full-width, no ribbon) --------------------------------
  tabPanel(
    "Conclusions", value = "conclusions",
    h2("Conclusions and recommendations"),
    fluidRow(
      column(6, card("What the data show",
        tags$ul(
          tags$li("Confirmed discoveries grew sharply from the late 2000s, driven by space-based transit surveys (Kepler, then TESS)."),
          tags$li("The catalogue is dominated by the Transit method (about three-quarters of planets) and by a handful of facilities."),
          tags$li("Observed planets span an enormous range of sizes and orbital periods, visible on the signature scatterplot."),
          tags$li("Different discovery methods occupy different regions of that size-period space, each clustering where it is most sensitive."),
          tags$li("Most confirmed systems have just a single known planet (about 78%), with roughly 22% having more than one found so far."),
          tags$li("Host stars span a wide temperature range, clustered around Sun-like and cooler stars.")
        ))),
      column(6, card("Limits and honest caveats",
        tags$ul(
          tags$li("This is a record of what has been DETECTED and confirmed, shaped by what each method and facility is able to find — it is not a census of the planets that exist."),
          tags$li("PSCompPars combines values from multiple studies, so a single row may not be internally self-consistent."),
          tags$li("Missing values differ by method; each chart uses only the planets with the values it needs."),
          tags$li("Equilibrium temperature does not establish habitability or life.")
        )))
    ),
    fluidRow(
      column(12, card("Recommendations and future improvements",
        tags$ul(
          tags$li("Compare the methods side by side on the Detection bias tab before generalising from any single chart."),
          tags$li("Future versions could add a searchable planet profile, a facility×method heatmap, and uncertainty-aware displays."),
          tags$li("Source: NASA Exoplanet Archive (2026) Planetary Systems Composite Parameters. Caltech/IPAC. doi:10.26133/NEA13, accessed 21 July 2026."),
          tags$li(tags$em("Generative-AI assistance was used during development and is declared per module policy; all analysis and interpretation were checked by the author."))
        )))
    )
  )
)

# ============================================================================
# SERVER  (unchanged reactives/outputs; only guided-entry tab switching uses
#          updateNavbarPage now that the shell is a navbarPage)
# ============================================================================
server <- function(input, output, session) {

  # --- Home hero: live headline stat (reactive to global filters) ----------
  output$hero_stat <- renderUI({
    d <- filtered_data()
    n <- nrow(d)
    span_txt <- if (n > 0) {
      yr <- range(d$disc_year, na.rm = TRUE)
      if (yr[1] == yr[2]) as.character(yr[1]) else paste0(yr[1], "–", yr[2])
    } else "—"
    tagList(
      tags$span(class = "hero-stat", format(n, big.mark = ",")),
      " confirmed planets, discovered ",
      tags$span(class = "hero-stat", span_txt)
    )
  })

  # --- Filter rail: expanded / collapsed state ------------------------------
  # Default EXPANDED on the analytical tabs, so a filter and the chart it drives
  # are co-visible from the first glance. Collapsing is visual only, and the
  # state deliberately PERSISTS across tab switches (unlike the old drawer, which
  # closed on every navigation) — the rail is furniture, not a transient popup.
  # ignoreInit stops the action buttons firing at startup.
  filters_open <- reactiveVal(TRUE)
  observeEvent(input$toggle_filters, { filters_open(!filters_open()) }, ignoreInit = TRUE)
  observeEvent(input$close_filters,  { filters_open(FALSE) },           ignoreInit = TRUE)
  # Narrow viewports open collapsed: there pushing content would crush the charts,
  # so the rail behaves as an overlay and should not cover the page on arrival.
  observeEvent(input$rail_narrow_init, { filters_open(FALSE) })

  # Single source of truth for the two body classes that drive the layout.
  observe({
    session$sendCustomMessage("railState", list(
      rail      = isTRUE(input$nav %in% ANALYTICAL_TABS),
      collapsed = !isTRUE(filters_open())
    ))
  })

  # --- Guided-entry buttons: switch tab + apply a sensible preset filter ----
  observeEvent(input$go_discovery, {
    updateSliderInput(session, "year_range", value = c(YEAR_MIN, YEAR_MAX))
    updateCheckboxGroupInput(session, "methods", selected = METHOD_LEVELS)
    updateNavbarPage(session, "nav", "discovery")
  })
  observeEvent(input$go_bias, {
    updateCheckboxGroupInput(session, "methods", selected = c("Transit", "Radial Velocity"))
    updateNavbarPage(session, "nav", "bias")
  })
  observeEvent(input$go_earth, {
    updateSliderInput(session, "radius_range", value = c(0, 2.5))  # spotlight small worlds (size only)
    updateNavbarPage(session, "nav", "earth")
  })

  # --- Era preset buttons update the year slider ---------------------------
  observeEvent(input$era_early,  updateSliderInput(session, "year_range", value = c(1992, 2008)))
  observeEvent(input$era_kepler, updateSliderInput(session, "year_range", value = c(2009, 2018)))
  observeEvent(input$era_tess,   updateSliderInput(session, "year_range", value = c(2019, YEAR_MAX)))
  observeEvent(input$era_all,    updateSliderInput(session, "year_range", value = c(YEAR_MIN, YEAR_MAX)))

  # --- Reset all visible filters -------------------------------------------
  # The Earth-context sliders are tab-scoped but remain mounted in the shared
  # rail. Resetting them here makes the button match what a reader sees when it
  # is clicked on the Earth-context tab.
  observeEvent(input$reset, {
    updateSliderInput(session, "year_range", value = c(YEAR_MIN, YEAR_MAX))
    updateCheckboxGroupInput(session, "methods", selected = METHOD_LEVELS)
    updateSelectizeInput(session, "facility", selected = character(0))
    updateSliderInput(session, "radius_range", value = DEFAULT_RADIUS_RANGE)
    updateSliderInput(session, "eqt_range", value = DEFAULT_EQT_RANGE)
    updateSliderInput(session, "max_dist", value = DEFAULT_MAX_DIST)
  })

  # --- Global filter state, debounced --------------------------------------
  # The three global inputs are bundled and the BUNDLE is debounced (450 ms), so
  # charts recompute once after the user stops dragging the year slider rather
  # than on every tick. Deriving both the data and every cache key from this one
  # debounced value keeps key and data in lockstep (a key taken straight from
  # input$ would advance ahead of the debounced data and cache a wrong pairing).
  global_filters_raw <- reactive({
    list(years    = input$year_range,
         methods  = input$methods,
         facility = input$facility)
  })
  global_filters <- debounce(global_filters_raw, 450)

  # --- Central reactive: filter once, reuse everywhere ---------------------
  filtered_data <- reactive({
    g <- global_filters()
    req(g$years)
    d <- exo
    d <- d[!is.na(d$disc_year) &
             d$disc_year >= g$years[1] &
             d$disc_year <= g$years[2], ]
    if (length(g$methods) > 0) {
      d <- d[d$discovery_group %in% g$methods, ]
    } else {
      d <- d[0, ]
    }
    if (length(g$facility) > 0) {
      d <- d[as.character(d$disc_facility) %in% g$facility, ]
    }
    d
  })

  # Plain-English status for keyboard and assistive-technology users. The
  # visible charts already communicate the selection, so this output is hidden
  # from the visual layout and does not alter the static screenshots.
  output$filter_status <- renderText({
    g <- global_filters()
    d <- filtered_data()
    methods <- g$methods
    facilities <- g$facility
    method_text <- if (is.null(methods) || length(methods) == 0) {
      "no detection methods"
    } else if (length(methods) == length(METHOD_LEVELS)) {
      "all detection methods"
    } else {
      paste(methods, collapse = ", ")
    }
    facility_text <- if (is.null(facilities) || length(facilities) == 0) {
      "all facilities"
    } else {
      paste(facilities, collapse = ", ")
    }
    earth_text <- if (identical(input$nav, "earth") &&
                      !is.null(input$radius_range) &&
                      !is.null(input$eqt_range) &&
                      !is.null(input$max_dist)) {
      sprintf(" Earth-context ranges are %s to %s Earth radii, %s to %s K, and up to %s parsecs.",
              format(input$radius_range[1], trim = TRUE),
              format(input$radius_range[2], trim = TRUE),
              format(input$eqt_range[1], big.mark = ",", trim = TRUE),
              format(input$eqt_range[2], big.mark = ",", trim = TRUE),
              format(input$max_dist, big.mark = ",", trim = TRUE))
    } else {
      ""
    }
    if (nrow(d) == 0) {
      return(paste0("The current filters return no confirmed planets.", earth_text))
    }
    sprintf("Current selection: %s confirmed planets, %s, recorded by %s.%s",
            format(nrow(d), big.mark = ","), method_text, facility_text, earth_text)
  })

  # Guard used by every output so empty selections show a message, not an error
  need_rows <- function(d) {
    validate(need(nrow(d) > 0,
                  "No planets match the current filters. Widen the year range, select more methods, or try another facility."))
  }

  # A global selection can contain rows but still have no usable values for a
  # particular chart. Return a clear Plotly message instead of passing an
  # empty complete-case frame into a chart builder.
  plot_empty_state <- function(d, required, description, positive = character()) {
    if (nrow(d) == 0) return(empty_plotly())
    required_df <- as.data.frame(d[, required, drop = FALSE])
    keep <- stats::complete.cases(required_df)
    for (nm in positive) {
      keep <- keep & is.finite(d[[nm]]) & d[[nm]] > 0
    }
    if (!any(keep)) {
      return(empty_plotly(paste0(
        "No ", description,
        " are available for the current filters. Widen the filters or choose another facility."
      )))
    }
    NULL
  }

  # Every chart is rendered through as_interactive() (R/02_plot_helpers.R), which
  # gives all ten the same seven-tool toolbar, Inter text, hover styling, and
  # the re-attached subtitle and source caption.
  # bindCache keys list EVERY reactive input each output reads (debounced global
  # filters + that chart's own toggles), so a cached frame can never be served
  # for a different state. Empty selections return a friendly blank figure
  # rather than validate()'s error, which renderPlotly cannot display.
  output$p_timeline <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, "disc_year", "discovery-year values")
    if (!is.null(empty)) return(empty)
    as_interactive(plot_timeline(d, mode = input$timeline_mode), filename = "discovery-timeline")
  }) |> bindCache(CACHE_VERSION, global_filters(), input$timeline_mode)

  output$p_facilities <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, "disc_facility", "facility values")
    if (!is.null(empty)) return(empty)
    as_interactive(plot_facilities(d, top_n = 10, metric = input$facility_metric), filename = "top-facilities")
  }) |> bindCache(CACHE_VERSION, global_filters(), input$facility_metric)
  # Feeds the caption under the share bar at the top of the Discovery Story tab.
  output$insight_text <- renderText({
    insight_sentence(filtered_data())
  })

  # Animated 100%-stacked share bar. Colours come straight from METHOD_COLOURS so
  # the band and all ten charts encode method identically. Every one of the five
  # levels is emitted even at 0% (factor levels, not observed values), so a
  # segment can shrink to nothing and grow back rather than popping in and out.
  output$method_share_bar <- renderUI({
    d <- filtered_data()
    if (nrow(d) == 0) {
      return(div(class = "msb-empty",
                 "No planets match the current filters \u2014 nothing to compare."))
    }
    lv <- levels(d$discovery_group)
    # Every level is emitted even at 0%, so a filtered-out method shrinks to
    # nothing and grows back rather than popping in and out of the DOM.
    cnt <- table(factor(d$discovery_group, levels = lv))
    share_bar("methods", stats::setNames(as.integer(cnt), lv), METHOD_COLOURS)
  })

  # --- TAB 2: planet diversity ---------------------------------------------
  # Continuous scatter: zoom/pan stay enabled (exploring is legitimate here) and
  # WebGL keeps ~6,000 points fast. Same helper, so the toolbar matches the rest.
  output$p_radius_period <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, c("pl_rade", "pl_orbper"),
                              "radius and orbital-period measurements",
                              positive = c("pl_rade", "pl_orbper"))
    if (!is.null(empty)) return(empty)
    as_interactive(plot_radius_period(d, show_earth = input$show_earth), webgl = TRUE,
                   filename = "radius-vs-orbital-period")
  }) |> bindCache(CACHE_VERSION, global_filters(), input$show_earth)
  output$p_sizemix <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, "planet_size_group", "planet-size group values")
    if (!is.null(empty)) return(empty)
    as_interactive(plot_size_mix(d), filename = "size-mix")
  }) |> bindCache(CACHE_VERSION, global_filters())

  output$p_distribution <- renderPlotly({
    d <- filtered_data()
    variable <- if (is.null(input$dist_var)) "pl_rade" else input$dist_var
    label <- if (identical(variable, "pl_rade")) {
      "valid radius measurements"
    } else {
      "valid mass or minimum-mass measurements"
    }
    empty <- plot_empty_state(d, variable, label, positive = variable)
    if (!is.null(empty)) return(empty)
    # Boxplots keep plotly's native quartile hover, so no `text` aesthetic.
    as_interactive(plot_distribution(d, variable = variable), tooltip = NULL,
                   filename = "distribution-by-method")
  }) |> bindCache(CACHE_VERSION, global_filters(), input$dist_var)
  output$t_medians <- renderTable({
    d <- filtered_data(); need_rows(d)
    data.frame(
      Measure = c("Median radius (R Earth)", "Median orbital period (days)",
                  "Median mass/min-mass (M Earth)", "Median distance (pc)"),
      Value = c(safe_median(d$pl_rade, 2), safe_median(d$pl_orbper, 1),
                safe_median(d$pl_bmasse, 1), safe_median(d$sy_dist, 1))
    )
  }, striped = TRUE, spacing = "s")

  # --- TAB: Host stars & systems -------------------------------------------
  # Single vs multi-planet SYSTEMS (deduped to one row per host star). Indigo
  # sequential, never a method hue — this bar encodes system multiplicity, so
  # borrowing the method palette would imply a meaning it does not have.
  output$hs_share_bar <- renderUI({
    d <- filtered_data()
    # TRUE system multiplicity (sy_pnum), not the planet count that survives the
    # filters - a Transit-only view must not turn a 4-planet system into a
    # "single". Same basis as plot_system_multiplicity(), so bar and chart agree.
    d <- d[!is.na(d$hostname) & !is.na(d$sy_pnum), ]
    d <- d[!duplicated(d$hostname), ]                 # one row per system
    if (nrow(d) == 0) {
      return(div(class = "msb-empty",
                 "No host stars match the current filters \u2014 nothing to compare."))
    }
    counts <- c("Single-planet systems" = sum(d$sy_pnum == 1),
                "Multi-planet systems"  = sum(d$sy_pnum >  1))
    share_bar("hostmult", counts,
              c("Single-planet systems" = BAR_PRIMARY,
                "Multi-planet systems"  = "#B7BFDD"))
  })
  output$hs_caption <- renderText({ hoststars_caption(filtered_data()) })

  output$p_star_temp <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, "st_teff", "host-star temperature measurements",
                              positive = "st_teff")
    if (!is.null(empty)) return(empty)
    # Histogram bins are stat-computed, so plotly's default bin hover is used.
    as_interactive(plot_star_temp(d), tooltip = NULL,
                   filename = "host-star-temperature")
  }) |> bindCache(CACHE_VERSION, global_filters())
  output$p_system_mult <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, "sy_pnum", "system planet-count values")
    if (!is.null(empty)) return(empty)
    as_interactive(plot_system_multiplicity(d), filename = "planets-per-system")
  }) |> bindCache(CACHE_VERSION, global_filters())

  # --- TAB 3: detection bias -----------------------------------------------
  output$bias_text <- renderText({ bias_interpretation(filtered_data()) })
  output$p_bias_facets <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, c("pl_rade", "pl_orbper"),
                              "radius and orbital-period measurements",
                              positive = c("pl_rade", "pl_orbper"))
    if (!is.null(empty)) return(empty)
    bias_plot <- as_interactive(
      plot_bias_facets(d),
      webgl = TRUE,
      tooltip = NULL,
      filename = "detection-bias-facets"
    )

    # This chart uses five small panels side by side. A Plotly hover label can
    # extend beyond the panel containing the point and cover a neighbouring
    # method, which makes the comparison difficult to read. Disable hover
    # labels for this faceted comparison only. WebGL keeps the full point cloud
    # responsive without changing the data shown. The toolbar still provides
    # zoom, pan, reset, autoscale, and download controls.
    bias_plot$x$data <- lapply(bias_plot$x$data, function(trace) {
      trace$hoverinfo <- "skip"
      trace$hovertemplate <- NULL
      trace$text <- NULL
      trace
    })
    bias_plot
  }) |> bindCache(CACHE_VERSION, global_filters())

  output$p_period_method <- renderPlotly({
    d <- filtered_data()
    empty <- plot_empty_state(d, "pl_orbper", "orbital-period measurements",
                              positive = "pl_orbper")
    if (!is.null(empty)) return(empty)
    as_interactive(plot_period_by_method(d), tooltip = NULL,
                   filename = "orbital-period-by-method")
  }) |> bindCache(CACHE_VERSION, global_filters())

  # --- TAB 4: Earth context ------------------------------------------------
  # How SELECTIVE the current filters are, against the full catalogue. Same
  # indigo family as the host-stars bar: both answer "how much of the whole is
  # this?", a different question from "which method", so a different palette.
  output$earth_share_bar <- renderUI({
    d <- earth_data()
    matched <- nrow(d)
    if (matched == 0) {
      return(div(class = "msb-empty",
                 "No planets match these filters \u2014 widen the radius, temperature or distance range."))
    }
    counts <- c("Matches your filters"  = matched,
                "Rest of the catalogue" = max(0L, TOTAL_PLANETS - matched))
    share_bar("earthsel", counts,
              c("Matches your filters"  = BAR_PRIMARY,
                "Rest of the catalogue" = "#DFE3F0"))
  })
  output$earth_caption_text <- renderText({ earth_caption(earth_data(), TOTAL_PLANETS) })

  earth_data <- reactive({
    # The Earth controls are mounted inside a tab-scoped conditional panel. On
    # a fast tab switch, the output can briefly be evaluated before all three
    # slider values have reached the server. Guard that transient state so it
    # stays silent instead of attempting to index NULL input values.
    req(input$radius_range, input$eqt_range, input$max_dist)
    d <- filtered_data()
    d <- d[!is.na(d$pl_rade) & !is.na(d$pl_eqt), ]
    d <- d[d$pl_rade  >= input$radius_range[1] & d$pl_rade  <= input$radius_range[2], ]
    d <- d[d$pl_eqt   >= input$eqt_range[1]    & d$pl_eqt   <= input$eqt_range[2], ]
    d <- d[is.na(d$sy_dist) | d$sy_dist <= input$max_dist, ]
    d
  })


  output$p_earth <- renderPlotly({
    d <- earth_data()
    empty <- plot_empty_state(d, c("pl_rade", "pl_eqt"),
                              "positive radius and equilibrium-temperature measurements",
                              positive = c("pl_rade", "pl_eqt"))
    if (!is.null(empty)) return(empty)
    as_interactive(plot_earth_context(d), webgl = TRUE,
                   filename = "earth-context")
  }) |> bindCache(CACHE_VERSION, global_filters(), input$radius_range, input$eqt_range, input$max_dist)
  # Shared shaping of the matching-planets set: displayed table AND CSV export
  # use the same readable, rounded columns.
  earth_table <- reactive({
    d <- earth_data()
    data.frame(
      Planet   = d$pl_name,
      `Host star` = d$hostname,
      `Radius (R Earth)` = round(d$pl_rade, 2),
      `Period (days)`    = round(d$pl_orbper, 1),
      `Est. eq. temp (K)` = round(d$pl_eqt),
      `Distance (pc)`    = round(d$sy_dist, 1),
      Method   = d$discoverymethod,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  output$t_earth <- renderDT({
    d <- earth_data(); need_rows(d)
    datatable(earth_table(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$dl_earth <- downloadHandler(
    filename = function() paste0("otherworlds_matching_planets_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(earth_table(), file)
  )
}

shinyApp(ui, server)
