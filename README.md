# Otherworlds Dashboard

## Beyond Our Solar System

Otherworlds is an interactive R Shiny dashboard exploring how discovery methods shape our view of confirmed exoplanets. It was developed for DSA8045 Applied Analytics, Assignment 2.

The dashboard describes the detected and confirmed catalogue. It is not a census of every planet that exists, and it makes no claim about habitability or life.

## What the dashboard contains

The dashboard has seven tabs:

1. **Home:** project introduction, scope, source, and routes into the main questions.
2. **Discovery story:** discovery growth over time, discovery methods, and leading facilities.
3. **Planet diversity:** planet size, orbital period, size groups, typical values, and method distributions.
4. **Host stars:** host-star temperature and the number of known planets per system.
5. **Detection bias:** method-specific visibility, size-period comparisons, orbital-period differences, and interpretation guidance.
6. **Earth context:** radius, estimated temperature, and distance filters, Earth reference lines, matching planets, and CSV export.
7. **Conclusions:** findings, limitations, recommendations, source, and the project AI declaration.

## Contents of the clean runnable project

```text
individual_dashboard/
├── app.R
├── R/
│   ├── 01_prepare_data.R
│   ├── 02_plot_helpers.R
│   └── 03_text_helpers.R
├── data/
│   ├── raw/PSCompPars_2026.07.21_16.45.43.csv
│   └── processed/exoplanets_dashboard.csv
├── www/
│   ├── custom.css
│   └── fonts/
└── README.md
```

The consolidated project master notes are kept separately as development documentation and are not required to run the application.

## Requirements

Install R 4.1 or later. RStudio is recommended but not required. The dashboard uses the following R packages:

```r
install.packages(c(
  "shiny", "dplyr", "readr", "ggplot2", "scales", "forcats",
  "plotly", "DT", "sysfonts", "showtext", "cachem"
))
```

The separate audit script uses additional packages and is not needed to run the dashboard. The dashboard uses the bundled raw and processed data files and does not need to download data when it starts.

## How to run

1. Extract the project ZIP into a normal folder. Do not run the app from inside the ZIP archive.
2. Open the extracted `individual_dashboard` folder in RStudio, or set it as the working directory.
3. Start a fresh R session.
4. Install the packages above if they are not already installed.
5. Run:

```r
shiny::runApp(".")
```

If the folder is not the current working directory, provide its full path:

```r
shiny::runApp("/path/to/individual_dashboard")
```

The application should open in a browser. Stop it with the RStudio Stop button or press `Esc` in the R console.

On first launch, the application uses `data/processed/exoplanets_dashboard.csv`. If the processed file is absent, the preparation code can rebuild it from the raw NASA file in `data/raw/`.

## Quick demonstration route

This short route lets a first-time user verify the main interactive behaviour:

1. Open **Discovery story** and click **Kepler** under Quick eras. The year filter and discovery charts should update.
2. Switch between **Annual** and **Cumulative** to change the timeline view.
3. Open **Planet diversity** and hover over the radius-period scatterplot to inspect an individual planet.
4. Open **Earth context**, adjust the radius or temperature slider, and confirm that the summary, scatterplot, and matching-planets table update together.
5. Select two facilities, remove one facility using the small x beside its name, and confirm that the remaining selection stays active.
6. Click **Reset filters**. All global and Earth-context filters should return to their default values.

The dashboard also gives a clear message when a filter combination has no usable measurements for a particular chart. This distinguishes an empty result from a software error.

## Data and reproducibility

The dashboard uses the NASA Exoplanet Archive's Planetary Systems Composite Parameters (PSCompPars) snapshot downloaded on 21 July 2026. The snapshot contains 6,324 confirmed planet records and is kept unchanged in `data/raw/`.

Source citation:

> NASA Exoplanet Archive, Planetary Systems Composite Parameters (PSCompPars), Caltech/IPAC, DOI: 10.26133/NEA13, downloaded 21 July 2026.

The application runs from the processed file in `data/processed/`. Different charts use plot-specific complete cases because not every planet has every measurement. This means the valid count can differ between charts without indicating a calculation error.

## Filters and interactivity

The dashboard includes:

- shared discovery-year, method, and facility filters;
- quick-era buttons for Early, Kepler, TESS+, and All;
- reset controls and a collapsible filter rail;
- Earth-context planet-radius, estimated-temperature, and maximum-distance filters;
- annual or cumulative discovery views;
- count or share facility views;
- Earth reference-line controls;
- radius or mass/minimum-mass distribution views;
- interpretation-guidance controls;
- Plotly hover, zoom, pan, zoom in, zoom out, autoscale, reset, and PNG download controls;
- a searchable, sortable, paginated matching-planets table with CSV download.

The reset control restores all filters currently used by the dashboard, including the three Earth-context sliders. Individual selected facilities can be removed without clearing the other facilities. Filter guidance is available through non-invasive hover and accessibility descriptions, while a screen-reader-friendly status reports the current selection in plain English.

All ten charts use the same interactive Plotly treatment. The toolbar appears when the pointer is moved over a chart. The two dense scatterplots use WebGL rendering for smoother interaction.

## Important interpretation limits

- Method shares describe the confirmed catalogue, not the true proportion of planets in the galaxy.
- Empty regions of a method-specific plot may reflect limited sensitivity rather than the absence of planets.
- Size bands are visual groupings based on reported radius, not direct composition classes.
- Mass or minimum mass combines different measurement types and should be read with that qualification.
- Estimated equilibrium temperature is a simplified model, not a measured surface temperature.
- Earth-context matches do not establish an atmosphere, liquid water, habitability, or life.
- The source catalogue combines values from multiple studies, so individual rows may not be internally uniform.

## Fonts, styling, and external libraries

Inter and Fraunces are bundled locally in `www/fonts/`, so the dashboard does not depend on a runtime Google Fonts request. Inter is used for interface and chart text, while Fraunces is used for display headings.

Keep these font files in the final submission:

- `Inter-Regular.ttf`
- `Inter-Medium.ttf`
- `Inter-SemiBold.ttf`
- `Inter-Bold.ttf`
- `Inter-Italic.ttf`
- `Fraunces-Variable.woff2`
- `Fraunces-Variable.ttf`, the fallback declared by the CSS

Also keep `Inter-LICENSE.txt` and `Fraunces-LICENSE.txt` with the bundled fonts.

Plotly and DT are declared external browser libraries used by the application and are loaded through their R packages when the app runs. The application also uses a shared reactive filter calculation with a short debounce to avoid unnecessary redraws while controls are being moved.

## Static fallback walkthrough

Submit the static PDF walkthrough alongside the runnable ZIP and the final report PDF. The walkthrough provides screenshots and explanations for a reader who cannot run R. It is a static fallback, so it cannot reproduce live filtering, hover details, or table searching.

## Optional development files

The following files are useful for development evidence but are not required to launch the app:

- `PROJECT_MASTER_NOTES.md`, the consolidated project reference;
- `decision_log.md`, the detailed decision history;
- `PROJECT_LOG.md`, the development diary;
- `REPORT_BLUEPRINT.md`, report planning notes;
- `QA_CHECKLIST.md`, the full QA test plan;
- `COLOUR_SYSTEM.md`, the colour and accessibility rationale;
- `exoplanet_data_audit.r` and `audit_outputs/`, the audit script and outputs;
- `report_draft.md`, a working draft that may contain placeholders;
- historical design, quality, and agent instruction files.

Remove `.RData`, `.Rhistory`, `.DS_Store`, `__MACOSX`, `.claude`, temporary screenshots, and duplicate or corrupt ZIP files before creating the final submission archive.

## Development and AI declaration

Generative-AI assistance was used during development and is declared in line with module policy. All analysis, interpretation, and final wording were reviewed by the author.
