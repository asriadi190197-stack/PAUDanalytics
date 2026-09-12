# PAUDanalytics 0.3.0

PAUDanalytics is a modular R Shiny application for longitudinal PAUD observation analysis.

## Install from GitHub

```r
install.packages("remotes")
remotes::install_github("asriadi190197-stack/PAUDanalytics")
PAUDanalytics::paudshiny()
```

After installation, launch with:

```r
PAUDanalytics::paudshiny()
```

## Version 0.3.0

The application source has been refactored into modules so new features can be added without expanding one very large `app.R` file.

Main app modules:
- Dashboard / home
- Longitudinal data input
- Class development
- Child profile
- Indicator change
- Narrative and recommendations
- Export
- Settings

Operational indicators in the application are editable analytic examples aligned to the six STPPA areas; they are not presented as a verbatim official indicator list.
