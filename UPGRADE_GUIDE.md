# Upgrade PAUDanalytics to 0.3.0

This release refactors the Shiny application into modules without changing the basic launch command.

## Replace files in the local GitHub repository

Copy these items into the root of your local `PAUDanalytics` repository and allow Windows to merge/replace existing files:

- `DESCRIPTION`
- `NAMESPACE`
- `README.md`
- `LICENSE`
- `R/`
- `inst/`

The important new structure is:

```text
PAUDanalytics/
├── DESCRIPTION
├── NAMESPACE
├── README.md
├── R/
│   └── paudshiny.R
└── inst/
    └── shiny-app/
        ├── app.R
        ├── R/
        │   ├── config.R
        │   ├── demo_data.R
        │   └── helpers.R
        └── modules/
            ├── mod_home.R
            ├── mod_data.R
            ├── mod_class.R
            ├── mod_child.R
            ├── mod_indicator.R
            ├── mod_narrative.R
            ├── mod_export.R
            └── mod_settings.R
```

## Commit and push

Suggested commit message:

`Refactor app into modular architecture v0.3.0`

Then commit to `main` and Push origin in GitHub Desktop.

## Reinstall for testing

```r
remotes::install_github("asriadi190197-stack/PAUDanalytics", force = TRUE, upgrade = "never")
PAUDanalytics::paudshiny()
```

## Future feature development

Add a new feature as a new module under `inst/shiny-app/modules/` when possible. Keep scoring, metadata, and reusable rules in `inst/shiny-app/R/` rather than putting them directly into `app.R`.
