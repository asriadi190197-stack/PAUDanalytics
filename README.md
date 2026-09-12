# PAUDanalytics 0.4.0

PAUDanalytics is an R Shiny application for longitudinal early-childhood observation analytics.

Version 0.4.0 adds:

- report-card style Word (.docx) output;
- school and child identity fields in the template;
- explicit Capaian Pembelajaran (CP) Fase Fondasi menu;
- longitudinal CP summaries mapped from six STPPA-aligned aspects;
- Word report sections for identity, CP, six STPPA aspects, narrative, recommendations, teacher notes, and signatures.

## Run

```r
PAUDanalytics::paudshiny()
```

## Update from GitHub

```r
remotes::install_github("asriadi190197-stack/PAUDanalytics", force = TRUE, upgrade = "never")
PAUDanalytics::paudshiny()
```
