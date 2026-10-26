# NIB pilot

*Updated 23 October 2026. English translation of `Readme_pilot.docx` (18 December 2025), revised to match the current code.*

R scripts that fetch bibliometric data from the NIB database (Web of Science and OpenAlex), combine it with NVA/CRIS data and run a set of example analyses. For a short overview of the repository, see the [README](../README.md). See <https://www.openscience.no/nib/pilot> for an introduction.

> **Status: prototype.** None of the analyses have been verified. Results and plots are for illustration only.

## Background

NIB is hard to use for analysis today. The database is hard to reach, the data model is complex, and the data is not prepared for analysis tools. Together this makes it hard to get started. The pilot aims to show that the gap between NIB data and an analysis of Norwegian research output can be bridged by simplifying the whole chain:

- **Simplify the data model with views.** Views tailor queries, selections and data formats into something that is easy to understand and fits the use case, while hiding the complexity of the database.
- **Simplify and partly automate downloading datasets from NIB.** The predefined datasets in the views are easy to download. They can also be exported, for example to CSV, and used in tools other than the pilot.
- **Combine NIB data with NVA/Cristin data.** One of NIB's strengths is that Cristin data can be used to analyse the Norwegian part of global publishing.
- **Run bibliometric analyses.** The scripts include a small set of generic analyses that give insight into the dataset.

In short, the pilot runs three steps in R:

1. Download data from the NIB server.
2. Combine the data with NVA/Cristin data and select a subset, for example by institution and time period.
3. Run example analyses with predefined functions and write the results to disk.

The pilot is meant as a starting point for discussing tools, and to help decide whether it could be developed into a production-ready analysis tool. It also shows how bibliometric analyses (here in R) can be built so that others can reuse them and adapt them to their own needs.

Some familiarity with R and with NIB data is assumed, and you need a user account for the NIB database.

## Limitations

- **Coverage.** Only WoS and OpenAlex records matched to a record in NVA/Cristin are included. Matching is mainly done on DOI. The current NVA/Cristin data file (release `data-2026-10`) covers publication years 2011–2025, plus a partial 2026.
- **Status of the sources.** WoS is production-ready in NIB. OpenAlex is still at the test stage: the OpenAlex data is kept up to date, but its calculations and indicators are at test level and not yet verified.
- **NVA/Cristin data.** The pilot uses a prepared NVA/Cristin data file (see [Data file](#data-file)). It cannot yet be replaced by API queries against Cristin/NVA.
- **Prototype quality.** Some functions will probably not work exactly as intended, some plots are not well adjusted, several plots may contain inaccuracies, and the code is not optimised. The pilot demonstrates what is possible; it is not production-ready.
- **WoS vs. OpenAlex.** Some analyses can currently only be run on WoS data, but most also work with OpenAlex. WoS indicators are calculated on WoS *categories*, a different classification system from OpenAlex. OpenAlex indicators have so far been developed and tested on *subfields* in the [topic classification](https://docs.openalex.org/api-entities/topics/topic-object). Which indicators are most useful should be discussed in Bibliometriforum; it is not a decision Sikt will make alone.

## Data in NIB

The pilot downloads pre-populated tables in the databases, called *materialized views*. These give better control over data format, column selection and complex underlying structures.

The test databases for WoS and OpenAlex each have several views and tables that support the pilot. For OpenAlex there are also test tables with calculated indicators. For WoS, the views live in `wos_test01`, the test database used before anything is put into production. OpenAlex still has test status and has no separate production instance. A more intuitive naming scheme for views and tables is being developed, so names may change before OpenAlex is moved to production.

All tables and views are addressed as `<db>.<schema>.<table/view>`, for example `openalex_test01.nib_views.mv_opa_works` or `openalex_test01.calculation.works_stats_full`. You can usually leave out `<db>`, since it is given by the connection.

### OpenAlex: `openalex_test01`

**Schema `nib_views` (materialized views)**

| View | Used by pilot | Description |
|---|---|---|
| `mv_opa_works` | Yes | Simplified, combined view of the most relevant columns for a work. Combines information from sources/journals with metrics, reference values and percentiles by subfield. Delivered in *tidy* format, i.e. several rows per record, since a work can belong to several subfields. |
| `mv_opa_authorinfo` | Yes | Short version of author and institution information: mainly counts, but also lists of authors and institutions. The author–institution link is not synchronised (and probably cannot be). |
| `mv_opa_authordetails` | Yes | Detailed author/institution information for each work. The same information as in `mv_opa_authorinfo`, but easier to use. |
| `mv_opa_fundings` | No | All fundings/grants linked to works matched with Cristin. Not used by the pilot functions. |
| `mv_opa_works_stats` | No | Simplified version of `calculation.works_stats_full` with averaged values (percentiles, field-normalised values), i.e. the mean across e.g. the three subfields a record is classified in. |
| `mv_opa_works_topics_extended` | No | Mainly a helper table for calculations that merges `works` and `works_topics`. Heavy to build and has many rows, but simplifies later calculations of other tables. |

**Schema `calculation` (tables)**

These tables are mainly used as input to the views.

| Table | Description |
|---|---|
| `master_percentiles_subfieldyeardt` | Master table of which percentile band a record falls in, given its number of citations. |
| `master_ref_values_subfieldyeardt` | Master table of reference values for a record by year, document type and subfield. |
| `work_cri_matches` | Link table between `varbeidlopenr` in Cristin (2010 onwards) and the OpenAlex id. |
| `works_stats_full` | All percentile calculations and field-normalised values within each subfield a record is placed in. |

### Web of Science: `wos_test01`

**Schema `nib_views` (materialized views)**

| View | Used by pilot | Description |
|---|---|---|
| `mv_wos_items` | Yes | Simplified view of the most relevant columns for a WoS item. Combines information from sources/journals with metrics, reference values and percentiles by WoS category. |
| `mv_wos_authorinfo` | Yes | Short version of author and institution information: mainly counts, but also lists of authors and institutions. The lists are not synchronised. |
| `mv_wos_authordetails` | Yes | Detailed author/institution information for each item. The same information as in `mv_wos_authorinfo`, but easier to use. |
| `mv_wos_fundings` | No | All fundings/grants linked to items matched with Cristin. Not used by the pilot functions. |

### Customising the underlying SQL

Each view is built from a more complex SQL query. You can download this SQL, change it locally and run it against the database as an ordinary query to get a dataset tailored to your needs. Both analysis scripts contain a commented-out query against `pg_catalog.pg_matviews` that does this. Note that a customised dataset will not necessarily work with the pilot's analysis functions without further changes to the code.

## Repository contents

| File | Purpose |
|---|---|
| `pilot_functions.R` | Checks and installs required packages, creates the folder structure (`data/`, `data/raw/`, `plots/`), downloads and loads the NVA/Cristin data file, and defines the analysis functions. |
| `pilot_analysis_wos.R` | Main script for Web of Science. |
| `pilot_analysis_opa.R` | Main script for OpenAlex. |
| `pilot_report_generator.Rmd` | R Markdown template that turns the results into a Word report. Called at the end of each main script. |

Each main script:

1. Sources `pilot_functions.R`.
2. Sets up the connection to the NIB server.
3. Fetches data from the NIB database (three queries).
4. Does some light post-processing and saves the raw data to `data/raw/` as `.rds` files.
5. Combines the dataset with NVA/Cristin data and applies the final selection.
6. Runs the analyses and writes the results to `plots/`.

### Data file

The NVA/Cristin data is not stored in the repository. It is published as an asset on a [GitHub release](https://github.com/openscience-no/nib_analytics_pilot/releases), and `pilot_functions.R` downloads it to `data/` the first time it runs. The file contains scientific publications (the current release covers 2011–2025, plus a partial 2026), limited to author shares with a Norwegian affiliation, and includes funder information. The file name includes the release tag, e.g. `data/data-2026-10_df_nva_oa_nib.parquet`.

When a new data file is published as a new release, update `data_release` in `pilot_functions.R` to the new tag to download it.

## Getting started

### Prerequisites

- **R and RStudio.** The scripts will probably also work in Positron, but this has not been tested.
- **A user account for the PostgreSQL database on the NIB server**, with access to `wos_test01` and/or `openalex_test01`. If you do not have an account, you can apply for access if you are affiliated with a Norwegian research institution. Read more at <https://www.openscience.no/nib>.
- **Network access to the NIB server.** Your machine's IP address must be allowed through Sikt's firewall. If your PC is located at Sikt, use edu-VPN.
- **R packages.** `pilot_functions.R` installs `tidyverse`, `RPostgres`, `ggthemes`, `stringr`, `cowplot`, `openxlsx` and `arrow` if they are missing. The Word report also needs `rmarkdown` and `flextable`, which you must install yourself.

### Installation

The easiest option is to create a new project in RStudio from version control (*File → New Project → Version Control → Git*) with the repository URL:

```
https://gitlab.sikt.no/openscience-no/nib/nib_analytics_pilot
```

This downloads a copy of all files to the right place. You can also download the files manually into a folder for your R projects. The `data/` and `plots/` folders are created automatically the first time you run the scripts.

### Running the pilot

All scripts are run manually in RStudio. There is one main script per database. Run it line by line, or source it to run the whole script at once.

1. Open `pilot_functions.R` and the main script for the database you want to test (`pilot_analysis_wos.R` or `pilot_analysis_opa.R`).
2. Set the variables at the top of the main script:

   | Variable | Description | Example |
   |---|---|---|
   | `scope` | `"Norway"` for all records, or a Cristin institution number to limit to one institution. The number can be found in the NVA/Cristin dataset. | `184` (UiB) |
   | `nvi_scope` | `"all"`, `"NVI"` (NVI-reported only) or `"NVI-articles"` (NVI-reported articles only). This choice strongly affects all inventories and analyses. | `"NVI"` |
   | `year_first`, `year_last` | Time period used in the analyses. `year_first` cannot be earlier than 2010. | `2019`, `2025` |
   | `nib_user` | Your NIB database username. | |

3. Run the line `source("pilot_functions.R")` to define the functions used by the rest of the script.
4. Run the rest of the script in order. Enter your database password when prompted.

#### Queries and post-processing

The script fetches three datasets from the database (items/works, author info and author details). Post-processing then combines the downloaded data, after which `wos_records` (WoS) or `opa_records` (OpenAlex) is defined.

#### Combining with NVA/Cristin

`wos_records` or `opa_records` is combined with the NVA/Cristin data into a final dataset. This creates `df_wos_analysis` and `df_wos_labels` (WoS) or `df_opa_analysis` and `df_opa_labels` (OpenAlex). An inventory of how many records were matched is then written to an Excel file.

#### Analyses

Every analysis consists of three calls:

1. Run the analysis function. It returns an object with a data table and a plot.
2. Save the plot to disk in `plots/`.
3. Write the data and the plot to a separate sheet in the Excel file in `plots/`. You can use this to build your own analyses or visualisations.

| # | Analysis | WoS | OpenAlex |
|---|---|:-:|:-:|
| 1 | Yearly publication counts | ✓ | ✓ |
| 2 | Most used journals | ✓ | ✓ |
| 3 | Countries with most collaboration | ✓ | ✓ |
| 4 | Open access distribution | ✓ | ✓ |
| 6 | Most productive authors (by count) | ✓ | ✓ |
| 7 | Most productive authors (by fractions) | ✓ | ✓ |
| 8 | Mean citations in the top categories | ✓ | |
| 10 | Percentile distribution within one NPI discipline | ✓ | |
| 11 | Citation distribution by OA status per NPI area (experimental) | ✓ | ✓ |

Finally, the script renders `pilot_report_generator.Rmd` into a Word report (`plots/analysepilot_oppsummering_<database>_<date>.docx`).

### Output

| Location | Content |
|---|---|
| `data/raw/` | Raw data from NIB as `.rds` files, for reuse without querying the database again. |
| `plots/` | Plots, the Excel inventory with one sheet per analysis, and the Word report. |
