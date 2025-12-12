# Pilot-script for retrieving data from NIB and run analysis
#-------------------------------------------------------------------
# OPENALEX - VERSION
#-------------------------------------------------------------------
# Prerequisites: 
#    nib-postgres-account to openalex
#    IP is allowed entry to the NIB-server
#    If pc is located in Sikt; edu-vpn
#
# In short: the script 
# - Loads a predefined cris-data-set (to be replaced with live download of data through an api)
# - Query OpenAlex-database and fetch a predefined dataset containing previously cris-matched data with citation and indicator-info
# - The two datasets are joined
# - The dataset can initially be further limited to t.ex a single institution
# - Set a time-span, which is utilized within each analysis

# Please note that some of the test-analyses choose info from either the cris or from OpenAlex, this is particularly
# important with respect to info laid out in time series. It is therefore important to understand
# the consequences of choices in each analyses and to what extent records are discarded from the dataset 

# As a general rule:
#  - Some analyses disregards time-information, and treats data as a single set
#  - In time-series, make note of which "year-info" that is used
#      - Analysis will use reported_year (from cris-data) as default and thus naturally limit the dataset to NVI-data. Cri_year is also used
#      - If citation-based indicators are used, published_year from OpenAlex will be used, naturally limiting the dataset to only matched records (both NVI and non-NVI scientific material).
#  - Disciplinary groupings (categories) will use taxonomies from OpenAlex (as indicators are connected to these), naturally limiting the dataset to only matched records.

# Also note that NONE of the analysis are verified, results and plots should therefore not be used for other than illustration purposes.
# Consider clearing the environment with
# rm(list=ls())
#-----------------------------------
# Load functions and set variables
#-----------------------------------
# Loading analysis-functions. Sourced script contains all functions necessary to run the bibliometric functions.
source("pilot_functions.R")

# Load Cristin-data from disk. (Must be located in the "/data"-folder).
# Selection of data is limited by >= 2010, contains only scientific material and only authors-hares with a Norwegian affiliation. Data also includes funder-info.
# Skip load data if variable already exists.
ifelse(exists("df_cri_oa_nib"),
       print("df_cri_oa_nib allready loaded"), 
       df_cri_oa_nib <- read.csv("data/df_cri_oa_nib.csv"))

# Set scope for analysis. Default is "Norway" e.g. all records, 
# but this can be further limited to a single institution by adding the cris-institution-number.
# for example with:
# scope <- 184      # UiB - e.g institution-number in Cristin
scope <- "Norway" # Full set (default)

# NIV-scope limits the dataset further by by selecting only data that are either 
# NVI-reported or NVI-reported articles. 
# Use the codes "NVI" or "NVI-articles" 
# Fore example nvi_scope   <- "NVI-articles"
# NB: this choice has significant bearings on all inventories and analysis as the share of matched records is depended on the variable 
nvi_scope   <- "all"    # Full set (default)

# Set year-span, which defaults to "2014" and "2024". Year_first can not be set older than 2010. 
# The limitaton will not take place in the initial creation of
# the dataset, but the values are are used for limiting the dataset in the analysis by either:
#  1. year_of_publication (OpenAlex). Found on all records that are matched
#  2. cri_year (cris). Found on all records in cris
#  3. Cri year_reported(cris). Found only on NVI-reported records

year_first <- 2019
year_last  <- 2024

# Set postgres-username
nib_user <- "larswen"

# Set db, User needs access-privileges to the database. 
db_name  <- "openalex_test01"

# Establish connection. User will be prompted for password
nib_connection <- DBI::dbConnect(RPostgres::Postgres(),
                                 host ="nibdb.bibliometri.no",
                                 dbname=db_name, 
                                 port= 5432,
                                 sslmode = "require",
                                 user=nib_user,
                                 rstudioapi::askForPassword("Database password"))

#-----------------------------------
# Retrieve data from OpenAlex
#-----------------------------------
# Fetch works
opa_works_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_opa_works") %>% 
  as_tibble()

# Retrieve authorinfo 
opa_authorinfo_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_opa_authorinfo") %>% 
  as_tibble() %>% 
  mutate(no_authors=as.double(no_authors),
         no_author_shares=as.double(no_author_shares))

# Retrieve authorinfo in details (tidyformat) 
opa_authordetails_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_opa_authordetails") %>% 
  group_by(cri_id) %>%
  mutate(no_authors=n(),
         no_author_shares=n_distinct(author_full_name)) %>% 
  ungroup() %>% 
  as_tibble()

# The below query (commented out) retrieve fundings from OpenAlex
# opa_fundings_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_opa_fundings") %>% 
#  as_tibble()

# The below query (commented out) Retrieves the underlying SQL used to create 'materialized views' fetched from OpenAlex
# May be useful if user need to change the scope of records or other tweaks.

# opa_query_definitions <- DBI::dbGetQuery(nib_connection,"SELECT schemaname, matviewname, definition FROM openalex_test01.pg_catalog.pg_matviews;")


# Disconnect db-connection
DBI::dbDisconnect(nib_connection)

# Post_treatment of openalex-data.
# Remove > 2024-records,limit on document_types, and add counts from opa_authorinfo_raw
# The scope on document_types will be expanded, but is currently limited since all matching happens on doi (which are only found on these document_types) 
opa_records <- opa_works_raw %>% 
  filter(document_type %in% c('article', 'book-chapter', 'book')) %>% 
  filter(year_of_publication < 2025) %>% 
  left_join(
    opa_authorinfo_raw %>% 
      select(unique_id,no_authors, no_author_shares) %>% 
      mutate(fraction_val = 1/no_author_shares) %>% 
      distinct(),by="unique_id") 

# Save data in folder /data/raw/ for later use
saveRDS(opa_works_raw,"data/raw/opa_works_raw.rds")
saveRDS(opa_authorinfo_raw,"data/raw/opa_authorinfo_raw.rds")
saveRDS(opa_authordetails_raw,"data/raw/opa_authordetails_raw.rds")
# saveRDS(opa_fundings_raw,"data/raw/opa_fundings_raw.rds")
saveRDS(opa_records,"data/raw/opa_records.rds")

#-----------------------------------
# Preparation of final data-set
#-----------------------------------
# At this point we have raw datasets with records from openalex, which is limited by previously matched cris-records.
# Next step is to join in cris-data into a final  data-set
# Information-labels are also for use in plots (full institution name, time-span,total number of publications etc.)

# Set scope for OpenAlex-data and join in cristin-info:
df_opa_analysis <- create_data_set(opa_records,df_cri_oa_nib,scope,nvi_scope) %>% 
  as_tibble()
df_opa_labels   <- create_labels(df_opa_analysis,scope, nvi_scope,year_first,year_last)

# -----------------------------------------
#   Create inventory
#------------------------------------------
# Create inventory for dataset and write to excel. 
# Data-sets that are generated by the below analyses, will be added as separate sheets

col_id <-"unique_id" # Setting a variable for which column is unique id, since the function can be used for other datasets.
outputfile <- paste0("plots/opa_inventory_",scope,"_",lubridate::today(),".xlsx")
df_opa_analysis_inventory <- match_inventory_cris(df_opa_analysis, col_id)
format_inventory_and_write_xlsx(df_opa_analysis_inventory, outputfile)

# Summary of different parameters that can be used in the different analyses. 
# The first four are mandatory in all analysis 
# - df_opa_analysis = scoped df to plot, 
# - df_opa_labels= labels to plot, 
# - Main data-source= "OpenAlex", "Cris" of "Cris/OpenAlex"
# - Number of elements (cutoff) = ~10-50
# - opa_authordetails_raw = authors as represented in OpenAlex
# - NPI-category = here set to "Engineering". Used in plot nr. 10
# - Themecolour = Background-colour can be set with a predefined colours set in themecolours (t.ex. themecolours[1]) or a hex-code
npi_category <- "Engineering"

# All analysis are performed in three steps: 
#  a) create a dataset and a plot, which are returned from each function as a list of two elements
#  b) Plots are saved on disk (in folder "/plots")
#  c) Data for plots (and the plot itself) are saved to inventory excel-file created above


#------------------------------------------------
#  Analysis - basic numbers
#------------------------------------------------

# 1. Basic yearly counts of publications in the dataset. Perspective from cris-data
opa_basic_numbers <- plot_basic_counts(df_opa_analysis,df_opa_labels,"Cris",themecolours[3]) 
save_plot(opa_basic_numbers$plot,df_opa_labels,"Cris","opa_basic_numbers")
add_datasheet_to_excel(opa_basic_numbers,"opa_basic_numbers",outputfile)

# 2. Most used journals
opa_top_journals <- plot_top_journals(df_opa_analysis,df_opa_labels,40,"OpenAlex",themecolors[2])
save_plot(opa_top_journals$plot,df_opa_labels,"OpenAlex","opa_top_journals")
add_datasheet_to_excel(opa_top_journals,"opa_top_journals",outputfile)

# 3. Countries with most cooperation. 
opa_top_contries <- plot_top_countries(df_opa_analysis,df_opa_labels, opa_authordetails_raw, 40,"OpenAlex",themecolors[2])
save_plot(opa_top_contries$plot,df_opa_labels,"OpenAlex","opa_top_contries")
add_datasheet_to_excel(opa_top_contries,"opa_top_contries",outputfile)


# 4. OA-distribution
opa_oa_distribution <- plot_oa_shares(df_opa_analysis,df_opa_labels,"Cris",themecolors[3])
save_plot(opa_oa_distribution$plot,df_opa_labels,"Cris","opa_oa_distribution")
add_datasheet_to_excel(opa_oa_distribution,"opa_oa_distribution",outputfile)
#------------------------------------------------
#  Analysis - Authors
#------------------------------------------------

# 6. Most productive authors by number of publications. 
opa_top_authors_count <- plot_top_authors_count(df_opa_analysis,df_opa_labels,opa_authordetails_raw,40,"OpenAlex",themecolors[2]) 
save_plot(opa_top_authors_count$plot,df_opa_labels,"OpenAlex","opa_top_authors_count")
add_datasheet_to_excel(opa_top_authors_count,"opa_top_authors_count",outputfile)

# 7. Most productive authors by fractions. 
opa_top_authors_fraction <- plot_top_authors_fraction(df_opa_analysis,df_opa_labels,opa_authordetails_raw,40,"OpenAlex",themecolors[2])
save_plot(opa_top_authors_fraction$plot,df_opa_labels,"OpenAlex","opa_top_authors_fraction")
add_datasheet_to_excel(opa_top_authors_fraction,"opa_top_authors_fraction",outputfile)
#------------------------------------------------
#  Analysis - citations 
#------------------------------------------------

# 8. Geom_point: Mean n citations in n number of NIB-categories,currently only for WoS-data 
# (Not ready)

# 10. Percentile-distribution within a single NIB-category, currently only for WoS-data 
# (Not ready)


# 11. Experimental: Citation-distribution on OA per NIB-category
opa_oa_cit_distribution_npi_area <- plot_oa_shares_citation(df_opa_analysis,df_opa_labels,"Cris/OpenAlex", themecolors[2])
save_plot(opa_oa_cit_distribution_npi_area$plot,df_opa_labels,"Cris-OpenAlex","opa_oa_cit_distribution_npi_area")
add_datasheet_to_excel(opa_oa_cit_distribution_npi_area,"opa_oa_cit_dist_npi_area",outputfile)

#-----------------------------------------
#------------END SCRIPT-------------------
#-----------------------------------------