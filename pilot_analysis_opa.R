# Pilot-script for retrieving data from NIB and run analysis
#-------------------------------------------------------------------
# OPENALEX - VERSION
#-------------------------------------------------------------------
# Prerequisites: 
#    nib-postgres-account to either wos or openalex, or both
#    IP is allowed entry at the NIB-server
#    If pc is located in Sikt; edu-vpn

# - Checks dependencies, sets up connection to server and connects to either WoS or OpenAlex. 
# - Retrieves datasets by passing queries to server. Performs some post-treatment of the raw datasets

# Please note that at current, NONE of the bibliometric analysis are verified. 
# Results and plots should therefore not be used for other than illustration purposes.

# Loading analysis-functions. Sourced script contains all functions necessary to run the bibliometric try-outs.
source("pilot_functions.R")

# Please note that Cristin-info is merged into opa_records and not the other way around. 
# This will naturally discard records from the cristin-data-set. 
# opa_records will thus never be complete with cris-representations

#-----------------------------------
# Set variables. Usually set once
#-----------------------------------

# Set scope for analysis. Default is "Norway" e.g. all records, 
# but this can be limited to a single institution by institution-number i cris
# Uncomment the below line with correct number
# scope <- 184      # UiB - e.g institution-number in Cristin

# Set year-span limiting the dataset by year_of_publication as represented in WoS. This feature does not use year_reported from cris
year_first <- 2020
year_last  <- 2024

# Define username
nib_user <- "larswen"

# Choose db to fetch data from.  Obviously user needs correct privileges and access to the database to be queried. 
db_name  <- "openalex_test01"

# Create connection. User will be prompted for password
nib_connection <- DBI::dbConnect(RPostgres::Postgres(),
                                 host ="nibdb.bibliometri.no",
                                 dbname=db_name, 
                                 port= 5432,
                                 sslmode = "require",
                                 user=nib_user,
                                 rstudioapi::askForPassword("Database password"))


# Retrieve works from OpenAlex
opa_works_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_opa_works") %>% 
  as_tibble()

# Retrieve authorinfo from OpenAlex
opa_authorinfo_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_opa_authorinfo") %>% 
  as_tibble() %>% 
  mutate(no_authors=as.double(no_authors),
         no_author_shares=as.double(no_author_shares))

# The below query (commented out) retrieve the SQL used to create materialized views for the  datasets from openalex
# May be useful if user need to change the scope of records or other tweaks in custom sql.
# opa_query_definitions <- DBI::dbGetQuery(nib_connection,"SELECT schemaname, matviewname, definition FROM openalex_test01.pg_catalog.pg_matviews;")


# Disconnect db-connection
DBI::dbDisconnect(nib_connection)

# Post_treatment of openalex-data.
# Remove > 2024-records,limit on document_types, and add counts from opa_authorinfo_raw
opa_records <- opa_works_raw %>% 
  filter(document_type %in% c('article', 'book-chapter', 'book')) %>% 
  filter(year_of_publication < 2025) %>% 
  left_join(
    opa_authorinfo_raw %>% 
      select(unique_id,no_authors, no_author_shares) %>% 
      mutate(fraction_val = 1/no_author_shares) %>% 
      distinct(),by="unique_id") 

# Save data
saveRDS(opa_works_raw,"data/raw/opa_works_raw.rds")
saveRDS(opa_authorinfo_raw,"data/raw/opa_authorinfo_raw.rds")

# Preparation of final data-set: 
# At this point we have raw datasets with records from wos, limited by previously matched records in the cris
# Next step is to join in cris-data into a final scoped data-set limited by institution (optional).. 
# Also create labels for use in plots (full institution name, time-span, total number of publications etc.)

# Set scope for OpenAlex-data and join in cristin-info:
df_opa_analysis <- create_data_set(opa_records,df_cri_oa_nib,scope,year_first,year_last)  
df_opa_labels   <- create_labels(df_opa_analysis,scope)

#------------------------------------------------
#  Analysis - basic numbers
#------------------------------------------------

# 1. Basic yearly counts of publications in the dataset
# Parameters: scoped df to plot, labels to plot, datasource, background-colour in plot (either "Blue" or "green")
plot_basic_opa <- plot_basic_counts(df_opa_analysis,df_opa_labels,"OpenAlex","green") # OpenAlex
save_plot(plot_basic_opa,df_opa_labels,"OpenAlex","1_basic_numbers_opa")


# 2. Most used journals
plot_top_publ_journals_opa <- plot_top_journals(df_opa_analysis,df_opa_labels,40,"OpenAlex","green")
save_plot(plot_top_publ_journals_opa,df_opa_labels,"OpenAlex","2_top_publ_journals_opa")


# 3. Countries with most cooperation
# (Not ready)

# 4. OA-distribution
# OpenAlex:
plot_oa_distribution_opa <- plot_oa_shares(df_opa_analysis,df_opa_labels,"OpenAlex","green")
save_plot(plot_oa_distribution_opa,df_opa_labels,"OpenAlex","4_oa_distribution_opa")


#------------------------------------------------
#  Analysis - Authors
#------------------------------------------------

# 5. Avg-author-count per year in each NPI-category (as given by source/journal)
plot_number_of_authors_opa <- plot_no_authors(df_opa_analysis,df_opa_labels,"OpenAlex","green")
save_plot(plot_number_of_authors_opa,df_opa_labels,"OpenAlex","5_number_of_authors")

# 6. Most productive authors by number of publications
# (Not ready)

# 7. Most productive authors by fractions 
# (Not ready)
#------------------------------------------------
#  Analysis - citations 
#------------------------------------------------

# 8. Geom_point: Mean n citations in n number of WoS-categories 
# (Not ready)

# 9. Experimental: Boxplot-test-version: Citation-distribution within WoS-categories. Limited to a single NPI-category. 
# (Not ready)


# 10. Percentile-distribution within  a single NIB-category.
# (Not ready)


# 11. Experimental: Citation-distribution on OA per NIB-category
# (Not ready)

#-----------------------------------------
#------------END SCRIPT-------------------
#-----------------------------------------