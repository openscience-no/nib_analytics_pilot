# Pilot-script for retrieving data from NIB and run analysis
#-------------------------------------------------------------------
# WOS - VERSION
#-------------------------------------------------------------------
# Prerequisites: 
#    nib-postgres-account to either wos or openalex, or both
#    IP is allowed entry at the NIB-server
#    If pc is located in Sikt; edu-vpn

# Please note that Cristin-info is merged into wos_records and not the other way around. 
# This will naturally discard records from the cristin-data-set. 
# wos_records will thus never be complete Cristin-representations
# Also note that NONE of the bibliometric analysis are verified. 
# Results and plots should therefore not be used for other than illustration purposes.




#-----------------------------------
# Load functions and set variables, usually set once
#-----------------------------------
# Loading analysis-functions. Sourced script contains all functions necessary to run the bibliometric try-outs.
source("pilot_functions.R")

# Set scope for analysis. Default is "Norway" e.g. all records, 
# but this can be further limited to a single institution by adding the cris-institution-number.
# Uncomment the below line with correct number
# scope <- 184      # UiB - e.g institution-number in Cristin

# Set year-span for limiting the dataset by year_of_publication as represented in WoS. 
# This feature does not use year_reported from cris
year_first <- 2020
year_last  <- 2024

# Set username
nib_user <- "larswen"

# Set db, User needs access-privileges to the database. 
db_name  <- "wos_prod"

# Establish connection. User will be prompted for password
nib_connection <- DBI::dbConnect(RPostgres::Postgres(),
                                 host ="nibdb.bibliometri.no",
                                 dbname=db_name, 
                                 port= 5432,
                                 sslmode = "require",
                                 user=nib_user,
                                 rstudioapi::askForPassword("Database password"))

#-----------------------------------
# Retrieve data from WoS
#-----------------------------------
# Retrieve items from WoS
wos_items_raw <-    DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_wos_items") %>% 
  as_tibble()

# Retrieve fundings from WoS
wos_fundings_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_wos_fundings") %>% 
  as_tibble()

# Retrieve authorinfo from WoS
wos_authorinfo_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_wos_authorinfo") %>% 
  as_tibble() %>% 
  mutate(no_authors=as.double(no_authors),
         no_author_shares=as.double(no_author_shares))

# Retrieve authorinfo in details (tidyformat) from WoS
wos_authordetails_raw <- DBI::dbGetQuery(nib_connection,"SELECT * FROM nib_views.mv_wos_authordetails") %>% 
  as_tibble()

# The below query (commented out) Retrieves the underlying SQL used to create 'materialized views' fetched from WoS.
# May be useful if user need to change the scope of records or other tweaks.

# wos_query_definitions <- DBI::dbGetQuery(nib_connection,"SELECT schemaname, matviewname, definition FROM wos_prod.pg_catalog.pg_matviews;") %>% 
#  as_tibble()

# Disconnect db-connection
DBI::dbDisconnect(nib_connection)


# Post_treatment of WoS-data.
# Remove > 2024-records, limit on document_types, and add counts of authors from wos_authorinfo_raw
wos_records <- wos_items_raw %>% 
  filter(document_type %in% c("Article", "Review", "Proceedings Paper")) %>% 
  filter(year_of_publication < 2025) %>% 
  left_join(
    wos_authorinfo_raw %>% 
      select(unique_id,no_authors, no_author_shares) %>% 
      mutate(fraction_val = 1/no_author_shares) %>% 
      distinct(),by="unique_id") 

# Save data in folder /data/raw/ for later use
saveRDS(wos_items_raw,"data/raw/wos_items_raw.rds")
saveRDS(wos_fundings_raw,"data/raw/wos_fundings_raw.rds")
saveRDS(wos_authorinfo_raw,"data/raw/wos_authorinfo_raw.rds")
saveRDS(wos_authordetails_raw,"data/raw/wos_authordetails_raw.rds")

#-----------------------------------
# Preparation of final data-set
#-----------------------------------
# At this point we have raw datasets with records from wos, which is limited by previously matched cris-records.
# Next step is to join in cris-data into a final  data-set
# Information-labels are also for use in plots (full institution name, time-span,total number of publications etc.)

# Set scope for WOS-data and join in cristin-info: 
df_wos_analysis <- create_data_set(wos_records,df_cri_oa_nib,scope,year_first,year_last)  
df_wos_labels   <- create_labels(df_wos_analysis,scope)

#------------------------------------------------
#  Analysis - basic numbers
#------------------------------------------------
# 1. Basic yearly counts of publications in the dataset
# Parameters: scoped df to plot, labels to plot, datasource, background-colour in plot (either "Blue" or "green")
plot_basic_wos <- plot_basic_counts(df_wos_analysis,df_wos_labels,"WoS", "blue") # WoS
save_plot(plot_basic_wos,df_wos_labels,"WoS","1_basic_numbers_wos")

# 2. Most used journals
plot_top_publ_journals_wos <- plot_top_journals(df_wos_analysis,df_wos_labels,40,"WoS", "blue")
save_plot(plot_top_publ_journals_wos,df_wos_labels,"WoS","2_top_publ_journals_wos")

# 3. Countries with most cooperation
plot_top_publ_contries_wos <- plot_top_countries(df_wos_analysis,df_wos_labels, wos_authordetails_raw, 40,"WoS","blue")
save_plot(plot_top_publ_contries_wos,df_wos_labels,"WoS","3_top_publ_contries")

# 4. OA-distribution
plot_oa_distribution_wos <- plot_oa_shares(df_wos_analysis,df_wos_labels,"WoS", "blue")
save_plot(plot_oa_distribution_wos,df_wos_labels,"WoS","4_oa_distribution_wos")

#------------------------------------------------
#  Analysis - Authors
#------------------------------------------------

# 5. Avg-author-count per year in each NPI-category (as given by source/journal)
plot_number_of_authors_wos <- plot_no_authors(df_wos_analysis,df_wos_labels,"WoS","blue")
save_plot(plot_number_of_authors_wos,df_wos_labels,"WoS","5_number_of_authors")

# 6. Most productive authors by number of publications
plot_top_authors_by_count_wos <- plot_top_authors_count(df_wos_analysis,df_wos_labels,wos_authordetails_raw,40,"WoS","blue") 
save_plot(plot_top_authors_by_count_wos,df_wos_labels,"WoS","6_plot_top_authors_by_count")

# 7. Most productive authors by fractions 
plot_top_authors_by_fraction_wos <- plot_top_authors_fraction(df_wos_analysis,df_wos_labels,wos_authordetails_raw,40,"WoS","blue")
save_plot(plot_top_authors_by_fraction_wos,df_wos_labels,"WoS","7_plot_top_authors_by_fraction")

#------------------------------------------------
#  Analysis - citations 
#------------------------------------------------

# 8. Geom_point: Mean n citations in n number of WoS-categories 
plot_citation_barplot_category_wos <- plot_citation_category(df_wos_analysis,df_wos_labels,40,"WoS","blue")
save_plot(plot_citation_barplot_category_wos,df_wos_labels,"WoS","8_plot_citation_barplot_category")

# 9. Experimental: Boxplot-test-version: Citation-distribution within WoS-categories. Limited to a single NPI-category. 
plot_cit_category_boxplot_wos <- plot_citation_category_box(df_wos_analysis,df_wos_labels,"Clinical medicine","WoS","blue")
save_plot(plot_cit_category_boxplot_wos,df_wos_labels,"WoS","9_plot_cit_category_boxplot")

# 10. Percentile-distribution within a single NIB-category.
plot_percentiles_wos <- plot_citation_percentiles(df_wos_analysis,df_wos_labels,"Clinical medicine","WoS","blue")
save_plot(plot_percentiles_wos,df_wos_labels,"WoS","10_plot_percentiles")

# 11. Experimental: Citation-distribution on OA per NIB-category
plot_citation_oa_distribution_wos <- plot_oa_shares_citation(df_wos_analysis,df_wos_labels,"WoS", "blue")
save_plot(plot_citation_oa_distribution_wos,df_wos_labels,"WoS","11_citation_oa_distribution")


#-----------------------------------------
#------------END SCRIPT-------------------
#-----------------------------------------