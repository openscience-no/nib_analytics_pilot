
#-------------------------------------------------------------------
# Step 0: Load data and check dependencies
#-------------------------------------------------------------------
# Set locale settings
Sys.setlocale(category = "LC_ALL", locale = "NO_NB.utf8") # A precaution for some systems

# Libraries used in the pilot
c("tidyverse", "RPostgres", "ggthemes", "stringr", "cowplot", "openxlsx", "arrow")

# Check and install packages if missing
ifelse(!"tidyverse" %in% installed.packages(),install.packages("tidyverse"), "tidyverse allready installed")
ifelse(!"RPostgres" %in% installed.packages(),install.packages("RPostgres"), "RPostgres allready installed")
ifelse(!"ggthemes" %in% installed.packages(),install.packages("ggthemes"), "ggthemes allready installed")
ifelse(!"stringr" %in% installed.packages(),install.packages("stringr"), "stringr allready installed")
ifelse(!"cowplot" %in% installed.packages(),install.packages("cowplot"), "cowplot allready installed")
ifelse(!"openxlsx" %in% installed.packages(),install.packages("openxlsx"), "openxlsx allready installed")
ifelse(!"arrow" %in% installed.packages(),install.packages("arrow"), "arrow allready installed")

# Create sub-folders if not exist
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), "Directory /data allready present")
ifelse(!dir.exists(file.path("data/raw")), dir.create(file.path("data/raw")), "Directory /data/raw allready present")
ifelse(!dir.exists(file.path("plots")), dir.create(file.path("plots")), "Directory /plots allready present")

# Load libraries
library("tidyverse")
library("ggthemes")
library("openxlsx")

# Default scopes 
year_first <- 2014
year_last  <- 2025
scope <- "Norway" # Full set (default)
nvi_scope   <- "all"    # Full set (default)
themecolours <- c( '#CFDEEC','#CFECDE','#ECECD0') # used for plotbackgrounds and signals the main datasource for the plot: 
# 1. WoS, 2. OpenAlex,3.Cris


#-------------------
# Download cris-data (NVA) from GitHub releases
#-------------------
# The data-file is not part of the git-repo, but is published as an asset on a GitHub release.
# The file is downloaded to the folder /data the first time the script is run.
# New data is published as a new release. Update data_release to the new tag to download it.
data_release <- "data-2026-10"   # Release-tag on GitHub
data_file    <- "df_nva_oa_nib.parquet"
data_url     <- paste0("https://github.com/openscience-no/nib_analytics_pilot/releases/download/",
                       data_release, "/", data_file)
data_path    <- file.path("data", paste0(data_release, "_", data_file)) # Tag in filename, so a new tag triggers a new download

if (!file.exists(data_path)) {
  message("Downloading ", data_file, " (", data_release, ") from GitHub..")
  options(timeout = max(600, getOption("timeout")))                  # Default timeout (60 sec) is too short for large files
  download.file(data_url, paste0(data_path, ".part"), mode = "wb")   # mode = "wb" is required on Windows
  file.rename(paste0(data_path, ".part"), data_path)                 # Only renamed when download is complete
} else {
  print(paste(data_file, "allready downloaded"))
}

# Loading cris-data (NVA) from disk
if (!exists("df_nva_oa_nib")) {
  df_nva_oa_nib <- arrow::read_parquet(data_path)
} else {
  print("df_nva_oa_nib allready loaded")
}

#-------------------
# Functions for inventory of data (duplicated from func_match_inventory_format_write.R)
#-------------------

# Function for creating an inventory of a dataset with joined data from cris and wos
match_inventory_cris <- function(nib_nva_df,col_id) {
  
  nib_nva_df <- nib_nva_df %>% 
    rename(unique_id:= !!col_id)
  
  # !!paste0(name_old,"_items_uid_count"):=db_old_items_uid_count
  # "col_a" %in% names(my_tibble)
  
  # Sette parametere pC% uid-unique_id og skriv-ut
  
  
  # Total numbers both total and  matched 
  tot_n_matched_per_year <- nib_nva_df %>% 
    group_by(nva_year_pub) %>% 
    summarise(n_total_records=n_distinct(nva_id),
              n_matched_records=n_distinct(unique_id)) %>% 
    arrange(desc(nva_year_pub)) %>% 
    rename(year=nva_year_pub)
  
  # Total NVI-numbers total and matched 
  nvi_n_per_year <- nib_nva_df %>% 
    group_by(nva_year_reported) %>% 
    filter(!is.na(nva_year_reported)) %>% 
    summarise(n_nvi_records=n_distinct(nva_id),
              n_matched_nvi_records=n_distinct(unique_id)) %>% 
    arrange(desc(nva_year_reported)) %>% 
    rename(year=nva_year_reported)
  
  # Total NVI-numbers total and matched (only articles)
  matched_nvi_art_n_per_year <- nib_nva_df %>% 
    group_by(nva_year_reported) %>% 
    filter(!is.na(nva_year_reported)) %>% 
    filter(nva_category_main %in% c("AcademicArticle","AcademicLiteratureReview") | 
             (nva_category_main=="AcademicChapter"& nva_chapter_type=="issn")) %>% 
    summarise(n_nvi_art_records=n_distinct(nva_id),
              n_matched_nvi_art_records=n_distinct(unique_id)) %>% 
    arrange(desc(nva_year_reported)) %>% 
    rename(year=nva_year_reported)
  
  # Bind everything together and calculates shares in percentage
  match_overview <- tot_n_matched_per_year %>% 
    left_join(nvi_n_per_year, by="year") %>% 
    left_join(matched_nvi_art_n_per_year, by="year") %>% 
    mutate(percent_matched_tot=round(n_matched_records/n_total_records*100,digits=1),
           percent_matched_nvi=round(n_matched_nvi_records/n_nvi_records*100,digits=1),
           percent_matched_nvi_art=round(n_matched_nvi_art_records/n_nvi_art_records*100,digits=1)) %>% 
    relocate(percent_matched_tot, .after=n_matched_records) %>% 
    relocate(percent_matched_nvi, .after=n_matched_nvi_records)
  
  print("Done with inventory..")
  return(match_overview)
} # End function

# Function for formatting result and write to file
format_inventory_and_write_xlsx <- function(df,file) {
  
  
  #----------Length of the three dataframes --------
  ncols_df <- ncol(df)
  nrows_df <- nrow(df)
  # -----Creating legends for each sheet ----------
  legend <- c("Publ. year","n records","n matched records","Share in percentage", 
              "n NVI records","n matched records","Share in percentage",
              "n NVI records (articles only","n matched records","Share in percentage")
  
  
  #-----------Create styles---------------
  # Create a title style
  title_style <- createStyle(
    fontSize = 14,
    fontColour = "#FFFFFF",
    halign = "center",
    fgFill = "#4F81BD",
    textDecoration = "bold",
    border = "TopBottom",
    borderColour = "#4F81BD"
  )
  
  # Create header style
  header_style <- createStyle(
    fontSize = 12,
    fontColour = "#000000",
    halign = "center",
    fgFill = "#DCE6F1",
    textDecoration = "bold",
    border = "bottom",
    borderColour = "#4F81BD"
  )
  
  # Create a cell style for legend-row
  legend_col_style <- createStyle(
    fontSize = 12,
    fontColour = "#000000",
    halign = "center",
    fgFill = "#DCE6F1",
    textDecoration = "bold",
    #border = "bottom",
    #borderColour = "darkgrey"
  )
  
  # Create a cell style for leftmost column (year)
  left_col_style <- createStyle(
    fontSize = 12,
    fontColour = "#000000",
    halign = "center",
    fgFill = "grey",
    textDecoration = "bold",
    border = "bottom",
    borderColour = "darkgrey"
  )
  
  
  # Create attention-background
  attention_col_style <- createStyle(
    #fontSize = 12,
    #fontColour = "#000000",
    #halign = "center",
    fgFill = "#f5f4eb",
    textDecoration = "bold",
    border = "TopBottomLeftRight ",
    #borderColour = "darkgrey"
  )
  
  # Create a cell style for leftmost column
  warning_style <- createStyle(
    fontSize = 12,
    fontColour = "darkred",
    # halign = "right",
    #bgFill = "white",  # Note! Use bgfill For conditional formating only
    textDecoration = "bold",
  )
  # style for "OK"-values within bounds
  green_style <- createStyle(
    fontSize = 12,
    fontColour = "darkgreen",
    # halign = "right",
    #bgFill = "white",  # Note! Use bgfill For conditional formating only
    textDecoration = "bold",
  )
  
  # Create a number style
  number_style <- createStyle(
    halign = "right",
    numFmt = "COMMA",
  )
  
  # ---------Initialize: Create new workbook/add sheets-----------
  wb <- createWorkbook()
  
  # Add worksheets
  addWorksheet(wb, "Stats")
  
  
  #-------------- Sheet 1: "Stats"-----------
  # Write header to sheet
  writeData(wb, "Stats", paste0("Inventory in dataframe"), startCol = 1, startRow = 1)
  mergeCells(wb, "Stats", cols = 1:ncols_df, rows = 1)
  addStyle(wb, "Stats", title_style, rows = 1, cols = 1:ncols_df)
  writeData(wb, "Stats", x = t(legend), startCol = 1, startRow = 2,colNames = F)
  addStyle(wb, "Stats", legend_col_style, rows = 2, cols = 1:ncols_df) 
  
  # Write data to sheet and format headers etc.
  writeData(wb, "Stats", df, startCol = 1, startRow = 3)
  addStyle(wb, "Stats", header_style,     rows = 3, cols = 1:ncols_df)
  addStyle(wb, "Stats", left_col_style,   rows = 4:(nrows_df+3), cols = 1)
  
  # Set up number-columns with 1000 separator
  addStyle(wb, "Stats", number_style, rows = 4:nrows_df, cols = 2:ncols_df, gridExpand = TRUE)
  
  # Add yellow background in percentage-columns
  addStyle(wb, "Stats", attention_col_style,   rows = 4:(nrows_df+3), cols = 4)
  addStyle(wb, "Stats", attention_col_style,   rows = 4:(nrows_df+3), cols = 7)
  addStyle(wb, "Stats", attention_col_style,   rows = 4:(nrows_df+3), cols = 10)
  
  # Adjust column widths
  setColWidths(wb, "Stats", cols = 2:10, widths = "auto")
  
  # -------------- Save the workbook-------------
  
  saveWorkbook(wb, file, overwrite = TRUE)
  
  return(paste0("Done generating ",file))
} # End function

# Function for adding a new sheet (or overwrite old one) to existing excel-file
add_datasheet_to_excel <- function(datalist,sheet_name, filepath) {
  # Function is adding a tibble made from any of the pilot-functions to a sheet in the previously made inventory 
  
  df <- datalist$data
  df_ncols <- ncol(df)
  
  # Create header style
  header_style <- createStyle(
    fontSize = 12,
    fontColour = "#000000",
    halign = "center",
    fgFill = "#DCE6F1",
    textDecoration = "bold",
    border = "bottom",
    borderColour = "#4F81BD"
  )
  
  # Opening xlsx
  wb <- loadWorkbook(filepath)
  
  # FIrst check if sheet exists, if so, delete it
  
  # Get all sheet names
  sheet_names <- getSheetNames(filepath)
  
  # Check if the sheet exists and remove it
  if (sheet_name %in% sheet_names) {
    removeWorksheet(wb, sheet_name)
    message(paste("Sheet (", sheet_name, ") allready present, sheet will be deleted and replaced.", sep = ""))
  } 
  
  # Add new sheet
  addWorksheet(wb, sheetName = sheet_name)
  # Write header and data
  writeData(wb, sheet_name, paste0("Data used for ",sheet_name), startCol = 1, startRow = 1)
  mergeCells(wb,sheet = sheet_name, cols = 1:df_ncols, rows = 1)
  addStyle(wb, sheet = sheet_name, header_style,  rows = 1, cols = 1:df_ncols)
  writeData(wb, sheet = sheet_name, x = df, startCol = 1, startRow = 2)  
  
  # Adjust column widths
  setColWidths(wb,sheet_name, cols = 1:df_ncols, widths = "auto")
  
  # Display plot and add to sheet
  print(datalist$plot)
  insertPlot(wb, sheet_name, width = 8, height = 6,
             startCol = df_ncols+ 2, startRow = 1)
  # Write file and weapping up
  saveWorkbook(wb, file = filepath, overwrite = TRUE)
  print(paste0("Done adding data: ",sheet_name, " to ",filepath))
} # End function

#-----------------------------
# Functions for preparation of data
#-----------------------------

# Prep a) Creates either a full dataset as limited and downloaded from NIB, or limited further by institution. 
create_data_set <- function(nib_items,df_cri_oa_nib,limit_code, limit_nvi) {
  
  nib_items <- nib_items %>% 
    select(-doi)
  
  if (limit_code=="Norway") { # National scope, no limiting
    nib_lim_merge_nva_full <- df_cri_oa_nib  %>% 
      left_join(nib_items, by="nva_id",relationship = "many-to-many")
    print("Scope = 'Norway'")
 # } else if (limit_code=="RCN") {  # Hack for limiting to records financed by RCN
 #    nib_lim_merge_nva_full <- df_cri_oa_nib  %>% 
 #     left_join(nib_items, by="nva_id",relationship = "many-to-many") %>%
 #      filter(cri_funder=="Norges forskningsrC%d")
 #   print("Scope = 'RCN'")
  } else if (stringr::str_length(as.integer(limit_code))>2) { # Limiting to institution by id eg. "185" for UiO
    nib_lim_merge_nva_full <- df_cri_oa_nib  %>% 
      left_join(nib_items, by="nva_id",relationship = "many-to-many") %>% 
      filter(nva_inst_id_main==limit_code) 
    print("Scope = 'Institution'")
  } else { # Exit-clause
    nib_lim_merge_nva_full <- df_cri_oa_nib  %>% 
      left_join(nib_items, by="nva_id",relationship = "many-to-many")
    
    print("No valid code supplied: Defaulting to scope = 'Norway'")
    return()
  }
  
# limit to NVI or NVI-articles
  if (limit_nvi=="NVI") {
    nib_lim_merge_nva_full <- nib_lim_merge_nva_full %>% 
      filter(nva_nvi_status=="REPORTED")
  } else if (limit_nvi=="NVI-articles") {
    nib_lim_merge_nva_full <- nib_lim_merge_nva_full %>% 
      filter(nva_nvi_status=="REPORTED") %>% 
      filter(nva_category_main %in% c("AcademicArticle","AcademicLiteratureReview") | 
               (nva_category_main=="AcademicChapter"& nva_chapter_type=="issn"))
  } # End limit
  
  print("Simplifying oa-status in dataset")
  # Simplifying some OA-info by mapping categories
  nib_lim_merge_nva_full <- nib_lim_merge_nva_full %>% 
    mutate(calculated_oa_status=calculated_oa_status %>%
             str_replace("gold_deal", "gold") %>%
             str_replace("bronze_available", "closed") %>%
             str_replace("green_no", "green") %>%
             str_replace("preprint_available_no", "closed") %>%
             str_replace("preprint_available", "closed") %>%
             str_replace("unknown", "closed") %>%
             str_replace("anomalies", "closed") %>%
             str_replace("NA", "closed") %>%
             replace_na("closed")) %>% 
    ungroup()
  
  print("calculating number of records (by nva_id)")
  # Number of records (cri_id)
  n_doc <- nib_lim_merge_nva_full %>% 
    ungroup() %>% 
    summarise(n_distinct(cri_id)) %>% 
    pull()
  
  # Returning
  print(paste("Returning a base dataset with ",n_doc," records. (Not limited by year)", sep=""))
  return(nib_lim_merge_nva_full)
  
} # End function

# Prep b) Once dataset is limited and ready for use, this function creates useful labels,later to be used in plots
create_labels <- function(nib_lim_merge_nva_full,limit_code,nvi_scope, year_first,year_last) {
  inst <- NA
  
  if (limit_code=="Norway") { # National scope, no limiting
    inst <- "Norway"
  } else if (limit_code=="RCN") { # National scope, no limiting
    inst <- "Research Council Norway"
    
  } else if (stringr::str_length(as.integer(limit_code))>2) { # Institutional scope, limiting to institution
    inst <- nib_lim_merge_nva_full %>% 
      ungroup() %>% 
      select(nva_inst_top_name) %>% 
      slice_head(n=1) %>% 
      pull()
  } else {
    print("No valid code supplied: Check and try again")
    return()
  }
  # Creating a list with info to be used as labels later
  labels <- list(institution_id=limit_code,
                 institution = inst,
                 year_first  = year_first,
                 year_last   = year_last,
                 nvi_scope = nvi_scope,
                 timespan_low=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   arrange(nva_year_pub) %>% 
                   slice_head(n=1) %>% 
                   pull(nva_year_pub), 
                 timespan_high=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   arrange(desc(nva_year_pub)) %>% 
                   slice_head(n=1) %>% 
                   pull(nva_year_pub),
                 # Counts total
                 no_publications=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   summarise(n_publ=n_distinct(nva_id)) %>% 
                   pull(),
                 no_matched_publications=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   summarise(n_nib_publ=n_distinct(unique_id)) %>% 
                   pull(),
                 # Counts NVI
                 no_nvi_publications=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   filter(!is.na(nva_year_reported)) %>%
                   summarise(n_publ=n_distinct(nva_id)) %>% 
                   pull(),
                 no_matched_nvi_publications=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   filter(!is.na(nva_year_reported)) %>%
                   summarise(n_nib_publ=n_distinct(unique_id)) %>% 
                   pull(),
                 # Counts NVI-articles
                 no_nvi_articles=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   filter(!is.na(nva_year_reported)) %>%
                   filter(nva_category_main %in% c("AcademicArticle","AcademicLiteratureReview") | 
                            (nva_category_main=="AcademicChapter"& nva_chapter_type=="issn")) %>% 
                   summarise(n_publ=n_distinct(nva_id)) %>% 
                   pull(),
                 no_matched_nvi_articles=nib_lim_merge_nva_full %>% 
                   ungroup() %>%
                   filter(!is.na(nva_year_reported)) %>%
                   filter(nva_category_main %in% c("AcademicArticle","AcademicLiteratureReview") | 
                            (nva_category_main=="AcademicChapter"& nva_chapter_type=="issn")) %>% 
                   summarise(n_nib_publ=n_distinct(unique_id)) %>% 
                   pull()
  )
  print(labels)
  return(labels)
  
} # End function

# Simple function for saving plot with correct naming
save_plot <- function(plot,analysis_data_labels,db,nr_type) {
  
  ggsave(plot, path="plots/", 
         filename=paste(analysis_data_labels$institution_id,"_",nr_type,"_",db,"_",
                        analysis_data_labels$no_publications,".jpg",sep=""), units="px", width=3000, height=2000, dpi=300)
  return("done saving plot")
} # End function

#-----------------------------
# Functions for plots
#-----------------------------
# 1. Plot for yearly counts of articles
plot_basic_counts <- function(analysis_data_set,analysis_data_labels,db, theme_colour) {
  
  # Set method of time-filtering for information in plot
  year_filtering <- "nva_year_pub"
    
  # Set timespan (n years in dataset). Used to get correct breaks in plot
  n_year_span <- analysis_data_labels$year_last-analysis_data_labels$year_first
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(nva_year_pub <= analysis_data_labels$year_last, nva_year_pub >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(nva_id))
  
  # Data-set creation
  df_all <- analysis_data_set %>% 
    ungroup() %>% 
    filter(nva_year_pub <= analysis_data_labels$year_last, nva_year_pub >=analysis_data_labels$year_first) %>% 
    group_by(nva_year_pub) %>% 
    summarise(n=n_distinct(nva_id))
  
  df_nvi <- analysis_data_set %>% 
    filter(!is.na(nva_year_reported)) %>% 
    filter(nva_year_pub <= analysis_data_labels$year_last, nva_year_pub >=analysis_data_labels$year_first) %>% 
    ungroup() %>% 
    group_by(nva_year_pub) %>% 
    summarise(n=n_distinct(nva_id))
  
  df_nvi_art <- analysis_data_set %>% 
    filter(!is.na(nva_year_reported)) %>% 
    filter(nva_category_main %in% c("AcademicArticle","AcademicLiteratureReview") | 
             (nva_category_main=="AcademicChapter"& nva_chapter_type=="issn")) %>%
    filter(nva_year_pub <= analysis_data_labels$year_last, nva_year_pub >=analysis_data_labels$year_first) %>% 
    ungroup() %>% 
    group_by(nva_year_pub) %>% 
    summarise(n=n_distinct(nva_id))
  
  # Add a source label to both datasets
  df_all <- df_all %>% mutate(source = "All data")
  df_nvi <- df_nvi %>% mutate(source = "NVI data")
  df_nvi_art <- df_nvi_art %>% mutate(source = "NVI articles")
  
  # Combine datasets for consistent color mapping
  df_combined <- bind_rows(df_all, df_nvi) %>% 
    bind_rows(df_nvi_art) %>% 
    rename(year_of_publication=nva_year_pub)
  
  # Make plot
  p <- ggplot(df_combined, aes(x = year_of_publication, y = n, colour = source)) +
    
    # Lines
    geom_line(linewidth = 1) +
    # Labels
    geom_label(aes(label = scales::comma(n)), size = 3, show.legend = FALSE) +
    # Styling
    theme_economist() +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_y_continuous(
      labels = scales::label_comma(),
      breaks = scales::breaks_pretty(n = n_year_span)) +
    scale_x_continuous(
      limits = c(analysis_data_labels$year_first, analysis_data_labels$year_last),
      breaks = seq(analysis_data_labels$year_first, analysis_data_labels$year_last, 1)
    ) +
    
    # Custom legend labels and colors
    scale_color_manual(
      values = c("All data" = "steelblue", "NVI data" = "darkgreen","NVI articles" ="black"),
      name = "Selection:"
    ) +
    
    labs(
      title = "Number of publications per year",
      subtitle = paste0(analysis_data_labels$institution, " (", 
                        analysis_data_labels$year_first, "-", 
                        analysis_data_labels$year_last, ")"),
      caption = paste0("Total no. publications used as basis for plot: ", 
                       n_records_basis,"\n(data: ", db, ", filtering on year:",year_filtering,")"),
      x = paste0("Year of publication (", db, ")"),
      y = "Number of publications"
    )
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  print("Done with calculations, returning plot and data ('wd_basic_numbers') ")
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = df_combined)
  return(result)
} # End function

# 2. Top n journals by number of articles
plot_top_journals <- function(analysis_data_set,analysis_data_labels, max_no_journals,db, theme_colour) {

  
  # Set method of time-filtering for information in plot
  year_filtering <- "year_of_publication"
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  # Count n journals
  n_journals <-analysis_data_set %>% 
    ungroup() %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>%  
    summarise(n=n_distinct(source_name)) %>% 
    pull()
  
  # Main data
  source_data <- analysis_data_set %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>%  
    filter(!is.na(source_name)) %>% 
    group_by(source_name) %>% 
    select(source_name,cri_id) %>% 
    mutate(source_name=stringr::str_to_title(source_name)) %>% 
    distinct() %>% 
    summarise(n=n_distinct(cri_id)) %>% 
    slice_max(n,n=max_no_journals) %>% 
    arrange(desc(n))
  
  # Function to truncate labels (long source-names)
  truncate_labels <- function(labels, n_char = 50) {
    ifelse(nchar(labels) > n_char, paste0(substr(labels, 1, n_char - 3), "..."), labels)
  }
  
  # Plot
  p <- source_data %>% 
    ggplot(aes(y=reorder(source_name, n), x=n)) +
    geom_segment(aes(xend=0, yend=reorder(source_name, n)), colour  = "steelblue",linewidth = 0.7) +
    geom_point(size=2, color="darkgreen") +
    geom_text(aes(label=round(n,digits=1)),size=3, nudge_x = 100) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    #scale_fill_economist() +
    #scale_color_economist() +
    #scale_x_continuous(labels = scales::label_comma()) +
    scale_y_discrete(labels = truncate_labels) +
    labs(
      title = paste("No. of publications per journal - top ",max_no_journals, sep=""),
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. journals/publications used as basis for plot:", n_journals,"/",n_records_basis," \n(data: ", db,  ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      y = "Journal",
      x = "No. publications") +
    guides(colour = "none") 
  
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  print("Done with calculations, returning plot and data ('wd_top_journals') ")
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = source_data)
  return(result)
} # End function

# 3. Top n countries in articles-cooperation 
plot_top_countries <- function(analysis_data_set,analysis_data_labels,authordetails, max_no_countries,db, theme_colour) {
  
  # Set method of time-filtering for information in plot
  year_filtering <- "year_of_publication"
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  
  # Extracting country-info from detailed author-dataset
  wad <- authordetails %>% # 
    dplyr::select(cri_id,country) %>% 
    mutate(country=replace_na(country,"unknown")) %>% 
    distinct()
  

  # Final dataset joining in country-info
  country_data <- analysis_data_set %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>%   
    select(cri_id) %>% 
    distinct() %>% 
    left_join(wad,by="cri_id") %>%     
    filter(country!="NORWAY") %>% 
    filter(country!="Norway") %>% 
    filter(country!="unknown") %>% 
    group_by(country) %>% 
    summarise(n=n_distinct(cri_id)) %>% 
    arrange(desc(n)) %>% 
    slice_max(n,n=max_no_countries)
  
  
  # Make plot  
  p <- country_data %>% #no_countries
    ggplot(aes(x=n, y=reorder(country, n),label=n)) +
    geom_segment(aes(xend=0,yend=reorder(country, n)),linewidth=0.8, color  = "steelblue") + 
    geom_point(size=3, color="darkgreen") +
    geom_text(size=3, nudge_x = 1000) + # 
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_fill_economist() +
    scale_color_economist() +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(
      title = paste("Top ",max_no_countries ," cooperating countries", sep=""),
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. publications used as basis for plot:", n_records_basis, "\n(data: ", db,  ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      x = "No. publications",
      y= "Country") +
    guides(colour = "none")
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  print("Done with calculations, returning plot and data ('wd_top_contries') ")
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = country_data)
  return(result)
} # End function

# 4. Shares of oa-categories per year
plot_oa_shares <- function(analysis_data_set,analysis_data_labels,db, theme_colour) {
  
  oa_ordered_levels_seven = (c("diamond","gold", "hybrid","hybrid_deal","green","deposited","closed"))
  oa_color_map_seven <- scale_fill_manual(values=c("lightblue3", "gold","gold3","gold4", "palegreen4","grey50","grey65"))
  
  # Set method of time-filtering for information in plot
  year_filtering <- "cri_year_reported"
  
  # Set timespan (n years in dataset). Used to get correct breaks in plot
  n_year_span <- analysis_data_labels$year_last-analysis_data_labels$year_first
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(cri_year_reported <= analysis_data_labels$year_last, cri_year_reported >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  
  # Plot of OA counts per year
  oa_data <- analysis_data_set %>% 
    filter(!is.na(cri_year_reported) & cri_category_type == "Artikkel") %>% # Selecting articles only
    filter(cri_year_reported <= analysis_data_labels$year_last, cri_year_reported >=analysis_data_labels$year_first) %>% 
    select(cri_id, cri_year_reported, calculated_oa_status) %>% 
    distinct() %>% 
    mutate(calculated_oa_status=factor(calculated_oa_status,levels=oa_ordered_levels_seven)) %>%
    group_by(cri_year_reported,calculated_oa_status) %>% 
    summarise(n = n_distinct(cri_id)) %>% 
    group_by(cri_year_reported) %>% 
    mutate(total=sum(n),
           pct = round(n/total*100,digits=1)) 
  
  p <- oa_data %>% 
    ggplot(aes(x = cri_year_reported, y = total, fill = calculated_oa_status, label = paste(pct," %/ n= ",n,sep=""))) +
    geom_bar(aes(y = pct), position = position_stack(reverse = TRUE),stat="identity") +
    geom_text(aes(y = pct), position = position_stack(reverse = TRUE),stat="identity", size=3, vjust=1) +
    theme_economist() +
    # Styling
    theme_economist() +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour))+
    
    scale_y_continuous(labels = scales::label_comma(),
                       breaks = scales::breaks_pretty(n = n_year_span)) +
    scale_x_continuous(
      #limits = c(analysis_data_labels$timespan_low, analysis_data_labels$timespan_high),
      breaks = seq(analysis_data_labels$year_first, analysis_data_labels$year_last, 1)
    ) +
    oa_color_map_seven +
    labs(
      title = "Number of articles per OA-category/year",
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. publications used as basis for plot:", n_records_basis,"\n(data: ", db,  ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      x = "Year of publication (reported-year)",
      y= "Percent of articles") 
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  print("Done with calculations, returning plot and data ('wd_oa_distribution') ")
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = oa_data)
  return(result)
} # End function

# 6. Top n most productive authors measured by number of publications
plot_top_authors_count <- function(analysis_data_set,analysis_data_labels,authordetails, max_no_auth,db, theme_colour) {
  
  # Top n author-list by number of publications and fractions. Includes only authorshares 
  # affiliated with "NORWAY". An author could thus have affiliations to a foreign institution (exclusively), these are not counted.
  
  # Set method of time-filtering for information in plot
  year_filtering <- "year_of_publication"
  
  # Calculating number of records that forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  # Limited dataset within current timespan
  lim_data_set <- analysis_data_set %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>%   
    select(cri_id, year_of_publication) %>% 
    distinct()
  
  # Create final dataset by limiting authordetails by publications within the given timeframe
  author_data <- lim_data_set %>% 
    left_join(authordetails, relationship = "many-to-many", by="cri_id") %>% 
    filter(toupper(country)=="NORWAY") %>% # Selecting 
    select(cri_id,author_full_name,no_authors) %>% 
    distinct() %>% 
    mutate(fraction_val=1/no_authors) %>% 
    group_by(author_full_name) %>% 
    summarise(n=n_distinct(cri_id),
              fraction_sum=sum(fraction_val)) %>% 
    slice_max(n,n=max_no_auth) 
  
  p <- author_data %>% 
    ggplot(aes(x=n, y=reorder(author_full_name, n))) +
    geom_segment(aes(xend=0,yend=reorder(author_full_name, n)),linewidth=0.8, color  = "steelblue") + 
    geom_point(size=2, color="darkblue") +
    geom_point(aes(x=fraction_sum),size=2, color="darkgreen") +
    geom_text(aes(label=n),size=3, nudge_x = 8) +
    geom_text(aes(x=fraction_sum,label=round(fraction_sum,digits=1)),size=3, nudge_x = 8) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_fill_economist() +
    scale_color_economist() +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(
      title = paste("Top ",max_no_auth," authors by number of publications \n(green=sum fractions)", sep=""),
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. publications used as basis for plot:", n_records_basis,"\n(data: ", db,  ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      x = "No. publications/fractions",
      y= "Author") +
    guides(colour = "none")
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  print("Done with calculations, returning plot and data ('wd_top_authors_count') ")
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = author_data)
  print("Done")
  return(result)
} # End function

# 7. Top n most productive authors measured by fractional summaries
plot_top_authors_fraction <- function(analysis_data_set,analysis_data_labels,authordetails, max_no_auth,db, theme_colour) {
  
  # Set method of time-filtering for information in plot
  year_filtering <- "year_of_publication"
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  # Limited dataset within current timespan
  lim_data_set <- analysis_data_set %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>%   
    select(cri_id, year_of_publication) %>% 
    distinct()
  
  # Sorted by fraction instead of number of publications
  author_f_data <- lim_data_set %>% 
    left_join(authordetails, relationship = "many-to-many", by="cri_id") %>% 
    filter(toupper(country)=="NORWAY") %>% 
    select(cri_id,author_full_name,no_authors) %>% 
    distinct() %>% 
    mutate(fraction_val=1/no_authors) %>% 
    group_by(author_full_name) %>%
    summarise(n=n_distinct(cri_id),
              fraction_sum=sum(fraction_val)) %>% 
    slice_max(fraction_sum,n=max_no_auth) 
  
  p <- author_f_data %>% 
    ggplot(aes(x=fraction_sum, y=reorder(author_full_name, fraction_sum))) +
    geom_segment(aes(xend=0,yend=reorder(author_full_name, fraction_sum), color  = "steelblue")) + 
    geom_point(size=2, color="darkgreen") +
    geom_text(aes(label=round(fraction_sum,digits=1)),size=3, nudge_x = 5) +
    geom_point(aes(x=n),size=2, color="darkblue") + # alpha=0.5
    geom_text(aes(x=n,label=round(n,digits=1)),size=3, nudge_x = 5) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_fill_economist() +
    scale_color_economist() +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(
      title = paste("Top ",max_no_auth," authors by fractional counting \n(blue=no. publications)",sep=""),
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. publications used as basis for plot:", n_records_basis,"\n(data: ", db, ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      x = "No. publications/fractions",
      y= "Author") +
    guides(colour = "none")
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = author_f_data)
  print("Done with calculations, returning plot and data ('wd_top_authors_fraction') ")
  return(result)
} # End function

# 8. Mean number of citations in top n categories
plot_citation_category <- function(analysis_data_set,analysis_data_labels, max_no_categories,db,theme_colour) {
  
  # Set method of time-filtering for information in plot
  year_filtering <- "year_of_publication"
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  # Geom_point: Citation-distribution per wos-category (limited to input_category) 
  cit_cat_dataset <- analysis_data_set %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>%   
    filter(!is.na(category)) %>% 
    group_by(category) %>% 
    select(category,cri_id, cited_by_count, category_reference_val) %>% 
    distinct() %>% 
    summarise(n=n_distinct(cri_id),
              avg_cited_by_count= mean(cited_by_count),
              mean_crv = mean(category_reference_val), # Probably not working
              max_cited=max(cited_by_count)) %>% 
    slice_max(order_by = avg_cited_by_count, n=max_no_categories) 
  
  p <- cit_cat_dataset %>% 
    ggplot(aes(x=avg_cited_by_count, y=reorder(category, avg_cited_by_count))) +
    geom_segment(aes(xend=0,yend=reorder(category, avg_cited_by_count), colour  = "steelblue")) + 
    geom_point(size=2, color="darkblue") +
    geom_point(aes(x=mean_crv),size=2, color="darkgreen") + # alpha=0.5
    geom_text(aes(label=round(avg_cited_by_count,digits=1)),size=3, nudge_x = 1.5) +
    geom_text(aes(x=mean_crv,label=round(mean_crv,digits=1)),size=3, nudge_x = -1.5) +
    geom_text(aes(x=-6,label=paste(" (n=",n,")", sep="")), size=3, nudge_x = 2) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_fill_economist() +
    scale_color_economist() +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(
      title = paste("Mean n citations per category"," (top ",max_no_categories,")", "\n","With field-norm ref-val (green).", sep=""),
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. publications used as basis for plot:", n_records_basis,"\n(data: ", db, ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      x = "No. citations",
      y= "Category") +
    guides(colour = "none")
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = cit_cat_dataset)
  print("Done with calculations, returning plot and data ('wd_mean_cit_top_categories') ")
  return(result)
} # End function

# 10. Shares of publication that sits  within the 10,25 and 50-percentile
plot_citation_percentiles <- function(analysis_data_set,analysis_data_labels, input_category,db,theme_colour) {
  
  # Set method of time-filtering for information in plot
  year_filtering <- "year_of_publication"
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  # Percentile-distribution
  percentile_data <- analysis_data_set %>% 
    filter(year_of_publication <= analysis_data_labels$year_last, year_of_publication >=analysis_data_labels$year_first) %>%   
    filter(nib_category==input_category) %>% 
    select(cri_id,category_avg_percentil,category) %>% 
    filter(!is.na(category)) %>% 
    distinct() %>% 
    group_by(category) %>% 
    summarise(n=n_distinct(cri_id),
              n_top10 = n_distinct(cri_id[category_avg_percentil<10]),
              n_top25 = n_distinct(cri_id[category_avg_percentil<25]),
              n_top50 = n_distinct(cri_id[category_avg_percentil<50]),
              share_top10=round(n_top10/n*100,digits=1),
              share_top25=round(n_top25/n*100,digits=1),
              share_top50=round(n_top50/n*100,digits=1)) 
  
  p <- percentile_data  %>% 
    ggplot(aes(x=share_top10, y=reorder(category, share_top10))) +
    geom_segment(aes(xend=0,yend=reorder(category, share_top10), color  = "steelblue")) + 
    geom_point(size=2, color="darkorange") +
    geom_text(aes(label=share_top10),size=3, nudge_x = 2) +
    geom_point(aes(x=share_top25), size=2, color="darkgreen") +
    geom_text(aes(x=share_top25,label=share_top25), size=3, nudge_x = 2) +
    geom_point(aes(x=share_top50),size=2, color="blue3") +
    geom_text(aes(x=share_top50,label=share_top50), size=3, nudge_x = 2) +
    geom_text(aes(x=-6,label=paste(" (n=",n,")", sep="")), size=3, nudge_x = 2) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_fill_economist() +
    scale_color_economist() +
    scale_x_continuous(labels = scales::label_comma()) +
    labs(
      title = paste("Shares of 10/25/50-percentil publications\n","Limited to ",input_category,sep=""),
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. publications used as basis for plot:", n_records_basis,"\n(data: ", db,  ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      x = "Share (in %)",
      y= "category") +
    guides(colour = "none")
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = percentile_data)
  print("Done with calculations, returning plot and data ('wd_percentiles') ")
  return(result)
} # End function

# 11. Summary of citation-impact per oa-category
plot_oa_shares_citation <- function(analysis_data_set,analysis_data_labels,db,theme_colour) {
  
  # Set palett for OA-categories
  oa_ordered_levels_seven = (c("diamond","gold", "hybrid","hybrid_deal","green","deposited","closed"))
  oa_color_map_seven <- scale_fill_manual(values=c("lightblue3", "gold","gold3","gold4", "palegreen4","grey50","grey65"))
  
  # Set method of time-filtering for information in plot
  year_filtering <- "cri_year"
  
  # Calculating number of records which forms basis for current analysis
  n_records_basis <-analysis_data_set %>%
    ungroup() %>% 
    filter(cri_year <= analysis_data_labels$year_last, cri_year >=analysis_data_labels$year_first) %>% 
    summarise(n=n_distinct(cri_id))
  
  # Calculating mean-values per NPI-area for comparison across OA-categories
  avg_lines <- analysis_data_set  %>% 
    filter(!is.na(cri_npi_area)) %>%
    select(cri_id, calculated_oa_status,cited_by_count,cri_npi_area) %>% 
    distinct() %>% 
    mutate(calculated_oa_status=factor(calculated_oa_status,levels=oa_ordered_levels_seven)) %>%
    group_by(cri_npi_area) %>% 
    summarise(mean_npi_cited=round(mean(cited_by_count,na.rm = T),digits=1))
  
  # Grouping on NPI-area.
  oa_cit_data <- analysis_data_set %>% 
    filter(!is.na(cri_npi_area)) %>%
    select(cri_id, calculated_oa_status,cited_by_count, cri_npi_area) %>% 
    distinct() %>% 
    mutate(calculated_oa_status=factor(calculated_oa_status,levels=oa_ordered_levels_seven)) %>%
    group_by(cri_npi_area,calculated_oa_status) %>% 
    summarise(n = n_distinct(cri_id),
              mean_oa_npi_cited=round(mean(cited_by_count,na.rm = T),digits=1)) 
  
  p <- oa_cit_data %>% 
    ggplot(aes(x = calculated_oa_status, y = mean_oa_npi_cited,fill=calculated_oa_status, 
               label = paste("cit=",mean_oa_npi_cited,"/n= ",n,sep=""))) + #\n
    geom_col() +
    geom_text(size=2,vjust=0.5) +
    geom_hline(aes(yintercept = mean_npi_cited), data = avg_lines, color = "darkred", linewidth=0.3, linetype="dashed") +
    theme_economist() +
    theme(axis.text=element_text(size=5),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.background = element_rect(fill = theme_colour),
          legend.text = element_text(size=5),
          strip.text = element_text(size = 10)) +
    #scale_fill_economist() +
    #scale_color_economist() +
    oa_color_map_seven +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(
      title = "Mean number of citations per OA-type/NPI-area",
      subtitle = paste(analysis_data_labels$institution," (",
                       analysis_data_labels$year_first,"-",
                       analysis_data_labels$year_last,")",sep=""),
      caption = paste("Total no. publications used as basis for plot:", n_records_basis,"\n(data: ", db,  ", filtering on year:",year_filtering,")"),
      tag = waiver(),
      x = "OA-type",
      y= "number of citations") +
    labs(fill="OA-type") +
    facet_wrap(~cri_npi_area, scales= "free_y",ncol=2)
  
  # Add annotation to the canvas (not plot panel)
  p <- cowplot::ggdraw(p) +
    cowplot::draw_label(
      paste0("This is a NIB-test-plot using ",db,"-data.\nResults are not verified."),
      x = 0.03, y = 0.03,  # bottom left corner in [0,1] canvas coords
      hjust = 0, vjust = 0,
      fontface = "plain",
      size = 10,
      color = "darkred"
    )
  # returning results in a list, plot and data
  result <- list(plot=p,
                 data = oa_cit_data)
  print("Done with calculations, returning plot and data ('wd_oa_cit_distribution_npi_area') ")
  return(result)
} # End function
