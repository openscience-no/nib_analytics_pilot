
#-------------------------------------------------------------------
# Step 0: Load data and check dependencies
#-------------------------------------------------------------------
Sys.setlocale("LC_ALL") # A precaution

# Libraries used in the pilot
c("tidyverse", "RPostgres","ggthemes","stringr","cowplot")

# Check and install packages if missing
ifelse(!"tidyverse" %in% installed.packages(),install.packages(tidyverse), "tidyverse allready installed")
ifelse(!"RPostgres" %in% installed.packages(),install.packages(RPostgres), "RPostgres allready installed")
ifelse(!"ggthemes" %in% installed.packages(),install.packages(ggthemes), "ggthemes allready installed")
ifelse(!"stringr" %in% installed.packages(),install.packages(stringr), "stringr allready installed")
ifelse(!"cowplot" %in% installed.packages(),install.packages(cowplot), "cowplot allready installed")

# Create sub-folders if not exist
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), "Directory /data allready present")
ifelse(!dir.exists(file.path("data/raw")), dir.create(file.path("data/raw")), "Directory /data/raw allready present")
ifelse(!dir.exists(file.path("plots")), dir.create(file.path("plots")), "Directory /plots allready present")

# Load libraries
library("tidyverse")
library("ggthemes")

# Load Cristin-data from disk. Must be copied into the "/data"-folder.
# Selection of data is limited to >= 2010 and only authors-hares with a Norwegian affiliation. Also includes funder-info.
# Do not load data if variable exists.
ifelse(exists("df_cri_oa_nib"),print("df_cri_oa_nib allready loaded"), df_cri_oa_nib <- read_rds("data/df_cri_oa_nib.rds"))

# Default scope 
scope <- "Norway" # Full set (default)
#-----------------------------
# Functions for preparation of data
#-----------------------------

# Prep a) Creates either a full dataset as limited and downloaded from NIB, or limited further by institution. 
create_data_set <- function(nib_items,df_cri_oa_nib,limit_code, year_first,year_last) {

  if (limit_code=="Norway") { # National scope, no limiting
    nib_lim_merge_nva_full <- nib_items %>% 
      left_join(df_cri_oa_nib, by="cri_id",relationship = "many-to-many")
    print("Scope = 'Norway'")
  } else if (limit_code=="RCN") {  # Hack for limiting to records financed by RCN
    nib_lim_merge_nva_full <- nib_items %>% 
      left_join(df_cri_oa_nib, by="cri_id",relationship = "many-to-many") %>%
      filter(cri_funder=="Norges forskningsråd")
    print("Scope = 'RCN'")
  } else if (stringr::str_length(as.integer(limit_code))>2) { # Limiting to institution by id eg. "185" for UiO
    nib_lim_merge_nva_full <- nib_items %>% 
      left_join(df_cri_oa_nib, by="cri_id",relationship = "many-to-many") %>% 
      filter(cri_inst_id==limit_code) 
    print("Scope = 'Institution'")
  } else { # Exit-clause
    nib_lim_merge_nva_full <- nib_items %>% 
      left_join(df_cri_oa_nib, by="cri_id",relationship = "many-to-many")
    
    print("No valid code supplied: Defaulting to scope = 'Norway'")
    return()
  }
  
  # Crude filtering without checking values. Will work technically
  nib_lim_merge_nva_full <-nib_lim_merge_nva_full %>% 
    filter(year_of_publication >= year_first) %>% 
    filter(year_of_publication <= year_last)
  
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
           replace_na("closed"))
  # Returning
  print(paste("Returning df with ",nrow(nib_lim_merge_nva_full)," rows.", sep=""))
  return(nib_lim_merge_nva_full)
  
} # End function

# Prep b) Once dataset is limited and ready for use, this function creates useful labels,later to be used in plots
create_labels <- function(nib_lim_merge_nva_full,limit_code) {
  inst <- NA
  
  if (limit_code=="Norway") { # National scope, no limiting
    inst <- "Norway"
  } else if (limit_code=="RCN") { # National scope, no limiting
      inst <- "Research Council Norway"
      
  } else if (stringr::str_length(as.integer(limit_code))>2) { # Institutional scope, limiting to institution
    inst <- nib_lim_merge_nva_full %>% 
      ungroup() %>% 
      select(cri_inst_name_all) %>% 
      slice_head(n=1) %>% 
      pull()
  } else {
    print("No valid code supplied: Check and try again")
    return()
  }
  # Setting labels in list
  labels <- list(institution_id=limit_code,
                 institution=inst,
                 timespan_low=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   arrange(year_of_publication) %>% 
                   slice_head(n=1) %>% 
                   pull(year_of_publication), 
                 timespan_high=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   arrange(desc(year_of_publication)) %>% 
                   slice_head(n=1) %>% 
                   pull(year_of_publication),
                 no_publications=nib_lim_merge_nva_full %>% 
                   ungroup() %>% 
                   summarise(n_publ=n_distinct(cri_id)) %>% 
                   pull())
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
plot_basic_counts <- function(analysis_data_set,analysis_data_labels,db, pick_colour) {
  
  # Set timespan (n years in dataset). Used to get correct breaks in plot
  n_year_span <- analysis_data_labels$timespan_high-analysis_data_labels$timespan_low
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Data-set creation
  df_all <- analysis_data_set %>% 
    ungroup() %>% 
    group_by(year_of_publication) %>% 
    summarise(n_publ=n_distinct(cri_id))
  
  df_nvi <- analysis_data_set %>% 
    filter(!is.na(cri_year_reported)) %>% 
    ungroup() %>% 
    group_by(year_of_publication) %>% 
    summarise(n_publ=n_distinct(cri_id))
  
  # Add a source label to both datasets
  df_all <- df_all %>% mutate(source = "All data")
  df_nvi <- df_nvi %>% mutate(source = "NVI data")
  
  # Combine datasets for consistent color mapping
  df_combined <- bind_rows(df_all, df_nvi)
  
  # Make plot
  #nudged_theme_and_axis <-nudge_theme("use_yearly",pick_colour,analysis_data_labels)

  p <- ggplot(df_combined, aes(x = year_of_publication, y = n_publ, colour = source)) +
    
    # Lines
    geom_line(linewidth = 1) +
    
    # Labels
    geom_label(aes(label = scales::comma(n_publ)), size = 3, show.legend = FALSE) +
    

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
       limits = c(analysis_data_labels$timespan_low, analysis_data_labels$timespan_high),
       breaks = seq(analysis_data_labels$timespan_low, analysis_data_labels$timespan_high, 1)
     ) +

    # Custom legend labels and colors
    scale_color_manual(
      values = c("All data" = "steelblue", "NVI data" = "darkgreen"),
      name = "Selection:"
    ) +
    
    labs(
      title = paste0("Number of publications per year (data: ", db, ")"),
      subtitle = paste0(analysis_data_labels$institution, " (", 
                        analysis_data_labels$timespan_low, "-", 
                        analysis_data_labels$timespan_high, ")"),
      caption = paste("Total no. publications in dataset: ", 
                      analysis_data_labels$no_publications),
      x = paste0("Year of publication (", db, ")"),
      y = "Number of publications"
    )
  
  return(p)
} # End function

# 2. Top n journals by number of articles
plot_top_journals <- function(analysis_data_set,analysis_data_labels, max_no_journals,db, pick_colour) {
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Count n journals
  n_journals <-analysis_data_set %>% 
    summarise(n=n_distinct(source_name)) %>% 
    pull()
  
  # Source-distribution
  p <- analysis_data_set %>% 
    filter(!is.na(source_name)) %>% 
    group_by(source_name) %>% 
    select(source_name,cri_id) %>% 
    mutate(source_name=stringr::str_to_title(source_name)) %>% 
    distinct() %>% 
    summarise(n_dis=n_distinct(cri_id),
              n=n()) %>% 
    slice_max(n_dis,n=max_no_journals) %>% 
    arrange(desc(n_dis)) %>% 
    # Plot
    ggplot(aes(y=reorder(source_name, n_dis), x=n_dis)) +
    geom_segment(aes(xend=0, yend=reorder(source_name, n_dis)), colour  = "steelblue",linewidth = 0.7) +
    geom_point(size=2, color="darkgreen") +
    geom_text(aes(label=round(n_dis,digits=1)),size=3, nudge_x = 7) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    #scale_fill_economist() +
    #scale_color_economist() +
    #scale_x_continuous(labels = scales::label_comma()) +
    labs(
      title = paste("No. of publications per journal - top ",max_no_journals, sep=""),
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. journals in ",db ,"-dataset:",n_journals, sep=""),
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
  
  return(p)
} # End function

# 3. Top n countries in articles-cooperation 
plot_top_countries <- function(analysis_data_set,analysis_data_labels,authordetails, max_no_countries,db, pick_colour) {

  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Creating dataset
  wad <- authordetails %>% # 
    dplyr::select(cri_id,country) %>% 
    mutate(country=replace_na(country,"unknown")) %>% 
    distinct()
  
  ds <- analysis_data_set %>% 
    select(cri_id) %>% 
    distinct() %>% 
    left_join(wad,by="cri_id") %>% 
    group_by(country) %>% 
    summarise(no=n_distinct(cri_id)) %>% 
    arrange(desc(no))

  # Identifying unknown contributors
  n_unknown <- ds %>%
    filter(country=="unknown") %>% 
    select(no) %>% 
    pull()
    
  # Make plot
  p <- ds %>% 
    filter(country!="NORWAY") %>% 
    filter(country!="unknown") %>% 
    slice_max(no,n=max_no_countries) %>% #no_countries
    ggplot(aes(x=no, y=reorder(country, no),label=no)) +
    geom_segment(aes(xend=0,yend=reorder(country, no)),linewidth=0.8, color  = "steelblue") + 
    geom_point(size=3, color="darkgreen") +
    geom_text(size=3, nudge_x = 100) + # 
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
      title = paste("Top ",max_no_countries ," cooperating countries",sep=""), # no_countries
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
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
  return(p)
} # End function

# 4. Shares of oa-categories per year
plot_oa_shares <- function(analysis_data_set,analysis_data_labels,db, pick_colour) {
  
  oa_ordered_levels_seven = (c("diamond","gold", "hybrid","hybrid_deal","green","deposited","closed"))
  oa_color_map_seven <- scale_fill_manual(values=c("lightblue3", "gold","gold3","gold4", "palegreen4","grey50","grey65"))
  
  # Set timespan (n years in dataset). Used to get correct breaks in plot
  n_year_span <- analysis_data_labels$timespan_high-analysis_data_labels$timespan_low
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  
  # Plot of OA counts per year
  p <- analysis_data_set %>% 
    select(cri_id, year_of_publication, calculated_oa_status) %>% 
    distinct() %>% 
    mutate(calculated_oa_status=factor(calculated_oa_status,levels=oa_ordered_levels_seven)) %>%
    group_by(year_of_publication,calculated_oa_status) %>% 
    summarise(n_oa = n_distinct(cri_id)) %>% 
    group_by(year_of_publication) %>% 
    mutate(total=sum(n_oa),
           pct = round(n_oa/total*100,digits=1)) %>% 
    ggplot(aes(x = year_of_publication, y = total, fill = calculated_oa_status, label = paste(pct," %/ n= ",n_oa,sep=""))) +
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
      breaks = seq(analysis_data_labels$timespan_low, analysis_data_labels$timespan_high, 1)
    ) +
    oa_color_map_seven +
    labs(
      title = "Number of publications per OA-category/year",
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
      tag = waiver(),
      x = "Year of publication",
      y= "Percent of publications") 
  
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
  
  return(p)
} # End function

# 5. Mean number of authors in the dataset per NPI-field
plot_no_authors <- function(analysis_data_set,analysis_data_labels,db, pick_colour) {
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Author-count per year and NIB-category (as given by source/journal)
  p <- analysis_data_set %>% 
    filter(!is.na(cri_npi_field)) %>% 
    group_by(cri_npi_field) %>% 
    select(cri_npi_field,cri_id,no_authors) %>% 
    distinct() %>% 
    summarise(n_publ=n_distinct(cri_id),
              avg_no_authors= round(mean(no_authors),digits = 1),
              max_auth=max(no_authors)) %>% 
    arrange(avg_no_authors) %>% 
    # Plot
    ggplot(aes(y=reorder(cri_npi_field, avg_no_authors), x=avg_no_authors, label=avg_no_authors)) +
    geom_segment(aes(xend=0, yend=reorder(cri_npi_field, avg_no_authors)),linewidth=0.7, colour  = "steelblue") +
    geom_point(size=2, color="darkgreen") +
    geom_text(size = 3, position = position_nudge(x = 30)) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_fill_economist() +
    scale_color_economist() +
    scale_x_continuous(labels = scales::label_comma()) + # Could try to use trans = 'log10',
    labs(
      title = "Avg. number of authors per NPI-field" ,
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
      tag = waiver(),
      y = "NPI-field",
      x = "Avg. No. tot. authors.") +
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
  return(p)
} # End function

# 6. Top n most productive authors measured by number of publications
plot_top_authors_count <- function(analysis_data_set,analysis_data_labels,authordetails, max_no_auth,db, pick_colour) {
  
  # Authorlists by number of publications and fractions. Includes only authorshares 
  # affiliated with "NORWAY". A person can have publications that address a foreign institution,
  # These are not counted.
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  
  p <- authordetails %>% 
    filter(country=="NORWAY") %>% 
    select(cri_id,author_full_name,no_authors) %>% 
    distinct() %>% 
    mutate(fraction_val=1/no_authors) %>% 
    group_by(author_full_name) %>% 
    summarise(n_dis=n_distinct(cri_id),
              n=n(),
              fraction_sum=sum(fraction_val)) %>% 
    slice_max(n_dis,n=max_no_auth) %>% 
    ggplot(aes(x=n_dis, y=reorder(author_full_name, n_dis))) +
    geom_segment(aes(xend=0,yend=reorder(author_full_name, n_dis)),linewidth=0.8, color  = "steelblue") + 
    geom_point(size=2, color="darkblue") +
    geom_point(aes(x=fraction_sum),size=2, color="darkgreen") +
    geom_text(aes(label=n_dis),size=3, nudge_x = 5) +
    geom_text(aes(x=fraction_sum,label=round(fraction_sum,digits=1)),size=3, nudge_x = 5) +
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
      title = paste("Top ",max_no_auth," authors by number of publications (Green=sum fractions)", sep=""),
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
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
  return(p)
} # End function

# 7. Top n most productive authors measured by fractional summaries
plot_top_authors_fraction <- function(analysis_data_set,analysis_data_labels,authordetails, max_no_auth,db, pick_colour) {
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Sorted by fraction instead of number of publications
  p <- authordetails %>% 
    filter(country=="NORWAY") %>% 
    select(cri_id,author_full_name,no_authors) %>% 
    distinct() %>% 
    mutate(fraction_val=1/no_authors) %>% 
    group_by(author_full_name) %>%
    summarise(n_dis=n_distinct(cri_id),
              n=n(),
              fraction_sum=sum(fraction_val)) %>% 
    slice_max(fraction_sum,n=max_no_auth) %>% 
    ggplot(aes(x=fraction_sum, y=reorder(author_full_name, fraction_sum))) +
    geom_segment(aes(xend=0,yend=reorder(author_full_name, fraction_sum), color  = "steelblue")) + 
    geom_point(size=2, color="darkgreen") +
    geom_text(aes(label=round(fraction_sum,digits=1)),size=3, nudge_x = 5) +
    geom_point(aes(x=n_dis),size=2, color="darkblue") + # alpha=0.5
    geom_text(aes(x=n_dis,label=round(n_dis,digits=1)),size=3, nudge_x = 5) +
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
      title = paste("Top ",max_no_auth," authors by fractional counting (blue=no. publications)",sep=""),
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
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
  return(p)
} # End function

# 8. Mean number of citations in top n categories
plot_citation_category <- function(analysis_data_set,analysis_data_labels, max_no_categories,db,pick_colour) {
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Geom_point: Citation-distribution per wos-category (limited to input_category) 
  p <- analysis_data_set %>% 
    filter(!is.na(category)) %>% 
    group_by(category) %>% 
    select(category,cri_id, cited_by_count, category_reference_val) %>% 
    distinct() %>% 
    summarise(n_distinct=n_distinct(cri_id),
              n=n(),
              avg_cited_by_count= mean(cited_by_count),
              mean_crv = mean(category_reference_val), # Probably not working
              max_cited=max(cited_by_count)) %>% 
    slice_max(order_by = avg_cited_by_count, n=max_no_categories) %>% 
    ggplot(aes(x=avg_cited_by_count, y=reorder(category, avg_cited_by_count))) +
    geom_segment(aes(xend=0,yend=reorder(category, avg_cited_by_count), colour  = "steelblue")) + 
    geom_point(size=2, color="darkblue") +
    geom_point(aes(x=mean_crv),size=2, color="darkgreen") + # alpha=0.5
    geom_text(aes(label=round(avg_cited_by_count,digits=1)),size=3, nudge_x = 2) +
    geom_text(aes(x=mean_crv,label=round(mean_crv,digits=1)),size=3, nudge_x = -2) +
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
      title = paste("No. citations per category"," (top ",max_no_categories,")", "\n","With field-norm ref-val (green).", sep=""),
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
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
  return(p)
} # End function

# 9. Mean number of citations categories mapped to a single NPI-category
plot_citation_category_box <- function(analysis_data_set,analysis_data_labels, input_category,db,pick_colour) {
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Boxplot-version: Citation-distribution per wos-category (limited to input_category) 

  p <- analysis_data_set %>% 
    filter(!is.na(category)) %>% 
    filter(nib_category==input_category) %>% 
    select(category,cri_id, cited_by_count, category_reference_val) %>% 
    distinct() %>% 
    group_by(category) %>% 
    mutate(mean_cited_by_count = mean(cited_by_count, na.rm = T),
           mean_crv = mean(category_reference_val, na.rm = T)) %>% 
    ggplot(aes(x=cited_by_count, y=reorder(category, mean_cited_by_count))) +
    geom_boxplot(outlier.size = 1, outlier.alpha = 0.5) +
    geom_point(aes(x=mean_cited_by_count),size=1.5, color="darkblue") +
    geom_point(aes(x=mean_crv),size=1.5, color="darkgreen", alpha=0.5) +
    theme_economist(base_size = 3) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.background = element_rect(fill = theme_colour)) +
    scale_fill_economist() +
    scale_color_economist() +
    scale_x_continuous(trans = 'log2') + 
    annotation_logticks(sides="b") + 
    labs(
      title = paste("No. citations per category (NPI: ",input_category,")","\n","With ref-value (green).", sep=""),
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db, "-dataset:", analysis_data_labels$no_publications, sep=""),
      tag = waiver(),
      x = "No. citations (log2-scale)",
      y= "WoS-Category") +
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
  return(p)
} # End function

# 10. Shares of publication that sits  within the 10,25 and 50-percentile
plot_citation_percentiles <- function(analysis_data_set,analysis_data_labels, input_category,db,pick_colour) {
  
  
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Percentile-distribution
  p <- analysis_data_set %>% 
    filter(nib_category==input_category) %>% #"Clinical medicine"
    select(cri_id,category_avg_percentil,category) %>% 
    filter(!is.na(category)) %>% 
    distinct() %>% 
    group_by(category) %>% 
    summarise(n_dis=n_distinct(cri_id),
              n_top10 = n_distinct(cri_id[category_avg_percentil<10]),
              n_top25 = n_distinct(cri_id[category_avg_percentil<25]),
              n_top50 = n_distinct(cri_id[category_avg_percentil<50]),
              share_top10=round(n_top10/n_dis*100,digits=1),
              share_top25=round(n_top25/n_dis*100,digits=1),
              share_top50=round(n_top50/n_dis*100,digits=1)) %>% 
    ggplot(aes(x=share_top10, y=reorder(category, share_top10))) +
    geom_segment(aes(xend=0,yend=reorder(category, share_top10), color  = "steelblue")) + 
    geom_point(size=2, color="darkorange") +
    geom_text(aes(label=share_top10),size=3, nudge_x = 2) +
    geom_point(aes(x=share_top25), size=2, color="darkgreen") +
    geom_text(aes(x=share_top25,label=share_top25), size=3, nudge_x = 2) +
    geom_point(aes(x=share_top50),size=2, color="blue3") +
    geom_text(aes(x=share_top50,label=share_top50), size=3, nudge_x = 2) +
    geom_text(aes(x=-6,label=paste(" (n=",n_dis,")", sep="")), size=3, nudge_x = 2) +
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
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
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
  return(p)
} # End function

# 11. Summary of citation-impact per oa-category
plot_oa_shares_citation <- function(analysis_data_set,analysis_data_labels,db,pick_colour) {
  # Set bg-color to green if chosen, else set to blue
  theme_colour <- "#CFDEEC"
  if (pick_colour=="green") theme_colour <- "#CFECDE"
  
  # Set palett for OA-categories
  oa_ordered_levels_seven = (c("diamond","gold", "hybrid","hybrid_deal","green","deposited","closed"))
  oa_color_map_seven <- scale_fill_manual(values=c("lightblue3", "gold","gold3","gold4", "palegreen4","grey50","grey65"))
  
  # Create mean-values for comparison across OA-categories
avg_lines <- analysis_data_set %>% 
  filter(!is.na(category)) %>%
  select(cri_id, calculated_oa_status,cited_by_count, nib_category, category) %>% 
  distinct() %>% 
  mutate(calculated_oa_status=factor(calculated_oa_status,levels=oa_ordered_levels_seven)) %>%
  group_by(nib_category) %>% 
  summarise(mean_cat_cited=round(mean(cited_by_count,digits=1)))

# A bit simple and coarse grouping.
 p <- analysis_data_set %>% 
    filter(!is.na(nib_category)) %>%
    select(cri_id, calculated_oa_status,cited_by_count, nib_category) %>% 
    distinct() %>% 
    mutate(calculated_oa_status=factor(calculated_oa_status,levels=oa_ordered_levels_seven)) %>%
    group_by(nib_category,calculated_oa_status) %>% 
    summarise(n_dis = n_distinct(cri_id),
              mean_oa_cat_cited=round(mean(cited_by_count,digits=1))) %>% 
    ggplot(aes(x = calculated_oa_status, y = mean_oa_cat_cited,fill=calculated_oa_status, 
               label = paste("cit=",mean_oa_cat_cited,"/n= ",n_dis,sep=""))) + #\n
    geom_col() +
    geom_text(size=2,vjust=0.5) +
   geom_hline(aes(yintercept = mean_cat_cited), data = avg_lines, color = "darkred", linewidth=0.3, linetype="dashed") +
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
      title = "Mean number of citations per OA-type/NPI-category",
      subtitle = paste(analysis_data_labels$institution," (",analysis_data_labels$timespan_low,"-",analysis_data_labels$timespan_high,")",sep=""),
      caption = paste("Total no. publications in ",db,"-dataset:", analysis_data_labels$no_publications, sep=""),
      tag = waiver(),
      x = "OA-type",
      y= "number of citatations") +
   labs(fill="OA-type") +
   facet_wrap(~nib_category, scales= "free_y",ncol=2)
  
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
  return(p)
} # End function
