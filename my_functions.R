

# generate_scope_plots -----------------------------------------------------


#' Title : generate_scope_plots
#'
#' @param data_path 
#' @param shapefiles_path 
#' @param scope_csv_path 
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
generate_scope_plots <- function(data_path, shapefiles_path, scope_csv_path, x, y) {
  setwd(shapefiles_path)
  
  if(!require('gganimate')) {
    install.packages('gganimate')
    library('gganimate')
  }
  if(!require('ggplot2')) {
    install.packages('ggplot2')
    library('ggplot2')
  }
  if(!require('dplyr')) {
    install.packages('dplyr')
    library('dplyr')
  }
  if(!require('sf')) {
    install.packages('sf')
    library('sf')
  }
  
  # Load necessary libraries
  library(tidyverse)
  library(lubridate)
  library(patchwork)
  library(writexl)
  
  # Define country lists
  block <- pays <- c("ALGERIA", "BURKINA FASO", "COTE D IVOIRE", "MAURITANIA", "MALI", "GUINEA", "GHANA", "TOGO", "BENIN", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", "THE GAMBIA", "SENEGAL", "NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO", "CONGO", "GABON", "EQUATORIAL GUINEA", "RWANDA", "BURUNDI", "ANGOLA", "KENYA", "ERITREA", "ETHIOPIA", "SOUTH SUDAN", "UGANDA", "TANZANIA", "MALAWI", "ZAMBIA", "MOZAMBIQUE", "ZIMBABWE", "BOTSWANA", "NAMIBIA", "ESWATINI (KINGDOM)", "LESOTHO(KINGDOM", "SOUTH AFRICA", "MADAGASCAR", "COMOROS", "SEYCHELLES", "MAURITIUS", "CAPE VERDE", "SAO TOME AND PRINCIPE", "UNITED REPUBLIC OF TANZANIA")
  
  # Load shapefiles
  all_countries <- read_rds(file.path(data_path, "global.ctry.rds"))
  all_provinces <- read_rds(file.path(data_path, "global.prov.rds"))
  all_districts <- read_rds(file.path(data_path, "global.dist.rds"))
  
  AFRO_layer <- all_countries |> 
    filter(WHO_REGION == "AFRO")
  Africa <- all_countries |> 
    filter(WORLD_CONTINENTS == "AFRICA")
  
  country_layer <- all_countries |>
    filter(ADM0_NAME %in% block)
  province_layer <- all_provinces |>
    filter(ADM0_NAME %in% block)
  district_layer <- all_districts |>
    filter(ADM0_NAME %in% block)
  
  data <- read_csv(scope_csv_path)
  
  # Get the maximum month label
  max_month_label <- max(data$round_start_date)
  
  # Add floor_date column
  data <- data |> mutate(floor_date = floor_date(round_start_date, "month"))
  
  # Calculate the last six months
  last_six_months <- seq(max(data$floor_date), by = "-1 month", length.out = x)
  data <- data |> 
    filter(floor_date %in% last_six_months)
  
  year <- year(last_six_months)
  
  # Map the scope
  plots_list <- list()
  
  # Dynamically set the vector to take the unique values of the month column
  unique_months <- unique(data$month)
  
  for (selected_month_yr in unique_months) {
    A <- data |>
      filter(month == selected_month_yr) |> 
      filter(country %in% pays) |>
      dplyr::select(country, province, district, vaccine.type)
    
    # Join the spatial layer of provinces
    LQAS_performance2 <- left_join(district_layer, A, relationship = "many-to-many",  
                                   by = c("ADM0_NAME" = "country",
                                          "ADM2_NAME" = "district")) |> 
      filter(vaccine.type != "NA")
    
    # Plot the result of the analysis
    last_6 <- ggplot() + 
      geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
      geom_sf(data = province_layer, color = NA, fill = "white") +
      geom_sf(data = LQAS_performance2, aes(fill = vaccine.type), color = NA) +
      geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
      scale_fill_manual(values = c("nOPV2" = "cornflowerblue", "bOPV" = "yellow3")) +
      theme_void() +
      labs(fill = paste0("Vaccine Type"), 
           title = paste("Scope in", month.abb[month(selected_month_yr)], year)) +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5))
    
    plots_list[[selected_month_yr]] <- last_6
  }
  
  # Update the title of the last plot
  plots_list[[length(plots_list)]] <- plots_list[[length(plots_list)]] +
    ggtitle(paste("Scope as of", max_month_label))
  
  # Filter out any NULL or invalid entries
  valid_plots_list <- Filter(function(x) inherits(x, "ggplot"), plots_list)
  
  # Combine the plots
  multiplot <- wrap_plots(valid_plots_list, ncol = y)
  
  # Add source and production date at the bottom of the last plot
  multiplot_with_caption <- multiplot + labs(caption = paste("Source: PEP SIA calendar \n© WHO AFRO, Production date:", Sys.Date()))
  
  # Display the plots caption = "Source: UNHCR Refugee Data Finder\n© UNHCR, The UN Refugee Agency"
  return(multiplot_with_caption)
}

# # Example usage
# data_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/"
# shapefiles_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/"
# scope_csv_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/SCOPE/scope_.csv"
# x <- 10
# y <- 4

# generate_scope_plots(data_path, shapefiles_path, scope_csv_path, x, y)


# generate lqas plots by district  ----------------------------------------
#' Title
#'
#' @param data_path 
#' @param shapefiles_path 
#'
#' @return
#' @export
#'
#' @examples
generate_lqas_plots_DS <- function(lqas_data_path, LQAS_shapefiles_path, x1, y1) {
  # Load necessary libraries
  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(
    ggplot2, dplyr, sf, lubridate, patchwork, readr
  )
  
  # Ensure valid inputs
  if (x1 <= 0 || y1 <= 0) stop("x1 and y1 must be positive integers.")
  
  # Load shapefiles
  all_countries <- read_rds(file.path(LQAS_shapefiles_path, "global.ctry.rds"))
  all_provinces <- read_rds(file.path(LQAS_shapefiles_path, "global.prov.rds"))
  all_districts <- read_rds(file.path(LQAS_shapefiles_path, "global.dist.rds"))
  
  # Filter shapefiles for the target region
  block <- c(
    "ALGERIA", "BURKINA FASO", "COTE D IVOIRE", "MAURITANIA", "MALI", "GUINEA", 
    "GHANA", "TOGO", "BENIN", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", 
    "THE GAMBIA", "SENEGAL", "NIGERIA", "NIGER", "CAMEROON", "CHAD", 
    "CENTRAL AFRICAN REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO", "CONGO", 
    "GABON", "EQUATORIAL GUINEA", "RWANDA", "BURUNDI", "ANGOLA", "KENYA", 
    "ERITREA", "ETHIOPIA", "SOUTH SUDAN", "UGANDA", "TANZANIA", "MALAWI", 
    "ZAMBIA", "MOZAMBIQUE", "ZIMBABWE", "BOTSWANA", "NAMIBIA", "ESWATINI (KINGDOM)", 
    "LESOTHO(KINGDOM", "SOUTH AFRICA", "MADAGASCAR", "COMOROS", "SEYCHELLES", 
    "MAURITIUS", "CAPE VERDE", "SAO TOME AND PRINCIPE", "UNITED REPUBLIC OF TANZANIA"
  )
  
  AFRO_layer <- all_countries |> 
    filter(WHO_REGION == "AFRO")
  Africa <- all_countries |> 
    filter(WORLD_CONTINENTS == "AFRICA")
  
  country_layer <- all_countries |>
    filter(ADM0_NAME %in% block)
  province_layer <- all_provinces |>
    filter(ADM0_NAME %in% block)
  district_layer <- all_districts |>
    filter(ADM0_NAME %in% block)
  
  # Load data and preprocess
  data <- read_csv(file.path(lqas_data_path, "AFRO_LQAS_data_C.csv"))
  max_month_label <- max(data$start_date, na.rm = TRUE)
  
  data <- data |> 
    mutate(floor_date = floor_date(start_date, "month")) |> 
    filter(floor_date %in% seq(max(floor_date), by = "-1 month", length.out = x1)) |> 
    mutate(month = month(start_date))
  
  unique_months <- sort(unique(data$month), decreasing = TRUE)
  plots_list <- vector("list", length(unique_months))
  
  # Generate plots
  for (i in seq_along(unique_months)) {
    selected_month <- unique_months[i]
    A <- data |> filter(month == selected_month)
    
    district_counts <- A |> count(performance) |> rename(count = n)
    labels_with_counts <- district_counts |> 
      mutate(label = paste0(performance, " (", count, ")")) |> 
      pull(label, name = performance)
    
    LQAS_performance2 <- left_join(district_layer, A, relationship = "many-to-many", by = c("ADM0_NAME" = "country", "ADM2_NAME" = "district")) |> 
      filter(!is.na(performance))
    
    plots_list[[i]] <- ggplot() + 
      geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
      geom_sf(data = province_layer, color = NA, fill = "white") +
      geom_sf(data = LQAS_performance2, aes(fill = performance), color = NA) +
      geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
      scale_fill_manual(values = c("High" = "green4", "Moderate" = "yellow", "Poor" = "red", "Very_poor" = "brown4"), 
                        labels = labels_with_counts) +
      theme_void() +
      labs(
        fill = "LQAS Range", 
        title = paste("LQAS Performance in", month.abb[selected_month], year(max_month_label))
      ) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # Update the title of the first plot
  plots_list[[1]] <- plots_list[[1]] +
    ggtitle(paste("LQAS performance as of", max_month_label))
  # Combine plots
  multiplot <- wrap_plots(plots_list, ncol = y1) + 
    plot_annotation(
      caption = paste("Source: PEP SIA repository \n© WHO AFRO, Production date:", Sys.Date())
    )
  
  # Return the combined plot
  return(multiplot)
}



# # Example usage
# data_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/"
# shapefiles_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/"
# x1 <- 10
# y1 <- 4
# 
# generate_lqas_plots_DS(data_path, shapefiles_path, x, y)



# generate lqas plots by country ------------------------------------------


#' Title
#'
#' @param data_path 
#' @param shapefiles_path 
#'
#' @return
#' @export
#'
#' @examples
generate_lqas_plots_CTRY <- function(LQAS_data_path, LQAS_shapefiles_path, x1, y1) {
  if(!require('gganimate')) {
    install.packages('gganimate')
    library('gganimate')
  }
  if(!require('ggplot2')) {
    install.packages('ggplot2')
    library('ggplot2')
  }
  if(!require('dplyr')) {
    install.packages('dplyr')
    library('dplyr')
  }
  if(!require('sf')) {
    install.packages('sf')
    library('sf')
  }
  
  # Load necessary libraries
  library(pacman)
  p_load(tidyverse, lubridate, sf, ggplot2, patchwork, writexl)
  
  # Define country lists
  block <- pays <- c("ALGERIA", "BURKINA FASO", "COTE D IVOIRE", "MAURITANIA", "MALI", "GUINEA", "GHANA", "TOGO", "BENIN", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", "THE GAMBIA", "SENEGAL", "NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO", "CONGO", "GABON", "EQUATORIAL GUINEA", "RWANDA", "BURUNDI", "ANGOLA", "KENYA", "ERITREA", "ETHIOPIA", "SOUTH SUDAN", "UGANDA", "TANZANIA", "MALAWI", "ZAMBIA", "MOZAMBIQUE", "ZIMBABWE", "BOTSWANA", "NAMIBIA", "ESWATINI (KINGDOM)", "LESOTHO(KINGDOM", "SOUTH AFRICA", "MADAGASCAR", "COMOROS", "SEYCHELLES", "MAURITIUS", "CAPE VERDE", "SAO TOME AND PRINCIPE", "UNITED REPUBLIC OF TANZANIA")
  
  # Load shapefiles
  all_countries <- read_rds(file.path(LQAS_shapefiles_path, "global.ctry.rds"))
  all_provinces <- read_rds(file.path(LQAS_shapefiles_path, "global.prov.rds"))
  all_districts <- read_rds(file.path(LQAS_shapefiles_path, "global.dist.rds"))
  
  AFRO_layer <- all_countries |> 
    filter(WHO_REGION == "AFRO")
  Africa <- all_countries |> 
    filter(WORLD_CONTINENTS == "AFRICA")
  
  country_layer <- all_countries |>
    filter(ADM0_NAME %in% block)
  province_layer <- all_provinces |>
    filter(ADM0_NAME %in% block)
  district_layer <- all_districts |>
    filter(ADM0_NAME %in% block)
  
  data <- read_csv(file.path(LQAS_data_path, "AFRO_LQAS_data_c.csv"))
  
  # Get the maximum month label
  max_month_label <- max(data$start_date)
  
  # Add floor_date column
  data <- data |> mutate(floor_date = floor_date(start_date, "month"))
  
  # Calculate the last six months
  last_six_months <- seq(max(data$floor_date), by = "-1 month", length.out = x1)
  data <- data |> 
    filter(floor_date %in% last_six_months) |> 
    mutate(month = month(start_date))
  year <- year(last_six_months)
  
  # Map the scope
  plots_list <- list()
  
  # Dynamically set the vector to take the unique values of the month column
  unique_months <- unique(data$month)
  
  for (selected_month in unique_months) {
    A <- data |>
      filter(month == selected_month) |>
      select(country, province, district, response, start_date, total_missed, roundNumber) |>  
      group_by(country, province, district) |>
      summarise(missed_chil = mean(total_missed), .groups = 'drop') |> 
      mutate(missed_chil = round(missed_chil, 2)) |>
      mutate(performance = case_when(
        missed_chil < 4 ~ "high",
        missed_chil >= 4 & missed_chil < 9 ~ "moderate",
        missed_chil >= 9 & missed_chil < 20 ~ "poor",
        missed_chil >= 20 ~ "very poor"
      )) |>  
      mutate(status = ifelse(performance == "high", 1, 0)) |> 
      group_by(country) |> 
      summarise(percent_pass = mean(status, na.rm = TRUE), .groups = 'drop') |> 
      mutate(percent_pass = round(percent_pass * 100, 2)) |>
      mutate(performance = case_when(
        percent_pass < 50 ~ "<50",
        percent_pass >= 50 & percent_pass < 90 ~ "[50-90[",
        percent_pass >= 90 ~ ">=90"
      )) |> 
      filter(country %in% pays)
    
    # Join the spatial layer of provinces
    LQAS_status3 <- left_join(AFRO_layer, A, by = c("ADM0_NAME" = "country")) |> 
      filter(!is.na(performance))
    
    # Create the plot
    plot_LQAS_CTRY <- ggplot() + 
      geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
      geom_sf(data = AFRO_layer, linewidth = 0.5, color = "black", fill = "white") +
      geom_sf(data = LQAS_status3, aes(fill = performance), color = NA) +
      scale_fill_manual(values = c(">=90" = "green4", "[50-90[" = "yellow", "<50" = "red")) +
      geom_sf(data = AFRO_layer, linewidth = 0.5, color = "black", fill = NA) +
      theme_void() +
      labs(fill = "Performance", 
           title = paste("LQAS performance in", month.abb[selected_month], year[which(month(last_six_months) == selected_month)])) +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5))
    
    plots_list[[as.character(selected_month)]] <- plot_LQAS_CTRY
  }
  
  # Update the title of the last plot
  plots_list[[length(plots_list)]] <- plots_list[[length(plots_list)]] +
    ggtitle(paste("LQAS performance as of", max_month_label))
  
  # Filter out any NULL or invalid entries
  valid_plots_list <- Filter(function(x) inherits(x, "ggplot"), plots_list)
  
  # Combine the plots
  multiplot <- wrap_plots(valid_plots_list, ncol = y1)
  
  # Add source and production date at the bottom of the last plot
  multiplot_with_caption <- multiplot + labs(caption = paste("Source: PEP SIA repository \n© WHO AFRO, Production date:", Sys.Date()))
  
  # Display the plots
  multiplot_with_caption
}

# # Example usage
# data_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/"
# shapefiles_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/"
# x <- 10
# y <- 4
# 
# generate_lqas_plots_CTRY(data_path, shapefiles_path, x, y)




# generate im coverage plot by district -----------------------------------


#' Title
#'
#' @param data_path 
#' @param shapefiles_path 
#'
#' @return
#' @export
#'
#' @examples
generate_im_coverage_plots_DS <- function(IM_data_path, IM_shapefiles_path, a, b) {
  setwd(IM_data_path)
  
  if(!require('gganimate')) {
    install.packages('gganimate')
    library('gganimate')
  }
  if(!require('ggplot2')) {
    install.packages('ggplot2')
    library('ggplot2')
  }
  if(!require('dplyr')) {
    install.packages('dplyr')
    library('dplyr')
  }
  if(!require('sf')) {
    install.packages('sf')
    library('sf')
  }
  
  # Load necessary libraries
  library(pacman)
  p_load(tidyverse, lubridate, ggplot2, patchwork, writexl)
  
  # Define country lists
  block <- pays <- c("ALGERIA", "BURKINA FASO", "COTE D IVOIRE", "MAURITANIA", "MALI", "GUINEA", "GHANA", "TOGO", "BENIN", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", "THE GAMBIA", "SENEGAL", "NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO", "CONGO", "GABON", "EQUATORIAL GUINEA", "RWANDA", "BURUNDI", "ANGOLA", "KENYA", "ERITREA", "ETHIOPIA", "SOUTH SUDAN", "UGANDA", "TANZANIA", "MALAWI", "ZAMBIA", "MOZAMBIQUE", "ZIMBABWE", "BOTSWANA", "NAMIBIA", "ESWATINI (KINGDOM)", "LESOTHO(KINGDOM", "SOUTH AFRICA", "MADAGASCAR", "COMOROS", "SEYCHELLES", "MAURITIUS", "CAPE VERDE", "SAO TOME AND PRINCIPE", "UNITED REPUBLIC OF TANZANIA")
  
  # Load shapefiles
  all_countries <- read_rds(file.path(IM_shapefiles_path, "global.ctry.rds"))
  all_provinces <- read_rds(file.path(IM_shapefiles_path, "global.prov.rds"))
  all_districts <- read_rds(file.path(IM_shapefiles_path, "global.dist.rds"))
  
  AFRO_layer <- all_countries |> 
    filter(WHO_REGION == "AFRO")
  Africa <- all_countries |> 
    filter(WORLD_CONTINENTS == "AFRICA")
  
  country_layer <- all_countries |>
    filter(ADM0_NAME %in% block)
  province_layer <- all_provinces |>
    filter(ADM0_NAME %in% block)
  district_layer <- all_districts |>
    filter(ADM0_NAME %in% block)
  
  data <- read_csv(file.path(IM_data_path, "AFRO_Inside_HH_M.csv"))
  # a <- 6
  # b <- 3
  # Get the maximum month label
  max_month_label <- max(data$start_date)
  year <- year(data$start_date)
  
  # Add floor_date column
  data <- data |> mutate(floor_date = floor_date(start_date, "month"))
  
  # Calculate the last six months
  last_six_months <- seq(max(data$floor_date), by = "-1 month", length.out = a)
  data <- data |> 
    filter(floor_date %in% last_six_months) |> 
    mutate(month = month(start_date))
  year <- year(last_six_months)
  # map the scope
  plots_list <- list()
  
  # Dynamically set the vector to take the unique values of the month column
  unique_months <- unique(data$month)
  
  for (selected_month_yr in unique_months) {
    A <- data |>
      filter(month == selected_month_yr) |>
      mutate(
        coverage = case_when(
          cv >= 0.95 ~ "good",
          cv >=0.8 & cv <0.95 ~ "moderate",
          cv <0.8 ~ "poor")) |> 
      filter(coverage != "NA") |> 
      filter(country %in% pays) |>
      dplyr::select(country, province, district, response, roundNumber, coverage)
    
    
    # Join the spatial layer of provinces
    LQAS_performance2 <- left_join(district_layer, A, relationship = "many-to-many",  
                                   by = c("ADM0_NAME" = "country", "ADM2_NAME" = "district")) %>%
      filter(!is.na(coverage))
    
    # Create the animated plot
    last_6 <- ggplot() + 
      geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
      geom_sf(data = province_layer, color = NA, fill = "white") +
      geom_sf(data = LQAS_performance2, aes(fill = coverage), color = NA) +
      geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
      scale_fill_manual(values = c("good" = "green4", "moderate" = "yellow", "poor" = "red"))+
      theme_void() +
      labs(fill = paste0("IM range"), 
           title = paste("IM coverage in", month(selected_month_yr, label = TRUE), year)) +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5))
    
    plots_list[[selected_month_yr]] <- last_6
  }
  
  # Update the title of the last plot
  plots_list[[length(plots_list)]] <- plots_list[[length(plots_list)]] +
    ggtitle(paste("IM coverage as of", max_month_label))
  
  # Filter out any NULL or invalid entries
  valid_plots_list <- Filter(function(x) inherits(x, "ggplot"), plots_list)
  
  # Combine the plots
  multiplot <- wrap_plots(valid_plots_list, ncol = b)
  
  # Add source and production date at the bottom of the last plot
  multiplot_with_caption <- multiplot + labs(caption = paste("Source: PEP SIA repository \n© WHO AFRO, Production date:", Sys.Date()))
  
  # Display the plots
  print(multiplot_with_caption)
  
  # Save the plot if output_path is provided
  if (!is.null(output_path)) {
    ggsave(file.path(output_path, "choropleth_map_of_cantons.png"), plot = multiplot_with_caption, width = 10, height = 8)
  }
}



# data_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/"
# shapefiles_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/"
# a <- 10
# b <- 4
# # Example usage:
# generate_im_coverage_plots_DS(data_path,shapefiles_path, a, b)


# generate im coverage plot by coutnry ------------------------------------


#' Title
#'
#' @param data_path 
#' @param shapefiles_path 
#'
#' @return
#' @export
#'
#' @examples
generate_im_performance_plots_CTRY <- function(IM_data_path, IM_shapefiles_path, a, b) {
  if(!require('gganimate')) {
    install.packages('gganimate')
    library('gganimate')
  }
  if(!require('ggplot2')) {
    install.packages('ggplot2')
    library('ggplot2')
  }
  if(!require('dplyr')) {
    install.packages('dplyr')
    library('dplyr')
  }
  if(!require('sf')) {
    install.packages('sf')
    library('sf')
  }
  
  # Load necessary libraries
  library(pacman)
  p_load(tidyverse, lubridate, sf, ggplot2, patchwork, writexl)
  
  # Define country lists
  block <- pays <- c("ALGERIA", "BURKINA FASO", "COTE D IVOIRE", "MAURITANIA", "MALI", "GUINEA", "GHANA", "TOGO", "BENIN", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", "THE GAMBIA", "SENEGAL", "NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO", "CONGO", "GABON", "EQUATORIAL GUINEA", "RWANDA", "BURUNDI", "ANGOLA", "KENYA", "ERITREA", "ETHIOPIA", "SOUTH SUDAN", "UGANDA", "TANZANIA", "MALAWI", "ZAMBIA", "MOZAMBIQUE", "ZIMBABWE", "BOTSWANA", "NAMIBIA", "ESWATINI (KINGDOM)", "LESOTHO(KINGDOM", "SOUTH AFRICA", "MADAGASCAR", "COMOROS", "SEYCHELLES", "MAURITIUS", "CAPE VERDE", "SAO TOME AND PRINCIPE", "UNITED REPUBLIC OF TANZANIA")
  
  # Load shapefiles
  all_countries <- read_rds(file.path(IM_shapefiles_path, "global.ctry.rds"))
  all_provinces <- read_rds(file.path(IM_shapefiles_path, "global.prov.rds"))
  all_districts <- read_rds(file.path(IM_shapefiles_path, "global.dist.rds"))
  
  AFRO_layer <- all_countries |> 
    filter(WHO_REGION == "AFRO")
  Africa <- all_countries |> 
    filter(WORLD_CONTINENTS == "AFRICA")
  
  country_layer <- all_countries |>
    filter(ADM0_NAME %in% block)
  province_layer <- all_provinces |>
    filter(ADM0_NAME %in% block)
  district_layer <- all_districts |>
    filter(ADM0_NAME %in% block)
  
  data <- read_csv(file.path(IM_data_path, "AFRO_Inside_HH_M.csv"))
  
  # Get the maximum month label
  max_month_label <- max(data$start_date)
  
  # Add floor_date column
  data <- data |> mutate(floor_date = floor_date(start_date, "month"))
  
  # Calculate the last six months
  last_six_months <- seq(max(data$floor_date), by = "-1 month", length.out = a)
  data <- data |> 
    filter(floor_date %in% last_six_months) |> 
    mutate(month = month(start_date), year = year(start_date))
  
  unique_months <- unique(data$month)
  plots_list <- list()
  
  for (selected_month in unique_months) {
    selected_year <- unique(data[data$month == selected_month, "year"])[1]
    
    A <- data %>%
      filter(month == selected_month) %>%
      group_by(country, province, district, response, roundNumber) %>% 
      summarise(cv = mean(cv)) %>% 
      mutate(coverage = case_when(
        cv >= 0.95 ~ "good",
        cv >= 0.8 & cv < 0.95 ~ "moderate",
        cv < 0.8 ~ "poor")) %>% 
      mutate(status = ifelse(coverage == "good", 1, 0)) %>% 
      group_by(country) %>% 
      summarise(percent_pass = mean(status, na.rm = TRUE)) %>% 
      ungroup() %>%
      select(country, percent_pass) %>%
      mutate(percent_pass = round(percent_pass * 100, 2)) %>%
      mutate(performance = case_when(
        percent_pass < 50 ~ "<50",
        percent_pass >= 50 & percent_pass < 90 ~ "[50-90[",
        percent_pass >= 90 ~ ">=90"))
    
    # Join the spatial layer of provinces
    map1 <- left_join(AFRO_layer, A, by = c("ADM0_NAME" = "country")) %>% 
      filter(!is.na(performance))
    
    # Create the plot
    plot_IM_CTRY <- ggplot() + 
      geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
      geom_sf(data = AFRO_layer, linewidth = 0.5, color = "black", fill = "white") +
      geom_sf(data = map1, aes(fill = performance), color = NA) +
      geom_sf(data = AFRO_layer, linewidth = 0.5, color = "black", fill = NA) +
      scale_fill_manual(values = c(">=90" = "green4", "[50-90[" = "yellow", "<50" = "red")) +
      theme_void() +
      labs(fill = "IM performance", 
           title = paste("IM coverage in", month.abb[selected_month], selected_year)) +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5))
    
    plots_list[[as.character(selected_month)]] <- plot_IM_CTRY
  }
  
  # Update the title of the last plot
  plots_list[[length(plots_list)]] <- plots_list[[length(plots_list)]] +
    ggtitle(paste("IM coverage as of", max_month_label))
  
  # Filter out any NULL or invalid entries
  valid_plots_list <- Filter(function(x) inherits(x, "ggplot"), plots_list)
  
  # Combine the plots
  multiplot <- wrap_plots(valid_plots_list, ncol = b)
  
  # Add source and production date at the bottom of the last plot
  multiplot_with_caption <- multiplot + labs(caption = paste("Source: PEP SIA repository \n© WHO AFRO, Production date:", Sys.Date()))
  
  # Display the plots
  multiplot_with_caption
}


# data_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/"
# shapefiles_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/"
# a <- 10
# b <- 4
# 
# generate_im_performance_plots_CTRY(data_path,shapefiles_path, a1, b1)



# high performance histogram ----------------------------------------------

#' Title
#'
#' @param data_path 
#' @param filter_year 
#' @param filter_months 
#'
#' @return
#' @export
#'
#' @examples
generate_plot <- function(data_path, filter_year, filter_months) {
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  
  # Load the data
  data <- read.csv(data_path)
  data$country[data$country == 'Ethiopia'] <- 'ETH'  # Standardize country name
  
  # Convert start_date to Date type and standardize country names
  data <- data %>%
    mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"),
           year = as.numeric(format(start_date, "%Y")))
  
  # Update start_date based on specific conditions
  data <- data %>% 
    mutate(start_date = case_when(
      country == "ETH" & response == "ETH-2024-08-nOPV2" & roundNumber == "Rnd1" ~ as_date("2024-10-14"),
      country == "ETH" & response == "ETH-2024-03-nOPV2" & roundNumber == "Rnd1" ~ as_date("2024-04-30"),
      country == "NIG" & response == "NIG-2024-10-01_nOPV" & roundNumber == "Rnd1" ~ as_date("2024-10-28"),
      country == "NIE" & roundNumber == "Rnd3" ~ as_date("2024-10-02"),
      country == "NIE" & roundNumber == "Rnd1" ~ as_date("2024-03-06"),
      country == "CHD" & roundNumber == "Rnd3" ~ as_date("2024-05-21"),
      country == "MOZ" & roundNumber == "Rnd2" ~ as_date("2024-08-03"),
      country == "MOZ" & roundNumber == "Rnd1" ~ as_date("2024-06-01"),
      country == "NIG" & roundNumber == "Rnd1" ~ as_date("2024-07-16"),
      country == "NIG" & roundNumber == "Rnd2" ~ as_date("2024-10-01"),
      country == "RCA" & roundNumber == "Rnd2" ~ as_date("2024-04-16"),
      country == "ZIM" & roundNumber == "Rnd1" ~ as_date("2024-02-24"),
      TRUE ~ start_date
    ))
  
  # Filter for the specified year and for the last specified months
  data <- data %>%
    filter(year == filter_year) %>%
    mutate(mois = factor(month.abb[month(start_date)], levels = month.abb)) %>%
    filter(start_date >= (max(start_date) %m-% months(filter_months)))
  
  # Differentiate rounds by including month within the same country
  data <- data %>%
    mutate(roundNumber = paste0(roundNumber, " (", mois, "-", country, ")"))
  
  # Extract month and year, and calculate high performance percentage
  data <- data %>%
    mutate(month = floor_date(start_date, "month")) %>%
    group_by(country, mois, month, roundNumber) %>%
    summarise(high_performance = mean(performance == "High", na.rm = TRUE) * 100, .groups = 'drop') %>%
    arrange(mois)
  
  # Define fill colors function based on roundNumber
  fill_colors <- function(roundNumber) {
    if (grepl("^Rnd1", roundNumber)) {
      return("grey")
    } else if (grepl("^Rnd2", roundNumber)) {
      return("green4")
    } else if (grepl("^Rnd3", roundNumber)) {
      return("cornflowerblue")
    } else {
      return("black")  # Default color if no match (optional)
    }
  }
  
  # Create a named vector for the legend 
  legend_labels <- c("Rnd1" = "Rnd1", "Rnd2" = "Rnd2", "Rnd3" = "Rnd3") 
  legend_colors <- c("Rnd1" = "grey", "Rnd2" = "green4", "Rnd3" = "cornflowerblue")
  
  # Plot histogram with country on x-axis and fill by roundNumber
  histo <- ggplot(data, aes(x = country, y = high_performance, fill = roundNumber)) + 
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) + 
    scale_fill_manual(values = setNames(sapply(unique(data$roundNumber), fill_colors), unique(data$roundNumber)), breaks = names(legend_labels), labels = legend_colors) + 
    geom_hline(yintercept = 90, color = "green", linetype = "dashed", linewidth = 1) + 
    labs(title = "Proportion of High Performance by round and by country Over the Last 11 Months",
         x = "Country",
         y = "High Performance (%)",
         caption = paste("Source: PEP SIA repository \n© WHO AFRO, Production date:", Sys.Date())) +
    theme_minimal()
  
  return(histo)
}

# # Usage
# data_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/AFRO_LQAS_data.csv"
# filter_year <- 2024
# filter_months <- 10
# histo_plot <- generate_plot(data_path, filter_year, filter_months)
# print(histo_plot)


# data_transformation_and_cleaning ----------------------------------------

#' Title
#'
#' @param file_path 
#' @param output_directory 
#'
#' @return
#' @export
#'
#' @examples
read_and_transform_file <- function(file_path, output_directory) {
  # Check and load required packages with pacman
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, lubridate, readxl, tools, tidyr)
  
  # Extract the file extension and read the file
  file_extension <- tools::file_ext(file_path)
  data <- switch(file_extension,
                 csv = read_csv(file_path),
                 xlsx = read_excel(file_path),
                 rds = readRDS(file_path),
                 stop("Unsupported file type"))
  
  # Add Country based on file name
  if (startsWith(basename(file_path), "GHA")) {
    data <- data %>% mutate(Country = "GHA")
  }
  
  # Rename 'district' to 'District' for ZMB
  if ("district" %in% names(data)) {
    data <- data %>% rename(District = district)
  }
  
  # Apply country-specific transformations
  data <- data %>%
    mutate(
      Region = if_else(Country == "BWT", District, Region),
      District = case_when(
        Country == "BWT" ~ District,
        TRUE ~ District
      )
    ) %>%
    drop_na(Country, roundNumber)  # Remove rows with NA in key columns
  
  # Dynamically select relevant columns
  selected_columns <- c(
    "Response", "roundNumber", "Country", "Region", "District", "Date_of_LQAS", "_GPS_hh_latitude", "_GPS_hh_longitude", "_GPS_hh_altitude",
    grep("^Count_HH\\[\\d+\\]/(Sex_Child|FM_Child|Reason_Not_FM|Reason_NC_NFM|Reason_ABS_NFM|Care_Giver_Informed_SIA)$",
         names(data), value = TRUE),
    "Count_HH_count", "Cluster"
  )
  selected_columns <- intersect(selected_columns, names(data))  # Ensure selected columns exist in data
  data <- data %>% select(all_of(selected_columns))
  data$Cluster[!is.na(data$Cluster)] <- 1
  
  # Standardize and recode columns
  standardize_yes_no <- function(x) {
    case_when(
      x %in% c("Yes", "YES", "yes", "Y") ~ 1,
      x %in% c("No", "NO", "no", "N") ~ 0,
      TRUE ~ NA_real_
    )
  }
  standardize_informed_sia <- function(x) {
    case_when(
      x %in% c("Y", "1") ~ 1,
      x %in% c("N", "0") ~ 0,
      TRUE ~ NA_real_
    )
  }
  
  data <- data %>%
    mutate(Country = toupper(Country),
           Region = toupper(Region),
           District = toupper(District)) |> 
    mutate(
      across(matches("^Count_HH\\[\\d+\\]/Sex_Child$"),
             ~ ifelse(. == "F", 1, ifelse(. == "M", 0, .))),
      across(matches("^Count_HH\\[\\d+\\]/FM_Child$"), standardize_yes_no),
      across(matches("^Count_HH\\[\\d+\\]/Care_Giver_Informed_SIA$"), standardize_informed_sia)
    )
  
  # Define reasons to check
  reasons <- c("House_not_visited", "childabsent", "Vaccinated_but_not_FM", 
               "Non_Compliance", "Child_was_asleep", "Child_is_a_visitor")
  
  # Reason for not vaccinated
  AC <- data %>%
    mutate(
      across(
        matches("^Count_HH\\[\\d+\\]/Reason_Not_FM$"), 
        .fns = list(
          House_not_visited = ~ as.numeric(. == "House_not_visited"),
          childabsent = ~ as.numeric(. == "childabsent"),
          Vaccinated_but_not_FM = ~ as.numeric(. == "Vaccinated_but_not_FM"),
          Non_Compliance = ~ as.numeric(. == "Non_Compliance"),
          Child_was_asleep = ~ as.numeric(. == "Child_was_asleep"),
          Child_is_a_visitor = ~ as.numeric(. == "Child_is_a_visitor")
        ),
        .names = "R_{.fn}_{col}"
      )
    )
  
  # Convert all "Count_" columns to numeric
  
  
  AC <- AC %>%
    mutate(across(
      starts_with("Count_"),
      ~ as.numeric(replace(., . %in% c(".", "NA", ""), NA))
    ))
  
  
  # Compute metrics (e.g., female_sampled, total_vaccinated, missed_child)
  AF <- AC %>%
    relocate(all_of(paste0("Count_HH[", 1:10, "]/Sex_Child")), .after = "_GPS_hh_altitude") %>%
    relocate(all_of(paste0("Count_HH[", 1:10, "]/FM_Child")), .after = paste0("Count_HH[10]/Sex_Child")) %>%
    mutate(
      female_sampled = rowSums(across(all_of(paste0("Count_HH[", 1:10, "]/Sex_Child"))), na.rm = TRUE),
      male_sampled = Count_HH_count - female_sampled,
      total_vaccinated = rowSums(across(all_of(paste0("Count_HH[", 1:10, "]/FM_Child"))), na.rm = TRUE),
      missed_child = Count_HH_count - total_vaccinated
    )
  
  
  # mal vaccinated
  AG <- AF %>%
    mutate(
      # Dynamically compute FV1 to FV10
      across(
        .cols = paste0("Count_HH[", 1:10, "]/Sex_Child"),
        .fns = ~ ifelse(
          . + get(str_replace(cur_column(), "Sex_Child", "FM_Child")) >= 2,
          1,
          0
        ),
        # Dynamically name new FV columns
        .names = "FV{gsub('[^0-9]', '', .col)}"
      ),
      # Calculate female_vaccinated as the sum of FV columns
      female_vaccinated = rowSums(across(starts_with("FV"))),
      # Calculate male_vaccinated
      male_vaccinated = total_vaccinated - female_vaccinated
    )
  
  library(dplyr)
  
  # Ensure all columns starting with "R_" are numeric
  AG <- AG %>%
    mutate(across(starts_with("R_"), as.numeric))
  
  # Compute the required summaries
  AS <- AG %>%
    mutate(
      R_House_not_visited = rowSums(across(matches("^R_House_not_visited_Count_HH"), ~ replace_na(., 0))),
      R_Vaccinated_but_not_FM = rowSums(across(matches("^R_Vaccinated_but_not_FM_Count_HH"), ~ replace_na(., 0))),
      R_Non_Compliance = rowSums(across(matches("^R_Non_Compliance_Count_HH"), ~ replace_na(., 0))),
      R_Child_was_asleep = rowSums(across(matches("^R_Child_was_asleep_Count_HH"), ~ replace_na(., 0))),
      R_Child_is_a_visitor = rowSums(across(matches("^R_Child_is_a_visitor_Count_HH"), ~ replace_na(., 0))),
      R_childabsent = rowSums(across(matches("^R_childabsent_Count_HH"), ~ replace_na(., 0))),
      Care_Giver_Informed_SIA = rowSums(across(matches("^Count_HH\\[\\d+\\]/Care_Giver_Informed_SIA$"), ~ replace_na(., 0))))
  
  AQ <- AS %>%
    select(
      Country,
      Region,
      District,
      Response,
      roundNumber,
      Date_of_LQAS,
      `_GPS_hh_latitude`,
      `_GPS_hh_longitude`,
      `_GPS_hh_altitude`,
      male_sampled,
      female_sampled,
      total_sampled = `Count_HH_count`,
      male_vaccinated,
      female_vaccinated,
      total_vaccinated,
      missed_child,
      R_Non_Compliance,
      R_House_not_visited,
      R_childabsent,
      R_Child_was_asleep,
      R_Child_is_a_visitor,
      R_Vaccinated_but_not_FM,
      Care_Giver_Informed_SIA,
      Cluster
    )
  
  # Convert `AF` to spatial format using GPS coordinates
  library(sf)
  AQ <- AQ %>%
    filter(!is.na(`_GPS_hh_latitude`) & !is.na(`_GPS_hh_longitude`))
  
  AQ <- AQ %>%
    mutate(
      `_GPS_hh_latitude` = as.numeric(`_GPS_hh_latitude`),
      `_GPS_hh_longitude` = as.numeric(`_GPS_hh_longitude`)
    )
  AQ_sf <- AQ %>%
    st_as_sf(
      coords = c("_GPS_hh_longitude", "_GPS_hh_latitude"),  # Longitude first, then latitude
      crs = 4326  # EPSG:4326 for WGS84 (latitude/longitude)
    )
  
  library(dplyr)
  library(sf)
  library(lubridate)
  
  # Ensure AQ is an sf object
  if (!inherits(AQ, "sf")) {
    AQ <- AQ %>%
      st_as_sf(coords = c("_GPS_hh_longitude", "_GPS_hh_latitude"), crs = 4326)
  }
  
  # Summarize with geometry handling
  F1 <- AQ %>%
    mutate(Date_of_LQAS = as_date(Date_of_LQAS)) %>%
    group_by(Country, Region, District, Response, roundNumber) %>%
    arrange(Date_of_LQAS) %>%
    mutate(
      date.diff = c(1, diff(Date_of_LQAS)),
      period = cumsum(date.diff != 1)
    ) %>%
    ungroup() %>%
    group_by(Country, Region, District, Response, roundNumber) %>%
    summarise(
      start_date = min(Date_of_LQAS, na.rm = TRUE),
      end_date = max(Date_of_LQAS, na.rm = TRUE),
      cluster = sum(Cluster, na.rm = TRUE),
      male_sampled = sum(male_sampled, na.rm = TRUE),
      female_sampled = sum(female_sampled, na.rm = TRUE),
      total_sampled = sum(total_sampled, na.rm = TRUE),
      male_vaccinated = sum(male_vaccinated, na.rm = TRUE),
      female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
      total_vaccinated = sum(total_vaccinated, na.rm = TRUE),
      missed_child = sum(missed_child, na.rm = TRUE),
      r_Non_Compliance = sum(R_Non_Compliance, na.rm = TRUE),
      r_House_not_visited = sum(R_House_not_visited, na.rm = TRUE),
      r_childabsent = sum(R_childabsent, na.rm = TRUE),
      r_Child_was_asleep = sum(R_Child_was_asleep, na.rm = TRUE),
      r_Child_is_a_visitor = sum(R_Child_is_a_visitor, na.rm = TRUE),
      r_Vaccinated_but_not_FM = sum(R_Vaccinated_but_not_FM, na.rm = TRUE),
      Care_Giver_Informed_SIA = sum(Care_Giver_Informed_SIA, na.rm = TRUE),
      percent_care_Giver_Informed_SIA = Care_Giver_Informed_SIA / total_sampled,
      geometry = st_union(geometry)  # Combine geometries
    )
  
  F2 <- F1 %>%
    filter(start_date > as.Date("2019-10-01")) %>%
    mutate(
      percent_care_Giver_Informed_SIA = round(percent_care_Giver_Informed_SIA * 100, 2),
      total_missed = ifelse(total_sampled < 60, (60 - total_sampled) + missed_child, missed_child)
    ) %>%
    filter(cluster >= 3) %>%
    mutate(
      roundNumber = toupper(roundNumber),  # Convert to uppercase
      roundNumber = case_when(
        str_detect(roundNumber, "1") ~ "Rnd1",
        str_detect(roundNumber, "2") ~ "Rnd2",
        str_detect(roundNumber, "3") ~ "Rnd3",
        TRUE ~ roundNumber
      ),
      Status = case_when(
        total_missed <= 3 ~ "Pass",
        total_missed > 3 ~ "Fail"
      ),
      Performance = case_when(
        total_missed < 4 ~ "high",
        total_missed < 9 ~ "moderate",
        total_missed < 20 ~ "poor",
        TRUE ~ "very poor"
      )
    )
  
  
  FI <- F2 %>%
    mutate(
      Vaccine.type = case_when(
        str_detect(Response, "BITTOU|MENAKA-mOPV2|BAMAKO-mOPV2|KANKAN-mOPV|MLI-12DS-01-2021-mOPV2|CONAKRY-mOPV|Ouagadogou|Bangui 1|GOTHEY|YOPOUGON|Golfe|MDG-2023-03-01_bOPV") ~ "mOPV",
        str_detect(Response, "nOPV|VPOn|TSHUAPA|Tanganyika|Liberia|Mauritania|KOUIBLY|Sierra Leone|SEN|CEN|MAL|BEN-26DS-08-2020|BEN-39DS-01-2021|BERTOUA|EBOLOWA|EXNORD|ExtNord2023|ADDIS ABABA|Mekelle|AMANSIE SOUTH|CAF-2020-002|CENBLOCK|CENTRALBLK|CHA-17DS-02-2020|DONOMANGA|GNBnOPV|GOLFE|GOTHEYE|KEN-13DS-02-2021|MopUp2022|SSD-79DS-09-2020|ALG-2023-09-01_nOPV|ALG-2024-01-01_nOPV|nOPV2022|BEN-2023-09-01_nOPV|BFA-2023-05-01_nOPV|BFA-2023-09-01_nOPV|BFA-2024-02-01_nOPV|BITTOU-mOPV2|Ouagadogou-mOPV2|BOT-2023-02-01_nOPV|CAM-2023-05-01_nOPV|CAM-2023-08-01_nOPV|CAM-2024-02-01_nOPV|nOPV2022|nOPV2023|nVPO|nVPO_Maradi|nVPO_Zinder|nVPO2|May2021|OPVb2021|OPVb2022|RSSmOPV10C2021|SEN_VPOn|UGAnOPV|VPOb|VPOb13ProV") ~ "nOPV2",
        str_detect(Response, "bOPV|OPVb|WPV1") ~ "bOPV",
        TRUE ~ NA_character_
      ),
      Response = case_when(
        Response == "CENTRALBLK" ~ "DRC-7DS-02-2022",
        Response == "nOPV2022" ~ "DRC-39DS-01-2021",
        Response %in% c("Tshuapa", "TSHUAPA") ~ "DRC-23DS-12-2020",
        Response == "VPOb13ProV" ~ "DRC-39DS-01-2021",
        TRUE ~ Response
      )
    )
  # Create a lookup table for start_date and end_date NIG-2024-10-01_nOPV
  lookup_table <- tibble::tribble(
    ~Response, ~Vaccine.type, ~roundNumber, ~start_date, ~end_date,
    "NIG-2024-10-01_nOPV", "nOPV2", "Rnd1", "2024-10-29", "2024-10-30",
    "ETH-2024-08-nOPV2", "nOPV2", "Rnd1", "2024-10-14", "2024-10-15",
    "GHA-2024-10-01_nOPV", "nOPV2", "Rnd1", "2024-10-21", "2024-10-22",
    "CAM-2024-10-01_nOPV", "nOPV2", "Rnd1", "2024-10-27", "2024-10-28",
    "DRC-2024-10-01_B_nOPV", "nOPV2", "Rnd1", "2024-10-14", "2024-10-15",
    "DRC-2024-10-01_B_nOPV", "bOPV", "Rnd1", "2024-10-14", "2024-10-15",
    "ALG-2024-01-01_nOPV", "nOPV2", "Rnd1", "2024-01-31", "2024-01-31",
    "Algeria Outbreak", "nOPV2", "Rnd1", "2022-12-07", "2022-12-07",
    "Algeria Outbreak", "nOPV2", "Rnd2", "2023-01-17", "2023-01-17",
    "Algeria Outbreak", "nOPV2", "Rnd3", "2023-03-15", "2023-03-15",
    "ANG-2023-05-01_bOPV", "bOPV", "Rnd1", "2023-09-11", "2023-09-11",
    "ANG-2023-05-01_bOPV", "bOPV", "Rnd2", "2023-10-16", "2023-10-16",
    "ANGOLA-NID-2024-nOPV2", "nOPV2", "Rnd1", "2024-05-20", "2024-05-20",
    "ANGOLA-NID-2024-nOPV2", "nOPV2", "Rnd2", "2024-09-09", "2024-09-09",
    "ANGOLA-SNID-2024-07-nOPV2", "nOPV2", "Rnd1", "2024-07-01", "2024-07-01",
    "BEN-2023-04-01_nOPV", "nOPV2", "Rnd1", "2023-07-24", "2023-07-24",
    "BEN-2023-04-01_nOPV", "nOPV2", "Rnd2", "2023-08-28", "2023-08-28",
    "BEN-2023-09-01_nOPV", "nOPV2", "Rnd1", "2024-02-05", "2024-02-05",
    "BEN-2023-09-01_nOPV", "nOPV2", "Rnd2", "2024-06-10", "2024-06-10",
    "BEN-26DS-08-2020", "nOPV2", "Rnd1", "2020-10-12", "2020-10-12",
    "BEN-26DS-08-2020", "nOPV2", "Rnd2", "2020-10-26", "2020-10-26",
    "BEN-39DS-01-2021", "nOPV2", "Rnd1", "2021-05-10", "2021-05-10",
    "BEN-39DS-01-2021", "nOPV2", "Rnd2", "2021-05-31", "2021-05-31",
    "BEN-39DS-01-2021", "nOPV2", "Rnd3", "2021-09-06", "2021-09-06",
    "BEN-86DS-08-2022", "nOPV2", "Rnd1", "2022-10-17", "2022-10-17",
    "BFA-10DS-05-2020", "mOPV2", "Rnd1", "2020-11-16", "2020-11-16",
    "BFA-10DS-05-2020", "mOPV2", "Rnd2", "2020-12-06", "2020-12-06",
    "BFA-2023-05-01_nOPV", "nOPV2", "Rnd1", "2022-11-28", "2022-11-28",
    "BFA-2023-05-01_nOPV", "nOPV2", "Rnd2", "2023-07-17", "2023-07-17",
    "BFA-2023-09-01_nOPV", "nOPV2", "Rnd0", "2023-09-25", "2023-09-25",
    "BFA-2023-09-01_nOPV", "nOPV2", "Rnd1", "2023-10-30", "2023-10-30",
    "BFA-2023-09-01_nOPV", "nOPV2", "Rnd2", "2023-11-27", "2023-11-27",
    "BFA-2024-02-01_nOPV", "nOPV2", "Rnd1", "2024-05-13", "2024-05-13",
    "BFA-2024-08-sNID_nOPV", "nOPV2", "Rnd1", "2024-09-09", "2024-09-09",
    "BFA-22DS-10-2020", "mOPV2", "Rnd1", "2020-12-06", "2020-12-06",
    "BFA-22DS-10-2020", "mOPV2", "Rnd2", "2020-12-21", "2020-12-21",
    "BOT-2023-02-01_nOPV", "nOPV2", "Rnd1", "2023-02-26", "2023-02-26",
    "BOT-2023-02-01_nOPV", "nOPV2", "Rnd2", "2023-03-30", "2023-03-30",
    "Burkina Faso 2022", "nOPV2", "Rnd1", "2022-11-28", "2022-11-28",
    "Burkina Faso 2022", "nOPV2", "Rnd2", "2022-12-19", "2022-12-19",
    "BUU-2023-04-01_nOPV", "nOPV2", "Rnd1", "2023-06-13", "2023-06-13",
    "BUU-2023-04-01_nOPV", "nOPV2", "Rnd2", "2023-08-20", "2023-08-20",
    "BUU-2023-04-01_nOPV", "nOPV2", "Rnd3", "2023-10-29", "2023-10-29",
    "CAF-2020-002", "mOPV2", "Rnd1", "2020-11-16", "2020-11-16",
    "CAF-2020-002", "mOPV2", "Rnd2", "2020-12-07", "2020-12-07",
    "CAM-2023-05-01_nOPV", "nOPV2", "Rnd1", "2023-05-29", "2023-05-29",
    "CAM-2023-05-01_nOPV", "nOPV2", "Rnd2", "2023-06-26", "2023-06-26",
    "CAM-2023-08-01_nOPV", "nOPV2", "Rnd1", "2023-09-25", "2023-09-25",
    "CAM-2023-08-01_nOPV", "nOPV2", "Rnd2", "2023-11-06", "2023-11-06",
    "CAM-2024-02-01_nOPV", "nOPV2", "Rnd1", "2024-03-04", "2024-03-04",
    "CAR-2023-05-01_nOPV", "nOPV2", "Rnd1", "2023-06-05", "2023-06-05",
    "CAR-2023-05-01_nOPV", "nOPV2", "Rnd2", "2023-09-10", "2023-09-10",
    "CAR-2024-02-01_nOPV", "nOPV2", "Rnd1", "2024-03-09", "2024-03-09",
    "CAR-2024-02-01_nOPV", "nOPV2", "Rnd2", "2024-04-15", "2024-04-15",
    "CAR-2024-sNIDs-09_nOPV", "nOPV2", "Rnd1", "2024-10-09", "2024-10-09",
    "CAR-35DS-02-2022", "nOPV2", "Rnd1", "2022-06-06", "2022-06-06",
    "CAR-35DS-02-2022", "nOPV2", "Rnd2", "2022-08-07", "2022-08-07",
    "CHA-91DS-11-2020", "mOPV2", "Rnd1", "2020-11-16", "2020-11-16",
    "CHA-91DS-11-2020", "mOPV2", "Rnd2", "2020-11-30", "2020-11-30",
    "Chad_2023", "nOPV2", "Rnd1", "2023-01-29", "2023-01-29",
    "CHAD-DS-02-2022", "nOPV2", "Rnd1", "2022-05-16", "2022-05-16",
    "CHAD-DS-02-2022", "nOPV2", "Rnd2", "2022-06-13", "2022-06-13",
    "CHD-2023-05-1_nOPV", "nOPV2", "Rnd1", "2023-05-29", "2023-05-29",
    "CHD-2023-05-1_nOPV", "nOPV2", "Rnd2", "2023-06-19", "2023-06-19",
    "CHD-2023-10-1_nOPV", "nOPV2", "Rnd1", "2023-11-13", "2023-11-13",
    "CHD-2023-10-1_nOPV", "nOPV2", "Rnd2", "2023-12-16", "2023-12-16",
    "CHD-2023-10-1_nOPV", "nOPV2", "Rnd3", "2024-05-20", "2024-05-20",
    "CIV-2023-06-01_nOPV", "nOPV2", "Rnd0", "2023-07-17", "2023-07-17",
    "CIV-2023-06-01_nOPV", "nOPV2", "Rnd1", "2023-09-25", "2023-09-25",
    "CIV-2023-06-01_nOPV", "nOPV2", "Rnd2", "2023-10-30", "2023-10-30",
    "CIV-2024-04-01_nOPV", "nOPV2", "Rnd1", "2024-05-13", "2024-05-13",
    "CIV-2024-04-01_nOPV", "nOPV2", "Rnd2", "2024-09-09", "2024-09-09",
    "CIV-72DS-05-2022", "nOPV2", "Rnd1", "2022-06-20", "2022-06-20",
    "CIV-72DS-05-2022", "nOPV2", "Rnd2", "2022-09-26", "2022-09-26",
    "CMR-16DS-01-2023", "nOPV2", "Rnd0", "2023-01-30", "2023-01-30",
    "CMR-31DS-02-2022", "nOPV2", "Rnd1", "2022-05-16", "2022-05-16",
    "CMR-31DS-02-2022", "nOPV2", "Rnd2", "2022-07-04", "2022-07-04",
    "CMR-31DS-02-2022", "nOPV2", "Rnd3", "2022-11-07", "2022-11-07",
    "CNG-2023-05-bOPV", "bOPV", "Rnd1", "2023-06-12", "2023-06-12",
    "CNG-2023-05-bOPV", "bOPV", "Rnd2", "2023-08-28", "2023-08-28",
    "CNG-2023-10-nOPV_2023", "nOPV2", "Rnd1", "2023-12-11", "2023-12-11",
    "CNG-2023-10-nOPV_2023", "nOPV2", "Rnd2", "2024-04-01", "2024-04-01",
    "CNG-2024-08-BOPV", "bOPV", "Rnd1", "2024-09-30", "2024-09-30",
    "CON-52DS-01-2021", "nOPV2", "Rnd1", "2021-05-30", "2021-05-30",
    "CON-52DS-01-2021", "nOPV2", "Rnd2", "2021-08-09", "2021-08-09",
    "DRC-11Prov-03-2022", "nOPV2", "Rnd1", "2022-11-13", "2022-11-13",
    "DRC-2023-03-01_bOPV", "bOPV", "Rnd1", "2023-03-19", "2023-03-19",
    "DRC-2023-03-01_nOPV", "nOPV2", "Rnd1", "2023-04-02", "2023-04-02",
    "DRC-2023-05-01_nOPV", "nOPV2", "Rnd1", "2023-06-04", "2023-06-04",
    "DRC-2023-09-KIN_nOPV", "nOPV2", "Rnd1", "2023-09-25", "2023-09-25",
    "DRC-2023-11-02_nOPV", "nOPV2", "Rnd1", "2023-11-19", "2023-11-19",
    "DRC-2024-10-01_B_nOPV", "bOPV, nOPV2", "Rnd1", "2024-10-13", "2024-10-13",
    "DRC-39DS-01-2021", "mOPV2", "Rnd1", "2021-03-28", "2021-03-28",
    "DRC-39DS-01-2021", "mOPV2", "Rnd2", "2021-04-11", "2021-04-11",
    "DRC-7DS-02-2022", "nOPV2", "Rnd1", "2022-05-29", "2022-05-29",
    "DRC-7DS-02-2022", "nOPV2", "Rnd2", "2022-07-29", "2022-07-29",
    "DRC-NID-03-2024-B_nOPV", "nOPV2", "Rnd1", "2024-03-31", "2024-03-31",
    "DRC-NID-04-2024-bOPV", "bOPV", "Rnd1", "2024-06-16", "2024-06-16",
    "DRC-NID-07-2023-bOPV", "bOPV", "Rnd1", "2023-07-30", "2023-07-30",
    "DRC-NID-07-2023-bOPV", "bOPV", "Rnd2", "2023-09-25", "2023-09-25",
    "DRC-sNID-01-2024_bOPV", "bOPV", "Rnd1", "2024-02-05", "2024-02-05",
    "DRC-sNID-08-2024-B_nOPV", "bOPV, nOPV2", "Rnd1", "2024-08-11", "2024-08-11",
    "DRC-sNID-09-2024_bOPV", "bOPV, nOPV2", "Rnd1", "2024-09-22", "2024-09-22",
    "ETH-2021-002-1", "nOPV2", "Rnd1", "2021-11-01", "2021-11-01",
    "ETH-2021-002-2", "nOPV2", "Rnd1", "2021-11-01", "2021-11-01",
    "ETH-2021-002-3", "nOPV2", "Rnd1", "2021-04-04", "2021-04-04",
    "ETH-2021-002-4", "nOPV2", "Rnd1", "2021-11-15", "2021-11-15",
    "ETH-2024-03-nOPV2", "nOPV2", "Rnd1", "2024-04-29", "2024-04-29",
    "ETH-2024-03-nOPV2", "nOPV2", "Rnd2", "2024-06-17", "2024-06-17",
    "ETH-2024-08-nOPV2", "nOPV2", "Rnd1", "2024-10-13", "2024-10-13",
    "GAM-65DS-06-2021", "nOPV2", "Rnd1", "2021-11-23", "2021-11-23",
    "GAM-65DS-06-2021", "nOPV2", "Rnd2", "2022-03-22", "2022-03-22",
    "GHA-2024-10-01_nOPV", "nOPV2", "Rnd1", "2024-10-20", "2024-10-20",
    "GHN-260-DS-07-2022", "nOPV2", "Rnd1", "2022-09-04", "2022-09-04",
    "GHN-260-DS-07-2022", "nOPV2", "Rnd2", "2022-10-09", "2022-10-09",
    "GHN-260-DS-07-2022", "nOPV2", "Rnd3", "2022-12-18", "2022-12-18",
    "GNB-117DS-02-2022", "nOPV2", "Rnd1", "2022-04-30", "2022-04-30",
    "GNB-117DS-02-2022", "nOPV2", "Rnd2", "2022-06-25", "2022-06-25",
    "GUE-20DS-02-2021", "mOPV2", "Rnd1", "2021-03-01", "2021-03-01",
    "GUE-20DS-02-2021", "mOPV2", "Rnd2", "2021-06-07", "2021-06-07",
    "GUI-2023-09-01_nOPV", "nOPV2", "Rnd0", "2023-09-11", "2023-09-11",
    "GUI-2023-09-01_nOPV", "nOPV2", "Rnd1", "2023-10-30", "2023-10-30",
    "GUI-2023-09-01_nOPV", "nOPV2", "Rnd2", "2024-05-13", "2024-05-13",
    "GUI-2024-06-LIDs_nOPV", "nOPV2", "Rnd1", "2024-09-09", "2024-09-09",
    "KEN-13DS-02-2021", "mOPV2", "Rnd1", "2021-05-25", "2021-05-25",
    "KEN-13DS-02-2021", "mOPV2", "Rnd2", "2021-07-20", "2021-07-20",
    "KEN-2023-08-01_nOPV", "nOPV2", "Rnd1", "2023-08-27", "2023-08-27",
    "KEN-2023-08-01_nOPV", "nOPV2", "Rnd2", "2023-10-10", "2023-10-10",
    "KEN-2023-08-01_nOPV", "nOPV2", "Rnd3", "2023-11-14", "2023-11-14",
    "KEN-2024-01-01_nOPV", "nOPV2", "Rnd1", "2024-01-30", "2024-01-30",
    "KEN-2024-08-01_nOPV", "nOPV2", "Rnd1", "2024-10-05", "2024-10-05",
    "Kwara Response", "nOPV2", "Rnd1", "2022-07-05", "2022-07-05",
    "Kwara Response", "nOPV2", "Rnd2", "2022-08-02", "2022-08-02",
    "LBR-15DS-10-2020", "nOPV2", "Rnd1", "2021-03-29", "2021-03-29",
    "LBR-15DS-10-2020", "nOPV2", "Rnd2", "2021-05-31", "2021-05-31",
    "LBR-2024-05-01_nOPV", "nOPV2", "Rnd1", "2024-05-13", "2024-05-13",
    "LBR-2024-05-01_nOPV", "nOPV2", "Rnd2", "2024-06-10", "2024-06-10",
    "LBR-2024-05-01_nOPV", "nOPV2", "Rnd3", "2024-09-30", "2024-09-30",
    "Madagascar Nationwide response cVDPV1", "bOPV", "Rnd1", "2022-07-02", "2022-07-02",
    "MAI-2023-05-01_nOPV", "nOPV2", "Rnd1", "2023-06-19", "2023-06-19",
    "MAI-2023-05-01_nOPV", "nOPV2", "Rnd2", "2023-08-21", "2023-08-21",
    "MAI-2023-09-01_nOPV", "nOPV2", "Rnd1", "2023-11-06", "2023-11-06",
    "MAI-2023-09-01_nOPV", "nOPV2", "Rnd2", "2023-12-21", "2023-12-21",
    "MAI-2024-02-01_nOPV", "nOPV2", "Rnd1", "2024-05-13", "2024-05-13",
    "MAI-2024-02-01_nOPV", "nOPV2", "Rnd2", "2024-09-30", "2024-09-30",
    "Malawi-WPV1", "bOPV", "Rnd1", "2022-03-24", "2022-03-24",
    "Malawi-WPV1", "bOPV", "Rnd2", "2022-04-28", "2022-04-28",
    "Malawi-WPV1", "bOPV", "Rnd3", "2022-08-14", "2022-08-14",
    "Malawi-WPV1", "bOPV", "Rnd4", "2022-10-16", "2022-10-16",
    "MAU_57DS_08-2021", "nOPV2", "Rnd1", "2021-12-20", "2021-12-20",
    "MAU_57DS_08-2021", "nOPV2", "Rnd2", "2022-04-01", "2022-04-01",
    "MAU_57DS_08-2021", "nOPV2", "Rnd3", "2022-07-06", "2022-07-06",
    "MAU-2024-01-01_nOPV", "nOPV2", "Rnd1", "2024-03-31", "2024-03-31",
    "MAU-2024-01-01_nOPV", "nOPV2", "Rnd2", "2024-05-26", "2024-05-26",
    "MDG-2023-03-01_bOPV", "bOPV", "Rnd1", "2023-05-19", "2023-05-19",
    "MDG-2023-03-01_bOPV", "bOPV", "Rnd2", "2023-07-28", "2023-07-28",
    "MDG-2023-03-01_bOPV", "bOPV", "Rnd3", "2023-09-08", "2023-09-08",
    "MDG-2023-03-01_bOPV", "bOPV", "Rnd4", "2023-10-19", "2023-10-19",
    "MDG-2024-05-01_bOPV", "bOPV", "Rnd1", "2024-05-17", "2024-05-17",
    "MDG-2024-05-01_bOPV", "bOPV", "Rnd2", "2024-07-12", "2024-07-12",
    "MLI-12DS-01-2021", "mOPV2", "Rnd1", "2021-03-08", "2021-03-08",
    "MLI-12DS-01-2021", "mOPV2", "Rnd2", "2021-03-22", "2021-03-22",
    "MLW-2023-04-1_bOPV", "bOPV", "Rnd1", "2023-05-18", "2023-05-18",
    "MLW-2023-04-1_bOPV", "bOPV", "Rnd2", "2023-07-16", "2023-07-16",
    "MLW-2023-04-1_bOPV", "bOPV", "Rnd3", "2023-09-16", "2023-09-16",
    "MOZ-118-DS-02-2022", "bOPV", "Rnd1", "2022-03-24", "2022-03-24",
    "MOZ-118-DS-02-2022", "bOPV, nOPV2", "Rnd2", "2022-04-24", "2022-04-24",
    "MOZ-118-DS-02-2022", "bOPV", "Rnd3", "2022-07-10", "2022-07-10",
    "MOZ-118-DS-02-2022", "bOPV", "Rnd4", "2022-08-21", "2022-08-21",
    "MOZ-118-DS-02-2022", "bOPV", "Rnd5", "2022-10-16", "2022-10-16",
    "MOZ-118-DS-02-2022", "bOPV, nOPV2", "Rnd6", "2022-12-10", "2022-12-10",
    "MOZ-2023-03-1_nOPV", "nOPV2", "Rnd1", "2023-04-15", "2023-04-15",
    "MOZ-2023-05-01_bOPV", "bOPV", "Rnd1", "2023-06-18", "2023-06-18",
    "MOZ-2023-05-01_bOPV", "bOPV", "Rnd2", "2023-09-18", "2023-09-18",
    "MOZ-2024-04-01_bOPV", "bOPV", "Rnd1", "2024-05-31", "2024-05-31",
    "MOZ-2024-04-01_bOPV", "bOPV", "Rnd2", "2024-08-02", "2024-08-02",
    "NER-14DS-09-2022", "nOPV2", "Rnd1", "2022-11-18", "2022-11-18",
    "NER-14DS-09-2022", "nOPV2", "Rnd2", "2022-12-16", "2022-12-16",
    "NGA-2021-011-1", "nOPV2", "Rnd1", "2021-04-13", "2021-04-13",
    "NGA-2021-011-1", "nOPV2", "Rnd2", "2021-05-25", "2021-05-25",
    "NGA-2021-013-1", "nOPV2", "Rnd1", "2021-03-16", "2021-03-16",
    "NGA-2021-013-1", "nOPV2", "Rnd2", "2021-04-13", "2021-04-13",
    "NGA-2021-014-1", "nOPV2", "Rnd1", "2021-06-29", "2021-06-29",
    "NGA-2021-014-1", "nOPV2", "Rnd2", "2021-08-03", "2021-08-03",
    "NGA-2021-016-1", "nOPV2", "Rnd1", "2021-06-22", "2021-06-22",
    "NGA-2021-016-1", "nOPV2", "Rnd2", "2021-07-20", "2021-07-20",
    "NGA-2021-019", "nOPV2", "Rnd1", "2021-08-10", "2021-08-10",
    "NGA-2021-020-1", "nOPV2", "Rnd1", "2021-09-14", "2021-09-14",
    "NGA-2021-020-1", "nOPV2", "Rnd2", "2021-10-12", "2021-10-12",
    "NGA-2021-020-2", "nOPV2", "Rnd1", "2021-09-07", "2021-09-07",
    "NGA-2021-020-2", "nOPV2", "Rnd2", "2021-10-19", "2021-10-19",
    "NGA-2021-020-3", "nOPV2", "Rnd1", "2021-10-12", "2021-10-12",
    "NGA-2021-020-3", "nOPV2", "Rnd2", "2021-11-02", "2021-11-02",
    "NGA-2021-020-4", "nOPV2", "Rnd1", "2021-09-21", "2021-09-21",
    "NGA-2021-020-4", "nOPV2", "Rnd2", "2021-10-19", "2021-10-19",
    "NIE-2023-04-02_nOPV", "nOPV2", "Rnd1", "2023-05-14", "2023-05-14",
    "NIE-2023-04-02_nOPV", "nOPV2", "Rnd2", "2023-06-20", "2023-06-20",
    "NIE-2023-07-03_nOPV", "nOPV2", "Rnd1", "2023-07-25", "2023-07-25",
    "NIE-2023-07-03_nOPV", "nOPV2", "Rnd2", "2023-10-03", "2023-10-03",
    "NIE-2023-07-03_nOPV", "nOPV2", "Rnd3", "2023-11-07", "2023-11-07",
    "NIE-2023-07-03_nOPV", "nOPV2", "Rnd4", "2023-12-17", "2023-12-17",
    "NIE-2023-10-01_bOPV", "bOPV", "Rnd1", "2023-10-03", "2023-10-03",
    "NIE-2023-10-01_bOPV", "bOPV", "Rnd2", "2023-11-21", "2023-11-21",
    "NIE-2024-nOPV2", "nOPV2", "Rnd1", "2024-03-05", "2024-03-05",
    "NIE-2024-nOPV2", "nOPV2", "Rnd2", "2024-04-23", "2024-04-23",
    "NIE-2024-nOPV2", "nOPV2", "Rnd3", "2024-10-01", "2024-10-01",
    "NIG-13DS-03-2021", "nOPV2", "Rnd1", "2021-06-14", "2021-06-14",
    "NIG-13DS-03-2021", "nOPV2", "Rnd2", "2021-07-14", "2021-07-14",
    "NIG-2023-05-01_nOPV", "nOPV2", "Rnd1", "2023-05-29", "2023-05-29",
    "NIG-2023-05-01_nOPV", "nOPV2", "Rnd2", "2023-06-22", "2023-06-22",
    "NIG-2023-11-01_nOPV", "nOPV2", "Rnd1", "2023-12-08", "2023-12-08",
    "NIG-2023-11-01_nOPV", "nOPV2", "Rnd2", "2024-01-08", "2024-01-08",
    "NIG-2024-06-01_nOPV", "nOPV2", "Rnd1", "2024-07-15", "2024-07-15",
    "NIG-2024-06-01_nOPV", "nOPV2", "Rnd2", "2024-09-30", "2024-09-30",
    "NIG-72DS-02-2022", "nOPV2", "Rnd1", "2022-06-05", "2022-06-05",
    "NIG-72DS-02-2022", "nOPV2", "Rnd2", "2022-07-02", "2022-07-02",
    "NIG-72DS-02-2022", "nOPV2", "Rnd3", "2022-10-31", "2022-10-31",
    "NIG-9DS-06-2021_Zinder", "nOPV2", "Rnd1", "2021-08-30", "2021-08-30",
    "NIG-9DS-06-2021_Zinder", "nOPV2", "Rnd2", "2021-09-20", "2021-09-20",
    "Preventive campaign bOPV - Guinea", "bOPV", "Rnd1", "2021-10-25", "2021-10-25",
    "Preventive campaign bOPV- CAR", "bOPV", "Rnd1", "2021-10-25", "2021-10-25",
    "Preventive campaign bOPV -CHAD", "bOPV", "Rnd1", "2021-11-01", "2021-11-01",
    "Preventive campaign bOPV -CHAD", "bOPV", "Rnd2", "2021-11-22", "2021-11-22",
    "RWA-2023-07-01_nOPV", "nOPV2", "Rnd1", "2023-07-27", "2023-07-27",
    "RWA-2023-07-01_nOPV", "nOPV2", "Rnd2", "2023-09-14", "2023-09-14",
    "SEN-79DS-01-2021", "nOPV2", "Rnd1", "2021-12-20", "2021-12-20",
    "SEN-79DS-01-2021", "nOPV2", "Rnd2", "2022-02-27", "2022-02-27",
    "SEN-79DS-01-2021", "nOPV2", "Rnd3", "2022-08-13", "2022-08-13",
    "SLE-16DS-01-2021", "nOPV2", "Rnd1", "2021-05-31", "2021-05-31",
    "SLE-16DS-01-2021", "nOPV2", "Rnd2", "2021-07-05", "2021-07-05",
    "SLE-16DS-01-2021", "nOPV2", "Rnd3", "2021-08-30", "2021-08-30",
    "SLE-2024-05-01_nOPV", "nOPV2", "Rnd1", "2024-05-13", "2024-05-13",
    "SLE-2024-05-01_nOPV", "nOPV2", "Rnd2", "2024-06-10", "2024-06-10",
    "SLE-2024-05-01_nOPV", "nOPV2", "Rnd3", "2024-09-30", "2024-09-30",
    "SSD-2024-02-01_nOPV2", "nOPV2", "Rnd1", "2024-03-01", "2024-03-01",
    "SSD-2024-02-01_nOPV2", "nOPV2", "Rnd2", "2024-04-19", "2024-04-19",
    "SSD-79DS-09-2020", "nOPV2", "Rnd1", "2020-11-13", "2020-11-13",
    "SSD-79DS-09-2020", "nOPV2", "Rnd2", "2021-02-19", "2021-02-19",
    "Tanganyika", "nOPV2", "Rnd1", "2023-01-29", "2023-01-29",
    "TNZ-2023-05-01_nOPV", "nOPV2", "Rnd1", "2023-09-24", "2023-09-24",
    "TNZ-2023-05-01_nOPV", "nOPV2", "Rnd2", "2023-11-05", "2023-11-05",
    "TOG-39DS-06-022", "nOPV2", "Rnd1", "2022-08-18", "2022-08-18",
    "TOG-39DS-06-022", "nOPV2", "Rnd2", "2022-10-16", "2022-10-16",
    "TZA...DS-02-2022", "bOPV", "Rnd1", "2022-03-27", "2022-03-27",
    "TZA...DS-02-2022", "bOPV", "Rnd2", "2022-05-21", "2022-05-21",
    "TZA...DS-02-2022", "bOPV", "Rnd3", "2022-09-04", "2022-09-04",
    "TZA...DS-02-2022", "bOPV", "Rnd4", "2022-12-04", "2022-12-04",
    "UGA-149DS-08-2021", "nOPV2", "Rnd1", "2022-01-17", "2022-01-17",
    "UGA-149DS-08-2021", "nOPV2", "Rnd2", "2022-10-31", "2022-10-31",
    "UGA-2024-08-01_nOPV", "nOPV2", "Rnd1", "2024-10-05", "2024-10-05",
    "Uganda_2023", "nOPV2", "Rnd1", "2022-11-07", "2022-11-07",
    "Uganda_2023", "nOPV2", "Rnd2", "2023-01-30", "2023-01-30",
    "ZAM-2023-02-01_nOPV", "nOPV2", "Rnd1", "2023-02-19", "2023-02-19",
    "ZAM-2023-02-01_nOPV", "nOPV2", "Rnd2", "2023-04-23", "2023-04-23",
    "ZAM-2023-05-01_bOPV", "bOPV", "Rnd1", "2023-06-22", "2023-06-22",
    "ZAM-2023-09-01_nOPV", "nOPV2", "Rnd1", "2023-09-03", "2023-09-03",
    "ZAM-2023-09-01_nOPV", "nOPV2", "Rnd2", "2023-10-29", "2023-10-29",
    "ZAM-2024-06-01_nOPV", "nOPV2", "Rnd1", "2024-07-28", "2024-07-28",
    "ZIM-2023-05-01_bOPV", "bOPV", "Rnd1", "2023-05-26", "2023-05-26",
    "ZIM-2023-05-01_bOPV", "bOPV", "Rnd2", "2023-10-13", "2023-10-13",
    "ZIM-2023-12-01_nOPV", "nOPV2", "Rnd1", "2024-02-23", "2024-02-23",
    "ZIM-2023-12-01_nOPV", "nOPV2", "Rnd2", "2024-03-22", "2024-03-22",
    "ZMB-30DS-02-2022", "bOPV", "Rnd1", "2022-03-24", "2022-03-24",
    "ZMB-30DS-02-2022", "bOPV", "Rnd2", "2022-04-24", "2022-04-24",
    "ZMB-30DS-02-2022", "bOPV", "Rnd3", "2022-08-14", "2022-08-14",
    "ZMB-30DS-02-2022", "bOPV", "Rnd4", "2022-10-30", "2022-10-30",
    "ZWE-2022", "bOPV", "Rnd1", "2022-10-30", "2022-10-30"
  )
  
  
  lookup_table<-lookup_table |> 
    mutate(start_date = as_date(start_date),
           end_date = as_date(end_date))
  # Join the lookup table with the original data
  
  F4 <- FI |> 
    left_join(lookup_table, by = c("Response", "Vaccine.type", "roundNumber")) |> 
    mutate(start_date = coalesce(start_date.y, as_date(start_date.x)), 
           end_date = coalesce(end_date.y, as_date(end_date.x)) ) |> 
    select(-start_date.x, -start_date.y, -end_date.x, -end_date.y) |> 
    filter(District != "NA")
  
  
  F5 <- F4 %>%
    mutate(
      tot_r = r_Non_Compliance + r_House_not_visited + r_childabsent +
        r_Child_was_asleep + r_Child_is_a_visitor + r_Vaccinated_but_not_FM,
      other_r = ifelse((total_missed - tot_r) < 0, 0, (total_missed - tot_r)),
      Country = case_when(
        Country == "DRC" ~ "RDC",
        Country == "Camerooun" ~ "CAE",
        Country == "CAMEROON" ~ "CAE",
        Country == "BURKINA_FASO" ~ "BFA",
        Country == "Ethiopia" ~ "ETH",
        Country == "ZAMBIA" ~ "ZMB",
        Country == "BENIN" ~ "BEN",
        Country == "CHAD" ~ "CHD",
        TRUE ~ Country
      )
    ) %>%
    select(
      country = Country,
      province = Region,
      district = District,
      response = Response,
      vaccine.type = Vaccine.type,
      roundNumber,
      numbercluster = cluster,
      start_date,
      end_date,
      male_sampled,
      female_sampled,
      total_sampled,
      male_vaccinated,
      female_vaccinated,
      total_vaccinated,
      total_missed,
      status = Status,
      performance = Performance,
      r_Non_Compliance,
      r_House_not_visited,
      r_childabsent,
      r_Child_was_asleep,
      r_Child_is_a_visitor,
      r_Vaccinated_but_not_FM,
      other_r,
      percent_care_Giver_Informed_SIA
    ) %>%
    mutate(
      end_date = ymd(end_date),  # Parse end_date into Date format
      end_date = case_when(
        year(end_date) == 2028 ~ update(end_date, year = 2023),  # Correct year
        year(end_date) == 2025 ~ update(end_date, year = 2024),
        TRUE ~ end_date
      )
    ) %>%
    arrange(start_date)
  
  F6 <- F5 %>%
    mutate(
      across(
        c(
          r_Non_Compliance, r_House_not_visited, r_childabsent, r_Child_was_asleep, 
          r_Child_is_a_visitor, r_Vaccinated_but_not_FM, other_r
        ),
        ~ ifelse(total_missed == 0, 0, (.x / total_missed) * 100),
        .names = "prct_{.col}"
      )
    ) %>%
    mutate(
      across(starts_with("prct_"), ~ round(.x, 2))
    )
  
  # Generate dynamic output file name
  country_name <- unique(data$Country) %>% na.omit()
  output_file_name <- paste0(country_name, "_LQAS.rds")
  output_file_path <- file.path(output_directory, output_file_name)
  
  # Save the transformed data
  saveRDS(F6, output_file_path)
  
  return(F6)
}

# # Example usage
# file_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/LQAS_raw/DRC_SIA_LQAS_1.csv"
# output_directory <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/LQAS_level1/"
# DF <- read_and_transform_file(file_path, output_directory)
# 
# # Check the data
# head(DF)






