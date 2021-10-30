###
# Helper functions
#
select_range <- function(data, start_date, end_date = NULL) {
  # Inputs a load/gen dataset and filters it, returning only the observations
  # for a given date
  start_date <- as.Date(start_date)
  
  if (is.null(end_date)) {
    end_date = start_date
  } else {
    end_date <- as.Date(end_date)
  }
  
  data %>% 
    filter(time_stamp >= start_date) %>%
    filter(time_stamp < end_date + days(1))
}

rollup_daily <- function(data) {
  # Rolls up the data from hourly to daily to make charts over long periods of
  # time more readable
  data %>%
    group_by(time_stamp = floor_date(time_stamp, "1 day")) %>%
    summarize(across(any_of(append(FUEL_COLUMNS, "load")), ~ sum(.x))) %>%
    ungroup
}

plot_line_per_fuel <- function(data) {
  # Plots each fuel source's output as a line on a graph
  data <- tibble(data)
  for (fuel_type in FUEL_COLUMNS) {
    # Convert MW to GW
    data[[fuel_type]] <- data[[fuel_type]] / 1000
  }
  data %>%
    ggplot(aes(x = time_stamp)) +
      geom_line(aes(y = dual_fuel), color = "#F95355") +
      geom_line(aes(y = nuclear), color = "#B0D894") +
      geom_line(aes(y = hydro), color = "#57A4B1") +
      geom_line(aes(y = natural_gas), color = "#FADE89") +
      geom_line(aes(y = other_fossil_fuels), color = "#957106") +
      geom_line(aes(y = other_renewables), color = "#4C6472") +
      geom_line(aes(y = wind), color = "#1BBC9B") +
      theme(
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
}

plot_clean_dirty_lines <- function(data) {
  # Categorizes each fuel source as clean or dirty then plots them as two lines
  # on a graph
  ggplot(data %>% categorize_fuels, aes(x = time_stamp)) +
    geom_line(aes(y = clean_gen), color = "#B0D894") +
    geom_line(aes(y = dirty_gen), color = "#7c5e05")
}

categorize_fuels <- function(data) {
  # Inputs a gen/load dataset and adds four new columns: clean_gen, dirty_gen
  # and clean_perc, dirty_perc.
  data <- tibble(data)
  data$clean_gen <- rowSums(data[CLEAN_FUELS])
  data$dirty_gen <- rowSums(data[DIRTY_FUELS])
  
  totals <- data$clean_gen + data$dirty_gen
  data$clean_perc <- data$clean_gen / totals
  data$dirty_perc <- data$dirty_gen / totals
  
  data
}

calculate_fuel_percs <- function(data) {
  data <- tibble(data)
  data$total_gen <- rowSums(data[FUEL_COLUMNS])
  for (fuel_type in FUEL_COLUMNS) {
    data[[paste(fuel_type, "_perc", sep="")]] <- data[[fuel_type]] / data$total_gen
  }
  
  data
}

plot_fuel_mix <- function(data) {
  fuel_perc_cols <- map_chr(FUEL_COLUMNS, function(s) { paste(s, "_perc", sep="") })
  
  data %>%
    select(time_stamp, any_of(fuel_perc_cols)) %>%
    pivot_longer(
      cols=fuel_perc_cols,
      names_to="fuel_type",
    ) %>%
    
    ggplot +
    aes(x = time_stamp, y = value, fill = fuel_type) +
    geom_bar(stat = "identity") +
    scale_x_datetime(label = date_format("%Y-%m"), breaks = pretty_breaks(), expand = c(0, 0))
}

plot_clean_dirty_mix <- function(data) {
  long <- data %>%
    select(time_stamp, clean_perc, dirty_perc) %>%
    pivot_longer(
      cols=c(clean_perc, dirty_perc),
      names_to="gen_category",
    )
  
  ggplot(long) +
    aes(x = time_stamp, y = value, fill = gen_category) +
    geom_area(stat = "identity") +
    scale_x_datetime(
      label = date_format("%Y-%m"),
      breaks = pretty_breaks(),
      expand = c(0, 0),
    ) +
    ylab("% Of Generation") +
    xlab("Date") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_datetime(date_breaks = "1 day", date_labels =  "%b %d, %Y") +
    scale_fill_manual(
      name = "Fuel Source",
      values = c(
        "clean_perc" = CLEAN_GEN_COLOR,
        "dirty_perc" = DIRTY_GEN_COLOR
      ),
      labels = c(
        "clean_perc" = "Clean Fuels",
        "dirty_perc" = "Dirty Fuels"
      )
    ) +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
}

plot_gen_area <- function(data, legend = TRUE) {
  # Plots an area graph, with each section of the area being a different fuel
  # source at that time of day.
  long <- data %>%
    select(time_stamp, any_of(FUEL_COLUMNS)) %>%
    pivot_longer(
      cols=any_of(FUEL_COLUMNS),
      names_to="fuel_type",
    ) %>%
    mutate(value = value / 1000)
  
  ggplot(long) +
    aes(x = time_stamp) +
    geom_area(aes(y = value, fill = fuel_type)) +
    scale_x_datetime(
      date_labels = "%b %d, %Y",
      date_breaks = "1 day"
    ) +
    ylab("Generation (GW)") +
    xlab("Date") +
    scale_fill_manual(
      name = "Fuel Source",
      values = c(
        "nuclear" = "#5EBD3E",
        "hydro" = "#009CDF",
        "wind" = "#973999",
        "natural_gas" = "#FFB900",
        "dual_fuel" = DIRTY_GEN_COLOR,
        "other_renewables" = "#00B4FF",
        "other_fossil_fuels" = "#515151"
      ),
      labels = c(
        "nuclear" = "Nuclear",
        "hydro" = "Hydro",
        "wind" = "Wind",
        "natural_gas" = "Natural Gas",
        "dual_fuel" = "Dual Fuel",
        "other_renewables" = "Other Renewables",
        "other_fossil_fuels" = "Other Fossil Fuels"
      )
    ) +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
    )
}