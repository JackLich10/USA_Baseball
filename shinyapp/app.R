# load libraries
library(shiny)
library(tidyverse)
library(scales)
library(ggrepel)
library(gridExtra)
library(grid)
library(gtable)
library(shinydashboard)
library(shinyWidgets)
library(fresh)

# set a theme for shinydashboard
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  )
)

# set a theme for plots
theme_set(theme_bw() +
              theme(plot.title = element_text(face = "bold", hjust = 0),
                    plot.subtitle = element_text(face = "italic", hjust = 0),
                    plot.caption = element_text(face = "italic"),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    axis.ticks = element_blank(),
                    strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
                    strip.text.x = element_text(face = "bold")))

# read in data
tidy_stats <- read_csv("tidy_stats.csv")
tidy_re <- read_csv("tidy_re.csv")

# vectors of each pdp assessment by type
physical <- c("height_in", "weight")
sprint <- c("reaction_to_go_sec", "iso_10", "iso_30", "fly_20")
jump <- c("cmj_gct_sec", "cmj_height_in", "bj_gct_sec", "bj_distance_in")
agility <- c("green_box", "green_3", "percent_change", "hawkeye")
grip_strength <- c("left_down", "left_90", "left_up", "right_down", "right_90", "right_up")
single_leg <- c("absval_dpwr_delta", "absval_dua_delta")

# pdp color palette
palette <- c("#ce3d53","#437ac7", "#56a3e7", "#C0C0C0", "black")

# function to transform dataframe to be ready to plot
transform_data <- function(df, type) {
  if (type == "College") {
    first_var <- "so_rate"
    last_var <- "slg"
  } else if (type == "PDP") {
    first_var <- "reaction_to_go_sec"
    last_var <- "weight"
  } else if (type == "RE") {
    first_var <- "dynamic_visual_acuity"
    last_var <- "vert_pursuit"
  } else if (type == "Trackman") {
    
  } else if (type == "PDP_large") {
    first_var <- "height_in"
    last_var <- "right_up"
  }

  # transform data to long format
  transformed <- df %>% 
    mutate(across(c(all_of(first_var):all_of(last_var)), ~ replace(., abs(scale(.)) > 3.5, NA))) %>% 
    pivot_longer(cols = c(all_of(first_var):all_of(last_var)), values_drop_na = T) %>%
    mutate(
      type = case_when(
        name %in% c("so_rate", "walk_rate") ~ "Discipline",
        name %in% c("avg", "obp", "slg", "ops") ~ "Performance",
        name == "dynamic_visual_acuity" ~ "Visual Acuity (ms)",
        name %in% c("choice_processing", "discriminate_processing") ~ "Processing (ms)",
        name %in% c("choice_speed", "discriminate_speed") ~ "Speed (ms)",
        name %in% c("choice_latency", "discriminate_latency") ~ "Latency (ms)",
        name %in% c("circ_pursuit", "horz_pursuit", "vert_pursuit") ~ "Pursuit (% accurate)",
        name %in% c("height_in", "weight") ~ "Physical",
        name %in% c("reaction_to_go_sec", "iso10", "iso30") ~ "Speed and Reaction",
        name %in% physical ~ "Physical",
        name %in% sprint ~ "Speed and Reaction",
        name %in% jump ~ "Jumping",
        name %in% agility ~ "Agility and Processing",
        name %in% grip_strength ~ "Grip Strength",
        name %in% single_leg ~ "Single Leg Jumping"),
      type = suppressWarnings(fct_relevel(type, "Physical", "Speed and Reaction", "Jumping", "Grip Strength",
                                          "Single Leg Jumping", "Agility and Processing",
                                          "Speed (ms)", "Latency (ms)", "Processing (ms)")),
      name = str_to_title(str_replace_all(name, "_", " ")),
      name = str_remove(name, " Visual Acuity| Processing| Speed| Latency| Pursuit"),
      name = case_when(
        name == "So Rate" ~ "SO Rate",
        name %in% c("Avg", "Obp", "Slg", "Iso 10", "Iso 25", "Iso 30") ~ str_to_upper(name),
        name == "Percent Change" ~ "% Change",
        name == "Reaction To Go Sec" ~ "Reaction Time",
        name == "Height In" ~ "Height",
        name == "Weight" ~ "Weight",
        name == "Cmj Height In" ~ "CMJ Height",
        name == "Cmj Gct Sec" ~ "CMJ GCT",
        name == "Bj Distance In" ~ "BJ Distance",
        name == "Bj Gct Sec" ~ "BJ GCT",
        name == "Absval Dpwr Delta" ~ "Drift PWR Delta",
        name == "Absval Dua Delta" ~ "Drift UA Delta",
        TRUE ~ name),
      name = suppressWarnings(fct_relevel(name, "Height", "Weight", 
                                          "Reaction Time", "ISO 10", "ISO 25", "ISO 30", "Fly 20",
                                          "CMJ Height", "CMJ GCT", "BJ Distance", "BJ GCT",
                                          "Green Box", "Green 3", "% Change", "Agility Diff", "Hawkeye",
                                          "Right Up", "Right Down", "Right 90", "Left Up", "Left Down", "Left 90")),
      name = fct_rev(name)) %>% 
    filter(!is.na(type))
  
  return(transformed)
}
# function to get ready to plot given a player id
ready_to_plot <- function(df, player, scale = T, sample = 1) {
  if (scale == T) {
    df <- df %>% 
      group_by(name, type) %>% 
      mutate(scaled_value = scale(value)) %>% 
      ungroup()
  } else if (scale == F) {
    df <- df %>% 
      mutate(scaled_value = value)
  }
  
  df <- df %>% 
    group_by(name) %>% 
    mutate(
      rank = case_when(
        name %in% c("SO Rate", "Dynamic",
                    "Discriminate", "Choice",
                    "Hawkeye", "CMJ GCT", "BJ GCT",
                    "Drift PWR Delta", "Drift UA Delta") ~ percent_rank(-scaled_value), # lower score is better
        type %in% c("Speed and Reaction", "Agility and Processing") ~ percent_rank(-scaled_value), # lower score is better
        TRUE ~ percent_rank(scaled_value))) %>% 
    ungroup() %>% 
    mutate(
      rank_lab = case_when(
        round(100*rank) %in% c(11, 12, 13) ~ paste0(format(100*round(rank, 2), nsmall = 0), "th%"),
        round(100*rank) %% 10 == 1 ~ paste0(format(100*round(rank, 2), nsmall = 0), "st%"),
        round(100*rank) %% 10 == 2 ~ paste0(format(100*round(rank, 2), nsmall = 0), "nd%"),
        round(100*rank) %% 10 == 3 ~ paste0(format(100*round(rank, 2), nsmall = 0), "rd%"),
        TRUE ~ paste0(format(100*round(rank, 2), nsmall = 0), "th%")),
      value_lab = case_when(
        name == "Hawkeye" ~ format(round(value, 0), nsmall = 0),
        name %in% c("% Change", "SO Rate", "Walk Rate") ~ paste0(format(round(100 * value, 1), nsmall = 0), "%"),
        name %in% c("Drift PWR Delta", "Drift UA Delta") ~ paste0(format(round(value, 1), nsmall = 0), "%"),
        name %in% c("Horz", "Vert", "Circ") ~ paste0(format(round(value, 2), nsmall = 0), "%"),
        type %in% c("Discipline", "Performance") ~ format(round(value, 3), nsmall = 0),
        type %in% c("Speed and Reaction", "Agility and Processing") ~ paste0(format(round(value, 2), nsmall = 0), " s"),
        name == "Height" ~ paste0(format(round(value, 1), nsmall = 0), " in"),
        name == "Weight" ~ paste0(format(round(value, 1), nsmall = 0), " lbs"),
        name %in% c("CMJ Height", "BJ Distance") ~ paste0(format(round(value, 1), nsmall = 0), " in"),
        name %in% c("CMJ GCT", "BJ GCT") ~ paste0(format(round(value, 2), nsmall = 0), " s"),
        type %in% c("Speed (ms)", "Processing (ms)", "Latency (ms)", "Visual Acuity (ms)") ~ paste0(format(round(value, 1), nsmall = 0), " ms"),
        TRUE ~ format(round(value, 1), nsmall = 0)))
  
  # if using display sample feature
  if (sample != 1) {
    player_df <- df %>% 
      filter(id %in% player)
    
    df <- df %>% 
      group_by(id) %>% 
      slice_sample(prop = sample, .preserve = T) %>% 
      ungroup() %>% 
      bind_rows(player_df) %>% 
      group_by(id) %>% 
      distinct(id, name, date, .keep_all = T) %>% 
      ungroup()
  }
  # make sure players of interest are first levels of factor for plotting
  df <- df %>% 
    mutate(id = as.character(id),
           id = suppressWarnings(fct_relevel(id, player))) %>% 
    arrange(desc(id))
  return(df)
}
# function to plot transformed dataframe
plot_distributions <- function(df, player, college = F, over_time = F, alpha = 0.15) {
  if (college == T) {
    xlab_pos <- 0.01
  } else {
    xlab_pos <- -2.9
  }

  # base plot of jittered points and boxplot
  plot <- df %>% 
    filter(!is.na(scaled_value)) %>% 
    ggplot(aes(scaled_value, name)) +
    geom_boxplot(color = "grey", outlier.alpha = 0) +
    geom_jitter(alpha = 0.2, height = 0.075) +
    guides(alpha = F, size = F) +
    theme(legend.position = "top") +
    labs(color = NULL)
  
  # if selecting one player over time
  if (over_time == T) {
    df_over_time <- df %>% 
      filter(!is.na(scaled_value), id %in% player) %>% 
      arrange(desc(date)) %>% 
      group_by(date) %>% 
      top_n(date, 5) %>% 
      ungroup()
    # change to sequential palette
    palette <- c("#9fcae1", "#6baed6", "#4192c6", "#2171b5", "#08519c")
    colors <- c(palette[1:length(unique(df_over_time$date))])

    plot <- plot + geom_point(data = df_over_time, 
                              mapping = aes(scaled_value, name, color = assess_number), 
                              alpha = 1, size = 4) +
      labs(color = "Assessment #")
  } else {
    # not looking over time
    colors <- c(palette[1:length(player)])
    plot <- plot + geom_point(data = df %>% filter(!is.na(scaled_value), id %in% player), 
                              mapping = aes(scaled_value, name, color = id), 
                              alpha = 1, size = 4)
  }
  plot <- plot + geom_text_repel(aes(label = ifelse(id %in% c(player), 
                                                    rank_lab, "")),
                                 fontface = "bold", size = 3) +
    scale_color_manual(values = colors)
  if (length(player) == 1 & over_time == F) {
    # if only one player and one test, then plot the label of their test result
    plot <- plot + geom_text(aes(x = xlab_pos, label = ifelse(id %in% c(player), 
                                                              value_lab, "")),
                             fontface = "bold", size = 3.5) +
      guides(color = F)
  }
  
  return(plot)
}
# function to plot tables of top performers for a given test
plot_table <- function(top_performers, test = c("ISO 10", "ISO 30", "Green Box", "Green 3", "% Change", "Hawkeye", "CMJ GCT", "CMJ Height", "BJ GCT", "BJ Distance")) {
  if (nrow(filter(top_performers, name == test)) == 0) {
    # return null if event did not record that specific test
    return(nullGrob())
  }
  if (test %in% c("ISO 10", "ISO 30")) {
    fill <- "#437ac7"
  } else if (test %in% c("Green Box", "Green 3")) {
    fill <- "#ce3d53"
  } else if (test %in% c("% Change", "Hawkeye")) {
    fill <- "#56a3e7"
  } else if (test %in% c("CMJ GCT", "CMJ Height")) {
    fill <- "#C0C0C0"
  } else if (test %in% c("BJ GCT", "BJ Distance")) {
    fill <- "#437ac7"
  }
  if (test == "% Change") {
    test_lab <- "MITB"
  } else {
    test_lab <- test
  }
  # set theme for table
  tt <- ttheme_default(base_size = 6.4, padding = unit(c(2.2, 2.2), "mm"),
                       core = list(bg_params = list(fill = "white", col = NA)),
                       colhead = list(bg_params = list(fill = "NA", col = NA)))
  # set title and padding
  title <- grobTree(rectGrob(gp = gpar(fill = fill, col = NA)), 
                    textGrob(paste0(test_lab, " Top Performers"), gp = gpar(fontsize = 7.5, fontface = "bold.italic")),
                    segmentsGrob(x0 = unit(1,"npc"),
                                 y0 = unit(0,"npc"),
                                 x1 = unit(0,"npc"),
                                 y1 = unit(0,"npc"),
                                 gp = gpar(lwd = 3)))
  padding <- unit(0.79,"line")
  
  table <- top_performers %>% 
    filter(name == test) %>% 
    mutate(grade = ifelse(grade == "Unknown", "-", grade),
           grad_year = ifelse(grad_year == "Unknown", "-", grad_year),
           position = ifelse(position == "Unknown", "-", position),
           value_lab = str_trim(value_lab)) %>% 
    transmute(id = paste(grade, grad_year, position, id, value_lab, sep = ": "), name, rank) %>%
    pivot_wider(id_cols = rank, names_from = "name", values_from = "id") %>%
    separate(col = test, into = c("Grade", "Year", "Pos", "Player", test), sep = ": ") %>% 
    select(-rank) %>% 
    tableGrob(rows = NULL, theme = tt) 
  
  g <- table %>% 
    gtable_add_grob(
      grobs = segmentsGrob( # line under column names
        x0 = unit(1,"npc"),
        y0 = unit(0,"npc"),
        x1 = unit(0,"npc"),
        y1 = unit(0,"npc"),
        gp = gpar(lwd = 3)),
      t = 1, l = 1, r = ncol(table)) %>% 
    gtable_add_grob(
      grobs = segmentsGrob( # line across the bottom
        x0 = unit(1,"npc"),
        y0 = unit(0,"npc"),
        x1 = unit(0,"npc"),
        y1 = unit(0,"npc"),
        gp = gpar(lwd = 3)),
      t = nrow(table), l = 1, r = ncol(table)) %>% 
    gtable_add_rows(heights = grobHeight(title) + padding, pos = 0) %>% 
    gtable_add_grob(grobs = title, # add title
                    t = 1, l = 1, r = ncol(table))
  g$widths <- unit(c(0.1, 0.1, 0.1, 0.35, 0.2), "npc") # setting widths of table columns
  return(g)
}

# transforming static data
stats_transformed <- transform_data(tidy_stats, type = "College")
RE_transformed <- transform_data(tidy_re, type = "RE")

# define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "USA Baseball Dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
          menuItem("PDP Athletic Assessments",
                   tabName = "PDP_tab",
                   icon = icon("fas fa-walking")),
          menuItem("College Batting Performance",
                   tabName = "college_tab",
                   icon = icon("fas fa-baseball-ball")),
          menuItem("RightEye Tests",
                   tabName = "RE_tab",
                   icon = icon("fas fa-eye"))
        ),
        img(src = "USABaseball.png", height = "100%", width = "100%", align = "center")
    ),
    
    dashboardBody(
      use_theme(mytheme),
      tabItems(
        tabItem(tabName = "PDP_tab",
                fluidRow(box(fileInput("PDP_file",
                                       label = "Choose Excel File: ",
                                       accept = c(".xlsx")),
                             width = 6),
                         box(sliderInput("PDP_top_n", # this is a slider bar for top performers
                                         label = "Top Performers: ", # for excel file download
                                         min = 20,
                                         max = 500,
                                         value = 100,
                                         step = 20),
                             width = 2),
                         box(downloadButton("PDP_top_excel",
                                            label = "Excel Download"),
                             width = 2),
                         box(downloadButton("PDP_top_performers",
                                            label = "Top Perfomers"),
                             width = 2),
                         box(downloadButton("PDP_report"),
                             width = 2),
                         box(actionButton("PDP_plot", 
                                          label = "Update Plot"),
                             width = 2)),
                
                fluidRow(
                  # box(radioButtons("PDP_identify", # used for Data+ where IRB does not allowed identified data
                  #                         label = "Identify: ",
                  #                         choices = c("De-Identify", "Player Name"),
                  #                         selected = "Player Name"),
                  #            width = 2),
                         box(radioButtons("PDP_assess",
                                          label = "Assessment: ",
                                          choices = c("First", "Last", "All"),
                                          selected = "Last"),
                             width = 2),
                         box(sliderInput("PDP_sample",
                                         label = "Display Sample: ",
                                         min = 0.1,
                                         max = 1,
                                         value = 1,
                                         step = 0.1),
                             width = 2), 
                         box(pickerInput("PDP_event",
                                         label = "Event: ",
                                         choices = "All",
                                         multiple = T,
                                         options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "font-weight: bold;"),
                                         selected = "All"),
                             width = 2), 
                         box(pickerInput("PDP_grade",
                                         label = "Grade: ",
                                         choices = c("All", "Unknown", sort(seq(3, 16, 1), decreasing = T)),
                                         multiple = T,
                                         options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "font-weight: bold;"),
                                         selected = "All"),
                             width = 2), 
                         box(pickerInput("PDP_position",
                                         label = "Position: ",
                                         choices = c("All", "Unknown", "INF", "OF", "C", "DH", "P", "UTIL"),
                                         multiple = T,
                                         options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "font-weight: bold;"),
                                         selected = "All"),
                             width = 2), 
                         box(pickerInput("PDP_id",
                                         label = "Player: ",
                                         choices = "All",
                                         multiple = T,
                                         options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "font-weight: bold;", `max-options` = 5, `max-options-text` = "Can only select max 5 players"),
                                         selected = "All"),
                             width = 2)),
                
                fluidRow(htmlOutput("PDP_text"),
                         tags$head(
                           tags$style("#PDP_text{
                                 font-size: 18px;
                                 font-style: italic;
                                 text-align: center;}"
                           )
                         ),
                         box(plotOutput("PDP_dist"), width = 12))
                
        ),
        tabItem(tabName = "college_tab",
                fluidRow(box(selectInput("college_position",
                                         label = "Position: ",
                                         choices = unique(c("All", str_to_upper(tidy_stats$position)))),
                             width = 3), 
                         box(sliderInput(inputId = "college_tot_ab",
                                         label = "Min. At-Bats: ",
                                         min = 50,
                                         max = 700,
                                         value = 50,
                                         step = 10),
                             width = 3), 
                         box(uiOutput("college_id_selector"),
                             width = 3),
                         box(downloadButton("college_report"),
                             width = 3)),
                
                fluidRow(box(plotOutput("college_dist"), width = 12))
                
        ),
        tabItem(tabName = "RE_tab",
                fluidRow(box(selectInput("RE_year",
                                         label = "Test Year: ",
                                         choices = unique(c("All", tidy_re$year))),
                             width = 3), 
                         box(uiOutput("RE_age_selector"),
                             width = 3), 
                         box(uiOutput("RE_id_selector"),
                             width = 3),
                         box(downloadButton("RE_report"),
                             width = 3)),
                
                fluidRow(box(plotOutput("RE_dist"), width = 12)))
      )
    )
)

# define server logic
server <- function(input, output, session) {
  
  # PDP assessment tab ------------------------------------------------------
  vals_PDP <- reactiveValues(dist_PDP = NULL)
  
  # read in excel file
  PDP_excel <- reactive({
    req(input$PDP_file)
    
    withProgress({
      PDP_excel <- suppressWarnings(readxl::read_excel(input$PDP_file$datapath,
                                                       na = c("", "-"))) %>% 
        janitor::clean_names() %>% 
        mutate(across(c(height_in:right_up), ~ suppressWarnings(as.numeric(.)))) %>% 
        unite(id, c("first_name", "last_name"), sep = " ") %>% 
        separate(position, into = c("position", NA), fill = "right", extra = "merge") %>% 
        mutate(position = str_to_upper(position),
               pos_simple = case_when(
                 position %in% c("OF", "CF", "COF") ~ "OF",
                 position %in% c("IF", "INF", "SS", "1B", "2B", "3B", "MIF", "CIF", "MI") ~ "INF",
                 position %in% c("P", "LHP", "RHP") ~ "P",
                 position == "C" ~ "C",
                 position == "DH" ~ "DH",
                 position %in% c("UTIL", "UT", "UTL") ~ "UTIL",
                 TRUE ~ "Unknown"),
               year = ifelse(is.na(year), "Unknown", as.character(year)),
               grade = ifelse(is.na(grade), "Unknown", as.character(grade))) 
    }, message = "Reading Data...") 
    PDP_excel
  })
  
  # choose de-identify or first/last assessment 
  PDP_assessments <- reactive({
    req(PDP_excel(), input$PDP_assess)
    PDP_excel <- PDP_excel()
    withProgress({
      if (input$PDP_assess == "First") {
        PDP_assessments <- PDP_excel %>% 
          arrange(date) %>% 
          distinct(id, .keep_all = T) # arrange by date so we take everyone's first test
      } else if (input$PDP_assess == "Last") {
        PDP_assessments <- PDP_excel %>% 
          arrange(desc(date)) %>% 
          distinct(id, .keep_all = T) # arrange by desc(date) so we take everyone's last test
      } else {
        PDP_assessments <- PDP_excel %>% 
          arrange(date) %>%
          group_by(id) %>%
          mutate(assess_number = as.factor(row_number())) %>% 
          ungroup() 
      }
      # if (input$PDP_identify == "De-Identify") { # used for Data+ for de-identifying data
      #   PDP_assessments <- PDP_assessments %>% 
      #     mutate(row = row_number(),
      #            id = paste0("Player ", row))
      # }
    }, message = "Reformatting Data...")
    PDP_assessments
  })
  
  # update PDP events from excel file
  observe({
    req(PDP_assessments())
    PDP_assessments <- PDP_assessments()
    updatePickerInput(session,
                      inputId = "PDP_event",
                      label = "Event: ",
                      choices = c("All", unique(PDP_assessments$event)),
                      selected = "All")
  })
  
  # update player IDs from excel file and other filters
  observe({
    req(input$PDP_event, input$PDP_grade, input$PDP_position, PDP_assessments())
    PDP_assessments <- PDP_assessments()
    ids <- unique(c("All", sort(PDP_assessments()$id)))
    
    if (!"All" %in% input$PDP_event) {
      ids <- c("All", intersect(ids, PDP_assessments() %>% 
                                  filter(event %in% input$PDP_event) %>% 
                                  pull(id)))
    }
    if (!"All" %in% input$PDP_position) {
      ids <- c("All", intersect(ids, PDP_assessments() %>% 
                                  filter(pos_simple %in% input$PDP_position) %>% 
                                  pull(id)))
    }
    if (!"All" %in% input$PDP_grade) {
      ids <- c("All", intersect(ids, PDP_assessments() %>% 
                                  filter(grade %in% input$PDP_grade) %>% 
                                  pull(id)))
    }
    ids_df <- PDP_assessments %>% 
      filter(id %in% ids) %>%
      add_row(id = "All")
    
    updatePickerInput(session,
                      inputId = "PDP_id",
                      label = "Player: ",
                      choices = ids_df$id,
                      choicesOpt = list(subtext = paste0(ids_df$grade, " | ", ids_df$pos_simple)),
                      selected = "All")
  })
  
  # transform data to long format
  PDP_transformed <- reactive({
    req(input$PDP_event, input$PDP_grade, input$PDP_position, PDP_assessments())
    PDP_transformed <- transform_data(PDP_assessments(), type = "PDP_large")
    
    if (!"All" %in% input$PDP_grade) {
      PDP_transformed <- PDP_transformed %>% 
        filter(grade %in% input$PDP_grade)
    }
    if (!"All" %in% input$PDP_position) {
      PDP_transformed <- PDP_transformed %>% 
        filter(pos_simple %in% input$PDP_position)
    }
    if (!"All" %in%input$PDP_event) {
      PDP_transformed <- PDP_transformed %>% 
        filter(event %in% input$PDP_event)
    }
    if (input$PDP_sample < 1) {
      shiny::validate(
        need(length(unique(PDP_transformed$id)) >= 20, "Filters reduce observations to below 20. Make sure to set sample = 1.")
      )
    }
    PDP_transformed
  })
  
  # render text for # of players within filter
  output$PDP_text <- renderText({ 
    req(input$PDP_grade, input$PDP_position, PDP_transformed())
    message <- paste0("Currently viewing ", str_to_lower(input$PDP_assess), " assessments. There are <b>", comma(length(unique(PDP_transformed()$id))), "</b> unique players under the current set of filters.")
    if (input$PDP_assess == "All") {
      message <- paste0(message, "<br>Select a player with multiple assessments to view over time.")
    }
    withProgress({
      message
    }, message = "Reformatting Data...")
  })
  
  # get plot ready on click
  PDP_ready <- eventReactive(input$PDP_plot, {
    req(input$PDP_event, input$PDP_grade, input$PDP_position, input$PDP_id, input$PDP_sample, PDP_transformed())
    if (input$PDP_assess == "All") {
      shiny::validate(
        need(length(input$PDP_id) == 1, "Can only look at one player's assessment history over time (make sure 'All' is not selected). If you wish to compare multiple players, please select either 'First' or 'Last' under the Assessment tab.")
      )
    }
    if (input$PDP_sample != 1) {
      caption <- paste0("Showing a ", 100 * input$PDP_sample, "% sample of ", comma(length(unique(PDP_transformed()$id))), " players within filter")
    } else {
      caption <- paste0("Among ", comma(length(unique(PDP_transformed()$id))), " players within filter")
    }
    if (input$PDP_assess == "All") {
      over_time <- T
    } else {
      over_time <- F
    }
    isolate(
      ready_to_plot(PDP_transformed(), player = input$PDP_id, sample = input$PDP_sample) %>% 
        plot_distributions(., player = input$PDP_id, over_time = over_time) +
        geom_vline(xintercept = 0, color = "red", lty = 2) +
        scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1), labels = seq(-3, 3, by = 1)) +
        facet_wrap(~ type, scales = "free_y", ncol = 2) +
        labs(title = "PDP Athletic Measurements",
             subtitle = paste0("Player(s): ", paste0(input$PDP_id, collapse = ", "), " | Event(s): ", paste0(input$PDP_event, collapse = ", "), " | Grade Level(s): ", paste0(input$PDP_grade, collapse = ", "), " | Position(s): ", paste0(input$PDP_position, collapse = ", ")),
             x = "Standard Deviations from Mean",
             y = NULL,
             caption = caption)
    )
  }, ignoreNULL = F, ignoreInit = T)
  
  # plot the distributions of test scores
  output$PDP_dist <- renderPlot({
    withProgress({
      vals_PDP$dist_PDP <- PDP_ready() 
    }, message = "Plotting...")
    vals_PDP$dist_PDP
  })
  
  # download file of test score plots
  output$PDP_report = downloadHandler(
    filename = function() { "PDPReport.pdf" },
    content = function(file) {
      pdf(file, onefile = TRUE, width = 10, height = 8, paper = "USr", title = "PDP Athletic Assessments")
      req(input$PDP_grade, input$PDP_position, input$PDP_id, PDP_assessments())
      shiny::validate(
        need(!is.null(vals_PDP$dist_PDP), "There is no plot to download. Please click the 'Update Plot' button.")
      )
      withProgress({
        vals_PDP$dist_PDP <- (cowplot::ggdraw(vals_PDP$dist_PDP) +
                                cowplot::draw_image("www/USABaseballColor.png", x = 0.45, y = 0.4725, scale = 0.125)) %>% 
          ggplotify::as.grob()
      }, message = "Downloading...")
      
      grid.arrange(grobs = list(vals_PDP$dist_PDP),
                   nrow = 1,
                   heights = c(1),
                   newpage = F)
      dev.off()
    }
  )
  # download top 10 performers .pdf
  output$PDP_top_performers <- downloadHandler(
    filename = function() { paste0("PDPtopperformers", paste0(input$PDP_event, collapse = ""),".pdf") },
    content = function(file) { 
      pdf(file, onefile = TRUE, width = 7, height = 9.5, title = "PDP Top Performers")
      req(input$PDP_grade, input$PDP_position, input$PDP_id, PDP_assessments())
      
      withProgress({
        top_performers <- ready_to_plot(PDP_transformed(), player = "All") %>% 
          group_by(name) %>%
          top_n(10, rank) %>%
          arrange(desc(rank)) %>%
          mutate(rank = row_number()) %>%
          ungroup() %>%
          filter(rank <= 10) %>% 
          arrange(desc(name), rank) %>% 
          select(id, grade, grad_year = year, position = pos_simple, type, name, value_lab, rank)
        
        iso_10 <- plot_table(top_performers = top_performers, test = "ISO 10")
        iso_30 <- plot_table(top_performers = top_performers, test = "ISO 30")
        green_box <- plot_table(top_performers = top_performers, test = "Green Box")
        green_3 <- plot_table(top_performers = top_performers, test = "Green 3")
        percent_change <- plot_table(top_performers = top_performers, test = "% Change")
        hawkeye <- plot_table(top_performers = top_performers, test = "Hawkeye")
        cmj_height <- plot_table(top_performers = top_performers, test = "CMJ Height")
        cmj_gct <- plot_table(top_performers = top_performers, test = "CMJ GCT")
        bj_distance <- plot_table(top_performers = top_performers, test = "BJ Distance")
        bj_gct <- plot_table(top_performers = top_performers, test = "BJ GCT")
        
        # usa_base_logo <- rasterGrob(png::readPNG("www/USABaseballColor.png"), just = "top")
        
        sprint <- arrangeGrob(grobs = list(iso_10, iso_30),
                              ncol = 2)
        green <- arrangeGrob(grobs = list(green_box, green_3),
                              ncol = 2)
        agility <- arrangeGrob(grobs = list(percent_change, hawkeye),
                               ncol = 2)
        cmj <- arrangeGrob(grobs = list(cmj_gct, cmj_height),
                           ncol = 2)
        bj <- arrangeGrob(grobs = list(bj_gct, bj_distance),
                          ncol = 2)
        tables <- arrangeGrob(grobs = list(sprint, green, agility, cmj, bj),
                              ncol = 1)
        
      }, message = "Downloading...")

      if (length(input$PDP_event) > 1) {
        event_lab <- "Multiple Events"
      } else {
        event_lab <- input$PDP_event
      }
      grid.arrange(top = textGrob(paste("PDP Top Performers", paste0(event_lab, " / ", Sys.Date()), sep = "\n"), gp = gpar(fontsize = 9, fontface = "bold")),
                   grobs = list(tables))
      dev.off()
    }
  )
  # download top n performers excel file
  output$PDP_top_excel <- downloadHandler(
    filename = function() { paste0("PDPtop", input$PDP_top_n, "performers.xlsx") },
    content = function(file) { 
      req(input$PDP_grade, input$PDP_position, input$PDP_id, PDP_assessments())
      
      withProgress({
        ready_to_plot(PDP_transformed(), player = "All") %>% 
          group_by(name) %>%                                 
          top_n(input$PDP_top_n, rank) %>%
          arrange(desc(rank)) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= input$PDP_top_n) %>% 
          ungroup() %>%
          select(date, id, grade, school_classification, grad_year = year,
                 position = pos_simple, type, test = name, value, rank) %>%
          arrange(desc(test), rank) %>%
          writexl::write_xlsx(., path = file)
      }, message = "Writing Excel File...")
    }
  )
  
  # College stats tab -------------------------------------------------------
  vals_college <- reactiveValues(dist_college = NULL)
  
  output$college_id_selector <- renderUI({
    
    if (input$college_position == "All") {
      selectInput(inputId = "college_id",
                  label = "Player ID: ",
                  choices = unique(c("All", tidy_stats$id[which(tidy_stats$tot_ab >= input$college_tot_ab)])))
    } else {
      selectInput(inputId = "college_id",
                  label = "Player ID: ",
                  choices = unique(c("All", tidy_stats$id[which(tidy_stats$position == str_to_lower(input$college_position) & 
                                                                  tidy_stats$tot_ab >= input$college_tot_ab)])))
    }
  })
  
  output$college_dist <- renderPlot({
    req(input$college_position, input$college_id)
    
    if (input$college_position != "All") {
      tidy_stats <- tidy_stats %>% 
        filter(position == str_to_lower(input$college_position))
    }
    
    withProgress({
      vals_college$dist_college <- stats_transformed %>% 
        filter(tot_ab >= input$college_tot_ab) %>% 
        ready_to_plot(., player = input$college_id, scale = F, sample = 1) %>% 
        plot_distributions(., player = input$college_id, college = T) +
        scale_x_continuous(labels = number_format(accuracy = 0.001)) +
        facet_wrap(~ type, scales = "free", nrow = 1) +
        labs(title = "College Batting Performance",
             subtitle = paste0("ID: ", input$college_id, " | Positions: ", input$college_position, " | Player At-Bats: ", tidy_stats$tot_ab[which(tidy_stats$id == input$college_id)]),
             x = NULL,
             y = NULL,
             caption = paste0("Min. At-Bats: ", input$college_tot_ab))
    }, message = "Plotting...")
    
    vals_college$dist_college
  })
  
  # Download file
  output$college_report = downloadHandler(
    filename = function() { "CollegeReport.pdf" },
    content = function(file) {
      pdf(file, onefile = TRUE, width = 10, height = 8, paper = "USr", title = "College Batting Performance")
      req(input$college_position, input$college_id)
      
      grid.arrange(grobs = list(vals_college$dist_college),
                   nrow = 1,
                   heights = c(1),
                   newpage = F)
      dev.off()
    }
  )
  
  # RE assessment tab -------------------------------------------------------
  vals_RE <- reactiveValues(dist_RE = NULL)
  
  output$RE_age_selector <- renderUI({
    req(input$RE_year)
    if (input$RE_year == "All") {
      selectInput(inputId = "RE_age",
                  label = "Age Group: ",
                  choices = unique(c("All", as.character(tidy_re$age_group))))
    } else {
      selectInput(inputId = "RE_age",
                  label = "Age Group: ",
                  choices = unique(c("All", as.character(tidy_re$age_group[which(tidy_re$year == input$RE_year)]))))
    }
  })
  
  output$RE_id_selector <- renderUI({
    req(input$RE_year, input$RE_age)
    ids <- unique(c("All", tidy_re$id))
    
    if (input$RE_year != "All") {
      ids <- c("All", intersect(ids, tidy_re %>% 
                                  filter(year == input$RE_year) %>% 
                                  pull(id)))
    }
    if (input$RE_age != "All") {
      ids <- c("All", intersect(ids, tidy_re %>% 
                                  filter(age_group == input$RE_age) %>% 
                                  pull(id)))
    }
    selectInput(inputId = "RE_id",
                label = "Select Player ID: ",
                choices = ids)
  })
  
  output$RE_dist <- renderPlot({
    req(input$RE_year, input$RE_age, input$RE_id)
    
    if (input$RE_year != "All") {
      RE_transformed <- RE_transformed %>% 
        filter(year == input$RE_year)
    }
    if (input$RE_age != "All") {
      RE_transformed <- RE_transformed %>% 
        filter(age_group == input$RE_age)
    }
    
    withProgress({
      vals_RE$dist_RE <- RE_transformed %>% 
        ready_to_plot(., player = input$RE_id, sample = 1, scale = T) %>% 
        plot_distributions(., player = input$RE_id) +
        geom_vline(xintercept = 0, color = "red", lty = 2) +
        scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1), labels = seq(-3, 3, by = 1)) +
        facet_wrap(~ type, scales = "free_y", ncol = 1) +
        labs(title = "RightEye Vision Measurements",
             subtitle = paste0("ID: ", input$RE_id, " | Age Group: ", input$RE_age, " | Event: ", input$RE_year),
             x = "Standard Deviations from Mean",
             y = NULL)
    }, message = "Plotting...")
    
    vals_RE$dist_RE
  })
  
  # Download file
  output$RE_report = downloadHandler(
    filename = function() { "REReport.pdf" },
    content = function(file) {
      pdf(file, onefile = TRUE, width = 10, height = 8, paper = "USr", title = "Right Eye Vision Assessments")
      req(input$RE_year, input$RE_age, input$RE_id)
      
      grid.arrange(grobs = list(vals_RE$dist_RE),
                   nrow = 1,
                   heights = c(1),
                   newpage = F)
      dev.off()
    }
  )
}

# run the application 
shinyApp(ui = ui, server = server)

