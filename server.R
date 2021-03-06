packageList = c('ggplot2', 'shiny', 'plyr', 'reshape', 'resahpe2', 'ggvis')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

library(ggplot2)
library(shiny)
library(plyr)
library(reshape)
library(reshape2)
library(ggvis)

server <- function(input, output, session) {
  assign("play", T, envir = .GlobalEnv)
  autoInvalidate <- reactiveTimer(500)
  
  getData <- function() {
    df_pop <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv')
    # Remove weird x column at end
    df_pop <- df_pop[,1:(ncol(df_pop)-1)]
    
    df_fert <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')
    df_life <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
    
    melt_frames <- function(df) {
      names(df) <- c('country', 'country_code', 'indicator', 'indicator_code', seq(1960,2016))
      mdf <- melt(df, id.vars = c('country', 'country_code', 'indicator', 'indicator_code'))
      names(mdf) <- c('country', 'country_code', 'indicator', 'indicator_code', 'year', 'value')
      return(mdf)
    }
    
    df_pop <- melt_frames(df_pop)
    df_fert <- melt_frames(df_fert)
    df_life <- melt_frames(df_life)
    
    merge_cols <- c('country', 'country_code', 'year')
    full_df <- merge(merge(df_pop, df_fert, merge_cols),df_life, merge_cols)
    names(full_df) <- c('country', 'country_code', 'year', 'pop_name', 'pop_code', 'population',
                        'fert_name', 'fert_code', 'fertility', 'life_name', 'life_code', 'life')
    
    country_meta <- read.csv('Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')[,1:2]
    names(country_meta) <- c('country_code', 'region')
    
    full_df <- merge(full_df, country_meta[,1:2], 'country_code')
    full_df <- subset(full_df, region != '')
    full_df$region <- droplevels(full_df$region)
    assign("full_df", full_df, envir = .GlobalEnv)
  }
  
  
  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    if (play) {
      updateSliderInput(session, 'year', 'Please Select a Year', min = 1960, max = 2014, value = input$year + 1)
    }
  })
  
  observe({
    # When the Play Button is Pressed, change the state of the global play variable
    input$play
    assign("play", !play, envir = .GlobalEnv)
    if (play) {
      updateActionButton(session, 'play', 'Pause')
    } 
    else {
      updateActionButton(session, 'play', 'Play')
    }
    
  })
  
  output$region <- renderUI({  
    if(!exists('full_df')) {
      getData()
    }
    selectInput('regions', 'Select Regions to See (Default is All)', 
                choices = unique(full_df$region), 
                multiple = T)
  })  
  
  output$country <- renderUI({  
    if(!exists('full_df')) {
      getData()
    }
    x <- full_df
    if(!is.null(input$regions)) {
      x <- subset(x, region %in% input$regions)
    }
    selectInput('countries', 'Select Countries to Highlight', 
                choices = unique(x$country),
                multiple = T)
  })  
  
  output$df <- renderDataTable({
    plot_df <- subset(full_df, year==input$year)
    if(!is.null(input$regions)) {
      plot_df <- subset(plot_df, region %in% input$regions)
    }
    if(!is.null(input$countries)) {
      plot_df <- subset(plot_df, country %in% input$countries)
    }
    keep_cols <- c('country', 'year', 'population', 'fertility', 'life')
    show_df <- plot_df[,names(plot_df) %in% keep_cols]
    names(show_df) <- c('Country', 'Year', 'Population', 'Fertility Rate (Births per Woman)', 'Life Expectancy at Birth')
    show_df
  })
  
  plot <- reactive({
    if(!exists('full_df')) {
      getData()
    }
    yr <- input$year
    plot_df <- subset(full_df, year==yr)
    if(!is.null(input$regions)) {
      plot_df <- subset(plot_df, region %in% input$regions)
    }
    if (is.null(input$countries)) {
      country_list <- c()
    } else {
      country_list <- input$countries
    }
  
    axis_lims <- list(life = c(10, 90), fertility=c(0,9), population= c(0, 1500000000))
    
    xmin = axis_lims
    xmax = ceiling(max(full_df[,names(full_df)==input$xaxis]))
    ymin = floor(min(full_df[,names(full_df)==input$yaxis]))
    ymax = ceiling(max(full_df[,names(full_df)==input$yaxis]))
    
    choices <- list(Life_Expectancy='life', Fertility_Rate='fertility', Population='population')
    xlab = gsub('_', ' ', names(choices[choices==input$xaxis]))
    ylab = gsub('_', ' ', names(choices[choices==input$yaxis]))
    
    if(input$yaxis == 'population') {
      y_push <- 100
    } else{
      y_push <- 50
    }
    
    plot_df <- subset(plot_df, year==yr)

    plot_df %>% 
      subset(!is.na(plot_df[,names(plot_df)==input$xaxis])) %>% 
      subset(!is.na(plot_df[,names(plot_df)==input$yaxis])) %>%
      ggvis(prop("x",as.name(input$xaxis)), prop("y",as.name(input$yaxis)), fill = ~region,
            fillOpacity := 0.5, fillOpacity.hover := 1,
            stroke := NA, stroke.hover = ~region, strokeWidth := 4, strokeOpacity := 0.7) %>% 
      scale_numeric("size", range = c(10, input$popSize), nice = FALSE) %>%
      layer_points(prop("size",as.name(input$plotscale)), key := ~country) %>%
      ggvis::hide_legend('size') %>%
      scale_numeric("x", domain = axis_lims[names(axis_lims) == input$xaxis][[1]], nice = FALSE) %>%
      scale_numeric("y", domain = axis_lims[names(axis_lims) == input$yaxis][[1]], nice = FALSE) %>%
      add_axis("x", title = xlab, title_offset = 50) %>%
      add_axis("y", title = ylab, title_offset = y_push) %>%
      add_axis("x", orient = "top", ticks = 0, title_offset = 50,
               title = paste(xlab, "vs", ylab, "In", yr),
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0))) %>% 
      add_tooltip(function(data){
        paste0("Country: <b>", data$country, "</b><br>",
               "Region: <b>", data$region, "</b><br>",
               "Population: <b>", prettyNum(data$population, mode='integer',big.mark=",", big.interval=3, scientific=FALSE), "</b><br>",
               "Fertility Rate: <b>", data$fertility, "</b>")
      }, "click") %>% 
      add_tooltip(function(data){
        paste0("Country: <t><b>", as.character(data$country), "</b>")
      }, "hover") %>% 
      set_options(width = 1000, height = 600, renderer = "svg") %>% 
      layer_text(text := ~country, data = subset(plot_df, country %in% country_list))
  })
  
  plot %>% 
    bind_shiny('plot', 'plot_stuff')
  
  
}

  