#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(bslib)
library(shinycssloaders)
library(catmaply)
library(scales)


load("data_for_app/Olympics.RData")


ui <-  navbarPage(
    theme = bs_theme(bootswatch = "simplex"),
    title="The Evolution of the Olympic Games 1896-2016",
    tabPanel(
        title="Participating Countries and Continents", id="countries",
        fluidRow(
            column(
                2,
                selectInput("seasonCountries",
                            "Season",
                            choices=c(Summer="Summer", Winter = "Winter"),
                            selected="summer")
            )
        ),
        withSpinner(plotlyOutput("countriesScatter", height = 700))
    ),
    tabPanel(
      title="Gender Differences in Participation", id="gender",
      fluidRow(
        column(
          2,
          selectInput("seasonGender",
                      "Season",
                      choices=c(Summer="Summer", Winter = "Winter"),
                      selected="summer")
        ),
        column(
          2,
          selectInput("barModeGender",
                      "Type of bar chart",
                      choices=c(Grouped="group", Stacked = "stack"),
                      selected="group")
        )
      ),
      withSpinner(plotlyOutput("genderBars", height=350)),
      br(),
      withSpinner(plotlyOutput("genderSport", height=350)),
    ),
    tabPanel(
      title="Appearing, Disappearing and Reappearing Sports", id="appear",
      fluidRow(
        column(
          2,
          selectInput("seasonSportAppear",
                      "Season",
                      choices=c(Summer="Summer", Winter = "Winter"),
                      selected="summer")
        )
      ),
      withSpinner(plotlyOutput("sportAppearance", height = 730))
    ),
    tabPanel(
        title="Number of Sports and Events", id="athletesportevent",
        fluidRow(
            column(
                2,
                selectInput("seasonSportsEvents",
                            "Season",
                            choices=c(Summer="Summer", Winter = "Winter"),
                            selected="summer")
            )
        ),
        withSpinner(plotlyOutput("sportsEvents", height = 350)),
        br(),
        withSpinner(plotlyOutput("events", height = 350))
    )
)


server <- function(input, output, session) {

    ##############################################################
    ## Participating Countries and Continents
    ##############################################################

    season_countries <- reactive({
      if (input$seasonCountries == "Summer") {
        return(summer_countries)
      } else {
        return(winter_countries)
      }
    })

    customColors <- c("violet", "forestgreen", "blue", "red", "turquoise", "gold3", "gray")

    output$countriesScatter <- renderPlotly({
        plot_ly(season_countries(), colors=customColors, alpha=0.5) %>%
            add_markers(x=~N_sports_by_games_NOC_name, y=~N_events_by_games_NOC_name,
                        size=~N_athletes_by_games_NOC_name, frame=~Year,
                        ids=~NOC_name, color=~Continent,
                        text=~paste0(NOC_name, "\n", "Sports, N = ", N_sports_by_games_NOC_name, "\n",
                                     "Events, N = ", N_events_by_games_NOC_name, "\n",
                                     "Athletes, N = ", N_athletes_by_games_NOC_name),
                        hoverinfo = 'text') %>%
            animation_opts(frame=2000, transition = 1000, easing = "linear",
                           redraw = FALSE, mode="immediate") %>%
            animation_slider(currentvalue = list(prefix = "Year: ", font = list(color="red"))) %>%
            layout(title=paste0("Country Participation: Number of Sports & Events Participated in and Number of Representing Athletes\n",
                                "(use box or lasso selection to track a country or countries across the years)"),
                   xaxis=list(title="Number of sports participated in"),
                   yaxis=list(title="Number of events participated in"),
                   legend= list(title=list(text='<b> Continent </b>'),
                                itemsizing='constant'))
    })

    ##############################################################
    ## Gender Differences in Participation
    ##############################################################

    current_yearGender <- reactiveVal(NULL)

    observeEvent(event_data("plotly_click", source = "genderBars"), {
      current_yearGender(event_data("plotly_click", source = "genderBars")$x)
    })

    observeEvent(event_data("plotly_doubleclick", source = "genderBars"), {
      current_yearGender(NULL)
    })

    observeEvent(input$seasonGender, current_yearGender(NULL))

    yrsGender <- reactive({
      if (input$seasonGender == "Summer") {
        return(summerYrs)
      } else {
        return(winterYrs)
      }
    })

    city_gamesGender <- reactive({
      if (input$seasonGender == "Summer") {
        return(summerCityGames)
      } else {
        return(winterCityGames)
      }
    })

    athletes <- reactive({
      games %>%
        select(Games, Gender, Year, Season, City, N_athletes_by_games_gender) %>%
        distinct(Games, Gender, .keep_all = TRUE) %>%
        filter(Season == input$seasonGender)
    })

    output$genderBars <- renderPlotly({
      athletes() %>%
        highlight_key(~Year) %>%
        plot_ly(colors="Set1", source = "genderBars") %>%
        add_bars(x=~Year, y~N_athletes_by_games_gender, color=~Gender,
                 text=~paste0(City, " ", Games, "\n",
                              Gender, ", N = ", N_athletes_by_games_gender),
                 hoverinfo = 'text') %>%
        layout(barmode = input$barModeGender,
               title=paste0("Gender Differences in Participation by Year\n",
                            "(click on a bar to see sport specific participation for a specified Olympics, double-click chart outside the bar to unselect)"),
               xaxis=list(tickvals = yrsGender()),
               yaxis=list(title="Number of athletes"),
               showlegend=TRUE) %>%
        highlight(
          on = "plotly_click",
          off = "plotly_doubleclick",
          opacityDim=0.2,
          selected = attrs_selected(showlegend = FALSE)
        )
    })

    output$genderSport <- renderPlotly({
      if (is.null(current_yearGender())) return(NULL)

      genderColors <- "Set1"
      if (current_yearGender() == 1896) {
        # Ensure that males retain their color since only they are present in 1896
        genderColors <- c("#377EB8", "#377EB8", "#377EB8")
      }

      games %>%
        filter(Season == input$seasonGender & Year == current_yearGender()) %>%
        droplevels() %>%
        group_by(Gender, Sport) %>%
        summarise(N=n_distinct(ID)) %>%
        plot_ly(colors=genderColors) %>%
        add_bars(x=~Sport, y=~N, color=~Gender) %>%
        layout(barmode = input$barModeGender,
               hovermode = 'compare',
               title=paste0("Gender Differences in Participation by Sport: ",
                            city_gamesGender()[which(current_yearGender() == yrsGender())]),
               yaxis=list(title="Number of athletes"),
               showlegend=TRUE)
    })

    ##############################################################
    ## Appearing, Disappearing and Reappearing Sports
    ##############################################################

    appear_font_size <- reactive({
      if (input$seasonSportAppear == "Summer") {
        return(10)
      } else {
        return(12)
      }
    })

    appearances <- reactive({
      sports <- games %>%
        filter(Season == input$seasonSportAppear) %>%
        droplevels() %>%
        group_by(Games, Sport, .drop = FALSE) %>%
        summarise(Exists=n() > 0) %>%
        ungroup() %>%
        mutate(Appeared=case_when(Exists ~ "Yes",
                                  TRUE ~ "No"),
               Exists=as.numeric(Exists),
               Games_num=as.integer(Games))

      left_join(sports, cities_games, by="Games")
    })

    output$sportAppearance <- renderPlotly({
      appear <- appearances()

      catmaply(appear,
               x=Games,
               x_order=Games_num,
               x_side="bottom",
               x_range=length(unique(appear$Games)),
               x_tickangle=30,
               y=Sport,
               z=Exists,
               color_palette=hue_pal(),
               font_size=appear_font_size(),
               legend_col = Appeared,
               rangeslider = FALSE,
               hover_template = paste0(Sport, "\n", City, " ", Games))%>%
        layout(title="Sport Appearances (click on legend or toggle spike lines to improve readability)",
               legend=list(title=list(text='<b>Appeared</b>')))
    })

    ##############################################################
    ## Number of Sports and Events
    ##############################################################

    current_yearSport <- reactiveVal(NULL)

    observeEvent(event_data("plotly_click", source = "sportsEvents"), {
      current_yearSport(event_data("plotly_click", source = "sportsEvents")$x)
    })

    observeEvent(event_data("plotly_doubleclick", source = "sportsEvents"), {
      current_yearSport(NULL)
    })

    observeEvent(input$seasonSportsEvents, current_yearSport(NULL))

    yrsSport <- reactive({
      if (input$seasonSportsEvents == "Summer") {
        return(summerYrs)
      } else {
        return(winterYrs)
      }
    })

    city_gamesSport <- reactive({
      if (input$seasonSportsEvents == "Summer") {
        return(summerCityGames)
      } else {
        return(winterCityGames)
      }
    })

    sports_events <- reactive({
      games %>%
        select(Games, Year, Season, City, N_sports_by_games, N_events_by_games) %>%
        distinct(Games, .keep_all = TRUE) %>%
        filter(Season == input$seasonSportsEvents) %>%
        pivot_longer(cols=c(N_sports_by_games, N_events_by_games),
                     names_to="Variable",
                     values_to="N") %>%
            mutate(Variable=case_when(Variable == "N_sports_by_games" ~ "Sports",
                                      Variable == "N_events_by_games" ~ "Events"))
    })

    output$sportsEvents <- renderPlotly({
      sports_events() %>%
        highlight_key(~Year) %>%
        plot_ly(colors="Dark2", source = "sportsEvents") %>%
            add_bars(x=~Year, y~N, color=~Variable,
                     text=~paste0(City, " ", Games, "\n", Variable, ", N = ", N),
                     hoverinfo = 'text') %>%
            layout(title=paste0("Number of Sports and Events by Year\n",
                                "(click on a bar to see details for a specified Olympics, double-click chart outside the bar to unselect)"),
                   barmode = "group",
                   bargroupgap=0.3,
                   xaxis=list(tickvals = yrsSport()),
                   yaxis=list(title="Number of sports/events")) %>%
        highlight(
          on = "plotly_click",
          off = "plotly_doubleclick",
          opacityDim=0.2,
          selected = attrs_selected(showlegend = FALSE)
        )
    })

    output$events <- renderPlotly({
      if (is.null(current_yearSport())) return(NULL)

      games %>%
        filter(Season == input$seasonSportsEvents & Year == current_yearSport()) %>%
        droplevels() %>%
        group_by(Sport) %>%
        summarise(N=n_distinct(Event)) %>%
        ungroup() %>%
        plot_ly(colors="Set1") %>%
        add_bars(x=~Sport, y=~N, color=~Sport,
                 text=~paste0(Sport, "\n", "Events, N = ", N),
                 hoverinfo = 'text') %>%
        layout(barmode = "group",
               title=paste0("Number of Events by Sport: ",
                            city_gamesSport()[which(current_yearSport() == yrsSport())]),
               yaxis=list(title="Number of events"),
               showlegend=TRUE)
    })
}


# Run the application
shinyApp(ui = ui, server = server)
