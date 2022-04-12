library(shiny)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(spsComps)
library(plotly)
library(knitr)
library(shinydisconnect)

my_msg <- disconnectMessage(
  text = "Session expirée. Cliquer pour réinitialiser la connexion",
  refresh = "Rafraîchir",
  background = "#FFFFFF",
  colour = "#444444",
  refreshColour = "#B80713",
  overlayColour = "#968787",
  overlayOpacity = 0.90,
  width = "full",
  top = "center",
  size = 34,
  css = "font: italic 1.2em \"Fira Sans\", serif;"
)

shinyUI(function(req) {
  navbarPage(
    inverse = T,
    header = tagList(
      useShinyjs(),
      my_msg,
      withMathJax(),
      div(
        id = "author_name_div",
        h3(id = "author_name_h3", "Idriss . S"),
        h5(id = "author_name_h5", "12 april 2022")
      ),
      includeCSS("www/style.css"),
      actionButton('disconnect', 'Disconnect the app')
    ),
    footer =tagList(
      div (
        id = "my_well_3",
        style = "position: relative;height:100px; align:center;",
        div(id = "wrapper_border_4"),
        align = "center",
        
        div(id = "CC_licence",
            div(
              HTML(
                '<p style="top: 0%;bottom: 0%;left: 0%;right: 0%;
                line-height: 60px;position:absolute;margin:0;text-align:center;" 
                xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/">
                <a class="cc_links" property="dct:title" rel="cc:attributionURL" 
                href="https://shiny.sekrane.fr/C9_W4/">Course 9 - W4 assignement</a>
                by <a class="cc_links" rel="cc:attributionURL dct:creator" property="cc:attributionName" 
                href="https://www.linkedin.com/in/idriss-s/">Idriss SEKRANE</a> is licensed under 
                <a class="cc_links" href="http://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" 
                rel="license noopener noreferrer" style="display:inline-block;">
                Attribution-NonCommercial-ShareAlike 4.0 International
                <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" 
                src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1">
                <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" 
                src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1">
                <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" 
                src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1">
                <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" 
                src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1"></a></p>'
              )
            ), )
        
      )
    ),
    "Coursera : course 9 assignement",
    id = "nav_id",
    tabPanel(
      "I. Data to study",
      id = "data_id",
      fluidRow(
        id = "row_1",
        div(id="container_github",
            div(id="wrapper_border_3"),
            div(id="github_repo",
                h3(id="h3_github", class = "H3_custom","Link to my Github repositorie :"),
                br(),
                a(class="cc_links repo_link",target="_blank", href="https://github.com/udriss/Reproducible-Pitch/tree/main/Shiny-App","Click here"),
                p("Access my repositorie containing server.R and ui.R files."))
        ),
        h1(class = "H1_custom", "Part I : Introduction of data to be regressed"),
        br(),
        h3(class = "H3_custom", "Documentation : how to use tricks"),
        div(
          class = "doc_div",
          tags$ol(
            tags$li("click on \"Add extra point\" to add new input field ;"),
            tags$li("click on \"Erase one point\" to delete the last field ;"),
            tags$li("click on \"Erase all points\" to delete ALL fields ;"),
            hr(),
            tags$li(
              "when enough points are placed with the desired coordinates,
            click on \"Send data to statistical analysis\" to get statistical analysis."
            ),
            hr(),
            tags$li("to make a plot, click above on the tab \"II. Linear regression\".")
          ),
        ),
        column(width = 3),
        column(
          width = 3,
          id = "points_x",
          align = "center",
          h3(class = "H3_custom", "X coordinate")
        ),
        column(
          width = 3,
          id = "points_y",
          align = "center",
          h3(class = "H3_custom", "Y coordinate")
        ),
        column(width = 3)
      ),
      fluidRow(
        id = "row_2",
        align = "center",
        hr(),
        column(
          width = 6,
          fluidRow(
            actionButton("add", "Add extra point"),
            actionButton("add_5", "Add five extra points"),
            hr()
          ),
          fluidRow(
            column(width = 6,
                   actionButton("rmv", "Erase one point")),
            column(width = 6,
                   actionButton("rmv_all", "Erase all points"))
            
          )
        ),
        column(
          width = 6,
          id = "send_col_1",
          actionButton("send_data", "Send data to statistical analysis"),
          
        ),
      ),
      fluidRow(
        id = "row_3",
        hr(),
        column(width = 2),
        column(width = 8,
               id = "send_col_2",
               uiOutput("remarques"), ),
        column(width = 2),
      ),
      fluidRow(
        id = "row_4",
        align = "center",
        column(
          id = "table_col",
          width = 6,
          br(),
          tableOutput("user_data_table"),
        ),
        column(
          id = "stat_col",
          align = "left",
          width = 6,
          br(),
          tableOutput("stat_sum")
        )
      )
    ),
    tabPanel(
      "II. Linear regression",
      id = "reg_id",
      fluidRow(
        id = "row_5",
        h1(class = "H1_custom",
           "Part II : regression to the mean of the data array"),
        br(),
        h3(class = "H3_custom", "Documentation : how to use tricks"),
        div(class = "doc_div",
            tags$ol(
              tags$li("click on \"Render a plot\" to make a new reactive plot ;"),
              hr(),
              tags$li("don't forget to put some lables ont he left !")
            ), ),
        br(),
        fluidRow(
          column(
            width = 6,
            id = "plot_col_1_1",
            align = "center",
            h3(class = "H3_custom", "Labels to add :"),
            textInput(
              "xlab",
              label = NULL,
              value = "",
              placeholder = "Label for X coordinate"
            ),
            textInput(
              "ylab",
              label = NULL,
              value = "",
              placeholder = "Label for Y coordinate"
            ),
            textInput(
              "main_title",
              label = NULL,
              value = "",
              placeholder = "Main title"
            ),
          ),
          column(
            width = 6,
            id = "plot_col_1_2",
            align = "center",
            actionButton("make_plot", "Render a plot"),
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 12,
            id = "plot_col_2",
            align = "center",
            fluidRow(
              id = "by_hand_row",
              h3(class = "H3_custom", "Reminder of statistical formulas with calculations :"),
              br(),
              uiOutput("by_hand"),
              
            ),
            fluidRow(
              id = "summary_row",
              br(),
              h3(class = "H3_custom", "Statistical model with the R language :"),
              verbatimTextOutput("summary"),
            ),
            fluidRow(
              id = "results_row",
              br(),
              h3(class = "H3_custom", "Graph of linear regression :"),
              uiOutput("results"),
            ),
            fluidRow(
              id = "plot_row",
              br(),
              plotlyOutput("plot", width = '800px', height = '680px'),
            ),
            
          )
        )
      )
    )
  )
})