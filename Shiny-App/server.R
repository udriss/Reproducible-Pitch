library(shiny)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(spsComps)
library(plotly)
library(knitr)
library(shinydisconnect)
library(shinybusy)

shinyServer(function(input, output, session) {
  useShinyjs()
  counter <- reactiveValues(value = 0)
  x_val <- reactiveValues(new = 0)
  y_val <- reactiveValues(new = 0)
  
  user_values <-
    reactiveValues(df = data.frame(coord_x = numeric(), coord_y = numeric()))
  rdm_values <-
    reactiveValues(df = data.frame(x = numeric(), y = numeric()))
  
  observeEvent(input$add, ignoreInit = T,
               {
                 output$remarques <- renderUI({
                   withMathJax()
                 })
                 output$stat_sum <- renderUI({
                   withMathJax()
                 })
                 counter$value <- counter$value + 1
                 x_val$new <- round(runif(1, 2, 18), 1)
                 y_val$new <-
                   round(3.2 * x_val$new + 3 * rnorm(1), 1)
                 rdm_values$df <-
                   rdm_values$df %>% bind_rows(new_coord = data.frame(x = x_val$new, y = y_val$new))
                 insertUI(
                   selector = paste0("#points_x"),
                   where = "beforeEnd",
                   ui = numericInput(
                     paste0("point_x_", counter$value),
                     withMathJax(paste0("\\(X_{",
                                        counter$value, "}\\)")),
                     value = x_val$new,
                     step = .05
                   )
                 )
                 insertUI(
                   selector = paste0("#points_y"),
                   where = "beforeEnd",
                   ui = numericInput(
                     paste0("point_y_", counter$value),
                     withMathJax(paste0("\\(Y_{",
                                        counter$value, "}\\)")),
                     value = y_val$new,
                     step = .05
                   )
                 )
               })
  
  observeEvent(input$add_5, ignoreInit = T,
               {
                 output$remarques <- renderUI({
                   withMathJax()
                 })
                 output$stat_sum <- renderUI({
                   withMathJax()
                 })
                 removeUI(selector = "#user_data_table")
                 removeUI(selector = "#summary")
                 removeUI(selector = "#plot")
                 removeUI(selector = "#results")
                 removeUI(selector = "#by_hand")
                 for (i in 1:5) {
                   counter$value <- counter$value + 1
                   x_val$new <- round(runif(1, 2, 18), 1)
                   y_val$new <-
                     round(3.2 * x_val$new + 3 * rnorm(1), 1)
                   rdm_values$df <-
                     rdm_values$df %>% bind_rows(new_coord = data.frame(x = x_val$new, y = y_val$new))
                   insertUI(
                     selector = paste0("#points_x"),
                     where = "beforeEnd",
                     ui = numericInput(
                       paste0("point_x_", counter$value),
                       withMathJax(paste0("\\(X_{",
                                          counter$value, "}\\)")),
                       value = x_val$new,
                       step = .05
                     )
                   )
                   insertUI(
                     selector = paste0("#points_y"),
                     where = "beforeEnd",
                     ui = numericInput(
                       paste0("point_y_", counter$value),
                       withMathJax(paste0("\\(Y_{",
                                          counter$value, "}\\)")),
                       value = y_val$new,
                       step = .05
                     )
                   )
                 }
                 
                 
               })
  
  
  
  
  observeEvent(input$disconnect, ignoreInit = T, {
    session$close()
  })
  
  
  setBookmarkExclude(c("send_data",
                       "add_5",
                       "add",
                       "rmv"))
  
  onBookmark(function(state) {
    state$values$counter <- counter$value
    state$values$df <- rdm_values$df
  })
  
  onBookmarked(updateQueryString)
  
  
  onRestore(function(state) {
    if (!is.null(state$values$counter)) {
      if (state$values$counter == 0) {
        
      }
      
      else {
        counter$value <- state$values$counter
        for (i in 1:counter$value) {
          insertUI(
            selector = paste0("#points_x"),
            where = "beforeEnd",
            ui = numericInput(
              paste0("point_x_", i),
              withMathJax(paste0("\\(X_{",
                                 i, "}\\)")),
              value = x_val$new,
              step = .05
            )
          )
          insertUI(
            selector = paste0("#points_y"),
            where = "beforeEnd",
            ui = numericInput(
              paste0("point_y_", i),
              withMathJax(paste0("\\(Y_{",
                                 i, "}\\)")),
              value = y_val$new,
              step = .05
            )
          )
        }
        user_values$df <- state$values$df
        if (sum(is.na(user_values$df)) > 0) {
          shinyCatch(stop(
            "Incorrect (non-numeric) characters were introduced.
           The corresponding points will not be taken into account in the regression towards the mean."
          ))
        }
        
        output$user_data_table <-
          renderTable(
            striped = T,
            hover = T,
            rownames = T,
            digits = 5,
            {
              user_values$df
            }
          )
        colnames(user_values$df) <-
          c("X coordinate", "Y coordinate")
        
        output$stat_sum <- renderUI({
          withMathJax(
            paste0("\\(\\bar{x} =\\) ", round(mean(
              user_values$df[, 1]
            ), 3)),
            br(),
            paste0("\\(\\sigma_X =\\) ", round(sd(
              user_values$df[, 1]
            ), 3)),
            br(),
            paste0("\\( Var_X =\\) ", round(var(
              user_values$df[, 1]
            ), 3)),
            br(),
            paste0("\\(\\bar{y} =\\) ", round(mean(
              user_values$df[, 2]
            ), 3)),
            br(),
            paste0("\\(\\sigma_Y =\\) ", round(sd(
              user_values$df[, 2]
            ), 3)),
            br(),
            paste0("\\(Var_Y =\\) ", round(var(
              user_values$df[, 2]
            ), 3)),
            br(),
            paste0("\\(n =\\) ", length(user_values$df[, 1])),
            br(),
            paste0("\\(Cov_{XY} =\\) ", round(
              cov(user_values$df[, 1],
                  user_values$df[, 2]), 3
            )),
            br(),
            br(),
            paste0(
              "\\(Cor_{XY} =\\frac{Cov_{XY}}{\\sigma_Y~\\times~\\sigma_X} =\\) ",
              round(cor(
                user_values$df[, 1], user_values$df[, 2]
              ), 3)
            )
          )
        })
        
        output$remarques <- renderUI({
          if (length(user_values$df[, 1]) < 6 |
              length(user_values$df[, 2]) < 6) {
            if (((sum(user_values$df[1] == 0) > 0) |
                 (sum(user_values$df[2] == 0) > 0))) {
              withMathJax("Please note two important points:",
                          tags$ul(
                            tags$li("some entries are zero. Have you checked the data?"),
                            tags$li(
                              "the number of values entered is insufficient.
                               At least 6 values are needed for a sufficiently reliable static approach."
                            )
                            
                          ))
            }
            else {
              withMathJax("Please note an important note:",
                          tags$ul(
                            tags$li(
                              "the number of values entered is insufficient.
                               At least 6 values are needed for a sufficiently reliable static approach."
                            ),
                          ))
            }
            
          }  else {
            withMathJax("Please note two important points:",
                        tags$ul(
                          tags$li("no entry is null. That's a good point."),
                          tags$li(
                            "there are more than 6 values, the statistical approach is
                             potentially sufficiently reliable. This is another good point."
                          ),
                        ))
          }
        })
      }
      
      if (input[["make_plot"]] > 0) {
        if (counter$value == 0) {
          shinyCatch(warning("There is not enought points (0) to make plot."))
        }
        else {
          show_modal_progress_line()
          output$plot <- renderPlotly({
            x <- user_values$df[, 1]
            y <- user_values$df[, 2]
            
            fit <- lm(y ~ x)
            dat <- data.frame(x, y)
            p <- ggplot(dat, aes(x = x, y = y)) +
              geom_point(size = 3, color = "#806008") +
              stat_smooth(method = "lm",
                          se = T,
                          show.legend = T) +
              ylab(input$ylab) +
              xlab(input$xlab) +
              ggtitle(input$main_title) +
              theme_linedraw() +
              theme(plot.background = element_rect(
                fill = "#f7d063",
                color = "black",
                size = 8
              ))
            ggplotly(p)
            
            
          })
          
          output$by_hand <- renderUI({
            x <- user_values$df[, 1]
            y <- user_values$df[, 2]
            fit <- lm(y ~ x)
            withMathJax(
              paste0(
                "\\(Slope ~: ~\\hat{\\beta}_1 = \\dfrac{\\sum^n_{i = 1}\\big(x_i - \\bar{x} \\big)^2 -
             \\sum^n_{i = 1} \\big(y_i - \\bar{y} \\big)^2}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ",
                round(fit$coef[[2]], 3)
              ),
              br(),
              paste0(
                "\\(Intercept ~\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ",
                round(fit$coef[[1]], 3)
              ),
              br(),
              br(),
              paste0(
                "\\( \\Rightarrow ~Statistical ~model ~: ~y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ",
                round(fit$coef[[1]], 3),
                " + ",
                round(fit$coef[[2]], 3),
                "\\( x \\)"
              )
            )
          })
          output$summary <- renderPrint({
            x <- user_values$df[, 1]
            y <- user_values$df[, 2]
            fit <- lm(y ~ x)
            summary(fit)
          })
          output$results <- renderUI({
            x <- user_values$df[, 1]
            y <- user_values$df[, 2]
            fit <- lm(y ~ x)
            withMathJax(
              paste0(
                "\\( R^2 = \\) ~adjusted :",
                round(summary(fit)$adj.r.squared, 3),
                ",~ \\( \\beta_0 = \\) ",
                round(fit$coef[[1]], 3),
                ",~ \\( \\beta_1 = \\) ",
                round(fit$coef[[2]], 3),
                ", p-value ",
                "\\( = \\) ",
                signif(summary(fit)$coef[2, 4], 3)
              )
            )
          })
          remove_modal_progress()
        }
      }
    }
    
    
  })
  
  
  
  
  
  
  # Traitement statistiques des données fournies et affichages
  observeEvent(input$send_data, ignoreInit = T,
               {
                 if (counter$value == 0) {
                   shinyCatch(stop(
                     "There are not enough points (0) to perform a statistical study."
                   ))
                 }
                 else {
                   user_values <-
                     reactiveValues(df = data.frame(coord_x = numeric(), coord_y = numeric()))
                   for (i in 0:counter$value) {
                     user_values$df <- user_values$df %>%
                       bind_rows(data.frame(coord_x = input[[paste0("point_x_", i)]],
                                            coord_y = input[[paste0("point_y_", i)]]))
                     
                   }
                   if (sum(is.na(user_values$df)) > 0) {
                     shinyCatch(stop(
                       "Incorrect (non-numeric) characters were introduced.
                      The corresponding points will not be taken into account in the
                      regression to the mean."
                     ))
                   }
                   
                   output$user_data_table <-
                     renderTable(
                       striped = T,
                       hover = T,
                       rownames = T,
                       digits = 5,
                       {
                         user_values$df
                       }
                     )
                   colnames(user_values$df) <-
                     c("X coordinate", "Y coordinate")
                   output$stat_sum <- renderUI({
                     withMathJax(
                       paste0("\\(\\bar{x} =\\) ", round(mean(
                         user_values$df[, 1]
                       ), 3)),
                       br(),
                       paste0("\\(\\sigma_X =\\) ", round(sd(
                         user_values$df[, 1]
                       ), 3)),
                       br(),
                       paste0("\\( Var_X =\\) ", round(var(
                         user_values$df[, 1]
                       ), 3)),
                       br(),
                       paste0("\\(\\bar{y} =\\) ", round(mean(
                         user_values$df[, 2]
                       ), 3)),
                       br(),
                       paste0("\\(\\sigma_Y =\\) ", round(sd(
                         user_values$df[, 2]
                       ), 3)),
                       br(),
                       paste0("\\(Var_Y =\\) ", round(var(
                         user_values$df[, 2]
                       ), 3)),
                       br(),
                       paste0("\\(n =\\) ", length(user_values$df[, 1])),
                       br(),
                       paste0("\\(Cov_{XY} =\\) ", round(
                         cov(user_values$df[, 1],
                             user_values$df[, 2]), 3
                       )),
                       br(),
                       br(),
                       paste0(
                         "\\(Cor_{XY} =\\frac{Cov_{XY}}{\\sigma_Y~\\times~\\sigma_X} =\\) ",
                         round(cor(
                           user_values$df[, 1], user_values$df[, 2]
                         ), 3)
                       )
                     )
                   })
                   output$remarques <- renderUI({
                     if (length(user_values$df[, 1]) < 6 |
                         length(user_values$df[, 2]) < 6) {
                       if (((sum(user_values$df[1] == 0) > 0) |
                            (sum(user_values$df[2] == 0) > 0))) {
                         withMathJax("Please note two important points:",
                                     tags$ul(
                                       tags$li("some entries are zero. Have you checked the data?"),
                                       tags$li(
                                         "the number of values entered is insufficient.
                               At least 6 values are needed for a sufficiently reliable static approach."
                                       )
                                       
                                     ))
                       }
                       else {
                         withMathJax("Please note an important note:",
                                     tags$ul(
                                       tags$li(
                                         "the number of values entered is insufficient.
                               At least 6 values are needed for a sufficiently reliable static approach."
                                       ),
                                     ))
                       }
                       
                     }  else {
                       withMathJax("Please note two important points:",
                                   tags$ul(
                                     tags$li("no entry is null. That's a good point."),
                                     tags$li(
                                       "there are more than 6 values, the statistical approach is
                             potentially sufficiently reliable. This is another good point."
                                     ),
                                   ))
                     }
                     
                   })
                   
                   insertUI(
                     selector = "#table_col",
                     where = "beforeEnd",
                     ui = tableOutput("user_data_table")
                   )
                   insertUI(selector = "#stat_col",
                            where = "beforeEnd",
                            ui = tableOutput("stat_sum"))
                   insertUI(
                     selector = "#send_col_2",
                     where = "beforeEnd",
                     ui = uiOutput("remarques")
                   )
                   
                   if (input$rmv_all > 0) {
                     insertUI(
                       selector = "#table_col",
                       where = "beforeEnd",
                       ui = tableOutput("user_data_table")
                     )
                     insertUI(
                       selector = "#stat_col",
                       where = "beforeEnd",
                       ui = tableOutput("stat_sum")
                     )
                     insertUI(
                       selector = "#send_col_2",
                       where = "beforeEnd",
                       ui = uiOutput("remarques")
                     )
                   }
                   session$doBookmark()
                 }
                 
                 
                 
               })
  # Gesion des supression des inputs
  observeEvent(input$rmv, ignoreInit = T, {
    if (counter$value < 1) {
      
    }
    else {
      removeUI(selector = paste0("div:has(> #point_y_", counter$value, ")"))
      removeUI(selector = paste0("div:has(> #point_x_", counter$value, ")"))
      counter$value <- counter$value - 1
      rdm_values$df <- head(rdm_values$df, -1)
    }
  })
  
  observeEvent(input$rmv_all, ignoreInit = T, {
    output$remarques <- renderUI({
      withMathJax()
    })
    output$stat_sum <- renderUI({
      withMathJax()
    })
    temp <- counter$value
    counter$value <- 0
    if (temp < 1) {
      
    }
    else {
      removeUI(selector = "#stat_sum")
      removeUI(selector = "#remarques")
      removeUI(selector = "#user_data_table")
      removeUI(selector = "#summary")
      removeUI(selector = "#plot")
      removeUI(selector = "#results")
      removeUI(selector = "#by_hand")
      
      for (i in 1:temp) {
        if (temp > 0) {
          removeUI(selector = paste0("div:has(> #point_y_", i, ")"))
          removeUI(selector = paste0("div:has(> #point_x_", i, ")"))
          rdm_values$df <- head(rdm_values$df, -1)
        }
      }
    }
    session$doBookmark()
  })
  
  
  
  # Plots
  observeEvent(input$make_plot, ignoreInit = T, {
    user_values <-
      reactiveValues(df = data.frame(coord_x = numeric(), coord_y = numeric()))
    for (i in 1:counter$value) {
      user_values$df <- user_values$df %>%
        bind_rows(data.frame(coord_x = input[[paste0("point_x_", i)]],
                             coord_y = input[[paste0("point_y_", i)]]))
      
    }
    if (counter$value == 0) {
      report_warning(title = "Error", text = "No graph to rend.")
      shinyCatch(stop("There is not enought points (0) to make plot."))
    }
    else {
      show_modal_progress_line(
        color = "#b52419",
        height = "10px",
        stroke_width = 5,
        easing = "easeInCirc"
      )
      
      
      update_modal_progress(0.1)
      
      
      
      
      output$plot <- renderPlotly({
        x <- user_values$df[, 1]
        y <- user_values$df[, 2]
        
        fit <- lm(y ~ x)
        dat <- data.frame(x, y)
        p <- ggplot(dat, aes(x = x, y = y)) +
          geom_point(size = 3, color = "#806008") +
          stat_smooth(method = "lm",
                      se = T,
                      show.legend = T) +
          ylab(input$ylab) +
          xlab(input$xlab) +
          ggtitle(input$main_title) +
          theme_linedraw() +
          theme(plot.background = element_rect(
            fill = "#f7d063",
            color = "black",
            size = 8
          ))
        ggplotly(p)
        
        
      })
      update_modal_progress(0.4)
      output$by_hand <- renderUI({
        x <- user_values$df[, 1]
        y <- user_values$df[, 2]
        fit <- lm(y ~ x)
        withMathJax(
          paste0(
            "\\(Slope ~: ~\\hat{\\beta}_1 = \\dfrac{\\sum^n_{i = 1}\\big(x_i - \\bar{x} \\big)^2 -
             \\sum^n_{i = 1} \\big(y_i - \\bar{y} \\big)^2}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ",
            round(fit$coef[[2]], 3)
          ),
          br(),
          paste0(
            "\\(Intercept ~\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ",
            round(fit$coef[[1]], 3)
          ),
          br(),
          br(),
          paste0(
            "\\( \\Rightarrow ~Statistical ~model ~: ~y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ",
            round(fit$coef[[1]], 3),
            " + ",
            round(fit$coef[[2]], 3),
            "\\( x \\)"
          )
        )
      })
      update_modal_progress(0.6)
      Sys.sleep(0.95)
      output$summary <- renderPrint({
        x <- user_values$df[, 1]
        y <- user_values$df[, 2]
        fit <- lm(y ~ x)
        summary(fit)
      })
      update_modal_progress(0.8)
      output$results <- renderUI({
        x <- user_values$df[, 1]
        y <- user_values$df[, 2]
        fit <- lm(y ~ x)
        withMathJax(
          paste0(
            "\\( R^2 = \\) adjusted :",
            round(summary(fit)$adj.r.squared, 3),
            ", \\( \\beta_0 = \\) ",
            round(fit$coef[[1]], 3),
            ", \\( \\beta_1 = \\) ",
            round(fit$coef[[2]], 3),
            ", p-value ",
            "\\( = \\) ",
            signif(summary(fit)$coef[2, 4], 3)
          )
        )
      })
      
      
      if (input$rmv_all > 0) {
        insertUI(
          selector = "#by_hand_row",
          where = "beforeEnd",
          ui = uiOutput("by_hand")
        )
        insertUI(
          selector = "#summary_row",
          where = "beforeEnd",
          ui = verbatimTextOutput("summary")
        )
        insertUI(
          selector = "#results_row",
          where = "beforeEnd",
          ui = uiOutput("results")
        )
        insertUI(
          selector = "#plot_row",
          where = "beforeEnd",
          plotlyOutput("plot", width = '800px', height = '680px'),
        )
      }
      
      
      update_modal_progress(0.99)
      Sys.sleep(1.5)
      remove_modal_progress()
      report_success(title = "Success", text = "Graph loaded")
    }
    session$doBookmark()
    
  })
})
enableBookmarking(store = "server")
