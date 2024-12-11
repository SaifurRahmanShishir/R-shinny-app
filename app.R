# Load necessary libraries
library(ISLR2)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(GGally)
library(tidyverse)
library(data.table)
library(vtable)

ui = fluidPage(
  titlePanel("An Introduction to Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", label = "Datasets",
                  choices = c("Upload", "Airquality", 'BrainCancer', 'College', 'Credit', 
                              'Iris', 'Mtcars', 'Quakes', 'USArrests', 'Wage')),

      fileInput('df', label = 'Upload CSV File'),
      textInput("delim", "Delimiter (leave blank to guess)", ""),
      numericInput("rows", "Rows to preview", 10, min = 1),
      selectInput("num_group_var", "Select Numeric Variable to Group:", choices = NULL, selected = "None"),
      textInput("group_thresholds", "Enter Thresholds (comma-separated):", ""),
      uiOutput("group_name_inputs"),
      actionButton("submit_changes", "Submit Changes"),


      
      # Dynamic UI elements for variable selection
      uiOutput("xvar_ui"),
      uiOutput("yvar_ui"),
      uiOutput("zvar_ui")
    ),
   
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel('Data preview', tableOutput('head'), uiOutput("data_description")),
        tabPanel('Summary', verbatimTextOutput('summary')),
        
        # Visualization tab with nested subtabs
        tabPanel('Visualization',
                 tabsetPanel(
                   id = "subtabs",
                
                   # Univariate Analysis Subtabs
                   tabPanel('Univariate plots',
                            tabsetPanel(
                              id = "subtabs_univariate",
                              tabPanel('Numerical Variables',
                                       fluidRow(
                                         column(6, plotOutput('histogram'),
                                         actionButton("show_hist_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_hist_code % 2 == 1",
                                                         verbatimTextOutput("hist_code", placeholder = TRUE)
                                                          )
                                                ),

                                         column(6, plotOutput('boxplot'),
                                         actionButton("show_box_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_box_code % 2 == 1",
                                                         verbatimTextOutput("box_code", placeholder = TRUE)
                                                          )
                                                )
                                       ),
                                       fluidRow(
                                         column(6, plotOutput('dotplot'),
                                         actionButton("show_dot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_dot_code % 2 == 1",
                                                         verbatimTextOutput("dot_code", placeholder = TRUE)
                                                          )
                                                ),
                                         column(6, plotOutput('violinplot'),
                                         actionButton("show_violin_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_violin_code % 2 == 1",
                                                         verbatimTextOutput("violin_code", placeholder = TRUE)
                                                          )
                                               )
                                       )
                              ),
                              
                              tabPanel('Categorical Variables',
                                       fluidRow(
                                         column(6, plotOutput('barplot'),
                                         actionButton("show_barplot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_barplot_code % 2 == 1",
                                                         verbatimTextOutput("barplot_code", placeholder = TRUE)
                                                          )
                                                ),
                                         column(6, plotOutput('piechart'),
                                         actionButton("show_piechart_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_piechart_code % 2 == 1",
                                                         verbatimTextOutput("piechart_code", placeholder = TRUE)
                                                          )
                                                )
                                       ),
                                       fluidRow(
                                         column(12, plotOutput('Pictogram_plot'),
                                         actionButton("show_Pictogram_plot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_Pictogram_plot_code % 2 == 1",
                                                         verbatimTextOutput("Pictogram_plot_code", placeholder = TRUE)
                                                          )
                                               )  
                                       )
                              )
                            )
                   ),
                   
                   # Bivariate Analysis Subtabs
                   tabPanel('Bivariate plots',
                            tabsetPanel(
                              id = "subtabs_bivariate",
                              
                              tabPanel('Numerical Variables',
                                       fluidRow(
                                         column(6, plotOutput('correlation_plot'),
                                         actionButton("show_correlation_plot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_correlation_plot_code % 2 == 1",
                                                         verbatimTextOutput("correlation_plot_code", placeholder = TRUE)
                                                          )
                                                ),
                                         column(6, plotOutput('Area_plot'),
                                         actionButton("show_Area_plot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_Area_plot_code % 2 == 1",
                                                         verbatimTextOutput("Area_plot_code", placeholder = TRUE)
                                                          )
                                                )
                                       ),

                                       fluidRow(
                                         column(6, plotOutput('contour_plot'),
                                         actionButton("show_contour_plot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_contour_plot_code % 2 == 1",
                                                         verbatimTextOutput("contour_plot_code", placeholder = TRUE)
                                                          )
                                                ),
                                         column(6, plotOutput('Spline_plot'),
                                         actionButton("show_spline_plot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_spline_plot_code % 2 == 1",
                                                         verbatimTextOutput("spline_plot_code", placeholder = TRUE)
                                                          )
                                                )
                                       ),
                                       fluidRow(
                                        column(12, plotOutput('Raddar_plot'))
                                        )
                              ),
                              
                              tabPanel('Categorical Variables',
                                       fluidRow(
                                         column(6, plotOutput('heatmap'),
                                         actionButton("show_heatmap_plot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_heatmap_plot_code % 2 == 1",
                                                         verbatimTextOutput("heatmap_plot_code", placeholder = TRUE)
                                                          )
                                                ),
                                         column(6, plotOutput('stripplot'),
                                         actionButton("show_strip_plot_code", "Show Code"),
                                         conditionalPanel(
                                                         condition = "input.show_strip_plot_code % 2 == 1",
                                                         verbatimTextOutput("strip_plot_code", placeholder = TRUE)
                                                          )
                                                )

                                       ),
                                       fluidRow(
                                        column(12, plotOutput("treemap_plot"),
                                        actionButton("show_treemap_plot_code", "Show Code"),
                                        conditionalPanel(
                                                         condition = "input.show_treemap_plot_code % 2 == 1",
                                                         verbatimTextOutput("treemap_plot_code", placeholder = TRUE)
                                                          )
                                              )
                                       )
                              )
                            )
                   ),

                   # Multivarite Analysis Subtabs
                   tabPanel('Multivariate plots',
                   
                        fluidRow(
                                column(12, plotOutput('pair_plot', height = "100vh", width = "100%"),
                                actionButton("show_pair_plot_code", "Show Code"),
                                conditionalPanel(
                                                  condition = "input.show_pair_plot_code % 2 == 1",
                                                  verbatimTextOutput("pair_plot_code", placeholder = TRUE)
                                                          )
                                      )
                                )
                              )    
                 )
        )
      )
    )
  )
)




server = function(input, output, session) {
   
  # Reactive expression to load data based on selection or upload
  df = reactive({
    if (input$dataset == "Mtcars") {
      return(mtcars)
    } else if (input$dataset == "Airquality") {
      return(airquality)
    } else if (input$dataset == "Iris") {
      return(iris)
    } else if (input$dataset == "USArrests") {
      return(USArrests)
    } else if (input$dataset == "Quakes") {
      return(quakes)
    } else if (input$dataset == "BrainCancer") {
      return(na.omit(BrainCancer))
    } else if (input$dataset == "Credit") {
      return(Credit)
    } else if (input$dataset == "College") {
      return(College)
    } else if (input$dataset == "Wage") {
      return(Wage)
    }
    
    req(input$df)
    delim = if (input$delim == "") ',' else input$delim
    read.csv(input$df$datapath, sep = delim, na.strings = c(''), stringsAsFactors = TRUE)
  })

  
  

  # Update the data description link based on the selected dataset
  output$data_description = renderUI({
    link = NULL
    if (input$dataset == "Mtcars") {
      link = a("Data Description: Mtcars", href = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html", target = "_blank")
    } else if (input$dataset == "Airquality") {
      link = a("Data Description: Airquality", href = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html", target = "_blank")
    } else if (input$dataset == "Wage") {
      link = a("Data Description: Wage", href = "https://rdrr.io/cran/ISLR2/man/Wage.html", target = "_blank")
    } else if (input$dataset == "College") {
      link = a("Data Description: College", href = "https://rdrr.io/cran/ISLR2/man/College.html", target = "_blank")
    } else if (input$dataset == "Iris") {
      link = a("Data Description: Iris", href = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html", target = "_blank")
    } else if (input$dataset == "Quakes") {
      link = a("Data Description: Quakes", href = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/quakes.html", target = "_blank")
    } else if (input$dataset == "USArrests") {
      link = a("Data Description: USArrests", href = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/USArrests.html", target = "_blank")
    } else if (input$dataset == "Credit") {
      link = a("Data Description: Credit", href = "https://rdrr.io/cran/ISLR2/man/Credit.html", target = "_blank")
    } else if (input$dataset == "BrainCancer") {
      link = a("Data Description: BrainCancer", href = "https://rdrr.io/cran/ISLR2/man/BrainCancer.html", target = "_blank")
    } else if (input$dataset == "Upload" && !is.null(input$df)) {
      link = tags$em("No data description available for the uploaded file.")
    }
    tags$div(link, style = "margin-top: 10px; font-size: 1em; color: gray;")
  })
  
    output$group_name_inputs = renderUI({
    req(input$num_group_var, input$group_thresholds)
    if (input$num_group_var == "None" || input$group_thresholds == "") {
        return(NULL)
    }

    # Parse thresholds into numeric values
    thresholds = sort(as.numeric(unlist(strsplit(input$group_thresholds, ","))))

    # Create inputs for each range
    inputs = list()
    inputs[[1]] = textInput("group_name_1", paste0("Name for values <= ", thresholds[1]), "Category 1")
    for (i in seq_along(thresholds)[-length(thresholds)]) {
        inputs[[i + 1]] = textInput(
            paste0("group_name_", i + 1),
            paste0("Name for values between ", thresholds[i], " and ", thresholds[i + 1]),
            paste0("Category ", i + 1)
        )
    }
    inputs[[length(thresholds) + 1]] = textInput(
        paste0("group_name_", length(thresholds) + 1),
        paste0("Name for values > ", thresholds[length(thresholds)]),
        paste0("Category ", length(thresholds) + 1)
    )

    do.call(tagList, inputs)
})


  # Reactive value to store the dataset
    current_df = reactiveVal()

    # Initialize current_df with the original dataset
    observe({
        req(df())
        current_df(df())
    })

    observeEvent(input$submit_changes, {
    req(current_df(), df_grouped())  # Ensure both the current and grouped datasets are available

    # Merge changes into the current dataset
    updated_df = current_df()

    # Update only the selected variable in the current dataset
    grouped_var = input$num_group_var
    if (grouped_var != "None") {
        updated_df[[paste0(grouped_var, "_grouped")]] = df_grouped()[[paste0(grouped_var, "_grouped")]]
    }

    # Save the updated dataset
    current_df(updated_df)
    
})



  # Update variable choices dynamically based on selected subtab
  observe({
    req(current_df())    
    
    # Get numeric and categorical variable names
    numeric_vars = names(current_df())[sapply(current_df(), is.numeric)]
    categorical_vars = names(current_df())[sapply(current_df(), function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "num_group_var", choices = c("None", numeric_vars), selected = "None")
    
    # Update UI based on main and sub tabs
    if (input$main_tabs == "Visualization") {
      # Univariate Plots
      if (input$subtabs == "Univariate plots") {
        if (input$subtabs_univariate == "Numerical Variables") {
          output$xvar_ui = renderUI({
            selectInput("xvar", label = 'Select Numeric Variable for X-axis', choices = numeric_vars, selected = NULL)
          })
          output$yvar_ui = renderUI({ NULL })  # No y-axis in univariate plots
          output$zvar_ui = renderUI({
                            selectInput("zvar", label = 'Group by (optional)', choices = c('None', categorical_vars), 
                            selected = NULL)
          })
        } else if (input$subtabs_univariate == "Categorical Variables") {
          output$xvar_ui = renderUI({
            selectInput("xvar", label = 'Select Categorical Variable for X-axis', choices = categorical_vars, selected = NULL)
          })
          output$yvar_ui = renderUI({ NULL })  # No y-axis in univariate plots
          output$zvar_ui = renderUI({
                            selectInput("zvar", label = 'Group by (optional)', choices = c('None', categorical_vars), 
                            selected = NULL)
          })
        }
      }
      
      # Bivariate Plots
      if (input$subtabs == "Bivariate plots") {
        if (input$subtabs_bivariate == "Numerical Variables") {
          output$xvar_ui = renderUI({
            selectInput("xvar", label = 'Select Numeric Variable for X-axis', choices = numeric_vars, selected = NULL)
          })
          output$yvar_ui = renderUI({
            selectInput("yvar", label = 'Select Numeric Variable for Y-axis', choices = numeric_vars, selected = NULL)
          })
          output$zvar_ui = renderUI({
          selectInput("zvar", label = 'Group by (optional)', choices = c('None', categorical_vars, numeric_vars), 
          selected = 'None')
          })
        } else if (input$subtabs_bivariate == "Categorical Variables") {
          output$xvar_ui = renderUI({
            selectInput("xvar", label = 'Select Categorical Variable for X-axis', choices = categorical_vars, selected = NULL)
          })
          output$yvar_ui = renderUI({
            selectInput("yvar", label = 'Select Categorical Variable for Y-axis', choices = categorical_vars, selected = NULL)
          })
          output$zvar_ui = renderUI({
          selectInput("zvar", label = 'Group by (optional)', choices = c('None', categorical_vars), 
          selected = 'None')})
        }
      }

      if (input$subtabs == 'Multivariate plots'){
        output$xvar_ui = renderUI({NULL})
        output$yvar_ui = renderUI({NULL})
        output$zvar_ui = renderUI({
                        selectInput("zvar", label = 'Group by (optional)', choices = c('None', categorical_vars), 
                        selected = 'None')
      })       
      }
      

    }
  })


  # Reactive dataset with converted numeric variable
    df_grouped = reactive({
    data = current_df()  # Use the current dataset for incremental changes
    req(data)

    # Check if a variable is selected for grouping
    if (input$num_group_var != "None" && input$group_thresholds != "") {
        num_var = input$num_group_var

        # Parse thresholds into numeric values
        thresholds = sort(as.numeric(unlist(strsplit(input$group_thresholds, ","))))
        if (length(thresholds) < 1) {
            showNotification("Please provide at least one threshold.", type = "error")
            return(data)
        }

        # Get category names from user inputs
        group_names = sapply(1:(length(thresholds) + 1), function(i) {
            input[[paste0("group_name_", i)]]
        })

        # Validate the number of labels matches the number of intervals
        if (length(group_names) != (length(thresholds) + 1)) {
            showNotification("Number of category names must match the number of intervals.", type = "error")
            return(data)
        }

        # Replace numeric values with category names based on thresholds
        data[[paste0(num_var, "_grouped")]] = cut(
            data[[num_var]],
            breaks = c(-Inf, thresholds, Inf),
            labels = group_names,
            include.lowest = TRUE
        )
    }
    data
})





  
  # Display the head of the data frame with specified number of rows
  output$head = renderTable({
    req(current_df())
    head(current_df(), n = input$rows)
  })


  # Display summary statistics of the dataset
  output$summary = renderPrint({
    req(current_df())
    skimr::skim_without_charts(current_df())

  })


################### Univariate
##### Numerical  
  # Plotting functions with conditional grouping
  output$histogram = renderPlot({
    req(current_df(), input$xvar)
    p = ggplot(current_df(), aes_string(x = input$xvar)) +
      geom_histogram(aes(y = ..density..), fill = '#2E86C1') +
      geom_density(color = 'red') +
      labs(title = paste("Histogram of", input$xvar), x = input$xvar) +
      theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.ticks.length.x = unit(10, 'pt')
      )
    
    # Apply grouping if zvar is selected
    if (input$zvar != "None") {
    p = ggplot(current_df(), aes_string(x = input$xvar)) +
      geom_density(color = 'black', alpha = 0.6) +
      labs(title = paste("Density plot of", input$xvar), x = input$xvar) +
      theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.ticks.length.x = unit(10, 'pt')
      ) +
      aes_string(fill = input$zvar)
    }
    p
  })

  output$hist_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df(), aes(x = xvar)) +
    geom_histogram(aes(y = ..density..), 
    fill = '#2E86C1') +
    geom_density(color = 'red')"
  } else {
    "ggplot(current_df(), aes(x = xvar)) +
     geom_density(color = 'black', alpha = 0.6) +
     labs(title = paste('Density plot of', xvar), x = xvar) +
     aes(fill = group_var)"}
  
  }) 
  
  output$boxplot = renderPlot({
    req(current_df(), input$xvar)
    b = ggplot(current_df(), aes_string(x = input$xvar)) +
      
      labs(title = paste("Boxplot of", input$xvar), x = input$xvar) +
      theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.ticks.length.x = unit(10, 'pt')
      ) 
      
    
    # Apply grouping if zvar is selected
    if (input$zvar != "None") {
      b = b + geom_boxplot() +
          aes_string(fill = input$zvar)
    }
    else{
        b = b + geom_boxplot(fill = '#FF6E33') 
    }
    b
  })

  output$box_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar)) +
     geom_boxplot(fill = '#FF6E33') +
     labs(title = paste('Boxplot of', xvar), 
          x = xvar)" 
  } else {
    "ggplot(df, aes(x = xvar)) +
     geom_boxplot(aes(fill = group_var)) +
     labs(title = paste('Boxplot of', xvar), 
          x = xvar)"}
  
  })
  
output$dotplot = renderPlot({
  req(current_df(), input$xvar)
  x_range = range(current_df()[[input$xvar]], na.rm = TRUE)
  binwidth = diff(x_range) / 50

  p = ggplot(current_df(), aes_string(x = input$xvar)) +
    labs(
      title = paste("Dot Plot of", input$xvar),
      x = input$xvar,
      y = "Count"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      plot.margin = margin(10, 10, 10, 10)  # Add margins to prevent clipping
    )

  if (input$zvar != "None") {
    # Grouped dot plot with binning
    p = p +
      geom_dotplot(
        aes_string(fill = input$zvar),
        binaxis = 'x',
        stackdir = 'up',  # Stack dots upwards
        method = 'histodot',  # Use histodot for continuous data
        binwidth = binwidth,  # Specify the number of bins
        stackratio = 0.6,
        alpha = 0.4
      ) +
      scale_y_continuous(name = "Count", breaks = scales::pretty_breaks()) +
      guides(fill = guide_legend(title = "Group"))
  } else {
    # Dot plot without grouping
    p = p +
      geom_dotplot(
        binaxis = 'x',
        stackdir = 'up',  # Stack dots upwards
        method = 'histodot',  # Use histodot for continuous data
        binwidth = binwidth,  # Specify the number of bins
        stackratio = 0.6,       
        fill = 'purple',
      ) +
      scale_y_continuous(name = "Count", breaks = scales::pretty_breaks())
  }

  p + coord_cartesian(clip = "off")
})

output$dot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar)) +
     geom_dotplot(fill = '#FF6E33') +
     labs(title = paste('Dot Plot of', xvar), 
          x = xvar)" 
  } else {
    "ggplot(df, aes(x = xvar)) +
     geom_dotplot(aes(fill = group_var)) +
     labs(title = paste('Dot Plot of', xvar), 
          x = xvar)"}
  
  })


  
  output$violinplot = renderPlot({
    req(current_df(), input$xvar)
    p = ggplot(current_df(), aes_string(x = "''", y = input$xvar)) +
      labs(title = paste("Violin Plot of", input$xvar), x = "", y = input$xvar) +
      theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
          axis.ticks.length.x = unit(10, 'pt')
      )
    
    # Apply grouping if zvar is selected
    if (input$zvar != "None") {
      p = p + geom_violin() +
          aes_string(fill = input$zvar)
    }
    else{
        p = p + geom_violin(fill = '#33FF77') 
    }
    p
  })

output$violin_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar)) +
     geom_violin(fill = '#FF6E33') +
     labs(title = paste('Violin Plot of', xvar), 
          x = xvar)" 
  } else {
    "ggplot(df, aes(x = xvar)) +
     geom_violin(aes(fill = group_var)) +
     labs(title = paste('Violin Plot of', xvar), 
          x = xvar)"}
  
  })



  


###### Univariate (Categorical)

  output$barplot = renderPlot({
    req(current_df(), input$xvar)
    p = ggplot(current_df(), aes_string(x = input$xvar)) +
      labs(title = paste("Bar Plot of", input$xvar), x = input$xvar, y = "Count") +
      theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.ticks.length.x = unit(10, 'pt')
      )
    
    # Apply grouping if zvar is selected
    if (input$zvar != "None") {
      p = p + geom_bar() +
          aes_string(fill = input$zvar)
    }
    else{
        p = p + 
            geom_bar(fill = '#16A085') 
    }
    p
    
  })

output$barplot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar)) +
     geom_bar(fill = '#16A085') +
     labs(title = paste('Bar Plot of', xvar), 
          x = xvar)" 
  } else {
    "ggplot(df, aes(x = xvar)) +
     geom_bar(aes(fill = group_var)) +
     labs(title = paste('Bar Plot of', xvar), 
          x = xvar)"}
  
  })



output$piechart = renderPlot({
    req(current_df(), input$xvar)
    
    # Ensure the selected variable is categorical
    if (!is.numeric(current_df()[[input$xvar]])) {
        if (input$zvar == "None") {
            # No grouping (zvar is None)
            data_counts = current_df() %>%
                count(!!sym(input$xvar)) %>%
                mutate(proportion = n / sum(n),
                       label = paste0(round(proportion * 100, 1), "%"))
            
            ggplot(data_counts, aes(x = 2, y = proportion, fill = !!sym(input$xvar))) +
                geom_bar(stat = "identity", width = 1, color = "white") +
                coord_polar(theta = "y") +
                xlim(1, 2.5) +  # Adjust the x-limits to create a donut shape
                geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
                labs(title = paste("Donut Chart of", input$xvar)) +
                theme(axis.text = element_blank(),       # Remove axis text
                      axis.ticks = element_blank(),      # Remove axis ticks
                      panel.grid = element_blank(),      # Remove grid lines
                      axis.title = element_blank(),
                      legend.title = element_text(),
                      legend.position = "right",
                      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
        } else {
            # Grouping by zvar
            data_counts = current_df() %>%
                count(!!sym(input$xvar), !!sym(input$zvar)) %>%
                group_by(!!sym(input$zvar)) %>%
                mutate(proportion = n / sum(n),
                       label = paste0(round(proportion * 100, 1), "%"))
            
            ggplot(data_counts, aes(x = 2, y = proportion, fill = !!sym(input$xvar))) +
                geom_bar(stat = "identity", width = 1, color = "white") +
                coord_polar(theta = "y") +
                xlim(1, 2.5) +  # Adjust the x-limits to create a donut shape
                geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
                facet_wrap(vars(!!sym(input$zvar)), scales = "free") +  # Facet by zvar
                labs(title = paste("Donut Chart of", input$xvar, "Grouped by", input$zvar),
                     fill = input$xvar) +
                   # Remove axes for a cleaner look
                theme(axis.text = element_blank(),       # Remove axis text
                        axis.ticks = element_blank(),      # Remove axis ticks
                        panel.grid = element_blank(),      # Remove grid lines
                        axis.title = element_blank(),
                        legend.title = element_text(),
                        legend.position = "right",
                        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
        }
    }
})


output$piechart_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "data_counts = df %>%
                count(xvar)) %>%
                mutate(proportion = n / sum(n),
                       label = paste0(round(proportion * 100, 1), '%'))
            
    ggplot(data_counts, aes(x = 2, y = proportion, fill = xvar)) +
        geom_bar(stat = 'identity', width = 1, color = 'white') +
        coord_polar(theta = 'y') +
        xlim(1, 2.5) +  # Adjust the x-limits to create a donut shape
        geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
        labs(title = paste('Donut Chart of', xvar)) +
        theme(axis.text = element_blank(),       # Remove axis text
              axis.ticks = element_blank(),      # Remove axis ticks
              panel.grid = element_blank(),      # Remove grid lines
              axis.title = element_blank(),
              legend.title = element_text(),
              legend.position = 'right',
              plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))" 
  } else {
    "data_counts = df %>%
                count(xvar, group_var) %>%
                group_by(group_var) %>%
                mutate(proportion = n / sum(n),
                       label = paste0(round(proportion * 100, 1), '%''))
            
    ggplot(data_counts, aes(x = 2, y = proportion, fill = xvar)) +
        geom_bar(stat = 'identity', width = 1, color = 'white') +
        coord_polar(theta = 'y') +
        xlim(1, 2.5) +  # Adjust the x-limits to create a donut shape
        geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
        facet_wrap(vars(group_var), scales = 'free') +  # Facet by zvar
        labs(title = paste('Donut Chart of', xvar, 'Grouped by', zvar),
              fill = xvar) +
            # Remove axes for a cleaner look
        theme(axis.text = element_blank(),       # Remove axis text
                axis.ticks = element_blank(),      # Remove axis ticks
                panel.grid = element_blank(),      # Remove grid lines
                axis.title = element_blank(),
                legend.title = element_text(),
                legend.position = 'right',
                plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'))"}
  
  })



output$Pictogram_plot = renderPlot({
    req(current_df(), input$xvar)
    
    # Ensure the selected variable is categorical
    if (!is.numeric(current_df()[[input$xvar]])) {
        # Prepare data based on zvar
        if (input$zvar == "None") {
            # Count the categories without grouping
            data_counts = current_df() %>%
                count(!!sym(input$xvar)) %>%
                mutate(icon = "\u2605",  # Define the pictogram icon (Unicode star)
                       label = paste0(icon, " ", n))  # Combine star and count
        } else {
            # Count the categories with grouping by zvar
            data_counts = current_df() %>%
                count(!!sym(input$xvar), !!sym(input$zvar)) %>%
                mutate(icon = "\u2605",  # Define the pictogram icon (Unicode star)
                       label = paste0(icon, " ", n))  # Combine star and count
        }

        # Base plot
        p = ggplot(data_counts, aes_string(x = input$xvar, y = "n")) +
            labs(title = paste("Pictogram Plot of", input$xvar, if (input$zvar != "None") paste("Grouped by", input$zvar) else ""),
                 x = input$xvar, y = "Count") +
            theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

        # Add pictograms
        if (input$zvar != "None") {
            p = p + geom_text(aes_string(label = "label", color = input$zvar), size = 6)   # Use group coloring
        } else {
            p = p + geom_text(aes(label = label), size = 6, color = "#3498DB")  # Single color
        }

        # Print the plot
        print(p)
    }
})

output$Pictogram_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "data_counts = df %>%
                count(xvar) %>%
                mutate(icon = '\u2605',  # Define the pictogram icon (Unicode star)
                       label = paste0(icon, '', n))  
     ggplot(data_counts, aes(x = xvar, y = 'n')) +
          labs(title = paste('Pictogram Plot of', xvar),
                 x = xvar, y = 'Count') +
          theme(plot.title = element_text(hjust = 0.5, size = 15, 
                face = 'bold')) +
          geom_text(aes(label = label), size = 6, color = '#3498DB') "                   
  } else {
    "data_counts = df %>%
                count(xvar, group_var) %>%
                mutate(icon = '\u2605',  # Define the pictogram icon (Unicode star)
                       label = paste0(icon, '', n))  
     ggplot(data_counts, aes(x = xvar, y = 'n')) +
          labs(title = paste('Pictogram Plot of', xvar, 'Grouped by', group_var) +
          theme(plot.title = element_text(hjust = 0.5, size = 15, 
                face = 'bold')) +
          geom_text(aes_string(label = 'label', color = group_var), size = 6)"}
  
  })



  ############# Bivariate
 ###### Numerical 
  
  output$correlation_plot = renderPlot({
    req(current_df(), input$xvar, input$yvar)

    p = ggplot(current_df(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_smooth(formula = 'y ~ x', method = "lm", se = FALSE, color = "#EB984E") +
      labs(title = paste("Scatter Plot of", input$yvar, "vs", input$xvar), x = input$xvar, y = input$yvar) +
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.ticks.length.x = unit(10, 'pt'))

    if (input$zvar != 'None'){
        p + geom_point(aes_string(color = input$zvar))
        
    }
    else{
        p + geom_point(color = '#8E44AD')
    }
    
  })

  output$correlation_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar, y = yvar)) +
     geom_point(color = '#8E44AD') +
     geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE, color = '#EB984E') +
     labs(title = paste('Scatter Plot of', yvar, 'vs', xvar), x = xvar, y = yvar)" 
  } else {
    "ggplot(df, aes(x = xvar, y = yvar)) +
     geom_point(color = group_var) +
     geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE, color = '#EB984E') +
     labs(title = paste('Scatter Plot of', yvar, 'vs', xvar), x = xvar, y = yvar)"}
  
  })




  output$Area_plot = renderPlot({
    req(current_df(), input$xvar, input$yvar)  # Ensure both x and y are provided

    # Base plot
    p = ggplot(current_df(), aes_string(x = input$xvar, y = input$yvar)) +
      labs(title = paste("Area Plot of", input$yvar, "vs", input$xvar),
           x = input$xvar, y = input$yvar) +
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.ticks.length.x = unit(10, 'pt'))

    # Conditional layer based on zvar
    if (input$zvar != 'None') {
        p + geom_area(aes_string(fill = input$zvar), alpha = 0.6)
    } else {
        p + geom_area(fill = '#DC7633', alpha = 0.6)
    }
})

output$Area_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar, y = yvar)) +
     geom_area(color = '#DC7633') +
     labs(title = paste('Area Plot of', yvar, 'vs', xvar), x = xvar, y = yvar)" 
  } else {
    "ggplot(df, aes(x = xvar, y = yvar)) +
     geom_area(color = group_var) +
     labs(title = paste('Area Plot of', yvar, 'vs', xvar), x = xvar, y = yvar)"}
  
  })

output$contour_plot = renderPlot({
  req(current_df(), input$xvar, input$yvar)



  # Base  plot
  p = ggplot(current_df(), aes_string(x = input$xvar, y = input$yvar)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
    scale_fill_viridis_c() +
    labs(title = paste("Contour Plot of", input$yvar, "vs", input$xvar),
         x = input$xvar, y = input$yvar) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )

  # Add grouping if zvar is selected
  if (input$zvar != "None") {
    p = ggplot(current_df(), aes_string(x = input$xvar, y = input$yvar, color = input$zvar)) +
      stat_density_2d(aes_string(fill = "..level.."), geom = "polygon", alpha = 0.6) +
      scale_fill_viridis_c() +
      labs(fill = input$zvar) + 
      labs(title = paste("Contour Plot of", input$yvar, "vs", input$xvar),
         x = input$xvar, y = input$yvar) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  }

  print(p)
})

output$contour_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar, y = yvar)) +
     stat_density_2d(aes(fill = ..level..), geom = 'polygon', alpha = 0.6) +
     scale_fill_viridis_c() +
     labs(title = paste('Contour Plot of', yvar, 'vs', xvar),
         x = xvar, y = yvar)" 
  } else {
    "ggplot(df, aes(x = xvar, y = yvar)) +
     stat_density_2d(aes(fill = '..level..'), geom = 'polygon', alpha = 0.6) +
     scale_fill_viridis_c() +
     labs(fill = group_var) +
     labs(title = paste('Contour Plot of', yvar, 'vs', xvar),
         x = xvar, y = yvar)" 
}
  
  })



output$Spline_plot = renderPlot({
    req(current_df(), input$xvar, input$yvar)

    # Base plot
    p = ggplot(current_df(), aes_string(x = input$xvar, y = input$yvar)) +
        labs(title = paste("Spline Plot of", input$yvar, "vs", input$xvar),
             x = input$xvar, y = input$yvar) +
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
              axis.text.x = element_text(size = 12),
              axis.ticks.length.x = unit(10, 'pt'))

    # Add spline layer
    if (input$zvar != 'None') {
        p + geom_point(aes_string(color = input$zvar), size = 3, alpha = 0.7) +
        geom_smooth(method = "loess", se = FALSE, color = "#E74C3C", size = 1)  # LOESS smoothing
    } else {
        p + geom_point(color = "#2ECC71", size = 2, alpha = 0.7) +
        geom_smooth(method = "loess", se = FALSE, color = "#E74C3C", size = 1)  # LOESS smoothing
    }

})

output$spline_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "ggplot(df, aes(x = xvar, y = yvar)) +
     geom_point(color = '#2ECC71', size = 2, alpha = 0.7) +
     geom_smooth(method = 'loess', se = FALSE, color = '#E74C3C', size = 1) +
     labs(title = paste('Spline Plot of', yvar, 'vs', xvar), x = xvar, y = yvar)" 
  } else {
    "ggplot(df, aes(x = xvar, y = yvar)) +
     geom_point(color = group_var, size = 3, alpha = 0.7) +
     geom_smooth(method = 'loess', se = FALSE, color = '#E74C3C', size = 1) +
     labs(title = paste('Spline Plot of', yvar, 'vs', xvar), x = xvar, y = yvar)" 
}
  
  })





output$Raddar_plot = renderPlot({
    req(current_df())

    # Select numeric variables dynamically
    numeric_vars = names(current_df())[sapply(current_df(), is.numeric)]
    if (length(numeric_vars) < 3) {
        showNotification("Radar plot requires at least 3 numeric variables. Please ensure the dataset has sufficient numeric variables.", type = "error")
        return(NULL)
    }

    # Handle missing values: Replace NA with column mean
    radar_data = current_df()[, numeric_vars, drop = FALSE] %>%
        mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

    # Check for grouping variable (zvar)
    group_var = if (input$zvar != "None") input$zvar else NULL

    if (!is.null(group_var)) {
        # Handle grouping by categorical zvar
        radar_data = radar_data %>%
            mutate(Group = as.factor(ifelse(is.na(current_df()[[group_var]]), "Missing", current_df()[[group_var]]))) %>%
            group_by(Group) %>%
            summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
            ungroup()
    } else {
        # Handle no grouping variable
        overall_data = radar_data %>%
            summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) # Overall mean
        radar_data = cbind(Group = "Overall", overall_data) # Add "Overall" group
    }

    # Compute max and min across the original dataset (not grouped data)
    max_row = sapply(current_df()[, numeric_vars], max, na.rm = TRUE)
    min_row = sapply(current_df()[, numeric_vars], min, na.rm = TRUE)

    # Combine max, min, and radar data
    radar_data_scaled = rbind(
        Max = max_row,
        Min = min_row,
        radar_data[, -1, drop = FALSE] # Exclude Group column for radar data
    )

    # Ensure proper row names
    rownames(radar_data_scaled) = c("Max", "Min", radar_data$Group)

    # Ensure the data is a matrix
    radar_data_scaled = as.data.frame(radar_data_scaled)

    # Adjust plotting parameters
    par(
        bg = "grey95",               # Set light grey background
        mar = c(3, 3, 3, 3),         # Adjust margins
        oma = c(4, 0, 2, 0),         # Outer margin for title
        xpd = NA                     # Allow plotting outside the plot area
    )

    # Generate radar plot
    fmsb::radarchart(
        radar_data_scaled,
        axistype = 1,                # Show axis labels
        pcol = c(rainbow(nrow(radar_data_scaled) - 2), "#1E90FF"),  # Line colors
        pfcol = c(scales::alpha(rainbow(nrow(radar_data_scaled) - 2), 0.2), scales::alpha("#1E90FF", 0.4)),  # Fill colors
        plwd = 2,                    # Line width
        cglcol = "grey",             # Grid line color
        cglty = 1,                   # Solid grid lines
        cglwd = 1.5,                 # Thicker grid lines
        vlcex = 1.3                  # Variable label text size
    )

    # Add a legend only if zvar is selected
    if (!is.null(group_var)) {
        
        title(
        main = paste("Radar plot of all the numerical variables by", input$zvar),
        cex.main = 1.8,
        font.main = 2,
        col.main = "black"
    )
        legend(
            "topright",
            legend = levels(as.factor(current_df()[[group_var]])), # Exclude "Overall"
            fill = scales::alpha(rainbow(length(levels(as.factor(current_df()[[group_var]])))), 0.2),
            border = rainbow(length(levels(as.factor(current_df()[[group_var]])))),
            bty = "n",  # No box around legend
            cex = 1.2
        )
    }

    else{
        title(
        main = paste("Radar Plot of all the numerical variables"),
        cex.main = 1.8,
        font.main = 2,
        col.main = "black"
    )
    }
})





  
 ####### Categorical

  output$heatmap = renderPlot({
    req(current_df(), input$xvar, input$yvar)

    if(input$zvar == 'None'){

        data_counts = current_df() %>%
        count(!!sym(input$xvar), !!sym(input$yvar), name = 'count')
        ggplot(data_counts, aes_string(x = input$xvar, y = input$yvar, fill = 'count')) +
        geom_tile(color = "black") +
        geom_text(aes(label = count), color = "white") +
 #       scale_fill_viridis_c() +
        labs(title = paste("Heatmap of", input$xvar, "and", input$yvar), x = input$xvar, y = input$yvar) +
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
             axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.ticks.length.x = unit(10, 'pt'))

    } else {

        data_counts = current_df() %>%
        count(!!sym(input$xvar), !!sym(input$yvar), !!sym(input$zvar), name = 'count')
        ggplot(data_counts, aes_string(x = input$xvar, y = input$yvar, fill = 'count')) +
        geom_tile(color = "black") +
        geom_text(aes(label = count), color = "black") +
        facet_grid(input$zvar) +
 #       scale_fill_viridis_c() +
        scale_fill_gradient(low = "white", high = "red")+
        labs(title = paste("Heatmap of", input$xvar, "and", input$yvar, 'group by', input$zvar), 
                     x = input$xvar, y = input$yvar) +
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
             axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.ticks.length.x = unit(10, 'pt'))             

    }
  })



output$heatmap_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar != 'None'){
    "data_counts = df %>%
                count(xvar, yvar, name = 'count')
     ggplot(data_counts, aes(x = xvar, y = yvar, fill = 'count')) +
        geom_tile(color = 'black') +
        geom_text(aes(label = count), color = 'white') +
        facet_grid(group_var) +
        scale_fill_gradient(low = 'white', high = 'red')+
        labs(title = paste('Heatmap of', xvar, 'and', yvar, 'group by', group_var),
             x = xvar, y = yvar)"

  } else {
    "data_counts = df %>%
                count(xvar, yvar, name = 'count')
     ggplot(data_counts, aes(x = xvar, y = yvar, fill = 'count')) +
        geom_tile(color = 'black') +
        geom_text(aes(label = count), color = 'white') +
        labs(title = paste('Heatmap of', xvar, 'and', yvar),
             x = xvar, y = yvar)"}
  
  })




  output$stripplot = renderPlot({
    req(current_df(), input$xvar, input$yvar)

    # Check if both xvar and yvar are categorical
    x_is_categorical = is.factor(current_df()[[input$xvar]]) || is.character(current_df()[[input$xvar]])
    y_is_categorical = is.factor(current_df()[[input$yvar]]) || is.character(current_df()[[input$yvar]])

  #  if (!x_is_categorical || !y_is_categorical) {
  #      showNotification("Both x and y variables must be categorical for this plot.", type = "error")
  #      return(NULL)
  #  }

    # Base plot
    p = ggplot(current_df(), aes_string(x = input$xvar, y = input$yvar))

    # Add points with or without zvar
    if (input$zvar == "None" || is.null(input$zvar)) {
        p = p + geom_jitter(width = 0.2, height = 0.2, color = "steelblue", size = 2, alpha = 0.7) +
            labs(title = paste("Strip Plot of", input$yvar, "by", input$xvar),
                 x = input$xvar, y = input$yvar)
    } else {
        p = p + geom_jitter(aes_string(color = input$zvar), width = 0.2, height = 0.2, size = 2, alpha = 0.7) +
            scale_color_brewer(palette = "Set2") +
            labs(title = paste("Strip Plot of", input$yvar, "by", input$xvar, "Grouped by", input$zvar),
                 x = input$xvar, y = input$yvar, color = input$zvar)
    }

    # Add theme
    p = p +
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12))

    # Print the plot
    print(p)
})

output$strip_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar != 'None'){
    "ggplot(df, aes(x = xvar, y = yvar)) +
     geom_jitter(width = 0.2, height = 0.2, color = 'steelblue', size = 2, alpha = 0.7) +
            labs(title = paste('Strip Plot of', yvar, 'by', xvar),
                 x = xvar, y = yvar)"

  } else {
  "ggplot(df, aes(x = xvar, y = yvar)) +
   geom_jitter(width = 0.2, height = 0.2, color = 'steelblue', size = 2, alpha = 0.7) +
   scale_color_brewer(palette = 'Set2')
   labs(title = paste('Strip Plot of', yvar, 'by', xvar, 'Grouped by', group_var),
                 x = xvar, y = yvar, color = group_var)"
          }
  
  })









output$treemap_plot = renderPlot({
  req(current_df(), input$xvar, input$yvar)

  # Grouping logic
  if (input$zvar == "None") {
    # Without grouping
    treemap_data = current_df() %>%
      count(!!sym(input$xvar), !!sym(input$yvar)) %>%
      mutate(
        percentage = round(n / sum(n) * 100, 2),  # Calculate percentages
        label = paste0(!!sym(input$xvar), ",", !!sym(input$yvar), "\n(", percentage, "%)")
      )
  } else{
    treemap_data = current_df() %>%
      count(!!sym(input$xvar), !!sym(input$yvar), !!sym(input$zvar)) %>%
      mutate(
        percentage = round(n / sum(n) * 100, 2),  # Calculate percentages within groups
        label = paste0(!!sym(input$xvar), ",", !!sym(input$yvar), ",", !!sym(input$zvar), "\n(", percentage, "%)")
      )  
  }

  # Adjust margins to make space for the title above the plot
  par(mar = c(6, 2, 8, 2))  # Set inner margins; increase bottom margin


  # Treemap plot
  treemap::treemap(
    treemap_data,
    index = if (input$zvar == "None") c("label") else c(input$zvar, "label"),
    vSize = "n",  # Size of rectangles based on counts
    vColor = "percentage",  # Color based on counts
    type = "value",  # Use numeric value for coloring
    fontsize.labels = 3,  # Adjust font size
    title = "",
    palette = "Blues",  # Use a blue palette
    align.labels = list(c("center", "center"), c("right", "bottom")),  # Align labels
    inflate.labels = TRUE
  )
  # Add a customized title
  mtext(
    side = 3,  # Place the text at the top
    line = 5,  # Distance from the plot (increase to move further up)
    paste("Treemap of", input$xvar, "and", input$yvar, if (input$zvar != "None") paste("Grouped by", input$zvar)),
    cex = 1.8,  # Font size
    font = 2,   # Bold font
    col = "white",  # Title color
    adj = 0.5   # Center the title
  )

})

output$treemap_plot_code = renderText({
      req(current_df(), input$xvar)
    
    if(input$zvar == 'None'){
    "data_counts = df %>%
                count(xvar, yvar, group_var, name = 'count') %>%
                mutate(
                  percentage = round(n / sum(n) * 100, 2), 
                  label = paste0(xvar, ',', yvar, '\n(', percentage, '%'))
    treemap::treemap(
    treemap_data,
    index = c('label'),
    vSize = 'n,  # Size of rectangles based on counts
    vColor = 'percentage',  # Color based on counts
    type = 'value',  # Use numeric value for coloring
    fontsize.labels = 3,  # Adjust font size
    title = '',
    palette = 'Blues',  # Use a blue palette
    align.labels = list(c('center', 'center'), c('right', 'bottom')),  # Align labels
    inflate.labels = TRUE
  )
    mtext(
     paste('Treemap of', xvar, 'and', yvar)
        )"

  } else {
        "data_counts = df %>%
                count(xvar, yvar, name = 'count') %>%
                mutate(
                  percentage = round(n / sum(n) * 100, 2), 
                  label = paste0(xvar, ',', yvar, ',', group_var, '\n(', percentage, '%'))
    treemap::treemap(
    treemap_data,
    index = c(group_var, 'label'),
    vSize = 'n,  # Size of rectangles based on counts
    vColor = 'percentage',  # Color based on counts
    type = 'value',  # Use numeric value for coloring
    fontsize.labels = 3,  # Adjust font size
    title = '',
    palette = 'Blues',  # Use a blue palette
    align.labels = list(c('center', 'center'), c('right', 'bottom')),  # Align labels
    inflate.labels = TRUE
  )
   mtext(
    paste('Treemap of', xvar, 'and', yvar, 'Grouped by', group_var)
        )"
  }
  
  })










####################### Multivariate

    output$pair_plot = renderPlot({
    req(current_df())
    numeric_vars_df = current_df()[, sapply(current_df(), is.numeric), drop = FALSE]

    if (ncol(numeric_vars_df) < 2) {
        showNotification("Pair plot requires at least two numeric variables. 
                          Please ensure the dataset has enough numeric variables.", type = "error") 
            return(NULL)
    }

    # Generate pair plot
    if (input$zvar == "None") {
        p = ggpairs(numeric_vars_df,
                            progress = FALSE,  # Disable progress bar
                            title = "Pair Plot of the Numeric Variables") +
            theme(
                plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                strip.text = element_text(size = 10),  # Increase facet label size
                panel.spacing = unit(2, "lines")       # Increase panel spacing
            )
    } else {
        p = ggpairs(numeric_vars_df,
                            mapping = aes(color = as.factor(current_df()[[input$zvar]]), alpha = 0.4),
                            progress = FALSE,  # Disable progress bar
                            title = paste("Pair Plot of the Numeric Variables by", input$zvar)) +
            theme(
                plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                strip.text = element_text(size = 10),  # Increase facet label size
                panel.spacing = unit(2, "lines")       # Increase panel spacing
            )
    }

    # Print the plot
    print(p)
    

    }) 



output$pair_plot_code = renderText({
      req(current_df(), input$xvar, input$yvar)
    
    if(input$zvar == 'None'){
    "numeric_vars_df = df[, sapply(df, is.numeric), drop = FALSE]
     ggpairs(numeric_vars_df,
             progress = FALSE,  # Disable progress bar
             title = 'Pair Plot of the Numeric Variables')"

  } else {
    "numeric_vars_df = df[, sapply(df, is.numeric), drop = FALSE]
     ggpairs(numeric_vars_df,
             mapping = aes(color = as.factor(df[[group_var]]), alpha = 0.4),
             progress = FALSE,  # Disable progress bar
             title = paste('Pair Plot of the Numeric Variables by', group_var))"
          }
  
  })

}
# Run the application
shinyApp(ui = ui, server = server)
