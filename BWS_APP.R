library(shiny)
library(shinydashboard)
library(rhandsontable)
library(agricolae)
library(openxlsx)
library(DoE.base) 
library(shinyjs)
library(officer) 
library(flextable)
library(dplyr) 
library(tidyr)
library(shinyWidgets)
library(reactable)
library(ggplot2)
library(mlogit)
library(tidyverse)
library(poLCA)
library(survival)

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = tagList(
    # Logo on the left
    img(src = "logo.png", height = "60px", style = "margin-right: 10px; vertical-align: middle;"),
    # App title next to logo
    span("PEAR-BWS", 
         style = "color: #1c802d; font-weight: bold; font-family: Georgia, serif; 
                  font-size: 30px; vertical-align: middle;")
  ),
  titleWidth = "100%"),
  
  # Sidebar with menu and BWS case selection
  dashboardSidebar(width = 330, 
                   sidebarMenu(
                     menuItem(strong("Home"), tabName = "home", icon = icon("home")),
                     menuItem(strong("Questionnaire Generation"), tabName = "questionnaire_generation", icon = icon("list")),
                     menuItem(strong("Statistical Analysis"), tabName = "statistical_analysis", icon = icon("chart-bar"))
                   ), 
       # --- Logo and Name  ---
        div(
            style = "text-align: center; margin-top: 250px; padding: 10px;",
            span("Developed by", style = "color: #777; font-size: 14px; display: block; margin-top: 5px;"),
            strong("Anjana Bivas T", style = "font-size: 16px; color: #2c3e50; display: block;"),
            span("M.Sc. Agricultural Statistics", style = "color: #555; display: block;"),
            span("College of Agriculture, Vellayani", style = "color: #555; display: block;"),
            span("Kerala Agricultural University", style = "color: #555; display: block;"),
            span("Thiruvananthapuram", style = "color: #555; display: block;")
        )                
  ),
  
  # Main body with tabs
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Set global font to Georgia but exclude icons */
      body, .content-wrapper, .sidebar, .box, .tab-content, .btn, .navbar, .main-header {
        font-family: 'Georgia', serif !important;
        font-size: 16px !important;
      }
      
     /* Set background for the entire app */
    body, .content-wrapper {
      background-color: #FFFFFF !important; /* White */
    }
    
     /* Dashboard Header (Title Bar) */
    .main-header .logo {
      background-color: #D0F0C0 !important; /* Tea Green */
      color: #800000 !important; /* Maroon */
      font-weight: bold;
    }
    
    /* Navbar background */
      .navbar, .main-header {
        background-color:#D0F0C0 !important; /* Tea Green */
        color: #800000 !important; /* Maroon */
        height: 80px !important;  /* header height */
        line-height: 100px !important;
      }

    /* Sidebar */
    .main-sidebar, .sidebar-menu > li {
      background-color: #D0F0C0 !important; /* Tea Green */
      height: 80px !important;
      color: #006400 !important; /* Dark green */
    }

    /* Sidebar text */
    .sidebar-menu > li > a {
      color: #006400 !important; /* Dark green */
      font-size: 20px !important;
      line-height: 50px !important; /* Ensures proper alignment */
      height: 50px !important; /* Ensures uniform height */
      display: flex !important;
      align-items: center !important; /* Centers text vertically */
  }

    /* Sidebar selected text */
    .sidebar-menu > li.active > a {
      background-color: 	#228B22 !important; /* Green */
      color: #ffffff !important; /* White text */
    }
    
    /* Adjust sidebar and content height */
      .main-sidebar { 
        padding-top: 100px !important;  /* Push sidebar down */
      }

    /* Tab navigation */
    .nav-tabs > li > a {
      color: #006400 !important; /* Dark green */
      background-color: #ACE1AF !important; /* Celadon */
      border: none;
       min-width: 283px !important;   
      text-align: center !important; 
      white-space: normal !important;/* Allows wrapping if the title is long */
      font-size: 18px !important;   
      font-weight: bold !important;
      padding: 10px 20px !important; /* Optional: adds more space around text */
    }

    /* Active tab */
    .nav-tabs > li.active > a {
      background-color: #008000 !important; /* Green */
      color: #FFFFFF !important; /* White text */
    }

    /* Main content box */
    .box, .box-header {
      background-color: #ffffff !important; /* White */
      color: #000000 !important; /* Dark green */
      border: 2px solid #008000 !important; /* Green border */
    }


    /* Buttons */
    .btn {
      background-color: #008000 !important; /* Green */
      color: #ffffff !important; /* White text */
      border: none;
    }

    .btn:hover {
      background-color: #006400 !important; /* Darker green */
    }

    /* Download button */
    .btn-success {
      background-color: #008000 !important; /* Green */
      color: #ffffff !important; /* White */
    }

    /* Error message */
    .shiny-output-error {
      color: #8B0000 !important; /* Dark red for visibility */
      font-weight: bold;
    }
    "))
    ),
    useShinyjs(),  # Initialize shinyjs for dynamic UI
    tabItems(
      ##============ Home tab ============##
      tabItem(tabName = "home",
              fluidRow(
                    div(style = "width: 100%; display: flex; align-items: center; justify-content: center; margin-bottom: 20px;",
                    div(style = "display: flex; align-items: center;",
                      img(src = "logo.png", height = "200px", style = "margin-right: 15px;"), 
                      div("PEAR-BWS", style = "color: #006400; font-weight: bold; font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, serif; font-size: 46px; text-shadow: 2px 2px 6px #aaa; margin: 0;")
                    )),
                    h2("Preference Evaluation in Agricultural Research using Best-Worst Scaling", style = "color: #006400; font-weight: bold; font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, serif; text-align: center; font-size: 28px; text-shadow: 1px 1px 3px #888;"),
                    h3("Bests and worsts, turned into insights", style = "color: #008080; font-family: Georgia, serif; text-align: center;  font-style: italic;"),
                    div(style = "height: 3px; width: 100%; margin: 10px auto; background: linear-gradient(to right, transparent, #006400, transparent);"), 
                    tags$br(),
                    div(style = "text-align: center; margin: 0 auto; width: 75%;",
                        p("Best-Worst Scaling (BWS) is a powerful survey method that reveals people’s true priorities. By selecting the most and least preferred options from a set, respondents provide clear insights into their choices, without the complexity of traditional surveys.
                          PEAR-BWS makes this process effortless, helping you capture precise preferences and generate reliable interpretations with ease.",
                          style = "font-size: 16px;"),
                        p("Let PEAR-BWS reveal the choices that shape decisions!") ),
                    div(h3("Why PEAR-BWS is the Optimal Choice ?", style = "color: #B03060; font-weight: bold; padding-left: 375px; font-family: Georgia, serif"),
                        p("🎯 ", strong("Easy to Understand"), " – Simple and intuitive.", style = "padding-left: 375px;"),
                        p("🎯 ", strong("Less Cognitive Effort"), " – Streamlined decision-making.", style = "padding-left: 375px;"),
                        p("🎯 ", strong("More Precise Insights"), " – Captures preferences effectively.", style = "padding-left: 375px;")
                    ),
                    tags$br(),
                    h4("✨ Get Started with PEAR-BWS", style = "color: #006400; font-weight: bold; padding-left: 30px; font-family: Georgia, serif"),
                    p("● Go to 📝", strong("'Questionnaire Generation'"), " to create a BWS questionnaire.", style = "padding-left: 40px;"),
                    p("● Go to 📊", strong("'Statistical Analysis'"), " to analyze your BWS data.", style = "padding-left: 40px;"),
                    p("● Use the 💾 'Download' options to save your results.", style = "padding-left: 40px;")
              )
      ),
      
      ##============ Tab for Questionnaire Generation ============##
      tabItem(tabName = "questionnaire_generation",
              tabsetPanel(id = "questionnaire_generation",
                          #====== Instructions Tab ======#
                          tabPanel("Instructions", value = "instructions",
                                   fluidRow(
                                     box(status = "primary", solidHeader = TRUE, width = 12,
                                         style = "background-color: #ffffff; padding: 25px;",
                                         
                                         # --- BWS Description --- #
                                         h3("Curious about Best-Worst Scaling ?", 
                                         style = "background: linear-gradient(90deg, #B22222, #FF8C00);
                                                   -webkit-background-clip: text; -webkit-text-fill-color: transparent;
                                                   font-family: Georgia, serif; font-weight: bold;"),
                                         p("Best-Worst Scaling (BWS) is a survey method used to identify individual priorities by asking respondents 
                                           to choose the most and least preferred items from a set. It is a specialized form of choice 
                                           experiment that focuses on distinguishing the relative preferences and performing a comparative ranking of
                                           the respondents’ inclinations."), 
                                         p("Unlike traditional rating methods that often require evaluating a larger 
                                           number of items at once, BWS presents smaller sets. This makes the respondents clearer in making decisions
                                           about their preferences and provides more reliable data. BWS is simple to understand, reduces scale-use and
                                           central tendency biases, and breaks down large ranking tasks into smaller, more cognitively manageable ones."),
                                         
                                         p("BWS is classified into three types, based on the context and the nature of the items or 
                                            profiles being assessed. Each type differs in the format and focus of the choice sets:"),
                                         
                                         tags$ul(
                                           tags$li(
                                             span(strong("BWS Case 1 (Object Case)"), style = "color: #1B4670; font-family: Georgia, serif;"), 
                                             a(icon("info-circle"), href = "case1.html", target = "_blank", style = "margin-left: 8px; color: #CF2415; animation: blink 1s infinite;")  
                                           ),
                                           tags$li(
                                             span(strong("BWS Case 2 (Profile Case)"), style = "color: #1B4670; font-family: Georgia, serif;"),
                                             a(icon("info-circle"), href = "case2.html", target = "_blank", style = "margin-left: 8px; color: #CF2415; animation: blink 1s infinite;")
                                           ),
                                           tags$li(
                                             span(strong("BWS Case 3 (Multi-profile Case)"), style = "color: #1B4670; font-family: Georgia, serif;"),
                                             a(icon("info-circle"), href = "case3.html", target = "_blank", style = "margin-left: 8px; color: #CF2415; animation: blink 1s infinite;")
                                           ),
                                           tags$style(HTML(" @keyframes blink {0%, 100% {opacity: 1;}
                                                  50% {opacity: 0.3;}}
                                              "))
                                         ),
                                         br(),
                                         
                                         actionButton("go_to_case_selection", "Proceed to Case Selection", class = "btn btn-primary") 
                                     )
                                   )), 
                                   
                          #====== Case selection Tab (Visible by Default) ======#
                          tabPanel("Case Selection", value = "case_selection",
                            fluidRow(
                              box(status = "primary", solidHeader = TRUE, width = 12,
                                  p("Create a Best-Worst Scaling (BWS) questionnaire based on your selected case."),
                                  p("Select a BWS case from the options below and click 'Proceed' to start."),
                                  radioButtons("bws_case", tags$span("Choose a BWS Case:", style = "color: #006400; font-weight: bold; font-size: 18px;"),
                                               choices = c("BWS Case 1" = "Case 1", "BWS Case 2" = "Case 2", "BWS Case 3" = "Case 3")),
                                  actionButton("start_questionnaire", "Proceed")
                              )
                            )
                          ),
                          
                          
                          #====== Input Data Tab (content is hidden until "Proceed" is clicked) ======#
                          tabPanel(
                            "Input Data", 
                            value = "input_data",
                            fluidRow(
                              #Initially hide the content using shinyjs
                              div(id = "input_data_content", 
                                  box( title = "Enter Data", status = "primary", solidHeader = TRUE, width = 12,
                                       
                                       # Conditional Panel for BWS Case 1
                                       conditionalPanel(
                                         condition = "input.bws_case == 'Case 1'",
                                         # Message displayed before entering items
                                         uiOutput("case1_info"),
                                         h4("Enter Items (one item per line):"),
                                         textAreaInput("items_input", "Enter Items:", 
                                                       placeholder = "Item 1\nItem 2\nItem 3\nItem 4\n...", rows = 5),
                                         actionButton("generate", "Generate Case 1 Questionnaire", disabled = TRUE)
                                       ),
                                       
                                       # Conditional Panel for BWS Case 2
                                       conditionalPanel(
                                         condition = "input.bws_case == 'Case 2'",
                                         # Display the validation message
                                         uiOutput("case2_info"),
                                         h4("Define Attributes and Levels:"),
                                         fluidRow(
                                           column(width = 6, textInput("attribute_name", "Attribute Name:", placeholder = "Enter attribute name")),
                                           column(width = 6, textAreaInput( "attribute_levels", "Attribute Levels (one item per line):", 
                                                                            placeholder = "Level 1\nLevel 2\nLevel 3\n...", rows = 3))
                                         ),
                                         actionButton("add_attribute", "Add Attribute"),
                                         br(), br(),
                                         uiOutput("attribute_list"),
                                         actionButton( "generate_case2_questionnaire",  "Generate Case 2 Questionnaire", disabled = TRUE)
                                       ),
                                       
                                       # Conditional Panel for BWS Case 3
                                       conditionalPanel(
                                         condition = "input.bws_case == 'Case 3'",
                                         # Display the validation message
                                         uiOutput("case3_info"),
                                         h4("Define Attributes and Levels:"),
                                         fluidRow(
                                           column(width = 6, textInput("attribute_name_case3", "Attribute Name:", placeholder = "Enter attribute name")),
                                           column(width = 6, textAreaInput( "attribute_levels_case", "Attribute Levels (one item per line):", 
                                                                            placeholder = "Level 1\nLevel 2\nLevel 3\n...", rows = 3))
                                         ),
                                         actionButton("add_attribute_case3", "Add Attribute"),
                                         br(), br(),
                                         uiOutput("attribute_list_case3"),
                                         actionButton( "generate_case3_questionnaire",  "Generate Case 3 Questionnaire", disabled = TRUE)
                                       )
                                  )
                              )
                            )
                          ),
                          
                          #====== Questionnaire Tab ======#
                          tabPanel(
                            "Questionnaire", 
                            value = "questionnaire",
                            fluidRow(
                              box(title = "Best-Worst Questionnaire", status = "primary", solidHeader = TRUE,  width = 12,
                                  div(style = "margin-bottom: 15px;", 
                                      downloadButton("downloadQuestionnaireWord", "Download Questionnaire (Word)")
                                  ),
                                  uiOutput("questionnaire")
                              )
                            )
                          )
              )
      ),
      
      ##============ Tab for Statistical Analysis ============##
      tabItem( tabName = "statistical_analysis",
               tabsetPanel(
                 id = "statistical_analysis", 
                 #====== Instructions Tab ======#
                 tabPanel("Instructions", value = "instructions",
                          fluidRow(
                            box(status = "primary", solidHeader = TRUE, width = 12,
                                style = "background-color: #ffffff; padding: 15px;",
                                
                                p(span(strong("How to Use the Statistical Analysis Section?"), style = "color: #e60742; font-size: 20px;")),
                                p("1️⃣ Select the appropriate BWS case for analysis."),
                                p("2️⃣ Choose the desired analysis method from the options."),
                                p("3️⃣ Upload your dataset in CSV format, ensuring it follows the structure of the corresponding sample dataset provided below."),
                                p("4️⃣ Click 'Proceed' to perform the analysis."),
                                br(),
                                
                                # Action button to open HTML
                                tags$a(
                                  href = "BWS_analysis_methods.html", target = "_blank",
                                          style = "background:#00796b;color:white;padding:10px 25px;border-radius:10px;
                                          font-weight:bold;text-decoration:none;transition:0.3s;",
                                  onmouseover = "this.style.boxShadow='0 0 15px #00796b';this.style.transform='scale(1.05)'",
                                  onmouseout  = "this.style.boxShadow='none';this.style.transform='scale(1)'",
                                  "View BWS Analysis Methods Available in PEAR-BWS"
                                ),
                                
                                h4(span(style = "color: #e60742; font-weight: bold; font-family: Georgia, serif; font-size: 20px;", "Download Model Datasets")),
                                p("Use these sample datasets as a reference for formatting your own data."),
                                p("Open a new  Excel file, enter the best-worst responses as outlined in the model dataset for each BWS case, and save it as a CSV file."),
                                
                                # Model Dataset 1
                                div(style = "display: flex; align-items: center;",
                                    strong(span("Model Dataset for BWS Case 1", style = "color: #3c2882; font-size: 18px;")),
                                    downloadButton("BWS_case1_dataset", "Download", class = "btn btn-success", style = "margin-left: 15px;", contentType = "text/csv")
                                ),
                                p("The model format for the input file to do statistical analysis of responses obtained from BWS Case 1 (Object Case) is given above."),
                                p("🔹 The items in each choice set have to be enlisted."),
                                p("🔹 The choices of respondents are to be marked in the ‘Selection’ column. Give a score 1 to the item chosen as best, -1 to the item chosen as worst, and give 0 to other item(s) of the particular choice set."),
                                p("🔹 Scoring should be done to each choice set for every respondent."),
                                p("🔹 For example: A choice set contains 4 items- A1, A2, A3, and A4. If A4 is selected as best by the respondent, give it a score 1 and if A2 is selected as worst, give score -1. The remaining items A1 and A3 can be given 0 as their scores."),
                                br(),
                                
                                # Model Dataset 2
                                div(style = "display: flex; align-items: center;",
                                    strong(span("Model Dataset for BWS Case 2", style = "color: #3c2882; font-size: 18px;")),
                                    downloadButton("BWS_case2_dataset", "Download", class = "btn btn-success", style = "margin-left: 15px;", contentType = "text/csv")
                                ),
                                p("The model format for the input file to do statistical analysis of responses obtained from BWS Case 2 (Profile Case) is given above."),
                                p("🔹 The attributes and their corresponding attribute levels in each choice set (profile) are to be enlisted."),
                                p("🔹 The choices of respondents are to be marked in the ‘Selection’ column. Give a score 1 to the attribute chosen as best, -1 to the attribute chosen as worst, and give 0 to other attribute(s) of the particular choice set (profile)."),
                                p("🔹 Scoring should be done to each choice set for every respondent."),
                                p("🔹 For example: A choice set contains attribute levels A2, B2, and C3 of the three attributes A, B, and C respectively. If the respondent selects A2 as best attribute, give it a score 1 and if C2 is selected as worst, give score -1. The remaining attribute B2 can be given 0 as its score."),
                                br(),
                                
                                # Model Dataset 3
                                div(style = "display: flex; align-items: center;",
                                    strong(span("Model Dataset for BWS Case 3", style = "color: #3c2882; font-size: 18px;")),
                                    downloadButton("BWS_case3_dataset", "Download", class = "btn btn-success", style = "margin-left: 15px;", contentType = "text/csv")
                                ),
                                p("The model format for the input file to do statistical analysis of responses obtained from BWS Case 3 (Multi- Profile Case) is given above."),
                                p("🔹 The profiles in each choice set are enlisted with the corresponding attribute levels in each profile. Each row represents a single profile within a choice set."),
                                p("🔹 The choices of respondents are to be marked in the ‘Selection’ column. Give a score 1 to the profile chosen as best, -1 to the profile chosen as worst, and give 0 to other profile(s) of the particular choice set."),
                                p("🔹 Scoring should be done to each choice set for every respondent."),
                                p("🔹 For example: Profiles are created as a combination of attribute levels of 3 attributes A (A1, A2, A3), B (B1, B2, B3), and C (C1, C2, C3). A choice set contains 3 profiles: Profile 1 (A2-B2-C2), Profile 2 (A3-B3-C3), and Profile 3 (A1-B3-C2). If the respondent selects Profile 3 as best attribute, give it a score 1 and if Profile 1 is selected as worst, give score -1. The remaining Profile 2 can be given 0 as its score."),
                                br(),
                                actionButton("go_to_analysis_tab", "Proceed with Analysis", class = "btn btn-primary") 
                            )
                          )),
                 #====== Select Analysis Tab ======#
                 tabPanel("Select Analysis", value = "select_analysis",
                          fluidRow(
                            box(status = "primary", solidHeader = TRUE, width = 12,
                                selectInput("bws_case", "Select BWS Case:", 
                                            choices = c("", "BWS Case 1", "BWS Case 2", "BWS Case 3"),
                                            selected = ""),
                                selectInput("analysis_method", "Choose Analysis Method:", choices = c(""), selected = ""),
                                # Conditional UI for Profile Case Method
                                uiOutput("profile_case_ui"),
                                # Conditional UI for Analysis Method
                                fileInput("analysis_data", "Upload BWS Data:", accept = c(".csv")),
                                # Rounding option
                                numericInput("round_digits", "Decimal Places for Results:", 
                                             value = 2, min = 0, max = 6, step = 1),
                                
                                actionButton("run_analysis", "Run Analysis")
                            ))
                 ),
                 
                 #====== Results Tab ======#
                 tabPanel("Results", value = "results",
                          fluidRow(
                            box(
                              title = "Analysis Results", status = "primary", solidHeader = TRUE, width = 12,
                              reactableOutput("bws_results"),
                              uiOutput("bws_results_multiple"), 
                              uiOutput("bws_description"),
                              downloadButton("download_results", "Download Results"),
                              # Add CSS to control spacing and visibility
                              tags$style(HTML("
                                #bws_results, #profile_results, #bws_description, #main_plot, #secondary_plot, #download_results {
                                margin-bottom: 10px;
                                }
                                .hidden-plot {
                                display: none;
                                }
                                "))
                            ))),
                 #====== Plots and Graphs Tab ======#
                 tabPanel(
                   "Plots and Graphs",
                   value = "plots_and_graphs",
                   fluidRow(
                     box(
                       title = "Analysis Plots", status = "primary", solidHeader = TRUE, width = 12,
                       plotOutput("main_plot", height = "400px"),
                       plotOutput("secondary_plot", height = "400px"),
                       tags$style(HTML("
                        #main_plot, #secondary_plot {
                          margin-bottom: 20px;
                        }
                        .hidden-plot {
                          display: none;
                        }
                      "))
                     )
                    )
                 )
               )))))

# Define server logic for the application
server <- function(input, output, session) {
  
  #######========================= QUESTIONNAIRE GENERATION ==================================================================================#######
  # Navigate to "Case Selection" tab from "Instructions" tab
  observeEvent(input$go_to_case_selection, {
    updateTabsetPanel(session, "questionnaire_generation", selected = "case_selection")
  })
  
  # Use shinyjs to reveal the input content upon clicking "Proceed"
  observeEvent(input$start_questionnaire, {
    show("input_data_content")  # Reveal the Input Data section
  })
  
  # Automatically navigate to "Input Data" tab when "Proceed" is clicked
  observeEvent(input$start_questionnaire, {
    updateTabsetPanel(session, "questionnaire_generation", selected = "input_data")
  })
  
  # Automatically navigate to "Questionnaire" tab after generating
  observeEvent(input$generate, {
    updateTabsetPanel(session, "questionnaire_generation", selected = "questionnaire")
  })
  
  observeEvent(input$generate_case2_questionnaire, {
    updateTabsetPanel(session, "questionnaire_generation", selected = "questionnaire")
  })
  
  observeEvent(input$generate_case3_questionnaire, {
    updateTabsetPanel(session, "questionnaire_generation", selected = "questionnaire")
  })
  
  # Dynamically enable/disable the "Generate Questionnaire" button
  observe({
    if (input$bws_case == "Case 1" && nchar(input$items_input) > 0) {
      shinyjs::enable("generate")
    } else if (input$bws_case == "Case 2" && length(rv2$attributes) >= 2 && all(sapply(rv2$attributes, function(attr) length(attr$levels) >= 2))) {
      shinyjs::enable("generate_case2_questionnaire")
    } else if (input$bws_case == "Case 3" && length(rv3$attributes) > 0) {
      shinyjs::enable("generate_case3_questionnaire")
    } else {
      shinyjs::disable("generate")
      shinyjs::disable("generate_case2_questionnaire")
      shinyjs::disable("generate_case3_questionnaire")
    }
  })
  
  ####========================= CASE 1 =========================#######
  # Reactive value to store Case 1
  rv1 <- reactiveValues(data = data.frame(Number = integer(0), Item = character(0), stringsAsFactors = FALSE))
  
  # Reactive expression to parse input data
  data <- reactive({
    req(input$bws_case) 
    
    if (input$bws_case == "Case 1") {
      req(input$items_input)  # Ensure items are provided
      items <- unlist(strsplit(input$items_input, "\n"))
      items <- trimws(items)  # Remove extra spaces
      data.frame(Number = seq_along(items), Item = items, stringsAsFactors = FALSE)
    } 
  })
  
  # UI message in the Input Data tab
  output$case1_info <- renderUI({
    if (is.null(input$items_input) || trimws(input$items_input) == "" || 
        length(trimws(unlist(strsplit(input$items_input, "\n")))) < 5) {
      return(h4("At least 5 items are required for questionnaire (BIBD) generation.", style = "color: gray;"))
    } else {
      return(NULL)
    }
  })
  
  # Helper function to check BIBD validity
  is_bibd_possible <- function(v, k, lambda) {
    if (k >= v) return(FALSE)
    r <- lambda * (v - 1) / (k - 1)
    if (r != floor(r)) return(FALSE)
    b <- v * r / k
    if (b != floor(b)) return(FALSE)
    return(TRUE)
  }
  
  # Generate BWS designs based on selected case
  design <- eventReactive(input$generate, {
    req(data())
    num_items <- nrow(data())
    req(num_items >= 5)
    
    valid_design <- NULL
    chosen_k <- NULL
    chosen_lambda <- NULL
    chosen_r <- NULL
    chosen_b <- NULL
    k_values <- 4:6  # Block sizes from 4 to 6
    
    # Try to find valid BIBD for the given number of items
    for (k in k_values) {
      if (k >= num_items) next
      
      for (lambda in 1:10) {
        if (is_bibd_possible(num_items, k, lambda)) {
          result <- tryCatch(
            {
              r <- as.integer(lambda * (num_items - 1) / (k - 1))
              b <- as.integer(num_items * r / k)
              
              # Generate BIBD using design.bib from agricolae
              bibd_result <- agricolae::design.bib(trt = 1:num_items, k = k, r = r, seed = 123)
              
              if (!is.null(bibd_result) && !is.null(bibd_result$book)) {
                # Extract design from book
                book <- bibd_result$book
                blocks <- unique(book$block)
                
                # Create design matrix: each row is a block with k treatments
                design_matrix <- matrix(NA, nrow = length(blocks), ncol = k)
                
                for (i in seq_along(blocks)) {
                  block_data <- book[book$block == blocks[i], ]
                  # Extract treatments - they're in a column (usually named with numbers or 'trt')
                  trt_col <- setdiff(names(block_data), c("block", "plots"))
                  treatments <- as.numeric(block_data[[trt_col[1]]])
                  design_matrix[i, ] <- treatments[1:k]
                }
                
                # Verify the design
                if (nrow(design_matrix) == b && ncol(design_matrix) == k) {
                  list(design = design_matrix, k = k, lambda = lambda, r = r, b = b)
                } else {
                  NULL
                }
              } else {
                NULL
              }
            },
            error = function(e) {
              NULL
            }
          )
          
          if (!is.null(result)) {
            valid_design <- result$design
            chosen_k <- result$k
            chosen_lambda <- result$lambda
            chosen_r <- result$r
            chosen_b <- result$b
            break
          }
        }
        if (!is.null(valid_design)) break
      }
      if (!is.null(valid_design)) break
    }
    
    if (!is.null(valid_design)) {
      showNotification(
        paste0("✅ Valid BIBD generated!\n",
               "Items (v) = ", num_items, ", Block size (k) = ", chosen_k, 
               ", Blocks (b) = ", chosen_b, ", Replications (r) = ", chosen_r,
               ", λ = ", chosen_lambda),
        type = "message",
        duration = 8
      )
      return(valid_design)
    } else {
      # Find what numbers of items ARE possible with k=4,5,6
      possible_items <- c()
      for (test_v in 5:30) {
        for (test_k in 4:6) {
          if (test_k >= test_v) next
          for (test_lambda in 1:5) {
            if (is_bibd_possible(test_v, test_k, test_lambda)) {
              possible_items <- c(possible_items, test_v)
              break
            }
          }
        }
      }
      possible_items <- unique(sort(possible_items))
      
      showNotification(
        paste0("❌ No valid BIBD exists for ", num_items, " items with block sizes 4-6.\n\n",
               "Valid item counts: ", paste(head(possible_items, 15), collapse = ", "), 
               ", etc.\n\n",
               "Please use one of these item counts."),
        type = "error",
        duration = 15
      )
      return(NULL)
    }
  })
  
  # Generate Case 1 Questionnaire UI
  generate_case1_questionnaire <- function() {
    tryCatch({
      req(design(), data())
      
      # Check if design generation failed
      if (is.null(design())) {
        return(tagList(
          h4("Unable to generate questionnaire. No valid BIBD design could be created for the given number of items.", 
             style = "color: red;"),
          br()
        ))
      }
      
      design_matrix <- as.matrix(design())
      items <- as.character(data()$Item)
      
      if (length(items) < 5) {
        return(tagList(
          h4("Questionnaire will be generated when at least 5 items are available.", style = "color: gray;"),
          br()
        ))
      }
      
      num_questions <- nrow(design_matrix)
      num_cols <- ncol(design_matrix)
      
      # Validate design matrix indices
      if (any(is.na(design_matrix))) {
        stop("Design matrix contains NA values.")
      }
      if (any(design_matrix <= 0)) {
        stop("Design matrix contains non-positive indices.")
      }
      if (any(design_matrix > length(items))) {
        stop(paste("Design matrix contains indices (", max(design_matrix), 
                   ") greater than number of items (", length(items), ")."))
      }
      
      question_list1 <- lapply(1:num_questions, function(question_number) {
        indices <- as.numeric(design_matrix[question_number, ])
        question_items <- items[indices]
        
        fluidRow(
          box(
            title = paste("Q", question_number), status = "primary", solidHeader = TRUE, width = 12,
            tags$table(
              class = "table table-bordered",  
              tags$thead(tags$tr(tags$th("Best"), tags$th("Levels"), tags$th("Worst"))),
              tags$tbody(
                lapply(question_items, function(level) {
                  tags$tr(
                    tags$td(tags$input(type = "checkbox", value = level)),
                    tags$td(level),
                    tags$td(tags$input(type = "checkbox", value = level))
                  )
                }))
            )
          )
        )
      })
      
      return(tagList(
        h4("This questionnaire is generated using BIBD (Balanced Incomplete Block Design).", style = "color: blue;"),
        br(),
        question_list1
      ))
    }, error = function(e) {
      return(tagList(
        h4(paste("Error generating Case 1 questionnaire:", e$message), style = "color: red;"),
        br()
      ))
    })
  }
  
  # Download Case 1 as Word Document
  download_case1_word <- function(doc) {
    req(design(), data())
    design_matrix <- design()
    items <- data()$Item
    num_questions <- nrow(design_matrix)
    
    doc <- doc %>%
      body_add_par("BWS Case 1 Questionnaire", style = "heading 1") %>%
      body_add_par("Please select the best and worst items from each choice set.", style = "Normal")
    
    for (block in 1:num_questions) {
      question_items <- items[design_matrix[block, ]]
      
      table_data <- data.frame(
        Best = "⬜",
        Item = question_items,
        Worst = "⬜",
        stringsAsFactors = FALSE
      )
      
      question_table <- flextable(table_data) %>%
        set_header_labels(Best = "Best", Item = "Items", Worst = "Worst") %>%
        bold(part = "header") %>%
        align(align = "center", part = "all") %>%
        width(j = 1, width = 0.75) %>%
        width(j = 2, width = 5.5) %>%
        width(j = 3, width = 0.75) %>%
        border_remove() %>%
        bg(part = "header", bg = "#f0f0f0")
      
      doc <- doc %>%
        body_add_par(paste("Question", block), style = "Normal") %>%
        body_add_flextable(question_table) %>%
        body_add_par("")
    }
    
    return(doc)
  }
  
  ####========================= CASE 2 =========================#######
  # Reactive value to store Case 2
  rv2 <- reactiveValues(attributes = list())
  
  # Add attributes and levels dynamically for Case 2
  observeEvent(input$add_attribute, {
    req(input$attribute_name, input$attribute_levels)
    levels <- unlist(strsplit(input$attribute_levels, "\n"))
    levels <- trimws(levels)
    if (length(levels) < 2) {
      showNotification("Each attribute must have at least two levels.", type = "error")
      return()
    }
    
    # Create a new attribute for case 2
    new_attribute <- list(name = input$attribute_name, levels = levels)
    
    # Store the attribute in the appropriate reactive value
    rv2$attributes <- c(rv2$attributes, list(new_attribute))  # Add to Case 2
    
    # Clear input fields
    updateTextInput(session, "attribute_name", value = "")
    updateTextAreaInput(session, "attribute_levels", value = "")
    # Notify user
    showNotification("Attribute added successfully!", type = "message")
  })
  
  # UI message for Case 2
  output$case2_info <- renderUI({
    if (length(rv2$attributes) < 2) {
      return(h4("At least two attributes, each with at least two levels, are required to generate the questionnaire.", style = "color: gray;"))
    } else {
      return(NULL)  # Hide message when condition is met
    }
  })
  
  # Display added attributes for Case 2
  output$attribute_list <- renderUI({
    attributes <- rv2$attributes  # Access the reactive list for Case 2
    if (length(attributes) == 0) {
      return(p("No attributes added yet."))
    }
    
    tagList(
      lapply(seq_along(attributes), function(i) {
        attr <- attributes[[i]]
        div(
          strong(paste("Attribute", i, ":", attr$name)),  # Attribute name
          tags$ul(lapply(attr$levels, tags$li))  # Attribute levels
        )
      })
    )
  })
  
  # Generate OMED for Case 2
  observeEvent(input$generate_case2_questionnaire, {
    req(rv2$attributes)
    num_levels <- sapply(rv2$attributes, function(attr) length(attr$levels))
    
    if (length(num_levels) < 2 || any(num_levels < 2)) {
      showNotification("At least two attributes are required, and each attribute must have at least two levels.", type = "error")
      return()
    }
    
    tryCatch({
      rv2$omed_design <- DoE.base::oa.design(nlevels = num_levels, randomize = TRUE)
      showNotification("Questionnaire successfully generated!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error generating OMED:", e$message), type = "error")
    })
  })
  
  # Generate Case 2 Questionnaire UI
  generate_case2_questionnaire <- function() {
    req(rv2$omed_design, rv2$attributes)
    design_matrix <- as.matrix(rv2$omed_design)
    num_questions <- nrow(design_matrix)
    
    question_list2 <- lapply(1:num_questions, function(question_number) {
      row <- design_matrix[question_number, ]
      levels <- mapply(function(attr, level_index) {
        if (!is.na(level_index) && level_index > 0 && level_index <= length(attr$levels)) {
          return(attr$levels[level_index])
        } else {
          return("N/A")
        }
      }, rv2$attributes, as.numeric(row), SIMPLIFY = FALSE)
      
      fluidRow(
        box(
          title = paste("Q", question_number), status = "primary", solidHeader = TRUE, width = 12,
          tags$table(
            class = "table table-bordered",  
            tags$thead(tags$tr(tags$th("Best"), tags$th("Levels"), tags$th("Worst"))),
            tags$tbody(
              lapply(seq_along(levels), function(i) {
                tags$tr(
                  tags$td(tags$input(type = "checkbox", value = levels[[i]])),
                  tags$td(levels[[i]]),
                  tags$td(tags$input(type = "checkbox", value = levels[[i]]))
                )
              }))
          )
        )
      )
    })
    
    return(tagList(
      h4("This questionnaire is generated using OMED (Orthogonal Main Effects Design).", style = "color: green;"),
      br(),
      question_list2
    ))
  }
  
  # Download Case 2 as Word Document
  download_case2_word <- function(doc) {
    req(rv2$omed_design, rv2$attributes)
    design_matrix <- as.matrix(rv2$omed_design)
    num_questions <- nrow(design_matrix)
    
    doc <- doc %>%
      body_add_par("BWS Case 2 Questionnaire", style = "heading 1") %>%
      body_add_par("Please select the best and worst attribute levels for each question.", style = "Normal")
    
    for (question_number in seq_len(num_questions)) {
      row <- design_matrix[question_number, ]
      
      attribute_names <- sapply(rv2$attributes, function(attr) attr$name)
      levels <- mapply(function(attr, level_index) {
        if (!is.na(level_index) && level_index > 0 && level_index <= length(attr$levels)) {
          return(attr$levels[[level_index]])
        } else {
          return("N/A")
        }
      }, rv2$attributes, as.numeric(row), SIMPLIFY = FALSE)
      
      attribute_names <- as.character(attribute_names)
      levels <- as.character(levels)
      
      table_data <- data.frame(
        Best = rep("⬜", length(attribute_names)),
        Attribute = attribute_names,
        Level = levels,
        Worst = rep("⬜", length(attribute_names)),
        stringsAsFactors = FALSE
      )
      
      question_table <- flextable(table_data) %>%
        set_header_labels(Best = "Best", Attribute = "Attribute", Level = "Level", Worst = "Worst") %>%
        bold(part = "header") %>%
        align(align = "center", part = "all") %>%
        width(j = c(1, 4), width = 0.75) %>%
        width(j = 2, width = 3) %>%
        width(j = 3, width = 3) %>%
        border_remove() %>%
        bg(part = "header", bg = "#f0f0f0")
      
      doc <- doc %>%
        body_add_par(paste("Question", question_number), style = "Normal") %>%
        body_add_flextable(question_table) %>%
        body_add_par("")
    }
    
    return(doc)
  }
  
  ####========================= CASE 3 =========================#######
  # Reactive value to store Case 3
  rv3 <- reactiveValues(attributes = list(), profiles = NULL, choice_sets = NULL)
  
  # Add attributes and levels dynamically for Case 3
  observeEvent(input$add_attribute_case3, {
    req(input$attribute_name_case3, input$attribute_levels_case)
    levels <- unlist(strsplit(input$attribute_levels_case, "\n"))
    levels <- trimws(levels)
    if (length(levels) < 2) {
      showNotification("Each attribute must have at least two levels.", type = "error")
      return()
    }
    
    new_attribute <- list(name = make.names(input$attribute_name_case3), display_name = input$attribute_name_case3, levels = levels)
    
    rv3$attributes <- append(rv3$attributes, list(new_attribute))
    
    updateTextInput(session, "attribute_name_case3", value = "")
    updateTextAreaInput(session, "attribute_levels_case", value = "")
    showNotification("Attribute added successfully!", type = "message")
  })
  
  # UI message for Case 3
  output$case3_info <- renderUI({
    if (length(rv3$attributes) < 3) {
      return(h4("At least three attributes, each with at least two levels, are required to generate the questionnaire.", 
                style = "color: gray;"))
    } else {
      return(NULL)
    }
  })
  
  # Display added attributes for Case 3
  output$attribute_list_case3 <- renderUI({
    if (length(rv3$attributes) == 0) return(p("No attributes added yet."))
    
    tagList(lapply(seq_along(rv3$attributes), function(i) {
      attr <- rv3$attributes[[i]]
      div(
        strong(paste("Attribute", i, ":", attr$display_name)),
        tags$ul(lapply(attr$levels, tags$li))
      )
    }))
  })
  
  # Generate profiles using OA and choice sets using BIBD
  observeEvent(input$generate_case3_questionnaire, {
    req(rv3$attributes)
    num_levels <- sapply(rv3$attributes, function(attr) length(attr$levels))
    
    if (length(num_levels) < 3 || any(num_levels < 2)) {
      showNotification("At least three attributes are required, and each attribute must have at least two levels.", 
                       type = "error")
      return()
    }
    
    # STEP 1: Generate Orthogonal Array
    tryCatch({
      profiles <- DoE.base::oa.design(nlevels = num_levels, randomize = FALSE, columns = "min3")
      if (is.null(profiles) || !is.data.frame(profiles) || nrow(profiles) == 0) {
        stop("OA generation failed - no valid orthogonal array found")
      }
      
      colnames(profiles) <- sapply(rv3$attributes, `[[`, "name")
      rv3$profiles <- profiles
      
      v <- nrow(profiles)  # Number of profiles (treatments)
      k <- 3               # Block size (profiles per choice set)
      
      showNotification(
        paste0("Generated ", v, " profiles using Orthogonal Array\n",
               "Attributes: ", length(num_levels), " | Levels: ", paste(num_levels, collapse=", ")), 
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste0("ERROR: Cannot generate Orthogonal Array\n",
               "Reason: ", e$message, "\n\n",
               "Try: Reduce attributes or use standard combinations (e.g., 3-4 attributes with 2-3 levels)"), 
        type = "error",
        duration = 15
      )
      return()
    })
    
    # STEP 2: Generate perfect BIBD
    bibd_success <- FALSE
    
    tryCatch({
      # Calculate BIBD parameters
      lambda <- 1
      r_required <- (lambda * (v - 1)) / (k - 1)
      
      # Check if r is an integer
      if (r_required %% 1 != 0) {
        stop(paste0("Perfect BIBD impossible: r=", round(r_required, 2), " is not an integer"))
      }
      
      r_required <- as.integer(r_required)
      b_expected <- (v * r_required) / k
      
      if (b_expected %% 1 != 0) {
        stop(paste0("Perfect BIBD impossible: b=", round(b_expected, 2), " is not an integer"))
      }
      
      b_expected <- as.integer(b_expected)
     
      bibd_design <- agricolae::find.BIB(trt = v, b = b_expected, k = k)
      
      if (is.null(bibd_design) || !is.matrix(bibd_design) || nrow(bibd_design) == 0) {
        stop("find.BIB returned NULL or empty design")
      }
      
      # Create choice sets from BIBD design
      choice_sets <- lapply(1:nrow(bibd_design), function(i) {
        profile_indices <- bibd_design[i, ]
        rv3$profiles[profile_indices, , drop = FALSE]
      })
      
      rv3$choice_sets <- choice_sets
      bibd_success <- TRUE
      
      # Calculate actual profile usage for verification
      profile_usage <- table(as.vector(bibd_design))
      
      showNotification(
        paste0("SUCCESS! Generated ", length(choice_sets), " choice sets using PERFECT BIBD\n",
               "Parameters: v=", v, ", k=", k, ", r=", r_required, 
               ", b=", b_expected, ", λ=", lambda, "\n",
               "Each profile appears ", r_required, " times"), 
        type = "message",
        duration = 10
      )
      
    }, error = function(e) {
      showNotification(
        paste0("Perfect BIBD does not exist for v=", v, " and k=", k, "\n",
               "Reason: ", e$message, "\n",
               "Using balanced fallback design..."), 
        type = "warning",
        duration = 8
      )
    })
    
    # STEP 3: Use FALLBACK only if BIBD failed
    if (!bibd_success) {
      tryCatch({
        # Use balanced design with controlled number of choice sets
        b <- ceiling(v * 1.2)
        b <- max(b, ceiling(v / k))
        
        choice_sets <- list()
        profile_usage <- rep(0, v)
        
        for (i in 1:b) {
          min_usage <- min(profile_usage)
          candidates <- which(profile_usage == min_usage)
          
          if (length(candidates) >= k) {
            selected <- sample(candidates, k)
          } else {
            selected <- candidates
            remaining_needed <- k - length(selected)
            next_min <- min(profile_usage[profile_usage > min_usage])
            next_candidates <- which(profile_usage == next_min)
            selected <- c(selected, sample(next_candidates, remaining_needed))
          }
          
          choice_sets[[i]] <- rv3$profiles[selected, , drop = FALSE]
          profile_usage[selected] <- profile_usage[selected] + 1
        }
        
        rv3$choice_sets <- choice_sets
        
        showNotification(
          paste0("Generated ", b, " choice sets using balanced fallback design\n",
                 "Profiles: ", v, " | Usage range: ", min(profile_usage), "-", max(profile_usage), 
                 "\n(Note: This is not a perfect BIBD but maintains good balance)"), 
          type = "warning",
          duration = 10
        )
        
      }, error = function(e2) {
        showNotification(paste("Fallback design failed:", e2$message), type = "error")
        rv3$choice_sets <- NULL
      })
    }
  })
  
  # Display choice sets
  output$choice_sets_case3 <- renderUI({
    req(rv3$choice_sets)
    
    tagList(lapply(seq_along(rv3$choice_sets), function(i) {
      set <- rv3$choice_sets[[i]]
      div(
        h4(paste("Choice Set", i)),
        tableOutput(paste0("table_case3_", i))
      )
    }))
  })
  
  # Render individual choice set tables
  observe({
    req(rv3$choice_sets, rv3$attributes)
    
    lapply(seq_along(rv3$choice_sets), function(i) {
      local({
        my_i <- i
        output[[paste0("table_case3_", my_i)]] <- renderTable({
          set <- rv3$choice_sets[[my_i]]
          set_named <- as.data.frame(set, stringsAsFactors = FALSE)
          
          # Convert level indices to level names
          for (col in colnames(set_named)) {
            attr_index <- match(col, sapply(rv3$attributes, `[[`, "name"))
            if (!is.na(attr_index)) {
              attr_levels <- rv3$attributes[[attr_index]]$levels
              set_named[[col]] <- sapply(set_named[[col]], function(level_idx) {
                if (!is.na(level_idx) && level_idx >= 1 && level_idx <= length(attr_levels)) {
                  attr_levels[level_idx]
                } else {
                  ""
                }
              })
            }}
          
          # Rename columns to display names
          for (col in colnames(set_named)) {
            attr_index <- match(col, sapply(rv3$attributes, `[[`, "name"))
            if (!is.na(attr_index)) {
              colnames(set_named)[colnames(set_named) == col] <- rv3$attributes[[attr_index]]$display_name
            }
          }
          set_named
        })
      })
    })
  })
  
  # Generate Case 3 Questionnaire UI
  generate_case3_questionnaire <- function() {
    req(rv3$choice_sets, rv3$attributes)
    
    if (is.null(rv3$choice_sets) || length(rv3$choice_sets) == 0) {
      return(div(class = "text-danger", "Error: Choice sets are missing or empty."))
    }
    
    question_list3 <- lapply(seq_along(rv3$choice_sets), function(question_number) {
      question_items <- rv3$choice_sets[[question_number]]
      
      if (is.null(question_items) || nrow(question_items) == 0) {
        return(NULL)
      }
      
      fluidRow(
        box(
          title = paste("Q", question_number), 
          status = "primary", 
          solidHeader = TRUE, 
          width = 12,
          tags$table(
            class = "table table-bordered",
            tags$thead(tags$tr(
              tags$th("Attributes"),
              lapply(seq_len(nrow(question_items)), function(i) tags$th(paste("Profile", i)))
            )),
            tags$tbody(
              lapply(seq_along(rv3$attributes), function(attr_index) {
                attr_name <- rv3$attributes[[attr_index]]$name
                attr_display_name <- rv3$attributes[[attr_index]]$display_name
                attr_levels <- rv3$attributes[[attr_index]]$levels
                tags$tr(
                  tags$td(attr_display_name),
                  lapply(seq_len(nrow(question_items)), function(i) {
                    level_index <- as.numeric(question_items[i, attr_name])
                    if (!is.na(level_index) && level_index >= 1 && level_index <= length(attr_levels)) {
                      level_name <- attr_levels[level_index]
                    } else {
                      level_name <- ""
                    }
                    tags$td(level_name)
                  })
                )
              }),
              tags$tr(
                tags$th("Best"), 
                lapply(seq_len(nrow(question_items)), function(i) {
                  tags$td(tags$input(type = "radio", name = paste0("best_q", question_number), value = i))
                })
              ),
              tags$tr(
                tags$th("Worst"), 
                lapply(seq_len(nrow(question_items)), function(i) {
                  tags$td(tags$input(type = "radio", name = paste0("worst_q", question_number), value = i))
                })
              )
            )
          )
        )
      )
    })
    
    question_list3 <- Filter(Negate(is.null), question_list3)
    
    return(tagList(
      h4("This questionnaire is generated using Orthogonal Array for profiles and BIBD for choice sets.", 
         style = "color: purple;"),
      br(),
      question_list3
    ))
  }
  
  # Download Case 3 as Word Document
  download_case3_word <- function(doc) {
    req(rv3$choice_sets, rv3$attributes)
    
    doc <- doc %>%
      body_add_par("BWS Case 3 Questionnaire", style = "heading 1") %>%
      body_add_par("Please select the best and worst profiles for each choice set.", style = "Normal")
    
    for (set in seq_along(rv3$choice_sets)) {
      profiles <- rv3$choice_sets[[set]]
      profiles_named <- as.data.frame(profiles, stringsAsFactors = FALSE)
      
      for (col in colnames(profiles_named)) {
        attr_index <- match(col, sapply(rv3$attributes, `[[`, "name"))
        if (is.na(attr_index)) stop(paste("Attribute", col, "not found"))
        
        profiles_named[[col]] <- rv3$attributes[[attr_index]]$levels[profiles_named[[col]]]
      }
      
      doc <- doc %>%
        body_add_par(paste("Question", set), style = "Normal") 
      
      table_data <- data.frame(Attributes = colnames(profiles_named), stringsAsFactors = FALSE)
      
      for (i in 1:nrow(profiles_named)) {
        table_data[[paste0("Profile ", i)]] <- as.character(profiles_named[i, ])
      }
      
      best_row <- data.frame(Attributes = "Best", matrix("☐", nrow = 1, ncol = ncol(table_data) - 1), 
                             stringsAsFactors = FALSE)
      names(best_row) <- names(table_data)
      
      worst_row <- data.frame(Attributes = "Worst", matrix("☐", nrow = 1, ncol = ncol(table_data) - 1), 
                              stringsAsFactors = FALSE)
      names(worst_row) <- names(table_data)
      
      table_data <- rbind(table_data, best_row, worst_row)
      
      question_table <- flextable(table_data) %>%
        set_header_labels(Attributes = "Attributes", `Profile 1` = "Profile 1", 
                          `Profile 2` = "Profile 2", `Profile 3` = "Profile 3") %>%
        bold(part = "header") %>%
        align(align = "center", part = "all") %>%
        width(j = 1, width = 2) %>%
        width(j = 2:ncol(table_data), width = 2) %>%
        border_remove() %>%
        bg(part = "header", bg = "#f0f0f0") %>%
        bold(i = nrow(table_data) - 1, j = 1, bold = TRUE) %>%
        bold(i = nrow(table_data), j = 1, bold = TRUE)
      
      doc <- doc %>%
        body_add_flextable(question_table) %>%
        body_add_par("")
    }
    
    return(doc)
  }
  
            # Render questionnaire
            output$questionnaire <- renderUI({
              tryCatch({
                if (input$bws_case == "Case 1") {
                  generate_case1_questionnaire()
                } else if (input$bws_case == "Case 2") {
                  generate_case2_questionnaire()
                } else if (input$bws_case == "Case 3") {
                  generate_case3_questionnaire()
                }
              }, error = function(e) {
                div(class = "text-danger", paste("Error generating questionnaire:", e$message))
              })
            })
            
            #Download word document
            output$downloadQuestionnaireWord <- downloadHandler(
              filename = function() {
                paste("bws_questionnaire_", Sys.Date(), ".docx", sep = "") 
              },
              content = function(file) {
                doc <- read_docx()
                
                tryCatch({
                  if (input$bws_case == "Case 1") {
                    doc <- download_case1_word(doc)
                  } else if (input$bws_case == "Case 2") {
                    doc <- download_case2_word(doc)
                  } else if (input$bws_case == "Case 3") {
                    doc <- download_case3_word(doc)
                  }
                  
                  print(doc, target = file)
                }, error = function(e) {
                  stop("Error generating Word document: ", e$message)
                })
              },
              contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            )
  
  #######========================= STATISTICAL ANALYSIS ==================================================================================#######
  # Store raw analysis results (not rounded)
  analysis_results <- reactiveVal(NULL)
  
  # Disable the "Choose Analysis Method" dropdown initially
  shinyjs::disable("analysis_method")
  
  # Navigate to "Select Analysis" tab from "Instructions" tab
  observeEvent(input$go_to_analysis_tab, {
    updateTabsetPanel(session, "statistical_analysis", selected = "select_analysis")
  })
  # Automatically navigate to "Results" tab from "Select Analysis" tab
  observeEvent(input$run_analysis, {
    updateTabsetPanel(session, "statistical_analysis", selected = "results")
  })
  
  ## Model Datasets
  # Case 1 Dataset
  output$BWS_case1_dataset <- downloadHandler(
    filename = function() { "bws1_analysis_model.csv" },
    content = function(file) {
      file.copy("data/bws1_analysis_model.csv", file)
    },
    contentType = "text/csv"
  )
  # Case 2 Dataset
  output$BWS_case2_dataset <- downloadHandler(
    filename = function() { "bws2_analysis_model.csv" },
    content = function(file) {
      file.copy("data/bws2_analysis_model.csv", file)
    },
    contentType = "text/csv"
  )
  # Case 3 Dataset
  output$BWS_case3_dataset <- downloadHandler(
    filename = function() { "bws3_analysis_model.csv" },
    content = function(file) {
      file.copy("data/bws3_analysis_model.csv", file)
    },
    contentType = "text/csv"
  )
  
  # Update Analysis Method Dropdown based on BWS Case
  observe({
    req(input$bws_case)
    updateSelectInput(session, "analysis_method", 
                      choices = if (input$bws_case == "BWS Case 1") {
                        c("Count Analysis", "Multinomial Logit Model", "Latent Class Analysis", "Hierarchical Bayesian Model")
                      } else if (input$bws_case == "BWS Case 2") {
                        c("Count Analysis", "Modeling Methods for Profile Case")
                      } else if (input$bws_case == "BWS Case 3") {
                        c("Multinomial Logit Model", "Latent Class Analysis")
                      }else {
                        c("")
                      })
    # Enable dropdown only if a valid BWS case is selected, otherwise disable it
    shinyjs::toggleState("analysis_method", input$bws_case != "")
  })
  
  # Dynamic UI for Profile Case Method
  output$profile_case_ui <- renderUI({
    if (input$analysis_method == "Modeling Methods for Profile Case") {
      selectInput("profile_case_method", "Select Profile Case Method:", 
                  choices = c("Paired Model", "Marginal Model", "Marginal Sequential Model"))
    }
  })
  
  # Centralized function to round numeric columns in a data frame
  round_df <- function(df, digits) {
    df[] <- lapply(df, function(x) {
      if (is.numeric(x)) round(x, digits) else x
    })
    return(df)
  }
  
  ####========================= CASE 1 =========================#######
  observeEvent(input$run_analysis, {
    req(input$analysis_data, input$analysis_method)
    df <- read.csv(input$analysis_data$datapath)
    # Normalize column names to lowercase
    colnames(df) <- tolower(colnames(df))
    
    if (input$bws_case == "BWS Case 1") {
      
      # 1.1. Count Analysis  =========================
      if (input$analysis_method == "Count Analysis") {
        
        if (!("selection" %in% colnames(df)) || !("item" %in% colnames(df))) {
          showModal(modalDialog("Error: The uploaded file must contain 'Item' and 'Selection' columns.", easyClose = TRUE))
          return(NULL)
        }
        
        # Compute the number of times each item appears
        item_appearances <- df %>%
          count(item, name = "Appearances")
        
        # Compute Best, Worst, and B-W Score
        count_results_raw <- df %>%
          group_by(item) %>%
          summarise(Best = sum(selection == 1),
                    Worst = sum(selection == -1),
                    `B-W Score` = Best - Worst, .groups = 'drop') %>%
          left_join(item_appearances, by = "item") %>%
          mutate(`Standard B-W Score` = `B-W Score` / Appearances,
                 `Square Root B-W Score` = ifelse(Worst > 0, sqrt(Best / Worst), NA))
        max_sqrt_bw <- max(count_results_raw$`Square Root B-W Score`, na.rm = TRUE)
        count_results_raw <- count_results_raw %>%
          mutate(`Standard Square Root B-W Score` = `Square Root B-W Score` / max_sqrt_bw) %>%
          dplyr::select(Item = item, everything(), -Appearances)   # Remove Appearances from final result
        
        # Create rounded version for UI display only
        count_results_ui <- round_df(count_results_raw, input$round_digits)
        
        # Render results in a table format
        output$bws_results <- renderReactable({
          reactable(count_results_ui, bordered = TRUE, striped = TRUE, highlight = TRUE)
        })
        
        # Add description text below the table
        output$bws_description <- renderUI({
          HTML("<p style='font-size:18px; color: #143085;'>
             📌 The table above presents the <strong>Best-Worst scores</strong> along with their standardized versions. 
             <strong>Standard B-W Scores</strong> ranges between -1 and +1. The <strong>Standard Square Root B-W Score</strong> provides a normalized ratio-based comparison.
             The higher value of the standard B-W score or Standard Square Root B-W Score indicates stronger preferences for an item.</p>")
        })
        # Render plot
        output$main_plot <- renderPlot({
          req(count_results_ui)  # Ensure count_results exists
          ggplot(count_results_ui, aes(x = reorder(Item, `Standard B-W Score`), y = `Standard B-W Score`, fill = `Standard B-W Score`)) +
            geom_col(width = 0.2, show.legend = FALSE) +
            theme_minimal() +
            labs(title = "Standard Best-Worst Score Plot", x = "Items", y = "Standard B-W Score") +
            scale_fill_gradient(low = "blue", high = "red")+  # Customize colors
            theme(plot.title = element_text(face = "bold", size = 16),  axis.text.x = element_text(angle = 45, hjust = 1))
        })
        output$secondary_plot <- renderPlot({
          req(df)
          df <- df %>%
            rename(`Respondent ID` = respondent.id, `Item` = item)
          # Compute respondent-level scores  
          respondent_scores <- df %>%
            group_by(`Respondent ID`, `Item`) %>%
            summarise(`Standard B-W Score` = sum(selection), .groups = "drop")
          ggplot(respondent_scores, aes(x = `Standard B-W Score`)) +
            geom_histogram(binwidth = 0.3, fill = "steelblue", color = "white") +
            facet_wrap(~ Item, scales = "free", ncol = 3, strip.position = "top") +  # Separate plots for each item
            theme_minimal() +
            labs(title = "Distribution of Standard B-W Scores Across Respondents", x = "Standard B-W Score", y = "Number of Respondents") +
            theme(axis.text.x = element_text(size = 10), panel.spacing.y = unit(2, "lines"),  
                  strip.text = element_text(size = 12, face = "bold"), strip.placement = "outside", plot.margin = margin(30, 10, 30, 10)) +
            scale_x_continuous(limits = range(respondent_scores$`Standard B-W Score`), expand = c(0.05, 0.05)) +
            coord_cartesian(clip = "off") 
        }) 
        
        # Add Word Download Button
        output$download_results <- downloadHandler(
          filename = function() {
            paste0("BWS1_countanalysis_Results", ".docx")
          },
          content = function(file) {
            req(count_results_raw, df)
            ft <- flextable(count_results_raw) %>%
              autofit() %>%
              set_table_properties(layout = "autofit") %>%
              width(j = 1:ncol(count_results_raw), width = 1.5)
            # Create overall plot
            main_plot <- ggplot(count_results_raw, aes(x = reorder(Item, `Standard B-W Score`), y = `Standard B-W Score`, fill = `Standard B-W Score`)) +
              geom_col(width = 0.2, show.legend = FALSE) +
              theme_minimal() +
              labs(title = "Standard Best-Worst Score Plot", x = "Items", y = "Standard B-W Score") +
              scale_fill_gradient(low = "blue", high = "red") +
              theme(plot.title = element_text(face = "bold", size = 16), axis.text.x = element_text(angle = 45, hjust = 1))
            
            # Create item-wise plot
            respondent_scores <- df %>%
              group_by(respondent.id, item) %>%
              summarise(`Standard B-W Score` = sum(selection), .groups = "drop")
            secondary_plot <- ggplot(respondent_scores, aes(x = `Standard B-W Score`)) +
              geom_histogram(binwidth = 0.3, fill = "steelblue", color = "white") +
              facet_wrap(~ item, scales = "free", ncol = 3, strip.position = "top") +  # Separate plots for each item
              theme_minimal() +
              labs(title = "Distribution of Standard B-W Scores Across Respondents", x = "Standard B-W Score", y = "Number of Respondents") +
              theme(axis.text.x = element_text(size = 10), panel.spacing.y = unit(2, "lines"),  
                    strip.text = element_text(size = 12, face = "bold"), strip.placement = "outside", plot.margin = margin(30, 10, 30, 10)) +
              scale_x_continuous(limits = range(respondent_scores$`Standard B-W Score`), expand = c(0.05, 0.05)) +
              coord_cartesian(clip = "off") 
            # Save plots
            plot1 <- tempfile(fileext = ".png")
            plot2 <- tempfile(fileext = ".png")
            ggsave(plot1, main_plot, width = 7, height = 5, dpi = 300)
            ggsave(plot2, secondary_plot, width = 7, height = 5, dpi = 300)
            
            # Create Word document
            doc <- read_docx() %>%
              body_add_par("Best-Worst Scaling (Case 1) Analysis Results- Count Analysis", style = "heading 1") %>%
              body_add_flextable(ft) %>%
              body_add_par(" ") %>%
              body_add_par("The table above presents the Best-Worst scores along with their standardized versions. The Standard Square Root B-W Score provides a normalized ratio-based comparison.  
                           Standard B-W Scores ranges between -1 and +1. The higher value of the standard B-W score or Standard Square Root B-W Score indicates stronger preferences for an item.", style = "Normal") %>%
              body_add_par(" ") %>%
              body_add_img(plot1, width = 6, height = 4) %>%
              body_add_img(plot2, width = 6, height = 4)
            
            print(doc, target = file)
          })
      }
      
      # 1.2. Multinomial Logit Model  =========================
      else if (input$analysis_method == "Multinomial Logit Model") {
        
        # Defining required columns based on your dataset
        required_cols <- c("respondent.id", "choice.set.id", "item", "selection")
        
        # Checking if required columns are present
        if (!all(required_cols %in% colnames(df))) {
          missing_cols <- setdiff(required_cols, colnames(df))
          showModal(modalDialog(
            paste("Error: Missing columns: ", paste(missing_cols, collapse = ", "), 
                  ". Found: ", paste(colnames(df), collapse = ", ")),
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        # Renaming columns to avoid issues in mlogit (standardizing names)
        df <- df %>% 
          rename(respondent_id = respondent.id, 
                 choice_set_id = choice.set.id, 
                 item = item,
                 Selection = selection)  # Capitalize Selection to match further processing
        
        # Validating selection values
        df <- df %>%
          mutate(Selection = case_when(
            Selection == -1 ~ -1,
            Selection == 0  ~ 0,
            Selection == 1  ~ 1,
            TRUE ~ NA_real_
          ))
        
        if (any(is.na(df$Selection))) {
          showModal(modalDialog("Error: Invalid or missing values in Selection column.", easyClose = TRUE))
          return(NULL)
        }
        
        # Creating a choice indicator (1 for selected best, 0 otherwise)
        df <- df %>%
          mutate(choice = (Selection == 1))
        
        # Preparing data for mlogit
        # Creating a unique choice ID by combining respondent_id and choice_set_id
        df <- df %>%
          mutate(choice_id = paste(respondent_id, choice_set_id, sep = "_"))
        
        # --- Item preprocessing ---
        df$item <- as.factor(df$item)
        
        # Drop constant items if any (unlikely in BWS design, but check)
        if (length(unique(df$item)) == 1) {
          showModal(modalDialog("Error: Only one unique item found, cannot run model.", easyClose = TRUE))
          return(NULL)
        }
        
        # Converting to mlogit.data format
        mlogit_data <- mlogit.data(
          data = df,
          choice = "choice",
          shape = "long",
          alt.var = "item",
          chid.var = "choice_id",
          id.var = "respondent_id"
        )
        
        # Build formula (only intercepts for items)
        model_formula <- as.formula("choice ~ 1") 
        
        # Set reference level to the first sorted item for consistency
        ref_item <- levels(df$item)[1]
        
        # Running the multinomial logit model
        mlogit_model <- tryCatch(
          {
            mlogit(model_formula, data = mlogit_data, reflevel = ref_item)
          },
          error = function(e) {
            showModal(modalDialog(
              paste("Error in model estimation:", e$message), easyClose = TRUE))
            return(NULL)
          }
        )
        
        if (is.null(mlogit_model)) {
          return(NULL)
        }
        
        # Extracting model summary
        model_summary <- summary(mlogit_model)
        
        # Get coefficients matrix directly
        coef_matrix <- model_summary$CoefTable
        
        # If it's just a vector, wrap into matrix
        if (is.null(dim(coef_matrix))) {
          coef_matrix <- cbind(Estimate = coef_matrix)
        }
        
        coef_df <- as.data.frame(coef_matrix)
        coef_df <- tibble::rownames_to_column(coef_df, var = "Parameter")
        
        # Expected column names
        expected_names <- c("Estimate", "Std.Error", "z.value", "Pr(>|z|)")
        colnames(coef_df) <- c("Parameter", expected_names[seq_len(ncol(coef_df) - 1)])
        
        # Pad missing cols with NA
        for (nm in setdiff(expected_names, colnames(coef_df))) coef_df[[nm]] <- NA
        coef_df <- coef_df[, c("Parameter", expected_names)]
        p_values <- coef_df$`Pr(>|z|)`
        p_values_fmt <- paste0(
          round(p_values, input$round_digits),
          ifelse(p_values < 0.001, "***",
                 ifelse(p_values < 0.01, "**",
                        ifelse(p_values < 0.05, "*", "")))
        )
        coef_df$`Pr(>|z|)` <- p_values_fmt
        
        # Final coefficients table
        coefficients_raw <- coef_df %>%
          mutate(
            Estimate = Estimate,
            Std.Error   = `Std.Error`,
            z.value   = z.value,
            `Pr(>|z|)`  = `Pr(>|z|)`, 
            Coefficient = Estimate
          ) %>%
          dplyr::select(Parameter, Coefficient, Std.Error, z.value, `Pr(>|z|)`) 
        
        # ✅ Clean up naming for better readability
        coefficients_raw$Parameter <- gsub("\\(Intercept\\):(\\w+)", "Item:\\1", coefficients_raw$Parameter)
        
        # Calculating item utilities
        items <- levels(df$item)
        utilities_raw <- data.frame(Item = items, Utility = 0)
        
        # Set reference item to 0
        utilities_raw$Utility[utilities_raw$Item == ref_item] <- 0
        
        # Fill in estimated utilities
        for (i in seq_len(nrow(coefficients_raw))) {
          param <- coefficients_raw$Parameter[i]
          if (grepl("^Item:", param)) {
            item_name <- gsub("Item:", "", param)
            utilities_raw$Utility[utilities_raw$Item == item_name] <- coefficients_raw$Coefficient[i]
          }
        }
        
        utilities_raw <- utilities_raw %>%
          arrange(desc(Utility))
        
        # Function to calculate BIC for mlogit models
        calc_bic <- function(model, data) {
          loglik <- as.numeric(logLik(model))
          k <- attr(logLik(model), "df")   # number of estimated parameters
          n <- nrow(data)                  # number of observations
          bic <- -2 * loglik + k * log(n)
          return(bic)
        }
        
        # Model fit statistics
        model_info_raw <- data.frame(
          LogLikelihood = round(model_summary$logLik, input$round_digits),
          AIC = round(AIC(mlogit_model), input$round_digits),
          BIC = round(calc_bic(mlogit_model, mlogit_data), input$round_digits),
          Observations = nrow(mlogit_data)
        )
        
        # Create rounded versions for UI display only
        coefficients_ui <- round_df(coefficients_raw, input$round_digits)
        utilities_ui <- round_df(utilities_raw, input$round_digits)
        model_info_ui <- round_df(model_info_raw, input$round_digits)
        
        # Rendering results
        output$bws_results_multiple <- renderUI({
          tagList(
            h4("Multinomial Logit Model Coefficients"),
            reactable(coefficients_ui, bordered = TRUE, striped = TRUE, highlight = TRUE),
            h4("Item Utilities"),
            reactable(utilities_ui, bordered = TRUE, striped = TRUE, highlight = TRUE),
            h4("Model Fit Statistics"),
            reactable(model_info_ui, bordered = TRUE),
            p("📌 The coefficients table shows the estimated utilities for each item relative to the base item (e.g., the reference item set to 0). Positive coefficients indicate that an item is preferred over the base, while negative coefficients indicate lower preference."),
            
            p("📌 The item utilities table provides the overall utility scores for each item, making it easier to compare preferences across items."),
            
            p("📌 The model fit statistics (log-likelihood, AIC, BIC) show how well the model explains the observed choices. Lower values of AIC and BIC usually indicate a better-fitting model. Note: This model uses only the 'best' selections (Selection = 1)."), 
            p("Significance is indicated with stars: * (p<0.05), ** (p<0.01), *** (p<0.001).")
          )
        })
        
        # Main plot: Bar plot of item utilities
        output$main_plot <- renderPlot({
          ggplot(utilities_ui, aes(x = reorder(Item, Utility), y = Utility, fill = Utility)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            theme_minimal() +
            labs(title = "Item Utilities from Multinomial Logit Model", x = "Item", y = "Utility") +
            scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
            theme(axis.text.y = element_text(size = 8))
        })
        output$secondary_plot <- renderUI({NULL})
        
        # Download results
        output$download_results <- downloadHandler(
          filename = function() { "BWS1_MLogit_Results.docx" },
          content = function(file) {
            main_plot <- ggplot(utilities_raw, aes(x = reorder(Item, Utility), y = Utility, fill = Utility)) +
              geom_bar(stat = "identity") +
              coord_flip() +
              theme_minimal() +
              labs(title = "Item Utilities from Multinomial Logit Model", x = "Item", y = "Utility") +
              scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
              theme(axis.text.y = element_text(size = 8))
            plot_file <- tempfile(fileext = ".png")
            ggsave(plot_file, main_plot, width = 7, height = 5, dpi = 300)
            
            doc <- read_docx() %>%
              body_add_fpar(fpar(ftext("Best-Worst Scaling (Case 1) Analysis Results – Multinomial Logit Model", fp_text(font.size = 16, bold = TRUE)))) %>%
              body_add_fpar(fpar(ftext("Model Coefficients", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(coefficients_raw)) %>%
              body_add_fpar(fpar(ftext("Item Utilities", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(utilities_raw)) %>%
              body_add_fpar(fpar(ftext("Model Fit Statistics", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(model_info_raw)) %>%
              body_add_par("📌 The coefficients table shows the estimated utilities for each item relative to the base item (e.g., the reference item set to 0). Positive coefficients indicate that an item is preferred over the base, while negative coefficients indicate lower preference.", style = "Normal") %>%
              body_add_par("📌 The item utilities table provides the overall utility scores for each item, making it easier to compare preferences across items.", style = "Normal") %>%
              body_add_par("📌 The model fit statistics (log-likelihood, AIC, BIC) show how well the model explains the observed choices. Lower values of AIC and BIC usually indicate a better-fitting model. Note: This model uses only the 'best' selections (Selection = 1).", style = "Normal") %>%
              body_add_par("Significance is indicated with stars: * (p<0.05), ** (p<0.01), *** (p<0.001).") %>%
              body_add_par(" ") %>%
              body_add_img(plot_file, width = 6, height = 4) 
            print(doc, target = file)
          },
          contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
      }
      
      # 1.3. Latent Class Analysis  =========================
      else if (input$analysis_method == "Latent Class Analysis") {
        required_cols <- c("respondent.id", "choice.set.id", "item", "selection")
        if (!all(required_cols %in% colnames(df))) {
          showModal(modalDialog("Error: The uploaded file must contain 'Respondent ID', 'Choice Set ID', 'Item', and 'Selection' columns.", easyClose = TRUE))
          return(NULL)
        }
        
        df <- df %>%
          mutate(selection = case_when(
            selection == -1 ~ -1,
            selection == 0  ~  0,
            selection == 1  ~  1
          ))
        
        if (any(is.na(df$selection))) {
          showModal(modalDialog("Error: Missing values detected in selection column.", easyClose = TRUE))
          return(NULL)
        }
        
        df_scores <- df %>%
          group_by(respondent.id, item) %>%
          summarise(score = sum(selection), .groups = "drop") %>%
          pivot_wider(names_from = item, values_from = score, values_fill = list(score = 0)) %>%
          arrange(respondent.id)
        
        df_scores <- as.data.frame(df_scores)
        manifest_vars <- names(df_scores)[-1]
        
        df_scores[manifest_vars] <- lapply(df_scores[manifest_vars], function(x) as.factor(as.character(x)))
        
        formula <- as.formula(paste("cbind(", paste(manifest_vars, collapse = ", "), ") ~ 1"))
        
        set.seed(123)
        best_model <- NULL
        best_bic <- Inf
        best_k <- NULL
        best_aic <- NULL
        best_llik <- NULL
        model_metrics <- data.frame(K = integer(), AIC = numeric(), BIC = numeric(), LogLikelihood = numeric())
        
        for (k in 2:5) {
          model <- tryCatch(
            {
              poLCA(formula, data = df_scores, nclass = k, nrep = 3, maxiter = 2000, verbose = FALSE)
            },
            error = function(e) {
              return(NULL)
            }
          )
          if (!is.null(model)) {
            model_metrics <- rbind(model_metrics, data.frame(K = k, AIC = model$aic, BIC = model$bic, LogLikelihood = model$llik))
            cat(paste("Model with", k, "classes: AIC =", model$aic, ", BIC =", model$bic, ", LogLikelihood =", model$llik, "\n"))
            if (model$bic < best_bic) {
              best_model <- model
              best_bic <- model$bic
              best_aic <- model$aic
              best_llik <- model$llik
              best_k <- k
            }}}
        
        if (is.null(best_model)) {
          showModal(modalDialog("Error: LCA model could not be estimated. Check data format.", easyClose = TRUE))
          return(NULL)
        }
        
        df_scores$Class <- best_model$predclass
        
        class_summary_raw <- df_scores %>%
          group_by(Class) %>%
          summarise(n = n(), Proportion = n() / nrow(df_scores)) %>%
          ungroup()
        
        model_info_raw <- data.frame(AIC = best_aic, BIC = best_bic, LogLikelihood = best_llik, Classes = best_k)
        
        # Add class-level item BWS summaries (interpretable profiles)
        class_profile_raw <- df %>%
          left_join(df_scores[, c("respondent.id", "Class")], by = "respondent.id") %>%
          group_by(Class, item) %>%
          summarise(mean_score = mean(selection), .groups = "drop") %>%
          pivot_wider(names_from = item, values_from = mean_score) %>%
          arrange(Class)
        
        # Create rounded versions for UI display only
        model_info_ui <- round_df(model_info_raw, input$round_digits)
        class_summary_ui <- round_df(class_summary_raw, input$round_digits)
        class_profile_ui <- round_df(class_profile_raw, input$round_digits)
        
        # Rendering results
        output$bws_results_multiple <- renderUI({
          tagList(
            h4("Model Info (AIC, BIC, LogLikelihood, Best K)"),
            reactable(model_info_ui, bordered = TRUE),
            h4("Latent Class Proportions"),
            reactable(class_summary_ui, bordered = TRUE, striped = TRUE, highlight = TRUE),
            h4("Class Profiles (Mean BWS Scores by Item)"),
            reactable(class_profile_ui, bordered = TRUE, striped = TRUE, highlight = TRUE),
            p("📌 The class profiles table shows the average Best-Worst Scaling (BWS) scores for each item, across different latent classes. 
      Higher positive values indicate items that are more likely to be selected as 'best', while lower (or negative) values suggest items more likely to be selected as 'worst' by respondents in that class. 
      Use this information to understand the preferences of each latent class.")
          )
        })
        
        output$main_plot <- renderPlot({
          ggplot(class_summary_ui, aes(x = factor(Class), y = Proportion, fill = factor(Class))) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(title = paste("Latent Class Proportions (K=", best_k, ")"), x = "Latent Class", y = "Proportion") +
            scale_fill_brewer(palette = "Set2")
        })
        
        output$secondary_plot <- renderUI({NULL})
        
        #Download Results
        output$download_results <- downloadHandler(
          filename = function() {
            paste("BWS1_LCA_Results", ".docx")
          },
          content = function(file) {
            doc <- read_docx()
            doc <- doc %>%
              body_add_fpar(
                fpar(ftext("Best-Worst Scaling (Case 1) Analysis Results – Latent Class Analysis", 
                           fp_text(font.size = 16, bold = TRUE))))
            doc <- doc %>%
              body_add_fpar(fpar(ftext("Model Info (AIC, BIC, LogLikelihood, Best K)", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(model_info_raw))
            doc <- doc %>%
              body_add_fpar(fpar(ftext("Latent Class Proportions", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(class_summary_raw))
            doc <- doc %>%
              body_add_fpar(fpar(ftext("Class Profiles (Mean BWS Scores by Item)", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(class_profile_raw))
            doc <- doc %>%
              body_add_par("📌 The class profiles table shows the average Best-Worst Scaling (BWS) scores for each item, across different latent classes. Higher positive values indicate items that are more likely to be selected as 'best', while lower (or negative) values suggest items more likely to be selected as 'worst' by respondents in that class. Use this information to understand the preferences of each latent class.", style = "Normal")
            print(doc, target = file)
          }
        )}
      
      # 1.4. Hierarchical Bayesian Model  =========================
      else if (input$analysis_method == "Hierarchical Bayesian Model") {
        if (!all(c("respondent.id", "choice.set.id", "item", "selection") %in% colnames(df))) {
          showModal(modalDialog("Error: The uploaded file must contain 'respondent.id', 'choice.set.id', 'item', and 'selection' columns.", easyClose = TRUE))
          return(NULL)
        }
        
        # Prepare data
        respondents <- unique(df$respondent.id)
        items <- unique(df$item)
        
        N <- length(respondents)
        K <- length(items)
        
        showModal(modalDialog(
          HTML(paste0("<h4>Running Hierarchical Bayesian Model</h4>")),
          easyClose = FALSE, footer = NULL
        ))
        
        # Aggregate counts
        item_counts <- data.frame(
          item = items,
          best = 0,
          worst = 0,
          neither = 0,
          stringsAsFactors = FALSE
        )
        for (i in 1:nrow(item_counts)) {
          item_name <- item_counts$item[i]
          item_data <- df[df$item == item_name, ]
          item_counts$best[i] <- sum(item_data$selection == 1)
          item_counts$worst[i] <- sum(item_data$selection == -1)
          item_counts$neither[i] <- sum(item_data$selection == 0)
        }
        
        # Individual-level counts
        indiv_data <- list()
        for (i in 1:N) {
          resp_id <- respondents[i]
          resp_data <- df[df$respondent.id == resp_id, ]
          resp_counts <- data.frame(
            item = items,
            best = 0,
            worst = 0,
            stringsAsFactors = FALSE
          )
          for (j in 1:K) {
            item_name <- items[j]
            item_resp_data <- resp_data[resp_data$item == item_name, ]
            resp_counts$best[j] <- sum(item_resp_data$selection == 1)
            resp_counts$worst[j] <- sum(item_resp_data$selection == -1)
          }
          indiv_data[[i]] <- resp_counts
        }
        
        # Dirichlet-Multinomial for population
        alpha_prior <- rep(1, K)
        alpha_post_best <- item_counts$best + alpha_prior
        
        # MCMC settings
        n_iter <- 5000
        n_burn <- 1000
        n_thin <- 4
        n_keep <- (n_iter - n_burn) / n_thin
        
        theta_draws <- matrix(0, n_keep, K)
        indiv_theta_draws <- array(0, dim = c(n_keep, N, K))
        set.seed(1234)
        draw_idx <- 1
        
        for (iter in 1:n_iter) {
          theta_pop <- MCMCpack::rdirichlet(1, alpha_post_best)[1, ]
          
          for (i in 1:N) {
            resp_counts <- indiv_data[[i]]
            alpha_indiv <- resp_counts$best + theta_pop * 10
            theta_indiv <- MCMCpack::rdirichlet(1, alpha_indiv)[1, ]
            if (iter > n_burn && (iter - n_burn) %% n_thin == 0) {
              indiv_theta_draws[draw_idx, i, ] <- theta_indiv
            }
          }
          
          if (iter > n_burn && (iter - n_burn) %% n_thin == 0) {
            theta_draws[draw_idx, ] <- theta_pop
            draw_idx <- draw_idx + 1
          }
        }
        
        # Compute population mean utility
        pop_utilities <- log(colMeans(theta_draws))
        pop_utilities <- pop_utilities - mean(pop_utilities)
        
        # Individual-level utilities
        indiv_utilities_log <- array(0, dim = c(n_keep, N, K))
        for (draw in 1:n_keep) {
          for (i in 1:N) {
            utils <- log(indiv_theta_draws[draw, i, ])
            indiv_utilities_log[draw, i, ] <- utils - mean(utils)
          }
        }
        
        individual_utilities <- matrix(0, N, K)
        for (i in 1:N) {
          individual_utilities[i, ] <- colMeans(indiv_utilities_log[, i, ])
        }
        colnames(individual_utilities) <- items
        rownames(individual_utilities) <- respondents
        
        # Compute Standard Error (SE) across individuals
        se_utilities <- apply(individual_utilities, 2, function(x) sd(x) / sqrt(N))
        
        # Compute t-values and p-values
        t_values <- pop_utilities / se_utilities
        p_values <- 2 * pt(-abs(t_values), df = N-1)
        
        # Append significance asterisks directly to p-value column
        p_values_fmt <- paste0(round(p_values, input$round_digits),
                               ifelse(p_values<0.001,"***", ifelse(p_values<0.01,"**", ifelse(p_values<0.05,"*",""))))
        
        # Create results table
        pop_results_raw <- data.frame(Item = items,
                                      "Mean Utility" = pop_utilities, "SE" = se_utilities,
                                      "t value" = t_values, "p value" = p_values_fmt,
                                      check.names = FALSE, row.names = NULL
        )
        pop_results_raw <- pop_results_raw[order(-pop_results_raw$`Mean Utility`), ]
        
        # Create rounded version for UI display only
        pop_results_ui <- round_df(pop_results_raw, input$round_digits)
        
        removeModal()
        
        # Render Reactable table
        output$bws_results <- renderReactable({
          reactable(pop_results_raw, bordered = TRUE, striped = TRUE, highlight = TRUE,defaultPageSize = 20,
                    defaultSorted = list("Mean Utility" = "desc"),
                    columns = list(
                      Item = colDef(minWidth = 150),
                      "Mean Utility" = colDef(format = colFormat(digits = 4)),
                      "SE" = colDef(format = colFormat(digits = 4)),
                      "t value" = colDef(format = colFormat(digits = 3)),
                      "p value" = colDef()
                    ))
        })
        
        # Description text
        output$bws_description <- renderUI({
          HTML("<p style='font-size:18px; color: #143085;'>
         📌 The table above shows the <strong>Hierarchical Bayesian estimates</strong> of item utilities. 
         <strong>Mean Utility</strong> represents the average preference for each item across respondents, 
         while <strong>SE</strong> (Standard Error) indicates the variability in preferences. Higher mean utilities indicate stronger preferences.
               *** p<0.001, ** p<0.01, * p<0.05.</p>")
        })
        
        # Render plot
        output$main_plot <- renderPlot({
          req(pop_results_ui)  
          tryCatch({
            ggplot(pop_results_ui, aes(x = reorder(Item, `Mean Utility`), y = `Mean Utility`, fill = `Mean Utility`)) +
              geom_col(width = 0.2, show.legend = FALSE) +
              theme_minimal() +
              labs(title = "Population-Level Mean Utilities", x = "Items", y = "Mean Utility") +
              scale_fill_gradient(low = "blue", high = "red") +
              theme(plot.title = element_text(face = "bold", size = 16), axis.text.x = element_text(angle = 45, hjust = 1))
          })
        })
        output$secondary_plot <- renderUI({
          NULL  # No secondary plot for HBM
        })
        
        # Download results as Word document
        output$download_results <- downloadHandler(
          filename = function() {
            paste0("BWS1_HB_Results", ".docx")
          },
          content = function(file) {
            ft <- flextable(pop_results_raw) %>%
              autofit() %>%
              set_table_properties(layout = "autofit") %>%
              width(j = 1:ncol(pop_results_raw), width = 1.5)
            
            main_plot <- ggplot(pop_results_raw, aes(x = reorder(Item, `Mean Utility`), y = `Mean Utility`, fill = `Mean Utility`)) +
              geom_col(width = 0.2, show.legend = FALSE) +
              theme_minimal() +
              labs(title = "Population-Level Mean Utilities", x = "Items", y = "Mean Utility") +
              scale_fill_gradient(low = "blue", high = "red") +
              theme(plot.title = element_text(face = "bold", size = 16), axis.text.x = element_text(angle = 45, hjust = 1))
            
            plot_file <- tempfile(fileext = ".png")
            ggsave(plot_file, main_plot, width = 7, height = 5, dpi = 300)
            
            doc <- read_docx() %>%
              body_add_par("Best-Worst Scaling (Case 1) Analysis Results- Hierarchical Bayesian Model", style = "heading 1") %>%
              body_add_flextable(ft) %>%
              body_add_par(" ") %>%
              body_add_par("The table shows the Hierarchical Bayesian estimates of item utilities. Mean Utility represents the average preference for each item across respondents, while SD Utility (Standard Deviation of Utility) indicates the variability in preferences.
                           Higher mean utilities indicate stronger preferences.", style = "Normal") %>%
              body_add_par(" ") %>%
              body_add_img(plot_file, width = 6, height = 4)
            
            print(doc, target = file)
          },
          contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
      }
    }})
  
  ####========================= CASE 2 =========================#######
  observeEvent(input$run_analysis, {
    req(input$analysis_data, input$analysis_method)
    df <- read.csv(input$analysis_data$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    
    if (input$bws_case == "BWS Case 2") {
      # Normalize column names to lowercase
      colnames(df) <- tolower(colnames(df))
      
      # 2.1. Count Analysis =========================
      if (input$analysis_method == "Count Analysis") { 
        if (!("selection" %in% colnames(df)) || !("attribute level" %in% colnames(df))) {
          showModal(modalDialog("Error: The uploaded file must contain 'Item' and 'Selection' columns.", easyClose = TRUE))
          return(NULL)
        }
        
        # Compute the number of times each item appears
        level_appearances <- df %>%
          count(`attribute level`, name = "Appearances")
        
        # Compute Best, Worst, and B-W Score
        count_results_raw <- df %>%
          group_by(`attribute level`) %>%
          summarise(Best = sum(selection == 1),
                    Worst = sum(selection == -1),
                    `B-W Score` = Best - Worst, .groups = 'drop') %>%
          left_join(level_appearances, by = "attribute level") %>%
          mutate(`Standard B-W Score` = `B-W Score` / Appearances,
                 `Square Root B-W Score` = ifelse(Worst > 0, sqrt(Best / Worst), NA))
        max_sqrt_bw <- max(count_results_raw$`Square Root B-W Score`, na.rm = TRUE)
        count_results_raw <- count_results_raw %>%
          mutate(`Standard Square Root B-W Score` = `Square Root B-W Score` / max_sqrt_bw) %>%
          dplyr::select(`attribute level`, everything(), -Appearances)   # Remove Appearances from final result
        
        # Create rounded version for UI display only
        count_results_ui <- round_df(count_results_raw, input$round_digits)
        
        # Render results in a table format
        output$bws_results <- renderReactable({
          reactable(count_results_ui, bordered = TRUE, striped = TRUE, highlight = TRUE)
        })
        
        # Add description text below the table
        output$bws_description <- renderUI({
          HTML("<p style='font-size:18px; color: #143085;'>
             📌 The table above presents the <strong>Best-Worst scores</strong> along with their standardized versions. 
             <strong>Standard B-W Scores</strong> ranges between -1 and +1. The <strong>Standard Square Root B-W Score</strong> provides a normalized ratio-based comparison.
             The higher value of the standard B-W score or Standard Square Root B-W Score indicates stronger preferences for an item.</p>")
        })
        # Render plot
        output$main_plot <- renderPlot({
          req(count_results_ui)  # Ensure count_results exists
          ggplot(count_results_ui, aes(x = reorder(`attribute level`, `Standard B-W Score`), y = `Standard B-W Score`, fill = `Standard B-W Score`)) +
            geom_col(width = 0.2, show.legend = FALSE) +
            theme_minimal() +
            labs(title = "Standard Best-Worst Score Plot", x = "Attribute Level", y = "Standard B-W Score") +
            scale_fill_gradient(low = "blue", high = "red")+  # Customize colors
            theme(plot.title = element_text(face = "bold", size = 16),  axis.text.x = element_text(angle = 45, hjust = 1))
        })
        output$secondary_plot <- renderPlot({
          req(df)
          # Compute respondent-level scores  
          respondent_scores_raw <- df %>%
            group_by(`respondent id`, `attribute level`) %>%
            summarise(`Standard B-W Score` = sum(selection), .groups = "drop")
          respondent_scores_ui <- round_df(respondent_scores_raw, input$round_digits)
          ggplot(respondent_scores_ui, aes(x = `Standard B-W Score`)) +
            geom_histogram(binwidth = 0.3, fill = "steelblue", color = "white") +
            facet_wrap(~ `attribute level`, scales = "free", ncol = 3, strip.position = "top") +  # Separate plots for each attribute level
            theme_minimal() +
            labs(title = "Distribution of Standard B-W Scores Across Respondents", x = "Standard B-W Score", y = "Number of Respondents") +
            theme(axis.text.x = element_text(size = 10), panel.spacing.y = unit(2, "lines"),  
                  strip.text = element_text(size = 12, face = "bold"), strip.placement = "outside", plot.margin = margin(30, 10, 30, 10)) +
            scale_x_continuous(limits = range(respondent_scores_ui$`Standard B-W Score`), expand = c(0.05, 0.05)) +
            coord_cartesian(clip = "off") 
        }) 
        
        # Add Word Download Button
        output$download_results <- downloadHandler(
          filename = function() {
            paste0("BWS2_countanalysis_Results", ".docx")
          },
          content = function(file) {
            req(count_results_raw, df)
            ft <- flextable(count_results_raw) %>%
              autofit() %>%
              set_table_properties(layout = "autofit") %>%
              width(j = 1:ncol(count_results_raw), width = 1.5)
            # Create overall plot
            main_plot <- ggplot(count_results_raw, aes(x = reorder(`attribute level`, `Standard B-W Score`), y = `Standard B-W Score`, fill = `Standard B-W Score`)) +
              geom_col(width = 0.2, show.legend = FALSE) +
              theme_minimal() +
              labs(title = "Standard Best-Worst Score Plot", x = "Attribute Levels", y = "Standard B-W Score") +
              scale_fill_gradient(low = "blue", high = "red") +
              theme(plot.title = element_text(face = "bold", size = 16), axis.text.x = element_text(angle = 45, hjust = 1))
            
            # Create attribute level-wise plot
            respondent_scores <- df %>%
              group_by(`respondent id`, `attribute level`) %>%
              summarise(`Standard B-W Score` = sum(selection), .groups = "drop")
            secondary_plot <- ggplot(respondent_scores, aes(x = `Standard B-W Score`)) +
              geom_histogram(binwidth = 0.3, fill = "steelblue", color = "white") +
              facet_wrap(~ `attribute level`, scales = "free", ncol = 3, strip.position = "top") +
              theme_minimal() +
              labs(title = "Distribution of Standard B-W Scores Across Respondents", x = "Standard B-W Score", y = "No. of Respondents") +
              theme(axis.text.x = element_text(size = 10), panel.spacing.y = unit(2, "lines"),  
                    strip.text = element_text(size = 12, face = "bold"), strip.placement = "outside", plot.margin = margin(30, 10, 30, 10)) +
              scale_x_continuous(limits = range(respondent_scores$`Standard B-W Score`), expand = c(0.05, 0.05)) +
              coord_cartesian(clip = "off") 
            # Save plots
            plot1 <- tempfile(fileext = ".png")
            plot2 <- tempfile(fileext = ".png")
            ggsave(plot1, main_plot, width = 7, height = 5, dpi = 300)
            ggsave(plot2, secondary_plot, width = 7, height = 5, dpi = 300)
            
            # Create Word document
            doc <- read_docx() %>%
              body_add_par("Best-Worst Scaling (Case 2) Analysis Results- Count Analysis", style = "heading 1") %>%
              body_add_flextable(ft) %>%
              body_add_par(" ") %>%
              body_add_par("The table above presents the Best-Worst scores along with their standardized versions. The Standard Square Root B-W Score provides a normalized ratio-based comparison.  
                           Standard B-W Scores ranges between -1 and +1. The higher value of the standard B-W score or Standard Square Root B-W Score indicates stronger preferences for an item.", style = "Normal") %>%
              body_add_par(" ") %>%
              body_add_img(plot1, width = 6, height = 4) %>%
              body_add_img(plot2, width = 6, height = 6)
            
            print(doc, target = file)
          })
      }
      
      # 2.2. Modeling Methods for Profile Case =========================
      else if (input$analysis_method == "Modeling Methods for Profile Case") {
        req(input$profile_case_method)
        if (!all(c("respondent id", "choice set id", "attribute level", "selection") %in% colnames(df))) {
          showModal(modalDialog("Error: The uploaded file must contain 'respondent id', 'choice set id', 'attribute level', and 'selection' columns.", easyClose = TRUE))
          return(NULL)
        }
        
        showModal(modalDialog(
          paste("Running", input$profile_case_method, "model... Please wait."),
          easyClose = FALSE, footer = NULL
        ))
        
        df$selection <- as.numeric(df$selection)
        results <- NULL
        model_fit <- NULL
        
        # ---------------- Paired Model ----------------
        if (input$profile_case_method == "Paired Model") {
          df_pairs <- df %>%
            group_by(`respondent id`, `choice set id`) %>%
            summarise(best = `attribute level`[selection == 1][1],
                      worst = `attribute level`[selection == -1][1], 
                      .groups = "drop") %>%
            filter(!is.na(best) & !is.na(worst))
          
          df_long <- df_pairs %>%
            pivot_longer(cols = c(best, worst),
                         names_to = "type", values_to = "level") %>%
            mutate(choice = ifelse(type == "best", 1, 0),
                   chid = paste(`respondent id`, `choice set id`, sep = "_"))
          
          fit <- clogit(choice ~ level + strata(chid), data = df_long)
          
          # Extract coefficients and statistics
          coefs <- coef(fit)
          se <- sqrt(diag(vcov(fit)))
          z_values <- coefs / se
          p_values <- 2 * pnorm(-abs(z_values))
          sig_stars <- ifelse(p_values < 0.001, "***",
                              ifelse(p_values < 0.01, "**",
                                     ifelse(p_values < 0.05, "*", "")))
          
          results <- data.frame(
            `Attribute Level` = names(coefs),
            `Mean Utility` = coefs,
            `SE` = se,
            `z-value` = z_values,
            `p-value` = paste0(format(round(p_values, 4), nsmall = 4), " ", sig_stars),
            check.names = FALSE, row.names = NULL
          )
          
          # Clean "level" prefix from Attribute Level
          results$`Attribute Level` <- gsub("^(`?attribute level`?|level)", "", results$`Attribute Level`)
          
          # Model fit statistics
          model_fit <- data.frame(
            Statistic = c("Log-Likelihood", "AIC", "BIC", "N (Observations)", "N (Choice Sets)"),
            Value = c(
              round(fit$loglik[2], 2),
              round(AIC(fit), 2),
              round(BIC(fit), 2),
              nrow(df_long),
              length(unique(df_long$chid))
            ), 
            row.names = NULL
          )
        }
        
        # ---------------- Marginal Model ----------------
        else if (input$profile_case_method == "Marginal Model") {
          # Best model
          df_best_marg <- df %>%
            mutate(choice_best = ifelse(selection == 1, 1, 0),
                   chid = paste(`respondent id`, `choice set id`, sep = "_"))
          
          fit_best <- clogit(choice_best ~ `attribute level` + strata(chid), data = df_best_marg)
          
          # Worst model
          df_worst_marg <- df %>%
            mutate(choice_worst = ifelse(selection == -1, 1, 0),
                   chid = paste(`respondent id`, `choice set id`, sep = "_"))
          
          fit_worst <- clogit(choice_worst ~ `attribute level` + strata(chid), data = df_worst_marg)
          
          # Combine utilities
          all_levels <- union(names(coef(fit_best)), names(coef(fit_worst)))
          
          utility_best <- setNames(rep(0, length(all_levels)), all_levels)
          utility_worst <- setNames(rep(0, length(all_levels)), all_levels)
          
          utility_best[names(coef(fit_best))] <- coef(fit_best)
          utility_worst[names(coef(fit_worst))] <- coef(fit_worst)
          
          combined_utility <- utility_best - utility_worst
          
          # Get standard errors
          var_best <- setNames(rep(0, length(all_levels)), all_levels)
          var_worst <- setNames(rep(0, length(all_levels)), all_levels)
          
          var_best[names(coef(fit_best))] <- diag(vcov(fit_best))
          var_worst[names(coef(fit_worst))] <- diag(vcov(fit_worst))
          
          combined_se <- sqrt(var_best + var_worst)
          
          # Calculate statistics
          z_values <- combined_utility / combined_se
          p_values <- 2 * pnorm(-abs(z_values))
          sig_stars <- ifelse(p_values < 0.001, "***",
                              ifelse(p_values < 0.01, "**",
                                     ifelse(p_values < 0.05, "*", "")))
          
          results <- data.frame(
            `Attribute Level` = all_levels,
            `Mean Utility` = combined_utility,
            `SE` = combined_se,
            `z-value` = z_values,
            `p-value` = paste0(format(round(p_values, 4), nsmall = 4), " ", sig_stars),
            check.names = FALSE, row.names = NULL
          )
          
          # Clean "level" prefix from Attribute Level
          results$`Attribute Level` <- gsub("^(`?attribute level`?|level)", "", results$`Attribute Level`)
          
          # Model fit statistics (combined from both models)
          model_fit <- data.frame(
            Statistic = c("Log-Likelihood (Best)", "Log-Likelihood (Worst)", 
                          "AIC (Best)", "AIC (Worst)", 
                          "BIC (Best)", "BIC (Worst)",
                          "N (Observations)", "N (Choice Sets)"),
            Value = c(
              round(fit_best$loglik[2], 2),
              round(fit_worst$loglik[2], 2),
              round(AIC(fit_best), 2),
              round(AIC(fit_worst), 2),
              round(BIC(fit_best), 2),
              round(BIC(fit_worst), 2),
              nrow(df_best_marg),
              length(unique(df_best_marg$chid)),
              row.names = NULL
            )
          )
        }
        
        # ---------------- Marginal Sequential Model ----------------
        else if (input$profile_case_method == "Marginal Sequential Model") {
          # Step 1: Model best choice among ALL items
          df_best_seq <- df %>%
            mutate(choice = ifelse(selection == 1, 1, 0),
                   chid = paste(`respondent id`, `choice set id`, sep = "_"))
          
          fit_best <- clogit(choice ~ `attribute level` + strata(chid), data = df_best_seq)
          
          # Step 2: Model worst choice among remaining items
          df_worst_seq <- df %>%
            filter(selection != 1) %>%
            mutate(choice = ifelse(selection == -1, 1, 0),
                   chid = paste(`respondent id`, `choice set id`, sep = "_"))
          
          fit_worst <- clogit(choice ~ `attribute level` + strata(chid), data = df_worst_seq)
          
          # Combine utilities
          all_levels <- union(names(coef(fit_best)), names(coef(fit_worst)))
          
          utility_best <- setNames(rep(0, length(all_levels)), all_levels)
          utility_worst <- setNames(rep(0, length(all_levels)), all_levels)
          
          utility_best[names(coef(fit_best))] <- coef(fit_best)
          utility_worst[names(coef(fit_worst))] <- coef(fit_worst)
          
          combined_utility <- utility_best - utility_worst
          
          # Calculate combined standard errors
          var_best <- setNames(rep(0, length(all_levels)), all_levels)
          var_worst <- setNames(rep(0, length(all_levels)), all_levels)
          
          var_best[names(coef(fit_best))] <- diag(vcov(fit_best))
          var_worst[names(coef(fit_worst))] <- diag(vcov(fit_worst))
          
          combined_se <- sqrt(var_best + var_worst)
          
          # Calculate statistics
          z_values <- combined_utility / combined_se
          p_values <- 2 * pnorm(-abs(z_values))
          sig_stars <- ifelse(p_values < 0.001, "***",
                              ifelse(p_values < 0.01, "**",
                                     ifelse(p_values < 0.05, "*", "")))
          
          results <- data.frame(
            `Attribute Level` = all_levels,
            `Mean Utility` = combined_utility,
            `SE` = combined_se,
            `z-value` = z_values,
            `p-value` = paste0(format(round(p_values, 4), nsmall = 4), " ", sig_stars),
            check.names = FALSE, row.names = NULL
          )
          
          # Clean "level" prefix from Attribute Level
          results$`Attribute Level` <- gsub("^(`?attribute level`?|level)", "", results$`Attribute Level`)
          
          # Model fit statistics
          model_fit <- data.frame(
            Statistic = c("Log-Likelihood (Best)", "Log-Likelihood (Worst)", 
                          "AIC (Best)", "AIC (Worst)", 
                          "BIC (Best)", "BIC (Worst)",
                          "N (Observations - Best)", "N (Observations - Worst)",
                          "N (Choice Sets)"),
            Value = c(
              round(fit_best$loglik[2], 2),
              round(fit_worst$loglik[2], 2),
              round(AIC(fit_best), 2),
              round(AIC(fit_worst), 2),
              round(BIC(fit_best), 2),
              round(BIC(fit_worst), 2),
              nrow(df_best_seq),
              nrow(df_worst_seq),
              length(unique(df_best_seq$chid))
            ),
            row.names = NULL
          )
        }
        
        removeModal()
        
        # Round numeric columns for UI (except p-value which is already formatted)
        results_ui <- results %>% 
          mutate(across(c(`Mean Utility`, `SE`, `z-value`), ~ round(.x, input$round_digits)))
        
        ##  Render Results Table 
        output$bws_results <- renderReactable({
          reactable(results_ui, bordered = TRUE, striped = TRUE, highlight = TRUE,
                    columns = list(
                      `Attribute Level` = colDef(name = "Attribute Level", minWidth = 150),
                      `Mean Utility` = colDef(name = "Mean Utility", format = colFormat(digits = input$round_digits)),
                      `SE` = colDef(name = "SE", format = colFormat(digits = input$round_digits)),
                      `z-value` = colDef(name = "z-value", format = colFormat(digits = input$round_digits)),
                      `p-value` = colDef(name = "p-value", minWidth = 120)
                    ))
        })
        results_ui$`Attribute Level` <- gsub("^(`?attribute level`?|level)", "", results_ui$`Attribute Level`)
        
            # # Plot
        output$main_plot <- renderPlot({
          ggplot(results_ui, aes(x = reorder(`Attribute Level`, `Mean Utility`), 
                                 y = `Mean Utility`, fill = `Mean Utility`)) +
            geom_col(width = 0.5, show.legend = FALSE) +
            theme_minimal() +
            labs(title = "Attribute-Level Mean Utilities", 
                 x = "Attribute Level", y = "Mean Utility") +
            scale_fill_gradient(low = "blue", high = "red") +
            theme(plot.title = element_text(face = "bold", size = 16),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        })
        
            # Description 
        output$bws_description <- renderUI({
          model_fit_html <- paste(
            "<div style='margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #143085;'>",
            "<h4 style='color: #143085; margin-top: 0;'>Model Fit Statistics</h4>",
            "<table style='width: 100%; border-collapse: collapse;'>",
            paste(
              "<tr style='border-bottom: 1px solid #ddd;'>",
              "<td style='padding: 8px; font-weight: bold;'>", model_fit$Statistic, "</td>",
              "<td style='padding: 8px; text-align: right;'>", model_fit$Value, "</td>",
              "</tr>",
              collapse = ""
            ),
            "</table>",
            "</div>",
            collapse = ""
          )
          
          HTML(paste0(
            "<p style='font-size:18px; color: #143085;'>",
            "📌 The table above shows the <strong>", input$profile_case_method, "</strong> estimates of attribute level utilities. ",
            "<strong>Mean Utility</strong> represents the average preference for each attribute level, ",
            "<strong>SE</strong> is the standard error, <strong>z-value</strong> is the test statistic, ",
            "and <strong>p-value</strong> indicates statistical significance with codes: *** p<0.001, ** p<0.01, * p<0.05.",
            "</p>",
            model_fit_html
          ))
        })
        
        # Download Word
        output$download_results <- downloadHandler(
          filename = function() {
            paste0("BWS2_", gsub(" ", "_", input$profile_case_method), "_Results.docx")
          },
          content = function(file) {
            results$`Attribute Level` <- gsub("^(`?attribute level`?|level)", "", results$`Attribute Level`)
            # Results table
            ft_results <- flextable(results) %>%
              autofit() %>%
              set_table_properties(layout = "autofit") %>%
              width(j = 1:ncol(results), width = 1.2)
            
            # Model fit table
            ft_fit <- flextable(model_fit) %>%
              autofit() %>%
              set_table_properties(layout = "autofit") %>%
              width(j = 1:ncol(model_fit), width = 2)
            
            # Save plot
            main_plot_file <- tempfile(fileext = ".png")
            ggsave(main_plot_file, 
                   ggplot(results, aes(x = reorder(`Attribute Level`, `Mean Utility`),
                                       y = `Mean Utility`, fill = `Mean Utility`)) +
                     geom_col() + theme_minimal() +
                     labs(title = "Mean Utilities")+
                     scale_fill_gradient(low = "blue", high = "red") ,
                   width = 7, height = 5, dpi = 300)
            
            # Create Word document
            doc <- read_docx() %>%
              body_add_par(paste("Best-Worst Scaling (Profile Case) -", input$profile_case_method),
                           style = "heading 1") %>%
              body_add_par(" ", style = "Normal") %>%
              body_add_par("Utility Estimates", style = "heading 2") %>%
              body_add_flextable(ft_results) %>%
              body_add_par(" ", style = "Normal") %>%
              body_add_par("Significance codes: *** p<0.001, ** p<0.01, * p<0.05.", 
                           style = "Normal") %>%
              body_add_par(" ", style = "Normal") %>%
              body_add_par("Model Fit Statistics", style = "heading 2") %>%
              body_add_flextable(ft_fit) %>%
              body_add_par(" ", style = "Normal") %>%
              body_add_par("Utility-plot", style = "heading 2") %>%
              body_add_img(main_plot_file, width = 6, height = 4)
            
            print(doc, target = file)
          },
          contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
      }
    }
  }) 
  
  ####========================= CASE 3 =========================#######
  observeEvent(input$run_analysis, {
    req(input$analysis_data, input$analysis_method)
    df <- read.csv(input$analysis_data$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    
    if (input$bws_case == "BWS Case 3") {
      
      # 3.1. Multinomial Logit Model =========================
      if (input$analysis_method == "Multinomial Logit Model") {
        
        # Defining required columns
        required_cols <- c("Respondent ID", "Choice set ID", "Profile ID", "Selection")
        
        # Checking if required columns are present
        if (!all(required_cols %in% colnames(df))) {
          missing_cols <- setdiff(required_cols, colnames(df))
          showModal(modalDialog(
            paste("Error: Missing columns: ", paste(missing_cols, collapse = ", "), 
                  ". Found: ", paste(colnames(df), collapse = ", ")),
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        # Identifying attribute columns dynamically
        attribute_cols <- setdiff(colnames(df), required_cols)
        
        # Checking if there are any attribute columns
        if (length(attribute_cols) == 0) {
          showModal(modalDialog("Error: No attribute columns found in the dataset.", easyClose = TRUE))
          return(NULL)
        }
        
        # Renaming Respondent ID, Choice set ID, and Profile ID to avoid space issues
        df <- df %>% 
          rename(respondent_id = `Respondent ID`, 
                 choice_set_id = `Choice set ID`, 
                 profile_id = `Profile ID`,
                 Selection = `Selection`)
        
        # Validating selection values
        df <- df %>%
          mutate(Selection = case_when(
            Selection == -1 ~ -1,
            Selection == 0  ~ 0,
            Selection == 1  ~ 1,
            TRUE ~ NA_real_
          ))
        
        if (any(is.na(df$Selection))) {
          showModal(modalDialog("Error: Invalid or missing values in Selection column.", easyClose = TRUE))
          return(NULL)
        }
        
        # Creating a choice indicator (1 for selected best, 0 otherwise)
        df <- df %>%
          mutate(choice = (Selection == 1))
        
        # Preparing data for mlogit
        # Creating a unique choice ID by combining respondent_id and choice_set_id
        df <- df %>%
          mutate(choice_id = paste(respondent_id, choice_set_id, sep = "_"))
        
        # --- Attribute preprocessing ---
        df[attribute_cols] <- lapply(df[attribute_cols], as.factor)
        
        # Drop constant attributes
        const_attrs <- names(Filter(function(x) length(unique(x)) == 1, df[attribute_cols]))
        if (length(const_attrs) > 0) {
          showModal(modalDialog(
            paste("Dropped constant attributes:", paste(const_attrs, collapse=", ")),
            easyClose = TRUE
          ))
          attribute_cols <- setdiff(attribute_cols, const_attrs)
          df <- df %>% select(-all_of(const_attrs))
        }
        
        if (length(attribute_cols) == 0) {
          showModal(modalDialog("Error: All attributes are constant, cannot run model.", easyClose = TRUE))
          return(NULL)
        }
        
        # Converting to mlogit.data format
        mlogit_data <- mlogit.data(
          data = df,
          choice = "choice",
          shape = "long",
          alt.var = "profile_id",
          chid.var = "choice_id",
          id.var = "respondent_id"
        )
        
        # Build formula safely (baseline coding via factors)
        attribute_formula <- paste0("`", attribute_cols, "`", collapse = " + ")
        model_formula <- as.formula(paste("choice ~", attribute_formula)) 
        
        # Running the multinomial logit model
        mlogit_model <- tryCatch(
          {
            mlogit(model_formula, data = mlogit_data)
          },
          error = function(e) {
            showModal(modalDialog(
              paste("Error in model estimation:", e$message), easyClose = TRUE))
            return(NULL)
          }
        )
        
        if (is.null(mlogit_model)) {
          return(NULL)
        }
        
        # Extracting model summary
        model_summary <- summary(mlogit_model)
        
        # Get coefficients matrix directly
        coef_matrix <- model_summary$CoefTable
        
        # If it's just a vector, wrap into matrix
        if (is.null(dim(coef_matrix))) {
          coef_matrix <- cbind(Estimate = coef_matrix)
        }
        
        coef_df <- as.data.frame(coef_matrix)
        coef_df <- tibble::rownames_to_column(coef_df, var = "Parameter")
        
        # Expected column names
        expected_names <- c("Estimate", "Std.Error", "z.value", "Pr(>|z|)")
        colnames(coef_df) <- c("Parameter", expected_names[seq_len(ncol(coef_df) - 1)])
        
        # Pad missing cols with NA
        for (nm in setdiff(expected_names, colnames(coef_df))) {
          coef_df[[nm]] <- NA
        }
        
        # Reorder consistently
        coef_df <- coef_df[, c("Parameter", expected_names)]
        
        # Final coefficients table
        coefficients_raw <- coef_df %>%
          mutate(
            Coefficient = Estimate,
            Std.Error = Std.Error,
            z.value = z.value,
            `Pr(>|z|)` = paste0(
              round(`Pr(>|z|)`, input$round_digits),
              ifelse(`Pr(>|z|)` < 0.001, "***",
                     ifelse(`Pr(>|z|)` < 0.01, "**",
                            ifelse(`Pr(>|z|)` < 0.05, "*", ""))))
          ) %>%
          dplyr::select(Parameter, Coefficient, Std.Error, z.value, `Pr(>|z|)`) 
        
        # ✅ Clean up naming for better readability
        coefficients_raw$Parameter <- gsub("`(.+?)`(\\w)(\\d+)", "\\1/\\2\\3", coefficients_raw$Parameter)
        coefficients_raw$Parameter <- gsub("\\(Intercept\\):(\\d+)", "Intercept:\\1", coefficients_raw$Parameter)
        
        # Calculating profile utilities
        profiles_raw <- df %>%
          dplyr::select(all_of(attribute_cols)) %>%
          distinct() %>%
          mutate(Profile = do.call(paste, c(.[attribute_cols], sep="_")))
        
        # Design matrix based on the same contrasts as the model
        design_matrix <- model.matrix(~ ., data = profiles_raw[, attribute_cols, drop = FALSE])
        
        # Keep only columns that exist in coefficients
        common_terms <- intersect(colnames(design_matrix), names(mlogit_model$coefficients))
        design_matrix <- design_matrix[, common_terms, drop = FALSE]
        
        # Align coefficients vector
        beta <- mlogit_model$coefficients[common_terms]
        
        # Compute utilities (raw)
        profiles_raw <- profiles_raw %>%
          mutate(Utility = as.vector(design_matrix %*% beta)) %>%
          dplyr::select(Profile, Utility, all_of(attribute_cols)) %>%
          arrange(desc(Utility))
        
        # Function to calculate BIC for mlogit models
        calc_bic <- function(model, data) {
          loglik <- as.numeric(logLik(model))
          k <- attr(logLik(model), "df")   # number of estimated parameters
          n <- nrow(data)                  # number of observations
          bic <- -2 * loglik + k * log(n)
          return(bic)
        }
        
        # Model fit statistics
        model_info_raw <- data.frame(
          LogLikelihood = model_summary$logLik,
          AIC = AIC(mlogit_model),
          BIC = calc_bic(mlogit_model, mlogit_data),
          Observations = nrow(mlogit_data)
        )
        
        # Create rounded versions for UI display only
        coefficients_ui <- round_df(coefficients_raw, input$round_digits)
        profiles_ui <- round_df(profiles_raw, input$round_digits)
        model_info_ui <- round_df(model_info_raw, input$round_digits)
        
        # Rendering results
        output$bws_results_multiple <- renderUI({
          tagList(
            h4("Multinomial Logit Model Coefficients"),
            reactable(coefficients_ui, bordered = TRUE, striped = TRUE, highlight = TRUE,
                      columns = list(`Pr(>|z|)` = colDef(name = "p-value", html = TRUE))),
            h4("Profile Utilities"),
            reactable(profiles_ui, bordered = TRUE, striped = TRUE, highlight = TRUE),
            h4("Model Fit Statistics"),
            reactable(model_info_ui, bordered = TRUE),
            p("📌 The coefficients table shows the estimated effects of each attribute level on choice. 
                 Terms such as 'Attribute A/A2' or 'Attribute B/B3' indicate the effect of that level relative to the base 
                 level (e.g., A1 or B1). Positive coefficients indicate that an attribute level increases preference compared to its base, 
                 while negative coefficients reduce preference. The terms labelled 'Intercept:k' (where k = 2, 3, …) are known as 
                 alternative-specific constants (ASCs). They capture the inherent preference for attributes 2, 3, ... 
                 relative to attribute 1, after accounting for attributes."),
            
            p("📌 The profile utilities table combines the estimated coefficients to calculate an overall utility score 
                 for each profile. These scores make it easier to compare profiles and see which ones are more or less 
                 preferred by respondents."),
            
            p("📌 The model fit statistics (log-likelihood, AIC, BIC) show how well the model explains the observed 
                 choices. Lower values of AIC and BIC usually indicate a better-fitting model."),
            
            p("Significance codes: *** p<0.001, ** p<0.01, * p<0.05.")
          )
        })
        
        # Main plot: Bar plot of profile utilities
        output$main_plot <- renderPlot({
          ggplot(profiles_ui, aes(x = reorder(Profile, Utility), y = Utility, fill = Utility)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            theme_minimal() +
            labs(title = "Profile Utilities from Multinomial Logit Model", x = "Profile", y = "Utility") +
            scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
            theme(axis.text.y = element_text(size = 8))
        })
        output$secondary_plot <- renderUI({NULL})
        
        # Download results
        output$download_results <- downloadHandler(
          filename = function() { "BWS3_MLogit_Results.docx" },
          content = function(file) {
            main_plot <- ggplot(profiles_raw, aes(x = reorder(Profile, Utility), y = Utility, fill = Utility)) +
              geom_bar(stat = "identity") +
              coord_flip() +
              theme_minimal() +
              labs(title = "Profile Utilities from Multinomial Logit Model", x = "Profile", y = "Utility") +
              scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
              theme(axis.text.y = element_text(size = 8))
            plot_file <- tempfile(fileext = ".png")
            ggsave(plot_file, main_plot, width = 7, height = 5, dpi = 300)
            
            doc <- read_docx() %>%
              body_add_fpar(fpar(ftext("Best-Worst Scaling (Case 3) Analysis Results – Multinomial Logit Model", fp_text(font.size = 16, bold = TRUE)))) %>%
              body_add_fpar(fpar(ftext("Model Coefficients", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(coefficients_raw)) %>%
              body_add_fpar(fpar(ftext("Profile Utilities", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(profiles_raw)) %>%
              body_add_fpar(fpar(ftext("Model Fit Statistics", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(model_info_raw)) %>%
              body_add_par("📌 The coefficients table shows the estimated effects of each attribute level on choice. 
                 Terms such as 'Attribute A/A2' or 'Attribute B/B3' indicate the effect of that level relative to the base
                 level (e.g., A1 or B1). Positive coefficients indicate that an attribute level increases preference compared to its base,
                 while negative coefficients reduce preference. The terms labelled 'Intercept:k' (where k = 2, 3, …) are known as
                 alternative-specific constants (ASCs). They capture the inherent preference for attributes 2, 3, ...
                 relative to attribute 1, after accounting for attributes.", style = "Normal") %>%
              body_add_par("📌 The profile utilities table combines the estimated coefficients to calculate an overall utility score
                 for each profile. These scores make it easier to compare profiles and see which ones are more or less
                 preferred by respondents.", style = "Normal") %>%
              body_add_par("📌 The model fit statistics (log-likelihood, AIC, BIC) show how well the model explains the observed
                 choices. Lower values of AIC and BIC usually indicate a better-fitting model.", style = "Normal") %>%
              body_add_par("Significance codes: *** p<0.001, ** p<0.01, * p<0.05.") %>%
              body_add_par(" ") %>%
              body_add_img(plot_file, width = 6, height = 4) 
            print(doc, target = file)
          },
          contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
      }
      
      # 3.2. Latent Class Analysis  =========================
      else if (input$analysis_method == "Latent Class Analysis") {
        # Define required column names
        required_cols <- c("Respondent ID", "Choice set ID", "Profile ID", "Attribute A", "Attribute B", "Attribute C", "Selection")
        
        # Check input dataset
        if (!all(required_cols %in% colnames(df))) {
          missing_cols <- setdiff(required_cols, colnames(df))
          showModal(modalDialog(
            paste("Error: Missing columns: ", paste(missing_cols, collapse = ", "), 
                  ". Found: ", paste(colnames(df), collapse = ", ")),
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        # Rename Respondent ID to avoid space issues
        df <- df %>% rename(respondent_id = `Respondent ID`)
        
        # Validate selection values
        df <- df %>%
          mutate(Selection = case_when(
            Selection == -1 ~ -1,
            Selection == 0  ~  0,
            Selection == 1  ~  1,
            TRUE ~ NA_real_
          ))
        
        if (any(is.na(df$Selection))) {
          showModal(modalDialog("Error: Invalid or missing values in Selection column.", easyClose = TRUE))
          return(NULL)
        }
        
        # Create profile identifier
        df <- df %>%
          mutate(profile = paste(`Attribute A`, `Attribute B`, `Attribute C`, sep = "_"))
        
        # Aggregate scores
        df_scores <- df %>%
          group_by(respondent_id, profile) %>%
          summarise(score = sum(Selection), .groups = "drop") %>%
          pivot_wider(id_cols = respondent_id, names_from = profile, values_from = score, values_fill = 0) %>%
          arrange(respondent_id)
        
        # Diagnostic: Check df_scores
        cat("Columns in df_scores after pivot_wider:", paste(colnames(df_scores), collapse = ", "), "\n")
        cat("Head of df_scores:\n")
        
        df_scores <- as.data.frame(df_scores)
        manifest_vars <- names(df_scores)[!names(df_scores) %in% c("respondent_id")]
        
        # Convert scores to factors for poLCA
        df_scores[manifest_vars] <- lapply(df_scores[manifest_vars], function(x) as.factor(as.character(x)))
        
        # Run LCA
        formula <- as.formula(paste("cbind(", paste(manifest_vars, collapse = ", "), ") ~ 1"))
        set.seed(123)
        best_model <- NULL
        best_bic <- Inf
        best_aic <- NULL
        best_llik <- NULL
        best_k <- NULL
        model_metrics <- data.frame(K = integer(), AIC = numeric(), BIC = numeric(), LogLikelihood = numeric())
        
        for (k in 2:5) {
          model <- tryCatch(
            {
              poLCA(formula, data = df_scores, nclass = k, nrep = 3, maxiter = 2000, verbose = FALSE)
            },
            error = function(e) {
              return(NULL)
            }
          )
          if (!is.null(model)) {
            model_metrics <- rbind(model_metrics, data.frame(K = k, AIC = model$aic, BIC = model$bic, LogLikelihood = model$llik))
            cat(paste("Model with", k, "classes: AIC =", model$aic, ", BIC =", model$bic, ", LogLikelihood =", model$llik, "\n"))
            if (model$bic < best_bic) {
              best_model <- model
              best_bic <- model$bic
              best_aic <- model$aic
              best_llik <- model$llik
              best_k <- k
            }
          }
        }
        
        if (is.null(best_model)) {
          showModal(modalDialog("Error: LCA model could not be estimated.", easyClose = TRUE))
          return(NULL)
        }
        
        # Assign class memberships
        df_scores$Class <- best_model$predclass
        
        # Check for required columns
        if (!all(c("respondent_id", "Class") %in% colnames(df_scores))) {
          showModal(modalDialog(
            paste("Error: 'respondent_id' or 'Class' missing in df_scores. Found: ", 
                  paste(colnames(df_scores), collapse = ", ")),
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        # Class proportions
        class_summary_raw <- df_scores %>%
          group_by(Class) %>%
          summarise(n = n(), Proportion = n() / nrow(df_scores)) %>%
          ungroup()
        
        # Model info
        model_info_raw <- data.frame(AIC = best_aic, BIC = best_bic, LogLikelihood = best_llik, Classes = best_k)
        
        # Class profiles
        class_profile_raw <- df %>%
          left_join(dplyr::select(df_scores, respondent_id, Class), by = "respondent_id") %>%
          group_by(Class, profile) %>%
          summarise(mean_score = mean(Selection), .groups = "drop") %>%
          pivot_wider(names_from = profile, values_from = mean_score) %>%
          arrange(Class)
        
        # Create rounded versions for UI display only
        model_info_ui <- round_df(model_info_raw, input$round_digits)
        class_summary_ui <- round_df(class_summary_raw, input$round_digits)
        class_profile_ui <- round_df(class_profile_raw, input$round_digits)
        
        # Render results
        output$bws_results_multiple <- renderUI({
          tagList(
            h4("Model Info (AIC, BIC, LogLikelihood, Best K)"),
            reactable(model_info_ui, bordered = TRUE),
            h4("Latent Class Proportions"),
            reactable(class_summary_ui, bordered = TRUE, striped = TRUE, highlight = TRUE),
            h4("Class Profiles (Mean BWS Scores by Profile)"),
            reactable(class_profile_ui, bordered = TRUE, striped = TRUE, highlight = TRUE),
            p("📌 The class profiles table shows the average Best-Worst Scaling scores for each profile across latent classes. 
            Higher positive values indicate 'best' preferences, negative values indicate 'worst'.")
          )
        })
        
        # Main plot
        output$main_plot <- renderPlot({
          ggplot(class_summary_ui, aes(x = factor(Class), y = Proportion, fill = factor(Class))) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(title = paste("Latent Class Proportions (K=", best_k, ")"), x = "Class", y = "Proportion") +
            scale_fill_brewer(palette = "Set2")
        })
        
        # Clear secondary plot
        output$secondary_plot <- renderUI({NULL})
        
        # Download results
        output$download_results <- downloadHandler(
          filename = function() { "BWS3_LCA_Results.docx" },
          content = function(file) {
            main_plot <- ggplot(class_summary, aes(x = factor(Class), y = Proportion, fill = factor(Class))) +
              geom_bar(stat = "identity") +
              theme_minimal() +
              labs(title = paste("Latent Class Proportions (K=", best_k, ")"), x = "Class", y = "Proportion") +
              scale_fill_brewer(palette = "Set2")
            plot_file <- tempfile(fileext = ".png")
            ggsave(plot_file, main_plot, width = 7, height = 5, dpi = 300)
            
            doc <- read_docx() %>%
              body_add_fpar(fpar(ftext("Best-Worst Scaling (Case 3) Analysis Results – Latent Class Analysis", fp_text(font.size = 16, bold = TRUE)))) %>%
              body_add_fpar(fpar(ftext("Model Info", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(model_info_raw)) %>%
              body_add_fpar(fpar(ftext("Latent Class Proportions", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(class_summary_raw)) %>%
              body_add_fpar(fpar(ftext("Class Profiles", fp_text(bold = TRUE)))) %>%
              body_add_flextable(flextable(class_profile_raw)) %>%
              body_add_par("📌 The class profiles table shows the average Best-Worst Scaling (BWS) scores for each item, across different latent classes. Higher positive values indicate items that are more likely to be selected as 'best', while lower (or negative) values suggest items more likely to be selected as 'worst' by respondents in that class. Use this information to understand the preferences of each latent class.", style = "Normal") %>%
              body_add_par(" ") %>%
              body_add_img(plot_file, width = 6, height = 4)
            print(doc, target = file)
          }
        )
      }
      
    }})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
