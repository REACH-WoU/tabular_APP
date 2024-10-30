library(shiny)
library(dplyr)
library(readxl)
library(scales)
library(stringr)
library(DT)
library(tidyverse)
library(sf)
library(cluster) 
library(survey) 
library(srvyr)
library(shinythemes)
library(shinycssloaders)
library(DescTools)
library(shinybusy)
library(echarts4r)
library(rstatix)
library(fastDummies)
library(bslib)
library(shinyBS)
library(leaflet)
# library(rgdal)
library(leaflet.extras)
library(exactextractr)
library(zip)
library(sharepointR)
library(mapview)
library(shinyjs)
webshot::install_phantomjs(force = T)
rm(list=ls())
source("www/src/utils/misc_utils.R")
source("www/src/utils/kobo_utils.R")
source("www/src/utils/tabular_analysis_utils.R")
options(shiny.maxRequestSize=100*1024^2,
        rsconnect.max.bundle.files = 5145728000)


js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"

css <- readLines(con = "www/style.css") %>% 
  paste(collapse = "\n")

ui <- fluidPage(
    # Setting up styling
    tags$head(
      HTML('<meta name="viewport content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>'), includeCSS("www/style.css"),
      HTML('<script src="echarts.js"></script>'),
      HTML('<script src="REACH_Theme.js"></script>'),
      tags$script(HTML(js)),
      tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = document.getElementById("map").clientWidth;
                        dimension[1] = document.getElementById("map").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = document.getElementById("map").clientWidth;
                        dimension[1] = document.getElementById("map").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '),
    ),
    uiOutput("css_style"),
    add_busy_bar(
      height = "5px",
      color = "#EE5759"
    ),
    navbarPage(id = "tabs",
      windowTitle = "ANALYSIS APP", 
      
      HTML('<a style="padding-left:10px;" class="navbar-brand" href= "https://www.reach-initiative.org/" target="_blank"><img src="reach.jpg" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>ANALYSIS APP</strong></span>'),
      
      tabPanel("Read Me",
               div(class ="title-message",
                   h2("Brief overview of the tool")),
               br(),
               column(class = "column1",
                      style = "margin-left: 220px;",
                      width = 4,
                      br(),
                      h3(class = "title-message","Introduction"),
                      p("This analysis tool will provide various tools and functionalities to analyse and interpret data across your different assessment and tools. It was built in a way to provide a user-friendly interface and a range of tools tailored to specific IMPACT needs."),
                      hr(),
                      h3(class = "title-message","Characteristics"),
                      tags$ul(
                        tags$li(em("Data input:")," The tool allows you to download your finalized/cleaned data and KoBo Tools (XLS format)."),
                        tags$li(em("Analysis Options:")," The tool provides various selection of analysis methods and techniques depending on the desired outcomes. These can include ",strong("single tabular analysis, visualizations, variances, and key highlight findings.")),
                        tags$li(em("Customization:")," Users will be able to customize their output depending on different analysis parameters and settings according to their specific requirements. Some of these parameters are",strong(" adding weights, strata, and others."))
                      ),
                      hr(),
                      h3(class = "title-message","Parameters"),
                      h4(style = "font-size:20px;",strong("Singular Table Output")),
                      tags$ul(
                        tags$li(em(strong("Variable: ")),"Variable to analyse."),
                        tags$li(em(strong("Disaggregated variable: ")),"Disaggregated variable against the first chosen variable."),
                        tags$li(em(strong("Weighting: ")),"[Yes] or [No] to include weighting to data."),
                        tags$li(em(strong("Include NA: ")),"[Selected] will add all the NA values that are originally omitted."),
                        tags$li(em(strong("Overall: ")),"[Selected] will consider the calculations to the overall population."),
                        tags$li(em(strong("Strata: ")),"[Selected] will consider the calculations by different popluation groups (Strata)."),
                        tags$li(em(strong("Minimum sample number: ")),"Minimum number of respondents per group analyzed, where the results can be considered significant.")
                      ),
                      h4(style = "font-size:20px;",strong("Singular Graph Output")),
                      tags$ul(
                        tags$li(em(strong("Variable: ")),"Variable to analyse."),
                        tags$li(em(strong("Disaggregated variable: ")),"Disaggregated variable against the first chosen variable."),
                        tags$li(em(strong("Weighting: ")),"[Yes] or [No] to include weighting to data."),
                        tags$li(em(strong("Overall: ")),"[Selected] will consider the calculations to the overall population."),
                        tags$li(em(strong("Strata: ")),"[Selected] will consider the calculations by different popluation groups (Strata).")
                      ),
                      h3(class = "title-message","Methodology"),
                      h4(style = "font-size:20px;",strong("Input Data")),
                      p("To be able to use all the functionalities of this tool, you should upload the cleaned data, the Kobo tool, and select the country related to the assessment (This is a required selection for the Map Output tab to start processing). "),
                      h4(style = "font-size:20px;",strong("Singular Table/Graph Output")),
                      p("First, you will be able to select the respective sheet from your dataset (main sheet or other loops). Second, the other dropdown lists are the targeted variable for the analysis from the selected sheet as well as the disaggregated variabled to be added towards the targeted variable."),
               ),
               column(class = "column2",
                      style = "margin-left: 130px;",
                      width = 4,
                      br(),
                     p("The ",em("variable and disaggregation"),"are mainly the select_one, select_multiple, and different numeric columns from your dataset. In case some of these columns are", strong("empty"),"in your data, it will not be shown in the dropdown list."),
                      p("The output is an interactive table/graph that will be updated automatically depending on your different selections in the Parameters section on the left or even the variable or disagregation."),
                      p("For the tabular tab, if the selected variable is a select_one or select_multiple, the table will output percentages of the different categories of the selected variable. If the selected variable is numeric, then the table will mainly show the mean, median, min, and max values in the data."),
                      h4(style = "font-size:20px;",strong("Map Output")),
                      p("First you will have to select the respective admin level that you want to show your level of disaggregation on the map. Keep in mind that you should have a column in your data that include similar pattern PCODE to the selected admin level."),
                      p("The application will automatically scan through all the columns of your data and identify the column/s that have similar pattern and will prompt you with the retrieved column/s name/s. Select [Yes] if it is the correct column to proceed. You might be prompt with a message to recheck if you have the similar pattern in your data."),
                      p("Then you will be able to select between showing the geographical distribution of your submission by selecting [Distribution], or you can select through the different variables of your data and respectively select between the categories of the selected variable to show their distribution over the selected admin."),
                      h3(class = "title-message","Requirements"),
                      h4(style = "font-size:20px;",strong("Singular Table/Graph Output")),
                      p("The Data downloaded should be of an Excel format. If the dataset is only including one tab (no loops included in the data), the sheet should be named ", strong('main.'),"If the Data include many tabs (Data with loops), please ensure that the first tab is named ", strong('main,'),"and then the others kept as downloaded from the Kobo Server (as named by the name value of the respective begin_repeat row)."),
                      p("The tool should include the survey and the choices tab."),
                      p("For the weighting to be captured and calculated, the data should include a column named ",strong("weight."),"Please make sure that in case data is consiting of loops as well, weight column should also be included in every tab."),
                      p("For the strata to be captured and calculated, the data should include a column named ", strong("strata."),"Please make sure that in case data is consiting of loops as well, strata column should also be included in every tab."),
                      hr(),
                      h3(class = "title-message","Credits"),
                      p("This application was built by Abraham Azar. For any information or question, please contact abraham.azar@impact-initiatives.org"),
                      p("The variance tool was built by Nestor Cheryba. For any information or question, please contact nestor.cheryba@reach-initiative.org"),
                      p("Huge thanks to Mariia Tomashchuk for the GIS support")
                      )
               ),
      # tabPanel("Login",
      #          # logout button
      #          div(class = "pull-right", style = "width: 50%",shinyauthr::logoutUI(id = "logout")),
      #          
      #          # login section
      #          shinyauthr::loginUI(id = "login")
      # ),
    tabPanel("Input Data", 
             div(class ="container-box",
                 style = "padding: 40px;display:flex;justify-content:center",
             div(class ="input-box",
                 style ="padding:20px",
                 fileInput("dataInput", "Upload your Cleaned data here (Excel)",
                       multiple = F,
                       accept = c(".xlsx"))
                 ),
             div(class ="input-box",
               style ="padding:20px",
               fileInput("toolInput", "Upload your Kobo tool here (Excel)",
                         multiple = F,
                         accept = c(".xlsx"))
               ),
             div(class = "input-box",
                 style = "padding:20px; margin: 0 0 28px 0",
                 uiOutput("language_selection")
                 )
             )),
    tabPanel("Singular Table Output / Variance",
             sidebarPanel(
               uiOutput("sheetInput"),
               uiOutput("var"),
               uiOutput("dis_var"),
               HTML("<h3><strong>Parameters</strong></h3>"),
               radioButtons("weightBTN","Weighting (only if weight is available in Data)",
                            choices = c(No = "no",
                                        Yes = "yes")),
               radioButtons("calculation", "Calculations",
                             choices = c(None = NA,
                                         `Include NA` = "include_na")),
               radioButtons("admin", "Level (only if strata is available in Data)",
                             choices = c(Overall ="overall",
                                         Strata = "strata")),
               uiOutput("variance_sample")
             ),
             mainPanel(
               uiOutput("title_table"),
               uiOutput("subtitle_table"),
               uiOutput("allNA"),
               uiOutput("table"),
               br(),
               div(id="parentDiv",
                   div(class = "childDiv",uiOutput("variance_title")),
                   div(class = "childDiv", uiOutput("variance_icon"))
               ),
               uiOutput("text_variance")
             )),
    tabPanel("Singular Graph Output",
      sidebarPanel(
        uiOutput("sheetInputGraph"),
        uiOutput("varGraph"),
        uiOutput("dis_var_Graph"),
        HTML("<h3><strong>Parameters</strong></h3>"),
        radioButtons("weightBTNGraph","Weighting (only if weight is available in Data)",
                     choices = c(Yes = "yes",
                                 No = "no"),
                     selected = "no"),
        radioButtons("adminGraph", "Level (only if strata is available in Data)",
                     choices = c(Overall ="overall",
                                 Strata = "strata")),
        uiOutput("graph")
      ),
      mainPanel(
        uiOutput("test"),
        uiOutput("textPlot"),
        echarts4rOutput("plotGraph", width = '100%', height = '620px')
      )
    )
    
    )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = user_base,
  #   user_col = user,
  #   pwd_col = password,
  #   sodium_hashed = TRUE,
  #   log_out = reactive(logout_init())
  # )
  # 
  # # Logout to hide
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )
  # 
  # observeEvent(!credentials()$user_auth, {
  # 
  #   if(!credentials()$user_auth){
  #     hideTab(inputId = "tabs", target = "Singular Table Output / Variance")
  #     hideTab(inputId = "tabs", target = "Singular Graph Output")
  #     # hideTab(inputId = "tabs", target = "Map Output")
  #     hideTab(inputId = "tabs", target = "Input Data")
  #   } else {
  #     showTab(inputId = "tabs", target = "Singular Table Output / Variance")
  #     showTab(inputId = "tabs", target = "Singular Graph Output")
  #     # showTab(inputId = "tabs", target = "Map Output")
  #     showTab(inputId = "tabs", target = "Input Data")
  #   }
  # })
    
  output$language_selection <- renderUI({
    req(input$toolInput$datapath)
    labels <- read_xlsx(input$toolInput$datapath, sheet = "survey", col_types = "text") %>% 
      select(starts_with("label::")) %>% 
      names()
    labels <- lapply(labels, function(x) gsub("label::","",x))
    selectInput("language","Select Language",
                choices = labels)
  })
 
  ########## Getting the Label_colname ##########
  label_colname <- reactive({
    req(input$language)
    load.label_colname(input$toolInput$datapath, language = input$language)
    })
  
  ########## Getting the Survey ##########
  tool.survey <- reactive({
    req(input$toolInput$datapath,
        input$dataInput$datapath)
    # sheet_names <- excel_sheets(input$dataInput$datapath)
    tool.survey <- read_xlsx(input$toolInput$datapath, sheet = "survey", col_types = "text") %>%
      filter(!is.na(type)) %>%
      mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
             list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
             list_name=ifelse(str_starts(type, "select_"), list_name, NA))
  
  
    # select only the relevant (English) labels, hints etc.
    lang_code <- str_split(label_colname(), "::", 2, T)[2]
    lang_code <- str_replace(str_replace(lang_code, "\\(", "\\\\("), "\\)", "\\\\)")
    cols <- colnames(tool.survey)
    cols_to_keep <- cols[str_detect(cols, paste0("((label)|(hint)|(constraint_message))::",lang_code)) |
                           !str_detect(cols, "((label)|(hint)|(constraint_message))::")]
  
    tool.survey <- dplyr::select(tool.survey, all_of(cols_to_keep))
  
    # Find which data sheet question belongs to:
    tool.survey <- tool.survey %>% mutate(datasheet = NA)
    sheet_name <- "main"
    for(i in 1:nrow(tool.survey)){
      toolrow <- tool.survey %>% slice(i)
      if(str_detect(toolrow$type, "begin[ _]repeat")) sheet_name <- toolrow$name
      else if(str_detect(toolrow$type, "end[ _]repeat")) sheet_name <- "main"  # watch out for nested repeats (Why would you even want to do that?)
      else if(str_detect(toolrow$type, "((end)|(begin))[ _]group", T)) tool.survey[i, "datasheet"] <- sheet_name
    }
    return(tool.survey)
  })
  
  ########## Getting the Choices ##########
  tool.choices <- reactive({
    req(input$toolInput$datapath)
    read_xlsx(input$toolInput$datapath, sheet = "choices", col_types = "text") %>%
      filter(!is.na(list_name)) %>%
      dplyr::select(all_of(c("list_name", "name")), !!sym(label_colname())) %>% distinct()
  })
  
  ############################################################################################################
  #                                                     TABLE                                                #
  ############################################################################################################
  
  ########## Getting the Sheet input ##########
  output$sheetInput <- renderUI({
    req(input$toolInput$datapath,
        input$dataInput$datapath)
    tool_var <- excel_sheets(input$dataInput$datapath)
    # tool_var <-  tool.survey() %>% filter(!is.na(datasheet)) %>% pull(datasheet) %>% unique()
    selectInput("sheet","Sheet",
                choices = c(tool_var),
                selected = NULL,
                multiple = F)
  })
  
  ########## Getting the Variable Input ##########
  observeEvent(input$sheet, {
    output$var <- renderUI({
      req(input$toolInput$datapath,
          input$dataInput$datapath)      
      df <- read_excel(input$dataInput$datapath,sheet = input$sheet, col_types = "text")
      empty_columns <- names(df[, colSums(is.na(df) | df == "") == nrow(df)])
      ## TO CHECK FOR DATA ONLY
      tool_var <- tool.survey() %>%
        filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheet & name %in% colnames(df) & !name%in% empty_columns) %>% pull(name) %>% unique
      if(length(tool_var)>0){
        selectizeInput("variable","Variable",
                    choices = c(tool_var),
                    selected = tool_var[1],
                    multiple = T,
                    options = list(maxOptions = 1000L,maxItems = 1))
      } else {
        HTML("<h4  style = 'color: rgba(238, 88, 89, .8)'>Please check that the sheet names of the dataset are matching with the tool</h4>")
      }
      
    })
  })
  
  ########## Getting the Disaggregated Variable Input ##########
  observeEvent(input$sheet, {
    output$dis_var <- renderUI({
      req(input$toolInput$datapath,
          input$dataInput$datapath,
          input$variable)
      tool.survey <- tool.survey()
      df <- read_excel(input$dataInput$datapath,sheet = input$sheet, col_types = "text")
      empty_columns <- names(df[, colSums(is.na(df) | df == "") == nrow(df)])
      ## TO CHECK FOR DATA ONLY
      tool_var <- tool.survey %>%
        filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheet & name %in% colnames(df) & name != input$variable & !name %in% empty_columns) %>% pull(name) %>% unique
      if(length(tool_var)>0){
      selectizeInput("dis_variables","Disaggregation",
                  choices = c("No disaggregation", tool_var),
                  selected = "No disaggregation",
                  multiple = T, 
                  options = list(maxOptions = 1000L,maxItems = 1))
    } else {
      HTML("<h4  style = 'color: rgba(238, 88, 89, .8)'>Please check that the sheet names of the dataset are matching with the tool</h4>")
    }
    })
  })
  
  ########## Getting the entry ##########
  entry <- reactive({
    req(input$variable,
        input$toolInput$datapath,
        input$dis_variables)
    entry <- list()
    entry$variable <- input$variable
    if (any(str_detect(input$variable, "/"))) input$variable <- str_split(input$variable, "/", 2, T)[,1]
    ## TO CHECK FOR DATA ONLY
    res <- data.frame(name = input$variable) %>% 
      left_join(dplyr::select(tool.survey(), name, !!sym(label_colname())), by = "name", na_matches = "never") %>% 
      pull(label_colname())
    if(is.na(res)){
      entry$label <- "No label"
    } else{
      entry$label <- res
    }
    if (input$dis_variables == "No disaggregation"){
      entry$disaggregate.variables <- NA
      entry$disaggregations <- NA
    } else{
      entry$disaggregate.variables <- input$dis_variables
      entry$disaggregations <- input$dis_variables
    }

    if (is.na(input$calculation)){
      entry$calculation <- NA
    } else{
      entry$calculation <- input$calculation
    }
    ## TO CHECK FOR DATA ONLY
    entry$func <- tool.survey() %>%
      filter(name == input$variable) %>% pull(q.type)

    if(entry$func %in% c("calculate","integer","decimal")) entry$func <- "numeric"

    entry$var_type <- input$func
    entry$admin <- input$admin
    entry$list_name <- tool.survey() %>%
      filter(name == input$variable) %>% pull(list_name)
    entry$comments <- ''
    entry$datasheet <- tool.survey() %>%
      filter(name == input$variable) %>% pull(datasheet)
    if (input$calculation == "include_na"){
      entry$omit_na <- F
    } else{
      entry$omit_na <- T
    }
    return(entry)
    })


  ########## Getting the Data List ##########
  data.list <- reactive({
    req(input$dataInput$datapath,
        input$weightBTN)
    tool.survey <- tool.survey()
    sheet_names <- excel_sheets(input$dataInput$datapath)
    sheet_names[1] <- "main"
    data.list <- list("main" = read_excel(input$dataInput$datapath, sheet=1, col_types = "text"))
    for(sheet in sheet_names[-1])
      data.list[[sheet]] <- read_excel(input$dataInput$datapath, sheet=sheet, col_types = "text")
    tool_datasheets <- tool.survey %>% distinct(datasheet) %>% filter(!is.na(.)) %>% pull
    for (sheet in sheet_names[-1]) {
      # take the first column from this sheet and find it in tool.survey
      i <- 1
      data_cnames <- data.list[[sheet]] %>% dplyr::select(-contains("/"), -starts_with("_")) %>% names
      first_col <- data_cnames[i]
      while (!first_col %in% tool.survey()$name) {
        i <- i + 1
        first_col <- data_cnames[i]
      }
      old_sheetname <- tool.survey %>% filter(name == first_col) %>% pull(datasheet)
      # change all occurences of `old_sheetname` to `sheet`
      tool.survey <- tool.survey %>% mutate(datasheet = ifelse(datasheet %==na% old_sheetname, sheet, datasheet))
    }
    for (sheet in names(data.list)){
      if(input$weightBTN == "no") {
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall",
                 weight = 1)
      } else if (input$weightBTN == "yes" & "weight" %in% colnames(data.list[[sheet]])){
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall",
                 weight = as.numeric(weight)) 
          
      } else{
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall")
        # ,
        #          weight = 1) 
      }
    }
    tryCatch(
      return(data.list), error = function(err) return(data.frame())
      )
  })

  ########## Creating the Title Table Output ##########
  output$title_table <- renderUI({
    req(input$variable,
        input$weightBTN)
    if(entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])) {
      HTML("")
    } else {
      if(input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
        HTML("")
      }else{
        HTML("<h3>",entry()[["label"]],"</h3>")
      }
    }
  })
  
  ########## Creating the Sub-Title Table Output ##########
  output$subtitle_table <- renderUI({
    req(input$variable)
    if(entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])) {
      HTML("")
    } else {
      if(input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
        HTML("")
      } else {
        HTML("<h5>Variable name: <em><strong>", entry()[["variable"]], "</strong></em></h5>")
      }
    }
  })
  
  ########## Creating the Srvyr Design List ##########
  srvyr.designs <- reactive({
    req(input$dataInput$datapath,
        input$toolInput$datapath,
        input$sheet,
        input$variable,
        input$dis_variables,
        input$weightBTN)
    data.list <- data.list()
    srvyr.designs <- list()
    entry <- entry()
    tool.choices <- tool.choices()
    tool.survey <- tool.survey()
    sheet <- input$sheet
    col <- input$variable
    disag.col <- input$dis_variables
    if(!all(is.na(entry$disaggregate.variables))){
      for(disagg.var in entry$disaggregate.variables){
        if(!disagg.var %in% colnames(data.list[[sheet]])){
          # disagg.var was not found, but maybe it's located in main? let's try to fix!
          if(sheet == "main") stop("Disaggregation variable ", disagg.var, " was not found in main!!\n")
          if(disagg.var %in% colnames(data.list$main)){
            # cat("... Disaggregation variable", disagg.var,"was not found in sheet",sheet,"but it exists in main. Will attempt to simply left_join by uuid... ")
            join_attempt <- data.list()$main %>% dplyr::select(uuid, !!sym(disagg.var))
            data.list[[sheet]] <- data.list[[sheet]] %>% left_join(join_attempt, by = "uuid")
            # cat(" success!")
          }else stop(paste("Disaggregation variable", disagg.var, "not found in sheet",sheet,"nor in main!!\n"))
        }
        data.list[[sheet]][[disagg.var]] <- as.character(data.list[[sheet]][[disagg.var]])  # as character, not factor!
      }
    }
    if(entry$func == "select_multiple"){
      # not converting to label here. instead just replace "/" with "___" and convert to numeric
      choice_cols <- colnames(data.list[[sheet]])[colnames(data.list[[sheet]]) %>% str_starts(paste0(col, "/"))]
      data.list[[sheet]] <- data.list[[sheet]] %>%
        mutate(across(all_of(choice_cols), as.numeric)) %>%
        rename_with(~str_replace(., "/", "___"), choice_cols)
      if(!entry$omit_na){
        # change NAs from all other choice columns to 0
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(across(starts_with(paste0(col,"___")), ~replace_na(., 0)))
        # create a new NA column
        na_colname <- paste0(col,"___NA")
        data.list[[sheet]][[na_colname]] <- is.na(data.list[[sheet]][[col]]) %>% as.numeric
        data.list[[sheet]] <- data.list[[sheet]] %>% relocate(all_of(na_colname), .after = !!sym(col))
      }
    }else {
      if(entry$func == "select_one") {
        # try to convert to label:
        choice_names <- tool.choices %>% filter(list_name == entry$list_name) %>% pull(name)
        not_in_choices <- data.list[[sheet]] %>% filter(!(!!sym(col) %in% choice_names) & !isna(!!sym(col))) %>%
          pull(!!sym(col)) %>% unique
        if(length(not_in_choices) > 0){
          conv_vec <- data.list[[sheet]][[col]]
        }else{
          if(!entry$list_name %in% tool.choices$list_name) stop(paste("list",entry$list_name, "not found in tool.choices!"))
          
          res <- data.frame(name = unlist(data.list[[sheet]][[col]])) %>%
            left_join(dplyr::select(tool.choices, name, list_name, label_colname()) %>% filter(list_name == entry$list_name),
                      by = "name", na_matches = "never")
          if(nrow(res) == 0) stop("All choices not in the list!")
          
          conv_vec <- pull(res, label_colname())
        }
        if(entry$omit_na) {
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NA)
        }else{
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NULL)
        }
        rm(conv_vec, choice_names, not_in_choices)
      }
      else if(entry$func %in% c("mean", "median", "integer", "numeric","decimal")) data.list[[sheet]][[col]] <- as.numeric(data.list[[sheet]][[col]])
    }
    ## Deal with disaggregated change to Label
    if(disag.col == "No disaggregation"){
    } else {
      type.disag.col <- tool.survey %>% 
        filter(name == disag.col) %>% pull(q.type)
      list_name_disag <- tool.survey %>%
        filter(name == disag.col) %>% pull(list_name)
      if(type.disag.col == "select_multiple"){
        data.list[[sheet]][[disag.col]] <- xml2label_choices_multiple(tool.survey,tool.choices,label_colname(),data.list[[sheet]],disag.col)
      }else {
        if(type.disag.col == "select_one") {
          # try to convert to label:
          choice_names <- tool.choices %>% filter(list_name == list_name_disag) %>% pull(name)
          not_in_choices <- data.list[[sheet]] %>% filter(!(!!sym(disag.col) %in% choice_names)) %>%
            pull(!!sym(disag.col)) %>% unique %>% na.omit()
          if(length(not_in_choices) > 0){
            conv_vec <- data.list[[sheet]][[disag.col]]
          }else{
            if(!list_name_disag %in% tool.choices$list_name) stop(paste("list",list_name_disag, "not found in tool.choices!"))
            
            res <- data.frame(name = unlist(data.list[[sheet]][[disag.col]])) %>%
              left_join(dplyr::select(tool.choices, name, list_name, label_colname()) %>% filter(list_name == list_name_disag),
                        by = "name", na_matches = "never")
            if(nrow(res) == 0) stop("All choices not in the list!")
            
            conv_vec <- pull(res, label_colname())
          }
          if(entry$omit_na) {
            data.list[[sheet]][[disag.col]] <- factor(conv_vec, exclude = NA)
          }else{
            data.list[[sheet]][[disag.col]] <- factor(conv_vec, exclude = NULL)
          }
          rm(conv_vec, choice_names, not_in_choices)
        }
        else if(type.disag.col %in% c("mean", "median", "integer", "numeric","decimal")) data.list[[sheet]][[disag.col]] <- as.numeric(data.list[[sheet]][[disag.col]])
      }
    }
 
    survey_data <- data.list[[sheet]]
    srvyr.designs[[sheet]] <- as_survey_design(survey_data, weights = weight)
    return(srvyr.designs)
  })
  
  ########## Creating the All NA Output ##########
  output$allNA <- renderUI({
      req(input$sheet,
          input$variable)
      # entry <- entry()
      # if(input$admin == "strata") entry$admin <- "strata"
      if(entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])) {
        HTML("")
      } else {
        if(input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
          HTML("")
        } else {
          srvyr.designs <- srvyr.designs()
          if(length(srvyr.designs[[input$sheet]][["variables"]][[input$variable]][!is.na(srvyr.designs[[input$sheet]][["variables"]][[input$variable]])]) == 0){
            HTML("<h4><em>No data for this variable (all NA)</em></h4>")
          } else{
            HTML("<h4><em>", as_perc((length(srvyr.designs[[input$sheet]][["variables"]][[input$variable]][!is.na(srvyr.designs[[input$sheet]][["variables"]][[input$variable]])])/nrow(data.list()[[input$sheet]]))),"of respondents answered this question.</em></h4>")
          }
        }
      }
    })
  
  
  ########## Creating the table ##########
  table <- reactive({
    req(input$dataInput$datapath,
        input$toolInput$datapath,
        input$sheet,
        input$variable)
    entry <- entry()
    srvyr.design <- srvyr.designs()
    tool.choices <- tool.choices()
    label_colname <- label_colname()
    if(entry$omit_na) srvyr.design[[input$sheet]] <- srvyr.design[[input$sheet]] %>% filter(!is.na(!!sym(input$variable)))
    if(entry$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])){
      srvyr.design[[input$sheet]]  <- srvyr.design[[input$sheet]]
    } else {
      srvyr.design[[input$sheet]]  <- srvyr.design[[input$sheet]] %>% group_by(!!sym(entry$admin))
    }
    for (disagg.var in entry$disaggregate.variables) {
      res <- make_table(srvyr.design[[input$sheet]],tool.choices = tool.choices,label_colname = label_colname, entry, disagg.var) %>% ungroup %>% dplyr::select(-any_of("overall"))
      # add overall
      if(entry$admin != "overall"){
        entry.ovrl <- entry
        entry.ovrl$admin <- "overall"
        if(!"overall" %in% (srvyr.design[[input$sheet]] %>% variable.names)) srvyr.design[[input$sheet]] <- srvyr.design[[input$sheet]] %>% mutate(overall = "overall")
        res.overall <- make_table(srvyr.design[[input$sheet]] %>% ungroup %>% group_by(overall),tool.choices = tool.choices,label_colname = label_colname,
                                  entry.ovrl, disagg.var)  %>%
          mutate(!!sym(entry$admin) := "overall") %>%
          ungroup %>% dplyr::select(-any_of("overall"))
        res <- res %>% bind_rows(res.overall) %>% distinct
      }
    }
    return(res)
  })
  
  ########## Creating the table Output ##########
  output$table <- renderUI({
      if(entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])){
        HTML("")
      } else {
        if(input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
          HTML("")
        } else{
          withProgress(
            message = "Generating Table...",
            value = NULL, {
              renderDataTable(table(), extensions= 'Buttons', options = tableFormat)
            }
          )
        }
      }
    })
  
  ############################################################################################################
  #                                                     Graph                                                #
  ############################################################################################################
  
  ########## Getting the Sheet input ##########
  output$sheetInputGraph <- renderUI({
    req(input$toolInput$datapath,
        input$dataInput$datapath)
    tool_var <- excel_sheets(input$dataInput$datapath)
    # tool.survey() %>% filter(!is.na(datasheet)) %>% pull(datasheet) %>% unique()
    selectInput("sheetGraph","Sheet",
                choices = c(tool_var),
                selected = NULL,
                multiple = F)
  })
  
  ########## Getting the Variable Input ##########
  observeEvent(input$sheetGraph, {
    output$varGraph <- renderUI({
      req(input$toolInput$datapath,
          input$dataInput$datapath)
      df <- read_excel(input$dataInput$datapath,sheet = input$sheetGraph, col_types = "text")
      empty_columns <- names(df[, colSums(is.na(df) | df == "") == nrow(df)])
      tool_var <- tool.survey() %>%
        filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheetGraph & name %in% colnames(df) & !name %in% empty_columns) %>% pull(name) %>% unique
      if(length(tool_var)>0){
      selectizeInput("variableGraph","Variable",
                  choices = c(tool_var),
                  selected = tool_var[1],
                  multiple = T,
                  options = list(maxOptions = 1000L, maxItems = 1))
    } else {
      HTML("<h4  style = 'color: rgba(238, 88, 89, .8)'>Please check that the sheet names of the dataset are matching with the tool</h4>")
    }
    })
  })
  
  ########## Getting the Disaggregated Variable Input ##########
  observeEvent(input$sheetGraph, {
    output$dis_var_Graph <- renderUI({
      req(input$toolInput$datapath,
          input$dataInput$datapath,
          input$variableGraph)
      df <- read_excel(input$dataInput$datapath,sheet = input$sheetGraph, col_types = "text")
      empty_columns <- names(df[, colSums(is.na(df) | df == "") == nrow(df)])
      tool_var <- tool.survey() %>%
        filter(q.type %in% c("integer","select_one","select_multiple","decimal") & datasheet == input$sheetGraph & name %in% colnames(df) & name != input$variableGraph & !name %in% empty_columns) %>% pull(name) %>% unique
      if(length(tool_var)>0){
      selectizeInput("dis_variables_Graph","Disaggregation",
                  choices = c("No disaggregation", tool_var),
                  selected = "No disaggregation",
                  multiple = T,
                  options = list(maxOptions = 1000L, maxItems = 1))
      } else {
        HTML("<h4  style = 'color: rgba(238, 88, 89, .8)'>Please check that the sheet names of the dataset are matching with the tool</h4>")
      }
    })
  })
  
  ########## Getting the entry ##########
  entryGraph <- reactive({
    req(input$variableGraph,
        input$toolInput$datapath,
        input$dis_variables_Graph)
    entry <- list()
    entry$variable <- input$variableGraph
    not_in_tool <- input$variableGraph[!input$variableGraph %in% tool.survey()$name]
    if (any(str_detect(input$variableGraph, "/"))) input$variableGraph <- str_split(input$variableGraph, "/", 2, T)[,1]
    res <- data.frame(name = input$variableGraph) %>% 
      left_join(dplyr::select(tool.survey(), name, !!sym(label_colname())), by = "name", na_matches = "never") %>% 
      pull(label_colname())
    if(is.na(res)){
      entry$label <- "No label"
    } else{
      entry$label <- res
    }
    if (input$dis_variables_Graph == "No disaggregation"){
      entry$disaggregate.variables <- NA
      entry$disaggregations <- NA
    } else{
      entry$disaggregate.variables <- input$dis_variables_Graph
      entry$disaggregations <- input$dis_variables_Graph
    }
    
    if (is.na(input$calculation)){
      entry$calculation <- NA
    } else{
      entry$calculation <- input$calculation
    }
    entry$func <- tool.survey() %>%
      filter(name == input$variableGraph) %>% pull(q.type)
    
    if(entry$func %in% c("calculate","integer","decimal")) entry$func <- "numeric"
    
    entry$var_type <- input$func
    entry$admin <- input$adminGraph
    entry$list_name <- tool.survey() %>%
      filter(name == input$variableGraph) %>% pull(list_name)
    entry$comments <- ''
    entry$datasheet<- tool.survey() %>%
      filter(name == input$variableGraph) %>% pull(datasheet)
    if (input$calculation == "include_na"){
      entry$omit_na <- F
    } else{
      entry$omit_na <- T
    }
    if (input$calculation == "add_total"){
      entry$add_total <- T
    } else{
      entry$add_total <- F
    }
    return(entry)
  })
  
  
  ########## Getting the Data List ##########
  data.listGraph <- reactive({
    req(input$dataInput$datapath,
        input$weightBTNGraph)
    tool.survey <- tool.survey()
    sheet_names <- excel_sheets(input$dataInput$datapath)
    sheet_names[1] <- "main"
    data.list <- list("main" = read_excel(input$dataInput$datapath, sheet=1, col_types = "text"))
    for(sheet in sheet_names[-1])
      data.list[[sheet]] <- read_excel(input$dataInput$datapath, sheet=sheet, col_types = "text")
    tool_datasheets <- tool.survey %>% distinct(datasheet) %>% filter(!is.na(.)) %>% pull
    for (sheet in sheet_names[-1]) {
      # take the first column from this sheet and find it in tool.survey
      i <- 1
      data_cnames <- data.list[[sheet]] %>% dplyr::select(-contains("/"), -starts_with("_")) %>% names
      first_col <- data_cnames[i]
      while (!first_col %in% tool.survey()$name) {
        i <- i + 1
        first_col <- data_cnames[i]
      }
      old_sheetname <- tool.survey %>% filter(name == first_col) %>% pull(datasheet)
      # change all occurences of `old_sheetname` to `sheet`
      tool.survey <- tool.survey %>% mutate(datasheet = ifelse(datasheet %==na% old_sheetname, sheet, datasheet))
    }
    for (sheet in names(data.list)){
      if(input$weightBTNGraph == "no") {
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall",
                 weight = 1)
      } else if (input$weightBTNGraph == "yes" & "weight" %in% colnames(data.list[[sheet]])){
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall",
                 weight = as.numeric(weight)) 
        
      } else{
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(overall = "overall",
                 weight = 1) 
      }
    }
    tryCatch(
      return(data.list), error = function(err) return(data.frame())
    )
  })
  ########## Creating the Srvyr Design List ##########
  srvyr.designsGraph <- reactive({
    req(input$dataInput$datapath,
        input$toolInput$datapath,
        input$sheetGraph,
        input$variableGraph,
        input$dis_variables_Graph,
        input$weightBTNGraph)
    data.list <- data.listGraph()
    srvyr.designs <- list()
    entry <- entryGraph()
    tool.survey <- tool.survey()
    tool.choices <- tool.choices()
    sheet <- input$sheetGraph
    col <- input$variableGraph
    disag.col <- input$dis_variables_Graph
    if(!all(is.na(entry$disaggregate.variables))){
      for(disagg.var in entry$disaggregate.variables){
        if(!disagg.var %in% colnames(data.list[[sheet]])){
          # disagg.var was not found, but maybe it's located in main? let's try to fix!
          if(sheet == "main") stop("Disaggregation variable ", disagg.var, " was not found in main!!\n")
          if(disagg.var %in% colnames(data.list$main)){
            # cat("... Disaggregation variable", disagg.var,"was not found in sheet",sheet,"but it exists in main. Will attempt to simply left_join by uuid... ")
            join_attempt <- data.list()$main %>% dplyr::select(uuid, !!sym(disagg.var))
            data.list[[sheet]] <- data.list[[sheet]] %>% left_join(join_attempt, by = "uuid")
            # cat(" success!")
          }else stop(paste("Disaggregation variable", disagg.var, "not found in sheet",sheet,"nor in main!!\n"))
        }
        data.list[[sheet]][[disagg.var]] <- as.character(data.list[[sheet]][[disagg.var]])  # as character, not factor!
      }
    }
    if(entry$func == "select_multiple"){
      # not converting to label here. instead just replace "/" with "___" and convert to numeric
      choice_cols <- colnames(data.list[[sheet]])[colnames(data.list[[sheet]]) %>% str_starts(paste0(col, "/"))]
      data.list[[sheet]] <- data.list[[sheet]] %>%
        mutate(across(all_of(choice_cols), as.numeric)) %>%
        rename_with(~str_replace(., "/", "___"), choice_cols)
      if(!entry$omit_na){
        # change NAs from all other choice columns to 0
        data.list[[sheet]] <- data.list[[sheet]] %>%
          mutate(across(starts_with(paste0(col,"___")), ~replace_na(., 0)))
        # create a new NA column
        na_colname <- paste0(col,"___NA")
        data.list[[sheet]][[na_colname]] <- is.na(data.list[[sheet]][[col]]) %>% as.numeric
        data.list[[sheet]] <- data.list[[sheet]] %>% relocate(all_of(na_colname), .after = !!sym(col))
      }
    }else {
      if(entry$func == "select_one") {
        # try to convert to label:
        choice_names <- tool.choices %>% filter(list_name == entry$list_name) %>% pull(name)
        not_in_choices <- data.list[[sheet]] %>% filter(!(!!sym(col) %in% choice_names) & !isna(!!sym(col))) %>%
          pull(!!sym(col)) %>% unique
        if(length(not_in_choices) > 0){

          conv_vec <- data.list[[sheet]][[col]]
        }else{
          if(!entry$list_name %in% tool.choices$list_name) stop(paste("list",entry$list_name, "not found in tool.choices!"))
          
          res <- data.frame(name = unlist(data.list[[sheet]][[col]])) %>%
            left_join(dplyr::select(tool.choices, name, list_name, label_colname()) %>% filter(list_name == entry$list_name),
                      by = "name", na_matches = "never")
          if(nrow(res) == 0) stop("All choices not in the list!")
          
          conv_vec <- pull(res, label_colname())
        }
        if(entry$omit_na) {
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NA)
        }else{
          data.list[[sheet]][[col]] <- factor(conv_vec, exclude = NULL)
        }
        rm(conv_vec, choice_names, not_in_choices)
      }
      else if(entry$func %in% c("mean", "median", "integer", "numeric","decimal")) data.list[[sheet]][[col]] <- as.numeric(data.list[[sheet]][[col]])
    }
    ## Deal with disaggregated change to Label
    if(disag.col == "No disaggregation"){
    } else {
      type.disag.col <- tool.survey %>% 
        filter(name == disag.col) %>% pull(q.type)
      list_name_disag <- tool.survey %>%
        filter(name == disag.col) %>% pull(list_name)
      if(type.disag.col == "select_multiple"){
        data.list[[sheet]][[disag.col]] <- xml2label_choices_multiple(tool.survey,tool.choices,label_colname(),data.list[[sheet]],disag.col)
      }else {
        if(type.disag.col == "select_one") {
          # try to convert to label:
          choice_names <- tool.choices %>% filter(list_name == list_name_disag) %>% pull(name)
          not_in_choices <- data.list[[sheet]] %>% filter(!(!!sym(disag.col) %in% choice_names)) %>%
            pull(!!sym(disag.col)) %>% unique
          if(length(not_in_choices) > 0){
            conv_vec <- data.list[[sheet]][[disag.col]]
          }else{
            if(!list_name_disag %in% tool.choices$list_name) stop(paste("list",list_name_disag, "not found in tool.choices!"))
            
            res <- data.frame(name = unlist(data.list[[sheet]][[disag.col]])) %>%
              left_join(dplyr::select(tool.choices, name, list_name, label_colname()) %>% filter(list_name == list_name_disag),
                        by = "name", na_matches = "never")
            if(nrow(res) == 0) stop("All choices not in the list!")
            
            conv_vec <- pull(res, label_colname())
          }
          if(entry$omit_na) {
            data.list[[sheet]][[disag.col]] <- factor(conv_vec, exclude = NA)
          }else{
            data.list[[sheet]][[disag.col]] <- factor(conv_vec, exclude = NULL)
          }
          rm(conv_vec, choice_names, not_in_choices)
        }
        else if(type.disag.col %in% c("mean", "median", "integer", "numeric","decimal")) data.list[[sheet]][[disag.col]] <- as.numeric(data.list[[sheet]][[disag.col]])
      }
    }
    survey_data <- data.list[[sheet]]
    srvyr.designs[[sheet]] <- as_survey_design(survey_data, weights = weight)
    return(srvyr.designs)
  })

  ########## Creating the table ##########
  tableGraph <- reactive({
    req(input$dataInput$datapath,
        input$toolInput$datapath,
        input$sheetGraph,
        input$variableGraph)
    entry <- entryGraph()
    tool.choices <- tool.choices()
    label_colname <- label_colname()
    srvyr.design <- srvyr.designsGraph()
    if(entry$omit_na) srvyr.design[[input$sheetGraph]] <- srvyr.design[[input$sheetGraph]] %>% filter(!is.na(!!sym(input$variableGraph)))
    srvyr.design[[input$sheetGraph]]  <- srvyr.design[[input$sheetGraph]] %>% group_by(!!sym(entry$admin))
    for (disagg.var in entry$disaggregate.variables) {
      res <- make_table(srvyr.design[[input$sheetGraph]],tool.choices = tool.choices, label_colname = label_colname, entry, disagg.var) %>% ungroup %>% dplyr::select(-any_of("overall"))
      # add overall
      if(entry$admin != "overall"){
        entry.ovrl <- entry
        entry.ovrl$admin <- "overall"
        if(!"overall" %in% (srvyr.design[[input$sheetGraph]] %>% variable.names)) srvyr.design[[input$sheetGraph]] <- srvyr.design[[input$sheetGraph]] %>% mutate(overall = "overall")
        res.overall <- make_table(srvyr.design[[input$sheetGraph]] %>% ungroup %>% group_by(overall),tool.choices = tool.choices, label_colname = label_colname,
                                  entry.ovrl, disagg.var)  %>%
          mutate(!!sym(entry$admin) := "overall") %>%
          ungroup %>% dplyr::select(-any_of("overall"))
        res <- res %>% bind_rows(res.overall) %>% distinct
      }
    }
    return(res)
  })
  ########## Creating the correlation table ##########
  correlation_table <- reactive({
    req(input$variableGraph,
        input$dis_variables_Graph)
    cols <- colnames(tableGraph())
    if(input$adminGraph == "strata"){
      cols <- cols[!cols %in% c(input$dis_variables_Graph,"num_samples","strata")]
    } else{
      cols <- cols[!cols %in% c(input$dis_variables_Graph,"num_samples")]
    }
    if(entryGraph()$func != "numeric"){
      table <- tableGraph() %>%
        mutate_at(cols, ~gsub("%","",.)) %>%
        mutate_at(cols, as.numeric) %>%
        mutate_at(cols, ~round((. * num_samples)/100),0) %>%
        dplyr::select(-num_samples)
      if(input$adminGraph == "strata"){
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0,strata != "overall")
      }else{
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0)
      }
    } else{
      table <- tableGraph() %>% 
         dplyr::select(-num_samples)
      if(input$adminGraph == "strata"){
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0,strata != "overall")
      }else{
        table <- table %>%
          pivot_longer(cols = cols,names_to = "names", values_to = "value") %>%
          filter(value != 0)
      }
    }
    return(table)
  })
  output$graph <- renderUI({
    req(input$variableGraph,
        input$dis_variables_Graph,
        input$adminGraph)
    data.list <- data.listGraph()
    tool.survey <- tool.survey()
    if(input$adminGraph == "strata" && !"strata" %in% colnames(data.list[[input$sheetGraph]])){
      HTML("")
    } else{
      if(input$weightBTNGraph == "yes" && !"weight" %in% colnames(data.list[[input$sheetGraph]])){
        HTML("")
      } else{
        if(input$dis_variables_Graph == "No disaggregation"){
          if(input$adminGraph == "strata"){
            if(entryGraph()$func == "numeric"){
              radioButtons("chooseGraph","Choose Graph",
                           choices = c(Barplot = "bar"))
            }else{
              radioButtons("chooseGraph","Choose Graph",
                           choices = c(Barplot = "bar",
                                       Line = "line"))
            }
          }else{
            if(entryGraph()$func == "numeric"){
              radioButtons("chooseGraph","Choose Graph",
                           choices = c(Barplot = "bar",
                                       Gauge = "gauge"))
            }else{
              if(entryGraph()$func == "select_one"){
                radioButtons("chooseGraph","Choose Graph",
                             choices = c(Barplot = "bar",
                                         Line = "line",
                                         Pie = "pie",
                                         Donut = "donut",
                                         RoseType = "roseType")) ## ADD A RADAR for also Strata
              } else {
                radioButtons("chooseGraph","Choose Graph",
                             choices = c(Barplot = "bar",
                                         Line = "line")) ## ADD A RADAR for also Strata
              }
              
            }
          }
        }else{
          if(input$adminGraph == "strata"){
            if(entryGraph()$func == "numeric"){
              list_numeric <- tool.survey() %>% filter(q.type %in% c("integer","decimal") & datasheet == input$sheetGraph & name %in% colnames(data.list[[input$sheetGraph]]) & name != input$variableGraph) %>% pull(name) %>% unique
              if(input$dis_variables_Graph %in% list_numeric){
                radioButtons("chooseGraph","Choose Graph",
                             choices = c(Barplot = "bar",
                                         Scatter = "scatter"))
              } else {
                radioButtons("chooseGraph","Choose Graph",
                             choices = c(Barplot = "bar"))
              }
              
            }else{
              radioButtons("chooseGraph","Choose Graph",
                           choices = c(Barplot = "bar"))
            }
          }else{
            if(entryGraph()$func=="numeric"){
              list_numeric <- tool.survey() %>% filter(q.type %in% c("integer","decimal") & datasheet == input$sheetGraph & name %in% colnames(data.list[[input$sheetGraph]]) & name != input$variableGraph) %>% pull(name) %>% unique
              if(input$dis_variables_Graph %in% list_numeric){
              radioButtons("chooseGraph","Choose Graph",
                           choices = c(Barplot = "bar",
                                       Scatter = "scatter"))
              }else {
                radioButtons("chooseGraph","Choose Graph",
                             choices = c(Barplot = "bar"))
              }
            }else{
              radioButtons("chooseGraph","Choose Graph",
                           choices = c(Barplot = "bar",
                                       Heatmap = "heatmap"))
            }
          }
        }
      }
    }
  })

  plot <- reactive({
    req(input$variableGraph,
        input$dis_variables_Graph,
        input$chooseGraph)
    correlation_table <- correlation_table()
    data.list <- data.listGraph()
      if(input$dis_variables_Graph == "No disaggregation"){
        if(input$adminGraph == "strata"){
          if(entryGraph()$func == "numeric"){ 
            correlation_table <- filter(correlation_table, names == "mean")
            ## Numeric/Strata/No disagg
            correlation_table %>% 
              group_by(strata) %>% 
              mutate(names = str_wrap(names,15)) %>% 
              e_charts(names) %>% 
              e_bar(value) %>% 
              e_flip_coords() %>% 
              e_labels(position = c("right")) %>% 
              e_tooltip(trigger = "item")
            
          }else{
            ## Select/Strata/No disagg
            if(input$chooseGraph == "bar"){
              correlation_table %>% 
                group_by(strata) %>% 
                arrange(value) %>% 
                mutate(names = str_wrap(names,15)) %>% 
                e_charts(names) %>% 
                e_bar(value) %>% 
                e_flip_coords() %>% 
                e_labels(position = c("right")) %>% 
                e_tooltip(trigger = "item")
            } else if(input$chooseGraph == "line") {
              correlation_table %>% 
                group_by(strata) %>% 
                mutate(names = str_wrap(names, 15)) %>% 
                e_charts(names) %>% 
                e_line(value) %>% 
                e_x_axis(axisLabel = list(
                  interval = 0L,
                  rotate = 45
                )) %>% 
                e_tooltip(trigger = "axis") %>% 
                e_animation(duration = 6000)
            }
          }
        }else{
          if(entryGraph()$func == "numeric"){
            ## Numeric/overall/No disagg
            if(input$chooseGraph == "bar") {
              correlation_table <- filter(correlation_table, names == "mean")
              correlation_table %>% 
                mutate(names = str_wrap(names,15)) %>% 
                e_charts(names) %>% 
                e_bar(value) %>% 
                e_flip_coords() %>% 
                e_labels(position = c("right")) %>% 
                e_tooltip(trigger = "item")
              
            } else if (input$chooseGraph == "gauge"){
              e_charts() %>% 
                e_gauge(correlation_table %>% 
                          filter(names == "mean") %>% 
                          pull(value),"Mean", 
                        min = correlation_table %>% 
                          filter(names == "min") %>% 
                          pull(value),
                        max = correlation_table %>% 
                          filter(names == "max") %>% 
                          pull(value))
            }
            
          }else{
            if(input$chooseGraph == "bar"){
              ## Select/overall/No disagg
              correlation_table %>% 
                arrange(value) %>% 
                mutate(names = str_wrap(names,15)) %>% 
                e_charts(names) %>% 
                e_bar(value) %>% 
                e_x_axis(axisLabel = list(
                  interval = 0L
                )) %>% 
                e_flip_coords() %>%
                e_labels(position = c("right")) %>% 
                e_tooltip(trigger = "item")
       
            } else if (input$chooseGraph == "line"){
              correlation_table %>% 
                mutate(names = str_wrap(names, 15)) %>% 
                e_charts(names) %>% 
                e_line(value) %>% 
                e_x_axis(axisLabel = list(
                  interval = 0L,
                  rotate = 45
                )) %>% 
                e_tooltip(trigger = "axis")
            } else if (input$chooseGraph == "pie") {
              correlation_table <- correlation_table %>% 
                mutate(perc = round((as.numeric(value)/sum(as.numeric(value))) * 100,2))
              correlation_table %>% 
                mutate(names = str_wrap(names, 15)) %>% 
                e_charts(names) %>% 
                e_pie(perc) %>%
                # e_labels(position = c("inside")) %>% 
                e_tooltip(trigger = "item")
              
            }else if (input$chooseGraph == "donut") {
              correlation_table <- correlation_table %>% 
                mutate(perc = round((as.numeric(value)/sum(as.numeric(value))) * 100,2))
              correlation_table %>% 
                mutate(names = str_wrap(names, 15)) %>% 
                e_charts(names) %>% 
                e_pie(perc, radius = c("50%","70%")) %>%
                # e_labels(position = c("inside")) %>% 
                e_tooltip(trigger = "item")
            } else if (input$chooseGraph == "roseType") {
              correlation_table <- correlation_table %>% 
                mutate(perc = round((as.numeric(value)/sum(as.numeric(value))) * 100,2))
              correlation_table %>% 
                mutate(names = str_wrap(names, 15)) %>% 
                e_charts(names) %>% 
                e_pie(perc, roseType = "radius") %>%
                # e_labels(position = c("inside")) %>% 
                e_tooltip(trigger = "item")
            }
          }
        }
      } else{
        if(input$adminGraph == "strata"){
          if(entryGraph()$func == "numeric"){
            if(input$chooseGraph == "bar"){
              correlation_table <- filter(correlation_table, names == "mean")
              correlation_table %>% 
                group_by(strata) %>% 
                rename(dis = input$dis_variables_Graph) %>%
                mutate(names = str_wrap(names,15)) %>% 
                e_charts(dis) %>% 
                e_bar(value) %>%
                e_flip_coords() %>%
                e_x_axis(axisLabel = list(
                  interval = 0L
                )) %>% 
                e_labels(position = c("right")) %>% 
                e_tooltip(trigger = "item")
            } else if(input$chooseGraph == "scatter"){
                point_table <- data.list[[input$sheetGraph]] %>%
                  dplyr::select(input$variableGraph, input$dis_variables_Graph, strata) %>% 
                  mutate(!!sym(input$variableGraph) := as.numeric(!!sym(input$variableGraph)),
                         !!sym(input$dis_variables_Graph) := as.numeric(!!sym(input$dis_variables_Graph)))
                point_table %>% 
                  group_by(strata) %>% 
                  e_charts_(input$variableGraph) %>% 
                  e_scatter_(input$dis_variables_Graph, symbol_size = 10) 
            } 
            
          }else{
            test <- correlation_table %>% 
              pivot_wider(names_from = input$dis_variables_Graph, values_from = value)
            
            if (ncol(test) >= 4) {
              test %>%
                group_by(strata) %>%
                mutate(names = str_wrap(names,15)) %>% 
                e_charts(names, timeline = TRUE) %>%
                { 
                  e_plot <- .
                  for (i in 3:ncol(test)) {
                    e_plot <- e_plot %>%
                      e_bar_(names(test)[i], stack = paste0("grp", i - 2))
                  }
                  e_plot %>%
                    e_flip_coords() %>%
                    e_x_axis(axisLabel = list(interval = 0L)) %>%
                    e_labels(position = c("inside")) %>%
                    e_tooltip(trigger = "item")
                }
            }

            
           
          }
        }else{
          if(entryGraph()$func=="numeric"){
            if(input$chooseGraph == "bar"){
              correlation_table <- filter(correlation_table, names == "mean")
              correlation_table %>% 
                rename(dis = input$dis_variables_Graph) %>%
                mutate(names = str_wrap(names,15)) %>% 
                e_charts(dis) %>% 
                e_bar(value) %>%
                e_flip_coords() %>% 
                e_labels(position = c("right")) %>% 
                e_tooltip(trigger = "item")
             
            } else if(input$chooseGraph == "scatter"){
              point_table <- data.list[[input$sheetGraph]] %>%
                dplyr::select(input$variableGraph, input$dis_variables_Graph) %>% 
                mutate(!!sym(input$variableGraph) := as.numeric(!!sym(input$variableGraph)),
                       !!sym(input$dis_variables_Graph) := as.numeric(!!sym(input$dis_variables_Graph)))
              point_table %>% 
                e_charts_(input$variableGraph) %>% 
                e_scatter_(input$dis_variables_Graph, symbol_size = 10) 
            }
            
          }else{
            if(input$chooseGraph == "heatmap"){
              correlation_table %>% 
                arrange(value) %>% 
                rename(dis = input$dis_variables_Graph) %>%
                mutate(names= str_wrap(names,15)) %>%
                e_charts(names) %>% 
                e_heatmap(dis,value) %>% 
                e_visual_map(value) %>% 
                e_x_axis(axisLabel = list(
                  interval = 0L,
                  rotate = 45
                )) %>% 
                e_tooltip(trigger = "item")
            } else if(input$chooseGraph == "bar"){
              correlation_table <- correlation_table %>% 
                pivot_wider(names_from = input$dis_variables_Graph, values_from = value)
              if (ncol(correlation_table) >= 2) {
                correlation_table %>%
                  mutate(names = str_wrap(names, 15)) %>% 
                  e_charts_(names(correlation_table[1])) %>%
                  {
                    e_plot <- .
                    for (i in 2:ncol(correlation_table)) {
                      e_plot <- e_plot %>%
                        e_bar_(names(correlation_table[i]), stack = paste0("grp", i - 1))
                    }
                    e_plot %>%
                      e_flip_coords() %>%
                      e_x_axis(axisLabel = list(interval = 0L)) %>%
                      e_labels(position = c("right")) %>%
                      e_tooltip(trigger = "item")
                  }
              }
            } 
          }
        }
      }
  })
  

  output$textPlot <- renderUI({
    req(input$variableGraph,
        input$dis_variables_Graph)
    if(input$adminGraph == "strata" & !"strata" %in% colnames(data.listGraph()[[input$sheetGraph]])|
       input$weightBTNGraph == "yes" & !"weight" %in% colnames(data.listGraph()[[input$sheetGraph]])){
        HTML("<h4 class = 'title-message' style = 'color: rgba(238, 88, 89, .8)'>Please ensure to have a column named strata or weight in your data</h4>")
    }
  }) 
  observe({
    req(input$variableGraph,
        input$dis_variables_Graph)
    output$plotGraph <- renderEcharts4r(
          if(input$adminGraph == "strata" & !"strata" %in% colnames(data.listGraph()[[input$sheetGraph]])){
          }else{
            if(input$weightBTNGraph == "yes" & !"weight" %in% colnames(data.listGraph()[[input$sheetGraph]])){
            } else{
              plot() %>% 
                e_theme_custom("www/REACH_Theme.json") %>%
                e_color(background = "#FFFFFF") %>% 
                e_toolbox_feature(feature = c("saveAsImage","dataZoom"))
            }
          })
  })
  
  ############################################################################################################
  #                                                     Variance                                             #
  ############################################################################################################
  variance <- reactive({
    req(input$variable,
        input$dis_variables,
        input$admin,
        input$min_sample_num)
    variable <- as.character(input$variable)
    dis_variable <- as.character(input$dis_variables)
    entry <- entry()
    sheet <- input$sheet
    data.list <- data.list()
    admin <- as.character(input$admin)
    tool.choices <- tool.choices()
    tool.survey <- tool.survey()
    label_colname <- label_colname()
    min_sample_num <- input$min_sample_num
    kobo_tool <- tool.survey %>% 
      mutate(type = sub("^(\\S*\\s+\\S+).*", "\\1", type)) %>%
      dplyr::rename(label_english_varname = label_colname) %>% 
      # get the q type and the list_name to get the choices
      separate(type,
               into = c('type', 'list_name'),
               sep = " ") %>%
      filter(type %in% c('select_one', 'select_multiple')) %>%
      dplyr::select(name, list_name,label_english_varname,type) %>% 
      dplyr::rename(varname = name) %>% 
      distinct() 
    kobo_choices <- tool.choices %>% 
      dplyr::rename(label_english = label_colname, ## FIXXX ERRROR
                    choice_name = name) %>% 
      dplyr::select(list_name, choice_name, label_english)
    
    kobo_full <- kobo_tool %>% left_join(kobo_choices,relationship = "many-to-many")  
    res_total_full <-  data.frame()
    if(dis_variable == "No disaggregation" && admin == "overall"){
      res <- HTML(HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>"),"This feature requires: <br> - 1 variable and by admin (strata selected).<br>- 2 variables and overall selected.<br>- 2 variables and by admin (strata selected).")
    return(res)
    } else if(dis_variable == "No disaggregation" && admin == "strata") {
      dictionary <-   kobo_full %>% 
        filter(varname == "strata")
      txt <-  paste0(variable,"~","strata")
      # get the function that needs to be applied 
      func_used <- entry$func
      n <- nrow(data.list[[sheet]])
      # clean na rows
      data_clean <- data.list[[sheet]] %>% 
        mutate(uuid = rep(1:n)) %>% 
        filter(!is.na(!!sym(variable))) %>% 
        dplyr::select(uuid,!!sym("strata"),!!sym(variable))
      
      # get the list of large admin units for the future inter_group difference testing
      big_admins <- data_clean %>% 
        group_by(!!sym("strata")) %>% 
        summarise(cnt = n()) %>% 
        filter(cnt > min_sample_num) %>% 
        pull(!!sym("strata"))
      if(length(na.omit(unique(data_clean[["strata"]])))<2 | length(na.omit(unique(data_clean[[variable]])))<2){
        result <- "All observations are in the same group."
      }else{
      # If the thing that we're testing is a select multiple, we need to split the answers
      if(func_used == 'select_multiple'){
        
        data_clean <- tidyr::separate_rows(data_clean,!!sym(variable) , sep= "[ \n]")
        
        data_clean <- fastDummies::dummy_cols(data_clean, select_columns =variable )
        
        col_list <- setdiff(names(data_clean),c("uuid","strata",variable))
        
        data_clean <- data_clean %>% 
          group_by(uuid) %>% 
          summarise(admin = unique(!!sym("strata")),
                    across(col_list, ~sum(.x))
          ) %>% ungroup() %>% 
          dplyr::select(-uuid)
        
        names(data_clean)[1] <- "strata"
      }
      
      # the test itself
      if(func_used %in% c('mean','decimal','numeric','integer')){
        res <- anova_test(formula = eval(parse(text=txt)),data=data_clean) %>% # get the stats
          data.frame() %>% 
          mutate(variable = variable) %>% 
          dplyr::rename(p_val=p)%>% # rename the columns
          mutate(test_conclusion = ifelse(p_val <0.01 & `F`>5, ' Significant difference', 'Insignificant difference'), #classify significance (Use F for magnitude)
                 test_result = paste0('ANOVA test results: F coefficient: ',round(`F`,2),', p-val: ',round(p_val,3))) %>% 
          dplyr::select(variable,test_conclusion,test_result)
      }else if (func_used == 'select_one'){
        res <- kruskal_test(formula = eval(parse(text=txt)),data=data_clean)%>% 
          dplyr::rename(variable = 1,
                        p_val=p)%>% # rename the columns
          mutate(test_conclusion = ifelse(p_val <0.01 , ' Significant difference', 'Insignificant difference'), #classify significance
                 test_result = paste0('Kruskal test results: Chisq: ',round(statistic,2),', p-val: ',round(p_val,3))) %>% 
          dplyr::select(variable,test_conclusion,test_result) # select what you need 
      }else if (func_used == 'select_multiple'){
        res_full = data.frame()
        for(var in col_list){
          
          text2 <- paste0( var,"~", "strata")
          res <- kruskal_test(formula = eval(parse(text=text2)),data=data_clean)%>% 
            dplyr::rename(var = 1,
                          p_val=p)
          
          res_full = rbind(res,res_full)
        }
        
        res <- res_full%>% # rename the columns
          mutate(p_val = p.adjust(p_val),
                 test_conclusion = ifelse(p_val <0.01 , ' Significant difference', 'Insignificant difference'), #classify significance
                 test_result = paste0('Kruskal test results: Chisq: ',round(statistic,2),', p-val: ',round(p_val,3))) %>% 
          filter(!test_conclusion == 'Insignificant difference') %>% 
          dplyr::select(variable,test_conclusion,test_result) %>%  # select what you need 
          summarise(variable = paste0(variable, collapse = '\n'),
                    test_conclusion = paste0(test_conclusion, collapse = '\n'),
                    test_result = paste0(test_result, collapse = '\n')
          )
      }
      
      # clean the data of small groups prior to running inter-group tests
      if(length(big_admins)<2){
        res_total <- res %>%
          mutate(test_result_det = 'Not enough significantly large subgroups to draw detailed conclusions about intergroup differences between specific subsamples',
                 disaggregations = NA,
                 admin_used = "strata")

        res_total_full <- rbind(res_total,res_total_full)
        result <- HTML(paste0("There is a/an ",str_to_lower(res_total_full[[2]])," between ", res_total_full[[1]], " and ", dis_variable, ". ",br(),br(), res_total_full$test_result_det,HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
      } else{
        
      data_clean <- data_clean %>%
        filter(!!sym("strata") %in% big_admins)

      if(func_used == 'select_multiple'){
        col_list <- apply(data_clean[,col_list],2,sum) %>%
          data.frame() %>%
          rownames_to_column(var = 'name') %>%
          filter(`.`> min_sample_num) %>%
          pull(name)


        if(length(col_list)==0){
          res_total <- res %>%
            mutate(test_result_det = 'Not enough significantly large subgroups to draw detailed conclusions about intergroup differences between specific subsamples',
                   disaggregations = NA,
                   admin_used = "strata")

          res_total_full <- rbind(res_total,res_total_full)
          next
        }}
      
      # post hoc test
      if(func_used%in%c('mean','integer','decimal','numeric')){
        post_hoc_test_result <-  data_clean %>% 
          tukey_hsd( formula = eval(parse(text=txt)))
      }else if (func_used=='select_multiple'){ 
        
        post_hoc_test_result = data.frame()
        for(var in col_list){
          
          text2 <- paste0( var,"~", "strata")
          res_post_h <- dunn_test( formula = eval(parse(text=text2)), p.adjust.method = 'bonferroni', data = data_clean)%>% 
            dplyr::rename(var = 1,
                          p_val=p)
          post_hoc_test_result = rbind(res_post_h,post_hoc_test_result)
        }
      }else{
        post_hoc_test_result <-  data_clean %>% 
          dunn_test( formula = eval(parse(text=txt)), p.adjust.method = 'holm')
      }
      
      
      post_hoc_test_result <-  post_hoc_test_result %>% 
        # filter out subgroups with small differences, and small subsamples
        filter(p.adj<0.01) %>% 
        rename(var=1) %>%  # get the text values of the result
        mutate(group1 = suppressMessages(plyr::mapvalues(group1, from=dictionary$choice_name, to=dictionary$label_english)),
               group2 = suppressMessages(plyr::mapvalues(group2, from=dictionary$choice_name, to=dictionary$label_english))
        )
      
      # how many total significant combinations were we able to find
      total_differences <- nrow(post_hoc_test_result)
      
      # get top and bottom admins
      
      strata_frequency <- post_hoc_test_result %>% 
        tidyr::pivot_longer(group1:group2, names_to = 'group', values_to = 'admin') %>% 
        group_by(var,admin) %>% 
        summarise(Freq = n()) %>% 
        filter(Freq>1) %>% 
        arrange(desc(Freq)) %>% 
        ungroup() 
      
      if(nrow(strata_frequency)>1){
        # get max and put all into 1 sentence
        top_stratas <- strata_frequency%>% 
          group_by(var) %>% 
          do(head(.,3)) %>% 
          summarise(test_result_det = paste0(paste0(admin , collapse = ', '),' being different from ', paste0(Freq,collapse=', '),
                                             ' other stratas, respectively.'))
        
        # get min and put all into 1 sentence
        bottom_stratas <- strata_frequency%>% 
          group_by(var) %>% 
          do(tail(.,3)) %>% 
          summarise(test_result_det = paste0(paste0(admin , collapse = ', '),' being different from ', 
                                             paste0(Freq,collapse=', '),
                                             ' other stratas, respectively.'))
        
        # in cases of small samples, we'll have some intersections. They can be removed like so
        rows_to_remove <- intersect(top_stratas,bottom_stratas)
        
        top_stratas <- paste0(
          paste0(top_stratas %>% reframe(test_result_det = paste0('Top Stratas with the most differences from other ones in terms of ',var,' are: ',test_result_det)) %>% pull(test_result_det),collapse = '\n'),
          '\n',
          paste0(setdiff(bottom_stratas,rows_to_remove) %>%  reframe(test_result_det = paste0('Bottom Stratas with the least differences from other ones in terms of ',var,' are: ',test_result_det)) %>% pull(test_result_det),collapse = '\n')
        )
        post_hoc <- top_stratas
        
        
      }else if(nrow(strata_frequency)==1){
        top_stratas <- paste0(strata_frequency%>% 
                                summarise(test_result_det = paste0('The only strata with being significantly different in terms of ',var,' from others is: ',
                                                                   paste0(admin , collapse = ', '),' it is different from ', paste0(Freq,collapse=', '),
                                                                   ' other stratas'))
        )
        post_hoc <- top_stratas
        
      }else{
        top_stratas <- 'Nothing significant to report'
        post_hoc <- top_stratas
      }
      # get the detailed stats for the inter-admin differences
      res_total <- res %>% 
        mutate(test_result_det = paste0('The analysis allowed to find ',total_differences,' differences between stratas.'),
               disaggregations = NA,
               admin_used = "strata")
      
      res_total_full <- rbind(res_total,res_total_full)
      if(post_hoc == "Nothing significant to report"){
        result <- HTML(paste0("There is a/an ",str_to_lower(res_total[[2]])," between ", res_total[[1]], " and the strata", ". ",br(),br(), post_hoc,HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
      } else{
        result <- HTML(paste0(res_total_full$test_result_det, br(), br(),"There is a/an ",str_to_lower(res_total[[2]])," between ", res_total[[1]], " and the strata" , ". ",br(),br(), post_hoc,HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
      }
      
      }
        return(result)
        }


    } else if(dis_variable != "No disaggregation" && admin == "strata") {
      res_total_full_mq <-  data.frame()
      # filter out combinations that aren't present in the data or are just 1 group (will break the test)
      dictionary <-   kobo_full %>% 
        filter(varname == dis_variable)
      
      admin_list <- data.list[[sheet]] %>%
        filter(!is.na(!!sym(variable)),
               !is.na(!!sym(dis_variable)),
               !is.na(!!sym("strata"))) %>%
        distinct(!!sym(variable),!!sym(dis_variable), !!sym("strata")) %>% 
        group_by(!!sym("strata")) %>% 
        mutate(n = length(unique(!!sym(variable))),
               n2 = length(unique(!!sym(dis_variable)))) %>% 
        ungroup() %>% 
        filter(n>1,
               n2>1) %>% 
        pull(!!sym("strata")) %>% 
        unique()
      
      n <- nrow(data.list[[sheet]])
      # clean na rows
      data_clean <- data.list[[sheet]] %>%
        mutate(uuid = rep(1:n)) %>% 
        filter(!!sym("strata") %in% admin_list,
                                    !is.na(!!sym(dis_variable)),
                                    !is.na(!!sym(variable)),
                                    !is.na(!!sym("strata"))) %>% 
        dplyr::select(!!sym("strata"),!!sym(dis_variable),!!sym(variable),uuid)
      
      # get the list of admins that are large enough for inter group testing 
      big_admins <- data_clean %>% 
        group_by(!!sym("strata")) %>% 
        summarise(cnt = n()) %>% 
        filter(cnt >min_sample_num) %>% 
        pull(!!sym("strata"))
      
      #filter out small admins here (since we're doing a group_by, this needs to be here)
      data_clean <- data_clean %>% 
        filter(!!sym("strata") %in% big_admins)
      
      if(length(na.omit(unique(data_clean[[dis_variable]])))<2 | length(na.omit(unique(data_clean[[variable]])))<2){
        result <- HTML("All observations are in the same group.",HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>"))
      }else{
      if (nrow(data_clean)==0){
        res_total <- data.frame(variable = variable,
                                test_conclusion = 'Not enough data to drawn conclusions',
                                test_result = 'Not enough data to drawn conclusions',
                                test_result_det = 'Not enough significantly large subgroups to draw detailed conclusions about intergroup differences between specific subsamples',
                                disaggregations = dis_variable,
                                admin_used = "strata")
        
        res_total_full_mq <- rbind(res_total_full_mq,res_total)
        
        next
      }
      
      # set up the formula
      txt <-  paste0(variable,"~", dis_variable)
      
      # get the function that needs to be applied 
      func_used <- entry$func
      
      # If the thing that we're testing is a select multiple, we need to split the answers
      if(func_used == 'select_multiple'){
        
        data_clean <- tidyr::separate_rows(data_clean,!!sym(variable) , sep= "[ \n]")
        
        data_clean <- fastDummies::dummy_cols(data_clean, select_columns =variable )
        
        col_list <- setdiff(names(data_clean),c("uuid","strata",variable,dis_variable))
        
        data_clean <- data_clean %>% 
          group_by(uuid) %>% 
          summarise(admin = unique(!!sym("strata")),
                    disag = unique(!!sym(dis_variable)),
                    across(col_list, ~sum(.x))
          ) %>% ungroup() %>% 
          dplyr::select(-uuid)
        
        names(data_clean)[1] <- "strata"
        names(data_clean)[2] <- dis_variable
        
      }
      
      
      # the test itself
      if(func_used %in% c('mean','numeric','integer','decimal')){
        test_result <- data_clean %>% 
          group_by(!!sym("strata")) %>% 
          anova_test(formula = eval(parse(text=txt))) %>% # get the stats
          data.frame() %>% 
          mutate(variable = variable,
                 admin_unit = suppressMessages(plyr::mapvalues(!!sym("strata"), from=dictionary$choice_name, to=dictionary$label_english))) %>% 
          dplyr::rename(p_val=p)%>% # rename the columns
          filter(p_val <0.05 ,
                 `F`>5)
      }else if(func_used == 'select_one'){
        test_result <- data_clean %>% 
          group_by(!!sym("strata")) %>% 
          kruskal_test(formula = eval(parse(text=txt))) %>%  # get the stats
          ungroup() %>% 
          filter(p<0.05) %>%
          dplyr::rename(variable = 2,
                        p_val=p) %>%
          mutate(admin_unit = suppressMessages(plyr::mapvalues(!!sym("strata"), from=dictionary$choice_name, to=dictionary$label_english)))
      }else{
        res_full = data.frame()
        for(var in col_list){
          
          text2 <- paste0( var,"~", dis_variable)
          test_result <- data_clean %>% 
            group_by(!!sym("strata")) %>% 
            kruskal_test(formula = eval(parse(text=text2)))%>% 
            ungroup() %>% 
            dplyr::rename(var = 2,
                          p_val=p)
          
          res_full = rbind(test_result,res_full)
        }
        test_result <- res_full %>% 
          filter(p_val<0.05) %>% 
          mutate(admin_unit = suppressMessages(plyr::mapvalues(!!sym("strata"), from=dictionary$choice_name, to=dictionary$label_english)))
        
      }
      if(nrow(test_result)>0){
        
        concl <- test_result %>% group_by(variable) %>% summarise(admin_unit = paste0(admin_unit,collapse=', '))
        
        general_result <- 'Significant difference'
        variance_test_result_summary <- paste0('Relationship between ',dis_variable,' and ',concl$variable,
                                               ' is significant in the following stratas: ',
                                               unlist(concl$admin_unit), collapse ='\n'
        )
      }else{
        general_result <- 'Insignificant difference'
        variance_test_result_summary <-  "No significant overall relationship between two variables in any admin\n(doesn't exclude the possibility of significant subgroup differences)"
      }
      
      # we need at least 2 subgroups to compare between them, so if there aren't any - we move on to the next iteration of the loop
      if(length(big_admins)<2){
        
        res_total <- data.frame(variable = variable,
                                test_conclusion = general_result,
                                test_result = variance_test_result_summary,
                                test_result_det = 'Not enough significantly large subgroups to draw detailed conclusions about intergroup differences between specific subsamples',
                                disaggregations = dis_variable,
                                admin_used = "strata")
        
        res_total_full_mq <- rbind(res_total_full_mq,res_total)
        result <- HTML(paste0("There is a/an ",str_to_lower(res_total_full_mq[[2]])," between ", res_total_full_mq[[1]], " and ", dis_variable, ". ",br(),br(), res_total_full_mq$test_result_det,HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
        
      }else{
      if(func_used == 'select_multiple'){
        col_list <- apply(data_clean[,col_list],2,sum) %>% 
          data.frame() %>% 
          rownames_to_column(var = 'name') %>% 
          filter(`.`>min_sample_num) %>% 
          pull(name)
        
        
        if(length(col_list)==0){
          # output frame
          res_total <- data.frame(variable = variable,
                                  test_conclusion = general_result,
                                  test_result = variance_test_result_summary,
                                  test_result_det = 'Not enough significantly large subgroups to draw detailed conclusions about intergroup differences between specific subsamples',
                                  disaggregations = dis_variable,
                                  admin_used = "strata")
          
          res_total_full_mq <- rbind(res_total_full_mq,res_total)
          
          next
        }}
      
      
      # Post-hoc tests
      if(func_used=='mean'){
        post_hoc_test_result <-  data_clean %>% 
          group_by(!!sym("strata")) %>% 
          tukey_hsd(formula = eval(parse(text=txt)))%>% # join with the effect size table 
          ungroup()
      }else if (func_used=='select_multiple'){
        
        post_hoc_test_result = data.frame()
        
        for(variable in col_list){
          # this is done for each variable to ensure that we don't have cases where there's only 1 unique subgroup
          ls_admins2 <- data_clean %>% 
            dplyr::select(!!sym("strata"),!!sym(variable),!!sym(dis_variable)) %>% 
            group_by(!!sym("strata")) %>% 
            mutate(n = length(unique(!!sym(variable))),
                   n2 = length(unique(!!sym(dis_variable)))) %>% 
            ungroup() %>% 
            filter(n>1,
                   n2>1) %>% 
            pull(!!sym("strata")) %>% 
            unique()
          
          if(length(ls_admins2)==0){
            next
          }
          
          data_clean2 <- data_clean %>% filter(!!sym("strata") %in% ls_admins2)
          
          text2 <- paste0( variable,"~",  dis_variable)
          
          res_post_h <- data_clean2 %>% 
            group_by(!!sym("strata")) %>% 
            dunn_test( formula = eval(parse(text=text2)), p.adjust.method = 'bonferroni')%>% 
            ungroup()
          
          post_hoc_test_result = rbind(res_post_h,post_hoc_test_result) 
          
        }
        
      }else{
        post_hoc_test_result <-  data_clean %>% 
          group_by(!!sym("strata")) %>% 
          dunn_test(formula = eval(parse(text=txt)), p.adjust.method = 'holm')%>% # join with the effect size table 
          ungroup()
      }
      
      post_hoc_test_result <-  post_hoc_test_result %>% 
        # filter out subgroups with small differences, and small subsamples
        filter(p.adj<0.05) %>% 
        dplyr::rename(check_var=2) %>%  # get the text values of the result
        mutate(group1 = suppressMessages(plyr::mapvalues(group1, from=dictionary$choice_name, to=dictionary$label_english)),
               group2 = suppressMessages(plyr::mapvalues(group2, from=dictionary$choice_name, to=dictionary$label_english)),
               admin_unit = suppressMessages(plyr::mapvalues(!!sym("strata"), from=dictionary$choice_name, to=dictionary$label_english)),
               subroup = paste0(group1, " and ", group2))
      if (nrow(post_hoc_test_result) > 0){ 
        
        
        short_result <- post_hoc_test_result %>% group_by(check_var,subroup) %>% summarise(admin_unit = paste0(admin_unit, collapse= ', '))
        
        post_hoc_test_result_summary <- HTML(paste0('Significant differences in ',short_result$check_var, 
                                               ' between the following subgroups of ', dis_variable,': ', 
                                               short_result$subroup, ' in ',  short_result$admin_unit,br(),
                                               collapse = '\n'))
        post_hoc <- post_hoc_test_result_summary
      }else{
        post_hoc_test_result_summary <- 'No significant intergroup differences discovered in any stratas'
        post_hoc <- post_hoc_test_result_summary
      }
      
      # output frame
      res_total <- data.frame(variable = variable,
                              test_conclusion = general_result,
                              test_result = variance_test_result_summary,
                              test_result_det = post_hoc_test_result_summary,
                              disaggregations = dis_variable,
                              admin_used = "strata")
      
      res_total_full_mq <- rbind(res_total_full_mq,res_total)
      if(post_hoc == 'No significant intergroup differences discovered in any stratas'){
        result <- HTML(paste0("There is a/an ",str_to_lower(res_total_full_mq[[2]])," between ", res_total_full_mq[[1]], " and ",dis_variable, ". ",br(),br(), post_hoc,HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
      } else{
        result <- HTML(paste0("There is a/an ",str_to_lower(res_total_full_mq[[2]])," between ", res_total_full_mq[[1]], " and ",dis_variable, ". ",br(),br(),res_total_full_mq$test_result_det,HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
      }
      }
      return(result)
    }
    }else if(dis_variable != "No disaggregation" && admin == "overall") {
      dictionary <-   kobo_full %>% 
        filter(varname == dis_variable)
      txt <-  paste0(variable,"~",dis_variable)
      # get the function that needs to be applied 
      func_used <- entry$func
      n <- nrow(data.list[[sheet]])
      # clean na rows
      data_clean <- data.list[[sheet]] %>%
        mutate(uuid = rep(1:n)) %>% 
        filter(!is.na(!!sym(variable))) %>% 
        dplyr::select(uuid,!!sym(dis_variable),!!sym(variable))

      
      # get the list of large admin units for the future inter_group difference testing
      big_admins <- data_clean %>% 
        group_by(!!sym(dis_variable)) %>% 
        summarise(cnt = n()) %>% 
        filter(cnt >min_sample_num) %>% 
        pull(!!sym(dis_variable))
      # clean the data of small groups prior to running inter-group tests
      
      # If the thing that we're testing is a select multiple, we need to split the answers
      if(func_used == 'select_multiple'){
        
        data_clean <- tidyr::separate_rows(data_clean,!!sym(variable) , sep= "[ \n]")
        
        data_clean <- fastDummies::dummy_cols(data_clean, select_columns =variable )
        
        col_list <- setdiff(names(data_clean),c("uuid",dis_variable,variable))
        
        data_clean <- data_clean %>% 
          group_by(uuid) %>% 
          summarise(admin = unique(!!sym(dis_variable)),
                    across(col_list, ~sum(.x))
          ) %>% 
          ungroup() %>% 
          dplyr::select(-uuid)
        
        names(data_clean)[1] <- dis_variable
      }
      if(length(na.omit(unique(data_clean[[dis_variable]])))<2 | length(na.omit(unique(data_clean[[variable]])))<2){
        result <- HTML("All observations are in the same group.",      HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>"))
      }else{
      # the test itself
      if(func_used %in% c('mean','decimal','numeric','integer')){
        res <- anova_test(formula = eval(parse(text=txt)),data=data_clean) %>% # get the stats
          data.frame() %>% 
          mutate(variable = variable) %>% 
          dplyr::rename(p_val=p)%>% # rename the columns
          mutate(test_conclusion = ifelse(p_val <0.01 & `F`>5, ' Significant difference', 'Insignificant difference'), #classify significance (Use F for magnitude)
                 test_result = paste0('ANOVA test results: F coefficient: ',round(`F`,2),', p-val: ',round(p_val,3))) %>% 
          dplyr::select(variable,test_conclusion,test_result)
      }else if (func_used == 'select_one'){
        res <- kruskal_test(formula = eval(parse(text=txt)),data=data_clean)%>% 
          dplyr::rename(variable = 1,
                        p_val=p)%>% # rename the columns
          mutate(test_conclusion = ifelse(p_val <0.01 , ' Significant difference', 'Insignificant difference'), #classify significance
                 test_result = paste0('Kruskal test results: Chisq: ',round(statistic,2),', p-val: ',round(p_val,3))) %>% 
          dplyr::select(variable,test_conclusion,test_result) # select what you need 
      }else if (func_used == 'select_multiple'){
        res_full = data.frame()
        for(var in col_list){
          
          text2 <- paste0( var,"~", dis_variable)
          res <- kruskal_test(formula = eval(parse(text=text2)),data=data_clean)%>% 
            dplyr::rename(var = 1,
                          p_val=p)
          
          res_full = rbind(res,res_full)
        }
        
        res <- res_full%>% # rename the columns
          mutate(p_val = p.adjust(p_val),
                 test_conclusion = ifelse(p_val <0.01 , ' Significant difference', 'Insignificant difference'), #classify significance
                 test_result = paste0('Kruskal test results: Chisq: ',round(statistic,2),', p-val: ',round(p_val,3))) %>% 
          filter(!test_conclusion == 'Insignificant difference') %>% 
          dplyr::select(variable,test_conclusion,test_result) %>%  # select what you need 
          summarise(variable = paste0(variable, collapse = '\n'),
                    test_conclusion = paste0(test_conclusion, collapse = '\n'),
                    test_result = paste0(test_result, collapse = '\n')
          )
      }
      if(length(big_admins)<2){
        res_total <- res %>%
          mutate(test_result_det = 'Not enough significantly large subgroups to draw detailed conclusions about intergroup differences between specific subsamples',
                 disaggregations = NA,
                 admin_used = dis_variable)
        
        res_total_full <- rbind(res_total,res_total_full)
        result <- HTML(paste0("There is a/an ",str_to_lower(res_total_full[[2]])," between ", res_total_full[[1]], " and ", dis_variable, ". ",br(),br(), res_total_full$test_result_det,HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))

      } else{
      
      
        data_clean <- data_clean %>%
          filter(!!sym(dis_variable) %in% big_admins)
        if(func_used == 'select_multiple'){
          col_list <- apply(data_clean[,col_list],2,sum) %>%
            data.frame() %>%
            rownames_to_column(var = 'name') %>%
            filter(`.`>min_sample_num) %>%
            pull(name)
          
          
          if(length(col_list)==0){
            res_total <- res %>%
              mutate(test_result_det = 'Not enough significantly large subgroups to draw detailed conclusions about intergroup differences between specific subsamples',
                     disaggregations = NA,
                     admin_used = dis_variable)
            
            res_total_full <- rbind(res_total,res_total_full)
            next
          }}
        # post hoc test
        if(func_used%in%c('mean','integer','decimal','numeric')){
          post_hoc_test_result <-  data_clean %>% 
            tukey_hsd( formula = eval(parse(text=txt)))
        }else if (func_used=='select_multiple'){ 
          
          post_hoc_test_result = data.frame()
          for(var in col_list){
            
            text2 <- paste0( var,"~", dis_variable)
            res_post_h <- dunn_test( formula = eval(parse(text=text2)), p.adjust.method = 'bonferroni', data = data_clean)%>% 
              dplyr::rename(var = 1,
                            p_val=p)
            post_hoc_test_result = rbind(res_post_h,post_hoc_test_result)
          }
        }else{
          post_hoc_test_result <-  data_clean %>% 
            dunn_test( formula = eval(parse(text=txt)), p.adjust.method = 'holm')
        }
        
        
        post_hoc_test_result <-  post_hoc_test_result %>% 
          # filter out subgroups with small differences, and small subsamples
          filter(p.adj<0.01) %>% 
          rename(var=1) %>%  # get the text values of the result
          mutate(group1 = suppressMessages(plyr::mapvalues(group1, from=dictionary$choice_name, to=dictionary$label_english)),
                 group2 = suppressMessages(plyr::mapvalues(group2, from=dictionary$choice_name, to=dictionary$label_english))
          )
        # how many total significant combinations were we able to find
        total_differences <- nrow(post_hoc_test_result)
        
        # get top and bottom admins
        
        strata_frequency <- post_hoc_test_result %>% 
          tidyr::pivot_longer(group1:group2, names_to = 'group', values_to = 'admin') %>% 
          group_by(var,admin) %>% 
          summarise(Freq = n()) %>% 
          filter(Freq>1) %>% 
          arrange(desc(Freq)) %>% 
          ungroup() 
        if(nrow(strata_frequency)>1){
          # get max and put all into 1 sentence
          top_stratas <- strata_frequency%>% 
            group_by(var) %>% 
            do(head(.,3)) %>% 
            summarise(test_result_det = paste0(paste0(admin , collapse = ', '),' being different from ', paste0(Freq,collapse=', '),
                                               ' other categories, respectively.'))
          
          # get min and put all into 1 sentence
          bottom_stratas <- strata_frequency%>% 
            group_by(var) %>% 
            do(tail(.,3)) %>% 
            summarise(test_result_det = paste0(paste0(admin , collapse = ', '),' being different from ', 
                                               paste0(Freq,collapse=', '),
                                               ' other categories, respectively.'))
          
          # in cases of small samples, we'll have some intersections. They can be removed like so
          rows_to_remove <- intersect(top_stratas,bottom_stratas)
          
          top_stratas <- HTML(paste0(
            paste0(top_stratas %>% reframe(test_result_det = paste0('Top categories with the most differences from other ones in terms of ',var,' are: ',test_result_det)) %>% pull(test_result_det),collapse = '\n'),
            br(),br(),
            paste0(setdiff(bottom_stratas,rows_to_remove) %>%  reframe(test_result_det = paste0('Bottom Categories with the least differences from other ones in terms of ',var,' are: ',test_result_det)) %>% pull(test_result_det),collapse = '\n')
          ))
          post_hoc <- top_stratas
          
          
        }else if(nrow(strata_frequency)==1){
          top_stratas <- paste0(strata_frequency%>% 
                                  summarise(test_result_det = paste0('The only category with being significantly different in terms of ',var,' from others is: "',
                                                                     paste0(admin , collapse = ', '),'" it is different from ', paste0(Freq,collapse=', '),
                                                                     ' other categories.'))
          )
          post_hoc <- top_stratas
          
        }else{
          top_stratas <- paste0('Nothing significant to report between different categories of ',dis_variable,'.')
          post_hoc <- top_stratas
        }
        # get the detailed stats for the inter-admin differences
        res_total <- res %>% 
          mutate(test_result_det = paste0('The analysis allowed to find ',total_differences,' differences between categories'),
                 disaggregations = NA,
                 admin_used = dis_variable)
        res_total_full <- rbind(res_total,res_total_full)
        
        if(post_hoc == paste0('Nothing significant to report between different categories of ',dis_variable,'.')){
          result <- HTML(paste0("There is a/an ",str_to_lower(res_total[[2]])," between ", res_total[[1]], " and ",dis_variable, ". ",br(),br(), post_hoc,      HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
        } else{
          result <- HTML(paste0(res_total_full$test_result_det, br(), br(),"There is a/an ",str_to_lower(res_total[[2]])," between ", res_total[[1]], " and ",dis_variable, ". ",br(),br(), post_hoc,      HTML("<style> 
           #text_variance{
  background-color: #f0f0f08d;
  border: 3px solid rgba(238, 88, 89, .8);
  padding: 10px;
  border-radius: 5px;
  font-size: 16px;
  font-weight: bold;
  color: #333;
}</style>")))
        }
      }
      
        return(result)
      } ## AA_Fix break the function
      
    }
  })
  output$variance_title <- renderUI({
    if(entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])|
       input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
      HTML("")
    } else {
      HTML("<h3 class = 'title-message'>Variance</h3>")
    }
  })
  output$variance_icon <- renderUI({
    if(entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]]) |
       input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
      HTML("")
    } else{
      actionButton("popupBtn",icon("info-circle"))
    }
  })
  output$text_variance <- renderUI({
    req(input$variable,
        input$dis_variables,
        input$admin)
    if((entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])) |
       input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
      HTML("<h4 class = 'title-message' style = 'color: rgba(238, 88, 89, .8)'>Please ensure to have a column named strata or weight in your data</h4>")
    } else{
        variance()
      }
  })
  
  output$variance_sample <- renderUI({
    req(input$dis_variables)
    if((entry()$admin == "strata" & !"strata" %in% colnames(data.list()[[input$sheet]])) |
       input$weightBTN == "yes" & !"weight" %in% colnames(data.list()[[input$sheet]])){
    } else{
        if(input$dis_variables == "No disaggregation"){
          numericInput("min_sample_num", label = span(
            "Minimum sample number",
            span(
              `data-toggle` = "tooltip", `data-placement` = "right",
              title = "Minimum number of respondents per group analyzed, where the results can be considered significant",
              icon("info-circle")
            )
          ), step = 1, min = 1, value = 30)
        } else{
          n <- table() %>% 
            arrange(desc(num_samples)) %>% 
            head(2) %>% 
            pull(num_samples)
          numericInput("min_sample_num", label = span(
            "Minimum sample number",
            span(
              `data-toggle` = "tooltip", `data-placement` = "right",
              title = "Minimum number of respondents per group analyzed, where the results can be considered significant",
              icon("info-circle")
            )
          ), step = 1, min = 1, value = n[2] - 1)
        }
      }
  })
  observeEvent(input$popupBtn, {
    showModal(modalDialog(
      title = "Description of the Variance",
      HTML("The Variance Analysis tool helps users find significant differences among variables in tables generated by the Tabular Analysis tool. It does this through a two-step process:

<br><br> 1. Users select a variable and a disaggregation (or strata), and the tool performs a basic variance test to check for a significant relationship between them. For example, it can determine if education level influences income across the entire dataset.
<br><br> 2. It also checks for differences between subcategories if the disaggregation has more than two categories. For instance, it examines income differences among respondents with different education levels (e.g., graduate vs. post-graduate). 

<br><br>If users add a strata, the tool checks for significant relationships between the variable and disaggregation within each strata. If multiple choices were selected, tests are run separately for each choice and aggregated into a single text output."),
      footer = NULL,
      easyClose = TRUE
    ))
  })

}
#### FIX isolating errors


  
# Run the application 
shinyApp(ui = ui, server = server)