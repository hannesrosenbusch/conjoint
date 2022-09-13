#TO-DO LIST
#Code cleaning and refactoring
#identify efficiency leaks
#Check explanations and instructions (Pete)
#possibility to compare groups/segments

#BIG-EXTENSION LIST
#automatic linking between attributes and decorative pictures
#possibility to show 2 instead of 3 profiles
#ranking conjoint

library(jpeg)
library(png)
library(grid)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tibble)
library(DT)
library(ggplot2)
library(gridExtra)
library(conjoint)
library(DoE.base)
library(flextable)
library(officer)
library(rvg)
library(openxlsx)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                tags$style(type="text/css", "body {padding-top: 65px;}"),
                
                #navbarpage includes all tabpanels of the UI
                navbarPage(
                  "Conjoint Analysis",
                  position = "fixed-top",
                  
                  #Instruction panel
                  tabPanel("Instructions", useShinyjs() ,"This online app is for research consultants of Appinio who want to run a study with a conjoint design.
                             On top of this page, you see the five required steps from Making Profiles to Analyzing Data.
                             Please always follow the steps in the right order, starting with 'Make profile subset' in step 1. 
                             If you have questions, contact Hannes on Slack.",
                           
                           
                           #authentication https://towardsdatascience.com/r-shiny-authentication-incl-demo-app-a599b86c54f7
                           # tags$div(
                           #   id = "login-basic", 
                           #   style = "width: 500px; max-width: 100%; margin: 0 auto;",
                           #   
                           #   tags$div(
                           #     class = "well",
                           #     h4(class = "text-center", "Please login"),
                           #     p(class = "text-center", 
                           #       tags$small("First approach login form")
                           #     ),
                           #     
                           #     textInput(
                           #       inputId     = "ti_user_name_basic", 
                           #       label       = tagList(icon("user"), 
                           #                             "User Name"),
                           #       placeholder = "Enter user name"
                           #     ),
                           #     
                           #     passwordInput(
                           #       inputId     = "ti_password_basic", 
                           #       label       = tagList(icon("unlock-alt"), 
                           #                             "Password"), 
                           #       placeholder = "Enter password"
                           #     ), 
                           #     
                           #     tags$div(
                           #       class = "text-center",
                           #       actionButton(
                           #         inputId = "ab_login_button_basic", 
                           #         label = "Log in",
                           #         class = "btn-primary"
                           #       )
                           #     )
                           #   )
                           # ),
                           tags$br(),
                           tags$br(),
                           tags$br(),
                           textOutput("login_feedback"),
                           
                           
                  ),
                  
                  #panel for user inputting attributes and levels
                  tabPanel("1. Make profiles", "In this step, you enter the attributes of the product on the left side in a comma separated list. An example is already filled in. 
                             Then you can enter the levels of each attribute below (notice commas vs semicolons in example). Please stick with attributes/levels resulting in matrices not bigger than 5x4 or 4x5. Verify your inputs on the right and click that button, friend. ",
                           tags$br(),
                           tags$br(),
                           sidebarPanel(style = "position: left; height: 580px; overflow-y:scroll",
                                        tags$h3("Input:"),
                                        textInput("txt1", "Names of attributes:", "Flavor, Price, Topping"),
                                        textAreaInput(inputId = "txt2",  label = "Names of levels:", value = " Strawberry, Vanilla, Kiwi; 1.50€, 2.00€, 2.50€; Brittle, Caramel, Sprinkles", height = "100px"),
                                        actionButton("testimages", "Test images"),
                                        actionButton("some", "Confirm design", class = "btn-primary", style = "float:right"),
                                        tags$br(),
                                        tags$br(),
                                        
                                        #optional images
                                        conditionalPanel(
                                          condition = "output.testimgs_provided",
                                          textInput("label_pic1", "Image 1 tag", "Logo1"),
                                          fileInput(inputId = "img1", label = "Image 1", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                          textInput("label_pic2", "Image 2 tag", "Logo2"),
                                          fileInput(inputId = "img2", label = "Image 2", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                          textInput("label_pic3", "Image 3 tag", NA),
                                          fileInput(inputId = "img3", label = "Image 3", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                          textInput("label_pic4", "Image 4 tag", NA),
                                          fileInput(inputId = "img4", label = "Image 4", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                          textInput("label_pic5", "Image 5 tag", NA),
                                          fileInput(inputId = "img5", label = "Image 5", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                        )
                           ), 
                           mainPanel(
                             h4("Chosen attributes and levels"),
                             tableOutput("table"),
                             tags$br(),
                             tags$br(),
                             
                             textOutput("designmessage"), 
                             tags$br(),
                             tags$br(),
                             tableOutput("betterdesigns"),
                             tags$br(),
                             tags$br(),
                             textOutput("confirmationtext")
                           ) 
                  ),
                  
                  #panel for inspecting the choice sets
                  tabPanel("2. Make sets", "If you clicked on 'Confirm design' in the previous step, the profile sets should appear here automatically. A profile set is a group of multiple (often three) profiles of which participants select one. Below, you see these profiles next to each other with one row containing all the information for one choice set.
                             In the next step, you can design the choice sets for the participants.", 
                           mainPanel(
                             h4("Choice sets"),
                             tableOutput("table3")
                           ),
                  ),
                  
                  #panel for making the image stimuli for the admin
                  #Aesthetics input very awkward still
                  tabPanel("3. Make stimuli", "Here you can alter the format of all the choice sets simultaneously and download them in a compressed folder. 
                             You can also download individual sets if they require unique formatting. 
                             The download of sets will include a csv file with the analysis codes that will be required in step 5. Careful: Image decorations entered in this step only make profiles look nicer for participants. They are not analyzable in step 5.",
                           tags$br(),
                           tags$br(),
                           sidebarPanel(style = "position: left; height: 580px; overflow-y:scroll",
                                        tags$h4("Aesthetics:"),
                                        
                                        #optional images
                                        fileInput(inputId = "imgprof1", label = "Image decoration A", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                        fileInput(inputId = "imgprof2", label = "Image decoration B", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                        fileInput(inputId = "imgprof3", label = "Image decoration C", multiple = FALSE, accept = c(".png", ".jpg", ".jpeg")),
                                        
                                        #aesthetics
                                        textAreaInput("none_text", "'None' text", "None of these"),
                                        
                                        textInput("bottom_pic1", "Picture bottom 1", "3.5"),
                                        textInput("left_pic1", "Picture left 1", "-Inf"),
                                        textInput("right_pic1", "Picture right 1", "Inf"),
                                        textInput("top_pic1", "Picture top 1", "Inf"),
                                        textInput("bottom_pic2", "Picture bottom 2", "3.5"),
                                        textInput("left_pic2", "Picture left 2", "-Inf"),
                                        textInput("right_pic2", "Picture right 2", "Inf"),
                                        textInput("top_pic2", "Picture top 2", "Inf"),
                                        textInput("bottom_pic3", "Picture bottom 3", "3.5"),
                                        textInput("left_pic3", "Picture left 3", "-Inf"),
                                        textInput("right_pic3", "Picture right 3", "Inf"),
                                        textInput("top_pic3", "Picture top 3", "Inf"),
                                        textInput("font_size_vals1", "Font size values 1", "5"),
                                        textInput("font_size_vals2", "Font size values 2", "5"),
                                        textInput("font_size_vals3", "Font size values 3", "5"),
                                        textInput("font_size_keys1", "Font size keys 1", "3"),
                                        textInput("font_size_keys2", "Font size keys 2", "3"),
                                        textInput("font_size_keys3", "Font size keys 3", "3"),
                                        textInput("bottom_buffer1", "Bottom buffer 1", "-1"),
                                        textInput("bottom_buffer2", "Bottom buffer 2", "-1"),
                                        textInput("bottom_buffer3", "Bottom buffer 3", "-1"),
                                        textInput("top_buffer1", "Top buffer 1", "2"),
                                        textInput("top_buffer2", "Top buffer 2", "2"),
                                        textInput("top_buffer3", "Top buffer 3", "2"),
                                        textInput("left_buffer1", "Left buffer 1", "0"),
                                        textInput("left_buffer2", "Left buffer 2", "0"),
                                        textInput("left_buffer3", "Left buffer 3", "0"),
                                        textInput("gap1", "Gap 1", "0"),
                                        textInput("gap2", "Gap 2", "0"),
                                        textInput("gap3", "Gap 3", "0"),
                                        textInput("resolut", "Resolution of output (dpi)", "400"),
                           ),
                           
                           #main panel for displaying the stimuli
                           mainPanel(
                             #h4("Choice set images"),
                             splitLayout(cellWidths = c("25%", "25%", "25%","25%"), plotOutput("plot1"),plotOutput("plot2"),plotOutput("plot3"),plotOutput("none_plot")),
                             splitLayout(cellWidths = c("75%", "25%"), textInput("set_number", "Choice set", "1"), switchInput("incl_none", label = "'None' option")),
                             downloadButton(outputId = "data_file", label = "Download sets"),
                             downloadButton(outputId = "singleset", label = "Download current set")
                           )
                  ),
                  #simple reminder panel that the actual data is collected in the admin
                  tabPanel("4. Collect data", "This step does not happen in this web app. You have to upload the generated images to the panel through the admin.
                            Use the question type 'Picture Gallery' and input the images 1A, 1B, 1C (and maybe the 'none' option) to the first question. In the answer options next to the images simply repeat the filename of the respective image (1A, 1B, 1C, and None).
                            This is important for the automatic data analysis in the next step. When you finished programming the study in the admin, please contact Hannes on Slack for a quick check. We will abandon this safety check once we are sure that conjoint analyses and this web app work smoothly.
                             Once all participants made their choices within the individual choice sets, you can reupload the data in Step 5."),
                  
                  #panel for uploading the data and receiving analysis outputs
                  tabPanel("5. Analyze data",
                           fileInput(inputId = "keydf", label = "Upload analyses codes (downloaded during step 3)", multiple = FALSE, accept = c(".csv")),
                           fileInput(inputId = "admindf", label = "Upload raw csv data from analyzer (not mapping version)", multiple = FALSE, accept = c(".csv")),
                           
                           
                           
                           
                           #display the columnselector and startbutton only when admindata was provided
                           conditionalPanel(
                             condition = "output.adminnames_provided",
                             uiOutput("columnselector"),
                             actionButton("analysisstart", "Start analyses")                           
                           ),
                           #plots and outputs
                           tags$br(),
                           tags$br(),
                           tags$br(),                            
                           
                           splitLayout(cellWidths = c("70%", "30%"), plotOutput("importanceplot") ,  DTOutput("importancetable")),#pending importance comparisons
                           tags$br(),
                           tags$br(),
                           tags$br(),                            
                           tags$br(),
                           tags$br(),                           
                           tags$br(),
                           tags$br(),                            
                           tags$br(),
                           splitLayout(cellWidths = c("70%", "30%"),plotOutput("utilityplot") ,  DTOutput("comparisontable")),
                           tags$br(),
                           tags$br(),
                           tags$br(),                            
                           tags$br(),
                           tags$br(),                           
                           tags$br(),
                           tags$br(),                            
                           tags$br(),
                           
                           splitLayout(cellWidths = c("70%", "30%"), DTOutput("utilitytable"),  plotOutput("market_pie")),
                           tags$br(),
                           downloadButton(outputId = "marketsDL", label = "Download Market Simulations"),
                           downloadButton(outputId = "pptxdownload", label = "Download PPTX"),
                           downloadButton(outputId = "xlsxdownload", label = "Download Utilities for Excel")
                  ),
                  
                  tabPanel("6. Pricing",
                           sidebarLayout(
                             sidebarPanel(
                               
                               selectInput(inputId = "priceselect",
                                           label = "Price",
                                           choices = c("Nil"),
                                           multiple = FALSE
                               ),
                               
                               actionBttn("priceanalysis", label = "Calculate", style = "jelly", size = "sm", color = "primary" )
                             ),
                             
                             mainPanel(
                               textOutput("infotext"),
                               DT::dataTableOutput("text")
                             ) # end main panel
                           ) # end sidebarLayout
                  ), # end tabPanel 6
                  #last panel for extar info, FAQs etc.
                  tabPanel("Intro to conjoint", "Here, I will likely add a description of conjoint designs (what they are good for an how they work). For now, check out:",
                           tags$a(href="https://www.surveyking.com/help/conjoint-analysis-explained","Online explanation"),
                           tags$br(),
                           HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/hdMulkVCR-E" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                           
                  )#end of tabpanels
                )#end of navbarPage
)#end of UI / fluidPage






#server 
server <- function(input, output, session) {
  set.seed(42)
  
  #hanneshelpers contains 5 custom functions:
  #1.[resample_without_creating_duplicates] shuffles the order of choices within orthogonal piles of profiles
  #2.[mix_match] creates additional piles from initial orthogonal subset and calls function 1 to create choice sets
  #3.[plot_set] plots a choice set with profiles next to each other
  #4.[importance_utility_ranking] conducts bayesian multilevel MNL regression and outputs plots etc.
  #5. [cust_choicemodelr] helperfunction for 4. which can deal with missing value designs
  
  source('hanneshelpers.R')
  load("all_designchecks.RData")
  
  
  user_base_basic_tbl <- tibble(
    user_name = "appinio",
    password  = "7umxJgk2Cd"
  )
  validate_password_basic <- eventReactive(input$ab_login_button_basic, {
    validate <- FALSE
    if (input$ti_user_name_basic == user_base_basic_tbl$user_name &&
        input$ti_password_basic == user_base_basic_tbl$password )
    {validate <- TRUE}
  })
  observeEvent(validate_password_basic(), {
    shinyjs::hide(id = "login-basic")
  })
  
  #login feedback
  feedback <- eventReactive(input$ab_login_button_basic, {
    req(input$ti_user_name_basic)
    req(input$ti_password_basic )
    if (input$ti_user_name_basic == user_base_basic_tbl$user_name &&
        input$ti_password_basic == user_base_basic_tbl$password ){
      "Login succeeded"
    }else{"Login failed"}
  })
  output$login_feedback <- renderText({
    feedback()})
  
  
  
  
  #Panel 2; imgpaths for reading in images in later panels
  output$testimgs_provided <- reactive({input$testimages %% 2 == 1})
  imgpaths <- reactive({
    req(input$img1$datapath)
    imgpaths = c(input$img1$datapath, input$img2$datapath, input$img3$datapath, input$img4$datapath, input$img5$datapath)
    imglabels = c(input$label_pic1,input$label_pic2,input$label_pic3,input$label_pic4, input$label_pic5)
    names(imgpaths) = imglabels[0:length(imgpaths)]
    imgpaths
  })
  
  #Panel 2; generates the dataframe that shows the user their inputs 
  v = eventReactive(c(validate_password_basic, input$txt2,input$txt1, input$testimages, input$label_pic1, input$label_pic2, input$label_pic3, input$label_pic4, input$label_pic5, input$img1, input$img2, input$img3, input$img4, input$img5),{
    
    
    req(validate_password_basic())
    
    
    attribute_names = strsplit(input$txt1, ",")
    nr_attributes = length(attribute_names[[1]])
    attribute_levels_strings = strsplit(input$txt2, ";")
    levels_vectors = list(); max_levels = 1
    for(i in 1:length(attribute_levels_strings[[1]])){
      level_list = strsplit(attribute_levels_strings[[1]][i], ",")
      l = length(unlist(level_list))
      max_levels = max(max_levels, l)
      levels_vectors = c(levels_vectors, level_list)}
    dm <- data.frame(matrix(ncol = nr_attributes, nrow = max_levels))
    colnames(dm) <- attribute_names[[1]]
    for(i in 1:length(levels_vectors)){
      for(j in 1:length(levels_vectors[[i]])){
        dm[j,i] = levels_vectors[[i]][j]
      } }
    if((input$testimages %% 2) == 1){
      Image = c(input$label_pic1, input$label_pic2, input$label_pic3,input$label_pic4,input$label_pic5)
      length(Image) = nrow(dm)
      dm = cbind(Image, dm)
    }
    dm[is.na(dm)] = ""
    colnames(dm) = trimws(colnames(dm))
    dm
  })
  
  #Panel 2, outputs user inputs
  output$table = renderTable({v()})
  
  
  #Panel 2, display hint how well current design works
  output$designmessage = renderText({
    des = current_and_alternative_designs(v())
    des[[3]]
  })
  
  #Panel 2, display current, and potentially better, designs
  output$betterdesigns = renderTable({
    des = current_and_alternative_designs(v())
    out = des[[2]]
    if(des[[3]] == "Good design!"){out = des[[1]]#out[des[[1]][1, "Design"],]
    }else if(des[[3]] == "Larger, optimized designs available!"){out = rbind(des[[1]],des[[2]]); out = out[!duplicated(out$Design),]
    }else {return(NULL)}
    out
  })
  
  #Panel 2; generates orthogonal subset
  orth = eventReactive(input$some, {
    set.seed(42)
    dm = v()
    dm = dm[,order(colSums(dm==""), decreasing = T)]
    dm_list = as.list(dm)
    dm_list = removeListElemComplete(dm_list, "")
    ddd = oa.design(factor.names = dm_list, columns = "min3", seed = 42)
    colnames(ddd) = gsub("X.", "", colnames(ddd) )
    ddd = as.data.frame(ddd)
    cur_des = paste( (nrow(dm) - colSums(dm=="")), collapse = "x")
    temp = all_designchecks[names(all_designchecks) == cur_des]
    achievable_nr_sets = temp[which.min(unlist(temp))]
    if(nrow(ddd) > achievable_nr_sets){ 
      dm[dm==''] = NA
      experiment<-expand.grid(dm)
      experiment=experiment[complete.cases(experiment),]
      ddd = caFactorialDesign(data=experiment,type="orthogonal", seed = 42)
    }
    ddd
  })
  
  #Panel 2; alert that user may continue
  output$confirmationtext = renderText({req(sets())
    "Please continue to the next step ('2. Make sets')"})
  
  #Panel 2-3; creates choice sets
  sets = eventReactive(input$some, {
    set.seed(42)
    withProgress(message = 'Finding optimal subset', value = 0.25, {
      temp = orth()
      incProgress(0.5)
      piles = mix_match(temp)
      pile1 = piles[[1]][colnames(v())];pile2 = piles[[2]][colnames(v())];pile3 = piles[[3]][colnames(v())]
      colnames(pile1) = paste(colnames(pile1), "a", sep = "_"); colnames(pile2) = paste(colnames(pile2), "b", sep ="_");colnames(pile3) = paste(colnames(pile3), "c", sep ="_")
      sets = cbind(pile1,pile2,pile3)
      sets["Set"] = 1:nrow(sets)
      incProgress(0.25)
    })
    sets})
  
  #Panel 3; outputs choices sets
  output$table3 <- renderTable({
    set.seed(42)
    s = sets()
    s$Set = NULL
    nr_profiles = 3
    nr_attributes = ncol(s)/nr_profiles
    colnames(s) = gsub("_a", "", rep(colnames(s)[1:nr_attributes], nr_profiles))
    
    temp = rbind(s[,1:nr_attributes], s[,(nr_attributes+1):(2*nr_attributes)], s[,(2*nr_attributes +1):(3*nr_attributes)])
    Set = sort(rep(1:nrow(s), 3))
    temp = cbind (Set, temp)
    temp 
  }) 
  
  #Panel 4; collect chosen aesthetics
  aests = reactive({aest1 = c("font_size_vals" = as.integer(input$font_size_vals1), 
                              "font_size_keys" = as.integer(input$font_size_keys1),
                              "bottom_buffer" = as.integer(input$bottom_buffer1),
                              "top_buffer" = as.integer(input$top_buffer1),
                              "gap" = as.numeric(input$gap1),
                              "left_buffer" = as.numeric(input$left_buffer1),
                              "left_pic"= as.numeric(input$left_pic1),
                              "right_pic"= as.numeric(input$right_pic1),
                              "bottom_pic"= as.numeric(input$bottom_pic1),
                              "top_pic"= as.numeric(input$top_pic1))
  aest2 = c("font_size_vals" = as.integer(input$font_size_vals2), 
            "font_size_keys" = as.integer(input$font_size_keys2),
            "bottom_buffer" = as.integer(input$bottom_buffer2),
            "top_buffer" = as.integer(input$top_buffer2),
            "gap" = as.numeric(input$gap2),
            "left_buffer" = as.numeric(input$left_buffer2),
            "left_pic"= as.numeric(input$left_pic2),
            "right_pic"= as.numeric(input$right_pic2),
            "bottom_pic"= as.numeric(input$bottom_pic2),
            "top_pic"= as.numeric(input$top_pic2))
  aest3 = c("font_size_vals" = as.integer(input$font_size_vals3), 
            "font_size_keys" = as.integer(input$font_size_keys3),
            "bottom_buffer" = as.integer(input$bottom_buffer3),
            "top_buffer" = as.integer(input$top_buffer3),
            "gap" = as.numeric(input$gap2),
            "left_buffer" = as.numeric(input$left_buffer3),
            "left_pic"= as.numeric(input$left_pic3),
            "right_pic"= as.numeric(input$right_pic3),
            "bottom_pic"= as.numeric(input$bottom_pic3),
            "top_pic"= as.numeric(input$top_pic3))
  list(aest1, aest2, aest3)})
  
  #Panel 4; plot profile A
  output$plot1 = renderPlot({
    if(isTruthy(input$imgprof1)){decorpath = input$imgprof1$datapath
    }else{decorpath = NA}
    set.seed(42)
    s = sets()
    set_plot = plot_set(s, as.integer(input$set_number), 1, aests()[[1]], decorpath, input$none_text, imgpaths())
    set_plot[[1]]
  }, width =200, height = 300)
  
  #Panel 4; plot profile B
  output$plot2 = renderPlot({
    if(isTruthy(input$imgprof2)){decorpath = input$imgprof2$datapath
    }else{decorpath = NA}
    set.seed(42)
    s = sets()
    set_plot = plot_set(s, as.integer(input$set_number), 2, aests()[[2]], decorpath, input$none_text, imgpaths())
    set_plot[[1]]
  }, width =200, height = 300)
  
  #Panel 4; plot profile C
  output$plot3 = renderPlot({
    if(isTruthy(input$imgprof3)){decorpath = input$imgprof3$datapath
    }else{decorpath = NA}
    set.seed(42)
    s = sets()
    set_plot = plot_set(s, as.integer(input$set_number), 3, aests()[[3]], decorpath, input$none_text, imgpaths())
    set_plot[[1]]
  }, width =200, height = 300)
  
  #Panel 4; none plot
  output$none_plot = renderPlot({
    if(!input$incl_none){return(NULL)}
    decorpath = NA
    s = sets()
    set_plot = plot_set(s, as.integer(input$set_number), 1, aests()[[1]], decorpath, input$none_text, imgpaths())
    set_plot[[2]]
  }, width =200, height = 300)
  
  #Panel 4; download single set (useful for set specific aethetics or pics)
  output$singleset <- downloadHandler(
    filename = function() {
      paste("Choice set", input$set_number, Sys.Date(),".tar", sep=" ")},
    content = function(fname) {
      set.seed(42)
      fs <- c()
      tmpdir <- tempdir()
      setwd(tmpdir)
      s = sets()
      withProgress(message = 'Making images...', value = 0.5, {
        if(input$incl_none){
          attr_names = colnames(s)[grepl("_a", colnames(s))]
          new_col_names = gsub("_a", "_d",attr_names)
          # removed "s[new_col_names] = "None""
          path <- "none.png"
          fs <- c(fs, path)
          p = plot_set(s, 1, 1, aests()[[1]], NA, input$none_text, imgpaths())[[2]]  
          ggsave(plot = p, file= path, height =9, width =6, units = "cm", dpi = as.numeric(input$resolut))
        }
        path = paste0("AnalysesCodes_", format(Sys.time(), "%d.%b %H:%M"), ".csv")
        fs <- c(fs, path)
        write.csv(s, path, row.names = F)
        
        for(profile in c(1,2,3)){
          if(profile == 1 & isTruthy(input$imgprof1)){decorpath = input$imgprof1$datapath
          }else if(profile == 2 & isTruthy(input$imgprof2)){decorpath = input$imgprof2$datapath
          }else if(profile == 3 & isTruthy(input$imgprof3)){decorpath = input$imgprof3$datapath
          }else{decorpath = NA}
          
          path <- paste(input$set_number, letters[profile], ".png", sep="")
          fs <- c(fs, path)
          s = sets()
          p = plot_set(s, as.numeric(input$set_number), profile, aests()[[profile]], decorpath, input$none_text, imgpaths())[[1]]  
          ggsave(plot = p, file= path, height =9, width =6, units = "cm", dpi = as.numeric(input$resolut))
        }})
      tar(fname, fs)},
    contentType = "application/tar")
  
  #Panel 4, download all sets
  output$data_file <- downloadHandler(
    filename = function() {
      paste("Choice sets", Sys.Date(),".tar", sep=" ")},
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tmpdir)
      s = sets()
      withProgress(message = 'Making images...', value = 0.5, {
        if(input$incl_none){
          attr_names = colnames(s)[grepl("_a", colnames(s))]
          new_col_names = gsub("_a", "_d",attr_names)
          # removed "s[new_col_names] = "None""
          path <- "none.png"
          fs <- c(fs, path)
          p = plot_set(s, 1, 1, aests()[[1]], NA, input$none_text, imgpaths())[[2]]  
          ggsave(plot = p, file= path, height =9, width =6, units = "cm", dpi = as.numeric(input$resolut))
        }
        path = paste0("AnalysesCodes_", format(Sys.time(), "%d.%b %H:%M"), ".csv")
        fs <- c(fs, path)
        write.csv(s, path, row.names = F)
        
        for(set_number in 1:nrow(sets())) {
          for(profile in c(1,2,3)){
            if(profile == 1 & isTruthy(input$imgprof1)){decorpath = input$imgprof1$datapath
            }else if(profile == 2 & isTruthy(input$imgprof2)){decorpath = input$imgprof2$datapath
            }else if(profile == 3 & isTruthy(input$imgprof3)){decorpath = input$imgprof3$datapath
            }else{decorpath = NA}  
            path <- paste(set_number, letters[profile], ".png", sep="")
            fs <- c(fs, path)
            s = sets()
            p = plot_set(s, set_number, profile, aests()[[profile]], decorpath, input$none_text, imgpaths())[[1]]  
            ggsave(plot = p, file= path, height =9, width =6, units = "cm", dpi = as.numeric(input$resolut))}}
      })
      
      tar(fname, fs)},
    contentType = "application/tar")
  
  #Panel 6, boolean check if data was provided
  output$adminnames_provided <- reactive({
    isTruthy(input$admindf) & isTruthy(input$keydf)})
  
  #Panel 6; select columns from the admin dataframe
  output$columnselector = renderUI({
    admin = data.table::fread(input$admindf$datapath, data.table = FALSE)
    colnames(admin) = sapply(colnames(admin), function(x){substr(x, 1, min(12, nchar(x)))})
    checkboxGroupInput("columns","Select columns containing profile choices",choices=colnames(admin),inline = T)})
  
  #Panel 6, carry out analysis and return plots, insights of interest
  analysis <- eventReactive(input$analysisstart, {
    withProgress(message = 'Analyzing... (takes a few minutes)', value = 0.5, {
      key = data.table::fread(input$keydf$datapath, data.table = FALSE)
      admin = data.table::fread(input$admindf$datapath, data.table = FALSE)
      colnames(admin) = sapply(colnames(admin), function(x){substr(x, 1, min(12, nchar(x)))})
      admin = admin[, input$columns]
      print(head(admin))
      #print('')
      #print('')
      print(head(key))
      r = importance_utility_ranking(df = admin,key = key, nr_profiles = 3, none_option = FALSE)})
    r})
  
  #Panel 6; importance plot
  output$importanceplot = renderPlot({
    req(input$analysisstart)
    analysis()[[1]]})
  
  #Panel 6; utility plot
  output$utilityplot = renderPlot({
    req(input$analysisstart)
    analysis()[[2]]})
  
  output$comparisontable <- renderDT({ 
    analysis()[[6]]    
  }, options = list(pageLength = 8))  
  
  
  output$importancetable <- renderDT({ 
    analysis()[[7]]    
  }, options = list(pageLength = 8))   
  
  
  output$utilitytable <- renderDT({ 
    analysis()[[3]]    
  }, options = list(pageLength = 8, dom = 't'), selection =  list(selected =c(2,5,6)), filter = 'top',rownames = F)   
  
  
  selected_rows = reactive({input$utilitytable_rows_selected})
  
  
  output$market_pie = renderPlot({
    req(input$utilitytable_rows_selected)
    selected_profiles = analysis()[[3]][as.integer(selected_rows()),]
    individuals_betas = analysis()[[4]]
    plotting_df = analysis()[[5]]
    market_simulator(selected_profiles, individuals_betas, plotting_df)})
  
  
  #download many market simulations
  output$marketsDL <- downloadHandler(
    filename = function() {
      paste0("market_simulations", ".xlsx")
    },
    content = function(file) {
      all_profiles = analysis()[[3]]
      individuals_betas = analysis()[[4]]
      plotting_df = analysis()[[5]]
      output_matrix = c("Nr_products", "Included_products", paste0("Product", 1:nrow(all_profiles)))
      withProgress(message = 'Simulating many markets', value = 0.5,{
      for(market_size in c(1,2)){
        print(market_size)
        product_combi_indeces = t(combn(1:nrow(all_profiles), market_size))
      for(i in 1:nrow(product_combi_indeces)){
        product_combo = product_combi_indeces[i,]
        
        # print(product_combo)
        selected_profiles = all_profiles[product_combo,]
        # print("start")
        market_shares = market_simulator_full(selected_profiles, individuals_betas, plotting_df, product_combo, all_profiles)
        # print(market_shares)
        # print(dim(market_shares))
      output_matrix = rbind(output_matrix, c(length(product_combo), paste(product_combo, collapse = "&"), market_shares))
      # print("end")
      # print("")
      }
      }

      #concatenate
      })
      wb = createWorkbook()
      addWorksheet(wb, "Markets")
      writeData(wb,"Markets", output_matrix)
      addWorksheet(wb, "Products")
      #colnames(all_profiles)[colnames(all_profiles == "Rank")] = "Product"
      writeData(wb,"Products", all_profiles)
      saveWorkbook(wb,file)
    }
  )

  
  
  # Download Plots as PPTX
  
  output$pptxdownload <- downloadHandler(
    filename = function() {
      "Conjoint_Results.pptx"
    },
    content = function(file) {
      
      pp_top10_utils <- flextable(head(analysis()[[3]], 10)) %>% #    pp_top10_utils <- flextable(head(analysis()[[3]], 10)) %>% 
        colformat_num(col_keys = c("Rank", "Utility"), digits = 0) %>%
        width(width = 1.25) %>%
        height_all(height = 0.35) %>%
        theme_zebra() %>%
        align(align = "center", part = "all")
      
      pp_bot10_utils <- flextable(tail(analysis()[[3]], 10))  %>% 
        colformat_num(col_keys = c("Rank", "Utility"), digits = 0) %>%
        width(width = 1.25) %>%
        height_all(height = 0.35) %>%
        theme_zebra() %>%
        align(align = "center", part = "all")
      
      pl1 = reactive({analysis()[[1]]})
      imp_pl <- rvg::dml(ggobj = pl1())
      pl2 = reactive({analysis()[[2]]})
      u_pl <- rvg::dml(ggobj = pl2())
      
      
      pp_download <- read_pptx() %>%
        add_slide(layout = "Title and Content") %>%
        
        ph_with(
          location = ph_location_type(type = "body"),
          value = imp_pl 
        ) %>%
        
        add_slide(layout = "Title and Content") %>%
        ph_with(
          location = ph_location_type(type = "body"),
          value = u_pl
          #   
        ) %>%
        
        add_slide(layout = "Title and Content") %>%
        
        ph_with(
          location = ph_location_type(type = "title"),
          value = "Top 10 Profiles"
        ) %>%
        
        ph_with(
          location = ph_location_type(type = "body"),
          value = pp_top10_utils
        ) %>%
        
        add_slide(layout = "Title and Content") %>%
        
        ph_with(
          location = ph_location_type(type = "title"),
          value = "Bottom 10 Profiles"
        ) %>%
        
        ph_with(
          location = ph_location_type(type = "body"),
          value = pp_bot10_utils
        )
      
      print(pp_download, target = file)
    }
  )
  
  # Download Utilities for Excel
  output$xlsxdownload <- downloadHandler(
    filename = function() {
      paste0("results", ".xlsx")
    },
    content = function(file) {
      wb = createWorkbook()
      addWorksheet(wb, "Sheet1")
      writeData(wb,"Sheet1", analysis()[[3]])
      
      saveWorkbook(wb,file)
    }
  )
  # Updating the SelectInputs from Panel 6 when starting Analysis
  
  observeEvent(input$analysisstart, {
    
    plotting_df = analysis()[[5]]
    updateSelectInput(session, "priceselect", label = "Select Price", choices = unique(plotting_df$all_attributes))
  })
  
  # Calculates the Worth of 1 Utility on selecting the Price
  
  rea_val <- reactiveVal(NULL)
  
  observeEvent(input$priceselect, {
    
    plotting_df = analysis()[[5]]
    
    
    df_price <- subset(plotting_df, all_attributes == input$priceselect)
    df_price$betas <- as.numeric(df_price$betas)
    df_price$all_levels <- as.numeric(gsub("[^[:digit:].,]","",(df_price$all_levels)))
    
    validate(
      need(sum(is.na(df_price$all_levels)) == 0, "Variable needs to be numeric")
    )
    
    model <- lm(data = df_price, all_levels~betas)
    
    inc_per_util <- model$coefficients['betas']*(-1)
    
    rea_val(inc_per_util)
    
    print(rea_val())
  })
  
  #    here starts the price analysis part:
  #    - read the required data and drop the price attribute
  #    - goal is a datatable that displays all comparisons among the attribute levels and the price increases
  
  observeEvent(input$priceanalysis, {
    print(1)
    pricing = analysis()[[5]]
    print(class(pricing))
    pricing = subset(pricing, all_attributes != "None") #Just a quick fix
    pricing = subset(pricing, all_attributes != input$priceselect)
    print(2)
    # this part creates a list, loops through all attributes and stores the combinations and price increases in it
    lister_the_tormentor = list()
    
    for(j in unique(pricing$all_attributes)) {
      
      df <- subset(pricing, all_attributes == j)
      df$all_levels <- trimws(df$all_levels)
      df$worth <- df$betas*rea_val()
      df <- df[order(df$worth, decreasing = TRUE),]
      print("loop")
      df_lists_comb <- expand(df,
                              nesting(all_attributes, all_levels, worth),
                              nesting(feature_2 = all_levels, b = worth))
      
      df_lists_comb <- df_lists_comb[order(df_lists_comb$worth, decreasing = TRUE),]
      
      df_lists_comb <- subset(df_lists_comb, all_levels != feature_2)
      
      # this loop drops all duplicated pairs while keeping the levels sorted from highest to lowest utility
      df_lists_comb$my_duplicated <- FALSE
      for(i in 1:nrow(df_lists_comb)){
        if(df_lists_comb$my_duplicated[i] == FALSE){
          my_test <- df_lists_comb$feature_2 %in% df_lists_comb$all_levels[i] & df_lists_comb$all_levels %in% df_lists_comb$feature_2[i]
          df_lists_comb$my_duplicated[my_test] <- TRUE
          
        }
      }
      df_lists_comb <- df_lists_comb[!df_lists_comb$my_duplicated,]
      df_lists_comb$my_duplicated <- NULL
      
      lister_the_tormentor[[(j)]] <- df_lists_comb
      
    }
    print(3)
    df_table <- do.call(rbind, lister_the_tormentor)
    df_table$price_increase <- round(df_table[3] - df_table[5], 2)
    df_table <- df_table[ , -which(names(df_table) %in% c("worth", "b"))]
    
    df_table <- plyr::rename(df_table,
                             c("all_levels" = "feature_1", 
                               "all_attributes" = "attribute"))
    
    output$infotext <- renderText({"Read the table from left to right: feature_1 compared to feature_2 is a price difference of xx. In these comparisons, feature_1 is always the more popular feature!"})
    
    output$text <- DT::renderDataTable(
      DT::datatable(
        {df_table},
        extensions = 'Buttons',
        
        options = list(
          paging = FALSE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'tBf',
          buttons = c('copy', 'csv', 'excel')
        ),
        
        class = "display")
    )
  })
  #only run when element is shown
  outputOptions(output, 'adminnames_provided', suspendWhenHidden = FALSE)
  outputOptions(output, 'testimgs_provided', suspendWhenHidden = FALSE)
} #end  server

shinyApp(ui = ui, server = server)