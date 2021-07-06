#TO-DO LIST
#image_labels -- done
#PNG in ADDITION TO JPG -- done
#dpi variable for lower resolutuon pics -- done
#insert images as attributes in step 1 -- done
#plot_set to plot_profile  -- done
#better isntructions  -- partly done
#loading progress bar for stimuli plots
#better aesthetics

#BIG-EXTENSION LIST
#automatic linking between attributes and decorative pictures
#possibility to show 2 instead of 3 profiles
#ranking conjoint

library(jpeg)
library(png)
library(grid)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(conjoint)
library(ggplot2)
library(gridExtra)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                #navbarpage includes all tabpanels of the UI
                navbarPage(
                    "Conjoint Analysis",
                    
                    #Instruction panel
                    tabPanel("Instructions", "This online app is for research consultants of Appinio who want to run a study with a conjoint design.
                             On top of this page, you see the five required steps from Making Profiles to Analyzing Data.
                             Please always follow the steps in the right order, starting with 'Make profile subset' in step 1. 
                             If you have questions, contact Hannes on Slack."
                             ),
                    
                    #panel for user inputting attributes and levels
                    tabPanel("1. Make profiles", "In this step, you enter the attributes of the product on the left side in a comma separated list. An example is already filled in. 
                             Then you can enter the levels of each attribute below (notice commas vs semicolons in example). Please stick with attributes/levels resulting in matrices not bigger than 5x4 or 4x5. Verify your inputs on the right and click that button, friend. ",
                             tags$br(),
                             sidebarPanel(
                               tags$h3("Input:"),
                               textInput("txt1", "Names of attributes:", "Attractiveness, Smiling, Job, Politik, Hobby"),
                               textAreaInput(inputId = "txt2",  label = "Names of levels:", value = "High, Low; Yes, No; Berater, Pfleger, Arbeitslos, Maler; Eher links,  Eher mittig, Eher rechts; Schwimmen, Tanzen, Lesen, Keine"),
                               actionButton("testimages", "Test images"),
                               actionButton("some", "Make profile subset", class = "btn-primary"),
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
                               
                               tableOutput("table2")
                                      ) 
                            ),
                    
                    #panel for inspecting the choice sets
                    tabPanel("2. Make sets", "If you clicked on 'Make profile subset' in the previous step, the profile sets should appear here automatically. A profile set is a group of multiple (often three) profiles of which participants select one. Below, you see these profiles next to each other with one row containing all the information for one choice set.
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
                             The download of sets will include a csv file with the analysis codes that will be required in step 5. Careful: Image decorations entered in this step only make profiles look nicer for participants. They are not analyzed in step 5.",
                             tags$br(),
                             sidebarPanel(
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
                             fileInput(inputId = "admindf", label = "Upload raw data from admin", multiple = FALSE, accept = c(".csv")),

                             #display the columnselector and startbutton only when admindata was provided
                             conditionalPanel(
                               condition = "output.adminnames_provided",
                               uiOutput("columnselector"),
                               actionButton("analysisstart", "Start analyses")                           
                                              ),
                            #plots and outputs
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("importanceplot") ,  plotOutput("utilityplot")),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tableOutput("bestprofiles"),
                            tableOutput("worstprofiles")
                            ),
                    #last panel for extar info, FAQs etc.
                    tabPanel("Intro to conjoint", "Here, I will likely add a description of conjoint designs (what they are good for an how they work). For now, check out:",
                            tags$a(href="https://www.surveyking.com/help/conjoint-analysis-explained","Online explanation")
                            )#end of tabpanels
                          )#end of navbarPage
)#end of UI / fluidPage






#server 
server <- function(input, output) {
  
  #hanneshelpers contains 4 custom functions:
  #1.[resample_without_creating_duplicates] shuffles the order of choices within orthogonal piles of profiles
  #2.[mix_match] creates additional piles from initial orthogonal subset and calls function 1 to create choice sets
  #3.[plot_set] plots a choice set with profiles next to each other
  #4.[importance_utility_ranking] conducts bayesian multilevel MNL regression and outputs plots etc.
  source('hanneshelpers.R')
  
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
  v = eventReactive(c(input$txt2,input$txt1, input$testimages, input$label_pic1, input$label_pic2, input$label_pic3, input$label_pic4, input$label_pic5, input$img1, input$img2, input$img3, input$img4, input$img5),{
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
    dm
})
  
  #Panel 2, outputs user inputs
  output$table <- renderTable({v()})

  #Panel 2; generates orthogonal subset using conjoint package
  orth = eventReactive(input$some, {
    dm = v()
    dm[dm==''] = NA
    experiment<-expand.grid(dm)
    experiment=experiment[complete.cases(experiment),]
    withProgress(message = 'Finding optimal subset', value = 0.5, {
      caFactorialDesign(data=experiment,type="orthogonal", seed = 42)})
                                    })
  #Panel 2; outputs orthogonal profiles
  output$table2 <- renderTable({
    Profile = 1:nrow(orth())
    cbind(Profile, orth())})
  
  #Panel 2-3; creates choice sets
  sets = eventReactive(input$some, {
    piles = mix_match(orth())
    pile1 = piles[[1]];pile2 = piles[[2]];pile3 = piles[[3]]
    colnames(pile1) = paste(colnames(pile1), "a", sep = "_"); colnames(pile2) = paste(colnames(pile2), "b", sep ="_");colnames(pile3) = paste(colnames(pile3), "c", sep ="_")
    sets = cbind(pile1,pile2,pile3)
    sets["Set"] = 1:nrow(sets)
    sets = sets[,c(ncol(sets),1:(ncol(sets)-1))]
    sets})

    #Panel 3; outputs choices sets
    output$table3 <- renderTable({
      sets()
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
      s = sets()
      #imgs = imgpaths()

      set_plot = plot_set(s, as.integer(input$set_number), 1, aests()[[1]], decorpath, input$none_text, imgpaths())

      set_plot[[1]]
      }, width =200, height = 300)
    
    #Panel 4; plot profile B
    output$plot2 = renderPlot({
      if(isTruthy(input$imgprof2)){decorpath = input$imgprof2$datapath
      }else{decorpath = NA}
      s = sets()
      set_plot = plot_set(s, as.integer(input$set_number), 2, aests()[[2]], decorpath, input$none_text, imgpaths())
      set_plot[[1]]
    }, width =200, height = 300)
    
    #Panel 4; plot profile C
    output$plot3 = renderPlot({
      if(isTruthy(input$imgprof3)){decorpath = input$imgprof3$datapath
      }else{decorpath = NA}
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
        fs <- c()
        tmpdir <- tempdir()
        setwd(tmpdir)
        s = sets()
        withProgress(message = 'Making images...', value = 0.5, {
          if(input$incl_none){
            attr_names = colnames(s)[grepl("_a", colnames(s))]
            new_col_names = gsub("_a", "_d",attr_names)
            s[new_col_names] = "None"
            path <- "none.png"
            fs <- c(fs, path)
            p = plot_set(s, 1, 1, aests()[[1]], NA, input$none_text, imgpaths())[[2]]  
            ggsave(plot = p, file= path, height =9, width =6, units = "cm", dpi = as.numeric(input$resolut))
          }
          path = "ANALYSES CODES DO NOT DELETE.csv"
          fs <- c(fs, path)
          write.csv(s, path, row.names = F)

          for(profile in c(1,2,3)){
            #super ugly, try pasting profile to variable name
            # path1 = input$imgprof1$datapath
            # path2 = input$imgprof2$datapath
            # path3 = input$imgprof3$datapath
            # decorpath = get(paste0(path, profile))
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
            s[new_col_names] = "None"
            path <- "none.png"
            fs <- c(fs, path)
            p = plot_set(s, 1, 1, aests()[[profile]], NA, input$none_text, imgpaths())[[2]]  
            ggsave(plot = p, file= path, height =9, width =6, units = "cm", dpi = as.numeric(input$resolut))
          }
          path = "ANALYSES CODES DO NOT DELETE.csv"
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
    
    #Panel 5, boolean check if data was provided
    output$adminnames_provided <- reactive({
      isTruthy(input$admindf) & isTruthy(input$keydf)})
    
    #Panel 5; select columns from the admin dataframe
    output$columnselector = renderUI({
      admin = data.table::fread(input$admindf$datapath, data.table = FALSE)
      colnames(admin) = sapply(colnames(admin), function(x){substr(x, 1, min(12, nchar(x)))})
      checkboxGroupInput("columns","Select columns containing profile choices",choices=colnames(admin),inline = T)})
    
    #Panel 5, carry out analysis and return plots, insights of interest
    analysis <- eventReactive(input$analysisstart, {
      withProgress(message = 'Analyzing...', value = 0.5, {
        print("ooh")
        key = data.table::fread(input$keydf$datapath, data.table = FALSE)
        admin = data.table::fread(input$admindf$datapath, data.table = FALSE)
        colnames(admin) = sapply(colnames(admin), function(x){substr(x, 1, min(12, nchar(x)))})
        admin = admin[, input$columns]
        print(dim(admin))
        r = importance_utility_ranking(df = admin,key = key, nr_profiles = 3, none_option = FALSE)})
        r})

    #Panel 5; importance plot
    output$importanceplot = renderPlot({
      req(input$analysisstart)
      analysis()[[1]]})
    
    #Panel 5; utility plot
    output$utilityplot = renderPlot({
      req(input$analysisstart)
      analysis()[[2]]})
    
    #Panel 5; utility top ranking
    output$bestprofiles <- renderTable({head(analysis()[[3]], 7)})
    
    #Panel 5; utility bottom ranking
    output$worstprofiles <- renderTable({tail(analysis()[[3]], 7)})

    #only run when element is shown
    outputOptions(output, 'adminnames_provided', suspendWhenHidden = FALSE)
    outputOptions(output, 'testimgs_provided', suspendWhenHidden = FALSE)
} #end  server

shinyApp(ui = ui, server = server)