# Load R packages https://www.youtube.com/watch?v=tfN10IUX9Lo
library(shiny)
library(shinythemes)
library(conjoint)
library(ggplot2)
library(gridExtra)
# sets= data.frame(matrix(rep(0,36), ncol = 6))
# dm = data.frame(matrix(rep(0,36), ncol = 6))
# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  #theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Conjoint Analysis",
                  tabPanel("Instructions", "This online app is for research consultants of Appinio who want to run a study with a conjoint design.
                           On top of this page, you see the five required steps from Making Profiles to Analyzing Data.
                           Please always follow the steps in the right order, starting with 'Make profile subset' in step 1."),
                  tabPanel("1. Make profiles", "In this step, you enter the attributes of the product on the left side in a comma separated list. An example is already filled in. 
                           Then you can enter the levels of each attribute below (notice commas vs semicolons in example). Please stick with attributes/levels resulting in matrices not bigger than 5x4 or 4x5. Verify your inputs on the right and click that button, friend. ",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Names of attributes:", "Taste,  Inside, Price, Sprinkles"),
                             textAreaInput(inputId = "txt2",  label = "Names of levels:", value = "Chocolate, Strawberry, Vanilla, Spinach;Plastic cup, Paper cup, Waffle; 1$, 2$, 3$, 4$;Yes, No"),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("Chosen attributes and levels"),
                             tableOutput("table"),
                             actionButton("some", "Make profile subset"),downloadButton('downloadData', 'Download profiles'),
                             tableOutput("table2")
                             ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("2. Make sets", "If you clicked on 'Make profile subset' in the previous step, the profile sets should appear here automatically. A profile set is a group of multiple (often three) profiles of which participants select one. Below, you see these profiles next to each other with one row containing all the information for one choice set.
                           In the next step, you can design the choice sets for the participants.", 
                           
                           mainPanel(
                             h4("Choice sets"),downloadButton('downloadData2', 'Download choice sets'),
                             tableOutput("table3")

                           ), # mainPanel2
                           
                           ),
                  tabPanel("3. Make images", "Here you can alter the format of all the choice sets simultaneously and download them in a compressed folder. You can also download individual sets if they require unique formatting.",
                           sidebarPanel(
                             tags$h4("Aesthetics:"),
                             textInput("font_size_vals1", "Font size values 1", "5"),
                             textInput("font_size_vals2", "Font size values 2", "5"),
                             textInput("font_size_vals3", "Font size values 3", "5"),
                             textInput("font_size_keys1", "Font size keys 1", "4"),
                             textInput("font_size_keys2", "Font size keys 2", "4"),
                             textInput("font_size_keys3", "Font size keys 3", "4"),
                             textInput("bottom_buffer1", "Bottom buffer 1", "0"),
                             textInput("bottom_buffer2", "Bottom buffer 2", "0"),
                             textInput("bottom_buffer3", "Bottom buffer 3", "0"),
                             textInput("top_buffer1", "Top buffer 1", "0"),
                             textInput("top_buffer2", "Top buffer 2", "0"),
                             textInput("top_buffer3", "Top buffer 3", "0"),
                             textInput("left_buffer1", "Left buffer 1", "0"),
                             textInput("left_buffer2", "Left buffer 2", "0"),
                             textInput("left_buffer3", "Left buffer 3", "0"),
                             textInput("gap1", "Gap 1", "0"),
                             textInput("gap2", "Gap 2", "0"),
                             textInput("gap3", "Gap 3", "0"),
                           ),
                           mainPanel(
                             h4("Choice set images"),
                             textInput("set_number", "Choice set", "1"),

                             plotOutput("plot1", width = "600px", height = "300px"),
                             downloadButton(outputId = "singleset", label = "Download current set"),
                             downloadButton(outputId = "data_file", label = "Download all sets")
                             )
                           
                           ),
                  tabPanel("4. Collect data", "This step does not happen in this web app. You have to upload the generated images to the panel (through the admin).
                           Once all participants made their choices within the individual sets, you can reupload the data in Step 5."),
                  #https://www.surveyking.com/help/conjoint-analysis-explained
                  tabPanel("5. Analyze data", "This step is not implemented yet, because we have to first find a solution for conjoint designs in the admin. Below are some typical example outputs from other conjoint websites.",
                           
                           imageOutput("example"),
                           imageOutput("example2")),
                  tabPanel("Intro to conjoint", "Here, I will likely add a description of conjoint designs (what they are good for an how they work). For now, check out:",
                  tags$a(href="https://www.surveyking.com/help/conjoint-analysis-explained","Online explanation"))
                  
                  
                ) # navbarPage
) # fluidPage





# Define server function  
server <- function(input, output) {
  source('hanneshelpers.R')
  v = eventReactive(input$txt2, {
    attribute_names = strsplit(input$txt1, ",")
    nr_attributes = length(attribute_names[[1]])
    attribute_levels_strings = strsplit(input$txt2, ";")
    levels_vectors = list(); max_levels = 1
    for(i in 1:length(attribute_levels_strings[[1]])){
      level_list = strsplit(attribute_levels_strings[[1]][i], ",")#
      l = length(unlist(level_list))
      max_levels = max(max_levels, l)
      levels_vectors = c(levels_vectors, level_list)
    }
    dm <- data.frame(matrix(ncol = nr_attributes, nrow = max_levels))
    colnames(dm) <- attribute_names[[1]]
    for(i in 1:length(levels_vectors)){
      for(j in 1:length(levels_vectors[[i]])){
        dm[j,i] = levels_vectors[[i]][j]
      } }
    dm[is.na(dm)] = ""
    dm
  })
  output$table <- renderTable({
    v()
    })
    


  orth = eventReactive(input$some, {
    dm = v()
    dm[dm==''] = NA
    experiment<-expand.grid(dm)
    experiment=experiment[complete.cases(experiment),]
    withProgress(message = 'Finding optimal subset', value = 0.5, {
      caFactorialDesign(data=experiment,type="orthogonal", seed = 42)
    })
  })
 sets = eventReactive(input$some, {
    piles = mix_match(orth())
    pile1 = piles[[1]];pile2 = piles[[2]];pile3 = piles[[3]]
    colnames(pile1) = paste(colnames(pile1), "A", sep = "_"); colnames(pile2) = paste(colnames(pile2), "B", sep ="_");colnames(pile3) = paste(colnames(pile3), "C", sep ="_")
    sets = cbind(pile1,pile2,pile3)
    sets["Set"] = 1:nrow(sets)
    sets = sets[,c(ncol(sets),1:(ncol(sets)-1))]
    sets
  })
  
  
    output$table2 <- renderTable({
    Profile = 1:nrow(orth())
    cbind(Profile, orth())
  })
    
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste("profiles-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(orth(), file, row.names = F)
      })

    
    output$table3 <- renderTable({
      sets()
    })

    
    output$downloadData2 <- downloadHandler(
      filename = function() { 
        paste("choice sets-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(sets(), file, row.names = F)
      })
    
    output$plot1 = renderPlot({
      s = sets()
      s['Set'] = NULL
      s=s[,order(ncol(s):1)]
      
      colnames(s) = gsub('_A', '', colnames(s));colnames(s) = gsub('_B', '', colnames(s));colnames(s) = gsub('_C', '', colnames(s))
      aest1 <<- c("font_size_vals" = as.integer(input$font_size_vals1), "
                font_size_keys" = as.integer(input$font_size_keys1),
                "bottom_buffer" = as.integer(input$bottom_buffer1),
                "top_buffer" = as.integer(input$top_buffer1),
                "gap" = as.numeric(input$gap1),
                "left_buffer" = as.numeric(input$left_buffer1))
      
      aest2 <<- c("font_size_vals" = as.integer(input$font_size_vals2), 
                "font_size_keys" = as.integer(input$font_size_keys2),
                "bottom_buffer" = as.integer(input$bottom_buffer2),
                "top_buffer" = as.integer(input$top_buffer2),
                "gap" = as.numeric(input$gap2),
                "left_buffer" = as.numeric(input$left_buffer2))
      
      aest3 <<- c("font_size_vals" = as.integer(input$font_size_vals3), 
                "font_size_keys" = as.integer(input$font_size_keys3),
                "bottom_buffer" = as.integer(input$bottom_buffer3),
                "top_buffer" = as.integer(input$top_buffer3),
                "gap" = as.numeric(input$gap2),
                "left_buffer" = as.numeric(input$left_buffer3))
      
      set_plot <<-plot_set(s, as.integer(input$set_number), aest1, aest2, aest3)
      gridExtra::grid.arrange(
        set_plot[[1]], set_plot[[2]], set_plot[[3]], nrow = 1)
    })
    
    output$singleset <- downloadHandler(
      filename =  function() {
        paste("Choice set ", as.integer(input$set_number), ".png")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        s = sets()
        s['Set'] = NULL
        s=s[,order(ncol(s):1)]
        colnames(s) = gsub('_A', '', colnames(s));colnames(s) = gsub('_B', '', colnames(s));colnames(s) = gsub('_C', '', colnames(s))
        
      png(file, width=2, height=2, units = "cm", res = "700")
      a = plot_set(s, as.integer(input$set_number), aest1, aest2, aest3)[[1]]
      print(a)# turn the device off
      dev.off()  
      
      } 
    )
    
    output$data_file <- downloadHandler(
      filename = function() {
        paste("Choice sets", Sys.Date(),".tar", sep=" ")
      },
      content = function(fname) {
        fs <- c()
        tmpdir <- tempdir()
        setwd(tmpdir)
        
        for(set_number in 1:nrow(sets())) {
          for(profile in c(1,2,3)){
          path <- paste(set_number, LETTERS[profile], ".png", sep="")
          fs <- c(fs, path)
          s = sets()
          s['Set'] = NULL
          s=s[,order(ncol(s):1)]
          
          colnames(s) = gsub('_A', '', colnames(s));colnames(s) = gsub('_B', '', colnames(s));colnames(s) = gsub('_C', '', colnames(s))
          png(paste(set_number,LETTERS[profile], ".png", sep=""),   width=2, height=2, units = "cm", res = 700)
          p = plot_set(s, set_number, aest1, aest2, aest3)[[profile]]  
          print(p)
          dev.off()
        }}
        tar(fname, fs)
      },
      contentType = "application/tar"
    )
    
    
    output$example <- renderImage({
      list(src = "www/example.png", contentType = 'image/png',width = 400, height = 300,
           alt = "example")
    }, deleteFile = FALSE)
    output$example2 <- renderImage({
           list(src = "www/example2.PNG", contentType = 'image/png',width = 400, height = 300,
           alt = "example2")
    }, deleteFile = FALSE)

  } # server


# Create Shiny object
shinyApp(ui = ui, server = server)