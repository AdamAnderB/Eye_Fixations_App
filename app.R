# Define UI for application that draws a histogram
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(shinyWidgets)
library(tidyr)

#load data
actual_distance=.3


#"#F7FBFF"
#build a GUI
ui <- navbarPage("TheArtOfWrangling: Removal of Eye Fixations", 
                 setBackgroundColor(
                   color = c("#E0E0E0","#E0E0E0"),
                   gradient = "linear",
                   direction = "bottom" ),
                 sidebarPanel(
                   p("PLEASE WAIT: DATA IS BEING WRANGLED IN REAL TIME"),
                   p(style = "font-style: italic; font-weight: bold;",
                     "Citation: Bramlett, A. A. & Wiener, S. (2024). ",
                     tags$em("The Art of Wrangling: Working with Web-based Eye-tracking Data in Language Research. "),
                     "Linguistic Approaches to Bilingualism."),
                   p("Input for all visualizations and dataframes is reactive (they change with the sliders). 
                     The visuals aim to help the reader build an understanding of the relationship between 
                     removing data and retaining the signal."),
                   p("White boxes represent the actual borders of the screen and the visual stimuli of 
                     the 2x2 VWP design."),
                   p("Preset loading standards are the same as those used in the Art of Wrangling."),
                   #p("•Red verticle lines indicate the approximate beginning and end of sentences."),
                   #p("•Blue verticle lines indicate the time of interest. (-400 to 800 with 0 being verb offset)"),
                   #p(tags$a(href="https://www.andrew.cmu.edu/user/swiener/lapp/", "See our lab here")),
                   #p(tags$a(href="https://www.researchgate.net/publication/369090199_An_eye-tracking_replication_without_an_eye_tracker_Capturing_predictive_sentence_processing_of_accented_speech_via_the_internet
                   #", "See the poster here")),
                   #p(tags$a(href="https://www.adamabramlett.com/", "See Adam's personal site here")),
                   #sliderInput("bin_size", label = h3("Bin size"), min = 10, max = 300, value = c(0, 60)),
                   sliderInput("distance", label = h3("Removal from center in normalised sizing"), min = 0, max = actual_distance, value = 0),
                   p("Notice how increasing the internal removal has huge implications for how much data is removed. 0 is no removal, .3 is maximal removal"),
                   sliderInput("beyond_screen", label = h3("Removal from outside screen in normalised sizing"), min = -1, max = 0, value = -.5),
                   p("Notice how increasing the outside removal has a very small effect on how much data is removed until close to the borders of the actual image. -1 is minimal removal, 0 is maximal removal"),
                 ),
                 tabPanel("Visualizations",mainPanel(
                   plotOutput("plot", brush = "plot_brush", dblclick = "plot_reset"),
                   plotOutput("plot2", brush = "plot_brush", dblclick = "plot_reset"),
                   plotOutput("plot3", brush = "plot_brush", dblclick = "plot_reset")
                 )),
                 #tabPanel("other visuals"),
                 navbarMenu("Data",
                            tabPanel("All data (post-wrangling-raw)",
                                     DTOutput(outputId = "tabledata")
                            ),
                            tabPanel("Individual Participant Looks",
                                     DTOutput(outputId = "part_looks_data")
                            )
                 ),
)
#build server
server <- function(input, output, session) {
  data <- reactive({
    read.csv("shiny_binning_data.csv",
             header=TRUE, row.names=1)
  })
  output$tabledata <- renderDT(data())
  center=.5
  time_start = -500
  time_end = 900
  time_bin_size<-400
  actual_screen=.5
  
  #data manipulation
  #looks
  part_looks <- reactive({
    data()%>%
      mutate(image_viewing=case_when(x_pred_normalised <= center-input$distance & 
                                       y_pred_normalised >= center+input$distance ~ image_1,
                                     x_pred_normalised >= center+input$distance & 
                                       y_pred_normalised >= center+input$distance ~ image_2,
                                     x_pred_normalised <= center-input$distance & 
                                       y_pred_normalised <= center-input$distance ~ image_3,
                                     x_pred_normalised >= center+input$distance & 
                                       y_pred_normalised <= center-input$distance ~ image_4))%>%
      filter(!is.na(image_viewing))%>%
      mutate(target = if_else(image_viewing == img_1_file, 1, 0), 
             comp_1 = if_else(image_viewing == img_2_file, 1, 0), 
             comp_2 = if_else(image_viewing == img_3_file, 1, 0), 
             dist = if_else(image_viewing == img_4_file, 1, 0))%>%
      filter(x_pred_normalised>center-abs(input$beyond_screen)-.5&
               x_pred_normalised<center+abs(input$beyond_screen)+.5&
               y_pred_normalised>center-abs(input$beyond_screen)-.5&
               y_pred_normalised<center+abs(input$beyond_screen)+.5)%>%
      mutate(time_elapsed=time_elapsed-object_start-200)%>% mutate(time_elapsed_rounded=time_bin_size*round((time_elapsed)/time_bin_size))%>%
      filter(time_elapsed_rounded>time_start &time_elapsed_rounded<time_end)%>%
      mutate(object_viewing = if_else(target == 1,"Target",
                                      if_else(comp_1 == 1,"Competitors",
                                              if_else(comp_2 == 1,"Competitors",
                                                      if_else(dist == 1,"Competitors","")))))
  })
  standard_looks <- reactive({
    data()%>%
      mutate(time_elapsed=time_elapsed-object_start-200)%>% mutate(time_elapsed_rounded=time_bin_size*round((time_elapsed)/time_bin_size))%>%
      filter(time_elapsed_rounded>time_start &time_elapsed_rounded<time_end)
  })
  
  
  
  #creates table for viewing
  output$part_looks_data <- renderDT(part_looks())
  
  variable<- reactive({(nrow(standard_looks())-nrow(part_looks()))/nrow(standard_looks())})
  
  #for agg dataset 
  df <- reactive({
    data.frame(matrix(ncol = 3, nrow = 0))
  })
  
  #plot 1
  output$plot <- renderPlot({
    part_looks()%>%
      ggplot(aes(x=x_pred_normalised,y=y_pred_normalised,color = as.factor(object_viewing)))+
      stat_density_2d(geom = "raster",
                      aes(fill = after_stat(density)),
                      contour = FALSE)+
      labs(title="Eye Fixations Across Adjusted Time: Each Window is 400ms",
           subtitle="0 is the start of restrictive/unrestrictive verbs")+
      #geom_point(alpha=.1,size=3)+
      #scale_fill_viridis_c(option="inferno")+
      scale_fill_gradient(low = "white", high = "#C41230")+
      annotate("rect", 
               xmin = center-actual_screen, xmax = center+actual_screen, 
               ymin = center-actual_screen, ymax = center+actual_screen
               ,alpha = 0, color= "white")+
      annotate("rect", 
               xmin = center-actual_screen, xmax = center-actual_distance, 
               ymin = center+actual_distance, ymax = center+actual_screen,
               alpha = 0, color= "white")+
      annotate("rect", 
               xmin = center+actual_distance, xmax = center+actual_screen, 
               ymin = center-actual_screen, ymax = center-actual_distance,
               alpha = 0, color= "white")+
      annotate("rect", 
               xmin = center-actual_screen, xmax = center-actual_distance, 
               ymin = center-actual_screen, ymax = center-actual_distance,
               alpha = 0, color= "white")+
      annotate("rect", 
               xmin = center+actual_screen, xmax = center+actual_distance, 
               ymin = center+actual_distance, ymax = center+actual_screen,
               alpha = 0, color= "white")+
      facet_grid(as.factor(object_viewing)~time_elapsed_rounded)+
      theme_minimal()+
      theme(legend.position="none")+
      ylab("Screen height - Y axis")+
      xlab("Screen width - X axis")+
      scale_y_continuous(breaks =c(0,.5,1))+
      scale_x_continuous(breaks =c(0,.5,1))+
      theme(panel.spacing = unit(0, "cm"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.text.x = element_text(face="bold", color="#C41230", 
                                       size=14, angle=0),
            axis.text.y = element_text(face="bold", color="#C41230", 
                                       size=14, angle=0))
  }, res = 96)
  output$plot2 <- renderPlot({
    ggplot(data=df())+
      geom_errorbar(aes(xmin = 0, xmax = 1,y=1),color="#009647")+
      geom_errorbar(aes(xmin = 0, xmax = variable(),y=1),color="#C41230")+
      scale_y_continuous(limits=c(-2,2),breaks=NULL)+
      theme_minimal()+
      ggtitle("Approximately ",paste(round(variable()*100,2),"% data loss",sep = ""))+
      ylab("")+
      xlab("")+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x=element_blank())
    
  }, res = 96)
}
shinyApp(ui, server)



