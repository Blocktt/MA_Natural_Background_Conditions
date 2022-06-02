#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# import data ####
df_4shiny_streams<-read.csv("WQ_Data/MA_WQ_Streams_StreamCat_Wide_20220504.csv")
df_4shiny_streams <- df_4shiny_streams %>%
  mutate(DistCat2 = case_when((DistCat_trim == "Ref") ~ "Ref"
                              , (DistCat_trim != "Ref") ~ "Non_Ref"))

df_4shiny_lakes<-read.csv("WQ_Data/MA_WQ_Lakes_4RShiny_20220602.csv")

# Server ####
shinyServer(function(input, output, session) {
  # modal dialog ####
  myModal <- modalDialog(
    title = "Greetings!"
    ,paste("Welcome to the MA Natural Background Condition Data Explorer (v0.0.1.9000)!")
    ,br()
    ,paste("This R Shiny app is in support of the project: Freshwater Natural"
           ,"Background Condition Determinations and Related Improvements to "
           ,"Surface Water Quality Assessment Procedures for Use Attainment "
           ,"Decisions in Massachusetts."
           ,"Currently the app includes stream and lake water chemistry and StreamCat/LakeCat data for exploratory"
           ,"analyses. The functionality of the app will changes as the project progresses."
           ,"This app was developed by Ben Block (Ben.Block@tetratech.com),"
           ,"Please contact Ben Block should any issues or questions arise.")
    ,easyClose = T)

  # Show the model on start up
  showModal(myModal)

  # Stream tab ####
  ## Define data ####
  myData <- reactive({
    df_4shiny_streams %>%
      select(WaterbodyID, COMID_Final, Outlier_COMID, DistCat_trim,
             DistCat2, input$waterchem, input$streamcat) %>%
      filter(complete.cases(.))
  }) # reactive
  
  ## Stats ####
  output$stats <- renderText({
    
    df_4stats <- myData()
    # get number of observations
    myCount <- nrow(df_4stats)
    myRefCount <- sum(df_4stats$DistCat2 == "Ref")
    
    df_4corr <- df_4stats %>% 
      select(input$waterchem, input$streamcat)
    
    corr_res <- cor(df_4corr, use = "complete.obs", method = "spearman")
    
    corr_res_final <- round(corr_res[1,2],2)
    
    return(paste0("<b># Ref Sites: ", myRefCount, "<br>","# Total Sites: "
                 ,myCount, "<br>", "Spearman rho: ",corr_res_final ,"</b>"))
    
  }) # renderPrint

  # create color scales
  dist_pal <- c("Ref" = "#0570b0"
                ,"Other" = "#cccccc"
                ,"Strs" = "#d7191c"
                ,"Unknown" = "#000000")
  
  dist2_pal  <- c("Ref" = "#0570b0"
                  ,"Non_Ref" = "#000000")
  
  outlier_pal <- c("Yes" = "#1a9641"
                   , "No" = "#cccccc")
  
  ## Plot 1 ####
  output$plot1 <- renderPlot({
    
    # filter data
    plot1_data <- myData()
    plot1_data$DistCat_trim <- as.factor(plot1_data$DistCat_trim)
    
    # create plot
    ggplot(data = plot1_data, aes(x = (.data[[input$streamcat]])
                                  , y = (.data[[input$waterchem]])))+
      geom_point(aes(color = DistCat_trim), size = 3, shape = 19)+
      scale_color_manual(values = dist_pal)+
      scale_y_log10()+
      scale_x_log10()+
      labs(x = paste0(input$streamcat)
           , y = paste0(input$waterchem)
           , color = "Ref Status")+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))
    }) # render Plot1
  
  ## Plot 2 ####
  # output$plot2 <- renderPlot({
  #   
  #   # filter data
  #   plot2_data <- myData()
  #   plot2_data$Outlier_COMID <- as.factor(plot2_data$Outlier_COMID)
  #   
  #   # create plot
  #   ggplot(data = plot2_data, aes(x = (.data[[input$streamcat]])
  #                                 , y = (.data[[input$waterchem]])))+
  #     geom_point(aes(color = Outlier_COMID), size = 3, shape = 19)+
  #     scale_color_manual(values = outlier_pal)+
  #     scale_y_log10()+
  #     scale_x_log10()+
  #     labs(x = paste0(input$streamcat)
  #          , y = ""
  #          , color = "Outlier Catchment")+
  #     theme(text = element_text(size = 14),
  #           axis.text = element_text(color = "black", size = 14),
  #           axis.text.x = element_text(angle = 0, hjust = 0.5),
  #           panel.background = element_rect(fill = "white"),
  #           panel.grid.major.y = element_blank(),
  #           panel.grid.major.x = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           axis.line = element_line(color = "black"),
  #           legend.position = "top",
  #           legend.text=element_text(size = 14))
  # }) # render Plot2
  
  ## Plot 3 ####
  output$plot3 <- renderPlot({
    
    # filter data
    plot3_data <- myData()
    
    # create plot
    ggplot(data = plot3_data, aes(x = (.data[[input$waterchem]])
                                  , color = DistCat2))+
      stat_ecdf(geom = "point", pad = FALSE, size = 2)+
      scale_x_log10()+
      scale_color_manual(values = dist2_pal)+
      labs(y = "Cumulative Percent"
           , x = input$waterchem
           , color = "Ref Status")+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))
    
  }) # render Plot3
  
  ## Plot 4 ####
  # output$plot4 <- renderPlot({
  #   
  #   # filter data
  #   plot4_data <- myData()
  #   
  #   # create plot
  #   ggplot(data = plot4_data, aes(x = (.data[[input$waterchem]])
  #                                 , color = Outlier_COMID))+
  #     stat_ecdf(geom = "point", pad = FALSE, size = 2)+
  #     scale_x_log10()+
  #     scale_color_manual(values = outlier_pal)+
  #     labs(y = "Cumulative Percent"
  #          , x = input$waterchem
  #          , color = "Outlier Catchment")+
  #     theme(text = element_text(size = 14),
  #           axis.text = element_text(color = "black", size = 14),
  #           axis.text.x = element_text(angle = 0, hjust = 0.5),
  #           panel.background = element_rect(fill = "white"),
  #           panel.grid.major.y = element_blank(),
  #           panel.grid.major.x = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           axis.line = element_line(color = "black"),
  #           legend.position = "top",
  #           legend.text=element_text(size = 14))
  #   
  # }) # render Plot4
  
  ## Plot 5 ####
  output$plot5 <- renderPlot({
    
    # filter data
    plot5_data <- myData()
    
    # create plot
    ggplot(data = plot5_data, aes(x = (.data[[input$streamcat]])
                                  , color = DistCat2))+
      stat_ecdf(geom = "point", pad = FALSE, size = 2)+
      scale_x_log10()+
      scale_color_manual(values = dist2_pal)+
      labs(y = "Cumulative Percent"
           , x = input$streamcat
           , color = "Ref Status")+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))
    
  }) # render Plot5
  
  ## Table 1 ####
  output$table1 <- renderDT({
    
    table1_data <- myData()
    table1_trim <- table1_data %>% 
      select(WaterbodyID, DistCat_trim, DistCat2, input$waterchem
             , input$streamcat) %>% 
      rename(DistCat3 = DistCat_trim)
    
    return(table1_trim)
    
  }) # render Table1
  
  # Lake tab ####
  ## Define data ####
  myData_lakes <- reactive({
    df_4shiny_lakes %>%
      select(WaterbodyID, DistCat_trim, DistCat2, input$waterchem_lakes
             , input$lakecat) %>%
      filter(complete.cases(.))
  }) # reactive
  
  ## Stats ####
  output$stats_lakes <- renderText({
    
    df_4stats_lakes <- myData_lakes()
    # get number of observations
    myCount_lakes <- nrow(df_4stats_lakes)
    myRefCount_lakes <- sum(df_4stats_lakes$DistCat2 == "Ref")
    
    df_4corr_lakes <- df_4stats_lakes %>% 
      select(input$waterchem_lakes, input$lakecat)
    
    corr_res_lakes <- cor(df_4corr_lakes, use = "complete.obs", method = "spearman")
    
    corr_res_lakes_final <- round(corr_res_lakes[1,2],2)
    
    return(paste0("<b># Ref Lakes: ", myRefCount_lakes, "<br>","# Total Lakes: "
                  ,myCount_lakes, "<br>", "Spearman rho: ",corr_res_lakes_final ,"</b>"))
    
  }) # renderPrint
  
  ## Plot 6 ####
  output$plot6 <- renderPlot({
    
    # filter data
    plot6_data <- myData_lakes()
    plot6_data$DistCat_trim <- as.factor(plot6_data$DistCat_trim)
    
    # create plot
    ggplot(data = plot6_data, aes(x = (.data[[input$lakecat]])
                                  , y = (.data[[input$waterchem_lakes]])))+
      geom_point(aes(color = DistCat_trim), size = 3, shape = 19)+
      scale_color_manual(values = dist_pal)+
      scale_y_log10()+
      scale_x_log10()+
      labs(x = paste0(input$lakecat)
           , y = paste0(input$waterchem_lakes)
           , color = "Ref Status")+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))
  }) # render Plot6
  
  ## Plot 7 ####
  output$plot7 <- renderPlot({

    # filter data
    plot7_data <- myData_lakes()

    # create plot
    ggplot(data = plot7_data, aes(x = (.data[[input$waterchem_lakes]])
                                  , color = DistCat2))+
      stat_ecdf(geom = "point", pad = FALSE, size = 2)+
      scale_x_log10()+
      scale_color_manual(values = dist2_pal)+
      labs(y = "Cumulative Percent"
           , x = input$waterchem_lakes
           , color = "Ref Status")+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))

  }) # render Plot7
  
  ## Plot 8 ####
  output$plot8 <- renderPlot({

    # filter data
    plot8_data <- myData_lakes()

    # create plot
    ggplot(data = plot8_data, aes(x = (.data[[input$lakecat]])
                                  , color = DistCat2))+
      stat_ecdf(geom = "point", pad = FALSE, size = 2)+
      scale_x_log10()+
      scale_color_manual(values = dist2_pal)+
      labs(y = "Cumulative Percent"
           , x = input$lakecat
           , color = "Ref Status")+
      theme(text = element_text(size = 14),
            axis.text = element_text(color = "black", size = 14),
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "top",
            legend.text=element_text(size = 14))

  }) # render Plot8
  
  ## Table 2 ####
  output$table2 <- renderDT({

    table2_data <- myData_lakes()
    table2_trim <- table2_data %>%
      select(WaterbodyID, DistCat_trim, DistCat2, input$waterchem_lakes
             , input$lakecat) %>%
      rename(DistCat3 = DistCat_trim)

    return(table2_trim)

  }) # render Table2
  
})##shinyServer~END
