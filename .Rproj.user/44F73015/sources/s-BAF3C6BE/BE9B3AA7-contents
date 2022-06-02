function(){
  tabPanel("Data Explorer"
           , titlePanel("Data Explorer - Stream Water Chemistry vs StreamCat")
           , h3("The following chemistry data are site average values.")
           , sidebarLayout(
             sidebarPanel(
               helpText("Use the drop down menus to select a water chemistry and StreamCat parameter.")
               ,selectInput(inputId = "waterchem"
                            ,label = "Water Quality Parameter:"
                            ,choices = c("Alkalinity_uEqL" # keep - high and medium priority params
                                         ,"Aluminum_ugL"
                                         ,"Arsenic_ugL"
                                         ,"Chloride_mgL"
                                         ,"Color_Apparent_PCU"
                                         ,"Color_True_PCU"
                                         ,"Conductivity_uScm"
                                         ,"Copper_ugL"
                                         ,"DO_mgL"
                                         ,"DO_SAT_pct"
                                         ,"E_coli_CFU100mL"
                                         ,"Enterococcus_CFU100mL"
                                         ,"Fecal_Coliform_CFU100mL"
                                         ,"Hardness_mgL"
                                         ,"Iron_ugL"
                                         ,"Lead_ugL"
                                         ,"Manganese_ugL"
                                         ,"pH_None"
                                         ,"Selenium_ugL"
                                         ,"Sodium_mgL"
                                         ,"Temperature_Water_DegC"
                                         ,"TN_mgL"
                                         ,"TP_mgL"
                                         ,"Zinc_ugL")) # selectInput
               ,selectInput(inputId = "streamcat"
                            ,label = "StreamCat Parameter:"
                            ,choices = c("Al2O3Cat" # kept - highly correlated with some wq params
                                         ,"AvgPredEC"
                                         ,"BFICat"
                                         ,"ClayCat"
                                         ,"ElevCat"
                                         ,"InorgNWetDep_2008Cat"
                                         ,"IWI_v2_1"
                                         ,"K2OCat"
                                         ,"KffactCat"
                                         ,"Na2OCat"
                                         ,"OmCat"
                                         ,"pcSLOPE"
                                         ,"PctForest2019Ws"
                                         ,"PctImp2019Ws"
                                         ,"PctUrban2019Ws"
                                         ,"PctWetland2019Ws"
                                         ,"RockNCat"
                                         ,"RunoffCat"
                                         ,"SandCat"
                                         ,"TmeanCat"
                                         ,"WetIndexCat"
                                         ,"WsAreaSqKm")) # selectInput
               , htmlOutput("stats")
               , br()
               , helpText(paste("StreamCat is a geospatial dataset with >600 land use metrics:"
                                , "https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset-0"))

             )##sidebarPanel.END
             , mainPanel(
               fluidRow(
                 column(6, plotOutput(outputId = "plot1"))
                 , column(6, plotOutput(outputId = "plot3")))
               , fluidRow(
                 column(6, plotOutput(outputId = "plot5"))
                 , column(6, DT::dataTableOutput("table1")))
             )##mainPanel.END
           )#sidebarLayout.End
  )## tabPanel~END
}##FUNCTION~END