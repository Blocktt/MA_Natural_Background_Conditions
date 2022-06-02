function(){
  tabPanel("Lakes"
           , titlePanel("Data Explorer - Lake Water Chemistry vs LakeCat")
           , h3("The following chemistry data are waterbody average values.")
           , sidebarLayout(
             sidebarPanel(
               helpText("Use the drop down menus to select a water chemistry and LakeCat parameter.")
               ,selectInput(inputId = "waterchem_lakes"
                            ,label = "Water Quality Parameter:"
                            ,choices = c("Alkalinity_uEqL"
                                         ,"Chla_ugL"
                                         ,"Chloride_mgL"
                                         ,"Conductivity_uScm"
                                         ,"DO_bttm_mgL"
                                         ,"DO_mean_1mBlwThrmclin_mgL"
                                         ,"DO_mean_hypo_mgL"
                                         ,"DO_mean_surf_mgL"
                                         ,"DO_mgL"
                                         ,"DO_SAT_pct"
                                         ,"DOSAT_bttm_pct"
                                         ,"DOSAT_mean_1mBlwThrmclin_pct"
                                         ,"DOSAT_mean_hypo_pct"
                                         ,"DOSAT_mean_surf_pct"
                                         ,"NO3NO2_mgL"
                                         ,"pH_bttm"
                                         ,"pH_mean_1mBlwThrmclin"
                                         ,"pH_mean_hypo"
                                         ,"pH_mean_surf"
                                         ,"pH_None"
                                         ,"Temp_bttm_C"
                                         ,"Temp_min_C"
                                         ,"Temperature_Water_DegC"
                                         ,"TN_mgL"
                                         ,"TP_mgL")) # selectInput
               ,selectInput(inputId = "lakecat"
                            ,label = "LakeCat Parameter:"
                            ,choices = c("WsAreaSqKm"
                                         ,"BFICat"
                                         ,"ElevCat"
                                         ,"Al2O3Cat"
                                         ,"K2OCat"
                                         ,"Na2OCat"
                                         ,"PctImp2019Cat"
                                         ,"KffactCat"
                                         ,"InorgNWetDep_2008Cat"
                                         ,"Tmean8110Cat"
                                         ,"RunoffCat"
                                         ,"SlopeCat"
                                         ,"ClayCat"
                                         ,"SandCat"
                                         ,"OmCat"
                                         ,"WetIndexCat"
                                         ,"WetIndexWs"
                                         ,"PctUrb2019Ws"
                                         ,"PctFor2019Ws"
                                         ,"PctWet2019Ws")) # selectInput
               , htmlOutput("stats_lakes")
               , br()
               , helpText(paste("LakeCat is a geospatial dataset with >600 land use metrics:"
                                , "https://www.epa.gov/national-aquatic-resource-surveys/lakecat-dataset"))

             )##sidebarPanel.END
             , mainPanel(
               fluidRow(
                 column(6, plotOutput(outputId = "plot6"))
                 , column(6, plotOutput(outputId = "plot7")))
               , fluidRow(
                 column(6, plotOutput(outputId = "plot8"))
                 , column(6, DT::dataTableOutput("table2")))
             )##mainPanel.END
           )#sidebarLayout.End
  )## tabPanel~END
}##FUNCTION~END