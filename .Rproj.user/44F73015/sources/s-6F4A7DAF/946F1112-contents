#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # modal dialog ####
  myModal <- modalDialog(
    title = "Greetings!"
    ,paste("Welcome to the IEPA Macroinvertebrate IBI Calculator (v0.1.1.9200)!")
    ,br()
    ,paste("IEPAtools was developed to calculate benthic macroinvertebrate metrics"
           ,"and the Composite F Index of Biotic Integrity (IBI) for lakes in Illinois."
           ,"This app was developed by Ben Block (Ben.Block@tetratech.com),"
           ,"with underlying R code written by Erik W. Leppo (Erik.Leppo@tetratech.com)"
           ,"and Ben Block. Please contact Ben Block should any issues or questions arise.")
    ,br()
    ,img(src = "IEPA_logo.png", height = 100)
    ,easyClose = T)

  # Show the model on start up
  showModal(myModal)

  # Setup ####

    # map and plots require df_metsc
    map_data <- reactiveValues(df_metsc = NULL)


    ## Misc Names ####
    output$fn_input_display <- renderText({input$fn_input}) ## renderText~END


    ## df_import ####
    output$df_import_DT <- renderDT({
        # input$df_import will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        inFile <- input$fn_input

        shiny::validate(
            need(inFile != "", "Please select a data set") # used to inform the user that a data set is required
        )

        if (is.null(inFile)){
            return(NULL)
        }##IF~is.null~END

        # Read user imported file
        df_input <- read.csv(inFile$datapath, header = TRUE,
                             sep = input$sep, quote = input$quote
                             , stringsAsFactors = FALSE)

        required_columns <- c("INDEX_NAME" # Required input columns
                              ,"LAKECODE"
                              ,"COLLDATE"
                              ,"COLLMETH"
                              ,"SAMPLEID"
                              ,"LAT"
                              ,"LONG"
                              ,"INDEX_REGION"
                              ,"TAXAID"
                              ,"N_TAXA"
                              ,"EXCLUDE"
                              ,"NONTARGET"
                              ,"PHYLUM"
                              ,"CLASS"
                              ,"ORDER"
                              ,"FAMILY"
                              ,"SUBFAMILY"
                              ,"TRIBE"
                              ,"GENUS"
                              ,"FFG"
                              ,"TOLVAL")

        column_names <- colnames(df_input)

        # QC Check for column names
        col_req_match <- required_columns %in% column_names
        col_missing <- required_columns[!col_req_match]

        shiny::validate(
            need(all(required_columns %in% column_names)
                 , paste0("Error\nChoose correct data separator; otherwise, you may have missing required columns\n"
                          , paste("Required columns missing from the data:\n")
                          , paste("* ", col_missing, collapse = "\n")))
        )##END ~ validate() code

        ########################### MAP and PLOT Observer
        observe({
          inFile<- input$fn_input
          if(is.null(inFile))
            return(NULL)

          df_input
          updateSelectInput(session, "siteid.select", choices = as.character(sort(unique(df_input[, "SAMPLEID"]))))
        }) ## observe~END


        ## Build results ####

        # Add "Results" folder if missing
        boo_Results <- dir.exists(file.path(".", "Results"))
        if(boo_Results==FALSE){
            dir.create(file.path(".", "Results"))
        }

        # Remove all files in "Results" folder
        fn_results <- list.files(file.path(".", "Results"), full.names=TRUE)
        file.remove(fn_results)

        # Write to "Results" folder - Import as TSV
        fn_input <- file.path(".", "Results", "data_import.tsv")
        write.table(df_input, fn_input, row.names=FALSE, col.names=TRUE, sep="\t")

        # Copy to "Results" folder - Import "as is"
        file.copy(input$fn_input$datapath, file.path(".", "Results", input$fn_input$name))

        return(df_input)

    }##expression~END
    , filter="top", options=list(scrollX=TRUE)
    )##output$df_import_DT~END

    # Calculate IBI ####
    # Calculate IBI (metrics and scores) from df_import

    observeEvent(input$b_Calc, {
        shiny::withProgress({
            #
            # Number of increments
            n_inc <- 6

            ## Sink output ####
            #fn_sink <- file.path(".", "Results", "results_log.txt")
            file_sink <- file(file.path(".", "Results", "results_log.txt"), open = "wt")
            sink(file_sink, type = "output", append = TRUE)
            sink(file_sink, type = "message", append = TRUE)

            # Log
            message("Results Log from IEPAtools Shiny App")
            message(Sys.time())
            inFile <- input$fn_input
            message(paste0("file = ", inFile$name))

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Data, Initialize")
            Sys.sleep(0.25)

            #df_data <- 'df_import_DT'
            # Read in saved file (known format)
            df_data <- NULL  # set as null for IF QC check prior to import
            fn_input <- file.path(".", "Results", "data_import.tsv")
            df_data <- read.delim(fn_input, stringsAsFactors = FALSE, sep="\t")

            # Quality control ####

            # QC, FAIL if TRUE
            if (is.null(df_data)){
                return(NULL)
            }

            # QC, FFG symbols
            FFG_approved <- c("CG", "CF", "PR", "SC", "SH")
            df_QC <- df_data
            df_QC[df_QC==""] <- NA


            N_FFG_wrong <- sum((na.omit(df_QC$FFG) %in% FFG_approved) == 0)
            if(N_FFG_wrong>0){
                message(paste0(N_FFG_wrong, "taxa have the incorrect FFG descriptor, please use the following:"))
                message("Replace 'Collector' with 'CG'")
                message("Replace 'Filterer' with 'CF'")
                message("Replace 'Predator' with 'PR'")
                message("Replace 'Scraper' with 'SC'")
                message("Replace 'Shredder' with 'SH'")
                message("Failure to change FFG to correct coding scheme will result in incorrect metric calculations")
            }


            # QC, N_TAXA = 0
            N_Taxa_zeros <- sum(df_data$N_TAXA == 0, na.rm = TRUE)
            if(N_Taxa_zeros>0){
                message("Some taxa in your dataset have a count (N_TAXA) of zero. Values for TAXAID with N_TAXA = 0 will be removed before calculations.")
            }

            # QC, Exclude as TRUE/FALSE
            Exclude.T <- sum(df_data$EXCLUDE==TRUE, na.rm=TRUE)
            if(Exclude.T==0){##IF.Exclude.T.START
                message("EXCLUDE column does not have any TRUE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END

            # QC, NonTarget as TRUE/FALSE
            NonTarget.F <- sum(df_data$NONTARGET==FALSE, na.rm=TRUE)
            if(NonTarget.F==0){##IF.Exclude.T.START
                message("NONTARGET column does not have any FALSE values. \n  Valid values are TRUE or FALSE.  \n  Other values are not recognized.")
            }##IF.Exclude.T.END

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Calculate, Metrics (takes ~ 30-45s)")
            Sys.sleep(0.5)

            # Data Prep ####

            # prior to metric calculation, we need to add columns that aren't part of the dataset but need to be in the input dataframe

            # convert Field Names to UPPER CASE
            names(df_data) <- toupper(names(df_data))

            # QC, Required Fields
            col.req <- c("SAMPLEID", "TAXAID", "N_TAXA", "EXCLUDE", "INDEX_NAME"
                         , "INDEX_REGION", "NONTARGET", "PHYLUM", "SUBPHYLUM", "CLASS", "SUBCLASS"
                         , "INFRAORDER", "ORDER", "FAMILY", "SUBFAMILY", "TRIBE", "GENUS"
                         , "FFG", "HABIT", "LIFE_CYCLE", "TOLVAL", "BCG_ATTR", "THERMAL_INDICATOR"
                         , "LONGLIVED", "NOTEWORTHY", "FFG2", "TOLVAL2", "HABITAT",
                         "UFC", "ELEVATION_ATTR", "GRADIENT_ATTR", "WSAREA_ATTR")
            col.req.missing <- col.req[!(col.req %in% toupper(names(df_data)))]

            # Add missing fields
            df_data[,col.req.missing] <- NA
            warning(paste("Metrics related to the following fields are invalid:"
                          , paste(paste0("   ", col.req.missing), collapse="\n"), sep="\n"))

            # calculate values and scores in two steps using BioMonTools
            # save each file separately

            # columns to keep
            keep_cols <- c("Lat", "Long", "LAKECODE", "COLLDATE", "COLLMETH")

            # Metric calculation ####
            df_metval <- suppressWarnings(metric.values(fun.DF = df_data, fun.Community = "bugs",
                                                           fun.MetricNames = BugMetrics, fun.cols2keep=keep_cols, boo.Shiny = TRUE))


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Metrics have been calculated!")
            Sys.sleep(1)

            # Log
            message(paste0("Chosen IBI from Shiny app = ", MMI))


            #
            # Save
            fn_metval <- file.path(".", "Results", "results_metval.csv")
            write.csv(df_metval, fn_metval, row.names = FALSE)
            #
            # QC - upper case Index.Name
            names(df_metval)[grepl("Index.Name", names(df_metval))] <- "INDEX.NAME"

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Calculate, Scores")
            Sys.sleep(0.50)

            # Metric Scores ####

            # Thresholds
            fn_thresh <- file.path(system.file(package="BioMonTools"), "extdata", "MetricScoring.xlsx")
            df_thresh_metric <- read_excel(fn_thresh, sheet="metric.scoring")
            df_thresh_index <- read_excel(fn_thresh, sheet="index.scoring")

            # run scoring code
            df_metsc <- metric.scores(DF_Metrics = df_metval
                                      , col_MetricNames = BugMetrics
                                      , col_IndexName = "INDEX_NAME"
                                      , col_IndexRegion = "INDEX_REGION"
                                      , DF_Thresh_Metric = df_thresh_metric
                                      , DF_Thresh_Index = df_thresh_index
                                      , col_ni_total = "ni_total")

            df_metsc$Index <- as.numeric(df_metsc$Index)

            # Save
            # fn_metsc <- file.path(".", "Results", "results_metsc.tsv")
            # write.table(df_metsc, fn_metsc, row.names = FALSE, col.names = TRUE, sep="\t")
            fn_metsc <- file.path(".", "Results", "results_metsc.csv")
            write.csv(df_metsc, fn_metsc, row.names = FALSE)

            # MAP and Plot requires df_metsc
            map_data$df_metsc <- df_metsc

            # Summary report ####

            # Render Summary Report (rmarkdown file)
            # rmarkdown::render(input = file.path(".", "Extras", "Summary_SNEP.rmd")
            #                   , output_format = "word_document"
            #                   , output_dir = file.path(".", "Results")
            #                   , output_file = "results_summary_report"
            #                   , quiet = TRUE)

            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Ben's code is magical!")
            Sys.sleep(0.75)


            # Increment the progress bar, and update the detail text.
            incProgress(1/n_inc, detail = "Create, Zip")
            Sys.sleep(0.50)

            # Zip results ####

            # Create zip file
            fn_4zip <- list.files(path = file.path(".", "Results")
                                  , pattern = "^results_"
                                  , full.names = TRUE)
            zip(file.path(".", "Results", "results.zip"), fn_4zip)

            # enable download button
            shinyjs::enable("b_downloadData")

            sink() # console
            sink() # message
            #
        }##expr~withProgress~END
        , message = "Calculating IBI"
        )##withProgress~END
    }##expr~ObserveEvent~END
    )##observeEvent~b_CalcIBI~END


    # Downloadable csv of selected dataset
    output$b_downloadData <- downloadHandler(
        # use index and date time as file name
        #myDateTime <- format(Sys.time(), "%Y%m%d_%H%M%S")

        filename = function() {
            paste(MMI, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip", sep = "")
        },
        content = function(fname) {##content~START
            file.copy(file.path(".", "Results", "results.zip"), fname)

            #
        }##content~END
    )##downloadData~END

    # Data Explorer ####

    # create quantile color palette to change color of markers based on index values

    output$mymap <- renderLeaflet({

      req(!is.null(map_data$df_metsc))

      df_data4Map <- map_data$df_metsc

      # create Narratives

      Nar_Map <- factor(c("Exceptional"
                          ,"Satisfactory"
                          ,"Moderately Degraded"
                          ,"Severely Degraded"))

      Narratives <- ifelse(df_data4Map$Index_Nar == "Exceptional", "Exceptional",
                           ifelse(df_data4Map$Index_Nar == "Satisfactory", "Satisfactory",
                                  ifelse(df_data4Map$Index_Nar == "Moderately Degraded", "Moderately Degraded",
                                         "Severely Degraded")))

      Narratives <- factor(Narratives, levels = c("Exceptional"
                                                  ,"Satisfactory"
                                                  ,"Moderately Degraded"
                                                  ,"Severely Degraded"))


      pal <- colorFactor(
        palette = c('green', 'yellow', 'orange', 'red'),
        domain = Narratives,
        ordered = TRUE)

      # subset data by Index_Region

      N_data <- df_data4Map %>%
        filter(INDEX_REGION == "NORTH")

      C_data <- df_data4Map %>%
        filter(INDEX_REGION == "CENTRAL")

      S_data <- df_data4Map %>%
        filter(INDEX_REGION == "SOUTH")

      # Main map

      leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.WorldStreetMap, group="Esri WSM") %>%
        addProviderTiles("CartoDB.Positron", group="Positron") %>%
        addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite")  %>%
        addPolygons(data = IL_BugClasses
                    , color = "green"
                    , weight = 3
                    , fill = FALSE
                    , label = IL_BugClasses$Site_Class
                    , group = "Bug Site Classes"

        ) %>%
        addCircleMarkers(data = N_data, lat = ~LAT, lng = ~LONG
                         , group = "North", popup = paste("SampleID:", N_data$SAMPLEID, "<br>"
                                                          ,"Site Class:", N_data$INDEX_REGION, "<br>"
                                                          ,"Coll Date:", N_data$COLLDATE, "<br>"
                                                          ,"Lake ID:", N_data$LAKECODE, "<br>"
                                                          ,"<b> Index Value:</b>", round(N_data$Index, 2), "<br>"
                                                          ,"<b> Narrative:</b>", N_data$Index_Nar)
                         , color = "black", fillColor = ~pal(Index_Nar), fillOpacity = 1, stroke = TRUE
                         , clusterOptions = markerClusterOptions()

        ) %>%
        addCircleMarkers(data = C_data, lat = ~LAT, lng = ~LONG
                         , group = "Central", popup = paste("SampleID:", C_data$SAMPLEID, "<br>"
                                                            ,"Site Class:", C_data$INDEX_REGION, "<br>"
                                                            ,"Coll Date:", C_data$COLLDATE, "<br>"
                                                            ,"Lake ID:", C_data$LAKECODE, "<br>"
                                                            ,"<b> Index Value:</b>", round(C_data$Index, 2), "<br>"
                                                            ,"<b> Narrative:</b>", C_data$Index_Nar)
                         , color = "black", fillColor = ~pal(Index_Nar), fillOpacity = 1, stroke = TRUE
                         , clusterOptions = markerClusterOptions()

        ) %>%
        addCircleMarkers(data = S_data, lat = ~LAT, lng = ~LONG
                         , group = "South", popup = paste("SampleID:", S_data$SAMPLEID, "<br>"
                                                          ,"Site Class:", S_data$INDEX_REGION, "<br>"
                                                          ,"Coll Date:", S_data$COLLDATE, "<br>"
                                                          ,"Lake ID:", S_data$LAKECODE, "<br>"
                                                          ,"<b> Index Value:</b>", round(S_data$Index, 2), "<br>"
                                                          ,"<b> Narrative:</b>", S_data$Index_Nar)
                         , color = "black", fillColor = ~pal(Index_Nar), fillOpacity = 1, stroke = TRUE
                         , clusterOptions = markerClusterOptions()

        ) %>%
        addLegend(pal = pal,
                  values = Narratives,
                  position = "bottomright",
                  title = "Index Narratives",
                  opacity = 1) %>%
        addLayersControl(overlayGroups = c("North", "Central", "South", "Bug Site Classes"),
                         baseGroups = c("Esri WSM", "Positron", "Toner Lite"),
                         options = layersControlOptions(collapsed = TRUE))%>%
        hideGroup(c("Bug Site Classes")) %>%
        addMiniMap(toggleDisplay = TRUE, tiles = providers$Esri.WorldStreetMap)
      }) ##renderLeaflet~END

    ## Site select map ####

    # Map that filters output data to only a single site
   observeEvent(input$siteid.select,{
      req(!is.null(map_data$df_metsc))

      df_data4Map <- map_data$df_metsc

      df_filtered <- df_data4Map[df_data4Map$SAMPLEID == input$siteid.select, ]

      # get centroid (use mean just in case have duplicates)
      view.cent <- c(mean(df_filtered$LONG), mean(df_filtered$LAT))

      # modify map
      leafletProxy("mymap") %>%
        removeShape("layer_site_selected") %>%
        addCircles(data=df_filtered
                   , lng=~LONG
                   , lat=~LAT
                   , popup= paste("SampleID:", df_filtered$SAMPLEID, "<br>"
                                 ,"Site Class:", df_filtered$INDEX_REGION, "<br>"
                                 ,"<b> Index Value:</b>", round(df_filtered$Index, 2), "<br>"
                                 ,"<b> Narrative:</b>", df_filtered$Index_Nar)
                   , color = "black"
                   , group = "Sites_selected"
                   , layerId = "layer_site_selected"
                   , radius=30) %>%

        setView(view.cent[1], view.cent[2], zoom = 16) # 1= whole earth

    }) ## observeEvent(input$siteid.select) ~ END



    ## Plots ####

    df_sitefilt <- reactive({
      req(!is.null(map_data$df_metsc))

      df_all_scores <- map_data$df_metsc

      df_all_scores[df_all_scores$SAMPLEID == input$siteid.select, ]
    })## reactive~ END

    output$DatExp_plot <- renderPlot({
      if (is.null(df_sitefilt()))
        return(NULL)

      df_selected_site <- df_sitefilt()

      df_trim <- df_selected_site %>%
        select_if(!is.na(df_selected_site)) %>%
        select(-c(Index_Nar)) %>%
        select(SAMPLEID, Index, starts_with("SC_"))%>%
        rename_at(vars(starts_with("SC_")),
                  funs(str_replace(., "SC_", "")))

      df_grph_input <- df_trim %>%
        pivot_longer(!SAMPLEID, names_to = "Variable", values_to = "Score")

      df_grph_input <- as.data.frame(df_grph_input)

      # shape palette
      shape_pal <- c("Index" = 16
                     ,"pt_EPT" = 15
                     ,"pt_Odon" = 15
                     ,"pi_Tanyp2Chi" = 15
                     ,"pi_tv_toler" = 15
                     ,"pt_ffg_shred" = 15)

      # size palette
      size_pal <- c("Index" = 10
                    ,"pt_EPT" = 5
                    ,"pt_Odon" = 5
                    ,"pi_Tanyp2Chi" = 5
                    ,"pi_tv_toler" = 5
                    ,"pt_ffg_shred" = 5)

      ggplot(df_grph_input, aes(x=Variable, y = Score, shape = Variable))+
        geom_point(aes(size = Variable))+
        scale_size_manual(values=size_pal)+
        scale_shape_manual(values=shape_pal)+
        ylim(0,100)+
        labs(y = "Scores",
             x = "")+
        coord_flip()+
        scale_x_discrete(limits = rev(levels(as.factor(df_grph_input$Variable))))+
        theme(text = element_text(size = 12),
              axis.text = element_text(color = "black", size = 12),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color = "black"),
              legend.position = "none")

    }) ## renderPlot ~ END

    output$Index_plot <- renderPlot({
      if (is.null(df_sitefilt()))
        return(NULL)

      df_all_scores <- map_data$df_metsc

      df_selected_site <- df_sitefilt()

      site_region <- as.character(df_selected_site$INDEX_REGION)

      df_sub_regions <- df_all_scores[df_all_scores$INDEX_REGION == site_region,]

      ggplot()+
        geom_boxplot(data = df_sub_regions, aes(x = INDEX_REGION, y = Index), width = 0.25)+
        geom_point(data = df_selected_site, aes(x = INDEX_REGION, y = Index), size = 5)+
        labs(y = "Index Scores of Input Data Frame",
             x = "Index Region")+
        ylim(0,100)+
        theme(text = element_text(size = 12),
              axis.text = element_text(color = "black", size = 12),
              axis.text.x = element_text(angle = 0, hjust = 0.5),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color = "black"),
              legend.position = "none")

    }) ## renderPlot ~ END
})##shinyServer~END
