#' Item calibration according to item response theory models
#' @importFrom mirt mirt fscores
#' @importFrom psych sim.VSS
#' @importFrom shinyjs useShinyjs
#' @importFrom plyr ldply
#' @importFrom stats coef
#' @importFrom foreign read.spss
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{IRT()}
#' @export
IRT <- function() {
  IRT_ENV <- new.env()
  js <- "
// execute the code after the shiny session has started
$(document).on('shiny:sessioninitialized', function(event) {
  // browser detection from https://stackoverflow.com/a/5918791/8099834
  navigator.sayswho= (function(){
    var ua= navigator.userAgent, tem,
    M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\\/))\\/?\\s*(\\d+)/i) || [];
    if(/trident/i.test(M[1])){
        tem=  /\\brv[ :]+(\\d+)/g.exec(ua) || [];
        return 'IE '+(tem[1] || '');
    }
    if(M[1]=== 'Chrome'){
        tem= ua.match(/\\b(OPR|Edge)\\/(\\d+)/);
        if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
    }
    M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
    if((tem= ua.match(/version\\/(\\d+)/i))!= null) M.splice(1, 1, tem[1]);
    return M.join(' ');
  })();
  // pass browser info from JS to R
  Shiny.onInputChange('myBrowser', navigator.sayswho);
});
"
  # Define UI for application
  ui <- fluidPage(
    shinyjs::useShinyjs(),

    theme = shinythemes::shinytheme("readable"),






    ####################################################################################
    tags$head(tags$style(
      type="text/css",
      "#image0 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image1 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image2 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image3 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image4 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image5 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image6 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),


    tags$head(tags$style(
      type="text/css",
      "#image7 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),


    tags$head(tags$style(
      type="text/css",
      "#image8 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),
    ###################################################################################

    ###################################################################################

    tags$style(HTML("#a{color:darkblue; font-family:Lucida Arial ;font-size: 16px;
             font-style: oblique;text-align:center}")), #tabs#

    tags$style(HTML("#ab{color:darkblue; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

    tags$style(HTML("#b{color:darkblue; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #
    ####################################################################################

    #### POP-UPS ####


    bsTooltip(
      id = "text1",
      title = "The top rows of the data are shown.",
      placement = "top",
      trigger = "hover"
    ),


    bsTooltip(
      id = "q3Slide",
      title = "Q3 results will differ according to your cut-off point.",
      placement = "top",
      trigger = "hover"
    ),

    bsTooltip(
      id = "dataType",
      title = "Please make sure that you selected the right data type.",
      placement = "bottom",
      trigger = "hover"
    ),

    bsTooltip(
      id = "varExp",
      title = "Only the information for the first factor are shown to help to decide if there is any dominant factor.",
      placement = "bottom",
      trigger = "hover"
    ),

    bsTooltip(
      id = "dataType2",
      title = "Please make sure that you selected the correct file type.",
      placement = "bottom",
      trigger = "hover"
    ),


    bsTooltip(
      id = "comparison",
      title = "Please select models to compare.",
      placement = "top",
      trigger = "hover"
    ),

    ##############################################################################################################

    ################################################ APP TITLE ##########################################

    # titlePanel("IRT CALIBRATION"),

    h1(id="title", "IRT CALIBRATION"),
    tags$style(HTML("#title{color: blue; font-family: Arial;font-size: 35px;
            font-style: oblique;text-align:left}")),

    ################################################ SIDEBAR PANEL ############################################

    sidebarLayout(
      sidebarPanel(
        ################################################## PANEL 1 HOME #########################################



        conditionalPanel(condition = "input.panel==0",
                         shiny::img(src = "img/rsp4.png", width = "97%"),

                         tags$head(
                           tags$script(HTML(js))
                         ),
                         br(),
                         br(),
                         br(),
                         textOutput("browser"),
                         tags$head(
                           tags$style(
                             "#browser{
                       color: darkblue;
                       font-size: 25px;
                       font-family: cursive;
                       font-style: oblique;
                       text-align:center;
                       letter-spacing:1px;
                       }"
                           )
                         ),
                         ###################################################################
                         #imageOutput("image1",width = "15%", height = "50px", inline = TRUE),
                         ###################################################################

        ),


        ########################################## PANEL 2 - DATA UPLOAD #########################################

        conditionalPanel(
          condition = "input.panel==1",
          shiny::img(src = "img/rsp4.png", width = "97%"),
          #imageOutput("image1", width = "75%", height = "100px", inline = TRUE),
          ###################################################################
          #imageOutput("image2",width = "15%", height = "50px", inline = TRUE),
          ###################################################################
          br(),
          br(),



          selectInput(
            "dataType",
            h3(   id="ab","Type of Data"),
            choices = c("Dichotomous" = "b", "Polytomous" = "p"),
            selected = 2
          ),
          br(),


          selectInput(
            "dataType2",
            h3(   id="ab","Type of File"),
            choices = list(
              "CSV - Semicolon  Separated  Excel" = 1,
              "CSV - Comma  Separated  Excel" = 2,
              "SAV - SPSS" = 3
            ),
            selected = 3
          ),


          br(),

          uiOutput("uiHeader"),
          fileInput("data",
                    h3(   id="ab","Upload your data"))
        ),

        ############################################ PANEL 3 - ASSUMPTIONS #######################################


        conditionalPanel(
          condition = "input.panel==2",
          shiny::img(src = "img/rsp4.png", width = "97%"),

          ###################################################################
          #imageOutput("image3",width = "15%", height = "50px", inline = TRUE),
          ###################################################################

          br(),
          br(),

          sliderInput(
            "q3Slide",
            min = 0,
            max = 1,
            h3(   id="ab","Cutoff for Yen's Q3 statistics"),
            value = 0.37
          ),


          br(),
          br(),


          uiOutput("textq3user"),


          tags$head(
            tags$style(
              "#textq3{
        color: blue;
        font-size: 20px;
        font-family: cursive;
        font-style: oblique;
        text-align:center;
        letter-spacing:1px;
        }"
            )
          ),


        br(),

        uiOutput("q33")
        ),




        ############################################  PANEL 4 - MODEL COMPARISON ###########################
        conditionalPanel(
          condition = "input.panel==3",
          shiny::img(src = "img/rsp4.png", width = "97%"),

          ###################################################################
          #imageOutput("image4",width = "15%", height = "50px", inline = TRUE),
          ###################################################################

          br(),
          br(),

          uiOutput("comparison")
        ),



        ########################################  PANEL 5 -  CALIBRATION  ##############################################

        conditionalPanel(
          condition = "input.panel==4",

          shiny::img(src = "img/rsp4.png", width = "97%"),

          ###################################################################
          #imageOutput("image5",width = "15%", height = "50px", inline = TRUE),
          ###################################################################

          br(),
          br(),

          uiOutput("model"),

          selectInput(
            "kestirim",
            h3(   id="ab","Select theta estimation method"),
            choices = c("EAP", "MAP", "WLE")
          ),
          selectInput(
            "d",
            h3(   id="ab","Select D coefficient"),
            choices = c(1.702, 1.000),
            selected = 1.702
          ),
          uiOutput("maddecikar"),
          actionButton("cikar", h3( id="ab","Remove / Update")),


          br(),
          br(),

          gt::gt_output("theta"),
          gt::gt_output("theta2"),

        ),


        ################################################  PANEL 6 - PLOTS ##############################################


        conditionalPanel(
          condition = "input.panel==5",
          shiny::img(src = "img/rsp4.png", width = "97%"),

          ###################################################################
          #imageOutput("image6",width = "15%", height = "50px", inline = TRUE),
          ###################################################################

          br(),
          br(),

          selectInput(
            "graph",
            h3(id="ab","Select the plot type"),
            choices = c(
              "Item Characteristic Curve" = 1,
              "Item Information Function" = 2,
              "Test Information Function" = 3,
              "Marginal Reliability" = 4,
              "ICC for all items" = 5,
              "IIF for all items" = 6
            ),
            selected = 1
          ),

          selectInput("madde.no",
                      h3(id="ab","Select the item for plot"),
                      choices = "Please upload data")
        ),



        ######################################## PANEL 7  - OUTPUT  ##############################################

        conditionalPanel(
          condition = "input.panel==6",
          shiny::img(src = "img/rsp4.png", width = "97%"),

          ###################################################################
          #imageOutput("image7",width = "15%", height = "50px", inline = TRUE),
          ###################################################################
          br(),

          ###################################################################
          #imageOutput("image8",width = "15%", height = "50px", inline = TRUE)
          ###################################################################
          shiny::img(src = "img/download.gif", width = "97%"),

        )

        #######################################################################################################

      ),
      # sidebar panel kapatır

      # Show a plot of the generated distribution


      ######################################################## MAIN PANEL #######################################

      mainPanel(
        tabsetPanel(
          id = "panel",


          ############################################## M.PANEL 1 ##################################################

          tabPanel(


            h4(id="a",   "INTRODUCTION"),

            value = 0,
            br(),
            br(),
            br(),

            fluidRow(column(
              12, align = "center",
              shiny::img(src = "img/rsp4.png", width = "97%"),

              ###################################################################
              #imageOutput("image0",width = "15%", height = "50px", inline = TRUE)
              ###################################################################

            ))),
          # imageOutput("imagek3",width = "75%", height = "100px", inline = TRUE)  ))),


          ############################################### M.PANEL 2 ###############################################

          tabPanel(
            h4(id="a", "DATA UPLOAD"),
            value = 1,

            br(),
            br(),

            textOutput("text1"),
            tags$head(
              tags$style(
                "#text1{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
              )
            ),
            br(),

            textOutput("text11"),
            tags$head(
              tags$style(
                "#text11{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
              )
            ),
            br(),

            DT::dataTableOutput("o.data"),
            ## REVISED

            withLoader(gt::gt_output("descriptive") , type = "html", loader = "loader1")
          ),
          ## REVISED


          ######################################## M. PANEL 3 ####################################################

          tabPanel(
            h4(id="a", "ASSUMPTION CHECKS"),
            value = 2,
            br(),
            br(),

            textOutput("text2"),
            tags$head(
              tags$style(
                "#text2{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
              )
            ),
            br(),
            br(),

            withLoader(plotOutput("plot1"), type = "html", loader = "loader1"),

            br(),
            br(),

            withLoader(gt::gt_output("varExp"), type = "html", loader = "loader1")

          ),


          ############################################## M.PANEL 4 ################################################

          tabPanel(
            h4(id="a", "MODEL FIT"),
            value = 3,

            br(),
            br(),

            textOutput("text3"),

            tags$head(
              tags$style(
                "#text3{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
              )
            ),

            br(),
            br(),

            withLoader(gt::gt_output("kars"), type = "html", loader = "loader1")
          ),


          ######################################## M. PANEL 5 ####################################################

          tabPanel(
            h4(id="a", "CALIBRATION"),
            value = 4,

            br(),
            br(),

            textOutput("text4"),

            tags$head(
              tags$style(
                "#text4{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
              )
            ),
            br(),
            br(),


            withLoader(gt::gt_output("calibrasyon"), type = "html", loader = "loader1"),
            # gt::gt_output("theta"),
            gt::gt_output("calibrasyon2"),
            # gt::gt_output("theta2")),

          ),


          ####################################### M.PANEL 6 #########################################################


          tabPanel(
            h4(id="a",  "PLOT"),
            value = 5,


            br(),
            br(),

            textOutput("text5"),

            tags$head(
              tags$style(
                "#text5{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
              )
            ),
            br(),
            br(),


            plotOutput("graph")
          ),


          #######################################  M.PANEL 7 #######################################################

          tabPanel(
            h4(id="a",  "OUTPUT"),
            value = 6,


            br(),
            br(),

            textOutput("text6"),

            tags$head(
              tags$style(
                "#text6{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
              )
            ),

            br(),
            fluidRow(column(
              4,
              downloadButton("compDl",  h1(id="b","Model comparison results"))
            ),




            column(
              8,
              downloadButton("thetaDl",  h1(id="b","Theta estimations"))
            )),

            br(),

            fluidRow(column(
              4,
              downloadButton("q3.dl",  h1(id="b","Full Q3 statistics"))
            ),



            column(
              8,
              downloadButton("calibDl",  h1(id="b","Item parameters"))
            )),




          )

        ) #  close tabsetpanel

      ) #  close main panel
    )
  ) # close fluid page

  ########################### SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER SERVER ##############################################


  # Define server logic

  server <- function(input, output, session) {


    output$browser <- renderText({
      req(input$myBrowser)
      if(input$myBrowser == "Chrome 102"){
        paste0("Please click 'Open in Browser' for a better experience")
      } else {
        NULL
      }
      # contains the value returned by the JS function
    })

    ###########################################################################

    output$image0<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image2<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image3<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image4<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image5<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image6<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image7<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp4.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image8<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "download.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)



    ###########################################################################





    # Madde türüne göre model değiştirme


    output$comparison <- renderUI({
      if (input$dataType == "p")

      {
        checkboxGroupInput(
          "comparison",

          choices = c(
            "PCM" = "Rasch",
            "GRM" = "graded",
            "GPCM" = "gpcm",
            "NRM" = "nominal",
            "RSM" = "rsm"
          ),
          label =  h3(   id="ab","Select models to compare")
        )
      }


      else


      {
        checkboxGroupInput(
          "comparison",
          choices = c(
            "Rasch" = "Rasch",
            "1PL" = "1PL",
            "2PL" = "2PL",
            "3PL" = "3PL",
            "4PL" = "4PL"
          ),
          label = h3(   id="ab","Select models to compare")
        )


      }


    })




    output$model <- renderUI({
      if (input$dataType == "p")

      {
        selectInput(
          "model",
          h3(   id="ab","Select IRT Model"),
          choices = c(
            "PCM" = "Rasch",

            "GRM" = "graded",
            "GPCM" = "gpcm",
            "NRM" = "nominal",
            "RSM" = "rsm"
          ),
          selected = "graded"
        )
      }


      else

      {
        selectInput(
          "model",
          h3(   id="ab","Select IRT Model"),
          choices = c(
            "Rasch" = "Rasch",
            "1PL" = "1PL",
            "2PL" = "2PL",
            "3PL" = "3PL",
            "4PL" = "4PL"
          ),
          selected = "Rasch"
        )


      }


    })




    ####################################################### DATA UPLOAD ####################################################

    output$uiHeader <- renderUI({
      if (input$dataType2 == 3) {
        NULL
      } else {
        checkboxInput("header",
                      h3(   id="ab", "Is the first row includes variable names?"),
                      value = TRUE)
      }
    })

    irtData <- reactive({
      data <- input$data
      if (is.null(data)) {
        return(paste0("Please upload your data"))
      } else if (input$dataType2 == 1) {
        if (tools::file_ext(data$datapath) != "csv") {
          data.frame(Caution = "Please select corret file type")
        } else {
          read.csv2(data$datapath,
                    header = isTRUE(input$header),
                    sep = ";")
        }
      } else if (input$dataType2 == 2) {
        if (tools::file_ext(data$datapath) != "csv") {
          data.frame(caution = "Please select correct file type")
        } else {
          read.csv2(data$datapath,
                    header = isTRUE(input$header),
                    sep = ",")
        }
      } else if (input$dataType2 == 3) {
        if (tools::file_ext(data$datapath) != "sav") {
          data.frame(caution = "Please select correct file type")
        } else {
          foreign::read.spss(data$datapath,
                    to.data.frame = TRUE,
                    use.value.labels = FALSE)
        }
      }
    })


    output$o.data <- DT::renderDataTable({
      if (!is.null(input$data)) {
        data <- irtData()
        colnames(data) <- paste0("i", 1:ncol(data))

        IRT_ENV$dataGL <- na.omit(irtData())
        colnames(IRT_ENV$dataGL) <- paste0("i", 1:ncol(IRT_ENV$dataGL))

        return(head(data, 10))


      }
    })


    ######################################################################## UI OUTPUTS ###################################################

    output$q33 <- renderUI({
      if (!is.null(input$data)) {
        withLoader(gt::gt_output("q3"), type = "html", loader = "loader1")

      } ## REVISED


    })


    output$textq3user <- renderUI({
      if (is.null(input$data)) {
        return(NULL)
      }

      textOutput("textq3")




    })





    ######################################### M.PANEL TEXTS ############################################################


    output$text1 <- renderText({
      if (!is.null(input$data)) {
        paste0 ("DATA UPLOAD AND DESCRIPTIVE STATISTICS")
      }
    })


    output$text11 <- renderText({
      if (!is.null(input$data))
      {
        paste0 ("DATA HAS BEEN UPLOADED SUCCESSFULLY")
      }
    })


    output$text2 <- renderText({
      if (!is.null(input$data)) {
        paste0 ("UNIDIMENSIONALITY AND LOCAL INDEPENDENCE")
      }
    })



    output$text3 <- renderText({
      if (!is.null(input$data)) {
        paste0 ("MODEL FIT")
      }
    })



    output$text4 <- renderText({
      if (!is.null(input$data)) {
        paste0 ("CALIBRATION AND ITEM PARAMETERS")
      }
    })



    output$text5 <- renderText({
      if (!is.null(input$data)) {
        paste0 ("ITEM CHARACTERISTIC CURVE - ITEM INFORMATION FUNCTION")
      }
    })


    output$text6 <- renderText({
      if (!is.null(input$data)) {
        paste0 ("DOWNLOAD THE RESULTS")
      }

    })


    ################################################### DESCRIPTIVE #################################################

    output$descriptive <-
      render_gt(align = "center", {
        ### REVISED


        if (!is.null(input$data)) {
          noItem <- ncol(irtData())
          sampleSize <- nrow(irtData())
          noMissing <- length(which(is.na(irtData())))
          descriptive1 <-
            data.frame(No_of_Items = noItem,
                       N = sampleSize,
                       No_of_Missings = noMissing)

          res <- gt::gt(descriptive1)


          res <- res  %>%    tab_header(title = md("*General information about the dataset*"))

          res <- res %>%

            tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                          0.15),

                      locations = cells_body())

          res <- res %>%

            cols_width(everything() ~ gt::px(180))



          res <- res %>%

            tab_options(
              column_labels.font.size = gt::px(17),
              column_labels.font.weight = "bold"
            )

          res <- res %>%

            tab_options(heading.title.font.size = gt::px(25))

          return(res)

        }
      })

    ########################################### ASSUMPTION CHECKS ############################################


    output$q3 <- render_gt(align = "center", {
      # dataGL<-irtData()

      mrt <- mirt(na.omit(irtData()), 1)


      q3 <- mirt::residuals(mrt, type = "Q3" , suppress = input$q3Slide)

      IRT_ENV$q3.1 <- mirt::residuals(mrt, type = "Q3")


      q3Index <- which(q3 > input$q3Slide & q3 < 1, arr.ind = TRUE)

      q3IndexValue <- q3[which(q3 > input$q3Slide & q3 < 1)]


      q3RowIndex <- q3Index[, 1]

      q3ColIndex <- q3Index[, 2]

      q3RowName <- rownames(q3)[q3RowIndex]
      q3ColName <- colnames(q3)[q3ColIndex]
      IRT_ENV$q3Table <- data.frame(itemNo1 = q3RowName,
                             itemNo2 = q3ColName,
                             q3Value = q3IndexValue)



      res <- gt::gt(IRT_ENV$q3Table)                               # REVISED

      res <- res  %>%

        tab_header(title = md("*Q3 - Items that violate local independence"))

      res <- res %>%

        tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                      0.15),

                  locations = cells_body())

      res <- res %>%

        cols_width(everything() ~ gt::px(120))



      res <- res %>%

        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%

        tab_options(heading.title.font.size = gt::px(25))


      return(res)

    })


    #################################### Q3 RESULTS #############################################


    output$textq3 <- renderText({
      if (is.null(input$data)) {
        return(NULL)
      }



      if (input$q3Slide != 0.37)   {
        return (NULL)
      }

#########################S4S4S4##################3
       # if (length(q3Table$q3Value) == 0) {

      if (length(IRT_ENV$q3Table$q3Value) == 0) {
        paste0 ("NONE OF THE ITEMS VIOLATES LOCAL INDEPENDENCE")
      }

      else {
        paste0 ("THE FOLLOWING ITEMS VIOLATE(S) LOCAL INDEPENDENCE")
      }


    })




    ########################################################## Q3 DOWNLOAD #################################################

    output$q3.dl <- downloadHandler(
      filename = function() {
        "q3.csv"
      },
      content = function(file) {
        write.csv2(IRT_ENV$q3.1, file)   # REVISED ????????
      }
    )

    #########################################################################################################################################

    #######################################SCREE PLOT#################################################################

    output$plot1 <- renderPlot({
      #dataGL <- irtData()                  # REVISED

      if (!is.null(input$data)) {
        if (input$dataType == "p") {
          corMatrix <- cor(IRT_ENV$dataGL)

          IRT_ENV$model1 <- principal(corMatrix,
                               nfactors = ncol(IRT_ENV$dataGL),
                               rotate = "none")
        }
        else {
          tetMatrix <- tetrachoric(IRT_ENV$dataGL)$rho

          IRT_ENV$model1 <- principal(tetMatrix,
                               nfactors = ncol(IRT_ENV$dataGL),
                               rotate = "none")

        }

        set.seed <- 123
        horn <- hornpa(k = ncol(IRT_ENV$dataGL),
                       size = nrow(IRT_ENV$dataGL),
                       reps = 200)
        PA_MEAN <- horn$Mean
        plot(
          IRT_ENV$model1$values,
          type = "b",
          col = 2,
          lty = 1,
          main = "SCREE PLOT AND PARALLEL ANALYSIS",
          xlab = "Number of factors",
          ylab = "Eigenvalue"
        )
        lines(PA_MEAN,
              type = "b",
              col = 1,
              lty = 2)
        abline(
          v = which(PA_MEAN > IRT_ENV$model1$values)[1],
          col = "blue",
          lty = "dashed"
        )
        legend(
          "topright",
          legend = c("PA Mean",
                     "Eigenvalue"),
          col = 1:2,
          lty = 1:2,
          lwd = 2
        )
      }
    })


    output$varExp <- render_gt(align = "center", {
      if (is.null(input$data)) {
        return(NULL)
      }

      Factor1 <- IRT_ENV$model1$Vaccounted[1:2, 1:2][, 1]
      names(Factor1) <- c("Eigenvalue", "Variance explained")

      Factor2 <- IRT_ENV$model1$Vaccounted[1:2, 1:2][, 2]
      names(Factor2) <- c("Eigenvalue", "Variance explained")


      res <-
        data.frame(statistic = c("Eigenvalue", "Variance explained"),
                   Factor1,
                   Factor2)

      res <- gt::gt(res)

      res <- res  %>%

        tab_header(title = md("*EIGENVALUE AND VARIANCE EXPLAINED"))


      res <- res %>%

        tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                      0.15),

                  locations = cells_body())

      res <- res %>%

        cols_width(everything() ~ gt::px(180))



      res <- res %>%

        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%

        tab_options(heading.title.font.size = gt::px(25))

      return(res)

    })

    ###############################################################################################################


    #################################### MODEL COMPARISON #####################################################

    output$kars <- render_gt(align = "center", {
      IRT_ENV$km <- as.vector(input$comparison)

      if (length(IRT_ENV$km) < 2) {
        gt::gt(data.frame(WARNING = "PLEASE SELECT TWO MODELS"))


      } else if (length(IRT_ENV$km) == 2) {
        if(length(which(IRT_ENV$km == "1PL")) != 0){
          firstLine <- paste0("F1 = 1 - ", ncol(irtData()))
          secondLine <-
            paste0("CONSTRAIN = (1 - ", ncol(irtData()), ", a1)")
          m1 <-
            mirt(
              na.omit(irtData()),
              paste(firstLine, secondLine, sep = "\n"),
              itemtype = NULL,
              D = input$d
            )
          m2 <- mirt(na.omit(irtData()),
                     model = 1,
                     itemtype = IRT_ENV$km[-which(IRT_ENV$km == "1PL")])
        } else {
          m1 <- mirt(na.omit(irtData()),
                     model = 1,
                     itemtype = IRT_ENV$km[1])
          m2 <- mirt(na.omit(irtData()),
                     model = 1,
                     itemtype = IRT_ENV$km[2])
        }
        IRT_ENV$model1 <- m1@Fit$AIC
        model2 <- m2@Fit$AIC
        if (IRT_ENV$model1 > model2) {
          IRT_ENV$m1m2 <- gt::gt(data.frame(model = c(IRT_ENV$km[1], IRT_ENV$km[2]), mirt::anova(m1, m2)))
        } else {
          IRT_ENV$m1m2 <- gt::gt(data.frame(model = c(IRT_ENV$km[2], IRT_ENV$km[1]), mirt::anova(m1, m2)))
        }
        # REVISED


        res <- IRT_ENV$m1m2

        res <- res  %>%

          tab_header(title = md("*MODEL COMPARISON*"))


        res <- res %>%

          tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                        0.15),

                    locations = cells_body())

        res <- res %>%

          cols_width(everything() ~ gt::px(100))



        res <- res %>%

          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%

          tab_options(heading.title.font.size = gt::px(25))

      } else {
        res <- gt::gt(data.frame(caution = "PLEASE SELECT ONLY TWO MODELS"))


      }

    })


    ## Calibration - Item remove ##

    output$maddecikar <- renderUI({
      if (is.null(input$data)) {
        IRT_ENV$secenek <- h4("Please wait for the calibration")
      } else {
        IRT_ENV$secenek <- colnames(IRT_ENV$dataGL)
        selectInput(
          "toRemove",
          h4(
            "Please select the items that you want to remove from the calibration"
          ),
          choices = IRT_ENV$secenek,
          multiple = TRUE
        )
      }
    })

    ################################################ KALİBRASYON #########################################################

    output$calibrasyon <- render_gt({
      if (is.null(input$data)) {
        return(NULL)
      }

      # dataGL<-irtData()

      IRT_ENV$optionPlot <- colnames(IRT_ENV$dataGL)

      updateSelectInput(session,
                        "madde.no",
                        "Select the items for plotting",
                        choices = IRT_ENV$optionPlot)

      IRT_ENV$calibData <- IRT_ENV$dataGL
      if (input$model == "1PL") {
        firstLine <- paste0("F1 = 1 - ", ncol(IRT_ENV$calibData))
        secondLine <-
          paste0("CONSTRAIN = (1 - ", ncol(IRT_ENV$calibData), ", a1)")
        IRT_ENV$calib <-
          mirt(
            IRT_ENV$calibData,
            paste(firstLine, secondLine, sep = "\n"),
            itemtype = NULL,
            D = input$d
          )
      } else {
        IRT_ENV$calib <- mirt(IRT_ENV$calibData, 1, itemtype = input$model, D = input$d)
      }

      katsayi <- mirt::coef(IRT_ENV$calib, IRTpars = TRUE)
      IRT_ENV$coefDf <- ldply(katsayi, data.frame)
      IRT_ENV$son <- ncol(IRT_ENV$coefDf) - 2
      coefDf2 <- IRT_ENV$coefDf[, 1:IRT_ENV$son]

      # coefDf2<-rename(coefDf2, Madde = .id)

      if (input$dataType == "b")

      {
        colnames(coefDf2) <-
          c("Item",
            "a-Discrimination",
            "b-Difficulty",
            "c-Pseudo-guess",
            "u")
      }

      #gt::gt

      res <- gt::gt(coefDf2[-nrow(coefDf2), ])

      res <- res  %>%

        tab_header(title = md("ITEM PARAMETERS"))


      res <- res %>%

        tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                      0.15),

                  locations = cells_body())

      res <- res %>%

        cols_width(everything() ~ gt::px(180))


      #
      #     # if ( input$dataType=="b")
      #     #
      #     # {  res<- res %>%
      #     #
      #     #
      #     # #cols_width(
      #     #  # c(Madde) ~ gt::px(85) )
      #     #  }
      #
      #
      #     else
      #
      #   {  res<- res %>%
      #
      #
      #     cols_width(
      #       c(.id) ~ gt::px(70) )  }





      res <- res %>%

        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%

        tab_options(heading.title.font.size = gt::px(25))


    })


    br()
    br()
    br()


    ############################################## MEAN THETA ###########################################

    output$theta <- render_gt({
      if (is.null(input$data)) {
        return(NULL)
      }

      IRT_ENV$theta <- fscores(IRT_ENV$calib, method = input$kestirim)
      ort.teta <- data.frame(MEAN_Theta = colMeans(IRT_ENV$theta))

      #gt::gt

      res <- gt::gt(ort.teta)

      res <- res  %>%

        tab_header(title = md("MEAN THETA"))

      res <- res %>%

        tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                      0.15),

                  locations = cells_body())

      res <- res %>%

        cols_width(everything() ~ gt::px(180))

      res <- res %>%

        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%

        tab_options(heading.title.font.size = gt::px(15))




    })

    ############################################## REMOVE ITEM OBSERVE EVENT ################################################

    observeEvent(input$cikar, {
      shinyjs::hide("calibrasyon")
      shinyjs::hide("theta")

      IRT_ENV$toRemove <- input$toRemove
      IRT_ENV$tumu <- colnames(IRT_ENV$dataGL)
      IRT_ENV$kalan <- setdiff(IRT_ENV$tumu, IRT_ENV$toRemove)
      IRT_ENV$kalan.data <- IRT_ENV$dataGL[, IRT_ENV$kalan]
      IRT_ENV$optionPlot <- colnames(IRT_ENV$kalan.data)
      updateSelectInput(session,
                        "madde.no",
                        "Select the item that you want to plot",
                        choices = IRT_ENV$optionPlot)

      output$calibrasyon2 <- render_gt({
        IRT_ENV$calibData <- IRT_ENV$kalan.data

        if (input$model == "1PL") {
          firstLine <- paste0("F1 = 1 - ", ncol(IRT_ENV$calibData))
          secondLine <-
            paste0("CONSTRAIN = (1 - ", ncol(IRT_ENV$calibData), ", a1)")
          IRT_ENV$calib <-
            mirt(
              IRT_ENV$calibData,
              paste(firstLine, secondLine, sep = "\n"),
              itemtype = NULL,
              D = input$d
            )
        } else {
          IRT_ENV$calib <- mirt(IRT_ENV$calibData,
                         1,
                         itemtype = input$model,
                         D = input$d)
        }
        katsayi <- mirt::coef(IRT_ENV$calib, IRTpars = TRUE)
        IRT_ENV$coefDf <- ldply(katsayi, data.frame)
        IRT_ENV$son <- ncol(IRT_ENV$coefDf) - 2
        coefDf2 <- IRT_ENV$coefDf[, 1:IRT_ENV$son]

        # coefDf2<-rename(coefDf2, Madde = .id)

        if (input$dataType == "b")

        {
          colnames(coefDf2) <-
            c("Item",
              "a-Discrimination",
              "b-Difficulty",
              "c-Pseudo guess",
              "u")
        }

        res <- gt::gt(coefDf2[-nrow(coefDf2), ])

        res <- res  %>%

          tab_header(title = md("ITEM PARAMETERS"))


        res <- res %>%

          tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                        0.15),

                    locations = cells_body())

        res <- res %>%

          cols_width(everything() ~ gt::px(180))

        res <- res %>%

          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%

          tab_options(heading.title.font.size = gt::px(25))


      })

      output$theta2 <- render_gt({
        IRT_ENV$theta <- fscores(IRT_ENV$calib, method = input$kestirim)
        ort.teta <- data.frame(MEAN_Theta = colMeans(IRT_ENV$theta))

        #gt::gt

        res <- gt::gt(ort.teta)

        res <- res  %>%

          tab_header(title = md("MEAN THETA"))

        res <- res %>%

          tab_style(style = cell_fill(color = sample(colors()[3:100], 1), alpha =
                                        0.15),

                    locations = cells_body())

        res <- res %>%

          cols_width(everything() ~ gt::px(180))

        res <- res %>%

          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%

          tab_options(heading.title.font.size = gt::px(15))




      })




    })



    ##################################################### GRAFİK ###################################################


    output$graph <- renderPlot({
      if (is.null(input$data)) {
        return(NULL)
      }

      plotType <- input$graph
      madde.no <- which(IRT_ENV$optionPlot == input$madde.no)
      if (plotType == 1) {
        IRT_ENV$mke <- mirt::plot(
          IRT_ENV$calib,
          type = "trace",
          which.items = madde.no,
          theta_lim = c(-4, 4),
          lwd = 2
        )
        IRT_ENV$mke$xlab <- "Theta Level"
        IRT_ENV$mke$ylab <- "Probability"
        IRT_ENV$mke$main <- "Item Characteristic Curve"
        IRT_ENV$mke
      } else if (plotType == 2) {
        mbf <- mirt::plot(
          IRT_ENV$calib,
          type = "infotrace",
          which.items = madde.no,
          theta_lim = c(-4, 4),
          lwd = 2
        )
        mbf$xlab <- "Theta Level"
        mbf$ylab <- "Info Level"
        mbf$main <- "Item Information Function"
        mbf
      } else if (plotType == 3) {
        tbf <- mirt::plot(
          IRT_ENV$calib,
          type = "infoSE",
          theta_lim = c(-4, 4),
          lwd = 2
        )
        tbf$xlab <- "Theta Level"
        tbf$main <- "Test Information / Standard Error Function"
        tbf[["legend"]][["right"]][["args"]][["label"]][[1]][[1]] <-
          "SH"
        tbf
      } else if (plotType == 4) {
        tgf <- mirt::plot(
          IRT_ENV$calib,
          type = "rxx",
          theta_lim = c(-4, 4),
          lwd = 2
        )
        tgf$xlab <- "Theta Level"
        tgf$ylab <- "Reliability"
        tgf$main <- "Marginal Reliability Function"
        tgf
      } else if (plotType == 5) {
        mket <- mirt::plot(
          IRT_ENV$calib,
          type = "trace",
          theta_lim = c(-4, 4),
          lwd = 2
        )
        mket$xlab <- "Theta Level"
        mket$ylab <- "Probability"
        mket$main <- "Item Characteristic Curve"
        mket
      }
      else if (plotType == 6) {
        mket <- mirt::plot(
          IRT_ENV$calib,
          type = "infotrace",
          theta_lim = c(-4, 4),
          lwd = 2
        )
        mket$xlab <- "Theta Level"
        mket$ylab <- "Probability"
        mket$main <- "Item Information Function"
        mket
      }
    })

    ##### OUTPUT #####
    output$compDl <- downloadHandler(
      filename = function() {
        "comparison.csv"
      },
      content = function(file) {
        write.csv2(IRT_ENV$m1m2, file)
      }
    )

    output$thetaDl <- downloadHandler(
      filename = function() {
        "theta.csv"
      },
      content = function(file) {
        write.csv2(IRT_ENV$theta, file)
      }
    )



    output$calibDl <- downloadHandler(
      filename = function() {
        "calibration.csv"
      },
      content = function(file) {
        write.csv2(IRT_ENV$coefDf, file)
      }
    )

    session$onSessionEnded(function() {
      stopApp()
    })


    # calib <- calibData <- dataGL <- kalan<- kalan.data<- km<- m1m2<- mke<- NULL
    # optionPlot<-  q3.1<- q3Table  <- secenek<- theta <- tumu<- NULL

  }

  # Run the application
  shinyApp(ui = ui, server = server)
  }




