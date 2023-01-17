#' Testing measurement & structural models for dichotomous and polytomous data
#' @import shiny
#' @import GPArotation igraph rJava xlsx
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyBS bsTooltip
#' @importFrom gt render_gt tab_header tab_style cols_width tab_options cell_text cells_body cell_fill md
#' @importFrom polycor polyserial
#' @importFrom ShinyItemAnalysis plotDistractorAnalysis
#' @importFrom DT formatStyle datatable
#' @importFrom foreign read.spss
#' @importFrom shinycustomloader withLoader
#' @importFrom shinythemes shinytheme
#' @importFrom grDevices colors dev.off pdf
#' @importFrom graphics abline barplot legend lines
#' @importFrom stats C D anova na.omit rbinom residuals rnorm runif sd var
#' @importFrom lavaan cfa fitMeasures
#' @importFrom semPlot semPaths
#' @importFrom MVN mvn
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{CFA()}
#' @export
CFA <- function(){
  CFA_ENV <- new.env()
  js <- "
// This solution from https://stackoverflow.com/a/59674107
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
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("readable"),

    useShinyjs(),

    ####################################################################################
    tags$head(tags$style(
      type="text/css",
      "#image0 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image1 img {max-width: 100%; width: auto; height: 50%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image2 img {max-width: 100%; width: auto; height: 50%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image3 img {max-width: 100%; width: auto; height: 50%; align: center}"
    )),

    tags$head(tags$style(
      type="text/css",
      "#image3.1 img {max-width: 100%; width: auto; height: 100%; align: center}"
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

    #######################################################################


    tags$style(HTML("#a{color:darkblue; font-family:Lucida Arial ;font-size: 16px;
             font-style: oblique;text-align:center}")), #tabs#

    tags$style(HTML("#ab{color:darkblue; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

    tags$style(HTML("#b{color:darkblue; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #

    ########################################################################3

    ################ POPUP ################
    shinyBS::bsTooltip(
      id = "type",
      title = "Make sure you choose the data type correctly!",
      placement = "right",
      trigger = "hover"
    ),
    shinyBS::bsTooltip(
      id = "type2",
      title = "Make sure you choose the file format correctly!",
      placement = "right",
      trigger = "hover"
    ),
    shinyBS::bsTooltip(
      id = "fmodel",
      title = "Write syntax according to the example above.",
      placement = "right",
      trigger = "hover"
    ),
    shinyBS::bsTooltip(
      id = "ozet",
      title = "Download model summary in excel format!",
      placement = "right",
      trigger = "hover"
    ),
    shinyBS::bsTooltip(
      id = "modIndis",
      title = "Download modification indexes in excel format!",
      placement = "right",
      trigger = "hover"
    ),
    shinyBS::bsTooltip(
      id = "fit",
      title = "You can download all fit indexes from the output tab!",
      placement = "top",
      trigger = "hover"
    ),

    #titlePanel("CONFIRMATORY FACTOR ANALYSIS (CFA)"),


    h1(id="title", "CONFIRMATORY FACTOR ANALYSIS (CFA)"),
    tags$style(HTML("#title{color: blue; font-family: cursive;font-size:30px;
            font-style: oblique;text-align:left}")),



    sidebarPanel(
      conditionalPanel(

        ## PANEL 1 ##

        condition = "input.panel==0",

        shiny::img(src = "img/rsp2.png", width = "97%"),
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
        ####################################################################
        # imageOutput("image1",width = "15%", height = "30px", inline = TRUE)
        ####################################################################
      ),

      ## PANEL 2 ##

      conditionalPanel(

        condition = "input.panel==1",
        shiny::img(src = "img/rsp2.png", width = "97%"),

        ######################################################################
        # imageOutput("image2",width = "15%", height = "30px", inline = TRUE),
        #####################################################################

        radioButtons(
          "type",
          h3(id="ab","Select the Data Type"),

          choices = list("Polytomous (Likert etc.)" = 1, "1-0 " = 2),
          selected = 1
        ),
        # BUNA GEREK VAR MI "YUKARIDAKİ RADIO BUTTON"????? DEHA
        selectInput(
          "type2",
          h3(id="ab", "Select File Format"),
          choices = list(
            "CSV - Semicolon Separated  Excel" = 1,
            "CSV - Comma  Separated  Excel" = 2,
            "SAV - SPSS" = 3
          ),
          selected = 3
        ),


        uiOutput("uiHeader"),

        fileInput("data1",
                  h3(id="ab","Uplad Data File")

        ),
        gt::gt_output("dat2"),

      ),

      ## PANEL 3 ##

      conditionalPanel(

        condition = "input.panel==2",

        br(),

        shiny::img(src = "img/cfa1.gif", width = "97%"),


        ###################################################################
        # imageOutput("image3",width = "15%", height = "30px", inline = TRUE),
        ###################################################################


        br(),

        textAreaInput("fmodel", h3(id="ab","Please Write Your Model"),
                      "",
                      height = "300px"),
        selectInput(
          "type3",
          h3(id="ab","Selecet Method Used for Prediction"),

          choices = list("MLO" = "ML", "GLS" = "GLS",
                         "WLS" = "WLS","DWLS"= "DWLS" ,
                         "ULS"= "ULS", "MLR" = "MLR"),
          selected = 1
        ),
        actionButton("send", h3(id="ab","Apply")),


        fluidRow(

          column(

            width = 6,

            selectInput(
              "tree",

              h3(id="ab","Graph Type"),

              choices = list("Tree_1" = "tree",

                             "Tree_2" = "tree2",

                             "Circle_1"="circle",

                             "Circle_2"="circle2",

                             "Spring"="spring"),

              selected = "tree2"
            ) ),

          column(
            width = 6,
            selectInput(
              "colour",
              h3(id="ab","Graph Colour"),

              choices = list("Black" = 1,

                             "Red" = 2,

                             "Green"=3,

                             "Blue"=4

              ),

              selected = 4
            ) )
        ),

        plotOutput("path")

      ),

      ## PANEL 4 ##

      conditionalPanel(

        condition = "input.panel==3",

        br(),
        shiny::img(src = "img/rsp2.png", width = "97%"),


        ####################################################################
        # imageOutput("image4",width = "15%", height = "30px", inline = TRUE),
        ####################################################################

        br(),

        ###################################################################
        # imageOutput("image5",width = "15%", height = "30px", inline = TRUE),
        ###################################################################

        shiny::img(src = "img/download.gif", width = "97%"),
      )


    ),



    ## MAIN PANEL

    mainPanel(
      tabsetPanel(
        id = "panel",

        tabPanel(

          h4(id="a", "INTRODUCTION"),


          value = 0,
          br(),
          br(),
          br(),
          fluidRow(

            column(12, align="center",
                   shiny::img(src = "img/rsp2.png", width = "97%"),

                   ###################################################################
                   # imageOutput("image0",width = "75%", height = "50px", inline = TRUE),
                   ###################################################################

            ))),
        tabPanel(
          h4(id="a","DATA UPLOAD"),
          value = 1,
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

          DT::dataTableOutput("dat1"),


          gt::gt_output("mvn1"),

          gt::gt_output("mvn4"),
          br(),

        ),

        tabPanel(
          h4(id="a","STRUCTURAL MODEL"),
          value = 2,
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
          uiOutput("cfaResult"),
          DT::dataTableOutput("cfaDT"),
          uiOutput("fitResult"),
          DT::dataTableOutput("fit"),
          uiOutput("modificationIndex"),
          DT::dataTableOutput("modification")

        ),


        tabPanel(

          h4(id="a","OUTPUT"),

          value = 3,

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
          fluidRow(
            column(3,
                   downloadButton("ozet", h3(id="b", "Download Model Summary"))),
            column(3,
                   downloadButton("modIndis", h3(id="b", "Download Modification Indexes"))),
            column(3, downloadButton("dlPath", h3(id="b", "Download Path Diagram"))),
            column(3, downloadButton("dlFit", h3(id="b", "Download All Fit Indexes")))
          )
        )

      )
    )
  )

  ### SERVER ###

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
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image2<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image3<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "cfa1.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)



    output$image4<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image5<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "download.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)

    ###########################################################################


    output$text1<- renderText({

      if (!is.null(input$data1)) {
        paste0 ( "DATA UPLOAD AND DESCRIPTIVE STATISTICS")
      }

    })

    output$text3<- renderText({

      if (!is.null(input$data1)) {
        paste0 ("DOWNLOAD ANALYSIS OUTPUT")
      }

    })

    output$uiHeader <- renderUI({
      if(input$type2 == 3){
        NULL
      } else {
        checkboxInput("header", h3(id="ab","The first line is the variable name"), value = TRUE)}
    })



    ## DATA UPLOAD ##

    data <- reactive({
      veri <- input$data1
      if (is.null(veri)){
        return(paste0("PLEASE UPLOAD DATA"))
      } else if (input$type2==1){
        if(tools::file_ext(veri$datapath) != "csv"){
          data.frame(uyari = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          utils::read.csv2(veri$datapath, header=isTRUE(input$header), sep = ";")
        }
      } else if(input$type2 == 2){
        if(tools::file_ext(veri$datapath) != "csv"){
          data.frame(uyari = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          utils::read.csv2(veri$datapath, header = isTRUE(input$header), sep = ",")
        }
      } else if(input$type2 == 3) {
        if(tools::file_ext(veri$datapath) != "sav"){
          data.frame(uyari = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          read.spss(veri$datapath, to.data.frame = TRUE, use.value.labels = FALSE)
        }
      }
    })

    data1 <- reactive({
      verix <- data()
      colnames(verix) <- paste0("i", c(1:ncol(verix)))
    })

    # VISUALS #

    output$dfaornek<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "dfaornekgif.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)

    ## USER INTERFACE UPDATE ##

    observeEvent(input$send,{


      output$cfaResult <- renderUI({
        h3(id="ab","CFA Results")
      })
      output$fitResult <- renderUI({
        h3(id="ab","Fit Indexes")
      })
      output$modificationIndex <- renderUI({
        h3(id="ab","Modification Indexes")
      })

      model <- reactive({
        input$fmodel
      })

      Cfa <- reactive({

        data <- data()
        colnames(data) <- paste0("i", c(1:ncol(data)))
        lavaan::cfa(model(), data, estimator = input$type3)

      })

      # OUTPUT TABLE #

      output$cfaDT <- DT::renderDataTable({

        CFA_ENV$CFA1 <- Cfa()
        Cfa <- CFA_ENV$CFA1
           # data <- data()
           # colnames(data) <- paste0("i", c(1:ncol(data)))

        cfaSum <- lavaan::summary(Cfa, standardized = TRUE)
        cfaSum2 <- data.frame(first = cfaSum$pe$lhs,
                              op = cfaSum$pe$op,
                              last = cfaSum$pe$rhs,
                              est = round(cfaSum$pe$est, 3),
                              std.all = round(cfaSum$pe$std.all, 3),
                              sh = round(cfaSum$pe$se, 3),
                              z = round(cfaSum$pe$z, 3),
                              p = round(cfaSum$pe$pvalue, 6))
        cfaSumDT <- print(cfaSum2)
        backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                    "orange", "lightblue", "gray"), 1)
        datatable(cfaSumDT) %>% formatStyle(colnames(cfaSumDT),
                                            backgroundColor = backgroundColor) %>%
          formatStyle('z',
                      backgroundColor = DT::styleInterval(c(-1.960,1.960),
                                                          c(backgroundColor, 'red', backgroundColor)))

      }, options = list(pageLength = 2))

      output$path <- renderPlot({

        Cfa<- Cfa()

        if(!is.null(Cfa)){

          if(Cfa@pta$nvar[[1]] < 9){
            print(semPaths(Cfa, whatLabels = "std", layout  = input$tree,
                           style = "lisrel", rotation = 1, sizeMan = 10,
                           sizeLat = 10, sizeLat2 = 10, sizeInt = 10,
                           nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                           inheritColor = FALSE, edge.label.cex = 1, width = 10,
                           height = 20))
          } else if(Cfa@pta$nvar[[1]] < 13) {
            print(semPaths(Cfa, whatLabels = "std", layout = input$tree,
                           style = "lisrel", rotation = 1, sizeMan = 7,
                           sizeLat = 7, sizeLat2 = 7, sizeInt = 7,
                           nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                           inheritColor = FALSE, edge.label.cex = 0.7, width = 10,
                           height = 20))
          } else {
            print(semPaths(Cfa, whatLabels = "std", layout = input$tree,
                           style = "lisrel", rotation = 1, sizeMan = 3,
                           sizeLat = 3, sizeLat2 = 3, sizeInt = 3, nCharNodes = 3,
                           nCharEdges = 3, edge.color = input$colour,
                           inheritColor = FALSE, edge.label.cex = 0.3, width = 10,
                           height = 20))
          }
        } else{
          return(NULL)
        }

      })

      output$fit <- DT::renderDataTable({

         Cfa<- Cfa()

        indexFit <- fitMeasures(Cfa)
        indexFitx <- cbind(indexFit[["chisq"]], indexFit[["df"]],
                           indexFit[["pvalue"]], indexFit[["rmsea"]],
                           indexFit[["cfi"]], indexFit[["agfi"]],
                           indexFit[["nnfi"]], indexFit["srmr"])
        colnames(indexFitx) <- c("Chi-square", "df", "p", "RMSEA",
                                 "CFI", "AGFI", "NNFI(TLI)", "SRMR")
        rownames(indexFitx) <- "Value"
        paste0("Fit Indexes")
        fitSumDT <- print(round(indexFitx, 3))
        backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                    "aquamarine", "lightblue", "gray"), 1)
        datatable(fitSumDT) %>% formatStyle(colnames(fitSumDT),
                                            backgroundColor = backgroundColor) %>%
          formatStyle('RMSEA',
                      backgroundColor = DT::styleInterval(c(0.05,0.08),
                                                          c("green", "orange", "red"))) %>%
          formatStyle('CFI',
                      backgroundColor = DT::styleInterval(c(0.90,0.95),
                                                          c("red", "orange", "green"))) %>%
          formatStyle('AGFI',
                      backgroundColor = DT::styleInterval(c(0.90,0.95),
                                                          c("red", "orange", "green"))) %>%
          formatStyle('NNFI(TLI)',
                      backgroundColor = DT::styleInterval(c(0.90, 0.95),
                                                          c("red", "orange", "green"))) %>%
          formatStyle('SRMR',
                      backgroundColor = DT::styleInterval(c(0.05,0.08),

                                                          c("green", "orange", "red")))
      })


      output$modification <- DT::renderDataTable({
        Cfa<- Cfa()
        #req( input$start2==1)
        mod <- lavaan::modificationindices(Cfa, sort = TRUE)
        modDT <- data.frame(
          first_Variable = mod$lhs,
          operator = mod$op,
          second_Variable = mod$rhs,
          modification_index = round(mod$mi, 3))
        backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                    "aquamarine", "lightblue", "gray"), 1)
        datatable(modDT) %>% formatStyle(colnames(modDT),
                                         backgroundColor = backgroundColor)

      })


      output$fak2 <- renderUI({

        data <- data()
        colnames(data) <- paste0("i", 1:ncol(data))

        lapply(1:input$fak, function(i) {
          selectInput(paste0('f', i), paste0('Factor', i),
                      choices = names(data), multiple = TRUE)})
      })

      output$text2<- renderText({


        if (!is.null(input$data1)) {
          paste0 ( "CFA RESULTS")
        }
      })
    })

    ## SHOW UPLOADED DATA ##

    output$dat1 <- DT::renderDataTable({


      if (!is.null(input$data1)) {
        data <- data()
        colnames(data) <- paste0("i", c(1:ncol(data)))
        if (ncol(data) <= 15)
        {
          data[1:10, 1:ncol(data)]

        }
        else
        {
          data[1:10, 1:15]

        } }  })

    ## DESCRIPTIVE ##

    output$dat2 <- gt::render_gt(align = "center",{
      if (!is.null(input$data1)) {
        data <- (data())

        N_Item <- ncol(data)
        N <- nrow(data)
        Na<- length(which(is.na(data)))

        res <- data.frame(N_Item, N, Na)

        res<- gt::gt(res)
        br()
        br()
        br()
        res<-res  %>%    tab_header(
          title= md("*Information About Dataset*"))

        res<-res %>%

          tab_style(
            style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

            locations = cells_body()
          )

        res<- res %>%

          cols_width(
            everything() ~ gt::px(120))

        res <- res %>%

          tab_options(
            column_labels.font.size=gt::px(14),
            column_labels.font.weight="bold"
          )

        res <- res %>%

          tab_options(
            heading.title.font.size = gt::px(25))

      }
    })


    ## MULTIVARIATE NORMALITY TEST ##

    #HZ#

    output$mvn1<- gt::render_gt({

      if (!is.null(input$data1)) {

        data<-data()

        hz<-mvn(data=data,mvnTest = "hz")

        hz<-hz$multivariateNormality

        colnames(hz)<-c("Test","Hz","P_value", "Result")

        hz<-gt::gt(hz)

        hz<-hz  %>%    tab_header(
          title= md("*Henze Zirkler Multivariate Nomality Test*"))

        hz<-hz %>%

          tab_style(
            style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

            locations = cells_body()
          )

        hz <- hz %>%

          cols_width(
            everything() ~ gt::px(180))

        hz <- hz %>%

          tab_options(
            column_labels.font.size=gt::px(17),
            column_labels.font.weight="bold"
          )

        hz <- hz %>%

          tab_options(
            heading.title.font.size = gt::px(25))

        return(hz)

      }
    })


    # MARDIA #

    output$mvn4<-gt::render_gt({

      if (!is.null(input$data1)) {

        data<-data()

        mrd<-mvn(data=data,mvnTest = "mardia")

        mrd<-mrd$multivariateNormality

        colnames(mrd)<-c("Test", "Statistic","p_value", "Reult")

        mrd<-mrd[-3,]

        mrd<-gt::gt(mrd)

        mrd<-mrd  %>%    tab_header(
          title= md("*Mardia Multivariate Nomality Test*"))

        mrd<-mrd %>%

          tab_style(
            style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

            locations = cells_body()
          )

        mrd<-mrd %>%

          cols_width(
            everything() ~ gt::px(180))

        mrd <- mrd %>%

          tab_options(
            column_labels.font.size=gt::px(17),
            column_labels.font.weight="bold"
          )

        mrd <- mrd %>%

          tab_options(
            heading.title.font.size = gt::px(25))

        return(mrd)
      }
    })

    ## DOWNLOAD OUTPUT ##

    output$ozet <- downloadHandler(
      filename = function() {
        "summary.csv"
      },
      content = function(file) {
        utils::write.csv2(lavaan::summary(CFA_ENV$CFA1)$pe, file)
      }
    )


    output$modIndis <- downloadHandler(
      filename = function() {
        "modIndeks.csv"
      },
      content = function(file) {

        utils::write.csv2(lavaan::modificationindices(CFA_ENV$CFA1), file)
      }
    )
    output$dlPath <- downloadHandler(


      filename = function() {


        "path.pdf"
      },

      content = function(file) {

        pdf(file)
        if(CFA_ENV$CFA1@pta$nvar[[1]] < 9){
          print(semPlot::semPaths(CFA_ENV$CFA1, whatLabels = "std",
                         layout = input$tree, style = "lisrel", rotation = 1, sizeMan = 10,
                         sizeLat = 10, sizeLat2 = 10, sizeInt = 10,
                         nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                         inheritColor = FALSE, edge.label.cex = 1, width = 10, height = 20))
        } else if(CFA_ENV$CFA1@pta$nvar[[1]] < 13) {


          print(semPlot::semPaths(CFA_ENV$CFA1, whatLabels = "std",
                         layout = input$tree, style = "lisrel", rotation = 1, sizeMan = 7,
                         sizeLat = 7, sizeLat2 = 7, sizeInt = 7,
                         nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                         inheritColor = FALSE, edge.label.cex = 0.7, width = 10, height = 20))
        } else {


          print(semPlot::semPaths(CFA_ENV$CFA1, whatLabels = "std",
                         layout = input$tree, style = "lisrel", rotation = 1, sizeMan = 5,
                         sizeLat = 5, sizeLat2 = 5, sizeInt = 5,
                         nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                         inheritColor = FALSE, edge.label.cex = 0.3, width = 10, height = 20))
        }

        dev.off()
      }
    )

    output$dlFit <- downloadHandler(


      filename = function() {
        "fitIndexes.csv"
      },

      content = function(file) {

        utils::write.csv2(lavaan::fitMeasures(CFA_ENV$CFA1), file)
      }
    )
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  shinyApp(ui = ui, server = server)
}


