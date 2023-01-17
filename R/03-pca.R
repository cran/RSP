#' Run principal component analysis for dichotomous and polytomous data
#' @import foreign
#' @import rJava
#' @importFrom stats cor
#' @importFrom hornpa hornpa
#' @importFrom utils read.csv2 write.csv2
#' @importFrom utils globalVariables
#' @importFrom psych cortest.bartlett KMO tetrachoric principal
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{PCA()}
#' @export

PCA <- function(){
  PCA_ENV <- new.env()
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
  ## USER INTERFACE ##

  ui <- fluidPage(
    useShinyjs(),
    theme = shinytheme("readable"),

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
    ###################################################################################

    tags$style(HTML("#a{color:darkblue; font-family:Lucida Arial ;font-size: 16px;
             font-style: oblique;text-align:center}")), #tabs#

    tags$style(HTML("#ab{color:darkblue; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

    tags$style(HTML("#b{color:darkblue; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #
    ####################################################################################

    ## POP UP ##

    bsTooltip(
      id = "text1",
      title = "Only a small part of the data is presented",
      placement = "bottom",
      trigger = "hover"
    ),
    bsTooltip(
      id = "rotation",
      title = "If the correlation between factors is low,choose varimax method",
      placement = "bottom",
      trigger = "hover"
    ),
    bsTooltip(
      id = "factornumber",
      title = "Determine the number of factors according to the result of the parallel analysis",
      placement = "bottom",
      trigger = "hover"
    ),
    bsTooltip(
      id = "scree_plot",
      title = "You can determine the number of eigen values over the black parallel analysis line as the number of factors",
      placement = "top",
      trigger = "hover"
    ),
    bsTooltip(
      id = "eigen_value",
      title = "Eigen values higher than the pa mean are indicated in red and underlined",
      placement = "top",
      trigger = "hover"
    ),
    bsTooltip(
      id = "fakor",
      title = "When the number of factors is more than 2 in order to see all the results slide the bar below to the right.",
      placement = "bottom",
      trigger = "hover"
    ),
    bsTooltip(
      id = "type",
      title = "Make sure you choose the data type correctly!",
      placement = "top",
      trigger = "hover"
    ),
    bsTooltip(
      id = "tableFactor",
      title = "Items with a lower factor loading than the determined cutting score are indicated in red and underlined",
      placement = "top",
      trigger = "hover"
    ),

    bsTooltip(
      id = "KMo",
      title = "You can examine the change in the KMO value when the items are removed or added.",
      placement = "bottom",
      trigger = "hover"
    ),

    ## TITLE PANEL - SIDE BAR PANEL ##

    # titlePanel("PRINCIPAL COMPONENT ANALYSIS",
    #   windowTitle = "PRINCIPAL COMPONENT ANALYSIS"
    # ),

    h1(id="title", "PRINCIPAL COMPONENT ANALYSIS"),
    tags$style(HTML("#title{color: blue; font-family: Arial;font-size: 35px;
            font-style: oblique;text-align:left}")),

    br(),

    sidebarPanel(
      ## PANEL 1 - INTRODUCTION ##
      conditionalPanel(
        condition = "input.panel==0",
        shiny::img(src = "img/rsp1.png", width = "97%"),
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
        ######################################################################################
        # imageOutput("image1",width = "75%", height = "100px", inline = TRUE),
        ######################################################################################
      ),

      ## PANEL 2 - DATA UPLOAD ##

      conditionalPanel(
        condition = "input.panel==1",
        shiny::img(src = "img/rsp1.png", width = "97%"),

        ###################################################################
        # imageOutput("image2",width = "15%", height = "50px", inline = TRUE),
        ###################################################################


        radioButtons(
          "type",
          h3(id="ab", "Select Data Type"),
          choices = list("Polytomous (Likert etc..)" = 1, "1-0 " = 2),
          selected = 1
        ),
        selectInput(
          "type2",
          h3(id="ab","Select File Format"),
          choices = list(
            "CSV - Semicolon  Separated  Excel" = 1,
            "CSV - Comma  Separated  Excel" = 2,
            "SAV - SPSS" = 3
          ),
          selected = 3
        ),
        uiOutput("uiHeader"),
        fileInput(
          "data1",
          h3(id="ab","Uplad Data File")
        ),
        gt::gt_output("dat3"),
        gt::gt_output("dat3.1")
      ),

      ## PANEL 3 - NUMBER OF FACTORS ##
      conditionalPanel(
        condition = "input.panel==2",
        shiny::img(src = "img/rsp1.png", width = "97%"),

        ###################################################################
        # imageOutput("image3",width = "15%", height = "50px", inline = TRUE),
        ###################################################################
        br(),
        br(),
        uiOutput("factornumber"),
        br(),
        br(),
        gt::gt_output("fakor"),
        br(),
        br(),
        selectizeInput(
          "rotation",
          h3(id="ab","Select Rotation Method"),
          choices = list(
            "Varimax" = "varimax",
            "Direct Oblimin" = "oblimin",
            "No Rotation" = "none"
          ),
          selected = "none"
        ),
      ),

      ## PANEL 4 - FACTOR LOADINGS ##
      conditionalPanel(
        condition = "input.panel==3",
        shiny::img(src = "img/rsp1.png", width = "97%"),

        ###################################################################
        # imageOutput("image4",width = "15%", height = "50px", inline = TRUE),
        ###################################################################


        br(),
        br(),
        br(),

        gt::gt_output("KMo"),
        br(),
        br(),
        uiOutput("remove_item"),
        br(),
        br(),
        uiOutput("select_item"),

        # ##################### NEW KMO 1 #################

        br(),
        br(),

        sliderInput(
          "cut_off",
          h3(id="ab","Select cut-off value for Factor Loadings"),
          min = 0.25,
          max = 0.60,
          step = 0.05,
          value = 0.30
        ),
      ),

      ## PANEL 5 - OUTPUT ##

      conditionalPanel(
        condition = "input.panel==4",
        br(),
        shiny::img(src = "img/rsp1.png", width = "97%"),

        ###################################################################
        # imageOutput("image5",width = "15%", height = "50px", inline = TRUE),
        ###################################################################
        br(),

        shiny::img(src = "img/download.gif", width = "97%"),

        br(),
        ###################################################################
        # imageOutput("image6",width = "100%", height = "50px", inline = TRUE),
        ###################################################################

        # uiOutput("grafmadde")
      ),
    ),
    # close sidebar panel

    ##  MAIN PANEL ##

    mainPanel(
      tabsetPanel(
        id = "panel",

        ## MAIN PANEL  1 ##
        tabPanel(

          # h4("INTRODUCTION"),


          h4(id="a", "INTRODUCTION"),


          value = 0,
          br(),
          br(),
          br(),

          ###################################################################
          # imageOutput("image0",width = "15%", height = "50px", inline = TRUE),
          ###################################################################

          fluidRow(
            column(
              12,
              align = "center",
              shiny::img(src = "img/rsp1.png", width = "97%")
            )
          )
        ),

        ##  MAIN PANEL 2 ##
        tabPanel(
          # h4( "DATA UPLOAD"),

          h4(id="a", "DATA UPLOAD"),
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
          withLoader(
            DT::dataTableOutput("dat1"),
            type = "html",
            loader = "loader1"
          ),
          br(),
          textOutput("text1_1"),
          tags$head(
            tags$style(
              "#text1_1{
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
          gt::gt_output("dat2"),
          br(),
          gt::gt_output("dat4"),
        ),

        ## MAIN PANEL 3 ##
        tabPanel(
          # h4("DETERMINING THE NUMBER OF FACTORS"),

          h4(id="a", "DETERMINING THE NUMBER OF FACTORS"),

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
          br(),
          br(),
          withLoader(plotOutput("scree_plot"), type = "html", loader = "loader1"),
          gt::gt_output("eigen_value"),
          br()
        ),

        ## MAIN PANEL 4 ##
        tabPanel(

          # h4( "FACTOR LOADINGS AND EXPLAINED VARIANCE"),

          h4 (id="a", "FACTOR LOADINGS AND EXPLAINED VARIANCE"),
          value = 3,
          textOutput("text2_1"),
          tags$head(
            tags$style(
              "#text2_1{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          gt::gt_output("tableFactor"),
          gt::gt_output("buton"),
          br(),
          gt::gt_output("tableEigen"),
          gt::gt_output("buton2")
        ),

        ## MAIN PANEL 5 ##
        tabPanel(
          # h4("OUTPUT"),


          h4(id="a", "OUTPUT"),

          value = 4,
          br(),
          br(),

          fluidRow(
            column(4, downloadButton(
              "factorDownload",
              h1(id="b",  "DOWNLOAD THE FACTOR LOADINGS"),
            )),
            column(8, downloadButton(
              "varianceDownload",
              h1(id="b", "DOWNLOAD THE EIGEN VALUES AND EXPLAINED VARIANCE")
            ))
          )
        )
      ) # close tabsetpanel
    ) #  close mainpanel
  ) #  close fluidpage


  ## SERVER ##

  server <- function(input, output, session) {





    # utils::globalVariables(c("."))

    # if(getRversion() >= "2.15.1")  utils::globalVariables(c("factor1", "othervar"))

    #  if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


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
      list(src = "rsp1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image2<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image3<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp1.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image4<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image5<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp1.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image6<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "download.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)

    ###########################################################################




    ##  DATA UPLOAD ##
    output$uiHeader <- renderUI({
      if (input$type2 == 3) {
        NULL
      } else {
        checkboxInput("header",
                      "The first line is the variable name",
                      value = TRUE
        )
      }
    })

    data <- reactive({
      veri <- input$data1
      if (is.null(veri)) {
        return(paste0("PLEASE UPLOAD DATA"))
      } else if (input$type2 == 1) {
        if (tools::file_ext(veri$datapath) != "csv") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          utils::read.csv2(veri$datapath,
                           header = isTRUE(input$header),
                           sep = ";"
          )
        }
      } else if (input$type2 == 2) {
        if (tools::file_ext(veri$datapath) != "csv") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          utils::read.csv2(veri$datapath,
                           header = isTRUE(input$header),
                           sep = ","
          )
        }
      } else if (input$type2 == 3) {
        if (tools::file_ext(veri$datapath) != "sav") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          read.spss(veri$datapath,
                    to.data.frame = TRUE,
                    use.value.labels = FALSE
          )
        }
      }
    })

    ## WIDGETS FOR UIOUTPUTS ##

    output$factornumber <- renderUI({
      req(input$data1)
      sliderInput(
        "fak2",
        h3(id="ab","Define the Number of Factors"),
        min = 1,
        max = ncol(data()),
        step = 1,
        value = 1
      )
    })

    output$select_item <- renderUI({
      if (!is.null(input$data1)) {
        madism <- (1:ncol(data()))
        madisimGL <- madism
        selectInput("grafmad",
                    h3(id="ab","Select Item Number"),
                    choices = madism,
                    multiple = TRUE
        )
      }
    })

    output$remove_item <- renderUI({
      actionButton("remove", h3(id="ab","Remove Selected Item/s"))
    })

    ##  TEXTS FOR MAIN PANELS ##
    output$text1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA UPLOAD AND BASIC STATISTICS")
      }
    })

    output$text1_1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA WAS UPLOADED SUCCESSFULLY")
      }
    })

    output$text2 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DEFINING NUMBER OF FACTORS -
                PARALLEL ANALYSIS - EIGEN VALUES")
      }
    })

    output$text2_1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("FACTOR LOADINGS AND EXPLAINED VARIANCE")
      }
    })

    output$dat1 <- DT::renderDataTable({
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        colnames(data) <- paste0("item", 1:ncol(data))
        if (dim(data)[2] == 1) {
          data.frame(WARNING = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          if (ncol(data) <= 15) {
            data[1:10, 1:ncol(data)]
          } else {
            data[1:10, 1:15]
          }
        }
      }
    })

    ## DESCRIPTIVES ##

    output$dat2 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- data()
        NUMBER_OF_ITEMS <- ncol(data)
        NUMBER_OF_RESPONDENTS <- nrow(data)
        NUMBER_OF_BLANK_ITEMS <- length(which(is.na(data)))

        res <- data.frame(
          NUMBER_OF_ITEMS,
          NUMBER_OF_RESPONDENTS,
          NUMBER_OF_BLANK_ITEMS
        )
        res <- gt::gt(res)
        br()
        br()
        br()
        res <- res %>%
          tab_header(title = md("*Basic Statistics About the Data Set*"))

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(
            c(NUMBER_OF_ITEMS) ~ gt::px(300),
            everything() ~ gt::px(300)
          )

        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))
      }
    })

    ## BARTLET - KMO ##

    output$dat3 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        BR <- psych::cortest.bartlett(data)
        Bartlett_Chi_Square <- BR$chisq
        p_value <- round(BR$p.value, 4)
        df <- BR$df
        if (input$type == 1) {
          cor_matrix <- stats::cor(data)
          kmo <- psych::KMO(cor_matrix)$MSA
        } else {
          tet_matrix <- tetrachoric(data)$rho
          kmo <- KMO(tet_matrix)$MSA
        }

        res <- data.frame(KMO = kmo, Bartlett_Chi_Square, p_value, df)
        res <- gt::gt(res)
        br()
        br()
        br()
        res <- res %>%
          tab_header(
            title =
              md("*KMO Test - Bartlett's Homogeneity of Variance Test*")
          )

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(
            c(Bartlett_Chi_Square) ~ gt::px(200),
            c(p_value) ~ gt::px(150),
            c(df) ~ gt::px(150),
            everything() ~ gt::px(200)
          )

        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))
      }
    })

    ## DETERMINANT ##

    output$dat4 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        if (input$type == 1) {
          cor_matrix1 <- stats::cor(data)
        } else {
          cor_matrix1 <- tetrachoric(data)$rho
        }

        Determinant <- det(cor_matrix1)
        total <- rowSums(data)
        skewness <- function(x) {
          numerator <- sum((x - mean(x))^3)
          denominator <- length(x) * (sd(x)^3)
          result <- numerator / denominator
          return(result)
        }

        kurtosis <- function(x) {
          numerator <- sum((x - mean(x))^4)
          denominator <- length(x) * (sd(x)^4)
          result <- (numerator / denominator) - 3
          return(result)
        }

        Skewness <- skewness(total)
        Kurtosis <- kurtosis(total)
        res <- data.frame(
          DETERMINANT = Determinant,
          SKEWNESS = Skewness,
          KURTOSIS = Kurtosis
        )

        res <- gt::gt(res)
        br()
        br()
        br()

        res <- res %>%
          tab_header(title = md(""))

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(everything() ~ gt::px(300))

        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))
      }
    })

    ## NUMBER OF FACTORS ##

    # scree Plot #

    output$scree_plot <- renderPlot({

      if (!is.null(input$data1)) {
        data <- na.omit(data())

        ## For 1-0 Data ##

        if (input$type == 1) {
          cor_matrix <- stats::cor(data)

          model1 <-
            principal(cor_matrix,
                      nfactors = ncol(data),
                      rotate = "none"
            )
        }

        if (input$type == 2) {
          tet_matrix <- tetrachoric(data)$rho

          model1 <-
            principal(tet_matrix,
                      nfactors = ncol(data),
                      rotate = "none"
            )
        }

        ## PARALLEL ANALYSIS ##

        set.seed <- 123
        horn <- hornpa(
          k = ncol(data),
          size = nrow(data),
          reps = 200
        )
        PA_MEAN <- horn$Mean

        plot(
          model1$values,
          type = "b",
          col = 2,
          lty = 1,
          lwd = 1.5,
          main = " SCREE PLOT AND PARALLEL ANALYSIS",
          xlab = "Number of Factors",
          ylab = " Eigenvalue"
        )

        lines(
          PA_MEAN,
          type = "b",
          col = 1,
          lty = 2,
          lwd = 1.5
        )

        legend(
          "topright",
          legend = c("PA Mean", "Eigenvalue"),
          col = 1:2,
          lty = 1:2,
          lwd = 1.5
        )
      }
    })

    ## Eigen Value ##

    output$eigen_value <- render_gt({
      if (!is.null(input$data1)) {
        set.seed <- 123
        data <- na.omit(data())

        if (input$type == 1) {
          cor_matrix <- stats::cor(data)
          model1 <-
            principal(cor_matrix,
                      nfactors = ncol(data),
                      rotate = "none"
            )
        } else {
          tet_matrix <- tetrachoric(data)$rho

          model1 <-
            principal(tet_matrix,
                      nfactors = ncol(data),
                      rotate = "none"
            )
        }

        eigenvalue <- unname(model1$Vaccounted[1, ]) ## CHECK
        eigenvalue <- round(eigenvalue, 4)

        horn <- hornpa::hornpa(
          k = ncol(data),
          size = nrow(data),
          reps = 200
        )
        PA_MEAN <- horn$Mean
        COMPONENT <- 1:ncol(data)
        res <- data.frame(COMPONENT, PA_MEAN, EIGENVALUE = eigenvalue)

        res <- gt::gt(res)
        res <- res %>%
          tab_header(title = md("**EIGENVALUES AND PARALLEL ANALYSIS**"))

        res <- res %>%
          tab_options(
            heading.title.font.size = gt::px(25),
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.20
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(everything() ~ gt::px(180))

        res <- res %>%
          gt::tab_style(
            style = gt::cell_text(
              weight = "bolder",
              color = "red",
              decorate = "underline",
              stretch = "extra-expanded"
            ),
            locations = gt::cells_body(
              columns = c(EIGENVALUE),
              rows = EIGENVALUE > PA_MEAN
            )
          )
        return(res)
      }
    })

    ## Correlation Among Factors ##

    output$fakor <- render_gt({
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        if (input$type == 1) {
          cor_matrix <- stats::cor(data)
          model1 <- principal(cor_matrix,
                              nfactors = input$fak2,
                              rotate = "oblimin"
          )
        } else {
          tet_matrix <- tetrachoric(data)$rho
          model1 <- principal(tet_matrix,
                              nfactors = input$fak2,
                              rotate = "oblimin"
          )
        }

        fac <- model1$r.scores
        Row <- paste("Factor", 1:input$fak2)

        colnames(fac) <- paste("Factor", 1:input$fak2)

        fac1 <- data.frame(FACTORS = Row, fac)

        fac1 <- gt::gt(fac1)

        fac1 <- fac1 %>% tab_header(title = md("**CORRELATION AMONG FACTORS**"))

        fac1 <- fac1 %>%
          gt::cols_width(everything() ~ gt::px(180))

        fac1 <- fac1 %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1), alpha =
                0.20
            ),
            locations = gt::cells_body()
          )

        fac1 <- fac1 %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        return(fac1)
      }
    })


    ## FACTOR LOADINGS ##

    output$tableFactor <- render_gt(align = "center", {
      if (!is.null(input$data1)) {

        data <- na.omit(data())


        if (input$type == 1) {
          cor_matrix <- stats::cor(data)


          result <- principal(cor_matrix,
                              nfactors = input$fak2,
                              rotate = input$rotation
          )
        } else {
          tet_matrix <- tetrachoric(data)$rho


          result <- principal(tet_matrix,
                              nfactors = input$fak2,
                              rotate = input$rotation
          )
        }


        eigenvalue <- unname(result$Vaccounted[1, ])

        rnames2 <- character()
        for (i in 1:length(eigenvalue)) {
          rnames2[i] <- paste("item", i)
        }

        nn <- ncol(data)
        nfac <- input$fak2
        total <- nn * nfac
        empty_mat <- matrix(NA, nn, nfac)

        if (nfac == 1) {
          if (input$type == 2) {
            empty_mat <- as.matrix(result$loadings[1:total])
          } else {
            empty_mat <- as.matrix(result[[5]][1:total])
          }
        } else {
          if (input$type == 1) {
            for (i in 1:total) {
              if (input$type == 2) {
                empty_mat[i] <- as.matrix(result$loadings[i:total])
              } else {
                empty_mat[i] <- result[[5]][i:total]
              }
            }
          } else {
            for (i in 1:total) {
              empty_mat[i] <- result$loadings[i:total]
            }
          }
        }

        namefac <- NULL

        for (k in 1:nfac) {
          namefac[k] <- paste0("factor", k)
        }

        item <- 1:ncol(data)

        item <- round(item)

        empty_mat <- round(empty_mat, 2)

        colnames(empty_mat) <- namefac[1:nfac]

        fload <- cbind(item, empty_mat)

        fload <- as.data.frame(fload)

        PCA_ENV$factorLoading <- fload

        fload <- gt::gt(fload)



        fload <- fload %>% tab_header(title = md("**FACTOR LOADINGS**"))

        ## ARRANGE COLUMNS ACCORDIG TO NUMBER OF FACTORS ##


        if (input$fak2 > 3) {
          fload <- fload %>%
            gt::cols_width(everything() ~ gt::px(120))
        }

        if (input$fak2 <= 3) {
          fload <- fload %>%
            gt::cols_width(everything() ~ gt::px(180))
        }

        ## HIGHLIGHT LOW FACTOR LOADINGS ##

        if (input$fak2 == 1) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < input$cut_off
              )
            )
        }

        if (input$fak2 == 2) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < input$cut_off
              )
            )
        }

        if (input$fak2 == 3) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )
        }



        if (input$fak2 == 4) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 5) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 6) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 7) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < input$cut_off
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor7),
                rows = abs(factor7) < abs(input$cut_off)
              )
            )
        }

        fload <- fload %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1), alpha =
                0.20
            ),
            locations = gt::cells_body()
          )

        fload <- fload %>%
          tab_options(
            column_labels.font.size = gt::px(20),
            column_labels.font.weight = "bolder"
          )

        return(fload)
      }
    })

    ## EXPAINED VARIANCE ##

    add <- reactive({
      data <- na.omit(data())
      if (!is.null(input$data1)) {
        if (input$type == 1) {
          cor_matrix <- stats::cor(data)

          result <- principal(cor_matrix,
                              nfactors = input$fak2,
                              rotate = input$rotation
          )
        } else {
          tet_matrix <- tetrachoric(data)$rho

          result <- principal(tet_matrix,
                              nfactors = input$fak2,
                              rotate = input$rotation
          )
        }

        result <- result$Vaccounted

        result <- result[-c(4, 5), ]

        result <- unname(result)

        nfac <- input$fak2

        name <- matrix(NA, 3, nfac)

        Col <- paste("Factor", 1:nfac)

        Row <- c(
          "Eigenvalue",
          "Explained Variance",
          "Cummilative Explained Variance"
        )

        colnames(name) <- Col

        if (nfac == 1) {
          name <- result
          Row <- c("Eigenvalue", "Explained Variance")
        } else {
          for (i in 1:nfac) {
            name[, i] <- result[, i]
          }
        }
      }

      req(input$data1)

      add <- data.frame(Statistic = Row, name)
    })


    output$tableEigen <- render_gt(align = "center", {
      PCA_ENV$explainedVar <- add()

      add <- gt::gt(add())

      add <- add %>% tab_header(
        title =
          md("**EIGENVALUE AND EXPLAINED VARIANCE**")
      )

      if (input$fak2 > 3) {
        add <- add %>%
          gt::cols_width(everything() ~ gt::px(120))
      }

      if (input$fak2 <= 3) {
        add <- add %>%
          gt::cols_width(everything() ~ gt::px(180))
      }

      add <- add %>%
        gt::tab_style(
          style = cell_fill(
            color = sample(colors()[3:100], 1), alpha =
              0.20
          ),
          locations = gt::cells_body()
        )

      add <- add %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold",
          row_group.font.weight = "bolder"
        )

      return(add)
    })

    ## reactive 1

    remainData <- reactive({
      omitted <- input$grafmad
      All <- 1:ncol(data()) # madisimGL
      dataGL <- na.omit(data())
      colnames(dataGL) <- All
      kalan <- setdiff(All, omitted)
      remainData <- dataGL[, kalan]
    })

    ## reactive 2

    model_removed <- reactive({
      if (input$type == 1) {
        remainCorMatrix <- stats::cor(remainData())

        modelRemain <-
          principal(remainCorMatrix,
                    nfactors = input$fak2,
                    rotate = input$rotation
          )
      } else {
        remainTetMatrix <- tetrachoric(remainData())$rho

        modelRemain <-
          principal(remainTetMatrix,
                    nfactors = input$fak2,
                    rotate = input$rotation
          )
      }
    })


    ############################### testttttttttt ###################

    output$KMo<-render_gt({

      if (!is.null(input$data1)) {

        if (input$type == 1) {

          korr<-stats::cor(remainData())

          res<-KMO(korr)[[1]]


        } else

        {

          korr<- tetrachoric(remainData())$rho

          res<-KMO(korr)[[1]]

        }


        res <- data.frame(KMO = res)

        res <- gt::gt(res)

        res <- res %>%
          tab_header(
            title =
              md("*Current KMO Test Result*")
          )

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        # res <- res %>%
        #   tab_options(
        #     column_labels.font.size = gt::px(17),
        #     column_labels.font.weight = "bold"
        #   )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))

        return(res)
      }
    })


    ############################### testttttttttt ###################


    ## OBSERVE EVENT - RE-COMPUTATION AFTER OMITTED items ##

    nfacRemain <- reactive({
      nfacRemain <- input$fak2
    })

    observeEvent(input$remove, {
      shinyjs::hide("tableFactor")
      shinyjs::hide("tableEigen")

      output$buton <- render_gt(align = "center", {
        remainEigenvalue <- unname(model_removed()$Vaccounted[1, ])

        rnames3 <- character()
        for (i in 1:length(remainEigenvalue)) {
          rnames3[i] <- paste0("Item", i)
        }
        remainN <- ncol(remainData())

        remainTotal <- remainN * nfacRemain()

        remainEmpty <- matrix(NA, remainN, nfacRemain())

        if (input$type == 2) {
          for (i in 1:remainTotal) {
            remainEmpty[i] <- model_removed()$loadings[i]
          }
        } else {
          for (i in 1:remainTotal) {
            remainEmpty[i] <- model_removed()[[5]][i]
          }
        }

        remainFactorName <- NULL
        for (k in 1:nfacRemain()) {
          remainFactorName[k] <- paste0("factor", k)
        }

        items <- 1:ncol(remainData())
        items <- round((items), 2)

        remainEmpty <- round(remainEmpty, 2)

        #

        omitted <- input$grafmad
        All <- 1:ncol(data())
        dataGL <- na.omit(data())
        colnames(dataGL) <- All
        remain_Items <- setdiff(All, omitted)

        #

        colnames(remainEmpty) <- remainFactorName[1:nfacRemain()]

        floadRemain <- cbind(remain_Items, items, remainEmpty)

        floadRemain <- as.data.frame(floadRemain)

        PCA_ENV$factorLoadRemain <- floadRemain
        floadRemain <- gt::gt(floadRemain)



        floadRemain <- floadRemain %>% tab_header(
          title =
            md("**FACTOR LOADINGS**")
        )

        ## Column Arrangement According to Factor Numbers ##

        if (input$fak2 > 3) {
          floadRemain <- floadRemain %>%
            gt::cols_width(everything() ~ gt::px(120))
        }

        if (input$fak2 <= 3) {
          floadRemain <- floadRemain %>%
            gt::cols_width(everything() ~ gt::px(180))
        }


        ## UNDERLINELOW FACTOR LOADINGS ##

        if (input$fak2 == 1) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < input$cut_off
              )
            )
        }


        if (input$fak2 == 2) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < input$cut_off
              )
            )
        }


        if (input$fak2 == 3) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )



          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )
        }



        if (input$fak2 == 4) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )
        }


        if (input$fak2 == 5) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 6) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )



          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 7) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor7),
                rows = abs(factor7) < abs(input$cut_off)
              )
            )
        }

        #

        floadRemain <- floadRemain %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1), alpha =
                0.20
            ),
            locations = gt::cells_body()
          )

        floadRemain <- floadRemain %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        return(floadRemain)
      })

      ## REMOVED EXPLAINED VARIANCE ##


      ad.var.2 <- reactive({

        # align sola sabitlendi
        if (input$type == 2) {
          modelRemain.var <- model_removed()$loadings
        }
        modelRemain.var <- model_removed()$Vaccounted
        modelRemain.var <- modelRemain.var[-c(4, 5), ]
        modelRemain.var <- unname(modelRemain.var)
        ad.var <- matrix(NA, 3, nfacRemain())
        sutun <- paste0("Factor", 1:nfacRemain())
        satir <-
          c(
            "Eigenvalue",
            "Explained Variance",
            "Cummilative Explained Variance"
          )
        if (nfacRemain() == 1) {
          satir <- c("Eigenvalue", "Explained Variance")
        } else {
          satir <-
            c(
              "Eigenvalue",
              "Explained Variance",
              "Cummilative Explained Variance"
            )
        }
        colnames(ad.var) <- sutun
        if (nfacRemain() == 1) {
          ad.var <- modelRemain.var
        } else {
          for (i in 1:nfacRemain()) {
            ad.var[, i] <- modelRemain.var[, i]
          }
        }

        ad.var.2 <- data.frame(Statistics = satir, ad.var)
      })

      output$buton2 <-
        render_gt(align = "center", {



          # gt table modifying 2
          PCA_ENV$explainedVarRemain <- ad.var.2()
          ad.var.2 <- gt::gt(ad.var.2())

          ad.var.2 <- ad.var.2 %>% tab_header(
            title =
              md("**EIGENVALUE AND EXPLAINED VARIANCE**")
          )

          if (input$fak2 > 3) {
            ad.var.2 <- ad.var.2 %>%
              gt::cols_width(everything() ~ gt::px(120))
          }

          if (input$fak2 <= 3) {
            ad.var.2 <- ad.var.2 %>%
              gt::cols_width(everything() ~ gt::px(180))
          }

          ad.var.2 <- ad.var.2 %>%
            gt::tab_style(
              style = cell_fill(
                color = sample(colors()[3:100], 1),
                alpha = 0.20
              ),
              locations = gt::cells_body()
            )

          ad.var.2 <- ad.var.2 %>%
            tab_options(
              column_labels.font.size = gt::px(17),
              column_labels.font.weight = "bold"
            )

          return(ad.var.2 <- ad.var.2)
        })
    }) ##  close observe event

    ## DOWNLOAD OUTPUTS ##

    output$factorDownload <- downloadHandler(


      filename = function() {
        "factor-loadings.csv"
      },
      content = function(file) {
        if (is.null(input$grafmad)) {
          utils::write.csv2(PCA_ENV$factorLoading, file)
        } else {
          utils::write.csv2(PCA_ENV$factorLoadRemain, file)
        }
      }
    )

    output$varianceDownload <- downloadHandler(

      filename = function() {
        "varyans.csv"
      },
      content = function(file) {
        if (is.null(input$grafmad)) {
          utils::write.csv2(PCA_ENV$explainedVar, file)
        } else {
          utils::write.csv2(PCA_ENV$explainedVarRemain, file)
        }
      }
    )
    session$onSessionEnded(function() {
      stopApp()
    })


    factor1<-factor2<-factor3<-factor4<-factor5<-factor6<-factor7<-EIGENVALUE<-NULL
    factorLoading <- factorLoadRemain <- NULL
    explainedVar <- explainedVarRemain <- NULL

  }

  shinyApp(ui = ui, server = server)
}



