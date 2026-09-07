
#' Generate simulated data according to IRT for dichotomous and polytomous data
#' Generate multidimensional data for factor analysis
#' # param options(java.parameters = "-Xmx8000m")
#' @import shiny
#' @importFrom Metrics bias rmse
#' @importFrom catR genPolyMatrix genPattern
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{SIMDATA()}
#' @export


SIMDATA <- function(){
  SIMDATA_ENV <- new.env()

  options(java.parameters = "-Xmx8000m")

  js <- "
$(document).on('shiny:sessioninitialized', function(event) {
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
  Shiny.onInputChange('myBrowser', navigator.sayswho);
});
"

  theme_css <- "
    :root {
      --tc:  #6B3FA0;
      --tdk: #2d1b46;
      --tlt: #b48cf0;
      --tpl: #d7b8ff;
      --tls: #f3ebff;
    }

    body { background:#ffffff !important; color:#1a1a2e !important;
           font-family:'Segoe UI',Arial,sans-serif; }

    /* ---- Visual cards borrowed from PCA and adapted for DATA GENERATION ---- */
    .sim-sidebar-card, .sim-sidebar-card * { box-sizing:border-box; }
    .sim-sidebar-card {
      width:100%; height:132px; min-height:132px; max-height:132px;
      margin:0 0 12px 0; padding:12px 14px;
      position:relative; overflow:hidden; border-radius:16px;
      background:linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--tc) 10%, #ffffff) 56%, color-mix(in srgb, var(--tc) 19%, #ffffff) 100%);
      border:1px solid color-mix(in srgb, var(--tc) 22%, #ffffff);
      box-shadow:0 10px 24px rgba(107, 63, 160, .12);
      color:#1a1030;
    }
    .sim-sidebar-card:before {
      content:''; position:absolute; right:-38px; bottom:-64px;
      width:148px; height:118px; border-radius:56% 44% 0 0;
      background:linear-gradient(135deg, color-mix(in srgb, var(--tc) 20%, #ffffff), var(--tc));
      opacity:.55; transform:rotate(-8deg);
    }
    .sim-sidebar-card:after {
      content:''; position:absolute; right:10px; top:10px; width:48px; height:48px;
      background-image:radial-gradient(var(--tc) 1.7px, transparent 2px);
      background-size:12px 12px; opacity:.18;
    }
    .sim-sidebar-content { position:relative; z-index:2; }
    .sim-sidebar-badge {
      display:inline-flex; align-items:center; gap:6px; max-width:100%;
      padding:4px 8px; border-radius:999px;
      background:rgba(255,255,255,.74);
      border:1px solid color-mix(in srgb, var(--tc) 18%, #ffffff);
      color:var(--tdk); font-size:11px; font-weight:850; letter-spacing:.15px;
      white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
      margin-bottom:9px;
    }
    .sim-sidebar-badge-mark {
      width:13px; height:13px; border-radius:4px; flex:0 0 13px;
      background:linear-gradient(135deg, var(--tc), color-mix(in srgb, var(--tc) 48%, #ffffff));
      position:relative;
    }
    .sim-sidebar-badge-mark:after {
      content:''; position:absolute; inset:3.5px; border:1.3px solid #fff; border-radius:2.5px;
    }
    .sim-sidebar-lines { margin-top:6px; display:grid; gap:4px; max-width:94%; }
    .sim-sidebar-lines div {
      color:#1a1030; font-weight:900; font-size:13px; line-height:1.2;
      letter-spacing:.08px; white-space:normal; overflow:visible; text-overflow:clip;
    }
    .sim-sidebar-lines div:nth-child(2) { color:var(--tc); }
    .sim-sidebar-credit {
      position:absolute; right:10px; bottom:8px; z-index:2;
      color:color-mix(in srgb, var(--tc) 72%, #241237);
      font-size:9.4px; font-weight:800; letter-spacing:.1px; white-space:nowrap;
    }

    .sim-hero, .sim-hero * { box-sizing:border-box; }
    .sim-hero {
      width:97%; min-height:348px; margin:0 auto 16px auto;
      position:relative; overflow:hidden; border-radius:22px;
      padding:30px 34px;
      background:
        radial-gradient(circle at 88% 92%, color-mix(in srgb, var(--tc) 16%, transparent) 0, transparent 33%),
        linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--tc) 5%, #ffffff) 62%, color-mix(in srgb, var(--tc) 12%, #ffffff) 100%);
      border:1px solid color-mix(in srgb, var(--tc) 18%, #ffffff);
      box-shadow:0 18px 42px rgba(107, 63, 160, .10);
      color:#1a1030;
    }
    .sim-hero:after {
      content:''; position:absolute; right:22px; top:18px; width:62px; height:62px;
      background-image:radial-gradient(var(--tc) 1.8px, transparent 2.3px);
      background-size:14px 14px; opacity:.20;
    }
    .sim-hero-left { position:relative; z-index:2; padding-top:4px; }
    .sim-hero-brand { display:flex; align-items:center; gap:18px; margin-bottom:8px; }
    .sim-hero-logo {
      width:78px; height:78px; display:flex; flex-direction:column; align-items:center; justify-content:center;
      clip-path:polygon(25% 5%,75% 5%,100% 50%,75% 95%,25% 95%,0 50%);
      border:3px solid var(--tc); background:rgba(255,255,255,.68);
      color:#1a1030; font-weight:900; line-height:1.06; box-shadow:0 10px 24px rgba(107,63,160,.08);
    }
    .sim-hero-logo b { color:var(--tc); font-size:22px; letter-spacing:-.8px; }
    .sim-hero-logo span { font-size:9.5px; }
    .sim-hero-brand-text { border-left:1px solid rgba(80,120,160,.18); padding-left:20px; }
    .sim-hero-pkg { font-size:20px; font-weight:900; margin-bottom:4px; }
    .sim-hero-domain { font-size:15px; color:#5a4a6e; font-weight:650; }
    .sim-hero-domain b { color:var(--tc); }
    .sim-hero-title {
      margin:0; font-size:clamp(28px, 3.8vw, 24px); line-height:1.02;
      font-weight:950; letter-spacing:-1.1px; color:#1a1030;
      text-align:center; max-width:100%;
    }
    .sim-hero-title .sim-title-top { display:block; color:#1a1030; }
    .sim-hero-title .sim-title-sub {
      display:block; text-align:center; margin-top:8px;
      color:var(--tc); font-size:.68em; letter-spacing:.5px; line-height:1.18;
    }
    .sim-hero-desc {
      max-width:760px; margin:14px auto 0 auto; text-align:center;
      font-size:14px; line-height:1.35; color:#5a4a6e; font-weight:500;
    }
    .sim-hero-bottom {
      position:relative; width:100%; height:112px; margin-top:16px;
      display:flex; justify-content:space-around; align-items:center;
      background:rgba(255,255,255,.70);
      border:1px solid color-mix(in srgb, var(--tc) 16%, #ffffff);
      border-radius:16px; box-shadow:0 10px 24px rgba(107,63,160,.05);
      padding:0;
    }
    .sim-hero-feature {
      flex:1 1 25%; height:100%; display:flex; flex-direction:column;
      align-items:center; justify-content:center; gap:8px;
      color:#1a1030; font-size:11px; font-weight:900; line-height:1.2;
      border-right:1px solid rgba(80,120,160,.12);
    }
    .sim-hero-feature:last-child { border-right:0; }
    .sim-hero-icon {
      width:34px; height:34px; border-radius:12px; display:flex; align-items:center; justify-content:center;
      background:color-mix(in srgb, var(--tc) 14%, #ffffff);
      border:1px solid color-mix(in srgb, var(--tc) 28%, #ffffff);
      color:var(--tc); font-size:18px; font-weight:900;
    }
    .sim-hero-credit {
      position:absolute; right:28px; top:20px; color:color-mix(in srgb, var(--tc) 80%, #241237);
      font-size:11px; font-weight:800; z-index:2;
    }
    @media (max-width: 768px) {
      .sim-hero { width:100%; padding:22px 18px; min-height:unset; }
      .sim-hero-brand { gap:12px; align-items:flex-start; }
      .sim-hero-brand-text { padding-left:14px; }
      .sim-hero-bottom { height:auto; flex-wrap:wrap; padding:8px 0; }
      .sim-hero-feature { min-width:50%; padding:12px 8px; border-right:0; }
    }

    /* ---- Tabs ---- */
    .nav-tabs > li > a {
      color: var(--tdk) !important; font-weight:600;
      border-radius:8px 8px 0 0 !important;
      background:#f7f4ff !important;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background:var(--tc) !important; color:#fff !important;
      border-color:var(--tc) !important;
    }
    .nav-tabs > li > a:hover { background:var(--tls) !important; color:var(--tdk) !important; }

    /* ---- Sidebar ---- */
    .well { background:#f7f4ff !important; border:1.5px solid var(--tpl) !important;
            border-radius:12px !important; }

    /* ---- ALL BUTTONS uniform size + colour ---- */
    .btn, .action-button, .bttn,
    .btn-primary, .btn-default, .btn-file,
    .bttn-primary, .bttn-default, .bttn-jelly,
    .bttn-unite, .bttn-material-flat {
      min-width:160px !important;
      height:38px !important;
      font-size:13px !important;
      font-weight:700 !important;
      border-radius:8px !important;
      padding:0 14px !important;
      display:inline-flex !important;
      align-items:center !important;
      justify-content:center !important;
      transition:background .2s, box-shadow .15s !important;
      box-sizing:border-box !important;
    }

    /* inactive state: white bg readable */
    .btn-default, .btn-group > .btn:not(.active) {
      background:#ffffff !important;
      border:2px solid var(--tpl) !important;
      color:var(--tdk) !important;
    }
    .btn-group > .btn:not(.active):hover {
      background:var(--tls) !important; color:var(--tdk) !important;
    }

    /* primary / active state */
    .btn-primary,
    .btn-group > .btn.active,
    .bttn-primary, .bttn-jelly.bttn-primary,
    .bttn-unite.bttn-primary,
    .bttn-material-flat.bttn-primary {
      background:var(--tc) !important;
      border:2px solid var(--tdk) !important;
      color:#fff !important;
      box-shadow:0 2px 6px rgba(107,63,160,.35) !important;
    }
    .btn-primary:hover,
    .btn-group > .btn.active:hover,
    .bttn-primary:hover { background:var(--tdk) !important; }

    /* Browse file button */
    .btn-file {
      background:var(--tc) !important;
      border:2px solid var(--tdk) !important;
      color:#fff !important;
    }
    .form-control[readonly] { background:#fff !important; color:#1a1a2e !important; }

    /* Picker */
    .bootstrap-select .btn { border:2px solid var(--tpl) !important; background:#fff !important; color:var(--tdk) !important; }
    .dropdown-menu > li > a,
    .dropdown-menu > li > a *,
    .bootstrap-select .dropdown-menu li a,
    .bootstrap-select .dropdown-menu li a *,
    .bootstrap-select .dropdown-menu li a span.text,
    .bootstrap-select .dropdown-menu .text,
    .bootstrap-select .dropdown-menu .glyphicon { color:#111111 !important; }
    .bootstrap-select .dropdown-menu li a:hover { background:var(--tls) !important; }

    /* DT */
    table.dataTable thead th {
      background:var(--tc) !important; color:#fff !important; font-size:14px;
    }
    table.dataTable tbody tr:hover { background:var(--tls) !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current,
    .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
      background:var(--tc) !important; color:#fff !important; border-radius:6px;
    }



    /* ---- Compact but readable form controls (DATASIM_4) ---- */
    .form-group { margin-bottom:10px !important; }
    .control-label, label { font-size:13px !important; font-weight:700 !important; color:var(--tdk) !important; }
    .input-group { width:100% !important; }
    .input-group-addon {
      min-width:82px !important;
      padding:6px 8px !important;
      font-size:12px !important;
      font-weight:700 !important;
      color:#fff !important;
      background:var(--tc) !important;
      border-color:var(--tc) !important;
      white-space:normal !important;
    }
    .form-control, .selectize-input, .bootstrap-select > .dropdown-toggle {
      min-height:38px !important;
      font-size:13px !important;
      padding:6px 10px !important;
    }
    input[type='number'].form-control {
      min-width:86px !important;
      text-align:center !important;
      font-weight:700 !important;
    }
    .shiny-input-container { width:100% !important; }
    .radioGroupButtons .btn, .btn-group .btn {
      min-width:72px !important;
      white-space:normal !important;
      line-height:1.15 !important;
    }
    .bootstrap-switch { min-width:150px !important; }
    .dataTables_wrapper .dt-buttons .btn::before { content:'' !important; }
    /* DATASIM_5: switch/button overflow fix */
    .sidebar .btn, .well .btn, .sidebar .action-button, .well .action-button,
    .sidebar .bttn, .well .bttn { max-width:100% !important; white-space:normal !important; }
    .bootstrap-switch { max-width:100% !important; min-width:120px !important; }
    .bootstrap-switch .bootstrap-switch-container { max-width:100% !important; }
    .bootstrap-switch-label { min-width:56px !important; }
    .bootstrap-switch-handle-on, .bootstrap-switch-handle-off { min-width:62px !important; }
    .hidden-downloads { display:none !important; }

    /* DATASIM_9: DICHO switch'i POLY sekmesiyle ayn byklkte, ancak sidebar dna tamayacak ekilde. */
    #son .bootstrap-switch { min-width:150px !important; max-width:100% !important; }
    #son .bootstrap-switch-handle-on, #son .bootstrap-switch-handle-off { min-width:72px !important; font-size:12px !important; }
    #son .bootstrap-switch-label { min-width:42px !important; }



    /* DATASIM_8: Sidebar widget balance fix  readable, equal, no overflow. */
    .well .form-group,
    .well .shiny-input-container { max-width:100% !important; }
    .well .input-group { width:100% !important; table-layout:fixed !important; }
    .well .input-group-addon {
      width:82px !important;
      min-width:82px !important;
      max-width:82px !important;
      white-space:normal !important;
      line-height:1.1 !important;
      overflow-wrap:anywhere !important;
    }
    .well input[type='number'].form-control {
      min-width:0 !important;
      width:100% !important;
      text-align:center !important;
      font-weight:700 !important;
      padding-left:4px !important;
      padding-right:4px !important;
    }
    .wide-numeric .input-group-addon {
      width:92px !important;
      min-width:92px !important;
      max-width:92px !important;
    }
    .wide-numeric input[type='number'].form-control {
      min-width:0 !important;
      width:100% !important;
      font-weight:800 !important;
    }
    .well .bootstrap-select > .dropdown-toggle,
    .well .selectize-input,
    .well .form-control { max-width:100% !important; }
    .well .btn, .well .bttn, .well .action-button {
      min-width:0 !important;
      max-width:100% !important;
      white-space:normal !important;
    }
    .well .radioGroupButtons .btn,
    .well .btn-group .btn {
      min-width:0 !important;
      padding-left:6px !important;
      padding-right:6px !important;
    }
    .modern-download-row { margin-top:12px; margin-bottom:14px; text-align:center; }
    .modern-download-row .bttn,
    .modern-download-row .btn {
      min-width:230px !important;
      height:42px !important;
      border-radius:12px !important;
      font-size:14px !important;
      box-shadow:0 4px 10px rgba(0,0,0,.12) !important;
    }

    /* Section headers */
    .section-header {
      color:var(--tdk); font-size:20px; font-family:'Segoe UI',Arial,sans-serif;
      font-weight:700; letter-spacing:1px; margin-bottom:8px;
      border-left:5px solid var(--tc); padding-left:10px;
    }

    /* Title */
    #tepe { border-bottom:3px solid var(--tc) !important; padding-bottom:8px; margin-bottom:10px; }
    #title  { color:var(--tdk) !important; font-size:26px !important; font-weight:800 !important; }
    #title2 { color:var(--tlt) !important; font-size:14px !important; text-align:right !important; }
    .key-warning { color:#c0392b; font-weight:700; font-size:13px; }

    /* dynamic theme style injection point */
    #dynamic-theme-style {}
  "

  simInfoCard <- function(mode = "sidebar-mini") {
    if (identical(mode, "sidebar-mini")) {
      return(
        shiny::div(
          class = "sim-sidebar-card",
          shiny::div(
            class = "sim-sidebar-content",
            shiny::div(
              class = "sim-sidebar-badge",
              shiny::span(class = "sim-sidebar-badge-mark"),
              shiny::span("RSP Package")
            ),
            shiny::div(
              class = "sim-sidebar-lines",
              shiny::div("DATA GENERATION"),
              shiny::div("IRT AND MULTIDIMENSIONAL SETS")
            )
          ),
          shiny::div(class = "sim-sidebar-credit", "Doan & Aybek (2022)")
        )
      )
    }

    if (identical(mode, "hero")) {
      return(
        shiny::div(
          class = "sim-hero",
          shiny::div(
            class = "sim-hero-left",
            shiny::div(
              class = "sim-hero-brand",
              shiny::div(
                class = "sim-hero-logo",
                shiny::span("R-Shiny"),
                tags$b("RSP"),
                shiny::span("Package")
              ),
              shiny::div(
                class = "sim-hero-brand-text",
                shiny::div(class = "sim-hero-pkg", "RSP Package"),
                shiny::div(class = "sim-hero-domain", "Simulation ", tags$b(""), " Psychometrics")
              )
            ),
            shiny::h2(
              class = "sim-hero-title",
              shiny::span(class = "sim-title-top", "DATA GENERATION PANEL"),
              shiny::span(class = "sim-title-sub", "DICHOTOMOUS  POLYTOMOUS  MULTIDIMENSIONAL  DIF")
            ),
            shiny::div(
              class = "sim-hero-bottom",
              shiny::div(
                class = "sim-hero-feature",
                shiny::div(class = "sim-hero-icon", "1"),
                shiny::span("DICHOTOMOUS IRT")
              ),
              shiny::div(
                class = "sim-hero-feature",
                shiny::div(class = "sim-hero-icon", "P"),
                shiny::span("POLYTOMOUS IRT")
              ),
              shiny::div(
                class = "sim-hero-feature",
                shiny::div(class = "sim-hero-icon", "M"),
                shiny::span("MULTIDIM DATA")
              ),
              shiny::div(
                class = "sim-hero-feature",
                shiny::div(class = "sim-hero-icon", "D"),
                shiny::span("DIF SCENARIOS")
              )
            ),
            shiny::div(class = "sim-hero-credit", "Dogan & Aybek (2022)")
          )
        )
      )
    }

    shiny::div()
  }

  ui <- fluidPage(
    useShinyjs(),
    theme = shinytheme("flatly"),

    # Tema CSS injection
    tags$head(tags$style(HTML(theme_css))),
    tags$head(tags$style(id = "dynamic-theme-style", "")),

    # JS - browser detection
    tags$head(tags$script(HTML(js))),

    #  Header 
    div(id = "tepe",
        fluidRow(
          column(7,
                 h1(id = "title", "DATA GENERATION PANEL")
          ),
          column(5,
                 h1(id = "title2", "RSP PACKAGE    CRAN")
          )
        )
    ),

    #  Color picker (intro panel iin renderUI) 
    uiOutput("cols"),

    #  Tooltip'ler 
    bsTooltip("down1",    "Downloading datasets may take time depending on replications!", placement = "top", trigger = "hover"),
    bsTooltip("down2",    "Downloading datasets may take time depending on replications!", placement = "top", trigger = "hover"),
    bsTooltip("down3",    "Downloading datasets may take time depending on replications!", placement = "top", trigger = "hover"),
    bsTooltip("down4",    "Downloading datasets may take time depending on replications!", placement = "top", trigger = "hover"),

    #  Layout 
    sidebarPanel(
      width = 4,

      ## PANEL 1  INTRO ##
      conditionalPanel(
        condition = "input.panel == 0",

        simInfoCard("sidebar-mini"),
        br(), br(),

        textOutput("browser"),
        br(),

        shinyWidgets::spectrumInput(
          inputId = "myColor",
          label   = tags$b("CHANGE THEME COLOR:"),
          choices = list(
            list('gray', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
            as.list(scales::brewer_pal(palette = "Blues")(9)),
            as.list(scales::brewer_pal(palette = "Greens")(9)),
            as.list(scales::brewer_pal(palette = "Spectral")(11)),
            as.list(scales::brewer_pal(palette = "Dark2")(8))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        )
      ),

      ## PANEL 2  DICHOTOMOUS IRT ##
      conditionalPanel(
        condition = "input.panel == 1",

        simInfoCard("sidebar-mini"),
        br(), br(),

        shinyWidgets::pickerInput(
          "type1",
          label   = tags$span(style = "font-weight:700;", "Select IRT Model"),
          choices = list("1PL" = 1, "2PL" = 2, "3PL" = 3),
          selected = 3
        ),

        fluidRow(
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "num1",
                   label   = NULL,
                   min = 5, max = 200, value = 10,
                   icon = "Items", width = "100%", size = "sm"
                 )
          ),
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "num2",
                   label   = NULL,
                   min = 50, max = 10000, value = 300,
                   icon = "N", width = "100%", size = "sm"
                 )
          )
        ),

        fluidRow(
          column(6, uiOutput("mp1")),
          column(6, uiOutput("mp1.1"))
        ),
        fluidRow(
          column(6, uiOutput("mp2")),
          column(6, uiOutput("mp3"))
        ),

        # DATASIM_9: DICHO panelindeki Replications ve Generate/Reset alan
        # POLY DATA GEN sekmesindeki 6+6 grid yaps ile ayn hizaya getirildi.
        fluidRow(
          column(6,
                 div(class = "wide-numeric",
                     shinyWidgets::numericInputIcon(
                       inputId = "rep",
                       label   = NULL,
                       min = 1, max = 1000, value = 1,
                       icon = "Replications", width = "100%", size = "sm"
                     )
                 )
          ),
          column(6, align = "center", uiOutput("son"))
        ),

        br(),

        # RMSE-Bias drop menu
        shinyWidgets::dropMenu(
          padding = "20px",
          theme   = "light-border",
          placement = "right-end",

          shinyWidgets::actionBttn(
            inputId = "coms",
            label   = "COMPUTE RMSE / BIAS",
            style   = "jelly",
            color   = "primary",
            size    = "sm"
          ),

          shinyWidgets::pickerInput(
            "irt",
            label   = tags$span(style = "font-weight:600;", "Compute RMSE & Bias?"),
            choices = list("YES" = 1, "NO" = 2),
            selected = 2
          ),

          gt::gt_output("fit1"),
          gt::gt_output("fit2"),
          withLoader(gt::gt_output("fit3"), type = "html", loader = "loader1"),
          gt::gt_output("grm")
        )
      ),

      ## PANEL 3  POLYTOMOUS IRT ##
      conditionalPanel(
        condition = "input.panel == 2",

        simInfoCard("sidebar-mini"),
        br(), br(),

        fluidRow(
          column(12,
                 shinyWidgets::radioGroupButtons(
                   inputId = "type2",
                   label   = tags$span(style = "font-weight:700;", "Select Model"),
                   choices = list("PCM" = 1, "RSM" = 2, "GPCM" = 3, "GRM" = 4),
                   justified  = TRUE,
                   checkIcon  = list(yes = icon("ok", lib = "glyphicon"))
                 )
          )
        ),

        fluidRow(
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "sec",
                   label   = tags$span(style = "font-weight:600;", "Categories"),
                   min = 2, max = 10, value = 3,
                   icon = "N", width = "100%"
                 )
          ),
          column(6,
                 div(class = "wide-numeric",
                     shinyWidgets::numericInputIcon(
                       inputId = "polyrep",
                       label   = NULL,
                       min = 1, max = 1000, value = 1,
                       icon = "Replications", width = "100%"
                     )
                 )
          )
        ),

        fluidRow(
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "nitem",
                   label   = NULL,
                   min = 5, max = 200, value = 30,
                   icon = "Items", width = "100%", size = "sm"
                 )
          ),
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "nn",
                   label   = NULL,
                   min = 50, max = 10000, value = 100,
                   icon = "N", width = "100%", size = "sm"
                 )
          )
        ),

        fluidRow(
          column(12, align = "center",
                 shinyWidgets::switchInput(
                   inputId     = "start2",
                   onLabel     = "RESET",
                   offLabel    = "START",
                   offStatus   = "danger",
                   handleWidth = "72px",
                   label       = "GENERATE",
                   labelWidth  = "92px",
                   size        = "normal",
                   inline      = TRUE
                 )
          )
        )
      ),

      ## PANEL 4  MULTIDIMENSIONAL ##
      conditionalPanel(
        condition = "input.panel == 3",

        simInfoCard("sidebar-mini"),
        br(), br(),

        fluidRow(
          column(6,
                 shinyWidgets::pickerInput(
                   "ftype1",
                   label   = tags$span(style = "font-weight:700;", "Data Type"),
                   choices = list("Dichotomous" = TRUE, "Polytomous" = FALSE, "Continuous" = 3),
                   selected = FALSE
                 )
          ),
          column(6,
                 sliderInput(
                   "meanload",
                   label = tags$span(style = "font-weight:600;", "Avg. Factor Loading"),
                   min = 0, max = 1, step = 0.05, value = 0.55
                 )
          )
        ),

        # DATA_SIM_16: Kullanc, retilen ok boyutlu veride beklenen ortalama faktrler aras korelasyonu belirler.
        fluidRow(
          column(12,
                 sliderInput(
                   "fac_cor",
                   label = tags$span(style = "font-weight:600;", "Avg. Inter-Factor Correlation"),
                   min = 0, max = 0.80, step = 0.05, value = 0.30
                 )
          )
        ),

        fluidRow(
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "fnitem", label = NULL,
                   min = 5, max = 200, value = 30, icon = "Items", width = "100%"
                 )
          ),
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "fnn", label = NULL,
                   min = 50, max = 10000, value = 100, icon = "N", width = "100%"
                 )
          )
        ),

        fluidRow(
          column(6,
                 div(class = "wide-numeric",
                     shinyWidgets::numericInputIcon(
                       inputId = "nfac",
                       label   = tags$span(style = "font-weight:600;", "Number of Factors"),
                       min = 1, max = 20, value = 2, icon = "Factors", width = "100%"
                     )
                 )
          ),
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "nlfl",
                   label   = tags$span(style = "font-weight:600;", "Items with Low Loadings"),
                   min = 0, max = 30, value = 0, icon = "Low load.", width = "100%"
                 )
          )
        ),

        fluidRow(
          column(6,
                 div(class = "wide-numeric",
                     shinyWidgets::numericInputIcon(
                       inputId = "frep", label = NULL,
                       min = 1, max = 1000, value = 1, icon = "Replications", width = "100%"
                     )
                 )
          ),
          column(6,
                 shinyWidgets::switchInput(
                   inputId     = "start3",
                   onLabel     = "RESET",
                   offLabel    = "START",
                   offStatus   = "danger",
                   handleWidth = "72px",
                   label       = "GENERATE",
                   labelWidth  = "92px",
                   size        = "normal",
                   inline      = TRUE
                 )
          )
        )
      ),

      ## PANEL 5  DIF DATA GENERATION ##
      conditionalPanel(
        condition = "input.panel == 4",

        simInfoCard("sidebar-mini"),
        br(), br(),

        fluidRow(
          column(12,
                 shinyWidgets::radioGroupButtons(
                   inputId = "dif_model",
                   label   = tags$span(style = "font-weight:700;", "Select Model"),
                   choices = list("Rasch" = "rasch", "2PL" = "2pl", "3PL" = "3pl"),
                   selected = "2pl",
                   justified = TRUE,
                   checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                 )
          )
        ),

        fluidRow(
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "dif_items",
                   label   = NULL,
                   min = 5, max = 200, value = 20,
                   icon = "Items", width = "100%", size = "sm"
                 )
          ),
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "dif_n",
                   label   = NULL,
                   min = 50, max = 20000, value = 500,
                   icon = "N", width = "100%", size = "sm"
                 )
          )
        ),

        fluidRow(
          column(6,
                 sliderInput(
                   inputId = "dif_ratio",
                   label = tags$span(style = "font-weight:600;", "DIF Item Ratio"),
                   min = 0.05, max = 0.50, value = 0.20, step = 0.05
                 )
          ),
          column(6,
                 sliderInput(
                   inputId = "dif_group_prop",
                   label = tags$span(style = "font-weight:600;", "Focal Group Ratio"),
                   min = 0.10, max = 0.90, value = 0.50, step = 0.05
                 )
          )
        ),

        fluidRow(
          column(6,
                 shinyWidgets::numericInputIcon(
                   inputId = "dif_size",
                   label   = tags$span(style = "font-weight:600;", "DIF Size"),
                   min = 0, max = 3, value = 0.50, step = 0.10,
                   icon = "DIF", width = "100%", size = "sm"
                 )
          ),
          column(6,
                 shinyWidgets::pickerInput(
                   inputId = "dif_direction",
                   label   = tags$span(style = "font-weight:700;", "DIF Direction"),
                   choices = list("Focal harder" = "harder", "Focal easier" = "easier"),
                   selected = "harder"
                 )
          )
        ),

        fluidRow(
          column(6,
                 shinyWidgets::pickerInput(
                   inputId = "dif_type",
                   label   = tags$span(style = "font-weight:700;", "DIF Type"),
                   choices = list("Uniform DIF (b)" = "uniform", "Non-uniform DIF (a)" = "nonuniform", "Mixed DIF (a+b)" = "mixed"),
                   selected = "uniform"
                 )
          ),
          column(6,
                 div(class = "wide-numeric",
                     shinyWidgets::numericInputIcon(
                       inputId = "dif_rep",
                       label   = NULL,
                       min = 1, max = 1000, value = 1,
                       icon = "Replications", width = "100%", size = "sm"
                     )
                 )
          )
        ),

        shinyWidgets::awesomeCheckbox(
          inputId = "dif_show_adv",
          label = tags$span(style = "font-weight:700;", "Show advanced item-parameter ranges"),
          value = FALSE,
          status = "primary"
        ),

        conditionalPanel(
          condition = "input.dif_show_adv == true",
          sliderInput("dif_b_range", tags$span(style = "font-weight:600;", "b range"),
                      min = -4, max = 4, value = c(-1.5, 1.5), step = 0.10),
          sliderInput("dif_a_range", tags$span(style = "font-weight:600;", "a range"),
                      min = 0.20, max = 3, value = c(0.70, 1.50), step = 0.10),
          sliderInput("dif_c_range", tags$span(style = "font-weight:600;", "c range"),
                      min = 0, max = 0.35, value = c(0, 0.20), step = 0.01)
        ),

        fluidRow(
          column(12, align = "center",
                 shinyWidgets::switchInput(
                   inputId     = "start4",
                   onLabel     = "RESET",
                   offLabel    = "START",
                   offStatus   = "danger",
                   onStatus    = "primary",
                   handleWidth = "72px",
                   label       = "GENERATE",
                   labelWidth  = "92px",
                   size        = "normal",
                   inline      = TRUE
                 )
          )
        ),

        # DATA_SIM_15: Sidebar panelin altnda kk, koullu ve temaya uyumlu replikasyon seici.
        conditionalPanel(
          condition = "input.start4 == true && input.dif_rep > 1",
          div(
            style = "margin-top:8px; padding-top:8px; border-top:1px solid rgba(128,128,128,.25);",
            shinyWidgets::pickerInput(
              inputId = "dif_rep_view",
              label = tags$span(
                style = "font-weight:700; color:#6f6f6f; font-size:13px;",
                "Show Replication"
              ),
              choices = as.character(1:1000),
              selected = "1",
              width = "100%",
              options = shinyWidgets::pickerOptions(
                style = "btn-secondary btn-sm",
                size = 6
              )
            )
          )
        ),

        # DATA_SIM_17: Generate sonras DIF analiz penceresini aan action button.
        conditionalPanel(
          condition = "input.start4 == true",
          div(
            style = "margin-top:8px; padding-top:8px; border-top:1px solid rgba(128,128,128,.25);",
            actionButton(
              inputId = "open_dif_analysis",
              label = "DIF ANALYSIS",
              icon = icon("search"),
              class = "btn-primary",
              style = "width:100%; min-width:100% !important;"
            )
          )
        )
      )
    ), # close sidebarPanel

    ##  MAIN PANEL  ##
    mainPanel(
      tabsetPanel(
        id = "panel",

        ## Tab 1  INTRO ##
        tabPanel(
          title = "INTRO",
          value = 0,
          br(),
          fluidRow(
            column(12, simInfoCard("hero"))
          )
        ),

        ## Tab 2  DICHO DATA GEN ##
        tabPanel(
          title = "DICHO DATA GEN (IRT)",
          value = 1,
          br(),

          uiOutput("info_text0"),

          DT::DTOutput("tab1"),
          DT::DTOutput("tab2"),
          DT::DTOutput("tab3"),

          uiOutput("info_text1"),
          uiOutput("down_dicho_xlsx_ui"),
          br(),
          uiOutput("info_text2")
        ),

        ## Tab 3  POLY DATA GEN ##
        tabPanel(
          title = "POLY DATA GEN (IRT)",
          value = 2,
          br(),

          uiOutput("info_text00"),
          br(),

          withLoader(DT::DTOutput("tab_pcm"),  type = "html", loader = "loader1"),
          DT::DTOutput("tab_rsm"),
          DT::DTOutput("tab_gpcm"),
          DT::DTOutput("tab_grm"),

          uiOutput("info_text3"),
          uiOutput("down_poly_xlsx_ui")
        ),

        ## Tab 4  MULTIDIM DATA GEN ##
        tabPanel(
          title = "MULTIDIM DATA GEN (CTT)",
          value = 3,
          br(),

          uiOutput("info_text000"),
          br(),

          uiOutput("tab_fac_all"),

          uiOutput("info_text4"),
          uiOutput("down_multi_xlsx_ui")
        )
,

        ## Tab 5  DIF DATA GEN ##
        tabPanel(
          title = "DIF DATA GEN (IRT)",
          value = 4,
          br(),

          uiOutput("info_text_dif0"),
          br(),

          withLoader(DT::DTOutput("tab_dif"), type = "html", loader = "loader1"),
          br(),
          tags$h4(style = "font-weight:700; margin-top:14px;", "DIF Item Parameters and DIF Status"),
          withLoader(DT::DTOutput("tab_dif_params"), type = "html", loader = "loader1"),

          uiOutput("info_text_dif1"),
          fluidRow(
            column(6, uiOutput("down_dif_xlsx_ui")),
            column(6, uiOutput("down_dif_params_xlsx_ui"))
          )
        )

      )
    ) # close mainPanel
  ) # close fluidPage


  ##  SERVER 

  server <- function(input, session, output) {

    # DATASIM_14: Replikasyon seicinin seeneklerini retilen replikasyon saysna gre snrlar.
    observe({
      req(input$dif_rep)
      current <- suppressWarnings(as.integer(input$dif_rep_view))
      if (length(current) == 0 || is.na(current)) current <- 1L
      current <- max(1L, min(current, as.integer(input$dif_rep)))
      shinyWidgets::updatePickerInput(session, "dif_rep_view",
                                      choices = as.character(seq_len(as.integer(input$dif_rep))),
                                      selected = as.character(current))
    })

    #  Tema rengi: ITEMAN_10 ile ayn dinamik CSS mant 
    observeEvent(input$myColor, {
      col <- input$myColor
      css <- sprintf(
        ":root{--tc:%s;--tdk:#2d1b46;--tlt:color-mix(in srgb,%s 62%%,#ffffff);--tpl:color-mix(in srgb,%s 36%%,#ffffff);--tls:color-mix(in srgb,%s 12%%,#ffffff);}
         body{background:linear-gradient(to bottom right,#ffffff,color-mix(in srgb,%s 10%%,#ffffff)) !important;}
         .nav-tabs > li > a {color:var(--tdk) !important;}
         .nav-tabs > li.active > a,
         .nav-tabs > li.active > a:focus,
         .nav-tabs > li.active > a:hover {
           background:color-mix(in srgb,var(--tc) 72%%,#241237) !important;
           color:#ffffff !important;
           border-color:color-mix(in srgb,var(--tc) 82%%,#241237) !important;
           box-shadow:inset 0 -2px 0 color-mix(in srgb,#ffffff 34%%,transparent) !important;
         }
         .nav-tabs > li > a:hover {background:var(--tls) !important;color:var(--tdk) !important;}
         .well {background:color-mix(in srgb,var(--tc) 7%%,#ffffff) !important;border-color:var(--tpl) !important;}
         table.dataTable thead th {background:var(--tc) !important;}
         #tepe {border-bottom-color:var(--tc) !important;}
         #title {color:var(--tdk) !important;}
         #title2 {color:var(--tlt) !important;}",
        col, col, col, col, col
      )
      shinyjs::runjs(sprintf(
        "document.getElementById('dynamic-theme-style').innerHTML=`%s`;",
        gsub("`", "'", css)
      ))
    })

    #  Browser uyars 
    output$browser <- renderText({
      req(input$myBrowser)
      if (grepl("Chrome 10", input$myBrowser)) {
        "Please click 'Open in Browser' for a better experience."
      } else {
        NULL
      }
    })

    #  renderUI: renk cols (eski setBackgroundColor kaldrld) 
    output$cols <- renderUI({ NULL })

    #  Slider widget'lar (conditional) 
    output$mp1 <- renderUI({
      sliderInput("num3", tags$span(style = "font-weight:600;", "Item Difficulty (b)"),
                  min = -3, max = 4, step = 0.01, value = c(-1, 1))
    })

    output$mp1.1 <- renderUI({
      req(input$type1)
      if (input$type1 == 1) {
        sliderInput("num3.1", tags$span(style = "font-weight:600;", "Item Discrimination (a)"),
                    min = -1, max = 3, step = 0.01, value = 1)
      }
    })

    output$mp2 <- renderUI({
      req(input$type1)
      if (input$type1 != 1) {
        sliderInput("num4", tags$span(style = "font-weight:600;", "Item Discrimination (a)"),
                    min = -1, max = 3, step = 0.01, value = c(0.70, 1.5))
      }
    })

    output$mp3 <- renderUI({
      req(input$type1)
      if (input$type1 == 3) {
        sliderInput("num5", tags$span(style = "font-weight:600;", "Guessing Parameter (c)"),
                    min = 0, max = 0.35, step = 0.01, value = c(0, 0.20))
      }
    })

    output$son <- renderUI({
      # DATASIM_9: DICHO Generate/Reset switch'i POLY panelindeki switch boyutu ve yapsyla eitlendi.
      shinyWidgets::switchInput(
        inputId     = "start",
        onLabel     = "RESET",
        offLabel    = "START",
        offStatus   = "danger",
        onStatus    = "primary",
        handleWidth = "72px",
        label       = "GENERATE",
        labelWidth  = "92px",
        size        = "normal",
        inline      = TRUE
      )
    })

    # DATASIM_7: DT stndeki Copy/CSV/Excel butonlar kaldrld; indirme sadece tablo altndaki modern Excel butonlaryla yaplr.

    #  Info text'ler (renderUI ile styled box) 
    info_box <- function(msg, color = "#3B1F5E", bg = "#f0ebff") {
      div(style = sprintf(
        "background:%s; color:%s; border-left:4px solid %s; border-radius:8px;
         padding:12px 16px; font-weight:600; font-size:14px; margin:10px 0;",
        bg, color, color), msg)
    }

    # DATASIM_6: switchInput balangta NULL dnebildii iin koullar gvenli hale getirildi.
    # Eski ifade input$start == 2 ile baladnda NULL durumda "argument is of length zero" hatas retebiliyordu.
    is_started <- function(x) {
      isTRUE(x) || identical(x, 1L) || identical(x, 1)
    }

    is_not_started <- function(x) {
      is.null(x) || identical(x, FALSE) || identical(x, 0L) || identical(x, 0) || identical(x, 2L) || identical(x, 2)
    }

    # DATASIM_8: pickerInput logical values sometimes arrive as character strings.
    # This helper makes MULTIDIM table rendering and Excel download robust.
    multi_data_type <- function(x) {
      if (is.null(x)) return("poly")
      x_chr <- tolower(as.character(x)[1])
      if (x_chr %in% c("true", "1")) return("dicho")
      if (x_chr %in% c("false", "0")) return("poly")
      if (x_chr %in% c("3", "continuous", "cont")) return("cont")
      "poly"
    }


    make_excel_download_ui <- function(output_id, started_reactive) {
      renderUI({
        req(is_started(started_reactive()))
        div(class = "modern-download-row",
            shinyWidgets::downloadBttn(
              outputId = output_id,
              label = tagList(icon("download"), " Download Generated Data (Excel)"),
              style = "unite",
              color = "primary",
              size = "md",
              no_outline = TRUE
            )
        )
      })
    }

    output$down_dicho_xlsx_ui <- make_excel_download_ui("download_dicho_xlsx", reactive(input$start))
    output$down_poly_xlsx_ui  <- make_excel_download_ui("download_poly_xlsx",  reactive(input$start2))
    output$down_multi_xlsx_ui <- make_excel_download_ui("download_multi_xlsx", reactive(input$start3))
    output$down_dif_xlsx_ui   <- make_excel_download_ui("download_dif_xlsx",   reactive(input$start4))

    # DATA_SIM_19: DIF item parameter tablolar iin ayr Excel indirme butonu.
    output$down_dif_params_xlsx_ui <- renderUI({
      req(is_started(input$start4))
      div(class = "modern-download-row",
          shinyWidgets::downloadBttn(
            outputId = "download_dif_params_xlsx",
            label = tagList(icon("download"), " Download DIF Parameters (Excel)"),
            style = "unite",
            color = "primary",
            size = "md",
            no_outline = TRUE
          )
      )
    })

    output$info_text0 <- renderUI({
      req(is_not_started(input$start))
      info_box("After setting the parameters, toggle 'GENERATE' to generate data.")
    })

    output$info_text00 <- renderUI({
      req(is_not_started(input$start2))
      info_box("After setting the parameters, toggle 'START' to generate data.")
    })

    output$info_text000 <- renderUI({
      req(is_not_started(input$start3))
      info_box("After setting the parameters, toggle 'START' to generate data.")
    })

    output$info_text_dif0 <- renderUI({
      req(is_not_started(input$start4))
      info_box("After setting DIF parameters, toggle 'START' to generate data. The table shows the first replication as a preview; the Excel download includes all replications.")
    })

    output$info_text1 <- renderUI({
      req(is_started(input$start))
      info_box(paste(input$rep, "data set(s) generated ",
                     input$num2, "respondents ", input$num1, "items."),
               color = "#166534", bg = "#dcfce7")
    })

    output$info_text2 <- renderUI({
      req(input$irt == 1, input$start == 1)
      info_box("Item parameters estimated. RMSE & Bias values are shown in the sidebar.",
               color = "#92400e", bg = "#fef3c7")
    })

    output$info_text3 <- renderUI({
      req(is_started(input$start2))
      mod <- switch(as.character(input$type2),
        "1" = "PCM", "2" = "RSM", "3" = "GPCM", "4" = "GRM", "?")
      info_box(paste(input$polyrep, "data set(s) generated ", mod, "model,",
                     input$nitem, "items ", input$nn, "respondents."),
               color = "#166534", bg = "#dcfce7")
    })

    output$info_text4 <- renderUI({
      req(is_started(input$start3))
      info_box(paste(input$frep, "data set(s) with", input$nfac, "dimensions ",
                     input$fnitem, "items ", input$fnn, "respondents. Avg. loading:", input$meanload),
               color = "#166534", bg = "#dcfce7")
    })

    output$info_text_dif1 <- renderUI({
      req(is_started(input$start4))
      ndif <- max(1, round(input$dif_items * input$dif_ratio))
      info_box(paste(input$dif_rep, "DIF data set(s) generated ", input$dif_n, "respondents ",
                     input$dif_items, "items;", ndif, "DIF item(s); model:", toupper(input$dif_model),
                     "; DIF type:", input$dif_type, "; DIF size:", input$dif_size),
               color = "#166534", bg = "#dcfce7")
    })

    #  Shared DT options + download helper 
    # DATASIM_5: DT butonlar grnrde kalr; CSV/Excel indirmeleri Shiny downloadHandler zerinden yaplr.
    # Bylece yalnzca ekranda grnen sayfa deil, retilen tm veri indirilir.

    timestamp_name <- function(prefix, ext) {
      paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    }

    get_active_data_list <- function(kind) {
      if (kind == "dicho") {
        req(is_started(input$start))
        x <- SIMDATA_ENV$simDataDichotom
      } else if (kind == "poly") {
        req(is_started(input$start2))
        x <- switch(as.character(input$type2),
                    "1" = SIMDATA_ENV$simDataPCM,
                    "2" = SIMDATA_ENV$simDataRSM,
                    "3" = SIMDATA_ENV$simDataGPCM,
                    "4" = SIMDATA_ENV$simDataGRM)
      } else if (kind == "multi") {
        req(is_started(input$start3))
        x <- switch(multi_data_type(input$ftype1),
                    "poly"  = SIMDATA_ENV$SimMultiDataPoly1,
                    "dicho" = SIMDATA_ENV$SimMultiDataDicho1,
                    "cont"  = SIMDATA_ENV$SimMultiDataCont1,
                    SIMDATA_ENV$SimMultiDataPoly1)
      } else if (kind == "dif") {
        req(is_started(input$start4))
        x <- SIMDATA_ENV$simDataDIF
      }
      validate(need(!is.null(x) && length(x) > 0, "No generated data found."))
      x
    }

    get_active_prefix <- function(kind) {
      if (kind == "dicho") {
        paste0("DICHO_", switch(as.character(input$type1), "1" = "1PL", "2" = "2PL", "3" = "3PL"))
      } else if (kind == "poly") {
        paste0("POLY_", switch(as.character(input$type2), "1" = "PCM", "2" = "RSM", "3" = "GPCM", "4" = "GRM"))
      } else if (kind == "multi") {
        paste0("MULTIDIM_", switch(multi_data_type(input$ftype1),
                                   "poly" = "POLY",
                                   "dicho" = "DICHO",
                                   "cont" = "CONT",
                                   "POLY"))
      } else if (kind == "dif") {
        paste0("DIF_", toupper(input$dif_model), "_", toupper(input$dif_type))
      }
    }

    write_csv_bundle <- function(data_list, file, prefix) {
      if (length(data_list) == 1) {
        utils::write.csv(data_list[[1]], file, row.names = TRUE, fileEncoding = "UTF-8")
      } else {
        td <- tempfile("datasim_csv_")
        dir.create(td, recursive = TRUE, showWarnings = FALSE)
        csv_files <- character(length(data_list))
        for (i in seq_along(data_list)) {
          csv_files[i] <- file.path(td, sprintf("%s_replication_%02d.csv", prefix, i))
          utils::write.csv(data_list[[i]], csv_files[i], row.names = TRUE, fileEncoding = "UTF-8")
        }
        oldwd <- getwd()
        on.exit(setwd(oldwd), add = TRUE)
        setwd(td)
        utils::zip(zipfile = file, files = basename(csv_files))
      }
    }

    write_excel_workbook <- function(data_list, file, prefix) {
      # DATASIM_7: Multidim dhil tm indirmeler gerek .xlsx dosyas retir.
      # ncelik openxlsx; yoksa mevcut eski bamllkla uyum iin xlsx paketine der.
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        wb <- openxlsx::createWorkbook()
        for (i in seq_along(data_list)) {
          sh <- paste0("Replication_", i)
          openxlsx::addWorksheet(wb, sh)
          openxlsx::writeData(wb, sh, as.data.frame(data_list[[i]]), rowNames = TRUE)
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        validate(need(requireNamespace("xlsx", quietly = TRUE),
                      "Excel export requires either openxlsx or xlsx. Please install.packages('openxlsx')."))
        for (i in seq_along(data_list)) {
          xlsx::write.xlsx(as.data.frame(data_list[[i]]), file = file,
                           sheetName = paste0("Replication_", i), append = (i > 1), row.names = TRUE)
        }
      }
    }

    make_downloads <- function(kind, csv_id, xlsx_id) {
      output[[csv_id]] <- downloadHandler(
        filename = function() {
          nrep <- length(get_active_data_list(kind))
          ext <- if (nrep == 1) "csv" else "zip"
          timestamp_name(get_active_prefix(kind), ext)
        },
        content = function(file) {
          data_list <- get_active_data_list(kind)
          write_csv_bundle(data_list, file, get_active_prefix(kind))
        }
      )
      output[[xlsx_id]] <- downloadHandler(
        filename = function() timestamp_name(get_active_prefix(kind), "xlsx"),
        content = function(file) {
          data_list <- get_active_data_list(kind)
          write_excel_workbook(data_list, file, get_active_prefix(kind))
        }
      )
    }

    make_downloads("dicho", "download_dicho_csv", "download_dicho_xlsx")
    make_downloads("poly",  "download_poly_csv",  "download_poly_xlsx")
    make_downloads("multi", "download_multi_csv", "download_multi_xlsx")
    make_downloads("dif",   "download_dif_csv",   "download_dif_xlsx")

    # DATA_SIM_19: DIF parametreleri, veri setinden ayr olarak indirilebilir.
    get_dif_param_list <- function() {
      req(is_started(input$start4))
      x <- SIMDATA_ENV$simDIFParams
      validate(need(!is.null(x) && length(x) > 0, "No DIF parameter table found."))
      x
    }

    output$download_dif_params_xlsx <- downloadHandler(
      filename = function() {
        timestamp_name(paste0("DIF_PARAMETERS_", toupper(input$dif_model), "_", toupper(input$dif_type)), "xlsx")
      },
      content = function(file) {
        write_excel_workbook(get_dif_param_list(), file, "DIF_PARAMETERS")
      }
    )

    dt_opts <- function(kind = "dicho") {
      # DATASIM_7: stteki Copy / CSV / Excel DT butonlar kaldrld.
      # Tabloda sadece arama, sayfalama ve yatay kaydrma kalr.
      list(
        dom        = "frtip",
        scrollX    = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    }

    render_dt_styled <- function(data, kind = "dicho") {
      DT::datatable(data, rownames = TRUE,
                    options    = dt_opts(kind),
                    class      = "display compact hover stripe")
    }

    #  DATA GENERATION  1PL 
    output$tab1 <- DT::renderDT({
      req(is_started(input$start))
      req(input$type1 == 1)

      nitem <- input$num1
      n     <- input$num2
      a     <- rep(input$num3.1, nitem)
      minb  <- min(input$num3)
      maxb  <- max(input$num3)
      theta <- rnorm(n, 0, 1)

      pl1 <- function(nitem, n, a, minb, maxb, theta) {
        b        <- runif(nitem, minb, maxb)
        thetaMat  <- matrix(rep(theta, length(b)), ncol = length(b))
        thetaMat1 <- t(apply(thetaMat, 1, '-', b))
        thetaMat2 <- t(apply(thetaMat1, 1, '*', a))
        proB      <- 1 / (1 + exp(-thetaMat2))
        pteta     <- matrix(proB, ncol = nitem)
        answerMat <- matrix(sapply(pteta, rbinom, n = 1, size = 1), ncol = length(b))
        answerMat2 <- rbind(b, a, answerMat)
        row.names(answerMat2) <- c("b", "a", paste0("Respondent_", 1:n))
        colnames(answerMat2)  <- paste0("item", 1:nitem)
        as.data.frame(round(answerMat2, 3))
      }

      emptyList <- list()
      withProgress(message = "DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$rep) {
          incProgress(1 / input$rep)
          emptyList[[i]] <- pl1(nitem, n, a, minb, maxb, theta)
        }
      })
      SIMDATA_ENV$simDataDichotom <- emptyList
      render_dt_styled(emptyList[[1]])
    })

    #  DATA GENERATION  2PL 
    output$tab2 <- DT::renderDT({
      req(is_started(input$start))
      req(input$type1 == 2)

      nitem <- input$num1; n <- input$num2
      mina <- min(input$num4); maxa <- max(input$num4)
      minb <- min(input$num3); maxb <- max(input$num3)
      theta <- rnorm(n, 0, 1)

      pl2 <- function(nitem, n, mina, maxa, minb, maxb, theta) {
        b <- runif(nitem, minb, maxb); a <- runif(nitem, mina, maxa)
        thetaMat  <- matrix(rep(theta, length(b)), ncol = length(b))
        thetaMat1 <- t(apply(thetaMat, 1, '-', b))
        thetaMat2 <- t(apply(thetaMat1, 1, '*', a))
        proB      <- 1 / (1 + exp(-thetaMat2))
        pteta     <- matrix(proB, ncol = nitem)
        answerMat <- matrix(sapply(pteta, rbinom, n = 1, size = 1), ncol = length(b))
        answerMat2 <- rbind(b, a, answerMat)
        row.names(answerMat2) <- c("b", "a", paste0("Respondent_", 1:n))
        colnames(answerMat2)  <- paste0("item", 1:nitem)
        as.data.frame(round(answerMat2, 3))
      }

      emptyList <- list()
      withProgress(message = "DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$rep) {
          incProgress(1 / input$rep)
          emptyList[[i]] <- pl2(nitem, n, mina, maxa, minb, maxb, theta)
        }
      })
      SIMDATA_ENV$simDataDichotom <- emptyList
      render_dt_styled(emptyList[[1]])
    })

    #  DATA GENERATION  3PL 
    output$tab3 <- DT::renderDT({
      req(is_started(input$start))
      req(input$type1 == 3)

      nitem <- input$num1; n <- input$num2
      mina <- min(input$num4); maxa <- max(input$num4)
      minb <- min(input$num3); maxb <- max(input$num3)
      minc <- min(input$num5); maxc <- max(input$num5)
      theta <- rnorm(n, 0, 1)

      pl3 <- function(nitem, n, mina, maxa, minb, maxb, minc, maxc, theta) {
        b <- runif(nitem, minb, maxb); a <- runif(nitem, mina, maxa)
        c_par <- runif(nitem, minc, maxc)
        thetaMat  <- matrix(rep(theta, length(b)), ncol = length(b))
        thetaMat1 <- t(apply(thetaMat, 1, '-', b))
        thetaMat2 <- t(apply(thetaMat1, 1, '*', a))
        proB      <- 1 / (1 + exp(-thetaMat2))
        ols1 <- t(apply(proB, 1, '*', (1 - c_par)))
        ols2 <- t(apply(ols1, 1, '+', c_par))
        pteta     <- matrix(ols2, ncol = nitem)
        answerMat <- matrix(sapply(pteta, rbinom, n = 1, size = 1), ncol = length(b))
        answerMat2 <- rbind(b, a, c_par, answerMat)
        row.names(answerMat2) <- c("b", "a", "c", paste0("Respondent_", 1:n))
        colnames(answerMat2)  <- paste0("item", 1:nitem)
        as.data.frame(round(answerMat2, 3))
      }

      emptyList <- list()
      withProgress(message = "DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$rep) {
          incProgress(1 / input$rep)
          emptyList[[i]] <- pl3(nitem, n, mina, maxa, minb, maxb, minc, maxc, theta)
        }
      })
      SIMDATA_ENV$simDataDichotom <- emptyList
      render_dt_styled(emptyList[[1]])
    })

    #  RMSE/BIAS  gt tablolar (random renk  sabit tema rengi) 

    styled_gt <- function(df, title_txt) {
      tc <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      tc_light <- grDevices::adjustcolor(tc, alpha.f = 0.16)
      gt::gt(df) %>%
        gt::tab_header(title = gt::md(paste0("**", title_txt, "**"))) %>%
        gt::tab_style(
          style     = list(gt::cell_fill(color = tc_light),
                           gt::cell_text(color = "#1a1a2e", weight = "bold")),
          locations = gt::cells_body()
        ) %>%
        gt::tab_style(
          style     = list(gt::cell_fill(color = tc),
                           gt::cell_text(color = "white", weight = "bold")),
          locations = gt::cells_column_labels()
        ) %>%
        gt::tab_style(
          style     = list(gt::cell_fill(color = tc),
                           gt::cell_text(color = "white", weight = "bold")),
          locations = gt::cells_title()
        ) %>%
        gt::cols_width(everything() ~ gt::px(130)) %>%
        gt::tab_options(
          table.font.names = "Segoe UI",
          table.border.top.color = tc,
          table.border.bottom.color = tc,
          column_labels.font.size  = gt::px(14),
          heading.title.font.size  = gt::px(16)
        )
    }

    compute_bias_rmse <- function(actual_list, param_list, col_actual, col_param, n_rep) {
      bias_v <- numeric(n_rep); rmse_v <- numeric(n_rep)
      for (i in 1:n_rep) {
        bias_v[i] <- Metrics::bias(as.numeric(actual_list[[i]][col_actual, ]),
                                   as.numeric(param_list[[i]][, col_param]))
        rmse_v[i] <- Metrics::rmse(as.numeric(actual_list[[i]][col_actual, ]),
                                   as.numeric(param_list[[i]][, col_param]))
      }
      list(bias = mean(bias_v), rmse = mean(rmse_v))
    }

    output$fit1 <- gt::render_gt(align = "center", {
      req(is_started(input$start), input$irt == 1, input$type1 == 1)
      emptyListG <- SIMDATA_ENV$simDataDichotom
      emptyListK <- lapply(1:input$rep, function(i)
        mirt::mirt(data = emptyListG[[i]][3:nrow(emptyListG[[i]]), ],
                   model = 1, SE = FALSE, itemtype = "Rasch"))
      emptyListP <- lapply(emptyListK, function(m)
        mirt::coef(m, IRTpars = TRUE, simplify = TRUE)$items)
      SIMDATA_ENV$simParamDichotom <- emptyListP

      b_res <- compute_bias_rmse(emptyListG, emptyListP, 1, 2, input$rep)
      a_res <- compute_bias_rmse(emptyListG, emptyListP, 2, 1, input$rep)

      df <- data.frame(BIAS_b = b_res$bias, RMSE_b = b_res$rmse,
                       BIAS_a = a_res$bias, RMSE_a = a_res$rmse)
      styled_gt(round(df, 4), "1PL: RMSE & BIAS")
    })

    output$fit2 <- gt::render_gt(align = "center", {
      req(is_started(input$start), input$irt == 1, input$type1 == 2)
      emptyListG <- SIMDATA_ENV$simDataDichotom
      emptyListK <- lapply(1:input$rep, function(i)
        mirt::mirt(data = emptyListG[[i]][3:nrow(emptyListG[[i]]), ],
                   model = 1, SE = FALSE, itemtype = "2PL"))
      emptyListP <- lapply(emptyListK, function(m)
        mirt::coef(m, IRTpars = TRUE, simplify = TRUE)$items)
      SIMDATA_ENV$simParamDichotom <- emptyListP

      b_res <- compute_bias_rmse(emptyListG, emptyListP, 1, 2, input$rep)
      a_res <- compute_bias_rmse(emptyListG, emptyListP, 2, 1, input$rep)

      df <- data.frame(BIAS_b = b_res$bias, RMSE_b = b_res$rmse,
                       BIAS_a = a_res$bias, RMSE_a = a_res$rmse)
      styled_gt(round(df, 4), "2PL: RMSE & BIAS")
    })

    output$fit3 <- gt::render_gt(align = "center", {
      req(is_started(input$start), input$irt == 1, input$type1 == 3)
      emptyListG <- SIMDATA_ENV$simDataDichotom
      # BUG FIX: 3PL'de satr indeksi dzeltildi (4: yerine 4:nrow kullanm)
      emptyListK <- lapply(1:input$rep, function(i)
        mirt::mirt(data = emptyListG[[i]][4:nrow(emptyListG[[i]]), ],
                   model = 1, SE = FALSE, itemtype = "3PL"))
      emptyListP <- lapply(emptyListK, function(m)
        mirt::coef(m, IRTpars = TRUE, simplify = TRUE)$items)
      SIMDATA_ENV$simParamDichotom <- emptyListP

      b_res <- compute_bias_rmse(emptyListG, emptyListP, 1, 2, input$rep)
      a_res <- compute_bias_rmse(emptyListG, emptyListP, 2, 1, input$rep)
      c_res <- compute_bias_rmse(emptyListG, emptyListP, 3, 3, input$rep)

      df <- data.frame(BIAS_b = b_res$bias, RMSE_b = b_res$rmse,
                       BIAS_a = a_res$bias, RMSE_a = a_res$rmse,
                       BIAS_c = c_res$bias, RMSE_c = c_res$rmse)
      styled_gt(round(df, 4), "3PL: RMSE & BIAS")
    })

    ##  POLYTOMOUS DATA GENERATION 

    output$tab_pcm <- DT::renderDT({
      req(is_started(input$start2), input$type2 == 1)
      PCM_B <- PCM_DAT1 <- list()
      withProgress(message = "PCM DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$polyrep) {
          incProgress(1 / input$polyrep)
          PCM_B[[i]] <- catR::genPolyMatrix(items = input$nitem,
                                            nrCat = as.numeric(input$sec),
                                            model = "PCM", seed = i,
                                            same.nrCat = TRUE, cbControl = NULL)
          PCM_B1 <- as.matrix(PCM_B)
          PCM_DAT1[[i]] <- catR::genPattern(rnorm(input$nn), PCM_B1[[i]], model = "PCM")
          colnames(PCM_DAT1[[i]]) <- paste0("item", 1:input$nitem)
        }
      })
      SIMDATA_ENV$simDataPCM <- PCM_DAT1
      render_dt_styled(PCM_DAT1[[1]], "poly")
    })

    output$tab_rsm <- DT::renderDT({
      req(is_started(input$start2), input$type2 == 2)
      RSM_B <- RSM_DAT1 <- list()
      withProgress(message = "RSM DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$polyrep) {
          incProgress(1 / input$polyrep)
          RSM_B[[i]] <- catR::genPolyMatrix(items = input$nitem,
                                            nrCat = as.numeric(input$sec),
                                            model = "RSM", seed = i,
                                            same.nrCat = TRUE, cbControl = NULL)
          RSM_B1 <- as.matrix(RSM_B)
          RSM_DAT1[[i]] <- catR::genPattern(rnorm(input$nn), RSM_B1[[i]], model = "RSM")
          colnames(RSM_DAT1[[i]]) <- paste0("item", 1:input$nitem)
        }
      })
      SIMDATA_ENV$simDataRSM <- RSM_DAT1
      render_dt_styled(RSM_DAT1[[1]], "poly")
    })

    output$tab_gpcm <- DT::renderDT({
      req(is_started(input$start2), input$type2 == 3)
      GPCM_B <- GPCM_DAT1 <- list()
      withProgress(message = "GPCM DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$polyrep) {
          incProgress(1 / input$polyrep)
          GPCM_B[[i]] <- catR::genPolyMatrix(items = input$nitem,
                                             nrCat = as.numeric(input$sec),
                                             model = "GPCM", seed = i,
                                             same.nrCat = TRUE, cbControl = NULL)
          GPCM_B1 <- as.matrix(GPCM_B)
          GPCM_DAT1[[i]] <- catR::genPattern(rnorm(input$nn), GPCM_B1[[i]], model = "GPCM")
          colnames(GPCM_DAT1[[i]]) <- paste0("item", 1:input$nitem)
        }
      })
      SIMDATA_ENV$simDataGPCM <- GPCM_DAT1
      render_dt_styled(GPCM_DAT1[[1]], "poly")
    })

    output$tab_grm <- DT::renderDT({
      req(is_started(input$start2), input$type2 == 4)
      GRM_B <- GRM_DAT1 <- list()
      withProgress(message = "GRM DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$polyrep) {
          incProgress(1 / input$polyrep)
          GRM_B[[i]] <- catR::genPolyMatrix(items = input$nitem,
                                            nrCat = as.numeric(input$sec),
                                            model = "GRM", seed = i,
                                            same.nrCat = TRUE, cbControl = NULL)
          GRM_B1 <- as.matrix(GRM_B)
          GRM_DAT1[[i]] <- catR::genPattern(rnorm(input$nn), GRM_B1[[i]], model = "GRM")
          colnames(GRM_DAT1[[i]]) <- paste0("item", 1:input$nitem)
        }
      })
      SIMDATA_ENV$simDataGRM <- GRM_DAT1
      render_dt_styled(GRM_DAT1[[1]], "poly")
    })



    ##  DIF DATA GENERATION 

    # DATASIM_11: DIF retimi veri ve parametre tablolarn ayr retir.
    # Veri tablosunda sadece yant verisi gsterilir; parametreler altta ayr tabloda sunulur.
    # DIF'li maddeler her replikasyonda rastgele atanr; sralama yaplmaz.
    generate_dif_bundle <- eventReactive(input$start4, {
      req(is_started(input$start4))

      nitem <- as.integer(input$dif_items)
      n     <- as.integer(input$dif_n)
      nrep  <- as.integer(input$dif_rep)
      focal_prop <- as.numeric(input$dif_group_prop)
      dif_nitem <- max(1, min(nitem, round(nitem * as.numeric(input$dif_ratio))))
      dif_delta <- as.numeric(input$dif_size)
      direction <- if (identical(input$dif_direction, "easier")) -1 else 1
      direction_label <- if (direction == -1) "Focal easier" else "Focal harder"
      model <- as.character(input$dif_model)
      dif_type <- as.character(input$dif_type)
      b_range <- if (!is.null(input$dif_b_range)) input$dif_b_range else c(-1.5, 1.5)
      a_range <- if (!is.null(input$dif_a_range)) input$dif_a_range else c(0.70, 1.50)
      c_range <- if (!is.null(input$dif_c_range)) input$dif_c_range else c(0, 0.20)

      generate_dif_once <- function(seed, rep_id) {
        set.seed(seed)
        group <- rbinom(n, size = 1, prob = focal_prop) # 0 = reference, 1 = focal
        theta <- rnorm(n, 0, 1)

        # DATASIM_11: DIF maddeleri sral deil, random srada atanr.
        dif_items <- sample(seq_len(nitem), dif_nitem, replace = FALSE)

        b_ref <- runif(nitem, min(b_range), max(b_range))
        if (model == "rasch") {
          a_ref <- rep(1, nitem)
        } else {
          a_ref <- runif(nitem, min(a_range), max(a_range))
        }
        c_ref <- if (model == "3pl") runif(nitem, min(c_range), max(c_range)) else rep(0, nitem)

        b_foc <- b_ref
        a_foc <- a_ref
        c_foc <- c_ref

        if (dif_type %in% c("uniform", "mixed")) {
          b_foc[dif_items] <- b_ref[dif_items] + direction * dif_delta
        }
        if (dif_type %in% c("nonuniform", "mixed")) {
          # Non-uniform DIF: focal grup iin ayrt edicilik deitirilir.
          a_foc[dif_items] <- pmax(0.05, a_ref[dif_items] + direction * dif_delta)
        }

        resp <- matrix(NA_integer_, nrow = n, ncol = nitem)
        for (j in seq_len(nitem)) {
          aa <- ifelse(group == 1, a_foc[j], a_ref[j])
          bb <- ifelse(group == 1, b_foc[j], b_ref[j])
          cc <- ifelse(group == 1, c_foc[j], c_ref[j])
          p <- cc + (1 - cc) / (1 + exp(-aa * (theta - bb)))
          resp[, j] <- rbinom(n, size = 1, prob = p)
        }

        out <- data.frame(
          Respondent_ID = paste0("Respondent_", seq_len(n)),
          Group = ifelse(group == 1, "Focal", "Reference"),
          Theta = round(theta, 3),
          resp,
          check.names = FALSE
        )
        names(out)[4:ncol(out)] <- paste0("item", seq_len(nitem))

        param <- data.frame(
          Replication = rep_id,
          Item = paste0("item", seq_len(nitem)),
          DIF_Status = ifelse(seq_len(nitem) %in% dif_items, "DIF", "No DIF"),
          DIF_Assignment_Order = ifelse(seq_len(nitem) %in% dif_items, match(seq_len(nitem), dif_items), NA_integer_),
          Model = toupper(model),
          DIF_Type = dif_type,
          DIF_Direction = ifelse(seq_len(nitem) %in% dif_items, direction_label, "No DIF"),
          DIF_Size = ifelse(seq_len(nitem) %in% dif_items, dif_delta, 0),
          a_reference = round(a_ref, 3),
          a_focal = round(a_foc, 3),
          b_reference = round(b_ref, 3),
          b_focal = round(b_foc, 3),
          c_reference = round(c_ref, 3),
          c_focal = round(c_foc, 3),
          check.names = FALSE
        )

        list(data = out, params = param, dif_items = paste0("item", dif_items))
      }

      seeds <- sample(1:1000000, nrep)
      SIMDATA_ENV$seedDIF <- seeds
      dif_list <- vector("list", nrep)
      param_list <- vector("list", nrep)
      dif_item_list <- vector("list", nrep)

      withProgress(message = "DIF DATA GENERATION IN PROGRESS", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in seq_len(nrep)) {
          incProgress(1 / nrep)
          tmp <- generate_dif_once(seeds[i], i)
          dif_list[[i]] <- tmp$data
          param_list[[i]] <- tmp$params
          dif_item_list[[i]] <- tmp$dif_items
        }
      })

      SIMDATA_ENV$simDataDIF <- dif_list
      SIMDATA_ENV$simDIFParams <- param_list
      SIMDATA_ENV$simDIFItems <- dif_item_list

      # DATA_SIM_15: Generate sonras replikasyon seenekleri 1:nrep olarak gncellenir.
      shinyWidgets::updatePickerInput(
        session,
        inputId = "dif_rep_view",
        choices = as.character(seq_len(nrep)),
        selected = "1"
      )

      list(data = dif_list, params = param_list, dif_items = dif_item_list)
    }, ignoreInit = TRUE)

    output$tab_dif <- DT::renderDT({
      x <- generate_dif_bundle()
      req(!is.null(x$data), length(x$data) > 0)

      # DATA_SIM_15: Sidebar'daki seime gre ilgili replikasyonun veri seti gsterilir.
      rep_index <- suppressWarnings(as.integer(input$dif_rep_view))
      if (is.null(rep_index) || length(rep_index) == 0 || is.na(rep_index)) rep_index <- 1L
      rep_index <- max(1L, min(rep_index, length(x$data)))

      render_dt_styled(x$data[[rep_index]], "dif")
    })

    output$tab_dif_params <- DT::renderDT({
      x <- generate_dif_bundle()
      req(!is.null(x$params), length(x$params) > 0)

      # DATA_SIM_15: Ayn replikasyonun DIF item parameters ve DIF status bilgileri gsterilir.
      rep_index <- suppressWarnings(as.integer(input$dif_rep_view))
      if (is.null(rep_index) || length(rep_index) == 0 || is.na(rep_index)) rep_index <- 1L
      rep_index <- max(1L, min(rep_index, length(x$params)))

      DT::datatable(x$params[[rep_index]], rownames = FALSE,
                    options = dt_opts("dif"),
                    class = "display compact hover stripe") %>%
        DT::formatStyle(
          columns = "DIF_Status",
          target = "row",
          backgroundColor = DT::styleEqual("DIF", "#fee2e2"),
          color = DT::styleEqual("DIF", "#991b1b"),
          fontWeight = DT::styleEqual("DIF", "bold")
        )
    })


    ##  DATA_SIM_17: DIF ANALYSIS DIAGNOSTICS 

    classify_mh_level <- function(delta_mh) {
      ad <- abs(delta_mh)
      ifelse(ad < 1.00, "A / Negligible",
             ifelse(ad < 1.50, "B / Moderate", "C / Large"))
    }

    classify_logistic_level <- function(delta_r2) {
      ifelse(delta_r2 < 0.035, "A / Negligible",
             ifelse(delta_r2 < 0.070, "B / Moderate", "C / Large"))
    }

    safe_mh_single_item <- function(dat, item_names, item_name) {
      y <- suppressWarnings(as.integer(dat[[item_name]]))
      g <- ifelse(dat$Group == "Focal", 1L, 0L)
      score <- rowSums(dat[, item_names, drop = FALSE], na.rm = TRUE) - y

      ok <- !is.na(y) & !is.na(g) & !is.na(score)
      y <- y[ok]; g <- g[ok]; score <- score[ok]

      if (length(unique(y)) < 2 || length(unique(g)) < 2) {
        return(list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE))
      }

      strata <- sort(unique(score))
      mats <- list()
      num <- 0
      den <- 0

      for (s in strata) {
        idx <- which(score == s)
        if (length(idx) < 2) next

        tab <- table(
          factor(g[idx], levels = c(0, 1)),
          factor(y[idx], levels = c(0, 1))
        )

        if (sum(tab[1, ]) == 0 || sum(tab[2, ]) == 0 || sum(tab[, 1]) == 0 || sum(tab[, 2]) == 0) next

        mats[[length(mats) + 1]] <- as.matrix(tab)

        n_s <- sum(tab)
        # Rows: Reference, Focal; columns: 0, 1. Haldane correction for stability.
        tt <- tab + 0.5
        num <- num + (tt[1, 2] * tt[2, 1] / n_s)
        den <- den + (tt[1, 1] * tt[2, 2] / n_s)
      }

      if (length(mats) < 1 || den <= 0) {
        return(list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE))
      }

      arr <- array(unlist(mats), dim = c(2, 2, length(mats)))
      pval <- tryCatch(stats::mantelhaen.test(arr, correct = FALSE)$p.value,
                       error = function(e) 1)

      alpha_mh <- ifelse(den <= 0, 1, num / den)
      delta_mh <- -2.35 * log(alpha_mh)
      level <- classify_mh_level(delta_mh)
      detected <- is.finite(pval) && pval < 0.05 && level != "A / Negligible"

      list(
        statistic = suppressWarnings(as.numeric(stats::qchisq(1 - pval, df = 1))),
        p = pval,
        effect = delta_mh,
        level = level,
        detected = detected
      )
    }

    safe_logistic_single_item <- function(dat, item_names, item_name) {
      y <- suppressWarnings(as.integer(dat[[item_name]]))
      g <- ifelse(dat$Group == "Focal", 1L, 0L)
      score <- rowSums(dat[, item_names, drop = FALSE], na.rm = TRUE) - y

      dd <- data.frame(y = y, score = score, g = g)
      dd <- dd[stats::complete.cases(dd), ]

      if (nrow(dd) < 20 || length(unique(dd$y)) < 2 || length(unique(dd$g)) < 2) {
        return(list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE, dif_type = "Not detected"))
      }

      out <- tryCatch({
        m0 <- stats::glm(y ~ score, data = dd, family = stats::binomial())
        m1 <- stats::glm(y ~ score + g, data = dd, family = stats::binomial())
        m2 <- stats::glm(y ~ score * g, data = dd, family = stats::binomial())

        a01 <- stats::anova(m0, m1, test = "Chisq")
        a12 <- stats::anova(m1, m2, test = "Chisq")

        p_group <- suppressWarnings(as.numeric(a01$`Pr(>Chi)`[2]))
        p_inter <- suppressWarnings(as.numeric(a12$`Pr(>Chi)`[2]))
        if (!is.finite(p_group)) p_group <- 1
        if (!is.finite(p_inter)) p_inter <- 1

        pval <- min(p_group, p_inter, na.rm = TRUE)
        dif_type <- if (p_inter < 0.05) {
          "Non-uniform / interaction"
        } else if (p_group < 0.05) {
          "Uniform / group"
        } else {
          "Not detected"
        }

        mnull <- stats::glm(y ~ 1, data = dd, family = stats::binomial())
        pseudo <- function(model, null_model) {
          val <- 1 - (as.numeric(stats::logLik(model)) / as.numeric(stats::logLik(null_model)))
          ifelse(is.finite(val), val, 0)
        }
        delta_r2 <- max(0, pseudo(m2, mnull) - pseudo(m0, mnull))
        level <- classify_logistic_level(delta_r2)
        detected <- is.finite(pval) && pval < 0.05 && level != "A / Negligible"

        list(
          statistic = suppressWarnings(as.numeric(stats::qchisq(1 - pval, df = 1))),
          p = pval,
          effect = delta_r2,
          level = level,
          detected = detected,
          dif_type = dif_type
        )
      }, error = function(e) {
        list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE, dif_type = "Not detected")
      })

      out
    }


    # DATA_SIM_19: Lord Ki-kare yaklam. Simlasyonda gerek grup bazl madde
    # parametreleri bulunduu iin a/b/c farklar zerinden yaklak Wald/Lord testi retir.
    safe_lord_single_item <- function(prm, item_name, n_ref, n_foc) {
      row <- prm[prm$Item == item_name, , drop = FALSE]
      if (nrow(row) < 1) {
        return(list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE, dif_type = "Not detected"))
      }

      pars <- c("a", "b")
      if ("c_reference" %in% names(row) && any(row$c_reference != 0 | row$c_focal != 0, na.rm = TRUE)) {
        pars <- c(pars, "c")
      }

      diffs <- numeric(0)
      vars  <- numeric(0)
      for (pp in pars) {
        ref_col <- paste0(pp, "_reference")
        foc_col <- paste0(pp, "_focal")
        if (ref_col %in% names(row) && foc_col %in% names(row)) {
          d <- suppressWarnings(as.numeric(row[[foc_col]] - row[[ref_col]]))
          # Conservative approximate SE; decreases with sample size, remains stable for classroom simulation.
          se <- if (pp == "b") sqrt(1 / max(20, n_ref) + 1 / max(20, n_foc)) * 2.00 else
                sqrt(1 / max(20, n_ref) + 1 / max(20, n_foc)) * 1.25
          diffs <- c(diffs, d)
          vars  <- c(vars, se^2)
        }
      }

      if (length(diffs) == 0 || any(!is.finite(diffs)) || any(!is.finite(vars)) || any(vars <= 0)) {
        return(list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE, dif_type = "Not detected"))
      }

      chisq <- sum((diffs^2) / vars)
      df <- length(diffs)
      pval <- stats::pchisq(chisq, df = df, lower.tail = FALSE)
      effect <- sqrt(chisq / max(1, df))
      level <- ifelse(effect < 1.00, "A / Negligible",
                      ifelse(effect < 1.50, "B / Moderate", "C / Large"))
      detected <- is.finite(pval) && pval < 0.05 && level != "A / Negligible"

      list(
        statistic = chisq,
        p = pval,
        effect = effect,
        level = level,
        detected = detected,
        dif_type = "Lord parameter difference"
      )
    }

    # DATA_SIM_19: IRT likelihood-ratio mantna yakn biimde, retilmi theta
    # deikeniyle kstl ve grup etkileimli lojistik modeller karlatrlr.
    safe_irt_lr_single_item <- function(dat, item_names, item_name) {
      y <- suppressWarnings(as.integer(dat[[item_name]]))
      g <- ifelse(dat$Group == "Focal", 1L, 0L)
      theta <- suppressWarnings(as.numeric(dat$Theta))

      dd <- data.frame(y = y, theta = theta, g = g)
      dd <- dd[stats::complete.cases(dd), ]

      if (nrow(dd) < 20 || length(unique(dd$y)) < 2 || length(unique(dd$g)) < 2) {
        return(list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE, dif_type = "Not detected"))
      }

      out <- tryCatch({
        m0 <- stats::glm(y ~ theta, data = dd, family = stats::binomial())
        m1 <- stats::glm(y ~ theta * g, data = dd, family = stats::binomial())

        lr <- 2 * (as.numeric(stats::logLik(m1)) - as.numeric(stats::logLik(m0)))
        if (!is.finite(lr) || lr < 0) lr <- 0
        pval <- stats::pchisq(lr, df = 2, lower.tail = FALSE)

        mnull <- stats::glm(y ~ 1, data = dd, family = stats::binomial())
        pseudo <- 1 - (as.numeric(stats::logLik(m1)) / as.numeric(stats::logLik(mnull)))
        base_pseudo <- 1 - (as.numeric(stats::logLik(m0)) / as.numeric(stats::logLik(mnull)))
        effect <- max(0, pseudo - base_pseudo)
        if (!is.finite(effect)) effect <- 0

        level <- classify_logistic_level(effect)
        detected <- is.finite(pval) && pval < 0.05 && level != "A / Negligible"

        list(
          statistic = lr,
          p = pval,
          effect = effect,
          level = level,
          detected = detected,
          dif_type = "IRT likelihood ratio"
        )
      }, error = function(e) {
        list(statistic = NA_real_, p = 1, effect = 0, level = "A / Negligible", detected = FALSE, dif_type = "Not detected")
      })

      out
    }

    compute_dif_diagnostics <- function(method = "MH") {
      data_list <- SIMDATA_ENV$simDataDIF
      param_list <- SIMDATA_ENV$simDIFParams
      validate(need(!is.null(data_list) && length(data_list) > 0, "Please generate DIF data first."))

      nrep <- length(data_list)
      all_rows <- list()

      withProgress(message = "DIF ANALYSIS IN PROGRESS", style = "notification", value = 0, {
        for (r in seq_len(nrep)) {
          incProgress(1 / nrep)
          dat <- data_list[[r]]
          prm <- param_list[[r]]
          item_names <- grep("^item", names(dat), value = TRUE)

          rep_rows <- lapply(item_names, function(it) {
            true_status <- if (it %in% prm$Item) prm$DIF_Status[match(it, prm$Item)] else "No DIF"

            n_ref <- sum(dat$Group == "Reference", na.rm = TRUE)
            n_foc <- sum(dat$Group == "Focal", na.rm = TRUE)

            res <- if (identical(method, "MH")) {
              safe_mh_single_item(dat, item_names, it)
            } else if (identical(method, "LOGISTIC")) {
              safe_logistic_single_item(dat, item_names, it)
            } else if (identical(method, "LORD")) {
              safe_lord_single_item(prm, it, n_ref, n_foc)
            } else if (identical(method, "IRT_LR")) {
              safe_irt_lr_single_item(dat, item_names, it)
            } else {
              safe_mh_single_item(dat, item_names, it)
            }

            data.frame(
              Method = method,
              Replication = r,
              Item = it,
              True_DIF_Status = true_status,
              Detected = isTRUE(res$detected),
              DIF_Level = res$level,
              Statistic = round(res$statistic, 3),
              p = round(res$p, 5),
              Effect_Size = round(res$effect, 4),
              Detected_DIF_Type = ifelse(is.null(res$dif_type), method, res$dif_type),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          })

          all_rows[[r]] <- do.call(rbind, rep_rows)
        }
      })

      all_df <- do.call(rbind, all_rows)
      true_dif <- all_df$True_DIF_Status == "DIF"
      detected <- all_df$Detected

      fp_n <- sum(detected & !true_dif, na.rm = TRUE)
      no_dif_n <- sum(!true_dif, na.rm = TRUE)
      fp_rate <- fp_n / max(1, no_dif_n)

      perf <- data.frame(
        Method = method,
        Replications = nrep,
        True_DIF_Cases = sum(true_dif, na.rm = TRUE),
        No_DIF_Cases = no_dif_n,
        Detected_True_DIF = sum(detected & true_dif, na.rm = TRUE),
        False_Positive = paste0(fp_n, " (", round(100 * fp_rate, 1), "%)"),
        Test_Power = round(sum(detected & true_dif, na.rm = TRUE) / max(1, sum(true_dif, na.rm = TRUE)), 3),
        Type_I_Error = round(fp_rate, 3),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      list(items = all_df, performance = perf)
    }

    get_dif_diagnostics_cached <- function(method = "MH") {
      method <- ifelse(is.null(method) || length(method) == 0, "MH", method)
      key <- paste(method, length(SIMDATA_ENV$simDataDIF), paste(SIMDATA_ENV$seedDIF, collapse = "_"), sep = "|")

      cache <- SIMDATA_ENV$difDiagnosticsCache
      if (is.null(cache) || is.null(cache$key) || !identical(cache$key, key)) {
        cache <- list(key = key, result = compute_dif_diagnostics(method))
        SIMDATA_ENV$difDiagnosticsCache <- cache
      }
      cache$result
    }

    observeEvent(input$open_dif_analysis, {
      req(is_started(input$start4))
      req(!is.null(SIMDATA_ENV$simDataDIF), length(SIMDATA_ENV$simDataDIF) > 0)

      showModal(modalDialog(
        title = tags$strong("DIF Analysis Results"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        fluidRow(
          column(
            4,
            shinyWidgets::pickerInput(
              inputId = "dif_analysis_method",
              label = tags$span(style = "font-weight:700;", "DIF Method"),
              choices = c("Mantel-Haenszel" = "MH",
                          "Logistic Regression" = "LOGISTIC",
                          "Lord Chi-square" = "LORD",
                          "IRT Likelihood Ratio" = "IRT_LR"),
              selected = "MH",
              width = "100%",
              options = shinyWidgets::pickerOptions(style = "btn-secondary btn-sm")
            )
          ),
          column(
            4,
            # DATA_SIM_18: DIF Analysis penceresi iinde bamsz replikasyon seici.
            shinyWidgets::pickerInput(
              inputId = "dif_analysis_rep_view",
              label = tags$span(style = "font-weight:700;", "Replication"),
              choices = as.character(seq_len(length(SIMDATA_ENV$simDataDIF))),
              selected = as.character(max(1L, min(
                suppressWarnings(as.integer(ifelse(is.null(input$dif_rep_view), 1, input$dif_rep_view))),
                length(SIMDATA_ENV$simDataDIF)
              ))),
              width = "100%",
              options = shinyWidgets::pickerOptions(style = "btn-secondary btn-sm", size = 6)
            )
          ),
          column(
            4,
            tags$div(
              style = "margin-top:26px; font-weight:600; color:#555;",
              "Selected-replication results are controlled here. The performance table is computed across all replications."
            )
          )
        ),
        tags$hr(),
        tags$h4(style = "font-weight:700;", "Detected DIF Items in Selected Replication"),
        DT::DTOutput("dif_analysis_items"),
        tags$br(),
        tags$h4(style = "font-weight:700;", "Overall Detection Performance Across All Replications"),
        DT::DTOutput("dif_analysis_performance")
      ))
    })

    output$dif_analysis_items <- DT::renderDT({
      req(!is.null(SIMDATA_ENV$simDataDIF), length(SIMDATA_ENV$simDataDIF) > 0)
      method <- ifelse(is.null(input$dif_analysis_method), "MH", input$dif_analysis_method)
      res <- get_dif_diagnostics_cached(method)

      # DATA_SIM_18: DIF Analysis penceresindeki replikasyon seimine gre sonu gsterilir.
      rep_index <- suppressWarnings(as.integer(input$dif_analysis_rep_view))
      if (is.null(rep_index) || length(rep_index) == 0 || is.na(rep_index)) rep_index <- 1L
      rep_index <- max(1L, min(rep_index, length(SIMDATA_ENV$simDataDIF)))

      out <- res$items[res$items$Replication == rep_index & res$items$Detected, , drop = FALSE]
      out <- out[, c("Replication", "Item", "DIF_Level", "Statistic", "p", "Effect_Size", "Detected_DIF_Type", "True_DIF_Status"), drop = FALSE]

      if (nrow(out) == 0) {
        out <- data.frame(Message = "No DIF item detected for the selected replication and method.", check.names = FALSE)
      }

      DT::datatable(out, rownames = FALSE, options = dt_opts("dif"),
                    class = "display compact hover stripe")
    })

    output$dif_analysis_performance <- DT::renderDT({
      req(!is.null(SIMDATA_ENV$simDataDIF), length(SIMDATA_ENV$simDataDIF) > 0)
      method <- ifelse(is.null(input$dif_analysis_method), "MH", input$dif_analysis_method)
      res <- get_dif_diagnostics_cached(method)

      DT::datatable(res$performance, rownames = FALSE, options = dt_opts("dif"),
                    class = "display compact hover stripe")
    })

    ##  MULTIDIMENSIONAL DATA GENERATION 

    # DATA_SIM_16: ok boyutlu veriler artk beklenen faktrler aras korelasyonu
    # dikkate alan korelasyonlu gizil faktrler zerinden retilir.
    make_factor_correlation_matrix <- function(nfac, target_cor) {
      nfac <- max(1L, as.integer(nfac))
      target_cor <- suppressWarnings(as.numeric(target_cor))
      if (is.null(target_cor) || length(target_cor) == 0 || is.na(target_cor)) target_cor <- 0
      target_cor <- max(0, min(0.95, target_cor))

      R <- diag(nfac)
      if (nfac > 1) {
        R[lower.tri(R)] <- target_cor
        R[upper.tri(R)] <- target_cor
      }
      R
    }

    simulate_correlated_factor_data <- function(nitem, n, nfac, factorloading,
                                                factor_cor = 0.30,
                                                lowfactoritem = 0,
                                                data_type = c("poly", "dicho", "cont")) {
      data_type <- match.arg(data_type)
      nitem <- as.integer(nitem)
      n <- as.integer(n)
      nfac <- max(1L, as.integer(nfac))
      factorloading <- suppressWarnings(as.numeric(factorloading))
      if (is.null(factorloading) || length(factorloading) == 0 || is.na(factorloading)) factorloading <- 0.55
      factorloading <- max(0.05, min(0.95, factorloading))

      # Her madde ana faktrne atanr; faktrler aras korelasyon kullanc girdisine gre kurulur.
      item_factor <- rep(seq_len(nfac), length.out = nitem)
      Rf <- make_factor_correlation_matrix(nfac, factor_cor)

      Z <- matrix(rnorm(n * nfac), nrow = n, ncol = nfac)
      F_scores <- Z %*% chol(Rf)

      E <- matrix(rnorm(n * nitem), nrow = n, ncol = nitem)
      X <- matrix(NA_real_, nrow = n, ncol = nitem)

      for (j in seq_len(nitem)) {
        f <- item_factor[j]
        X[, j] <- factorloading * F_scores[, f] + sqrt(1 - factorloading^2) * E[, j]
      }

      low_idx <- integer(0)
      if (lowfactoritem > 0) {
        low_idx <- sample(seq_len(nitem), min(lowfactoritem, nitem))
        X[, low_idx] <- matrix(rnorm(n * length(low_idx)), nrow = n, ncol = length(low_idx))
      }

      if (data_type == "poly") {
        data1 <- as.data.frame(apply(X, 2, function(z) {
          as.integer(cut(z,
                         breaks = c(-Inf, -1.2815516, -0.4537622, 0.4537622, 1.2815516, Inf),
                         labels = c("1", "2", "3", "4", "5")))
        }))
      } else if (data_type == "dicho") {
        data1 <- as.data.frame(apply(X, 2, function(z) as.integer(z > 0)))
      } else {
        data1 <- as.data.frame(round(X, 4))
      }

      colnames(data1) <- paste0("item", seq_len(nitem))
      attr(data1, "low_items") <- low_idx
      attr(data1, "factor_correlation_matrix") <- Rf
      attr(data1, "item_factor") <- item_factor
      data1
    }

    output$tab_fac_Poly <- DT::renderDT({
      req(is_started(input$start3), multi_data_type(input$ftype1) == "poly")

      seed_vec <- sample(1:1000000, input$frep)
      SIMDATA_ENV$seedPoly <- seed_vec
      listGL1 <- list()

      withProgress(message = "MULTIDIM (POLY) DATA GENERATION", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$frep) {
          incProgress(1 / input$frep)
          set.seed(seed_vec[i])
          listGL1[[i]] <- simulate_correlated_factor_data(
            nitem = input$fnitem, n = input$fnn, nfac = input$nfac,
            factorloading = input$meanload, factor_cor = input$fac_cor,
            lowfactoritem = input$nlfl, data_type = "poly"
          )
        }
      })

      SIMDATA_ENV$SimMultiDataPoly1 <- listGL1
      SIMDATA_ENV$multiFactorCor <- make_factor_correlation_matrix(input$nfac, input$fac_cor)
      SIMDATA_ENV$low <- attr(listGL1[[1]], "low_items")

      dt_out <- render_dt_styled(listGL1[[1]], "multi")
      if (input$nlfl > 0 && length(SIMDATA_ENV$low) > 0) {
        dt_out <- dt_out %>% DT::formatStyle(SIMDATA_ENV$low, backgroundColor = "#fde68a")
      }
      dt_out
    })

    output$tab_fac_Dicho <- DT::renderDT({
      req(is_started(input$start3), multi_data_type(input$ftype1) == "dicho")

      seed_vec <- sample(1:1000000, input$frep)
      SIMDATA_ENV$seedDicho <- seed_vec
      listGL1 <- list()

      withProgress(message = "MULTIDIM (DICHO) DATA GENERATION", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$frep) {
          incProgress(1 / input$frep)
          set.seed(seed_vec[i])
          listGL1[[i]] <- simulate_correlated_factor_data(
            nitem = input$fnitem, n = input$fnn, nfac = input$nfac,
            factorloading = input$meanload, factor_cor = input$fac_cor,
            lowfactoritem = input$nlfl, data_type = "dicho"
          )
        }
      })

      SIMDATA_ENV$SimMultiDataDicho1 <- listGL1
      SIMDATA_ENV$multiFactorCor <- make_factor_correlation_matrix(input$nfac, input$fac_cor)
      SIMDATA_ENV$lowD <- attr(listGL1[[1]], "low_items")

      dt_out <- render_dt_styled(listGL1[[1]], "multi")
      if (input$nlfl > 0 && length(SIMDATA_ENV$lowD) > 0) {
        dt_out <- dt_out %>% DT::formatStyle(SIMDATA_ENV$lowD, backgroundColor = "#fde68a")
      }
      dt_out
    })

    output$tab_fac_Cont <- DT::renderDT({
      req(is_started(input$start3), multi_data_type(input$ftype1) == "cont")

      seed_vec <- sample(1:1000000, input$frep)
      SIMDATA_ENV$seedCont <- seed_vec
      listGL1 <- list()

      withProgress(message = "MULTIDIM (CONTINUOUS) DATA GENERATION", style = "notification", value = 0.1, {
        Sys.sleep(0.2)
        for (i in 1:input$frep) {
          incProgress(1 / input$frep)
          set.seed(seed_vec[i])
          listGL1[[i]] <- simulate_correlated_factor_data(
            nitem = input$fnitem, n = input$fnn, nfac = input$nfac,
            factorloading = input$meanload, factor_cor = input$fac_cor,
            lowfactoritem = input$nlfl, data_type = "cont"
          )
        }
      })

      SIMDATA_ENV$SimMultiDataCont1 <- listGL1
      SIMDATA_ENV$multiFactorCor <- make_factor_correlation_matrix(input$nfac, input$fac_cor)
      SIMDATA_ENV$lowC <- attr(listGL1[[1]], "low_items")

      dt_out <- render_dt_styled(listGL1[[1]], "multi")
      if (input$nlfl > 0 && length(SIMDATA_ENV$lowC) > 0) {
        dt_out <- dt_out %>% DT::formatStyle(SIMDATA_ENV$lowC, backgroundColor = "#fde68a")
      }
      dt_out
    })

    output$tab_fac_all <- renderUI({
      tp <- multi_data_type(input$ftype1)
      if (tp == "poly") {
        withLoader(DT::DTOutput("tab_fac_Poly"), type = "html", loader = "loader1")
      } else if (tp == "dicho") {
        withLoader(DT::DTOutput("tab_fac_Dicho"), type = "html", loader = "loader1")
      } else {
        withLoader(DT::DTOutput("tab_fac_Cont"), type = "html", loader = "loader1")
      }
    })

    # DATASIM_4: Ayr downloadHandler blm kaldrld.

    session$onSessionEnded(function() { stopApp() })

  } # close server

  shinyApp(ui = ui, server = server)
}
