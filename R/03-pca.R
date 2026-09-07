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

  ##  PCA theme CSS (teal/green palette) 
  pca_theme_css <- "
    :root {
      --tc:  #0e7490;
      --tdk: #0c4a6e;
      --tlt: #38bdf8;
      --tpl: #7dd3fc;
      --tls: #e0f2fe;
    }

    body { background:#ffffff !important; color:#0f1f2e !important;
           font-family:'Segoe UI',Arial,sans-serif; }

    .nav-tabs > li > a {
      color: var(--tdk) !important; font-weight:600;
      border-radius:8px 8px 0 0 !important;
      background:#f0f9ff !important;
      white-space:nowrap !important;
    }
    .nav-tabs > li > a h4 { margin:0 !important; padding:0 !important; white-space:nowrap !important; }
    @media (max-width: 1280px) {
      .nav-tabs > li > a { font-size:12px !important; padding:6px 8px !important; }
      .nav-tabs > li > a h4 { font-size:12px !important; }
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background:color-mix(in srgb, var(--tc) 72%, #0f1f2e) !important;
      color:#ffffff !important;
      border-color:color-mix(in srgb, var(--tc) 82%, #0f1f2e) !important;
      box-shadow:inset 0 -2px 0 color-mix(in srgb, #ffffff 34%, transparent) !important;
    }
    .nav-tabs > li.active > a h4,
    .nav-tabs > li.active > a:focus h4,
    .nav-tabs > li.active > a:hover h4 {
      color:#ffffff !important;
    }
    .nav-tabs > li > a:hover { background:var(--tls) !important; color:var(--tdk) !important; }

    .well { background:#f0f9ff !important; border:1.5px solid var(--tpl) !important; border-radius:12px !important; }

    .btn, .action-button, .bttn,
    .btn-primary, .btn-default, .btn-file,
    .bttn-primary, .bttn-default, .bttn-jelly,
    .bttn-unite, .bttn-material-flat {
      min-width:160px !important; height:38px !important; font-size:13px !important;
      font-weight:700 !important; border-radius:8px !important; padding:0 14px !important;
      display:inline-flex !important; align-items:center !important; justify-content:center !important;
      transition:background .2s, box-shadow .15s !important; box-sizing:border-box !important;
    }
    .btn-default, .btn-group > .btn:not(.active) {
      background:#ffffff !important; border:2px solid var(--tpl) !important; color:var(--tdk) !important;
    }
    .btn-group > .btn:not(.active):hover { background:var(--tls) !important; color:var(--tdk) !important; }
    .btn-primary,
    .btn-group > .btn.active,
    .bttn-primary, .bttn-jelly.bttn-primary,
    .bttn-unite.bttn-primary,
    .bttn-material-flat.bttn-primary {
      background:var(--tc) !important; border:2px solid var(--tdk) !important; color:#fff !important;
      box-shadow:0 2px 6px rgba(14,116,144,.35) !important;
    }
    .btn-primary:hover, .btn-group > .btn.active:hover, .bttn-primary:hover { background:var(--tdk) !important; }
    .btn-file { background:var(--tc) !important; border:2px solid var(--tdk) !important; color:#fff !important; }
    .form-control[readonly] { background:#fff !important; color:#0f1f2e !important; }

    .bootstrap-select .btn { border:2px solid var(--tpl) !important; background:#fff !important; color:var(--tdk) !important; }
    .dropdown-menu > li > a,
    .dropdown-menu > li > a *,
    .bootstrap-select .dropdown-menu li a,
    .bootstrap-select .dropdown-menu li a *,
    .bootstrap-select .dropdown-menu li a span.text,
    .bootstrap-select .dropdown-menu .text,
    .bootstrap-select .dropdown-menu .glyphicon { color:#111111 !important; }
    .bootstrap-select .dropdown-menu li a:hover { background:var(--tls) !important; }

    table.dataTable thead th { background:var(--tc) !important; color:#fff !important; font-size:14px; }
    table.dataTable tbody tr:hover { background:var(--tls) !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current,
    .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
      background:var(--tc) !important; color:#fff !important; border-radius:6px;
    }

    .section-header {
      color:var(--tdk); font-size:20px; font-family:'Segoe UI',Arial,sans-serif;
      font-weight:700; letter-spacing:1px; margin-bottom:8px;
      border-left:5px solid var(--tc); padding-left:10px;
    }

    #tepe { border-bottom:8px solid var(--tc) !important; padding-bottom:8px; margin-bottom:8px; }
    #title  { color:var(--tdk) !important; font-size:26px !important; font-weight:800 !important; font-style:normal !important; }
    #title2 { color:var(--tlt) !important; font-size:14px !important; text-align:right !important; font-style:normal !important; }

    /*  PCA visual cards  */
    .pca-sidebar-simple, .pca-sidebar-simple * { box-sizing:border-box; }
    .pca-sidebar-simple {
      width:100%; height:128px; min-height:128px; max-height:128px;
      margin:0 0 12px 0; padding:12px 13px;
      position:relative; overflow:hidden; border-radius:16px;
      background:linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--tc) 9%, #ffffff) 56%, color-mix(in srgb, var(--tc) 18%, #ffffff) 100%);
      border:1px solid color-mix(in srgb, var(--tc) 24%, #ffffff);
      box-shadow:0 10px 24px rgba(14, 116, 144, .10);
      color:#07183f;
    }
    .pca-sidebar-simple:before {
      content:''; position:absolute; right:-42px; bottom:-68px;
      width:150px; height:122px; border-radius:58% 42% 0 0;
      background:linear-gradient(135deg, color-mix(in srgb, var(--tc) 22%, #ffffff), var(--tc));
      opacity:.55; transform:rotate(-8deg);
    }
    .pca-sidebar-simple:after {
      content:''; position:absolute; right:10px; top:9px; width:46px; height:46px;
      background-image:radial-gradient(var(--tc) 1.6px, transparent 2px);
      background-size:12px 12px; opacity:.18;
    }
    .pca-sidebar-content { position:relative; z-index:2; }
    .pca-sidebar-package {
      display:inline-flex; align-items:center; gap:6px; max-width:100%;
      padding:4px 8px; border-radius:999px;
      background:rgba(255,255,255,.72);
      border:1px solid color-mix(in srgb, var(--tc) 18%, #ffffff);
      color:var(--tdk); font-size:11px; font-weight:850; letter-spacing:.15px;
      white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
      margin-bottom:8px;
    }
    .pca-sidebar-mark { width:13px; height:13px; border-radius:4px; flex:0 0 13px; background:linear-gradient(135deg, var(--tc), color-mix(in srgb, var(--tc) 48%, #ffffff)); position:relative; }
    .pca-sidebar-mark:after { content:''; position:absolute; inset:3.5px; border:1.3px solid #fff; border-radius:2.5px; }
    .pca-sidebar-lines { margin-top:6px; display:grid; gap:4px; max-width:94%; }
    .pca-sidebar-lines div {
      color:#07183f; font-weight:900; font-size:13px; line-height:1.22;
      letter-spacing:.08px; white-space:normal; overflow:visible; text-overflow:clip;
    }
    .pca-sidebar-lines div:nth-child(2) { color:var(--tc); }
    .pca-sidebar-credit {
      position:absolute; right:10px; bottom:8px; z-index:2;
      color:color-mix(in srgb, var(--tc) 72%, #17213d);
      font-size:9.4px; font-weight:800; letter-spacing:.1px;
      white-space:nowrap;
    }

    /*  PCA hero card  */
    .pca-hero, .pca-hero * { box-sizing:border-box; }
    .pca-hero {
      width:97%; min-height:340px; margin:0 auto 16px auto;
      position:relative; overflow:hidden; border-radius:22px;
      padding:28px 34px;
      background:
        radial-gradient(circle at 88% 92%, color-mix(in srgb, var(--tc) 16%, transparent) 0, transparent 33%),
        linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--tc) 5%, #ffffff) 62%, color-mix(in srgb, var(--tc) 11%, #ffffff) 100%);
      border:1px solid color-mix(in srgb, var(--tc) 18%, #ffffff);
      box-shadow:0 18px 42px rgba(14,116,144,.09);
      color:#07183f;
    }
    .pca-hero:after {
      content:''; position:absolute; right:22px; top:18px; width:62px; height:62px;
      background-image:radial-gradient(var(--tc) 1.8px, transparent 2.3px);
      background-size:14px 14px; opacity:.20;
    }
    .pca-hero-left { position:relative; z-index:2; padding-top:4px; }
    .pca-hero-brand { display:flex; align-items:center; gap:18px; margin-bottom:8px; }
    .pca-hero-logo {
      width:76px; height:76px; display:flex; flex-direction:column; align-items:center; justify-content:center;
      clip-path:polygon(25% 5%,75% 5%,100% 50%,75% 95%,25% 95%,0 50%);
      border:3px solid var(--tc); background:rgba(255,255,255,.68);
      color:#07183f; font-weight:900; line-height:1.06; box-shadow:0 10px 24px rgba(14,116,144,.08);
    }
    .pca-hero-logo b { color:var(--tc); font-size:23px; letter-spacing:-.8px; }
    .pca-hero-logo span { font-size:9.5px; }
    .pca-hero-brand-text { border-left:1px solid rgba(80,120,160,.18); padding-left:20px; }
    .pca-hero-pkg { font-size:20px; font-weight:900; margin-bottom:4px; }
    .pca-hero-domain { font-size:15px; color:#436070; font-weight:650; }
    .pca-hero-domain b { color:var(--tc); }
    .pca-hero-title {
      margin:0; font-size:clamp(28px, 3.8vw, 24px); line-height:1.02;
      font-weight:950; letter-spacing:-1.1px; color:#07183f;
      text-align:center; max-width:100%;
    }
    .pca-hero-title .pca-title-top { display:block; color:#07183f; }
    .pca-hero-title .pca-title-sub {
      display:block; text-align:center; margin-top:8px;
      color:var(--tc); font-size:.68em; letter-spacing:.5px; line-height:1.18;
    }
    .pca-hero-desc {
      max-width:720px; margin:14px auto 0 auto; text-align:center;
      font-size:14px; line-height:1.35; color:#4f6070; font-weight:500;
    }
    .pca-hero-bottom {
      position:relative; width:100%; height:112px; margin-top:16px;
      display:flex; justify-content:space-around; align-items:center;
      background:rgba(255,255,255,.68);
      border:1px solid color-mix(in srgb, var(--tc) 16%, #ffffff);
      border-radius:16px; box-shadow:0 10px 24px rgba(14,116,144,.05);
      padding:0;
    }
    .pca-hero-feature {
      flex:1 1 25%; height:100%; display:flex; flex-direction:column;
      align-items:center; justify-content:center; gap:8px;
      color:#07183f; font-size:11px; font-weight:900; line-height:1.2;
      border-right:1px solid rgba(80,120,160,.12);
    }
    .pca-hero-feature:last-child { border-right:0; }
    .pca-hero-feature .pca-emoji { font-size:28px; display:block; }
    .pca-hero-credit { position:absolute; right:28px; top:20px; color:color-mix(in srgb, var(--tc) 80%, #17213d); font-size:11px; font-weight:800; z-index:2; }

    #dynamic-theme-style {}
  "

  ##  PCA info card helper (sidebar-mini & hero) 
  pcaInfoCard <- function(mode = "sidebar-mini") {
    if (identical(mode, "sidebar-mini")) {
      return(
        shiny::div(
          class = "pca-sidebar-simple",
          shiny::div(
            class = "pca-sidebar-content",
            shiny::div(
              class = "pca-sidebar-package",
              shiny::span(class = "pca-sidebar-mark"),
              shiny::span("RSP Package")
            ),
            shiny::div(
              class = "pca-sidebar-lines",
              shiny::div("PRINCIPAL COMPONENT"),
              shiny::div("ANALYSIS (PCA)")
            )
          ),
          shiny::div(class = "pca-sidebar-credit", "Do\u011fan & Aybek (2022)")
        )
      )
    }

    if (identical(mode, "hero")) {
      return(
        shiny::div(
          class = "pca-hero",
          shiny::div(
            class = "pca-hero-left",
            shiny::div(
              class = "pca-hero-brand",
              shiny::div(
                class = "pca-hero-logo",
                shiny::span("R-Shiny"),
                tags$b("RSP"),
                shiny::span("Package")
              ),
              shiny::div(
                class = "pca-hero-brand-text",
                shiny::div(class = "pca-hero-pkg", "RSP Package"),
                shiny::div(class = "pca-hero-domain", "R-Shiny ", tags$b("\u2022"), " Psychometry")
              )
            ),
            shiny::h2(
              class = "pca-hero-title",
              shiny::span(class = "pca-title-top", "PRINCIPAL COMPONENT ANALYSIS"),
              shiny::span(class = "pca-title-sub", "DIMENSION REDUCTION \u2022 FACTOR STRUCTURE \u2022 PARALLEL ANALYSIS")
            ),
            shiny::div(
              class = "pca-hero-bottom",
              shiny::div(
                class = "pca-hero-feature",
                shiny::span(class = "pca-emoji", "\U0001F50D"),
                shiny::span("KMO & BARTLETT")
              ),
              shiny::div(
                class = "pca-hero-feature",
                shiny::span(class = "pca-emoji", "\U0001F4C9"),
                shiny::span("SCREE PLOT")
              ),
              shiny::div(
                class = "pca-hero-feature",
                shiny::span(class = "pca-emoji", "\U0001F9EE"),
                shiny::span("PARALLEL ANALYSIS")
              ),
              shiny::div(
                class = "pca-hero-feature",
                shiny::span(class = "pca-emoji", "\U0001F4CA"),
                shiny::span("COMPONENT LOADINGS")
              )
            ),
            shiny::div(class = "pca-hero-credit", "Do\u011fan & Aybek (2022)")
          )
        )
      )
    }

    shiny::div()
  }

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
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(HTML(pca_theme_css))),
    tags$head(tags$style(id = "dynamic-theme-style", "")),

    uiOutput("cols"),
    
    ####################################################################################
    tags$head(tags$style(
      type="text/css",
      "#image0 img {max-width: 100%; width: auto; height: 100%; align: center}


    table,img, .tippy-content{ border-collapse: collapse;

  border-radius: 1em;

  overflow: hidden;}

  th, td {

  padding: 1em;

  background: #ddd;

  border-bottom: 2px solid white;

  border-top: 2px solid white;

  }

   #tepe{
  border-bottom: 3px solid black;
  }
    "
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
    
    tags$style(HTML("
/* --- PCA_THEME_2: selected tab and dropdown hover visibility fixes --- */

/* Selected/active tabs: lighter background, visible text */
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover,
.tabbable > .nav-tabs > li.active > a,
.tabbable > .nav-tabs > li.active > a:focus,
.tabbable > .nav-tabs > li.active > a:hover,
ul.nav.nav-tabs li.active a,
ul.nav.nav-tabs li.active a:focus,
ul.nav.nav-tabs li.active a:hover {
  background-color: color-mix(in srgb, var(--tc) 72%, #0f1f2e) !important;
  color: #ffffff !important;
  border-color: color-mix(in srgb, var(--tc) 82%, #0f1f2e) !important;
  box-shadow: inset 0 -2px 0 color-mix(in srgb, #ffffff 34%, transparent) !important;
}

.nav-tabs > li.active > a h4,
.nav-tabs > li.active > a:focus h4,
.nav-tabs > li.active > a:hover h4,
.tabbable > .nav-tabs > li.active > a h4,
.tabbable > .nav-tabs > li.active > a:focus h4,
.tabbable > .nav-tabs > li.active > a:hover h4,
ul.nav.nav-tabs li.active a h4,
ul.nav.nav-tabs li.active a:focus h4,
ul.nav.nav-tabs li.active a:hover h4 {
  color: #ffffff !important;
}

/* Hovered tabs */
.nav-tabs > li > a:hover,
.nav-tabs > li > a:focus,
ul.nav.nav-tabs li a:hover,
ul.nav.nav-tabs li a:focus {
  background-color: rgba(255, 255, 255, 0.55) !important;
  color: #071b4a !important;
}

/* Sidebar selected menu item */
.skin-blue .main-sidebar .sidebar .sidebar-menu .active a,
.skin-blue .main-sidebar .sidebar .sidebar-menu .active a:hover,
.main-sidebar .sidebar-menu li.active > a,
.sidebar-menu li.active > a,
.sidebar-menu li.active > a:hover {
  background-color: rgba(255, 255, 255, 0.22) !important;
  color: #ffffff !important;
  border-left-color: rgba(255, 255, 255, 0.78) !important;
}

/* Sidebar selectize/dropdown widgets: hover and selected option visibility */
.sidebar .selectize-dropdown .option:hover,
.sidebar .selectize-dropdown .option.active,
.sidebar .selectize-dropdown .selected,
.sidebar .selectize-dropdown-content .option:hover,
.sidebar .selectize-dropdown-content .option.active,
.sidebar .selectize-dropdown-content .selected,
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active,
.selectize-dropdown .selected {
  background-color: rgba(255, 255, 255, 0.72) !important;
  color: #071b4a !important;
}

/* Selectize input text visibility */
.sidebar .selectize-input,
.sidebar .selectize-input input,
.selectize-input,
.selectize-input input {
  color: #071b4a !important;
}

.sidebar .selectize-input.full,
.selectize-input.full {
  background-color: rgba(255, 255, 255, 0.86) !important;
}

/* Native selectInput hover/focus fallback */
.sidebar select,
.sidebar select:focus,
.sidebar select:hover {
  background-color: rgba(255, 255, 255, 0.86) !important;
  color: #071b4a !important;
}

.sidebar option,
.sidebar option:hover,
.sidebar option:checked {
  background-color: rgba(255, 255, 255, 0.88) !important;
  color: #071b4a !important;
}

#a{color:black; font-family:Lucida Arial ;font-size: 16px;
             font-style: oblique;text-align:center}")), #tabs#
    
    tags$style(HTML("#ab{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#
    
    tags$style(HTML("#b{color:black; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #
    # CHANGE: Modern styling from FA_10 applied while preserving FA_9 layout.
    tags$head(tags$style(HTML("
      .well {background: #ffffff; border: 1px solid #e9eef5; border-radius: 18px; box-shadow: 0 8px 24px rgba(31, 41, 55, 0.08);}
      .tabbable > .nav > li > a {border-radius: 12px; font-weight: 600; margin-right: 6px;}
      .nav-tabs {border-bottom: 0;}
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background: color-mix(in srgb, var(--tc) 72%, #0f1f2e);
        color: #ffffff;
        border: 1px solid color-mix(in srgb, var(--tc) 82%, #0f1f2e);
      }
      .btn, .bttn {border-radius: 12px !important; box-shadow: 0 6px 16px rgba(44, 123, 229, 0.16);}
      .dropdown-menu {border-radius: 16px; box-shadow: 0 12px 28px rgba(15, 23, 42, 0.14); padding: 12px;}
      .dropdown-menu > li > a,
      .dropdown-menu > li > a *,
      .bootstrap-select .dropdown-menu li a,
      .bootstrap-select .dropdown-menu li a *,
      .bootstrap-select .dropdown-menu li a span.text,
      .bootstrap-select .dropdown-menu .text,
      .bootstrap-select .dropdown-menu .glyphicon { color: #111111 !important; }
      .form-control, .bootstrap-select > .dropdown-toggle {border-radius: 12px !important; border-color: #dbe4f0;}
      .irs--shiny .irs-bar, .irs--shiny .irs-single {background: #2c7be5; border-top-color: #2c7be5; border-bottom-color: #2c7be5;}
      .gt_table, .dataTable {border-radius: 14px; overflow: hidden;}
    "))),
    
    # CHANGE: Data preview table in DATA UPLOAD tab made more practical for wide item sets.
    tags$head(tags$style(HTML("
      #dat1 table.dataTable th, #dat1 table.dataTable td {white-space: nowrap; padding: 6px 10px;}
      #dat1 .dataTables_scrollHeadInner, #dat1 table.dataTable {width: 100% !important;}
      #dat1 .dataTables_wrapper {width: 100%; overflow-x: auto;}
    "))),
    
    
    # CHANGE: Sidebar widget spacing tightened further across all tabs.
    tags$head(tags$style(HTML("
      .col-sm-4 .well {padding-top: 10px; padding-bottom: 10px;}
      .col-sm-4 .well .form-group {margin-bottom: 6px !important;}
      .col-sm-4 .well .dropdown {margin-bottom: 6px !important;}
      .col-sm-4 .well .btn, .col-sm-4 .well .bttn {margin-bottom: 4px !important;}
      .col-sm-4 .well .bootstrap-select,
      .col-sm-4 .well .bootstrap-select > .dropdown-toggle,
      .col-sm-4 .well .irs,
      .col-sm-4 .well .shiny-input-container {margin-bottom: 4px !important;}
      .col-sm-4 .well br {display:block; content:''; margin: 2px 0;}
      .col-sm-4 .well h3, .col-sm-4 .well h4 {margin-top: 4px; margin-bottom: 5px;}
      .col-sm-4 .well .radio-group-buttons,
      .col-sm-4 .well .btn-group-container-sw {margin-bottom: 4px !important;}
    "))),
    
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
      title = "Choose continuous, ordinal, or 1-0 (binary) according to your data.",
      placement = "top",
      trigger = "hover"
    ),
    bsTooltip(
      id = "tableFactor",
      title = "Items with a lower factor loading than the determined cutting score are shown in red",
      placement = "top",
      trigger = "hover"
    ),
    
    bsTooltip(
      id = "KMo",
      title = "You can examine the change in the KMO value when the items are removed or added.",
      placement = "bottom",
      trigger = "hover"
    ),
    bsTooltip(
      id = "antiImageBtn",
      title = "Click to view Anti-image values and a brief interpretation guide.",
      placement = "bottom",
      trigger = "hover"
    ),
    
    ## TITLE PANEL - SIDE BAR PANEL ##

    div(id = "tepe",
        fluidRow(
          column(6,
                 h1(id = "title", "PRINCIPAL COMPONENT ANALYSIS (PCA)")
          ),
          column(6,
                 h1(id = "title2", "RSP PACKAGE \u2014 CRAN")
          )
        )),
    
    
    br(),
    
    sidebarPanel(
      ## PANEL 1 - INTRODUCTION ##
      conditionalPanel(
        condition = "input.panel==0",
        pcaInfoCard("sidebar-mini"),
        tags$head(
          tags$script(HTML(js))
        ),
        br(),
        textOutput("browser"),
        tags$head(
          tags$style(
            "#browser{color:var(--tdk);font-size:16px;font-family:cursive;font-style:oblique;text-align:center;}"
          )
        ),
        br(),
        br(),
        shinyWidgets::spectrumInput(   # RENK PALET WDGET
          inputId = "myColor",
          label = "CHANGE THE COLOR OF THE THEME:",
          choices = list(
            list('gray', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
            as.list(scales::brewer_pal(palette = "Blues")(9)),
            as.list(scales::brewer_pal(palette = "Greens")(9)),
            as.list(scales::brewer_pal(palette = "Spectral")(11)),
            as.list(scales::brewer_pal(palette = "Dark2")(8))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        ),
      ),
      
      ## PANEL 2 - DATA UPLOAD ##
      
      conditionalPanel(
        condition = "input.panel==1",
        pcaInfoCard("sidebar-mini"),
        br(),
        
        shinyWidgets::radioGroupButtons(
          inputId = "type",
          label =  h3(id="ab","Select Data Type"),
          # CHANGE: Data type options revised as continuous / ordinal / binary
          choices = c("Continuous"=1,
                      "Ordinal"=2,
                      "1-0 (Binary)"=3),
          
          justified = TRUE,
          checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon")),
          
          # status = "primary"
        ),
        
        # prettyRadioButtons(
        #   inputId = "type",
        #   label = h3(id="ab","Select Data Type"),
        #   choices = c("Polytomous (Likert etc..)"=1,
        #               "1-0"=2),
        #
        #   shape = "curve",animation = "rotate" , inline = FALSE,
        #   bigger = TRUE, status = "primary", outline = TRUE,
        #   fill = FALSE, width = "500px"
        # ),
        
        
        shinyWidgets::pickerInput(
          inputId = "type2",
          label = h3(id="ab","Select File Format"),
          choices = list(
            "CSV - Semicolon  Separated  Excel" = 1,
            "CSV - Comma  Separated  Excel" = 2,
            "SAV - SPSS" = 3,
            "XLSX - Excel"=4
          ),
          selected = 3,
          options = shinyWidgets::pickerOptions(showTick = TRUE)
        ),
        
        uiOutput("uiHeader"),
        
        div(  style="color:red;",
              
              HTML( "<marquee direction='left' scrollamount = '5'>
                      THE DATASET SHOULD CONTAIN ONLY THE VARIABLES TO BE INCLUDED IN THE ANALYSIS!!!

               </marquee>"  )),
        
        fileInput(
          "data1",
          h3(id="ab","Uplad Data File",icon("paper-plane"))
        ),
        
        br(),
        
        # CHANGE: KMO/Bartlett moved back to DATA UPLOAD panel
        shinyWidgets::dropMenu(
          padding = "20px",
          theme="light-border",
          placement = "right-end",
          shinyWidgets::actionBttn(
            inputId = "acb2",
            label = "CLICK TO SEE KMO AND BARTLETT TEST RESULTS",
            style = "jelly",
            color = "primary"
          ),
          gt::gt_output("dat3")
        ),
        
        br(),
        br(),
        
        # CHANGE: Item-level MSA / Anti-image moved under KMO in DATA UPLOAD panel
        shinyWidgets::dropMenu(
          padding = "20px",
          theme = "light-border",
          placement = "right-end",
          shinyWidgets::actionBttn(
            inputId = "antiImageBtn",
            label = "CLICK TO SEE ITEM-LEVEL MSA (ANTI-IMAGE)",
            style = "jelly",
            color = "primary"
          ),
          gt::gt_output("antiImageGuide"),
          br(),
          gt::gt_output("antiImageTable")
        ),
        
        br(),
        br(),
        
        # CHANGE: Basic statistics action button removed; tables are shown directly in the main panel under the data preview.
        
      ),
      
      ## PANEL 3 - NUMBER OF FACTORS ##
      conditionalPanel(
        condition = "input.panel==2",
        pcaInfoCard("sidebar-mini"),
        br(),
        
        shinyWidgets::chooseSliderSkin("Big", color = "#112446"),
        
        uiOutput("factornumber"),
        br(),
        br(),
        
        # CHANGE: Component-correlation preview is always computed with default oblimin
        shinyWidgets::dropMenu(
          
          padding = "20px",
          
          theme="light-border",
          
          placement = "right-end",
          
          shinyWidgets::actionBttn(
            inputId = "korfak",
            label = "CLICK TO SEE COMPONENT CORRELATIONS (OBLIMIN / STRUCTURE CHECK)",
            style = "jelly",
            color = "primary"
            
          ),
          
          gt::gt_output("fakor")
          
        ),
        
        br(),
        br(),
        
        # CHANGE: Final rotation is selected after inspecting Phi
        shinyWidgets::pickerInput(
          inputId = "rotation",
          label = h3(id="ab","Select Final Rotation Method"),
          choices = list(
            "Varimax" = "varimax",
            "Direct Oblimin" = "oblimin",
            "No Rotation" = "none"
          ),
          selected = "oblimin",
          options = shinyWidgets::pickerOptions(showTick = TRUE)
        ),
        
        br(),
        br(),
      ),
      
      ## PANEL 4 - COMPONENT LOADINGS ##
      conditionalPanel(
        condition = "input.panel==3",
        pcaInfoCard("sidebar-mini"),
        
        #  br(),
        
        gt::gt_output("KMo"),
        br(),
        
        uiOutput("select_item"),
        br(),
        fluidRow(
          column(6, uiOutput("remove_item")),
          column(6)
        ),
        
        # ##################### NEW KMO 1 #################
        
        br(),
        
        
        sliderInput(
          "cut_off",
          h3(id="ab","Select cut-off value for Factor Loadings"),
          min = 0.25,
          max = 0.60,
          step = 0.05,
          value = 0.30
        ),
        
        
        br(),
        
        fluidRow(
          
          column(6,
                 
                 shinyWidgets::dropMenu(
                   
                   padding = "20px",
                   
                   theme="light-border",
                   
                   placement = "right-end",
                   
                   
                   shinyWidgets::actionBttn(
                     inputId = "coms",
                     label = "CLICK TO SEE COMMUNALITIES",
                     style = "jelly",
                     color = "primary"
                     
                   ),
                   
                   
                   DT::DTOutput("commons"),
                   
                 ),
                 
                 
          ), # close column
          column(6,
                 shinyWidgets::dropMenu(
                   
                   padding = "20px",
                   
                   theme="light-border",
                   
                   placement = "right-end",
                   
                   shinyWidgets::actionBttn(
                     inputId = "acb1",
                     label = "CLICK TO  SEE DOWNLOADS",
                     style = "jelly",
                     color = "primary"
                     
                   ),
                   
                   
                   shinyWidgets::downloadBttn(
                     "factorDownload",
                     label = h1(id="b", "COMPONENT LOADINGS"),
                     style = "unite",
                     color = "primary",
                     size = "sm",
                     block = FALSE,
                     no_outline = TRUE,
                     icon = shiny::icon("download")
                   ),
                   
                   
                   br(),
                   br(),
                   
                   
                   shinyWidgets::downloadBttn(
                     "varianceDownload",
                     label = h1(id="b", " EXPLAINED VARIANCE"),
                     style = "unite",
                     color = "primary",
                     size = "sm",
                     block = FALSE,
                     no_outline = TRUE,
                     icon = shiny::icon("download")
                   )
                   
                 ) # close drop menu
                 
                 
          ) # close column
          
        ) # close fluidrow
        
        
        
      ), # close conditional panel
      
      # close conditional panels within sidebar
    ), # sidebar panel
    
    ##  MAIN PANEL ##
    
    mainPanel(
      tabsetPanel(
        id = "panel",
        
        ## MAIN PANEL  1 ##
        tabPanel(
          h4(id="a", "INTRODUCTION"),
          value = 0,
          br(),
          fluidRow(
            column(12, pcaInfoCard("hero"))
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
          shinycustomloader::withLoader(
            DT::DTOutput("dat1"),
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
          
          # CHANGE: Basic statistics are displayed directly under the data preview table.
          fluidRow(
            column(12, gt::gt_output("dat2"))
          ),
          br(),
          fluidRow(
            column(12, gt::gt_output("dat4"))
          ),
          
          
        ),
        
        ## MAIN PANEL 3 ##
        tabPanel(
          # h4("NUMBER OF FACTORS"),
          
          h4(id="a", "NUMBER OF COMPONENTS"),
          
          value = 2,
          textOutput("text2"),
          tags$head(
            tags$style(
              "#text2{
            color: #153a7a;
            font-size: 30px;
            font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
            font-style: normal;
            font-weight: 800;
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          br(),
          br(),
          shinycustomloader::withLoader(plotOutput("scree_plot"), type = "html", loader = "loader1"),
          
          br(),
          # CHANGE: PA table wrapped in scrollable div - first 10 rows visible without scrolling
          div(style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            gt::gt_output("eigen_value")
          ),
          br()
        ),
        
        ## MAIN PANEL 4 ##
        tabPanel(
          
          # h4( "COMPONENT LOADINGS-EXPLAINED VARIANCE"),
          
          h4 (id="a", "LOADINGS & EXPLAINED VARIANCE"),
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
          # CHANGE: Component Loadings wrapped in scrollable div - first 10 rows visible without scrolling
          div(style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            gt::gt_output("tableFactor")
          ),
          div(style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            gt::gt_output("buton")
          ),
          br(),
          # CHANGE: Eigenvalue/Variance table wrapped in scrollable div - first 10 rows visible without scrolling
          div(style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            gt::gt_output("tableEigen")
          ),
          div(style = "max-height: 420px; overflow-y: auto; overflow-x: auto;",
            gt::gt_output("buton2")
          ),
          br(),
          
          
        ),
        
      ) # close tabsetpanel
    ) #  close mainpanel
  ) #  close fluidpage
  
  
  ## SERVER ##
  
  server <- function(input, output, session) {
    
    
    shinyjs::addCssClass(class = "bttn bttn-unite bttn-default bttn-no-outline",
                         selector = ".btn-file")
    
    observeEvent(input$myColor,{
      col <- input$myColor

      css <- sprintf(
        ":root{--tc:%s;--tdk:#0f1f2e;--tlt:color-mix(in srgb,%s 62%%,#ffffff);--tpl:color-mix(in srgb,%s 40%%,#ffffff);--tls:color-mix(in srgb,%s 14%%,#ffffff);}
         body{background:linear-gradient(to bottom right,#ffffff,color-mix(in srgb,%s 10%%,#ffffff)) !important;}
         .nav-tabs > li > a, .nav-tabs > li > a h4 {color:#0f1f2e !important;}
         .nav-tabs > li.active > a,
         .nav-tabs > li.active > a:focus,
         .nav-tabs > li.active > a:hover {
           background:color-mix(in srgb,var(--tc) 72%%,#0f1f2e) !important;
           color:#ffffff !important;
           border-color:color-mix(in srgb,var(--tc) 82%%,#0f1f2e) !important;
           box-shadow:inset 0 -2px 0 color-mix(in srgb,#ffffff 34%%,transparent) !important;
         }
         .nav-tabs > li.active > a h4,
         .nav-tabs > li.active > a:focus h4,
         .nav-tabs > li.active > a:hover h4 {color:#ffffff !important;}
         .nav-tabs > li > a:hover {background:var(--tls) !important;color:#0f1f2e !important;}
         .well {background:var(--tls) !important;border-color:var(--tpl) !important;}
         .bttn-gradient,.bttn-gradient.bttn-primary {background:var(--tc) !important;border-color:#0f1f2e !important;}
         .bttn-jelly.bttn-primary,.bttn-unite.bttn-primary {background:var(--tc) !important;}
         table.dataTable thead th {background:var(--tc) !important;}
         .gt_col_heading {background:var(--tc) !important;}
         #tepe {border-bottom-color:var(--tc) !important;}
         #title {color:#0f1f2e !important;}
         #title2 {color:var(--tlt) !important;}
         h3,h4 {color:#0f1f2e !important;}
         .irs--shiny .irs-bar,.irs--shiny .irs-single {background:var(--tc) !important;border-top-color:var(--tc) !important;border-bottom-color:var(--tc) !important;}",
        col, col, col, col, col
      )
      shinyjs::runjs(sprintf(
        "document.getElementById('dynamic-theme-style').innerHTML=`%s`;",
        gsub("`", "'", css)
      ))
    })
    
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
    
    
    # CHANGE: Helper functions added to make correlation, parallel analysis,
    # CHANGE: and FA computations consistent with continuous / ordinal / binary data types.
    # CHANGE: Data are coerced to numeric and zero-variance items are removed defensively.
    sanitize_data <- function(dat) {
      dat <- as.data.frame(dat)
      dat[] <- lapply(dat, function(x) suppressWarnings(as.numeric(as.character(x))))
      dat <- dat[, vapply(dat, function(x) !all(is.na(x)), logical(1)), drop = FALSE]
      dat <- dat[, vapply(dat, function(x) stats::sd(x, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
      dat
    }
    
    # CHANGE: Correlation matrix helper now smooths non-positive-definite matrices when needed.
    compute_cor_matrix <- function(dat, data_type, smooth = TRUE) {
      dat <- sanitize_data(dat)
      dat <- na.omit(dat)
      
      if (ncol(dat) < 2) {
        stop("At least two non-constant items are required for analysis.")
      }
      
      cor_mat <- if (data_type == 1) {
        stats::cor(dat, use = "pairwise.complete.obs")
      } else if (data_type == 2) {
        psych::polychoric(dat)$rho
      } else {
        psych::tetrachoric(dat)$rho
      }
      
      if (smooth) {
        eigvals <- tryCatch(eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values,
                            error = function(e) NULL)
        if (is.null(eigvals) || any(!is.finite(eigvals)) || any(eigvals <= 1e-08)) {
          cor_mat <- psych::cor.smooth(cor_mat)
        }
      }
      
      cor_mat
    }
    
    # CHANGE: Safe KMO helper added for binary / ordinal matrices that may be near-singular.
    compute_kmo_value <- function(dat, data_type) {
      cor_mat <- compute_cor_matrix(dat, data_type, smooth = TRUE)
      out <- tryCatch(
        psych::KMO(cor_mat)$MSA,
        error = function(e) {
          cor_mat2 <- psych::cor.smooth(cor_mat)
          psych::KMO(cor_mat2)$MSA
        }
      )
      as.numeric(out)
    }
    
    # CHANGE: Anti-image / item-level MSA helper added.
    # CHANGE: This returns the anti-image correlation matrix diagonal (MSA by item)
    # CHANGE: and the full anti-image correlation matrix for optional future use.
    compute_anti_image <- function(dat, data_type) {
      cor_mat <- compute_cor_matrix(dat, data_type, smooth = TRUE)
      kmo_obj <- tryCatch(
        psych::KMO(cor_mat),
        error = function(e) psych::KMO(psych::cor.smooth(cor_mat))
      )
      
      list(
        item_msa = as.numeric(kmo_obj$MSAi),
        anti_image = kmo_obj$Image,
        overall_msa = as.numeric(kmo_obj$MSA)
      )
    }
    
    # CHANGE: Horn's PA is retained for continuous data via hornpa.
    # CHANGE: For ordinal/binary data, PA is made data-type-sensitive via fa.parallel.
    compute_parallel <- function(dat, data_type) {
      dat <- na.omit(dat)
      set.seed(123)
      
      observed_values <- eigen(compute_cor_matrix(dat, data_type), symmetric = TRUE, only.values = TRUE)$values
      
      if (data_type == 1) {
        horn <- hornpa::hornpa(
          k = ncol(dat),
          size = nrow(dat),
          reps = 200
        )
        pa_mean <- horn$Mean
      } else if (data_type == 2) {
        pa_obj <- psych::fa.parallel(
          dat,
          fa = "fa",
          cor = "poly",
          fm = "minres",
          n.iter = 200,
          plot = FALSE
        )
        pa_mean <- pa_obj$fa.sim
      } else {
        pa_obj <- psych::fa.parallel(
          dat,
          fa = "fa",
          cor = "tet",
          fm = "minres",
          n.iter = 200,
          plot = FALSE
        )
        pa_mean <- pa_obj$fa.sim
      }
      
      list(
        observed = observed_values,
        pa_mean = as.numeric(pa_mean)
      )
    }
    
    
    # CHANGE: Single PCA helper used everywhere in the app
    # CHANGE: PCA estimation made robust for high component counts / unstable correlation matrices.
    compute_pca_model <- function(dat, nfactors, rotation, data_type) {
      dat <- sanitize_data(dat)
      dat <- na.omit(dat)
      cor_matrix <- compute_cor_matrix(dat, data_type, smooth = TRUE)

      eigvals <- tryCatch(eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NULL)
      usable_rank <- if (is.null(eigvals)) ncol(cor_matrix) else sum(is.finite(eigvals) & eigvals > 1e-08)
      usable_rank <- max(1, usable_rank)
      nfactors_safe <- max(1, min(nfactors, ncol(cor_matrix), usable_rank))

      safe_rotation <- if (is.null(rotation) || rotation == "none") "none" else rotation

      fit_try <- function(mat, nf, rot) {
        psych::principal(
          r = mat,
          nfactors = nf,
          rotate = rot,
          scores = FALSE
        )
      }

      result <- tryCatch(
        fit_try(cor_matrix, nfactors_safe, safe_rotation),
        error = function(e1) {
          tryCatch(
            fit_try(cor_matrix, nfactors_safe, "none"),
            error = function(e2) {
              cor_matrix2 <- psych::cor.smooth(cor_matrix)
              fit_try(cor_matrix2, nfactors_safe, "none")
            }
          )
        }
      )

      result
    }

    # CHANGE: Helper to style low loadings dynamically for any number of components.
    apply_loading_cutoff_style <- function(gt_tbl, df, prefix, cutoff) {
      comp_cols <- names(df)[grepl(paste0("^", prefix, "\\d+$"), names(df))]
      if (length(comp_cols) == 0) return(gt_tbl)

      for (col_name in comp_cols) {
        low_rows <- which(is.na(df[[col_name]]) | abs(df[[col_name]]) < abs(cutoff))
        if (length(low_rows) > 0) {
          gt_tbl <- gt_tbl %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline"
              ),
              locations = gt::cells_body(
                columns = tidyselect::all_of(col_name),
                rows = low_rows
              )
            )
        }
      }
      gt_tbl
    }

    current_settings <- reactive({
      list(
        type = if (is.null(input$type)) 1 else as.numeric(input$type),
        fak2 = if (is.null(input$fak2)) 1 else input$fak2,
        rotation = if (is.null(input$rotation)) "oblimin" else input$rotation,
        cut_off = if (is.null(input$cut_off)) 0.30 else input$cut_off,
        grafmad = input$grafmad
      )
    })
    
    ##  DATA UPLOAD ##
    output$uiHeader <- renderUI({
      if (input$type2 == 3) {
        NULL
      } else {
        
        shinyWidgets::materialSwitch(
          inputId = "header",
          label =   h4("The first line is the variable name"),
          value = TRUE,
          status = "primary"
        )
        # switchInput(inputId = "header",
        #             label =  " Is the first line include the variable name?",
        #             onLabel = "YES",
        #             offLabel = "NO",
        #             value = TRUE,
        #             inline = TRUE)
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
          foreign::read.spss(veri$datapath,
                             to.data.frame = TRUE,
                             use.value.labels = FALSE
          ) }
      } else if(input$type2==4) {
        if (tools::file_ext(veri$datapath) != "xlsx") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          xlsx::read.xlsx(veri$datapath, 1, header = isTRUE(input$header)
          )
          
        } }
      
    })
    
    ## WIDGETS FOR UIOUTPUTS ##
    
    output$factornumber <- renderUI({
      req(input$data1)
      sliderInput(
        "fak2",
        h3(id="ab","Define the Number of Components"),
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
      shinyWidgets::actionBttn(
        inputId = "remove",
        label = "REMOVE",
        style = "jelly",
        size = "sm",
        color = "primary", no_outline = TRUE
      )
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
    
    output$text2 <- renderText({ NULL })
    
    output$text2_1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("LOADINGS & EXPLAINED VARIANCE")
      }
    })
    
    output$dat1 <- DT::renderDT({
      if (!is.null(input$data1)) {
        data_preview <- na.omit(data())
        colnames(data_preview) <- paste0("item", 1:ncol(data_preview))
        
        if (dim(data_preview)[2] == 1) {
          return(DT::datatable(data.frame(WARNING = "PLEASE SELECT THE CORRECT FILE FORMAT"),
                               options = list(dom = "t"),
                               rownames = FALSE))
        }
        
        # CHANGE: Show all item columns with a wide-table friendly DT configuration
        # CHANGE: instead of truncating at 15 columns.
        preview_rows <- min(10, nrow(data_preview))
        data_preview <- data_preview[1:preview_rows, , drop = FALSE]
        
        DT::datatable(
          data_preview,
          extensions = c("FixedColumns"),
          rownames = FALSE,
          class = "compact stripe hover order-column",
          options = list(
            scrollX = TRUE,
            autoWidth = TRUE,
            pageLength = preview_rows,
            lengthChange = FALSE,
            searching = FALSE,
            info = FALSE,
            paging = FALSE,
            fixedColumns = list(leftColumns = 1),
            columnDefs = list(list(width = '90px', targets = "_all"))
          )
        )
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
          N_Items = ncol(data),
          N_Respondents = nrow(data),
          N_Missing = length(which(is.na(data)))
        )
        res <- gt::gt(res)
        br()
        br()
        br()
        res <- res %>%
          tab_header(title = md("*BASIC STATISTICS ABOUT THE DATA SET*"),
                     subtitle = md("Dataset size and missing-value summary"))
        
        res <- res %>%
          gt::tab_style(
            style = cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "#1e293b", weight = "bold", size = gt::px(18)),
            locations = gt::cells_body()
          )
        
        res <- res %>%
          gt::cols_width(
            c(N_Items) ~ gt::px(200),
            c(N_Respondents) ~ gt::px(200),
            c(N_Missing) ~ gt::px(200)
          )
        
        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(14),
            table.font.size = gt::px(16),
            heading.title.font.size = gt::px(24),
            heading.subtitle.font.size = gt::px(14),
            heading.background.color = "#f1f5f9",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
      }
    })
    
    ## BARTLET - KMO ##
    
    output$dat3 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        
        # CHANGE: KMO and Bartlett are now computed from the same data-type-specific correlation matrix
        cor_matrix <- compute_cor_matrix(data, current_settings()$type)
        BR <- psych::cortest.bartlett(cor_matrix, n = nrow(data))
        Bartlett_Chi_Square <- BR$chisq
        p_value <- formatC(BR$p.value, format = "f", digits = 3)
        df <- BR$df
        kmo <- compute_kmo_value(data, current_settings()$type)
        
        kmo_warning <- ifelse(kmo < 0.60,
                              "WARNING: KMO is low. Please inspect item-level MSA (anti-image) values before continuing.",
                              "KMO is acceptable. You may still inspect item-level MSA (anti-image) values if needed.")
        
        bartlett_comment <- ifelse(as.numeric(p_value) < 0.05,
                                   "Bartlett's test is significant (p < .05): the correlation matrix is suitable for factor analysis.",
                                   "Bartlett's test is NOT significant (p  .05): the correlation matrix may not be suitable for factor analysis.")
        
        res <- data.frame(KMO = round(kmo, 3), Bartlett_Chi_Square = round(Bartlett_Chi_Square, 3), p_value, df,
                          KMO_Interpretation = kmo_warning,
                          Bartlett_Interpretation = bartlett_comment)
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
            style = cell_fill(color = "#f0fdf4"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#fef2f2"),
            locations = gt::cells_body(rows = KMO < 0.50)
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#fff7ed"),
            locations = gt::cells_body(rows = KMO >= 0.50 & KMO < 0.60)
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          )
        
        res <- res %>%
          gt::cols_width(
            c(Bartlett_Chi_Square) ~ gt::px(160),
            c(p_value) ~ gt::px(100),
            c(df) ~ gt::px(80),
            c(KMO) ~ gt::px(90),
            c(KMO_Interpretation) ~ gt::px(300),
            c(Bartlett_Interpretation) ~ gt::px(340)
          )
        
        res <- res %>%
          gt::tab_style(
            style = list(
              gt::cell_text(weight = "bold", color = "#b91c1c"),
              cell_fill(color = "#fef2f2")
            ),
            locations = gt::cells_body(columns = c(KMO_Interpretation), rows = KMO < 0.50)
          )
        
        res <- res %>%
          gt::tab_style(
            style = list(
              gt::cell_text(weight = "bold", color = "#c2410c"),
              cell_fill(color = "#fff7ed")
            ),
            locations = gt::cells_body(columns = c(KMO_Interpretation), rows = KMO >= 0.50 & KMO < 0.60)
          )
        
        res <- res %>%
          gt::tab_style(
            style = list(
              gt::cell_text(weight = "bold", color = "#15803d"),
              cell_fill(color = "#dcfce7")
            ),
            locations = gt::cells_body(columns = c(KMO_Interpretation), rows = KMO >= 0.60)
          )
        
        res <- res %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold", color = "#15803d", size = gt::px(14)),
            locations = gt::cells_body(columns = c(KMO), rows = KMO >= 0.60)
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold", color = "#c2410c", size = gt::px(14)),
            locations = gt::cells_body(columns = c(KMO), rows = KMO >= 0.50 & KMO < 0.60)
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold", color = "#b91c1c", size = gt::px(14)),
            locations = gt::cells_body(columns = c(KMO), rows = KMO < 0.50)
          ) %>%
          gt::tab_style(
            style = list(gt::cell_text(weight = "bold", color = "#15803d"), cell_fill(color = "#dcfce7")),
            locations = gt::cells_body(columns = c(Bartlett_Interpretation), rows = as.numeric(p_value) < 0.05)
          ) %>%
          gt::tab_style(
            style = list(gt::cell_text(weight = "bold", color = "#b91c1c"), cell_fill(color = "#fef2f2")),
            locations = gt::cells_body(columns = c(Bartlett_Interpretation), rows = as.numeric(p_value) >= 0.05)
          )
        
        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(12),
            table.font.size = gt::px(15),
            heading.title.font.size = gt::px(22),
            heading.background.color = "#f0fdf4",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
      }
    })
    
    ## DETERMINANT ##
    
    output$dat4 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        
        # CHANGE: determinant now uses the selected data-type-specific correlation matrix
        cor_matrix1 <- compute_cor_matrix(data, current_settings()$type)
        
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
          tab_header(title = md("*ADDITIONAL DATA INDICATORS*"),
                     subtitle = md("Determinant and total-score shape indices"))
        
        res <- res %>%
          gt::tab_style(
            style = cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "#1e293b", weight = "bold", size = gt::px(17)),
            locations = gt::cells_body()
          )
        
        res <- res %>%
          gt::cols_width(
            c(DETERMINANT) ~ gt::px(220),
            c(SKEWNESS) ~ gt::px(220),
            c(KURTOSIS) ~ gt::px(220)
          )
        
        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(14),
            table.font.size = gt::px(16),
            heading.title.font.size = gt::px(24),
            heading.subtitle.font.size = gt::px(14),
            heading.background.color = "#f1f5f9",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
      }
    })
    
    ## NUMBER OF FACTORS ##
    
    # scree Plot #
    
    output$scree_plot <- renderPlot({
      
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        
        # CHANGE: Parallel analysis is now data-type-sensitive
        pa_res <- compute_parallel(data, current_settings()$type)
        
        obs  <- pa_res$observed
        pa   <- pa_res$pa_mean
        nf   <- length(obs)
        xval <- seq_len(nf)
        
        # Ka faktr PA izgisini geiyor?
        n_sig <- sum(obs > pa)
        
        #  Renk paleti 
        col_obs    <- "#16a34a"   # yeil  - gzlenen eigenvalue
        col_pa     <- "#dc2626"   # krmz - PA mean
        col_sig    <- "#15803d"   # koyu yeil - PA st noktalar
        col_insig  <- "#b91c1c"   # koyu krmz - PA alt noktalar
        col_grid   <- "#e2e8f0"   # ak gri grid
        col_shade  <- "#dcfce7"   # yeil erit - anlaml alan
        col_bg     <- "#f8fafc"   # arka plan
        
        #  Grafik alan 
        par(
          bg        = col_bg,
          mar       = c(5, 5.5, 4.5, 2),
          family    = "sans",
          las       = 1
        )
        
        ymax <- max(obs, pa) * 1.12
        ymin <- 0
        
        # Bo grafik erevesi
        plot(
          xval, obs,
          type = "n",
          xlim = c(0.5, nf + 0.5),
          ylim = c(ymin, ymax),
          xlab = "",
          ylab = "",
          axes = FALSE,
          main = ""
        )
        
        # Arka plan rengi
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
             col = col_bg, border = NA)
        
        # PA altndaki blge (krmzms erit) - anlaml olmayan alan
        if (n_sig < nf) {
          rect(n_sig + 0.5, ymin, nf + 0.5, ymax,
               col = "#fef2f230", border = NA)
        }
        
        # PA stndeki blge (yeil erit) - anlaml alan
        if (n_sig > 0) {
          rect(0.5, ymin, n_sig + 0.5, ymax,
               col = "#dcfce740", border = NA)
        }
        
        # Dikey kesim izgisi
        if (n_sig > 0 && n_sig < nf) {
          abline(v = n_sig + 0.5, col = "#94a3b8", lty = 2, lwd = 1.2)
        }
        
        # Grid yatay izgileri
        grid_vals <- pretty(c(ymin, ymax), n = 6)
        abline(h = grid_vals, col = col_grid, lty = 1, lwd = 0.8)
        
        # Eksenler
        axis(1, at = xval, labels = xval,
             col = "#94a3b8", col.axis = "#475569",
             cex.axis = 1.1, tick = FALSE, line = -0.5)
        axis(2, at = grid_vals,
             col = "#94a3b8", col.axis = "#475569",
             cex.axis = 1.0, tick = FALSE, line = -0.5)
        
        # Eksen kutusu (hafif)
        box(col = "#cbd5e1", lwd = 0.8)
        
        #  PA mean izgisi 
        lines(xval, pa,
              col  = col_pa,
              lty  = 5,
              lwd  = 2.2)
        points(xval, pa,
               pch = 18,   # dolu elmas
               col = col_pa,
               cex = 1.6)
        
        #  Gzlenen eigenvalue izgisi 
        lines(xval, obs,
              col  = col_obs,
              lty  = 1,
              lwd  = 2.8)
        
        # Noktalar anlaml / anlamsz olarak farkl renkle iz
        pt_cols <- ifelse(obs > pa, col_sig, col_insig)
        points(xval, obs,
               pch = 21,   # dolduralabilir ember
               bg  = pt_cols,
               col = "white",
               cex = 2.2,
               lwd = 1.5)
        
        # Eigenvalue deer etiketleri (sadece ilk 10 faktr iin)
        show_n <- min(nf, 10)
        text(xval[1:show_n],
             obs[1:show_n] + ymax * 0.04,
             labels = round(obs[1:show_n], 2),
             col    = pt_cols[1:show_n],
             cex    = 0.85,
             font   = 2)
        
        #  Balk ve eksen etiketleri 
        mtext("SCREE PLOT  &  PARALLEL ANALYSIS",
              side = 3, line = 2.5,
              cex  = 1.4, font = 2, col = "#1e293b")
        
        mtext(paste0("( ", n_sig, " component",
                     ifelse(n_sig != 1, "s", ""),
                     " above PA line )"),
              side = 3, line = 1.0,
              cex  = 1.0, font = 3, col = "#64748b")
        
        mtext("Component Number", side = 1, line = 3.2,
              cex = 1.1, font = 2, col = "#334155")
        
        mtext("Eigenvalue", side = 2, line = 4.0,
              cex = 1.1, font = 2, col = "#334155", las = 0)
        
        #  Legend 
        legend(
          "topright",
          legend  = c("Observed Eigenvalue", "PA Mean (Random Data)"),
          col     = c(col_obs, col_pa),
          lty     = c(1, 5),
          lwd     = c(2.8, 2.2),
          pch     = c(21, 18),
          pt.bg   = c(col_sig, col_pa),
          pt.cex  = c(1.6, 1.4),
          bty     = "o",
          box.col = "#cbd5e1",
          bg      = "#ffffffcc",
          text.col = "#1e293b",
          cex     = 1.05,
          inset   = c(0.01, 0.02)
        )
      }
    })
    
    ## Eigen Value ##
    
    output$eigen_value <- render_gt({
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        
        # CHANGE: Parallel analysis and eigenvalues now follow selected data type
        pa_res <- compute_parallel(data, current_settings()$type)
        eigenvalue <- round(pa_res$observed, 4)
        PA_MEAN <- round(pa_res$pa_mean, 4)
        FACTOR <- 1:ncol(data)
        res <- data.frame(FACTOR, PA_MEAN, EIGENVALUE = eigenvalue)
        
        res <- gt::gt(res)
        res <- res %>%
          tab_header(title = md("**EIGENVALUES AND PARALLEL ANALYSIS**"))
        
        res <- res %>%
          tab_options(
            heading.title.font.size = gt::px(25),
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(10),
            heading.background.color = "#f1f5f9",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
        
        res <- res %>%
          gt::tab_style(
            style = cell_fill(color = "#fef2f2"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          )
        
        res <- res %>%
          gt::cols_width(everything() ~ gt::px(180))
        
        res <- res %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#dcfce7"),
              gt::cell_text(weight = "bold", color = "#15803d", size = gt::px(16))
            ),
            locations = gt::cells_body(
              columns = c(EIGENVALUE),
              rows = EIGENVALUE > PA_MEAN
            )
          ) %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#fef2f2"),
              gt::cell_text(weight = "bold", color = "#b91c1c", size = gt::px(16))
            ),
            locations = gt::cells_body(
              columns = c(EIGENVALUE),
              rows = EIGENVALUE <= PA_MEAN
            )
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "#1e293b", weight = "bold"),
            locations = gt::cells_body(columns = c(FACTOR))
          )
        return(res)
      }
    })
    
    ## Correlation Among Factors ##
    
    output$fakor <- render_gt({
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        
        # CHANGE: Component correlations are previewed with oblimin because Phi is a rotation-based output;
        # CHANGE: extraction method may alter it only slightly in many datasets
        if (current_settings()$fak2 <= 1) {
          fac1 <- data.frame(FACTORS = "Factor 1", `Factor 1` = NA)
          fac1 <- gt::gt(fac1)
          fac1 <- fac1 %>% tab_header(title = md("**CORRELATION AMONG COMPONENTS (PHI / OBLIMIN)**"))
          fac1 <- fac1 %>% gt::cols_width(everything() ~ gt::px(220))
          return(fac1)
        }
        
        model1 <- compute_pca_model(
          dat = data,
          nfactors = current_settings()$fak2,
          rotation = "oblimin",
          data_type = current_settings()$type
        )
        
        # CHANGE: Phi is used instead of r.scores
        fac <- round(model1$Phi, 3)
        Row <- paste("Component", 1:current_settings()$fak2)
        
        colnames(fac) <- paste("Component", 1:current_settings()$fak2)
        
        fac1 <- data.frame(FACTORS = Row, fac, check.names = FALSE)
        
        fac1 <- gt::gt(fac1)
        
        fac1 <- fac1 %>% tab_header(title = md("**CORRELATION AMONG COMPONENTS (PHI / OBLIMIN)**"))
        
        fac1 <- fac1 %>%
          gt::cols_width(everything() ~ gt::px(180))
        
        fac1 <- fac1 %>%
          gt::tab_style(
            style = cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          )
        
        comp_cols <- paste("Component", 1:current_settings()$fak2)
        fac1 <- fac1 %>%
          gt::data_color(
            columns = tidyselect::any_of(comp_cols),
            method = "numeric",
            palette = c("#f0fdf4", "#fdba74", "#dc2626"),
            na_color = "#f8fafc"
          )
        
        fac1 <- fac1 %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(10),
            heading.background.color = "#f1f5f9",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
        
        return(fac1)
      }
    })
    
    
    ## COMPONENT LOADINGS ##
    
    output$tableFactor <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        
        data <- na.omit(data())
        
        
        
        # CHANGE: Final component loadings now follow the selected data type
        result <- compute_pca_model(
          dat = data,
          nfactors = current_settings()$fak2,
          rotation = current_settings()$rotation,
          data_type = current_settings()$type
        )
        
        common <- result$communality
        PCA_ENV$common <- common
        
        load_mat <- as.matrix(unclass(result$loadings))
        if (is.null(dim(load_mat))) {
          load_mat <- matrix(load_mat, ncol = 1)
        }
        load_mat <- load_mat[, seq_len(current_settings()$fak2), drop = FALSE]
        
        nfac <- current_settings()$fak2
        namefac <- paste0("component", seq_len(nfac))
        colnames(load_mat) <- namefac
        item <- seq_len(nrow(load_mat))
        load_mat <- round(load_mat, 2)
        
        fload <- data.frame(item = item, load_mat, check.names = FALSE)
        
        PCA_ENV$factorLoading <- fload
        
        fload <- gt::gt(fload)
        
        
        
        fload <- fload %>% tab_header(title = md("**COMPONENT LOADINGS**"))
        
        ## ARRANGE COLUMNS ACCORDIG TO NUMBER OF FACTORS ##
        
        
        if (current_settings()$fak2 > 3) {
          fload <- fload %>%
            gt::cols_width(everything() ~ gt::px(120))
        }
        
        if (current_settings()$fak2 <= 3) {
          fload <- fload %>%
            gt::cols_width(everything() ~ gt::px(180))
        }
        
        ## HIGHLIGHT LOW COMPONENT LOADINGS ##
        # CHANGE: data_color first, then cut-off red text on top so it is not overwritten
        fload <- fload %>%
          gt::tab_style(
            style = cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::data_color(
            columns = tidyselect::starts_with("component"),
            method = "numeric",
            palette = c("#fef2f2", "#fff7ed", "#dcfce7"),
            domain = c(-1, 1),
            na_color = "#f8fafc"
          )

        # CHANGE: apply_loading_cutoff_style called AFTER data_color so red text is not overwritten
        fload <- apply_loading_cutoff_style(
          gt_tbl = fload,
          df = PCA_ENV$factorLoading,
          prefix = "component",
          cutoff = current_settings()$cut_off
        )
        
        fload <- fload %>%
          tab_options(
            column_labels.font.size = gt::px(20),
            column_labels.font.weight = "bolder",
            data_row.padding = gt::px(10),
            heading.background.color = "#f1f5f9",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
        
        return(fload)
      }
    })
    
    
    
    
    
    #### COMMUNALITIES ###
    
    
    output$commons<-DT::renderDT({
      
      data <- na.omit(data())
      
      if (  length(current_settings()$grafmad)==0) {
        
        data1<-data }  else
          
          
        {       x<-as.numeric(current_settings()$grafmad)
        
        dat<-as.data.frame(data)
        
        data1<-dat[,-x]  }
      
      # CHANGE: Communalities now follow the selected data type
      result <- compute_pca_model(
        dat = data1,
        nfactors = current_settings()$fak2,
        rotation = current_settings()$rotation,
        data_type = current_settings()$type
      )
      
      common<- result$communality
      
      
      res_comon<- data.frame( Items=paste0("item", 1:ncol(data1)),  Extraction= round(common,3) )
      
      backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                  "aquamarine", "lightblue", "gray"), 1)
      datatable(res_comon) %>% formatStyle(colnames(res_comon),
                                           backgroundColor = backgroundColor) })
    
    ## EXPAINED VARIANCE ##
    
    add <- reactive({
      data <- na.omit(data())
      if (!is.null(input$data1)) {
        # CHANGE: Explained variance now follows the selected data type
        result <- compute_pca_model(
          dat = data,
          nfactors = current_settings()$fak2,
          rotation = current_settings()$rotation,
          data_type = current_settings()$type
        )
        
        result <- result$Vaccounted
        
        result <- result[-c(4, 5), ]
        
        result <- unname(result)
        
        nfac <- current_settings()$fak2
        
        name <- matrix(NA, 3, nfac)
        
        Col <- paste("Component", 1:nfac)
        
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
      
      if (current_settings()$fak2 > 3) {
        add <- add %>%
          gt::cols_width(everything() ~ gt::px(120))
      }
      
      if (current_settings()$fak2 <= 3) {
        add <- add %>%
          gt::cols_width(everything() ~ gt::px(180))
      }
      
      add <- add %>%
        gt::tab_style(
          style = cell_fill(color = "#f8fafc"),
          locations = gt::cells_body()
        ) %>%
        gt::tab_style(
          style = list(cell_fill(color = "#f0fdf4"), gt::cell_text(weight = "bold", color = "#15803d")),
          locations = gt::cells_body(rows = Statistic == "Eigenvalue")
        ) %>%
        gt::tab_style(
          style = list(cell_fill(color = "#dcfce7"), gt::cell_text(weight = "bold", color = "#166534")),
          locations = gt::cells_body(rows = Statistic == "Explained Variance")
        ) %>%
        gt::tab_style(
          style = list(cell_fill(color = "#bbf7d0"), gt::cell_text(weight = "bold", color = "#14532d")),
          locations = gt::cells_body(rows = Statistic == "Cummilative Explained Variance")
        ) %>%
        gt::tab_style(
          style = cell_fill(color = "#1e293b"),
          locations = gt::cells_column_labels()
        ) %>%
        gt::tab_style(
          style = gt::cell_text(color = "white", weight = "bold"),
          locations = gt::cells_column_labels()
        )
      
      add <- add %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold",
          row_group.font.weight = "bolder",
          data_row.padding = gt::px(12),
          heading.background.color = "#f0fdf4",
          table.border.top.color = "#1e293b",
          table.border.top.width = gt::px(3)
        )
      
      return(add)
    })
    
    ## reactive 1
    
    remainData <- reactive({
      omitted <- current_settings()$grafmad
      All <- 1:ncol(data()) # madisimGL
      dataGL <- na.omit(data())
      colnames(dataGL) <- All
      kalan <- setdiff(All, omitted)
      remainData <- dataGL[, kalan]
    })
    
    ## reactive 2
    
    model_removed <- reactive({
      # CHANGE: Recomputed FA after item removal now follows the selected data type
      modelRemain <- compute_pca_model(
        dat = remainData(),
        nfactors = current_settings()$fak2,
        rotation = current_settings()$rotation,
        data_type = current_settings()$type
      )
    })
    
    
    ############################### testttttttttt ###################
    
    output$KMo<-render_gt({
      
      if (!is.null(input$data1)) {
        
        # CHANGE: Current KMO now follows the selected data type
        korr <- compute_cor_matrix(remainData(), current_settings()$type)
        res <- compute_kmo_value(remainData(), current_settings()$type)
        
        res <- data.frame(KMO = res)
        
        res <- gt::gt(res)
        
        res <- res %>%
          tab_header(
            title =
              md("*Current KMO Test Result*")
          )
        
        res <- res %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#dcfce7"),
              gt::cell_text(weight = "bold", color = "#15803d", size = gt::px(18))
            ),
            locations = gt::cells_body(rows = KMO >= 0.70)
          ) %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#f0fdf4"),
              gt::cell_text(weight = "bold", color = "#15803d", size = gt::px(18))
            ),
            locations = gt::cells_body(rows = KMO >= 0.60 & KMO < 0.70)
          ) %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#fff7ed"),
              gt::cell_text(weight = "bold", color = "#c2410c", size = gt::px(18))
            ),
            locations = gt::cells_body(rows = KMO >= 0.50 & KMO < 0.60)
          ) %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#fef2f2"),
              gt::cell_text(weight = "bold", color = "#b91c1c", size = gt::px(18))
            ),
            locations = gt::cells_body(rows = KMO < 0.50)
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(weight = "bold", color = "white"),
            locations = gt::cells_column_labels()
          )
        
        res <- res %>%
          tab_options(
            heading.title.font.size = gt::px(25),
            data_row.padding = gt::px(16),
            heading.background.color = "#f1f5f9",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
        
        return(res)
      }
    })
    
    # CHANGE: Brief interpretation guide for Anti-image / MSA
    output$antiImageGuide <- render_gt({
      if (!is.null(input$data1)) {
        guide <- data.frame(
          Rule_of_thumb = ".80+ very good | .70+ good | .60+ acceptable | .50-.59 weak | < .50 consider removing/reviewing item"
        )
        
        guide <- gt::gt(guide)
        guide <- guide %>% tab_header(title = md("*HOW TO INTERPRET ANTI-IMAGE*"))
        guide <- guide %>% gt::cols_width(everything() ~ gt::px(660))
        guide <- guide %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#f8fafc"),
              gt::cell_text(align = "center", color = "#1e293b", size = gt::px(14))
            ),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = list(
              cell_fill(color = "#1e293b"),
              gt::cell_text(color = "white", weight = "bold", align = "center")
            ),
            locations = gt::cells_column_labels()
          )
        guide <- guide %>% tab_options(
          heading.title.font.size = gt::px(20),
          heading.align = "center",
          column_labels.font.size = gt::px(14),
          column_labels.font.weight = "bold",
          data_row.padding = gt::px(12),
          heading.background.color = "#f1f5f9",
          table.border.top.color = "#1e293b",
          table.border.top.width = gt::px(3)
        )
        guide
      }
    })
    
    # CHANGE: Anti-image / item-level MSA table added.
    output$antiImageTable <- render_gt({
      if (!is.null(input$data1)) {
        anti_obj <- compute_anti_image(remainData(), current_settings()$type)
        item_names <- colnames(remainData())
        if (is.null(item_names)) {
          item_names <- paste0("item", seq_along(anti_obj$item_msa))
        }
        
        anti_tab <- data.frame(
          Item = item_names,
          MSA = round(anti_obj$item_msa, 3)
        )
        
        anti_tab <- gt::gt(anti_tab)
        anti_tab <- anti_tab %>% tab_header(title = md("*ITEM-LEVEL MSA (ANTI-IMAGE)*"))
        anti_tab <- anti_tab %>% gt::cols_width(everything() ~ gt::px(180))
        anti_tab <- anti_tab %>%
          gt::tab_style(
            style = cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          )
        anti_tab <- anti_tab %>%
          gt::tab_style(
            style = list(cell_fill(color = "#fef2f2"), gt::cell_text(weight = "bold", color = "#b91c1c", decorate = "underline")),
            locations = gt::cells_body(columns = c(MSA), rows = MSA < 0.50)
          ) %>%
          gt::tab_style(
            style = list(cell_fill(color = "#fff7ed"), gt::cell_text(weight = "bold", color = "#c2410c")),
            locations = gt::cells_body(columns = c(MSA), rows = MSA >= 0.50 & MSA < 0.60)
          ) %>%
          gt::tab_style(
            style = list(cell_fill(color = "#fefce8"), gt::cell_text(weight = "bold", color = "#854d0e")),
            locations = gt::cells_body(columns = c(MSA), rows = MSA >= 0.60 & MSA < 0.70)
          ) %>%
          gt::tab_style(
            style = list(cell_fill(color = "#f0fdf4"), gt::cell_text(weight = "bold", color = "#15803d")),
            locations = gt::cells_body(columns = c(MSA), rows = MSA >= 0.70 & MSA < 0.80)
          ) %>%
          gt::tab_style(
            style = list(cell_fill(color = "#dcfce7"), gt::cell_text(weight = "bold", color = "#166534")),
            locations = gt::cells_body(columns = c(MSA), rows = MSA >= 0.80)
          )
        anti_tab <- anti_tab %>% tab_options(
          heading.title.font.size = gt::px(22),
          column_labels.font.size = gt::px(16),
          column_labels.font.weight = "bold",
          data_row.padding = gt::px(10),
          heading.background.color = "#f1f5f9",
          table.border.top.color = "#1e293b",
          table.border.top.width = gt::px(3)
        )
        anti_tab
      }
    })
    
    
    ############################### testttttttttt ###################
    
    
    ## OBSERVE EVENT - RE-COMPUTATION AFTER OMITTED items ##
    
    nfacRemain <- reactive({
      nfacRemain <- current_settings()$fak2
    })
    
    observeEvent(input$remove, {
      shinyjs::hide("tableFactor")
      shinyjs::hide("tableEigen")
      
      output$buton <- render_gt(align = "center", {
        
        load_mat_remain <- as.matrix(unclass(model_removed()$loadings))
        if (is.null(dim(load_mat_remain))) {
          load_mat_remain <- matrix(load_mat_remain, ncol = 1)
        }
        if (ncol(load_mat_remain) < nfacRemain()) {
          load_mat_remain <- cbind(load_mat_remain, matrix(NA_real_, nrow(load_mat_remain), nfacRemain() - ncol(load_mat_remain)))
        }
        load_mat_remain <- load_mat_remain[, seq_len(nfacRemain()), drop = FALSE]
        remainFactorName <- paste0("component", seq_len(nfacRemain()))
        colnames(load_mat_remain) <- remainFactorName
        
        items <- seq_len(nrow(load_mat_remain))
        load_mat_remain <- round(load_mat_remain, 2)
        
        omitted <- current_settings()$grafmad
        All <- 1:ncol(data())
        dataGL <- na.omit(data())
        colnames(dataGL) <- All
        remain_Items <- setdiff(All, omitted)
        
        floadRemain <- data.frame(remain_Items = remain_Items, item = items, load_mat_remain, check.names = FALSE)
        
        PCA_ENV$factorLoadRemain <- floadRemain
        floadRemain <- gt::gt(floadRemain)
        
        
        
        floadRemain <- floadRemain %>% tab_header(
          title =
            md("**COMPONENT LOADINGS**")
        )
        
        ## Column Arrangement According to Factor Numbers ##
        
        if (current_settings()$fak2 > 3) {
          floadRemain <- floadRemain %>%
            gt::cols_width(everything() ~ gt::px(120))
        }
        
        if (current_settings()$fak2 <= 3) {
          floadRemain <- floadRemain %>%
            gt::cols_width(everything() ~ gt::px(180))
        }
        
        
        ## UNDERLINELOW COMPONENT LOADINGS ##
        # CHANGE: data_color first, then cut-off red text on top so it is not overwritten
        floadRemain <- floadRemain %>%
          gt::tab_style(
            style = cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = cell_fill(color = "#1e293b"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "white", weight = "bold"),
            locations = gt::cells_column_labels()
          ) %>%
          gt::data_color(
            columns = tidyselect::starts_with("component"),
            method = "numeric",
            palette = c("#fef2f2", "#fff7ed", "#dcfce7"),
            domain = c(-1, 1),
            na_color = "#f8fafc"
          )

        # CHANGE: apply_loading_cutoff_style called AFTER data_color so red text is not overwritten
        floadRemain <- apply_loading_cutoff_style(
          gt_tbl = floadRemain,
          df = PCA_ENV$factorLoadRemain,
          prefix = "component",
          cutoff = current_settings()$cut_off
        )

        #
        
        floadRemain <- floadRemain %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            data_row.padding = gt::px(10),
            heading.background.color = "#f1f5f9",
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
        
        return(floadRemain)
      })
      
      ## REMOVED EXPLAINED VARIANCE ##
      
      
      ad.var.2 <- reactive({
        
        # align sola sabitlendi
        # CHANGE: Same explained-variance extraction used for all data types
        modelRemain.var <- model_removed()$Vaccounted
        modelRemain.var <- modelRemain.var[-c(4, 5), ]
        modelRemain.var <- unname(modelRemain.var)
        ad.var <- matrix(NA, 3, nfacRemain())
        sutun <- paste0("Component", 1:nfacRemain())
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
          
          if (current_settings()$fak2 > 3) {
            ad.var.2 <- ad.var.2 %>%
              gt::cols_width(everything() ~ gt::px(120))
          }
          
          if (current_settings()$fak2 <= 3) {
            ad.var.2 <- ad.var.2 %>%
              gt::cols_width(everything() ~ gt::px(180))
          }
          
          ad.var.2 <- ad.var.2 %>%
            gt::tab_style(
              style = cell_fill(color = "#f8fafc"),
              locations = gt::cells_body()
            ) %>%
            gt::tab_style(
              style = list(cell_fill(color = "#f0fdf4"), gt::cell_text(weight = "bold", color = "#15803d")),
              locations = gt::cells_body(rows = Statistics == "Eigenvalue")
            ) %>%
            gt::tab_style(
              style = list(cell_fill(color = "#dcfce7"), gt::cell_text(weight = "bold", color = "#166534")),
              locations = gt::cells_body(rows = Statistics == "Explained Variance")
            ) %>%
            gt::tab_style(
              style = list(cell_fill(color = "#bbf7d0"), gt::cell_text(weight = "bold", color = "#14532d")),
              locations = gt::cells_body(rows = Statistics == "Cummilative Explained Variance")
            ) %>%
            gt::tab_style(
              style = cell_fill(color = "#1e293b"),
              locations = gt::cells_column_labels()
            ) %>%
            gt::tab_style(
              style = gt::cell_text(color = "white", weight = "bold"),
              locations = gt::cells_column_labels()
            )
          
          ad.var.2 <- ad.var.2 %>%
            tab_options(
              column_labels.font.size = gt::px(17),
              column_labels.font.weight = "bold",
              data_row.padding = gt::px(12),
              heading.background.color = "#f0fdf4",
              table.border.top.color = "#1e293b",
              table.border.top.width = gt::px(3)
            )
          
          return(ad.var.2 <- ad.var.2)
        })
    }) ##  close observe event
    
    ## DOWNLOAD OUTPUTS ##
    
    output$factorDownload <- downloadHandler(
      
      
      filename = function() {
        "component-loadings.csv"
      },
      content = function(file) {
        # CHANGE: Download logic made robust so selection without removal does not break downloads
        if (is.null(current_settings()$grafmad) || length(current_settings()$grafmad) == 0 || is.null(PCA_ENV$factorLoadRemain)) {
          utils::write.csv2(PCA_ENV$factorLoading, file)
        } else {
          utils::write.csv2(PCA_ENV$factorLoadRemain, file)
        }
      }
    )
    
    output$varianceDownload <- downloadHandler(
      
      filename = function() {
        "variance.csv"
      },
      content = function(file) {
        # CHANGE: Download logic made robust so selection without removal does not break downloads
        if (is.null(current_settings()$grafmad) || length(current_settings()$grafmad) == 0 || is.null(PCA_ENV$explainedVarRemain)) {
          utils::write.csv2(PCA_ENV$explainedVar, file)
        } else {
          utils::write.csv2(PCA_ENV$explainedVarRemain, file)
        }
      }
    )
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
    EIGENVALUE <- NULL
  }
  
  
  
  
  shinyApp(ui = ui, server = server)
}
