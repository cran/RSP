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
#' @examples \dontrun{
#' CFA()
#' }
#' @export
CFA <- function() {
  CFA_ENV <- new.env()
  js <- "
// This solution from https://stackoverflow.com/a/59674107
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

  rspInfoCard <- function(mode = "compact") {
    is_hero <- identical(mode, "hero")
    is_sidebar <- identical(mode, "sidebar-mini")

    if (is_sidebar) {
      return(
        shiny::div(
          class = "rsp-sidebar-simple",
          shiny::div(
            class = "rsp-sidebar-content",
            shiny::div(
              class = "rsp-sidebar-package",
              shiny::span(class = "rsp-sidebar-mark"),
              shiny::span("RSP Package")
            ),
            shiny::div(
              class = "rsp-sidebar-lines",
              shiny::div("CONFIRMATORY FACTOR"),
              shiny::div("ANALYSIS - CLICK MODE")
            )
          ),
          shiny::div(class = "rsp-sidebar-credit", "Dogan & Aybek (2022)")
        )
      )
    }

    if (is_hero) {
      return(
        shiny::div(
          class = "ctt-hero",
          shiny::div(
            class = "ctt-left",
            shiny::div(
              class = "ctt-brand",
              shiny::div(
                class = "ctt-logo",
                shiny::span("R-Shiny"),
                shiny::tags$b("RSP"),
                shiny::span("Package")
              ),
              shiny::div(
                class = "ctt-brand-text",
                shiny::div(class = "ctt-package", "RSP Package"),
                shiny::div(class = "ctt-domain", "R-Shiny ", shiny::tags$b("*"), " Psychometry")
              )
            ),
            shiny::h2(
              class = "ctt-main-title ctt-main-title-08",
              shiny::span(class = "ctt-title-top", "CONFIRMATORY FACTOR ANALYSIS"),
              shiny::span(class = "ctt-title-sub", "MEASUREMENT AND STRUCTURAL MODELS")
            ),
          ),
          shiny::div(
            class = "ctt-bottom",
            shiny::div(
              class = "ctt-feature",
              shiny::div(class = "ctt-icon shield"),
              shiny::span("MODEL FIT")
            ),
            shiny::div(
              class = "ctt-feature",
              shiny::div(class = "ctt-icon bars", shiny::span(), shiny::span(), shiny::span()),
              shiny::span("PARAMETERS")
            ),
            shiny::div(
              class = "ctt-feature",
              shiny::div(class = "ctt-icon line"),
              shiny::span("PATH DIAGRAM")
            )
          ),
          shiny::div(class = "ctt-credit", "Dogan & Aybek (2022)")
        )
      )
    }

    shiny::div()
  }

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
      shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"),
      shiny::tags$script(shiny::HTML("
      window.cfaVisStore = window.cfaVisStore || {};
      Shiny.addCustomMessageHandler('fitVisPathDiagram', function(message) {
        var net = window.cfaVisStore[message.id];
        if (net) {
          net.fit({animation: {duration: 500, easingFunction: 'easeInOutQuad'}});
        }
      });

      async function exportCurrentPathDiagram(format) {
        var el = document.getElementById('path_capture_container');
        if (!el || typeof html2canvas === 'undefined') return;

        var canvas = await html2canvas(el, {
          backgroundColor: '#ffffff',
          scale: 8,
          useCORS: true,
          logging: false,
          allowTaint: true
        });

        if (format === 'jpeg') {
          var link = document.createElement('a');
          link.download = 'path_diagram_' + new Date().toISOString().slice(0,10) + '.jpeg';
          // 35.6: higher-resolution JPEG export; quality remains maximum.
          link.href = canvas.toDataURL('image/jpeg', 1.0);
          link.click();
        } else {
          var imgData = canvas.toDataURL('image/png', 1.0);
          var jsPDF = window.jspdf.jsPDF;
          var pdf = new jsPDF({
            orientation: canvas.width >= canvas.height ? 'landscape' : 'portrait',
            unit: 'pt',
            format: [canvas.height, canvas.width],
            compress: false
          });
          pdf.addImage(imgData, 'PNG', 0, 0, canvas.width, canvas.height, undefined, 'NONE');
          pdf.save('path_diagram_' + new Date().toISOString().slice(0,10) + '.pdf');
        }
      }

      $(document).on('click', '#vis_download_pdf', function() { exportCurrentPathDiagram('pdf'); });
      $(document).on('click', '#vis_download_jpeg', function() { exportCurrentPathDiagram('jpeg'); });
      "))
    ),
    shiny::uiOutput("cols2"),
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      #image0 img {max-width: 100%; width: auto; height: 100%; align: center}
      table,img, .tippy-content, textarea{
        border-collapse: collapse;
        border-radius: 1em;
        overflow: hidden;
      }
      th, td {
        padding: 1em;
        background: #ddd;
        border-bottom: 2px solid white;
        border-top: 2px solid white;
      }
      #tepe{ border-bottom: 3px solid black; }
      #a{color:black; font-family:Lucida Arial ;font-size: 16px; font-style: oblique;text-align:center}
      #ab{color:black; font-family:Lucida Arial ;font-size: 20px; font-style: oblique;text-align:center}
      #b{color:black; font-family: cursive;font-size: 15px; font-style: oblique;text-align:center}
      .compact-sidebar .form-group {margin-bottom: 8px !important;}
      .compact-sidebar .control-label {margin-bottom: 3px !important; font-size: 13px !important;}
      .compact-sidebar .irs {margin-top: 2px; margin-bottom: 2px;}
      .compact-sidebar .shiny-input-container {margin-bottom: 6px !important;}
      .compact-sidebar .btn-group-container-sw {margin-bottom: 4px !important;}
      .compact-sidebar .radio-group-buttons {margin-bottom: 4px !important;}
      .compact-sidebar .help-block {margin-top: 4px; margin-bottom: 4px; font-size: 12px;}
      /* 35.6: theme-colored sliders/download/action buttons; selected widget texts fixed */
      :root { --rsp-theme: #1565C0; }
      .btn, .btn:hover, .btn:focus, .btn:active,
      .bttn, .bttn:hover, .bttn:focus, .bttn:active,
      .bttn * { color: #ffffff !important; }
      /* 35.6: Grsellerde belirtilen input/radio widget yazlar siyah */
      .radio-group-buttons .btn,
      .radio-group-buttons .btn:hover,
      .radio-group-buttons .btn:focus,
      .radio-group-buttons .btn:active,
      .btn-group-container-sw .btn,
      .btn-group-container-sw .btn:hover,
      .btn-group-container-sw .btn:focus,
      .btn-group-container-sw .btn:active,
      .radioGroupButtons .btn,
      .radioGroupButtons .btn:hover,
      .radioGroupButtons .btn:focus,
      .radioGroupButtons .btn:active {
        color: #111111 !important;
      }
      .radio-group-buttons .btn *,
      .btn-group-container-sw .btn *,
      .radioGroupButtons .btn * {
        color: #111111 !important;
      }
      .selectize-input,
      .selectize-input *,
      .selectize-dropdown,
      .selectize-dropdown * {
        color: #111111 !important;
      }

      /* 35.7: Bu uc widget basligi ve icerik metinleri siyah kalir */
      #type2-label, #type2-label *,
      #data1-label, #data1-label *,
      #type3_2-label, #type3_2-label *,
      .force-black-label,
      .force-black-label *,
      div.form-group:has(#type2) label,
      div.form-group:has(#type2) label *,
      div.form-group:has(#data1) label,
      div.form-group:has(#data1) label *,
      div.form-group:has(#type3_2) label,
      div.form-group:has(#type3_2) label * {
        color: #111111 !important;
      }
      #type2 + .dropdown-toggle,
      #type2 + .dropdown-toggle *,
      #type3_2 + .dropdown-toggle,
      #type3_2 + .dropdown-toggle *,
      .btn.dropdown-toggle.btn-default,
      .btn.dropdown-toggle.btn-default *,
      .bootstrap-select > .dropdown-toggle,
      .bootstrap-select > .dropdown-toggle *,
      .dropdown-menu > li > a,
      .dropdown-menu > li > a *,
      .bootstrap-select .dropdown-menu li a,
      .bootstrap-select .dropdown-menu li a span.text,
      .bootstrap-select .dropdown-menu li a *,
      #data1_progress,
      #data1_progress * {
        color: #111111 !important;
      }
      #dwn { color: #ffffff !important; margin: 0 !important; }
      .bttn-jelly.bttn-primary, .bttn-unite.bttn-primary, .bttn-gradient,
      .btn-primary, .btn-success, .btn-info, .btn-warning {
        background: var(--rsp-theme) !important;
        background-color: var(--rsp-theme) !important;
        border-color: var(--rsp-theme) !important;
        color: #ffffff !important;
      }
      .irs-bar, .irs-bar-edge, .irs-single { background: var(--rsp-theme) !important; border-color: var(--rsp-theme) !important; }
      .irs-from, .irs-to, .irs-single { color: #ffffff !important; }
      .irs-slider, .irs-handle, .irs-handle > i:first-child { border-color: var(--rsp-theme) !important; }
      .path-toolbar {margin-bottom: 10px; display:flex; gap:8px; flex-wrap:wrap; align-items:center;}
      .path-diagram-wrap {
        height: calc(100vh - 240px);
        min-height: 760px;
        position: relative;
        overflow: hidden !important;
      }

      #visPathDiagram {
        position: relative;
        z-index: 1;
      }
      #visPathDiagram .vis-network,
      #visPathDiagram canvas {
        position: relative !important;
        z-index: 1 !important;
      }










      .path-diagram-wrap .vis-network {border: 1px solid #d9d9d9; border-radius: 10px; background: white;}
      .path-legend-top {display:flex; gap:18px; align-items:center; flex-wrap:wrap; margin-bottom:10px; padding:8px 12px; border:1px solid #d9d9d9; border-radius:10px; background:#ffffff;}
      .path-legend-chip {display:inline-flex; align-items:center; gap:8px; font-size:14px; font-weight:600;}
      .path-legend-shape {display:inline-block; width:18px; height:18px; border:2px solid #2C3E50; background:#cccccc;}
      .path-legend-shape.ellipse, .path-legend-shape.circle {border-radius:50%;}
      .sm2-section-box {margin-top: 8px; margin-bottom: 10px; padding: 8px; border: 1px solid #d9d9d9; border-radius: 8px; background: #fafafa;}
      .sm2-toggle-btn {width: 100%; margin-bottom: 8px;}

      .rsp-sidebar-simple, .rsp-sidebar-simple * { box-sizing:border-box; }
      .rsp-sidebar-simple {
        width:100%; height:128px; min-height:128px; max-height:128px;
        margin:0 0 12px 0; padding:12px 12px;
        position:relative; overflow:hidden; border-radius:15px;
        background:linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--rsp-theme) 9%, #ffffff) 56%, color-mix(in srgb, var(--rsp-theme) 18%, #ffffff) 100%);
        border:1px solid color-mix(in srgb, var(--rsp-theme) 24%, #ffffff);
        box-shadow:0 10px 24px rgba(24, 20, 40, .08);
        color:#07183f;
      }
      .rsp-sidebar-simple:before {
        content:''; position:absolute; right:-42px; bottom:-68px;
        width:150px; height:122px; border-radius:58% 42% 0 0;
        background:linear-gradient(135deg, color-mix(in srgb, var(--rsp-theme) 22%, #ffffff), var(--rsp-theme));
        opacity:.55; transform:rotate(-8deg);
      }
      .rsp-sidebar-simple:after {
        content:''; position:absolute; right:10px; top:9px; width:46px; height:46px;
        background-image:radial-gradient(var(--rsp-theme) 1.6px, transparent 2px);
        background-size:12px 12px; opacity:.18;
      }
      .rsp-sidebar-content { position:relative; z-index:2; }
      .rsp-sidebar-package {
        display:inline-flex; align-items:center; gap:6px; max-width:100%;
        padding:4px 8px; border-radius:999px;
        background:rgba(255,255,255,.72);
        border:1px solid color-mix(in srgb, var(--rsp-theme) 18%, #ffffff);
        color:#16325c; font-size:11px; font-weight:850; letter-spacing:.15px;
      }
      .rsp-sidebar-mark { width:13px; height:13px; border-radius:4px; flex:0 0 13px; background:linear-gradient(135deg, var(--rsp-theme), color-mix(in srgb, var(--rsp-theme) 48%, #ffffff)); position:relative; }
      .rsp-sidebar-mark:after { content:''; position:absolute; inset:3.5px; border:1.3px solid #fff; border-radius:2.5px; }
      .rsp-sidebar-lines { margin-top:6px; gap:2px; max-width:100%; display:grid; }
      .rsp-sidebar-lines div { font-size:10.4px; line-height:1.12; letter-spacing:.25px; font-weight:900; }
      .rsp-sidebar-lines div:nth-child(2) { color:var(--rsp-theme); }
      .rsp-sidebar-credit {
        position:absolute; left:12px; right:auto; bottom:8px; max-width:calc(100% - 24px);
        color:color-mix(in srgb, var(--rsp-theme) 72%, #17213d);
        font-size:9.6px; font-weight:800; letter-spacing:.1px;
      }

      .ctt-hero, .ctt-hero * { box-sizing:border-box; }
      .ctt-hero {
        width:97%; min-height:340px; margin:0 auto 16px auto;
        position:relative; overflow:hidden; border-radius:22px;
        padding:28px 34px 28px 34px;
        background:
          radial-gradient(circle at 88% 92%, color-mix(in srgb, var(--rsp-theme) 16%, transparent) 0, transparent 33%),
          linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--rsp-theme) 5%, #ffffff) 62%, color-mix(in srgb, var(--rsp-theme) 11%, #ffffff) 100%);
        border:1px solid color-mix(in srgb, var(--rsp-theme) 18%, #ffffff);
        box-shadow:0 18px 42px rgba(22,24,44,.09);
        color:#07183f;
      }
      .ctt-hero:after {
        content:''; position:absolute; right:22px; top:18px; width:62px; height:62px;
        background-image:radial-gradient(var(--rsp-theme) 1.8px, transparent 2.3px);
        background-size:14px 14px; opacity:.20;
      }
      .ctt-left { position:relative; z-index:2; width:100%; min-width:0; max-width:100%; padding-top:0; }
      .ctt-brand { display:flex; align-items:center; gap:18px; margin-bottom:8px; }
      .ctt-logo {
        width:76px; height:76px; display:flex; flex-direction:column; align-items:center; justify-content:center;
        clip-path:polygon(25% 5%,75% 5%,100% 50%,75% 95%,25% 95%,0 50%);
        border:3px solid var(--rsp-theme); background:rgba(255,255,255,.68);
        color:#07183f; font-weight:900; line-height:1.06; box-shadow:0 10px 24px rgba(22,24,44,.08);
      }
      .ctt-logo b { color:var(--rsp-theme); font-size:23px; letter-spacing:-.8px; }
      .ctt-logo span { font-size:9.5px; }
      .ctt-brand-text { border-left:1px solid rgba(80,80,120,.18); padding-left:20px; }
      .ctt-package { font-size:20px; font-weight:900; margin-bottom:8px; }
      .ctt-domain { font-size:15px; color:#59627c; font-weight:650; }
      .ctt-main-title-08 {
        max-width:100%; text-align:center;
        font-size:clamp(32px, 4.4vw, 24px);
        line-height:1.06; letter-spacing:-1.1px; margin-top:4px;
      }
      .ctt-main-title-08 .ctt-title-top { display:block; color:#07183f; }
      .ctt-main-title-08 .ctt-title-sub {
        display:block; text-align:center; margin-top:8px;
        color:var(--rsp-theme); font-size:.68em;
        letter-spacing:.5px; line-height:1.18;
      }
      .ctt-desc-08 {
        max-width:720px; margin:14px auto 0 auto;
        text-align:center; font-size:14px; line-height:1.35;
        color:#4f5873;
      }
      .ctt-panels {
        position:absolute; z-index:2; left:43%; right:32px; top:30px; bottom:58px;
        display:grid; grid-template-columns:1fr 1.72fr; grid-template-rows:1fr 1fr; gap:14px;
      }
      .ctt-card { position:relative; overflow:hidden; border-radius:16px; background:rgba(255,255,255,.72); border:1px solid rgba(90,80,130,.18); box-shadow:0 10px 24px rgba(22,24,44,.06); padding:14px 16px; }
      .ctt-card-title { margin:0 0 12px 0; color:var(--rsp-theme); font-size:12px; font-weight:900; letter-spacing:.3px; }
      .ctt-reliability { grid-column:1; grid-row:1; }
      .ctt-difficulty { grid-column:2; grid-row:1; }
      .ctt-discrimination { grid-column:1; grid-row:2; }
      .ctt-table-card { grid-column:2; grid-row:2; }
      .ctt-ring { width:122px; height:122px; border-radius:50%; margin:8px auto 0 auto; background:conic-gradient(var(--rsp-theme) 0 78deg, color-mix(in srgb, var(--rsp-theme) 38%, #ffffff) 78deg 360deg); display:flex; align-items:center; justify-content:center; }
      .ctt-ring-inner { width:82px; height:82px; border-radius:50%; background:#fff; display:flex; flex-direction:column; align-items:center; justify-content:center; color:#07183f; }
      .ctt-ring-inner b { font-size:28px; line-height:1; }
      .ctt-ring-inner span { font-size:11px; margin-top:5px; color:#4f5873; font-weight:700; }
      .ctt-bars { position:absolute; left:24px; right:24px; bottom:12px; height:88px; border-bottom:2px solid color-mix(in srgb, var(--rsp-theme) 52%, #ffffff); }
      .ctt-bars span { position:absolute; bottom:0; width:5.5%; border-radius:5px 5px 0 0; background:color-mix(in srgb, var(--rsp-theme) 36%, #ffffff); }
      .ctt-bars span:nth-child(1){left:2%;height:18%}.ctt-bars span:nth-child(2){left:10%;height:34%}.ctt-bars span:nth-child(3){left:18%;height:55%}.ctt-bars span:nth-child(4){left:26%;height:75%}.ctt-bars span:nth-child(5){left:34%;height:90%}.ctt-bars span:nth-child(6){left:42%;height:100%}.ctt-bars span:nth-child(7){left:50%;height:82%}.ctt-bars span:nth-child(8){left:58%;height:64%}.ctt-bars span:nth-child(9){left:66%;height:76%}.ctt-bars span:nth-child(10){left:74%;height:52%}.ctt-bars span:nth-child(11){left:82%;height:34%}.ctt-bars span:nth-child(12){left:90%;height:18%}
      .ctt-scatter { position:absolute; left:20px; right:20px; top:40px; bottom:12px; border-left:1px solid rgba(80,80,120,.25); border-bottom:2px solid rgba(80,80,120,.25); background:linear-gradient(rgba(80,80,120,.08) 1px, transparent 1px), linear-gradient(90deg, rgba(80,80,120,.08) 1px, transparent 1px); background-size:28px 24px; }
      .ctt-dot { position:absolute; width:6px; height:6px; border-radius:50%; background:var(--rsp-theme); opacity:.88; }
      .ctt-dot.d1{left:12%;bottom:18%}.ctt-dot.d2{left:20%;bottom:28%}.ctt-dot.d3{left:28%;bottom:36%}.ctt-dot.d4{left:35%;bottom:44%}.ctt-dot.d5{left:41%;bottom:42%}.ctt-dot.d6{left:48%;bottom:55%}.ctt-dot.d7{left:56%;bottom:62%}.ctt-dot.d8{left:63%;bottom:70%}.ctt-dot.d9{left:73%;bottom:78%}.ctt-dot.d10{left:84%;bottom:86%}.ctt-dot.d11{left:50%;bottom:28%}.ctt-dot.d12{left:69%;bottom:54%}
      .ctt-mini-table { width:100%; border-collapse:collapse; font-size:12px; color:#07183f; }
      .ctt-mini-table th { color:#1b2440; font-size:10px; text-align:left; border-bottom:1px solid rgba(80,80,120,.16); padding:6px 4px; }
      .ctt-mini-table td { border-bottom:1px solid rgba(80,80,120,.12); padding:7px 4px; font-weight:650; background:transparent; }
      .ctt-mini-table tr:last-child td { border-bottom:0; }
      .ctt-bottom {
        position:relative; left:auto; right:auto; transform:none;
        width:100%; height:86px; margin-top:16px;
        display:flex; justify-content:space-around; align-items:center;
        background:rgba(255,255,255,.68); border:1px solid rgba(90,80,130,.16); border-radius:16px;
      }
      .ctt-feature { display:flex; flex-direction:column; align-items:center; justify-content:center; gap:6px; color:#07183f; font-size:11px; font-weight:900; flex:1 1 33.33%; border-right:1px solid rgba(80,80,120,.12); }
      .ctt-feature:last-child { border-right:0; }
      .ctt-icon { width:26px; height:22px; position:relative; }
      .ctt-icon.shield { background:linear-gradient(135deg, var(--rsp-theme), color-mix(in srgb, var(--rsp-theme) 60%, #ffffff)); border-radius:3px; border:2px solid var(--rsp-theme); }
      .ctt-icon.shield:before { content:''; position:absolute; left:6px; right:6px; top:4px; bottom:8px; border:1.5px solid #fff; border-radius:2px; }
      .ctt-icon.bars { border-left:3px solid var(--rsp-theme); }
      .ctt-icon.bars span { position:absolute; bottom:0; width:3px; background:var(--rsp-theme); border-radius:2px; }
      .ctt-icon.bars span:nth-child(1){left:6px;height:6px}.ctt-icon.bars span:nth-child(2){left:12px;height:12px}.ctt-icon.bars span:nth-child(3){left:18px;height:18px}
      .ctt-icon.line { border-bottom:3px solid var(--rsp-theme); }
      .ctt-icon.line:before { content:''; position:absolute; left:4px; right:4px; top:50%; width:18px; height:3px; background:var(--rsp-theme); border-radius:2px; transform:rotate(-35deg) translateY(-50%); }
      .ctt-icon.line:after { content:''; position:absolute; left:2px; top:50%; width:6px; height:6px; border-radius:50%; background:var(--rsp-theme); transform:translateY(-50%); }
      .ctt-credit {
        position:absolute; right:32px; top:28px;
        color:color-mix(in srgb, var(--rsp-theme) 80%, #17213d);
        font-size:11px; font-weight:800; z-index:2;
      }
      @media (max-width: 1120px) {
        .ctt-hero { min-height:365px; }
        .ctt-panels { display:none; }
        .ctt-bottom { height:84px; }
      }
    "))),
    shinyBS::bsTooltip(id = "type2", title = "Make sure you choose the file format correctly!", placement = "right", trigger = "hover"),
    shiny::div(
      id = "tepe",
      shiny::fluidRow(
        shiny::column(
          6,
          h1(
            id = "title",
            div(span("CONFIRMATORY FACTOR ANALYSIS", style = "color: black;")),
            div(span("(CFA-CLICK MODE)", style = "color: #1565C0; font-size: 22px;"))
          ),
          tags$style(HTML("
  #title {
    font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;
    font-size: 30px;
    font-style: oblique;
    text-align: left;
  }
"))
        ),
        shiny::column(
          6,
          shiny::h1(id = "title2", "RSP PACKAGE  - CRAN"),
          shiny::tags$style(shiny::HTML("#title2{color: black; font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;font-size:15px; font-style: oblique;text-align:right}"))
        )
      )
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        class = "compact-sidebar",
        shiny::conditionalPanel(
          condition = "input.panel==0",
          rspInfoCard("sidebar-mini"),
          shiny::tags$head(shiny::tags$script(shiny::HTML(js))),
          shiny::textOutput("browser2"),
          shiny::tags$head(shiny::tags$style("#browser2{color: darkblue; font-size: 25px; font-family: cursive; font-style: oblique; text-align:center; letter-spacing:1px;}")),
          shiny::br(), shiny::br(),
          shinyWidgets::spectrumInput(
            inputId = "myColor2",
            label = "CHANGE THE COLOR OF THE THEME:",
            choices = list(
              list("gray", "white", "blanchedalmond", "steelblue", "forestgreen"),
              as.list(scales::brewer_pal(palette = "Blues")(9)),
              as.list(scales::brewer_pal(palette = "Greens")(9)),
              as.list(scales::brewer_pal(palette = "Spectral")(11)),
              as.list(scales::brewer_pal(palette = "Dark2")(8))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
          )
        ),
        shiny::conditionalPanel(
          condition = "input.panel==1",
          rspInfoCard("sidebar-mini"),
          shinyWidgets::pickerInput(
            inputId = "type2",
            label = shiny::h3(id = "ab", "Select File Format", class = "force-black-label"),
            choices = list(
              "CSV - Semicolon  Separated  Excel" = 1,
              "CSV - Comma  Separated  Excel" = 2,
              "SAV - SPSS" = 3,
              "XLSX - Excel" = 4
            ),
            selected = 3,
            options = shinyWidgets::pickerOptions(showTick = TRUE)
          ),
          shiny::uiOutput("uiHeader2"),
          shiny::fileInput("data1", shiny::h3(id = "ab", "Upload File", shiny::icon("paper-plane"), class = "force-black-label")),
          gt::gt_output("dat2_2")
        ),
        shiny::conditionalPanel(
          condition = "input.panel==2",
          shiny::br(),
          shiny::actionButton("toggle_model2", "MODEL", class = "btn btn-info sm2-toggle-btn"),
          shinyjs::hidden(
            shiny::div(
              id = "model_section2",
              class = "sm2-section-box",
              shiny::sliderInput("fak_sm2", shiny::h3(id = "ab", "Select Number of Factors"), min = 1, max = 10, value = 1, step = 1),
              shiny::uiOutput("sm2_builder_ui"),
              shiny::actionButton("toggle_manual_model2", "MANUAL LAVAAN MODEL", class = "btn btn-default sm2-toggle-btn"),
              shinyjs::hidden(
                shiny::div(
                  id = "manual_model_section2",
                  class = "sm2-section-box",
                  shiny::textAreaInput(
                    "manual_model_text",
                    "Manual Lavaan Model Syntax",
                    value = "",
                    width = "100%",
                    height = "220px",
                    placeholder = "F1 =~ item1 + item2 + item3\nF2 =~ item4 + item5 + item6\nF2 ~ F1\nitem1 ~~ item2"
                  )
                )
              )
            )
          ),
          shiny::actionButton("toggle_struct2", "STRUCTURAL RELATIONS", class = "btn btn-success sm2-toggle-btn"),
          shinyjs::hidden(
            shiny::div(
              id = "struct_section2",
              class = "sm2-section-box",
              shiny::uiOutput("sm2_struct_controls_ui")
            )
          ),
          shiny::actionButton("toggle_mod2", "MODIFICATIONS", class = "btn btn-warning sm2-toggle-btn"),
          shinyjs::hidden(
            shiny::div(
              id = "mod_section2",
              class = "sm2-section-box",
              shiny::uiOutput("sm2_mod_controls_ui")
            )
          ),
          shinyWidgets::pickerInput(
            "type3_2",
            shiny::h3(id = "ab", "Select Method of Prediction", class = "force-black-label"),
            choices = c(
              "ML (continuous data)" = "ML",
              "MLR (robust, non-normal continuous)" = "MLR",
              "GLS (continuous data)" = "GLS",
              "WLS (ordinal/categorical)" = "WLS",
              "DWLS (ordinal/categorical)" = "DWLS",
              "WLSMV (ordinal/categorical, recommended)" = "WLSMV",
              "ULS (non-normal / small samples)" = "ULS"
              # ,"BAYESIAN" = "BAYESIAN"  # BAYESIAN ICIN INAKTIF
            ),
            selected = "ML",
            options = shinyWidgets::pickerOptions(showTick = TRUE, liveSearch = FALSE)
          ),
          shiny::actionButton("send2", "APPLY", class = "btn btn-primary"),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput("tree_b", shiny::h3(id = "ab", "Graph Type"),
                choices = list("Tree_1" = "tree", "Tree_2" = "tree2", "Circle_1" = "circle", "Circle_2" = "circle2", "Spring" = "spring"),
                selected = "tree2"
              )
            ),
            shiny::column(
              6,
              shiny::selectInput("colour_b", shiny::h3(id = "ab", "Graph Colour"),
                choices = list("Black" = 1, "Red" = 2, "Green" = 3, "Blue" = 4),
                selected = 4
              )
            )
          ),
          shiny::plotOutput("path2")
        ),
        shiny::conditionalPanel(
          condition = "input.panel==3",
          shiny::br(),
          shiny::h4(id = "ab", "Path Diagram Settings"),
          shiny::tags$div(style = "height:2px;"),
          shinyWidgets::spectrumInput(
            inputId = "vis_latent_color",
            label = shiny::span("Latent Variable Name", style = "color:black !important;"),
            choices = list(list("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#B07AA1", "#9C755F", "#BAB0AC", "#EDC948", "#FF9DA7")),
            selected = "#4E79A7",
            options = list(`show-palette-only` = TRUE, `show-buttons` = FALSE, `palette-size` = "large")
          ),
          shinyWidgets::spectrumInput(
            inputId = "vis_observed_color",
            label = "Observed Variable Color",
            choices = list(list("#A0CBE8", "#FFBE7D", "#FF9D9A", "#BAB0AC", "#D4E6B5", "#D7B5DC", "#CFAD8E", "#E5E5E5", "#F7E6A0", "#FFCCD5")),
            selected = "#A0CBE8",
            options = list(`show-palette-only` = TRUE, `show-buttons` = FALSE, `palette-size` = "large")
          ),
          shinyWidgets::spectrumInput(
            inputId = "vis_edge_color",
            label = "Edge (Arrow) Color",
            choices = list(list("#333333", "#E15759", "#4E79A7", "#59A14F", "#F28E2B", "#B07AA1", "#9C755F", "#76B7B2", "#EDC948", "#000000")),
            selected = "#333333",
            options = list(`show-palette-only` = TRUE, `show-buttons` = FALSE, `palette-size` = "large")
          ),
          shinyWidgets::radioGroupButtons(
            inputId = "vis_latent_shape",
            label = "Latent Variable Shape",
            choices = c("Ellipse" = "ellipse", "Circle" = "circle"),
            selected = "ellipse",
            justified = TRUE,
            checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
          ),
          shinyWidgets::radioGroupButtons(
            inputId = "vis_observed_shape",
            label = "Observed Variable Shape",
            choices = c("Box" = "box", "Square" = "square"),
            selected = "box",
            justified = TRUE,
            checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
          ),
          shiny::sliderInput("vis_latent_node_size", "Latent Variable Size", min = 10, max = 120, value = 20, step = 2),
          shiny::sliderInput("vis_observed_node_size", "Observed Variable Size", min = 10, max = 120, value = 15, step = 2),
          shiny::sliderInput("vis_font_size", "Font Size", min = 10, max = 32, value = 18, step = 1)
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = "panel",
          shiny::tabPanel(
            shiny::h4(id = "a", "INTRODUCTION"),
            value = 0,
            shiny::br(),
            rspInfoCard("hero")
          ),
          shiny::tabPanel(
            shiny::h4(id = "a", "DATA UPLOAD"),
            value = 1,
            shiny::textOutput("text1_2"),
            shiny::tags$head(shiny::tags$style("#text1_2{color: darkblue; font-size: 25px; font-family: cursive; font-style: oblique; text-align:center; letter-spacing:1px;}")),
            shiny::br(),
            DT::dataTableOutput("dat1_2"),
            gt::gt_output("mvn1_2"),
            shiny::br(),
            gt::gt_output("mvn4_2"),
            shiny::br()
          ),
          shiny::tabPanel(
            shiny::h4(id = "a", "STRUCTURAL MODEL"),
            value = 2,
            shiny::textOutput("text2b"),
            shiny::tags$head(shiny::tags$style("#text2b{color: darkblue; font-size: 25px; font-family: cursive; font-style: oblique; text-align:center; letter-spacing:1px;}")),
            shiny::uiOutput("model_error_b"),
            shiny::uiOutput("cfaResult_b"),
            DT::dataTableOutput("cfaDT_b"),
            shiny::uiOutput("fitResult_b"),
            DT::dataTableOutput("fit_b"),
            shiny::br(),
            shiny::uiOutput("modificationIndex_b"),
            shiny::br(),
            shiny::uiOutput("foraction_b"),
            shiny::uiOutput("foraction2_b")
          ),
          shiny::tabPanel(
            shiny::h4(id = "a", "PATH DIAGRAM"),
            value = 3,
            shiny::br(),
            shiny::uiOutput("visLegendTop"),
            shiny::tags$div(
              class = "path-toolbar",
              style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center; margin-bottom:8px;",
              shiny::tags$div(
                style = "display:flex; gap:8px; align-items:center;",
                shiny::actionButton("vis_download_pdf", "Export PDF", icon = shiny::icon("file-pdf"), class = "btn btn-primary btn-sm"),
                shiny::actionButton("vis_download_jpeg", "Export JPEG", icon = shiny::icon("image"), class = "btn btn-success btn-sm")
              ),
              shiny::tags$div(style = "flex:1 1 auto; min-width:40px;"),
              shiny::actionButton(
                "show_ave_cr",
                "AVE / CR / OMEGA",
                icon = shiny::icon("table"),
                class = "btn btn-primary btn-sm"
              ),
              shiny::actionButton(
                "toggle_vis_edge_stats",
                "Show Z Values",
                icon = shiny::icon("calculator"),
                class = "btn btn-info btn-sm"
              )
            ),
            shiny::uiOutput("visEdgeValueModeText"),
            shiny::tags$div(
              id = "path_capture_container",
              class = "path-diagram-wrap",
              visNetwork::visNetworkOutput("visPathDiagram", height = "540px")
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    `%||%` <- function(x, y) if (is.null(x)) y else x

    shorten_label <- function(x, max_chars = 18) {
      x <- as.character(x)
      vapply(x, function(one) {
        if (is.na(one) || !nzchar(one)) {
          return(one)
        }
        if (nchar(one) <= max_chars) {
          return(one)
        }
        paste0(substr(one, 1, max_chars - 3), "...")
      }, character(1), USE.NAMES = FALSE)
    }

    make_node_style <- function(size_value) {
      size_value <- as.numeric(size_value)
      list(
        size = size_value,
        widthConstraint = list(
          minimum = round(size_value * 2.2),
          maximum = round(size_value * 2.2)
        ),
        heightConstraint = list(
          minimum = round(size_value * 1.2)
        ),
        margin = round(size_value * 0.22)
      )
    }

    build_cfa_visnetwork <- function(cfaModel,
                                     node_positions = NULL,
                                     latent_color = "#4E79A7",
                                     observed_color = "#A0CBE8",
                                     edge_color = "#333333",
                                     latent_shape = "ellipse",
                                     observed_shape = "box",
                                     latent_node_size = 60,
                                     observed_node_size = 52,
                                     font_size = 18,
                                     edge_value_type = "std") {
      pe <- lavaan::parameterestimates(cfaModel, standardized = TRUE)
      edge_value_type <- if (identical(edge_value_type, "z")) "z" else "std"
      edge_label_values <- function(df) {
        if (is.null(df) || nrow(df) == 0) {
          return(character(0))
        }
        vals <- if (edge_value_type == "z" && "z" %in% names(df)) df$z else df$std.all
        vals <- suppressWarnings(as.numeric(vals))
        vals[!is.finite(vals)] <- NA_real_
        ifelse(is.na(vals), "", sprintf("%.2f", vals))
      }
      loadings <- pe[pe$op == "=~", c("lhs", "rhs", "std.all", "z")]
      latent_vars <- unique(loadings$lhs)
      observed_vars <- unique(loadings$rhs)

      latent_regs_all <- pe[
        pe$op == "~" & pe$lhs %in% latent_vars & pe$rhs %in% latent_vars,
        c("lhs", "rhs", "std.all", "z")
      ]
      endogenous_latent_vars <- unique(latent_regs_all$lhs)
      exogenous_latent_vars <- setdiff(latent_vars, endogenous_latent_vars)

      n_lat <- length(latent_vars)
      latent_x <- seq(from = -(n_lat - 1) * 250, to = (n_lat - 1) * 250, length.out = n_lat)

      latent_style <- make_node_style(latent_node_size)
      latent_nodes <- data.frame(
        id = latent_vars,
        label = shorten_label(latent_vars, max_chars = 16),
        title = latent_vars,
        group = "latent",
        x = latent_x,
        y = 0,
        exogenous = latent_vars %in% exogenous_latent_vars,
        size = latent_style$size,
        stringsAsFactors = FALSE
      )
      latent_nodes$widthConstraint <- I(rep(list(latent_style$widthConstraint), nrow(latent_nodes)))
      latent_nodes$heightConstraint <- I(rep(list(latent_style$heightConstraint), nrow(latent_nodes)))
      latent_nodes$margin <- I(rep(list(latent_style$margin), nrow(latent_nodes)))

      observed_nodes_list <- lapply(seq_along(latent_vars), function(i) {
        lv <- latent_vars[i]
        items <- loadings$rhs[loadings$lhs == lv]
        k <- length(items)
        if (k == 1) {
          item_x <- latent_x[i]
        } else {
          spread <- max(120, min(180, 600 / k))
          item_x <- seq(from = latent_x[i] - ((k - 1) / 2) * spread, to = latent_x[i] + ((k - 1) / 2) * spread, length.out = k)
        }
        data.frame(
          id = items,
          label = shorten_label(items, max_chars = 16),
          title = items,
          group = "observed",
          x = item_x,
          y = 220,
          stringsAsFactors = FALSE
        )
      })

      observed_nodes <- do.call(rbind, observed_nodes_list)
      observed_nodes <- observed_nodes[!duplicated(observed_nodes$id), ]
      observed_nodes$exogenous <- FALSE
      observed_style <- make_node_style(observed_node_size)
      observed_nodes$size <- observed_style$size
      observed_nodes$widthConstraint <- I(rep(list(observed_style$widthConstraint), nrow(observed_nodes)))
      observed_nodes$heightConstraint <- I(rep(list(observed_style$heightConstraint), nrow(observed_nodes)))
      observed_nodes$margin <- I(rep(list(observed_style$margin), nrow(observed_nodes)))
      nodes <- rbind(latent_nodes, observed_nodes)

      nodes$shapeProperties <- I(lapply(nodes$exogenous, function(z) list(borderDashes = isTRUE(z))))
      nodes$borderWidth <- ifelse(nodes$exogenous, 3, 1.5)

      if (!is.null(node_positions) && length(node_positions) > 0) {
        pos_names <- intersect(names(node_positions), nodes$id)
        if (length(pos_names) > 0) {
          for (nm in pos_names) {
            pos <- node_positions[[nm]]
            if (!is.null(pos$x) && !is.null(pos$y)) {
              nodes[nodes$id == nm, c("x", "y")] <- c(pos$x, pos$y)
            }
          }
        }
      }

      edges_load <- data.frame(
        from = loadings$lhs,
        to = loadings$rhs,
        label = edge_label_values(loadings),
        arrows = "to",
        smooth = FALSE,
        dashes = FALSE,
        stringsAsFactors = FALSE
      )

      latent_regs <- latent_regs_all

      edges_struct <- NULL
      if (nrow(latent_regs) > 0) {
        edges_struct <- data.frame(
          from = latent_regs$rhs,
          to = latent_regs$lhs,
          label = edge_label_values(latent_regs),
          arrows = "to",
          smooth = FALSE,
          dashes = FALSE,
          stringsAsFactors = FALSE
        )
      }

      latent_covs <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% latent_vars & pe$rhs %in% latent_vars, c("lhs", "rhs", "std.all", "z")]
      observed_covs <- pe[pe$op == "~~" & pe$lhs != pe$rhs & pe$lhs %in% observed_vars & pe$rhs %in% observed_vars, c("lhs", "rhs", "std.all", "z")]

      make_cov_edges <- function(cov_df, smooth_type = "curvedCW", roundness = 0.18) {
        if (nrow(cov_df) == 0) {
          return(NULL)
        }
        key <- apply(cov_df[, c("lhs", "rhs")], 1, function(z) paste(sort(z), collapse = "___"))
        cov_df <- cov_df[!duplicated(key), , drop = FALSE]
        data.frame(
          from = cov_df$lhs,
          to = cov_df$rhs,
          label = edge_label_values(cov_df),
          arrows = "to;from",
          smooth = I(rep(list(list(enabled = TRUE, type = smooth_type, roundness = roundness)), nrow(cov_df))),
          dashes = TRUE,
          stringsAsFactors = FALSE
        )
      }

      edges <- edges_load
      if (!is.null(edges_struct)) edges <- rbind(edges, edges_struct)
      ec1 <- make_cov_edges(latent_covs, "curvedCW", 0.15)
      ec2 <- make_cov_edges(observed_covs, "curvedCCW", 0.25)
      if (!is.null(ec1)) edges <- rbind(edges, ec1)
      if (!is.null(ec2)) edges <- rbind(edges, ec2)

      visNetwork::visNetwork(nodes, edges, width = "100%", height = "760px") %>%
        visNetwork::visNodes(
          shadow = FALSE,
          font = list(size = font_size, face = "Arial", color = "#111111", strokeWidth = 0)
        ) %>%
        visNetwork::visGroups(
          groupname = "latent",
          shape = latent_shape,
          font = list(size = font_size, face = "Arial", color = "#111111"),
          color = list(background = latent_color, border = "#2C3E50", highlight = list(background = latent_color, border = "#000000"))
        ) %>%
        visNetwork::visGroups(
          groupname = "observed",
          shape = observed_shape,
          font = list(size = font_size, face = "Arial", color = "#111111"),
          color = list(background = observed_color, border = "#2C3E50", highlight = list(background = observed_color, border = "#000000"))
        ) %>%
        visNetwork::visEdges(
          color = list(color = edge_color, highlight = "#D62728"),
          font = list(size = max(10, font_size - 2), align = "middle", strokeWidth = 0),
          arrows = list(to = list(enabled = TRUE, scaleFactor = 0.8), from = list(enabled = TRUE, scaleFactor = 0.8)),
          shadow = FALSE
        ) %>%
        visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = FALSE, navigationButtons = FALSE) %>%
        visNetwork::visPhysics(enabled = FALSE) %>%
        visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var network = this;
            window.cfaVisStore = window.cfaVisStore || {};
            window.cfaVisPersistentPositions = window.cfaVisPersistentPositions || {};
            window.cfaVisStore[el.id] = network;

            function clonePositions(pos) {
              var out = {};
              Object.keys(pos || {}).forEach(function(k) {
                if (pos[k] && isFinite(pos[k].x) && isFinite(pos[k].y)) {
                  out[k] = {x: pos[k].x, y: pos[k].y};
                }
              });
              return out;
            }

            function pushPositions() {
              var pos = clonePositions(network.getPositions());
              window.cfaVisPersistentPositions[el.id] = pos;
              window.cfaVisPersistentPositions['visPathDiagram'] = pos;
              if (typeof Shiny !== 'undefined') {
                Shiny.setInputValue(el.id + '_positions', pos, {priority: 'event'});
                Shiny.setInputValue('visPathDiagram_positions', pos, {priority: 'event'});
              }
            }

            function restorePositions() {
              var pos = window.cfaVisPersistentPositions[el.id] || window.cfaVisPersistentPositions['visPathDiagram'];
              var restored = false;
              if (pos) {
                Object.keys(pos).forEach(function(id) {
                  if (network.body && network.body.nodes && network.body.nodes[id] &&
                      pos[id] && isFinite(pos[id].x) && isFinite(pos[id].y)) {
                    network.moveNode(id, pos[id].x, pos[id].y);
                    restored = true;
                  }
                });
              }
              return restored;
            }

            network.once('afterDrawing', function() {
              setTimeout(function() {
                var restored = restorePositions();
                if (!restored) network.fit({animation: false});
                pushPositions();
              }, 120);
            });

            network.on('dragEnd', function() { setTimeout(pushPositions, 20); });
            network.on('stabilized', function() { restorePositions(); pushPositions(); });
          }
        ")
    }

    draw_semplot_diagram <- function(cfa_obj) {
      shiny::req(cfa_obj)
      pe <- lavaan::parameterEstimates(cfa_obj, standardized = TRUE)
      loadings <- pe[pe$op == "=~", c("lhs", "rhs")]
      observed_vars <- unique(loadings$rhs)
      nvar <- length(observed_vars)

      size_man <- if (nvar <= 8) 8 else if (nvar <= 14) 6.5 else 5.5
      size_lat <- if (nvar <= 8) 10 else if (nvar <= 14) 8.5 else 7
      edge_cex <- if (nvar <= 8) 1 else if (nvar <= 14) 0.85 else 0.7

      current_layout <- input$tree_b %||% "tree2"
      edge_col <- switch(as.character(input$colour_b %||% 1),
        "1" = "black",
        "2" = "red3",
        "3" = "forestgreen",
        "4" = "royalblue3",
        "black"
      )

      semPlot::semPaths(
        object = cfa_obj,
        what = "std",
        whatLabels = "std",
        layout = current_layout,
        style = "lisrel",
        rotation = 1,
        residuals = FALSE,
        intercepts = FALSE,
        exoCov = TRUE,
        covAtResiduals = FALSE,
        curveAdjacent = TRUE,
        curvePivot = TRUE,
        optimizeLatRes = TRUE,
        sizeMan = size_man,
        sizeLat = size_lat,
        shapeMan = "rectangle",
        shapeLat = "ellipse",
        nCharNodes = 0,
        nCharEdges = 0,
        edge.color = edge_col,
        color = list(lat = "white", man = "white"),
        border.color = "black",
        label.color = "black",
        residualColor = "gray40",
        edge.label.cex = edge_cex,
        fade = FALSE,
        pastel = FALSE,
        mar = c(6, 6, 6, 6)
      )
    }

    current_vis_positions <- shiny::reactiveVal(NULL)
    sm2_mod_text <- shiny::reactiveVal("")
    sm2_struct_text <- shiny::reactiveVal("")

    path_edge_value_type <- shiny::reactive({
      clicks <- input$toggle_vis_edge_stats %||% 0
      if ((clicks %% 2) == 1) "z" else "std"
    })

    output$visEdgeValueModeText <- shiny::renderUI({
      mode_now <- if (((input$toggle_vis_edge_stats %||% 0) %% 2) == 1) {
        "Showing lavaan z values on arrows"
      } else {
        "Showing standardized coefficients on arrows"
      }
      shiny::tags$div(
        style = "margin-bottom:8px; color:#0b5394; font-weight:600;",
        shiny::icon("info-circle"),
        mode_now
      )
    })

    shiny::observeEvent(input$toggle_model2, {
      shinyjs::toggle("model_section2", anim = TRUE, time = 0.2)
    })

    shiny::observeEvent(input$toggle_manual_model2, {
      shinyjs::toggle("manual_model_section2", anim = TRUE, time = 0.2)
    })

    shiny::observeEvent(input$toggle_struct2, {
      shinyjs::toggle("struct_section2", anim = TRUE, time = 0.2)
    })

    shiny::observeEvent(input$toggle_mod2, {
      shinyjs::toggle("mod_section2", anim = TRUE, time = 0.2)
    })

    shiny::observeEvent(input$sm2_struct_text,
      {
        sm2_struct_text(input$sm2_struct_text %||% "")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$sm2_mod_text,
      {
        sm2_mod_text(input$sm2_mod_text %||% "")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$visPathDiagram_positions,
      {
        current_vis_positions(input$visPathDiagram_positions)
      },
      ignoreInit = FALSE
    )

    shiny::observeEvent(input$myColor2, {
      bbb <- input$myColor2

      # Arka plan rengini gncelle
      shinyWidgets::setBackgroundColor(
        color     = c("#FDFBFF", bbb),
        gradient  = "linear",
        direction = c("bottom", "right")
      )

      # CSS deikenlerini ve dinamik tema stilini JS ile gncelle
      shinyjs::runjs(sprintf('
        var c = "%s";
        var root = document.documentElement;
        root.style.setProperty("--mor-deep",   c);
        root.style.setProperty("--mor-mid",    c);
        root.style.setProperty("--mor-light",  c);
        root.style.setProperty("--mor-accent", c);
        root.style.setProperty("--rsp-theme", c);

        // Inline style ile sabitlenmi balk span renklerini dorudan deitir
        var titleEl = document.getElementById("title");
        if (titleEl) {
          var spans = titleEl.querySelectorAll("span");
          spans.forEach(function(s) { s.style.color = c; });
        }

        var st = document.getElementById("dyn-theme");
        if (!st) {
          st = document.createElement("style");
          st.id = "dyn-theme";
          document.head.appendChild(st);
        }
        st.textContent =
          ".nav-tabs > li.active > a, " +
          ".nav-tabs > li.active > a:focus, " +
          ".nav-tabs > li.active > a:hover { background: " + c + " !important; color: #fff !important; border-color: " + c + " !important; }" +
          ".nav-tabs > li > a:hover { background: " + c + "33 !important; }" +
          ".nav-tabs > li > a h4#a { color: " + c + " !important; }" +
          ".nav-tabs > li.active > a h4#a { color: #ffffff !important; }" +
          "#title { color: " + c + " !important; }" +
          ".well { border-color: " + c + "66 !important; box-shadow: 0 4px 20px " + c + "22 !important; }" +
          "table.dataTable thead th { background: " + c + " !important; }" +
          ".gt_col_heading { background: " + c + " !important; }" +
          "#tepe { border-bottom-color: " + c + " !important; }" +
          ".btn-dl-pdf { background: " + c + " !important; }" +
          ".tab-content { border-color: " + c + "44 !important; }" +
          ".btn-primary { background-color: " + c + " !important; border-color: " + c + " !important; }" +
          ".btn-primary:hover { background-color: " + c + "cc !important; border-color: " + c + "cc !important; }" +
          ".btn-info { background-color: " + c + " !important; border-color: " + c + " !important; }" +
          ".btn-info:hover { background-color: " + c + "cc !important; border-color: " + c + "cc !important; }" +
          ".btn-success { background-color: " + c + " !important; border-color: " + c + " !important; }" +
          ".btn-success:hover { background-color: " + c + "cc !important; border-color: " + c + "cc !important; }" +
          ".btn-warning { background-color: " + c + " !important; border-color: " + c + " !important; }" +
          ".btn-warning:hover { background-color: " + c + "cc !important; border-color: " + c + "cc !important; }" +
          ".btn-default.sm2-toggle-btn { background-color: " + c + " !important; border-color: " + c + " !important; color: #fff !important; }" +
          ".bttn, .bttn *, #dwn, .btn { color: #ffffff !important; }" +
          ".btn.dropdown-toggle.btn-default, .btn.dropdown-toggle.btn-default *, .bootstrap-select > .dropdown-toggle, .bootstrap-select > .dropdown-toggle * { color: #000000 !important; }" +
          ".radio-group-buttons .btn, .radio-group-buttons .btn *, .btn-group-container-sw .btn, .btn-group-container-sw .btn *, .radioGroupButtons .btn, .radioGroupButtons .btn * { color: #111111 !important; }" +
          ".selectize-input, .selectize-input *, .selectize-dropdown, .selectize-dropdown * { color: #111111 !important; }" +
          ".dropdown-menu > li > a, .dropdown-menu > li > a *, .bootstrap-select .dropdown-menu li a, .bootstrap-select .dropdown-menu li a *, .bootstrap-select .dropdown-menu li a span.text, .bootstrap-select .dropdown-menu .text, .bootstrap-select .dropdown-menu .glyphicon { color: #111111 !important; }" +
          ".bttn-jelly.bttn-primary { background: " + c + " !important; color:#fff !important; }" +
          ".bttn-jelly.bttn-primary:hover { background: " + c + "cc !important; color:#fff !important; }" +
          ".bttn-unite.bttn-primary { background: " + c + " !important; color:#fff !important; }" +
          ".bttn-unite.bttn-primary:hover { background: " + c + "cc !important; color:#fff !important; }" +
          ".bttn-gradient { background: " + c + " !important; color:#fff !important; }" +
          ".irs-bar, .irs-bar-edge, .irs-single { background: " + c + " !important; border-color: " + c + " !important; }" +
          ".irs-from, .irs-to, .irs-single { color: #ffffff !important; }" +
          ".irs-slider, .irs-handle, .irs-handle > i:first-child { border-color: " + c + " !important; }";
      ', bbb))
    })

    output$browser2 <- shiny::renderText({
      shiny::req(input$myBrowser)
      if (input$myBrowser == "Chrome 102") "Please click 'Open in Browser' for a better experience" else NULL
    })

    output$uiHeader2 <- shiny::renderUI({
      if (input$type2 == 3) NULL else shinyWidgets::materialSwitch("header", shiny::h4("The first line is the variable name"), value = TRUE, status = "primary")
    })

    data <- shiny::reactive({
      veri <- input$data1
      if (is.null(veri)) {
        return(NULL)
      } else if (input$type2 == 1) {
        if (tools::file_ext(veri$datapath) != "csv") data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT") else utils::read.csv2(veri$datapath, header = isTRUE(input$header), sep = ";")
      } else if (input$type2 == 2) {
        if (tools::file_ext(veri$datapath) != "csv") data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT") else utils::read.csv2(veri$datapath, header = isTRUE(input$header), sep = ",")
      } else if (input$type2 == 3) {
        if (tools::file_ext(veri$datapath) != "sav") data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT") else as.data.frame(foreign::read.spss(veri$datapath, to.data.frame = TRUE, use.value.labels = FALSE), stringsAsFactors = FALSE)
      } else if (input$type2 == 4) {
        if (tools::file_ext(veri$datapath) != "xlsx") data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT") else xlsx::read.xlsx(veri$datapath, 1, header = isTRUE(input$header))
      }
    })

    available_vars <- shiny::reactive({
      d <- data()
      shiny::req(is.data.frame(d))
      names(d)
    })

    latent_names_current <- shiny::reactive({
      shiny::req(input$data1)
      vapply(seq_len(input$fak_sm2), function(i) {
        nm <- input[[paste0("latent_name_", i)]]
        if (is.null(nm) || !nzchar(trimws(nm))) paste0("F", i) else trimws(nm)
      }, character(1))
    })

    output$sm2_builder_ui <- shiny::renderUI({
      shiny::req(input$data1)
      vars <- available_vars()
      fac_n <- input$fak_sm2
      shiny::tagList(
        lapply(seq_len(fac_n), function(i) {
          shiny::wellPanel(
            shiny::textInput(paste0("latent_name_", i), paste0("Latent Variable ", i, " Name"), value = paste0("F", i)),
            shinyWidgets::pickerInput(
              inputId = paste0("factor_items_", i),
              label = paste0("Observed Variables for Factor ", i),
              choices = vars,
              selected = NULL,
              multiple = TRUE,
              options = shinyWidgets::pickerOptions(`actions-box` = TRUE, liveSearch = TRUE)
            )
          )
        })
      )
    })

    output$sm2_struct_controls_ui <- shiny::renderUI({
      shiny::req(input$data1)
      latent_choices <- latent_names_current()
      shiny::tagList(
        shiny::h4(id = "ab", "Latent Variable Relations"),
        shiny::textAreaInput("sm2_struct_text", "Structural Relations", value = sm2_struct_text(), height = "120px"),
        shiny::fluidRow(
          shiny::column(5, shinyWidgets::pickerInput("struct_first_2", "First Latent Variable", choices = latent_choices, multiple = FALSE, options = shinyWidgets::pickerOptions(liveSearch = TRUE))),
          shiny::column(2, shiny::selectInput("struct_op_2", "Operator", choices = c("~", "~~"), selected = "~")),
          shiny::column(5, shinyWidgets::pickerInput("struct_second_2", "Second Latent Variable", choices = latent_choices, multiple = FALSE, options = shinyWidgets::pickerOptions(liveSearch = TRUE)))
        ),
        shiny::actionButton("add_selected_struct2", "ADD LATENT RELATION", class = "btn btn-success")
      )
    })

    output$sm2_mod_controls_ui <- shiny::renderUI({
      shiny::req(input$data1)
      obs_vars <- sort(unique(unlist(factor_item_map(), use.names = FALSE)))
      shiny::tagList(
        shiny::h4(id = "ab", "Observed Variable Modifications (Manual)"),
        shiny::textAreaInput("sm2_mod_text", "Modification Relations", value = sm2_mod_text(), height = "120px"),
        shiny::fluidRow(
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              "mod_first_2",
              "First Observed Variable",
              choices = obs_vars,
              multiple = FALSE,
              options = shinyWidgets::pickerOptions(liveSearch = TRUE)
            )
          ),
          shiny::column(
            6,
            shinyWidgets::pickerInput(
              "mod_second_2",
              "Second Observed Variable",
              choices = obs_vars,
              multiple = FALSE,
              options = shinyWidgets::pickerOptions(liveSearch = TRUE)
            )
          )
        ),
        shiny::actionButton("add_selected_mod2", "ADD SELECTED MODIFICATION", class = "btn btn-info")
      )
    })

    factor_item_map <- shiny::reactive({
      shiny::req(input$data1)
      stats::setNames(
        lapply(seq_len(input$fak_sm2), function(i) {
          items <- input[[paste0("factor_items_", i)]]
          unique(items[!is.na(items) & nzchar(items)])
        }),
        latent_names_current()
      )
    })

    same_factor_mod_choices <- shiny::reactive({
      fmap <- factor_item_map()
      out <- character(0)
      for (items in fmap) {
        items <- unique(items)
        if (length(items) >= 2) {
          prs <- utils::combn(items, 2, simplify = FALSE)
          out <- c(out, vapply(prs, function(z) paste(z[1], "~~", z[2]), character(1)))
        }
      }
      unique(out)
    })

    valid_selected_mod_lines <- shiny::reactive({
      current <- trimws(sm2_mod_text() %||% input$sm2_mod_text %||% "")
      if (!nzchar(current)) {
        return(character(0))
      }
      lines <- trimws(unlist(strsplit(current, "\n")))
      unique(lines[nzchar(lines)])
    })

    model_syntax_base <- shiny::reactive({
      shiny::req(input$data1)
      parts <- c()
      for (i in seq_len(input$fak_sm2)) {
        items <- input[[paste0("factor_items_", i)]]
        if (!is.null(items) && length(items) > 0) {
          nm <- input[[paste0("latent_name_", i)]]
          if (is.null(nm) || !nzchar(trimws(nm))) nm <- paste0("F", i)
          parts <- c(parts, paste0(trimws(nm), " =~ ", paste(items, collapse = " + ")))
        }
      }
      shiny::req(length(parts) > 0)
      paste(parts, collapse = "\n")
    })

    full_model_syntax <- shiny::reactive({
      manual_text <- trimws(input$manual_model_text %||% "")
      if (nzchar(manual_text)) {
        return(manual_text)
      }

      base <- model_syntax_base()
      struct_text <- trimws(sm2_struct_text() %||% input$sm2_struct_text %||% "")
      mod_lines <- valid_selected_mod_lines()
      pieces <- c(base)
      if (nzchar(struct_text)) pieces <- c(pieces, struct_text)
      if (length(mod_lines) > 0) pieces <- c(pieces, mod_lines)
      paste(pieces, collapse = "\n")
    })

    model_mode_current <- shiny::reactive({
      manual_text <- trimws(input$manual_model_text %||% "")
      if (nzchar(manual_text)) "manual" else "click"
    })

    ordered_vars_current <- shiny::reactive({
      d <- data()
      shiny::req(is.data.frame(d))
      names(d)
    })

    fit_current_model <- function(model_syntax, d, estimator, ordered_vars = NULL, model_mode = "click") {
      # if (identical(estimator, "BAYESIAN")) {
      #   fit_args <- list(
      #     model = model_syntax,
      #     data = d,
      #     target = "stan",
      #     burnin = 500,
      #     sample = 1000,
      #     n.chains = 3,
      #     save.lvs = TRUE,
      #     seed = 1234
      #   )
      #   if (!is.null(ordered_vars) && length(ordered_vars) > 0) {
      #     fit_args$ordered <- ordered_vars
      #   }
      #   return(do.call(blavaan::bcfa, fit_args))
      # }

      fit_args <- list(
        model = model_syntax,
        data = d,
        estimator = estimator
      )
      if (!is.null(ordered_vars) && length(ordered_vars) > 0 &&
        estimator %in% c("WLSMV", "DWLS", "WLS", "ULS")) {
        fit_args$ordered <- ordered_vars
      }
      fit_fun <- if (identical(model_mode, "manual")) lavaan::sem else lavaan::cfa
      do.call(fit_fun, fit_args)
    }

    safe_modindices <- function(fit_object) {
      out <- tryCatch(
        lavaan::modificationindices(fit_object, sort. = TRUE),
        error = function(e) NULL
      )
      if (is.null(out)) {
        return(data.frame(
          lhs = character(0), op = character(0), rhs = character(0),
          mi = numeric(0), stringsAsFactors = FALSE
        ))
      }
      out
    }

    safe_fit_table <- function(fit_object, estimator_name) {
      # if (identical(estimator_name, "BAYESIAN")) {
      #   fm <- tryCatch(lavaan::fitMeasures(fit_object), error = function(e) NULL)
      #   bfi <- tryCatch(blavaan::blavFitIndices(fit_object), error = function(e) NULL)
      #   bfi_sum <- tryCatch(summary(bfi, prob = 0.90), error = function(e) NULL)
      #
      #   get_fm <- function(x) {
      #     if (!is.null(fm) && x %in% names(fm)) unname(fm[[x]]) else NA_real_
      #   }
      #   get_bfi <- function(x) {
      #     if (is.null(bfi_sum)) return(NA_real_)
      #     rn <- rownames(bfi_sum)
      #     if (is.null(rn) || !(x %in% rn)) return(NA_real_)
      #     cn <- colnames(bfi_sum)
      #     pref <- intersect(c("median", "mean", "mode", "50%"), cn)
      #     if (length(pref) == 0) {
      #       val <- suppressWarnings(as.numeric(bfi_sum[x, 1]))
      #     } else {
      #       val <- suppressWarnings(as.numeric(bfi_sum[x, pref[1]]))
      #     }
      #     if (length(val) == 0) NA_real_ else val
      #   }
      #
      #   vals <- c(
      #     PPP = get_fm("ppp"),
      #     DIC = get_fm("dic"),
      #     WAIC = get_fm("waic"),
      #     LOOIC = get_fm("looic"),
      #     BRMSEA = get_bfi("BRMSEA"),
      #     BCFI = get_bfi("BCFI"),
      #     BTLI = get_bfi("BTLI"),
      #     SRMR = get_fm("srmr")
      #   )
      #   fitSumDT <- as.data.frame(t(round(vals, 3)))
      #   rownames(fitSumDT) <- "Value"
      #   return(fitSumDT)
      # }

      if (identical(estimator_name, "MLR")) {
        indexFit <- lavaan::fitMeasures(fit_object)
        chisq_val <- indexFit[["chisq"]]
        df_val <- indexFit[["df"]]
        chisq_df_ratio <- if (!is.null(df_val) && !is.na(df_val) && df_val > 0) round(chisq_val / df_val, 3) else NA_real_
        vals <- c(
          indexFit[["chisq"]], indexFit[["df"]], indexFit[["pvalue"]],
          chisq_df_ratio,
          indexFit[["rmsea.robust"]], indexFit[["cfi.robust"]],
          indexFit[["agfi"]], indexFit[["tli.robust"]], indexFit[["srmr"]]
        )
        nms <- c(
          "Chi-square", "df", "p", "Chi-sq/df",
          "RMSEA (Robust)",
          "CFI (Robust)", "AGFI", "NNFI (TLI) (Robust)", "SRMR"
        )
        fitSumDT <- as.data.frame(t(round(vals, 3)))
        colnames(fitSumDT) <- nms
        rownames(fitSumDT) <- "Value"
        return(fitSumDT)
      }

      indexFit <- lavaan::fitMeasures(fit_object)
      rmsea_name <- if ("rmsea.scaled" %in% names(indexFit) && estimator_name == "WLSMV") "rmsea.scaled" else "rmsea"
      cfi_name <- if ("cfi.scaled" %in% names(indexFit) && estimator_name == "WLSMV") "cfi.scaled" else "cfi"
      tli_name <- if ("tli.scaled" %in% names(indexFit) && estimator_name == "WLSMV") "tli.scaled" else if ("nnfi" %in% names(indexFit)) "nnfi" else "tli"
      tli_label <- if (estimator_name == "WLSMV" && "tli.scaled" %in% names(indexFit)) "NNFI(TLI) (Scaled)" else "NNFI(TLI)"
      rmsea_label <- if (estimator_name == "WLSMV" && "rmsea.scaled" %in% names(indexFit)) "RMSEA (Scaled)" else "RMSEA"
      cfi_label <- if (estimator_name == "WLSMV" && "cfi.scaled" %in% names(indexFit)) "CFI (Scaled)" else "CFI"

      vals <- c(
        indexFit[["chisq"]], indexFit[["df"]], indexFit[["pvalue"]],
        round(indexFit[["chisq"]] / max(indexFit[["df"]], 1), 3),
        indexFit[[rmsea_name]], indexFit[[cfi_name]],
        indexFit[["agfi"]], indexFit[[tli_name]], indexFit[["srmr"]]
      )
      nms <- c("Chi-square", "df", "p", "Chi-sq/df", rmsea_label, cfi_label, "AGFI", tli_label, "SRMR")
      fitSumDT <- as.data.frame(t(round(vals, 3)))
      colnames(fitSumDT) <- nms
      rownames(fitSumDT) <- "Value"
      fitSumDT
    }


    model_fit_result <- shiny::eventReactive(input$send2,
      {
        d <- data()
        shiny::req(is.data.frame(d))
        syntax_now <- full_model_syntax()
        tryCatch(
          list(
            fit = fit_current_model(
              model_syntax = syntax_now,
              d = d,
              estimator = input$type3_2,
              ordered_vars = ordered_vars_current(),
              model_mode = model_mode_current()
            ),
            error = NULL,
            syntax = syntax_now
          ),
          error = function(e) {
            list(fit = NULL, error = conditionMessage(e), syntax = syntax_now)
          }
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    Cfa2_model <- shiny::reactive({
      res <- model_fit_result()
      shiny::req(!is.null(res$fit))
      res$fit
    })

    ave_cr_values <- shiny::reactive({
      fit_object <- Cfa2_model()
      shiny::req(fit_object)

      std <- lavaan::standardizedSolution(fit_object)
      loadings <- std[
        std$op == "=~" &
          !is.na(std$est.std) &
          is.finite(std$est.std),
        c("lhs", "rhs", "est.std"),
        drop = FALSE
      ]

      shiny::validate(
        shiny::need(nrow(loadings) > 0, "No standardized factor loadings are available.")
      )

      factors <- unique(loadings$lhs)

      out <- lapply(factors, function(factor_name) {
        one <- loadings[loadings$lhs == factor_name, , drop = FALSE]
        lambda <- as.numeric(one$est.std)

        # For the completely standardized solution:
        # item error variance = 1 - standardized loading^2.
        theta <- pmax(0, 1 - lambda^2)

        ave <- sum(lambda^2, na.rm = TRUE) /
          (sum(lambda^2, na.rm = TRUE) + sum(theta, na.rm = TRUE))

        cr <- sum(lambda, na.rm = TRUE)^2 /
          (sum(lambda, na.rm = TRUE)^2 + sum(theta, na.rm = TRUE))

        data.frame(
          Factor = factor_name,
          AVE = ave,
          CR = cr,
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, out)
    })


    shiny::observeEvent(input$add_selected_struct2, {
      shiny::req(input$struct_first_2, input$struct_op_2, input$struct_second_2)
      shiny::req(input$struct_first_2 != input$struct_second_2)
      new_line <- paste(input$struct_first_2, input$struct_op_2, input$struct_second_2)
      current <- trimws(input$sm2_struct_text %||% "")
      updated <- if (nzchar(current)) paste(current, new_line, sep = "\n") else new_line
      sm2_struct_text(updated)
      shiny::updateTextAreaInput(session, "sm2_struct_text", value = updated)
    })
    shiny::observeEvent(input$add_selected_mod2, {
      shiny::req(input$mod_first_2, input$mod_second_2)
      shiny::req(input$mod_first_2 != input$mod_second_2)
      new_line <- paste(input$mod_first_2, "~~", input$mod_second_2)
      current <- trimws(sm2_mod_text() %||% input$sm2_mod_text %||% "")
      current_lines <- if (nzchar(current)) trimws(unlist(strsplit(current, "\n"))) else character(0)
      current_lines <- unique(current_lines[nzchar(current_lines)])
      updated_text <- paste(unique(c(current_lines, new_line)), collapse = "\n")
      sm2_mod_text(updated_text)
      shiny::updateTextAreaInput(session, "sm2_mod_text", value = updated_text)
    })

    shiny::observeEvent(input$send2, {
      current_vis_positions(NULL)
    })


    omega_total_value <- shiny::reactive({
      fit_object <- Cfa2_model()
      shiny::req(fit_object)

      std_mats <- lavaan::lavInspect(fit_object, "std")

      lambda <- std_mats$lambda
      theta <- std_mats$theta
      psi <- std_mats$psi

      shiny::validate(
        shiny::need(!is.null(lambda) && nrow(lambda) > 0, "Standardized loading matrix is unavailable."),
        shiny::need(!is.null(theta) && nrow(theta) > 0, "Standardized residual covariance matrix is unavailable."),
        shiny::need(!is.null(psi) && nrow(psi) > 0, "Standardized latent covariance matrix is unavailable.")
      )

      # Reliability of the unit-weighted total score:
      # true-score variance = 1' Lambda Psi Lambda' 1
      # error variance      = 1' Theta 1
      item_weights <- rep(1, nrow(lambda))

      true_variance <- as.numeric(
        t(item_weights) %*% lambda %*% psi %*% t(lambda) %*% item_weights
      )

      error_variance <- as.numeric(
        t(item_weights) %*% theta %*% item_weights
      )

      total_variance <- true_variance + error_variance

      if (!is.finite(total_variance) || total_variance <= 0) {
        return(NA_real_)
      }

      true_variance / total_variance
    })

    shiny::observeEvent(input$show_ave_cr, {
      shiny::req(input$send2 > 0)

      vals <- ave_cr_values()

      modal_table <- shiny::tags$table(
        class = "table table-striped table-hover",
        style = "width:100%; margin-bottom:8px;",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Factor"),
            shiny::tags$th(style = "text-align:center;", "AVE"),
            shiny::tags$th(style = "text-align:center;", "Composite Reliability")
          )
        ),
        shiny::tags$tbody(
          lapply(seq_len(nrow(vals)), function(i) {
            shiny::tags$tr(
              shiny::tags$td(style = "font-weight:700;", vals$Factor[i]),
              shiny::tags$td(
                style = "text-align:center; font-variant-numeric:tabular-nums;",
                sprintf("%.3f", vals$AVE[i])
              ),
              shiny::tags$td(
                style = "text-align:center; font-variant-numeric:tabular-nums;",
                sprintf("%.3f", vals$CR[i])
              )
            )
          })
        )
      )

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tagList(
            shiny::icon("chart-bar"),
            "AVE and Composite Reliability"
          ),
          modal_table,
          shiny::tags$div(
            style = paste(
              "margin-top:12px;",
              "padding:10px 12px;",
              "border:1px solid rgba(21,101,192,.22);",
              "border-radius:9px;",
              "background:#f8fbff;",
              "display:flex;",
              "justify-content:space-between;",
              "align-items:center;"
            ),
            shiny::tags$span(
              style = "font-weight:800; color:#1e293b;",
              "Model-based Omega Total"
            ),
            shiny::tags$span(
              style = "font-size:18px; font-weight:900; color:var(--rsp-theme); font-variant-numeric:tabular-nums;",
              {
                omega_now <- omega_total_value()
                if (is.finite(omega_now)) sprintf("%.3f", omega_now) else "NA"
              }
            )
          ),
          shiny::tags$p(
            style = "margin:8px 0 0 0; color:#64748b; font-size:12px;",
            ""
          ),
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          size = "m"
        )
      )
    })

    output$visPathDiagram <- visNetwork::renderVisNetwork({
      shiny::req(input$send2 > 0)
      shiny::req(is.null(model_fit_result()$error))
      build_cfa_visnetwork(
        cfaModel = Cfa2_model(),
        node_positions = shiny::isolate(current_vis_positions()),
        latent_color = input$vis_latent_color,
        observed_color = input$vis_observed_color,
        edge_color = input$vis_edge_color,
        latent_shape = input$vis_latent_shape,
        observed_shape = input$vis_observed_shape,
        latent_node_size = input$vis_latent_node_size,
        observed_node_size = input$vis_observed_node_size,
        font_size = input$vis_font_size,
        edge_value_type = path_edge_value_type()
      )
    })

    output$visLegendTop <- shiny::renderUI({
      shiny::req(input$vis_latent_color)
      latent_shape_class <- if (input$vis_latent_shape %in% c("ellipse", "circle")) input$vis_latent_shape else ""
      observed_shape_class <- if (input$vis_observed_shape %in% c("ellipse", "circle")) input$vis_observed_shape else ""
      shiny::tags$div(
        class = "path-legend-top",
        shiny::tags$div(
          class = "path-legend-chip",
          shiny::tags$span(class = paste("path-legend-shape", latent_shape_class), style = paste0("background:", input$vis_latent_color, ";")),
          shiny::tags$span("Latent Variable")
        ),
        shiny::tags$div(
          class = "path-legend-chip",
          shiny::tags$span(class = paste("path-legend-shape", latent_shape_class), style = paste0("background:", input$vis_latent_color, "; border-style:dashed; border-width:3px;")),
          shiny::tags$span("Exogenous Latent Variable")
        ),
        shiny::tags$div(
          class = "path-legend-chip",
          shiny::tags$span(class = paste("path-legend-shape", observed_shape_class), style = paste0("background:", input$vis_observed_color, ";")),
          shiny::tags$span("Observed Variable")
        ),
        shiny::tags$div(
          class = "path-legend-chip",
          shiny::tags$span(style = paste0("display:inline-block;width:28px;height:0;border-top:3px solid ", input$vis_edge_color, ";")),
          shiny::tags$span(if (((input$toggle_vis_edge_stats %||% 0) %% 2) == 1) "Arrow / Z Value" else "Arrow / Standardized Coefficient")
        ),
        shiny::tags$div(
          class = "path-legend-chip",
          shiny::tags$span(style = "display:inline-block;width:28px;height:0;border-top:3px dashed #666;"),
          shiny::tags$span("Covariance / Modification Path")
        )
      )
    })

    output$model_error_b <- shiny::renderUI({
      shiny::req(input$send2 > 0)
      res <- model_fit_result()
      if (is.null(res$error)) {
        return(NULL)
      }
      shiny::div(
        style = "color:#b30000; font-weight:600; margin:12px 0; white-space:pre-wrap;",
        paste("Model error:", res$error)
      )
    })

    output$cfaResult_b <- shiny::renderUI({
      shiny::req(input$send2 > 0)
      if (!is.null(model_fit_result()$error)) {
        return(NULL)
      }
      shiny::h3(id = "ab", "MODEL SUMMARY")
    })
    output$fitResult_b <- shiny::renderUI({
      shiny::req(input$send2 > 0)
      if (!is.null(model_fit_result()$error)) {
        return(NULL)
      }
      shiny::h3(id = "ab", "FIT INDEXES")
    })
    output$modificationIndex_b <- shiny::renderUI({
      shiny::req(input$send2 > 0)
      if (!is.null(model_fit_result()$error)) {
        return(NULL)
      }
      shiny::h3(id = "ab", "DETAILED OUTPUTS")
    })

    output$cfaDT_b <- DT::renderDataTable(
      {
        shiny::req(input$send2 > 0)
        shiny::req(is.null(model_fit_result()$error))
        cfa_model <- Cfa2_model()
        CFA_ENV$CFA1 <- cfa_model
        pe <- lavaan::parameterEstimates(cfa_model, standardized = TRUE)
        cfaSum2 <- data.frame(
          first = pe$lhs, op = pe$op, last = pe$rhs,
          est = round(pe$est, 3), std.all = round(pe$std.all, 3),
          sh = round(pe$se, 3), z = round(pe$z, 3), p = round(pe$pvalue, 3)
        )
        DT::datatable(
          cfaSum2,
          options = list(pageLength = 15, scrollX = TRUE, dom = "tip"),
          class = "stripe hover compact"
        ) %>%
          DT::formatStyle(
            colnames(cfaSum2),
            backgroundColor = "#f8fafc",
            color = "#1e293b"
          ) %>%
          DT::formatStyle(
            "p",
            backgroundColor = DT::styleInterval(
              c(0.001, 0.01, 0.05),
              c("#dcfce7", "#f0fdf4", "#fff7ed", "#fef2f2")
            ),
            color = DT::styleInterval(
              c(0.001, 0.01, 0.05),
              c("#166534", "#15803d", "#c2410c", "#b91c1c")
            ),
            fontWeight = "bold"
          ) %>%
          DT::formatStyle(
            "z",
            backgroundColor = DT::styleInterval(
              1.960,
              c("#fef2f2", "#dcfce7")
            ),
            color = DT::styleInterval(
              1.960,
              c("#b91c1c", "#166534")
            ),
            fontWeight = "bold"
          )
      },
      options = list(pageLength = 2)
    )

    output$path2 <- shiny::renderPlot({
      shiny::req(input$send2 > 0)
      shiny::req(is.null(model_fit_result()$error))
      draw_semplot_diagram(Cfa2_model())
    })

    output$fit_b <- DT::renderDataTable({
      shiny::req(input$send2 > 0)
      shiny::req(is.null(model_fit_result()$error))
      fitSumDT <- safe_fit_table(Cfa2_model(), input$type3_2)
      nms <- colnames(fitSumDT)
      dt <- DT::datatable(
        fitSumDT,
        options = list(scrollX = TRUE, dom = "t"),
        class = "stripe hover compact"
      ) %>%
        DT::formatStyle(
          nms,
          backgroundColor = "#f8fafc",
          color = "#1e293b",
          fontWeight = "bold"
        )

      # if (input$type3_2 == "BAYESIAN") {
      #   bayes_red_green <- c("PPP", "BRMSEA", "BCFI", "BTLI", "SRMR")
      #   if ("PPP" %in% nms) {
      #     dt <- dt %>% DT::formatStyle("PPP", backgroundColor = DT::styleInterval(c(0.05, 0.95), c("red", "green", "red")))
      #   }
      #   if ("BRMSEA" %in% nms) {
      #     dt <- dt %>% DT::formatStyle("BRMSEA", backgroundColor = DT::styleInterval(c(0.05, 0.08), c("green", "orange", "red")))
      #   }
      #   if ("BCFI" %in% nms) {
      #     dt <- dt %>% DT::formatStyle("BCFI", backgroundColor = DT::styleInterval(c(0.90, 0.95), c("red", "orange", "green")))
      #   }
      #   if ("BTLI" %in% nms) {
      #     dt <- dt %>% DT::formatStyle("BTLI", backgroundColor = DT::styleInterval(c(0.90, 0.95), c("red", "orange", "green")))
      #   }
      #   if ("SRMR" %in% nms) {
      #     dt <- dt %>% DT::formatStyle("SRMR", backgroundColor = DT::styleInterval(c(0.05, 0.08), c("green", "orange", "red")))
      #   }
      #   dt
      # } else {
      rmsea_col <- nms[grepl("^RMSEA", nms)][1]
      cfi_col <- nms[grepl("^CFI", nms)][1]
      tli_col <- nms[grepl("NNFI|TLI", nms)][1]
      if (!is.na(rmsea_col)) {
        dt <- dt %>%
          DT::formatStyle(rmsea_col,
            backgroundColor = DT::styleInterval(c(0.05, 0.08), c("#dcfce7", "#fff7ed", "#fef2f2")),
            color = DT::styleInterval(c(0.05, 0.08), c("#166534", "#c2410c", "#b91c1c")),
            fontWeight = "bold"
          )
      }
      if (!is.na(cfi_col)) {
        dt <- dt %>%
          DT::formatStyle(cfi_col,
            backgroundColor = DT::styleInterval(c(0.90, 0.95), c("#fef2f2", "#fff7ed", "#dcfce7")),
            color = DT::styleInterval(c(0.90, 0.95), c("#b91c1c", "#c2410c", "#166534")),
            fontWeight = "bold"
          )
      }
      if ("AGFI" %in% nms) {
        dt <- dt %>%
          DT::formatStyle("AGFI",
            backgroundColor = DT::styleInterval(c(0.90, 0.95), c("#fef2f2", "#fff7ed", "#dcfce7")),
            color = DT::styleInterval(c(0.90, 0.95), c("#b91c1c", "#c2410c", "#166534")),
            fontWeight = "bold"
          )
      }
      if (!is.na(tli_col)) {
        dt <- dt %>%
          DT::formatStyle(tli_col,
            backgroundColor = DT::styleInterval(c(0.90, 0.95), c("#fef2f2", "#fff7ed", "#dcfce7")),
            color = DT::styleInterval(c(0.90, 0.95), c("#b91c1c", "#c2410c", "#166534")),
            fontWeight = "bold"
          )
      }
      if ("SRMR" %in% nms) {
        dt <- dt %>%
          DT::formatStyle("SRMR",
            backgroundColor = DT::styleInterval(c(0.05, 0.08), c("#dcfce7", "#fff7ed", "#fef2f2")),
            color = DT::styleInterval(c(0.05, 0.08), c("#166534", "#c2410c", "#b91c1c")),
            fontWeight = "bold"
          )
      }
      if ("Chi-sq/df" %in% nms) {
        dt <- dt %>%
          DT::formatStyle("Chi-sq/df",
            backgroundColor = DT::styleInterval(c(2.5, 5), c("#dcfce7", "#fff7ed", "#fef2f2")),
            color = DT::styleInterval(c(2.5, 5), c("#166534", "#c2410c", "#b91c1c")),
            fontWeight = "bold"
          )
      }
      dt
      # }
    })

    output$allfit1_b <- shiny::renderUI({
      shiny::req(input$send2 > 0)
      shiny::req(is.null(model_fit_result()$error))
      DT::DTOutput("allfit2_b")
    })

    output$allfit2_b <- DT::renderDT({
      shiny::req(input$send2 > 0)
      shiny::req(is.null(model_fit_result()$error))
      Values <- tryCatch(
        {
          fm <- round(lavaan::fitMeasures(Cfa2_model()), 3)
          data.frame(
            Measure = names(fm),
            Value = as.numeric(fm),
            stringsAsFactors = FALSE
          )
        },
        error = function(e) {
          data.frame(
            Measure = "Error", Value = NA,
            stringsAsFactors = FALSE
          )
        }
      )
      DT::datatable(
        Values,
        rownames = FALSE,
        options = list(
          pageLength = 20, scrollY = "400px",
          dom = "tip", order = list()
        ),
        class = "stripe hover compact"
      ) %>%
        DT::formatStyle(
          c("Measure", "Value"),
          backgroundColor = "#f8fafc",
          color           = "#1e293b"
        ) %>%
        DT::formatStyle(
          "Measure",
          fontWeight = "bold"
        )
    })

    output$modification_b <- DT::renderDataTable({
      shiny::req(input$send2 > 0)
      shiny::req(is.null(model_fit_result()$error))
      mod <- safe_modindices(Cfa2_model())
      modDT <- data.frame(first_Variable = mod$lhs, operator = mod$op, second_Variable = mod$rhs, modification_index = round(mod$mi, 3))
      DT::datatable(
        modDT,
        options = list(
          pageLength = 15, scrollX = TRUE, dom = "tip",
          order = list(list(3, "desc"))
        ),
        class = "stripe hover compact"
      ) %>%
        DT::formatStyle(
          colnames(modDT),
          backgroundColor = "#f8fafc",
          color = "#1e293b"
        ) %>%
        DT::formatStyle(
          "modification_index",
          backgroundColor = DT::styleInterval(
            c(4, 10),
            c("#f8fafc", "#fff7ed", "#fef2f2")
          ),
          color = DT::styleInterval(
            c(4, 10),
            c("#1e293b", "#c2410c", "#b91c1c")
          ),
          fontWeight = "bold"
        )
    })

    output$text2b <- shiny::renderText({
      if (!is.null(input$data1) && (input$send2 > 0)) "CFA RESULTS" else NULL
    })

    output$dat1_2 <- DT::renderDataTable({
      if (!is.null(input$data1)) {
        d <- data()
        shiny::req(is.data.frame(d))
        if (ncol(d) <= 15) d[1:10, 1:ncol(d)] else d[1:10, 1:15]
      }
    })

    output$dat2_2 <- gt::render_gt({
      if (!is.null(input$data1)) {
        d <- data()
        shiny::req(is.data.frame(d))
        res <- data.frame(N_Item = ncol(d), N = nrow(d), Na = length(which(is.na(d))))
        gt::gt(res) %>%
          gt::tab_header(title = gt::md("*Information About Dataset*")) %>%
          gt::tab_style(
            style = gt::cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#1e293b"),
              gt::cell_text(color = "white", weight = "bold")
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = gt::cell_text(color = "#1e293b", weight = "bold", size = gt::px(16)),
            locations = gt::cells_body()
          ) %>%
          gt::cols_width(gt::everything() ~ gt::px(160)) %>%
          gt::tab_options(
            column_labels.font.size = gt::px(14),
            column_labels.font.weight = "bold",
            heading.title.font.size = gt::px(22),
            heading.background.color = "#f1f5f9",
            data_row.padding = gt::px(12),
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
      }
    })

    output$mvn1_2 <- gt::render_gt({
      if (!is.null(input$data1)) {
        d <- data()
        shiny::req(is.data.frame(d))
        hz <- MVN::mvn(data = d, mvn_test = "hz")$multivariate_normality
        hz <- hz[, c("Test", "Statistic", "p.value", "MVN")]
        colnames(hz) <- c("Test", "HZ Statistic", "P-value", "Result")
        gt::gt(hz) %>%
          gt::tab_header(title = gt::md("*Henze Zirkler Multivariate Normality Test*")) %>%
          gt::tab_style(
            style = gt::cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#1e293b"),
              gt::cell_text(color = "white", weight = "bold")
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#dcfce7"),
              gt::cell_text(color = "#166534", weight = "bold")
            ),
            locations = gt::cells_body(
              columns = "Result",
              rows = Result == "YES"
            )
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#fef2f2"),
              gt::cell_text(color = "#b91c1c", weight = "bold")
            ),
            locations = gt::cells_body(
              columns = "Result",
              rows = Result == "NO"
            )
          ) %>%
          gt::cols_width(gt::everything() ~ gt::px(180)) %>%
          gt::tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            heading.title.font.size = gt::px(22),
            heading.background.color = "#f1f5f9",
            data_row.padding = gt::px(12),
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
      }
    })

    output$mvn4_2 <- gt::render_gt({
      if (!is.null(input$data1)) {
        d <- data()
        shiny::req(is.data.frame(d))
        mrd <- MVN::mvn(data = d, mvn_test = "mardia")$multivariate_normality
        mrd <- mrd[, c("Test", "Statistic", "p.value", "MVN")]
        colnames(mrd) <- c("Test", "Statistic", "P-value", "Result")
        gt::gt(mrd) %>%
          gt::tab_header(title = gt::md("*Mardia Multivariate Normality Test*")) %>%
          gt::tab_style(
            style = gt::cell_fill(color = "#f8fafc"),
            locations = gt::cells_body()
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#1e293b"),
              gt::cell_text(color = "white", weight = "bold")
            ),
            locations = gt::cells_column_labels()
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#dcfce7"),
              gt::cell_text(color = "#166534", weight = "bold")
            ),
            locations = gt::cells_body(
              columns = "Result",
              rows = Result == "YES"
            )
          ) %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#fef2f2"),
              gt::cell_text(color = "#b91c1c", weight = "bold")
            ),
            locations = gt::cells_body(
              columns = "Result",
              rows = Result == "NO"
            )
          ) %>%
          gt::cols_width(gt::everything() ~ gt::px(180)) %>%
          gt::tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold",
            heading.title.font.size = gt::px(22),
            heading.background.color = "#f1f5f9",
            data_row.padding = gt::px(12),
            table.border.top.color = "#1e293b",
            table.border.top.width = gt::px(3)
          )
      }
    })

    output$text1_2 <- shiny::renderText({
      if (!is.null(input$data1)) "DATA UPLOAD AND DESCRIPTIVE STATISTICS" else NULL
    })

    output$ozet_b <- shiny::downloadHandler(
      filename = function() {
        "summary.csv"
      },
      content = function(file) {
        utils::write.csv2(lavaan::parameterEstimates(CFA_ENV$CFA1, standardized = TRUE), file)
      }
    )
    output$modIndis_b <- shiny::downloadHandler(
      filename = function() {
        "modIndeks.csv"
      },
      content = function(file) {
        utils::write.csv2(safe_modindices(CFA_ENV$CFA1), file)
      }
    )
    output$dlPath_b <- shiny::downloadHandler(
      filename = function() {
        "path.pdf"
      },
      content = function(file) {
        shiny::req(CFA_ENV$CFA1)
        grDevices::pdf(file = file, width = 24, height = 14, pointsize = 18)
        on.exit(grDevices::dev.off(), add = TRUE)
        draw_semplot_diagram(CFA_ENV$CFA1)
      }
    )
    output$dlFit_b <- shiny::downloadHandler(
      filename = function() {
        "fitIndexes.csv"
      },
      content = function(file) {
        utils::write.csv2(lavaan::fitMeasures(CFA_ENV$CFA1), file)
      }
    )

    shiny::observeEvent(input$send2, {
      output$foraction_b <- shiny::renderUI({
        shiny::req(is.null(model_fit_result()$error))
        shiny::fluidRow(
          shiny::column(
            4,
            shinyWidgets::dropMenu(
              padding = "20px",
              theme = "light-border",
              placement = "top-start",
              shinyWidgets::actionBttn(inputId = "acb3_b", label = " Click To See All Fit Index", style = "gradient", color = "default", size = "lg"),
              shiny::uiOutput("allfit1_b")
            )
          ),
          shiny::column(
            4,
            shinyWidgets::dropMenu(
              padding = "20px",
              theme = "light-border",
              placement = "top-start",
              shinyWidgets::actionBttn(inputId = "modif_b", label = "Click To See Modifications", style = "gradient", color = "default", size = "lg"),
              DT::dataTableOutput("modification_b")
            )
          ),
          shiny::column(
            4,
            shinyWidgets::dropMenu(
              padding = "40px",
              theme = "light-border",
              placement = "top-start",
              shinyWidgets::actionBttn(inputId = "downs_b", label = "Download All Outputs", style = "gradient", color = "default", size = "lg"),
              shiny::fluidRow(
                shiny::column(3, shinyWidgets::downloadBttn("ozet_b", label = shiny::h5(id = "dwn", "Model Sum"), style = "jelly", color = "primary", size = "xs", block = FALSE, no_outline = TRUE, icon = shiny::icon("download"))),
                shiny::column(3, shinyWidgets::downloadBttn("modIndis_b", label = shiny::h5(id = "dwn", "Mod Index"), style = "jelly", color = "primary", size = "xs", block = FALSE, no_outline = TRUE, icon = shiny::icon("download"))),
                shiny::column(3, shinyWidgets::downloadBttn("dlPath_b", label = shiny::h5(id = "dwn", "Path Diag"), style = "jelly", color = "primary", size = "xs", block = FALSE, no_outline = TRUE, icon = shiny::icon("download"))),
                shiny::column(3, shinyWidgets::downloadBttn("dlFit_b", label = shiny::h5(id = "dwn", "Fit Index"), style = "jelly", color = "primary", size = "xs", block = FALSE, no_outline = TRUE, icon = shiny::icon("download")))
              )
            )
          )
        )
      })
    })

    session$onSessionEnded(function() {
      shiny::stopApp()
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
