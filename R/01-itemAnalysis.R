#' Item and test statistics based on classical test theory,
#' @import shiny
#' @importFrom rstudioapi isAvailable jobRunScript jobSetStatus executeCommand
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{
#' ITEMAN()
#' }
#' @export


ITEMAN <- function() {
  ITEMAN_ENV <- new.env()

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

  ################################## FUNCTIONS ##############################

  # Score data against key  0/1 matrix
  ITEMAN_ENV$scoreData <- function(data, key) {
    d01 <- as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
    for (i in 1:ncol(d01)) {
      d01[, i] <- ifelse(data[, i] == key[i], 1, 0)
      d01[is.na(d01[, i]), i] <- 0
    }
    d01
  }

  # Item statistics from 0/1 matrix
  ITEMAN_ENV$calcItemStats <- function(d01) {
    sum_score <- rowSums(d01)
    p <- colMeans(d01)
    pbis <- sapply(1:ncol(d01), function(k) {
      cor(d01[, k], sum_score, use = "pairwise.complete.obs")
    })
    bis <- sapply(1:ncol(d01), function(k) {
      polycor::polyserial(sum_score, d01[, k])
    })
    data.frame(
      Items = paste0("item", 1:ncol(d01)),
      Item_Difficulty = round(p, 3),
      Point_Biserial = round(pbis, 3),
      Biserial = round(bis, 3),
      stringsAsFactors = FALSE
    )
  }

  # Render DT with colour-coded bad items, 15 rows default, scroll
  ITEMAN_ENV$renderItemDT <- function(df, item_col_name = "Items") {
    # Rename first col to "Items" for consistency
    colnames(df)[1] <- "Items"
    DT::datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "430px",
        scrollCollapse = TRUE,
        dom = "tip",
        ordering = TRUE,
        rowCallback = DT::JS(
          "function(row, data, index) {",
          "  var diff = parseFloat(data[2]);",
          "  var pb   = parseFloat(data[3]);",
          "  var bis  = parseFloat(data[4]);",
          "  if (diff >= 0.80 || diff <= 0.25)",
          "    $('td:eq(2)',row).css({'color':'#c0392b','font-weight':'700','text-decoration':'underline'});",
          "  if (pb <= 0.25)",
          "    $('td:eq(3)',row).css({'color':'#c0392b','font-weight':'700','text-decoration':'underline'});",
          "  if (bis <= 0.25)",
          "    $('td:eq(4)',row).css({'color':'#c0392b','font-weight':'700','text-decoration':'underline'});",
          "}"
        )
      ),
      rownames = FALSE,
      class = "stripe hover compact",
      extensions = "Scroller"
    ) %>%
      DT::formatStyle(
        "Item_Difficulty",
        background = DT::styleInterval(c(0.25, 0.80), c("#fde8e8", "#ffffff", "#fde8e8"))
      )
  }

  ###################### CSS ##############################

  theme_css <- "
    :root {
      --tc:  #6B3FA0;
      --tdk: #3B1F5E;
      --tlt: #9B72CF;
      --tpl: #C084FC;
      --tls: #E8DCFF;
    }

    body { background:#ffffff !important; color:#1a1a2e !important;
           font-family:'Segoe UI',Arial,sans-serif; }

    /* ---- Tabs ---- */
    .nav-tabs > li > a {
      color: var(--tdk) !important; font-weight:600;
      border-radius:8px 8px 0 0 !important;
      background:#f7f4ff !important;
      white-space:nowrap !important;
    }
    .nav-tabs > li > a h4 {
      margin:0 !important;
      padding:0 !important;
      white-space:nowrap !important;
    }
    @media (max-width: 1280px) {
      .nav-tabs > li > a { font-size:12px !important; padding:6px 8px !important; }
      .nav-tabs > li > a h4 { font-size:12px !important; }
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

    /* Section headers */
    .section-header {
      color:var(--tdk); font-size:20px; font-family:'Segoe UI',Arial,sans-serif;
      font-weight:700; letter-spacing:1px; margin-bottom: 8px;
      border-left:5px solid var(--tc); padding-left:10px;
    }

    /* Title */
    #tepe { border-bottom: 8px solid var(--tc) !important; padding-bottom: 8px; margin-bottom: 8px; }
    #title  { color:var(--tdk) !important; font-size:26px !important; font-weight:800 !important; }
    #title2 { color:var(--tlt) !important; font-size:14px !important; text-align:right !important; }
    .key-warning { color:#c0392b; font-weight:700; font-size:13px; }



    /* ---- Theme-aware package information card: PNG-free visual ---- */
    .rsp-visual-card, .rsp-visual-card * { box-sizing:border-box; }
    .rsp-visual-card { width:97%; margin:0 auto 14px auto; position:relative; overflow:hidden; border-radius:20px; padding:20px 22px; background:radial-gradient(circle at 12% 18%, var(--tls) 0, transparent 34%), linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--tc) 8%, #ffffff) 54%, color-mix(in srgb, var(--tc) 18%, #ffffff) 100%); border:1px solid color-mix(in srgb, var(--tc) 24%, #ffffff); box-shadow:0 16px 38px rgba(24, 20, 40, .10); color:#15203a; min-height:170px; }
    .rsp-visual-card.compact { min-height:150px; padding:18px 18px; }
    .rsp-visual-card.hero { min-height:390px; padding:32px 36px; }
    .rsp-visual-card.sidebar-mini { width:100%; height:154px; min-height:154px; max-height:154px; margin:2px 0 16px 0; padding:13px 14px; border-radius:17px; overflow:hidden; }
    .rsp-visual-card.sidebar-mini:before { right:-58px; bottom:-92px; width:195px; height:160px; opacity:.78; }
    .rsp-visual-card.sidebar-mini:after { right:12px; top:12px; width:46px; height:46px; background-size:12px 12px; opacity:.22; }
    .rsp-visual-card.sidebar-mini .rsp-pill { padding:5px 9px; gap:6px; margin-bottom: 8px; font-size:11px; max-width:100%; }
    .rsp-visual-card.sidebar-mini .rsp-mark { width:16px; height:16px; border-radius:5px; box-shadow:0 0 0 3px color-mix(in srgb, var(--tc) 12%, transparent); }
    .rsp-visual-card.sidebar-mini .rsp-mark:after { inset:4px; border-width:1.5px; border-radius:3px; }
    .rsp-visual-card.sidebar-mini .rsp-title { font-size:22px; line-height:.98; letter-spacing:-.6px; max-width:72%; }
    .rsp-visual-card.sidebar-mini .rsp-title span { margin-top:2px; }
    .rsp-visual-card.sidebar-mini .rsp-subtitle { display:none; }
    .rsp-visual-card.sidebar-mini .rsp-mini-grid { grid-template-columns:repeat(2, minmax(0, 1fr)); gap:5px; margin-top:9px; max-width:72%; }
    .rsp-visual-card.sidebar-mini .rsp-chip { padding:5px 6px; border-radius:9px; font-size:9.5px; gap:5px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
    .rsp-visual-card.sidebar-mini .rsp-chip .rsp-dot { width:12px; height:12px; flex-basis:12px; border-radius:4px; }
    .rsp-visual-card.sidebar-mini .rsp-chip .rsp-dot:after { inset:4px; }
    .rsp-visual-card.sidebar-mini .rsp-path-visual, .rsp-visual-card.sidebar-mini .rsp-fit-card { display:none; }
    .rsp-visual-card.sidebar-mini .rsp-credit { right:10px; bottom: 8px; font-size:9.5px; padding:4px 7px; max-width:125px; white-space:nowrap; }
    .rsp-visual-card:before { content:''; position:absolute; right:-70px; bottom:-100px; width:280px; height:230px; background:linear-gradient(135deg, color-mix(in srgb, var(--tc) 26%, #ffffff), var(--tc)); border-radius:60% 40% 0 0; opacity:.92; transform:rotate(-8deg); }
    .rsp-visual-card:after { content:''; position:absolute; right:22px; top:22px; width:72px; height:72px; background-image:radial-gradient(var(--tc) 2px, transparent 2.5px); background-size:16px 16px; opacity:.32; }
    .rsp-visual-content { position:relative; z-index:2; }
    .rsp-pill { display:inline-flex; align-items:center; gap:9px; padding:8px 14px; border-radius:999px; background:rgba(255,255,255,.72); border:1px solid color-mix(in srgb, var(--tc) 20%, #ffffff); box-shadow:0 8px 22px rgba(20,20,40,.08); color:var(--tdk); font-weight:800; letter-spacing:.2px; margin-bottom: 8px; }
    .rsp-mark { width:22px; height:22px; border-radius:7px; background:linear-gradient(135deg, var(--tc), color-mix(in srgb, var(--tc) 50%, #ffffff)); position:relative; box-shadow:0 0 0 5px color-mix(in srgb, var(--tc) 12%, transparent); }
    .rsp-mark:after { content:''; position:absolute; inset:5px; border:2px solid #fff; border-radius:5px; }
    .rsp-title { margin:0; font-size:clamp(28px, 4.2vw, 54px); line-height:.96; font-weight:900; letter-spacing:-1.5px; color:#07183f; }
    .rsp-title span { color:var(--tc); display:block; margin-top:6px; }
    .rsp-subtitle { margin:13px 0 0 0; max-width:440px; color:#566079; font-size:15px; line-height:1.45; font-weight:500; }
    .rsp-mini-grid { display:grid; grid-template-columns:repeat(2, minmax(0, 1fr)); gap:8px; margin-top:18px; max-width:420px; }
    .rsp-chip { display:flex; align-items:center; gap:8px; padding:9px 10px; border-radius:12px; background:rgba(255,255,255,.62); border:1px solid color-mix(in srgb, var(--tc) 15%, #ffffff); color:#26314f; font-size:12px; font-weight:800; }
    .rsp-chip .rsp-dot { width:18px; height:18px; border-radius:6px; background:color-mix(in srgb, var(--tc) 72%, #ffffff); display:inline-block; position:relative; flex:0 0 18px; }
    .rsp-chip .rsp-dot:after { content:''; position:absolute; inset:5px; border-radius:50%; background:#fff; opacity:.9; }
    .rsp-path-visual { position:absolute; right:54px; top:82px; width:300px; height:230px; opacity:.95; }
    .rsp-node, .rsp-item, .rsp-error { position:absolute; display:flex; align-items:center; justify-content:center; border:2px solid color-mix(in srgb, var(--tc) 42%, #ffffff); background:rgba(255,255,255,.60); color:var(--tdk); box-shadow:0 10px 28px rgba(20,20,40,.08); font-weight:900; }
    .rsp-node { width:82px; height:82px; border-radius:50%; font-size:24px; background:color-mix(in srgb, var(--tc) 10%, #ffffff); }
    .rsp-node.n1 { left:0; top:8px; } .rsp-node.n2 { left:0; bottom: 8px; }
    .rsp-item { width:50px; height:36px; border-radius:9px; right:66px; font-size:14px; }
    .rsp-item.i1 { top:6px; } .rsp-item.i2 { top:58px; } .rsp-item.i3 { top:110px; } .rsp-item.i4 { top:162px; }
    .rsp-error { width:34px; height:34px; border-radius:50%; right:8px; font-size:12px; opacity:.82; }
    .rsp-error.e1 { top:7px; } .rsp-error.e2 { top:59px; } .rsp-error.e3 { top:111px; } .rsp-error.e4 { top:163px; }
    .rsp-line { position:absolute; height:2px; background:linear-gradient(90deg, var(--tc), color-mix(in srgb, var(--tc) 40%, #ffffff)); transform-origin:left center; opacity:.88; }
    .rsp-line.l1 { left:76px; top:34px; width:130px; transform:rotate(-8deg); } .rsp-line.l2 { left:77px; top:74px; width:128px; transform:rotate(0deg); } .rsp-line.l3 { left:76px; top:116px; width:132px; transform:rotate(9deg); } .rsp-line.l4 { left:76px; top:160px; width:132px; transform:rotate(7deg); }
    .rsp-fit-card { position:absolute; left:36px; bottom: 8px; width:150px; padding:11px 12px; border-radius:16px; background:rgba(255,255,255,.72); border:1px solid color-mix(in srgb, var(--tc) 16%, #ffffff); box-shadow:0 10px 24px rgba(20,20,40,.08); font-size:12px; color:#556078; font-weight:800; }
    .rsp-fit-card div { display:flex; justify-content:space-between; border-bottom: 8px solid rgba(120,120,160,.14); padding:3px 0; } .rsp-fit-card div:last-child { border-bottom:0; } .rsp-fit-card b { color:var(--tc); }
    .rsp-credit { position:absolute; right:22px; bottom: 8px; z-index:3; font-size:12px; font-weight:800; letter-spacing:.2px; color:color-mix(in srgb, var(--tc) 72%, #17213d); background:rgba(255,255,255,.62); border:1px solid color-mix(in srgb, var(--tc) 15%, #ffffff); border-radius:999px; padding:6px 10px; }
    @media (max-width: 1180px) { .rsp-path-visual { right:28px; transform:scale(.88); transform-origin:top right; } }
    @media (max-width: 980px) { .rsp-path-visual, .rsp-fit-card { display:none; } .rsp-visual-card.hero { min-height:310px; } }



    /* ---- 05 revision: simplified sidebar card, collision-safe hero ---- */
    .rsp-sidebar-simple, .rsp-sidebar-simple * { box-sizing:border-box; }
    .rsp-sidebar-simple {
      width:100%; height:118px; min-height:118px; max-height:118px;
      margin:0 0 12px 0; padding:12px 13px;
      position:relative; overflow:hidden; border-radius:16px;
      background:linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--tc) 9%, #ffffff) 56%, color-mix(in srgb, var(--tc) 18%, #ffffff) 100%);
      border:1px solid color-mix(in srgb, var(--tc) 24%, #ffffff);
      box-shadow:0 10px 24px rgba(24, 20, 40, .08);
      color:#07183f;
    }
    .rsp-sidebar-simple:before {
      content:''; position:absolute; right:-42px; bottom:-68px;
      width:150px; height:122px; border-radius:58% 42% 0 0;
      background:linear-gradient(135deg, color-mix(in srgb, var(--tc) 22%, #ffffff), var(--tc));
      opacity:.55; transform:rotate(-8deg);
    }
    .rsp-sidebar-simple:after {
      content:''; position:absolute; right:10px; top:9px; width:46px; height:46px;
      background-image:radial-gradient(var(--tc) 1.6px, transparent 2px);
      background-size:12px 12px; opacity:.18;
    }
    .rsp-sidebar-content { position:relative; z-index:2; }
    .rsp-sidebar-package {
      display:inline-flex; align-items:center; gap:6px; max-width:100%;
      padding:4px 8px; border-radius:999px;
      background:rgba(255,255,255,.72);
      border:1px solid color-mix(in srgb, var(--tc) 18%, #ffffff);
      color:var(--tdk); font-size:10.5px; font-weight:850; letter-spacing:.15px;
      white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
    }
    .rsp-sidebar-mark { width:13px; height:13px; border-radius:4px; flex:0 0 13px; background:linear-gradient(135deg, var(--tc), color-mix(in srgb, var(--tc) 48%, #ffffff)); position:relative; }
    .rsp-sidebar-mark:after { content:''; position:absolute; inset:3.5px; border:1.3px solid #fff; border-radius:2.5px; }
    .rsp-sidebar-lines { margin-top:10px; display:grid; gap:4px; max-width:83%; }
    .rsp-sidebar-lines div {
      color:#07183f; font-weight:900; font-size:13px; line-height:1.05;
      letter-spacing:-.1px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
    }
    .rsp-sidebar-lines div:nth-child(2) { color:var(--tc); }
    .rsp-sidebar-credit {
      position:absolute; left:13px; bottom: 8px; z-index:2;
      color:color-mix(in srgb, var(--tc) 72%, #17213d);
      font-size:10px; font-weight:800; letter-spacing:.1px;
      background:rgba(255,255,255,.60); border:1px solid color-mix(in srgb, var(--tc) 14%, #ffffff);
      border-radius:999px; padding:3px 7px; white-space:nowrap;
    }
    .rsp-visual-card.hero .rsp-visual-content { max-width:50%; }
    .rsp-visual-card.hero .rsp-subtitle { max-width:430px; }
    .rsp-visual-card.hero .rsp-mini-grid { max-width:430px; }
    .rsp-visual-card.hero .rsp-path-visual { right:34px; top:118px; transform:scale(.92); transform-origin:top right; opacity:.82; }
    .rsp-visual-card.hero .rsp-fit-card { left:36px; bottom: 8px; }
    @media (max-width: 1320px) { .rsp-visual-card.hero .rsp-path-visual { transform:scale(.78); right:18px; top:130px; } }
    @media (max-width: 1120px) { .rsp-visual-card.hero .rsp-path-visual { display:none; } .rsp-visual-card.hero .rsp-visual-content { max-width:100%; } }



    /* ---- 06 revision: CTT-focused clean text and non-overlapping visuals ---- */
    .rsp-sidebar-simple { height:128px; min-height:128px; max-height:128px; padding:12px 12px; border-radius:15px; }
    .rsp-sidebar-simple:before { width:150px; height:115px; right:-54px; bottom:-70px; opacity:.54; }
    .rsp-sidebar-simple:after { width:38px; height:38px; right:10px; top:10px; background-size:11px 11px; opacity:.16; }
    .rsp-sidebar-package { font-size:11px; padding:5px 8px; margin-bottom: 8px; max-width:126px; }
    .rsp-sidebar-lines { margin-top:6px; gap:2px; max-width:100%; }
    .rsp-sidebar-lines div { font-size:10.2px; line-height:1.12; letter-spacing:.25px; }
    .rsp-sidebar-credit { left:12px; right:auto; bottom: 8px; max-width:calc(100% - 24px); font-size:9.6px; padding:3px 0; background:transparent; border:0; }

    .rsp-visual-card.hero { min-height:390px; padding:30px 34px; }
    .rsp-visual-card.hero .rsp-visual-content { max-width:48%; }
    .rsp-visual-card.hero .rsp-title { font-size:clamp(30px, 4.1vw, 52px); line-height:.95; max-width:540px; }
    .rsp-visual-card.hero .rsp-subtitle { max-width:420px; font-size:16px; }
    .rsp-visual-card.hero .rsp-mini-grid { max-width:620px; width:calc(100% - 68px); grid-template-columns:repeat(4, minmax(0, 1fr)); gap:10px; position:absolute; left:34px; bottom: 8px; margin-top:0; }
    .rsp-visual-card.hero .rsp-chip { min-height:58px; justify-content:center; flex-direction:column; text-align:center; font-size:11px; line-height:1.15; padding:8px 8px; }
    .rsp-visual-card.hero .rsp-chip .rsp-dot { width:20px; height:20px; flex-basis:20px; }
    .rsp-visual-card.hero .rsp-path-visual { display:none; }
    .rsp-visual-card.hero .rsp-fit-card { left:auto; right:42px; top:74px; bottom:auto; width:330px; min-height:215px; padding:18px; opacity:.92; }
    .rsp-visual-card.hero .rsp-fit-card:before { content:'ITEM DIFFICULTY'; display:block; color:#3e4562; font-size:12px; font-weight:900; margin-bottom: 8px; letter-spacing:.4px; }
    .rsp-visual-card.hero .rsp-fit-card div { font-size:13px; padding:6px 0; }
    .rsp-visual-card.hero .rsp-fit-card div:nth-child(2)::before { content:''; display:block; height:82px; margin:4px 0 10px 0; border-radius:12px; background:linear-gradient(180deg, color-mix(in srgb, var(--tc) 18%, transparent), transparent), repeating-linear-gradient(90deg, color-mix(in srgb, var(--tc) 50%, #ffffff) 0 13px, transparent 13px 25px); opacity:.74; }
    .rsp-visual-card.hero .rsp-credit { right:24px; bottom: 8px; }
    @media (max-width: 1320px) { .rsp-visual-card.hero .rsp-fit-card { width:275px; right:24px; } .rsp-visual-card.hero .rsp-visual-content { max-width:54%; } }
    @media (max-width: 1120px) { .rsp-visual-card.hero .rsp-fit-card { display:none; } .rsp-visual-card.hero .rsp-visual-content { max-width:100%; } .rsp-visual-card.hero .rsp-mini-grid { position:relative; left:auto; bottom:auto; width:100%; grid-template-columns:repeat(2, minmax(0, 1fr)); margin-top:18px; } }


    /* ---- 07: CSS-only, simplified CTT dashboard visual ---- */
    .ctt-hero, .ctt-hero * { box-sizing:border-box; }
    .ctt-hero {
      width:97%; min-height:420px; margin:0 auto 16px auto;
      position:relative; overflow:hidden; border-radius:22px;
      padding:30px 32px;
      background:
        radial-gradient(circle at 88% 92%, color-mix(in srgb, var(--tc) 16%, transparent) 0, transparent 33%),
        linear-gradient(135deg, #ffffff 0%, color-mix(in srgb, var(--tc) 5%, #ffffff) 62%, color-mix(in srgb, var(--tc) 11%, #ffffff) 100%);
      border:1px solid color-mix(in srgb, var(--tc) 18%, #ffffff);
      box-shadow:0 18px 42px rgba(22,24,44,.09);
      color:#07183f;
    }
    .ctt-hero:after {
      content:''; position:absolute; right:22px; top:18px; width:62px; height:62px;
      background-image:radial-gradient(var(--tc) 1.8px, transparent 2.3px);
      background-size:14px 14px; opacity:.20;
    }
    .ctt-left { position:relative; z-index:2; width:39%; min-width:320px; padding-top:4px; }
    .ctt-brand { display:flex; align-items:center; gap:18px; margin-bottom: 8px; }
    .ctt-logo {
      width:92px; height:92px; display:flex; flex-direction:column; align-items:center; justify-content:center;
      clip-path:polygon(25% 5%,75% 5%,100% 50%,75% 95%,25% 95%,0 50%);
      border:3px solid var(--tc); background:rgba(255,255,255,.68);
      color:#07183f; font-weight:900; line-height:1.06; box-shadow:0 10px 24px rgba(22,24,44,.08);
    }
    .ctt-logo b { color:var(--tc); font-size:28px; letter-spacing:-.8px; }
    .ctt-logo span { font-size:11px; }
    .ctt-brand-text { border-left:1px solid rgba(80,80,120,.18); padding-left:20px; }
    .ctt-package { font-size:22px; font-weight:900; margin-bottom: 8px; }
    .ctt-domain { font-size:16px; color:#59627c; font-weight:650; }
    .ctt-domain b { color:var(--tc); }
    .ctt-main-title { margin:0; font-size:48px; line-height:.98; font-weight:950; letter-spacing:-1.8px; color:#07183f; }
    .ctt-main-title span { display:block; }
    .ctt-subtitle { margin:18px 0 0 0; font-size:26px; line-height:1.15; font-weight:820; color:var(--tc); }
    .ctt-desc { margin:18px 0 0 0; max-width:430px; font-size:17px; line-height:1.42; color:#4f5873; font-weight:520; }
    .ctt-panels { position:absolute; z-index:2; left:43%; right:32px; top:30px; bottom: 8px; display:grid; grid-template-columns:1fr 1.72fr .72fr; grid-template-rows:1fr 1fr; gap:14px; }
    .ctt-card { position:relative; overflow:hidden; border-radius:16px; background:rgba(255,255,255,.72); border:1px solid rgba(90,80,130,.18); box-shadow:0 10px 24px rgba(22,24,44,.06); padding:14px 16px; }
    .ctt-card-title { margin:0 0 12px 0; color:var(--tc); font-size:13px; font-weight:900; letter-spacing:.3px; }
    .ctt-reliability { grid-column:1; grid-row:1; }
    .ctt-difficulty { grid-column:2 / span 2; grid-row:1; }
    .ctt-discrimination { grid-column:1; grid-row:2; }
    .ctt-table-card { grid-column:2; grid-row:2; }
    .ctt-score { grid-column:3; grid-row:2; }

    .ctt-ring { width:122px; height:122px; border-radius:50%; margin:8px auto 0 auto; background:conic-gradient(var(--tc) 0 78deg, color-mix(in srgb, var(--tc) 38%, #ffffff) 78deg 360deg); display:flex; align-items:center; justify-content:center; }
    .ctt-ring-inner { width:82px; height:82px; border-radius:50%; background:#fff; display:flex; flex-direction:column; align-items:center; justify-content:center; color:#07183f; }
    .ctt-ring-inner b { font-size:28px; line-height:1; }
    .ctt-ring-inner span { font-size:11px; margin-top:5px; color:#4f5873; font-weight:700; }

    .ctt-bars { position:absolute; left:46px; right:46px; bottom: 8px; height:108px; border-bottom: 8px solid color-mix(in srgb, var(--tc) 52%, #ffffff); }
    .ctt-bars span { position:absolute; bottom:0; width:4.8%; border-radius:5px 5px 0 0; background:color-mix(in srgb, var(--tc) 36%, #ffffff); }
    .ctt-bars span:nth-child(1){left:2%;height:18%}.ctt-bars span:nth-child(2){left:10%;height:34%}.ctt-bars span:nth-child(3){left:18%;height:55%}.ctt-bars span:nth-child(4){left:26%;height:75%}.ctt-bars span:nth-child(5){left:34%;height:90%}.ctt-bars span:nth-child(6){left:42%;height:100%}.ctt-bars span:nth-child(7){left:50%;height:82%}.ctt-bars span:nth-child(8){left:58%;height:64%}.ctt-bars span:nth-child(9){left:66%;height:76%}.ctt-bars span:nth-child(10){left:74%;height:52%}.ctt-bars span:nth-child(11){left:82%;height:34%}.ctt-bars span:nth-child(12){left:90%;height:18%}
    .ctt-curve { position:absolute; left:42px; right:42px; bottom: 8px; height:98px; border-bottom:0; }
    .ctt-curve:before { content:''; position:absolute; left:0; right:0; bottom: 8px; height:78px; border:3px solid var(--tc); border-color:var(--tc) transparent transparent transparent; border-radius:50% 50% 0 0; transform:scaleY(.72); opacity:.92; }
    .ctt-axis-labels { position:absolute; left:42px; right:42px; bottom: 8px; display:flex; justify-content:space-between; color:#07183f; font-size:11px; font-weight:800; }

    .ctt-scatter { position:absolute; left:42px; right:30px; top:52px; bottom: 8px; border-left:1px solid rgba(80,80,120,.25); border-bottom: 8px solid rgba(80,80,120,.25); background:linear-gradient(rgba(80,80,120,.08) 1px, transparent 1px), linear-gradient(90deg, rgba(80,80,120,.08) 1px, transparent 1px); background-size:38px 34px; }
    .ctt-scatter:after { content:''; position:absolute; left:8px; right:8px; top:74px; height:2px; background:var(--tc); transform:rotate(-30deg); transform-origin:left center; opacity:.80; }
    .ctt-dot { position:absolute; width:6px; height:6px; border-radius:50%; background:var(--tc); opacity:.88; }
    .ctt-dot.d1{left:12%;bottom:18%}.ctt-dot.d2{left:20%;bottom:28%}.ctt-dot.d3{left:28%;bottom:36%}.ctt-dot.d4{left:35%;bottom:44%}.ctt-dot.d5{left:41%;bottom:42%}.ctt-dot.d6{left:48%;bottom:55%}.ctt-dot.d7{left:56%;bottom:62%}.ctt-dot.d8{left:63%;bottom:70%}.ctt-dot.d9{left:73%;bottom:78%}.ctt-dot.d10{left:84%;bottom:86%}.ctt-dot.d11{left:50%;bottom:28%}.ctt-dot.d12{left:69%;bottom:54%}

    .ctt-mini-table { width:100%; border-collapse:collapse; font-size:12px; color:#07183f; }
    .ctt-mini-table th { color:#1b2440; font-size:10px; text-align:left; border-bottom: 8px solid rgba(80,80,120,.16); padding:6px 4px; }
    .ctt-mini-table td { border-bottom: 8px solid rgba(80,80,120,.12); padding:7px 4px; font-weight:650; }
    .ctt-mini-table tr:last-child td { border-bottom:0; }

    .ctt-score-curve { position:absolute; left:20px; right:20px; top:58px; height:92px; border-bottom: 8px solid rgba(80,80,120,.18); }
    .ctt-score-curve:before { content:''; position:absolute; left:8px; right:8px; bottom: 8px; height:70px; border:3px solid var(--tc); border-color:var(--tc) transparent transparent transparent; border-radius:50% 50% 0 0; transform:scaleY(1.05); }
    .ctt-score-curve:after { content:''; position:absolute; left:50%; top:4px; height:74px; border-left:2px dashed var(--tc); opacity:.72; }
    .ctt-score-stats { position:absolute; left:20px; right:20px; bottom: 8px; display:flex; justify-content:space-between; font-weight:900; color:#07183f; }
    .ctt-score-stats span { display:block; color:#536078; font-size:10px; font-weight:800; }

    .ctt-steps { position:absolute; z-index:2; left:43%; right:90px; bottom: 8px; display:grid; grid-template-columns:repeat(4, 1fr); align-items:center; text-align:center; color:#07183f; font-size:11px; font-weight:900; }
    .ctt-step { position:relative; }
    .ctt-step:before { content:''; display:flex; align-items:center; justify-content:center; margin:0 auto 7px auto; width:28px; height:28px; border-radius:50%; color:#fff; background:var(--tc); font-size:17px; box-shadow:0 7px 15px rgba(22,24,44,.11); }
    .ctt-step:not(:last-child):after { content:''; position:absolute; left:60%; top:13px; width:80%; border-top:2px dashed color-mix(in srgb, var(--tc) 45%, #ffffff); }
    .ctt-bottom { position:absolute; z-index:2; left:32px; right:28%; bottom: 8px; height:72px; display:grid; grid-template-columns:repeat(4, 1fr); background:rgba(255,255,255,.64); border:1px solid rgba(90,80,130,.16); border-radius:16px; box-shadow:0 10px 24px rgba(22,24,44,.05); }
    .ctt-feature { display:flex; flex-direction:column; align-items:center; justify-content:center; gap:6px; color:#07183f; font-size:12px; font-weight:900; border-right:1px solid rgba(80,80,120,.14); }
    .ctt-feature:last-child { border-right:0; }
    .ctt-icon { width:26px; height:22px; position:relative; }
    .ctt-icon.shield { background:linear-gradient(135deg, var(--tc), color-mix(in srgb, var(--tc) 60%, #ffffff)); border-radius:3px; position:relative; border:2px solid var(--tc); }
    .ctt-icon.shield:before { content:''; position:absolute; left:6px; right:6px; top:4px; bottom:8px; border:1.5px solid #fff; border-radius:2px; }
    .ctt-icon.bars { border-left:3px solid var(--tc); position:relative; }
    .ctt-icon.bars span { position:absolute; bottom:0; width:3px; background:var(--tc); border-radius:2px; }
    .ctt-icon.bars span:nth-child(1){left:6px;height:6px}.ctt-icon.bars span:nth-child(2){left:12px;height:12px}.ctt-icon.bars span:nth-child(3){left:18px;height:18px}
    .ctt-icon.line { position:relative; border-bottom:3px solid var(--tc); }
    .ctt-icon.line:before { content:''; position:absolute; left:4px; right:4px; top:50%; width:18px; height:3px; background:var(--tc); border-radius:2px; transform:rotate(-35deg) translateY(-50%); }
    .ctt-icon.line:after { content:''; position:absolute; left:2px; top:50%; width:6px; height:6px; border-radius:50%; background:var(--tc); transform:translateY(-50%); }
    .ctt-icon.pie { border-radius:50%; background:conic-gradient(var(--tc) 0 78%, color-mix(in srgb, var(--tc) 24%, #ffffff) 78% 100%); }
    .ctt-credit { position:absolute; right:32px; bottom: 8px; color:color-mix(in srgb, var(--tc) 80%, #17213d); font-size:13px; font-weight:800; z-index:2; }

    @media (max-width: 1200px) {
      .ctt-hero { min-height:500px; }
      .ctt-left { width:100%; min-width:0; }
      .ctt-brand { margin-bottom: 8px; }
      .ctt-main-title { font-size:38px; }
      .ctt-panels, .ctt-steps { display:none; }
      .ctt-bottom { right:32px; bottom: 8px; }
      .ctt-credit { bottom: 8px; }
    }



    /* ---- 08 revision: clean text-focused hero and simplified sidebar ---- */
    .ctt-hero { min-height:360px; padding:34px 38px 30px 38px; }
    .ctt-left { width:100% !important; min-width:0; max-width:100%; padding-top:0; }
    .ctt-brand { margin-bottom: 8px; }
    .ctt-main-title-08 {
      max-width:980px; font-size:clamp(34px, 5.2vw, 62px);
      line-height:1.02; letter-spacing:-1.9px;
    }
    .ctt-main-title-08 span { display:block; }
    .ctt-desc-08 { max-width:760px; font-size:18px; margin-top:18px; }
    .ctt-panels, .ctt-steps { display:none !important; }
    .ctt-bottom {
      left:50%; transform:translateX(-50%); width:auto; bottom: 8px; height:76px;
      background:rgba(255,255,255,.68);
      display:flex !important; justify-content:center; align-items:center; gap:24px;
    }
    .ctt-credit { right:38px; top: 28px; bottom:auto; }
    .ctt-feature { font-size:12px; letter-spacing:.1px; }

    .rsp-sidebar-simple {
      height:148px !important; min-height:148px !important; max-height:148px !important;
      padding:13px 13px !important;
    }
    .rsp-sidebar-package { font-size:11px !important; max-width:150px !important; margin-bottom: 8px !important; }
    .rsp-sidebar-lines { max-width:94% !important; gap:4px !important; margin-top:6px !important; }
    .rsp-sidebar-lines div {
      font-size:13px !important; line-height:1.22 !important;
      white-space:normal !important; overflow:visible !important; text-overflow:clip !important;
      letter-spacing:.08px !important; font-weight:900 !important;
    }
    .rsp-sidebar-lines div:nth-child(2) { color:var(--tc) !important; }
    .rsp-sidebar-credit {
      left:auto !important; right:10px !important; bottom: 8px !important;
      font-size:9.4px !important; padding:0 !important; background:transparent !important; border:0 !important;
    }

    @media (max-width: 1120px) {
      .ctt-hero { min-height:380px; }
      .ctt-bottom { grid-template-columns:repeat(2, 1fr); height:132px; bottom: 8px; }
      .ctt-credit { bottom: 8px; }
    }


    /* ---- 09 revision: smaller centered hero title and icon-only footer ---- */
    .ctt-hero { min-height:340px !important; padding:28px 34px 28px 34px !important; }
    .ctt-brand { margin-bottom: 8px !important; }
    .ctt-logo { width:76px !important; height:76px !important; }
    .ctt-logo b { font-size:23px !important; }
    .ctt-logo span { font-size:9.5px !important; }
    .ctt-package { font-size:20px !important; }
    .ctt-domain { font-size:15px !important; }
    .ctt-main-title-08 {
      max-width:100% !important;
      text-align:center !important;
      font-size:clamp(32px, 4.4vw, 56px) !important;
      line-height:1.06 !important;
      letter-spacing:-1.1px !important;
      margin-top:4px !important;
    }
    .ctt-main-title-08 .ctt-title-top {
      display:block !important;
      color:#07183f !important;
    }
    .ctt-main-title-08 .ctt-title-sub {
      display:block !important;
      text-align:center !important;
      margin-top:8px !important;
      color:var(--tc) !important;
      font-size:.68em !important;
      letter-spacing:.5px !important;
      line-height:1.18 !important;
    }
    .ctt-desc-08 {
      max-width:720px !important;
      margin:14px auto 0 auto !important;
      text-align:center !important;
      font-size:14px !important;
      line-height:1.35 !important;
    }
    .ctt-bottom {
      position:relative !important;
      left:auto !important;
      right:auto !important;
      transform:none !important;
      width:100% !important;
      height:112px !important;
      margin-top:16px !important;
      bottom:auto !important;
      display:flex !important;
      justify-content:space-around !important;
      align-items:center !important;
      gap:0 !important;
      padding:0 !important;
      background:rgba(255,255,255,.68) !important;
    }
    .ctt-feature {
      gap:8px !important;
      font-size:11px !important;
      line-height:1.2 !important;
      display:flex !important;
      flex-direction:column !important;
      align-items:center !important;
      justify-content:center !important;
      flex:1 1 33.333% !important;
      height:100% !important;
      border-right:1px solid rgba(80,80,120,.12) !important;
    }
    .ctt-feature:last-child { border-right:0 !important; }
    .ctt-feature .ctt-emoji { font-size:28px !important; display:block !important; }
    .ctt-icon { display:none !important; }
    .ctt-credit { bottom: 8px !important; font-size:11px !important; }
    @media (max-width: 1120px) {
      .ctt-hero { min-height:365px !important; }
      .ctt-bottom { height:100px !important; display:flex !important; justify-content:space-around !important; gap:0 !important; bottom: 8px !important; }
      .ctt-credit { bottom: 8px !important; }
    }

    /* dynamic theme style injection point */
    #dynamic-theme-style {}
  "

  ################################## UI ##############################

  # PNG kullanmadan, tamamen HTML/CSS ile izilen tema-uyumlu tantm grseli
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
              shiny::div("CLASSICAL TEST THEORY -"),
              shiny::div("RELIABLITY AND ITEM ANALYSIS.")
            )
          ),
          shiny::div(class = "rsp-sidebar-credit", "Doan & Aybek (2022)")
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
                tags$b("RSP"),
                shiny::span("Package")
              ),
              shiny::div(
                class = "ctt-brand-text",
                shiny::div(class = "ctt-package", "RSP Package"),
                shiny::div(class = "ctt-domain", "R-Shiny ", tags$b(""), " Psychometry")
              )
            ),
            shiny::h2(
              class = "ctt-main-title ctt-main-title-08",
              shiny::span(class = "ctt-title-top", "CLASSICAL TEST THEORY"),
              shiny::span(class = "ctt-title-sub", "RELIABLITY AND ITEM ANALYSIS")
            ),
            shiny::p(class = "ctt-desc ctt-desc-08", )
          ),
          shiny::div(
            class = "ctt-panels",
            shiny::div(
              class = "ctt-card ctt-reliability",
              shiny::div(class = "ctt-card-title", "RELIABILITY"),
              shiny::div(
                class = "ctt-ring",
                shiny::div(
                  class = "ctt-ring-inner",
                  tags$b("0.87"),
                  shiny::span("Cronbach's ")
                )
              )
            ),
            shiny::div(
              class = "ctt-card ctt-difficulty",
              shiny::div(class = "ctt-card-title", "ITEM DIFFICULTY"),
              shiny::div(
                class = "ctt-bars",
                lapply(1:12, function(i) shiny::span())
              ),
              shiny::div(class = "ctt-curve"),
              shiny::div(
                class = "ctt-axis-labels",
                shiny::span("EASY"), shiny::span("MODERATE"), shiny::span("HARD")
              )
            ),
            shiny::div(
              class = "ctt-card ctt-discrimination",
              shiny::div(class = "ctt-card-title", "ITEM DISCRIMINATION"),
              shiny::div(
                class = "ctt-scatter",
                lapply(1:12, function(i) shiny::span(class = paste0("ctt-dot d", i)))
              )
            ),
            shiny::div(
              class = "ctt-card ctt-table-card",
              tags$table(
                class = "ctt-mini-table",
                tags$thead(tags$tr(
                  tags$th("ITEM"), tags$th("DIFFICULTY (b)"), tags$th("DISCRIMINATION (a)")
                )),
                tags$tbody(
                  tags$tr(tags$td("Q1"), tags$td("-1.20"), tags$td("1.25")),
                  tags$tr(tags$td("Q2"), tags$td("-0.65"), tags$td("0.98")),
                  tags$tr(tags$td("Q3"), tags$td("0.10"), tags$td("1.34")),
                  tags$tr(tags$td("Q4"), tags$td("0.85"), tags$td("1.10")),
                  tags$tr(tags$td("..."), tags$td("..."), tags$td("..."))
                )
              )
            ),
            shiny::div(
              class = "ctt-card ctt-score",
              shiny::div(class = "ctt-card-title", "SCORE ANALYSIS"),
              shiny::div(class = "ctt-score-curve"),
              shiny::div(
                class = "ctt-score-stats",
                shiny::div(shiny::span("MEAN"), "79.3"),
                shiny::div(shiny::span("SD"), "12.1")
              )
            )
          ),
          shiny::div(
            class = "ctt-steps",
            shiny::div(class = "ctt-step", "ITEM QUALITY"),
            shiny::div(class = "ctt-step", "ANALYZE"),
            shiny::div(class = "ctt-step", "SUMMARIZE"),
            shiny::div(class = "ctt-step", "EVALUATE")
          ),
          shiny::div(
            class = "ctt-bottom",
            shiny::div(
              class = "ctt-feature",
              shiny::span(class = "ctt-emoji", "\U0001F4CA"),
              shiny::span("RELIABILITY")
            ),
            shiny::div(
              class = "ctt-feature",
              shiny::span(class = "ctt-emoji", "\U0001F4CB"),
              shiny::span("ITEM ANALYSIS")
            ),
            shiny::div(
              class = "ctt-feature",
              shiny::span(class = "ctt-emoji", "\U0001F4C8"),
              shiny::span("GRAPHICS")
            )
          ),
          shiny::div(class = "ctt-credit", "Doan & Aybek (2022)")
        )
      )
    }

    shiny::div(
      class = paste("rsp-visual-card", mode),
      shiny::div(
        class = "rsp-visual-content",
        shiny::div(class = "rsp-pill", shiny::span(class = "rsp-mark"), shiny::span("RSP Package")),
        shiny::h2(class = "rsp-title", "ITEM", shiny::span("ANALYSIS")),
        shiny::div(
          class = "rsp-mini-grid",
          shiny::div(class = "rsp-chip", shiny::span(class = "rsp-dot"), "RELIABILITY"),
          shiny::div(class = "rsp-chip", shiny::span(class = "rsp-dot"), "ITEM DIFFICULTY"),
          shiny::div(class = "rsp-chip", shiny::span(class = "rsp-dot"), "DISCRIMINATION"),
          shiny::div(class = "rsp-chip", shiny::span(class = "rsp-dot"), "SCORE ANALYSIS")
        )
      ),
      shiny::div(class = "rsp-credit", "Doan & Aybek (2022)")
    )
  }


  ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(theme_css))),
    tags$head(tags$style(id = "dynamic-theme-style", "")),
    theme = shinythemes::shinytheme("flatly"),

    ## Tooltips
    shinyBS::bsTooltip("type", "For option matrix: first row = answer key.", "bottom", "focus"),
    shinyBS::bsTooltip("type2", "Select the correct file format.", "right", "hover"),
    shinyBS::bsTooltip("crbh", "Cronbach Alpha updates as you add/remove items.", "bottom", "hover"),

    ## Title bar
    div(
      id = "tepe",
      fluidRow(
        column(7, h1(id = "title", "CLASSICAL TEST THEORY - RELIABLITY AND ITEM ANALYSIS")),
        column(5, h1(id = "title2", "RSP PACKAGE \u2014 CRAN"))
      )
    ),
    sidebarPanel(
      width = 4,

      ## Introduction panel
      conditionalPanel(
        condition = "input.panel==0",
        rspInfoCard("sidebar-mini"),
        tags$head(tags$script(HTML(js))),
        br(), br(),
        textOutput("browser"),
        tags$head(tags$style(
          "#browser{color:#3B1F5E;font-size:16px;font-family:cursive;font-style:oblique;text-align:center;}"
        )),
        br(),
        shinyWidgets::spectrumInput(
          inputId = "myColor",
          label = tags$b("CHANGE THEME COLOR:"),
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

      ## Data Upload panel
      conditionalPanel(
        condition = "input.panel==1",
        rspInfoCard("sidebar-mini"),
        br(),
        shinyWidgets::radioGroupButtons(
          inputId   = "type",
          label     = tags$b("Select Data Type"),
          choices   = list("Option Matrix" = 1, "1-0 Matrix" = 2),
          justified = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        div(
          class = "key-warning",
          HTML("<marquee direction='left' scrollamount='5'>
              \u26A0 FIRST ROW OF OPTION MATRIX MUST BE THE ANSWER KEY!
            </marquee>")
        ),
        shinyWidgets::pickerInput(
          inputId = "type2",
          label = tags$b("Select File Format"),
          choices = list(
            "CSV - Semicolon Separated" = 1,
            "CSV - Comma Separated"     = 2,
            "SAV - SPSS"                = 3,
            "XLSX - Excel"              = 4
          ),
          selected = 3,
          options = shinyWidgets::pickerOptions(showTick = TRUE)
        ),
        uiOutput("uiHeader"),
        fileInput("data1", tags$b("Upload Data File"))
      ),

      ## Item Analysis panel
      conditionalPanel(
        condition = "input.panel==2",
        rspInfoCard("sidebar-mini"),
        br(),
        gt::gt_output("crbh"),
        br(),
        uiOutput("rmItem"),
        br(),
        # All action buttons in one centered block
        div(
          style = "display:flex; flex-direction:column; align-items:center; gap:12px;",
          uiOutput("uyg"),
          shinyWidgets::dropMenu(
            padding = "20px", theme = "light-border", placement = "right-end",
            shinyWidgets::actionBttn(
              "acb2", "TEST STATISTICS & RELIABILITY",
              style = "jelly", color = "primary"
            ),
            withLoader(gt::gt_output("table3"), type = "html", loader = "loader1"),
            br(), br(),
            gt::gt_output("table3.1"),
            br(), br(),
            gt::gt_output("table3.2"),
            br()
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            div(
              style = "display:flex;justify-content:center;",
              shinyWidgets::downloadBttn("dlItemAn", "DOWNLOAD ITEM ANALYSIS",
                style = "unite", color = "primary", size = "sm",
                no_outline = TRUE, icon = shiny::icon("download")
              )
            )
          ),
          column(
            6,
            div(
              style = "display:flex;justify-content:center;",
              shinyWidgets::downloadBttn("downloadTestIst", "DOWNLOAD TEST STATS",
                style = "unite", color = "primary", size = "sm",
                no_outline = TRUE, icon = shiny::icon("download")
              )
            )
          )
        )
      ),

      ## Graphics panel
      conditionalPanel(
        condition = "input.panel==4",
        rspInfoCard("sidebar-mini"),
        br(),
        uiOutput("graphitem"),
        br(),
        shinyWidgets::radioGroupButtons(
          inputId = "distGraph",
          label = tags$b("Select Graph Type"),
          choices = list(
            "Option Selection Rate"     = 1,
            "Distractor Discrimination" = 2
          ),
          justified = TRUE, direction = "vertical", status = "primary",
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        br(), br(),
        uiOutput("dlDistractor")
      )
    ), # end sidebarPanel

    mainPanel(
      tabsetPanel(
        id = "panel",
        tabPanel(h4(id = "a", "INTRODUCTION"),
          value = 0,
          br(),
          rspInfoCard("hero")
        ),
        tabPanel(h4(id = "a", "DATA UPLOAD"),
          value = 1,
          br(),
          div(class = "section-header", textOutput("text1")),
          br(),
          withLoader(DT::dataTableOutput("dat1"), type = "html", loader = "loader1"),
          br(),
          div(class = "section-header", textOutput("dat3")),
          br(),
          withLoader(gt::gt_output("dat2"), type = "html", loader = "loader5")
        ),
        tabPanel(h4(id = "a", "ITEM ANALYSIS & TEST STATISTICS"),
          value = 2,
          br(),
          div(class = "section-header", textOutput("text2")),
          br(),
          # table1_dt = initial full analysis; buton_dt = post-removal analysis
          # Only one is visible at a time, toggled by observeEvent(omit)
          withLoader(DT::dataTableOutput("table1_dt"), type = "html", loader = "loader1"),
          DT::dataTableOutput("buton_dt"),
          # invisible gt output for download / test-stats side effects
          shinyjs::hidden(gt::gt_output("print_helper"))
        ),
        tabPanel(h4(id = "a", "GRAPHICS"),
          value = 4,
          br(),
          div(class = "section-header", textOutput("text5")),
          br(),
          textOutput("warning2"),
          tags$head(tags$style(
            "#warning2{color:red;font-size:16px;font-style:oblique;text-align:center;}"
          )),
          uiOutput("graphUI")
        )
      ) # end tabsetPanel
    ) # end mainPanel
  ) # end fluidPage

  ################################## SERVER ##############################

  server <- function(input, output, session) {
    # ---- Reactive value to track whether removal has been applied ----
    rv <- reactiveValues(removed = FALSE)

    # ---- Helper: single-item ggplot selection-rate bar chart ----
    make_sel_plot <- function(rem_data, rem_key, item_name, theme_col = "#6B3FA0") {
      tbl <- prop.table(table(rem_data[[item_name]]))
      df_plot <- data.frame(
        Option = names(tbl), Rate = as.numeric(tbl), stringsAsFactors = FALSE
      )
      key_idx <- which(colnames(rem_data) == item_name)
      if (length(key_idx) == 0) key_idx <- 1
      key_val <- as.character(rem_key[key_idx])
      df_plot$Correct <- df_plot$Option == key_val

      col_base <- theme_col
      col_dist <- grDevices::adjustcolor(theme_col, alpha.f = 0.40)

      ggplot2::ggplot(df_plot, ggplot2::aes(
        x = reorder(Option, -Rate), y = Rate,
        fill = Correct, color = Correct
      )) +
        ggplot2::geom_col(
          ggplot2::aes(linetype = Correct),
          width = 0.65, alpha = 0.85, size = 1
        ) +
        # overlay diagonal-line pattern on correct bar only
        ggplot2::geom_col(
          data = df_plot[df_plot$Correct, , drop = FALSE],
          ggplot2::aes(x = reorder(Option, -Rate), y = Rate),
          fill = "white", alpha = 0.25,
          width = 0.65, inherit.aes = FALSE
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::percent(Rate, accuracy = 0.1)),
          vjust = -0.35, size = 3.6, fontface = "bold", color = "#1a1a2e"
        ) +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = col_base, "FALSE" = col_dist),
          labels = c("TRUE" = paste0("Correct (", key_val, ")"), "FALSE" = "Distractor"),
          name = NULL, drop = FALSE
        ) +
        ggplot2::scale_color_manual(
          values = c("TRUE" = col_base, "FALSE" = col_dist),
          guide  = "none"
        ) +
        ggplot2::scale_linetype_manual(
          values = c("TRUE" = "solid", "FALSE" = "solid"),
          guide  = "none"
        ) +
        # thick border on correct bar to visually mark it
        ggplot2::geom_col(
          data = df_plot[df_plot$Correct, , drop = FALSE],
          ggplot2::aes(x = reorder(Option, -Rate), y = Rate),
          fill = NA, color = col_base, linewidth = 2,
          width = 0.65, inherit.aes = FALSE
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          limits = c(0, 1.12), expand = c(0, 0)
        ) +
        ggplot2::labs(
          title = item_name,
          subtitle = paste0("Correct Answer: ", key_val),
          x = NULL, y = "Selection Rate"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          plot.title         = ggplot2::element_text(face = "bold", color = "#3B1F5E", size = 12),
          plot.subtitle      = ggplot2::element_text(color = "#6B3FA0", size = 9),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor   = ggplot2::element_blank(),
          legend.position    = "top",
          legend.text        = ggplot2::element_text(size = 8),
          axis.text          = ggplot2::element_text(size = 9),
          plot.background    = ggplot2::element_rect(fill = "white", color = NA)
        )
    }

    # ---- ITEMAN_12 FIX: Distractor Discrimination grafiklerini tekil ya da toplu izime uygun grob/ggplot nesnesine evirir ----
    as_drawable_plot <- function(p) {
      if (inherits(p, "gg") || inherits(p, "ggplot")) {
        return(p)
      }
      if (inherits(p, "grob") || inherits(p, "gtable")) {
        return(cowplot::ggdraw() + cowplot::draw_grob(p))
      }
      if (is.list(p)) {
        plist <- lapply(p, as_drawable_plot)
        return(cowplot::plot_grid(plotlist = plist, ncol = 1, align = "v"))
      }
      cowplot::ggdraw() + cowplot::draw_label("Graph could not be rendered.", size = 14)
    }

    make_distractor_plot <- function(rem_data, rem_key, item_name) {
      idx <- which(colnames(rem_data) == item_name)
      if (length(idx) == 0) {
        return(cowplot::ggdraw() + cowplot::draw_label(paste("Item not found:", item_name), size = 14))
      }
      p <- ShinyItemAnalysis::plotDistractorAnalysis(data = rem_data, key = rem_key, item = idx)
      as_drawable_plot(p)
    }

    draw_distractor_grid <- function(rem_data, rem_key, items_to_plot, ncol = 2, theme_col = "#6B3FA0") {
      plots <- lapply(items_to_plot, function(nm) make_distractor_plot(rem_data, rem_key, nm))
      if (length(plots) == 0) {
        return(invisible(NULL))
      }
      if (length(plots) == 1) {
        print(plots[[1]])
      } else {
        print(cowplot::plot_grid(plotlist = plots, ncol = ncol, align = "hv"))
      }
      invisible(NULL)
    }

    # ---- Color picker  CSS variable injection ----
    observeEvent(input$myColor, {
      col <- input$myColor
      css <- sprintf(
        ":root{--tc:%s;--tdk:%s;--tlt:%s;--tpl:%s;--tls:color-mix(in srgb, %s 14%%, #ffffff);}
         body{background:#ffffff !important;}",
        col, col, col, col, col
      )
      shinyjs::runjs(sprintf(
        "document.getElementById('dynamic-theme-style').innerHTML=`%s`;",
        gsub("`", "'", css)
      ))
      shinyjs::runjs(sprintf(
        "document.body.style.background='linear-gradient(to bottom right,#ffffff,color-mix(in srgb, %s 10%%, #ffffff))';", col
      ))
    })

    output$browser <- renderText({
      req(input$myBrowser)
      if (input$myBrowser == "Chrome 102") "Please click 'Open in Browser'" else ""
    })

    ######################## DATA ########################

    data <- reactive({
      veri <- input$data1
      if (is.null(veri)) {
        return(data.frame(Info = "PLEASE UPLOAD DATA"))
      }
      ext <- tools::file_ext(veri$datapath)
      if (input$type2 == 1) {
        if (ext != "csv") {
          return(data.frame(Warning = "WRONG FORMAT"))
        }
        utils::read.csv2(veri$datapath, header = isTRUE(input$header), sep = ";")
      } else if (input$type2 == 2) {
        if (ext != "csv") {
          return(data.frame(Warning = "WRONG FORMAT"))
        }
        utils::read.csv(veri$datapath, header = isTRUE(input$header))
      } else if (input$type2 == 3) {
        if (ext != "sav") {
          return(data.frame(Warning = "WRONG FORMAT"))
        }
        foreign::read.spss(veri$datapath, to.data.frame = TRUE, use.value.labels = FALSE)
      } else {
        if (ext != "xlsx") {
          return(data.frame(Warning = "WRONG FORMAT"))
        }
        xlsx::read.xlsx(veri$datapath, 1, header = isTRUE(input$header))
      }
    })

    # Answer key = first row (option matrix); character vector
    keyA <- reactive({
      as.character(as.matrix(data()[1, ]))
    })

    # Scored data: strip first row for option matrix
    datA <- reactive({
      d <- data()
      colnames(d) <- paste0("item", 1:ncol(d))
      if (input$type == 1) d[-1, ] else d
    })

    names_r <- reactive(colnames(datA()))

    # Data with selected items removed
    datARem <- reactive({
      omit <- input$itm
      remain <- setdiff(names_r(), omit)
      ITEMAN_ENV$remain_items <- remain
      datA()[, remain, drop = FALSE]
    })

    # Key for remaining items
    keyARem <- reactive({
      omit <- input$itm
      remain <- setdiff(names_r(), omit)
      kv <- keyA()
      names(kv) <- names_r()
      unname(kv[remain])
    })

    ###################### DATA UPLOAD TAB ######################

    output$text1 <- renderText({
      req(input$data1)
      "DATA UPLOAD & BASIC STATISTICS"
    })
    output$dat3 <- renderText({
      req(input$data1)
      "DATA UPLOADED SUCCESSFULLY \u2713"
    })

    output$dat1 <- DT::renderDataTable({
      req(input$data1)
      d <- data()
      if (ncol(d) == 1) {
        return(data.frame(WARNING = "INCORRECT FORMAT"))
      }
      colnames(d) <- paste0("item", 1:ncol(d))
      DT::datatable(d,
        options = list(
          pageLength = 15, scrollX = TRUE, scrollY = "350px",
          scrollCollapse = TRUE, dom = "tip"
        ),
        rownames = FALSE, class = "stripe hover compact", extensions = "Scroller"
      )
    })

    output$dat2 <- render_gt(align = "center", {
      req(input$data1)
      tc <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      tc_light <- grDevices::adjustcolor(tc, alpha.f = 0.18)
      d <- data()
      gt::gt(data.frame(
        NUMBER_OF_ITEMS       = ncol(d),
        NUMBER_OF_RESPONDENTS = if (input$type == 1) nrow(d) - 1 else nrow(d),
        NUMBER_OF_BLANKS      = sum(is.na(d))
      )) %>%
        gt::tab_header(title = gt::md("*Basic Data Statistics*")) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc_light),
            gt::cell_text(color = "#1a1a2e", weight = "bold")
          ),
          locations = gt::cells_body()
        ) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc),
            gt::cell_text(color = "#ffffff", weight = "bold")
          ),
          locations = gt::cells_column_labels()
        ) %>%
        gt::cols_width(gt::everything() ~ gt::px(230)) %>%
        gt::tab_options(
          heading.title.font.size   = gt::px(15),
          heading.title.font.weight = "bold",
          column_labels.font.size   = gt::px(13),
          column_labels.font.weight = "bold"
        )
    })

    ###################### ITEM ANALYSIS TAB ######################

    output$text2 <- renderText({
      req(input$data1)
      "ITEM ANALYSIS RESULTS"
    })

    output$rmItem <- renderUI({
      req(input$data1)
      shinyWidgets::pickerInput("itm", tags$b("Select Items to Remove"),
        choices = names_r(), options = list(`actions-box` = TRUE), multiple = TRUE
      )
    })

    # ---- Initial full-data item table ----
    output$table1_dt <- DT::renderDataTable({
      req(input$data1)
      scoreData <- ITEMAN_ENV$scoreData
      calcStats <- ITEMAN_ENV$calcItemStats
      renderDT <- ITEMAN_ENV$renderItemDT

      if (input$type == 2) {
        d01 <- datA()
        for (i in 1:ncol(d01)) d01[is.na(d01[, i]), i] <- 0
        res <- calcStats(d01)
      } else {
        res <- calcStats(scoreData(datA(), keyA()))
      }
      ITEMAN_ENV$itemResult_df <- res
      renderDT(res)
    })

    # ---- Remove button ----
    output$uyg <- renderUI({
      req(input$data1)
      if (!is.null(input$itm) && length(input$itm) > 0) {
        shinyWidgets::actionBttn("omit", "Remove Selected Item(s)",
          size = "lg", color = "primary", no_outline = TRUE
        )
      }
    })

    # ---- observeEvent: apply removal  show new table, hide original ----
    observeEvent(input$omit, {
      rv$removed <- TRUE
      shinyjs::hide("table1_dt")
      shinyjs::show("buton_dt")
    })

    output$buton_dt <- DT::renderDataTable({
      # Only render after removal is triggered
      req(rv$removed)
      renderDT <- ITEMAN_ENV$renderItemDT

      omit <- isolate(input$itm)
      remain <- setdiff(isolate(names_r()), omit)
      drem <- isolate(datA())[, remain, drop = FALSE]

      if (isolate(input$type) == 2) {
        d01 <- drem
        for (i in 1:ncol(d01)) d01[is.na(d01[, i]), i] <- 0
        res <- ITEMAN_ENV$calcItemStats(d01)
      } else {
        kv <- isolate(keyA())
        names(kv) <- isolate(names_r())
        k_rem <- unname(kv[remain])
        res <- ITEMAN_ENV$calcItemStats(ITEMAN_ENV$scoreData(drem, k_rem))
      }
      res$Items <- remain
      ITEMAN_ENV$rmdat <- res
      ITEMAN_ENV$RemovedItemResult_df <- res
      renderDT(res)
    })

    # Reset: when itm selection changes back to NULL, show original table
    observeEvent(input$itm,
      {
        if (is.null(input$itm) || length(input$itm) == 0) {
          rv$removed <- FALSE
          shinyjs::show("table1_dt")
          shinyjs::hide("buton_dt")
        }
      },
      ignoreNULL = FALSE
    )

    # ---- Download: Item Analysis  Excel ----
    output$dlItemAn <- downloadHandler(
      filename = function() "item_analysis.xlsx",
      content = function(file) {
        dat <- if (!is.null(ITEMAN_ENV$rmdat)) {
          ITEMAN_ENV$rmdat
        } else {
          ITEMAN_ENV$itemResult_df
        }
        openxlsx::write.xlsx(as.data.frame(dat), file, rowNames = FALSE)
      }
    )

    # ---- Download: Test Statistics  Excel ----
    output$downloadTestIst <- downloadHandler(
      filename = function() "test_statistics.xlsx",
      content = function(file) {
        r1 <- as.data.frame(ITEMAN_ENV$saveist.1)
        r2 <- as.data.frame(ITEMAN_ENV$saveist.2)
        r3 <- as.data.frame(ITEMAN_ENV$saveist.3)
        s <- t(cbind(r1, r2, r3)[, -1, drop = FALSE])
        colnames(s) <- "Value"
        openxlsx::write.xlsx(as.data.frame(s), file, rowNames = TRUE)
      }
    )

    # ---- Download: Graphs  JPEG ----
    # ITEMAN_10 FIX: Grafik indirme ilemi HTML hata sayfas retmesin diye
    # ekranda izilen grafik mant ile ayn mantk burada da kuruldu.
    # - Dosya ad artk .jpeg uzantldr.
    # - oktan semeli veride doru anahtara gre seenek seim grafikleri indirilir.
    # - 1-0 veride anahtar gerekmeden doru/yanl oran grafikleri indirilir.
    # - Seili tek madde varsa yalnzca o madde; ALL ITEMS seiliyse tm maddeler indirilir.
    # - Distractor Discrimination sadece tek madde iin gvenli biimde JPEG'e baslr.
    output$downloadGraph <- downloadHandler(
      filename = function() {
        paste0("ITEMAN_graphs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpeg")
      },
      contentType = "image/jpeg",
      content = function(file) {
        rd <- datARem()
        req(!is.null(rd), ncol(rd) > 0)

        remv <- colnames(rd)
        tc <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) {
          input$myColor
        } else {
          "#6B3FA0"
        }

        graph_type <- input$distGraph
        data_type <- input$type
        sel <- input$graphItem
        if (is.null(sel) || length(sel) == 0) sel <- "_ALL_"

        items_to_plot <- if (identical(sel, "_ALL_")) remv else sel
        items_to_plot <- intersect(items_to_plot, remv)
        if (length(items_to_plot) == 0) items_to_plot <- remv[1]

        # ITEMAN_12 FIX: JPEG boyutu grafik trne gre ayarlanr; distractor grid daha okunakl tutulur.
        if (isTRUE(graph_type == 2)) {
          ncols_p <- if (length(items_to_plot) == 1) 1 else min(2, length(items_to_plot))
          nrows_p <- ceiling(length(items_to_plot) / ncols_p)
          img_w <- if (length(items_to_plot) == 1) 1600 else ncols_p * 1600
          img_h <- if (length(items_to_plot) == 1) 1100 else nrows_p * 1200
        } else {
          ncols_p <- if (length(items_to_plot) == 1) 1 else min(3, length(items_to_plot))
          nrows_p <- ceiling(length(items_to_plot) / ncols_p)
          img_w <- if (length(items_to_plot) == 1) 1600 else ncols_p * 1400
          img_h <- if (length(items_to_plot) == 1) 1100 else nrows_p * 1000
        }

        grDevices::jpeg(
          filename = file,
          width = img_w, height = img_h,
          units = "px", res = 150, quality = 95,
          bg = "white"
        )
        on.exit(grDevices::dev.off(), add = TRUE)

        if (isTRUE(graph_type == 2)) {
          # ITEMAN_12 FIX: Distractor Discrimination artk ALL ITEMS seiliyken de tm maddeler iin tek JPEG iinde indirilir.
          if (!isTRUE(data_type == 1)) {
            graphics::plot.new()
            graphics::text(0.5, 0.5,
              "Distractor Discrimination is not available for 1-0 scored data.",
              cex = 1.4, col = tc, font = 2
            )
          } else {
            rk <- keyARem()
            ncols_d <- if (length(items_to_plot) == 1) 1 else min(2, length(items_to_plot))
            draw_distractor_grid(rd, rk, items_to_plot, ncol = ncols_d, theme_col = tc)
          }
        } else {
          # ITEMAN_10 FIX: Option Selection Rate indirme ilemi hem oktan semeli hem 1-0 veri iin alr.
          if (isTRUE(data_type == 1)) {
            rk <- keyARem()
            plots <- lapply(items_to_plot, function(nm) {
              make_sel_plot(rd, rk, nm, theme_col = tc)
            })
          } else {
            col_correct <- tc
            col_incorrect <- grDevices::adjustcolor(tc, alpha.f = 0.45)
            plots <- lapply(items_to_plot, function(nm) {
              x <- suppressWarnings(as.numeric(as.character(rd[[nm]])))
              p_val <- mean(x, na.rm = TRUE)
              if (is.nan(p_val)) p_val <- 0
              df <- data.frame(
                Category = c("Correct", "Incorrect"),
                Rate = c(p_val, 1 - p_val)
              )
              ggplot2::ggplot(df, ggplot2::aes(x = Category, y = Rate, fill = Category)) +
                ggplot2::geom_col(width = .55, alpha = .88) +
                ggplot2::geom_text(ggplot2::aes(label = scales::percent(Rate, accuracy = .1)),
                  vjust = -.35, size = 3.6, fontface = "bold"
                ) +
                ggplot2::scale_fill_manual(
                  values = c(
                    "Correct" = col_correct,
                    "Incorrect" = col_incorrect
                  ),
                  guide = "none"
                ) +
                ggplot2::scale_y_continuous(
                  labels = scales::percent_format(accuracy = 1),
                  limits = c(0, 1.12), expand = c(0, 0)
                ) +
                ggplot2::labs(title = nm, x = NULL, y = "Rate") +
                ggplot2::theme_minimal(base_size = 11) +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(face = "bold", color = tc, size = 12),
                  panel.grid.major.x = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank(),
                  plot.background = ggplot2::element_rect(fill = "white", color = NA)
                )
            })
          }

          if (length(plots) == 1) {
            print(plots[[1]])
          } else {
            print(
              patchwork::wrap_plots(plots, ncol = ncols_p) +
                patchwork::plot_annotation(
                  title = "Option Selection Rate  Selected Items",
                  theme = ggplot2::theme(
                    plot.title = ggplot2::element_text(
                      face = "bold", color = tc,
                      size = 18, hjust = .5
                    )
                  )
                )
            )
          }
        }
      }
    )

    # Hidden gt placeholder (kept for UI reference only, no download logic inside)
    output$print_helper <- render_gt({
      req(input$data1)
      return(NULL)
    })

    #### Cronbach Alpha widget ####

    output$crbh <- render_gt({
      req(input$data1)
      d <- if (!is.null(input$itm) && length(input$itm) > 0) datARem() else datA()
      k <- if (!is.null(input$itm) && length(input$itm) > 0) keyARem() else keyA()

      if (input$type == 1) {
        d01 <- ITEMAN_ENV$scoreData(d, k)
      } else {
        d01 <- d
        for (i in 1:ncol(d01)) d01[is.na(d01[, i]), i] <- 0
      }
      d01 <- na.omit(d01)
      n <- ncol(d01)
      cr <- n / (n - 1) * (1 - sum(apply(d01, 2, var)) / var(rowSums(d01)))

      gt::gt(data.frame(Cronbach_Alpha = round(cr, 4))) %>%
        gt::tab_header(title = gt::md("*Current Cronbach \u03b1*")) %>%
        gt::tab_style(style = gt::cell_fill(color = "#C084FC", alpha = .25), locations = gt::cells_body()) %>%
        gt::tab_options(column_labels.font.size = gt::px(14), column_labels.font.weight = "bold")
    })

    ###################### TEST STATISTICS ######################

    # Shared: build scored 0/1 data for stats
    scored_data <- reactive({
      req(input$data1)
      d <- if (!is.null(input$itm) && length(input$itm) > 0) datARem() else datA()
      k <- if (!is.null(input$itm) && length(input$itm) > 0) keyARem() else keyA()
      if (input$type == 1) {
        ITEMAN_ENV$scoreData(d, k)
      } else {
        d01 <- d
        for (i in 1:ncol(d01)) d01[is.na(d01[, i]), i] <- 0
        d01
      }
    })

    output$table3 <- render_gt(align = "left", {
      req(input$data1)
      d01 <- na.omit(scored_data())

      two.halves <- function(x) {
        L <- ncol(x)
        colnames(x) <- 1:L
        s1 <- rowSums(x[, seq(1, L, 2), drop = FALSE])
        s2 <- rowSums(x[, seq(2, L, 2), drop = FALSE])
        r <- cor(s1, s2)
        (2 * r) / (1 + r)
      }
      cronb <- function(x) {
        n <- ncol(x)
        n / (n - 1) * (1 - sum(apply(x, 2, var)) / var(rowSums(x)))
      }
      kr20 <- function(x) {
        n <- ncol(x)
        pqs <- sum(apply(x, 2, function(c) {
          p <- mean(c)
          p * (1 - p)
        }))
        n / (n - 1) * (1 - pqs / var(rowSums(x)))
      }
      skew <- function(x) sum((x - mean(x))^3) / (length(x) * sd(x)^3)
      kurt <- function(x) sum((x - mean(x))^4) / (length(x) * sd(x)^4) - 3

      ist <- data.frame(
        Two_Halves = round(two.halves(d01), 4), Cronbach_Alpha = round(cronb(d01), 4),
        KR20 = round(kr20(d01), 4), Skewness = round(skew(rowSums(d01)), 4),
        Kurtosis = round(kurt(rowSums(d01)), 4)
      )
      ITEMAN_ENV$saveist.1 <- ist
      tc <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      tc_light <- grDevices::adjustcolor(tc, alpha.f = 0.18)
      gt::gt(ist) %>%
        gt::tab_header(title = gt::md("**TEST STATISTICS**")) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc_light),
            gt::cell_text(color = "#1a1a2e", weight = "bold")
          ),
          locations = gt::cells_body()
        ) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc),
            gt::cell_text(color = "#ffffff", weight = "bold")
          ),
          locations = gt::cells_column_labels()
        ) %>%
        gt::cols_width(gt::everything() ~ gt::px(180)) %>%
        gt::tab_options(
          heading.title.font.size = gt::px(18),
          column_labels.font.size = gt::px(13), column_labels.font.weight = "bold"
        )
    })

    output$table3.1 <- render_gt(align = "left", {
      req(input$data1)
      tc <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      tc_light <- grDevices::adjustcolor(tc, alpha.f = 0.12)
      d01 <- na.omit(scored_data())
      ist <- data.frame(
        N = nrow(d01), N_Items = ncol(d01),
        Mean = round(mean(rowSums(d01)), 4), SD = round(sd(rowSums(d01)), 4),
        Variance = round(var(rowSums(d01)), 4)
      )
      ITEMAN_ENV$saveist.2 <- ist
      gt::gt(ist) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc_light),
            gt::cell_text(color = "#1a1a2e", weight = "bold")
          ),
          locations = gt::cells_body()
        ) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc),
            gt::cell_text(color = "#ffffff", weight = "bold")
          ),
          locations = gt::cells_column_labels()
        ) %>%
        gt::cols_width(gt::everything() ~ gt::px(180)) %>%
        gt::tab_options(column_labels.font.size = gt::px(13), column_labels.font.weight = "bold")
    })

    output$table3.2 <- render_gt(align = "left", {
      req(input$data1)
      tc <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      tc_xlt <- grDevices::adjustcolor(tc, alpha.f = 0.08)
      d01 <- na.omit(scored_data())
      rd <- if (!is.null(ITEMAN_ENV$RemovedItemResult_df)) {
        ITEMAN_ENV$RemovedItemResult_df
      } else if (!is.null(ITEMAN_ENV$itemResult_df)) {
        ITEMAN_ENV$itemResult_df
      } else {
        return(NULL)
      }
      ist <- data.frame(
        Min = min(rowSums(d01)), Max = max(rowSums(d01)),
        Avg_Difficulty = round(mean(rd$Item_Difficulty, na.rm = TRUE), 4),
        Avg_Biserial = round(mean(rd$Biserial, na.rm = TRUE), 4),
        Avg_PBiserial = round(mean(rd$Point_Biserial, na.rm = TRUE), 4)
      )
      ITEMAN_ENV$saveist.3 <- ist
      gt::gt(ist) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc_xlt),
            gt::cell_text(color = "#1a1a2e", weight = "bold")
          ),
          locations = gt::cells_body()
        ) %>%
        gt::tab_style(
          style = list(
            gt::cell_fill(color = tc),
            gt::cell_text(color = "#ffffff", weight = "bold")
          ),
          locations = gt::cells_column_labels()
        ) %>%
        gt::cols_width(gt::everything() ~ gt::px(180)) %>%
        gt::tab_options(column_labels.font.size = gt::px(13), column_labels.font.weight = "bold")
    })

    ###################### GRAPHICS ######################

    output$text5 <- renderText({
      req(input$data1)
      "GRAPHS: DISTRACTOR ANALYSIS"
    })
    output$warning2 <- renderText({
      if (isTRUE(input$type == 2)) "Distractor graphs not available for 1-0 scored items."
    })

    output$graphitem <- renderUI({
      req(input$data1)
      remv <- colnames(datARem())
      choices <- c("ALL ITEMS" = "_ALL_", setNames(remv, remv))
      shinyWidgets::pickerInput("graphItem", tags$b("Select Item"),
        choices = choices, selected = "_ALL_", multiple = FALSE,
        options = shinyWidgets::pickerOptions(showTick = TRUE, liveSearch = TRUE)
      )
    })

    # Dynamic container: single plot or grid
    output$graphUI <- renderUI({
      req(input$data1, input$graphItem)
      if (isTRUE(input$type == 2)) {
        return(
          div(
            style = "color:red;text-align:center;font-size:16px;padding:20px;",
            "Not available for 1-0 scored data."
          )
        )
      }
      sel <- input$graphItem
      remv <- colnames(datARem())
      n <- if (sel == "_ALL_") length(remv) else 1
      # ITEMAN_12 FIX: Distractor Discrimination ALL ITEMS grnmnde grafik ykseklii artrld.
      if (isTRUE(input$distGraph == 2)) {
        ncols <- if (n == 1) 1 else 2
        nrows <- ceiling(n / ncols)
        h <- if (n == 1) 520 else nrows * 520
      } else {
        ncols <- 3
        nrows <- ceiling(n / ncols)
        h <- if (n == 1) 480 else nrows * 300
      }

      tagList(
        plotOutput("graph_out", height = paste0(h, "px"))
      )
    })

    output$graph_out <- renderPlot({
      req(input$data1, input$graphItem)
      if (isTRUE(input$type == 2)) {
        return(NULL)
      }

      # Snapshot all reactive values at once to avoid cascading invalidations
      sel <- input$graphItem
      graph_type <- input$distGraph
      data_type <- input$type
      theme_col <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) {
        input$myColor
      } else {
        "#6B3FA0"
      }

      rem_data <- datARem()
      rem_key <- if (data_type == 1) keyARem() else NULL
      # remv must come from the actual data column names (works for ALL ITEMS too)
      remv <- colnames(rem_data)

      use_key <- isTRUE(data_type == 1)

      items_to_plot <- if (sel == "_ALL_") remv else sel

      if (graph_type == 2) {
        # --- ITEMAN_12 FIX: Distractor Discrimination ALL ITEMS seiliyken tm maddeler tek seferde gsterilir. ---
        if (!use_key) {
          return(NULL)
        }
        ncols_d <- if (length(items_to_plot) == 1) 1 else min(2, length(items_to_plot))
        draw_distractor_grid(rem_data, rem_key, items_to_plot, ncol = ncols_d, theme_col = theme_col)
      } else {
        # --- Option Selection Rate (ggplot) ---
        if (use_key) {
          plots <- lapply(items_to_plot, function(nm) {
            make_sel_plot(rem_data, rem_key, nm, theme_col = theme_col)
          })
        } else {
          # 1-0 mode: proportion correct per item as simple bar
          col_correct <- theme_col
          col_incorrect <- grDevices::adjustcolor(theme_col, alpha.f = 0.45)
          plots <- lapply(items_to_plot, function(nm) {
            p_val <- mean(as.numeric(as.character(rem_data[[nm]])), na.rm = TRUE)
            df <- data.frame(Category = c("Correct", "Incorrect"), Rate = c(p_val, 1 - p_val))
            ggplot2::ggplot(df, ggplot2::aes(x = Category, y = Rate, fill = Category)) +
              ggplot2::geom_col(width = .55, alpha = .88) +
              ggplot2::geom_text(ggplot2::aes(label = scales::percent(Rate, .1)),
                vjust = -.35, size = 3.6, fontface = "bold"
              ) +
              ggplot2::scale_fill_manual(
                values = c("Correct" = col_correct, "Incorrect" = col_incorrect), guide = "none"
              ) +
              ggplot2::scale_y_continuous(
                labels = scales::percent_format(1), limits = c(0, 1.12), expand = c(0, 0)
              ) +
              ggplot2::labs(title = nm, x = NULL, y = "Rate") +
              ggplot2::theme_minimal(base_size = 11) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = theme_col, size = 12),
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                plot.background = ggplot2::element_rect(fill = "white", color = NA)
              )
          })
        }

        if (length(plots) == 0) {
          return(NULL)
        }

        if (length(plots) == 1) {
          print(plots[[1]])
        } else {
          print(
            patchwork::wrap_plots(plots, ncol = 3) +
              patchwork::plot_annotation(
                title = "Option Selection Rate \u2014 All Items",
                theme = ggplot2::theme(
                  plot.title = ggplot2::element_text(
                    face = "bold", color = theme_col, size = 15, hjust = .5
                  )
                )
              )
          )
        }
      }
    })

    output$dlDistractor <- renderUI({
      req(input$type == 1)
      fluidRow(column(
        8,
        shinyWidgets::downloadBttn("downloadGraph", "DOWNLOAD GRAPHS",
          style = "unite", color = "primary", size = "sm",
          no_outline = TRUE, icon = shiny::icon("download")
        )
      ))
    })

    output$uiHeader <- renderUI({
      if (input$type2 == 3) {
        NULL
      } else {
        shinyWidgets::materialSwitch("header", tags$b("First row = variable name"),
          value = TRUE, status = "primary"
        )
      }
    })

    session$onSessionEnded(function() stopApp())

    Biserial <- Item_Difficulty <- Items <- ItemS <- Point_Biserial <- NULL
  }

  shinyApp(ui = ui, server = server)
}
