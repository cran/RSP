#' Run exploratory factor analysis for dichotomous and polytomous data
#' @import foreign
#' @import rJava
#' @import scales
#' @importFrom stats cor
#' @importFrom hornpa hornpa
#' @importFrom utils read.csv2 write.csv2
#' @importFrom utils globalVariables
#' @importFrom psych cortest.bartlett KMO tetrachoric principal
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{FA()}
#' @export


INTERNAL <- function(){
  REL<- new.env()

  ## -- Internal Reliability theme CSS (rose/crimson palette) --
  internal_theme_css <- "
    :root {
      --tc:  #1565C0;
      --tdk: #0f3d75;
      --tlt: #67a8e6;
      --tpl: #bfdcf5;
      --tls: #eef6fd;
      --rsp-theme: #1565C0;
    }

    body { background:#ffffff !important; color:#0f1020 !important;
           font-family:'Segoe UI',Arial,sans-serif; }

    .nav-tabs > li > a {
      color: var(--tdk) !important; font-weight:600;
      border-radius:8px 8px 0 0 !important;
      background:#eef2ff !important;
      white-space:nowrap !important;
    }
    .nav-tabs > li > a h4 { margin:0 !important; padding:0 !important; white-space:nowrap !important; }
    .nav-tabs > li > a h4,
    .tabbable > .nav-tabs > li > a h4,
    ul.nav.nav-tabs li a h4 {
      color: var(--tdk) !important;
    }
    @media (max-width: 1280px) {
      .nav-tabs > li > a { font-size:12px !important; padding:6px 8px !important; }
      .nav-tabs > li > a h4 { font-size:12px !important; }
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background:var(--rsp-theme) !important; color:#fff !important; border-color:var(--rsp-theme) !important;
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
      color:#ffffff !important;
    }
    .nav-tabs > li > a:hover { background:var(--tls) !important; color:var(--tdk) !important; }

    .well {
      background:#ffffff !important;
      border:1px solid #e9eef5 !important;
      border-radius:18px !important;
      box-shadow:0 8px 24px rgba(31, 41, 55, 0.08) !important;
    }

    .btn, .action-button, .bttn,
    .btn-primary, .btn-default, .btn-file,
    .bttn-primary, .bttn-default, .bttn-jelly,
    .bttn-unite, .bttn-material-flat {
      min-width:160px !important; height:38px !important; font-size:13px !important;
      font-weight:700 !important; border-radius:12px !important; padding:0 14px !important;
      display:inline-flex !important; align-items:center !important; justify-content:center !important;
      transition:background .2s, box-shadow .15s !important; box-sizing:border-box !important;
      box-shadow:0 6px 16px rgba(44, 123, 229, 0.16) !important;
    }
    .btn-default, .btn-group > .btn:not(.active) {
      background:#ffffff !important; border:2px solid var(--tpl) !important; color:var(--tdk) !important;
    }
    .btn-group > .btn:not(.active):hover { background:var(--tls) !important; color:var(--tdk) !important; }
    .btn-primary,
    .btn-group > .btn.active,
    .bttn-primary, .bttn-jelly.bttn-primary,
    .bttn-unite.bttn-primary,
    .bttn-material-flat.bttn-primary,
    .bttn-gradient {
      background:var(--rsp-theme) !important; border:2px solid var(--tdk) !important; color:#fff !important;
      box-shadow:0 6px 16px rgba(44, 123, 229, 0.16) !important;
    }
    .btn-primary:hover, .btn-group > .btn.active:hover, .bttn-primary:hover,
    .bttn-gradient:hover { background:var(--tdk) !important; }
    .btn-file { background:var(--rsp-theme) !important; border:2px solid var(--tdk) !important; color:#fff !important; }
    .form-control[readonly] { background:#fff !important; color:#0f1020 !important; }
      .btn-dl-pdf, .btn-dl-jpg {
        background:var(--rsp-theme) !important;
        border:1px solid var(--rsp-theme) !important;
        color:#ffffff !important;
        border-radius:12px !important;
        box-shadow:0 6px 16px rgba(44, 123, 229, 0.16) !important;
        font-weight:700 !important;
      }
      .btn-dl-pdf:hover, .btn-dl-pdf:focus,
      .btn-dl-jpg:hover, .btn-dl-jpg:focus {
        background:var(--tdk) !important;
        border-color:var(--tdk) !important;
        color:#ffffff !important;
      }

    .btn, .btn:hover, .btn:focus, .btn:active,
    .bttn, .bttn:hover, .bttn:focus, .bttn:active,
    .bttn * { color:#ffffff !important; }
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
    .radioGroupButtons .btn:active,
    .radio-group-buttons .btn *,
    .btn-group-container-sw .btn *,
    .radioGroupButtons .btn * { color:#111111 !important; }
    .selectize-input,
    .selectize-input *,
    .selectize-dropdown,
    .selectize-dropdown * { color:#111111 !important; }

    .bootstrap-select .btn { border:2px solid var(--tpl) !important; background:#fff !important; color:var(--tdk) !important; }
    .dropdown-menu > li > a,
    .dropdown-menu > li > a *,
    .bootstrap-select .dropdown-menu li a,
    .bootstrap-select .dropdown-menu li a *,
    .bootstrap-select .dropdown-menu li a span.text,
    .bootstrap-select .dropdown-menu .text,
    .bootstrap-select .dropdown-menu .glyphicon { color:#111111 !important; }
      .dropdown-menu > li > a:hover,
      .dropdown-menu > li > a:focus,
      .bootstrap-select .dropdown-menu li a:hover,
      .bootstrap-select .dropdown-menu li a:focus,
      .bootstrap-select .dropdown-menu li.selected a,
      .bootstrap-select .dropdown-menu li.active a,
      .bootstrap-select .dropdown-menu li.selected a *,
      .bootstrap-select .dropdown-menu li.active a * {
        background:var(--tls) !important;
        color:var(--tdk) !important;
      }

    table.dataTable thead th { background:var(--tc) !important; color:#fff !important; font-size:14px; }
    table.dataTable tbody tr:hover { background:var(--tls) !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current,
    .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
      background:var(--tc) !important; color:#fff !important; border-radius:6px;
    }

    h3, h4 { color:var(--tdk) !important; font-weight:700 !important; }

    #tepe { border-bottom:8px solid var(--rsp-theme) !important; padding-bottom:8px; margin-bottom:8px; }
    #title  { color:var(--tdk) !important; font-size:26px !important; font-weight:800 !important; font-style:normal !important; }
    #title2 { color:var(--tlt) !important; font-size:14px !important; text-align:right !important; font-style:normal !important; }

    .compact-sidebar .form-group {margin-bottom: 8px !important;}
    .compact-sidebar .control-label {margin-bottom: 3px !important; font-size: 13px !important;}
    .compact-sidebar .irs {margin-top: 2px; margin-bottom: 2px;}
    .compact-sidebar .shiny-input-container {margin-bottom: 6px !important;}
    .compact-sidebar .btn-group-container-sw {margin-bottom: 4px !important;}
    .compact-sidebar .radio-group-buttons {margin-bottom: 4px !important;}
    .compact-sidebar .help-block {margin-top: 4px; margin-bottom: 4px; font-size: 12px;}

    .rsp-sidebar-simple, .rsp-sidebar-simple * { box-sizing:border-box; }
    .rsp-sidebar-simple {
      width:100%; height:128px; min-height:128px; max-height:128px;
      margin:0 0 12px 0; padding:12px 12px;
      position:relative; overflow:hidden; border-radius:16px;
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
    .rsp-sidebar-lines div {
      font-size:10.4px; line-height:1.12; letter-spacing:.25px; font-weight:900;
    }
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
    .ctt-bottom {
      position:relative; left:auto; right:auto; transform:none;
      width:100%; height:86px; margin-top:16px;
      display:flex; justify-content:space-around; align-items:center;
      background:rgba(255,255,255,.68); border:1px solid rgba(90,80,130,.16); border-radius:16px;
    }
    .ctt-feature {
      display:flex; flex-direction:column; align-items:center; justify-content:center; gap:6px; color:#07183f; font-size:11px; font-weight:900; flex:1 1 25%; border-right:1px solid rgba(80,80,120,.12); height:100%;
    }
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
    .ctt-icon.grid { border:2px solid var(--rsp-theme); border-radius:4px; }
    .ctt-icon.grid:before,
    .ctt-icon.grid:after { content:''; position:absolute; left:50%; top:0; bottom:0; width:2px; background:var(--rsp-theme); transform:translateX(-50%); }
    .ctt-icon.grid:after { left:auto; top:50%; right:0; width:auto; height:2px; transform:none; background:var(--rsp-theme); }
    .ctt-credit {
      position:absolute; right:32px; top:28px;
      color:color-mix(in srgb, var(--rsp-theme) 80%, #17213d);
      font-size:11px; font-weight:800; z-index:2;
    }
    @media (max-width: 1120px) {
      .ctt-hero { min-height:365px; }
      .ctt-bottom { height:84px; }
    }

    #dynamic-theme-style {}
  "

  ## -- Internal info card helper (sidebar-mini & hero) --
  internalInfoCard <- function(mode = "sidebar-mini") {
    if (identical(mode, "sidebar-mini")) {
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
              shiny::div("INTERNAL"),
              shiny::div("RELIABILITY")
            )
          ),
          shiny::div(class = "rsp-sidebar-credit", "Dogan & Aybek (2022)")
        )
      )
    }

    if (identical(mode, "hero")) {
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
                shiny::div(class = "ctt-domain", "R-Shiny ", tags$b("*"), " Psychometry")
              )
            ),
            shiny::h2(
              class = "ctt-main-title ctt-main-title-08",
              shiny::span(class = "ctt-title-top", "INTERNAL RELIABILITY"),
              shiny::span(class = "ctt-title-sub", "CONSISTENCY ESTIMATES AND SCALE SUMMARY")
            ),
            shiny::div(
              class = "ctt-bottom",
              shiny::div(
                class = "ctt-feature",
                shiny::div(class = "ctt-icon shield"),
                shiny::span("CRONBACH")
              ),
              shiny::div(
                class = "ctt-feature",
                shiny::div(class = "ctt-icon bars", shiny::span(), shiny::span(), shiny::span()),
                shiny::span("TWO-HALVES")
              ),
              shiny::div(
                class = "ctt-feature",
                shiny::div(class = "ctt-icon line"),
                shiny::span("KR-20 / KR-21")
              ),
              shiny::div(
                class = "ctt-feature",
                shiny::div(class = "ctt-icon grid"),
                shiny::span("OMEGA")
              )
            ),
            shiny::div(class = "ctt-credit", "Dogan & Aybek (2022)")
          )
        )
      )
    }

    shiny::div()
  }

  as_numeric_df <- function(data) {
    as.data.frame(
      lapply(data, function(x) suppressWarnings(as.numeric(as.character(x)))),
      check.names = FALSE
    )
  }

  complete_numeric_df <- function(data, min_rows = 2, min_cols = 2) {
    d <- as_numeric_df(data)
    if (ncol(d) < min_cols) {
      return(NULL)
    }
    d <- d[stats::complete.cases(d), , drop = FALSE]
    if (nrow(d) < min_rows) {
      return(NULL)
    }
    d
  }

  cronbach_safe <- function(data) {
    d <- complete_numeric_df(data, min_rows = 2, min_cols = 2)
    n_items <- ncol(data)
    if (is.null(d)) {
      return(list(alpha = NA_real_, n = 0, n_items = n_items, lower_ci = NA_real_, upper_ci = NA_real_))
    }

    out <- tryCatch(ltm::cronbach.alpha(d, CI = TRUE), error = function(e) NULL)
    if (is.null(out)) {
      return(list(alpha = NA_real_, n = nrow(d), n_items = n_items, lower_ci = NA_real_, upper_ci = NA_real_))
    }

    list(
      alpha = as.numeric(out[[1]]),
      n = as.numeric(out[[2]]),
      n_items = as.numeric(out[[3]]),
      lower_ci = as.numeric(out[[6]][1]),
      upper_ci = as.numeric(out[[6]][2])
    )
  }
  
  ### FUNCTIONS ###
  
  hoyt <- function(data) {
    d <- complete_numeric_df(data, min_rows = 2, min_cols = 2)
    i <- ncol(data)
    if (is.null(d)) {
      return(data.frame(Nitem = i, N = 0, Hoyt = NA_real_))
    }

    N <- nrow(d) * ncol(d)
    n <- nrow(d)
    i <- ncol(d)
    Total_SS <- sum(d^2) - sum(d)^2 / N
    person_SS <- sum(rowSums(d)^2) / i - sum(d)^2 / N
    item_SS <- sum(colSums(d)^2) / n - sum(d)^2 / N
    remainder_SS <- Total_SS - (person_SS + item_SS)

    person_MS <- person_SS / (n - 1)
    remainder_MS <- remainder_SS / (n - 1) / (i - 1)
    hoyt_val <- (person_MS - remainder_MS) / person_MS
    if (!is.finite(hoyt_val)) {
      hoyt_val <- NA_real_
    }

    data.frame(Nitem = i, N = n, Hoyt = round(hoyt_val, 3))
  }
  
  REL$hoyt<-hoyt
  
  ####
  
  iki.yari<-function(data,yontem="tekcift") {
    data <- as_numeric_df(data)
    
    tekcift<-function(data) {
      uzunluk<-ncol(data)
      colnames(data)<-1:uzunluk
      bir<-data[,seq(1,uzunluk,2)]
      iki<-data[,seq(2,uzunluk,2)]
      list(bir,iki)
    }
    rastgele<-function(data) {
      n<-ncol(data)
      colnames(data)<- paste0("soru",1:n)
      nn<-n/2
      x1<-colnames(data)
      
      ifelse(n%%2 == 0, s1<-sample(x1,nn),
             s1<-sample(x1,(n+1)/2))
      
      s2<-which(!x1  %in%  s1)
      bir<- data[, s1]
      iki<- data[, s2]
      list(bir,iki)
    }
    if(yontem=="tekcift"){ yari<-tekcift(data)}
    if(yontem=="seckisiz") { yari<-rastgele(data)}
    sum1 <- rowSums(yari[[1]], na.rm = TRUE)
    sum2 <- rowSums(yari[[2]], na.rm = TRUE)
    valid <- rowSums(!is.na(yari[[1]])) > 0 & rowSums(!is.na(yari[[2]])) > 0
    if (sum(valid) < 2) {
      return(data.frame(rsb = NA_real_))
    }
    kor <- suppressWarnings(stats::cor(sum1[valid], sum2[valid], use = "complete.obs"))
    sonuc <- (2 * kor) / (1 + kor)
    if (!is.finite(sonuc)) {
      sonuc <- NA_real_
    }
    sonuc<-data.frame(rsb=sonuc)
    return(sonuc) }
  
  REL$iki.yari<-iki.yari
  
  ###
  
  KR20 <- function(data){
    i <- ncol(data)
    d <- complete_numeric_df(data, min_rows = 2, min_cols = 2)
    if (is.null(d)) {
      return(data.frame(I = i, KR20 = NA_real_))
    }
    n <- nrow(d)
    # get the item difficulties
    colM <- colMeans(d)
    # Get total scores
    total <- rowSums(d)
    # observed score variance
    var.total <- var(total)*(n-1)/n
    if (!is.finite(var.total) || var.total == 0) {
      return(data.frame(I = i, KR20 = NA_real_))
    }
    #  KR-20
    KR20 <- (i/(i-1)) * (1-sum(colM*(1-colM)) / var.total)
    if (!is.finite(KR20)) {
      KR20 <- NA_real_
    }
    result <- data.frame( I=i, KR20=round(KR20,3) )
    return(result)
  }
  
  
  
  KR21 <- function(data){
    i <- ncol(data)
    d <- complete_numeric_df(data, min_rows = 2, min_cols = 2)
    if (is.null(d)) {
      return(data.frame(I = i, KR21 = NA_real_))
    }
    n <- nrow(d)
    # Get the average of item difficulties
    mean.p <- mean(rowSums(d))/i
    # Observed score variance
    var.total <- var(rowSums(d))*(n-1)/n
    if (!is.finite(var.total) || var.total == 0) {
      return(data.frame(I = i, KR21 = NA_real_))
    }
    # KR-21
    KR21 <- (i/(i-1))*(1-(i*mean.p*(1-mean.p))/var.total)
    if (!is.finite(KR21)) {
      KR21 <- NA_real_
    }
    result <- data.frame( I=i, KR21=round(KR21,3) )
    return(result)
  }
  
  REL$KR21<-KR21
  
  REL$KR20<-KR20
  
  ####
  
  omega<- function(data, nfactor){
    i <- ncol(data)
    d <- complete_numeric_df(data, min_rows = 3, min_cols = 2)
    if (is.null(d)) {
      return(data.frame(I = i, omega = NA_real_))
    }

    omega.g <- tryCatch(
      suppressMessages(
        suppressWarnings(
          psych::omega(d, nfactor, ply = TRUE, rotate = "oblimin", fm = "minres", digits = 3, sl = TRUE)
        )
      ),
      error = function(e) NULL
    )

    if (is.null(omega.g) || is.null(omega.g$omega.tot) || !is.finite(omega.g$omega.tot)) {
      return(data.frame(I = i, omega = NA_real_))
    }

    data.frame(I = i, omega = round(as.numeric(omega.g$omega.tot), 3))
  }
  
  
  REL$omega<-omega
  
  ###
  
  str.alpha2 <- function( data)
  {
    data <- as_numeric_df(data)
    alpha <- function(data)
    {
      # covariance
      cov <- stats::cov( data, use="pairwise.complete.obs" )
      # mean covariance
      I <- ncol(data)
      if (I < 2 || all(is.na(cov))) {
        return(data.frame(Nitem = I, Alpha = NA_real_, Var.Tot = NA_real_))
      }
      mean_cov <- sum(cov[row(cov)!=col(cov)]) / ( I^2 - I )
      # mean and variance
      mean_var <- mean( diag(cov) )
      alpha <- I*mean_cov / ( mean_var + (I-1)*mean_cov )
      if (!is.finite(alpha)) {
        alpha <- NA_real_
      }
      var.tot <- stats::var( rowSums(data), na.rm=TRUE )
      result <- data.frame( Nitem=I, Alpha=alpha, Var.Tot=var.tot )
      return(result)
    }
    subtest <- cbind( colnames(data), substring( colnames(data), 1,1 ) )
    res0 <- data.frame( Scale="total", alpha(data) )
    for (s in sort(unique( subtest[,2] ))){
      data_s <- data[, subtest[ subtest[,2]==s, 1] ]
      res1 <- data.frame(Scale=paste0("factor",s),alpha(data=data_s) )
      res0 <- rbind( res0, res1 )
    }
    ######tabakal alfann hesaplanmas
    res0$Stratified.Alpha <- NA
    den <- res0[1,4]
    if (is.na(den) || den == 0) {
      res0$Stratified.Alpha[1] <- NA_real_
    } else {
      res0$Stratified.Alpha[1] <- 1 - sum((1 - res0[-1, 3]) * res0[-1,4], na.rm = TRUE) / den
    }
    print(res0)
  }
  
  
  REL$str.alpha2<-str.alpha2
  
  
  
  ui<- fluidPage(

    useShinyjs(),
    theme = shinythemes::shinytheme("flatly"),
    tags$head(tags$style(HTML(internal_theme_css))),
    tags$head(tags$style(id = "dynamic-theme-style", "")),

    uiOutput("cols"),
    
    tags$head(tags$style(HTML("
      /*  Transitional theme overrides, bound to the active theme  */
      body {
        background: #ffffff !important;
        color: var(--tdk) !important;
        font-family: 'Segoe UI', 'Inter', sans-serif !important;
      }

      /* Navbar / header strip */
      .navbar, .navbar-default {
        background: linear-gradient(90deg, var(--tdk), var(--rsp-theme)) !important;
        border: none !important;
        box-shadow: 0 2px 12px color-mix(in srgb, var(--rsp-theme) 24%, transparent) !important;
      }

      /* Sidebar */
      .well {
        background: rgba(255,255,255,0.75) !important;
        border: 1.5px solid var(--tpl) !important;
        border-radius: 16px !important;
        box-shadow: 0 4px 20px color-mix(in srgb, var(--rsp-theme) 18%, transparent) !important;
        backdrop-filter: blur(6px);
      }

      /* Tabs */
      .nav-tabs > li > a {
        color: var(--rsp-theme) !important;
        font-weight: 700 !important;
        font-size: 13px !important;
        border-radius: 10px 10px 0 0 !important;
        background: #FFFFFF !important;
        border: 1.5px solid #dbe4f0 !important;
        margin-right: 3px !important;
        transition: color 0.2s, border-color 0.2s;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background: var(--rsp-theme) !important;
        color: #ffffff !important;
        border-color: var(--rsp-theme) !important;
        text-shadow: none !important;
      }
      .nav-tabs > li > a:hover {
        background: var(--tls) !important;
        color: var(--tdk) !important;
      }
      .nav-tabs > li.active > a h4 {
        color: #ffffff !important;
      }
      .nav-tabs > li > a h4 {
        color: var(--tdk) !important;
        font-size: 13px !important;
      }
      .tab-content {
        background: rgba(255,255,255,0.82) !important;
        border-radius: 0 0 16px 16px !important;
        border: 1.5px solid var(--tpl) !important;
        box-shadow: 0 4px 18px color-mix(in srgb, var(--rsp-theme) 18%, transparent) !important;
        padding: 18px !important;
      }

      /* Action buttons */
      .bttn-gradient {
        background: linear-gradient(135deg, var(--rsp-theme), var(--tlt)) !important;
        color: #fff !important;
        border: none !important;
        border-radius: 12px !important;
        box-shadow: 0 3px 12px color-mix(in srgb, var(--rsp-theme) 32%, transparent) !important;
        font-weight: 700 !important;
        letter-spacing: 0.5px;
        transition: transform 0.15s, box-shadow 0.15s !important;
      }
      .bttn-gradient:hover {
        transform: translateY(-2px) !important;
        box-shadow: 0 6px 20px color-mix(in srgb, var(--rsp-theme) 40%, transparent) !important;
      }

      /* Select / Picker inputs */
      .bootstrap-select .btn, select.form-control, .form-control {
        border: 1.5px solid var(--tpl) !important;
        border-radius: 10px !important;
        background: #ffffff !important;
        color: var(--tdk) !important;
      }
      .bootstrap-select.open .btn,
      .form-control:focus {
        border-color: var(--rsp-theme) !important;
        box-shadow: 0 0 0 3px color-mix(in srgb, var(--rsp-theme) 20%, transparent) !important;
      }

      /* DT tables */
      table.dataTable thead th {
        background: linear-gradient(135deg, var(--tdk), var(--rsp-theme)) !important;
        color: #fff !important;
        font-size: 14px !important;
        font-weight: 700 !important;
        border: none !important;
      }
      table.dataTable tbody tr {
        background: #fff !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background: var(--tls) !important;
      }
      table.dataTable tbody tr:hover {
        background: color-mix(in srgb, var(--rsp-theme) 18%, #ffffff) !important;
      }
      table.dataTable {
        border-radius: 12px !important;
        overflow: hidden !important;
        box-shadow: 0 2px 14px color-mix(in srgb, var(--rsp-theme) 18%, transparent) !important;
      }

      /* gt tables */
      .gt_table {
        border-radius: 12px !important;
        overflow: hidden !important;
        box-shadow: 0 2px 14px color-mix(in srgb, var(--rsp-theme) 18%, transparent) !important;
        font-family: 'Segoe UI', sans-serif !important;
      }
      .gt_col_heading {
        background: linear-gradient(135deg, var(--tdk), var(--rsp-theme)) !important;
        color: #fff !important;
        font-weight: 700 !important;
        font-size: 14px !important;
      }
      .gt_row {
        border-bottom: 1px solid var(--tpl) !important;
      }
      .gt_row:nth-child(even) {
        background-color: var(--tls) !important;
      }

      /* Headings */
      h3, h4 {
        color: var(--tdk) !important;
        font-weight: 700 !important;
      }

      /* Switch / checkbox widgets */
      .bootstrap-switch-handle-on {
        background: var(--rsp-theme) !important;
      }

      /* Loader */
      #shiny-notification-panel { border-radius: 12px !important; }

      /* Rounded table cells */
      table, img, .tippy-content, textarea {
        border-collapse: collapse;
        border-radius: 1em;
        overflow: hidden;
      }
      th, td {
        padding: 1em;
        background: #f0eaff;
        border-bottom: 2px solid white;
        border-top: 2px solid white;
      }
    "))),
    
    # FA_25 theme / tab structure override
    tags$head(tags$style(HTML("
      body { font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif !important; }
      .well {background: #ffffff !important; border: 1px solid #e9eef5 !important; border-radius: 18px !important; box-shadow: 0 8px 24px rgba(31, 41, 55, 0.08) !important;}
      .tabbable > .nav > li > a {border-radius: 12px !important; font-weight: 600 !important; margin-right: 6px !important;}
      .nav-tabs {border-bottom: 0 !important;}
      .nav-tabs > li > a {background: #ffffff !important; color: var(--tdk) !important; border: 1px solid #e9eef5 !important; border-radius: 12px !important;}
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {background: var(--rsp-theme) !important; color: #ffffff !important; border: 1px solid var(--rsp-theme) !important;}
      .nav-tabs > li.active > a h4, .nav-tabs > li.active > a:focus h4, .nav-tabs > li.active > a:hover h4 {color: #ffffff !important; text-shadow: none !important;}
      .nav-tabs > li > a h4 {color: var(--tdk) !important; font-family: Lucida Arial !important; font-size: 16px !important; font-style: oblique !important; text-align:center !important;}
      .tab-content {background: #ffffff !important; border: 0 !important; box-shadow: none !important; border-radius: 0 !important;}
      .btn, .bttn {border-radius: 12px !important; box-shadow: 0 6px 16px rgba(44, 123, 229, 0.16) !important;}
      .dropdown-menu {border-radius: 16px !important; box-shadow: 0 12px 28px rgba(15, 23, 42, 0.14) !important; padding: 12px !important;}
      .dropdown-menu > li > a,
      .dropdown-menu > li > a *,
      .bootstrap-select .dropdown-menu li a,
      .bootstrap-select .dropdown-menu li a *,
      .bootstrap-select .dropdown-menu li a span.text,
      .bootstrap-select .dropdown-menu .text,
      .bootstrap-select .dropdown-menu .glyphicon { color: #111111 !important; }
        .dropdown-menu > li > a:hover,
        .dropdown-menu > li > a:focus,
        .bootstrap-select .dropdown-menu li a:hover,
        .bootstrap-select .dropdown-menu li a:focus,
        .bootstrap-select .dropdown-menu li.selected a,
        .bootstrap-select .dropdown-menu li.active a,
        .bootstrap-select .dropdown-menu li.selected a *,
        .bootstrap-select .dropdown-menu li.active a * {background: var(--tls) !important; color: var(--tdk) !important;}
      .form-control, .bootstrap-select > .dropdown-toggle {border-radius: 12px !important; border-color: #dbe4f0 !important;}
      .irs--shiny .irs-bar, .irs--shiny .irs-single {background: var(--rsp-theme) !important; border-top-color: var(--rsp-theme) !important; border-bottom-color: var(--rsp-theme) !important;}
      .gt_table, .dataTable {border-radius: 14px !important; overflow: hidden !important;}
      .col-sm-4 .well {padding-top: 10px !important; padding-bottom: 10px !important;}
      .col-sm-4 .well .form-group {margin-bottom: 6px !important;}
      .col-sm-4 .well .dropdown {margin-bottom: 6px !important;}
      .col-sm-4 .well .btn, .col-sm-4 .well .bttn {margin-bottom: 4px !important;}
      .col-sm-4 .well .bootstrap-select, .col-sm-4 .well .bootstrap-select > .dropdown-toggle, .col-sm-4 .well .irs, .col-sm-4 .well .shiny-input-container {margin-bottom: 4px !important;}
      .col-sm-4 .well br {display:block; content:''; margin: 2px 0 !important;}
      .col-sm-4 .well h3, .col-sm-4 .well h4 {margin-top: 4px !important; margin-bottom: 5px !important;}
      #tepe{border-bottom: 3px solid black !important;}
    "))),
    
    tags$head(tags$style(
      type="text/css",
      "# img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),
    
    
    tags$head(tags$style(
      type="text/css",
      "#image2 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),
    
    
    
    ### RENK DEM  shinyjs::runjs ile yaplyor (uiOutput kaldrld)
    
    
    
    
    ###################################################################################
    
    tags$style(HTML("#a{color:black; font-family:Lucida Arial ;font-size: 16px;
             font-style: oblique;text-align:center}")), #tabs#
    
    tags$style(HTML("#ab{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#
    
    tags$style(HTML("h3{color:var(--rsp-theme); font-family:'Segoe UI',sans-serif;font-size: 18px;
             font-weight:700;text-align:center}")),
    
    tags$style(HTML("#b{color:black; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #

    # 8.4: Default text-color theme for tabs and widgets; tab backgrounds stay white.
    tags$head(tags$style(HTML("
      .nav-tabs > li > a,
      .nav-tabs > li > a:hover,
      .nav-tabs > li > a:focus,
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #ffffff !important;
        text-shadow: none !important;
      }
      .nav-tabs > li > a,
      .nav-tabs > li > a:hover,
      .nav-tabs > li > a:focus {
        background: #ffffff !important;
        color: var(--tdk) !important;
        border-color: #dbe4f0 !important;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: var(--rsp-theme) !important;
        color: #ffffff !important;
        border-color: var(--rsp-theme) !important;
      }
      .nav-tabs > li > a h4,
      .nav-tabs > li.active > a h4 {
        color: inherit !important;
        text-shadow: none !important;
      }
      .nav-tabs > li.active > a h4,
      .nav-tabs > li.active > a:hover h4,
      .nav-tabs > li.active > a:focus h4,
      .tabbable > .nav-tabs > li.active > a h4,
      .tabbable > .nav-tabs > li.active > a:hover h4,
      .tabbable > .nav-tabs > li.active > a:focus h4,
      ul.nav.nav-tabs li.active a h4 {
        color: #ffffff !important;
      }
      .nav-tabs > li > a h4,
      .tabbable > .nav-tabs > li > a h4,
      ul.nav.nav-tabs li a h4 {
        color: var(--tdk) !important;
      }
      .nav-tabs > li.active > a #a,
      .nav-tabs > li.active > a:hover #a,
      .nav-tabs > li.active > a:focus #a,
      .tabbable > .nav-tabs > li.active > a #a,
      .tabbable > .nav-tabs > li.active > a:hover #a,
      .tabbable > .nav-tabs > li.active > a:focus #a,
      ul.nav.nav-tabs li.active a #a {
        color: #ffffff !important;
      }
      #a, #ab, #dt, #ff, h3, h4, .control-label, label,
      .pretty label, .shiny-input-container label,
      .bootstrap-select .filter-option-inner-inner {
        color: var(--tdk) !important;
      }
      .bootstrap-select > .dropdown-toggle,
      select.form-control, .form-control {
        background: #ffffff !important;
        color: var(--tdk) !important;
      }
      .btn-dl-pdf, .btn-dl-jpg {
        background: var(--rsp-theme) !important;
        border-color: var(--rsp-theme) !important;
        color: #ffffff !important;
      }
      .btn-dl-pdf:hover, .btn-dl-pdf:focus,
      .btn-dl-jpg:hover, .btn-dl-jpg:focus {
        background: var(--tdk) !important;
        border-color: var(--tdk) !important;
        color: #ffffff !important;
      }

      #text1, #text2, #text3, #text31, #text4, #text5,
      #text6, #text7, #text8, #textsum, #textint {
        color: var(--rsp-theme) !important;
      }
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
      id = "type2",
      title = "Make sure you choose the file format correctly!",
      placement = "bottom",
      trigger = "hover"
    ),
    
    
    bsTooltip(
      id = "resint",
      title = "Green : Good, Orange: Average, Red: Insufficent",
      placement = "left",
      trigger = "hover"
    ),
    
    
    
    bsTooltip(
      id = "multint",
      title = "Green : Good, Orange: Average, Red: Insufficent",
      placement = "left",
      trigger = "hover"
    ),
    
    ####################################
    
    tags$head(tags$style(
      type="text/css",
      "#imagewarn img {max-width: 100%; width: auto; height: 100%; align: center}




        table,img, .tippy-content, textarea{ border-collapse: collapse;

  border-radius: 1em;

  overflow: hidden;}

  th, td {

  padding: 1em;

  background: #ddd;

  border-bottom: 2px solid white;

  border-top: 2px solid white;
  }

"
    )),
    
    tags$head(tags$style(
      type="text/css",
      "#imagegif img {max-width: 100%; width: auto; height: 100%; align: right}"
    )),
    
    
    tags$head(tags$style(
      type="text/css",
      "#internal1 img {max-width: 100%; width: auto; height: 100%; align: right}"
    )),
    
    tags$head(tags$style(
      type="text/css",
      "#internal2 img {max-width: 100%; width: auto; height: 100%; align: right}"
    )),
    
    
    tags$head(tags$style(
      type="text/css",
      "#internal3 img {max-width: 100%; width: auto; height: 100%; align: right}"
    )),
    
    
    
    tags$head(tags$style(
      type="text/css",
      "#internal4 img {max-width: 100%; width: auto; height: 100%; align: right}"
    )),
    
    
    
    tags$head(tags$style(
      type="text/css",
      "#internal5 img {max-width: 100%; width: auto; height: 100%; align: right}"
    )),
    
    
    ##################################
    
    div(id = "tepe",
        fluidRow(
          column(6,
                 h1(id = "title", "INTERNAL RELIABILITY")
          ),
          column(6,
                 h1(id = "title2", "RSP PACKAGE \u2014 CRAN")
          )
        )
    ),
    
    
    
    br(),
    
    sidebarPanel( width = 4, class = "compact-sidebar",
                  
                  ## PANEL 1 - INTRODUCTION ##
                  conditionalPanel(
                    condition = "input.panel==0",
                    internalInfoCard("sidebar-mini"),
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
                    
                    verbatimTextOutput(outputId = "coltext"), # SELEN RENG TEXTE DNTRME
                    
                    
                  ),
                  
                  ## PANEL 2 - DATA UPLOAD ##
                  conditionalPanel(
                    condition = "input.panel==1",
                    internalInfoCard("sidebar-mini"),
                    br(),
                    shinyWidgets::pickerInput(
                      inputId = "type2",
                      label = "Select File Format",
                      choices =  list(
                        "CSV - Semicolon  Separated  Excel" = 1,
                        "CSV - Comma  Separated  Excel" = 2,
                        "SAV - SPSS" = 3,
                        "XLSX - Excel"=4
                      ), selected = 3
                    ),
                    
                    
                    
                    br(),
                    
                    fileInput(
                      "data1",
                      h3(id="ab","Uplad Data File")),
                    
                    br(),
                    
                    uiOutput("uiHeader"),
                    
                    br(),
                    
                    textOutput("text2"),
                    
                    tags$head(
                      tags$style(
                        "#text2{
            color: #6B3FA0;
            font-size: 22px;
            font-family: 'Segoe UI', sans-serif;
            font-weight: 700;
            text-align:center;
            letter-spacing:1px;
            }"
                      )
                    ),
                    
                  ),
                  
                  ## PANEL 3 - UNI-DIMENSIONAL DATA ##
                  conditionalPanel(
                    condition = "input.panel==2",
                    internalInfoCard("sidebar-mini"),
                    br(),
                    shinyWidgets::pickerInput(
                      inputId = "type",
                      label =  h3(id="dt", "Select Data Type"),
                      choices = list("Polytomous (Likert etc..)" = 1, "1-0 " = 2),
                      selected = 1 ),
                    
                    
                    uiOutput("method"),
                    
                    uiOutput("Items") ,
                    
                    br(),
                    
                    fluidRow(
                      column(12,
                             uiOutput("rep")
                      )
                    ),
                    
                    fluidRow(
                      column(12,
                              shinyWidgets::actionBttn(
                                inputId = "act",
                                label = "Compute",
                                color = "default",
                                style = "gradient",
                                icon = icon("glyphicon"),
                                size="md",
                                block = TRUE
                              )
                      )
                    ),
                    
                    br(),
                    
                    # withLoader( plotOutput("plotint1"),  type = "html", loader = "loader1")
                    
                  ),
                  
                  
                  
                  
                  
                  ## PANEL 4 - MULTI-DIEMNSIONAL DATA ##

                  conditionalPanel(
                    condition = "input.panel==3",
                    internalInfoCard("sidebar-mini"),
                    br(),
                    
                    
                    shinyWidgets::switchInput(
                      inputId = "gifvid",
                      label = "Watch Tutorial",
                      labelWidth = "100px",
                      width = "150px"
                    ),
                    
                    
                    # withLoader(
                    uiOutput("imagegif"),
                    
                    # imageOutput("imagegif",width = "150%", height = "100px", inline = TRUE),
                    # type = "html", loader = "loader1"),
                    
                    br(),
                    
                    
                    fluidRow(
                      
                      column(6,
                             
                             uiOutput("method2")),
                      
                      column(6,
                             
                             uiOutput("Items2") ) ),
                    
                    uiOutput("nfac"),
                    
                    # actionButton("act2", h3("Compute"), icon = icon("cog",
                    #                                                 lib = "glyphicon"), width = "260px",col="blue" )
                    
                    
                    
                    shinyWidgets::actionBttn(
                      inputId = "act2",
                      label = "Compute",
                      color = "default",
                      style = "gradient",
                      icon = icon("glyphicon"),
                      size="lg",
                      block = TRUE)
                    
                    
                    # plotOutput("plotint2")
                    
                    
                  ),
                  
                  
                  ## PANEL 5 -Summary

                  conditionalPanel(
                    condition = "input.panel==4",
                    internalInfoCard("sidebar-mini"),
                    br(),
                    shinyWidgets::pickerInput(
                      inputId = "type3",
                      label = h3(id="dt", "Select Data Type"),
                      choices = list("Polytomous (Likert etc..)" = 1, "1-0 " = 2),
                      selected = 1
                    ),
                    
                    #
                    # selectInput(
                    #   "dim",
                    #   h3(id="dt", "Select Factor Structure"),
                    #   choices = list("Uni-dimensional)" = 1, "Multi-Dimensional " = 2),
                    #   selected = 1
                    # ),
                    
                    
                    uiOutput("Items3"),
                    #
                    # actionButton("act3", h3("Compute"), icon = icon("cog",
                    #                                                 lib = "glyphicon"), width = "260px",col="blue" )
                    
                    
                    
                    shinyWidgets::actionBttn(
                      inputId = "act3",
                      label = "Compute",
                      color = "default",
                      style = "gradient",
                      icon = icon("glyphicon"),
                      size="lg",
                      block = TRUE)
                    
                    
                  ),
                  
                  
    ), # close sidebar panel
    
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
            column(12, internalInfoCard("hero"))
          )
        ),
        
        ##  MAIN PANEL 2 ##
        tabPanel(
          
          
          h4(id="a", "DATA UPLOAD"),
          value = 1,
          
          br(),
          
          textOutput("text1"),
          tags$head(
            tags$style(
              "#text1{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          DT::DTOutput("upload"),
          
          br(),
          
          gt::gt_output("desc1")
          
        ),
        
        ## MAIN PANEL 3 ##
        tabPanel(
          
          h4(id="a", "UNI-DIMENSIONAL DATA"),
          value = 2,
          
          br(),
          br(),
          
          br(),
          
          textOutput("text3"),
          tags$head(
            tags$style(
              "#text3{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          shiny::dataTableOutput("unidata"),
          
          br(),
          
          textOutput("text4"),
          tags$head(
            tags$style(
              "#text4{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          textOutput("text5"),
          tags$head(
            tags$style(
              "#text5{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          
          
          textOutput("text7"),
          tags$head(
            tags$style(
              "#text7{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          
          withLoader(DT::DTOutput("resint"),  type = "html", loader = "loader1"),
          
          br(),
          br(),
          
          withLoader(  plotly::plotlyOutput("plotint1"),  type = "html", loader = "loader1"),
          
          
          textOutput("textint"),
          tags$head(
            tags$style(
              "#textint{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          
        ),
        
        ## MAIN PANEL 4 ##
        tabPanel(
          
          h4 (id="a", "MULTI-DIMENSIOANAL DATA"),
          value = 3,
          
          
          textOutput("text31"),
          tags$head(
            tags$style(
              "#text31{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          shiny::dataTableOutput("multidata"),
          
          br(),
          
          textOutput("text6"),
          tags$head(
            tags$style(
              "#text6{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          
          textOutput("text8"),
          tags$head(
            tags$style(
              "#text8{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          withLoader(DT::DTOutput("multint"), type = "html", loader = "loader1"),
          
          br(),
          br(),
          
          withLoader( plotly::plotlyOutput("plotint2"),     type = "html", loader = "loader1")
          
        ),
        
        # MAIN PANEL 5 #
        
        tabPanel(
          
          
          h4 (id="a", "SUMMARY"),
          
          br(),
          br(),
          
          textOutput("textsum"),
          tags$head(
            tags$style(
              "#textsum{
            color: #6B3FA0;
            font-size: 20px; font-weight: 700;
            font-family: 'Segoe UI', sans-serif;
            
            text-align:center;
            letter-spacing:1px;
            }"
            )
          ),
          
          br(),
          br(),
          
          withLoader(  gt::gt_output ("sumint"),     type = "html", loader = "loader1"),
          
          br(),
          
          withLoader(  gt::gt_output ("sumint2"),     type = "html", loader = "loader1"),
          
          br(),
          
          withLoader(  gt::gt_output ("sumint3"),type = "html", loader = "loader1"),
          
          br(),
          
          withLoader(  gt::gt_output ("sumint4"),type = "html", loader = "loader1"),
          
          br(),
          
          withLoader(   plotly::plotlyOutput("plotint3"), type = "html", loader = "loader1"),
          
          br(),
          
          fluidRow(
            column(6, downloadButton("dl_sum_excel", label = " Excel ndir", class = "btn-dl-pdf")),
            column(6, downloadButton("dl_plotint3_jpg", label = " Grafik JPG", class = "btn-dl-jpg"))
          ),
          
          value = 4)
        
      ) # close tabsetpanel
    ) #  close mainpanel
  ) #  close fluidpage
  
  
  ## SERVER ##
  
  server <- function(input, output, session) {

    mix_hex <- function(color, target = "#000000", weight = 0.35) {
      base_rgb <- grDevices::col2rgb(color) / 255
      target_rgb <- grDevices::col2rgb(target) / 255
      mixed <- base_rgb * (1 - weight) + target_rgb * weight
      grDevices::rgb(mixed[1], mixed[2], mixed[3])
    }

    theme_palette <- function(color = NULL) {
      primary <- if (!is.null(color) && nzchar(color)) color else "#1565C0"
      list(
        primary = primary,
        dark = mix_hex(primary, "#000000", 0.40),
        light = grDevices::adjustcolor(primary, alpha.f = 0.12),
        pale = grDevices::adjustcolor(primary, alpha.f = 0.20),
        text = "#1e293b"
      )
    }

    style_summary_gt <- function(gt_tbl) {
      pal <- theme_palette(input$myColor)
      gt_tbl %>%
        gt::tab_style(
          style = gt::cell_fill(color = pal$light),
          locations = gt::cells_body()
        ) %>%
        gt::tab_options(
          heading.title.font.size = gt::px(22),
          heading.title.font.weight = "bold",
          heading.background.color = pal$primary,
          column_labels.background.color = pal$dark,
          table.border.top.color = pal$primary,
          table.border.bottom.color = pal$primary,
          table_body.hlines.color = pal$pale,
          table.font.names = "Segoe UI"
        )
    }
    
    output$internal1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)
    
    output$internal2<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)
    
    output$internal3<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)
    
    
    output$internal4<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)
    
    
    output$internal5<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)
    
    
    
    
    
    output$uiHeader <- renderUI({
      
      if(input$type2 == 3){
        
        NULL
        
      } else {
        
        # checkboxInput("header", h2("The first line is the variable name?")
        
        
        
        shinyWidgets::prettySwitch(
          inputId = "header",
          label = "Click if the first line is the variable name!",
          fill = FALSE
        )
        
      }
      
    })
    
    
    
    ## DATA UPLOAD ##
    
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
          ) }
      } else if(input$type2==4) {
        if (tools::file_ext(veri$datapath) != "xlsx") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          xlsx::read.xlsx(veri$datapath, 1, header = isTRUE(input$header)
          )
          
        } }
      
      
      
    })
    
    
    # reactive(
    #
    #   setBackgroundColor(
    #     color = c("white", aaa),
    #     gradient = "linear",
    #     direction = c("bottom", "right")
    #   ),
    # )
    #
    
    
    ## MAIN PANEL 2 ##
    
    output$upload<-DT::renderDT({
      
      req(input$data1)
      
      data<-data()[1:10,1:5]
      
      
      datatable(data, options = list(dom = 't', scrollX = TRUE)) %>% formatStyle(colnames(data),
                                                                                 
                                                                                 backgroundColor = "white")
      
      
    })
    
    
    ##  TEXTS FOR MAIN PANELS ##
    output$text1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA UPLOAD AND BASIC STATISTICS")
      }
    })
    
    output$text2 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA WAS UPLOADED SUCCESSFULLY")
      }
    })
    output$text3<- renderText({
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("VARIABLES INCLUDED IN THE ANALYSIS")
      }
    })
    
    
    output$text31<- renderText({
      if (!is.null(input$data1)){
        req( length(input$items2)>1)
        paste0("VARIABLES INCLUDED IN THE ANALYSIS")
      }
    })
    
    output$text4<- renderText({
      req(input$mthd==1)
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("RESULTS FOR CRONBACH'S ALPHA")
      }
    })
    
    output$text5<- renderText({
      req(input$mthd==2)
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("RESULTS FOR TWO HALVES RELIABILITY")
      }
    })
    
    
    output$text6<- renderText({
      req(input$mthd2==1)
      if (!is.null(input$data1)){
        req( length(input$items2)>1)
        paste0("RESULTS FOR STRATIFIED ALPHA")
      }
    })
    
    
    output$text7<- renderText({
      req(input$mthd==3)
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("RESULTS FOR HOYT'S VARIANCE ANALYSIS")
      }
    })
    
    output$text8<- renderText({
      req(input$mthd2==2)
      if (!is.null(input$data1)){
        req( length(input$items2)>1 )
        paste0("RESULTS FOR OMEGA RELIABILITY")
      }
    })
    
    output$textsum<- renderText({
      if (!is.null(input$data1)){
        req( length(input$items3)>1 )
        paste0("SUMMARY OF RELIABILITY ANALYSIS")
      }
    })
    
    
    ## MAIN PANEL 2 DECRIPTIVES ##
    
    output$desc1 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- data()
        pal <- theme_palette(input$myColor)
        NUMBER_OF_ITEMS <- ncol(data)
        NUMBER_OF_RESPONDENTS <- nrow(data)
        NUMBER_OF_BLANK_ITEMS <- length(which(is.na(data)))
        
        res <- data.frame(
          N = NUMBER_OF_ITEMS,
          N_ITEMS = NUMBER_OF_RESPONDENTS,
          NA. =  NUMBER_OF_BLANK_ITEMS
        )
        res <- gt::gt(res)
        br()
        br()
        br()
        res <- res %>%
          tab_header(title = md("*Basic Statistics About the Data Set*"))
        
        res <- res %>%
          gt::tab_style(
            style = gt::cell_fill(
              color = pal$light,
              alpha = 0.7
            ),
            locations = gt::cells_body()
          )
        
        res <- res %>%
          gt::cols_width(
            c(N_ITEMS) ~ gt::px(250),
            everything() ~ gt::px(250)
          )
        
        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )
        
        res <- style_summary_gt(res)
      }
    })
    
    
    ### UI WIDGETS UNI-DIMENSIONAL ##
    
    #### 1
    output$Items<- renderUI({
      
      req (input$data1)
      
      
      shinyWidgets::pickerInput(
        "items",
        label=h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1,
        
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE )
      
    })
    
    
    
    ### 2
    
    output$method<-renderUI({
      
      req(input$data1)
      
      if (input$type==1)
        
        
        
      {
        shinyWidgets::pickerInput(
          "mthd",
          h3("Select Method"),
          choices = list(
            "Cronbach Alpha" = 1,
            "Two-Halves"=2,
            "Hoyt's Analysis of Variance"=3
          ),
          selected = 1 ) }
      
      else
        
      {
        shinyWidgets::pickerInput(
          "mthd",
          h3("Select Method"),
          choices = list(
            
            "Cronbach Alpha" = 1,
            "Two-Halves"=2,
            "Hoyt's Analysis of Variance"=3,
            "KR 20-21"=4 )) }
      
    })
    
    output$rep<-  renderUI({
      
      req(input$mthd==2)
      
      # numericInput("repth", h3("Replication"), min=1, max=100000,
      #              value = 1000)
      
      
      shinyWidgets::numericInputIcon(
        inputId = "repth",
        label = h4(id="ab", "Replications"),
        min = 1,
        max = 10000,
        value = 1000,
        icon = list(shiny::icon("refresh"))
      )
      
      
    })
    
    
    
    ### UI WIDGETS MULT-DIMENSIONAL ##
    
    
    
    output$Items2<- renderUI({
      
      req (input$data1)
      
      
      
      shinyWidgets::pickerInput(
        "items2",
        label=h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1,
        
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE )
      
    })
    
    
    
    
    
    
    
    
    output$method2<-renderUI({
      
      req(input$data1)
      
      
      shinyWidgets::pickerInput(
        "mthd2",
        h3("Select Method"),
        choices = list(
          "Stratfied Alpha"=1,
          "Omega" = 2
        ),
        selected = 1 )
      
    })
    
    output$nfac <- renderUI({
      
      req(input$data1)
      req(length(input$items2) > 1)
      
      rem <- data()[input$items2]
      colnames(rem) <- toupper(colnames(rem))
      subtest <- substring(colnames(rem), 1, 1)
      auto_nfac <- length(unique(subtest))
      
      tagList(
        br(),
        shinyWidgets::numericInputIcon(
          inputId  = "nfac2",
          label    = h3(id = "dt", paste0("Number of Factors (auto-detected: ", auto_nfac, ")")),
          min      = 1,
          max      = 20,
          value    = auto_nfac,
          icon     = list(icon("layer-group"))
        )
      )
      
    })
    
    
    
    ##### PANEL 3 OUTPUTS  UNI-DATA
    
    output$unidata<-renderDataTable({
      
      req(input$data1)
      
      remitems<-input$items
      
      head(data()[remitems],3)
      
    }, options = list(scrollX = TRUE, dom = 't'))
    
    # CR.ALPHA - TWO HALVES ETC...
    
    output$resint<-DT::renderDT({
      
      req(input$data1)
      pal <- theme_palette(input$myColor)
      
      remitems<-input$items
      
      intdata<-data()[remitems]
      
      req(input$act)
      
      req(length(input$items)>1)
      
      
      ### ALPHA
      
      if ( input$mthd==1)    {
        
        alfa <- cronbach_safe(intdata)


        alpha <- alfa$alpha
        N <- alfa$n
        N_items <- alfa$n_items
        lower_CI <- alfa$lower_ci
        upper_CI <- alfa$upper_ci
        
        res1<-data.frame(N, N_items,Alpha=alpha,lower_CI,upper_CI )
        
        res1<-round(res1,3)
        
        
        
        res1<-  datatable(res1, width = "50px", options = list(dom = 't', scrollX = TRUE)) %>% formatStyle(colnames(res1),
                                                                                                           
                                                                                                           backgroundColor = pal$light) %>%
          
          formatStyle('Alpha',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))
        
        
        # return(res1)
        
        REL$res1<-res1
        
        return(REL$res1)
      }
      
      ### TWO HALVES
      
      
      
      
      if ( input$mthd==2) {
        
        # REL$iki.yari<-iki.yari
        
        ##############
        oddeven <- unname(iki.yari(intdata, "tekcift"))
        
        ikiyari_tekrar <- replicate(2500, iki.yari(intdata,yontem="seckisiz")  )
        
        rsb<-NULL
        
        for (i in 1:2500) { rsb[i]<-ikiyari_tekrar[[i]]}
        
        random<-mean(rsb)
        
        res2<-data.frame( N= nrow(intdata), Nitem= ncol(intdata), oddeven, random ,
                          replication=input$repth )
        
        res2<-round(res2,3)
        
        
        
        res2<-   datatable(res2, width = "50px", options = list(dom = 't', scrollX = TRUE)) %>% formatStyle(colnames(res2),
                                                                                                            
                                                                                                            backgroundColor = pal$light) %>%
          formatStyle('random',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))  %>%
          
          formatStyle('oddeven',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))
        
        # return(res2)
        
        
        REL$res2<-res2
        
        return(REL$res2)
        
        
      }
      
      
      if ( input$mthd==3) {
        
        # REL$hoyt<-hoyt
        
        res3<-hoyt(data()[remitems])
        
        
        res3<-round(res3,3)
        
        
        
        res3<-   datatable(res3, width = "50px",  options = list(dom = 't', scrollX = TRUE)) %>% formatStyle(colnames(res3),
                                                                                                             
                                                                                                             backgroundColor = pal$light) %>%
          formatStyle('Hoyt',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))
        
        #  return(res3)
        
        
        REL$res3<-res3
        
        return(REL$res3)
        
      }
      
      ### KR 20 21
      
      if ( input$mthd==4) {
        
        
        res4_1<-KR20(data()[remitems])
        res4_2<-KR21(data()[remitems] )
        
        
        res4_3<-data.frame(N= nrow(data()[remitems]),
                           Nitem= ncol(data()[remitems]),
                           KR20=res4_1$KR20, KR21=res4_2$KR21)
        
        
        
        res4_3<-   datatable(res4_3, width = "50px", options = list(dom = 't', scrollX = TRUE)) %>% formatStyle(colnames(res4_3),
                                                                                                                
                                                                                                                backgroundColor = pal$light) %>%
          formatStyle('KR20',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981"))) %>%
          
          formatStyle('KR21',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))
        
        
        # return(res4_3)
        
        
        REL$res4_3<-res4_3
        
        return(REL$res4_3)
        
        
        
      }
      
      
    })
    
    ########################################################################################
    
    
    
    ## PLOT UNI-DATA
    output$plotint1 <- plotly::renderPlotly({
      
      req(input$data1)
      req(input$act)
      req(length(input$items) > 1)
      
      remitems <- input$items
      intdata  <- data()[remitems]
      
      #  Tema rengi 
      base_col <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      
      #  Plotly yatay bar yardmc fonksiyonu 
      make_plotly_bar <- function(methods, values, title) {
        n         <- length(methods)
        pal       <- colorRampPalette(c(paste0(base_col, "55"), base_col))(n)
        patterns  <- rep(c("/", "\\", "x", "-", "+", ".", "|", ""), length.out = n)
        bar_width <- if (n == 1) 0.25 else 0.6
        
        hover_txt <- paste0("<b>", methods, "</b><br>",
                            "Reliability: <b>", round(values, 3), "</b><br>",
                            ifelse(values >= 0.80, " Good (0.80)",
                                   ifelse(values >= 0.70, " Acceptable (0.70)",
                                          " Low (<0.70)")))
        
        # Kkten bye srala (altta kk, stte byk)
        ord     <- order(values)
        methods <- methods[ord]
        values  <- values[ord]
        pal     <- pal[ord]
        patterns <- patterns[seq_along(ord)]
        hover_txt <- hover_txt[ord]
        
        fig <- plotly::plot_ly(
          x            = factor(methods, levels = methods),
          y            = values,
          type         = "bar",
          width        = bar_width,
          marker       = list(
            color   = pal,
            opacity = 0.72,
            line    = list(color = base_col, width = 1.5),
            pattern = list(
              shape     = patterns,
              bgcolor   = paste0(base_col, "22"),
              fgcolor   = base_col,
              fgopacity = 0.45,
              size      = 8
            )
          ),
          text         = round(values, 3),
          textposition = "outside",
          hovertext    = hover_txt,
          hoverinfo    = "text"
        ) %>%
          plotly::layout(
            title  = list(text = paste0("<b>", title, "</b>"),
                          font = list(size = 17, color = base_col)),
            xaxis  = list(title    = "",
                          tickfont = list(size = 13, color = base_col, family = "Segoe UI"),
                          showgrid = FALSE),
            yaxis  = list(range     = c(0, 1.18),
                          tickvals  = seq(0, 1, 0.2),
                          gridcolor = "#E8DCFF",
                          zeroline  = FALSE,
                          showline  = FALSE),
            paper_bgcolor = "rgba(253,251,255,0.0)",
            plot_bgcolor  = "rgba(253,251,255,0.0)",
            margin        = list(l = 60, r = 20, t = 50, b = 60),
            shapes = list(
              list(type = "line", y0 = 0.80, y1 = 0.80, x0 = -0.5, x1 = n - 0.5,
                   line = list(color = "#059669", width = 2, dash = "dash")),
              list(type = "line", y0 = 0.70, y1 = 0.70, x0 = -0.5, x1 = n - 0.5,
                   line = list(color = "#F59E0B", width = 1.5, dash = "dot"))
            ),
            annotations = list(
              list(y = 0.82, x = n - 0.5, text = "0.80", showarrow = FALSE,
                   font = list(color = "#059669", size = 11), xanchor = "right"),
              list(y = 0.72, x = n - 0.5, text = "0.70", showarrow = FALSE,
                   font = list(color = "#F59E0B", size = 11), xanchor = "right")
            ),
            showlegend = FALSE
          ) %>%
          plotly::config(displayModeBar = FALSE)
        
        fig
      }
      # 
      
      if (input$mthd == 1) {
        alfa <- cronbach_safe(intdata)
        return(make_plotly_bar("Alpha", round(alfa$alpha, 3), "Cronbach Alpha Reliability"))
      }
      
      if (input$mthd == 2) {
        iki.yari_fn <- REL$iki.yari
        oddeven     <- unname(iki.yari_fn(intdata, "tekcift"))
        reps        <- replicate(input$repth, iki.yari_fn(intdata, yontem = "seckisiz"))
        rsb         <- sapply(seq_len(input$repth), function(i) reps[[i]])
        return(make_plotly_bar(c("Odd-Even", "Random"),
                               round(c(unlist(oddeven), mean(rsb)), 3),
                               "Two-Halves Reliability"))
      }
      
      if (input$mthd == 3) {
        hoyt_fn <- REL$hoyt
        reshp   <- hoyt_fn(data()[remitems])
        return(make_plotly_bar("Hoyt", round(reshp[, 3], 3), "Hoyt Variance Analysis"))
      }
      
      if (input$mthd == 4) {
        KR20_fn <- REL$KR20
        KR21_fn <- REL$KR21
        return(make_plotly_bar(c("KR-20", "KR-21"),
                               round(c(KR20_fn(data()[remitems])[, 2],
                                       KR21_fn(data()[remitems])[, 2]), 3),
                               "KR-20 / KR-21 Reliability"))
      }
      
    }) # close renderPlotly plotint1
    
    
    
    
    
    
    # PANEL 4 OUTPUTS  MULTI-DATA
    
    output$multidata<-renderDataTable({
      
      req(input$data1)
      
      remitems<-input$items2
      
      head(data()[remitems],3)
      
    }, options = list(scrollX = TRUE, dom = 't'))
    
    
    ### STR ALPHA
    
    output$multint<- DT::renderDT({
      
      req(input$data1)
      pal <- theme_palette(input$myColor)
      
      req(input$act2)
      
      req( length(input$items2)>1)
      
      remitems<-input$items2
      
      rem<- data()[remitems]
      
      colnames(rem)<-toupper(colnames(rem))
      
      
      
      if(input$mthd2==1) {
        
        res<- str.alpha2(rem)
        
        
        res<-  cbind(res[1:2],round(res[,3:5],3))
        
        
        res<-as.data.frame(res)
        
        Nres1<-length(res$Scale)-1
        
        REL$Nres1<-Nres1
        
        
        res_str<- datatable(res, width = "50px", options = list(dom = 't', scrollX = TRUE)) %>% formatStyle(colnames(res),
                                                                                                            
                                                                                                            backgroundColor = pal$light)  %>%
          
          
          formatStyle('Alpha',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))  %>%
          
          formatStyle('Stratified.Alpha',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))
        
        return(res_str)
        
        
      } # close str alpha
      
      ### OMEGA
      
      
      if(input$mthd2==2) {
        
        req(input$nfac2)
        nfac_use <- max(1, as.integer(input$nfac2))
        
        res_omega <- omega(rem, nfac_use)
        
        res_omega <- data.frame( N=nrow(data()), Nitem=ncol(rem), Nfac= nfac_use,
                                 Omega=res_omega$omega  )
        
        
        
        res_omega<- datatable(res_omega, height = "400px", options = list(dom = 't', scrollX = TRUE)) %>%
          
          formatStyle(colnames(res_omega),
                      
                      backgroundColor = pal$light)  %>%
          
          formatStyle('Omega',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("#EF4444", "#F59E0B", "#10B981")))
        
        return(res_omega)
        
        
        
      }
      
      
      
    })
    
    
    
    ## PLOT MULTI - DATA
    
    output$plotint2 <- plotly::renderPlotly({
      
      req(input$data1)
      req(input$act2)
      req(length(input$items2) > 1)
      
      remitems <- input$items2
      rem      <- data()[remitems]
      colnames(rem) <- toupper(colnames(rem))
      
      #  Tema rengi 
      base_col <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      
      #  Plotly yatay bar yardmc (ayn make_plotly_bar mant) 
      make_plotly_bar2 <- function(methods, values, title) {
        n         <- length(methods)
        pal       <- colorRampPalette(c(paste0(base_col, "55"), base_col))(n)
        patterns  <- rep(c("\\", "/", "x", "-", "+", ".", "|", ""), length.out = n)
        bar_width <- if (n == 1) 0.25 else 0.6
        
        hover_txt <- paste0("<b>", methods, "</b><br>",
                            "Reliability: <b>", round(values, 3), "</b><br>",
                            ifelse(values >= 0.80, " Good (0.80)",
                                   ifelse(values >= 0.70, " Acceptable (0.70)",
                                          " Low (<0.70)")))
        
        ord       <- order(values)
        methods   <- methods[ord]
        values    <- values[ord]
        pal       <- pal[ord]
        patterns  <- patterns[seq_along(ord)]
        hover_txt <- hover_txt[ord]
        
        plotly::plot_ly(
          x            = factor(methods, levels = methods),
          y            = values,
          type         = "bar",
          width        = bar_width,
          marker       = list(
            color   = pal,
            opacity = 0.72,
            line    = list(color = base_col, width = 1.5),
            pattern = list(shape = patterns, bgcolor = paste0(base_col, "22"),
                           fgcolor = base_col, fgopacity = 0.45, size = 8)
          ),
          text         = round(values, 3),
          textposition = "outside",
          hovertext    = hover_txt,
          hoverinfo    = "text"
        ) %>%
          plotly::layout(
            title  = list(text = paste0("<b>", title, "</b>"),
                          font = list(size = 17, color = base_col)),
            xaxis  = list(title    = "",
                          tickfont = list(size = 13, color = base_col, family = "Segoe UI"),
                          showgrid = FALSE),
            yaxis  = list(range     = c(0, 1.18),
                          tickvals  = seq(0, 1, 0.2),
                          gridcolor = "#E8DCFF",
                          zeroline  = FALSE,
                          showline  = FALSE),
            paper_bgcolor = "rgba(253,251,255,0.0)",
            plot_bgcolor  = "rgba(253,251,255,0.0)",
            margin        = list(l = 60, r = 20, t = 50, b = 60),
            shapes = list(
              list(type = "line", y0 = 0.80, y1 = 0.80, x0 = -0.5, x1 = n - 0.5,
                   line = list(color = "#059669", width = 2, dash = "dash")),
              list(type = "line", y0 = 0.70, y1 = 0.70, x0 = -0.5, x1 = n - 0.5,
                   line = list(color = "#F59E0B", width = 1.5, dash = "dot"))
            ),
            annotations = list(
              list(y = 0.82, x = n - 0.5, text = "0.80", showarrow = FALSE,
                   font = list(color = "#059669", size = 11), xanchor = "right"),
              list(y = 0.72, x = n - 0.5, text = "0.70", showarrow = FALSE,
                   font = list(color = "#F59E0B", size = 11), xanchor = "right")
            ),
            showlegend = FALSE
          ) %>%
          plotly::config(displayModeBar = FALSE)
      }
      # 
      
      if (input$mthd2 == 1) {
        res      <- str.alpha2(rem)
        rstr     <- res$Alpha[-1]
        r4       <- res$Stratified.Alpha[1]
        scales_v <- c(as.character(res[-1, 1]), "Str.Alpha")
        return(make_plotly_bar2(scales_v, round(c(rstr, r4), 3), "Stratified Alpha by Factor"))
      }
      
      if (input$mthd2 == 2) {
        req(input$nfac2)
        nfac_use2 <- max(1, as.integer(input$nfac2))
        res_omg   <- omega(rem, nfac_use2)
        return(make_plotly_bar2("Omega", round(res_omg[, 2], 3), "Omega Reliability"))
      }
      
    }) # close renderPlotly plotint2
    
    
    ## IMAGE RENDER ##
    
    
    ### 1
    # output$imagegif<- renderImage({
    #
    #   req (input$data1)
    #
    #
    #   req( input$gifvid== TRUE)
    #
    #   resim2 <- tempfile(fileext = '.gif')
    #   list(src = "gifrel.gif", contentType = "image/gif")
    # },
    # deleteFile = FALSE)
    
    output$imagegif <- renderUI({
      req(input$data1)
      req(input$gifvid == TRUE)
      shiny::img(src = "https://shiny.eptlab.com/RSPEN/reliability/img/gifrel.gif", width = "97%")
    })
    
    
    
    ### 2
    #
    # output$imagewarn<- renderImage({
    #
    #   req (input$data1)
    #
    #   shiny::img(src = "img/warn3.png", width = "97%")
    #   resim2 <- tempfile(fileext = '.png')
    #   list(src = "warn3.png", contentType = "image/png")
    # },
    # deleteFile = FALSE)
    
    
    
    
    
    ### SUMMARY ###
    
    
    
    output$Items3<- renderUI({
      
      req (input$data1)
      shinyWidgets::pickerInput(
        "items3",
        h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1, multiple = TRUE
      )
      
      
      
      shinyWidgets::pickerInput(
        "items3",
        label=h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1,
        
        options = list(
          `actions-box` = TRUE),
        multiple = TRUE )
      
    })
    
    
    output$sumint<- gt::render_gt ( align = "center", {
      
      req(input$data1)
      
      req(input$act3)
      
      req(length(input$items3)>1)
      
      remitems<-input$items3
      
      rem<- data()[remitems]
      
      dat<-data()
      
      colnames(dat)<-toupper(colnames(dat))
      
      sumres<-data.frame( N=nrow(data()), Nitem= length(input$items3),
                          
                          Number_NA = length(which(is.na(data()))))

      pal <- theme_palette(input$myColor)
      
      res <- gt::gt(sumres)
      
      res <- res %>%
        gt::tab_style(
          style = cell_fill(
            color = pal$light,
            alpha = 0.7
          ),
          locations = gt::cells_body()
        )
      
      res <- res %>%
        gt::cols_width(
          c(Nitem) ~ gt::px(300),
          everything() ~ gt::px(300)
        )
      
      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- style_summary_gt(res)
      
    })
    
    ################################
    
    output$sumint2<- gt::render_gt ( align = "center", {
      
      req(input$data1)
      
      req(input$act3)
      
      req( length(input$items3)>1)
      
      remitems<-input$items3
      
      rem<- data()[remitems]
      
      
      colnames(rem)<-toupper(colnames(rem))
      
      ####
      
      alfa <- cronbach_safe(rem)
      Alpha <- alfa$alpha
      
      REL$Alphas<-Alpha
      
      ###
      
      hoyt<-REL$hoyt
      
      HoyT<- hoyt(rem)
      
      Hoyt<-HoyT[,3]
      
      REL$Hoyts<-Hoyt
      
      ###
      
      oddeven <- unname(iki.yari(rem, "tekcift"))
      
      REL$oddevens<-oddeven
      
      ikiyari_tekrar <- replicate(2000, iki.yari(rem,yontem="seckisiz")  )
      
      rsb<-NULL
      
      for (i in 1:1000) { rsb[i]<-ikiyari_tekrar[[i]]}
      
      random<-mean(rsb)
      
      REL$randoms<-random
      
      sumres<-data.frame( Alpha, TwoHalves_odd=oddeven, TwoHalves_rand=random,
                          Hoyt)

      pal <- theme_palette(input$myColor)
      
      res <- gt::gt(sumres)
      
      
      res <- res %>%
        gt::tab_style(
          style = cell_fill(
            color = pal$light,
            alpha = 0.7
          ),
          locations = gt::cells_body()
        )
      
      res <- res %>%
        gt::cols_width(
          c(TwoHalves_odd) ~ gt::px(250),
          everything() ~ gt::px(250)
        )
      
      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- style_summary_gt(res)
      
      
    })
    
    
    
    #############
    
    
    
    output$sumint3<- gt::render_gt ( align = "center", {
      
      req(input$data1)
      
      req(input$act3)
      
      
      remitems<-input$items3
      
      rem<- data()[remitems]
      
      colnames(rem)<-toupper(colnames(rem))
      
      
      
      res<- str.alpha2(rem)
      
      
      Str.Alpha<-res$Stratified.Alpha[1]
      
      REL$Str.Alphas<-Str.Alpha
      
      res<-  cbind(res[1:2],round(res[,3:5],3))
      
      
      
      res<-as.data.frame(res)
      
      Nres1<-length(unique(substring(colnames(rem), 1, 1)))
      
      REL$Nresum<-Nres1
      
      omega_res<-omega(rem,Nres1)
      
      nn<-Nres1-1
      
      omg<-omega_res$omega
      
      REL$omegas<-omg
      
      res$Omega<- c(omg, rep(NA,Nres1))

      pal <- theme_palette(input$myColor)
      
      res <- gt::gt(res)
      
      res <- res %>%
        gt::tab_style(
          style = cell_fill(
            color = pal$light,
            alpha = 0.7
          ),
          locations = gt::cells_body()
        )
      
      res <- res %>%
        gt::cols_width(
          c(Stratified.Alpha) ~ gt::px(180),
          everything() ~ gt::px(180)
        )
      
      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- style_summary_gt(res)
    })
    
    #######################################################################
    
    output$sumint4<- gt::render_gt ( align = "center", {
      
      req(input$data1)
      
      req( length(input$items3)>1)
      
      remitems<-input$items3
      
      rem<- data()[remitems]
      
      colnames(rem)<-toupper(colnames(rem))
      
      req(input$type3==2)
      
      req(input$act3)
      
      rkr20<-KR20(rem)
      rkr21<-KR21(rem)
      
      res1<-data.frame(KR20=rkr20$KR20, KR21=rkr21$KR21)

      pal <- theme_palette(input$myColor)
      
      res <- gt::gt(res1)
      
      
      res <- res %>%
        gt::tab_style(
          style = cell_fill(
            color = pal$light,
            alpha = 0.7
          ),
          locations = gt::cells_body()
        )
      
      res <- res %>%
        gt::cols_width(
          c(KR21) ~ gt::px(300),
          everything() ~ gt::px(300)
        )
      
      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- style_summary_gt(res)
      
      return(res)
      
    })
    
    
    ############ SUMMARY PLOT ###########
    
    output$plotint3 <- plotly::renderPlotly({
      
      req(input$act3)
      req(input$data1)
      req(length(input$items3) > 1)
      
      remitems <- input$items3
      rem      <- data()[remitems]
      colnames(rem) <- toupper(colnames(rem))
      
      base_col <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#6B3FA0"
      
      alfa      <- cronbach_safe(rem)
      Alpha_v   <- round(alfa$alpha, 3)
      
      iki.yari_fn <- REL$iki.yari
      oddeven_v   <- round(unlist(unname(iki.yari_fn(rem, "tekcift"))), 3)
      reps_s      <- replicate(2500, iki.yari_fn(rem, yontem = "seckisiz"))
      rsb_s       <- sapply(seq_len(2500), function(i) reps_s[[i]])
      random_v    <- round(mean(rsb_s), 3)
      
      HoyT_v   <- round(hoyt(rem)[, 3], 3)
      
      res_s    <- str.alpha2(rem)
      Nres_s   <- length(unique(substring(colnames(rem), 1, 1)))
      StrAlpha <- round(res_s$Stratified.Alpha[1], 3)
      Omega_v  <- round(omega(rem, Nres_s)[, 2], 3)
      
      if (input$type3 == 1) {
        Methods <- c("Alpha", "Hoyt", "TH Odd-Even", "TH Random", "Str.Alpha", "Omega")
        Values  <- c(Alpha_v, HoyT_v, oddeven_v[1], random_v, StrAlpha, Omega_v)
      } else {
        rkr20_s <- KR20(rem); rkr21_s <- KR21(rem)
        Methods <- c("Alpha", "Hoyt", "TH Odd-Even", "TH Random",
                     "KR-20", "KR-21", "Str.Alpha", "Omega")
        Values  <- c(Alpha_v, HoyT_v, oddeven_v[1], random_v,
                     round(rkr20_s$KR20, 3), round(rkr21_s$KR21, 3), StrAlpha, Omega_v)
      }
      
      Values  <- as.numeric(Values)
      n       <- length(Methods)
      
      # Kkten bye srala
      ord        <- order(Values)
      Methods    <- Methods[ord]
      Values     <- Values[ord]
      
      qual_color <- ifelse(Values >= 0.80, base_col,
                           ifelse(Values >= 0.70, paste0(base_col, "99"),
                                  paste0(base_col, "55")))
      patterns   <- rep(c("/", "\\", "x", "-", "+", ".", "|", ""), length.out = n)
      bar_width  <- if (n == 1) 0.25 else 0.6
      
      hover_txt <- paste0("<b>", Methods, "</b><br>",
                          "Reliability: <b>", round(Values, 3), "</b><br>",
                          ifelse(Values >= 0.80, " Good (0.80)",
                                 ifelse(Values >= 0.70, " Acceptable (0.70)",
                                        " Low (<0.70)")))
      
      plotly::plot_ly(
        x            = factor(Methods, levels = Methods),
        y            = Values,
        type         = "bar",
        width        = bar_width,
        marker       = list(
          color   = qual_color,
          opacity = 0.72,
          line    = list(color = base_col, width = 1.5),
          pattern = list(shape = patterns, bgcolor = paste0(base_col, "22"),
                         fgcolor = base_col, fgopacity = 0.45, size = 8)
        ),
        text         = round(Values, 3),
        textposition = "outside",
        hovertext    = hover_txt,
        hoverinfo    = "text"
      ) %>%
        plotly::layout(
          title  = list(text = "<b>Internal Reliability  Summary</b>",
                        font = list(size = 17, color = base_col)),
          xaxis  = list(title    = "",
                        tickfont = list(size = 13, color = base_col, family = "Segoe UI"),
                        showgrid = FALSE),
          yaxis  = list(range     = c(0, 1.18),
                        tickvals  = seq(0, 1, 0.2),
                        gridcolor = paste0(base_col, "22"),
                        zeroline  = FALSE,
                        showline  = FALSE),
          paper_bgcolor = "rgba(253,251,255,0.0)",
          plot_bgcolor  = "rgba(253,251,255,0.0)",
          margin        = list(l = 60, r = 20, t = 50, b = 60),
          shapes = list(
            list(type = "line", y0 = 0.80, y1 = 0.80, x0 = -0.5, x1 = n - 0.5,
                 line = list(color = "#059669", width = 2, dash = "dash")),
            list(type = "line", y0 = 0.70, y1 = 0.70, x0 = -0.5, x1 = n - 0.5,
                 line = list(color = "#F59E0B", width = 1.5, dash = "dot"))
          ),
          annotations = list(
            list(y = 0.82, x = n - 0.5, text = "0.80  Good", showarrow = FALSE,
                 font = list(color = "#059669", size = 11), xanchor = "right"),
            list(y = 0.72, x = n - 0.5, text = "0.70  Acceptable", showarrow = FALSE,
                 font = list(color = "#F59E0B", size = 11), xanchor = "right")
          ),
          showlegend = FALSE
        ) %>%
        plotly::config(displayModeBar = FALSE)
      
    }) # close renderPlotly plotint3
    
    
    
    
    
    
    
    
    ###  DOWNLOAD HANDLERS  ###
    
    #  Shared horizontal bar builder for downloads 
    dl_hbar <- function(df, title, cat_col = "method", val_col = "value") {
      df[[val_col]] <- as.numeric(df[[val_col]])
      df[[cat_col]] <- factor(df[[cat_col]], levels = rev(df[[cat_col]]))
      ggplot2::ggplot(df, ggplot2::aes_string(x = cat_col, y = val_col, fill = cat_col)) +
        ggplot2::geom_col(width = 0.55, alpha = 0.88, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes_string(label = paste0("round(", val_col, ", 3)")),
                           hjust = -0.15, colour = "#3B1F5E", fontface = "bold", size = 5.5) +
        ggplot2::scale_fill_manual(values = colorRampPalette(c("#3B1F5E","#6B3FA0","#9B72CF","#C084FC","#A78BFA","#7C3AED","#5B21B6","#E8DCFF"))(20)) +
        ggplot2::scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2),
                                    expand = ggplot2::expansion(mult = c(0, 0.08))) +
        ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed",
                            colour = "#059669", linewidth = 1.3, alpha = 0.7) +
        ggplot2::labs(title = title) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title         = ggplot2::element_text(size = 19, colour = "#3B1F5E", face = "bold"),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = "#E8DCFF", linewidth = 0.5),
          panel.background   = ggplot2::element_rect(fill = "#FDFBFF", colour = NA),
          plot.background    = ggplot2::element_rect(fill = "#FDFBFF", colour = NA),
          legend.position    = "none",
          axis.text.y = ggplot2::element_text(colour = "#3B1F5E", size = 13, face = "bold"),
          axis.text.x = ggplot2::element_text(colour = "#6B3FA0", size = 11),
          axis.title  = ggplot2::element_blank()
        ) +
        ggplot2::coord_flip()
    }
    
    #  plotint3 (summary grafik) JPG download  gerek JPEG retir 
    output$dl_plotint3_jpg <- downloadHandler(
      filename = function() paste0("summary_plot_", Sys.Date(), ".jpg"),
      contentType = "image/jpeg",
      content  = function(file) {
        req(input$act3)
        req(input$data1)
        req(length(input$items3) > 1)
        
        rem <- data()[input$items3]
        colnames(rem) <- toupper(colnames(rem))
        
        alfa        <- cronbach_safe(rem)
        Alpha_v     <- round(alfa$alpha, 3)
        iki.yari_fn <- REL$iki.yari
        oddeven_v   <- round(unlist(unname(iki.yari_fn(rem, "tekcift"))), 3)
        reps_dl     <- replicate(2500, iki.yari_fn(rem, yontem = "seckisiz"))
        rsb_dl      <- sapply(seq_len(2500), function(i) reps_dl[[i]])
        random_v    <- round(mean(rsb_dl), 3)
        HoyT_v      <- round(hoyt(rem)[, 3], 3)
        res_dl      <- str.alpha2(rem)
        Nres_dl     <- length(unique(substring(colnames(rem), 1, 1)))
        StrAlpha    <- round(res_dl$Stratified.Alpha[1], 3)
        Omega_v     <- round(omega(rem, Nres_dl)[, 2], 3)
        
        if (input$type3 == 1) {
          Methods <- c("Alpha", "Hoyt", "TH Odd-Even", "TH Random", "Str.Alpha", "Omega")
          Values  <- c(Alpha_v, HoyT_v, oddeven_v[1], random_v, StrAlpha, Omega_v)
        } else {
          rkr20_dl <- KR20(rem)
          rkr21_dl <- KR21(rem)
          Methods  <- c("Alpha", "Hoyt", "TH Odd-Even", "TH Random", "KR-20", "KR-21", "Str.Alpha", "Omega")
          Values   <- c(Alpha_v, HoyT_v, oddeven_v[1], random_v,
                        round(rkr20_dl$KR20, 3), round(rkr21_dl$KR21, 3), StrAlpha, Omega_v)
        }
        
        Values <- as.numeric(Values)
        keep <- is.finite(Values)
        Methods <- Methods[keep]
        Values <- Values[keep]
        if (length(Values) == 0) {
          Methods <- "No valid estimate"
          Values <- 0
        }
        ord <- order(Values)
        Methods <- Methods[ord]
        Values <- Values[ord]
        
        quality <- ifelse(Values >= 0.80, "Good",
                          ifelse(Values >= 0.70, "Acceptable", "Low"))
        base_col <- if (!is.null(input$myColor) && nchar(input$myColor) > 0) input$myColor else "#2c7be5"
        theme_cols <- grDevices::colorRampPalette(c("#FFFFFF", base_col))(5)
        fill_cols <- c(
          Good = base_col,
          Acceptable = theme_cols[4],
          Low = theme_cols[3]
        )
        grid_col <- grDevices::adjustcolor(base_col, alpha.f = 0.18)
        text_col <- base_col
        splot <- data.frame(
          Methods = factor(Methods, levels = Methods),
          Values = Values,
          quality = factor(quality, levels = c("Good", "Acceptable", "Low"))
        )
        
        # Bilerek ggpattern / webshot / plotly export kullanlmad.
        # Bylece indirme HTML deil dorudan JPEG dosyas retir.
        p <- ggplot2::ggplot(splot, ggplot2::aes(x = Methods, y = Values, fill = quality)) +
          ggplot2::geom_col(width = 0.60, alpha = 0.75, colour = base_col, linewidth = 0.7) +
          ggplot2::geom_text(ggplot2::aes(label = round(Values, 3)),
                             vjust = -0.45, colour = text_col, fontface = "bold", size = 5) +
          ggplot2::scale_fill_manual(
            values = fill_cols,
            drop = FALSE,
            name = "Reliability"
          ) +
          ggplot2::scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2),
                                      expand = ggplot2::expansion(mult = c(0, 0.08))) +
          ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed",
                              colour = "#000000", linewidth = 1.2, alpha = 0.90) +
          ggplot2::geom_hline(yintercept = 0.70, linetype = "dotted",
                              colour = "#000000", linewidth = 1.2, alpha = 0.90) +
          ggplot2::annotate("text", x = Inf, y = 0.82, label = "0.80  Good",
                            hjust = 1.02, colour = "#000000", size = 4.5, fontface = "bold") +
          ggplot2::annotate("text", x = Inf, y = 0.72, label = "0.70  Acceptable",
                            hjust = 1.02, colour = "#000000", size = 4.5, fontface = "bold") +
          ggplot2::labs(title = "Internal Reliability  Summary") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title         = ggplot2::element_text(size = 20, colour = base_col, face = "bold", hjust = 0.5),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(colour = grid_col, linewidth = 0.5),
            panel.grid.minor   = ggplot2::element_blank(),
            panel.background   = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
            plot.background    = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
            legend.position    = "bottom",
            legend.title       = ggplot2::element_text(colour = base_col, face = "bold"),
            legend.text        = ggplot2::element_text(colour = base_col),
            axis.text.x        = ggplot2::element_text(colour = base_col, size = 12, face = "bold"),
            axis.text.y        = ggplot2::element_text(colour = base_col, size = 11),
            axis.title         = ggplot2::element_blank()
          )
        
        ggplot2::ggsave(
          filename = file,
          plot = p,
          device = "jpeg",
          width = 12,
          height = 7,
          units = "in",
          dpi = 180,
          bg = "white"
        )
      }
    )
    
    #  Summary Excel download 
    output$dl_sum_excel <- downloadHandler(
      filename = function() paste0("summary_reliability_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(input$act3); req(input$data1); req(length(input$items3) > 1)
        rem <- data()[input$items3]; colnames(rem) <- toupper(colnames(rem))
        
        #  Tm hesaplamalar 
        alfa        <- cronbach_safe(rem)
        alpha_val   <- round(alfa$alpha, 3)
        lower_ci    <- round(alfa$lower_ci, 3)
        upper_ci    <- round(alfa$upper_ci, 3)
        
        hoyt_res    <- hoyt(rem)
        hoyt_val    <- if (is.data.frame(hoyt_res)) round(hoyt_res[1, 3], 3) else round(as.numeric(hoyt_res)[1], 3)
        
        iki.yari_fn <- REL$iki.yari
        oddeven_val <- round(unlist(unname(iki.yari_fn(rem, "tekcift")))[1], 3)
        reps_xl     <- replicate(2000, iki.yari_fn(rem, yontem = "seckisiz"))
        rsb_xl      <- sapply(seq_len(1000), function(i) reps_xl[[i]])
        rand_val    <- round(mean(rsb_xl), 3)
        
        res_xl      <- str.alpha2(rem)
        Nres_xl     <- length(unique(substring(colnames(rem), 1, 1)))
        omg_xl      <- omega(rem, Nres_xl)
        str_alpha   <- round(res_xl$Stratified.Alpha[1], 3)
        omega_val   <- round(omg_xl$omega, 3)
        
        #  Sayfa 1: Veri bilgisi 
        tbl1 <- data.frame(
          N         = nrow(data()),
          N_Items   = length(input$items3),
          N_Missing = length(which(is.na(data())))
        )
        
        #  Sayfa 2: zet gvenirlik tablosu (sumint ile ayn) 
        tbl2 <- data.frame(
          Alpha           = alpha_val,
          TwoHalves_odd   = oddeven_val,
          TwoHalves_rand  = rand_val,
          Hoyt            = hoyt_val
        )
        
        #  Sayfa 3: Tm yntemler Method/Value formatnda 
        tbl3 <- data.frame(
          Method = c("Cronbach Alpha", "Cronbach Alpha Lower CI", "Cronbach Alpha Upper CI",
                     "Hoyt", "Two-Halves (Odd-Even)", "Two-Halves (Random)",
                     "Stratified Alpha", "Omega"),
          Value  = c(alpha_val, lower_ci, upper_ci,
                     hoyt_val, oddeven_val, rand_val,
                     str_alpha, omega_val)
        )
        
        # KR-20/KR-21 ekle (binary veri)
        if (!is.null(input$type3) && input$type3 == 2) {
          kr20_xl <- tryCatch(round(KR20(rem)$KR20, 3), error = function(e) NA)
          kr21_xl <- tryCatch(round(KR21(rem)$KR21, 3), error = function(e) NA)
          tbl3 <- rbind(tbl3,
                        data.frame(Method = "KR-20", Value = kr20_xl),
                        data.frame(Method = "KR-21", Value = kr21_xl))
        }
        
        #  Sayfa 4: Stratified Alpha detay 
        tbl4 <- cbind(res_xl[1:2], round(res_xl[, 3:5], 3))
        tbl4$Omega <- c(omega_val, rep(NA, nrow(tbl4) - 1))
        
        #  Workbook olutur 
        wb <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(wb, "Data Info")
        openxlsx::writeData(wb, "Data Info", tbl1)
        
        openxlsx::addWorksheet(wb, "Summary Table")
        openxlsx::writeData(wb, "Summary Table", tbl2)
        
        openxlsx::addWorksheet(wb, "All Methods")
        openxlsx::writeData(wb, "All Methods", tbl3)
        
        openxlsx::addWorksheet(wb, "Stratified Alpha & Omega")
        openxlsx::writeData(wb, "Stratified Alpha & Omega", tbl4)
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    ###  END DOWNLOAD HANDLERS  ###
    
    observeEvent(input$myColor, {
      col <- input$myColor

      css <- sprintf(
        ":root{--tc:%s;--tdk:%s;--tlt:%s;--tpl:%s;--tls:color-mix(in srgb,%s 14%%,#ffffff);--rsp-theme:%s;}
         body{background:linear-gradient(to bottom right,#ffffff,color-mix(in srgb,%s 10%%,#ffffff)) !important;}
         .nav-tabs > li > a h4,
         .tabbable > .nav-tabs > li > a h4,
         ul.nav.nav-tabs li a h4 {color:var(--tdk) !important;}
         .nav-tabs > li.active > a,
         .nav-tabs > li.active > a:focus,
         .nav-tabs > li.active > a:hover {background:var(--tc) !important;color:#fff !important;border-color:var(--tc) !important;}
         .nav-tabs > li.active > a h4,
         .nav-tabs > li.active > a:focus h4,
         .nav-tabs > li.active > a:hover h4,
         .tabbable > .nav-tabs > li.active > a h4,
         .tabbable > .nav-tabs > li.active > a:focus h4,
         .tabbable > .nav-tabs > li.active > a:hover h4,
         ul.nav.nav-tabs li.active a h4,
         ul.nav.nav-tabs li.active a:focus h4,
         ul.nav.nav-tabs li.active a:hover h4 {color:#ffffff !important;}
         #panel .nav-tabs > li > a h4,
         #panel .tabbable > .nav-tabs > li > a h4 {color:var(--tdk) !important;}
         #panel .nav-tabs > li.active > a h4,
         #panel .nav-tabs > li.active > a:focus h4,
         #panel .nav-tabs > li.active > a:hover h4,
         #panel ul.nav.nav-tabs li.active a h4,
         #panel .nav-tabs > li.active > a #a,
         #panel .nav-tabs > li.active > a:focus #a,
         #panel .nav-tabs > li.active > a:hover #a,
         #panel ul.nav.nav-tabs li.active a #a {color:#ffffff !important;}
         .nav-tabs > li > a:hover {background:var(--tls) !important;color:var(--tdk) !important;}
         .well {background:var(--tls) !important;border-color:var(--tpl) !important;}
         .dropdown-menu > li > a:hover,
         .dropdown-menu > li > a:focus,
         .bootstrap-select .dropdown-menu li a:hover,
         .bootstrap-select .dropdown-menu li a:focus,
         .bootstrap-select .dropdown-menu li.selected a,
         .bootstrap-select .dropdown-menu li.active a,
         .bootstrap-select .dropdown-menu li.selected a *,
         .bootstrap-select .dropdown-menu li.active a * {background:var(--tls) !important;color:var(--tdk) !important;}
         .bttn-gradient,.bttn-gradient.bttn-primary {background:var(--tc) !important;border-color:var(--tdk) !important;}
         .bttn-jelly.bttn-primary,.bttn-unite.bttn-primary {background:var(--tc) !important;}
         .btn-dl-pdf,.btn-dl-jpg {background:var(--rsp-theme) !important;border-color:var(--rsp-theme) !important;color:#ffffff !important;}
         .btn-dl-pdf:hover,.btn-dl-pdf:focus,.btn-dl-jpg:hover,.btn-dl-jpg:focus {background:var(--tdk) !important;border-color:var(--tdk) !important;color:#ffffff !important;}
         table.dataTable thead th {background:var(--tc) !important;}
         .gt_col_heading {background:var(--tc) !important;}
         #tepe {border-bottom-color:var(--tc) !important;}
         #title {color:var(--tdk) !important;}
         #title2 {color:var(--tlt) !important;}
         h3,h4 {color:var(--tdk) !important;}
         .irs--shiny .irs-bar,.irs--shiny .irs-single {background:var(--rsp-theme) !important;border-top-color:var(--rsp-theme) !important;border-bottom-color:var(--rsp-theme) !important;}
         .rsp-sidebar-simple {background:linear-gradient(135deg,#ffffff 0%%,color-mix(in srgb,var(--rsp-theme) 9%%,#ffffff) 56%%,color-mix(in srgb,var(--rsp-theme) 18%%,#ffffff) 100%%) !important;border-color:color-mix(in srgb,var(--rsp-theme) 24%%,#ffffff) !important;}
         .rsp-sidebar-simple:before {background:linear-gradient(135deg,color-mix(in srgb,var(--rsp-theme) 22%%,#ffffff),var(--rsp-theme)) !important;}
         .rsp-sidebar-package {border-color:color-mix(in srgb,var(--rsp-theme) 18%%,#ffffff) !important;}
         .rsp-sidebar-lines div:nth-child(2), .ctt-main-title-08 .ctt-title-sub, .ctt-credit {color:var(--rsp-theme) !important;}
         .ctt-logo {border-color:var(--rsp-theme) !important;}
         .ctt-logo b, .ctt-domain b {color:var(--rsp-theme) !important;}
         .ctt-hero {background:radial-gradient(circle at 88%% 92%%, color-mix(in srgb, var(--rsp-theme) 16%%, transparent) 0, transparent 33%%), linear-gradient(135deg, #ffffff 0%%, color-mix(in srgb, var(--rsp-theme) 5%%, #ffffff) 62%%, color-mix(in srgb, var(--rsp-theme) 11%%, #ffffff) 100%%) !important; border-color:color-mix(in srgb, var(--rsp-theme) 18%%, #ffffff) !important;}",
        col, col, col, col, col, col, col
      )
      shinyjs::runjs(sprintf(
        "document.getElementById('dynamic-theme-style').innerHTML=`%s`;",
        gsub("`", "'", css)
      ))
    })
    
    
    # output$cols<- renderUI({   # WIDET RENDER UI RENK DEM
    #
    #   bbb<-aaa
    #
    #   setBackgroundColor(
    #     color = c("white", bbb),
    #     gradient = "linear",
    #     direction = c("bottom", "right")
    #   ) })
    
    
    
    
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
    # calib <- calibData <- dataGL <- kalan<- kalan.data<- km<- m1m2<- mke<- NULL
    # optionPlot<-  q3.1<- q3Table  <- secenek<- theta <- tumu<- NULL
    
    
  } # close server
  
  
  shinyApp(ui = ui, server = server)
}



