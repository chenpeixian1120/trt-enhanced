# Libraries ---------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr) 
library(ggplot2)
library(plotly)
library(readxl)
library(lubridate)
# Removed leaflet and RColorBrewer (map not used)
library(rhandsontable)
# Removed rstudioapi (not used)
library(shinyalert)
library(shinyjs)
# Removed install.packages("writexl") call to avoid side effects (write_xlsx loaded via writexl when needed)

# Define global simulation parameters ---------------------------------
numSim <- 1000           # Number of simulations to run
Samp <- 10               # Sample size chunk for calculating confidence intervals

# Initialize reactive values for baseline data (site info) -----------
# Will be updated upon file upload; default placeholders for 5 sites
initialSites <- 1:5
vals <- reactiveValues(
  baseData = data.frame(BaseT_Site = initialSites,
                        Location = LETTERS[initialSites],
                        Longitude = rep(NA, length(initialSites)),   # placeholder, no map usage
                        Latitude  = rep(NA, length(initialSites)),   # placeholder, no map usage
                        RecruitGoal = rep(50, length(initialSites))),
  sv_longr = NULL,       # will hold processed site-visit long data after upload
  sitePlanUploadData = NULL,  # will hold site plan table data after file upload
  scheduleData = NULL         # will hold scheduled visit data after file upload
)

# Simulation model for recruitment duration --------------------------
runSimulation <- function(param) {
  # Simulate recruitment durations numSim times given a recruit rate and goal
  durVector <- numeric(numSim)
  
  if (length(param$recruitRate) == 0 || length(param$RecruitGoal) == 0) {
    return(rep(NA, numSim))  # 如果参数为空，返回NA避免报错
  }
  for (i in seq_len(numSim)) {
    # Simulate recruitment process until reaching RecruitGoal
    sim <- cumsum(rpois(10000, param$recruitRate))
    reached <- which(sim >= param$RecruitGoal)
    durVector[i] <- ifelse(length(reached) > 0,
                           min(reached),
                           10000)          # ← 到不了目标就记 10000 周 
  }
  
  return(durVector)
}

# Confidence interval calculation for simulation output -------------
calc_CI <- function(simVector, Prob) {
  # Calculate confidence interval for mean duration from simulation results
  noSamp <- numSim / Samp
  # Compute sample means for chunks of simulation results (without global assignment)
  xbar_vals <- numeric(noSamp)
  for (i in 1:noSamp) {
    idx_low <- (i - 1) * Samp + 1
    idx_high <- i * Samp
    xbar_vals[i] <- mean(simVector[idx_low:idx_high])
  }
  # Calculate t-based confidence interval for the mean of durations
  stdDev <- sd(xbar_vals)
  xdbar <- mean(xbar_vals)
  lower <- xdbar - stdDev * qt((1 - Prob) / 2, noSamp - 1, lower.tail = FALSE) / sqrt(noSamp)
  upper <- xdbar + stdDev * qt((1 - Prob) / 2, noSamp - 1, lower.tail = FALSE) / sqrt(noSamp)
  return(c(lower, upper))
}

# UI Components ---------------------------------

# Dashboard header with title
header <- dashboardHeader(
  title = tags$a(href='http://www.sun.ac.za/english',
                 tags$img(src='logo.png')),
  titleWidth = 600
)

# Sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Planning", tabName = "plan", icon = icon("edit")),
              menuItem("Monitoring", tabName = "monitor", icon = icon("line-chart"),
                       menuSubItem("1. Latest Recruitment Data", tabName = "baseline"),
                       menuSubItem("2. Trial Overview", tabName = "monTrial"),
                       menuSubItem("3. Site Information", tabName = "monSite")
                       # Removed separate file inputs for baseline and recruitment data from sidebar
              )
  )
)

# Planning Page UI ---------------------------------
planPage <- fluidPage(
  fluidRow(
    box(
      # Tabs for Planning outputs
      column(width = 12,
             tabBox(
               tabPanel("Overview",
                        fluidRow(
                          box(title = "Specify trial parameters", status = "primary", width = 2,
                              numericInput("planRecruitRate", "Expected recruitment rate per week:", value = 1.12, min = 0, step = 0.01),
                              numericInput("planSample", "Total recruitment goal for the trial:", value = 60, min = 0, max = 100000, step = 1),
                              numericInput("planProb", "Confidence level for prediction (%):", value = 95, min = 0, max = 100, step = 0.01)
                          ),
                          box(title = "Output", status = "primary", width = 10,
                              fluidRow(
                                valueBoxOutput("recGoal", width = 4),
                                valueBoxOutput("expDate", width = 4),
                                valueBoxOutput("planRecRate", width = 4)
                              ),
                              fluidRow()
                          )
                        )
               ),
               tabPanel("Multiple Sites",
                        fluidRow(
                          box(title = "Specify trial parameters", status = "primary", width = 3,
                              numericInput("siteQty", "Specify the number of sites:", value = 10, min = 2, step = 1),
                              fileInput("sitePlanUpload", "Upload site plan (.csv or .xlsx):", accept = c(".csv", ".xlsx")),
                              "Once the table has been populated, click the button below:",
                              rHandsontableOutput("sitePlanTable"),
                              numericInput("multiplanProb", "Confidence level for prediction (%):", value = 95, min = 0, max = 100, step = 0.01),
                              actionButton("runSim", "Run Simulation")
                          ),
                          box(title = "Output", status = "primary", width = 9,
                              fluidRow(
                                valueBoxOutput("multiRecGoal", width = 4),
                                valueBoxOutput("multiDurationInfo", width = 4),
                                valueBoxOutput("multiSiteNo", width = 4)
                              ),
                              fluidRow(
                                plotlyOutput("planMultiPlot", height = "auto", width = "100%")
                              )
                          )
                        )
               ),
               width = 12
             )
      ),
      width = 12
    )
  )
)

# Monitoring - Site Information Page UI -----------------------------
siteMonitor <- fluidPage(
  # Site Overview: Bar chart of recruitment per site (Map removed)
  fluidRow(
    box(title = "Site Overview", width = 12,
        # Removed tabBox with Map; only showing the site bar graph
        plotlyOutput("sitePlot", height = "500px")
    )
  ),
  # Site-specific summary metrics and controls
  fluidRow(
    # Value boxes for selected site metrics
    column(width = 4, valueBoxOutput("vbSiteRec", width = 12)),
    column(width = 4, valueBoxOutput("vbSiteEnd", width = 12)),
    column(width = 4, valueBoxOutput("vbSiteRate", width = 12))
  ),
  fluidRow(
    # Site timeline graph and input controls
    column(width = 9,
           plotlyOutput("siteGraph", height = "400px")
    ),
    column(width = 3,
           selectInput("siteSelect", "Select a site:", choices = c("All")),
           numericInput("siteRate", "Current weekly recruitment rate:", value = 0, step = 0.001, min = 0),
           numericInput("sitePend", "Number of participants pending:", value = 0, step = 1, min = 0),
           numericInput("siteplanProb", "Confidence level for site projection (%):", value = 95, min = 0, max = 100, step = 0.01)
    )
  )
)

# Monitoring - Trial Overview Page UI ------------------------------
trialMonitor <- fluidPage(
  fluidRow(
    box(title = "Overview", width = 12,
        fluidRow(
          column(width = 4, valueBoxOutput("valueRec", width = 12)),
          column(width = 4, valueBoxOutput("expDur", width = 12)),
          column(width = 4, valueBoxOutput("detSite", width = 12))
        ),
        fluidRow(
          column(width = 12, plotlyOutput("recrGraph", width = "100%"))
        )
    )
  ),
  fluidRow(
    box(title = "Visit Completion Rate", status = "primary", width = 12,
        selectInput("vc_site", "Select a site:", choices = NULL),
        plotlyOutput("visitCompletionPlot", height = "400px"))
  ),
  fluidRow(
    box(title = "Visit Timing Status", status = "primary", width = 12,
        fileInput("schedUpload", "Upload scheduled visit data (.csv or .xlsx):", accept = c(".csv", ".xlsx")),
        numericInput("winDays", "Visit window (± days):", value = 7, min = 0, step = 1),
        plotlyOutput("visitTimingPlot", height = "400px"))
  )
)

# Baseline Parameters Page UI (Data Upload & Preview) ---------------
baselinePage <- fluidPage(
  fluidRow(
    box(title = "Import Latest Data", width = 12, solidHeader = TRUE,
        # Single file input for both baseline and recruitment data (.csv or .xlsx)
        fileInput("dataUpload", "Upload here (.csv or .xlsx):", accept = c(".csv", ".xlsx"))
    )
  ),
  fluidRow(
    box(title = "Site Visit Data Preview (first 25 rows)", width = 12, solidHeader = TRUE,
        # Show first 25 rows of the processed long data (scrollable)
        DT::dataTableOutput("previewTable")
    )
  )
)

# Dashboard body assembling all pages ------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "plan",
            h1("Planning"),
            planPage
    ),
    tabItem(tabName = "monitor"  # empty parent tab (subtabs used)
    ),
    tabItem(tabName = "monTrial",
            h1("Trial Overview"),
            trialMonitor
    ),
    tabItem(tabName = "monSite",
            h1("Site Breakdown"),
            siteMonitor
    ),
    tabItem(tabName = "baseline",
            h1("Latest Recruitment Data"),
            baselinePage
    )
  )
)

# Complete UI with header, sidebar, and body -----------------------
ui <- dashboardPage(header, sidebar, body)

# Server logic ---------------------------------
server <- function(input, output, session){
  
  # ---- 试验总目标：site-plan 优先，其次 baseData 数，再退到手动 planSample ----
  totalRecruitGoal <- reactive({
    if (!is.null(vals$baseData) && "RecruitGoal" %in% names(vals$baseData))
      return(sum(vals$baseData$RecruitGoal, na.rm = TRUE))
    if (!is.null(input$planSample) && !is.na(input$planSample))
      return(input$planSample)
    NA_real_
  })
  
  
  # Reactive expression to read uploaded file (CSV or Excel) --------
  rawData <- reactive({
    req(input$dataUpload)  # ensure a file is uploaded
    file <- input$dataUpload
    ext <- tools::file_ext(file$name)
    # Read data based on file extension
    if (ext %in% c("csv", "txt")) {
      # Assume CSV has header and comma separator
      read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      read_excel(file$datapath, sheet = 1)
    } else {
      shinyalert(title = "Error", text = "Unsupported file type. Please upload a .csv or .xlsx file.", type = "error")
      return(NULL)
    }
  })
  
  
  # Observe file upload to process data ----------------------------
  # 处理 trial 访视原始数据  + 生成/覆盖 baseData ----------------------
  observeEvent(rawData(), {
    df <- rawData()
    if (is.null(df)) return()
    
    ## ---------- 1. 解析访视表，得到 sv_long -------------------------
    long_format <- all(c("record_id","visit","visit_comp","visitnotdone","dov") %in% names(df))
    if (long_format) {
      sv <- df
    } else {
      sv <- tidyr::pivot_longer(
        df,
        cols = -c(record_id, arm, site),
        names_to = c("visit", ".value"),
        names_pattern = "^(.*)_(comp|notdone|dov)$"
      )
      names(sv)[names(sv) == "comp"]    <- "visit_comp"
      names(sv)[names(sv) == "notdone"] <- "visitnotdone"
    }
    
    if (any(class(sv$dov) %in% c("character","numeric"))) {
      sv$dov <- suppressWarnings(lubridate::dmy(sv$dov))
      if (all(is.na(sv$dov)) && is.numeric(df$dov))
        sv$dov <- as.Date(df$dov, origin = "1899-12-30")
    }
    
    sv$visit_comp[is.na(sv$visit_comp)] <- 0
    sv$visitnotdone[is.na(sv$visitnotdone)] <- 0
    
    sv_long <- sv %>% 
      group_by(record_id, site) %>% 
      tidyr::complete(visit = unique(sv$visit),
                      fill = list(visit_comp = 0, visitnotdone = 0, dov = NA)) %>% 
      ungroup()
    
    vals$sv_longr <- sv_long
    
    ## ---------- 2. 生成默认 baseDataNew -----------------------------
    sites <- sort(unique(sv_long$site))
    nSites <- length(sites)
    totalGoal <- if (!is.null(input$planSample)) input$planSample else dplyr::n_distinct(sv_long$record_id)
    
    base_goal <- floor(totalGoal / nSites)
    remainder <- totalGoal - base_goal * nSites
    
    baseDataNew <- data.frame(
      BaseT_Site = seq_len(nSites),
      Location = sites,
      Longitude = NA,
      Latitude = NA,
      RecruitGoal = base_goal,
      stringsAsFactors = FALSE
    )
    
    if (remainder > 0)
      baseDataNew$RecruitGoal[1:remainder] <- baseDataNew$RecruitGoal[1:remainder] + 1
    
    ## 【关键新增修复部分】确保RecruitGoal非NA
    baseDataNew$RecruitGoal[is.na(baseDataNew$RecruitGoal)] <- 10
    
    ## ---------- 3. 若已上传 site-plan，用其 RecruitGoal 覆盖 ----------
    if (!is.null(vals$sitePlanUploadData)) {
      if ("BaseT_Site" %in% names(vals$sitePlanUploadData)) {
        goal_tbl <- vals$sitePlanUploadData %>%            
          dplyr::select(BaseT_Site, RecruitGoal.plan = RecruitGoal)
        baseDataNew <- baseDataNew %>% 
          dplyr::left_join(goal_tbl, by = "BaseT_Site") %>% 
          dplyr::mutate(RecruitGoal = ifelse(!is.na(RecruitGoal.plan),
                                             RecruitGoal.plan, RecruitGoal)) %>% 
          dplyr::select(-RecruitGoal.plan)
      } else {
        goal_tbl <- vals$sitePlanUploadData %>%            
          dplyr::select(Location, RecruitGoal.plan = RecruitGoal)
        baseDataNew <- baseDataNew %>% 
          dplyr::left_join(goal_tbl, by = "Location") %>% 
          dplyr::mutate(RecruitGoal = ifelse(!is.na(RecruitGoal.plan),
                                             RecruitGoal.plan, RecruitGoal)) %>% 
          dplyr::select(-RecruitGoal.plan)
      }
    }
    ## ---------- 4. 更新全局 baseData & 下拉列表 ----------------------
    baseDataNew$Location <- factor(baseDataNew$Location, levels = sites)
    vals$baseData <- baseDataNew
    
    updateSelectInput(session, "siteSelect",
                      choices  = c("All", levels(baseDataNew$Location)),
                      selected = "All")
  })
  # ------------------------------------------------------------------
  
  # Observe site plan file upload to update baseData and table ----
  observeEvent(input$sitePlanUpload, {
    file <- input$sitePlanUpload
    req(file)
    ext <- tools::file_ext(file$name)
    sitePlanDf <- if (ext %in% c("csv", "txt")) {
      read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      read_excel(file$datapath, sheet = 1)
    } else {
      shinyalert(title = "Error", text = "Unsupported file type. Please upload a .csv or .xlsx file for site plan.", type = "error")
      return(NULL)
    }
    # Validate required columns in site plan file
    if (!all(c("BaseT_Site", "Location", "RecruitGoal") %in% names(sitePlanDf))) {
      shinyalert(title = "Error", text = "Site plan file must contain columns: BaseT_Site, Location, RecruitGoal.", type = "error")
      return(NULL)
    }
    # Prepare new baseData from the uploaded site plan
    baseDataNew <- sitePlanDf[, c("BaseT_Site", "Location", "RecruitGoal")]
    # Add missing columns Longitude and Latitude as NA
    baseDataNew$Longitude <- NA
    baseDataNew$Latitude <- NA
    # Reorder columns to match baseData structure
    baseDataNew <- baseDataNew[, c("BaseT_Site", "Location", "Longitude", "Latitude", "RecruitGoal")]
    # Ensure Location is factor with levels sorted
    sites <- sort(unique(baseDataNew$Location))
    baseDataNew$Location <- factor(baseDataNew$Location, levels = sites)
    # Update reactive baseData
    vals$baseData <- baseDataNew
    # Create table data for site plan (including recruitRate and StartDelay defaults)
    dfTable <- data.frame(
      SiteNo = baseDataNew$BaseT_Site,
      Location = as.character(baseDataNew$Location),
      recruitRate = rep(1, nrow(baseDataNew)),
      RecruitGoal = baseDataNew$RecruitGoal,
      StartDelay = rep(0, nrow(baseDataNew)),
      stringsAsFactors = FALSE
    )
    vals$sitePlanUploadData <- dfTable
    # Inform user if needed (no alert required here, table will update automatically)
  })
  
  # ---- schedule file (visit, scheduledtime_indays) ------------------
  observeEvent(input$schedUpload, {
    file <- input$schedUpload;  req(file)
    ext  <- tools::file_ext(file$name)
    
    schedDf <- if (ext %in% c("csv","txt")) {
      read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx","xls")) {
      readxl::read_excel(file$datapath, sheet = 1)
    } else {
      shinyalert("Error",
                 "Schedule file must be .csv or .xlsx", type = "error")
      return(NULL)
    }
    
    # must have exactly two columns
    if (!all(c("visit","scheduledtime_indays") %in% names(schedDf))) {
      shinyalert("Error",
                 "Schedule file needs columns: visit, scheduledtime_indays", "error")
      return(NULL)
    }
    
    schedDf$scheduledtime_indays <- as.numeric(schedDf$scheduledtime_indays)
    vals$scheduleData <- schedDf        # store in reactiveValues
  })
  
  
  # Reactive data: allData (one row per recruited participant with first visit date) ---
  allData <- reactive({
    req(vals$sv_longr, vals$baseData)
    sv_long <- vals$sv_longr
    if (is.null(sv_long)) return(NULL)
    # Determine each participant's recruitment date (use visit "T0" or earliest completed visit)
    recruits <- sv_long %>% 
      filter((visit == "T0" | visit == "Baseline" | visit == "V0") & visit_comp == 1)
    if (nrow(recruits) == 0) {
      # If no explicit baseline visit identified, use earliest completed visit per participant
      recruits <- sv_long %>% 
        filter(visit_comp == 1) %>% 
        group_by(record_id, site) %>% 
        slice_min(order_by = dov, with_ties = FALSE) %>% 
        ungroup()
    }
    recruits <- recruits %>% 
      mutate(Date = as.Date(dov)) %>%    # ensure Date is Date type
      # Join numeric site ID from baseData
      left_join(vals$baseData[, c("BaseT_Site", "Location")], by = c("site" = "Location"))
    # Prepare summary data frame with required columns
    summaryData <- recruits %>% transmute(BaseT_Site = BaseT_Site, Location = site, Date = Date)
    return(summaryData)
  })
  
  # Reactive: sortSum (assign week numbers relative to start) -------
  sortSum <- reactive({
    data <- allData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    # Calculate week index (1 = first week of earliest recruit date)
    minYear <- year(min(data$Date))
    weekIndex <- week(data$Date) + (year(data$Date) - minYear) * 52
    weekIndex <- weekIndex - min(weekIndex) + 1
    data <- cbind(data, WeekNo = weekIndex)
    return(data)
  })
  
  # Reactive: siteSum (cumulative recruits per week per site) -------
  siteSum <- reactive({
    data <- sortSum()
    if (is.null(data)) return(NULL)
    # Group by site and week, count recruits, then get cumulative
    sumData <- data %>% group_by(BaseT_Site, Location, WeekNo) %>% summarise(Total = n(), .groups = 'drop')
    sumData <- sumData %>% group_by(BaseT_Site, Location) %>% mutate(cum = cumsum(Total)) %>% ungroup()
    return(sumData)
  })
  
  # Reactive: filterSum (for any date range filtering in UI) --------
  filterSum <- reactive({
    data <- allData()
    if (is.null(data)) return(NULL)
    if (is.null(input$monitorPeriod)) {
      return(data %>% group_by(Location) %>% summarise(Total = n()))
    }
    dateRange <- input$monitorPeriod
    data %>% filter(Date >= dateRange[1] & Date <= dateRange[2]) %>%
      group_by(Location) %>% summarise(Total = n(), .groups = 'drop')
  })
  
  # Reactive: siteOverview (summary per site with Total recruits and site info) ---
  # --- Site overview (include every site, even 0-recruit ones) -------
  siteOverview <- reactive({
    req(vals$baseData)                     # need site list & goals
    base <- vals$baseData                  # BaseT_Site, Location, RecruitGoal …
    
    cnt  <- siteSum() %>%                  # may be empty for zero-recruit sites
      group_by(BaseT_Site) %>%
      summarise(Total = max(cum), .groups = "drop")
    
    out <- base %>%
      left_join(cnt, by = "BaseT_Site") %>%
      mutate(
        Total       = tidyr::replace_na(Total, 0),
        RecruitGoal = tidyr::replace_na(RecruitGoal, 0)
      ) %>%
      select(BaseT_Site, Location, Longitude, Latitude,
             Total, RecruitGoal)
    
    return(out)
  })
  
  # ---- site-level周招募率 ----
  
  siteRecrRateReactive <- reactive({
    ss <- siteSum()                       # 每周累计
    trialAvg <- trialRecrRate()           # 全试验平均
    if (is.na(trialAvg) || trialAvg <= 0) trialAvg <- 0.1   # 最小正值
    
    if (is.null(ss)) {
      return(data.frame(BaseT_Site = vals$baseData$BaseT_Site,
                        recruitRate = trialAvg))
    }
    
    rates <- ss %>%
      group_by(BaseT_Site) %>%
      summarise(recruitRate = max(cum) / max(WeekNo), .groups = "drop")
    
    full_join(data.frame(BaseT_Site = vals$baseData$BaseT_Site),
              rates, by = "BaseT_Site") %>%
      mutate(recruitRate = ifelse(is.na(recruitRate) | recruitRate <= 0,
                                  trialAvg,
                                  recruitRate))
  })
  
  # ---- simulate remaining duration per site -------------------------
  siteSim <- reactive({
    req(vals$sv_longr, vals$baseData)     # 确保数据已上传
    
    so <- siteOverview()                  # 已招总数 / 目标
    if (is.null(so)) return(NULL)
    
    rr <- siteRecrRateReactive()          # 每站速率
    
    tmp <- so %>% 
      left_join(rr, by = "BaseT_Site") %>%           # 合并速率
      mutate(
        recruitRate = ifelse(is.na(recruitRate) | recruitRate <= 0,
                             0.1,                    # 兜底正值
                             recruitRate),
        pending     = pmax(0, RecruitGoal - Total)  # 待招人数
      )
    
    if (all(tmp$pending == 0)) return(NULL)          # 全部已满额
    
    out <- tmp %>% 
      rowwise() %>% 
      mutate(
        ci      = list(calc_CI(
          runSimulation(data.frame(
            recruitRate = recruitRate,
            RecruitGoal = pending)), 0.95)),
        lowerCI = ci[1],
        upperCI = ci[2]
      ) %>% 
      ungroup()
    
    return(out)
  })
  
  # Reactive: trialSum (overall recruits per week, cumulative) ------
  trialSum <- reactive({
    data <- sortSum()
    if (is.null(data)) return(NULL)
    totalByWeek <- data %>% group_by(WeekNo) %>% summarise(Total = n(), .groups = 'drop')
    totalByWeek <- totalByWeek %>% mutate(cum = cumsum(Total))
    return(totalByWeek)
  })
  
  # Reactive: listLocation (vector of site names for filtering, if needed) --
  listLocation <- reactive({
    levels(vals$baseData$Location)
  })
  
  # Reactive: trial-level recruitment rate (lambda) -----------------
  trialRecrRate <- reactive({
    data <- sortSum()
    if (is.null(data)) return(NA)
    # Calculate overall average recruits per week (Poisson mean)
    weeklyCounts <- data %>% group_by(WeekNo) %>% summarise(Total = n(), .groups = 'drop')
    siteObs <- max(weeklyCounts$WeekNo)
    freqData <- weeklyCounts %>% group_by(Total) %>% summarise(freq = n(), .groups = 'drop')
    # Weighted mean of Poisson is sum(Total*freq)/sum(freq)
    lambda <- sum(freqData$Total * freqData$freq) / sum(freqData$freq)
    return(lambda)
  })
  
  # Update siteSelect choices when baseData changes (in case of manual baseline edit) --
  observeEvent(vals$baseData, {
    # Keep siteSelect choices in sync with baseData Location levels
    updateSelectInput(session, "siteSelect",
                      choices  = c("All", levels(vals$baseData$Location)),
                      selected = if (is.null(input$siteSelect)) "All" else input$siteSelect)
    updateSelectInput(session, "vc_site", choices = levels(vals$baseData$Location),
                      selected = if (nlevels(vals$baseData$Location) > 0) levels(vals$baseData$Location)[1] else NULL)
  })
  
  # Update numeric inputs for selected site (called within siteGraph render) ---
  updateNum <- reactive({
    # Recalculate site-specific pending and rate when site selection changes
    req(input$siteSelect)
    siteName <- input$siteSelect
    # Find site numeric ID
    siteRow <- vals$baseData[vals$baseData$Location == siteName, ]
    if (nrow(siteRow) == 0) return(NULL)
    siteID <- siteRow$BaseT_Site[1]
    # Get current recruited total and recruit rate for this site
    curTotal <- 0
    curRate <- 0
    siteData <- siteOverview()
    if (!is.null(siteData)) {
      si <- siteData %>% filter(BaseT_Site == siteID)
      if (nrow(si) > 0) curTotal <- si$Total[1]
    }
    rates <- siteRecrRateReactive()
    if (!is.null(rates)) {
      ri <- rates %>% filter(BaseT_Site == siteID)
      if (nrow(ri) > 0 && !is.na(ri$recruitRate)) curRate <- ri$recruitRate
    }
    # Calculate pending recruits (goal - current), not below 0
    goal <- siteRow$RecruitGoal[1]
    pending <- max(0, goal - curTotal)
    # Update numeric input fields in UI
    updateNumericInput(session, "siteRate", value = round(curRate, 4))
    updateNumericInput(session, "sitePend", value = pending)
  })
  
  # Synchronize siteSelect with plot clicks (bar chart or map) ----
  # (Removed observeEvent for siteMap marker click, since map is removed)
  observeEvent(event_data("plotly_click", source = "sitePlot"), {
    click <- event_data("plotly_click", source = "sitePlot")
    if (!is.null(click$y))
      updateSelectInput(session, "siteSelect", selected = as.character(click$y))
  })
  
  # ValueBox Outputs and Plots -------------------------------------
  
  # Baseline Parameters page: preview table of uploaded data (first 25 rows)
  output$previewTable <- DT::renderDataTable({
    req(vals$sv_longr)
    # Display first 25 rows of sv_longr in a scrollable table
    head(vals$sv_longr, 25)
  }, options = list(pageLength = 25, scrollY = "300px"))
  
  # Planning page outputs (value boxes and plot)
  output$durationInfo <- renderValueBox({
    # Estimated recruitment duration (weeks) for given parameters
    val <- round(normDuration(), 0)
    valueBox(paste(val, "weeks"), "Estimated recruitment duration", icon = icon("clock"), color = "teal")
  })
  output$expDate <- renderValueBox({
    # Confidence interval for recruitment duration
    lambda <- input$planRecruitRate
    n <- input$planSample
    simData <- data.frame(recruitRate = lambda, RecruitGoal = n)
    simVec <- runSimulation(simData)
    ci <- calc_CI(simVec, input$planProb / 100)
    valueBox(paste0(round(ci[1], 2), " - ", round(ci[2], 2), " weeks"), "Estimated recruitment duration", icon = icon("clock"), color = "purple")
  })
  output$recGoal <- renderValueBox({
    # Total specified recruitment goal (from input)
    valueBox(paste(input$planSample, "participants"), "Total specified recruitment goal", icon = icon("users"), color = "teal")
  })
  output$planRecRate <- renderValueBox({
    # Expected weekly recruitment rate (from input)
    valueBox(input$planRecruitRate, "Expected number of recruitments per week", icon = icon("line-chart"), color = "light-blue")
  })
  output$planPlot <- renderPlotly({
    # Distribution of simulated trial durations (histogram)
    simVec <- simDuration()
    # Compute distribution of sample mean durations (without using global xbar)
    noSamp <- numSim / Samp
    xbar_vals <- sapply(1:noSamp, function(i) mean(simVec[((i-1)*Samp + 1):(i*Samp)]))
    simData <- data.frame(Durations = round((xbar_vals - 1.5) / 1) * 1)  # rounding as per original logic
    simData <- simData %>% group_by(Durations) %>% summarise(Frequency = n() / length(xbar_vals), .groups = 'drop')
    plot_ly(simData, x = ~Durations, y = ~Frequency, type = "scatter", mode = "lines", fill = "tozeroy") %>%
      layout(title = "Distribution of Simulated Recruitment Durations",
             xaxis = list(title = "Duration (Weeks)"),
             yaxis = list(title = "Probability"))
  })
  
  # Multiple Sites planning outputs
  output$sitePlanTable <- renderRHandsontable({
    # Generate editable table for site-specific planning parameters
    if (!is.null(vals$sitePlanUploadData)) {
      # Use uploaded site plan data if available
      dfSitePlan <- vals$sitePlanUploadData
    } else {
      noSites <- input$siteQty
      dfSitePlan <- data.frame(SiteNo = 1:noSites,
                               recruitRate = rep(1, noSites),
                               RecruitGoal = rep(10, noSites),
                               StartDelay = rep(0, noSites))
    }
    rhandsontable(dfSitePlan)
  })
  multiSitePlan <- eventReactive(input$runSim, {
    # Simulate recruitment duration for each site in the planning table
    sitePlan <- hot_to_r(input$sitePlanTable)
    if (is.null(sitePlan)) return(NULL)
    noSites <- nrow(sitePlan)
    sitePlan <- sitePlan %>% select(SiteNo, recruitRate, RecruitGoal)
    siteCI <- data.frame(SiteNo = sitePlan$SiteNo, lowerCI = NA, upperCI = NA)
    for (i in 1:noSites) {
      simVec <- runSimulation(sitePlan[i, ])
      ci <- calc_CI(simVec, input$multiplanProb / 100)
      siteCI$lowerCI[i] <- ci[1]
      siteCI$upperCI[i] <- ci[2]
    }
    return(siteCI)
  })
  output$multiRecGoal <- renderValueBox({
    # Sum of site recruitment goals (from planning table)
    sitePlan <- hot_to_r(input$sitePlanTable)
    totalGoal <- if (!is.null(sitePlan)) sum(sitePlan$RecruitGoal) else 0
    valueBox(paste(totalGoal, "participants"), "Total specified recruitment goal", icon = icon("users"), color = "teal")
  })
  output$multiDurationInfo <- renderValueBox({
    # Overall estimated duration (weeks) for multi-site plan (max of CIs)
    sitePlan <- hot_to_r(input$sitePlanTable)
    if (is.null(sitePlan)) {
      return(valueBox("0 weeks", "Estimated recruitment duration", icon = icon("clock"), color = "purple"))
    }
    # Include any start delays in CI calculation
    siteDelay <- sitePlan$StartDelay
    siteCI <- multiSitePlan()
    siteCI$lowerCI <- siteCI$lowerCI + siteDelay
    siteCI$upperCI <- siteCI$upperCI + siteDelay
    idx <- which.max(siteCI$upperCI)
    valueBox(paste0(round(siteCI$lowerCI[idx], 2), " - ", round(siteCI$upperCI[idx], 2), " weeks"),
             "Estimated recruitment duration", icon = icon("clock"), color = "purple")
  })
  output$multiSiteNo <- renderValueBox({
    # Identify the site with the longest recruitment duration (determinative site)
    sitePlan <- hot_to_r(input$sitePlanTable)
    if (is.null(sitePlan)) {
      return(valueBox("Site ...", "Determinative site", icon = icon("building"), color = "light-blue"))
    }
    siteDelay <- sitePlan$StartDelay
    siteCI <- multiSitePlan()
    siteCI$upperCI <- siteCI$upperCI + siteDelay
    idx <- which.max(siteCI$upperCI)
    valueBox(paste("Site number", siteCI$SiteNo[idx]), "Determinative site", icon = icon("building"), color = "light-blue")
  })
  output$planMultiPlot <- renderPlotly({
    # Plot distribution of recruitment durations for each site in multi-site plan
    siteCI <- multiSitePlan()
    if (is.null(siteCI)) {
      return(plot_ly() %>% layout(title = "Duration Distribution per Site"))
    }
    # Create error bar chart of duration CIs per site
    plot_ly(siteCI, x = ~SiteNo, y = ~upperCI, type = 'bar', name = 'Upper CI', marker = list(color = 'rgba(204,204,204,1)')) %>%
      add_trace(y = ~lowerCI, name = 'Lower CI', marker = list(color = 'rgba(186, 228, 179, 1)')) %>%
      layout(title = "Estimated Recruitment Duration per Site",
             xaxis = list(title = "Site Number"),
             yaxis = list(title = "Duration (weeks)", rangemode = "tozero"),
             barmode = "overlay")
  })
  
  ### 1) Trial Overview - 已招人数 / 总目标 ----------------
  output$valueRec <- renderValueBox({
    req(allData())
    valueBox(
      paste(nrow(allData()), "out of", totalRecruitGoal()),
      "Total number of participants recruited",
      icon = icon("users"), color = "teal"
    )
  })
  
  ### 2) Trial Overview - 剩余持续时间 ---------------------
  output$expDur <- renderValueBox({
    ci_tbl <- siteSim()
    if (is.null(ci_tbl))                               # 完全算不出
      return(valueBox("No data", "Expected remaining recruitment duration",
                      icon = icon("clock"), color = "purple"))
    
    ci_tbl <- dplyr::filter(ci_tbl, !is.na(upperCI))   # 去掉 NA 行
    if (nrow(ci_tbl) == 0)
      return(valueBox("No data", "Expected remaining recruitment duration",
                      icon = icon("clock"), color = "purple"))
    
    idx <- which.max(ci_tbl$upperCI)
    valueBox(sprintf("%.0f – %.0f weeks",
                     ci_tbl$lowerCI[idx], ci_tbl$upperCI[idx]),
             "Expected remaining recruitment duration",
             icon = icon("clock"), color = "purple")
  })
  
  ### 3) Trial Overview - 决定性站点 -----------------------
  output$detSite <- renderValueBox({
    ci_tbl <- siteSim()
    if (is.null(ci_tbl))
      return(valueBox("No data", "Determinative site",
                      icon = icon("building"), color = "light-blue"))
    
    ci_tbl <- dplyr::filter(ci_tbl, !is.na(upperCI))
    if (nrow(ci_tbl) == 0)
      return(valueBox("B", "Determinative site",
                      icon = icon("building"), color = "light-blue"))
    
    worst <- ci_tbl$BaseT_Site[which.max(ci_tbl$upperCI)]
    valueBox(vals$baseData$Location[vals$baseData$BaseT_Site == worst],
             "Determinative site",
             icon = icon("building"), color = "light-blue")
  })
  
  output$recrGraph <- renderPlotly({
    # Overall recruitment curve (cumulative recruits over time)
    if (is.null(input$dataUpload) || is.null(trialSum())) {
      return(plot_ly() %>% layout(title = "Recruitment Curve",
                                  xaxis = list(title = "Week Number"),
                                  yaxis = list(title = "Cumulative Recruitment")))
    }
    data <- trialSum()
    plot_ly(data, x = ~WeekNo, y = ~cum, type = "scatter", mode = "lines", fill = "tozeroy") %>%
      layout(title = "Recruitment Curve",
             xaxis = list(title = "Week Number"),
             yaxis = list(title = "Cumulative Recruitment"))
  })
  
  
  output$sitePlot <- renderPlotly({
    
    # Horizontal bar chart: Recruitment Goal vs Current Recruitment by site
    if (is.null(input$dataUpload) || is.null(siteOverview())) {
      return(plot_ly() %>% layout(title = "Recruitment by Site",
                                  xaxis = list(title = "Count"),
                                  yaxis = list(title = "Site")))
    }
    siteData <- siteOverview()
    # Plot recruitment goal and current total for each site
    p <- plot_ly(siteData, x = ~RecruitGoal, y = ~Location, type = "bar", orientation = "h",
                 source = "sitePlot", name = "Recruitment Goal", marker = list(color = "#EADEDB"), hoverinfo = "x+y") %>%
      add_trace(x = ~Total, name = "Current Recruitment", marker = list(color = "#77BA99"), hoverinfo = "x+y") %>%
      layout(title = "Recruitment Goal vs Current by Site",
             xaxis = list(title = "Participants"),
             yaxis = list(title = "Site", categoryorder = "total ascending"),
             barmode = "overlay")
    
    p <- event_register(p, "plotly_click") 
    
    # Add text annotations for "X out of Y (Z%)"
    # 单次注释：X out of Y (%) 或 goal NA
    p <- p %>% add_annotations(
      x = siteData$RecruitGoal + 2,   # 调位置
      y = siteData$Location,
      text = ifelse(is.na(siteData$RecruitGoal),
                    "goal NA",
                    paste(siteData$Total, "out of",
                          siteData$RecruitGoal,
                          sprintf("(%d%%)",
                                  round(100 * siteData$Total /
                                          siteData$RecruitGoal)))),
      showarrow = FALSE,
      font = list(size = 11)
    )
    
    return(p)
  })
  
  output$vbSiteRec <- renderValueBox({
    # Selected site: number of recruitments out of target
    if (is.null(input$dataUpload)) {
      return(valueBox("0 out of 0 participants", "Number of recruitments (selected site)", icon = icon("users"), color = "teal"))
    }
    baseLine <- siteBase()    # planned goal for site
    curRecr <- siteRecr()     # current recruited count for site
    valueBox(paste(curRecr, "out of", baseLine, "participants"), "Number of recruitments (selected site)", icon = icon("users"), color = "teal")
  })
  output$vbSiteRate <- renderValueBox({
    if (is.null(input$dataUpload)) {
      return(valueBox("0", "Recruitments per week (selected site)",
                      icon = icon("line-chart"), color = "light-blue"))
    }
    
    if (input$siteSelect == "All") {
      rateVal <- trialRecrRate()                    # 全试验平均
    } else {
      siteName <- input$siteSelect
      rates    <- siteRecrRateReactive()
      rateVal  <- 0
      if (!is.null(rates)) {
        siteID <- vals$baseData$BaseT_Site[vals$baseData$Location == siteName]
        rr     <- rates %>% filter(BaseT_Site == siteID)
        if (nrow(rr) > 0 && !is.na(rr$recruitRate)) rateVal <- rr$recruitRate
      }
    }
    
    valueBox(round(rateVal, 4), "Recruitments per week (selected site)",
             icon = icon("line-chart"), color = "light-blue")
  })
  
  output$vbSiteEnd <- renderValueBox({
    if (is.null(input$dataUpload)) {
      return(valueBox("0 weeks", "Expected remaining duration (selected site)",
                      icon = icon("clock"), color = "purple"))
    }
    
    # ---- All 站点：取 trial-level 的“最慢站” CI ----
    if (input$siteSelect == "All") {
      ci_tbl <- siteSim()
      if (is.null(ci_tbl) || nrow(ci_tbl) == 0) {
        return(valueBox("No data", "Expected remaining duration (selected site)",
                        icon = icon("clock"), color = "purple"))
      }
      idx <- which.max(ci_tbl$upperCI)
      return(valueBox(sprintf("%.2f – %.2f weeks",
                              ci_tbl$lowerCI[idx], ci_tbl$upperCI[idx]),
                      "Expected remaining duration (selected site)",
                      icon = icon("clock"), color = "purple"))
    }
    
    # ---- 单一站点：沿用原逻辑 ----
    simParam <- data.frame(recruitRate = input$siteRate,
                           RecruitGoal = input$sitePend)
    simVec <- runSimulation(simParam)
    ci     <- calc_CI(simVec, input$siteplanProb / 100)
    
    valueBox(sprintf("%.2f – %.2f weeks", ci[1], ci[2]),
             "Expected remaining duration (selected site)",
             icon = icon("clock"), color = "purple")
  })
  output$siteGraph <- renderPlotly({
    if (is.null(input$dataUpload)) {
      return(plot_ly() %>% layout(title = "Recruitment Over Time (Site)",
                                  xaxis = list(title = "Week Number"),
                                  yaxis = list(title = "Cumulative Recruitment")))
    }
    
    # ---- All：画整体累计曲线 ----
    if (input$siteSelect == "All") {
      ts <- trialSum()
      
      # 若 trialSum 空，再用 siteSum 汇总生成
      if (is.null(ts) || nrow(ts) == 0) {
        ss <- siteSum()
        if (!is.null(ss) && nrow(ss) > 0) {
          ts <- ss %>%
            group_by(WeekNo) %>%
            summarise(cum = sum(cum), .groups = "drop")
        }
      }
      
      if (is.null(ts) || nrow(ts) == 0) {
        return(plot_ly() %>% layout(title = "Recruitment Over Time – All sites",
                                    xaxis = list(title = "Week Number"),
                                    yaxis = list(title = "Cumulative Recruitment")))
      }
      
      return(
        plot_ly(ts, x = ~WeekNo, y = ~cum, type = "scatter",
                mode = "lines", fill = "tozeroy") %>%
          layout(title = "Recruitment Over Time – All sites",
                 xaxis = list(title = "Week Number"),
                 yaxis = list(title = "Cumulative Recruitment"))
      )
    }
    
    
    # ---- 单站：沿用原逻辑 ----
    siteName <- input$siteSelect
    data <- siteSum() %>% filter(Location == siteName)
    updateNum()                                          # 同步右侧输入框
    plot_ly(data, x = ~WeekNo, y = ~cum, color = ~Location,
            type = "scatter", mode = "lines", fill = 'tozeroy') %>%
      layout(title = paste("Recruitment Over Time –", siteName),
             xaxis = list(title = "Week Number"),
             yaxis = list(title = "Cumulative Recruitment"))
  })
  
  # Helper reactives for site-specific values (using updated baseData) ---
  # 选中站点的目标样本数
  siteBase <- reactive({
    if (input$siteSelect == "All") {
      if (!is.null(vals$baseData))
        return(sum(vals$baseData$RecruitGoal, na.rm = TRUE))
      return(0)
    }
    
    site <- input$siteSelect
    row  <- vals$baseData[vals$baseData$Location == site, ]
    if (nrow(row) == 0) return(0)
    row$RecruitGoal[1]
  })
  
  # 选中站点的已招人数
  siteRecr <- reactive({
    if (input$siteSelect == "All") {
      # 优先用 siteOverview 汇总，若为空再用 allData 兜底
      so <- siteOverview()
      if (!is.null(so) && nrow(so) > 0)
        return(sum(so$Total, na.rm = TRUE))
      return(nrow(allData()))
    }
    
    site <- input$siteSelect
    data <- siteOverview()
    if (is.null(data)) return(0)
    row <- data %>% filter(Location == site)
    if (nrow(row) == 0) return(0)
    row$Total[1]
  })
  
  
  # --- Grouped Bar Charts for Visit Completion and Timing ---
  default_interval_days <- 30
  default_win_days <- 7
  
  # Visit Completion Rate
  output$visitCompletionPlot <- renderPlotly({
    df <- vals$sv_longr
    req(df, input$vc_site)
    # Filter data for selected site
    df_site <- df %>% filter(site == input$vc_site)
    if (nrow(df_site) == 0) {
      return(plot_ly() %>% layout(title = paste("No data for site", input$vc_site)))
    }
    # Determine visit completion status for each record/visit
    df_site <- df_site %>% mutate(
      status = case_when(
        visit_comp == 1                         ~ "completed",
        visitnotdone == 1                       ~ "early discontinuation",
        visitnotdone == 2                       ~ "Other reason",
        is.na(visit_comp) & is.na(visitnotdone) ~ "No data",
        TRUE                                    ~ "unknown"
      )
    )
    # Calculate percentage of each status per visit
    plot_data <- df_site %>% count(visit, status) %>% group_by(visit) %>% mutate(percentage = n / sum(n) * 100)
    # Create bar chart
    p <- ggplot(plot_data, aes(x = visit, y = percentage, fill = status,
                               text = paste("Visit", visit, "-", status, ":", round(percentage, 1), "%"))) +
      geom_bar(stat = "identity", position = position_dodge2()) +
      scale_fill_manual(values = c(
        "completed" = "#2ca02c",
        "early discontinuation" = "#d62728",
        "Other reason" = "#ff7f0e",
        "No data" = "#7f7f7f",
        "unknown" = "#1f77b4"
      )) +
      labs(x = "Visit", y = "Percentage (%)", fill = "Status", title = paste("Visit Completion Rate -", input$vc_site)) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # --- Visit Timing Status (new logic) ------------------------------
  output$visitTimingPlot <- renderPlotly({
    df <- vals$sv_longr;  req(df)
    win_days <- input$winDays
    
    # 2-col schedule file – one row per visit
    sched_tbl <- vals$scheduleData
    if (is.null(sched_tbl)) {
      # fallback default: -14 / 0 / 14 / 42
      sched_tbl <- data.frame(
        visit = c("T0","T1","T2","T3"),
        scheduledtime_indays = c(-14,0,14,42)
      )
    }
    
    # ---- baseline date (T0) per participant ------------------------
    df <- df %>%
      group_by(record_id) %>%
      mutate(T0_date = first(dov[visit == "T0"])) %>%   # baseline
      ungroup()
    
    # add scheduled offset
    df <- df %>%
      left_join(sched_tbl, by = "visit") %>%                 # adds offset
      mutate(
        scheduled_date = ifelse(visit == "T0", NA,           # T0 excluded
                                T0_date + scheduledtime_indays),
        dov_date       = as.Date(dov),
        visit_time_status = case_when(
          visit == "T0"                          ~ NA_character_,
          is.na(dov_date) & scheduled_date > Sys.Date()  ~ "not due",
          is.na(dov_date) & visitnotdone == 1            ~ "Early discontinuation",
          is.na(dov_date) & visitnotdone == 2            ~ "Other reason",
          is.na(dov_date)                                ~ "No data",
          dov_date < scheduled_date - win_days           ~ "early visit",
          dov_date > scheduled_date + win_days           ~ "delay visit",
          TRUE                                           ~ "ontime visit"
        )
      )
    
    # drop T0 rows (status is NA)
    plot_data <- df %>%
      filter(!is.na(visit_time_status)) %>%
      count(visit, visit_time_status) %>%
      group_by(visit) %>%
      mutate(percentage = n / sum(n) * 100)
    
    p <- ggplot(plot_data,
                aes(x = visit, y = percentage,
                    fill = factor(visit_time_status,
                                  levels = c("not due","Early discontinuation",
                                             "Other reason","No data",
                                             "early visit","delay visit",
                                             "ontime visit")),
                    text = paste0("Visit ", visit, " – ",
                                  visit_time_status, ": ",
                                  sprintf("%.1f%%", percentage)))) +
      geom_bar(stat = "identity",
               position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c(
        "not due"      = "#aec7e8",
        "Early discontinuation" = "#d62728",
        "Other reason" = "#ff7f0e",
        "No data"      = "#7f7f7f",
        "early visit"  = "#9467bd",
        "delay visit"  = "#8c564b",
        "ontime visit" = "#2ca02c"
      )) +
      labs(x = "Visit", y = "Percentage (%)",
           fill = "Timing Status",
           title = "Visit Timing Status") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
