# Copy of app.R

# app.R
library(shiny)
library(DT)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(shinyjs)
library(shinyFeedback)
library(purrr)
library(ggiraph)
library(plotly)

# -------------------------------
# Helpers
# -------------------------------

# Validate each sheet has required columns
validate_file <- function(benchmarks_list) {
  for (i in seq_along(benchmarks_list)) {
    df <- benchmarks_list[[i]]
    if (!all(c("Questions", "Per_Question_Average") %in% colnames(df))) {
      stop(paste("Sheet", names(benchmarks_list)[i], "is missing required columns."))
    }
  }
  TRUE
}

# Clean a single sheet to a standard shape
clean_each_sheet <- function(df) {
  df <- as.data.frame(df)
  if (nrow(df) >= 1) df <- df[-1, , drop = FALSE]          # drop first "% correct" row
  cols <- colnames(df)
  if (length(cols) >= 1) cols[1] <- "Questions"
  if (length(cols) >= 2) cols[length(cols) - 1] <- "Per_Question_Average"
  colnames(df) <- cols
  if (ncol(df) >= 2) df <- df[, -ncol(df), drop = FALSE]   # drop last "Skills" column
  df
}

# -------------------------------
# UI
# -------------------------------
ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
  titlePanel("IS 259 Science Benchmark Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Current year data
      fileInput("file", "Upload Current Year Excel (.xlsx) or CSV",
                accept = c(".xlsx", ".csv")),
      
      # Previous year (optional, for YoY tab)
      h4("Previous Year Data (Optional)"),
      fileInput("file_prev", "Upload Previous Year File",
                accept = c(".xlsx", ".csv")),
      
      actionButton("analyze", "Analyze Data", class = "btn-primary"),
      tags$hr(),
      downloadButton("download_cleaned", "Download Cleaned Data")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Cleaned Data",
                 DTOutput("cleanedTable")),
        
        tabPanel("Performance Trends (by Grade)",
                 uiOutput("gradeSelectorPerformance"),
                 girafeOutput("linePlot", width = "100%", height = "520px")),
        
        tabPanel("Class Trends",
                 uiOutput("classSelector_in_tab"),
                 girafeOutput("classPlot", width = "100%", height = "520px")),
        
        tabPanel("Bar Plot: Growth/Decay",
                 uiOutput("gradeSelectorGrowth"),
                 girafeOutput("barPlot", width = "100%", height = "520px")),
        
        tabPanel(
          "Suggestions", 
          tags$br(),
          tags$div(
            HTML("
              <b>How to read this table</b><br/>
              • We compare <b>Benchmark 1</b> vs <b>Benchmark 3</b> at the <b>grade</b> level (averaged across classes).<br/>
              • A small statistical check (<b>Welch’s t-test across classes</b>) flags whether the change looks real.<br/>
              • The <i>Suggestion</i> column adds a tag:<br/>
              &nbsp;&nbsp;&nbsp;&nbsp;— <b>Confirmed</b>: change is unlikely due to chance (p &lt; 0.05).<br/>
              &nbsp;&nbsp;&nbsp;&nbsp;— <b>Inconclusive</b>: change may be noise (few classes or higher p-value).
            ")
          ),
          DTOutput("suggestionsTable"),
          tags$br(),
          downloadButton("download_suggestions", "Download Suggestions (CSV)")
        ),
        
        tabPanel("Performance Table by Class",
                 uiOutput("classPerformanceSelector_in_tab"),
                 DTOutput("performanceByClassTable")),
        
        tabPanel(
          "Class Deviation",
          uiOutput("classDeviationSelector_in_tab"),
          tags$div(style = "margin-bottom:8px;",
                   HTML("
              <b>What is Class Deviation?</b><br/>
              For each question on Benchmark 3, we compare this class’s average to the
              <i>grade mean</i> (the average across <u>all classes in the same grade</u>).<br/>
              <span style='color:#2e7d32; font-weight:600;'>Green bars</span> (above zero) = class scored <i>above</i> the grade mean.<br/>
              <span style='color:#c62828; font-weight:600;'>Red bars</span> (below zero) = class scored <i>below</i> the grade mean.
            ")
          ),
          girafeOutput("classDeviationPlot", width = "100%", height = "440px"),
          tags$h4("Highlights"),
          DTOutput("classDeviationTopTable")
        ),
        
        tabPanel(
          "Year-over-Year Comparison",
          uiOutput("comparisonUI"),
          h4("Predicted At-Risk Questions"),
          DTOutput("predictedRiskTable"),
          h4("Benchmark 1 Comparison: Current vs Previous Year"),
          plotlyOutput("comparisonPlot", height = "520px")
        )
      )
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  
  # === Reactive: current year wide sheets ===
  dataInput <- reactive({
    req(input$file)
    tryCatch({
      if (grepl("\\.xlsx$", input$file$name, ignore.case = TRUE)) {
        sheets <- excel_sheets(input$file$datapath)
        lst <- lapply(sheets, function(s) read_excel(input$file$datapath, sheet = s))
        names(lst) <- sheets
        lst <- lapply(lst, clean_each_sheet)
        validate_file(lst)
        lst
      } else {
        df <- read.csv(input$file$datapath, check.names = FALSE)
        list(Sheet1 = clean_each_sheet(df))
      }
    }, error = function(e) {
      showFeedbackDanger("file", paste("Upload error:", e$message))
      stop(e)
    })
  })
  
  # === Reactive: previous year wide sheets (optional) ===
  dataInputPrev <- reactive({
    req(input$file_prev)
    tryCatch({
      if (grepl("\\.xlsx$", input$file_prev$name, ignore.case = TRUE)) {
        sheets <- excel_sheets(input$file_prev$datapath)
        lst <- lapply(sheets, function(s) read_excel(input$file_prev$datapath, sheet = s))
        names(lst) <- sheets
        lst <- lapply(lst, clean_each_sheet)
        validate_file(lst)
        lst
      } else {
        df <- read.csv(input$file_prev$datapath, check.names = FALSE)
        list(Sheet1 = clean_each_sheet(df))
      }
    }, error = function(e) {
      showFeedbackDanger("file_prev", paste("Upload error (previous year):", e$message))
      NULL
    })
  })
  
  # === Reactive: current year combined long-format data ===
  combinedData <- eventReactive(input$analyze, {
    req(dataInput())
    tryCatch({
      all_sheets <- dataInput()
      combined <- bind_rows(
        lapply(names(all_sheets), function(nm) {
          w <- all_sheets[[nm]]
          keep <- c("Questions", "Per_Question_Average")
          cls_cols <- setdiff(colnames(w), keep)
          pivot_longer(
            w,
            cols = all_of(cls_cols),
            names_to = "Class",
            values_to = "Score"
          ) %>%
            mutate(Benchmark = nm)
        })
      )
      
      combined <- combined %>%
        mutate(
          Score = suppressWarnings(as.numeric(Score)),
          Per_Question_Average = suppressWarnings(as.numeric(Per_Question_Average)),
          Questions = suppressWarnings(as.integer(Questions)),
          Class = suppressWarnings(as.integer(as.numeric(Class)))
        ) %>%
        filter(!is.na(Score), !is.na(Per_Question_Average), !is.na(Questions), !is.na(Class))
      
      combined$Benchmark[combined$Benchmark == " 6th Grade Benchmark 3"] <- "6th Grade Benchmark 3"
      
      combined <- combined %>%
        mutate(
          Grade = suppressWarnings(as.numeric(str_extract(Benchmark, "^[0-9]"))),
          Benchmark_Number = suppressWarnings(as.numeric(str_extract(Benchmark, "[0-9]$"))),
          Class = as.factor(Class)
        ) %>%
        filter(!is.na(Grade), !is.na(Benchmark_Number))
      
      combined
    }, error = function(e) {
      showFeedbackDanger("file", paste("Processing error:", e$message))
      stop(e)
    })
  })
  
  # === Reactive: previous year combined long-format data (optional) ===
  combinedPrevData <- eventReactive(input$analyze, {
    if (is.null(input$file_prev)) return(NULL)
    req(dataInputPrev())
    tryCatch({
      all_sheets <- dataInputPrev()
      combined <- bind_rows(
        lapply(names(all_sheets), function(nm) {
          w <- all_sheets[[nm]]
          keep <- c("Questions", "Per_Question_Average")
          cls_cols <- setdiff(colnames(w), keep)
          pivot_longer(
            w,
            cols = all_of(cls_cols),
            names_to = "Class",
            values_to = "Score"
          ) %>%
            mutate(Benchmark = nm)
        })
      )
      
      combined <- combined %>%
        mutate(
          Score = suppressWarnings(as.numeric(Score)),
          Per_Question_Average = suppressWarnings(as.numeric(Per_Question_Average)),
          Questions = suppressWarnings(as.integer(Questions)),
          Class = suppressWarnings(as.integer(as.numeric(Class)))
        ) %>%
        filter(!is.na(Score), !is.na(Per_Question_Average), !is.na(Questions), !is.na(Class))
      
      combined$Benchmark[combined$Benchmark == " 6th Grade Benchmark 3"] <- "6th Grade Benchmark 3"
      
      combined <- combined %>%
        mutate(
          Grade = suppressWarnings(as.numeric(str_extract(Benchmark, "^[0-9]"))),
          Benchmark_Number = suppressWarnings(as.numeric(str_extract(Benchmark, "[0-9]$"))),
          Class = as.factor(Class)
        ) %>%
        filter(!is.na(Grade), !is.na(Benchmark_Number))
      
      combined
    }, error = function(e) {
      showFeedbackDanger("file_prev", paste("Processing error (previous year):", e$message))
      NULL
    })
  })
  
  # === Reactive: growth by grade (for plots) ===
  growthData <- reactive({
    req(combinedData())
    combined <- combinedData()
    combined %>%
      group_by(Questions, Grade) %>%
      summarize(
        Benchmark_1 = mean(Score[Benchmark_Number == 1], na.rm = TRUE),
        Benchmark_2 = mean(Score[Benchmark_Number == 2], na.rm = TRUE),
        Benchmark_3 = mean(Score[Benchmark_Number == 3], na.rm = TRUE),
        Growth_1_to_2 = Benchmark_2 - Benchmark_1,
        Growth_2_to_3 = Benchmark_3 - Benchmark_2,
        .groups = "drop"
      ) %>%
      arrange(Grade, Questions)
  })
  
  # === Reactive: growth by class (for per-class tabs) ===
  classTrendsData <- reactive({
    req(combinedData())
    combined <- combinedData()
    combined %>%
      group_by(Questions, Class) %>%
      summarize(
        Benchmark_1 = mean(Score[Benchmark_Number == 1], na.rm = TRUE),
        Benchmark_2 = mean(Score[Benchmark_Number == 2], na.rm = TRUE),
        Benchmark_3 = mean(Score[Benchmark_Number == 3], na.rm = TRUE),
        Growth_1_to_2 = Benchmark_2 - Benchmark_1,
        Growth_2_to_3 = Benchmark_3 - Benchmark_2,
        .groups = "drop"
      ) %>%
      arrange(Class, Questions)
  })
  
  # =========================
  # Tab: Cleaned Data
  # =========================
  output$cleanedTable <- renderDT({
    req(combinedData())
    datatable(combinedData(), options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$download_cleaned <- downloadHandler(
    filename = function() "cleaned_long_format.csv",
    content = function(file) {
      req(combinedData())
      write.csv(combinedData(), file, row.names = FALSE)
    }
  )
  
  # =========================
  # Grade selectors (no facets)
  # =========================
  output$gradeSelectorPerformance <- renderUI({
    req(growthData())
    grades <- sort(unique(growthData()$Grade))
    selectInput("selectedGradePerformance", "Select Grade:",
                choices = grades, selected = grades[1])
  })
  
  output$gradeSelectorGrowth <- renderUI({
    req(growthData())
    grades <- sort(unique(growthData()$Grade))
    selectInput("selectedGradeGrowth", "Select Grade:",
                choices = grades, selected = grades[1])
  })
  
  # =========================
  # Tab: Performance Trends (by Grade) — no facets
  # =========================
  output$linePlot <- renderGirafe({
    req(growthData(), input$selectedGradePerformance)
    df <- growthData() %>%
      filter(Grade == input$selectedGradePerformance)
    if (nrow(df) == 0) return(NULL)
    
    dfm <- df %>%
      pivot_longer(
        cols = c(Benchmark_1, Benchmark_2, Benchmark_3),
        names_to = "Benchmark",
        values_to = "Score"
      ) %>%
      mutate(
        tooltip = paste0(
          "Grade: ", Grade,
          "\nQuestion: ", Questions,
          "\n", Benchmark, ": ", sprintf("%.1f", Score)
        ),
        gid = paste(Grade, Benchmark, Questions, sep = "_")
      )
    
    p <- ggplot(dfm, aes(x = Questions, y = Score, color = Benchmark, group = Benchmark)) +
      geom_line_interactive(aes(tooltip = tooltip, data_id = gid), na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = gid), na.rm = TRUE) +
      labs(
        title = paste("Student Performance Trends - Grade", input$selectedGradePerformance),
        x = "Questions",
        y = "Score",
        color = "Benchmark"
      ) +
      theme_minimal()
    
    girafe(
      ggobj = p,
      options = list(
        opts_sizing(rescale = TRUE),
        opts_hover(css = "stroke-width:3px;"),
        opts_toolbar(saveaspng = TRUE, position = "topright")
      )
    )
  })
  
  # =========================
  # Tab: Class Trends — INTERACTIVE
  # =========================
  output$classSelector_in_tab <- renderUI({
    req(classTrendsData())
    classes <- unique(classTrendsData()$Class)
    selectInput("selectedClass", "Select Class:", choices = classes)
  })
  
  output$classPlot <- renderGirafe({
    req(classTrendsData(), input$selectedClass)
    sub <- classTrendsData() %>% filter(Class == input$selectedClass)
    if (nrow(sub) == 0) return(NULL)
    
    dfm <- sub %>%
      pivot_longer(
        cols = c(Benchmark_1, Benchmark_2, Benchmark_3),
        names_to = "Benchmark",
        values_to = "Score"
      ) %>%
      mutate(
        tooltip = paste0(
          "Class: ", input$selectedClass,
          "\nQuestion: ", Questions,
          "\n", Benchmark, ": ", sprintf("%.1f", Score)
        ),
        gid = paste(input$selectedClass, Benchmark, Questions, sep = "_")
      )
    
    p <- ggplot(dfm, aes(x = Questions, y = Score, color = Benchmark, group = Benchmark)) +
      geom_line_interactive(aes(tooltip = tooltip, data_id = gid),
                            linewidth = 1.1, na.rm = TRUE) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = gid),
                             size = 2.5, na.rm = TRUE) +
      scale_color_manual(values = c(
        "Benchmark_1" = "#1f77b4",
        "Benchmark_2" = "#2ca02c",
        "Benchmark_3" = "#d62728"
      )) +
      labs(
        title = paste("Class", input$selectedClass, ": Benchmark Trends"),
        x = "Questions",
        y = "Score",
        color = "Benchmark"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    girafe(
      ggobj = p,
      options = list(
        opts_sizing(rescale = TRUE),
        opts_hover(css = "stroke-width:3px;"),
        opts_toolbar(saveaspng = TRUE, position = "topright")
      )
    )
  })
  
  # =========================
  # Tab: Bar Plot Growth/Decay — no facets
  # =========================
  output$barPlot <- renderGirafe({
    req(growthData(), input$selectedGradeGrowth)
    df <- growthData() %>%
      filter(Grade == input$selectedGradeGrowth)
    if (nrow(df) == 0) return(NULL)
    
    dfm <- df %>%
      select(Questions, Grade, Growth_1_to_2, Growth_2_to_3) %>%
      pivot_longer(
        cols = c(Growth_1_to_2, Growth_2_to_3),
        names_to = "Window",
        values_to = "Growth"
      ) %>%
      mutate(
        tooltip = paste0(
          "Grade: ", Grade,
          "\nQuestion: ", Questions,
          "\nWindow: ", Window,
          "\nGrowth: ", sprintf("%+.1f", Growth)
        ),
        gid = paste(Grade, Questions, Window, sep = "_")
      )
    
    p <- ggplot(dfm, aes(x = Questions, y = Growth, fill = Window)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = gid),
                           position = "dodge", na.rm = TRUE) +
      labs(
        title = paste("Growth Between Benchmarks - Grade", input$selectedGradeGrowth),
        x = "Questions",
        y = "Growth",
        fill = "Window"
      ) +
      theme_minimal()
    
    girafe(
      ggobj = p,
      options = list(
        opts_sizing(rescale = TRUE),
        opts_hover_inv(css = "opacity:0.3;"),
        opts_toolbar(saveaspng = TRUE, position = "topright")
      )
    )
  })
  
  # =========================
  # Tab: Suggestions (Welch t-test + tags)
  # =========================
  suggestionsData <- reactive({
    req(combinedData())
    df <- combinedData()
    
    sug <- df %>%
      group_by(Grade, Questions) %>%
      summarize(
        b1_vec = list(Score[Benchmark_Number == 1]),
        b3_vec = list(Score[Benchmark_Number == 3]),
        Benchmark_1 = mean(Score[Benchmark_Number == 1], na.rm = TRUE),
        Benchmark_3 = mean(Score[Benchmark_Number == 3], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rowwise() %>%
      mutate(
        n1 = sum(!is.na(unlist(b1_vec))),
        n3 = sum(!is.na(unlist(b3_vec))),
        p_1_to_3 = ifelse(
          n1 >= 2 & n3 >= 2,
          tryCatch(
            t.test(unlist(b1_vec), unlist(b3_vec), var.equal = FALSE)$p.value,
            error = function(e) NA_real_
          ),
          NA_real_
        )
      ) %>%
      ungroup() %>%
      mutate(
        diff_1_to_3 = Benchmark_3 - Benchmark_1,
        base_text = dplyr::case_when(
          Benchmark_3 < 40 ~ "Most students in class are struggling with this scientific concept.",
          Benchmark_3 >= 40 & Benchmark_3 < 65 ~ "Students' grade average for this question is less than 65.",
          diff_1_to_3 >= 10 ~ "Students show growth on this question.",
          TRUE ~ "No significant change."
        ),
        tag = ifelse(!is.na(p_1_to_3) & p_1_to_3 < 0.05, " — Confirmed", " — Inconclusive"),
        Suggestion = paste0(base_text, tag)
      ) %>%
      select(Grade, Questions, Benchmark_1, Benchmark_3, diff_1_to_3, p_1_to_3, Suggestion)
    
    sug[order(sug$Grade, sug$Questions), ]
  })
  
  output$suggestionsTable <- renderDT({
    req(suggestionsData())
    datatable(suggestionsData(), options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle(
        "Suggestion",
        backgroundColor = styleEqual(
          c("Most students in class are struggling with this scientific concept. — Confirmed",
            "Most students in class are struggling with this scientific concept. — Inconclusive",
            "Students' grade average for this question is less than 65. — Confirmed",
            "Students' grade average for this question is less than 65. — Inconclusive",
            "Students show growth on this question. — Confirmed",
            "Students show growth on this question. — Inconclusive",
            "No significant change. — Confirmed",
            "No significant change. — Inconclusive"),
          c("#ffe6e6", "#ffe6e6",
            "#ffecec", "#ffecec",
            "#e9f9e9", "#e9f9e9",
            "#fff7cc", "#fff7cc")
        )
      )
  })
  
  output$download_suggestions <- downloadHandler(
    filename = function() "suggestions_grade_level.csv",
    content = function(file) {
      req(suggestionsData())
      write.csv(suggestionsData(), file, row.names = FALSE)
    }
  )
  
  # =========================
  # Tab: Performance Table by Class
  # =========================
  output$classPerformanceSelector_in_tab <- renderUI({
    req(classTrendsData())
    classes <- unique(classTrendsData()$Class)
    selectInput("selectedPerformanceClass", "Select Class for Table:", choices = classes)
  })
  
  output$performanceByClassTable <- renderDT({
    req(classTrendsData(), input$selectedPerformanceClass)
    sub <- classTrendsData() %>% filter(Class == input$selectedPerformanceClass)
    if (nrow(sub) == 0) return(NULL)
    sub <- sub %>%
      mutate(
        Suggestion = case_when(
          Benchmark_3 < 40 ~ "Students are struggling, they need more practice on this question.",
          (Benchmark_3 - Benchmark_1) >= 10 ~ "Students show growth on this question.",
          TRUE ~ "No significant change."
        )
      ) %>%
      select(Class, Questions, Benchmark_1, Benchmark_2, Benchmark_3, Suggestion)
    
    datatable(sub, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle(
        "Suggestion",
        backgroundColor = styleEqual(
          c("Students are struggling, they need more practice on this question.",
            "Students show growth on this question.",
            "No significant change."),
          c("#ffe6e6", "#e9f9e9", "#fff7cc")
        )
      )
  })
  
  # =========================
  # Tab: Class Deviation
  # =========================
  output$classDeviationSelector_in_tab <- renderUI({
    req(classTrendsData())
    classes <- sort(unique(classTrendsData()$Class))
    selectInput("selectedDeviationClass", "Select Class:", choices = classes)
  })
  
  classDeviationData <- reactive({
    req(combinedData(), input$selectedDeviationClass)
    
    df <- combinedData() %>% filter(Benchmark_Number == 3)
    
    grade_means <- df %>%
      group_by(Grade, Questions) %>%
      summarize(GradeMean_BM3 = mean(Score, na.rm = TRUE), .groups = "drop")
    
    class_means <- df %>%
      group_by(Grade, Class, Questions) %>%
      summarize(Class_BM3 = mean(Score, na.rm = TRUE), .groups = "drop") %>%
      filter(Class == as.numeric(as.character(input$selectedDeviationClass)))
    
    class_means %>%
      inner_join(grade_means, by = c("Grade", "Questions")) %>%
      mutate(
        ClassDeviation = Class_BM3 - GradeMean_BM3,
        DevFlag = ifelse(ClassDeviation >= 0, "Above grade mean", "Below grade mean")
      )
  })
  
  output$classDeviationPlot <- renderGirafe({
    req(classDeviationData())
    dd <- classDeviationData()
    if (nrow(dd) == 0) return(NULL)
    
    dd <- dd %>%
      mutate(
        tooltip = paste0(
          "Question: ", Questions,
          "\nClass BM3: ", sprintf("%.1f", Class_BM3),
          "\nGrade Mean BM3: ", sprintf("%.1f", GradeMean_BM3),
          "\nDeviation: ", sprintf("%+.1f", ClassDeviation)
        ),
        gid = paste(Questions, round(ClassDeviation, 2), sep = "_")
      )
    
    p <- ggplot(dd, aes(x = Questions, y = ClassDeviation, fill = DevFlag)) +
      geom_col_interactive(aes(tooltip = tooltip, data_id = gid)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      scale_fill_manual(values = c("Above grade mean" = "#2e7d32",
                                   "Below grade mean" = "#c62828")) +
      labs(
        title = paste("Class", input$selectedDeviationClass,
                      "Deviation from Grade Mean (BM3)"),
        x = "Questions",
        y = "ClassDeviation = BM3(class) \u2212 GradeMean_BM3",
        fill = ""
      ) +
      theme_minimal() +
      theme(legend.position = "top")
    
    girafe(
      ggobj = p,
      options = list(
        opts_sizing(rescale = TRUE),
        opts_hover(css = "opacity:1; filter:brightness(1.1);"),
        opts_hover_inv(css = "opacity:0.3;"),
        opts_toolbar(saveaspng = TRUE, position = "topright")
      )
    )
  })
  
  output$classDeviationTopTable <- renderDT({
    req(classDeviationData())
    dd <- classDeviationData()
    
    top_pos <- dd %>% arrange(desc(ClassDeviation)) %>% slice_head(n = 3) %>%
      mutate(Category = "Top above")
    top_neg <- dd %>% arrange(ClassDeviation) %>% slice_head(n = 3) %>%
      mutate(Category = "Top below")
    
    tbl <- bind_rows(top_pos, top_neg) %>%
      select(Category, Questions, Class_BM3, GradeMean_BM3, ClassDeviation) %>%
      arrange(match(Category, c("Top above", "Top below")),
              desc(ClassDeviation))
    
    datatable(
      tbl,
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align:left; font-weight:bold;",
        "Top 3 questions above and below the grade mean (BM3)"
      )
    )
  })
  
  # =========================
  # Year-over-Year Comparison
  # =========================
  predictedRiskData <- reactive({
    if (is.null(combinedPrevData())) return(NULL)
    req(combinedPrevData(), combinedData())
    
    tryCatch({
      prev_patterns <- combinedPrevData() %>%
        group_by(Questions, Grade) %>%
        summarize(
          Prev_B1 = mean(Score[Benchmark_Number == 1], na.rm = TRUE),
          Prev_B2 = mean(Score[Benchmark_Number == 2], na.rm = TRUE),
          Prev_B3 = mean(Score[Benchmark_Number == 3], na.rm = TRUE),
          Prev_Growth_1_to_2 = Prev_B2 - Prev_B1,
          Prev_Growth_2_to_3 = Prev_B3 - Prev_B2,
          .groups = "drop"
        )
      
      current_b1 <- combinedData() %>%
        filter(Benchmark_Number == 1) %>%
        group_by(Questions, Grade) %>%
        summarize(Current_B1 = mean(Score, na.rm = TRUE), .groups = "drop")
      
      prev_b1_data <- combinedPrevData() %>%
        filter(Benchmark_Number == 1) %>%
        select(Questions, Grade, Score) %>%
        rename(Prev_Score = Score)
      
      curr_b1_data <- combinedData() %>%
        filter(Benchmark_Number == 1) %>%
        select(Questions, Grade, Score) %>%
        rename(Curr_Score = Score)
      
      t_test_results <- bind_rows(
        prev_b1_data %>% mutate(Year = "Previous"),
        curr_b1_data %>% mutate(Year = "Current")
      ) %>%
        group_by(Questions, Grade) %>%
        summarize(
          p_value = tryCatch({
            prev_scores <- na.omit(Prev_Score)
            curr_scores <- na.omit(Curr_Score)
            
            if (length(prev_scores) > 1 && length(curr_scores) > 1 &&
                sd(prev_scores, na.rm = TRUE) > 0 &&
                sd(curr_scores, na.rm = TRUE) > 0) {
              t.test(curr_scores, prev_scores, var.equal = FALSE)$p.value
            } else {
              NA_real_
            }
          }, error = function(e) NA_real_),
          .groups = "drop"
        ) %>%
        mutate(
          status = ifelse(!is.na(p_value) & p_value < 0.05, "Confirmed", "Inclusive")
        )
      
      predictions <- prev_patterns %>%
        left_join(current_b1, by = c("Questions", "Grade")) %>%
        left_join(t_test_results, by = c("Questions", "Grade")) %>%
        mutate(
          B1_Comparison = Current_B1 - Prev_B1,
          Predicted_B2 = Current_B1 + Prev_Growth_1_to_2,
          Predicted_B3 = Predicted_B2 + Prev_Growth_2_to_3,
          Risk_Level = dplyr::case_when(
            Predicted_B3 < 40 ~ "High Risk",
            Predicted_B3 < 55 ~ "Medium Risk",
            Prev_B3 < 50 & Prev_Growth_2_to_3 < 0 ~ "Medium Risk",
            TRUE ~ "Low Risk"
          ),
          Recommendation = dplyr::case_when(
            Risk_Level == "High Risk" ~ "Priority intervention needed - students likely to struggle",
            Risk_Level == "Medium Risk" ~ "Monitor closely and provide additional support",
            TRUE ~ "Continue current instruction approach"
          )
        ) %>%
        arrange(
          Grade,
          desc(Risk_Level == "High Risk"),
          desc(Risk_Level == "Medium Risk")
        )
      
      predictions
    }, error = function(e) {
      shinyjs::alert(paste("Error in predictive analysis:", e$message))
      NULL
    })
  })
  
  output$comparisonUI <- renderUI({
    if (is.null(combinedPrevData())) {
      return(div(
        style = "padding: 12px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;",
        h4("No Previous Year Data"),
        p("Upload previous year data (optional) to enable year-over-year comparison and predictive analysis.")
      ))
    } else {
      grades <- sort(unique(combinedData()$Grade))
      selectInput("selectedGradeComparison", "Select Grade:",
                  choices = grades, selected = grades[1])
    }
  })
  
  output$predictedRiskTable <- renderDT({
    if (is.null(predictedRiskData())) {
      return(datatable(data.frame(Message = "No previous year data available")))
    }
    req(predictedRiskData())
    
    risk_data <- predictedRiskData() %>%
      select(
        Grade, Questions, Current_B1, Prev_B1, B1_Comparison,
        Predicted_B2, Predicted_B3, p_value, status, Risk_Level, Recommendation
      ) %>%
      mutate(across(where(is.numeric), ~round(., 1)))
    
    datatable(risk_data, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound(columns = "p_value", digits = 4) %>%
      formatStyle(
        "Risk_Level",
        backgroundColor = styleEqual(
          c("High Risk", "Medium Risk", "Low Risk"),
          c("#ffcccc", "#fff4cc", "#ccffcc")
        ),
        fontWeight = styleEqual("High Risk", "bold")
      ) %>%
      formatStyle(
        "B1_Comparison",
        color = styleInterval(c(-5, 5), c("red", "black", "green"))
      ) %>%
      formatStyle(
        "status",
        backgroundColor = styleEqual(
          c("Confirmed", "Inclusive"),
          c("#d4edda", "#fff3cd")
        ),
        fontWeight = styleEqual("Confirmed", "bold")
      )
  })
  
  output$comparisonPlot <- renderPlotly({
    if (is.null(combinedPrevData())) return(NULL)
    req(combinedPrevData(), combinedData(), input$selectedGradeComparison)
    
    prev_b1 <- combinedPrevData() %>%
      filter(Benchmark_Number == 1, Grade == input$selectedGradeComparison) %>%
      group_by(Questions) %>%
      summarize(Previous_Year_B1 = mean(Score, na.rm = TRUE), .groups = "drop")
    
    curr_b1 <- combinedData() %>%
      filter(Benchmark_Number == 1, Grade == input$selectedGradeComparison) %>%
      group_by(Questions) %>%
      summarize(Current_Year_B1 = mean(Score, na.rm = TRUE), .groups = "drop")
    
    comparison_data <- prev_b1 %>%
      left_join(curr_b1, by = "Questions") %>%
      pivot_longer(
        cols = c(Previous_Year_B1, Current_Year_B1),
        names_to = "Year",
        values_to = "Score"
      ) %>%
      mutate(
        Year = recode(
          Year,
          "Previous_Year_B1" = "Previous Year",
          "Current_Year_B1" = "Current Year"
        )
      )
    
    p <- ggplot(comparison_data,
                aes(x = Questions, y = Score, color = Year, group = Year)) +
      geom_line(size = 1) +
      geom_point(size = 2.5) +
      labs(
        title = paste("Benchmark 1 Comparison: Current vs Previous Year - Grade",
                      input$selectedGradeComparison),
        x = "Question Number",
        y = "Average Score (%)",
        color = "Year"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(hovermode = "x unified")
  })
}

# -------------------------------
# Run the app
# -------------------------------
shinyApp(ui = ui, server = server)
