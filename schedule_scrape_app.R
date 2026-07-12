###############################################################
# ECU Engineering Degree Checker – Shiny App
#
# How to run locally:
#   1. Place this file (app.R) and student_schedule.xlsx in the same folder
#   2. Open in RStudio and click "Run App"  OR  run: shiny::runApp(".")
#
# Required packages (run once if not already installed):
#   install.packages(c("shiny","rvest","dplyr","stringr","readxl","tidyr"))
#Note: Add missing courses tab, fix electives tab so it shows what classes we have listed
###############################################################

library(shiny)
library(rvest) #this is a scraping package
library(dplyr) 
library(stringr)
library(readxl)
library(tidyr)

# ── HELPERS ──────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (length(a) == 0 || all(is.na(a))) b else a

#takes input and Matches 2-4 uppercase letters, a space, then 3-4 numbers. Final optional uppercase letter .
extract_primary_code <- function(x) {
  str_extract(x, "[A-Z]{2,4}\\s\\d{3,4}[A-Z]?")
}

#Takes input, removes whitespace,converts uppercase, pulls the code, and extracts a unique error if N/A
clean_codes <- function(x) {
  x %>% str_trim() %>% str_to_upper() %>%
    extract_primary_code() %>%
    { .[!is.na(.)] } %>% unique()
}

# Parses a prereq string into groups that must each be satisfied.
# Returns a list of requirement groups, each is a list with:
#   $type    – "P" (prior semester) or "PC" (same semester or prior)
#   $options – character vector; requirement is met if ANY option is in available courses
#              (handles "MATH 1083 or MATH 2171" — only one needed)
#
# Example: "P: ENGR 3000   P/C: MATH 1083 or MATH 2171"
#   -> list(
#        list(type="P",  options=c("ENGR 3000")),
#        list(type="PC", options=c("MATH 1083", "MATH 2171"))  # either satisfies
#      )
parse_prereq_codes <- function(x) {
  if (is.na(x) || x == "") return(list())
  code_pat <- "[A-Z]{2,4}\\s\\d{3,4}[A-Z]?"
  
  # Split string on section headers, keeping header with its content
  # Handles: "P: ...", "P/C: ..."
  sections <- str_split(x, "(?i)(?=\\bP/C:|(?<![/])\\bP:)")[[1]]
  sections <- sections[str_trim(sections) != ""]
  
  groups <- list()
  for (sec in sections) {
    sec <- str_trim(sec)
    if (str_detect(sec, "(?i)^P/C:")) {
      type    <- "PC"
      content <- str_remove(sec, "(?i)^P/C:\\s*")
    } else if (str_detect(sec, "(?i)^P:")) {
      type    <- "P"
      content <- str_remove(sec, "(?i)^P:\\s*")
    } else {
      # No header — treat whole string as strict prereq group
      type    <- "P"
      content <- sec
    }
    
    # Split on " or " (case-insensitive) to get OR alternatives
    # Each alternative may contain one course code
    or_parts <- str_split(content, "(?i)\\s+or\\s+")[[1]]
    codes <- unlist(lapply(or_parts, function(p) {
      m <- str_extract(p, code_pat)
      if (!is.na(m)) m else character(0)
    }))
    
    if (length(codes) > 0) {
      groups <- c(groups, list(list(type = type, options = unique(codes))))
    }
  }
  groups
}

# Check a parsed list of requirement groups against available course sets.
# available_before: courses completed or in a prior semester (for P: requirements)
# available_same:   courses completed, prior, or same semester  (for P/C: requirements)
# Returns a character vector of human-readable unmet requirement descriptions.
check_prereq_groups <- function(groups, available_before, available_same) {
  unmet <- character(0)
  for (g in groups) {
    avail <- if (g$type == "PC") available_same else available_before
    # Satisfied if ANY option is available
    if (!any(g$options %in% avail)) {
      label <- if (length(g$options) == 1) {
        paste0(g$options, if (g$type == "PC") " (P/C)" else " (P)")
      } else {
        paste0("(", paste(g$options, collapse = " or "), ")",
               if (g$type == "PC") " (P/C)" else " (P)")
      }
      unmet <- c(unmet, label)
    }
  }
  unmet
}

# count_slot: filters tag_map rows whose tag starts with type_prefix.
# Returns n, credits, and course codes (not raw tag strings).
count_slot <- function(tag_map, type_prefix) {
  matching <- tag_map %>% filter(str_starts(tag, type_prefix))
  list(n       = nrow(matching),
       credits = sum(as.integer(str_extract(matching$tag, "\\d+")), na.rm = TRUE),
       courses = matching$code)
}
#requires internet
fetch_curriculum <- function(conc) {
  url <- paste0("https://cet.ecu.edu/engineering/", conc, "-engineering-curriculum-table/")
  page <- tryCatch(
    read_html(url),
    error = function(e) stop("Could not reach the ECU curriculum page. Check your internet connection.")
  )
  tbl <- page %>% html_element("table") %>% html_table(trim = TRUE)
  names(tbl) <- names(tbl) %>%
    str_to_lower() %>% str_replace_all("[^a-z0-9]+", "_") %>% str_remove("_$")
  tbl <- tbl %>% rename_with(~ case_when(
    . == "course"                            ~ "course_raw",
    str_detect(., "title")                   ~ "title",
    str_detect(., "semester")               ~ "semester",
    str_detect(., "lab")                     ~ "has_lab",
    str_detect(., "category")               ~ "category",
    str_detect(., "grade")                   ~ "min_grade",
    str_detect(., "prior|concurrent|prereq") ~ "prereqs",
    TRUE ~ .
  ))
  tbl <- tbl %>% mutate(primary_code = extract_primary_code(course_raw))
  curriculum <- tbl %>%
    filter(!is.na(primary_code)) %>%
    select(semester, primary_code, title, category, min_grade, prereqs)
  elective_rows <- tbl %>%
    filter(is.na(primary_code)) %>%
    select(semester, course_raw) %>%
    mutate(slot_type = case_when(
      str_detect(str_to_lower(course_raw), "technical")            ~ "tech",
      str_detect(str_to_lower(course_raw), "humanities|fine arts") ~ "hum",
      str_detect(str_to_lower(course_raw), "social")               ~ "soc",
      TRUE ~ "other"
    ))
  req_slots <- elective_rows %>%
    filter(slot_type != "other") %>%
    count(slot_type, name = "slots_needed")
  list(curriculum = curriculum, req_slots = req_slots)
}
#CHANGE 156 if sheet name different
parse_student_file <- function(filepath) {
  raw <- read_excel(filepath, sheet = "Complete_Intended", col_names = TRUE)
  completed_codes <- raw[[1]] %>% clean_codes()
  completed_tags  <- raw[[2]][!is.na(raw[[1]]) & !is.na(raw[[2]])] %>%
    str_trim() %>% str_to_lower() %>% { .[. != ""] }
  intended_codes <- raw[[3]] %>% clean_codes()
  intended_tags  <- raw[[4]][!is.na(raw[[3]]) & !is.na(raw[[4]])] %>%
    str_trim() %>% str_to_lower() %>% { .[. != ""] }
  intended_codes <- setdiff(intended_codes, completed_codes)
  
  # Build tag -> course code lookup maps so we can display course codes in UI
  # Completed: col1 = course code, col2 = tag
  completed_tag_map <- tibble(
    code    = raw[[1]] %>% str_trim() %>% str_to_upper() %>% extract_primary_code(),
    tag     = raw[[2]] %>% str_trim() %>% str_to_lower(),
    credits = as.integer(str_extract(raw[[2]] %>% str_trim(), "\\d+"))
  ) %>% filter(!is.na(code), !is.na(tag), tag != "")
  
  # Intended: col3 = course code, col4 = tag
  intended_tag_map <- tibble(
    code    = raw[[3]] %>% str_trim() %>% str_to_upper() %>% extract_primary_code(),
    tag     = raw[[4]] %>% str_trim() %>% str_to_lower(),
    credits = as.integer(str_extract(raw[[4]] %>% str_trim(), "\\d+"))
  ) %>% filter(!is.na(code), !is.na(tag), tag != "")
  
  #actual plan
  raw_plan <- read_excel(filepath, sheet = "planned_schedule",
                         col_names = FALSE, .name_repair = "minimal")
  sem_headers <- raw_plan[1, ] %>% unlist(use.names = FALSE)
  sem_col_idx <- which(!is.na(sem_headers) & str_detect(as.character(sem_headers), "(?i)semester"))
  
  planned_by_sem <- lapply(sem_col_idx, function(ci) {
    sem_name  <- as.character(raw_plan[1, ci, drop = TRUE])
    sem_num   <- as.integer(str_extract(sem_name, "\\d+"))
    raw_names <- raw_plan[2:(nrow(raw_plan) - 1), ci, drop = TRUE] %>%
      unlist(use.names = FALSE) %>% as.character()
    # Credits are in the column immediately to the right of the course column
    raw_credits <- if ((ci + 1) <= ncol(raw_plan))
      raw_plan[2:(nrow(raw_plan) - 1), ci + 1, drop = TRUE] %>%
      unlist(use.names = FALSE) %>% as.character()
    else rep(NA_character_, length(raw_names))
    codes <- str_trim(str_to_upper(raw_names)) %>% extract_primary_code()
    keep  <- !is.na(codes)
    codes   <- codes[keep]
    credits <- suppressWarnings(as.integer(raw_credits[keep]))
    if (length(codes) == 0) return(NULL)
    tibble(sem_name = sem_name, sem_num = sem_num,
           course_code = codes, credits = credits)
  }) %>% bind_rows()
  list(completed_codes    = completed_codes, completed_tags = completed_tags,
       intended_codes     = intended_codes,  intended_tags  = intended_tags,
       planned_by_sem     = planned_by_sem,
       completed_tag_map  = completed_tag_map,
       intended_tag_map   = intended_tag_map)
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("ECU Engineering Degree Checker"),
  
  tabsetPanel(
    
    # ── Setup ────────────────────────────────────────────────────────────────
    tabPanel("Setup",
             br(),
             fluidRow(
               column(4,
                      wellPanel(
                        h4("Configuration"),
                        selectInput("concentration", "Concentration",
                                    choices = c(
                                      "Biomedical"            = "biomedical",
                                      "Biochemical"           = "biochemical",
                                      "Electrical"            = "electrical",
                                      "Civil & Environmental" = "civil-and-environmental",
                                      "Industrial & Systems"  = "industrial-and-systems",
                                      "Mechanical"            = "mechanical"
                                    ),
                                    selected = "civil-and-environmental"
                        ),
                        hr(),
                        fileInput("schedule_file", "Upload student_schedule.xlsx",
                                  accept = ".xlsx", placeholder = "No file chosen"),
                        tags$small("Needs sheets: 'Script List' and 'planned_schedule'"),
                        hr(),
                        actionButton("run_check", "Run Degree Check",
                                     class = "btn-primary btn-lg", style = "width:100%;")
                      )
               ),
               column(8,
                      wellPanel(
                        h4("How to use"),
                        tags$ol(
                          tags$li("Select your concentration."),
                          tags$li("Upload your ", tags$code("student_schedule.xlsx"), "."),
                          tags$li("Click Run Degree Check."),
                          tags$li("Navigate the tabs to review results.")
                        ),
                        hr(),
                        h5("Excel sheet requirements"),
                        tags$ul(
                          tags$li(tags$code("Script list, Page 1"),
                                  "Before any work, please see the provided example Excel file. Index: | Col 1: completed course codes | Col 2: Completed GEd/Tech tags | Col 3: intended course codes | Col 4: intended GEd/Tech tags |"),
                          tags$li(tags$code("Planned schedule, sheet 2"),
                                  " — Semesters laid out in column pairs (course name, credits)")
                        ),
                        hr(),
                        uiOutput("status_box")
                      )
               )
             )
    ),
    
    # ── Summary ──────────────────────────────────────────────────────────────
    tabPanel("Summary",
             br(),
             uiOutput("summary_ui")
    ),
    
    # ── Courses ──────────────────────────────────────────────────────────────
    tabPanel("Courses",
             br(),
             uiOutput("courses_ui"),
             tableOutput("courses_table")
    ),
    
    # ── Prerequisites ────────────────────────────────────────────────────────
    tabPanel("Prerequisites",
             br(),
             uiOutput("prereq_ui"),
             tableOutput("prereq_table")
    ),
    
    # ── Planned Schedule ─────────────────────────────────────────────────────
    tabPanel("Planned Schedule",
             br(),
             uiOutput("schedule_ui")
    ),
    
    # ── Electives ────────────────────────────────────────────────────────────
    tabPanel("Electives",
             br(),
             uiOutput("electives_ui")
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  results <- eventReactive(input$run_check, {
    req(input$schedule_file)
    
    withProgress(message = "Running degree check...", value = 0, {
      
      incProgress(0.1, detail = "Fetching curriculum from ECU website...")
      curr_data <- tryCatch(
        fetch_curriculum(input$concentration),
        error = function(e) {
          showNotification(paste("Error fetching curriculum:", e$message),
                           type = "error", duration = 10)
          NULL
        }
      )
      req(curr_data)
      curriculum <- curr_data$curriculum
      req_slots  <- curr_data$req_slots
      
      incProgress(0.35, detail = "Parsing student schedule...")
      student <- tryCatch(
        parse_student_file(input$schedule_file$datapath),
        error = function(e) {
          showNotification(paste("Error reading Excel file:", e$message),
                           type = "error", duration = 10)
          NULL
        }
      )
      req(student)
      
      incProgress(0.55, detail = "Matching against degree requirements...")
      
      # ── Course substitution rules ──────────────────────────────────────────
      # Each rule: if the student has ALL courses in `have`, treat `replace` as
      # also satisfied. Rules are concentration-specific.
      substitution_rules <- list(
        list(
          concentrations = c("civil-and-environmental", "electrical", "industrial-and-systems"),
          have    = c("GEOL 1500", "GEOL 1501"),
          replace = c("BIOL 1050", "BIOL 1051")
        )
      )
      
      apply_substitutions <- function(codes, conc, rules) {
        extra <- character(0)
        for (rule in rules) {
          if (conc %in% rule$concentrations && all(rule$have %in% codes)) {
            extra <- union(extra, rule$replace)
          }
        }
        union(codes, extra)
      }
      
      effective_completed <- apply_substitutions(
        student$completed_codes, input$concentration, substitution_rules)
      effective_intended  <- apply_substitutions(
        union(student$completed_codes, student$intended_codes),
        input$concentration, substitution_rules)
      
      slots_done    <- list(tech = count_slot(student$completed_tag_map, "tech"),
                            hum  = count_slot(student$completed_tag_map, "hum"),
                            soc  = count_slot(student$completed_tag_map, "soc"))
      slots_planned <- list(tech = count_slot(student$intended_tag_map, "tech"),
                            hum  = count_slot(student$intended_tag_map, "hum"),
                            soc  = count_slot(student$intended_tag_map, "soc"))
      
      res <- curriculum %>%
        mutate(
          done    = primary_code %in% effective_completed,
          planned = (!done) & (primary_code %in% effective_intended),
          missing = (!done) & (!planned),
          status  = case_when(done ~ "Complete", planned ~ "Planned", TRUE ~ "Missing")
        )
      
      incProgress(0.75, detail = "Checking prerequisites...")
      prereq_lookup <- curriculum %>%
        select(primary_code, prereqs) %>%
        filter(!is.na(prereqs), prereqs != "")
      
      planned_by_sem <- student$planned_by_sem
      
      prereq_issues <- lapply(seq_len(nrow(planned_by_sem)), function(i) {
        course     <- planned_by_sem$course_code[i]
        sem_num    <- planned_by_sem$sem_num[i]
        sem_name   <- planned_by_sem$sem_name[i]
        prereq_str <- prereq_lookup$prereqs[prereq_lookup$primary_code == course]
        if (length(prereq_str) == 0 || is.na(prereq_str)) return(NULL)
        
        parsed <- parse_prereq_codes(prereq_str)
        if (length(parsed) == 0) return(NULL)
        
        # Strict prereqs (P:): must appear in a PRIOR semester or completed
        earlier_planned  <- planned_by_sem %>%
          filter(sem_num < !!sem_num) %>% pull(course_code)
        available_before <- apply_substitutions(
          union(student$completed_codes, earlier_planned),
          input$concentration, substitution_rules)
        
        # P/C co-reqs: same semester OR earlier is fine
        same_or_earlier <- planned_by_sem %>%
          filter(sem_num <= !!sem_num) %>% pull(course_code)
        available_same  <- apply_substitutions(
          union(student$completed_codes, same_or_earlier),
          input$concentration, substitution_rules)
        
        unmet_all <- check_prereq_groups(parsed, available_before, available_same)
        if (length(unmet_all) == 0) return(NULL)
        
        tibble(Semester = sem_name, Course = course,
               `Unmet Requirement` = unmet_all,
               `Full Prereq String` = prereq_str)
      }) %>% bind_rows()
      
      incProgress(1.0, detail = "Done!")
      
      list(
        curriculum      = curriculum,
        req_slots       = req_slots,
        results         = res,
        done_courses    = res %>% filter(done),
        planned_courses = res %>% filter(planned),
        missing_courses = res %>% filter(missing),
        prereq_issues   = prereq_issues,
        planned_by_sem  = planned_by_sem,
        slots_done      = slots_done,
        slots_planned   = slots_planned,
        student         = student,
        completed_tag_map = student$completed_tag_map,
        intended_tag_map  = student$intended_tag_map,
        concentration   = input$concentration,
        timestamp       = format(Sys.time(), "%Y-%m-%d %H:%M")
      )
    })
  })
  
  # ── Status box (Setup tab) ────────────────────────────────────────────────
  output$status_box <- renderUI({
    r <- results()
    if (is.null(r)) return(NULL)
    pct    <- if (nrow(r$results) > 0) round(100 * nrow(r$done_courses) / nrow(r$results)) else 0
    all_ok <- nrow(r$missing_courses) == 0 && nrow(r$prereq_issues) == 0
    div(
      class = if (all_ok) "alert alert-success" else "alert alert-warning",
      strong(if (all_ok) "All requirements met!" else "Action needed"),
      br(),
      sprintf("Progress: %d / %d courses complete (%d%%)", nrow(r$done_courses), nrow(r$results), pct),
      br(),
      sprintf("Prerequisite conflicts: %d", nrow(r$prereq_issues))
    )
  })
  
  # ── Summary tab ───────────────────────────────────────────────────────────
  output$summary_ui <- renderUI({
    r <- results()
    if (is.null(r)) return(p("Run a degree check on the Setup tab first."))
    
    pct      <- if (nrow(r$results) > 0) round(100 * nrow(r$done_courses) / nrow(r$results)) else 0
    tech_ok  <- (r$slots_done$tech$n + r$slots_planned$tech$n) >= (r$req_slots$slots_needed[r$req_slots$slot_type == "tech"] %||% 0L)
    hum_ok   <- (r$slots_done$hum$n  + r$slots_planned$hum$n)  >= (r$req_slots$slots_needed[r$req_slots$slot_type == "hum"]  %||% 0L)
    soc_ok   <- (r$slots_done$soc$n  + r$slots_planned$soc$n)  >= (r$req_slots$slots_needed[r$req_slots$slot_type == "soc"]  %||% 0L)
    all_ok   <- nrow(r$missing_courses) == 0 && tech_ok && hum_ok && soc_ok && nrow(r$prereq_issues) == 0
    
    tagList(
      
      # Overall status banner
      div(class = if (all_ok) "alert alert-success" else "alert alert-warning",
          strong(if (all_ok)
            "All required courses complete or planned, elective slots covered, no prereq conflicts!"
            else
              "Action needed — review sections below."
          ),
          br(),
          tags$small("Always confirm your plan with your academic advisor.")
      ),
      
      # Quick counts
      wellPanel(
        h4(paste0("Degree Check — ",
                  str_to_title(str_replace_all(r$concentration, "-", " ")),
                  " — ", r$timestamp)),
        fluidRow(
          column(3, div(style = "text-align:center; padding:10px; background:#a6d492; border-radius:4px;",
                        h3(sprintf("%d / %d", nrow(r$done_courses), nrow(r$results))), p("Complete"))),
          column(3, div(style = "text-align:center; padding:10px; background:#80b7f2; border-radius:4px;",
                        h3(nrow(r$planned_courses)), p("Planned"))),
          column(3, div(
            style = if (nrow(r$missing_courses) == 0)
              "text-align:center; padding:10px; background:#a6d492; border-radius:4px;"
            else
              "text-align:center; padding:10px; background:#fa5f5f; border-radius:4px;",
            h3(nrow(r$missing_courses)), p("Unscheduled"))),
          column(3, div(
            style = if (nrow(r$prereq_issues) == 0)
              "text-align:center; padding:10px; background:#a6d492; border-radius:4px;"
            else
              "text-align:center; padding:10px; background:#f2dede; border-radius:4px;",
            h3(nrow(r$prereq_issues)), p("Prereq Conflicts")))
        ),
        br(),
        p(strong(sprintf("Degree completion: %d%%", pct))),
        div(style = "background:#eee; border-radius:4px; height:24px; width:100%;",
            div(style = sprintf("background:#5cb85c; width:%d%%; height:24px; border-radius:4px; line-height:24px; color:white; text-align:center;", pct),
                sprintf("%d%%", pct))
        )
      ),
      
      # Missing courses table
      wellPanel(
        h4(sprintf("Missing Courses (%d)", nrow(r$missing_courses))),
        if (nrow(r$missing_courses) == 0) {
          div(class = "alert alert-success", "All required courses are complete or planned!")
        } else {
          tableOutput("missing_table")
        }
      ),
      
      # Prereq conflicts table
      wellPanel(
        h4(sprintf("Prerequisite Conflicts (%d)", nrow(r$prereq_issues))),
        if (nrow(r$prereq_issues) == 0) {
          div(class = "alert alert-success", "No prerequisite conflicts found!")
        } else {
          tableOutput("prereq_summary_table")
        }
      ),
      
      # Elective slots
      wellPanel(
        h4("General Education and Technical Electives *TECHNICAL ELECTIVES ARE STUDENT REPORTED*"),
        tableOutput("elective_slots_table"),
        tags$small(
          "*Technical electives are self-reported, double check the approved list for your concentration* - ",
          # "Pre-approved technical electives: ",
          tags$a(href = "https://cet.ecu.edu/engineering/approved-technical-electives/",
                 target = "_blank", "Approved Technical Electives")
        )
      ),
      
      # Tech electives detail
      wellPanel(
        h4("Your Marked Technical Electives - Please double check with the approved list or your advisor!"),
        tableOutput("tech_electives_table"),
        tags$small(
          "*Technical electives are self-reported, double check the approved list for your concentration*"
        )
      )
    )
  })
  
  output$missing_table <- renderTable({
    r <- results(); req(r); req(nrow(r$missing_courses) > 0)
    r$missing_courses %>%
      select(Code = primary_code, Title = title,
             `Req. Semester` = semester, Category = category)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  output$prereq_summary_table <- renderTable({
    r <- results(); req(r); req(nrow(r$prereq_issues) > 0)
    r$prereq_issues %>% select(Semester, Course, `Unmet Prereq`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  output$elective_slots_table <- renderTable({
    r <- results(); req(r)
    
    make_slot_rows <- function(type, label) {
      done_map    <- r$completed_tag_map %>% filter(str_starts(tag, type))
      planned_map <- r$intended_tag_map  %>% filter(str_starts(tag, type))
      
      done_rows <- if (nrow(done_map) > 0)
        data.frame(Type = label, Course = done_map$code,
                   Credits = done_map$credits, Status = "Done",
                   stringsAsFactors = FALSE)
      else NULL
      
      planned_rows <- if (nrow(planned_map) > 0)
        data.frame(Type = label, Course = planned_map$code,
                   Credits = planned_map$credits, Status = "Planned",
                   stringsAsFactors = FALSE)
      else NULL
      
      bind_rows(done_rows, planned_rows)
    }
    
    tbl <- bind_rows(
      make_slot_rows("tech", "Technical Elective"),
      make_slot_rows("hum",  "Humanities / Fine Arts"),
      make_slot_rows("soc",  "Social Sciences")
    )
    
    if (nrow(tbl) == 0)
      return(data.frame(Note = "No electives tagged in your schedule yet."))
    
    tbl
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  output$tech_electives_table <- renderTable({
    r <- results(); req(r)
    done_map    <- r$completed_tag_map %>% filter(str_starts(tag, "tech"))
    planned_map <- r$intended_tag_map  %>% filter(str_starts(tag, "tech"))
    if (nrow(done_map) == 0 && nrow(planned_map) == 0) {
      return(data.frame(Note = "No technical electives tagged in your schedule yet."))
    }
    bind_rows(
      if (nrow(done_map) > 0)
        data.frame(Status = "Done",    Course = done_map$code,
                   Credits = done_map$credits,    stringsAsFactors = FALSE),
      if (nrow(planned_map) > 0)
        data.frame(Status = "Planned", Course = planned_map$code,
                   Credits = planned_map$credits, stringsAsFactors = FALSE)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  # ── Courses tab ───────────────────────────────────────────────────────────
  output$courses_ui <- renderUI({
    r <- results()
    if (is.null(r)) return(p("Run a degree check on the Setup tab first."))
    selectInput("course_filter", "Filter by status:",
                choices = c("All", "Complete", "Planned", "Missing"),
                selected = "All", width = "200px")
  })
  
  output$courses_table <- renderTable({
    r <- results(); req(r)
    tbl <- r$results %>%
      mutate(Status = case_when(
        status == "Complete" ~ "Complete",
        status == "Planned"  ~ "Planned",
        TRUE                 ~ "Missing"
      )) %>%
      select(Semester = semester, Code = primary_code,
             Title = title, Category = category,
             `Min Grade` = min_grade, Status)
    fv <- input$course_filter
    if (!is.null(fv) && fv != "All") tbl <- tbl %>% filter(Status == fv)
    tbl
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  # ── Prereq tab ────────────────────────────────────────────────────────────
  output$prereq_ui <- renderUI({
    r <- results()
    if (is.null(r)) return(p("Run a degree check on the Setup tab first."))
    if (nrow(r$prereq_issues) == 0) {
      return(div(class = "alert alert-success",
                 "No prerequisite conflicts found in your planned schedule."))
    }
    div(class = "alert alert-warning",
        strong(sprintf("%d conflict(s) found.", nrow(r$prereq_issues))),
        " Courses are scheduled before their prerequisites are satisfied.",
        br(),
        tags$small("P/C (prior or concurrent) requirements are flagged if they don't appear in a prior semester or your completed list.")
    )
  })
  
  output$prereq_table <- renderTable({
    r <- results(); req(r); req(nrow(r$prereq_issues) > 0)
    r$prereq_issues
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  
  # ── Planned Schedule tab ──────────────────────────────────────────────────
  output$schedule_ui <- renderUI({
    r <- results()
    if (is.null(r)) return(p("Run a degree check on the Setup tab first."))
    pbs <- r$planned_by_sem
    if (nrow(pbs) == 0) return(p("No courses parsed from the planned_schedule sheet."))
    
    sems <- unique(pbs$sem_name)
    cols <- lapply(sems, function(sem) {
      sem_courses <- pbs %>% filter(sem_name == sem)
      issues_here <- if (nrow(r$prereq_issues) > 0)
        r$prereq_issues %>% filter(Semester == sem) %>% pull(Course)
      else character(0)
      
      rows <- lapply(seq_len(nrow(sem_courses)), function(i) {
        code    <- sem_courses$course_code[i]
        cr      <- sem_courses$credits[i]
        flag    <- code %in% issues_here
        style   <- if (flag) "color:darkorange;" else ""
        cr_text <- if (!is.na(cr)) paste0(" (", cr, " cr)") else ""
        tags$li(style = style,
                code, tags$small(cr_text),
                if (flag) tags$small(" [prereq issue]")
        )
      })
      
      total_cr <- sum(sem_courses$credits, na.rm = TRUE)
      column(3, wellPanel(
        h5(sem),
        tags$ul(rows),
        tags$hr(style = "margin: 6px 0;"),
        tags$strong(sprintf("Total: %d credit hours", total_cr))
      ))
    })
    
    do.call(fluidRow, cols)
  })
  
  # ── Electives tab ─────────────────────────────────────────────────────────
  output$electives_ui <- renderUI({
    r <- results()
    if (is.null(r)) return(p("Run a degree check on the Setup tab first."))
    
    make_panel <- function(type, label) {
      needed      <- r$req_slots$slots_needed[r$req_slots$slot_type == type] %||% 0L
      done        <- r$slots_done[[type]]
      planned     <- r$slots_planned[[type]]
      total       <- done$n + planned$n
      ok          <- total >= needed
      done_codes    <- done$courses     # already course codes from count_slot
      planned_codes <- planned$courses
      
      wellPanel(
        h5(label),
        p(sprintf("Needed: %d  |  Done: %d  |  Planned: %d  |  Status: %s",
                  needed, done$n, planned$n, if (ok) "Met" else "Incomplete")),
        if (length(done_codes) > 0)
          p(strong("Completed: "), paste(done_codes, collapse = ", ")),
        if (length(planned_codes) > 0)
          p(strong("Planned: "), paste(planned_codes, collapse = ", "))
      )
    }
    
    tagList(
      fluidRow(
        column(4, make_panel("tech", "Technical Electives")),
        column(4, make_panel("hum",  "Humanities / Fine Arts")),
        column(4, make_panel("soc",  "Social Sciences"))
      ),
      p(tags$strong("!!! Pre-approved technical electives !!!: ",
                   tags$a(href = "https://cet.ecu.edu/engineering/approved-technical-electives/",
                          target = "_blank", "ECU list")))
    )
  })
}

shinyApp(ui, server)