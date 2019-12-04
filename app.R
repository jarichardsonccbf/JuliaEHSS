library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(flextable)
library(officer)
# library(xlsx)

options(shiny.maxRequestSize=30*1024^2)

source("locations.R")

# Function for uploading multiple. I have no idea how this works.
fileInput2 <- function(inputId, label = NULL, labelIcon = NULL, multiple = FALSE, 
                       accept = NULL, width = NULL, progress = TRUE, ...) {
  # add class fileinput_2 defined in UI to hide the inputTag
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", 
                         class = "fileinput_2")
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  
  div(..., style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"), 
      inputTag,
      # label customized with an action button
      tags$label(`for` = inputId, div(icon(labelIcon), label, 
                                      class = "btn btn-default action-button")),
      # optionally display a progress bar
      if(progress)
        tags$div(id = paste(inputId, "_progress", sep = ""), 
                 class = "progress shiny-file-input-progress", 
                 tags$div(class = "progress-bar")
        )
  )
}          


ui <- fluidPage(
  
  # App title ----
  titlePanel("TIR/LTR Tool"),
  
  # define class fileinput_2 to hide inputTag in fileInput2. Not sure what this is doing.
  tags$head(tags$style(HTML(
    ".fileinput_2 {
      width: 0.1px;
      height: 0.1px;
      opacity: 0;
      overflow: hidden;
      position: absolute;
      z-index: -1;
    }"
  ))),
  
  # Side bar layout and inputs ----
  sidebarLayout(
    sidebarPanel(
      
      # exempt upload ----
      h4("Exempt Employees"),
      fileInput2("file1", "File location", labelIcon = "folder-open-o", 
                 accept = c(".xlsx"), progress = TRUE),
      
      # nonexempt upload ----
      h4("Non-Exempt Employees"),
      fileInput2("file2", "File location", labelIcon = "folder-open-o", 
                 accept = c(".xlsx"), progress = TRUE),
      
      # expenditures upload ----
      # h4("Expenditures Billed"),  
      # fileInput2("file3", "File location", labelIcon = "folder-open-o", 
      #            accept = c(".xlsx"), progress = TRUE),
      
      # cbcs osha upload ----
      h4("CBCS OSHA"),  
      fileInput2("file4", "File location", labelIcon = "folder-open-o", 
                 accept = c(".xlsx"), progress = TRUE),
      
      # month and year ----
      h4("Timeframe"),
      selectInput("month", label = "Month:", 
                  choices = list(
                    "January" = "January", 
                    "February" = "February", 
                    "March" = "March",
                    "April" = "April",
                    "May" = "May",
                    "June" = "June",
                    "July" = "July",
                    "August" = "August",
                    "September" = "September",
                    "October" = "October",
                    "November" = "November",
                    "December" = "December"), 
                  selected = months(Sys.Date() %m+% months(-1))),
        textInput("year", "Year:", value = year(Sys.Date()))
    ),
    
    # Main panel display. Use tabs, one for each slide ----
    mainPanel(
      
      h2("Upload the specified data to the left, copy and paste the output into the Monthly Hours Report spreadsheet", align = "center"),
      
      span(textOutput(outputId = "warning.message"), style = "color:red"),
      
      splitLayout(
        
        uiOutput(outputId = "hours.table"),
        uiOutput(outputId = "inc.table")
        
        )
                  
      )
      
    )
  )

server <- function(input, output, session) {
  
  # reporting reactive ----

  hours.df <- reactive({
    req(input$file1)
    
    exempt.hours <- read_excel(input$file1$datapath, sheet = "Sheet1") %>% 
      left_join(exempt.locations, by = c("L4 Org Unit Name", "Personnel Area Desc")) %>% 
      group_by(Facility, Section) %>% 
      summarise(`Hours worked` = sum(`Total Hours`, na.rm = T)) %>% 
      drop_na()
    
    all.loc <- exempt.locations %>% 
      select(-c(`L4 Org Unit Name`, `Personnel Area Desc`)) %>% 
      unique()
    
    all.loc <- exempt.hours %>%  
      right_join(all.loc, by = c("Facility", "Section"))
    
    exempt.totals <- all.loc %>% 
      group_by(Facility) %>% 
      summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
      mutate(Section = "A") %>% 
      select(Facility, Section, `Hours worked`) %>% 
      mutate(Section = as.factor(Section))
    
    exempt.all <- bind_rows(exempt.totals, all.loc) %>%
      arrange(Facility, Section) %>% 
      mutate(Section = recode(Section,
                              "A" = "Total")) %>% 
      ungroup() 
    
    exempt.all[exempt.all == 0] <- NA
    
    exempt.all <- rbind(as.data.frame(exempt.all),
                        exempt.all %>% 
                          ungroup() %>% 
                          summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
                          mutate(Facility = "Total",
                                 Section = "Total") %>% 
                          select(Facility, Section, `Hours worked`))

    
    req(input$file2)
    
    non.exempt <- read_excel(input$file2$datapath, sheet = "Details") %>%
      filter(`Wagetype Desc.` != "3C Sick Pay",
             `Wagetype Desc.` != "3C Vacation Pay",
             `Wagetype Desc.` != "3C Vac Payout SupTx") %>% 
      droplevels() %>%
      rename(Cost.Center.Desc. = `Cost Center Desc.`) %>% 
      mutate(Cost.Center.Desc. = as.factor(Cost.Center.Desc.)) %>% 
      left_join(non.exempt.locations, by = "Cost.Center.Desc.")
    
    non.exempt.hours <- non.exempt %>% 
      group_by(Facility, Section) %>% 
      summarise(`Hours worked` = sum(Hours))
    
    all.loc <- non.exempt.locations %>% 
      select(-c(Cost.Center.Desc.)) %>% 
      unique()
    
    all.loc <- non.exempt.hours %>%  
      right_join(all.loc, by = c("Facility", "Section"))
    
    
    non.exempt.totals <- all.loc %>% 
      group_by(Facility) %>% 
      summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
      mutate(Section = "A") %>% 
      select(Facility, Section, `Hours worked`) %>% 
      mutate(Section = as.factor(Section))
    
    non.exempt.all <- bind_rows(non.exempt.totals, all.loc) %>%
      arrange(Facility, Section) %>% 
      mutate(Section = recode(Section,
                              "A" = "Total")) %>% 
      ungroup() 
    
    non.exempt.all[non.exempt.all == 0] <- NA
    
    non.exempt.all <- rbind(as.data.frame(non.exempt.all),
                            non.exempt.all %>% 
                              ungroup() %>% 
                              summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
                              mutate(Facility = "Total",
                                     Section = "Total") %>% 
                              select(Facility, Section, `Hours worked`))
    
    
    # req(input$file3)
    
    # file 3 stuff
    
    # Join them and get output df
    
    req(input$file4)
    
    osha <- read_excel(input$file4$datapath, sheet = "Data") %>% 
      mutate(month = months(`Loss Date`),
             year = year(`Loss Date`)) %>% 
      filter(year == input$year,
             month == input$month) %>%
        left_join(cbcs.locations, by = c("Location"))
      
      osha.total <- osha %>% 
        group_by(Facility, Section) %>% 
        summarise(OR = n())
      
      osha.work.loss <- osha %>% 
        filter(`Lost Work Days` > 0) %>% 
        group_by(Facility, Section) %>% 
        summarise(LT = n())
      
      osha.count <- osha.total %>% 
        left_join(osha.work.loss, by = c("Facility", "Section"))
      
      osha.totals <- osha.count %>% 
        pivot_longer(cols = OR:LT, names_to = "incident.type") %>% 
        group_by(Facility, incident.type) %>% 
        summarise(total = sum(value, na.rm = T)) %>% 
        pivot_wider(names_from = incident.type, values_from = total) %>% 
        mutate(Section = "A") %>% 
        select(Facility, Section, OR, LT)
      
      osha.all <- rbind(osha.count, osha.totals) %>%
        arrange(Facility, Section) %>% 
        mutate(Section = recode(Section,
                                "A" = "Total")) %>% 
        ungroup() 
      
      osha.all[osha.all == 0] <- NA
    
    osha.all <- rbind(osha.all,
                      osha.all %>% 
                        summarise_at(vars (OR:LT), sum, na.rm = T) %>% 
                        mutate(Facility = "Total",
                               Section = "Total") %>% 
                        select(Facility, Section, OR, LT))
    
    osha.exempt.non <- non.exempt.all %>% 
      full_join(exempt.all, by = c("Facility", "Section")) %>% 
      replace(is.na(.), 0) %>%
      mutate(`Hours worked` = `Hours worked.x` + `Hours worked.y`) %>% 
      select(-c(`Hours worked.x`, `Hours worked.y`)) %>% 
      full_join(osha.all, by = c("Facility", "Section")) %>% 
      arrange(Facility) %>%
      select(Facility, Section, `Hours worked`)

  })
  
  output$warning.message <- renderText({
    
    req(input$file4)
    
    osha <- read_excel(input$file4$datapath, sheet = "Data") %>% 
      mutate(month = months(`Loss Date`),
             year = year(`Loss Date`)) %>% 
      filter(year == input$year,
             month == input$month)
    
    osha.message <- if("HEADQUARTERS - HQ" %in% osha$Location == TRUE) {
      
      "There is a HQ incident this month. Please convert 'HEADQUARTERS - HQ' to 'HEADQUARTERS - SALES' or 'HEADQUARTERS - ADMIN', save the file, and reupload." }
    
  })
  
  
  output$hours.table <- renderUI({
    hours.df() %>% 
      flextable() %>% 
      border_remove() %>% 
      bold( i = ~ `Section` == "Total") %>%
      bg(bg = "#92D050", part = "body", i = ~ `Section` == "Total") %>% 
      bg(bg = "#FFFF00", part = "body", i = nrow(flextable(hours.df())$body$dataset)) %>% 
      colformat_num(col_keys = "Hours worked", big.mark = "", digits = 2) %>% 
      htmltools_value()
  })
  
  inc.df <- reactive({
    
    req(input$file1)
    
    exempt.hours <- read_excel(input$file1$datapath, sheet = "Sheet1") %>% 
      left_join(exempt.locations, by = c("L4 Org Unit Name", "Personnel Area Desc")) %>% 
      group_by(Facility, Section) %>% 
      summarise(`Hours worked` = sum(`Total Hours`, na.rm = T)) %>% 
      drop_na()
    
    all.loc <- exempt.locations %>% 
      select(-c(`L4 Org Unit Name`, `Personnel Area Desc`)) %>% 
      unique()
    
    all.loc <- exempt.hours %>%  
      right_join(all.loc, by = c("Facility", "Section"))
    
    exempt.totals <- all.loc %>% 
      group_by(Facility) %>% 
      summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
      mutate(Section = "A") %>% 
      select(Facility, Section, `Hours worked`) %>% 
      mutate(Section = as.factor(Section))
    
    exempt.all <- bind_rows(exempt.totals, all.loc) %>%
      arrange(Facility, Section) %>% 
      mutate(Section = recode(Section,
                              "A" = "Total")) %>% 
      ungroup() 
    
    exempt.all[exempt.all == 0] <- NA
    
    exempt.all <- rbind(as.data.frame(exempt.all),
                        exempt.all %>% 
                          ungroup() %>% 
                          summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
                          mutate(Facility = "Total",
                                 Section = "Total") %>% 
                          select(Facility, Section, `Hours worked`))
    
    
    req(input$file2)
    
    non.exempt <- read_excel(input$file2$datapath, sheet = "Details") %>%
      filter(`Wagetype Desc.` != "3C Sick Pay",
             `Wagetype Desc.` != "3C Vacation Pay",
             `Wagetype Desc.` != "3C Vac Payout SupTx") %>% 
      droplevels() %>%
      rename(Cost.Center.Desc. = `Cost Center Desc.`) %>% 
      mutate(Cost.Center.Desc. = as.factor(Cost.Center.Desc.)) %>% 
      left_join(non.exempt.locations, by = "Cost.Center.Desc.")
    
    non.exempt.hours <- non.exempt %>% 
      group_by(Facility, Section) %>% 
      summarise(`Hours worked` = sum(Hours))
    
    all.loc <- non.exempt.locations %>% 
      select(-c(Cost.Center.Desc.)) %>% 
      unique()
    
    all.loc <- non.exempt.hours %>%  
      right_join(all.loc, by = c("Facility", "Section"))
    
    
    non.exempt.totals <- all.loc %>% 
      group_by(Facility) %>% 
      summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
      mutate(Section = "A") %>% 
      select(Facility, Section, `Hours worked`) %>% 
      mutate(Section = as.factor(Section))
    
    non.exempt.all <- bind_rows(non.exempt.totals, all.loc) %>%
      arrange(Facility, Section) %>% 
      mutate(Section = recode(Section,
                              "A" = "Total")) %>% 
      ungroup() 
    
    non.exempt.all[non.exempt.all == 0] <- NA
    
    non.exempt.all <- rbind(as.data.frame(non.exempt.all),
                            non.exempt.all %>% 
                              ungroup() %>% 
                              summarise(`Hours worked` = sum(`Hours worked`, na.rm = T)) %>% 
                              mutate(Facility = "Total",
                                     Section = "Total") %>% 
                              select(Facility, Section, `Hours worked`))
    
    
    # req(input$file3)
    
    # file 3 stuff
    
    # Join them and get output df
    
    req(input$file4)
    
    osha <- read_excel(input$file4$datapath, sheet = "Data") %>% 
      mutate(month = months(`Loss Date`),
             year = year(`Loss Date`)) %>% 
      filter(year == input$year,
             month == input$month) %>%
      left_join(cbcs.locations, by = c("Location"))
    
    osha.total <- osha %>% 
      group_by(Facility, Section) %>% 
      summarise(OR = n())
    
    osha.work.loss <- osha %>% 
      filter(`Lost Work Days` > 0) %>% 
      group_by(Facility, Section) %>% 
      summarise(LT = n())
    
    osha.count <- osha.total %>% 
      left_join(osha.work.loss, by = c("Facility", "Section"))
    
    osha.totals <- osha.count %>% 
      pivot_longer(cols = OR:LT, names_to = "incident.type") %>% 
      group_by(Facility, incident.type) %>% 
      summarise(total = sum(value, na.rm = T)) %>% 
      pivot_wider(names_from = incident.type, values_from = total) %>% 
      mutate(Section = "A") %>% 
      select(Facility, Section, OR, LT)
    
    osha.all <- rbind(osha.count, osha.totals) %>%
      arrange(Facility, Section) %>% 
      mutate(Section = recode(Section,
                              "A" = "Total")) %>% 
      ungroup() 
    
    osha.all[osha.all == 0] <- NA
    
    osha.all <- rbind(osha.all,
                      osha.all %>% 
                        summarise_at(vars (OR:LT), sum, na.rm = T) %>% 
                        mutate(Facility = "Total",
                               Section = "Total") %>% 
                        select(Facility, Section, OR, LT))
    
    osha.exempt.non <- non.exempt.all %>% 
      full_join(exempt.all, by = c("Facility", "Section")) %>% 
      replace(is.na(.), 0) %>%
      mutate(`Hours worked` = `Hours worked.x` + `Hours worked.y`) %>% 
      select(-c(`Hours worked.x`, `Hours worked.y`)) %>% 
      full_join(osha.all, by = c("Facility", "Section")) %>% 
      arrange(Facility) %>%
      select(Facility, Section, OR, LT)

  })
  
  output$inc.table <- renderUI({
    inc.df() %>% 
      flextable() %>% 
      border_remove() %>% 
      bold( i = ~ `Section` == "Total") %>%
      bg(bg = "#92D050", part = "body", i = ~ `Section` == "Total") %>% 
      bg(bg = "#FFFF00", part = "body", i = nrow(flextable(inc.df())$body$dataset)) %>% 
      htmltools_value()
  })
  
}

shinyApp(ui, server)
