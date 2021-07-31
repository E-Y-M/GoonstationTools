# libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(tidyverse)
    library(stringr)
    library(googlesheets4)
})

# functions ----
source("scripts/func.R") # helper functions
#source("scripts/gs4.R")

sheet_append <- function(ss, data, sheet = 1) {
    # always use faster append on deployed apps
    is_local <- Sys.getenv('SHINY_PORT') == ""
    if (!is_local) {
        x <- googlesheets4::sheet_append(ss, data, sheet)
        return(x)
    }
    
    # safer append for a development environment
    data_sheet_orig <- read_sheet(ss, sheet)
    
    # convert data to characters to avoid bind problems
    data_sheet_char <- apply(data_sheet_orig, 2, as.character, simplify = FALSE)
    data_sheet <- as.data.frame(data_sheet_char)
    list_char <- apply(data, 2, as.character, simplify = FALSE)
    data_char <- as.data.frame(list_char)
    
    # bind and convert
    if (nrow(data_sheet) > 0) {
        data_bind <- dplyr::bind_rows(data_sheet, data_char)
    } else {
        data_bind <- data_char
    }
    data_conv <- type.convert(data_bind, as.is = TRUE)
    
    # get data types
    orig_types <- apply(data_sheet, 2, typeof)
    conv_types <- apply(data_conv, 2, typeof)
    
    if (identical(orig_types, conv_types)) {
        # append last row: fast and won't cause overwrite
        last_row <- data_conv[nrow(data_conv), ]
        googlesheets4::sheet_append(ss, last_row, sheet)
    } else {
        # the data has extra columns, or changes data types, so over-write
        # slower and might cause simultaneous access problems
        googlesheets4::write_sheet(data_conv, ss, sheet)
    }
}

# setup ----
gs4_deauth()
#gs4_auth(cache = ".secrets", email = "")
words_data = read.csv("data/hangman_processed.csv", fileEncoding = 'UTF-8-BOM')
google_sheet_id = "1V1T2Jml1OyxDyIY71GQtv-8EG6A66g7qnyGCKLPj6yk"

# user interface ----
shinyjs::useShinyjs()

## Tabs ----
### Hangman ----
hangman_tab <- tabItem(
    tabName = "hangman_tab",
    box(width = 8,
        collapsible = FALSE,
        title = NULL,
        radioButtons("n_letters",
                     "Word length",
                     c("5" = 5,
                       "7" = 7,
                       "9" = 9),
                     selected = character(0)),
        textInput("guess",
                  "Enter correct letters (leave unknowns as .)",
                  value = "........."),
        textInput("wrong_letter",
                  "Enter incorrect letters",
                  value = ""),
        tags$p(strong("Incorrect letters:")),
        textOutput("wrong_letters"),
        tags$p(strong("Recommended guess:")),
        textOutput("recommended_guess"),
        tags$br(),
        actionButton("reset",
                     "Reset")),
    box(width = 4,
        collapsible = FALSE,
        title = "Candidates",
        tableOutput("candidates"))
        
    #box(width = 12,
    #    collapsible = FALSE,
    #    title = "Candidates",
    #    tableOutput("candidates"))
)

### Calibration ----
calib_tab <- tabItem(
    tabName = "calib_tab",
    box(width = 6,
        collapsible = TRUE,
        title = "Input Coords",
        numericInput("X1", 
                     "X1",
                     50),
        numericInput("Y1",
                     "Y1",
                     50),
        numericInput("X2",
                     "X2",
                     51),
        numericInput("Y2",
                     "Y2",
                     51),
        numericInput("stationZ",
                     "Station Z",
                     0)
    ),
    box(width = 6,
        collapsible = TRUE,
        title = "Output Coords",
        numericInput("X1_out", 
                     "X1",
                     50),
        numericInput("Y1_out",
                     "Y1",
                     50),
        numericInput("X2_out",
                     "X2",
                     51),
        numericInput("Y2_out",
                     "Y2",
                     51)
    )
)

### Custom coordinate entry ----
custom_tab = tabItem(
    tabName = "custom_tab",
    box(width = 6,
        collapsible = FALSE,
        title = "Input coords",
        numericInput("X_in", 
                     "Raw X",
                     50),
        numericInput("Y_in",
                     "Raw Y",
                     50),
        numericInput("Z_in",
                     "Z",
                     0)),
    box(width = 6,
        collapsible = FALSE,
        title = "Output coords",
        textOutput("custom_coords"))
)

### Chemgroups ----
chem_tab = tabItem(
    tabName = "chem_tab",
    box(width = 12,
        collapsible = TRUE,
        title = "Chemgroups",
        tags$p("HIGHLY experimental--connects to a Google Sheet that is editable by anyone using the app"),
        textInput("chem_search",
                  "Search",
                  value = ""),
        div(style = 'overflow-x: scroll', tableOutput("chem_data"))),
    box(width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Add new chem",
        textInput("chem_name",
                  "Name",
                  value = ""),
        textInput("chem_grp1",
                  "Group 1",
                  ""),
        textInput("chem_grp2",
                  "Group 2",
                  ""),
        textInput("chem_grp3",
                  "Group 3",
                  ""),
        textInput("chem_notes",
                  "Notes",
                  ""),
        actionButton("chem_add",
                     "Add")),
    box(width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Edit existing chem",
        textInput("chem_edit_name",
                  "Name",
                  value = ""),
        textInput("chem_edit_newname",
                  "New name",
                  value = ""),
        textInput("chem_edit_grp1",
                  "Group 1",
                  ""),
        textInput("chem_edit_grp2",
                  "Group 2",
                  ""),
        textInput("chem_edit_grp3",
                  "Group 3",
                  ""),
        textInput("chem_edit_notes",
                  "Notes",
                  ""),
        actionButton("chem_edit",
                     "Change"))
)

## UI ----
skin_color <- "black"

ui <- dashboardPage(
    skin = skin_color,
    dashboardHeader(title = "Goonstation Tools", 
                    titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
    ),
    dashboardSidebar(
        # https://fontawesome.com/icons?d=gallery&m=free
        sidebarMenu(
            id = "tabs",
            menuItem("Hangman solver", tabName = "hangman_tab", icon = icon("archive")),
            menuItem("TSci calibration", tabName = "calib_tab", icon = icon("cog")),
            menuItem("TSci conversion", tabName = "custom_tab", icon = icon("map-pin")),
            menuItem("Chemistry", tabName = "chem_tab", icon = icon("vial"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            # links to files in www/
            tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"), 
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
            tags$script(src = "custom.js")
        ),
        tabItems(
            hangman_tab,
            calib_tab,
            custom_tab,
            chem_tab
        )
    )
)

server <- function(input, output, session) {
    
    store = reactiveValues()
    
    store$wrong_letters = vector()
    
    store$candidates = words_data %>% 
        filter(str_count(word) == 9)

    # Update default word length in textbox ----
    observeEvent(c(input$n_letters, input$reset), {
        req(input$n_letters)
        updateTextInput(session,
                        "guess",
                        value = paste(rep(".", input$n_letters), collapse = ""))
        
        store$candidates = words_data %>% 
            filter(str_count(word) == as.numeric(input$n_letters))
        
        candidates_collapsed = store$candidates %>%
            select(word) %>% 
            as_vector() %>% 
            paste(collapse = "")
        
        output$recommended_guess = renderText({
            most_repeated_character(candidates_collapsed)
        })
    })
    
    # Reactive variable to hold guess ----
    guess = reactive({
        tolower(sprintf("^%s", input$guess))
    })
    
    output$guess = renderText({
        tolower(guess)
    })
    
    # Reactive variable to hold forbidden letters ----
    newEntry = observe({
        if(str_length(input$wrong_letter) > 0) {
            newLetter = isolate(input$wrong_letter)
            isolate(store$wrong_letters <<- c(store$wrong_letters, newLetter))
        }
    })
    
    observeEvent(input$wrong_letter, {
        if (length(store$wrong_letters) > 0) {
            output$wrong_letters = renderText({
                store$wrong_letters
            })
        } else {
            output$wrong_letters = renderText({
                ""
            })
        }
    })
    
    observeEvent(input$wrong_letter, {
        updateTextInput(session, "wrong_letter", value = NA)
    })
    
    # Filtering the dataframe ----
    observeEvent(c(input$guess, input$wrong_letter), {
        req(input$n_letters)
        test_letters = unlist(strsplit((str_replace_all(input$guess, "[^[:alnum:]]", "")), ""))
        store$candidates = words_data
        
        if (length(test_letters) > 0 & length(store$wrong_letters) == 0) {
            for (i in 1:length(test_letters)) {
                curr_letter = test_letters[i]
                curr_num = sum(str_count(test_letters, curr_letter))
                
                for (j in 1:nrow(store$candidates)) {
                    if (sum(str_count(store$candidates$word[j], curr_letter)) == curr_num & 
                        grepl(input$guess, store$candidates$word[j], ignore.case = TRUE) &
                        str_count(store$candidates$word[j]) == as.numeric(input$n_letters)) {
                        store$candidates$exclude[j] = store$candidates$exclude[j] 
                    } else {
                        store$candidates$exclude[j] = store$candidates$exclude[j] + 1
                    }
                }
            }
        } else if (length(test_letters) > 0 & length(store$wrong_letters) > 0) {
            for (i in 1:length(test_letters)) {
                curr_letter = test_letters[i]
                curr_num = sum(str_count(test_letters, curr_letter))
                
                for (j in 1:nrow(store$candidates)) {
                    if (sum(str_count(store$candidates$word[j], curr_letter)) == curr_num &
                        grepl(input$guess, store$candidates$word[j], ignore.case = TRUE) &
                        !grepl(paste(store$wrong_letters, collapse = "|"), store$candidates$word[j], ignore.case = TRUE) &
                        str_count(store$candidates$word[j]) == as.numeric(input$n_letters)) {
                        store$candidates$exclude[j] = store$candidates$exclude[j]
                    } else {
                        store$candidates$exclude[j] = store$candidates$exclude[j] + 1
                    }
                }
            }
        } else {
            store$candidates = store$candidates %>% 
                filter(str_count(word) == as.numeric(input$n_letters))
        }
    
        #if (length(store$wrong_letters) == 0) {
        #    store$candidates = store$candidates %>%
        #        filter(grepl(input$guess, word, ignore.case = TRUE) &
        #                   str_count(word) == as.numeric(input$n_letters) &
        #                   (exclude == 0 | is.na(exclude)))
        #} else {
        #    store$candidates = store$candidates %>%
        #        filter(grepl(input$guess, word, ignore.case = TRUE) &
        #                   !grepl(paste(store$wrong_letters, collapse = "|"), word, ignore.case = TRUE) &
        #                   str_count(word) == as.numeric(input$n_letters) &
        #                   (exclude == 0 | is.na(exclude)))
        #}
    })
    
    observeEvent(store$candidates, {
        guess_split = unlist(strsplit(tolower(input$guess), ""))
        
        candidates_collapsed = store$candidates %>% 
            filter(exclude == 0 | is.na(exclude)) %>% 
            select(word) %>% 
            as_vector() %>% 
            paste(collapse = "")
        
        candidates_expanded = unlist(strsplit(candidates_collapsed, ""))
        
        candidates_subset = candidates_expanded[candidates_expanded %nin% guess_split] %>% 
            paste(collapse = "")
        
        output$recommended_guess = renderText({
            most_repeated_character(candidates_subset)
        })
    })
    
    
    output$candidates = renderTable({
        store$candidates %>% 
            filter(exclude == 0) %>% 
            select(word)
    })
    
    # Reset button stuff ----
    observeEvent(c(input$reset, input$n_letters), {
        store$wrong_letters = vector()
        
        output$wrong_letters = renderText({
            ""
        })
    })
    
    ## Change telesci calibration inputs automatically ----
    observeEvent(input$X1, {
        input$X1
        
        X1_original = input$X1
        
        updateNumericInput(session, "X2", value = X1_original + 1)
    })
    
    observeEvent(input$Y1, {
        input$Y1
        
        Y1_original = input$Y1
        
        updateNumericInput(session, "Y2", value = Y1_original + 1)
    })
    
    ## Calculate telesci calibration parameters ----
    mX = reactive({
        input$X2_out - input$X1_out
    })
    
    mY = reactive({
        input$Y2_out - input$Y1_out
    })
    
    cX = reactive({
        input$X1_out - (mX()*input$X1)
    })
    
    cY = reactive({
        input$Y1_out - (mY()*input$Y1)
    })
    
    ## Reactively get custom coordinates ----
    custom_X = reactive({
        as.character((input$X_in - cX()) / mX())
    })
    
    custom_Y = reactive({
        as.character((input$Y_in - cY()) / mY())
    })
    
    custom_Z = reactive({
        as.character(input$Z_in)
    })
    
    observeEvent(c(input$X_in, input$Y_in, input$Z_in), {
        custom_coords = sprintf("%s - %s - %s",
                                custom_X(),
                                custom_Y(),
                                custom_Z())
        
        output$custom_coords = renderText({
            custom_coords
        })
    })
    
    ## Chemgroup sheet ----
    ### Searching chems ----
    observeEvent(input$chem_search, {
        if (length(input$chem_search) == 0) {
            chem_data = as.data.frame(read_sheet(google_sheet_id, "Chemlist"))
        } else {
            chem_data = as.data.frame(read_sheet(google_sheet_id, "Chemlist")) %>% 
                filter(grepl(input$chem_search, Name, ignore.case = TRUE))
        }
        
        output$chem_data = renderTable({
            chem_data %>% 
                arrange(Name)
        })
    })
    
    ### Adding chems ----
    chem_add = reactive({
        data.frame("Name" = input$chem_name,
                   "Group1" = input$chem_grp1,
                   "Group2" = input$chem_grp2,
                   "Group3" = input$chem_grp3,
                   "Notes" = input$chem_notes)
    })
    
    observeEvent(input$chem_add, {
        chem_new_data = sheet_append(google_sheet_id, chem_add(), "Chemlist")
        
        output$chem_data = renderTable({
            as.data.frame(read_sheet(google_sheet_id, "Chemlist")) %>% 
                arrange(Name)
        })
    })
    
    ### Editing chems ----
    output$chem_data = renderTable({
        as.data.frame(read_sheet(google_sheet_id, "Chemlist")) %>% 
            arrange(Name)
    })
    
    
    observeEvent(input$chem_edit_name, {
        chem_edit_data = as.data.frame(read_sheet(google_sheet_id, "Chemlist"))
        
        updateTextInput(session, 
                        "chem_edit_grp1",
                        value = chem_edit_data$Group1[chem_edit_data$Name == input$chem_edit_name])
        
        updateTextInput(session, 
                        "chem_edit_grp2",
                        value = chem_edit_data$Group2[chem_edit_data$Name == input$chem_edit_name])
        
        updateTextInput(session, 
                        "chem_edit_grp3",
                        value = chem_edit_data$Group3[chem_edit_data$Name == input$chem_edit_name])
        
        updateTextInput(session, 
                        "chem_edit_notes",
                        value = chem_edit_data$Notes[chem_edit_data$Name == input$chem_edit_name])
    })
    
    #### On submission ----
    observeEvent(input$chem_edit, {
        chem_new_data = data.frame("Name" = input$chem_edit_newname,
                                   "Group1" = input$chem_edit_grp1,
                                   "Group2" = input$chem_edit_grp2,
                                   "Group3" = input$chem_edit_grp3,
                                   "Notes" = input$chem_edit_notes)
        
        chem_edit_data = as.data.frame(read_sheet(google_sheet_id, "Chemlist")) %>% 
            filter(Name != input$chem_edit_name) %>% 
            rbind(chem_new_data)
        
        #chem_edit_data$Group1[chem_edit_data$Name == input$chem_edit_name] = input$chem_edit_grp1
        #chem_edit_data$Group2[chem_edit_data$Group2 == input$chem_edit_name] = input$chem_edit_grp2
        #chem_edit_data$Group3[chem_edit_data$Group3 == input$chem_edit_name] = input$chem_edit_grp3
        #chem_edit_data$Notes[chem_edit_data$Notes == input$chem_edit_name] = input$chem_edit_notes
        
        write_sheet(chem_edit_data, google_sheet_id, "Chemlist")
        
        output$chem_data = renderTable({
            as.data.frame(read_sheet(google_sheet_id, "Chemlist")) %>% 
                arrange(Name)
        })
    })
}

shinyApp(ui, server)