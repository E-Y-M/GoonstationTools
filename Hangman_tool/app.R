# libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(tidyverse)
    library(stringr)
})

# functions ----
# display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
    is_local <- Sys.getenv('SHINY_PORT') == ""
    in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
    txt <- toString(list(...))
    if (is_local) message(txt)
    if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}

debug_sprintf <- function(fmt, ...) {
    debug_msg(sprintf(fmt, ...))
}

`%nin%` = Negate(`%in%`)

most_repeated_character <- function(x) {
    tab <- table(strsplit(x, '')[[1]])
    names(tab)[tab == max(tab)]
}

# setup ----
words_data = read.csv("data/hangman_processed.csv", fileEncoding = 'UTF-8-BOM')
#words_data = data.frame(word = c("gazelle", "chemistry", "razor"),
#                        exclude = 0)

# functions ----
#source("scripts/func.R") # helper functions

# user interface ----
shinyjs::useShinyjs()

## Tabs ----
### Calibration ----
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
            menuItem("Hangman solver", tabName = "hangman_tab", icon = icon("archive"))
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
            hangman_tab
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
}

shinyApp(ui, server)