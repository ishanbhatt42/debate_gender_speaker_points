
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(infer)
library(broom)

# Read in the data.

ld_data <- read_csv("raw-data/ld_data_standard.csv")

# Define a list of the tournaments for input purposes.

ld_tournaments <- tibble(
    "Apple Valley" = "apple_valley",
    "Bronx" = "bronx",
    "Cal Berkeley" = "cal",
    "Emory" = "emory",
    "Glenbrooks" = "glenbrooks",
    "Greenhill" = "greenhill",
    "Harvard" = "harvard",
    "Harvard Westlake" = "hw",
    "St. Mark's Heart of Texas" = "st_marks",
    "Valley Mid America Cup" = "valley")

# Define UI.

ui <- fluidPage(
    
    navbarPage(
        "Gender's Effect on Speaker Points in National Circuit Lincoln-Douglas Debate", 
        theme = shinytheme("yeti"),
        tabPanel(
            
            # Add a page to explore the data. Within it, 
            # add a sidebar with inputs for the competitive
            # seasons and the LD tournaments in the data.
            
            # Add a picker for the plot type.
            
            "Explore the Data",
            fluidPage(
                titlePanel("Speaker Point Totals by Gender"),
                sidebarLayout(
                    sidebarPanel(
                        p("This data is sourced from every octafinals bid tournament 
                        starting in the 2016-17 season. Since different tournaments 
                        have a different number of prelim debates, the speaker points 
                        are standardized. It displays the association of speaker 
                        points with gender."),
                        pickerInput(
                            inputId = "explore_szn",
                            label = "Choose the Competitive Season(s)",
                            choices = c(2018, 2019, 2020),
                            multiple = TRUE,
                            selected = c(2018, 2019, 2020)
                        ),
                        
                        pickerInput(
                            inputId = "explore_tourney",
                            label = "Choose the Tournament(s)",
                            choices = ld_tournaments,
                            multiple = TRUE,
                            selected = ld_tournaments
                        ),
                        pickerInput(
                            inputId = "explore_plot_type",
                            label = "Choose the Plot Type",
                            choices = c("Density", "Point", "Averages"),
                            multiple = FALSE,
                            selected = "Density"
                        ),
                        
                        # Add two select boxes. One to choose the size 
                        # of the bootstrap. One to select to display
                        # the bootstrap.
                        
                        p(
                            "Here, I've set up a function to bootstrap* the data. This will
                            give you an estimated average difference between genders, which is
                            a robustness check on the data. Bootstrapping the data slows down the 
                            app a lot, so it's initially disabled, but feel free to turn it on!"
                        ),
                        checkboxInput(
                            inputId = "display_boot_plot",
                            label = "Run the Bootstrap!!",
                            value = FALSE
                        ),
                        sliderInput(
                            inputId = "rep_size",
                            label = "Choose the Bootstrap Replicate Number",
                            min = 10, 
                            max = 1000,
                            value = 500,
                            ticks = FALSE,
                            sep = ""
                        ),
                        p(
                            "*Using the observations whatever subset of the data 
                            you select, the bootstrap will take a random sample 
                            with replacement of the data equating to the number of 
                            points in the data. It will repeat this process many times 
                            in order to create a bunch of similar data sets that 
                            each exclude a little bit of the data. This introduces 
                            variation into each set to check if the data is skewed 
                            by a couple of points."
                        )
                    ),
                    mainPanel(
                        plotOutput("explore_plot"),
                        verbatimTextOutput("explore_lm"),
                        verbatimTextOutput("explore_boot"),
                        plotOutput("bootstrap_plot")
                    )
                )
            )
        ),
        
        tabPanel(
            "Findings",
            titlePanel("Results of Statistical Analysis"),
            sidebarLayout(
                sidebarPanel(
                    h3("An Initial Regression"),
                    h3("A Two-Sample T Test"),
                    ),
                mainPanel(
                    h3("The Total Distribution"),
                    plotOutput("main_plot"),
                    verbatimTextOutput("main_lm"),
                    h3("Speaker Point Difference Over Time"),
                    plotOutput("main_plot_time")
                    )
                )
            ),
        
        # Add the about page.
        
        tabPanel(
            "About",
            titlePanel("Information Page"),
            h3("The Data"),
        )
    )

)

# Define server logic.

server <- function(input, output) {
    
    output$explore_plot <- renderPlot ({
        
        x <- ld_data %>%
            filter(season %in% input$explore_szn) %>%
            filter(tourn %in% input$explore_tourney)
        
        x_avg <- x %>%
            group_by(gender_numeric) %>%
            summarize(mean_pts = mean(z))
        
        x_distrib <- ggplot(data = x, aes(x = z, fill = gender)) + 
            geom_density(alpha = .6) +
            labs(
                title = "Distribution of Speaker Points by Gender",
                x = "Points (1 High Low, Standardized)",
                y = "Density",
                fill = "Gender"
            ) +
            theme_classic()
        
        x_point <- ggplot(x, aes(gender_numeric, z)) + 
            geom_point() +
            geom_jitter(width = .2, alpha = .4) +
            scale_x_continuous("Gender", breaks = c(0, 1), labels = c("Female", "Male")) +
            geom_smooth(data = x, aes(x = gender_numeric, y = z), method = lm) +
            geom_point(data = x_avg, aes(x = gender_numeric, y = mean_pts, color = "Red"), size = 4) +
            scale_color_discrete("Average") +
            theme_classic() +
            labs(
                title = "Best fit line added to illustrate difference in speaker points average",
                y = "Points (1 High Low, Standardized)"
            )
        
        x_averages <- ggplot(data = x_avg, aes(x = gender_numeric, y = mean_pts), size = 4) + 
            geom_point() +
            scale_x_continuous("Gender", breaks = c(0, 1), labels = c("Female", "Male")) +
            scale_color_discrete("Average") +
            geom_smooth(data = x, aes(x = gender_numeric, y = z), method = lm) +
            theme_classic() +
            labs(
                title = "Close up of best fit line illustrates difference in average speaker points",
                y = "Points (1 High Low, Standardized)"
            )
        
        if(input$explore_plot_type == "Density") {
            print(x_distrib)
        }
        
        if(input$explore_plot_type == "Point") {
            print(x_point)
        }
        
        if(input$explore_plot_type == "Averages") {
            print(x_averages)
        }
    })
    
    output$explore_lm <- renderText ({
        
        x <- ld_data %>%
            filter(season %in% input$explore_szn) %>%
            filter(tourn %in% input$explore_tourney) 
        
        x_model <- lm(data = x, z ~ gender) %>%
            tidy()
        
        avg <- x_model %>%
            slice(2) %>%
            pull(estimate) %>%
            round(3)
        
        p_value <- x_model %>%
            slice(2) %>%
            pull(p.value) %>%
            round(10)
        
        paste0("The average difference between speaker points for men and women is ", avg, ", which has a p-value of ", p_value, ".")
        
    })
    
    output$explore_boot <- renderText ({
        
        if(input$display_boot_plot == TRUE) {
            
        
        x <- ld_data %>%
            filter(season %in% input$explore_szn) %>%
            filter(tourn %in% input$explore_tourney)
        
        x_number <- x %>%
            summarize(n = n()) %>%
            pull(n)
        
        bootstrap <- rep_sample_n(tbl = x, size = x_number, replace = TRUE, reps = input$rep_size) %>%
            group_by(replicate) %>%
            nest() %>%
            mutate(mod = map(data, ~ lm(data = ., z ~ gender))) %>%
            mutate(reg_results = map(mod, ~tidy(.))) %>%
            mutate(estimate = map_dbl(reg_results, ~ slice(., 2) %>% pull(estimate) %>% round(4))) %>%
            mutate(p_value = map_dbl(reg_results, ~ slice(., 2) %>% pull(p.value) %>% round(10))) %>%
            ungroup() %>%
            select(replicate, estimate, p_value) 
        
        bootstrap_mean <- bootstrap %>%
            summarize(mean(estimate)) %>%
            round(4)
        
        paste0("Bootstrapping the data (size = ", x_number, ", replications = ", input$rep_size, 
               ") produced an average difference of ", bootstrap_mean, ".")
        }
    })
    
    output$bootstrap_plot <- renderPlot ({
        
        if(input$display_boot_plot == TRUE) {
            
        x <- ld_data %>%
            filter(season %in% input$explore_szn) %>%
            filter(tourn %in% input$explore_tourney) 
        
        x_number <- x %>%
            summarize(n = n()) %>%
            pull(n)
        
        x_model <- lm(data = x, z ~ gender) %>%
            tidy()
        
        avg <- x_model %>%
            slice(2) %>%
            pull(estimate) %>%
            round(3)
        
        bootstrap <- rep_sample_n(tbl = x, size = x_number, replace = TRUE, reps = input$rep_size) %>%
            group_by(replicate) %>%
            nest() %>%
            mutate(mod = map(data, ~ lm(data = ., z ~ gender))) %>%
            mutate(reg_results = map(mod, ~tidy(.))) %>%
            mutate(estimate = map_dbl(reg_results, ~ slice(., 2) %>% pull(estimate) %>% round(4))) %>%
            mutate(p_value = map_dbl(reg_results, ~ slice(., 2) %>% pull(p.value) %>% round(10)))
        
        boot_plot <- ggplot(bootstrap, aes(x = replicate, y = estimate)) + geom_point() + 
            geom_hline(yintercept = avg) +
            theme_classic() + 
            labs(
                title = "Bootstrapped Mean Difference Compared to Actual Mean Difference"
            )
        
            print(boot_plot)
        }
        
    })
    
    output$main_plot <- renderPlot ({
        
        x <- ld_data
        
        x_avg <- x %>%
            group_by(gender_numeric) %>%
            summarize(mean_pts = mean(z))
        
        ggplot(data = x, aes(x = z, fill = gender)) + 
            geom_density(alpha = .6) +
            labs(
                title = "Distribution of Speaker Points by Gender",
                x = "Points (1 High Low, Standardized)",
                y = "Density",
                fill = "Gender"
            ) +
            theme_classic()
        
    })
    
    output$main_lm <- renderText ({
        
        x <- ld_data
        
        x_model <- lm(data = x, z ~ gender) %>%
            tidy()
        
        avg <- x_model %>%
            slice(2) %>%
            pull(estimate) %>%
            round(3)
        
        p_value <- x_model %>%
            slice(2) %>%
            pull(p.value) %>%
            round(15)
        
        paste("The regression gives you an average difference of ", avg, " with a p-value of ", p_value, ".")
        
    })
    
    output$main_plot_time <- renderPlot ({
        
        x <- ld_data %>%
            group_by(season, tourn, gender) %>%
            summarize(avg = mean(z))
        
        ggplot(x, aes(x = season, y = avg, color = gender)) +
            geom_point() +
            scale_x_continuous(breaks = c(2018, 2019, 2020)) +
            geom_smooth(aes(x = season, y = avg), method = lm) +
            theme_classic() +
            labs(
                title = "LD Speaker Point Average Over Time", 
                subtitle = "Stratified by Gender",
                color = "Gender",
                x = "Season",
                y = "Average Points"
            )
        
    })
}

# Run the application.

shinyApp(ui = ui, server = server)
