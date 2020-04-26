
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
  "Valley Mid America Cup" = "valley"
)

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
                        starting in the 2017-18 season. This page displays the association of speaker 
                        points with gender and allows you to explore subsets of the data."),
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
# the bootstrap. I added some text to explain the bootstrap.

            p(
              "Here, I've set up a bootstrap* of the data. This will
              give you an estimated average difference between genders, which is
              a robustness check on the data. Bootstrapping the data slows down 
              the app a lot, so it's initially disabled, but feel free to turn it on!"
            ),
            checkboxInput(
              inputId = "display_boot_plot",
              label = "Run the Bootstrap",
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

# Now, I add the main panel, which calls a 
# bunch of plots and results of some regressions
# I'll run later.

          mainPanel(
            plotOutput("explore_plot"),
            verbatimTextOutput("explore_lm"),
            verbatimTextOutput("explore_boot"),
            plotOutput("bootstrap_plot")
          )
        )
      )
    ),

# Add the findings tab. Here, I do most of my
# my analysis in a sidebar and use the main panel
# to display the regressions and graphics.

    tabPanel(
      "Findings",
      titlePanel("Results of Analysis"),
      sidebarLayout(
        sidebarPanel(
          h3("An Initial Regression"),
          p("I ran a basic linear regression on the data, which revealed an average
                      difference of 0.255 standardized speaker points between the two genders.
                      This had a p-value of nearly zero (3.06e-11). This indicates a statistically
                      signficant relationship between the gender and a difference in speaker 
                      point score."),
          h3("A Two-Sample T Test"),
          p("I additionally ran a two-sided t-test which placed the mean difference in 
                      speaker score in a 95% confidence interval between 0.1799355 and 0.3290768. This
                      indicates we ought to reject the null hypothesis that there is no difference in the set of male speaker points
                      and set of female speaker points ought. I then ran a one-sided t-test
                      with an alternative hypothesis of men speaking higher than women. The test, again,
                      rejected the null hypothesis.")
        ),
        
# I call the plots and regressions below.

        mainPanel(
          h3("The Total Distribution"),
          plotOutput("main_plot"),
          h3("Testing the Data"),
          verbatimTextOutput("main_lm"),
          verbatimTextOutput("t_test"),
          verbatimTextOutput("t_test_2")
        )
      )
    ),

# This is the misc. tab, where I will continously
# upload various explorations into the data.

    tabPanel(
      "Misc.",
      titlePanel("Intersting and Random Investigations Into the Data"),
      sidebarLayout(
        sidebarPanel(
          h3("Interesting Findings"),
          p("This is a page where I'll post some interesting graphics using the data.
                      I plan to add some graphs relating to regional success, since the data I 
                      collected gives me debaters' states as well."),
          p("The first graph gives the average speaker points at each tournament
                      and maps it over time. It also divides the averages of each tournament
                      into the average for men and women. I then map a regression line for each 
                      gender. The standard error around the lines is further evidence of the 
                      difference in speaker points between men and women. The divide between the 
                      two genders seems to be increasing! I, however, would not 
                      read too much into the change of speaker points over time, due to the small
                      sample of three seasons."),
        ),
        mainPanel(
          h3("Speaker Point Difference Over Time"),
          plotOutput("main_plot_time")
        )
      )
    ),

# Add the about page.

# I used ahref to add links. I do a lot of explanation
# about sourcing the data here. 

    tabPanel(
      "About",
      titlePanel("Information Page"),
      mainPanel(
        h3("The Data"),
        p("The purpose of this project was to investigate the relationship between gender
              and the amount of 'speaker points' a debater recieves. In competitive debate, in addition to
              wins and losses, judges assign speaker points to individual debaters in order to have some
              scale by which to compare every debater at the tournament and break ties in terms of seeding
              for elimination brackets. Each debater gets speaker points somehwere between 20 and 30 in every 
              preliminary debate (there are usually six or seven preliminary debates). At most tournaments, speaker 
              awards are given to the debaters at the top of the speaker point charts. Seaker awards are 
              calculated by dropping the highest and lowest points for each debater and then adding up 
              the remaining four points. That's called 'one high low.'"),
        p("There are a few important aspects of the data set to clarify."),
        p("First, this data set contains every octafinals bid tournament
              in national circuit Lincoln-Douglas debate. This refers to the most prestigious regular season tournaments, which qualify
              their top sixteen debaters to the year-end national tournament, the Tournament of Champions. The tournaments 
              that are missing to due incomplete tabulation on their part are Bronx (2018), Cal (2019, 2020), 
              Greenhill (2018), Harvard (2018, 2019), Harvard-Westlake (2018), and St. Mark's (2018)."),
        p("Second, due to differences in the preliminary rounds at each tournament, the one-high-low scores of 
              each observation have been standardized within their tournament. The analysis in this project uses
              a z-score assigned to each observation based on their speaker points at that tournament."),
        p("Third, this data set stretches back to the 2017-2018 season but does not go futher because the rate at 
              which tournaments uploaded their full speaker data before that is low. This data set is an extremely comphrehensive 
              sample of the speaker points data that is currently available to access."),
        h3("Sources"),
        p("I sourced all the tournament results from", tags$a(href = "https://www.tabroom.com/index/index.mhtml", "Tabroom,"), "an online results 
              tabulation software used by major national tournaments."),
        p("The gender approximation package can be found", tags$a(href = "https://github.com/ropensci/gender", "here.")),
        p("The source code for the data, the raw data, and the function I created to scrape results from 
              tabroom can be found", tags$a(href = "https://github.com/ishanbhatt42/ldgendernorms", "here.")),
        h3("About Me"),
        p("I'm a freshman majoring in Government and Economics and because 
              I'm interested in the social sciences, I'm doing a little data science 
              too. This project was born out of my love of competitive debate and its community."),
        p("You can contact me at ishanbhatt@college.harvard.edu."),
        p("Cheers,"),
        p("Ishan")
      )
    )
  )
)

# Define server logic.

server <- function(input, output) {
    
# Here's the main plot on teh explore the data page.
    
  output$explore_plot <- renderPlot({
      
# I define the data and filter it by the inputs 
# in the sidebar.
      
    x <- ld_data %>%
      filter(season %in% input$explore_szn) %>%
      filter(tourn %in% input$explore_tourney)

# Then, define the averages for both genders.
    
    x_avg <- x %>%
      group_by(gender_numeric) %>%
      summarize(mean_pts = mean(z))

# Define three graphs for each graph type. Add 
# add laels and a nice theme_classic().
    
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

# I add a geom_smooth here to display the relationship
# to show the trend in difference between teh genders.
        
      geom_smooth(data = x, aes(x = gender_numeric, y = z), method = lm) +
        
# I use the averages I defined earlier to add
# two big red dots for each average.
        
      geom_point(data = x_avg, aes(x = gender_numeric, y = mean_pts, color = "Red"), size = 4) +
      scale_color_discrete("Average") +
      theme_classic() +
      labs(
        title = "Best fit line added to illustrate difference in speaker points average",
        y = "Points (1 High Low, Standardized)"
      )

# The averages plot is basically just a zoom in 
# on the line in the above graph. It's meant to illustrate
# the difference in averages more closely.
    
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

# This chooses which plot to display based on input.
    
    if (input$explore_plot_type == "Density") {
      print(x_distrib)
    }

    if (input$explore_plot_type == "Point") {
      print(x_point)
    }

    if (input$explore_plot_type == "Averages") {
      print(x_averages)
    }
  })

# For the lm, start with the same defining of data
# we did for the above.I'll do this a lot because I 
# I want the functions to change their results on 
# people's input. 
  
  output$explore_lm <- renderText({
    x <- ld_data %>%
      filter(season %in% input$explore_szn) %>%
      filter(tourn %in% input$explore_tourney)

    x_model <- lm(data = x, z ~ gender) %>%
      tidy()

# I pull the estimate and the p-value from the regression.
    
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

 # Here's the bootstrap code. It runs a bootstrap based on 
 # the user's input of the data. This requires defining 
 # the number of data points based on the modified data, which 
 # I do in x_number. 
  
 # I initially had put the if() function around the display of
 # the regression, but I choose to put it around the entire set of 
 # code because the regression slowed down the app so much
 # I didn't want it to run at all if not selected.
  
  output$explore_boot <- renderText({
    if (input$display_boot_plot == TRUE) {
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
        mutate(reg_results = map(mod, ~ tidy(.))) %>%
        mutate(estimate = map_dbl(reg_results, ~ slice(., 2) %>%
          pull(estimate) %>%
          round(4))) %>%
        mutate(p_value = map_dbl(reg_results, ~ slice(., 2) %>%
          pull(p.value) %>%
          round(10))) %>%
        ungroup() %>%
        select(replicate, estimate, p_value)

      bootstrap_mean <- bootstrap %>%
        summarize(mean(estimate)) %>%
        round(4)

      paste0(
        "Bootstrapping the data (size = ", x_number, ", replications = ", input$rep_size,
        ") produced an average difference of ", bootstrap_mean, "."
      )
    }
  })

 # I have to run the entire bootstrap again
 # because the objects are only defined within
 # each output.
  
  output$bootstrap_plot <- renderPlot({
    if (input$display_boot_plot == TRUE) {
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
        mutate(reg_results = map(mod, ~ tidy(.))) %>%
        mutate(estimate = map_dbl(reg_results, ~ slice(., 2) %>%
          pull(estimate) %>%
          round(4))) %>%
        mutate(p_value = map_dbl(reg_results, ~ slice(., 2) %>%
          pull(p.value) %>%
          round(10)))

      boot_plot <- ggplot(bootstrap, aes(x = replicate, y = estimate)) +
        geom_point() +
        geom_hline(yintercept = avg) +
        theme_classic() +
        labs(
          title = "Bootstrapped Mean Difference Compared to Actual Mean Difference"
        )

      print(boot_plot)
      
    }
  })

# Here's just the density plot with all the data,
# not modified by the user.
  
  output$main_plot <- renderPlot({
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

 # Same regression, but for all the data with
 # with no user input.
  
  output$main_lm <- renderText({
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

 # I run a t-test on the data and pull
 # the high and low confidence intervals.
  
 # In order to run a t-test, I had to 
 # define two separate data sets - one for
 # men and one for women.
  
  output$t_test <- renderText({
    dudes <- ld_data %>%
      filter(gender == "male")

    girls <- ld_data %>%
      filter(gender == "female")

    two_sided_low <- t.test(dudes$z, girls$z, alternative = c("two.sided")) %>%
      tidy() %>%
      pull(conf.low) %>%
      round(4)

    two_sided_high <- t.test(dudes$z, girls$z, alternative = c("two.sided")) %>%
      tidy() %>%
      pull(conf.high) %>%
      round(4)

    paste0("The two-sided t-test gave a confidence interval of (", two_sided_low, ", ", two_sided_high, ").")
  })

# Same deal but with a one-sided
# t test.
  
  output$t_test_2 <- renderText({
    dudes <- ld_data %>%
      filter(gender == "male")

    girls <- ld_data %>%
      filter(gender == "female")

    one_sided_low <- t.test(dudes$z, girls$z, alternative = c("greater")) %>%
      tidy() %>%
      pull(conf.low) %>%
      round(4)

    one_sided_high <- t.test(dudes$z, girls$z, alternative = c("greater")) %>%
      tidy() %>%
      pull(conf.high)

    paste0("The one-sided t-test gave a confidence interval of (", one_sided_low, ", ", one_sided_high, ").")
  })

# I define a plot of the average of each tournaments
# points for men and women over time. Add a geom_smooth()
# line for each gender.
  
  output$main_plot_time <- renderPlot({
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
