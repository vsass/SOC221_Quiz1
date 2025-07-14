# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages
library(surveydown)
library(tidyverse)
library(scales)
library(rsconnect)
library(ggtext)
library(shiny)


# helpful post for emojis in ggplot2: https://emilhvitfeldt.com/post/2020-01-02-real-emojis-in-ggplot2/
link_to_img <- function(x, alt, size = 30) {
  paste0("<img src='", x, "' width='", size, "' alt='", alt, "'/>")
}

# define named vector of html image and color for confidence ggplots
conf_emojis <- tibble(emojis = c("confident", "unsure", "lost")) |>
  mutate(url = c("https://emojiisland.com/cdn/shop/products/Emoji_Icon_-_Sunglasses_cool_emoji_large.png",
                 "https://emojiisland.com/cdn/shop/products/Thinking_Face_Emoji_large.png",
                 "https://emojiisland.com/cdn/shop/products/Sweat_Emoji_Icon_2_large.png"),
         label = c(link_to_img(url[1], alt = "Confident"), link_to_img(url[2], alt = "Unsure"), link_to_img(url[3], alt = "Lost")),
         color = c("#cb2f43", "#009465", "#59256a")) |> # directly set color
  select(-url, -emojis) |> # remove intermediary steps
  deframe() # turns remaining 2 cols into named vector for ease of plotting same colors to same emoji html values

## Define function for all mc questions - COULDN'T GET TO WORK, NEEDED TO MOVE ON, TRY AGAIN LATER?
# generate_mc_heatmap <- function(number, question_text, choices = list(a = NULL, b = NULL, c = NULL, d = NULL, e = NULL), correct_answer = NULL) {
#
#   renderPlot({
#
#     # Dynamic variable names based on number
#     mc_var <- sym(paste0("mc", number))
#     conf_var <- sym(paste0("confidenceB", number))
#     input_mc <- input[[paste0("mc", number)]]
#     input_conf <- input[[paste0("confidenceB", number)]]
#
#     # Drop NULL choices and wrap correct one in HTML
#     clean_choices <- choices |>
#       discard(is.null) |>
#       imap_chr(~ if (.y == correct_answer) {
#         glue::glue("<b style='color: #d96629;'>{.x}</b>")
#       } else {
#         .x
#       })
#
#     # Build plot data
#     plot_data <- data() |>
#       filter(!is.na(!!mc_var), !is.na(!!conf_var)) |>
#       count(!!mc_var, !!conf_var) |>
#       rename(Count = n) |>
#       mutate(
#         emoji = case_when(
#           !!conf_var == "confident" ~ names(conf_emojis)[1],
#           !!conf_var == "unsure" ~ names(conf_emojis)[2],
#           !!conf_var == "lost" ~ names(conf_emojis)[3],
#           TRUE ~ NA_character_
#         ) |> fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
#
#         mc_question = case_when(
#           !!!imap(clean_choices, ~ expr(!!mc_var == !!.y ~ !!.x)),
#           TRUE ~ NA_character_
#         ) |> fct(levels = unname(clean_choices))
#       )
#
#     # Create label data for respondent's answer
#     label_data <- plot_data |>
#       mutate(
#         y = as.numeric(emoji),
#         x = as.numeric(mc_question)
#       ) |>
#       filter(!!mc_var == input_mc, !!conf_var == input_conf)
#
#     # Plot
#     plot_data |>
#       ggplot() +
#       geom_tile(aes(mc_question, emoji, fill = Count)) +
#       geom_richtext(
#         data = label_data,
#         label = "Your<br>answer",
#         aes(x = x, y = y),
#         color = "white", fill = NA, size = 8
#       ) +
#       scale_fill_gradient(
#         low = "#d1b0a7", high = "#4e1d4c",
#         breaks = function(x) floor(min(x)):ceiling(max(x))
#       ) +
#       scale_x_discrete(drop = FALSE) +
#       scale_y_discrete(drop = FALSE) +
#       theme_minimal(base_size = 20) +
#       theme(
#         axis.text.x = element_markdown(),
#         axis.text.y = element_markdown(),
#         legend.position = "bottom"
#       ) +
#       labs(
#         x = "Multiple choice selections", y = "",
#         title = str_wrap(question_text, width = 65)
#       )
#   })
# }

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect(env_file = ".env", ignore = FALSE)

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {



  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_forward(
    input$skip_to_page == "end" ~ "end"
  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(

    # Part A questions
    input$n != "" ~ "confidenceAa",
    input$four_hours_less != "" ~ "confidenceAb",
    input$six_hours_less != "" ~ "confidenceAc",
    input$four_hours_more != "" ~ "confidenceAd",
    input$mean != "" ~ "confidenceAe1",
    input$median != "" ~ "confidenceAe2",
    input$mode != "" ~ "confidenceAe3",
    input$distribution_shape != "" ~ "confidenceAf",
    input$central_tendency != "" ~ "confidenceAg",
    input$range != "" ~ "confidenceAh1",
    input$sd != "" ~ "confidenceAh2",
    input$comparison != "" ~ "confidenceAi",

    # Part B questions
    input$mc1 != "" ~ "confidenceB1",
    input$mc2 != "" ~ "confidenceB2",
    input$mc3 != "" ~ "confidenceB3",
    input$mc4 != "" ~ "confidenceB4",
    input$mc5 != "" ~ "confidenceB5",
    input$mc6 != "" ~ "confidenceB6",
    input$mc7 != "" ~ "confidenceB7",
    input$mc8 != "" ~ "confidenceB8",
    input$mc9 != "" ~ "confidenceB9",
    input$mc10 != "" ~ "confidenceB10",
    input$mc11 != "" ~ "confidenceB11",
    input$mc12 != "" ~ "confidenceB12",
    input$mc13 != "" ~ "confidenceB13",
    input$mc14 != "" ~ "confidenceB14",
    input$mc15 != "" ~ "confidenceB15",
    input$mc16 != "" ~ "confidenceB16",
    input$mc17 != "" ~ "confidenceB17",
    input$mc18 != "" ~ "confidenceB18",
    input$mc19 != "" ~ "confidenceB19",
    input$mc20 != "" ~ "confidenceB20"
  )

  # Refresh data every 5 seconds
  data <- sd_get_data(db, refresh_interval = 15)

  # Render plots
  output$Aa_dist_plot <- renderPlot({
    data() |>
      mutate(n = as.numeric(n),
             colors = if_else(n == 202, "correct", "incorrect")) |>
      filter(!is.na(n)) |>
      ggplot() +
      geom_histogram(aes(x = n, fill = colors), binwidth = 1, boundary = 0.5, show.legend = FALSE) +
      geom_vline(xintercept = as.numeric(input$n), color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = c("incorrect" = "#0093a5", "correct" = "#d96629")) +
      theme_minimal(base_size = 20) +
      labs(x = "Answer", y = "Frequency of Response", title = "Distribution of Answers for Part A.a")
  })

  output$Aa_confidence_plot <- renderPlot({
    data() |>
    #data |>
      mutate(emoji = case_when(
        confidenceAa == "confident" ~ names(conf_emojis)[1],
        confidenceAa == "unsure" ~ names(conf_emojis)[2],
        confidenceAa == "lost" ~ names(conf_emojis)[3],
        TRUE ~ NA_character_
      ) |> fct(levels = c(names(conf_emojis)[1], names(conf_emojis)[2], names(conf_emojis)[3]))) |>
      filter(!is.na(emoji)) |>
      ggplot() +
      geom_rect(data = . %>% mutate(x = as.numeric(emoji)) %>% filter(n == input$n & confidenceAa == input$confidenceAa),
                aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
                    color = "black", fill = NA, linetype = "dashed") +
      geom_bar(aes(x = emoji, fill = emoji), show.legend = FALSE) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = conf_emojis) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown()) +
      labs(x = "", y = "Frequency of Response", title = "Distribution of Confidence Levels for Part A.a")
  })

  output$Ab_dist_plot <- renderPlot({
    data() |>
      mutate(four_hours_less = as.numeric(four_hours_less),
             colors = if_else(four_hours_less == 182, "correct", "incorrect")) |>
      filter(!is.na(four_hours_less)) |>
      ggplot() +
      geom_histogram(aes(x = four_hours_less, fill = colors), binwidth = 1, boundary = 0.5, show.legend = FALSE) +
      geom_vline(xintercept = as.numeric(input$four_hours_less), color = "black", linetype = "dashed") +
      scale_x_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = c("incorrect" = "#0093a5", "correct" = "#d96629")) +
      theme_minimal(base_size = 20) +
      labs(x = "Answer", y = "Frequency of Response", title = "Distribution of Answers for Part A.b")
  })

  output$Ab_confidence_plot <- renderPlot({
    data() |>
      mutate(emoji = case_when(
        confidenceAb == "confident" ~ names(conf_emojis)[1],
        confidenceAb == "unsure" ~ names(conf_emojis)[2],
        confidenceAb == "lost" ~ names(conf_emojis)[3],
        TRUE ~ NA_character_
      ) |> fct(levels = c(names(conf_emojis)[1], names(conf_emojis)[2], names(conf_emojis)[3]))) |>
      filter(!is.na(emoji)) |>
      ggplot() +
      geom_bar(aes(x = emoji, fill = emoji), show.legend = FALSE) +
      geom_rect(data = . %>% mutate(x = as.numeric(emoji)) %>% filter(four_hours_less== input$four_hours_less & confidenceAb == input$confidenceAb),
                aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
                color = "black", fill = NA, linetype = "dashed") +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = conf_emojis) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown()) +
      labs(x = "", y = "Frequency of Response", title = "Distribution of Confidence Levels for Part A.b")
  })

  output$Ac_dist_plot <- renderPlot({
    data() |>
      mutate(six_hours_less = as.numeric(six_hours_less),
             colors = if_else(six_hours_less >= 99 & six_hours_less < 99.02, "correct", "incorrect")) |>
      filter(!is.na(six_hours_less)) |>
      ggplot() +
      geom_histogram(aes(x = six_hours_less, fill = colors), show.legend = FALSE, binwidth = 0.1) +
      geom_vline(xintercept = as.numeric(input$six_hours_less), color = "black", linetype = "dashed") +
      annotate(geom = "label", label = "99.01", x = 99.01, y = -0.1, color = "#d96629") +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_continuous(labels = scales::label_percent(scale = 1)) +
      scale_fill_manual(values = c("incorrect" = "#0093a5", "correct" = "#d96629")) +
      theme_minimal(base_size = 20) +
      labs(x = "Answer", y = "Frequency of Response", title = "Distribution of Answers for Part A.c")
  })

  output$Ac_confidence_plot <- renderPlot({
    data() |>
      mutate(emoji = case_when(
        confidenceAc == "confident" ~ names(conf_emojis)[1],
        confidenceAc == "unsure" ~ names(conf_emojis)[2],
        confidenceAc == "lost" ~ names(conf_emojis)[3],
        TRUE ~ NA_character_
      ) |> fct(levels = c(names(conf_emojis)[1], names(conf_emojis)[2], names(conf_emojis)[3]))) |>
      filter(!is.na(emoji)) |>
      ggplot() +
      geom_bar(aes(x = emoji, fill = emoji), show.legend = FALSE) +
      geom_rect(data = . %>% mutate(x = as.numeric(emoji)) %>% filter(six_hours_less== input$six_hours_less & confidenceAc == input$confidenceAc),
                aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
                color = "black", fill = NA, linetype = "dashed") +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = conf_emojis) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown()) +
      labs(x = "", y = "Frequency of Response", title = "Distribution of Confidence Levels for Part A.c")
  })

  output$Ad_dist_plot <- renderPlot({
    data() |>
      mutate(four_hours_more = as.numeric(four_hours_more),
             colors = if_else(four_hours_more >= 35.1 & four_hours_more < 35.2, "correct", "incorrect")) |>
      filter(!is.na(four_hours_more)) |>
      ggplot() +
      geom_histogram(aes(x = four_hours_more, fill = colors), show.legend = FALSE, binwidth = 0.1) +
      geom_vline(xintercept = as.numeric(input$four_hours_more), color = "black", linetype = "dashed") +
      annotate(geom = "label", label = "35.15", x = 35.15, y = -0.1, color = "#d96629") +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_continuous(labels = scales::label_percent(scale = 1)) +
      scale_fill_manual(values = c("incorrect" = "#0093a5", "correct" = "#d96629")) +
      theme_minimal(base_size = 20) +
      labs(x = "Answer", y = "Frequency of Response", title = "Distribution of Answers for Part A.d")
  })

  output$Ad_confidence_plot <- renderPlot({
    data() |>
      mutate(emoji = case_when(
        confidenceAd == "confident" ~ names(conf_emojis)[1],
        confidenceAd == "unsure" ~ names(conf_emojis)[2],
        confidenceAd == "lost" ~ names(conf_emojis)[3],
        TRUE ~ NA_character_
      ) |> fct(levels = c(names(conf_emojis)[1], names(conf_emojis)[2], names(conf_emojis)[3]))) |>
      filter(!is.na(emoji)) |>
      ggplot() +
      geom_bar(aes(x = emoji, fill = emoji), show.legend = FALSE) +
      geom_rect(data = . %>% mutate(x = as.numeric(emoji)) %>% filter(four_hours_more== input$four_hours_more & confidenceAd == input$confidenceAd),
                aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -Inf, ymax = Inf), inherit.aes = FALSE,
                color = "black", fill = NA, linetype = "dashed") +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = conf_emojis) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown()) +
      labs(x = "", y = "Frequency of Response", title = "Distribution of Confidence Levels for Part A.d")
  })

  output$Ae_dist_plot <- renderPlot({
    data() |>
      mutate(mean = as.numeric(mean),
             median = as.numeric(median),
             mode = as.numeric(mode)) |>
      filter(!is.na(mean), !is.na(median), !is.na(mode)) |>
      pivot_longer(cols = c(mean, median, mode), names_to = "measure", values_to = "answer", names_transform = str_to_title) |>
      mutate(colors = case_when(measure == "Mean" & answer == 3.00 ~ "correct",
                                measure == "Median" & answer == 3 ~ "correct",
                                measure == "Mode" & answer == 3 ~ "correct",
                                TRUE ~ "incorrect")) |>
      ggplot() +
      geom_histogram(aes(x = answer, fill = colors), show.legend = FALSE, binwidth = 0.1) +
      facet_wrap(facets = vars(measure), nrow = 3, scales = "free_y") +
      geom_vline(data = . %>% filter(measure == "Mean"), aes(xintercept = input$mean), color = "black", linetype = "dashed") +
      geom_vline(data = . %>% filter(measure == "Median"), aes(xintercept = input$median), color = "black", linetype = "dashed") +
      geom_vline(data = . %>% filter(measure == "Mode"), aes(xintercept = input$mode), color = "black", linetype = "dashed") +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = c("incorrect" = "#0093a5", "correct" = "#d96629")) +
      theme_minimal(base_size = 20) +
      labs(x = "Answer", y = "Frequency of Response", title = "Distribution of Answers for Part A.e")
  })

  output$Ae_confidence_plot <- renderPlot({
    plot_data <- data() |>
      mutate(
        emoji_mean = case_when(
          confidenceAe1 == "confident" ~ names(conf_emojis)[1],
          confidenceAe1 == "unsure" ~ names(conf_emojis)[2],
          confidenceAe1 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_
        ),
        emoji_median = case_when(
          confidenceAe2 == "confident" ~ names(conf_emojis)[1],
          confidenceAe2 == "unsure" ~ names(conf_emojis)[2],
          confidenceAe2 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_
        ),
        emoji_mode = case_when(
          confidenceAe3 == "confident" ~ names(conf_emojis)[1],
          confidenceAe3 == "unsure" ~ names(conf_emojis)[2],
          confidenceAe3 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_
        )
      ) |>
      rename(
        confidence_mean = confidenceAe1,
        confidence_median = confidenceAe2,
        confidence_mode = confidenceAe3
      ) |>
      filter(!is.na(confidence_mean), !is.na(confidence_median), !is.na(confidence_mode)) |>
      pivot_longer(
        cols = c(mean, median, mode, emoji_mean, emoji_median, emoji_mode, confidence_mean, confidence_median, confidence_mode),
        names_to = c("column_type", "measure"),
        names_pattern = "(emoji|confidence)?_?(mean|median|mode)",
        values_to = "value",
        values_transform = as.character,
        names_transform = list(measure = str_to_title)
      ) |>
      mutate(column_type = case_when(
        is.na(column_type) | column_type == "" ~ "answers",
        column_type == "confidence" ~ "confidence",
        column_type == "emoji" ~ "emoji",
        TRUE ~ NA_character_
      )) |>
      pivot_wider(names_from = column_type, values_from = value) |>
      mutate(answers = as.numeric(answers),
             emoji = factor(emoji, levels = names(conf_emojis)))

    # To plot a rectangle, get x-position of emoji
    rect_data <- plot_data |>
      mutate(x = as.numeric(emoji)) |>
      filter(
        (measure == "Mean" & confidence == input$confidenceAe1) |
          (measure == "Median" & confidence == input$confidenceAe2) |
          (measure == "Mode" & confidence == input$confidenceAe3)
      ) |>
      mutate(
        xmin = x - 0.5,
        xmax = x + 0.5,
        ymin = -Inf,
        ymax = Inf
      )

    ggplot(plot_data) +
      geom_bar(aes(x = emoji, fill = emoji), show.legend = FALSE) +
      facet_wrap(vars(measure), nrow = 3, scales = "free_y", drop = FALSE) +
      scale_fill_manual(values = conf_emojis) +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      geom_rect(
        data = rect_data,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        color = "black",
        fill = NA,
        linetype = "dashed"
      ) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown()) +
      labs(x = "", y = "Frequency of Response", title = "Distribution of Confidence Levels for Part A.e")
    })

## TEXT QUESTIONS f & g here ###

  output$Ah_dist_plot <- renderPlot({
    data() |>
      mutate(range = as.numeric(range),
             sd = as.numeric(sd)) |>
      filter(!is.na(range), !is.na(sd)) |>
      pivot_longer(cols = c(range, sd), names_to = "measure", values_to = "answer", names_transform = str_to_title) |>
      mutate(colors = case_when(measure == "Range" & answer == 12 ~ "correct",
                                measure == "Sd" & answer == 1.65 ~ "correct",
                                TRUE ~ "incorrect")) |>
      ggplot() +
      geom_histogram(aes(x = answer, fill = colors), show.legend = FALSE, binwidth = 0.1) +
      facet_wrap(facets = vars(measure), nrow = 2, scales = "free") + # too diff to have same x axis
      geom_vline(data = . %>% filter(measure == "Range"), aes(xintercept = input$range), color = "black", linetype = "dashed") +
      geom_vline(data = . %>% filter(measure == "Sd"), aes(xintercept = input$sd), color = "black", linetype = "dashed") +
      scale_y_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_continuous(breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_fill_manual(values = c("incorrect" = "#0093a5", "correct" = "#d96629")) +
      theme_minimal(base_size = 20) +
      labs(x = "Answer", y = "Frequency of Response", title = "Distribution of Answers for Part A.h")
  })

  output$Ah_confidence_plot <- renderPlot({
    plot_data <- data() |>
      mutate(
        emoji_range = case_when(
          confidenceAh1 == "confident" ~ names(conf_emojis)[1],
          confidenceAh1 == "unsure" ~ names(conf_emojis)[2],
          confidenceAh1 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_
        ),
        emoji_sd = case_when(
          confidenceAh2 == "confident" ~ names(conf_emojis)[1],
          confidenceAh2 == "unsure" ~ names(conf_emojis)[2],
          confidenceAh2 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_
        )
      ) |>
      rename(
        confidence_range = confidenceAh1,
        confidence_sd = confidenceAh2
      ) |>
      filter(!is.na(confidence_range), !is.na(confidence_sd)) |>
      pivot_longer(
        cols = c(range, sd, emoji_range, emoji_sd, confidence_range, confidence_sd),
        names_to = c("column_type", "measure"),
        names_pattern = "(emoji|confidence)?_?(range|sd)",
        values_to = "value",
        values_transform = as.character,
        names_transform = list(measure = str_to_title)
      ) |>
      mutate(column_type = case_when(
        is.na(column_type) | column_type == "" ~ "answers",
        column_type == "confidence" ~ "confidence",
        column_type == "emoji" ~ "emoji",
        TRUE ~ NA_character_
      )) |>
      pivot_wider(names_from = column_type, values_from = value) |>
      mutate(answers = as.numeric(answers),
             emoji = factor(emoji, levels = names(conf_emojis)))

    # To plot a rectangle, get x-position of emoji
    rect_data <- plot_data |>
      mutate(x = as.numeric(emoji)) |>
      filter(
        (measure == "Range" & confidence == input$confidenceAh1) |
          (measure == "Sd" & confidence == input$confidenceAh2)
      ) |>
      mutate(
        xmin = x - 0.5,
        xmax = x + 0.5,
        ymin = -Inf,
        ymax = Inf
      )

    ggplot(plot_data) +
      geom_bar(aes(x = emoji, fill = emoji), show.legend = FALSE) +
      facet_wrap(vars(measure), nrow = 2, scales = "free_y", drop = FALSE) +
      scale_fill_manual(values = conf_emojis) +
      scale_x_discrete(drop = FALSE) +
      geom_rect(
        data = rect_data,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        color = "black",
        fill = NA,
        linetype = "dashed"
      ) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown()) +
      labs(x = "", y = "Frequency of Response", title = "Distribution of Confidence Levels for Part A.h")
  })

  ## TEXT QUESTION i here ###

  ## MULTIPLE CHOICE

  output$B1_heatmap <- renderPlot({

      plot_data <- #data |>
      data() |>
      filter(!is.na(mc1), !is.na(confidenceB1)) |>
      count(mc1, confidenceB1) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB1 == "confident" ~ names(conf_emojis)[1],
          confidenceB1 == "unsure" ~ names(conf_emojis)[2],
          confidenceB1 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc1 == "a" ~ "nominal",
          mc1 == "b" ~ "**ordinal**",
          mc1 == "c" ~ "inferential",
          mc1 == "d" ~ "interval",
          mc1 == "e" ~ "continuous",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("nominal", "**ordinal**", "inferential", "interval", "continuous")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc1 == input$mc1,
        confidenceB1 == input$confidenceB1
      )

    # create correct answer color
    b <- c("#b6bfc1","#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(colour = b), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("What is the level of measurement for a variable with categories that can be put in order but with no consistent distance between the categories?",
                    width = 65))

  })

  output$B2_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc2), !is.na(confidenceB2)) |>
      count(mc2, confidenceB2) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB2 == "confident" ~ names(conf_emojis)[1],
          confidenceB2 == "unsure" ~ names(conf_emojis)[2],
          confidenceB2 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc2 == "a" ~ "interval and continuous",
          mc2 == "b" ~ "interval and discrete",
          mc2 == "c" ~ "nominal and discrete",
          mc2 == "d" ~ "ordinal and continuous",
          mc2 == "e" ~ "ordinal and nominal",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("interval and continuous", "interval and discrete",
                         "nominal and discrete", "ordinal and continuous", "ordinal and nominal")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc2 == input$mc2,
        confidenceB2 == input$confidenceB2
      )

    # create correct answer color
    a <- c("#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1", "#b6bfc1")
    a2 <- c("bold", "plain", "plain", "plain", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(20)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = a, face = a2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("A variable indicating the number of pounds of garbage produced by a sample of Americans is...",
                            width = 65))

  })

  output$B3_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc3), !is.na(confidenceB3)) |>
      count(mc3, confidenceB3) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB3 == "confident" ~ names(conf_emojis)[1],
          confidenceB3 == "unsure" ~ names(conf_emojis)[2],
          confidenceB3 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc3 == "a" ~ "discrete",
          mc3 == "b" ~ "random",
          mc3 == "c" ~ "undefined",
          mc3 == "d" ~ "**independent**",
          mc3 == "e" ~ "mutually exclusive",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("discrete", "random", "undefined", "**independent**", "mutually exclusive")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc3 == input$mc3,
        confidenceB3 == input$confidenceB3
      )

    # create correct answer color
    d <- c("#b6bfc1", "#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = d), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("If the occurrence of one event or outcome does not affect the chance the other occurs, the events are considered...",
                            width = 65))

  })

  output$B4_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc4), !is.na(confidenceB4)) |>
      count(mc4, confidenceB4) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB4 == "confident" ~ names(conf_emojis)[1],
          confidenceB4 == "unsure" ~ names(conf_emojis)[2],
          confidenceB4 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc4 == "a" ~ "the arithmetic average of the distribution",
          mc4 == "b" ~ "the highest score in the distribution",
          mc4 == "c" ~ "the cumulative frequency of 50",
          mc4 == "d" ~ "the most common score",
          mc4 == "e" ~ "the 50th percentile",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("the arithmetic average of the distribution", "the highest score in the distribution",
                         "the cumulative frequency of 50", "the most common score", "the 50th percentile")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc4 == input$mc4,
        confidenceB4 == input$confidenceB4
      )

    # create correct answer color
    e <- c("#b6bfc1", "#b6bfc1", "#b6bfc1", "#b6bfc1", "#d96629")
    e2 <- c("plain", "plain", "plain", "plain", "bold")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(20)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = e, face = e2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("The median of a distribution is the value at...",
                            width = 65))

  })

  output$B5_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc5), !is.na(confidenceB5)) |>
      count(mc5, confidenceB5) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB5 == "confident" ~ names(conf_emojis)[1],
          confidenceB5 == "unsure" ~ names(conf_emojis)[2],
          confidenceB5 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc5 == "a" ~ "**nominal**",
          mc5 == "b" ~ "ordinal",
          mc5 == "c" ~ "inferential",
          mc5 == "d" ~ "interval",
          mc5 == "e" ~ "all of the above",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("**nominal**", "ordinal", "inferential", "interval", "all of the above")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc5 == input$mc5,
        confidenceB5 == input$confidenceB5
      )

    # create correct answer color
    a <- c("#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = a), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("The cumulative frequency column of a frequency table is inappropriate for variables
                            measured at which of the following levels of measurement.",
                            width = 65))

  })

  output$B6_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc6), !is.na(confidenceB6)) |>
      count(mc6, confidenceB6) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB6 == "confident" ~ names(conf_emojis)[1],
          confidenceB6 == "unsure" ~ names(conf_emojis)[2],
          confidenceB6 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc6 == "a" ~ "range",
          mc6 == "b" ~ "standard deviation",
          mc6 == "c" ~ "**mode**",
          mc6 == "d" ~ "median",
          mc6 == "e" ~ "mean",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("range", "standard deviation", "**mode**", "median", "mean")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc6 == input$mc6,
        confidenceB6 == input$confidenceB6
      )

    # create correct answer color
    c <- c("#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = c), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Which of the following is the best measure for describing the central tendency of a
                            variable measuring the racial identification of a sample of students.",
                            width = 65))

  })

  output$B7_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc7), !is.na(confidenceB7)) |>
      count(mc7, confidenceB7) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB7 == "confident" ~ names(conf_emojis)[1],
          confidenceB7 == "unsure" ~ names(conf_emojis)[2],
          confidenceB7 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc7 == "a" ~ "Random is the same as haphazard or unpredictable.",
          mc7 == "b" ~ "Random phenomena are never actually observed in the real world.",
          mc7 == "c" ~ "With a random phenomenon the outcome of any one trial is known before the trial.",
          mc7 == "d" ~ "With a random phenomenon the outcome for one trial depends on previous outcomes.",
          mc7 == "e" ~ "With a random phenomenon there is a predictable pattern to the outcomes over the long run.",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("Random is the same as haphazard or unpredictable.", "Random phenomena are never actually observed in the real world.",
                         "With a random phenomenon the outcome of any one trial is known before the trial.",
                         "With a random phenomenon the outcome for one trial depends on previous outcomes.",
                         "With a random phenomenon there is a predictable pattern to the outcomes over the long run.")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc7 == input$mc7,
        confidenceB7 == input$confidenceB7
      )

    # create correct answer color
    e <- c("#b6bfc1", "#b6bfc1", "#b6bfc1", "#b6bfc1", "#d96629")
    e2 <- c("plain", "plain", "plain", "plain", "bold")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(15)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = e, face = e2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Which of the following is a TRUE statement about random phenomena?",
                            width = 65))

  })

  output$B8_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc8), !is.na(confidenceB8)) |>
      count(mc8, confidenceB8) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB8 == "confident" ~ names(conf_emojis)[1],
          confidenceB8 == "unsure" ~ names(conf_emojis)[2],
          confidenceB8 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc8 == "a" ~ "The total area under the curve equals 1",
          mc8 == "b" ~ "The height of the curve gives exact probabilities",
          mc8 == "c" ~ "The distribution must be symmetric",
          mc8 == "d" ~ "Values with zero height on the curve have zero probability",
          mc8 == "e" ~ "It can only be used with normally distributed data",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("The total area under the curve equals 1", "The height of the curve gives exact probabilities",
                         "The distribution must be symmetric", "Values with zero height on the curve have zero probability",
                         "It can only be used with normally distributed data")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc8 == input$mc8,
        confidenceB8 == input$confidenceB8
      )

    # create correct answer color
    a <- c("#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1", "#b6bfc1")
    a2 <- c("bold", "plain", "plain", "plain", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(20)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = a, face = a2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Which of the following is true about a probability density function (PDF) for a continuous variable?",
                            width = 60))

  })

  output$B9_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc9), !is.na(confidenceB9)) |>
      count(mc9, confidenceB9) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB9 == "confident" ~ names(conf_emojis)[1],
          confidenceB9 == "unsure" ~ names(conf_emojis)[2],
          confidenceB9 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc9 == "a" ~ "draw conclusions about a population using information taken from a sample",
          mc9 == "b" ~ "summarize the distribution of a variable.",
          mc9 == "c" ~ "determine the representativeness of a non-probability sample.",
          mc9 == "d" ~ "find the proportion of cases under various segments of the normal distribution.",
          mc9 == "e" ~ "All of the above.",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("draw conclusions about a population using information taken from a sample",
                         "summarize the distribution of a variable.",
                         "determine the representativeness of a non-probability sample.",
                         "find the proportion of cases under various segments of the normal distribution.", "All of the above.")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc9 == input$mc9,
        confidenceB9 == input$confidenceB9
      )

    # create correct answer color
    a <- c("#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1", "#b6bfc1")
    a2 <- c("bold", "plain", "plain", "plain", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(15)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = a, face = a2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Inferential statistics are used to...",
                            width = 65))

  })

  output$B10_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc10), !is.na(confidenceB10)) |>
      count(mc10, confidenceB10) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB10 == "confident" ~ names(conf_emojis)[1],
          confidenceB10 == "unsure" ~ names(conf_emojis)[2],
          confidenceB10 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc10 == "a" ~ "standard deviation",
          mc10 == "b" ~ "**mean**",
          mc10 == "c" ~ "median",
          mc10 == "d" ~ "mode",
          mc10 == "e" ~ "range",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("standard deviation", "**mean**", "median", "mode", "range")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc10 == input$mc10,
        confidenceB10 == input$confidenceB10
      )

    # create correct answer color
    b <- c("#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = b), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Which of the following measure of central tendency can be misleading if the distribution of the variable is highly skewed?",
                            width = 65))

  })

  output$B11_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc11), !is.na(confidenceB11)) |>
      count(mc11, confidenceB11) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB11 == "confident" ~ names(conf_emojis)[1],
          confidenceB11 == "unsure" ~ names(conf_emojis)[2],
          confidenceB11 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc11 == "a" ~ "a random set of our respondents refuse to answer a particular survey question.",
          mc11 == "b" ~ "we report too many different measures of central tendency or variability.",
          mc11 == "c" ~ "we rely on data from a sample that does not represent the population of interest.",
          mc11 == "d" ~ "we use a survey instead of in-depth interviews to collect our data.",
          mc11 == "e" ~ "All of the above.",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("a random set of our respondents refuse to answer a particular survey question.",
                         "we report too many different measures of central tendency or variability.",
                         "we rely on data from a sample that does not represent the population of interest.",
                         "we use a survey instead of in-depth interviews to collect our data.", "All of the above.")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc11 == input$mc11,
        confidenceB11 == input$confidenceB11
      )

    # create correct answer color
    c <- c("#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1")
    c2 <- c("plain", "plain", "bold", "plain", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(18)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = c, face = c2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("The results of our statistical analysis is likely to be biased if...",
                            width = 65))

  })

  output$B12_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc12), !is.na(confidenceB12)) |>
      count(mc12, confidenceB12) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB12 == "confident" ~ names(conf_emojis)[1],
          confidenceB12 == "unsure" ~ names(conf_emojis)[2],
          confidenceB12 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc12 == "a" ~ "stratified random sample",
          mc12 == "b" ~ "simple random sample",
          mc12 == "c" ~ "convenience sample",
          mc12 == "d" ~ "snowball sample",
          mc12 == "e" ~ "merit sample",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("stratified random sample", "simple random sample", "convenience sample",
                         "snowball sample", "merit sample")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc12 == input$mc12,
        confidenceB12 == input$confidenceB12
      )

    # create correct answer color
    c <- c("#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1")
    c2 <- c("plain", "plain", "bold", "plain", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(15)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = c, face = c2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Standing in front of Kane Hall to find people willing to complete a survey would provide which kind of sample?",
                            width = 65))

  })

  output$B13_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc13), !is.na(confidenceB13)) |>
      count(mc13, confidenceB13) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB13 == "confident" ~ names(conf_emojis)[1],
          confidenceB13 == "unsure" ~ names(conf_emojis)[2],
          confidenceB13 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc13 == "a" ~ "it measures the distance from the mode, median, and mean simultaneously",
          mc13 == "b" ~ "it is not affected by unusually high or unusually low scores in the distribution",
          mc13 == "c" ~ "it takes into consideration the value of every score in the distribution",
          mc13 == "d" ~ "it is appropriate for all levels of measurement",
          mc13 == "e" ~ "all of the above",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("it measures the distance from the mode, median, and mean simultaneously",
                         "it is not affected by unusually high or unusually low scores in the distribution",
                         "it takes into consideration the value of every score in the distribution",
                         "it is appropriate for all levels of measurement", "all of the above")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc13 == input$mc13,
        confidenceB13 == input$confidenceB13
      )

    # create correct answer color
    c <- c("#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1")
    c2 <- c("plain", "plain", "bold", "plain", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(18)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = c, face = c2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("The standard deviation is considered superior to the range for indicating the variability of a set of scores because...",
                            width = 65))

  })

  output$B14_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc14), !is.na(confidenceB14)) |>
      count(mc14, confidenceB14) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB14 == "confident" ~ names(conf_emojis)[1],
          confidenceB14 == "unsure" ~ names(conf_emojis)[2],
          confidenceB14 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc14 == "a" ~ "standard difference",
          mc14 == "b" ~ "mean difference",
          mc14 == "c" ~ "variance",
          mc14 == "d" ~ "**deviation**",
          mc14 == "e" ~ "mode",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("standard difference", "mean difference", "variance", "**deviation**", "mode")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc14 == input$mc14,
        confidenceB14 == input$confidenceB14
      )

    # create correct answer color
    d <- c("#b6bfc1", "#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = d), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("The difference between a single score in a distribution and the mean of the distribution of scores is referred to as the...",
                            width = 65))

  })

  output$B15_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc15), !is.na(confidenceB15)) |>
      count(mc15, confidenceB15) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB15 == "confident" ~ names(conf_emojis)[1],
          confidenceB15 == "unsure" ~ names(conf_emojis)[2],
          confidenceB15 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc15 == "a" ~ "620",
          mc15 == "b" ~ "**650**",
          mc15 == "c" ~ "710",
          mc15 == "d" ~ "585",
          mc15 == "e" ~ "470",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("620", "**650**", "710", "585", "470")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc15 == input$mc15,
        confidenceB15 == input$confidenceB15
      )

    # create correct answer color
    b <- c("#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = b), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Suppose SAT math scores are normally distributed with a mean of 520 and a standard deviation of 100.
                            What score marks the top 10% of the distribution?",
                            width = 65))

  })

  output$B16_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc16), !is.na(confidenceB16)) |>
      count(mc16, confidenceB16) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB16 == "confident" ~ names(conf_emojis)[1],
          confidenceB16 == "unsure" ~ names(conf_emojis)[2],
          confidenceB16 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc16 == "a" ~ "If you flip a fair coin 5 times and the result is 'heads' each time, the likelihood of flipping  a 'tails' on the next flip is increased.",
          mc16 == "b" ~ "If you flip a fair coin 10 times, you are sure to get 5 'heads' and 5 'tails.'",
          mc16 == "c" ~ "Over thousands of rolls of a fair 6-sided die, about 1/6 of the roles should result in a '3.'",
          mc16 == "d" ~ "All of these statements are true.",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("If you flip a fair coin 5 times and the result is 'heads' each time, the likelihood of flipping  a 'tails' on the next flip is increased.",
                         "If you flip a fair coin 10 times, you are sure to get 5 'heads' and 5 'tails.'",
                         "Over thousands of rolls of a fair 6-sided die, about 1/6 of the roles should result in a '3.'",
                         "All of these statements are true.")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc16 == input$mc16,
        confidenceB16 == input$confidenceB16
      )

    # create correct answer color
    c <- c("#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1")
    c2 <- c("plain", "plain", "bold", "plain", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(18)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = c, face = c2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Which of the following is a true statement?",
                            width = 65))

  })

  output$B17_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc17), !is.na(confidenceB17)) |>
      count(mc17, confidenceB17) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB17 == "confident" ~ names(conf_emojis)[1],
          confidenceB17 == "unsure" ~ names(conf_emojis)[2],
          confidenceB17 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc17 == "a" ~ "1.0",
          mc17 == "b" ~ "0.75",
          mc17 == "c" ~ "0.50",
          mc17 == "d" ~ "**0.25**",
          mc17 == "e" ~ "0",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("1.0", "0.75", "0.50", "**0.25**", "0")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc17 == input$mc17,
        confidenceB17 == input$confidenceB17
      )

    # create correct answer color
    d <- c("#b6bfc1", "#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = d), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("What is the probability of flipping 'heads' on two consecutive flips of a fair coin?",
                            width = 65))

  })

  output$B18_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc18), !is.na(confidenceB18)) |>
      count(mc18, confidenceB18) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB18 == "confident" ~ names(conf_emojis)[1],
          confidenceB18 == "unsure" ~ names(conf_emojis)[2],
          confidenceB18 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc18 == "a" ~ "0.5184",
          mc18 == "b" ~ "**0.4032**",
          mc18 == "c" ~ "0.2016",
          mc18 == "d" ~ "0.144",
          mc18 == "e" ~ "0.28",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("0.5184", "**0.4032**", "0.2016", "0.144", "0.28")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc18 == input$mc18,
        confidenceB18 == input$confidenceB18
      )

    # create correct answer color
    b <- c("#b6bfc1", "#d96629", "#b6bfc1", "#b6bfc1", "#b6bfc1")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = b), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("A researcher finds that the probability a randomly chosen adult uses social media daily is 0.72.
                            What is the probability that in a random sample of two adults, exactly one uses social media daily?",
                            width = 65))

  })

  output$B19_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc19), !is.na(confidenceB19)) |>
      count(mc19, confidenceB19) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB19 == "confident" ~ names(conf_emojis)[1],
          confidenceB19 == "unsure" ~ names(conf_emojis)[2],
          confidenceB19 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc19 == "a" ~ "93.32%",
          mc19 == "b" ~ "43.32%",
          mc19 == "c" ~ "34.13%",
          mc19 == "d" ~ "15.32%",
          mc19 == "e" ~ "**6.68%**",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("93.32%", "43.32%", "34.13%", "15.32%", "**6.68%**")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc19 == input$mc19,
        confidenceB19 == input$confidenceB19
      )

    # create correct answer color
    e <- c("#b6bfc1", "#b6bfc1", "#b6bfc1", "#b6bfc1", "#d96629")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_markdown(color = e), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("Based on the information above, what percentage of men are 76 inches or taller?",
                            width = 65))

  })

  output$B20_heatmap <- renderPlot({

    plot_data <- #data |>
      data() |>
      filter(!is.na(mc20), !is.na(confidenceB20)) |>
      count(mc20, confidenceB20) |>
      rename(Count = n) |>
      mutate(
        emoji = case_when(
          confidenceB20 == "confident" ~ names(conf_emojis)[1],
          confidenceB20 == "unsure" ~ names(conf_emojis)[2],
          confidenceB20 == "lost" ~ names(conf_emojis)[3],
          TRUE ~ NA_character_) |>
          fct(levels = c(names(conf_emojis)[3], names(conf_emojis)[2], names(conf_emojis)[1])),
        mc_question = case_when(
          mc20 == "a" ~ "66.00 and 74.00 inches",
          mc20 == "b" ~ "64.00 and 76.00 inches",
          mc20 == "c" ~ "63.44 and 76.56 inches",
          mc20 == "d" ~ "62.16 and 77.84 inches",
          mc20 == "e" ~ "58.00 and 82.00 inches",
          TRUE ~ NA_character_
        ) |>
          fct(levels = c("66.00 and 74.00 inches", "64.00 and 76.00 inches", "63.44 and 76.56 inches",
                         "62.16 and 77.84 inches", "58.00 and 82.00 inches")))

    # To plot respondent answer, need to create data from input
    label_data <- plot_data |>
      mutate(y = as.numeric(emoji),
             x = as.numeric(mc_question)) |>
      filter(
        mc20 == input$mc20,
        confidenceB20 == input$confidenceB20
      )

    # create correct answer color
    d <- c("#b6bfc1", "#b6bfc1", "#b6bfc1", "#d96629", "#b6bfc1")
    d2 <- c("plain", "plain", "plain", "bold", "plain")

    plot_data |>
      ggplot() +
      geom_tile(aes(mc_question, emoji, fill = Count)) +
      geom_richtext(data = label_data, label = "Your<br>answer", aes(x = x, y = y), color = "white", fill = NA, size = 8) +
      scale_fill_gradient(low = "#d1b0a7", high = "#4e1d4c", breaks = function(x) floor(min(x)):ceiling(max(x))) +
      scale_x_discrete(drop = FALSE, labels = scales::label_wrap(20)) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(color = d, face = d2), axis.text.y = element_markdown(), legend.position = "bottom") +
      labs(x = "Multiple choice selections", y = "",
           title = str_wrap("What two values of height define the middle 95% of men in the distribution?",
                            width = 65))

  })

  # Database designation and other settings
  sd_server(
    start_page = "pre",
    db = db,
    auto_scroll = FALSE,
    rate_survey = TRUE,
    use_cookies = FALSE,
    all_questions_required = TRUE
    )

}

# Launch the app
shiny::shinyApp(ui = ui, server = server)


