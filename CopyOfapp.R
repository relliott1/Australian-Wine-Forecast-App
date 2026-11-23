#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shiny)
library(bslib)
library(fpp3)        # tsibble, fable, feasts, dplyr, tidyr, ggplot2, lubridate
library(readr)
library(purrr)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
# app.R -------------------------------------------------------------


# ---- 1. Data ------------------------------------------------------
load_wines <- function(path = "AustralianWines.csv") {
    raw <- read_csv(path, show_col_types = FALSE)
    names(raw)[1] <- "Date"
    
    raw |>
        mutate(
            Month = yearmonth(parse_date_time(
                as.character(Date),
                orders = c("b-y", "b y", "my", "Y-m", "Y-m-d", "m/d/Y", "m/Y", "d-b-y"),
                quiet = TRUE
            )),
            across(-c(Date, Month), ~ suppressWarnings(parse_number(as.character(.x))))
        ) |>
        select(-Date) |>
        pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
        filter(!is.na(Month)) |>
        as_tsibble(key = Varietal, index = Month) |>
        fill_gaps() |>
        group_by_key() |>
        # linearly interpolate any missing months (e.g., gaps in Rose)
        mutate(Sales = approx(seq_along(Sales), Sales, seq_along(Sales), rule = 2)$y) |>
        ungroup()
}

wines <- load_wines()

# ---- 2. Modelling helper -----------------------------------------
fit_models <- function(data, use_log = FALSE, ensemble = TRUE) {
    fit <- if (use_log) {
        data |> model(
            TSLM  = TSLM(log(Sales) ~ trend() + season()),
            ETS   = ETS(log(Sales)),
            ARIMA = ARIMA(log(Sales))
        )
    } else {
        data |> model(
            TSLM  = TSLM(Sales ~ trend() + season()),
            ETS   = ETS(Sales),
            ARIMA = ARIMA(Sales)
        )
    }
    if (ensemble) fit <- fit |> mutate(Ensemble = (TSLM + ETS + ARIMA) / 3)
    fit
}

get_specs <- function(fit, window) {
    fit |>
        select(-any_of("Ensemble")) |>
        as_tibble() |>
        pivot_longer(-Varietal, names_to = "Model", values_to = "mdl") |>
        mutate(
            Specification = map_chr(mdl, ~ gsub("^<|>$", "", format(.x))),
            Specification = ifelse(Model == "TSLM", "TSLM(trend + season)", Specification),
            Window = window
        ) |>
        select(Varietal, Model, Window, Specification)
}

# ---- 3. UI --------------------------------------------------------
ui <- page_sidebar(
    title = "Australian Wine Sales Forecasting",
    theme = bs_theme(bootswatch = "flatly"),
    sidebar = sidebar(
        width = 340,
        selectInput("varietals", "Varietal(s)",
                    choices = sort(unique(wines$Varietal)),
                    selected = "Fortified", multiple = TRUE),
        dateRangeInput("dates", "Date range",
                       start = min(wines$Month) |> as.Date(),
                       end   = max(wines$Month) |> as.Date(),
                       min   = min(wines$Month) |> as.Date(),
                       max   = max(wines$Month) |> as.Date(),
                       format = "M yyyy", startview = "year"),
        hr(),
        numericInput("n_val", "Validation window (last N months)", 12, min = 6, max = 36),
        numericInput("h", "Forecast horizon beyond data (months)", 12, min = 1, max = 60),
        sliderInput("level", "Prediction interval (%)", min = 50, max = 99, value = 95),
        hr(),
        checkboxInput("log", "Log-transform sales (stabilizes seasonal variance)", FALSE),
        checkboxInput("ensemble", "Add Ensemble (average of TSLM, ETS, ARIMA)", TRUE),
        actionButton("run", "Fit models", class = "btn-primary w-100")
    ),
    navset_card_tab(
        nav_panel("Overview",
                  layout_columns(
                      col_widths = c(6, 6),
                      checkboxInput("show_trend", "Show trend lines", TRUE),
                      checkboxInput("show_total", "Show total of selected varietals", TRUE)
                  ),
                  plotOutput("ts_plot", height = 450),
                  h5("Totals for the selected date range"),
                  DTOutput("totals_tbl")
        ),
        nav_panel("Model specs",
                  h5("Selected specifications"),
                  DTOutput("specs_tbl")
        ),
        nav_panel("Accuracy",
                  h5("Best model per varietal (lowest validation RMSE)"),
                  DTOutput("best_tbl"),
                  h5("All models: training vs. validation"),
                  DTOutput("acc_tbl")
        ),
        nav_panel("Validation forecast",
                  plotOutput("val_plot", height = 550)
        ),
        nav_panel("Future forecast",
                  plotOutput("fut_plot", height = 550),
                  downloadButton("dl", "Download forecasts (CSV)")
        )
    )
)

# ---- 4. Server ----------------------------------------------------
server <- function(input, output, session) {
    
    filtered <- reactive({
        req(input$varietals, input$dates)
        wines |>
            filter(Varietal %in% input$varietals,
                   Month >= yearmonth(input$dates[1]),
                   Month <= yearmonth(input$dates[2]))
    })
    
    # --- Overview plots ---
    # --- Overview: selected varietals + trend lines + total ---
    overview_data <- reactive({
        dat <- filtered() |> as_tibble() |> mutate(Month = as.Date(Month))
        
        if (input$show_total) {
            tot <- dat |>
                group_by(Month) |>
                summarise(Sales = sum(Sales), .groups = "drop") |>
                mutate(Varietal = "Total")
            dat <- bind_rows(dat, tot)
        }
        dat
    })
    
    output$ts_plot <- renderPlot({
        dat   <- overview_data()
        vars  <- setdiff(unique(dat$Varietal), "Total")
        cols  <- setNames(scales::hue_pal()(length(vars)), vars)
        if (input$show_total) cols <- c(cols, Total = "black")
        
        p <- ggplot(dat, aes(Month, Sales, color = Varietal)) +
            geom_line(alpha = 0.6, linewidth = 0.5) +
            scale_color_manual(values = cols) +
            labs(title = "Monthly sales of selected varietals",
                 y = "Thousands of litres", x = NULL, color = NULL) +
            theme(legend.position = "bottom")
        
        if (input$show_trend) {
            p <- p + geom_smooth(method = "loess", span = 0.5, se = FALSE,
                                 linewidth = 1.2, formula = y ~ x)
        }
        p
    })
    
    output$totals_tbl <- renderDT({
        dat <- filtered() |> as_tibble()
        tbl <- dat |>
            group_by(Varietal) |>
            summarise(`Total sales` = sum(Sales),
                      `Avg monthly` = mean(Sales),
                      .groups = "drop") |>
            mutate(`Share of total (%)` = 100 * `Total sales` / sum(`Total sales`)) |>
            arrange(desc(`Total sales`))
        
        tbl <- bind_rows(
            tbl,
            tibble(Varietal = "All selected",
                   `Total sales` = sum(dat$Sales),
                   `Avg monthly` = sum(dat$Sales) / n_distinct(dat$Month),
                   `Share of total (%)` = 100)
        )
        
        datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
            formatRound(c("Total sales", "Avg monthly", "Share of total (%)"), 1)
    })
    
    output$stl_plot <- renderPlot({
        req(input$decomp_var)
        validate(need(nrow(filtered() |> filter(Varietal == input$decomp_var)) >= 24,
                      "Need at least 24 months for STL."))
        filtered() |> filter(Varietal == input$decomp_var) |>
            model(STL(Sales ~ season(window = "periodic"))) |>
            components() |> autoplot() + labs(title = "STL decomposition")
    })
    
    # --- Model fitting (only when button is pressed) ---
    results <- eventReactive(input$run, {
        dat   <- filtered()
        n_val <- input$n_val
        n_min <- dat |> as_tibble() |> count(Varietal) |> pull(n) |> min()
        validate(need(n_min >= n_val + 36,
                      "Not enough data: need at least 36 training months plus the validation window."))
        
        cutoff <- max(dat$Month) - n_val
        train  <- filter(dat, Month <= cutoff)
        
        withProgress(message = "Fitting models...", value = 0, {
            fit_tr  <- fit_models(train, input$log, input$ensemble)
            incProgress(0.4, detail = "Training fit done")
            fc_val  <- forecast(fit_tr, h = n_val)
            fit_all <- fit_models(dat, input$log, input$ensemble)
            incProgress(0.4, detail = "Full-data refit done")
            fc_fut  <- forecast(fit_all, h = input$h)
            incProgress(0.2)
        })
        
        acc <- bind_rows(
            accuracy(fit_tr) |> mutate(Set = "Training"),
            accuracy(fc_val, dat) |> mutate(Set = "Validation")
        ) |>
            select(Varietal, Model = .model, Set, RMSE, MAE, MAPE)
        
        list(dat = dat, cutoff = cutoff, fc_val = fc_val, fc_fut = fc_fut, acc = acc,
             specs = bind_rows(get_specs(fit_tr,  "Training window"),
                               get_specs(fit_all, "Full data (final)")))
    }, ignoreNULL = FALSE)
    
    # --- Tables ---
    output$specs_tbl <- renderDT({
        datatable(results()$specs, rownames = FALSE, options = list(pageLength = 20))
    })
    
    output$acc_tbl <- renderDT({
        results()$acc |>
            arrange(Varietal, desc(Set), RMSE) |>
            datatable(rownames = FALSE, filter = "top", options = list(pageLength = 20)) |>
            formatRound(c("RMSE", "MAE", "MAPE"), 2)
    })
    
    output$best_tbl <- renderDT({
        results()$acc |>
            filter(Set == "Validation") |>
            group_by(Varietal) |> slice_min(RMSE, n = 1) |> ungroup() |>
            datatable(rownames = FALSE, options = list(dom = "t")) |>
            formatRound(c("RMSE", "MAE", "MAPE"), 2)
    })
    
    # --- Forecast plots ---
    output$val_plot <- renderPlot({
        r <- results()
        autoplot(r$fc_val, r$dat |> filter(Month >= r$cutoff - 36), level = input$level) +
            geom_vline(xintercept = as.Date(r$cutoff), linetype = "dashed") +
            labs(title = "Validation-window forecasts vs. actuals",
                 subtitle = "Dashed line = train/validation split", y = "Thousands of litres")
    })
    
    output$fut_plot <- renderPlot({
        r <- results()
        autoplot(r$fc_fut, r$dat |> filter(Month >= max(Month) - 60), level = input$level) +
            labs(title = "Forecasts from models refit on all selected data",
                 y = "Thousands of litres")
    })
    
    output$dl <- downloadHandler(
        filename = function() paste0("wine_forecasts_", Sys.Date(), ".csv"),
        content = function(file) {
            results()$fc_fut |>
                hilo(level = c(80, 95)) |>
                unpack_hilo(all_of(c("80%", "95%"))) |>
                as_tibble() |>
                select(-where(distributional::is_distribution)) |>
                write_csv(file)
        }
    )
}

shinyApp(ui, server)
