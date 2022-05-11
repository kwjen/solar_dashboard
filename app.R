library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(reticulate)

reticulate::install_miniconda()
# for local deployment
# reticulate::use_condaenv('solar_dashboard', required = TRUE)

solar_df = readr::read_csv("./data/data_files/solar_df.csv")
solar_df$solar_lat = as.numeric(solar_df$solar_lat)
solar_df$solar_lng = as.numeric(solar_df$solar_lng)
solar_df$kwh = as.numeric(solar_df$kwh)
solar_df$date = as.Date(solar_df$date)
solar_df$radius = solar_df$kwh*5

solar_df_grouped = readr::read_csv("./data/data_files/solar_df_grouped.csv")

conda_install("r-reticulate", "scikit-learn")
conda_install("r-reticulate", "matplotlib")
conda_install("r-reticulate", "numpy")
conda_install("r-reticulate", "pandas")
ensemble = reticulate::import("sklearn.ensemble")
tree = import("sklearn.tree")
ms = import("sklearn.model_selection")
metrics = import("sklearn.metrics")
random_forest = ensemble['RandomForestClassifier']
gradient_boosting_classifier = ensemble['GradientBoostingClassifier']
decision_tree = tree['DecisionTreeClassifier']
split = ms['train_test_split']
cm = metrics['confusion_matrix']
cm_plot = metrics['ConfusionMatrixDisplay']
plt = import("matplotlib.pyplot")
np = import("numpy")
pd = import("pandas")

ui = fluidPage(
  titlePanel("Solar Stuff"),
  tabsetPanel(
    tabPanel("Map", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "name",
                             label = "Location",
                             choices = c("All", unique(solar_df$solar_names))),
                 sliderInput("date", "Date",
                             min = min(solar_df$date),
                             max = max(solar_df$date),
                             value = min(solar_df$date),
                             step = 7,
                             timeFormat = "%d-%m-%Y",
                             animate = animationOptions(interval = 400, loop = FALSE))
               ),
               mainPanel(
                 leafletOutput(outputId = "solar_map")
               )
             )
    ),
    tabPanel("ML", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "algorithm",
                             label = "Algorithm",
                             choices = c("decision_tree", "random_forest",
                                         "gradient_boosting_classifier"))
               ),
               mainPanel(
                 plotlyOutput(outputId = "grouped_plot"),
                 plotOutput(outputId = "confusion_matrix")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  solar_filtered = reactive({
    if (input$name == "All") {
      solar_df[solar_df$date == input$date,] %>% 
        group_by(solar_names) %>%  
        summarise_all(last)
    } else {
      solar_df[solar_df$solar_names == input$name & solar_df$date == input$date,] %>%  
        summarise_all(last)
    }
  })
  
  output$solar_map <- renderLeaflet({
    
    leaflet(data = solar_filtered()) %>%
      addTiles() %>%
      setView(lat = "51.04455977703975", 
              lng = "-114.04804724550243",
              zoom = 10) %>%
      addCircleMarkers(lat = ~solar_lat,
                       lng = ~solar_lng,
                       label = ~solar_names,
                       radius = ~radius)
  })
  
  output$grouped_plot = renderPlotly({
    fig = plot_ly(data = solar_df_grouped,
                  x = ~week,
                  y = ~max_kwh,
                  color = ~solar_names,
                  type = "scatter",
                  mode = "markers") %>% 
      layout(legend = list(x = -5, y = 1))
  })
  
  output$confusion_matrix = renderPlot({
    clf = get(input$algorithm)()
    
    X = pd$DataFrame(solar_df_grouped$max_kwh)
    y = pd$DataFrame(solar_df_grouped$solar_names)
    
    dfs = split(X, y, test_size=0.3)
    
    for (i in length(dfs)) {
      dfs[i] = pd$DataFrame(dfs[i][[1]])
    }
    
    y_test = as.factor(dfs[4][[1]])
    y_train = as.factor(dfs[3][[1]]$`0`)
    X_test = dfs[2][[1]]
    X_train = dfs[1][[1]]
    
    clf$fit(X = X_train, y = y_train)
    
    y_pred = as.factor(clf$predict(X_test))
    
    cm = caret::confusionMatrix(y_pred, y_test)
    
    cm = data.frame(cm$table)
    
    # see: https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot?msclkid=4099256cd0b911ec8fe76f31f4256f3d
    plotTable = cm %>%
      mutate(goodbad = ifelse(cm$Prediction == cm$Reference, "good", "bad")) %>%
      group_by(Reference) %>%
      mutate(prop = Freq/sum(Freq))
    
    ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
      geom_tile() +
      geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
      scale_fill_manual(values = c(good = "green", bad = "red")) +
      theme_bw() +
      xlim(rev(levels(plotTable$Reference))) +
      theme(axis.text.x = element_text(angle = 90))
  })
}

shinyApp(ui, server)