library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(broom)
library(DT)
library(tibble)


ui <- shinydashboard::dashboardPage(dashboardHeader(title = "Modeling"),
                                    dashboardSidebar(sidebarMenu(
                                      menuItem("Build Models",tabName = "models")
                                    )),
                                    dashboardBody(
                                      tabItems(
                                        tabItem(tabName = "models",
                                                fluidPage(
                                                  fluidRow(
                                                    numericInput("clusters","Select Number of Clusters",value = 5),
                                                    plotlyOutput("lasso"),
                                                    actionButton("runK","Run Kmeans,Hclust,PCA, & Summary Statistics"),
                                                    br(),
                                                    column(6,
                                                    h2("Hierarchical Clusters"),
                                                    plotlyOutput("HieracrchPlot")),
                                                    column(6,
                                                    h2("Kmeans Clusters"),
                                                    plotlyOutput("KmeansPlot")),
                                                    br(),
                                                    column(6,
                                                    h2("2 Principle Components by Cluster"),
                                                    plotlyOutput("plotPCA")),
                                                    column(6,
                                                    h2("Summary Stats by Cluster"),
                                                    DTOutput("PCA"),
                                                    h2("Heatmap by Cluster"),
                                                    plotlyOutput("plotGEO"))
                                                  )
                                                ))
                                      )
                                    ))

server <- function(input,output,session){
  
  rv<- reactiveValues(state_crime = data.frame(),PCA = data.frame(),PCA_pivot = data.frame(),
                      geo_plot = data.frame(), state_abb = data.frame())
  rv$state_crime <- USArrests
  rv$states_abb <- data.frame('state' = rownames(USArrests)) %>% mutate(state_abb = state.abb[match(state, state.name)])
  
  output$lasso <- renderPlotly({plot_ly(rv$state_crime,x=~Assault,y =~Murder,color =~UrbanPop)})
  
  observeEvent(input$runK, {
    
    fitK <- kmeans(rv$state_crime, input$clusters)
    fitK_tidy <- tidy(fitK)
    fitK_glance <- glance(fitK)
    rv$state_crime$ClusterK <- fitK$cluster
    output$KmeansPlot <- renderPlotly({plot_ly(rv$state_crime,x=~Murder, y=~Assault,size =~UrbanPop,color =~ClusterK)})
    fitH <- hclust(dist(rv$state_crime), method = "ward.D", members = NULL)
    rv$state_crime <- rv$state_crime %>% mutate("ClusterH" = cutree(fitH,k = input$clusters))
    output$HieracrchPlot <- renderPlotly({plot_ly(rv$state_crime,a=~Murder,b =~Assault, c=~UrbanPop,color =~ClusterH,type = "scatterternary")})
    tmp_PCA <- prcomp(rv$state_crime, scale = TRUE)
    rv$PCA <- augment(tmp_PCA, data = rv$state_crime)
    
    rv$PCA_pivot <- rv$PCA %>% group_by(ClusterH) %>% 
      summarize(count = n(),
          avgAssault = mean(Assault),
          avgMurder = mean(Murder),
          avgRape = mean(Rape),
          avgPop = mean(UrbanPop)) %>%
      mutate(AssaultperPerson = avgAssault/avgPop)
    output$PCA <- renderDT({datatable(rv$PCA_pivot,rownames = FALSE)})
    output$plotPCA <- renderPlotly({plot_ly(rv$PCA,x=~.fittedPC1,y=~.fittedPC2,color =~ClusterH)})
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    rv$geo_plot <- cbind.data.frame(rv$state_crime,rv$states_abb)
    output$plotGEO <- renderPlotly({plot_geo(rv$geo_plot, locationmode = 'USA-states') %>%
        add_trace(
          z = ~ClusterK, locations = ~state_abb,
          color = ~ClusterK, colors = 'Purples'
        ) %>%
        colorbar(title = "Cluster H") %>%
        layout(
          title = "Clusters over the States",
          geo = g
        )})
    QC_rv <<- reactiveValuesToList(rv)
  })
  
}

shinyApp(ui,server)
