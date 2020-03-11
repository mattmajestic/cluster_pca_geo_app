library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(broom)
library(DT)
library(tibble)


ui <- shinydashboard::dashboardPage(dashboardHeader(title = "Modeling"),
                                    dashboardSidebar(sidebarMenu(
                                      menuItem("Clustering",tabName = "models"),
                                      menuItem("PCA",tabName = "pca"),
                                      menuItem("Regressions",tabName = "lms")
                                    )),
                                    dashboardBody(
                                      tabItems(
                                        tabItem(tabName = "models",
                                                fluidPage(
                                                  fluidRow(
                                                    column(6,numericInput("clusters","Select Number of Clusters",value = 5)),
                                                    actionButton("runK","Run Analysis")),
                                                  fluidRow(plotlyOutput("lasso")),
                                                  br(),
                                                  fluidRow(column(6,
                                                                  h2("Hierarchical Clusters"),
                                                                  plotlyOutput("HieracrchPlot")),
                                                           column(6,
                                                                  h2("Kmeans Clusters"),
                                                                  plotlyOutput("KmeansPlot"))))),
                                        tabItem(tabName = "pca",
                                                fluidRow(column(6,
                                                                h2("2 Principle Components by Cluster"),
                                                                plotlyOutput("plotPCA")),
                                                         column(6,
                                                                h2("Summary Stats by Cluster"),
                                                                DTOutput("PCA")))),
                                        tabItem(tabName = "lms",
                                                fluidRow(
                                                  h2("Heatmap by Cluster"),
                                                  plotlyOutput("plotGEO")),
                                                fluidRow(h2("Regression Output by Cluster"),
                                                         plotlyOutput("lm_group"),
                                                         DTOutput("lm_glance_group")),
                                                fluidRow(h2("Regression Output for All Data"),
                                                         plotlyOutput("lm_all"),
                                                         DTOutput("lm_glance_all"))
                                                ))
                                    )
)

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
    output$HieracrchPlot <- renderPlotly({plot_ly(rv$state_crime,x=~Murder, y=~Assault,size =~UrbanPop,color =~ClusterH)})
    tmp_PCA <- prcomp(rv$state_crime, scale = TRUE)
    rv$PCA <- augment(tmp_PCA, data = rv$state_crime)
    
    rv$PCA_pivot <- rv$PCA %>% group_by(ClusterH) %>% 
      summarize(count = n(),
                avgAssault = mean(Assault),
                avgMurder = mean(Murder),
                avgRape = mean(Rape),
                avgPop = mean(UrbanPop)) %>%
      mutate(AssaultperPerson = avgAssault/avgPop)
    output$PCA <- renderDT({datatable(rv$PCA_pivot,rownames = FALSE,options = list(scrollX = TRUE))})
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
    
    tmp_lm_all <- rv$geo_plot %>%
      do(fit_all = lm(Murder ~ Assault + UrbanPop + Rape, data = .))
    tmp_coef_all <- tidy(tmp_lm_all,fit_all,conf.int = TRUE)
    output$lm_all <- renderPlotly({ggplotly(ggplot(tmp_coef_all , aes(estimate, term, color = term)) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)))})
    tmp_glance_all <- glance(tmp_lm_all,fit_all)
    output$lm_glance_all <- renderDT(tmp_glance_all)
    
    
    tmp_lm_group <- rv$geo_plot %>% dplyr::group_by(ClusterK) %>%
      do(fit_group = lm(Murder ~ Assault + UrbanPop + Rape, data = .))
    tmp_coef_group <- tidy(tmp_lm_group,fit_group,conf.int = TRUE)
    output$lm_group <- renderPlotly({ggplotly(ggplot(tmp_coef_group , aes(estimate, term, color = term)) +
        geom_point() +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + 
        facet_grid(cols = vars(ClusterK)))})
    tmp_glance_group <- glance(tmp_lm_group,fit_group)
    output$lm_glance_group <- renderDT(tmp_glance_group)
    
    QC_rv <<- reactiveValuesToList(rv)
  })
  
}

shinyApp(ui,server)
