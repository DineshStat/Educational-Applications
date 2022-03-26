rm(list=ls())
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(tibble)
options(warn = 0)


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "CLT - Explanation", titleWidth = 280),
                    dashboardSidebar(
                        width = 280,
                        sidebarMenu(
                            
                            selectInput("Dist", "Population:",
                                        c("Uniform Distribution" = "Uniform",
                                          "Poission Distribution" = "Poission",
                                          "Normal Distribution" = "Normal"
                                        )),
                            
                            sidebarMenuOutput("Normal_mean"),
                            
                            sidebarMenuOutput("Normal_sd"),
                            
                            sidebarMenuOutput("Uniform_minmax"),
                            
                            sidebarMenuOutput("Poisson_lambda"),
                            
                            sliderInput("n",
                                        "Sample size:", 
                                        value = 850,
                                        min = 2,
                                        max = 1000),
                            
                            sliderInput("k",
                                        "Number of samples:",
                                        value = 9000,
                                        min = 10,
                                        max = 10000)
                            
                        )
                    ),
                    
                    dashboardBody(
                        tags$head(tags$style(HTML(
                            '.main-header .logo{
                        font-family: "Georgia", Times, "Times New Roman", serif;
                        font-weight: bold;
                        font-size: 18px;
                        }'
                        ))),
                        
                        tags$style(type = "text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before {visibility: hidden; }"
                        ),
                        
                        fluidRow(
                            box(title = textOutput("pop_title"), solidHeader = T, status = 'primary',
                                plotOutput("Population_Dist"), height = 520),
                            box(title = textOutput("sample_title"), solidHeader = T, status = 'primary',
                                plotOutput("Sample_Dist"), height = 520)
                        ),
                        
                        fluidRow(
                            infoBoxOutput("info_pop_mean", width = 3),
                            infoBoxOutput("info_pop_sd", width = 3),
                            infoBoxOutput("info_sample_mean", width = 3),
                            infoBoxOutput("info_sample_se", width = 3)
                        ),
                        
                        fluidRow(
                            div(textOutput("Disclaimer_1", inline = TRUE), align = "center"),
                            div(textOutput("Disclaimer_2", inline = TRUE), align = "center"),
                            div(textOutput("Disclaimer_3", inline = TRUE), align = "center")
                        )
                        
                    )
)



server <- function(input, output, session) {
    
    output$Disclaimer_1 = renderText({
        paste("According to CLT, the distribution of sample mean from sampling distribution should follows approximately normal distribution.")
    })
    
    output$Disclaimer_2 = renderText({
        paste("* Distribution of Means from", input$k, "random samples, each consisting of", input$n, "observations from the parent population")
    })
    
    output$Disclaimer_3 = renderText({
        pop = Population()
        s_pop = round(sd(pop),2)
        n = input$n
        se=round(s_pop/sqrt(n),2)
        
        paste("The Mean of the Sampling Distribution should be approximately equal to the Population Mean and the Standard Error should be approximately equal to the SD of the population divided by square root of sample size (", s_pop, "/sqrt(",n, ") = ", se,")")
    })
    
    output$pop_title = renderText({
        if(input$Dist == "Normal")
        {
            paste("Normal Population Distribution")
        } else
            if(input$Dist == "Poission")
            {
                paste("Poission Population Distribution")
            } else
            {
                paste("Uniform Population Distribution")
            }
        
    })
    
    output$sample_title = renderText({
        if(input$Dist == "Normal")
        {
            paste("Sampling Distribution from Normal Population *")
        } else
            if(input$Dist == "Poission")
            {
                paste("Sampling Distribution from Poission Population *")
            } else
            {
                paste("Sampling Distribution from Uniform Population *")
            }
    })
    
    output$Normal_mean = renderUI({
        
        if(input$Dist == "Normal")
        {
            sliderInput("mu",
                        "Mean:",
                        value = 0,
                        min = -50,
                        max = 50)
        } else {
            br()
        }
    })
    
    output$Normal_sd = renderUI({
        
        if(input$Dist == "Normal")
        {
            sliderInput("sd",
                        "Standard deviation:",
                        value = 20,
                        min = 1,
                        max = 100)
        } else {
            br()
        }
    })
    
    output$Uniform_minmax = renderUI({
        
        if(input$Dist == "Uniform")
        {
            sliderInput("minmax",
                        "Lower and Upper Bounds:",
                        value = c(5, 15),
                        min = 0,
                        max = 100)
        } else {
            br()
        }
    })
    
    observeEvent(input$minmax,{
        
        req(input$minmax)
        
        if (input$minmax[1] == input$minmax[2]){
            if (input$minmax[1] == 0){
                updateSliderInput(session, "minmax", value = c(0, 1))
            } else if (input$minmax[2] == 20){
                updateSliderInput(session, "minmax", value = c(19, 20))
            } else {
                updateSliderInput(session, "minmax", value = c(input$minmax[2], input$minmax[2] + 1))
            }
        }
    })
    
    output$Poisson_lambda = renderUI({
        
        if(input$Dist == "Poission")
        {
            sliderInput("lam",
                        "Lamda: ",
                        value = 1,
                        min = 0.1,
                        max = 5)
        } else {
            br()
        }
    })
    
    norm_population = reactive({
        
        n = 1e5
        mu = input$mu
        sd = input$sd
        
        return(rnorm(n, input$mu, input$sd))
        
    })
    
    uniform_population = reactive({
        
        n = 1e5
        min_unif = input$minmax[1]
        max_unif = input$minmax[2]
        
        return(runif(n, min_unif, max_unif))
    })
    
    poission_population = reactive({
        
        n = 1e5
        l = input$lam
        
        return(rpois(n, lambda = l))
        
    })
    
    Population = reactive({
        
        if(input$Dist == "Normal")
        {
            norm_population()
        } else
            if(input$Dist == "Poission")
            {
                poission_population()
            } else
            {
                uniform_population()
            }
        
    })
    
    Sampling_dist = reactive({
        
        pop = Population()
        n = input$n
        k = input$k
        
        return(replicate(k, sample(x = pop, size = n, replace=TRUE)))
    })
    
    output$info_pop_mean = renderInfoBox({
        pop = Population()
        m_pop =  round(mean(pop), 2)
        infoBox("Population Mean", m_pop, width = 2, icon = icon("chart-line"))
    })
    
    output$info_pop_sd = renderInfoBox({
        pop = Population()
        sd_pop = round(sd(pop), 2)
        infoBox("Population Std Deviation", sd_pop, width = 2, icon = icon("chart-line"))
    })
    
    output$info_sample_mean = renderInfoBox({
        samples = Sampling_dist()
        ndist = tibble(means = colMeans(samples))
        m_samp =  round(mean(ndist$means),2)
        infoBox("Sample Mean", m_samp, width = 2, icon = icon("chart-line"))
    })
    
    output$info_sample_se = renderInfoBox({
        samples = Sampling_dist()
        ndist = tibble(means = colMeans(samples))
        sd_samp = round(sd(ndist$means),2)
        infoBox("Sample Standard Error", sd_samp, width = 2, icon = icon("chart-line"))
    })
    
    output$Population_Dist = renderPlot({
        
        if(input$Dist == "Normal")
        {
            pop = Population()
            
            m_pop =  round(mean(pop), 2)
            sd_pop = round(sd(pop), 2)
            
            pop = tibble(samples = pop)
            pdens = density(pop$samples)
            
            x_range = max(pop$samples) - min(pop$samples)
            y_pos = max(pdens$y) - 0.2*max(pdens$y)
            x_pos = ifelse(input$mu > 0, min(-100, min(pop$samples)) + 20,
                           max(100, max(pop$samples)) - 20)
            
            ggplot(data = pop, aes(x = samples, y = ..density..)) + 
                geom_histogram(bins = 45, color = "white", fill = "#195190") +
                stat_density(geom="line", color = "#195190", size = 1) +
                scale_x_continuous(limits = c(min(-100, pop$samples), max(100, pop$samples))) +
                labs(x = "Population Distribution") +
                theme_light(base_size = 19) +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_line())
            
        } else
            if(input$Dist == "Poission")
            {
                pop = Population()
                
                m_pop = round(mean(pop), 2)
                sd_pop = round(sd(pop), 2)
                
                pop = tibble(samples = pop)
                pdens = density(pop$samples)
                
                x_range = max(pop$samples) - min(pop$samples)
                y_pos = max(pdens$y) - 0.2*max(pdens$y)
                x_pos = ifelse(input$mu > 0, min(-100, min(pop$samples)) + 20,
                               max(100, max(pop$samples)) - 20)
                
                ggplot(as.data.frame(Population()), aes(x = Population())) +
                    geom_histogram(binwidth = 1, colour = "black", fill = "#195190") + 
                    labs(x = "Population Distribution") +
                    theme_light(base_size = 19) +
                    theme(plot.title = element_text(hjust = 0.5),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          axis.line = element_line())
            } else
            {
                pop = Population()
                
                m_pop =  round(mean(pop), 2)
                sd_pop = round(sd(pop), 2)
                
                pop = tibble(samples = pop)
                pdens = density(pop$samples)
                
                x_range = max(pop$samples) - min(pop$samples)
                y_pos = max(pdens$y) - 0.2*max(pdens$y)
                
                x_pos = max(pop$samples) - 0.1*x_range
                
                ggplot(data = pop, aes(x = samples, y = ..density..)) +
                    geom_histogram(bins = 45, color = "white", fill = "#195190") +
                    stat_density(geom = "line", color = "#195190", size = 1) +
                    scale_y_continuous(expand = expand_scale(mult = c(0, .3))) +
                    labs(x = "Population Distribution") +
                    theme_light(base_size = 19) +
                    theme(plot.title = element_text(hjust = 0.5),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          axis.line = element_line())
            }
        
    })
    
    output$Sample_Dist = renderPlot({
        
        if(input$Dist == "Normal")
        {
            n = input$n
            k = input$k
            
            pop_2 = Population()
            samples = Sampling_dist()
            
            ndist = tibble(means = colMeans(samples))
            m_samp =  round(mean(ndist$means),2)
            sd_samp = round(sd(ndist$means),2)
            
            
            ndens = density(ndist$means)
            nhist = hist(ndist$means, plot=FALSE)
            
            x_range = max(ndist$means) - min(ndist$means)
            
            y_pos = max(ndens$y) - 0.1*max(ndens$y)
            x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                           max(ndist$means) - 0.1*x_range)
            
            ggplot(data = ndist, aes(x = means, y = ..density..)) +
                geom_histogram(bins = 20, color = "white", fill = "#009499") +
                stat_density(geom = "line", color = "#009499", size = 1) +
                labs(x = "Sample means", y = "") +
                theme_light(base_size = 19) +
                theme(plot.title = element_text(hjust = 0.5),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_line())
        } else
            if(input$Dist == "Poission")
            {
                n = input$n
                k = input$k
                
                pop_2 = Population()
                samples = Sampling_dist()
                
                ndist = tibble(means = colMeans(samples))
                m_samp =  round(mean(ndist$means),2)
                sd_samp = round(sd(ndist$means),2)
                
                
                ndens = density(ndist$means)
                nhist = hist(ndist$means, plot=FALSE)
                
                x_range = max(ndist$means) - min(ndist$means)
                
                y_pos = max(ndens$y) - 0.1*max(ndens$y)
                x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                               max(ndist$means) - 0.1*x_range)
                
                ggplot(data = ndist, aes(x = means, y = ..density..)) +
                    geom_histogram(bins = 20, color = "white", fill = "#009499") +
                    stat_density(geom = "line", color = "#009499", size = 1) +
                    labs(x = "Sample means", y = "") +
                    theme_light(base_size = 19) +
                    theme(plot.title = element_text(hjust = 0.5),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          axis.line = element_line())
                
            } else
            {
                n = input$n
                k = input$k
                
                
                pop = Population()
                samples = Sampling_dist()
                
                m_pop =  round(mean(pop),2)
                sd_pop = round(sd(pop),2)
                
                ndist = tibble(means = colMeans(samples))
                
                m_samp =  round(mean(ndist$means),2)
                sd_samp = round(sd(ndist$means),2)
                
                ndens = density(ndist$means)
                nhist = hist(ndist$means, plot=FALSE)
                
                x_range = max(ndist$means) - min(ndist$means)
                
                y_pos = max(ndens$y) - 0.1*max(ndens$y)
                x_pos = ifelse(m_samp > 0, min(ndist$means) + 0.1*x_range, 
                               max(ndist$means) - 0.1*x_range)
                
                ggplot(data = ndist, aes(x = means, y = ..density..)) +
                    geom_histogram(bins = 20, color = "white", fill = "#009499") +
                    stat_density(geom = "line", color = "#009499", size = 1) +
                    labs(x = "Sample means", y = "") +
                    theme_light(base_size = 19) +
                    theme(plot.title = element_text(hjust = 0.5),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          axis.line = element_line())
            }
        
    })
    
}



shinyApp(ui, server)