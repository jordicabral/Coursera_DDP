# DDP Coursera - Project

library(shiny)
library(faraway)
data("teengamb")
teengamb$sex <- factor(teengamb$sex)
levels(teengamb$sex) <- c("Men", "Women")

shinyUI(fluidPage(
    titlePanel("Teen Gambling Dataset from Faraway package"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("x", "Select X-axis:",
                         list("sex"='a', "status"='b',
                              "income"='c', "verbal"='d')),
            h4("Select a variable to show relationship with gambling expenditure and the regression line")
            ),   
        mainPanel(       
            h3("Relationship between variables"),
            plotOutput("distPlot")     
        )   
    ) 
))
