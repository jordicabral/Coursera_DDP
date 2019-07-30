# DDP Coursera - Project TEST

library(shiny)
library(faraway)
data("teengamb")
teengamb$sex <- factor(teengamb$sex)
levels(teengamb$sex) <- c("Men", "Women")

shinyServer(function(input, output) {   
    output$distPlot <- renderPlot({     
        if(input$x=='a'){       
            i<-1     
            }     
        if(input$x=='b'){       
            i<-2     
            }     
        if(input$x=='c'){       
            i<-3     
            }     
        if(input$x=='d'){       
            i<-4     
            }     
        x    <- teengamb[, i]     
        y    <- teengamb[, 5]
        
        plot(x,y, ylab = "Expenditure on gambling in pounds per year")
        abline(lm(y ~ x), col="red")
    })
})

