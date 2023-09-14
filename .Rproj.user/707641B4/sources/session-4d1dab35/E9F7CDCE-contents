VizServer <- function(id, sel) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$Viz <- renderUI({
                   tagList(

                     renderPlot({
                       #plot_dist(sel$obj,OMind=sel$OMind, Iind=sel$Iind,yind=sel$yind,powind=sel$powind,tail=sel$tail,alp=sel$alp)
                       plot_dist(sel$obj,OMind=sel$OMind, Iind=sel$Iind,yind=sel$yind,powind=sel$powind,tail=sel$tail,alp=sel$alp)

                      },height=850)


                   )})
               }
  )

}



Viz_UI <- function(id, label="Viz") {

  ns <- NS(id)

  tagList(

    fluidRow(
      htmlOutput(ns('Viz'))
    )
  )
}


