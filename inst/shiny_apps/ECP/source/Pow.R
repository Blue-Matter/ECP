PowServer <- function(id, sel) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 output$Pow <- renderUI({
                   tagList(

                     renderPlot({
                       #plot_dist(sel$obj,OMind=sel$OMind, Iind=sel$Iind,yind=sel$yind,powind=sel$powind,tail=sel$tail,alp=sel$alp)
                       #plot_dist(sel$obj,OMind=sel$OMind, Iind=sel$Iind,yind=sel$yind,powind=sel$powind,tail=sel$tail,alp=sel$alp)
                       #plot_CC(sel$obj,quanty=0.5,ptcex=0.6,maxn=10,OMind=sel$OMind, Iind=sel$Iind,powind=sel$powind,dopow=!is.na(sel$powind))
                       Seq_Pow_Calc_Marg(sel$obj, OMind = sel$OMind, Iind=sel$Iind, yind=sel$yind, powind=sel$powind, alp=sel$alp, tail = sel$tail,plot=T,main=F)
                       },height=850)


                   )})
               }
  )

}



Pow_UI <- function(id, label="Pow") {

  ns <- NS(id)

  tagList(

    fluidRow(
      htmlOutput(ns('Pow'))
    )
  )
}


