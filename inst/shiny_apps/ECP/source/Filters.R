


FiltersServer <- function(id, sel) {

  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 # States of Nature Filters
                 # (may need wrapping for robustness https://stackoverflow.com/questions/24205676/r-shiny-wrapping-ui-elements)
                 output$SN_filters <- renderUI({
                   lapply(1:sel$nFac, function(i) {
                     checkboxGroupInput(inputId= session$ns(paste0("Fil_SN",i)),
                                        label = sel$Factor_Labels[i],
                                        choiceNames=sel$Labels[[i]],
                                        selected=sel$obj$Defaults[[1]][[3]][[i]], # first default, third slot is OM, i is factor
                                        inline=T,
                                        choiceValues=sel$Labels[[i]])
                   })
                 })

                 #observeEvent(Object$Loaded, {
                  # observeEvent(input$Filt,{
                   #  FilterOMs(Object, input, SNkeep, MPkeep, Detkeep, Stochkeep, Projkeep, Det, Stoch, Proj)
                    # Object$Filt<-FALSE
                  # })
                 #})

                 output$show_filters <- renderUI({
                   tagList(
                     column(12,align = 'left', class='multicol',
                            hr(),
                            radioGroupButtons(inputId= session$ns("Presets"),
                                               label = "Presets",
                                               choiceNames=names(sel$obj$Defaults),
                                               selected=1,
                                               choiceValues=1:length(sel$obj$Defaults)),
                            hr(),
                            numericInput(inputId= session$ns("alp"),"% Type I error (alpha) (per Data-Year)",
                                         value = sel$obj$Defaults[[1]]$alph,min=0.1,max=10,step=0.1),

                            hr(),
                            sliderInput(inputId= session$ns("yind"),"Number of projection years",
                                        value = max(sel$obj$Defaults[[1]]$yind),min=3,max=dim(sel$obj$Obs)[2],step=1),

                            hr(),
                            radioButtons(inputId= session$ns("tail"),"Tail",
                                         choiceNames=c("LB","UB","interval","auto"),
                                         selected=sel$obj$Defaults[[1]]$tail,
                                         choiceValues=c("LB","UB","interval","auto")),

                            hr(),
                            radioButtons(inputId= session$ns("pow"),"Definition of Null / Alternative sets",
                                         choiceNames=c(dimnames(sel$obj$Pow)[[3]],"None"),
                                         selected=sel$obj$Defaults[[1]]$powind,
                                         choiceValues=c(1:(dim(sel$obj$Pow)[3]),NaN)),
                            hr(),
                            checkboxGroupInput(inputId= session$ns("Ind_filters"),
                                               label = "Data types",
                                               choiceNames=row.names(sel$obj$Obs),
                                               selected=sel$obj$Defaults[[1]]$Data,
                                               inline=T,
                                               choiceValues=1:nrow(sel$obj$Obs)),
                            hr(),
                            h5('Operating Models'),
                            uiOutput(session$ns('SN_filters'))

                     )
                   )
                 })


                 observeEvent(input$Presets,{
                   defind=as.numeric(input$Presets)
                   updateNumericInput(getDefaultReactiveDomain(),'alp',value=sel$obj$Defaults[[defind]]$alph*100)
                   updateSliderInput(getDefaultReactiveDomain(),'yind',value=as.integer(max(sel$obj$Defaults[[defind]]$yind)))
                   updateRadioButtons(getDefaultReactiveDomain(),'tail',selected= sel$obj$Defaults[[defind]]$tail)
                   updateRadioButtons(getDefaultReactiveDomain(),'pow',selected=sel$obj$Defaults[[defind]]$powind)
                   updateCheckboxGroupInput(getDefaultReactiveDomain(),'Ind_filters',selected=sel$obj$Defaults[[defind]]$Data)
                   lapply(1:sel$nFac, function(i) {
                     updateCheckboxGroupInput(getDefaultReactiveDomain(),paste0("Fil_SN",i),
                                        selected=sel$obj$Defaults[[defind]][[3]][[i]]) # first default, third slot is OM, i is factor
                   })
                 })

                 observeEvent(input$alp,{sel$alp=input$alp/100})
                 observeEvent(input$yind,{sel$yind=1:input$yind})
                 observeEvent(input$tail,{sel$tail=input$tail})
                 observeEvent(input$pow,{sel$powind=as.numeric(input$pow)})
                 observeEvent(input$Ind_filters,{sel$Iind=as.numeric(input$Ind_filters)})

                 observe({
                   FilterNames<-c(paste0("Fil_SN",1:sel$nFac)) #,"Fil_MP","Fil_PM_Det","Fil_PM_Stoch","Fil_PM_Proj")
                   observeEvent(sapply(FilterNames, function(x) input[[x]]),{

                     keep <- array(T,dim(sel$obj$OM_Design))
                     for(fac in 1:sel$nFac) keep[,fac] <- sel$obj$OM_Design[,fac]%in%input[[paste0("Fil_SN",fac)]]
                     #print(OMind)
                     sel$OMind=(1:nrow(sel$obj$OM_Design))[apply(keep,1,all)]

                   })
                 })




               }
  )
}



FiltersUI <- function(id, label="filters") {

  ns <- NS(id)
  tagList(
     fluidRow(
     column(12,
            htmlOutput(ns('show_filters'))
     )
    )
  )
}






