#################################
# Shiny App * UK Eat Out - ui.R #
#################################

fluidPage(
    
    titlePanel('UK Eatout'),
    
    sidebarLayout(
        
        sidebarPanel(
            
            pickerInput('cbo_geo', 'GEOGRAPHY:', lcn.tpe),
            
            uiOutput('ui_1st'),
                     
            uiOutput('ui_2nd'),
            
            tags$br(),
            
            column(12, actionButton('btn_go', 'FIND RESTAURANTS'), align = "center", style = "margin-bottom: 10px;margin-top: -10px;"), 
      #            actionButton('btn_go', 'FIND RESTAURANTS'),
            tags$br(),
            
            
            width = 3
            
        ),
        
        mainPanel(
                  
            tabsetPanel(
                                               
                tabPanel('Map', withSpinner(leafletOutput('out_map', height = '600px') ) ),
            
                tabPanel('Table', withSpinner(DTOutput('out_tbl') ) )
            
            )
            
        )
        
    )
    
)
