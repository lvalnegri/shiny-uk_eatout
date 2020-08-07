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
            column(12, actionButton('btn_go', 'FIND RESTAURANTS'), align = "center", style = "margin-bottom:30px;margin-top:-10px;"), 
            HTML(
                '<p>Data: <a href="https://github.com/hmrc/eat-out-to-help-out-establishments" target="_blank">HMRC</a></p>',
                '<p>Code: <a href="https://github.com/lvalnegri/shiny-uk_eatout" target="_blank">GitHub</a></p>'
            ),

            width = 3

        ),

        mainPanel(

            tabsetPanel(

                tabPanel('Map', tags$br(), withSpinner(leafletOutput('out_map', height = '600px') ) ),

                tabPanel('Table', tags$br(), textOutput('out_txt'), withSpinner(DTOutput('out_tbl') ) )

            )

        )

    )

)
