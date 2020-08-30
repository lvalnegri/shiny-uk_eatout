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
            column(12, 
                actionButton('btn_go', 'FIND RESTAURANTS',
                    icon = icon('search-location'),
                    style = "color:#fff;background-color:#337ab7;border-color:#2e6da4;font-weight:600"
                ),
                align = 'center', style = "margin-bottom:30px;margin-top:-10px"
            ), 
            
            uiOutput('ui_nrs'),
            HTML(paste('<p></p><hr><p>The dataset contains', formatC(nrow(dts), big.mark = ','),'records.</p>')),
            HTML(
                '<p>Data: <a href="https://github.com/hmrc/eat-out-to-help-out-establishments" target="_blank">HMRC</a></p>',
                '<p>Code: <a href="https://github.com/lvalnegri/shiny-uk_eatout" target="_blank">GitHub</a></p><br>',
                '<p>Contains <a href="http://geoportal.statistics.gov.uk/" target="_blank">OS data</a>
                 <p> All content is available under the 
                     <a href="http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/" target="_blank">
                        Open Government Licence v3.0
                     </a></p>
                 <p> &copy; Crown copyright and database right [2020]</p>'
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
