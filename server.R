#####################################
# Shiny App * UK Eat Out - server.R #
#####################################

server <- function(input, output) {

    output$ui_1st <- renderUI({
        
        switch(input$cbo_geo,
            
            'PCU' = { textInput('xx1_pcu', 'Enter a valid postcode:', placeholder = 'Example: SW12 8AA') },
            
            { pickerInput('xx1_rgn', 'REGION:', levels(dts$RGN), 'London') }
               
        )
        
    })
    
    output$ui_2nd <- renderUI({
        
        switch(input$cbo_geo,
            
            'PCU' = { sliderInput('xx2_pcu', 'Distance (miles):', min = 0.2, max = 2, value = 0.6, step = 0.2) },
               
            { 
                if(is.null(input$xx1_rgn)) return(NULL)
                y <- droplevels(dts[RGN == input$xx1_rgn, get(input$cbo_geo)])
                pickerInput('xx2_lcn', paste0(toupper(names(which(lcn.tpe == input$cbo_geo))), ':'), levels(y)) 
            }
               
        )
        
        
    })
    
    dt <- eventReactive(input$btn_go, {
        
        switch(input$cbo_geo,
            
            'PCU' = {
                yc <- clean_postcode(data.table(postcode = input$xx1_pcu))
                if(is.na(yc)){
                    list('coords' = NULL)
                } else {
                    yc <- pc[postcode == yc$postcode, .(x_lon, y_lat)]
                    yb <- bounding_box(yc$y_lat, yc$x_lon, as.numeric(input$xx2_pcu))
                    list(
                        'coords' = yc,
                        'box' = yb,
                        'data' = dts[ x_lon >= yb[1, 1] & x_lon <= yb[1, 2] & y_lat >= yb[2, 1] & y_lat <= yb[2, 2] ]
                    )
                }
            },
            
            {
                yd <- dts[RGN == input$xx1_rgn & get(input$cbo_geo) == input$xx2_lcn]
                yb <- c(min(yd$x_lon), min(yd$y_lat), max(yd$x_lon), max(yd$y_lat))
                dim(yb) <- c(2,2)
                dimnames(yb) <- list(c('lng', 'lat'), c('min', 'max'))
                list('coords' = data.table('x_lon' = mean(yb[1,]), 'y_lat' = mean(yb[2,])), 'box' = yb, 'data' = yd)
            }
               
        )
        
    })
    

    output$out_map <- renderLeaflet({
        
        if(is.null(dt()$coords)) return(mp)
        
        yd <- dt()$data
        yb <- dt()$box
        mp %>%
            fitBounds(yb[1, 1], yb[2, 1], yb[1, 2], yb[2, 2]) %>%
            addMarkers(
                data = yd,
                lng = ~x_lon, lat = ~y_lat,
                label = lapply(
                    1:nrow(yd),
                    function(x)
                        HTML(paste0('<p style="font-weight:bold;color:red;font-size:14px;">', 
                                    yd[x, name], '</p>', 
                                    yd[x, address], '<br>', 
                                    yd[x, postcode], '<br>',
                                    yd[x, WARD], ', ', yd[x, PCT]
                        ))
                )
            )
        
    })
    
    output$out_tbl <- renderDT({
        
        datatable( 
            dt()$data[, .(name, address, postcode, Ward = WARD, Town = PCT)],
            rownames = NULL, 
            selection = 'none',
            class = 'stripe nowrap hover compact row-border',
            extensions = c('Buttons', 'FixedColumns', 'Scroller'),
            options = list(
                scrollX = TRUE,
                scrollY = 600,
                scroller = TRUE,
                buttons = c('copy', 'csv', 'print'),
                fixedColumns = list(leftColumns = 1),
                ordering = TRUE,
                deferRender = TRUE,
                dom = 'Biftp'
            )
        )
        
    })

    
}