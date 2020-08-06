#####################################
# Shiny App * UK Eat Out - global.R #
#####################################

pkgs <- c('popiFun', 'Cairo', 'colourpicker', 'data.table', 'DT', 'fst', 'htmltools', 'leaflet', 'leaflet.extras', 'shiny', 'shinycssloaders', 'shinyWidgets')
lapply(pkgs, require, char = TRUE)

options(spinner.color = '#e5001a', spinner.size = 1, spinner.type = 4)
options(bitmapType = 'cairo', shiny.usecairo = TRUE)

dts <- read_fst(file.path(app_path, 'uk_eatout', 'dataset'), as.data.table = TRUE)
pc <- read_fst(file.path(geouk_path, 'postcodes'), columns = c('postcode', 'x_lon', 'y_lat'), as.data.table = TRUE)

lcn.tpe <- c(
    'Postcode [Bounding Box]' = 'PCU', 'Postcode Sector ' = 'PCS', 'Postal Town' = 'PCT', 'Postcode District' = 'PCD',
    'Ward' = 'WARD', 'Parish' = 'PAR'
)

lbl.options <- labelOptions(
    nohide = TRUE,
    textsize = '12px',
    direction = 'right', 
    sticky = FALSE, 
    opacity = 0.8,
    offset = c(10, -10), 
    style = list(
        'color' = 'black',
        'border-color' = 'rgba(0,0,0,0.5)',
        'font-family' = 'verdana',
        'font-style' = 'normal',
        'font-size' = '14px',
        'font-weight' = 'normal', 
        'padding' = '2px 6px',
        'box-shadow' = '3px 3px rgba(0,0,0,0.25)'
    )
)

addLegendFixedCustom <- function(map, colors, labels, sizes = 20, opacity = 0.5, radius = 50, ...){
    colorAdditions <- paste0(colors, ';margin-top:4px;margin-bottom:4px;border-radius:', radius, '%;width:', sizes, 'px;height:', sizes, 'px')
    labelAdditions <- paste0(
        '<div style=display:inline-block;height:', sizes, 'px;margin-top:4px;margin-bottom:4px;line-height:', sizes, 'px;>',
        labels, 
        '</div>'
    )
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, ...))
}

mp <- leaflet(options = leafletOptions(minZoom = 6)) %>% 
        setView(lat = 54.003419, lng = -2.547973, zoom = 6) %>% 
        enableTileCaching() %>%
        addTiles(options = tileOptions(useCache = TRUE, crossOrigin = TRUE)) %>% 
        addSearchOSM() %>%
        addResetMapButton() %>%
        addFullscreenControl()
tiles.lst <- c(
    'OSM Mapnik' = 'OpenStreetMap.Mapnik', 
    'OSM B&W' = 'OpenStreetMap.BlackAndWhite', 
    'Stamen Toner' = 'Stamen.Toner', 
    'Stamen Toner Lite' = 'Stamen.TonerLite'
)
for(idx in 1:length(tiles.lst)) 
    mp <- mp %>% addProviderTiles(providers[[tiles.lst[idx]]], group = names(tiles.lst)[idx])
mp <- mp %>% 
        addLayersControl( baseGroups = names(tiles.lst), options = layersControlOptions(collapsed = FALSE) ) %>% 
    	addLegendFixedCustom(
            colors = c('orange', 'lightblue'),
            labels = c('Big Chains', 'Other Shops'),
    		opacity = 1,
            title = '',
            position = 'bottomright',
    	)

markers <- iconList(
    red = makeIcon(file.path(pub_path, 'images', 'icons', 'leaflet', 'markers', 'sm-orange.png'), iconWidth = 24, iconHeight =32),
    blue = makeIcon(file.path(pub_path, 'images', 'icons', 'leaflet', 'markers', 'sm-lightblue.png'), iconWidth = 24, iconHeight =32)
)

bounding_box <- function(lat, lon, dist, in.miles = TRUE) {

    if (in.miles) {
        ang_rad <- function(miles) miles/3958.756
    } else {
        ang_rad <- function(miles) miles/1000
    }
    `%+/-%` <- function(x, margin){x + c(-1, +1)*margin}
    deg2rad <- function(x) x/(180/pi)
    rad2deg <- function(x) x*(180/pi)
    lat_range <- function(latr, r) rad2deg(latr %+/-% r)
    lon_range <- function(lonr, dlon) rad2deg(lonr %+/-% dlon)

    r <- ang_rad(dist)
    latr <- deg2rad(lat)
    lonr <- deg2rad(lon)
    dlon <- asin(sin(r)/cos(latr))

    m <- matrix(c(lon_range(lonr = lonr, dlon = dlon), lat_range(latr=latr, r=r)), nrow=2, byrow = TRUE)

    dimnames(m) <- list(c("lng", "lat"), c("min", "max"))
    m
}
