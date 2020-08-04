pkgs <- c('popiFun', 'data.table', 'fst')
lapply(pkgs, require, char = TRUE)

add_loca_name <- function(dt, geo, codes = TRUE, coords = FALSE){
    yn <- names(dt)
    cols <- c('type', 'location_id', 'name')
    if(coords) cols <- c(cols, 'x_lon', 'y_lat')
    y <- read_fst(file.path(geouk_path, 'locations'), columns = cols, as.data.table = TRUE)
    y <- y[type == geo][, type := NULL][, name := factor(name)]
    cols <- c('id', geo)
    if(coords) cols <- c(cols,  paste0(geo, c('x', 'y')))
    setnames(y, cols)
    dt <- y[dt, on = c(id = geo)][, id := NULL]
    setcolorder(dt, yn)
    dt
}

y <- fread(
        'https://raw.githubusercontent.com/hmrc/eat-out-to-help-out-establishments/master/data/participating-establishments/restaurants.csv',
        na.strings = ''
)
setnames(y, tolower(names(y)))
y[name == `line 1`, `:=`(`line 1` = `line 2`, `line 2` = NA )]

y <- capitalize(y, 'name')
y <- capitalize(y, 'line 1')
y <- capitalize(y, 'line 2')
y[, address := paste0(trimws(`line 1`), ifelse(is.na(`line 2`), '', paste(',', trimws(`line 2`))))][is.na(`line 1`), address := NA]
y[, `:=`(`line 1` = NULL, `line 2` = NULL )]
cols <- c('PAR', 'WARD', 'PCS', 'PCD', 'PCT', 'RGN')
y <- add_geocodes(y, add_oa = TRUE, census = FALSE, admin = FALSE, postal = FALSE, cols_in = cols)
for(t in cols) y <- add_loca_name(y, t, FALSE)
y[, c('town', 'county') := NULL]
#===
pc <- read_fst(file.path(geouk_path, 'postcodes'), columns = c('postcode', 'x_lon', 'y_lat'), as.data.table = TRUE)
y <- pc[y, on = 'postcode']
#===
setcolorder(y, c('name', 'address'))

y[, is_chain := 'green']
txt <- c('burger king', 'kfc', 'mcdonald')
for(tx in txt) y[grepl(tx, tolower(name)), is_chain := 'red']

write_fst(y, file.path(app_path, 'uk_eatout', 'dataset'))

rm(list = ls())
gc()

