input_xx2_lcn <- function(r, g){
    if(is.null(r)) return(NULL)
    y <- data.table(levels(droplevels(dts[RGNn == r, get(g)])))
    setnames(y, 'X')
    if(g == 'PCD'){
        y <- unique(pco[, .(X = PCD, ord_PCD)])[y, on = 'X'][order(ord_PCD)][, ord_PCD := NULL][, X]
    } else if(g == 'PCS'){
        y <- pco[, .(X = PCS, ord_PCS)][y, on = 'X'][order(ord_PCS)][, ord_PCS := NULL][, X]
    } else {
        y <- build_list_loca(y, g, 'X')
    }
    pickerInput('xx2_lcn', paste0(toupper(names(which(lcn.tpe == g))), ':'), y, options = list(`live-search` = TRUE, size = 12) ) 
}
