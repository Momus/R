#' Hurricanes
#'
#' A map of hurricane speeds at a single observation
#' showing the extent of observed wind speeds.
#'


#' @title geoSectorGrob
#'
#' @description Create a \code{\link{ggplot2::Grob}} object that draws
#'     a sector (⌔g) of a circle mapped on the earth's surface.  The
#'     defaults will draw the Northeast quadrant of a one nautical
#'     mile circle about the Prime Meridian on the Equator.
#' 
#' @param lon the longitude of the center of the circle
#' @param lat the latitude of the center.
#' @param start_angle the angle, ∠θ, in degrees with respect to the
#'     latitude, of the initial radius of ⌔g.
#' @param stop_angle the angle, ∠φ, in degrees, of the final radius of ⌔g.
#' @param radius in standard nautical miles: 1 nmi == 1852 meters
#' @param scale_radii multiplier to change the size of the Grob.
#' @param smooth multiplier that controls how many points
#'     \code{\link{geosphere::destPoint}} will produce to draw the
#'     sector. Since a polygon is being used to approximate a curve,
#'     the more points, the less "segmented" the appearance. The
#'     default will produce a point every 1°
#' @param gp vector of graphical parameters passed to the Grob.
#'
#' @examples
#' testG <- geoSectorGrob(){
#' ex_vp <- viewport(xscale = range(as.numeric(testG$x)),
#'                   yscale = range(as.numeric(testG$y)))
#' pushViewport(ex_vp)
#' grid.draw(testG)
#' 
#' @export
#' 
geoSectorGrob <- function(longitude = 0, latitude = 0,
                         start_angle = 0, stop_angle = 90,
                         radius = 1, scale_radii = 1, smooth = 1,
                         gp = grid::gpar(fill="pink")){

    
    center <- c(lon, lat)

    
    segment7G <- geosphere::destPoint(p = center,
                                     b=(seq(start_angle, stop_angle, smooth)),
                                     d=(radius
                                         * 1852 #Convert from nautical miles
                                         *  scale_radii)) #scale

    
    ##Insert the center to make a sector ⌔ from a segment ⌓ 
    sector7G <- rbind(segment7G, center,
                     deparse.level = 0) #Prevents row from being named.
                     
    
    
    grid::polygonGrob(x=sector7G[,'lon'], 
                      y=sector7G[,'lat'],
                      gp = gp,
                      default.units = "native")
}

#' @title Geom Util functions
#'
#' @description helpers to compose draw_panel() in the GeomHurricane
#'     object.
#' @inheritParams GeomHurricane
#'
#' @keywords hidden private
## .hurricane_panel <- function(data, panel_scales, coord){

    
 
    

##     ## caineGrobs  <- function(quadrant="SE"){
##     ##     }
##     ## grid::grobTree(children = grid::gList(qNE, qNW, qSE, qSW))
##     ## testGTree <- (addGrob(gTree(testG, testG), testG ))
## }



#' @title GeomHurricane
#'
#' @description Map the asymmetric wind speeds from a single hurricane
#'     observation.
GeomHurricane <- ggplot2::ggproto("GeomHurricane", Geom,
                                 required_aes = c("longitude", "latitude",
                                                  "r_ne", "r_nw", "r_se", "r_sw",
                                                  "wind_speed"),
                                 default_aes = aes(color = c("red", "orange", "yellow"),
                                                   fill = c("red", "orange", "yellow")),
                                 draw_key = draw_key_polygon,
                                 draw_panel =    function(data, panel_scales, coord){
                                     geoSectorGrob(lon=data[1,1], lat=data[1,2],
                                                   start_angle=0, stop_angle=90,
                                                   radius = data[1,3],
                                                   gp=(gpar(fill="red", alpha=.5)))
                                 })
                                 


    #.hurricane_panel(data, panel_scales, coord))





    
#' @title geom_hurricane
#'
#' @description Boilerplate to wrap GeomHurricane for plotting with
#'     ggplot2.
#' 
#' @importFrom ggplot2 layer
#'
#' @section Aesthetics
#' \aesthetics{geom}{hurricaine}
#'
#' @export
#' @inheritParams ggplot::layer
#' @inheritParams GeomHurricane
#' @seealso \code{\link{ggplot::layer}} describes the parameters
#'     needed to wrap GeomHurricane. Please see
#'     \code{\link{GeomHurricane}} above for the parameters specific
#'     to this geom.
#' 
#' @examples
#' library(ggmap)
#' get_map("Louisiana", zoom = 6, maptype = "toner-background") %>% ggmap(extent = "device") +
#' ggplot(data = katrina) +
#'     geom_hurricane(aes(x=longitude, y=latitude,
#'                        r_ne = ne, r_se = se, r_nw = nw, r_sw = sw.
#'                        fill = wind_speed, color = wind_speed)) +
#'     scale_color_manual(name = "Wind speed (kts)",
#'                        values = c("red", "organge", "yellow")) +
#'     scale_fill_manual(name = "Wind speed (kts)",
#'                        values = c("red", "organge", "yellow"))
geom_hurricane <- function(mapping = NULL, data = NULL,
                          position = "indentity", show.legend = NA,
                          inherit.aes = TRUE, ...){
    ggplot2::layer(geom = GeomHurricane, #The rest is copypasta
                   data = data, mapping = mapping, position="identity", stat="identity",
                   show.legend = show.legend , inherit.aes = inherit.aes,
                   params = list(...))
}                             
