#' Map elections data
#'
#' @param data An \code{sf} object with elections data returned by
#'   \code{\link{join_to_spatial}}.
#' @param projection If not provided, then the best state plane projection will
#'   be guessed using the \code{\link[USAboundaries]{state_plane}} function from
#'   the \code{USAboundaries} package. If \code{NULL}, then leaflet's default
#'   Web Mercator projection will be used. To use a custom projection, provide a
#'   projection/CRS object returned by the \code{\link[leaflet]{leafletCRS}}
#'   function in the \code{leaflet} package.
#' @param legend Should a legend be displayed or not?
#' @param state_boundaries Draw state boundaries in addition to county
#'   boundaries?
#' @param cities Number of largest cities to draw. Pass \code{FALSE} to not draw
#'   any cities.
#' @param scale The type of scale to use for the choropleth map.
#' @param width The width of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#' @param height The height of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#'
#' @examples
#' votes <- vote_counts("meae.congressional.congress05.ny.county")
#' aggregates <- aggregate_party_votes(votes)
#' map_data <- join_to_spatial(aggregates)
#' map_elections(map_data, legend = TRUE)
#'
#' @export
map_elections <- function(data, projection, legend = FALSE,
                          state_boundaries = TRUE, cities = 3L,
                          scale = federalist_vs_republican,
                          width = "100%", height = NULL) {

  stopifnot(is.logical(legend),
            is.logical(state_boundaries),
            is.list(scale),
            is.numeric(cities) || cities == FALSE)

  data <- data %>%
    dplyr::mutate(fed_diff = federalist_percentage - 0.5)

  state <- unique(stats::na.omit(data$state))

  if (missing(projection)) {
    # Guess the state plane projection
    if (length(state) > 1) {
      warning("More than one state in the data. Using web mercator projection.\n",
              "Pass a custom projection if you wish.")
      map <- leaflet::leaflet(data, width = width, height = height)
    } else {
    projection <- leaflet::leafletCRS(crsClass = "L.Proj.CRS",
      code = paste("ESRI:", USAboundaries::state_plane(state), sep = ""),
      proj4def = USAboundaries::state_plane(state, type = "proj4"),
      resolutions = 1.5^(25:15))
    map <-   map <- leaflet::leaflet(data, options =
                                       leaflet::leafletOptions(crs = projection),
                                     width = width, height = height)
    }
  } else if (is.null(projection)) {
    # No projection
    map <- leaflet::leaflet(data, width = width, height = height)
  } else {
    # Use the user-provided projection
    stopifnot(inherits(projection, "leaflet_crs"))
    map <- leaflet::leaflet(data,
                            options = leaflet::leafletOptions(crs = projection),
                            width = width, height = height)
  }

  map <- map %>%
    leaflet::addPolygons(
      # layerId = "county",
      stroke = TRUE,
      smoothFactor = 1,
      color = "#bbb",
      opacity = 1,
      weight = 2,
      dashArray = "5, 5",
      fillOpacity = 1,
      fillColor = ~scale$palette(fed_diff),
      popup = ~popup_maker(county = tools::toTitleCase(tolower(name)),
                           federalist = federalist_vote,
                           republican = republican_vote,
                           other = other_vote,
                           fed_percent = federalist_percentage,
                           rep_percent = republican_percentage,
                           oth_percent = other_percentage)
    )

  if (state_boundaries) {
    state_names <- USAboundaries::state_codes %>%
      dplyr::filter(state_abbr %in% state)
    state_sf <- USAboundaries::us_states(map_date = unique(data$map_date),
                                         resolution = "high",
                                         states = state_names$state_name)
    map <- map %>%
      leaflet::addPolygons(
        data = state_sf,
        # layerId = "state",
        stroke = TRUE,
        smoothFactor = 1,
        color = "#222",
        opacity = 1,
        weight = 3,
        fill = NULL
      )
  }

  if (cities > 0) {
    decade <- trunc(as.integer(format(data$map_date, "%Y")) / 10) * 10
    decade <- unique(stats::na.omit(decade))
    city_locations <- USAboundaries::census_cities %>%
      dplyr::filter(ST %in% state,
                    population > 100,
                    year == decade) %>%
      dplyr::group_by(ST) %>%
      dplyr::top_n(cities, population)

    map <- map %>%
      leaflet::addCircleMarkers(data = city_locations, lat = ~LAT, lng = ~LON,
                                color = "red", opacity = 1,
                                fill = "red", fillOpacity = 1,
                                radius = 3,
                                label = ~City)
  }

  if (legend) {
    map <- map %>%
      leaflet::addLegend("bottomright",
                         title = "Election results",
                         colors = scale$colors,
                         labels = scale$labels)
  }

  map

}

#' @rdname map_elections
#' @export
federalist_vs_republican <- list(
  palette = leaflet::colorBin(
    "PRGn",
    domain = c(-0.5, 0.5),
    bins = c(-0.5, -0.375, -0.25, -0.125, -0.01, 0.01, 0.125, 0.25, 0.375, 0.5),
    na.color = "#808080",
    alpha = FALSE
  ),
  colors = RColorBrewer::brewer.pal(9, "PRGn"),
  labels = c(
    "Republicans (> 87.5%)",
    "Republicans (> 75%)",
    "Republicans (> 62.5%)",
    "Republicans (> 51%)",
    "Tied",
    "Federalists (> 51%)",
    "Federalists (> 62.5%)",
    "Federalists (> 75%)",
    "Federalists (> 87.5%)"
  )
)

popup_maker <- function(county, federalist, republican, other, fed_percent,
                        rep_percent, oth_percent) {
  paste0("<b>County: </b>", county, "<br>",
         "<b>Federalist: </b>", federalist, " (", fed_percent * 100, "%)<br>",
         "<b>Republican: </b>", republican, " (", rep_percent * 100, "%)<br>",
         "<b>Other: </b>", other, " (", oth_percent * 100, "%)<br>")
}
