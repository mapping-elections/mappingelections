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
#' @param congressional_boundaries Draw Congressional district boundaries in
#'   addition to county boundaries?
#' @param cities Number of largest cities to draw. Pass \code{FALSE} to not draw
#'   any cities.
#' @param width The width of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#' @param height The height of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#'
#' @rdname map_elections
#'
#' @importFrom dplyr ends_with
#' @export
map_counties <- function(data, projection = NULL,
                          congressional_boundaries = TRUE, cities = 8L,
                          width = "100%", height = "800px") {

  stopifnot(is.logical(congressional_boundaries),
            is.numeric(cities) || cities == FALSE)

  state_to_filter <- most_common_state(data$state)
  statename_to_filter <- USAboundaries::state_codes %>%
    dplyr::filter(state_abbr == state_to_filter) %>%
    dplyr::pull(state_name)

  congress <- unique(stats::na.omit(data$congress))[1]

  if (is.null(projection)) {
    # Use the state plane projection
    projection <- leaflet::leafletCRS(crsClass = "L.Proj.CRS",
      code = paste("ESRI:", USAboundaries::state_plane(state_to_filter), sep = ""),
      proj4def = USAboundaries::state_plane(state_to_filter, type = "proj4"),
      resolutions = 1.5^(25:15))
  } else {
    stopifnot(inherits(projection, "leaflet_crs"))
  }

  colors <- poli_chrome(dplyr::as_data_frame(data))

  # Instantiate the map with the data and the projection
  map <- leaflet::leaflet(data, width = width, height = height,
                          options = leaflet::leafletOptions(
                            crs = projection,
                            zoomControl = FALSE, dragging = TRUE,
                            minZoom = 7, maxZoom = 11
                            ))

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
      fillColor = colors
      # popup = ~popup_maker(county = tools::toTitleCase(tolower(name)),
      #                      federalist = federalist_vote,
      #                      republican = republican_vote,
      #                      other = other_vote,
      #                      fed_percent = federalist_percentage,
      #                      rep_percent = republican_percentage,
      #                      oth_percent = other_percentage)
    )

  # if (state_boundaries) {
  #   state_names <- USAboundaries::state_codes %>%
  #     dplyr::filter(state_abbr %in% state_to_filter)
  #   state_sf <- USAboundaries::us_states(map_date = unique(data$map_date),
  #                                        resolution = "high",
  #                                        states = state_names$state_name)
  #   map <- map %>%
  #     leaflet::addPolygons(
  #       data = state_sf,
  #       # layerId = "state",
  #       stroke = TRUE,
  #       smoothFactor = 1,
  #       color = "#222",
  #       opacity = 1,
  #       weight = 3,
  #       fill = NULL
  #     )
  # }

  if (congressional_boundaries) {
    congress_sf <- histcongress %>%
      dplyr::filter(statename %in% statename_to_filter,
             startcong <= congress,
             congress <= endcong)
    map <- map %>%
      leaflet::addPolygons(
        data = congress_sf,
        stroke = TRUE,
        smoothFactor = 1,
        color = "#222",
        opacity = 1,
        weight = 3,
        fill = NULL
      )
  }

  if (cities > 0) {
    decade <- round(as.integer(format(data$map_date, "%Y")) / 10) * 10
    decade <- unique(stats::na.omit(decade))
    if (decade < 1790L) decade <- 1790L
    city_locations <- USAboundaries::us_cities(map_date = decade) %>%
      dplyr::filter(state %in% state_to_filter,
                    population > 100) %>%
      dplyr::group_by(state) %>%
      dplyr::top_n(cities, population)

    if (nrow(city_locations) > 0) {
      map <- map %>%
        leaflet::addCircleMarkers(data = city_locations, lat = ~lat, lng = ~lon,
                                  stroke = TRUE, color = "#333", opacity = 1, weight = 1.5,
                                  fill = TRUE, fillColor = "#eaf945", fillOpacity = 1,
                                  radius = 5,
                                  label = ~city)
    }
  }

  map

}

# Get the colors from the data
poli_chrome <- function(df) {
  # The data frame will contain the percentages for various groups.
  df <- df %>%
    dplyr::select(dplyr::ends_with("_percentage")) %>%
    dplyr::mutate_all(replace_with_zero)

  party <- colnames(df)[max.col(df)] %>% stringr::str_replace("_percentage", "")
  percentage <- apply(df, 1, max)
  pal_mapping <- c("federalist" = "Greens", "republican" = "Purples",
                   "antifederalist" = "Oranges", "other" = "Reds")
  pals <- pal_mapping[party]

  pos <- cut(percentage, breaks = seq(0, 1, 0.2), labels = FALSE)
  out <- purrr::map2_chr(pals, pos, get_color)
  names(out) <- NULL
  out
}

# Takes an RColorBrewer palette and the position in that palette
get_color <- function(pal, i) {
  if (is.na(i)) return("#C7C7C7") # return grey for missing values
  RColorBrewer::brewer.pal(5, pal)[i]
}

popup_maker <- function(county, federalist, republican, other, fed_percent,
                        rep_percent, oth_percent) {
  paste0("<b>County: </b>", county, "<br>",
         "<b>Federalist: </b>", federalist, " (", fed_percent * 100, "%)<br>",
         "<b>Republican: </b>", republican, " (", rep_percent * 100, "%)<br>",
         "<b>Other: </b>", other, " (", oth_percent * 100, "%)<br>")
}
