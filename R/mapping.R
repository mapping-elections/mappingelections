#' Map elections data
#'
#' @param data An \code{sf} object with elections data returned by
#'   \code{\link{join_to_spatial}}.
#' @param congress The number of the Congress. If \code{NULL}, it will be
#'   guessed from the data.
#' @param projection If not provided, then the best state plane projection will
#'   be guessed using the \code{\link[USAboundaries]{state_plane}} function from
#'   the \code{USAboundaries} package. If \code{NULL}, then leaflet's default
#'   Web Mercator projection will be used. To use a custom projection, provide a
#'   projection/CRS object returned by the \code{\link[leaflet]{leafletCRS}}
#'   function in the \code{leaflet} package.
#' @param state Override the guessing of the state from the data passed in.
#' @param congressional_boundaries Draw Congressional district boundaries in
#'   addition to county boundaries?
#' @param state_boundaries Draw state boundaries in addition to county
#'   boundaries?
#' @param cities Number of largest cities to draw. Pass \code{FALSE} to not draw
#'   any cities.
#' @param width The width of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#' @param height The height of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#'
#' @rdname map_elections
#'
#' @examples
#' map_data <- get_county_map_data("meae.congressional.congress19.nc.county")
#' map_counties(map_data)
#'
#' @importFrom dplyr ends_with
#' @export
map_counties <- function(data, congress = NULL, projection = NULL,
                         congressional_boundaries = TRUE,
                         state_boundaries = FALSE,
                         cities = 4L,
                         state = NULL, width = "100%", height = "600px") {

  stopifnot(is.logical(congressional_boundaries),
            is.numeric(cities) || cities == FALSE)

  if (is.null(state)) {
    statename_to_filter <- most_common_state(data$state_terr)
  } else {
    statename_to_filter <- state
  }
  state_to_filter <- USAboundaries::state_codes %>%
    dplyr::filter(state_name == statename_to_filter) %>%
    dplyr::pull(state_abbr)

  if (is.null(congress)) {
    congress <- unique(stats::na.omit(data$congress))[1]
  }

  if (congressional_boundaries) {
    # Get the Congressional data now if needed
    congress_sf <- histcongress %>%
      dplyr::filter(statename %in% statename_to_filter,
             startcong <= congress,
             congress <= endcong,
             district != -1)
  }

  # Calculate the bounding box. Sometimes the state/counties are bigger;
  # sometimes the Congressional district is bigger.
  bbox_counties <- as.list(sf::st_bbox(data))
  if (congressional_boundaries) {
    bbox <- as.list(sf::st_bbox(congress_sf))
    # Now get the biggest bounding box
    bbox$xmin <- min(bbox$xmin, bbox_counties$xmin)
    bbox$ymin <- min(bbox$ymin, bbox_counties$ymin)
    bbox$xmax <- max(bbox$xmax, bbox_counties$xmax)
    bbox$ymax <- max(bbox$ymax, bbox_counties$ymax)
  } else {
    # If we are not using Congressional boundaries, just use the counties boundaries
    bbox <- bbox_counties
  }
  # Now add a minimum padding based on the size of the state
  lng_pad <- max((bbox$xmax - bbox$xmin) * 0.15, 0.45)
  lat_pad <- max((bbox$ymax - bbox$ymin) * 0.15, 0.45)
  bbox$xmin <- bbox$xmin - lng_pad
  bbox$xmax <- bbox$xmax + lng_pad
  bbox$ymin <- bbox$ymin - lat_pad
  bbox$ymax <- bbox$ymax + lat_pad

  if (is.null(projection)) {
    # Use the state plane projection
    projection <- make_leaflet_crs(state_to_filter)
  } else {
    stopifnot(inherits(projection, "leaflet_crs"))
  }

  colors <- poli_chrome(dplyr::as_data_frame(data))

  # Instantiate the map with the data and the projection
  map <- leaflet::leaflet(data, width = width, height = height,
                          options = leaflet::leafletOptions(
                            crs = projection,
                            zoomControl = FALSE, dragging = TRUE,
                            minZoom = 7, maxZoom = 12
                            ))

  # Set the maximum bounds of the map
  map <- map %>%
    leaflet::setMaxBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)

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
      fillColor = colors,
      label = label_maker(leaflet::getMapData(map)),
      labelOptions = leaflet::labelOptions(direction = "auto"),
      popup = popup_maker(leaflet::getMapData(map))
      # popup = ~popup_maker(county = tools::toTitleCase(tolower(name)),
      #                      federalist = federalist_vote,
      #                      demrep = demrep_vote,
      #                      other = other_vote,
      #                      fed_percent = federalist_percentage,
      #                      demrep_percent = demrep_percentage,
      #                      oth_percent = other_percentage)
    )

  if (state_boundaries) {
    state_names <- USAboundaries::state_codes %>%
      dplyr::filter(state_abbr %in% state_to_filter)
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
        fill = FALSE
      )
  }

  if (congressional_boundaries) {
    map <- map %>%
      leaflet::addPolygons(
        data = congress_sf,
        stroke = TRUE,
        smoothFactor = 1,
        color = "#222",
        opacity = 1,
        weight = 3,
        fill = FALSE
      )
  }

  if (cities > 0) {
    decade <- round(as.integer(format(data$map_date, "%Y")) / 10) * 10
    decade <- unique(stats::na.omit(decade))
    if (decade < 1790L) decade <- 1790L
    city_locations <- USAboundaries::us_cities(map_date = decade) %>%
      dplyr::filter(state_abbr %in% state_to_filter,
                    population > 100) %>%
      dplyr::group_by(state_abbr) %>%
      dplyr::top_n(cities, population)

    if (nrow(city_locations) > 0) {
      map <- map %>%
        leaflet::addCircleMarkers(data = city_locations,
                                  stroke = TRUE, color = "#333", opacity = 1, weight = 1.5,
                                  fill = TRUE, fillColor = "#eaf945", fillOpacity = 1,
                                  radius = 5,
                                  label = ~city)
    }
  }

  map

}

#' Map the Congressional data for the nation
#'
#' @param data An \code{sf} object with elections data returned by
#'   \code{\link{join_to_spatial}}.
#' @param congress The number of the Congress. If \code{NULL}, it will be
#'   guessed from the data.
#' @param congressional_boundaries Draw Congressional district boundaries in
#'   addition to county boundaries?
#' @param state_boundaries Draw state boundaries in addition to county
#'   boundaries?
#' @param width The width of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#' @param height The height of the map in pixels or percentage. Passed on to
#'   \code{\link[leaflet]{leaflet}}.
#' @examples
#' map_data <- get_national_map_data("meae.congressional.congress01.national.county")
#' map_national(map_data)
#' @export
map_national <- function(data, congress = NULL,
                         congressional_boundaries = TRUE,
                         state_boundaries = FALSE,
                         width = "100%", height = "800px") {

  stopifnot(is.logical(congressional_boundaries))

  if (is.null(congress)) {
    congress <- unique(stats::na.omit(data$congress))[1]
  }

  if (congressional_boundaries) {
    # Get the Congressional data now if needed
    congress_sf <- histcongress %>%
      dplyr::filter(startcong <= congress,
                    congress <= endcong,
                    district != -1)
  }

  # Calculate the bounding box. Sometimes the state/counties are bigger;
  # sometimes the Congressional district is bigger.
  bbox_counties <- as.list(sf::st_bbox(data))
  if (congressional_boundaries) {
    bbox <- as.list(sf::st_bbox(congress_sf))
    # Now get the biggest bounding box
    bbox$xmin <- min(bbox$xmin, bbox_counties$xmin)
    bbox$ymin <- min(bbox$ymin, bbox_counties$ymin)
    bbox$xmax <- max(bbox$xmax, bbox_counties$xmax)
    bbox$ymax <- max(bbox$ymax, bbox_counties$ymax)
  } else {
    # If we are not using Congressional boundaries, just use the counties boundaries
    bbox <- bbox_counties
  }
  # Now add a minimum padding based on the size of the state
  lng_pad <- max((bbox$xmax - bbox$xmin) * 0.05, 0.40)
  lat_pad <- max((bbox$ymax - bbox$ymin) * 0.05, 0.40)
  bbox$xmin <- bbox$xmin - lng_pad
  bbox$xmax <- bbox$xmax + lng_pad
  bbox$ymin <- bbox$ymin - lat_pad
  bbox$ymax <- bbox$ymax + lat_pad

  projection <- leaflet::leafletCRS(
      crsClass = "L.Proj.CRS",
      code = "ESRI:102003",
      proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
      resolutions = 2^(20:0)
    )

  colors <- poli_chrome(dplyr::as_data_frame(data))

  # Instantiate the map with the data and the projection
  map <- leaflet::leaflet(data, width = width, height = height,
                          options = leaflet::leafletOptions(
                            crs = projection,
                            zoomControl = FALSE, dragging = TRUE,
                            minZoom = 7, maxZoom = 12
                            ))

  # Set the maximum bounds of the map
  map <- map %>%
    leaflet::setMaxBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)

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
      fillColor = colors,
      label = label_maker(leaflet::getMapData(map), states = TRUE),
      labelOptions = leaflet::labelOptions(direction = "auto"),
      popup = popup_maker(leaflet::getMapData(map))
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
  #       fill = FALSE
  #     )
  # }

  if (congressional_boundaries) {
    map <- map %>%
      leaflet::addPolygons(
        data = congress_sf,
        stroke = TRUE,
        smoothFactor = 1,
        color = "#222",
        opacity = 1,
        weight = 3,
        fill = FALSE
      )
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
  pals <- pal_mapping[party]

  pos <- cut(percentage, breaks = seq(0, 1, 0.2), labels = FALSE)
  out <- purrr::map2_chr(pals, pos, get_color)
  names(out) <- NULL
  out
}

# Takes an RColorBrewer palette and the position in that palette
get_color <- function(pal, i) {
  if (is.na(i)) return("#FFFFFF") # return white for missing values
  RColorBrewer::brewer.pal(5, pal)[i]
}

#' @importFrom stringr str_c
label_maker <- function(df, states = FALSE) {

  if (any(unique(df$state_abbr) %in% c("SC", "LA"))) {
    county_label <- ""
  } else {
    county_label <- " County"
  }

  labels <- vector("character", nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    county <- str_c(tools::toTitleCase(tolower(row$name)), county_label)
    if (!is.na(row$state_name)) {
      state <- str_c(", ", row$state_name)
    } else {
      state <- NULL
    }
    district_word <- ifelse(stringr::str_detect(row$district, ", "),
                            "Districts", "District")
    district <- str_c(district_word, " ", row$district)
    if (is.na(district)) {
      district <- NULL
      } else if (district == "District At-large") {
        district <- "At-large district"
      }
    if (states) {
      if (is.null(district)) {
        label <- str_c(county, state)
      } else {
        label <- sprintf("%s, %s / %s", county, state, district)
        label <- str_c(county, state, " / ", district)
      }
    } else {
      label <- str_c(county, district, sep = " / ")
    }
    labels[i] <- label
  }
  labels
}

#' @importFrom stringr str_c
popup_maker <- function(df) {

  if (any(unique(df$state_abbr) %in% c("SC", "LA"))) {
    county_label <- ""
  } else {
    county_label <- " County"
  }

  popups <- vector("character", nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    if (!is.na(row$state_name)) {
      state <- str_c(", ", row$state_name)
    } else {
      state <- NULL
    }
    county <- str_c("<b>", tools::toTitleCase(tolower(row$name)), county_label,
                    state, "</b><br/>")
    districts <- str_c("District: ", row$districts, "<br/>")
    if (is.na(districts)) districts <- NULL
    federalists <- votes_to_popup("Federalists", row$federalist_percentage,
                                  row$federalist_vote)
    antifeds <- votes_to_popup("Anti-Federalists", row$antifederalist_percentage,
                               row$antifederalist_vote)
    demreps <- votes_to_popup("Democratic-Republicans", row$demrep_percentage,
                               row$demrep_vote)
    chesapeake <- votes_to_popup("Chesapeake", row$chesapeake_percentage,
                               row$chesapeake_vote)
    potomac <- votes_to_popup("Potomac", row$potomac_percentage,
                               row$potomac_vote)
    repfacs <- votes_to_popup("Republican faction", row$repfac_percentage,
                               row$repfac_vote)
    adamsclay <- votes_to_popup("Adams/Clay supporters", row$adamsclay_percentage,
                                row$adamsclay_vote)
    jacksonian <- votes_to_popup("Jacksonian supporters", row$jacksonian_percentage,
                                 row$jacksonian_vote)
    caucus <- votes_to_popup("Caucus", row$caucus_percentage,
                                row$caucus_vote)
    anticaucus <- votes_to_popup("Anti-Caucus", row$anticaucus_percentage,
                                row$anticaucus_vote)
    others <- votes_to_popup("Unaffiliated or other parties", row$other_percentage,
                             row$other_vote)
    if (!is.na(row$county_source) && row$county_source == "district") {
      disclaimer <- "<br/><span class='county-disclaimer'>County-level returns are not available for this county, so party percentages for the district as a whole have been displayed.</span>"
    } else {
      disclaimer <- NULL
    }
    popup <- str_c(county, districts, federalists, antifeds, demreps,
                   adamsclay, jacksonian, caucus, anticaucus, repfacs,
                   chesapeake, potomac, others, disclaimer, sep = "\n")
    popups[i] <- popup
  }
  popups
}

#' @importFrom stringr str_c
votes_to_popup <- function(party, percentage, vote) {
  if (is.na(percentage)) return(NULL)
  out <- str_c(party, ": ", round(percentage * 100, 1), "%")
  if (!is.na(vote)) {
    out <- str_c(out, " (", prettyNum(vote, big.mark = ","), " votes)")
  }
  out <- str_c(out, "<br/>")
  out
}

# Make a leaflet CRS from a state name
make_leaflet_crs <- function(state) {
    epsg <- USAboundaries::state_plane(state, type = "epsg")
    proj4 <- USAboundaries::state_plane(state, type = "proj4")
    leaflet::leafletCRS(
      crsClass = "L.Proj.CRS",
      code = sprintf("ESRI:%s", epsg),
      proj4def = proj4,
      resolutions = 2^(20:0)
      )
}

# Mapping of parties to palettes
pal_mapping <- c("federalist" = "Greens",
                 "antifederalist" = "Oranges",
                 "demrep" = "Purples",
                 "repfac" = "Oranges",
                 "adamsclay" = "Reds",
                 "jacksonian" = "Blues",
                 "other" = "RdPu",
                 "potomac" = "Blues",
                 "chesapeake" = "Reds",
                 "caucus" = "Reds",
                 "anticaucus" = "Blues")
