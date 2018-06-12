construct_url <- function(obj, epsg, query) {
  baseurl <- "https://openmaps.gov.bc.ca/geo/pub/{obj}/ows?service=WFS&version=2.0.0&request=GetFeature&typeName={obj}&SRSNAME=epsg:{epsg}&outputFormat=json{query}"

  if (!is.null(query)) {
    query <- paste0("&CQL_FILTER=", query)
  } else {
    query <- ""
  }
  URLencode(glue::glue(baseurl, obj = obj, epsg = epsg, query = query))
}

bcdc_map <- function(id, epsg = 3005, query = NULL, sf = TRUE) {
  url <- construct_url(id, epsg, query)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  res <- httr::GET(url, httr::write_disk(tmp))
  httr::stop_for_status(res)
  if (sf) {
    sf::st_read(tmp, quiet = TRUE)
  }
  # TODO: If sf, check for sf package, if not, read geojson to a dataframe...
}


#' Get EMS monitoring locations
#'
#' Get an `sf` object of EMS monitoring locations
#'
#' @param epsg The coordinate reference system
#' @param query Query string in ECQL syntax. Default is to only get monitoring
#' location types of 'River Stream or Creek' (`LOCATION_TYPE_CD='21'`). See the
#' [geoserver documentation](http://docs.geoserver.org/stable/en/user/filter/ecql_reference.html#filter-ecql-reference)
#'
#' @return an `sf` object
#' @export
#'
#' @examples
#' # Get river stream or creek locations with a purpose code of 2 (TREND):
#' ems_locations(query = "LOCATION_TYPE_CD='21' AND LOCATION_PURPOSE_CD='2'")
ems_locations <- function(epsg = 3005, query = "LOCATION_TYPE_CD='21'") {
  bcdc_map("WHSE_ENVIRONMENTAL_MONITORING.EMS_MONITORING_LOCN_TYPES_SVW",
           query = query)
}
