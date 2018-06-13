# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

construct_url <- function(obj, epsg, query) {
  baseurl <- "https://openmaps.gov.bc.ca/geo/pub/{obj}/ows?service=WFS&version=2.0.0&request=GetFeature&typeName={obj}&SRSNAME=epsg:{epsg}&outputFormat=json{query}"

  if (!is.null(query)) {
    query <- paste0("&CQL_FILTER=", query)
  } else {
    query <- ""
  }
  URLencode(glue::glue(baseurl, obj = obj, epsg = epsg, query = query))
}

bcdc_map <- function(id, epsg = 3005, query = NULL, sf) {
  url <- construct_url(id, epsg, query)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  res <- httr::GET(url, httr::write_disk(tmp))
  httr::stop_for_status(res)
  if (sf) {
    return(sf::st_read(tmp, quiet = TRUE))
  } else {
    res <- jsonlite::fromJSON(tmp)
    ret <- res$features$properties
    attr(ret, "geometry_type") <- unique(res$features$geometry$type)
    attr(ret, "crs") <- res$crs$properties$name
    ret
  }
  # TODO: look at using crul for paging
}


#' Get EMS monitoring locations
#'
#' Get an `sf` object of EMS monitoring locations
#'
#' @param epsg The coordinate reference system
#' @param query Query string in ECQL syntax. Default is to only get monitoring
#' location types of 'River Stream or Creek' (`LOCATION_TYPE_CD='21'`). See the
#' [geoserver documentation](http://docs.geoserver.org/stable/en/user/filter/ecql_reference.html#filter-ecql-reference)
#' @param sf should an `sf` spatial object be returned (default `TRUE`),
#' or just a `data.frame`?
#'
#' @return an `sf` object if `sf = TRUE`, otherwise a data.frame of the attributes
#' @export
#'
#' @examples
#' # Get river stream or creek locations with a purpose code of 2 (TREND):
#' ems_locations(query = "LOCATION_TYPE_CD='21' AND LOCATION_PURPOSE_CD='2'")
ems_locations <- function(epsg = 3005, query = "LOCATION_TYPE_CD='21'", sf = TRUE) {
  if (sf && !requireNamespace("sf", quietly = TRUE)) {
    stop("Package sf required", call. = FALSE)
  }
  bcdc_map("WHSE_ENVIRONMENTAL_MONITORING.EMS_MONITORING_LOCN_TYPES_SVW",
           query = query, sf = sf)
}
