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
  bcdc_map(ems_loc_object_name(), query = query, sf = sf,
           sort = "MONITORING_LOCATION_ID")
}

#' Get the data field names and types for the EMS monitoring locations
#'
#' This can help you know what to query in [ems_locations()]
#'
#' @return A data.frame of column names and their properties
#' @export
ems_location_features <- function() {
  cli <- make_client()

  res <- cli$get(path = sprintf("geo/pub/%s/ows", ems_loc_object_name()),
                 query = list(service = "WFS",
                              version = "2.0.0",
                              request = "describeFeatureType",
                              typeName = ems_loc_object_name(),
                              outputFormat = "application/json"))

  res$raise_for_status()

  jsonlite::fromJSON(res$parse())[["featureTypes"]][["properties"]][[1]]
}

bcdc_map <- function(id, epsg = 3005, query = NULL, sf, sort) {
  cli <- make_client()

  cli_pag <- crul::Paginator$new(cli,
                                 by = "query_params",
                                 limit_param = "count",
                                 offset_param = "startIndex",
                                 limit = 100000,
                                 limit_chunk = 1000)

  query_params <- list(service = "WFS",
                       version = "2.0.0",
                       request = "GetFeature",
                       typeName = id,
                       SRSNAME = sprintf("epsg:%s", epsg),
                       outputFormat = "json",
                       sortBy = sort,
                       cql_filter = query)

  cli_pag$get(path = sprintf("geo/pub/%s/ows", id),
              query = Filter(Negate(is.null), query_params))

  status <- cli_pag$status_code()

  if (all(status >= 300)) {
    stop("All API calls failed")
  } else if (any(status >= 300)) {
    warning("Some API calls failed")
  }

  parsed_response <- cli_pag$parse()

  if (sf) {
    parsed_response_to_sf(parsed_response[status < 300])
  } else {
    parsed_response_to_df(parsed_response[status < 300])
  }
}


ems_loc_object_name <- function()
  "WHSE_ENVIRONMENTAL_MONITORING.EMS_MONITORING_LOCN_TYPES_SVW"

make_client <- function()
  crul::HttpClient$new("https://openmaps.gov.bc.ca",
                       progress = httr::progress("down"))

parsed_response_to_sf <- function(x) {
  ret_list <- lapply(x, safe_st_read)

  to_bind <- vapply(ret_list, function(x) {
    !is.null(x) && inherits(x, "sf") && nrow(x) > 0
  }, FUN.VALUE = logical(1))

  do.call(rbind, ret_list[to_bind])
}

safe_st_read <- function(x, ...) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf package required to return an sf object")
  }
  tryCatch(sf::st_read(x, stringsAsFactors = FALSE, quiet = TRUE, ...),
           error = function(e) NULL)
}

parsed_response_to_df <- function(x) {
  ret_list <- lapply(x, function(y) {
    jsonlite::fromJSON(y)[["features"]][["properties"]]
  })

  to_bind <- vapply(ret_list, function(x) {
    !is.null(x) && inherits(x, "data.frame") && nrow(x) > 0
  }, FUN.VALUE = logical(1))

  do.call(rbind, ret_list[to_bind])
}
