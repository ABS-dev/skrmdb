#' Example titration data.
#'
#' A dataset that illustrates the dichotomous data format for titration data as
#' found in the CVB DATA Guide.  The data is purposely incomplete and not-
#' monotonic.
#'
#' @format A data.frame with 67 rows and 9 columns
#' * `testID`: Mandatory. A test identifier that is unique in the table.
#' Every test must have a test identifier.
#' * `PrepID`: Mandatory. The identifier of the preparation used. This
#' will usually be a lot or serial number of a vaccine.
#' * `PrepRole`: Mandatory. The role of the preparation. This must be
#' reference, test, or other.
#' * `Date`: Optional. The Date the test was performed.
#' * `Vial`: Optional. The vial number tested.
#' * `Operator`: Optional. The operator who performed the test.
#' * `dil`: Mandatory. The dilution used in a well.
#' * `positive`: Mandatory. Total number of positive readings (tubes or
#' wells) affected by challenge.
#' * `total`: Mandatory. The total number of tubes or wells in a group
#' (treatment group or at a specified dilution).
"titration"
