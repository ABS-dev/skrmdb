# Example Titration Data

A data set that illustrates the dichotomous data format for titration
data as found in the CVB Data Guide. The data are intentionally
incomplete and nonmonotonic.

## Usage

``` r
titration
```

## Format

A `data.frame` with 67 rows and 9 columns.

- `testID`: Mandatory. A test identifier that is unique within table.

- `PrepID`: Mandatory. The identifier for the preparation used. This
  will usually be a vaccine lot or serial number.

- `PrepRole`: Mandatory. The role of the preparation. This must be
  "reference", "test", or "other".

- `Date`: Optional. The date the test was performed.

- `Vial`: Optional. The vial number tested.

- `Operator`: Optional. The operator who performed the test.

- `dil`: Mandatory. The dilution used in a well.

- `positive`: Mandatory. The total number of positive readings (tubes or
  wells) affected by the challenge.

- `total`: Mandatory. The total number of tubes or wells in a group at
  the specified dilution.
