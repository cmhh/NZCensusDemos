# New Zealand Census Meshblock Data Demos

A package which provides demos using the data provided by the package
[`NZCensus`](https://github.com/cmhh/NZCensus).

The package includes `SpatialPolygonDataFrame`s which were created from
shapefiles found:

[Statistics New Zealand - Geographic boundary files](http://www.stats.govt.nz/browse_for_stats/Maps_and_geography/Geographic-areas/digital-boundary-files.aspx)

These files are relatively large, and this is largely the reason
[`NZCensus`](https://github.com/cmhh/NZCensus) and
[`NZCensusDemos`](https://github.com/cmhh/NZCensusDemos) weren't provided as a single,
combined package.


## Installation

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('cmhh/NZCensus')
devtools::install_github('cmhh/NZCensusDemos')
```

## Usage

This package currently provides 2 functions: `mapdemo` and `distributiondemo`:

![`NZCensusDemos::mapdemo()`](img/mapdemo.png)

![`NZCensusDemos::mapdemo()`](img/distributiondemo.png)


## License

This package is licensed to you under the terms of the [GNU General Public
License](http://www.gnu.org/licenses/gpl.html) version 3 or later.

Copyright 2013-2015 Chris Hansen.
