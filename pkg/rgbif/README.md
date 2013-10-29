# `rgbif`

<!-- [![Build Status](https://api.travis-ci.org/ropensci/rgbif.png)](https://travis-ci.org/ropensci/rgbif) -->

## About
This package gives you access to data from [GBIF](http://www.gbif.org/) via their API.

## Transitioning to the new GBIF API

*Note that development is rapid at this point so expect changes, but changes should slow down with time...*

+ The old GBIF API
	+ See documentation here: [http://data.gbif.org/tutorial/services](http://data.gbif.org/tutorial/services)
	+ is accessible from the `master` branch of this repo, check it out there if you want that. 
+ The new GBIF API
	+ See documentation here: [http://dev.gbif.org/wiki/display/POR/Webservice+API](http://dev.gbif.org/wiki/display/POR/Webservice+API)
	+ Is being developed on the `newapi` branch of this repo.

The rgbif branch `newapi` will have development code to use the new GBIF API, and will be folded into the master branch at some later point. You can use functions to get data via the new GBIF API and old GBIF API in the `newapi` branch. The functions for the old GBIF API give deprecation messages, signaling that they are on their way out. A future version of `rgbif` will remove functions for the old API, so do transition to the functions for the new API soon.

The new GBIF API only uses JSON as a data format - no more XML. Of course this probably doesn't matter to you unless you are a hacker...

Changes in the new GBIF API from last with respect to `rgbif`:

| Function in old API | New API | Notes |
| ------ | ------- | ------ |
| country_codes | same | none |
| density_spplist | deprecated | service not provided anymore |
| densitylist | deprecated | service not provided anymore |
| gbifdata | deprecated | not needed |
| gbifmap_dens | deprecated | none |
| gbifmap_list | deprecated | none |
| is.gbifdens | deprecated | none |
| is.gbiflist | deprecated | none |
| networks | same | some parameters differ |
| occurrencecount | occ_count | some parameters differ |
| occurrencedensity | deprecated | service not provided anymore |
| occurrenceget | occ_get | none |
| occurrencelist | occ_search | none |
| occurrencelist_all | occ_search | none |
| occurrencelist_many | occ_search | none |
| providers | deprecated | see note 1 |
| resources | deprecated | see note 1 |
| stylegeojson | same | not implemented yet |
| taxoncount | deprecated | See ?occ_count |
| taxonget | deprecated | See ?name_lookup |
| taxonsearch | deprecated | See note 2 |
| taxrank | same | none |
| togeojson | same | not implemented yet |

* Note 1: See ?datasets, ?networks, ?nodes, and ?organizations
* Note 2: See ?name_lookup for names across all of GBIF and ?gbif_lookup for names only in the GBIF backbone taxonomy

## Install

### Install the development version on the `newapi` branch using `install_github`.

```R
install.packages("devtools")
require(devtools)

install_github("rgbif", "ropensci", ref="newapi")
require(rgbif)
```

Note: Windows users have to first install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).

### Search for occurrence data

#### A single species. Get the taxonKey first with `gbif_lookup`.

```coffee
key <- gbif_lookup(name='Helianthus annuus', kingdom='plants')$speciesKey

occ_search(taxonKey=key, limit=20, return='data')

                name  longitude latitude
1  Helianthus annuus   16.42280 56.57660
2  Helianthus annuus -116.99648 32.84967
3  Helianthus annuus   15.11370 56.22530
4  Helianthus annuus   15.85090 57.47840
5  Helianthus annuus   12.95860 57.72000
6  Helianthus annuus   15.50840 58.39800
7  Helianthus annuus   16.47670 56.65180
8  Helianthus annuus   14.61910 56.05580
9  Helianthus annuus   16.48530 58.60580
10 Helianthus annuus    3.58007 50.89776
11 Helianthus annuus    5.31755 51.05837
12 Helianthus annuus   15.55870 59.42630
13 Helianthus annuus   16.25400 56.67720
14 Helianthus annuus   15.50140 58.50290
15 Helianthus annuus   21.21820 64.90890
16 Helianthus annuus -118.22911 34.08994
17 Helianthus annuus   14.66350 58.23940
18 Helianthus annuus   16.42290 56.57660
19 Helianthus annuus   16.44190 58.60620
20 Helianthus annuus   16.41930 56.57470
```

#### Search for many species. Get the keys first with `gbif_lookup`, then pass to `occ_search`

```coffee
splist <- c('Accipiter erythronemius', 'Junco hyemalis', 'Aix sponsa')
keys <- sapply(splist, function(x) gbif_lookup(name=x, kingdom='plants')$speciesKey, USE.NAMES=FALSE)

occ_search(taxonKey=keys, limit=5, return='data', georeferenced=TRUE)

$`2480598`
                     name longitude   latitude
1 Accipiter erythronemius -60.73333   5.266667
2 Accipiter erythronemius -60.73333   5.266667
3 Accipiter erythronemius -60.73333   5.266667
4 Accipiter erythronemius -59.01670 -31.133300
5 Accipiter erythronemius -64.78353 -19.563531

$`2492010`
            name  longitude latitude
1 Junco hyemalis  -78.42087 38.16281
2 Junco hyemalis -116.84687 32.89224
3 Junco hyemalis -122.05739 36.95236
4 Junco hyemalis  -97.28027 32.88511
5 Junco hyemalis -111.82421 40.76623

$`2498387`
        name  longitude latitude
1 Aix sponsa -116.91708 32.85833
2 Aix sponsa  -81.90983 40.55409
3 Aix sponsa   16.37410 56.66830
4 Aix sponsa -120.47935 37.31153
5 Aix sponsa  -76.70940 38.74238
```

### Maps

#### Make a simple map of species occurrences. 

```coffee
splist <- c('Cyanocitta stelleri', 'Junco hyemalis', 'Aix sponsa')
keys <- sapply(splist, function(x) gbif_lookup(name=x, kingdom='plants')$speciesKey, USE.NAMES=FALSE)
dat <- occ_search(taxonKey=keys, limit=100, return='data', georeferenced=TRUE)
library(plyr)
datdf <- ldply(dat)
gbifmap(datdf)
```

![](inst/assets/img/gbifmap.png)

<br><br>

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)