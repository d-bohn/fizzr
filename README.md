fizzr: A tidy port of PhysioScripts
================

fizzr
=====

***DISCLAIMER*** Very much a work in progress!

The `fizzr` package (*phys*iological analysis in *r*) is a tidy implementation/port of the `PhysioScripts` suite of functions, originally published by Christie and Gianaros (2013). Further documentation and relevant publication can be found online in the journal [*Behavioral Research Methods*](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3767411/).

Installing
----------

    devtools::install_github('d-bohn/fizzr')
    library(fizzr)

Browsing
--------

Each function is being re-evaluated for 'tidyness', and then ported (slowly) to this package. If you would like to load the `PhysioScripts` suite of functions to browse all of the functions that will be available once this package is finished, you can do so by:

    library(fizzr)
    load_physioscripts()

Currently Imported Functions
----------------------------

So far, the following functions are be called directly from the package without having to load the `PhysioScripts` suite:

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
fizzr Function
</th>
<th style="text-align:left;">
PhysioScripts Function
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
process\_ecg()
</td>
<td style="text-align:left;">
process.ecg()
</td>
</tr>
<tr>
<td style="text-align:left;">
extract\_ibi()
</td>
<td style="text-align:left;">
extract.ibi()
</td>
</tr>
<tr>
<td style="text-align:left;">
read\_vars()
</td>
<td style="text-align:left;">
read.vars()
</td>
</tr>
<tr>
<td style="text-align:left;">
read\_data()
</td>
<td style="text-align:left;">
read.data()
</td>
</tr>
</tbody>
</table>
Motivation and Rationale
========================

The current motivation for porting these functions to a new package is three-fold. First, `PhysioScripts` is no longer maintained by the original author(s). Providing appropriate documentation and package structure will allow for the community to easily add to, modify, and continue development on this package.

Second, `R` lacks an intuitive and straightforward package to do physiological data pre-processing. While the functions provided by `PhysioScripts` are meant to be an end-to-end solution, most of the currently ported functions are used for pre-processing heart rate data. The pre-processed data can then passed to other packages that require cleaned data (e.g., [`RHRV`](http://rhrv.r-forge.r-project.org/)).

Finally, porting code that you did not write yourself is an excellent learning opportunity for working with second-hand code and package building. Creating a package of functions from original code that both maintains its functionality while also fitting into a larger ecosystem (e.g., the tidyverse) is a great learning exercise. One immediate change from `PhysioScripts` that I made was renaming functions to fit the "underscore" naming convention often used in the tidy ecosystem. This convention is desirable over the "period" naming convention for a number of reasons, one of which being that periods are used in `python` to denote "within", much like the `$` operator in `R`.

References
==========

Christie, Israel C, and Peter J Gianaros. 2013. “PhysioScripts: An Extensible, Open Source Platform for the Processing of Physiological Data.” *Behavior Research Methods* 45 (1). Springer: 125–31. <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3767411/>.
