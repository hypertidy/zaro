# Articles (precomputed)

The site's articles read live data (AODN on S3, BRAN2023 on NCI THREDDS,
EOPF Sentinel-2 on Copernicus S3), which CI may not reach. So each article
is written as `<name>.Rmd.orig`, knitted locally into `<name>.Rmd` with its
outputs and figures (`figures/`), and pkgdown renders the knitted `.Rmd`
without running anything.

To refresh after a change, from this directory, with zaro and ndr
installed and network access:

```r
for (f in list.files(pattern = "[.]Rmd[.]orig$")) {
  knitr::knit(f, sub("[.]orig$", "", f))
}
```

`connecting.Rmd` runs no code (its examples are shown, not run), so it
has no `.orig`; edit it directly.

Then check the knitted files for errors (`grep "#> Error" *.Rmd`) and
commit the `.Rmd.orig`, the `.Rmd` and `figures/`. This directory is
build-ignored, so `R CMD check` doesn't see it.
