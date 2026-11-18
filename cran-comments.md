## Submission

This is a new submission.

(Draft, kept up to date before submission: the version is still a
development version, 0.0.1.90xx, which is the "large components" part of
the NOTE below; it becomes a release version when submitted.)

## Test environments

* local Ubuntu 24.04, R 4.6.1, with every Suggests installed, and again
  with altarr and tabularberg not installed and NOT_CRAN unset
* GitHub Actions, Ubuntu (GDAL release and GDAL development images)
* to do before submission: win-builder (R-devel, R-release), macOS builder

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* Suggests or Enhances not in mainstream repositories: altarr, tabularberg.
  Both are on https://hypertidy.r-universe.dev, given in
  Additional_repositories. They are optional: altarr gives lazy arrays
  (`zaro_array()`) and tabularberg reads Icechunk repositories
  (`zaro_icechunk()`). Both are used only after `requireNamespace()`, with
  an error saying how to install them; examples that use them run only
  when they are installed (`@examplesIf requireNamespace(...)`) and tests
  that use them `skip_if_not_installed()`. The package passes
  `R CMD check --as-cran` with neither installed.

## Notes

* Examples read a small Zarr store shipped in `inst/extdata` (12 KB).
  Examples that read public cloud stores need network access and run only
  in interactive sessions.
* Tests that need network access, a local Python HTTP server or
  zarr-python are skipped on CRAN.
