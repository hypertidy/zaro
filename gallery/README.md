# Store gallery

Public stores that zaro is checked against: one small read from each, every
week (`.github/workflows/gallery.yaml`, Tuesdays, or by hand from the
Actions tab). This replaces the gallery tests that were in
`tests/testthat/`; those failed as a whole whenever any one host was down.

- `stores.R`: the stores, one entry each (what it exercises, how to open
  it, one read and its expected type and shape).
- `run.R`: reads each one and reports it as

  | result | meaning | on Actions |
  |---|---|---|
  | ok | the read gave the expected type and shape | |
  | unreachable | a network-level failure (timeout, DNS, refused connection, TLS, HTTP 5xx): the host's problem | warning |
  | failed | anything else: zaro broke, or the store changed (moved, removed, made private, restructured) and its entry needs updating | error, the job fails |

  The table goes to the run summary.

Run it locally, with zaro and its Suggests installed:

```sh
Rscript gallery/run.R                       # every store
Rscript gallery/run.R cmems-time cmip6-gcs  # some of them
```

To add a store, add an entry to `stores.R` that exercises something the
others don't (a transport, a format version, a codec, a reference
kind), with a read small enough to take a few seconds. A store that
stays unreachable for weeks should be removed: Pangeo's GPCP on the OSN
NCSA pod (`ncsa.osn.xsede.org`) was dropped for that in November 2026.

This directory is build-ignored: it is not part of the package.
