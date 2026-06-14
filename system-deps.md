# System Dependencies

This file documents the mapping between Linux system libraries and the R packages
that require them. Used as a reference for maintaining the `Dockerfile`.

## Current dependencies

| System library         | Required by R package(s)              | Purpose                          |
|------------------------|---------------------------------------|----------------------------------|
| `libfontconfig1-dev`   | `systemfonts`, `ragg`                 | Font discovery and configuration |
| `libfreetype6-dev`     | `systemfonts`, `ragg`, `ggplot2`      | Font rendering                   |
| `libpng-dev`           | `ragg`, `png`                         | PNG image read/write             |
| `libicu-dev`           | `stringi`                             | Unicode and string encoding      |
| `libcurl4-openssl-dev` | `curl`, `httr`, `httr2`               | HTTP requests                    |
| `libssl-dev`           | `openssl`, `httr`, `curl`             | TLS/SSL support                  |
| `cmake`                | `nloptr`, packages with C++ deps      | C++ build system                 |
| `make`                 | General R package compilation         | Build toolchain                  |
| `pandoc`               | `rmarkdown`, `knitr`                  | Document rendering               |


