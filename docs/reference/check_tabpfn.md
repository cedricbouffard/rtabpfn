# Check TabPFN Installation

Helper function to check if TabPFN is installed in the Python
environment and optionally install it if missing.

## Usage

``` r
check_tabpfn(install = FALSE, envname = "tabpfn", method = "auto")
```

## Arguments

- install:

  Logical. If TRUE, will attempt to install TabPFN if not found.

- envname:

  Name of the virtual environment to use

- method:

  Installation method: "auto", "virtualenv", or "conda"

## Value

Logical indicating if TabPFN is available

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if TabPFN is available
check_tabpfn()

# Install if not available
check_tabpfn(install = TRUE)
} # }
```
