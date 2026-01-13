# Setup PyTorch with GPU Support

Setup PyTorch with correct GPU support

## Usage

``` r
setup_torch(envname = "tabpfn", force_gpu = FALSE, cuda_version = NULL)
```

## Arguments

- envname:

  Name of the virtual environment

- force_gpu:

  If TRUE, forces GPU installation even if not detected

- cuda_version:

  CUDA version to install (default: NULL for auto-detect)

## Value

NULL (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
# Setup torch with GPU support
setup_torch()

# Force GPU installation
setup_torch(force_gpu = TRUE)
} # }
```
