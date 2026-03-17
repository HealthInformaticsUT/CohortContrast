# Convert a relative path to an absolute path

This internal function converts a given relative path into an absolute
path using the built-in \`normalizePath()\` function.

## Usage

``` r
convertToAbsolutePath(path)
```

## Arguments

- path:

  A string representing the file or directory path, which may be
  relative or absolute.

## Value

A string with the absolute path. If the file or directory does not
exist, \`normalizePath()\` will throw an error due to \`mustWork =
TRUE\`.

## Details

The function utilizes \`normalizePath()\` to handle both Windows and
Unix-based systems. It converts the provided path, even if it is
relative, into an absolute path. It ensures that paths are formatted
with forward slashes on Windows systems by setting \`winslash = "/"\`.

## Examples

``` r
if (FALSE) { # \dontrun{
  relative_path <- "relative/path/to/file.txt"
  absolute_path <- convertToAbsolutePath(relative_path)
  print(absolute_path)
} # }
```
