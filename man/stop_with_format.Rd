% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Helpers.R
\name{stop_with_format}
\alias{stop_with_format}
\title{Stop with Formatted and Translated Error Message}
\usage{
stop_with_format(fmt, ..., domain = "R-data.table")
}
\arguments{
\item{fmt}{A character string providing a formatting template for the message.
This string can include placeholders that will be filled with data from
additional arguments passed to `...`.}

\item{...}{Additional arguments to be used for formatting the message string.
These are typically values or variables that are inserted into the `fmt`
template.}

\item{domain}{Character string specifying the translation domain. This is useful
for packages that provide translations for their messages. Defaults to
"R-data.table", which should be changed to reflect the domain of your package
if used for translation.}
}
\value{
This function does not return a value; it interrupts function execution
  by throwing an error with a formatted and possibly translated message.
}
\description{
This function stops execution by throwing an error with a formatted message.
The message can be translated based on the specified domain. This is typically
used within a package to standardize error messages and potentially support
multilingual error outputs.
}
\keyword{internal}
