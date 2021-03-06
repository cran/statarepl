% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R6.R
\name{StataSession}
\alias{StataSession}
\title{Stata Session}
\description{
Launch a Stata Session
}
\examples{

## ------------------------------------------------
## Method `StataSession$new`
## ------------------------------------------------

\dontrun{
n <- StataSession$new()
n$eval("display 1 + 1")
}

## ------------------------------------------------
## Method `StataSession$eval`
## ------------------------------------------------

\dontrun{
n <- StataSession$new()
n$eval("display 1 + 1")
}

## ------------------------------------------------
## Method `StataSession$state`
## ------------------------------------------------

\dontrun{
n <- StataSession$new()
n$state()
n$kill()
n$state()
}

## ------------------------------------------------
## Method `StataSession$kill`
## ------------------------------------------------

\dontrun{
n <- StataSession$new()
n$kill()
n$state()
}

## ------------------------------------------------
## Method `StataSession$terminate`
## ------------------------------------------------

\dontrun{
n <- StataSession$new()
n$terminate()
n$state()
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{bin}}{Path to Stata bin directory.}

\item{\code{handle}}{A handle as returned by \link[subprocess]{spawn_process}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{StataSession$new()}}
\item \href{#method-finalize}{\code{StataSession$finalize()}}
\item \href{#method-eval}{\code{StataSession$eval()}}
\item \href{#method-state}{\code{StataSession$state()}}
\item \href{#method-kill}{\code{StataSession$kill()}}
\item \href{#method-terminate}{\code{StataSession$terminate()}}
\item \href{#method-clone}{\code{StataSession$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StataSession$new(bin = NULL, params = "-q")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{bin}}{Path to Stata bin directory, if \code{NULL} then statarepl
will set it to \code{getOption("RStata.StataPath", stop("You need to set up a Stata path; ?find_stata"))}, if you don't have \code{options("RStata.StataPath")} setted, you can search and set it use \code{\link{find_stata}}}

\item{\code{params}}{Additional parameters to pass to the initialisation.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Initialise a Stata session
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
n <- StataSession$new()
n$eval("display 1 + 1")
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-finalize"></a>}}
\subsection{Method \code{finalize()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StataSession$finalize()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Terminate a Stata session
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-eval"></a>}}
\subsection{Method \code{eval()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StataSession$eval(code, wait = TRUE, print = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{code}}{The code to evaluate.}

\item{\code{wait}}{Whether to re-attempt to evaluate if it first fails.}

\item{\code{print}}{Whether to print the result to the R console.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Evaluate Stata code
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
n <- StataSession$new()
n$eval("display 1 + 1")
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-state"></a>}}
\subsection{Method \code{state()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StataSession$state()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Retrieve Stata state
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
n <- StataSession$new()
n$state()
n$kill()
n$state()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-kill"></a>}}
\subsection{Method \code{kill()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StataSession$kill()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Kill Stata
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
n <- StataSession$new()
n$kill()
n$state()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-terminate"></a>}}
\subsection{Method \code{terminate()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StataSession$terminate()}\if{html}{\out{</div>}}
}

\subsection{Details}{
Terminate Stata
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
n <- StataSession$new()
n$terminate()
n$state()
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StataSession$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
