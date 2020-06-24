#' @import httr jsonlite

.pool <- new.env(parent=emptyenv())

.handle <- function(new=FALSE, token=Sys.getenv("CANVAS_API_TOKEN")) {
    if (new || is.null(.pool$handle)) {
        h <- new_handle()
        curl::handle_setheaders(h, Authorization=paste("Bearer", token))
        if (!new)
            .pool$handle <- h
        h
    } else .pool$handle
}

.canvas <- function(method="GET", path, ...,
                    headers=NULL,
                    token=Sys.getenv("CANVAS_API_TOKEN"),
                    api.url=Sys.getenv("CANVAS_API_DOMAIN")) {
    h <- .handle(token=token)
    if (headers) handle_
    handle_setopt(handle, .list = list(...))
}

#' @rdname API
#' @description \code{.hc} is the main function performing Canvas LMS REST requests. It returns the \code{httr::response()} object. The result is also stored in an internal variable which can be retrieved with \code{last.result()}.
.hc <- function(method="GET", path, ...,
                headers=NULL, handle = NULL,
                token=Sys.getenv("CANVAS_API_TOKEN"),
                api.url=Sys.getenv("CANVAS_API_DOMAIN")) {
    if (is.list(path)) path <- paste(as.character(path), collapse='/')
    if (!nzchar(token) || !nzchar(api.url)) {
        stop("invalid token or API-URL. Please set CANVAS_API_TOKEN and CANVAS_API_DOMAIN environment varaibles.")
    }

    url <- paste(api.url, path, sep='/')
    hu <- httr:::handle_url(handle, url, ...)
    config <- add_headers(Authorization=paste("Bearer", token))
    req <- httr:::request_build(method, hu$url, config, ...)
    .pool$last <- httr:::request_perform(req, hu$handle$handle)
}

.chk <- function(r, desc="Canvas request failed", valid.code=200L, flatten=TRUE) {
    if (!(status_code(r) %in% valid.code))
        stop(errorCondition(desc, res=r, class="httrRequestError"))
    tryCatch(fromJSON(rawToChar(r$content), flatten=flatten),
             error=function(e) stop(errorCondition("Parsing of the request result failed",
                                                   res=r, class="parseError")))
}

#' @rdname API
#' @export
#' @param path string, API path (e.g. \code{"/courses"})
#' @param ... low-level arguments passed to \code{.hc}
#' @param autopage logical, if \code{TRUE} the multi-page results are automatically collected and merged into one. If \code{FALSE} then only a signle request is done but \code{max} is respected. If \code{NA} then \code{path} is sent-as is without any attempt to manage pagination.
#' @param max integer, maximum number of items to retrieve per request. Ignored if \code{autopage=NA}, otherwise maps to the \code{pre_page=} query string.
#' @param flatten logical, if \code{TRUE} then nested tables are merged into one final one. Note that auto-pagination requires \code{flatten=TRUE} if there are nested tables as it is not possible to merge nested tables from multiple pages.
#' @param method string, HTTP request method
.api <- function(path, ..., max=100L, autopage=TRUE, flatten=TRUE, method="GET") {
    if (is.na(autopage)) return(.chk(.hc(method, path, ...), flatten=flatten))
    pre.q <- gsub("\\?.*", "", path)
    q <- if (pre.q == path) "" else gsub("^[^?]+\\?", "", path)
    pg.path <- paste0(path, if (nchar(q)) "&" else "?", "per_page=", max)
    r0 <- .hc(method, pg.path, ...)
    if (!autopage || status_code(r0) != 200L) return(.chk(r0, flatten=flatten))

    ## FIXME: This is not the official way, we just want to avoid
    ## the brain-dead API which doesn't even provide the "next" link
    ## reliably, but they do always provide "last" so we just
    ## iterate from 1 to last by detecting the last page number
    ## in the "last" link
    links <- strsplit(r0$headers$link, ",", TRUE)[[1]]
    ll <- links[grep("rel=.last.", links)]
    lp <- ""
    if (length(ll)) {
        lp <- gsub(".*[^_]page=(\\d+).*", "\\1", ll)
        if (lp != ll) {
            lp <- as.integer(lp)
        }
    }
    ## carry on only if we found a valid last page number that is > 1
    if (is.integer(lp) && !is.na(lp) && lp > 1L) {
        l <- vector("list", lp)
        l[[1]] <- .chk(r0, flatten=flatten)
        for (page in 2:lp)
            l[[page]] <- .chk(.hc(method, paste0(pg.path, "&page=", page), ...), flatten=flatten)
        ## check if there are nested data frames since that won't work
        if (!flatten &&
            any(sapply(l, function(df) any(sapply(df, is.data.frame)))))
                stop(errorCondition("At least one of returned parts contains nested data frames, cannot combine paginated result in that case unless flatten=TRUE is used.", parts=l, class="pagedCombineError"))

        do.call(rbind, l)
    } else .chk(r0, flatten=flatten)
}

#' @title Manage courses
#'
#' @description List all accessible courses
#' @export
courses <- function()
    .api("courses")

#' @title Manage course modules
#'
#' @description \code{modules} lists all modules in a course
#'
#' @param course, integer, course-id
#' @param ... any futher options to \code{\link{.api}}
#' @export
modules <- function(course, ...)
    .api(file.path("courses", course, "modules"), ...)

#' @description List all items contained in a module
#'
#' @rdname modules
#' @export
module.items <- function(course, module, max=100)
    .api(file.path("courses", course, "modules", module, "items"), ...)

#' @title API-related functions
#' @name API
#'
#' @description \code{last.request} returns the last Canvas HTTPS request and result. Mostly useful for debugging.
#' 
#' @export
last.request <- function() .pool$last

