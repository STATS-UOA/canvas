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

.chk <- function(r, desc="Canvas request failed", valid.code=200L) {
    if (status_code(r) != 200)
        stop(errorCondition(desc, res=r, class="httrRequestError"))
    tryCatch(fromJSON(rawToChar(r$content)),
             error=function(e) stop(errorCondition("Parsing of the request result failed",
                                                   res=r, class="parseError")))
}

#' @title Manage courses
#'
#' @description List all accessible courses
#' @export
courses <- function()
    .chk(.hc(, "courses"))

#' @title Manage course modules
#'
#' @description \code{modules} lists all modules in a course
#'
#' @param course, integer, course-id
#' @param max integer, maximum number of entries. Note that pagination is currently not supported and Canvas limits the value of \code{max}, empirically at 100.
#' @export
modules <- function(course, max=100)
    .chk(.hc(,paste0(file.path("courses", course, "modules"), "?per_page=", max)))

#' @description List all items contained in a module
#'
#' @rdname modules
#' @export
module.items <- function(course, module, max=100)
    .chk(.hc(,paste0(file.path("courses", course, "modules", module, "items"), "?per_page=", max)))

#' @title API-related functions
#' @name API
#'
#' @description \code{last.request} returns the last Canvas HTTPS request and result. Mostly useful for debugging.
#' 
#' @export
last.request <- function() .pool$last
