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

.hc <- function(method="GET", path, ...,
                headers=NULL, handle = NULL,
                token=Sys.getenv("CANVAS_API_TOKEN"),
                api.url=Sys.getenv("CANVAS_API_DOMAIN")) {
    if (is.list(path)) path <- paste(as.character(path), collapse='/')
    url <- paste(api.url, path, sep='/')
    hu <- httr:::handle_url(handle, url, ...)
    config <- add_headers(Authorization=paste("Bearer", token))
    req <- httr:::request_build(method, hu$url, config, ...)
    .pool$last <- httr:::request_perform(req, hu$handle$handle)
}

#' Manage files
#' @name files
NULL

#' upload a file
#' @rdname files
#' @export
file.upload <- function(file, name=basename(file), folder, mime.type="text/plain") {
    sz <- file.info(file)$size
    if (is.na(sz)) stop("cannot determine file size")
    r <- .hc("POST", file.path("folders", folder, "files"),
             httr:::body_config(
                        list(content_type=mime.type,
                             name=name,
                             size=sz,
                             parent_folder_id=folder)))
    if (status_code(r) != 200)
        stop(errorCondition("Inital POST to request upload failed", res=r, class="httrRequestError"))
    u <- fromJSON(rawToChar(r$content))
    r2 <- POST(u$upload_url, body=c(u$upload_params, list(file=cont)), encode="multipart")
    if (status_code(r) != 201)
        stop(errorCondition("Upload of the file failed", res=r, class="httrRequestError"))
    tryCatch(fromJSON(rawToChar(r2$headers)), error=function(e) stop(errorCondition("Parsing of the request result failed, but file may have been created", res=r2, class="parseError")))
}

.chk <- function(r, desc="Canvas request failed", valid.code=200L) {
    if (status_code(r) != 200)
        stop(errorCondition(desc, res=r, class="httrRequestError"))
    tryCatch(fromJSON(rawToChar(r$content)),
             error=function(e) stop(errorCondition("Parsing of the request result failed",
                                                   res=r, class="parseError")))
}

#' delete a file
#' @rdname files
#' @export
file.delete <- function(id)
    .chk(.hc("DELETE", file.path("files", id)), "DELETE request failed")

#' Manage courses
#'
#' List all accessible courses
#'
#' @export
courses <- function()
    .chk(.hc(, "courses"))

#' List all files in a course
#'
#' @rdname files
#' @export
files <- function(folder, course, max=100) {
    if (!missing(folder))
        return(.chk(.hc(,paste0(file.path("folders", folder, "files"), "?per_page=", max))))
    if (!missing(course))
        return(.chk(.hc(,paste0(file.path("courses", course, "files"), "?per_page=", max))))
    stop("One of folders, courses must be set")
}

#' list all folders
#'
#' @rdname files
#' @export
folders <- function(course, max=100) {
    if (!missing(course))
        return(.chk(.hc(,paste0(file.path("courses", course, "folders"), "?per_page=", max))))
    stop("One of folders, courses must be set")
}

#' Manage course modules
#'
#' list all modules in a course
#'
#' @export
modules <- function(course, max=100)
    .chk(.hc(,paste0(file.path("courses", course, "modules"), "?per_page=", max)))

#' List all items contained in a module
#'
#' @rdname modules
#' @export
module.items <- function(course, module, max=100)
    .chk(.hc(,paste0(file.path("courses", course, "modules", module, "items"), "?per_page=", max)))

#' Manage Wiki Pages
#'
#' list all pages in a course
#' @export
pages <- function(course, max=100)
    .chk(.hc(,paste0(file.path("courses", course, "pages"), "?per_page=", max)))

.page.args <- function(content, title, editing_roles, notify, published, front) {
    if (!missing(content)) {
        if (is.raw(content))
            content <- rawToChar(content)
        if (length(content) > 1)
            content <- paste(as.character(content), collapse="\n")
        l[['wiki_page[body]']] <- content
    }
    if (!missing(title))
        l[['wiki_page[title]']] <- title
    if (!missing(editing_roles)) {
        if (length(editing_roles) > 1)
            editing_roles <- paste(editing_roles, collapse=",")
        l[['wiki_page[editing_roles]']] <- as.character(editing_roles)
    }
    if (!missing(notify)) {
        notify <- as.logical(notify)
        if (length(notify) != 1) stop("notify must be a scalar logical if set")
        if (!is.na(notify))
            l[['wiki_page[notify_of_update]']] <- if (notify) 'true' else 'false'
    }
    if (!missing(published)) {
        published <- as.logical(published)
        if (length(published) != 1) stop("published must be a scalar logical if set")
        if (!is.na(published))
            l[['wiki_page[published]']] <- if (published) 'true' else 'false'
    }
    if (!missing(front)) {
        front <- as.logical(front)
        if (length(front) != 1) stop("front must be a scalar logical if set")
        if (!is.na(front))
            l[['wiki_page[front_page]']] <- if (front) 'true' else 'false'
    }
    l
}

#' Modify an existing wiki page - content and/or attributes
#' @rdname pages
#' @export
page.edit <- function(course, page, content, title, editing_roles, notify, published, front) {
    l <- .page.args(content, title, editing_roles, notify, published, front)
    if (missing(course) || missing(page)) stop("course and page are mandatory")
    .chk(.hc("PUT", file.path("courses", course, "pages", as.character(page)),
             httr:::body_config(l, encode="form")))
}

#' Create a new Wiki page
#' @rdname pages
#' @export
page.create <- function(course, title, content, editing_roles, notify, published, front) {
    if (missing(course) || missing(title)) stop("course and title are mandatory")
    l <- .page.args(content, title, editing_roles, notify, published, front)
    .chk(.hc("POST", file.path("courses", course, "pages"),
             httr:::body_config(l, encode="form")))
}

#' Retrieve a page (content and attributes)
#' @rdname pages
#' @export
page <- function(course, page)
    .chk(.hc(, file.path("courses", course, "pages", as.character(page))))

#' API-related functions
#'
#' @name API
NULL

#' Return the last Canvas HTTPS request and result. Mostly useful for debugging.
#' @export
#' @rdname API
last.request <- function() .pool$last
