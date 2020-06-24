#' @title Manage Wiki Pages
#' @name pages
#'
#' @description \code{pages} lists all pages in a course.
#' @param course integer, \code{id} of the course
#' @param ... additional arguments for \code{\link{.api}}
#' @export
pages <- function(course, ...)
    .api(file.path("courses", course, "pages"), ...)

.page.args <- function(content, title, editing_roles, notify, published, front) {
    l <- list()
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

#' @description \code{page.edit} modifies an existing wiki page - content and/or attributes. Only those components that are specified will be changed, all argumebts apart for \code{course} and \code{page} are optional.
#' @param course integer, mandatory, course-id.
#' @param page integer or string, mandatory, either the \code{id} or the name of the page (as defined by \code{url} entry of the \code{pages()} data frame).
#' @param content string, new content
#' @param title string, title
#' @param editing_roles character vector, roles which are allowed to edit the page. Valid roles are \code{"teachers"}, \code{"students"}, \code{"members"} and \code{"public"}.
#' @param notify logical, if \code{TRUE} notifications are sent on modification.
#' @param published logical, if \code{TRUE} the page is published
#' @param front logical, if \code{TRUE} set the page as front page
#' @rdname pages
#' @export
page.edit <- function(course, page, content, title, editing_roles, notify, published, front) {
    l <- .page.args(content, title, editing_roles, notify, published, front)
    if (missing(course) || missing(page)) stop("course and page are mandatory")
    .chk(.hc("PUT", file.path("courses", course, "pages", as.character(page)),
             httr:::body_config(l, encode="form")))
}

#' @description \code{page.create} creates a new Wiki page
#' @rdname pages
#' @export
page.create <- function(course, title, content, editing_roles, notify, published, front) {
    if (missing(course) || missing(title)) stop("course and title are mandatory")
    l <- .page.args(content, title, editing_roles, notify, published, front)
    .chk(.hc("POST", file.path("courses", course, "pages"),
             httr:::body_config(l, encode="form")))
}

#' @description \code{page} retrieves a page, both content (in the \code{body} element) and attributes.
#' @rdname pages
#' @export
page <- function(course, page)
    .chk(.hc(, file.path("courses", course, "pages", as.character(page))))

