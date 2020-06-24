#' @title Manage enrollments
#'
#' @description \code{enrollments} lists all students enrolled in a course and associated information.
#'
#' @param course \code{id} of the course
#' @param ... additional argument for \code{\link{.api}}
#' @export
enrollments <- function(course, ...)
    api(file.path("courses", course, "enrollments"), ...)
