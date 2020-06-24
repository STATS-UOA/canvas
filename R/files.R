#' @title Manage files
#'
#' @description \code{files} lists all files in a course or folder. Exactly one of \code{course} or \code{folder} has to be specified.
#'
#' @param folder \code{id} of the folder
#' @param course \code{id} of the course
#' @param ... additional argument for \code{\link{.api}}
#' @export
files <- function(folder, course, ...) {
    if (!missing(folder))
        return(.api(file.path("folders", folder, "files"), ...))
    if (!missing(course))
        return(.api(file.path("courses", course, "files"), ...))
    stop("One of folders, courses must be set")
}

#' @description \code{file.upload} uploads a file into Canvas. It is a two-stage process where the metadata is posted first and then the file is uploaded. When successful it returns the newly created file entry.
#'
#' @param file string, path to the file to upload
#' @param name string, name of the file as it will appear in Cavnas
#' @param folder integer, \code{id} of the folder to place the file into
#' @param mime.type string, mime-type of the file
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
    r2 <- POST(u$upload_url, body=c(u$upload_params, list(file=readBin(file, raw(), sz))), encode="multipart")
    if (status_code(r2) != 201)
        stop(errorCondition("Upload of the file failed", res=r, class="httrRequestError"))
    tryCatch(fromJSON(rawToChar(r2$content)), error=function(e) stop(errorCondition("Parsing of the request result failed, but file may have been created", res=r2, class="parseError")))
}

#' @description \code{file.delete} removes a file
#'
#' @param id integer, identifier of the file
#' @rdname files
#' @export
file.delete <- function(id)
    .chk(.hc("DELETE", file.path("files", id)), "DELETE request failed")

#' @description \code{folder} lists all folders. Note that the output is a list of all folders, the hierarchical structure is achieved by linkning \code{parent_folder_id} and \code{id}. The root folder has the \code{parent_folder_id} of \code{NA}.
#'
#' @rdname files
#' @export
folders <- function(course, ...) {
    if (!missing(course))
        return(.api(file.path("courses", course, "folders"), ...))
    stop("One of folders, courses must be set")
}
