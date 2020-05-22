##
#r <- .hc(,"courses")
#fromJSON(rawToChar(r$content))
## .hc(,file.path("courses", 47973, "students"))
## .hc(,file.path("courses", 47973, "users", 133814)) ## jsut one line, but same
## fromJSON(rawToChar(.hc(,file.path("courses", 47973, "pages"))$content))
## recordings: 220594
## fromJSON(rawToChar(.hc(,file.path("courses", 47973, "pages", "schedule-of-lecture-recordings-2019-s2-second-half"))$content))
## fromJSON(rawToChar(.hc(,file.path("courses", 47973, "pages", "schedule-of-lecture-recordings-2019-s2-second-half", "revisions"))$content))
## r <- .hc("PUT",file.path("courses", 47973, "pages", "schedule-of-lecture-recordings-2019-s2-second-half"), httr:::body_config(list(`wiki_page[body]`=a), encode="form"))>
## fromJSON(rawToChar(.hc(,file.path("courses", 47973, "modules"))$content))
## fromJSON(rawToChar(.hc(,file.path("courses", 47973, "modules", 130855, "items"))$content))
## fromJSON(rawToChar(.hc(,file.path("courses", 47973, "files"))$content))
## fromJSON(rawToChar(.hc(,file.path("courses", 47973, "folders"))$content))
## NOTE: when accessing folder it is no longer part of the course
## fromJSON(rawToChar(.hc(,file.path("folders", 795762, "files"))$content))
## fromJSON(rawToChar(.hc(,paste0(file.path("folders", 795762, "files"), "?per_page=20"))$content))
##
## r<-.hc("POST", file.path("folders", 795762, "files"), httr:::body_config(list(content_type="text/markdown", name="test.Rmd", size=file.info("test.Rmd")$size, parent_folder_id=795762)))
## u=fromJSON(rawToChar(r$content))
## r2 <- POST(u$upload_url, body=c(u$upload_params, list(file=cont)), encode="multipart")
## 201: $headers$location = "https://canvas.auckland.ac.nz/api/v1/files/4651370?include%5B%5D=enhanced_preview_url" - i.e. contains the ID
#47973
## update lectures:
##  a <- GET("https://stat.auckland.ac.nz/~urbanek/782/lectures.html")
##  page.edit(47973, 220594, rawToChar(a$content))
##
##    r <- .hc("PUT", file.path("courses", 47973, "pages", "schedule-of-lecture-recordings-2019-s2-second-half"), httr:::body_config(list(`wiki_page[body]`=rawToChar(a$content)), encode="form"))
