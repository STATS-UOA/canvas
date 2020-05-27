## Canvas LMS API for R

This package attempts to provide programmatic access to Camvas LMS from R. It is far from complete, the main purpose is at this point to centralise the efforts, such that as we need to do something we add the functions for it here rather than just keeping various scripts.

Before you can use this package, you have to obtain a Canvas API token and set the two environemnt variables `CANVAS_API_TOKEN` and `CANVAS_API_DOMAIN` where the latter is the URL of your Canvas server, typically something like `https://canvas.my-uni.edu/api/v1`.

For high-level functions try something like `courses()` and look up help such as `?files` or `?pages`.

The low-level work-horse is `canvas:::.hc()` which simply takes the path portion of the URL and adds all other necessary pieces. The API is still in flux, so it may change (for the better), but generally something like `canvas:::.hc(,"courses")` works. There is also `canvas:::.chk()` which checks the result and attempt to parse it, so most high-level function simply call `.chk(.hc(...))`.

### Installation

From the repository:

```r
install.packages("canvas", repo="https://rforge.net")
```

From the source: you have to call `sh mkdist` first to generate a valid package tar ball and you need `knitr` and `roxygen2` packages to generate artifacs.


[![RForge](https://rforge.net/do/versvg/canvaslms)](https://RForge.net/canvaslms)

