index_html <- readLines("inst/html_js/index.html")
ldwdev0_js <- readLines("inst/html_js/ldwdev0.js")
onefunc_html <- readLines("inst/html_js/onefunc.html")
save(index_html, ldwdev0_js, onefunc_html, file = "R/sysdata.rda")
