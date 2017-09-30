library(hrbrthemes)
library(tidyverse)
library(curl)
library(httr)
library(rvest)
library(stringi)
library(urltools)
library(rappalyzer) # devtools::install_github("hrbrmstr/rappalyzer")
library(rprojroot)
# I'm also using a user agent shortcut from the splashr package which is on CRAN

rt <- find_rstudio_root_file()

f1k <- read_csv(file.path(rt, "data", "f1k.csv"))

if (!file.exists(file.path(rt, "data", "f1k_gets.rds"))) {

  targets <- pull(f1k, website)

  results <- list()
  errors <- list()

  OK <- function(res) {
    cat(".", sep="")
    results <<- c(results, list(res))
  }

  BAD <- function(err_msg) {
    cat("X", sep="")
    errors <<- c(errors, list(err_msg))
  }

  pool <- multi_set(total_con = 20)

  walk(targets, ~{
    multi_add(
      new_handle(url = .x, useragent = splashr::ua_macos_chrome, followlocation = TRUE, timeout = 60),
      OK, BAD
    )
  })

  multi_run(pool = pool)

  write_rds(results, file.path(rt, "data", "f1k_gets.rds"), compress = "xz")

} else {
  results <- read_rds(file.path(rt, "data", "f1k_gets.rds"))
}

if (!file.exists(file.path(rt, "data", "rapp_results.rds"))) {

  results <- keep(results, ~.x$status_code < 300)
  map(results, ~{
    list(
      url = .x$url,
      status_code = .x$status_code,
      content = .x$content,
      headers = httr:::parse_headers(.x$headers)
    ) -> res
    class(res) <- "response"
    res
  }) -> results

  # this takes a *while*
  pb <- progress_estimated(length(results))
  map_df(results, ~{
    pb$tick()$print()
    rap_df <- rappalyze(.x)
    if (nrow(rap_df) > 0) rap_df <- mutate(rap_df, url = .x$url)
  }) -> rapp_results

  write_rds(rapp_results, file.path(rt, "data", "rapp_results.rds"))

} else {
  rapp_results <- read_rds(file.path(rt, "data", "rapp_results.rds"))
}

left_join(
  mutate(rapp_results, host = domain(rapp_results$url)) %>%
    bind_cols(suffix_extract(.$host))
  ,
  mutate(f1k, host = domain(website)) %>%
    bind_cols(suffix_extract(.$host)),
  by = c("domain", "suffix")
) %>%
  filter(!is.na(name)) -> rapp_results

length(unique(rapp_results$name))
## [1] 754

xdf <- distinct(rapp_results, name, sector, category, tech)

sort(unique(xdf$category))
##  [1] "Advertising Networks"  "Analytics"             "Blogs"
##  [4] "Cache Tools"           "Captchas"              "CMS"
##  [7] "Ecommerce"             "Editors"               "Font Scripts"
## [10] "JavaScript Frameworks" "JavaScript Graphics"   "Maps"
## [13] "Marketing Automation"  "Miscellaneous"         "Mobile Frameworks"
## [16] "Programming Languages" "Tag Managers"          "Video Players"
## [19] "Web Frameworks"        "Widgets"

count(xdf, tech, sort=TRUE)
## # A tibble: 115 x 2
##                        tech     n
##                       <chr> <int>
##  1                   jQuery   572
##  2       Google Tag Manager   220
##  3                Modernizr   197
##  4        Microsoft ASP.NET   193
##  5          Google Font API   175
##  6        Twitter Bootstrap   162
##  7                WordPress   150
##  8                jQuery UI   143
##  9             Font Awesome   118
## 10 Adobe Experience Manager    69
## # ... with 105 more rows

group_by(xdf, sector) %>%
  count(category) %>%
  ungroup() %>%
  arrange(category) %>%
  mutate(category = factor(category, levels=rev(unique(category)))) %>%
  ggplot(aes(category, n)) +
  geom_boxplot() +
  scale_y_comma() +
  coord_flip() +
  labs(x=NULL, y="Tech/Services Detected across Sectors",
       title="Usage of Tech Stack Categories by Sector") +
  theme_ipsum_rc(grid="X")

filter(xdf, category=="CMS") %>%
  count(tech, sort=TRUE)
## # A tibble: 15 x 2
##                        tech     n
##                       <chr> <int>
##  1                WordPress    75
##  2 Adobe Experience Manager    69
##  3                   Drupal    68
##  4               Sitefinity    26
##  5     Microsoft SharePoint    19
##  6                 Sitecore    14
##  7                      DNN    11
##  8                Concrete5     6
##  9     IBM WebSphere Portal     4
## 10        Business Catalyst     2
## 11                    Hippo     2
## 12                TYPO3 CMS     2
## 13               Contentful     1
## 14              Orchard CMS     1
## 15             SilverStripe     1

filter(xdf, category=="Web Frameworks") %>%
  count(tech, sort=TRUE)
## # A tibble: 7 x 2
##                          tech     n
##                         <chr> <int>
## 1           Microsoft ASP.NET   193
## 2           Twitter Bootstrap   162
## 3             ZURB Foundation    61
## 4               Ruby on Rails     2
## 5            Adobe ColdFusion     1
## 6                    Kendo UI     1
## 7 Woltlab Community Framework     1

devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)
library(data.tree)
library(treemap)

cpdf <- count(xdf, sector, tech)

cpdf$pathString <- paste("rapp",
                            cpdf$sector,
                            cpdf$tech,
                            sep = "/")
stacks <- as.Node(cpdf)

circlepackeR(stacks, size = "n", color_min = "hsl(56,80%,80%)",
             color_max = "hsl(341,30%,40%)", width = "100%", height="800px")

