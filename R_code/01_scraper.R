# Scraping the Course Directory


# Setup Environment -------------------------------------------------------

library(tidyverse)
library(rvest)

# Params ------------------------------------------------------------------

params <- list(language = "de", # "de" for German, "en" for english
               year = 2024,
               semester = "W") # "S" for Summer, "W" for Winter

# Basisadresse generieren
params$adress_base <- 
  paste0("https://ufind.univie.ac.at/",
         params$language,
         "/")

params$adress_lvl1 <- 
  paste0(params$adress_base,
         "vvz.html?semester=",
         params$year,
         params$semester)

# Level 1 extrahieren -----------------------------------------------------

lvl_1_directories_html <- read_html(params$adress_lvl1)

lvl_1_directories <-
  tibble(
    directorate_name = lvl_1_directories_html %>% 
      html_elements("h2.icon-usse-info") %>% 
      html_text(),
    directorate = lvl_1_directories_html %>% 
      html_elements("div.usse-id-vvz") %>% 
      map(
        ~ html_elements(.x,
                        "ul li a") %>% 
          {tibble(text = html_text(.),
                  address = html_attr(.,
                                      "href") %>% 
                    {paste0(params$adress_base,
                            .)})}
      )
  )

save(lvl_1_directories,
     file = file.path("R_data",
                      "lvl_1_directories.RData"))

# Level 2 extrahieren -----------------------------------------------------

# examplary for Law to save processing time

lvl_2_courses_exams <-
  lvl_1_directories %>% 
  filter(str_detect(directorate_name, "Rechtswissenschaften")) %>% 
  unnest(directorate,
         names_sep = "_") %>% 
  mutate(courses_exams = 
           map(directorate_address,
               read_html),
         .keep = "unused")


lvl_2_courses_exams <- lvl_2_courses_exams %>% 
  mutate(
    courses = 
      map(courses_exams,
          ~ .x %>% 
            html_elements("div.usse-id-vvz ul.lv-and-exam-list li.course") %>% 
            {tibble(number = html_element(.,
                                          "span.number") %>% 
                      html_text(),
                    type = html_element(.,
                                        "abbr.type") %>% 
                      html_attr("title"),
                    name = html_element(.,
                                        "a.what") %>% 
                      html_text(),
                    subtext = html_element(.,
                                           "span.subwhat.text") %>% 
                      html_text(),
                    address = html_element(.,
                                           "a.what") %>% 
                      html_attr("href") %>% 
                      {paste0(params$adress_base,
                              .)})}
      ),
    exams = 
      map(courses_exams,
          ~ .x %>% 
            html_elements("div.usse-id-vvz ul.lv-and-exam-list li.list.exam") %>% 
            {tibble(name = html_element(.,
                                        "a") %>% 
                      html_text(),
                    address = html_element(.,
                                           "a") %>% 
                      html_attr("href") %>% 
                      {paste0(params$adress_base,
                              .)})}),
  .keep = "unused")

save(lvl_2_courses_exams,
     file = file.path("R_data",
                      "lvl_2_courses_exams_rewi.RData"))
