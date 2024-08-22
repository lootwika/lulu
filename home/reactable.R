library(htmltools)
library(reactable)
## Hilfsfunktionen für reactable ----------------------------------------------

icon_img <- function(filename, alt, height = "16px") {
  img(
    src = paste0("../pics/", filename),
    style = "height: 16px",
    alt = alt
  )
}

kostet_img <- function() {
  icon_img("Gold.png", "Kostet")
}

free_img <- function() {
  icon_img("free.png", "Kostenlos")
}

fertig_img <- function() {
  icon_img("fertig.png", "Fertig")
}

angefangen_img <- function() {
  icon_img("angefangen.png", "Angefangen")
}

todo_img <- function() {
  icon_img("todo.png", "To Do")
}

clean_title <- function(title) {
  title |>
    tolower() |>
    stringr::str_replace_all(" ", "-") |>
    stringr::str_remove_all(":") |>
    stringr::str_remove_all("&") |>
    stringr::str_remove_all("#") |>
    stringr::str_remove_all("'") |>
    stringr::str_remove_all("\\(") |>
    stringr::str_remove_all("\\)") |>
    stringr::str_remove_all("/") |>
    stringr::str_remove_all("\\.")
}

get_image_src <- function(title) {
  title <- clean_title(title)
  pics_path <- "../pics/"
  if (interactive()) {
    pics_path <- "pics/"
  }
  paste0(pics_path, title, ".png")
}

lore_field <- function(name, ...) {
  if (any(is.na(...))) {
    NULL
  } else {
    tagList(div(class = "detail-label", name), ...)
  }
}

create_review <- function(value) {
  if (is.na(value) || value == 0 || value < 0) {
    value <- 0
  }
  if (value > 5) {
    value <- 5
  }
  stars <- list()
  if (value > 0) {
    for (i in 1:value) {
      stars[[i]] <- span(class = "fa fa-star checked")
    }
  }
  if (value < 5) {
    black_stars <- 5 - value
    for (i in 1:black_stars) {
      stars[[value + i]] <- span(class = "fa-regular fa-star")
    }
  }
  div(stars)
}

## Reactable Row Details ------------------------------------------------------

row_details <- function(index) {
  werk <- lore$Werk[index]
  if (is.na(werk)) {
    werk <- ""
  }
  titel <- lore$Titel[index]
  english_title <- lore$title_engl[index]


  image_div <- NULL
  if (lore$pic_exists[index]) {
    image_div <- div(
      img
      (
        src = lore$pic[index],
        style = ifelse(lore$Bildausrichtung[index] == "hochkant", "width: 200px", "height: 120px"),
        alt = paste("Cover-Bild zu", titel)
      )
    )
  } else if (!is.na(lore$Werk[index]) && lore$pic_exists_werk[index]) {
      image_div <- div(
        img(
          src = lore$pic_werk[index],
          style = ifelse(lore$Bildausrichtung[index] == "hochkant", "width: 200px", "height: 100px"),
          alt = paste("Cover-Bild zu", titel)
        )
      )
    # }
  }

  wiki_tag <- lore$Wiki[index]
  if (!is.na(wiki_tag)) {
    wiki_tag <- htmltools::tags$a(href = wiki_tag, target = "_blank", wiki_tag)
  }

  description_tag <- lore$Description[index]
  if (!is.na(description_tag)) {
    description_tag <- tags$em(description_tag)
  }

  isbn <- lore$ISBN[index]
  if (is.na(isbn) && !is.na(lore$ISBN_werk[index])) {
    isbn <- lore$ISBN_werk[index]
  }
  if (!is.na(isbn)) {
    isbn <- tags$a(href = paste0("https://isbnsearch.org/isbn/", isbn), target = "_blank", isbn)
  }

  english_link <- lore[["Link (englisch)"]][index]
  if (!is.na(english_link)) {
    english_link <- tags$a(href = english_link, target = "_blank", english_title)
  }

  div(
    class = "lore-detail",
    style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 0.75rem;",
    div(
      image_div
    ),
    div(
      style = "grid-column: span 2;",
      lore_field(
        "Englischer Titel",
        english_link
      ),
      lore_field(
        "Description",
        description_tag
      ),
      lore_field(
        "Warcraft Wiki",
        wiki_tag
      ),
      lore_field(
        "Zusatzinfos",
        lore$Zusatzinfos[index]
      ),
      lore_field(
        "ISBN",
        isbn
      ),
    )
  )
}

## Reactable ------------------------------------------------------------------


lore_reactable <- function(lore) {
  reactable::reactable(
    dplyr::select(lore, Status, Kosten, Titel, Bewertung),
    resizable = TRUE,
    compact = TRUE,
    striped = TRUE,
    columns = list(
      Status = colDef(
        name = "",
        width = 22,
        cell = function(value) {
          if (is.na(value) || value != "check"){
            todo_img()
          }

          else{
            fertig_img()
          }
        }
      ),
      Titel = colDef(
        name = ""#,
        # cell = function(value, index) {
        #   url <- lore$Link[index]
        #   htmltools::tags$a(href = url, target = "_blank", value)
        # }
      ),
      Kosten = colDef(
        name = "",
        width = 25,
        cell = function(value) {
          if (!is.na(value) && value == "kostenlos"){
            free_img()
          } else {
            kostet_img()
          }
        }
      ),
      Bewertung = colDef(
        name = "",
        cell = function(value) {
          create_review(value)
          #shiny::icon("star", class = "fas")
        }
      )
    ),
    details = row_details,
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "#272b30", # "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
  )
}

get_lore_path <- function() {
  path <- "../lore-resources.xlsx"
  if (interactive()) {
    path <- "lore-resources.xlsx"
  }
  path
}

lore_data <- function(sheet = 1) {
  path <- get_lore_path()
  readxl::read_excel(path, sheet) |>
    dplyr::rename(title_engl = "Titel (englisch)") |>
    dplyr::mutate(pic = get_image_src(title_engl)) |>
    dplyr::mutate(
      pic_exists = file.exists(fs::path(getwd(), pic))
    )
}

read_lore <- function() {
  lore <- lore_data()
  werke <- lore_data(2)
  dplyr::left_join(lore, werke, by = c("Werk" = "Id"), suffix = c("", "_werk"))
}

