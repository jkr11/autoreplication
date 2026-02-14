
# --- Injected Wrappers 
.auto_mkdir_wrapper <- function(orig_func) {
  function(x, file, ...) {
    if (missing(file)) {
        # Handle cases where file is passed as first arg (x) implicitly
        if (is.character(x)) file <- x
    }
    if (!is.null(file) && is.character(file)) {
      dir_path <- dirname(file)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }
    }
    orig_func(x, file, ...)
  }
}

.auto_read_wrapper <- function(orig_func) {
  function(file, ...) {
    orig_func(file, ..., fileEncoding = "UTF-8-BOM")
  }
}

read.csv   <- .auto_read_wrapper(utils::read.csv)
read.csv2  <- .auto_read_wrapper(utils::read.csv2)

write.csv  <- .auto_mkdir_wrapper(utils::write.csv)
write.csv2 <- .auto_mkdir_wrapper(utils::write.csv2)
saveRDS    <- .auto_mkdir_wrapper(base::saveRDS)
# --- End Injection

library(tidyverse)
library(stringr)

path <- "data/all_data.csv"
data <- read_delim(path, delim = ";", show_col_types = FALSE)

keywords <- c(
  "open science",
  "open-science",
  "open source",
  "open-source",
  "replikation",
  "replication",
  "replizier",
  "reproduzier",
  "Reproduktion",
  "Reproduzierbarkeit",
  "reproducib",
  "open data",
  "offene daten",
  "präregistrier",
  "prereg",
  "forschungstransparenz",
  "transparente forschung",
  "research transparency",
  "reproducibility",
  "open science",
  "open-access",
  "open access",
  "open source",
  "open data",
  "open materials",
  "open research",
  "open code",
  "open data/code",
  "open data/",
  "open data/materials/code",
  "open science/data",
  "freie Wissenschaft",
  "offene Wissenschaft",
  "offene Forschung",
  "freie Forschung",
  "replizierbare",
  "reproduzierbar",
  "reproduzierbare",
  "reproduzierbarkeit",
  "replikation",
  "replikationen",
  "reproduzierbarkeit",
  "arxiv",
  "osf.io",
  "open science framework",
  "open science framework (osf)",
  "open science framework (OSF)",
  "open science framework (OSF.io)",
  "offene Daten, Materialien",
  "offene Daten",
  "offene Materialien",
  "offene Daten/Materialien",
  "offene Wissenschaft",
  "offene Wissenschaften",
  "offene Wissenschaften (Open Science)"
)

all_keywords <- stringr::str_escape(unique(tolower(trimws(keywords))))
pattern <- paste(all_keywords, collapse = "|")

get_matched_sentences <- function(text, keywords) {
  if (is.na(text) || text == "") {
    return(NA)
  }

  sentences <- unlist(str_split(text, "(?<=[.!?])\\s+"))

  matched_sentences <- unlist(lapply(keywords, function(k) {
    sentences[str_detect(sentences, regex(k, ignore_case = TRUE))]
  }))

  matched_sentences <- unique(matched_sentences)
  if (length(matched_sentences) == 0) {
    return(NA)
  }

  paste(matched_sentences, collapse = " | ")
}

result <- data %>%
  mutate(
    matched_sentences = map_chr(fulltext, ~ get_matched_sentences(.x, pattern)),
    matched_keywords = str_extract_all(
      tolower(fulltext),
      regex(pattern, ignore_case = TRUE)
    ) %>%
      map_chr(~ paste(unique(.x), collapse = ", ")),
    has_os = !is.na(matched_sentences)
  ) %>%
  filter(has_os)

stats_summary <- data %>%
  mutate(has_os = str_detect(fulltext, regex(pattern, ignore_case = TRUE))) %>%
  group_by(subject_area) %>%
  summarise(
    total_postings = n(),
    os_mentions = sum(has_os, na.rm = TRUE),
    os_percentage = round((os_mentions / total_postings) * 100, 1)
  ) %>%
  arrange(desc(os_percentage))

print(stats_summary)
print(result$position)

write_excel_csv(result, "results_with_sentences.csv")
write_excel_csv(stats_summary, "subject_stats.csv")