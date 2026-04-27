
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


# --- Injected Resiliency Wrapper ---
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
    # Ensure we use the explicit utils:: function to avoid recursion
    orig_func(file, ..., fileEncoding = "UTF-8-BOM")
  }
}

# Apply wrappers using explicit namespaces
read.csv   <- .auto_read_wrapper(utils::read.csv)
read.csv2  <- .auto_read_wrapper(utils::read.csv2)

write.csv  <- .auto_mkdir_wrapper(utils::write.csv)
write.csv2 <- .auto_mkdir_wrapper(utils::write.csv2)
saveRDS    <- .auto_mkdir_wrapper(base::saveRDS)
# --- End Injection ---

auto_mkdir_wrapper <- function(orig_func) {
  function(x, file, ...) {
    if (is.character(file)) {
      dir_path <- dirname(file)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }
    }
    orig_func(x, file, ...)
  }
}

auto_read_wrapper <- function(orig_func) {
  function(file, ...) {
    orig_func(file, ..., fileEncoding = "UTF-8-BOM")
  }
}

read.csv <- auto_read_wrapper(utils::read.csv)
read.csv2 <- auto_read_wrapper(utils::read.csv2)

write.csv <- auto_mkdir_wrapper(utils::write.csv)
write.csv2 <- auto_mkdir_wrapper(utils::write.csv2)
saveRDS <- auto_mkdir_wrapper(base::saveRDS)

################################################################################
# Dieser Syntax gehört zu:
#
# Zellner, J., Ebenbeck, N., Gebhardt, M. (2024).
# Entwicklung digitaler Simulationsspiele mit integrierten Entscheidungsbäumen
# zur Förderung der diagnostischen Entscheidungskompetenzen in der
# sonderpädagogischen Lehrkräfteausbildung
#
# Kontakt: Judith Zellner (judith.zellner@edu.lmu.de)
#
# Letzte Änderung: 20.02.2024
################################################################################
# 1. Pakete und Daten ####
library(tidyverse) # Pakete einladen
getwd()
data <- read.csv2("simulationsspiele.csv") # Daten einladen
colnames(data)
# 2. Datenaufbereitung ####
data_2 <- data %>%
  # NA zu 0
  mutate_all(~ coalesce(na_if(., NA), 0)) %>%
  # Auswahl der beiden Lernspiele
  select(c("game1", "game2")) %>%
  # Anzahl an ausgewählten Tests
  mutate(n_tests_1 = nchar(game1), n_tests_2 = nchar(game2)) %>%
  # Position passender Test
  mutate(
    pos_test_1 = str_locate(game1, "4")[, 1],
    pos_test_2 = str_locate(game2, "4")[, 1]
  ) %>%
  # Passender Test als erstes?
  mutate(
    first_test_1 = ifelse(substr(game1, 1, 1) == "4", 1, 0),
    first_test_2 = ifelse(substr(game2, 1, 1) == "4", 1, 0)
  ) %>%
  # Passender Test als letztes?
  mutate(
    last_test_1 = ifelse(
      substr(game1, n_tests_1, n_tests_1) == "4",
      1,
      0
    ),
    last_test_2 = ifelse(
      substr(game2, n_tests_2, n_tests_2) == "4",
      1,
      0
    )
  )

# 3. Datenanalyse ####

summary(data_2)

sd1 <- sd(data_2$n_tests_1)
sd2 <- sd(data_2$n_tests_2)


# 4. Darstellung der Daten ####

neu <- data_2 %>%
  select(1:2) %>%
  mutate(across(everything(), ~ na_if(., 0))) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(clicks1 = nchar(game1)) %>%
  mutate(clicks2 = nchar(game2)) %>%
  mutate(correct1 = str_detect(game1, "4")) %>%
  mutate(correct2 = str_detect(game2, "4"))


a <- neu %>%
  select(clicks1, clicks2, correct1, correct2) %>%
  pivot_longer(
    cols = c(clicks1, clicks2),
    names_to = "Messzeitpunkt",
    values_to = "n"
  ) %>%
  pivot_longer(
    cols = c(correct1, correct2),
    names_to = "Messzeitpunkt2",
    values_to = "n2"
  ) %>%
  mutate(Messung = str_sub(Messzeitpunkt, start = -1)) %>%
  select(-c(Messzeitpunkt, Messzeitpunkt2)) %>%
  mutate(n2 = ifelse(as.character(n2) == TRUE, 1, 0)) %>%
  mutate(Treffer = (n2 / n) * 100)

a <- a %>%
  mutate(Messung = ifelse(Messung == "6", "2", Messung)) %>%
  mutate(Messung = as.factor(Messung))

a <- rename(a, Klicks = n)


# Boxplots aus a

ggplot(data = a, aes(x = Messung, y = Treffer)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_bw()

ggplot(data = a, aes(x = Messung, y = Klicks)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_bw()