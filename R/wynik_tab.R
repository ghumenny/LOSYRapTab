#' @title Wypełnia kolumnę 'wynik' tabeli parametrów wygenerowanymi tabelami krzyżowymi
#' @description Funkcja iteruje przez wiersze tabeli parametrów i dla każdej kombinacji
#'   parametru (wskaźnik, kryterium, filtry, itd.) filtruje odpowiednie dane źródłowe
#'   i wywołuje właściwą funkcję generującą tabelę krzyżową. Wynik tabeli krzyżowej
#'   jest zapisywany w kolumnie 'wynik' jako element listy.
#' @param param_df Ramka danych z parametrami wskaźników (np. wygenerowana przez `polacz_ramki_wskaznikow`).
#'   Powinna zawierać kolumny: `wskaznik`, `WOJ_NAZWA`, `typ_szk2`, `rok_abs`, `edycja`, `kryterium`,
#'   `rok`, `miesiac`, `typ_k1_analizy`, `parametr_K`, `parametr_grupa_szkol`.
#' @param p2_1 Dane źródłowe dla wskaźnika K2.
#' @param p3 Dane źródłowe dla wskaźników S7, K1, W1, B1.
#' @param p4 Dane źródłowe dla wskaźników D1, D2, typ_szk2, meta_zaw.
#' @param debug_mode Logiczny. Jeśli TRUE, funkcja działa w trybie sekwencyjnym
#'   i zwraca rozszerzone informacje diagnostyczne o błędach. Domyślnie FALSE.
#' @return Zmodyfikowana ramka danych `param_df` z wypełnioną kolumną `wynik`,
#'   gdzie każda komórka zawiera tabelę krzyżową lub informację o błędzie/braku danych.
#' @importFrom dplyr %>% filter pull row_number mutate select bind_rows group_by group_split
#' @importFrom purrr map map_dfr
#' @importFrom rlang .data sym
#' @importFrom furrr future_map
#' @importFrom progressr progressor
#' @export
wypelnij_wyniki_wskaznikow <- function(param_df, p2_1, p3, p4, debug_mode = FALSE) {

  # Mapowanie wskaźników do nazw zbiorów danych źródłowych
  wskaznik_to_data_map <- list(
    "S7" = "p3", "K1" = "p3", "W1" = "p3", "B1" = "p3",
    "D1" = "p4", "D2" = "p4", "typ_szk2" = "p4", "meta_zaw" = "p4",
    "K2" = "p2_1"
  )

  # Dynamiczne funkcje (na podstawie stringa)
  funkcje_generujace_global <- list(
    tab_wier = tab_wier, tab_kolu = tab_kolu, tab_msc = tab_msc,
    tab_wier_K1 = tab_wier_K1, tab_kolu_K1 = tab_kolu_K1, tab_wier_K2 = tab_wier_K2,
    tab_wyna = tab_wyna, tab_sz_sex = tab_sz_sex, tab_sz_zaw = tab_sz_zaw
  )

  # Dodajemy tymczasowe ID wiersza, aby zachować oryginalną kolejność
  param_df_indexed <- param_df %>%
    mutate(
      row_id = row_number(),
      data_source_name = purrr::map_chr(wskaznik, ~purrr::pluck(wskaznik_to_data_map, .x, .default = NA_character_))
    )

  if (any(is.na(param_df_indexed$data_source_name))) {
    warning("Znaleziono wskaźniki w `param_df`, dla których nie ma mapowania w `wskaznik_to_data_map`. Te wiersze mogą prowadzić do błędów.")
  }

  # Grupowanie param_df
  grouped_tasks <- param_df_indexed %>%
    group_by(data_source_name, WOJ_NAZWA, typ_szk2, rok_abs) %>%
    group_split()

  p <- progressor(steps = length(grouped_tasks))

  # Funkcja pomocnicza do przetwarzania JEDNEJ GRUPY wierszy param_df
  process_group <- function(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref, is_debug_mode) {

    p()

    data_source_name_for_group <- group_df_chunk$data_source_name[1]

    dane_do_filtrowania_group <- switch(
      data_source_name_for_group,
      "p2_1" = all_p2_1,
      "p3" = all_p3,
      "p4" = all_p4,
      NULL
    )

    if (is.null(dane_do_filtrowania_group)) {
      return(group_df_chunk %>% mutate(wynik = list(data.frame(Uwaga = "Brak zdefiniowanych danych źródłowych dla tej grupy."))))
  }

    group_wskaznik <- group_df_chunk$wskaznik[1]
    group_WOJ_NAZWA <- group_df_chunk$WOJ_NAZWA[1]
    group_typ_szk2 <- group_df_chunk$typ_szk2[1]
    group_rok_abs <- group_df_chunk$rok_abs[1]

    filtered_data_for_group <- dane_do_filtrowania_group

    if (!is.na(group_WOJ_NAZWA) && group_WOJ_NAZWA != "Polska") {
      filtered_data_for_group <- filtered_data_for_group %>% filter(.data$WOJ_NAZWA == group_WOJ_NAZWA)
    }

    if (!is.na(group_typ_szk2)) {
      if (group_typ_szk2 == "Branżowa szkoła I stopnia") {
        filtered_data_for_group <- filtered_data_for_group %>%
          filter(.data$typ_szk2 %in% c("Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia"))
      } else {
        filtered_data_for_group <- filtered_data_for_group %>% filter(.data$typ_szk2 == group_typ_szk2)
      }
    } else if (group_wskaznik == "Grupa_Szkol_Nazwa_Zaw" && !is.null(group_df_chunk$parametr_grupa_szkol[1])) {
      param_szkol_val <- if (is.list(group_df_chunk$parametr_grupa_szkol[1])) {
        unlist(group_df_chunk$parametr_grupa_szkol[1])
      } else {
        group_df_chunk$parametr_grupa_szkol[1]
      }
      filtered_data_for_group <- filtered_data_for_group %>% filter(.data$typ_szk2 %in% param_szkol_val)
    }

    if (!is.na(group_rok_abs)) {
      filtered_data_for_group <- filtered_data_for_group %>% filter(.data$rok_abs == group_rok_abs)
    }

    results_for_group <- group_df_chunk %>%
      rowwise() %>%
      mutate(
        wynik = {
          current_wskaznik <- .data$wskaznik
          current_kryterium <- .data$kryterium
          current_rok <- .data$rok
          current_miesiac <- .data$miesiac
          current_edycja <- .data$edycja
          current_parametr_K <- .data$parametr_K
          current_parametr_grupa_szkol <- .data$parametr_grupa_szkol
          current_row_id_in_group <- .data$row_id

          dane_dla_pojedynczego_wiersza <- filtered_data_for_group

          if (current_wskaznik %in% c("S7", "K1", "W1", "B1") && !is.na(current_rok) && !is.na(current_miesiac)) {
            if (current_wskaznik == "W1") {
              mies_do_filtrowania <- unique(c(current_miesiac, current_miesiac - 1, current_miesiac - 2))
              mies_do_filtrowania <- mies_do_filtrowania[mies_do_filtrowania > 0]
              dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>% filter(.data$rok == current_rok, .data$miesiac %in% mies_do_filtrowania)
            } else {
              dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>% filter(.data$rok == current_rok, .data$miesiac == current_miesiac)
            }
          }

          if (nrow(dane_dla_pojedynczego_wiersza) == 0) {
            return(list(data.frame(Uwaga = paste0("Brak danych źródłowych po filtrowaniu dla wskaźnika: ",
                                                  current_wskaznik, ", kryterium: ", current_kryterium,
                                                  ", ID wiersza (orig): ", current_row_id_in_group))))
          }

          func_name <- switch(
            current_wskaznik,
            "S7" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw" || current_kryterium == "teryt_pow_szk") "tab_kolu"
              else if (current_kryterium == "okres") "tab_msc"
              else NULL
            },
            "D1" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu"
              else NULL
            },
            "D2" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu"
              else NULL
            },
            "K1" = {
              if (current_kryterium == "sexf") "tab_wier_K1"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu_K1"
              else NULL
            },
            "K2" = "tab_wier_K2",
            "W1" = "tab_wyna",
            "B1" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu"
              else if (current_kryterium == "okres") "tab_msc"
              else NULL
            },
            "typ_szk2" = "tab_sz_sex",
            "Grupa_Szkol_Nazwa_Zaw" = "tab_sz_zaw",
            NULL
          )

          if (is.null(func_name)) {
            return(list(data.frame(Uwaga = paste0("Brak zdefiniowanej funkcji dla wskaźnika: ",
                                                  current_wskaznik, ", kryterium: ", current_kryterium,
                                                  ", ID wiersza (orig): ", current_row_id_in_group))))
          }

          args <- list(dat = dane_dla_pojedynczego_wiersza)

          if (func_name %in% c("tab_wier", "tab_kolu", "tab_msc", "tab_wyna")) {
            if (func_name == "tab_msc") {
              args$wskaznik <- rlang::sym(current_wskaznik)
              args$kryterium <- rlang::sym(current_kryterium)
              args$edycja <- current_edycja
            } else if (func_name == "tab_wyna") {
              args$kryterium <- rlang::sym(current_kryterium)
            } else { # tab_wier, tab_kolu
              args$wskaznik <- rlang::sym(current_wskaznik)
              args$kryterium <- rlang::sym(current_kryterium)
            }
          } else if (func_name %in% c("tab_wier_K1", "tab_kolu_K1")) {
            args$wskazniki <- unlist(current_parametr_K)
            args$kryterium <- rlang::sym(current_kryterium)
          } else if (func_name == "tab_wier_K2") {
            args$wskaznik <- rlang::sym(current_parametr_K)
            args$kryterium <- rlang::sym(current_kryterium)
          } else if (func_name %in% c("tab_sz_sex", "tab_sz_zaw")) {
            # Te funkcje przyjmują tylko 'dat'
          }

          # ROBUSTNE TRYCATCH
          tryCatch({
            if (is_debug_mode) { # Upewnij się, że to tylko w trybie debugowania
              message("DEBUG: Próba wywołania funkcji: ", func_name)
              message("DEBUG: Argumenty: ", paste(names(args), collapse = ", "))
            }
            result <- do.call(funcs_gen_ref[[func_name]], args)
            if (!inherits(result, "data.frame")) {
              warning_msg <- paste0("Funkcja '", func_name, "' nie zwróciła ramki danych dla ID wiersza: ", current_row_id_in_group)
              if (is_debug_mode) message(warning_msg)
              return(list(data.frame(Uwaga = warning_msg)))
            }
            list(result)
          }, error = function(e) {
            error_msg <- paste0("Błąd podczas generowania tabeli krzyżowej (", func_name, ") dla wiersza ID: ",
                                current_row_id_in_group, ", wskaźnik: ", current_wskaznik,
                                ", kryterium: ", current_kryterium, ", błąd: ", e$message)
            if (is_debug_mode) {
              message("Fatal error in row ", current_row_id_in_group, ": ", error_msg)
              # UWAGA: Użyj browser() lub recover() dla głębszego debugowania
              # browser() # Zatrzyma wykonywanie i pozwoli wejść w tryb interaktywny
              # lub
              recover() # Pozwoli wybrać ramkę stosu do przeglądania (bardziej zaawansowane)
              stop(error_msg, call. = FALSE) # Nadal zatrzyma po browser()/recover()
            }
            list(data.frame(Uwaga = error_msg))
          })
        }
      ) %>%
      ungroup()

    return(results_for_group)
}

  if (debug_mode) {
    message("Uruchamianie w trybie debugowania (sekwencyjnym).")
    final_param_df_results <- purrr::map_dfr(
      .x = grouped_tasks,
      .f = function(group_df_chunk) {
        process_group(group_df_chunk, p2_1, p3, p4, funkcje_generujace_global, debug_mode)
      }
    )
  } else {
    message("Uruchamianie w trybie równoległym.")
    # Ustaw plan równoległy (poza funkcją, ale przypomnienie)
    # future::plan(multisession, workers = 15)

    final_param_df_results <- furrr::future_map_dfr(
      .x = grouped_tasks,
      .f = function(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref, is_debug_mode) {
        process_group(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref, is_debug_mode)
      },
      all_p2_1 = p2_1,
      all_p3 = p3,
      all_p4 = p4,
      funcs_gen_ref = funkcje_generujace_global,
      is_debug_mode = debug_mode, # Przekazujemy debug_mode do workerów
      .options = furrr_options(
        globals = c("process_group", "wskaznik_to_data_map",
                    "dplyr", "purrr", "rlang", "future", "furrr", "progressr",
                    "tab_wier", "tab_kolu", "tab_msc", "tab_wier_K1", "tab_kolu_K1",
                    "tab_wier_K2", "tab_wyna", "tab_sz_sex", "tab_sz_zaw"
        ),
        packages = c("dplyr", "purrr", "rlang", "future", "furrr", "progressr")
      )
    )
  }

  return(final_param_df_results %>%
           arrange(.data$row_id) %>%
           select(-.data$row_id, -.data$data_source_name))
  }