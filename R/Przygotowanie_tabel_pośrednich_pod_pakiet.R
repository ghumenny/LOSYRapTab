#' @title Przekształcenie tabeli p2 do formatu szerokiego
#' #' @description Ta funkcja przetwarza ramkę danych 'p2' w celu zsumowania
#' wystąpień dziedzin i dyscyplin dla każdego absolwenta, roku i okresu.
#' @param p2 Ramka danych 'p2' do przetworzenia.
#' @return Zwraca przetworzoną ramkę danych.
#' @import dplyr
#' @import tidyr
#' @export
przetwarzaj_p2 <- function(p2) {
  p2_1_proba <- p2 %>%
    filter(!is.na(dyscyplina_wiodaca_kont)) %>%
    mutate(dziedzina_kont = sub("^Dziedzina ", "", dziedzina_kont),
           across(c(dziedzina_kont, dyscyplina_wiodaca_kont), factor)) %>%
    group_by(id_abs, rok_abs, okres) %>%
    summarise(dziedzina =
                list(as.data.frame(t(as.matrix(table(dziedzina_kont))))),
              dyscyplina_wiodaca =
                list(as.data.frame(t(as.matrix(table(dyscyplina_wiodaca_kont))))),
              .groups = "drop") %>%
    unnest(c(dziedzina, dyscyplina_wiodaca)) %>%
    mutate(across(-c(id_abs, rok_abs, okres), sign))

  return(p2_1_proba)
}


#' @title Przygotowanie zbiorów danych p2, p3 i p4
#' @description Funkcja, która przetwarza ramki danych p2, p3 i p4,
#'              tworząc z nich lżejsze wersje z wybranymi i przekształconymi
#'              kolumnami.
#' @param p2 Ramka danych w formacie tibble.
#' @param p3 Ramka danych w formacie tibble.
#' @param p4 Ramka danych w formacie tibble.
#' @return Zwraca listę z trzema ramkami danych: p2_1_light, p3_light, p4_light.
#' @importFrom dplyr %>% inner_join select mutate rename left_join if_else
#' @importFrom tidyr unite
#' @importFrom stringr str_pad
#' @export
przygotuj_p2p3p4 <- function(p2, p3, p4) {


  # Przetwarzanie zbioru p4
  p4_light <- p4 %>%
    mutate(
      sexf = factor(
        ifelse(plec == "K", 1, 0),
        levels = c(0, 1),
        labels = c("Mężczyzna",
                   "Kobieta")),
      D2 = factor(
        matura_zdana,
        levels = c(1, 0),
        labels = c("Uzyskanie świadectwa dojrzałości",
                   "Brak świadectwa dojrzałości")),
      typ_szk2 = factor(case_when(typ_szk_mlodoc == "Liceum ogólnokształcące" ~ "Liceum ogólnokształcące dla młodzieży",
                           typ_szk_mlodoc == "Liceum dla dorosłych" ~ "Liceum ogólnokształcące dla dorosłych",
                           TRUE ~ typ_szk_mlodoc),
                        levels = c(
                          "Liceum ogólnokształcące dla młodzieży",
                          "Liceum ogólnokształcące dla dorosłych",
                          "Branżowa szkoła I stopnia",
                          "Młodociani w Branżowej szkole I stopnia",
                          "Niemłodociani w Branżowej szkole I stopnia",
                          "Technikum",
                          "Branżowa szkoła II stopnia",
                          "Szkoła policealna",
                          "Szkoła specjalna przysposabiająca do pracy"
                        ))) %>%
    select(id_abs, rok_abs, WOJ_NAZWA = nazwa_woj_szk, branza, typ_szk2, sexf,
           nazwa_zaw, teryt_pow_szk, D1 = dyplom_zaw, D2)
#  p4_light$typ_szk2 <- droplevels(p4_light$typ_szk2)

  # Przetwarzanie zbioru p2
  p2_1_light <- przetwarzaj_p2(p2) %>%
    left_join(p4_light %>%
                 select(id_abs, rok_abs, sexf, typ_szk2,
                        WOJ_NAZWA, branza, nazwa_zaw),
               by = c("id_abs", "rok_abs")) %>%
    # mutate(
    #   sexf = factor(
    #     ifelse(plec == "K", 1, 0),
    #     levels = c(0, 1),
    #     labels = c("Mężczyzna",
    #                "Kobieta"))) %>%
    rename(okres_kont = okres)
#  p2_1_light$typ_szk2 <- droplevels(p2_1_light$typ_szk2)


  # Przetwarzanie zbioru p3
  p3_light <- p3 %>%
    left_join(p4_light %>% select(id_abs, rok_abs, teryt_pow_szk, WOJ_NAZWA,
                            typ_szk2, sexf, nazwa_zaw, branza),
              by = c("id_abs" = "id_abs", "rok_abs" = "rok_abs")) %>%
    unite("mscrok", c(miesiac, rok), remove = FALSE, sep = ".") %>%
    mutate(
      # sexf = factor(
      #   ifelse(plec == "K", 1, 0),
      #   levels = c(0, 1),
      #   labels = c("Mężczyzna", "Kobieta")),
      B1 = factor(
        ifelse(is.na(bezrobocie) | bezrobocie < 1, 0, 1),
        levels = c(0, 1),
        labels = c("Brak statusu bezrobotnego", "Zarejestrowany jako bezrobotny")),
      nauka_bs2stf = factor(
        ifelse(nauka_bs2st >= 1, 1, 0),
        levels = c(0, 1),
        labels = c("Brak kontynuacji nauki w BS II", "Kontynuacja nauki w BS II")),
      nauka_loddf = factor(
        ifelse(nauka_lo == 1, 1, 0),
        levels = c(0, 1),
        labels = c("Brak kontynuacji nauki w liceum dla dorosłych", "Kontynuacja nauki w liceum dla dorosłych")),
      nauka_spolicf = factor(
        ifelse(nauka_spolic %in% c(1, 2, 3), 1, 0),
        levels = c(0, 1),
        labels = c("Brak kontynuacji nauki w szkole policealnej", "Kontynuacja nauki w szkole policealnej")),
      nauka_studiaf = factor(
        ifelse(nauka_studia == 1, 1, 0),
        levels = c(0, 1),
        labels = c("Brak kontynuacji nauki na studiach", "Kontynuacja nauki na studiach")),
      nauka_kkzf = factor(
        ifelse(nauka_kkz == 1, 1, 0),
        levels = c(0, 1),
        labels = c("Brak kontynuacji nauki w ramach KKZ", "Kontynuacja nauki w ramach KKZ")),
      teryt_woj = as.character(teryt_zam) %>%
        str_pad(width = 4, side = "left", pad = "0") %>%
        substr(1, 2) %>%
        as.numeric() %>%
        as.character()
    ) %>%
    select(id_abs, rok_abs, rok, miesiac, mscrok, WOJ_NAZWA,
           branza, typ_szk2, teryt_pow_szk, sexf, nazwa_zaw,
           S7 = status, nauka_studiaf, nauka_spolicf, nauka_kkzf,
           nauka_bs2stf, nauka_loddf, B1, W1 = wynagrodzenie, teryt_woj)
#  p3_light$typ_szk2 <- droplevels(p3_light$typ_szk2)

  # Zwracanie listy z wynikowymi ramkami danych
  return(list(p2_1_light = p2_1_light,
              p3_light = p3_light,
              p4_light = p4_light))
}
