library(tidyverse)
library(plotly)

options(scipen = 999)

gRate <- function(df, x, y) {
  # growth rate of ertekesites_netto_arbevetele
  # calculation base don logdiff >> ensure robust results
  formula <- str_c("log(", y, ") ~ ", x)
  lm(formula = eval(formula), data = df) %>% 
    coef() %>% 
    .[2]*100
}

gRate <- possibly(gRate, as.numeric(NA), TRUE)


# ggplot themes -----------------------------------------------------------

update_geom_defaults("point", list(fill = "#C42E35", 
                                   shape = 21, 
                                   color = "black", 
                                   size = 1.4))
update_geom_defaults("line", 
                     list(color = "#C42E35", size = 1.4))
update_geom_defaults("smooth", list(color = "#C42E35", size = 1.4))
update_geom_defaults("col", list(fill = "#2DA2BF"))
update_geom_defaults("bar", list(fill = "#2DA2BF"))
update_geom_defaults("hline", list(yintercept = 0, color = "grey50", size = 1))
update_geom_defaults("vline", list(xintercept = 0, color = "grey50", size = 1))
update_geom_defaults("density", 
                     list(color = "#C42E35", fill =  "#C42E35", alpha = .3, 
                          size = 1.4))
theme_set(
  theme_minimal() + 
    theme(
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      axis.title.y = element_text(size = 13)
    )
)

myplotly <- function(p) {
font = list(
  family = "Alright Sans Regular", 
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#2DA2BF",
  bordercolor = "transparent",
  font = font
)

tooltip = "text"
plotly::ggplotly(p, tooltip = tooltip) %>% 
  plotly::style(hoverlabel = label) %>% 
  plotly::layout(font = font) %>% 
  plotly::config(displayModeBar = FALSE)
}

FormatMoney <- function(x, digits = 0) {
  x <- as.numeric(x)
  case_when(
    abs(x) >= 1e9 ~ str_c(format(x / 1e9, digits = digits), " mrd Ft"),
    abs(x) >= 1e6 ~ str_c(format(x / 1e6, digits = digits), " m Ft"),
    abs(x) >= 1e3 ~ str_c(format(x / 1e3, digits = digits), " ezer Ft"),
    T ~ str_c(format(x, digits = digits), " Ft")
  )
}

NiceName <- function(x) {
  case_when(
    x == "nev" ~ "Cégnév",
    x == "crefo_id" ~ "Crefo azonosító",
    x == "alapitas_eve" ~ "Alapítás éve",
    x == "megye" ~ "Megye",
    x == "letszam" ~ "Létszám",
    x == "ertekesites_netto_arbevetele" ~ "Értékesítés nettó árbevétele",
    x == "aktivalt_sajat_teljesitmenyek_erteke" ~ "Aktivált saját teljesítmények értéke",
    x == "anyagjellegu_raforditasok" ~ "Anyagjellegű ráfordítások",
    x == "szemelyi_jellegu_raforditasok" ~ "Személyi jellegű ráfordítások",
    x == "ertekcsokkenesi_leiras" ~ "Értékscökkenési leírás",
    x == "uzemi_uzleti_tevekenyseg_eredmenye" ~ "Üzemi üzleti tevékenységek eredménye",
    x == "export_ertekesites_netto_arbevetele" ~ "Export értékesítés nettó árbevétele",
    x == "rovid_lejaratu_kotelezettsegek" ~ "Rövid lejáratú kötelezettségek",
    x == "hosszu_lejaratu_kotelezettsegek" ~ "Hosszú lejáratú kötelezettségek",
    x == "sajat_toke" ~ "Saját tőke",
    x == "jegyzett_toke" ~ "Jegyzett tőke",
    x == "toketartalek" ~ "Tőketartalék",
    x == "adozott_eredmeny" ~ "Adózott eredmény",
    x == "forgoeszkozok" ~ "Forgó eszközök",
    x == "BHE1" ~ "Bruttó hozzáadott értéke",
    x == "BHE2" ~ "Bruttó hozzáadott értéke (2)",
    x == "arbevetel_aranyos_nyereseg" ~ "Árbevétel arányos nyereség",
    x == "eladosottsag" ~ "Eladósodottság",
    x == "export_arbevetel_aranya" ~ "Export árbevétel aránya",
    x == "likviditas" ~ "Likviditás",
    x == "telepules" ~ "Település",
    x == "kkv" ~ "Működési forma",
    x == "jogi_statusz" ~ "Jogi státusz",
    x == "tulajdonos_orszag_kod" ~ "Legnagyobb tulajdonos országkódja",
    x == "reszesedes" ~ "Legnagyobb tulajdonos részesedése",
    x == "g_rate" ~ "Elmúlt évek árbevételénke növekedési üteme",
    x == "teaor_code" ~ "TEAOR kód",
    x == "teaor_group" ~ "TEAOR besorolás",
    x == "teaor_name" ~ "Szakágazatai besorolás",
    x == "pontos_hely" ~ "Cím",
    TRUE ~ x
  )
}

NiceFormat <- function(x, y) {
  case_when(
    x == "nev" ~ as.character(y),
    x == "crefo_id" ~ as.character(y),
    x == "alapitas_eve" ~ as.character(y),
    x == "megye" ~ as.character(y),
    x == "letszam" ~ as.character(y),
    x == "ertekesites_netto_arbevetele" ~ FormatMoney(y),
    x == "aktivalt_sajat_teljesitmenyek_erteke" ~ FormatMoney(y),
    x == "anyagjellegu_raforditasok" ~ FormatMoney(y),
    x == "szemelyi_jellegu_raforditasok" ~ FormatMoney(y),
    x == "ertekcsokkenesi_leiras" ~ FormatMoney(y),
    x == "uzemi_uzleti_tevekenyseg_eredmenye" ~ FormatMoney(y),
    x == "export_ertekesites_netto_arbevetele" ~ FormatMoney(y),
    x == "rovid_lejaratu_kotelezettsegek" ~ FormatMoney(y),
    x == "hosszu_lejaratu_kotelezettsegek" ~ FormatMoney(y),
    x == "sajat_toke" ~ FormatMoney(y),
    x == "jegyzett_toke" ~ FormatMoney(y),
    x == "toketartalek" ~ FormatMoney(y),
    x == "adozott_eredmeny" ~ FormatMoney(y),
    x == "forgoeszkozok" ~ FormatMoney(y),
    x == "BHE1" ~ FormatMoney(y),
    x == "BHE2" ~ FormatMoney(y),
    x == "arbevetel_aranyos_nyereseg" ~ FormatMoney(y),
    x == "eladosottsag" ~ FormatMoney(y),
    x == "export_arbevetel_aranya" ~ scales::percent(as.integer(y), accuracy = 1),
    x == "likviditas" ~ scales::percent(as.integer(y), accuracy = 1),
    x == "telepules" ~ as.character(y),
    x == "kkv" ~ as.character(y),
    x == "jogi_statusz" ~ as.character(y),
    x == "tulajdonos_orszag_kod" ~ as.character(y),
    x == "reszesedes" ~ scales::percent(as.integer(y), accuracy = 1),
    x == "g_rate" ~ scales::percent(as.integer(y) / 100, accuracy = 1),
    x == "teaor_code" ~ as.character(y),
    x == "teaor_group" ~ as.character(y),
    x == "teaor_name" ~ as.character(y),
    x == "pontos_hely" ~ as.character(y),
    TRUE ~ y
  )
}

