library(shiny)
library(shinymanager)
library(shinydashboard)
library(leaflet)
library(googlesheets4)

# TODO all companies
# TODO download button

# setup -------------------------------------------------------------------

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



update_geom_defaults("point", list(fill = "#C42E35", 
                                   shape = 21, 
                                   color = "black", 
                                   size = 1.4))
update_geom_defaults("line", list(color = "#C42E35", size = 1.4))
update_geom_defaults("smooth", list(color = "#C42E35", size = 1.4))
update_geom_defaults("col", list(fill = "#2DA2BF"))
update_geom_defaults("bar", list(fill = "#2DA2BF"))
update_geom_defaults("hline", list(yintercept = 0, color = "grey50", size = 1))
update_geom_defaults("vline", list(xintercept = 0, color = "grey50", size = 1))
update_geom_defaults("density", list(color = "#C42E35", fill =  "#C42E35", alpha = .3, 
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


source("df_utf8.R", echo = FALSE, encoding = 'UTF-8')

locations_to_companies <- readxl::read_excel("locations_to_companies.xlsx") %>% 
  mutate(
    crefo_id = as.character(crefo_id)
  )

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

gs4_auth(cache = ".secrets")
gs4_auth(cache = ".secrets", email = "granatcellmar98@gmail.com", use_oob = TRUE)

records <- read_sheet("https://docs.google.com/spreadsheets/d/1hQe-8OhkaqqaRKc-gXm_dFqrNz58C94hBhmyeq6YwHo/edit#gid=0") 



dbHeader <- dashboardHeader(title = "Ágazati 1000",
                            tags$li(a(href = 'https://makronomintezet.hu/',
                                      img(src = 'company_logo.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

credentials <- data.frame(
  user = c("1", "Purczeld Eszter", "Kerekes György", "Blazsanik Bernadett", "Hartmann Anita", "Mikó Márton", "Granát Marcell", "Faragó Lili", "Bajzai Viktória", "Tarjáni Zsolt", "Tóth Csaba", "Bató Alex", "Dányi Bernadett", "Benedeczki Tamás", "Nébald Sarolta", "Vékásy Zsuzsa", "Csontos Szabolcs"),
  password = c("1", "Purczeld Eszter", "Kerekes György", "Blazsanik Bernadett", "Hartmann Anita", "Mikó Márton", "Granát Marcell", "Faragó Lili", "Bajzai Viktória", "Tarjáni Zsolt", "Tóth Csaba", "Bató Alex", "Dányi Bernadett", "Benedeczki Tamás", "Nébald Sarolta", "Vékásy Zsuzsa", "Csontos Szabolcs"),
  stringsAsFactors = FALSE
)

# UI ----------------------------------------------------------------------

ui <- secure_app(head_auth = tags$script(inactivity),
                 dashboardPage(skin = "yellow",
                               dbHeader,
                               dashboardSidebar(
                                 sidebarMenu(
                                   menuItem("Szűrők", tabName = "filters", icon = icon("filter"),
                                            h1(textOutput("n_companies")),
                                            selectInput("teaor", "TEAOR beosztás", choices = c("Összes", sort(as.character(unique(df$teaor_group))))),
                                            selectInput("megye", "Megye", choices = c("Összes", sort(as.character(unique(df$megye))))),
                                            sliderInput("arbevetel", label = "Árbevétel (mrd Ft)",
                                                        min = 0,
                                                        max = ceiling(max(df$ertekesites_netto_arbevetele, na.rm = TRUE) / 1e9),
                                                        value = c(0,
                                                                  ceiling(max(df$ertekesites_netto_arbevetele, na.rm = TRUE) / 1e9)
                                                        )
                                            ),
                                            
                                            sliderInput("letszam", label = "Létszám (fő)",
                                                        min = 0,
                                                        max = max(df$letszam, na.rm = TRUE),
                                                        value = c(0,
                                                                  max(df$letszam, na.rm = TRUE)
                                                        )
                                            ),
                                            
                                            sliderInput("export", label = "Export arány (%)",
                                                        min = 0,
                                                        max = 100,
                                                        value = c(0, 100)
                                            ),
                                            dateRangeInput(
                                              "choose_date",
                                              h3("Elérhető ekkor"),
                                              start ="2021-06-21",
                                              end = "2021-08-31",
                                              min = NULL,
                                              max = NULL,
                                              format = "mm-dd",
                                              startview = "month",
                                              language = "hu",
                                              separator = "-",
                                              weekstart = 1
                                            ),
                                            checkboxInput("only_looked", "Csak felkeresettek", FALSE),
                                            checkboxInput("hide_reserved", "Foglaltak elrejtése", FALSE),
                                            checkboxInput("show_decline", "Visszautasítók mutatása", FALSE),
                                            actionButton(inputId = "reload", label = "Elérhetőségek frissítése", icon("refresh"))
                                   ),
                                   menuItem("Térkép", tabName = "map", icon = icon("map")),
                                   menuItem("Táblázat", tabName  = "table", icon = icon("th")),
                                   menuItem("Céginfo", tabName  = "info", icon = icon("building")),
                                   menuItem("Adminisztráció", tabName = "admin", icon = icon("book")),
                                   menuItem("Az én cégeim", tabName = "user", icon = icon("user"))
                                 )
                               ),
                               
                               dashboardBody(
                                 tabItems(
                                   tabItem(tabName = "map",
                                           box(width = 5, title = "Térkép opciók", 
                                               collapsible = TRUE, collapsed = TRUE,
                                               checkboxInput("colleagues", "Interjúztatók megjelenítése", FALSE),
                                               checkboxInput("show_car", "Céges autók megjelenítése", FALSE),
                                               checkboxInput("show_firms", "Cégek megjelenítése", TRUE)
                                           ),
                                           box(width = 7, title = "Jelmagyarázat", 
                                               collapsible = TRUE, collapsed = TRUE,
                                               h4("Kék: még nem kerestük és nem is jelentkezett senki."),
                                               h4("Zöld: már van jelentkező."),
                                               h4("Vörös: nem fogad minket."),
                                           ),
                                           box(width = 12,
                                               leaflet::leafletOutput("lef", height = "850px")
                                           )
                                   ),
                                   tabItem(
                                     tabName = "table",
                                     DT::DTOutput("table1")
                                   ),
                                   tabItem(
                                     tabName = "info", 
                                     box(width = 8,
                                         selectizeInput(
                                           inputId = 'search',
                                           label = 'Cégkereső',
                                           choices = sort(df$nev),
                                           selected = "ARANY DELFIN Ipari és Kereskedelmi Korlátolt Felelősségű Társaság",
                                           multiple = FALSE, # allow for multiple inputs
                                           options = list(create = FALSE) # if TRUE, allows newly created inputs
                                         ),
                                         tableOutput("company_summary")
                                         # FIXME
                                         #,
                                         # uiOutput("downloadbutton")
                                     )
                                   ),
                                   tabItem(
                                     tabName = "admin",
                                     box(
                                       selectizeInput(
                                         inputId = 'admin_search',
                                         label = 'Cégkereső',
                                         choices = sort(df$nev),
                                         selected = "ARANY DELFIN Ipari és Kereskedelmi Korlátolt Felelősségű Társaság",
                                         multiple = F, 
                                         options = list(create = FALSE)
                                       ),
                                       shiny::uiOutput("admin_allowed"),
                                       strong(shiny::textOutput("admin_visitors")),
                                       shiny::uiOutput("admin_go"),
                                       shiny::uiOutput("admin_car"),
                                       shiny::uiOutput("admin_address"),
                                       shiny::uiOutput("admin_date"),
                                       shiny::uiOutput("admin_note"),
                                       shiny::actionButton("new_record", label = "Rögzítés", class = "btn-warning")
                                     )
                                   ),
                                   tabItem(
                                     tabName = "user",
                                     box(width = 12, 
                                         title = "Feljelentkezett cégeid Gantt ábrája",
                                         selectInput("gantt_show", label = "", choices = c("Minden feljelentkezés",
                                                                                           "Csak dátummal rendelkezők",
                                                                                           "Csak fix dátummal rendelkezők")),
                                         timevis::timevisOutput("gantt_chart"),
                                         collapsible = TRUE
                                     ),
                                     box(
                                       title = "Általad meglátogatni kívánt cégek",
                                       DT::dataTableOutput("my_data_table")
                                     ),
                                     box(
                                     title = "Meglátogatott cégek",
                                         DT::dataTableOutput("users_done_table")
                                     )
                                   )
                                 )
                               )
                 )
)


# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  
  observeEvent(eventExpr = input$reload, handlerExpr = {
    records <<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hQe-8OhkaqqaRKc-gXm_dFqrNz58C94hBhmyeq6YwHo/edit#gid=0")
  })
  
  df_filtered <- reactive({  # FIXME
    input$admin_message_ok
    input$reload
    if (input$teaor != "Összes") {
      df <- filter(df, teaor_group == input$teaor)
    }
    if (input$megye != "Összes") {
      df <- filter(df, megye == input$megye)
    }
    
    df <- df %>%
      filter(
        ertekesites_netto_arbevetele >= input$arbevetel[1]*1e9 &
          ertekesites_netto_arbevetele <= input$arbevetel[2]*1e9 &
          letszam >= input$letszam[1] &
          letszam <= input$letszam[2] &
          export_arbevetel_aranya >= input$export[1] &
          export_arbevetel_aranya <= input$export[2]
      )
    
    
    df <- records %>%
      group_by(crefo_id) %>%
      group_modify(~ tail(.x, 1)) %>%
      ungroup() %>%
      right_join(df)
    
    df <- records %>%
      group_by(crefo_id, user) %>%
      group_modify(~ tail(.x, 1)) %>%
      ungroup() %>%
      filter(user_go) %>%
      group_by(crefo_id) %>%
      summarise(visitors = str_c(user, collapse = ", ")) %>%
      ungroup() %>%
      right_join(df)
    
    df <- df %>%
      mutate(
        start_date = lubridate::ymd(start_date),
        end_date = lubridate::ymd(end_date)
      ) %>% 
      replace_na(list(allow = "Még nem lett felkeresve",
                      visitors = "Még senki nem jelentkezett ennek a cégnek a meglátogatására.",
                      start_date = "2021-06-21",
                      end_date = "2021-08-31"
      ))  %>%
      filter(lubridate::ymd(start_date) >= lubridate::ymd(input$choose_date[1]) &
               lubridate::ymd(end_date) <= lubridate::ymd(input$choose_date[2]))
    
    if (input$only_looked) {
      df <- df %>%
        filter(allow != "Még nem lett felkeresve")
    }
    
    if (input$hide_reserved) {
      df <- df %>%
        filter(visitors == "Még senki nem jelentkezett ennek a cégnek a meglátogatására.")
    }
    
    if (!input$show_decline) {
      df <- df %>%
        filter(allow != "Nem")
    }
    
    df %>% 
      filter(allow != "A cég már megszűnt")
  })
  
  output$n_companies <- renderText({
    str_c(nrow(df_filtered()), " találat!")
  })
  
  output$lef <- renderLeaflet({
    input$new_record
    
    df_lef <- df_filtered() %>%
      left_join(locations_to_companies) %>%
      mutate(
        visitors = ifelse(visitors != "Még senki nem jelentkezett ennek a cégnek a meglátogatására.", str_c("látogatók: ", visitors), visitors),
        type = case_when(
          allow == "Nem" ~ "reject",
          visitors != "Még senki nem jelentkezett ennek a cégnek a meglátogatására." ~ "reserved",
          TRUE ~ "unquestioned"
        ),
        lat = Latitude, lng = Longitude, color = teaor_group, type = type,
        popup = str_c(
          "<b>", nev, "</b>", "<br/>",
          pontos_hely, "<br/>",
          teaor_group, " - ", teaor_name, "<br/>",
          "Létszám: ", letszam, "<br/>",
          "Árbevétel: ", FormatMoney(ertekesites_netto_arbevetele), "<br/>",
          "<b>", visitors, "</b>"
        )
      )
    
    
    add_icon <- function(x) {
      icon_color <- case_when(
        x == "reject" ~ "red",
        x == "unquestioned" ~ "blue",
        x == "reserved" ~ "green"
      )
      awesomeIcons(
        iconColor = 'black',
        library = 'ion',
        markerColor = icon_color
      )
    }
    
    out <- df_lef %>% 
      filter(!duplicated(crefo_id)) %>%
      leaflet() %>%
      addTiles()
    
    if (input$show_firms) {
      out <- out %>%
        addAwesomeMarkers(~Longitude, ~Latitude, popup = ~popup, label = ~ nev,
                          icon = ~add_icon(type))
    }
    
    selected_id <- df_filtered() %>% 
      pull(crefo_id)
    
    if (input$colleagues) {
      
      df_colleagues <- records %>% 
        group_by(user, crefo_id) %>% 
        group_modify(~ tail(.x, 1)) %>% 
        left_join(locations_to_companies) %>% 
        filter(user_go & crefo_id %in% selected_id) %>% 
        left_join(select(df, crefo_id, nev)) %>% 
        distinct()
      
      df_colleagues$add_lat <- runif(nrow(df_colleagues), min = -4e-2, max = 4e-2)
      
      df_colleagues <- df_colleagues %>% 
        mutate(
          add_long = (4e-2^2 - add_lat^2)^.5,
          add_long = ifelse(str_detect(pontos_hely, "Budapest"), add_long / 50, add_long),
          add_lat = ifelse(str_detect(pontos_hely, "Budapest"), add_lat / 50, add_lat),
          Longitude = Longitude + add_long,
          Latitude = Latitude + add_lat
        ) %>% 
        mutate(
          short_user = map_chr(user, function(x) {
            str_split(x, " ") %>% 
              .[[1]] %>% 
              str_sub(end = 1) %>% 
              str_c(collapse = "")
          }),
          popup = str_c(
            "<b>", user, "</b>", "<br/>",
            nev, "<br/>",
            "Ekkor: ", ifelse(start_date == end_date, as.character(lubridate::ymd(start_date)), 
                              str_c(as.character(lubridate::ymd(start_date)), " - ", as.character(lubridate::ymd(end_date))))
          )
        )
      
      add_colleague_icon <- function(x) {
        awesomeIcons(
          icon = "user",
          iconColor = 'yellow',
          library = 'ion',
          markerColor = "black",
          text = x
        )
      }
      
      out <- out %>% 
        addAwesomeMarkers(data = df_colleagues, ~Longitude, ~ Latitude, 
                          icon = ~ add_colleague_icon(short_user),
                          popup = ~popup
        )
    }
    
    if (input$show_car) {
      
      df_car <- records %>% 
        group_by(crefo_id) %>% 
        group_modify(~ tail(.x, 1)) %>% 
        ungroup() %>% 
        filter(car != "Saját autóval" & car != "Autó nélkül" & crefo_id %in% selected_id) %>% 
        left_join(locations_to_companies) %>% 
        left_join(select(df, crefo_id, nev))
      
      df_car$add_lat <- runif(nrow(df_car), min = -2e-2, max = 2e-2)
      
      df_car <- df_car %>% 
        mutate(
          add_long = (2e-2^2 - add_lat^2)^.5,
          add_long = ifelse(str_detect(pontos_hely, "Budapest"), add_long / 50, add_long),
          add_lat = ifelse(str_detect(pontos_hely, "Budapest"), add_lat / 50, add_lat),
          Longitude = Longitude + add_long,
          Latitude = Latitude + add_lat
        ) %>% 
        mutate(
          popup = str_c(
            "<b>", car, "</b>", " (adminisztrálta: ", user, ")", "<br/>",
            nev, "<br/>",
            "Ekkor: ", ifelse(start_date == end_date, as.character(lubridate::ymd(start_date)), 
                              str_c(as.character(lubridate::ymd(start_date)), " - ", as.character(lubridate::ymd(end_date))))
          )
        ) %>% 
        mutate(
          icon_type = case_when(
            car == "SST-865" ~ "a",
            car == "SST-866" ~ "b",
            car == "SST-908" ~ "c",
            car == "Van igény céges autóra" ~ "d"
          )
        )
      
      
      car_icons <-  iconList( 
        a = makeIcon(
          iconUrl = "icons/corsa_red.png", 
          iconWidth = 80, iconHeight = 50),
        b = makeIcon(
          iconUrl = "icons/corsa_blue.png", 
          iconWidth = 80, iconHeight = 50),
        c = makeIcon(
          iconUrl = "icons/corsa_orange.png", 
          iconWidth = 80, iconHeight = 50),
        d = makeIcon(
          iconUrl = "icons/car_needed.png", 
          iconWidth = 50, iconHeight = 50)
      )
      
      
      add_car_icon <- function(x) {
        if (x == "SST-866") {
          car_icon_1
        }
        if (x == "SST-908") {
          car_icon_2
        }
        if (x == "SST-865") {
          car_icon_3
        }
        if (x == "Van igény céges autóra") {
          car_icon_4
        }
      }
      
      out <- out %>% 
        addTiles() %>% 
        addMarkers(data = df_car, ~Longitude, ~ Latitude, label = ~ car, popup = ~ popup, icon = ~car_icons[icon_type])
      
    }
    
    out
  })
  
  output$table1 <- DT::renderDT({
    df <- df_filtered() %>%
      left_join(locations_to_companies) %>%
      select(nev, teaor_group, teaor_name, ertekesites_netto_arbevetele,
             letszam, export_arbevetel_aranya, pontos_hely) %>%
      mutate(
        ertekesites_netto_arbevetele = FormatMoney(ertekesites_netto_arbevetele),
        export_arbevetel_aranya = scales::percent(export_arbevetel_aranya, accuracy = 1)
      ) %>%
      set_names("Cégnév", "TEAOR", "Szakági besorolás", "Árbevétel", "Létszám", "Export arány", "Cím")
    rownames(df) <- NULL
    
    DT::datatable(df,
                  extensions = c('Buttons', "Scroller"),
                  options = list(
                    deferRender = TRUE,
                    scrollY = 800,
                    scroller = TRUE,
                    style = "bootstrap4",
                    searching = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ),
                  
                  class = "display"
    )
  })
  
  output$company_summary <- renderTable({
    if (!is.null(input$search)) {
      df <- df %>%
        filter(nev == input$search) %>%
        select(nev, crefo_id, alapitas_eve, megye, everything())
      df <- df %>%
        mutate(
          teaor_group = str_c(unique(df$teaor_group), collapse = ", "),
          teaor_name = str_c(unique(df$teaor_name), collapse = ", ")
        ) %>%
        head(1)
      
      
      df %>%
        left_join(select(locations_to_companies, crefo_id, pontos_hely)) %>%
        mutate(
          pontos_hely = str_c(megye, " megye, ", pontos_hely),
          pontos_hely = str_remove_all(pontos_hely, "Budapest megye, ")
        ) %>%
        select(nev, crefo_id, pontos_hely,
               kkv, teaor_group, teaor_name,
               ertekesites_netto_arbevetele, g_rate,
               export_arbevetel_aranya, letszam, everything()
        ) %>%
        select(-jogi_statusz, -megye, -telepules, -teaor_code, -BHE2) %>%
        t() %>%
        data.frame() %>%
        rownames_to_column() %>%
        set_names("variable", "value") %>%
        mutate(
          value = NiceFormat(variable, value),
          variable = NiceName(variable)
        ) %>% 
        set_names("Változó", "Érték")
    }
  })
  
  output$downloadbutton <- renderUI({
    if (!is.null(input$search)) {
      downloadButton("report", "Részletes összefoglaló letöltése")
    }
  })
  
  output$report <- downloadHandler(
    
    filename = function() {
      str_c("REPORT_", snakecase::to_snake_case(input$search), ".doc")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(nev = input$search)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  
  observeEvent(eventExpr = input$new_record, handlerExpr = {
    
    appended_df <- data.frame(
      crefo_id = df$crefo_id[which(df$nev == input$admin_search)],
      system_time = Sys.time(),
      user = reactiveValuesToList(result_auth)$user,
      allow = input$admin_allowed,
      address = input$admin_address,
      user_go = input$admin_go,
      car = input$admin_car,
      start_date = input$admin_date[1],
      end_date = input$admin_date[2],
      note = input$admin_note
    )
    
    googlesheets4::sheet_append(appended_df, ss = "https://docs.google.com/spreadsheets/d/1hQe-8OhkaqqaRKc-gXm_dFqrNz58C94hBhmyeq6YwHo/edit#gid=0")
    
    records <<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hQe-8OhkaqqaRKc-gXm_dFqrNz58C94hBhmyeq6YwHo/edit#gid=0") 
    records_reactive$records <<- records
    
    check_df <- records %>% 
      tail(1) %>% 
      bind_rows(appended_df) %>% 
      select(user, allow, address, user_go, car) %>% 
      distinct() %>% 
      nrow()
    if (check_df == 1) {
      showModal(
        modalDialog(
          span('Sikeres rögzítés!'),
          footer = tagList(actionButton("admin_message_ok", "OK")
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          span('Hiba történt'),
          easyClose = TRUE
        )
      )
    }
  })
  
  observeEvent(input$admin_message_ok, {
    removeModal()
  })
  
  company_records <- reactive({
    input$admin_search
    null_df <- data.frame(
      crefo_id = df$crefo_id[which(df$nev == input$admin_search)],
      system_time = NA,
      user = reactiveValuesToList(result_auth)$user,
      allow = "Még nem lett felkeresve",
      car = "Céges autó nélkül",
      address = locations_to_companies$pontos_hely[which(locations_to_companies$crefo_id == df$crefo_id[which(df$nev == input$admin_search)])],
      user_go = TRUE,
      car = FALSE,
      start_date = lubridate::ymd("2021-07-08"),
      end_date = lubridate::ymd("2021-08-31"),
      note = ""
    )
    
    add_df <- filter(records, crefo_id == df$crefo_id[which(df$nev == input$admin_search)])
    if (nrow(add_df) != 0) {
      null_df <- bind_rows(null_df, add_df)
    }
    null_df
  })
  
  output$admin_visitors <- shiny::renderText({
    visitors <- company_records() %>%
      group_by(user) %>%
      group_modify(~ tail(.x, 1)) %>%
      ungroup() %>%
      filter(user_go == TRUE & user != reactiveValuesToList(result_auth)$user) %>%
      pull(user)
    
    if (input$admin_go == TRUE) {
      visitors <- c(visitors, reactiveValuesToList(result_auth)$user)
    }
    
    if (length(visitors) == 0) {
      "Még senki nem jelentkezett ennek a cégnek a meglátogatására."
    } else {
      str_c("Látogatók: ", str_c(sort(visitors), collapse = ", "))
    }
  })
  
  output$admin_allowed <- shiny::renderUI({
    answer <- company_records() %>%
      pull(allow) %>%
      last()
    shiny::selectInput("admin_allowed", label = "Fogad minket?", choices = c("Igen", "Nem", "Még nem lett felkeresve", "A cég már megszűnt"), selected = answer)
  })
  
  output$admin_go <- shiny::renderUI({
    answer <- company_records() %>%
      filter(user == reactiveValuesToList(result_auth)$user) %>%
      pull(user_go) %>%
      last()
    shiny::checkboxInput("admin_go", "Én (is) megyek", value = answer)
  })
  
  output$admin_car <- shiny::renderUI({
    answer <- company_records() %>%
      pull(car) %>%
      last()
    shiny::selectInput("admin_car", "Céges autó rendszáma", 
                       c("Saját autóval", "Van igény céges autóra", 
                         "Autó nélkül", "SST-865", "SST-908", "SST-866"),
                       selected = answer
    )
  })
  
  output$admin_address <- shiny::renderUI({
    answer <- company_records() %>%
      pull(address) %>%
      na.omit() %>%
      last()
    shiny::textInput("admin_address", "Cím", value = answer)
  })
  
  output$admin_date <- shiny::renderUI({
    answer1 <- company_records() %>%
      pull(start_date) %>%
      last()
    
    answer2 <- company_records() %>%
      pull(end_date) %>%
      last()
    
    dateRangeInput(
      "admin_date",
      "Elérhető ekkor (két dátum egyezzen meg a már lefixált időpont esetén)",
      start = answer1,
      end =  answer2,
      format = "mm-dd",
      startview = "month",
      language = "hu",
      separator = "-",
      weekstart = 1
    )
  })
  
  output$admin_note <- renderUI({
    answer <- company_records() %>%
      pull(note) %>%
      last()
    if (is.na(answer)) {
      answer <- ""
    }
    textAreaInput("admin_note", "Megjegyzés (például elérhetőségek)", value = answer)
  })
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  
  records_reactive <- reactiveValues(
    records = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hQe-8OhkaqqaRKc-gXm_dFqrNz58C94hBhmyeq6YwHo/edit#gid=0")
  )
  
  my_data_table <- reactive({
    nev <- records_reactive$records %>% 
      group_by(user, crefo_id) %>% 
      group_modify(~ tail(.x, 1)) %>% 
      ungroup() %>% 
      filter(user == reactiveValuesToList(result_auth)$user & user_go == TRUE & is.na(done)) %>% 
      left_join(df) %>% 
      pull(nev) %>% 
      unique()
    
    tibble::tibble(
      `Cégnév` = nev,
      `Lezajlott az interjú?` = shinyInput(actionButton, length(nev),
                           'button_',
                           label = "Kész",
                           icon = icon("check"),
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                           onclick = paste0('Shiny.onInputChange( \"select_button\" , this.id)') 
      )    
    )
  })
  
  output$my_data_table <- DT::renderDataTable({
    my_data_table()
  }, escape = FALSE)
  
  output$users_done_table <- DT::renderDataTable({
    records_reactive$records %>% 
      group_by(user, crefo_id) %>% 
      group_modify(~ tail(.x, 1)) %>% 
      ungroup() %>% 
      filter(user == reactiveValuesToList(result_auth)$user & user_go == TRUE & !is.na(done)) %>% 
      left_join(df) %>% 
      select(nev) %>% 
      distinct() %>% 
      set_names("Cégnév")
  })
  
  observeEvent(input$select_button, {
    
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    current_crefo_id <- df %>% 
      filter(nev == as.character(my_data_table()[selectedRow,1])) %>% 
      pull(crefo_id) %>% 
      first()
    
    records %>% 
      filter(user == reactiveValuesToList(result_auth)$user & crefo_id == current_crefo_id) %>% 
      tail() %>% 
      mutate(
        done = TRUE
      ) %>% 
      googlesheets4::sheet_append(ss = "https://docs.google.com/spreadsheets/d/1hQe-8OhkaqqaRKc-gXm_dFqrNz58C94hBhmyeq6YwHo/edit#gid=0")
    
    records_reactive$records <<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hQe-8OhkaqqaRKc-gXm_dFqrNz58C94hBhmyeq6YwHo/edit#gid=0") 
    
  })
  
  output$gantt_chart <- timevis::renderTimevis({
    input$admin_message_ok
    
    df_gantt <- records %>% 
      group_by(user, crefo_id) %>% 
      group_modify(~ tail(.x, 1)) %>% 
      ungroup() %>% 
      filter(user == reactiveValuesToList(result_auth)$user & user_go == TRUE) %>% 
      select(crefo_id, start_date, end_date) %>% 
      left_join(df) %>% 
      arrange(start_date) %>% 
      mutate(id = row_number()) %>% 
      transmute(id, content = str_c("<b>", nev, "</b> <br>", ifelse(megye != "Budapest", str_c(megye, " megye"), megye)),
                start = start_date, end = end_date) %>% 
      mutate(
        style = case_when(
          start == end ~ "background-color: #C42E35",
          lubridate::ymd(start) != lubridate::ymd("2021-07-08") | lubridate::ymd(end) != lubridate::ymd("2021-08-31") ~ "background-color: #9D9062; color: white",
          TRUE ~ "background-color: #9197AE"
        ),
        style = str_c(style, "; border-color: black")
      ) 
    
    if (input$gantt_show == "Csak dátummal rendelkezők") {
      df_gantt <- df_gantt %>%
        filter(lubridate::ymd(start) != lubridate::ymd("2021-07-08") | 
                 lubridate::ymd(end) != lubridate::ymd("2021-08-31")
               
        )
    }
    
    if (input$gantt_show == "Csak fix dátummal rendelkezők") {
      df_gantt <- df_gantt %>%
        filter(start == end)
    }
    
    df_gantt %>% 
      mutate(
        start = lubridate::ymd_hm(str_c(start, " 00:01")),
        end = lubridate::ymd_hm(str_c(end, " 23:59"))
      ) %>% 
      timevis::timevis()
  })
  
}

shinyApp(ui, server)
