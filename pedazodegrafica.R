
# librerias---------

library(tidyverse)
library(rvest)
library(gt)
library(gtExtras)


# logos, colores, y nombres-------
tm_acb <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/acbTeams2022/main/acb.csv") |>
  select(tm, abb, id_team, logo) # data of logos colors and names


# función que extrae nombres y licencias ----------------------------------

#
#   id <- tm_acb$id_team
#
#   licencias <- function(id) {
#     url <- paste0("https://www.acb.com/club/plantilla/id/", id) |>
#       read_html()
#
#     p1 <- tibble(
#       plantilla = url |> html_elements(".caja_jugador_medio_cuerpo .nombre a") |>
#         html_text("href"),
#       licencia = url |> html_elements("span.roboto_bold:nth-of-type(2)") |>
#         html_text("href")
#     )
#     p2 <- tibble(
#       plantilla = url |> html_elements(".ellipsis a") |>
#         html_text("href"),
#       licencia = url |> html_elements(".info_personal div:nth-of-type(2) span") |>
#         html_text("href")
#     )
#
#
#     df <- rbind(p1, p2)
#
#     df$id_team <- id
#     return(df)
#   }
#
#   id_df <- map_df(id, licencias)
#
#   write.csv(id_df, "id_df.csv")
df_id <- read.csv("id_df.csv")

# Tabla jugadores y su licencia

df_id <- "https://raw.githubusercontent.com/IvoVillanueva/licencias_csv/main/id_df.csv"

# Tabla clasificacion 2021-22 ---------------------------------------------



clasificacion <- "https://www.acb.com/resultados-clasificacion/ver/temporada_id/2021/competicion_id/1/jornada_numero/34" |>
  read_html() |>
  html_element("table") |>
  html_table() |>
  janitor::clean_names() |>
  mutate(equipo = substr(equipo, 1, 3)) |>
  select(-x, abb = equipo)


# Data wrangler -----------------------------------------------------------


df_id <- df_id |>
  group_by(id_team) |>
  count(licencia)


df_id1 <- df_id |>
  group_by(id_team) |>
  summarise(lic = sum(n)) |>
  left_join(df_id) |>
  left_join(tm_acb) |>
  left_join(clasificacion) |>
  mutate(licencia_pct = round(n / lic, 2)) |>
  ungroup() |>
  select(tm:dif, licencia, -licencia_pct, -id_team, -lic, licencia_jug = n)

# funcion para las barras

bar_fx <- function(data_inx) {
  data_inx %>%
    ggplot(aes(x = licencia_jug, y = abb, fill = licencia)) +
    geom_bar(
      position = "fill",
      stat = "identity",
      color = "#fff5e3",
      size = 20
    ) +
    geom_text(
      aes(label = licencia_jug, group = licencia),
      position = position_fill(vjust = .5),
      color = "white",
      hjust = 0.5,
      size = 45,
      family = "Lato"
    ) +
    scale_fill_manual(
      breaks = c("JFL", "EXT", "EUR", "COT"),
      values = c("#F18A70", "#B1D877", "#8CDCDA", "#4D4D4D"),
      aesthetics = "fill"
    ) +
    theme_void() +
    theme(legend.position = "none")
}


tab_df <- df_id1 %>%
  group_by(abb, tm) %>%
  summarise(
    across(-contains("licencia"), unique),
    data = list(tibble(abb = abb, licencia = licencia, licencia_jug = licencia_jug) %>% bar_fx())
  ) %>%
  ungroup() %>%
  arrange(pos) |>
  mutate(plot = NA)

# funcion para poner juntos los nombres y el logo

combine_word <- function(tm, abb, logo) {
  glue::glue(
    "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;horizontal-align:left;font-size:15px'>{tm}</span></div>
        <div style='font-size:12px;line-height:18px;vertical-align:middle;horizontal-align:left'><span style ='font-weight:bold;color:grey;font-size:10px'><img src='{logo}'
    style='width:16px; height:16px;vertical-align:middle;horizontal-align:left'> {abb}</span></div>"
  )
}

{gt}
path <- here::here("licencias", "licencias") # donde lo va aguardar

tab_df %>%
  mutate(
    v_d = paste0(v, "-", d),
    combo = combine_word(tm, abb, logo),
    combo = map(combo, gt::html),
    percent = paste0(percent, "%")
  ) |>
  select(combo, pos, v_d, percent, p_f, p_c, dif, data, plot) |>
  gt() %>%
  cols_hide(data) %>%
  cols_align(align = "left", columns = combo) %>%
  cols_align(align = "center", columns = pos:dif) %>%
  cols_width(
    plot ~ px(325),
    combo ~ px(190),
    v_d:p_c ~ px(70)
  ) %>%
  text_transform(
    locations = cells_body(plot),
    fn = function(x) {
      map(tab_df$data, ggplot_image, height = px(30), aspect_ratio = 9.9)
    }
  ) %>%
  tab_spanner(
    label = html("<span style='font-weight:bold;font-size:12px'>LICENCIAS</span>"),
    columns = plot,
    id = "licencias"
  ) %>%
  tab_spanner(
    label = html("<span style='font-weight:bold;font-size:12px'>PUNTOS</span>"),
    columns = c(p_f, p_c),
    id = "puntos"
  ) %>%
  cols_label(
    combo = html("<span style='font-weight:bold;font-size:12px'>EQUIPO</span>"),
    pos = html("<span style='font-weight:bold;font-size:12px'>POS</span>"),
    v_d = html("<span style='font-weight:bold;font-size:12px'>V-D</span>"),
    percent = html("<span style='font-weight:bold;font-size:12px'>V-D%</span>"),
    p_f = html("<span style='font-weight:bold;font-size:12px'>ANOTADOS</span>"),
    p_c = html("<span style='font-weight:bold;font-size:12px'>RECIBIDOS</span>"),
    dif = html("<span style='font-weight:bold;font-size:12px'>DIF</span>"),
    plot = html(
      "<span style='color:#F16A70'><b>JFL</b></span>",
      "||",
      "<span style='color:#B1D877'><b>EXT</b></span>",
      "||",
      "<span style='color:#8CDCDA'><b>EUR</b></span>",
      "||",
      "<span style='color:#4D4D4D'><b>COT</b></span>"
    )
  ) %>%
  gt_theme_538() |>
  tab_options(
    heading.align = "left",
    table.background.color = "#fff5e3",
    column_labels.background.color = "#fff5e3",
    table.font.names = "Inconsolata",
    data_row.padding = px(0)
  ) %>%
  tab_header(
    title = md("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Liga_Endesa_2019_logo.svg/800px-Liga_Endesa_2019_logo.svg.png'
               style='height:40px;'>"),
    subtitle = md("<span style='font-weight:bold;font-size:25px;font-family:Chivo'>Clasificación temporada regular 2021-22 y distribución de licencias</span>")
  ) %>%
  tab_source_note(
    source_note = md("<span style='color:#F16A70'><b>JFL :</b></span> *Todo aquel jugador comunitario y/o asimilado que se ha formado deportivamente en España*<br>
                       <span style='color:#B1D877'><b>EXT :</b></span>  *Extracomunitario*<br>
                       <span style='color:#8CDCDA'><b>EUR :</b></span> *Comunitario o FIBA Europa*<br>
                       <span style='color:#4D4D4D'><b>COT :</b></span> *‘Jugadores cotonou’ (A falta de acreditar una vinculación familiar o deportiva con el país del nuevo pasaporte)*<br>
                       **Datos**: *ACB.com*<br> **Gráfico**: *Ivo Villanueva* &bull;  <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf099;</span> **@elcheff**")
  ) |>
  gtsave(glue::glue("{path}.png"))
