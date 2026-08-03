# ==============================================================================
# Sistema cromático para visualização de dados — CGREQ | DEAPS | SAPS
# ------------------------------------------------------------------------------
# Fonte: "Sistema cromático para visualização de dados - CGREQ - PREVIA_1_1.pdf"
# Referência do documento original: Manual de comunicação de período de
# defeso eleitoral 2026 (ver observação sobre a seção 1, abaixo).
#
# Este script define:
#   1. Todas as paletas de cor do documento, como vetores nomeados
#   2. Funções de escala prontas para uso em ggplot2 (fill e color)
#   3. Registro da tipografia Rawline (via systemfonts + showtext)
#   4. Um tema ggplot2 (theme_cgreq) alinhado à tabela tipográfica do manual
#
# Uso básico:
#   source("sistema_cromatico_cgreq.R")
#   ggplot(dados, aes(regiao, valor, fill = regiao)) +
#     geom_col() +
#     scale_fill_cgreq_categorias() +
#     theme_cgreq()
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Pacotes necessários
# ------------------------------------------------------------------------------
# Se algum destes pacotes não estiver instalado, instale antes de rodar o script:
#   install.packages(c("ggplot2", "systemfonts", "sysfonts", "showtext"))


pacman::p_load(tidyverse, systemfonts, sysfonts, showtext)



# ------------------------------------------------------------------------------
# 1. Cores institucionais (Ministério da Saúde — período de defeso eleitoral 2026)
# ------------------------------------------------------------------------------


cores_institucionais_defeso <- c(
  "verde_institucional"  = "#009B48",
  "amarelo_institucional" = "#FFC72C",
  "azul_institucional"    = "#0071BC",
  "branco_institucional"  = "#FFFFFF",
  "cinza_institucional"   = "#6B6B6B"
)


# ------------------------------------------------------------------------------
# 2. Núcleo analítico (base) — uso recorrente em títulos, textos e elementos
# ------------------------------------------------------------------------------

nucleo_analitico <- c(
  "azul_principal" = "#173A70",
  "azul_apoio"     = "#3E67B1",
  "verde_apoio"    = "#009B77",
  "azul_claro"     = "#AFC8EA",
  "cinza_texto"    = "#475569",
  "cinza_linha"    = "#E2E8F0"
)


# ------------------------------------------------------------------------------
# 3. Escala quantitativa — volume ou frequência (mapas, heatmaps, intensidade)
# ------------------------------------------------------------------------------
# Sequência ordenada (do menor para o maior valor) — usar com scale_*_gradientn
# ou como paleta discreta ordinal.
# "critico" foi listado separadamente no PDF, na sequência imediatamente após
# esta escala; mantive como um 6º nível opcional para destacar outliers/valores
# extremos além de "Muito Alto" — use com cautela, pois o documento não deixa
# explícito se pertence a esta escala ou é de uso independente.

escala_quantitativa <- c(
  "muito_baixo" = "#EEF5FC",
  "baixo"       = "#D7E7F8",
  "medio"       = "#AFC8EA",
  "alto"        = "#6E97D0",
  "muito_alto"  = "#173A70"
)

escala_quantitativa_critico <- "#D64545"  # nível extra opcional, ver nota acima


# ------------------------------------------------------------------------------
# 4. Escala semântica — desempenho, situação ou prioridade (indicadores)
# ------------------------------------------------------------------------------

escala_semantica <- c(
  "excelente" = "#009B77",
  "bom"       = "#4CB38E",
  "atencao"   = "#F6C445",
  "alerta"    = "#F28C28"
)



escala_semantica_completa <- c(
  escala_semantica,
  "critico"      = "#D64545",
  "sem_info"     = "#CBD5E1"
)


# ------------------------------------------------------------------------------
# 5. Escala divergente — comparações positivas x negativas
# ------------------------------------------------------------------------------

escala_divergente <- c(
  "perda_forte"     = "#D64545",
  "perda_moderada"  = "#F08A7A",
  "neutro"          = "#E8EDF3",
  "ganho_moderado"  = "#79C7B4",
  "ganho_forte"     = "#009B77"
)


# ------------------------------------------------------------------------------
# 6. Categorias — séries qualitativas, até 5 categorias
# ------------------------------------------------------------------------------

categorias_cgreq <- c(
  "categoria_1" = "#173A70",
  "categoria_2" = "#3E67B1",
  "categoria_3" = "#6E97D0",
  "categoria_4" = "#009B77",
  "categoria_5" = "#7CCBA2"
)


# ------------------------------------------------------------------------------
# 7. Status — classificação de situações
# ------------------------------------------------------------------------------

status_cgreq <- c(
  "informacao"     = "#3E67B1",
  "positivo"       = "#009B77",
  "atencao"        = "#F6C445",
  "critico"        = "#D64545",
  "sem_informacao" = "#CBD5E1"
)


# ------------------------------------------------------------------------------
# 8. Neutros — escala de cinza para textos, linhas e fundos
# ------------------------------------------------------------------------------

neutros_cgreq <- c(
  "texto_principal"   = "#1E293B",
  "texto_secundario"  = "#475569",
  "texto_apoio"       = "#64748B",
  "linha"             = "#CBD5E1",
  "fundo"             = "#F8FAFC",
  "divisor"           = "#E2E8F0"
)


# ------------------------------------------------------------------------------
# 9. Lista consolidada — todas as paletas em um único objeto
# ------------------------------------------------------------------------------

paleta_cgreq <- list(
  institucional_defeso        = cores_institucionais_defeso,
  nucleo_analitico             = nucleo_analitico,
  escala_quantitativa          = escala_quantitativa,
  escala_quantitativa_critico  = escala_quantitativa_critico,
  escala_semantica             = escala_semantica,
  escala_semantica_completa    = escala_semantica_completa,
  escala_divergente            = escala_divergente,
  categorias                   = categorias_cgreq,
  status                       = status_cgreq,
  neutros                      = neutros_cgreq
)


# ------------------------------------------------------------------------------
# 10. Funções de escala para ggplot2
# ------------------------------------------------------------------------------

# --- Escalas discretas (fill e color) para cada paleta qualitativa ----------

scale_fill_cgreq_categorias  <- function(...) scale_fill_manual(values = unname(categorias_cgreq), ...)
scale_color_cgreq_categorias <- function(...) scale_color_manual(values = unname(categorias_cgreq), ...)

scale_fill_cgreq_status  <- function(...) scale_fill_manual(values = status_cgreq, ...)
scale_color_cgreq_status <- function(...) scale_color_manual(values = status_cgreq, ...)

scale_fill_cgreq_semantica  <- function(completa = FALSE, ...) {
  paleta <- if (completa) escala_semantica_completa else escala_semantica
  scale_fill_manual(values = paleta, ...)
}
scale_color_cgreq_semantica <- function(completa = FALSE, ...) {
  paleta <- if (completa) escala_semantica_completa else escala_semantica
  scale_color_manual(values = paleta, ...)
}

scale_fill_cgreq_divergente  <- function(...) scale_fill_manual(values = escala_divergente, ...)
scale_color_cgreq_divergente <- function(...) scale_color_manual(values = escala_divergente, ...)

# --- Escala quantitativa contínua (gradiente) --------------------------------
# Para variáveis contínuas (ex.: taxa de consumo por estado, mapa coroplético)

scale_fill_cgreq_quantitativa <- function(..., incluir_critico = FALSE) {
  cores <- if (incluir_critico) c(unname(escala_quantitativa), escala_quantitativa_critico) else unname(escala_quantitativa)
  scale_fill_gradientn(colors = cores, ...)
}
scale_color_cgreq_quantitativa <- function(..., incluir_critico = FALSE) {
  cores <- if (incluir_critico) c(unname(escala_quantitativa), escala_quantitativa_critico) else unname(escala_quantitativa)
  scale_color_gradientn(colors = cores, ...)
}

# --- Escala quantitativa discreta (ex.: mapa coroplético com faixas, item 08) -

scale_fill_cgreq_quantitativa_discreta <- function(...) {
  scale_fill_manual(values = unname(escala_quantitativa), ...)
}


# ------------------------------------------------------------------------------
# 11. Tipografia — registro da fonte Rawline
# ------------------------------------------------------------------------------
# Localiza automaticamente os arquivos da fonte Rawline já instalados no
# sistema operacional (você indicou que já está instalada) e os registra no
# R via sysfonts::font_add(), habilitando o uso via showtext.
#
# A tabela tipográfica do manual usa 5 pesos: Light, Regular, Medium,
# SemiBold e Bold (+ Italic). Como sysfonts::font_add() só aceita 4 variantes
# por família (regular/bold/italic/bolditalic), registramos:
#   - "Rawline"           -> Regular / Bold / Italic / Bold Italic
#   - "Rawline Light"     -> peso Light isolado
#   - "Rawline Medium"    -> peso Medium isolado (usado em Título 4)
#   - "Rawline SemiBold"  -> peso SemiBold isolado (usado em Título 2 e 3)
#
# Se a fonte não for encontrada, o script emite um aviso e segue usando a
# fonte padrão do sistema — os gráficos funcionam, só não usarão Rawline.

registrar_fonte_rawline <- function() {
  fontes_sistema <- systemfonts::system_fonts()
  rawline <- fontes_sistema[fontes_sistema$family == "Rawline", ]
  
  if (nrow(rawline) == 0) {
    warning(
      "Fonte 'Rawline' não encontrada pelo systemfonts::system_fonts(). ",
      "Verifique se o nome da família está exatamente como 'Rawline' ",
      "(rode systemfonts::system_fonts() manualmente para conferir os nomes ",
      "disponíveis). Os gráficos usarão a fonte padrão."
    )
    return(invisible(FALSE))
  }
  
  caminho_estilo <- function(padrao) {
    linha <- rawline[grepl(padrao, rawline$style, ignore.case = TRUE), ]
    if (nrow(linha) == 0) NA_character_ else linha$path[1]
  }
  
  regular    <- caminho_estilo("^Regular$")
  bold       <- caminho_estilo("Bold(?! Italic)")
  italic     <- caminho_estilo("^Italic$")
  bolditalic <- caminho_estilo("Bold Italic")
  light      <- caminho_estilo("Light")
  medium     <- caminho_estilo("Medium")
  semibold   <- caminho_estilo("SemiBold")
  
  if (!is.na(regular)) {
    sysfonts::font_add(
      family = "Rawline",
      regular = regular,
      bold = if (!is.na(bold)) bold else regular,
      italic = if (!is.na(italic)) italic else regular,
      bolditalic = if (!is.na(bolditalic)) bolditalic else regular
    )
  }
  if (!is.na(light))    sysfonts::font_add(family = "Rawline Light", regular = light)
  if (!is.na(medium))   sysfonts::font_add(family = "Rawline Medium", regular = medium)
  if (!is.na(semibold)) sysfonts::font_add(family = "Rawline SemiBold", regular = semibold)
  
  showtext::showtext_auto()
  invisible(TRUE)
}

registrar_fonte_rawline()


# ------------------------------------------------------------------------------
# 12. Tema ggplot2 — theme_cgreq()
# ------------------------------------------------------------------------------
# Segue a tabela tipográfica do manual (seção "Família tipográfica"):
#   Título 1 (24pt, Bold, Azul Institucional)     -> plot.title
#   Título 2 (18pt, SemiBold, Azul Institucional) -> plot.subtitle
#   Título 4 (13pt, Medium, Cinza Escuro)         -> axis.title
#   Texto Corrido (10,5pt, Regular, Cinza Escuro) -> texto geral / axis.text
#   Legenda de Gráfico (8,5pt, Regular, Cinza Médio) -> legend.text
#   Fonte / Observações (8pt, Regular Italic, Cinza Médio) -> caption
#
# Nota: como o tamanho de fonte real na tela/impressão depende do dispositivo
# gráfico (showtext escala em relação ao base_size), os tamanhos abaixo estão
# em pontos e podem precisar de ajuste fino conforme o dispositivo de saída
# (ex.: ragg_png para HTML/Quarto, cairo_pdf para PDF).

theme_cgreq <- function(base_size = 10.5, base_family = "Rawline") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(
        family = "Rawline", face = "bold", size = 24,
        color = nucleo_analitico["azul_principal"]
      ),
      plot.subtitle = element_text(
        family = "Rawline SemiBold", size = 18,
        color = nucleo_analitico["azul_principal"]
      ),
      axis.title = element_text(
        family = "Rawline Medium", size = 13,
        color = neutros_cgreq["texto_secundario"]
      ),
      axis.text = element_text(
        family = "Rawline", size = base_size,
        color = neutros_cgreq["texto_secundario"]
      ),
      legend.text = element_text(
        family = "Rawline", size = 8.5,
        color = neutros_cgreq["texto_apoio"]
      ),
      legend.title = element_text(
        family = "Rawline Medium", size = 9,
        color = neutros_cgreq["texto_secundario"]
      ),
      plot.caption = element_text(
        family = "Rawline", face = "italic", size = 8,
        color = neutros_cgreq["texto_apoio"], hjust = 0
      ),
      panel.grid.major = element_line(color = neutros_cgreq["divisor"], linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = neutros_cgreq["fundo"], color = NA),
      panel.background = element_rect(fill = neutros_cgreq["fundo"], color = NA)
    )
}



message("Sistema cromático CGREQ carregado: paleta_cgreq, funções scale_*_cgreq_*() e theme_cgreq() disponíveis.")