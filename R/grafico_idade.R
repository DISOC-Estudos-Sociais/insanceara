library(PNADcIBGE)
library(srvyr)
library(dplyr)
library(forcats)
library(ggplot2)

dados <- readRDS("data/clean_data/painel_san.RDS")

# Paleta de cores
paleta_san <- scale_fill_manual(
  values = c("#2ecc71", "#f39c12", "#e67e22", "#e74c3c"),
  name = "Situação de Segurança Alimentar",
  labels = c("Segurança alimentar", "IA leve", "IA moderada", "IA grave")
)

# As vezes precisa dela invertida
paleta_san_invertida <- scale_fill_manual(
  values = c("#e74c3c", "#e67e22", "#f39c12","#2ecc71"),
  name = "Situação de Segurança Alimentar",
  labels = c("IA grave", "IA moderada", "IA leve","Segurança alimentar")
)

# Eixo Y padrão
eixo_y_percentual <- scale_y_continuous(
  labels = scales::percent,
  limits = c(0, NA)
)

# Tema customizado
tema_san <- theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(face = "bold", size = 8),
    strip.text = element_text(face = "bold", size = 8),
    strip.background = element_rect(fill = "grey90", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

dados_svy <- dados[!is.na(dados$peso),] |> 
  as_survey_design(
    weights = peso,
    strata = estrato
  )

plot_faixa_idade <- dados |>
  mutate(
    faixa_idade = fct_collapse(faixa_idade,
                               "0 a 4 anos" = "0 a 4 anos",
                               "5 a 19 anos" = c("5 a 9 anos", "10 a 13 anos", "14 a 19 anos"),
                               "20 a 49 anos" = c("20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos","40 a 44 anos", "45 a 49 anos"),
                               "50 a 64 anos" = c("50 a 54 anos", "55 a 59 anos", "60 a 64 anos"),
                               "65 anos ou mais" = c("65 a 69 anos", "70 a 74 anos", "75 a 79 anos","80 anos ou mais")
    )
  ) |> 
  filter(uf == "Ceará" & ano %in% c("PNADC 2023", "PNADC 2024")) |>
  as_survey_design(weights = peso) |>
  group_by(ano, faixa_idade, san) |>
  summarise(
    n = survey_total(),
    .groups = "drop"
  ) |>
  group_by(ano, faixa_idade) |>
  mutate(
    populacao = sum(n),
    proporcao = n / populacao
  ) |>
  ungroup()  |> 
  ggplot(aes(ano, proporcao, fill = san)) +
  geom_col(position = "stack", width = 0.7)+
  facet_grid(cols = vars(faixa_idade)) +
  labs(
    #title = "Domicílios particulares, por situação de segurança alimentar,\nsegundo a raça da pessoa responsável pelo domicílio",
    #subtitle = "Ceará – 2023 e 2024",
    y = "Proporção de domicílios",
    x = NULL,
    #caption = "Fonte: PNAD Contínua/IBGE"
  ) +
  geom_text(
    aes(label = scales::percent(proporcao, accuracy = 0.1)), 
    position = position_stack(vjust = 0.5),
    size = 3,
    colour="white",
    fontface = "bold"
  ) +
  paleta_san+
  eixo_y_percentual +
  tema_san

ggplot2::ggsave(
  "output/faixa_idade.png",
  plot   = plot_faixa_idade,
  width  = 12, height = 7, dpi = 150, bg = "white"
)
