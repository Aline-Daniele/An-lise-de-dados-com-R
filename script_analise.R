# Instalação de pacotes

install.packages('tidyverse')
install.packages('readxl')
install.packages('arrow')
install.packages('conflicted')

conflicted::conflict_prefer('filter', 'dplyr')
conflicted::conflict_prefer('lag', 'dplyr')

# Carregamento
library(tidyverse)
library(arrow)
library(readxl)
library(ggplot2)

# Configuração inicial
setwd('C:/Users/User/Desktop/curso-analise-de-dados-main')

# Verificar o diretório atual
getwd()

# Importação do arquivo
df_csv <- read_csv('sim_salvador_2023.csv')

# Visualizar a estrutura dos dados
glimpse(df_csv)

# Visualizar as primeiras linhas
head(df_csv)

# Visualizar as últimas linhas
tail(df_csv)

# Resumo estatístico
summary(df_csv)

# Usando o table() do R base
table(df_csv$SEXO, useNA = 'always')

#Usando count() do tidyverse
df_csv %>%
  count(SEXO, sort = TRUE)

# Criar variável SEXO padronizada usando if_else
df_csv <- df_csv %>%
  mutate(
    sexo_p = if_else(SEXO == 1, 'Masculino',
                     if_else(SEXO == 2, 'Feminino',
                             if_else(SEXO == 0, 'Ignorado', NA_character_)))
  )
# Verificar resultado
df_csv %>% count(SEXO)
df_csv %>% count(sexo_p)

# Alternativa usando o case_when
df_csv <- df_csv %>%
  mutate(
    sexo_p = case_when(
      SEXO == 1 ~ 'Masculino',
      SEXO == 2 ~ 'Feminino',
      SEXO == 0 ~ 'Ignorado',
      is.na(SEXO) ~ NA_character_
      
    )
  )
# Verificar resultado
df_csv %>% count(SEXO)
df_csv %>% count(sexo_p)

# Converter variável data formato para Date
df_csv <- df_csv %>% 
  mutate(
    DTOBITO_dt = dmy(DTOBITO) #dmy = dia,mes,ano
  )

# Extratir ano da data
df_csv <- df_csv %>%
  mutate(
    ano_obito = year(DTOBITO_dt)
  )
glimpse(df_csv)

# Contar óbitos por ano
df_csv %>% count(ano_obito)

# Contar óbitos por ano e sexo
df_csv %>% count(sexo_p, ano_obito)

# Extrair primeiro caractere (tipo de idade)
df_csv <- df_csv %>%
  mutate(
    tipo_idade = str_sub(IDADE, 1, 1), #posição 1 até 1
    idade = str_sub(IDADE, 2), # posição 2 até o final
  )
# Verificar resultado
df_csv %>% 
  select(IDADE, tipo_idade, idade) %>% # select seleciona apenas as variáveis dentro do parentese
  head() # head visualiza apenas o início da tabela

# Regra de conversão

# Se tipo_idade 0 a 3(minutos, horas, dias, meses) + considera 0 anos (< 1 ano)
# Se tipo_idade 4 (já está em anos) + usa o valor direto
# Se tipo_idade 5 (centenarios) + soma 100 + valor
df_csv <- df_csv %>%
  mutate(
    tipo_idade = str_sub(IDADE, 1, 1), # extrai posição 1
    idade = str_sub(IDADE, 2), # extrai da posição 2 em diante
    idade_anos = case_when(
      tipo_idade <= 3 ~ 0, # menores de 1 ano
      tipo_idade == 4 ~ as.numeric(idade), # já em anos
      tipo_idade == 5 ~ 100 + as.numeric(idade) # centenarios
    )
  )
# Verficar estratura final
glimpse(df_csv)

# Agrupar óbitos por mês

df_csv_agrupado <- df_csv %>%
  mutate(mes_obito = month(DTOBITO_dt, label = TRUE)) %>%
  group_by(mes_obito) %>%
  summarise(
    total_obitos = n(),
    idade_media = mean(idade_anos, na.rm = TRUE)
  )

# ATIVIDADE 1: EXPLORAÇÃO E TRANSFORMAÇÃO DE DADOS

df_csv <- df_csv %>%
  mutate(
    faixa_etaria = case_when(
      idade_anos >= 0 & idade_anos <= 12 ~ "Criança",
      idade_anos >= 13 & idade_anos <= 17 ~ "Adolescente",
      idade_anos >= 18 & idade_anos <= 59 ~ "Adulto",
      idade_anos >= 60 ~ "Idoso",
      TRUE ~ NA_character_  # Para valores ausentes ou fora do esperado
    )
  )

# Verificar a criação da variável
glimpse(df_csv)

# 1.2 Contar óbitos por faixa etária

# Opção 1: Usando count()
df_csv %>%
  count(faixa_etaria, sort = TRUE)


# Opção 2: Usando group_by() + summarise()
df_csv %>%
  group_by(faixa_etaria) %>%
  summarise(
    total_obitos = n()
  ) %>%
  arrange(desc(total_obitos))

# ATIVIDADE 2: MANIPULAÇÃO DE DATAS E AGRUPAMENTO

# 2.1 Criar variável trimestre
df_csv <- df_csv %>%
  mutate(
    mes_numero = month(DTOBITO_dt),
    trimestre = case_when(
      mes_numero %in% c(1, 2, 3) ~ "1º Trimestre",
      mes_numero %in% c(4, 5, 6) ~ "2º Trimestre",
      mes_numero %in% c(7, 8, 9) ~ "3º Trimestre",
      mes_numero %in% c(10, 11, 12) ~ "4º Trimestre",
      TRUE ~ NA_character_
    )
  )

# Verificar a criação
df_csv %>%
  count(trimestre)

# 2.2 Calcular total de óbitos e idade média por trimestre e sexo

df_csv %>%
  group_by(trimestre, sexo_p) %>%
  summarise(
    total_obitos = n(),
    idade_media = mean(idade_anos, na.rm = TRUE),
    .groups = "drop"  # Remove agrupamento após summarise
  ) %>%
  arrange(trimestre, desc(total_obitos))

# Visualização alternativa com pivot_wider para facilitar comparação
df_csv %>%
  group_by(trimestre, sexo_p) %>%
  summarise(
    total_obitos = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sexo_p,
    values_from = total_obitos
  )

# ATIVIDADE 3: ANÁLISE INTEGRADA

# 3.1 Identificar o mês com maior número de óbitos
df_csv %>%
  mutate(mes_obito = month(DTOBITO_dt, label = TRUE, abbr = FALSE)) %>%
  count(mes_obito, sort = TRUE) %>%
  slice(1)  # Pega apenas a primeira linha (maior valor)



# Alternativa mostrando os top 3 meses
df_csv %>%
  mutate(mes_obito = month(DTOBITO_dt, label = TRUE, abbr = FALSE)) %>%
  count(mes_obito, sort = TRUE) %>%
  slice(1:3)

# 3.2 Calcular diferença percentual entre óbitos masculinos e femininos
# Opção 1: Diferença percentual simples
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(sexo_p) %>%
  mutate(
    percentual = (n / sum(n)) * 100,
    percentual = round(percentual, 2)
  )


# Opção 2: Calculando a diferença absoluta em pontos percentuais
diff_percentual <- df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(sexo_p) %>%
  mutate(percentual = (n / sum(n)) * 100) %>%
  summarise(
    diferenca_pp = max(percentual) - min(percentual),  # Diferença em pontos percentuais
    razao = max(n) / min(n)  # Razão entre masculino e feminino
  )

print(diff_percentual)

# 3.3 Determinar qual faixa etária teve mais óbitos
df_csv %>%
  count(faixa_etaria, sort = TRUE) %>%
  slice(1)



# Alternativa com percentual
df_csv %>%
  count(faixa_etaria, sort = TRUE) %>%
  mutate(
    percentual = (n / sum(n)) * 100,
    percentual = round(percentual, 2)
  )

# ANÁLISES COMPLEMENTARES

# Análise 1: Óbitos por faixa etária e sexo
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(faixa_etaria, sexo_p) %>%
  pivot_wider(
    names_from = sexo_p,
    values_from = n
  ) %>%
  mutate(
    total = Masculino + Feminino,
    prop_masculino = round((Masculino / total) * 100, 1)
  )

# Análise 2: Distribuição de óbitos ao longo do ano
df_csv %>%
  mutate(
    mes = month(DTOBITO_dt, label = TRUE),
    trimestre = case_when(
      month(DTOBITO_dt) %in% 1:3 ~ "1º Tri",
      month(DTOBITO_dt) %in% 4:6 ~ "2º Tri",
      month(DTOBITO_dt) %in% 7:9 ~ "3º Tri",
      month(DTOBITO_dt) %in% 10:12 ~ "4º Tri"
    )
  ) %>%
  group_by(trimestre) %>%
  summarise(
    total = n(),
    idade_media = round(mean(idade_anos, na.rm = TRUE), 1),
    idade_min = min(idade_anos, na.rm = TRUE),
    idade_max = max(idade_anos, na.rm = TRUE)
  )

# Análise 3: Top 5 faixas etárias mais afetadas (detalhado)
df_csv %>%
  mutate(
    faixa_5anos = cut(
      idade_anos,
      breaks = seq(0, 120, by = 5),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(faixa_5anos, sort = TRUE) %>%
  head(5)

# VISUALIZAÇÕES

# Gráfico 1: Óbitos por mês
df_csv %>%
  mutate(mes = month(DTOBITO_dt, label = TRUE)) %>%
  count(mes) %>%
  ggplot(aes(x = mes, y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  labs(
    title = "Distribuição de Óbitos por Mês - Salvador 2023",
    x = "Mês",
    y = "Número de Óbitos"
  ) +
  theme_minimal()
ggsave('grafico_obitos_mes.png', plot = last_plot(), width = 8, height = 6)

# Gráfico 2: Distribuição de idade por sexo
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  ggplot(aes(x = idade_anos, fill = sexo_p)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Distribuição de Idade dos Óbitos por Sexo",
    x = "Idade (anos)",
    y = "Frequência",
    fill = "Sexo"
  ) +
  theme_minimal()
ggsave('grafico_distribuicao_sexo.png', plot = last_plot(), width = 8, height = 6)

# Gráfico 3: Óbitos por faixa etária e sexo
df_csv %>%
  filter(sexo_p %in% c("Masculino", "Feminino")) %>%
  count(faixa_etaria, sexo_p) %>%
  ggplot(aes(x = faixa_etaria, y = n, fill = sexo_p)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Óbitos por Faixa Etária e Sexo - Salvador 2023",
    x = "Faixa Etária",
    y = "Número de Óbitos",
    fill = "Sexo"
  ) +
  theme_minimal()
ggsave('grafico_distribuicao_faixa_etaria.png', plot = last_plot(), width = 8, height = 6)

# ==============================================================================
# EXPORTAR RESULTADOS (OPCIONAL)
# ==============================================================================

# Salvar resumo das análises em CSV
resumo_atividades <- df_csv %>%
  group_by(trimestre, faixa_etaria, sexo_p) %>%
  summarise(
    total_obitos = n(),
    idade_media = round(mean(idade_anos, na.rm = TRUE), 1),
    .groups = "drop"
  )

write_csv(resumo_atividades, "resumo_atividades_aula2.csv")

# Mensagem final
cat("\n✓ Gabarito das atividades concluído!\n")
cat("✓ Todas as análises foram executadas com sucesso.\n")
cat("✓ Explore os gráficos e resultados gerados.\n")
