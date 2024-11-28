# ================================================
# SCRIPT: Análise de Dados da PNAD Contínua com R
# ================================================
# Autor: Aline K. Dalcin
# Data: 28/11/2024
# Descrição: Este script apresenta operações básicas no R, manipulação de dados da PNAD Contínua,
# uso de amostras complexas e visualização com gráficos.

# ------------------------------------------------
# Índice:
# 1. Operações Básicas no R
# 2. Instalação e Carregamento de Pacotes
# 3. Importação de Dados da PNAD Contínua
# 4. Manipulação de Dados com dplyr
# 5. Importância dos Pesos Amostrais
# 6. Análise com Amostra Complexa (Survey Package)
# 7. Visualização de Resultados
# ------------------------------------------------

# ================================================
# 1. Operações Básicas no R
# ================================================
# Exemplos simples de cálculos e manipulação de variáveis no R.

1 + 1  # Soma
18327 - 7981  # Subtração
24 / 7  # Divisão
5 * 78  # Multiplicação
14^2  # Potência (14 ao quadrado)

# Trabalhando com variáveis
soma <- 234 + 458  # Cria uma variável chamada 'soma'
rm(soma)  # Remove a variável da memória

# ================================================
# 2. Instalação e Carregamento de Pacotes
# ================================================
# Aqui instalamos (se necessário) e carregamos os pacotes necessários.

# Instalar pacotes (apenas na primeira vez)
install.packages(c("PNADcIBGE", "dplyr", "ggplot2", "survey", "convey"))

# Carregar pacotes para a análise
library(PNADcIBGE)  # Para trabalhar com dados da PNAD Contínua
library(dplyr)      # Manipulação de dados
library(ggplot2)    # Criação de gráficos
library(survey)     # Análise com pesos amostrais
library(convey)     # Análises de desigualdade (Índice de Gini e outros)

# ================================================
# 3. Importação de Dados da PNAD Contínua
# ================================================
# Importamos dados da PNAD Contínua de forma online ou offline.

# 3.1 Importação Online (diretamente do IBGE)
dadosPNADc2024 <- get_pnadc(year = 2024, quarter = 3)  # Dados do 3º trimestre de 2024
View(dadosPNADc2024)  # Visualiza a base de dados
View(dadosPNADc2024$variables)  # Visualiza o dicionário de variáveis

# 3.2 Importação Offline (dados locais)
setwd("xxxx")  # Define o diretório onde os dados estão armazenados

dadosPNADc2024_brutos <- read_pnadc("PNADC_032024.txt", "input_PNADC_trimestral.txt")  # Carrega os dados
dadosPNADc2024_brutos <- pnadc_labeller(dadosPNADc2024_brutos, "dicionario_PNADC_microdados_trimestral.xls")  # Rotula variáveis
dadosPNADc2024 <- pnadc_design(dadosPNADc2024_brutos)  # Cria objeto para análise com pesos

# ================================================
# 4. Manipulação de Dados com dplyr
# ================================================
# Exploramos operações básicas como ordenar, selecionar, filtrar e criar novas variáveis.

# 4.1 Ordenação
exemplo_arrange <- arrange(dadosPNADc2024_brutos, V2007)  # Ordem crescente
exemplo_arrange_desc <- arrange(dadosPNADc2024_brutos, desc(V2007))  # Ordem decrescente

# 4.2 Seleção de Variáveis
exemplo_select <- select(dadosPNADc2024_brutos, c("Ano", "UF", "V2007", "V2010", "V3001", "VD4002", "VD4016"))

# 4.3 Filtragem
exemplo_filter <- filter(dadosPNADc2024_brutos, UF == "Rio Grande do Sul")  # Apenas registros do RS
exemplo_filter2 <- filter(dadosPNADc2024_brutos, UF %in% c("Rio Grande do Sul", "Santa Catarina", "Paraná"))  # Sul do Brasil

# 4.4 Criação de Novas Variáveis
exemplo_mutate <- mutate(dadosPNADc2024_brutos, valor_per_capita = VD4019 / V2001)

# 4.5 Resumo Estatístico
exemplo_summarise <- summarise(dadosPNADc2024_brutos, 
                               media = mean(V2009, na.rm = TRUE), 
                               minimo = min(V2009, na.rm = TRUE), 
                               maximo = max(V2009, na.rm = TRUE))

# ================================================
# 5. Importância dos Pesos Amostrais
# ================================================
# Comparamos análises com e sem pesos para entender a relevância dos pesos amostrais.

# 5.1 Sem Peso
tab_sem_peso <- dadosPNADc2024_brutos %>%
  group_by(V2010) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))

# 5.2 Com Peso
tab_com_peso <- svytable(~V2010, design = dadosPNADc2024) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq)) %>%
  arrange(desc(Prop))

# ================================================
# 6. Análise com Amostra Complexa (Survey Package)
# ================================================
# Aqui utilizamos o pacote survey para análises que consideram a estrutura complexa da PNAD.

# 6.1 Variáveis Categóricas
svytable(~V2010, design = dadosPNADc2024)  # Tabela de frequência ponderada
svytotal(~V2010, design = dadosPNADc2024)  # Total ponderado
svymean(~V2010, design = dadosPNADc2024)  # Média ponderada

# Interação entre variáveis
svytable(~V2010 + V3001, design = dadosPNADc2024)

# 6.2 Subconjuntos (Subset)
svytotal(~V2010, design = subset(dadosPNADc2024, UF == "Rio Grande do Sul"))

# 6.3 Variáveis Quantitativas
svymean(~VD4016, design = dadosPNADc2024, na.rm = TRUE)  # Média ponderada
svytotal(~VD4016, design = dadosPNADc2024, na.rm = TRUE)  # Soma ponderada
svyquantile(~VD4016, design = dadosPNADc2024, na.rm = TRUE, quantile = 0.5)  # Mediana ponderada

# 6.4 Índice de Gini
dadosPNADc2024 <- convey_prep(dadosPNADc2024)  # Prepara o objeto para análise de desigualdade
gini <- svygini(~VD4020, design = dadosPNADc2024, na.rm = TRUE)  # Calcula o índice de Gini
gini

# ================================================
# 7. Visualização de Resultados
# ================================================
# Criamos gráficos para ilustrar os resultados da análise.
# Abaixo estão exemplos detalhados e passo a passo.

# ------------------------------------------------
# 7.1 Gráfico de Barras: Proporção por Raça/Cor e Alfabetização
# ------------------------------------------------

# Preparar os dados para o gráfico
tab1 <- svytable(~V2010 + V3001, design = dadosPNADc2024) %>%
  as.data.frame() %>%
  mutate(Prop = Freq / sum(Freq))

# Gráfico básico de barras
ggplot(data = tab1, aes(x = V2010, y = Prop, fill = V3001)) +
  geom_col() +
  labs(
    title = "Proporção por Raça/Cor e Alfabetização",
    subtitle = "Dados da PNAD Contínua - 1º Trimestre de 2020",
    x = "Raça/Cor",
    y = "Proporção (%)",
    fill = "Sabe Ler?"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# ------------------------------------------------
# 7.2 Gráfico de Barras com Proporção Normalizada
# ------------------------------------------------

# Gráfico ajustado para proporções totais em 100%
ggplot(data = tab1, aes(x = V2010, y = Prop, fill = V3001)) +
  geom_col(position = "fill") +  # position = "fill" normaliza as proporções
  labs(
    title = "Distribuição Normalizada de Alfabetização por Raça/Cor",
    x = "Raça/Cor",
    y = "Proporção Acumulada (%)",
    fill = "Sabe Ler?"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# ------------------------------------------------
# 7.3 Gráfico de Dispersão: Relação entre Educação e Renda
# ------------------------------------------------

# Criar um gráfico de dispersão para explorar a relação entre educação e renda
ggplot(data = dadosPNADc2024_brutos, aes(x = VD3005, y = VD4016)) +
  geom_point(alpha = 0.3, color = "blue") +  # Pontos transparentes para maior visibilidade
  labs(
    title = "Relação entre Educação e Renda",
    x = "Nível Educacional",
    y = "Renda Mensal (R$)"
  ) +
  theme_light()

# ------------------------------------------------
# 7.4 Gráfico de Histograma: Distribuição de Horas Trabalhadas
# ------------------------------------------------

# Histograma para visualizar a distribuição de horas trabalhadas por semana
ggplot(data = dadosPNADc2024_brutos, aes(x = VD4035)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(
    title = "Distribuição de Horas Trabalhadas por Semana",
    x = "Horas Trabalhadas",
    y = "Frequência"
  ) +
  theme_minimal()

