#Carregamento de Bibliotecas

library(dados)
library(dplyr)
library(writexl)
library(ggplot2)
library(e1071) 


#Carregue o pacote dados e avalie as variáveis do dataset diamante. Qual o tipo de cada uma delas?

diamante <- diamante
diamante
str(dados::diamante)

# Criar tabela de frequências

tabela_frequencia_corte <- dados::diamante %>%
  count(corte) %>%                                         # frequência absoluta
  arrange(desc(corte)) %>%
  mutate(
    freq_relativa = (n / sum(n))*100,                      # frequência relativa
    freq_abs_acumulada = cumsum(n),                        # frequência absoluta acumulada
    freq_rel_acumulada = (cumsum(freq_relativa))*100       # frequência relativa acumulada
  )
tabela_frequencia_corte
write_xlsx(tabela_frequencia_corte, "tabela_frequencia_corte.xlsx")

#Crie um gráfico de barras para visualizar a variável cor do dataset diamante, do pacote dados.

## Calcular a frequência de cada categoria da variável 'cor'
tabela_cor <- dados::diamante %>%
  count(cor)

## Criar o gráfico de barras com cores em degradê azul
ggplot(tabela_cor, aes(x = cor, y = n, fill = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 1.5, color = "white", size = 4) +   #acrescentar valores nas barras
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Degradê azul claro → azul escuro
  labs(
    x = "Cor",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta legenda de cor (opcional)
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

## Avaliação dos preços 
summary(diamante$preco)

# Calcular a curtose
curtose_preco <- kurtosis(dados::diamante$preco)            

# Gráfico de densidade com valor da curtose no título
ggplot(dados::diamante, aes(x = preco)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  labs(
    title = paste0("Curtose = ", round(curtose_preco, 2)),
    x = "Preço",
    y = "Densidade"
  ) +
  theme_minimal()
