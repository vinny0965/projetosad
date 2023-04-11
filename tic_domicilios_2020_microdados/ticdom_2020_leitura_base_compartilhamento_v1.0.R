#*******************************************************************************
#*************************** TIC Domicílios 2020 *******************************
#* Esta sintaxe contempla:
#* Leitura das bases de dados (Domicílios e Indivíduos) 
#* Criação de variáveis (e labels)
#* Produção de tabelas cruzadas (utilizando o desenho amostral)      
#* Última atualização: 27/09/2021
#*******************************************************************************

#===============================================================================
# Limpa objetos da memória (Environment)
rm(list=ls())
#===============================================================================
# Limpa Console
cat("\014")
#===============================================================================
# Carrega (e instala, se necessário) pacotes requeridos
if (!require("install.load")) install.packages("install.load")
suppressMessages(install.load::install_load("tidyverse", "survey", "haven", "labelled"))
#===============================================================================
# Define diretório (pasta onde se encontra a base de microdados)
setwd("Diretório da base .RData")
#===============================================================================
# Carrega as bases de dados da TIC DOMICÍLIOS 2020 - Domicílios e Indivíduos
load("ticdom_2020_base_de_microdados_v1.0.RData")
#===============================================================================

#*******************************************************************************
#******************************** Domicílios ***********************************
#*******************************************************************************

# Cria variáveis derivadas (que serão utilizadas para tabulação)
basedom <-update(basedom, 
                 RENDA_FAMILIAR_2 = ifelse(RENDA_FAMILIAR == 1,1,
                                    ifelse(RENDA_FAMILIAR == 2,2,
                                    ifelse(RENDA_FAMILIAR == 3,3,
                                    ifelse(RENDA_FAMILIAR == 4,4,
                                    ifelse(RENDA_FAMILIAR == 5,5,
                                    ifelse(RENDA_FAMILIAR == 6,6,
                                    ifelse(RENDA_FAMILIAR == 7,6,
                                    ifelse(RENDA_FAMILIAR == 8,6,
                                    ifelse(RENDA_FAMILIAR == 9,7,
                                    ifelse(RENDA_FAMILIAR == 97,97,
                                    ifelse(RENDA_FAMILIAR == 98,98,999998))))))))))),
                 # PARA O INDICADOR A - DOMÍCILIOS QUE POSSUEM EQUIPAMENTO TIC
                 DIC_TV = ifelse(TV>0,1,0),
                 DIC_RADIO = ifelse(RADIO>0,1,0),
                 # INDICADOR A1 - DOMICÍLIOS COM COMPUTADOR¹
                 A1_AGREG = ifelse(A1_A == 1 | A1_B == 1 | A1_C == 1, 1, 0),
                 # INDICADOR A2A - DOMICÍLIOS COM COMPUTADOR, POR TIPO DE COMPUTADOR PRESENTE DE FORMA EXCLUSIVA OU SIMULTÂNEA NO DOMICÍLIO
                 A1_EXCLUSIVOS = ifelse(A1_A == 1 & A1_B != 1 & A1_C != 1, 1,
                                 ifelse(A1_A != 1 & A1_B == 1 & A1_C != 1, 2,
                                 ifelse(A1_A != 1 & A1_B != 1 & A1_C == 1, 3,99))),
                 # INDICADOR A2B - DOMICÍLIOS COM COMPUTADOR, POR FAIXA DE QUANTIDADE DE TIPO DE COMPUTADOR
                 A2_A_FAIXA = ifelse(A2_QTD_DESK == 1, 1,
                              ifelse(A2_QTD_DESK > 1 & A2_QTD_DESK<999999999, 2,
                              ifelse(A2_QTD_NOTE >=1 & A2_QTD_TAB >=1 & A2_QTD_DESK==999999999,0,999999999))),
                 A2_B_FAIXA = ifelse(A2_QTD_NOTE == 999999999 & A2_QTD_TAB >=1 & A2_QTD_DESK>=1,0,
                              ifelse(A2_QTD_NOTE == 1,1,
                              ifelse(A2_QTD_NOTE > 1  &  A2_QTD_NOTE < 999999999, 2,999999999))),
                 A2_C_FAIXA = ifelse(A2_QTD_NOTE >= 1 & A2_QTD_TAB == 999999999 & A2_QTD_DESK>=1, 0,
                              ifelse(A2_QTD_TAB == 1, 1,
                              ifelse(A2_QTD_TAB > 1 & A2_QTD_TAB <999999999,  2, 999999999))),
                 # INDICADOR A4B - DOMICÍLIOS, POR PRESENÇA DE COMPUTADOR E INTERNET¹
                 A1A4 = ifelse(A1_AGREG == 1 & A4 == 1, 1,
                        ifelse(A1_AGREG == 1 & A4 == 0, 2,
                        ifelse(A1_AGREG == 0 & A4 == 1, 3,
                        ifelse(A1_AGREG == 0 & A4 == 0, 4, 4)))),
                 # INDICADOR A5 - DOMICÍLIOS COM ACESSO À INTERNET, POR TIPO DE CONEXÃO
                 A7_1 = ifelse(A7 == 1, 1, 99),
                 A7_AGREG = ifelse(A7 == 2 | A7 == 3 | A7 == 4 | A7 == 5, 1, 99),
                 A7_3 = ifelse(A7 == 3, 1, 99),
                 A7_4 = ifelse(A7 == 2, 1, 99),
                 A7_5 = ifelse(A7 == 4, 1, 99),
                 A7_6 = ifelse(A7 == 5, 1, 99),
                 A7_7 = ifelse(A7 == 6, 1, 99),
                 A7_97 = ifelse(A7 == 97, 1, 99),
                 A7_98 = ifelse(A7 == 98, 1, 99),
                 # INDICADOR A10 - DOMICÍLIOS SEM ACESSO À INTERNET, POR MOTIVOS PARA A FALTA DE INTERNET
                 A5_NENHUM = ifelse((A5_A == 0 | A5_A == 97 | A5_A == 98) & 
                                     A5_B != 1 & A5_C != 1 & A5_D != 1 & A5_E != 1 & 
                                     A5_F != 1 & A5_G != 1 & A5_H !=1 & A5_I != 1 & 
                                     A5_OUTRO != 1, 1, 99),
                 # INDICADOR A11 - DOMICÍLIOS COM ACESSO À INTERNET, POR VALOR PAGO PELA PRINCIPAL CONEXÃO
                 A9_FAIXA = ifelse(A9 == 1 | A9 == 2 | A9 == 3 | A9 == 17, 1,
                            ifelse(A9 == 4, 2,
                            ifelse(A9 == 5, 3,
                            ifelse(A9 == 6, 4,
                            ifelse(A9 == 7, 5,
                            ifelse(A9 == 8, 6,
                            ifelse(A9 == 9, 7,
                            ifelse(A9 == 10, 8,
                            ifelse(A9 == 11 | A9 == 12 | A9 == 13 | A9 == 14 | A9 == 15, 9,
                            ifelse(A9 == 16, 10, A9)))))))))))

# Cria os labels de valores das variáveis derivadas acima
basedom$variables <- basedom$variables  %>% 
         add_value_labels(RENDA_FAMILIAR_2 =  c("Até 1 SM" = 1,
                                                "Mais de 1' SM até 2 SM" = 2,
                                                "Mais de 2 SM até 3 SM" = 3,
                                                "Mais de 3 SM até 5 SM" = 4,
                                                "Mais de 5 SM até 10 SM" = 5,
                                                "Mais de 10 SM" = 6,
                                                "Não tem renda" = 7,
                                                "Não sabe" = 97,
                                                "Não respondeu" = 98),
                          DIC_TV = c("Televisão" = 1),
                          DIC_RADIO = c("Rádio" = 1),
                          A1_AGREG = c("Sim" = 1,
                                       "Não" = 0),
                          A1_EXCLUSIVOS = c("Apenas computador de mesa" = 1,
                                            "Apenas notebook" = 2,
                                            "Apenas tablet" = 3,
                                            "Mais de um tipo de computador" = 99),
                          A2_A_FAIXA = c("Nenhum" = 0,
                                         "Um" = 1,
                                         "Dois ou mais" = 2),
                          A2_B_FAIXA = c("Nenhum" = 0,
                                         "Um" = 1,
                                         "Dois ou mais" = 2),
                          A2_C_FAIXA = c("Nenhum" = 0,
                                         "Um" = 1,
                                         "Dois ou mais" = 2),
                          A1A4 = c("Ambos" = 1,
                                   "Apenas computador" = 2,
                                   "Apenas Internet" = 3,
                                   "Nem computador nem Internet" = 4),
                          A7_1 = c("Conexão discada" = 1),
                          A7_AGREG = c("Total - Banda larga fixa" = 1),
                          A7_3 = c("Conexão via cabo de TV ou fibra ótica" = 1),
                          A7_4 = c("Conexão via linha telefônica (DSL)" = 1),
                          A7_5 = c("Conexão via rádio" = 1),
                          A7_6 = c("Conexão via satélite" = 1),
                          A7_7 = c("Conexão móvel via modem ou chip 3G ou 4G" = 1),
                          A7_97 = c("Não sabe" = 1),
                          A7_98 = c("Não respondeu" = 1),
                          A5_NENHUM = c("Nenhum desses motivos" = 1),
                          A9_FAIXA = c("Até R$ 30" = 1,
                                       "R$ 31 a R$ 40" = 2,
                                       "R$ 41 a R$ 50" = 3,
                                       "R$ 51 a R$ 60" = 4,
                                       "R$ 61 a R$ 70" = 5,
                                       "R$ 71 a R$ 80" = 6,
                                       "R$ 81 a R$ 90" = 7,
                                       "R$ 91 a R$ 100" = 8,
                                       "R$ 101 a R$ 150" = 9,
                                       "Mais de R$ 150" = 10,
                                       "Não sabe" = 97,
                                       "Não respondeu" = 98))

# Converte o tipo de todas as variáveis que possuem label para fator
basedom$variables <- as_factor(basedom$variables,
                               only_labelled = TRUE, 
                               levels = "default", 
                               ordered = FALSE)

# Reproduz os indicadores publicados (resultados devem ser iguais as tabelas do site)
# Indicador: A4 - DOMICÍLIOS COM ACESSO À INTERNET
# Variável utilizada:
# A4 - Este domicílio tem acesso à Internet?
# Filtro utilizado:
# Este indicador foi calculado para o Total de domicílios
# Portanto, não há filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
# svyby (combinado com svymean) fornece as proporções e os erros-padrão
# svyby (combinado com svytotal) fornece os totais e os erros-padrão
# margem de erro = erro-padrão*1.96
A4_AREA <- svyby(formula =~A4, 
                 by =~AREA, 
                 design = basedom, 
                 FUN = svymean)
A4_REGIAO <- svyby(~A4, ~COD_REGIAO, design = basedom, svymean)
A4_RENDA <- svyby(~A4, ~RENDA_FAMILIAR_2, design = basedom, svymean)
A4_CLASSE <- svyby(~A4, ~CLASSE_CB2015, design = basedom, svymean)

# Indicador: A1 - DOMICÍLIOS COM COMPUTADOR¹
# Variável utilizada:
# A1_AGREG - Possuir ao menos um tipo de computador (var criada no update acima) 
# Filtro utilizado:
# Este indicador foi calculado para o Total de domicílios
# Portanto, não há filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
A1_AREA <- svyby(~A1_AGREG, ~AREA, design = basedom, svymean)
A1_REGIAO <- svyby(~A1_AGREG, ~COD_REGIAO, design = basedom, svymean)
A1_RENDA <- svyby(~A1_AGREG, ~RENDA_FAMILIAR_2, design = basedom, svymean)
A1_CLASSE <- svyby(~A1_AGREG, ~CLASSE_CB2015, design = basedom, svymean)

# Indicador: A12 - DOMICÍLIOS COM ACESSO À INTERNET, POR PRESENÇA DE WIFI
# Variável utilizada:
# A7A - Neste domicílio tem Wi-Fi?
# Filtro utilizado:
# Este indicador foi calculado para o Total de domicílios com acesso à Internet
# Portanto, deve-se filtrar os que responderam A4 = 1 (utilizando a função subset)

# Verifica o tipo da variável A4
str(basedom$variables$A4)
# Tabela para ver as categorias da variável A4
table(basedom$variables$A4)

# Gera as tabelas (utilizando o desenho amostral)
A12_AREA <- svyby(~A7A, ~AREA, design = subset(basedom, A4 == "Sim"), svymean)
A12_REGIAO <- svyby(~A7A, ~COD_REGIAO, design = subset(basedom, A4 == "Sim"), svymean)
A12_RENDA <- svyby(~A7A, ~RENDA_FAMILIAR_2, design = subset(basedom, A4 == "Sim"), svymean)
A12_CLASSE <- svyby(~A7A, ~CLASSE_CB2015, design = subset(basedom, A4 == "Sim"), svymean)


#*******************************************************************************
#******************************** Indivíduos ***********************************
#*******************************************************************************

# Cria variáveis derivadas (que serão utilizadas para tabulação)
baseind <-update(baseind,
                 RENDA_FAMILIAR_2 = ifelse(RENDA_FAMILIAR == 1,1,
                                    ifelse(RENDA_FAMILIAR == 2,2,
                                    ifelse(RENDA_FAMILIAR == 3,3,
                                    ifelse(RENDA_FAMILIAR == 4,4,
                                    ifelse(RENDA_FAMILIAR == 5,5,
                                    ifelse(RENDA_FAMILIAR == 6,6,
                                    ifelse(RENDA_FAMILIAR == 7,6,
                                    ifelse(RENDA_FAMILIAR == 8,6,
                                    ifelse(RENDA_FAMILIAR == 9,7,
                                    ifelse(RENDA_FAMILIAR == 97,97,
                                    ifelse(RENDA_FAMILIAR == 98,98,999998))))))))))),
                 GRAU_INST = ifelse(GRAU_INSTRUCAO ==1| GRAU_INSTRUCAO ==2 |
                                    GRAU_INSTRUCAO ==3| GRAU_INSTRUCAO ==4,1,
                             ifelse(GRAU_INSTRUCAO ==5| GRAU_INSTRUCAO ==6 |
                                    GRAU_INSTRUCAO ==7 | GRAU_INSTRUCAO ==8,2,
                             ifelse(GRAU_INSTRUCAO == 9| GRAU_INSTRUCAO ==10,3,
                             ifelse(GRAU_INSTRUCAO == 11| GRAU_INSTRUCAO ==12,4,999998)))),
                 PEA_2 = ifelse(PEA == 1 | PEA == 2 | PEA ==3 | PEA == 4, 1,
                         ifelse(PEA == 5, 2, 999998)),
                 # PARA O INDICADOR C2A - USUÁRIOS DE INTERNET - INDICADOR AMPLIADO¹
                 C3J3 = ifelse(C3 == 1 | J3 == 1, 1,
                        ifelse(C3 != 1 & (J3 == 0 | J3 == 99), 0,
                        ifelse(C3 != 1 & J3 == 97, 97,
                        ifelse(C3 != 1 & J3 == 98, 98, 999)))),
                 # INDICADOR C15 - INDIVÍDUOS QUE NUNCA UTILIZARAM INTERNET, POR MOTIVO DECLARADO PARA NUNCA TER UTILIZADO A INTERNET
                 C2_OUTRO = ifelse(C2_J == 1 | C2_K == 1 | C2_L == 1 | C2_M == 1 | 
                                   C2_N == 1 | C2_O == 1 | C2_P == 1, 1, 0),
                 # INDICADOR C15A - INDIVÍDUOS QUE NUNCA UTILIZARAM INTERNET, POR PRINCIPAL MOTIVO DECLARADO PARA NUNCA TER UTILIZADO A INTERNET
                 C2A_2 = ifelse(C2A_1> 9 & C2A_1 < 97, 8, C2A_1),
                 # INDICADOR C16 - USUÁRIOS DE INTERNET, POR DISPOSITIVO UTILIZADO
                 C5_AGREG = ifelse(C5_A==1 | C5_B==1 | C5_C==1, 1, 0),
                 # INDICADOR C16A - USUÁRIOS DE INTERNET, POR DISPOSITIVO UTILIZADO DE FORMA EXCLUSIVA OU SIMULTÂNEA
                 C5_DISPOSITIVOS = ifelse(C5_AGREG == 1 & C5_D != 1, 1,
                                   ifelse(C5_AGREG != 1 & C5_D == 1, 2,
                                   ifelse(C5_AGREG == 1 & C5_D == 1, 3,99))),
                 # INDICADOR G3 - USUÁRIOS DE INTERNET, POR ATIVIDADES DE INTERAÇÃO COM AUTORIDADES PÚBLICAS
                 C8_NAO = ifelse(C8_F != 1 & C8_G != 1,1,99),
                 # INDICADOR I1 - USUÁRIOS DE COMPUTADOR, POR HABILIDADES PARA USO DO COMPUTADOR
                 I1_NENHUM = ifelse(I1_A!=1 & I1_B!=1 & I1_C!=1 & I1_D!=1 & I1_E!=1 & I1_F!=1 &
                                    I1_G!=1 & I1_H!=1 & I1_I!=1, 1, 99),
                 # INDICADOR J2A - INDIVÍDUOS, POR QUANTIDADE DE LINHAS DE TELEFONE CELULAR
                 J5_FAIXAS = ifelse(J5 == 0 | J5_QTD_LINHAS == 0, 0,
                             ifelse(J5 == 1 & J5_QTD_LINHAS == 1, 1, 
                             ifelse(J5 == 1 & J5_QTD_LINHAS == 2, 2, 
                             ifelse(J5 == 1 & (J5_QTD_LINHAS > 2 & J5_QTD_LINHAS < 96), 3,
                             ifelse(J5 == 97, 97,
                             ifelse(J5 == 98, 98, 99)))))),
                 # INDICADOR J4 - USUÁRIOS DE TELEFONE CELULAR, POR ATIVIDADES REALIZADAS NO TELEFONE CELULAR NOS ÚLTIMOS TRÊS MESES
                 J2_NENHUM = ifelse(J2_H1!=1 & J2_I!=1 & J2_J!=1 & J2_K!=1 & J2_L!=1 & J2_N!=1, 1, 99),
                 # INDICADOR J5 - INDIVÍDUOS QUE USARAM A INTERNET NO TELEFONE CELULAR NOS ÚLTIMOS TRÊS MESES¹
                 J3_AGREG = ifelse(J3 == 1,1,
                            ifelse(J3 == 0 | J1 == 0,0,
                            ifelse(J3 == 97| J1 == 97, 97,
                            ifelse(J3 == 98| J1 == 97, 98,99)))),
                 # INDICADOR J6A - USUÁRIOS DE INTERNET PELO TELEFONE CELULAR, POR TIPO DE CONEXÃO UTILIZADA DE FORMA EXCLUSIVA OU SIMULTÂNEA¹
                 J3AB = ifelse(J3A_A==1 & J3A_B!=1,1,
                        ifelse(J3A_B==1 & J3A_A!=1,2,
                        ifelse(J3A_A==1 & J3A_B==1,3,4))),
                 # INDICADOR L1 - NÃO USUÁRIOS QUE UTILIZARAM ALGUMA APLICAÇÃO SELECIONADA NOS ÚLTIMOS TRÊS MESES¹
                 C1_COB_AGREG = ifelse(C1_COB_A == 99 | C1_COB_B == 99 | C1_COB_C == 99 |
                                       C1_COB_D == 99, 99, 
                                ifelse(C1_COB_A == 1 | C1_COB_B == 1 | C1_COB_C == 1 |
                                       C1_COB_D == 1, 1, 0)),
                 # INDICADOR L5 - NÃO USUÁRIOS QUE UTILIZARAM ALGUMA APLICAÇÃO SELECIONADA NOS ÚLTIMOS TRÊS MESES, POR DISPOSITIVO UTILIZADO¹
                 C5_COB_AGREG = ifelse(C5_COB_A == 1 | C5_COB_B == 1 | C5_COB_C == 1, 1, 0),
                 C5_COB_NENHUM = ifelse(C5_COB_A!= 1 & C5_COB_B!= 1 & C5_COB_C!= 1 & 
                                        C5_COB_D!= 1 & C5_COB_E!= 1 & C5_COB_F!= 1 &
                                        C5_COB_G!= 1,1,0),
                 # INDICADOR B1 - INDIVÍDUOS QUE JÁ UTILIZARAM UM COMPUTADOR¹
                 B1 = ifelse(C5_A==1 | C5_B==1 | C5_C==1 | B1==1,1,B1),
                 # INDICADOR B2 - INDIVÍDUOS QUE USARAM UM COMPUTADOR, POR ÚLTIMO ACESSO
                 B2 = ifelse(C5_A==1 | C5_B==1 | C5_C==1 | B2==1,1,B2))

# Cria os labels de valores das variáveis derivadas acima
baseind$variables <- baseind$variables  %>% 
  add_value_labels(RENDA_FAMILIAR_2 = c("Até 1 SM" = 1,
                                        "Mais de 1 SM até 2 SM" = 2,
                                        "Mais de 2 SM até 3 SM" = 3,
                                        "Mais de 3 SM até 5 SM" = 4,
                                        "Mais de 5 SM até 10 SM" = 5,
                                        "Mais de 10 SM" = 6,
                                        "Não tem renda" = 7,
                                        "Não sabe" = 97,
                                        "Não respondeu" = 98),
                   GRAU_INST = c("Analfabeto/Educação Infantil" = 1,
                                 "Fundamental" = 2,
                                 "Médio" = 3,
                                 "Superior" = 4),
                   PEA_2 = c("Na força de trabalho" = 1,
                             "Fora da força de trabalho" = 2),
                   C3J3 = c("Sim" = 1,
                            "Não" = 0,
                            "Não sabe" = 97,
                            "Não respondeu" = 98),
                   C2_OUTRO = c("Por outro motivo" = 1),
                   C2A_2 = c("Por falta de necessidade" = 1,
                             "Por falta de interesse" = 2,
                             "Por falta de habilidade com o computador" = 3,
                             "Por não ter onde usar" = 4,
                             "Por ser muito caro" = 5,
                             "Por preocupações com segurança ou privacidade" = 6,
                             "Para evitar o contato com conteúdo perigoso" = 7,
                             "Por não ter Internet em casa" = 9,
                             "Por outro motivo" = 8,
                             "Não sabe" = 97,
                             "Não respondeu" = 98,
                             "Não se aplica" = 99),
                   C5_AGREG = c("Total - Computador" = 1),
                   C5_DISPOSITIVOS = c("Apenas computador" = 1,
                                       "Apenas telefone celular" = 2,
                                       "Ambos" = 3),
                   C8_NAO = c("Não utilizou a Internet para realizar atividades de interação com autoridades públicas" = 1),
                   I1_NENHUM = c("Nenhuma dessas atividades" = 1),
                   J5_FAIXAS = c("Nenhuma" = 0,
                                 "Uma" = 1, 
                                 "Duas" = 2, 
                                 "Três ou mais" = 3,
                                 "Não sabe" = 97,
                                 "Não respondeu" = 98),
                   J2_NENHUM = c("Nenhuma dessas atividades" = 1),
                   J3_AGREG = c("Sim" = 1,
                                "Não" = 0,
                                "Não sabe" = 97,
                                "Não respondeu" = 98),
                   J3AB = c("Apenas 3G ou 4G" = 1,
                            "Apenas WiFi" = 2,
                            "Ambos" = 3,
                            "Indeterminado" = 4),
                   C1_COB_AGREG = c("Sim" = 1,
                                    "Não" = 0,
                                    "Não se aplica" = 99),
                   C5_COB_AGREG = c("Total - Computador" = 1),
                   C5_COB_NENHUM = c("Nenhum desses dispositivos" = 1),
                   B1 = c("Sim" = 1,
                          "Não" = 0,
                          "Não sabe" = 97,
                          "Não respondeu" = 98),
                   B2 = c("Há menos de três meses (usuário)¹" = 1,
                          "Entre três e doze meses atrás" = 2,
                          "Mais de doze meses atrás" = 3,
                          "Nunca usou um computador" = 99))
    
# Converte o tipo de todas as variáveis que possuem label para fator
baseind$variables <- as_factor(baseind$variables,
                               only_labelled = TRUE, 
                               levels = "default", 
                               ordered = FALSE)


# Reproduz os indicadores publicados (resultados devem ser iguais as tabelas do site)
# Indicador: B1 - INDIVÍDUOS QUE JÁ UTILIZARAM UM COMPUTADOR¹
# Variável utilizada:
# B1 - O respondente já usou um computador de mesa, um notebook ou um tablet?
# Filtro utilizado:
# Este indicador foi calculado para o Total da população
# Portanto, não há filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
# svyby (combinado com svymean) fornece as proporções e os erros-padrão
# svyby (combinado com svytotal) fornece os totais e os erros-padrão
# margem de erro = erro-padrão*1.96
B1_AREA <- svyby(~B1, ~AREA, design = baseind, svymean)
B1_REGIAO <- svyby(~B1, ~COD_REGIAO, design = baseind, svymean)
B1_SEXO <- svyby(~B1, ~SEXO, design = baseind, svymean)
B1_RACA <- svyby(~B1, ~RACA, design = baseind, svymean)
B1_ESCOL <- svyby(~B1, ~GRAU_INST, design = baseind, svymean)
B1_FX_ETA <- svyby(~B1, ~FAIXA_ETARIA, design = baseind, svymean)
B1_RENDA <- svyby(~B1, ~RENDA_FAMILIAR_2, design = baseind, svymean)
B1_CLASSE <- svyby(~B1, ~CLASSE_CB2015, design = baseind, svymean)
B1_PEA <- svyby(~B1, ~PEA_2, design = baseind, svymean)

# Indicador: B2 - INDIVÍDUOS QUE USARAM UM COMPUTADOR, POR ÚLTIMO ACESSO
# Variável utilizada:
# B2 - Quando o respondente usou um computador de mesa, um notebook ou um tablet pela última vez?
# Filtro utilizado:
# Este indicador foi calculado para o Total da população
# Portanto, não há filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
B2_REGIAO <- svyby(~B2, ~COD_REGIAO, design = baseind, svymean)
B2_RACA <- svyby(~B2, ~RACA, design = baseind, svymean)
B2_PEA <- svyby(~B2, ~PEA_2, design = baseind, svymean)

# Indicador: C1 - INDIVÍDUOS QUE JÁ ACESSARAM A INTERNET¹
# Variável utilizada:
# C1 - O respondente já usou a Internet?
# Filtro utilizado:
# Este indicador foi calculado para o Total da população
# Portanto, não há filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
C1_ESCOL <- svyby(~C1, ~GRAU_INST, design = baseind, svymean)
C1_FX_ETA <- svyby(~C1, ~FAIXA_ETARIA, design = baseind, svymean)
C1_RENDA <- svyby(~C1, ~RENDA_FAMILIAR_2, design = baseind, svymean)

# Indicador: C2 - INDIVÍDUOS, POR ÚLTIMO ACESSO À INTERNET
# Variável utilizada:
# C3 - Quando o respondente usou a Internet pela última vez?
# Filtro utilizado:
# Este indicador foi calculado para o Total da população
# Portanto, não há filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
C2_PEA <- svyby(~C3, ~PEA_2, design = baseind, svymean)
C2_REGIAO <- svyby(~C3, ~COD_REGIAO, design = baseind, svymean)
C2_ESCOL <- svyby(~C3, ~GRAU_INST, design = baseind, svymean)

# Indicador: J1 - INDIVÍDUOS QUE USARAM TELEFONE CELULAR NOS ÚLTIMOS TRÊS MESES
# Variável utilizada:
# J1 - Nos últimos 3 meses, o respondente usou um telefone celular?
# Filtro utilizado:
# Este indicador foi calculado para o Total da população
# Portanto, não há filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
J1_AREA <- svyby(~J1, ~AREA, design = baseind, svymean)
J1_REGIAO <- svyby(~J1, ~COD_REGIAO, design = baseind, svymean)
J1_SEXO <- svyby(~J1, ~SEXO, design = baseind, svymean)

# Indicador: C16A - USUÁRIOS DE INTERNET, POR DISPOSITIVO UTILIZADO DE FORMA EXCLUSIVA OU SIMULTÂNEA
# Variável utilizada:
# C5_DISPOSITIVOS - Nos últimos 3 meses, o respondente utilizou a Internet no computador, telefone celular ou ambos? (var criada no update acima) 
# Filtro utilizado:
# Este indicador foi calculado para o Total de de usuários de Internet
# Portanto, deve-se filtrar os que responderam C3 = 1 (utilizando a função subset)

# Verifica o tipo da variável C3
str(baseind$variables$C3)
# Tabela para ver as categorias da variável C3
table(baseind$variables$C3)

# Gera as tabelas (utilizando o desenho amostral)
C16A_RACA <- svyby(~as.factor(C5_DISPOSITIVOS), ~RACA, design = subset(baseind, C3 == "Há menos de 3 meses"), svymean)
C16A_ESCOL <- svyby(~as.factor(C5_DISPOSITIVOS), ~GRAU_INST, design = subset(baseind, C3 == "Há menos de 3 meses"), svymean)
C16A_FX_ETA <- svyby(~as.factor(C5_DISPOSITIVOS), ~FAIXA_ETARIA, design = subset(baseind, C3 == "Há menos de 3 meses"), svymean)

