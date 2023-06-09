#*******************************************************************************
#*************************** TIC Domic�lios 2020 *******************************
#* Esta sintaxe contempla:
#* Leitura das bases de dados (Domic�lios e Indiv�duos) 
#* Cria��o de vari�veis (e labels)
#* Produ��o de tabelas cruzadas (utilizando o desenho amostral)      
#* �ltima atualiza��o: 27/09/2021
#*******************************************************************************

#===============================================================================
# Limpa objetos da mem�ria (Environment)
rm(list=ls())
#===============================================================================
# Limpa Console
cat("\014")
#===============================================================================
# Carrega (e instala, se necess�rio) pacotes requeridos
if (!require("install.load")) install.packages("install.load")
suppressMessages(install.load::install_load("tidyverse", "survey", "haven", "labelled"))
#===============================================================================
# Define diret�rio (pasta onde se encontra a base de microdados)
setwd("Diret�rio da base .RData")
#===============================================================================
# Carrega as bases de dados da TIC DOMIC�LIOS 2020 - Domic�lios e Indiv�duos
load("ticdom_2020_base_de_microdados_v1.0.RData")
#===============================================================================

#*******************************************************************************
#******************************** Domic�lios ***********************************
#*******************************************************************************

# Cria vari�veis derivadas (que ser�o utilizadas para tabula��o)
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
                 # PARA O INDICADOR A - DOM�CILIOS QUE POSSUEM EQUIPAMENTO TIC
                 DIC_TV = ifelse(TV>0,1,0),
                 DIC_RADIO = ifelse(RADIO>0,1,0),
                 # INDICADOR A1 - DOMIC�LIOS COM COMPUTADOR�
                 A1_AGREG = ifelse(A1_A == 1 | A1_B == 1 | A1_C == 1, 1, 0),
                 # INDICADOR A2A - DOMIC�LIOS COM COMPUTADOR, POR TIPO DE COMPUTADOR PRESENTE DE FORMA EXCLUSIVA OU SIMULT�NEA NO DOMIC�LIO
                 A1_EXCLUSIVOS = ifelse(A1_A == 1 & A1_B != 1 & A1_C != 1, 1,
                                 ifelse(A1_A != 1 & A1_B == 1 & A1_C != 1, 2,
                                 ifelse(A1_A != 1 & A1_B != 1 & A1_C == 1, 3,99))),
                 # INDICADOR A2B - DOMIC�LIOS COM COMPUTADOR, POR FAIXA DE QUANTIDADE DE TIPO DE COMPUTADOR
                 A2_A_FAIXA = ifelse(A2_QTD_DESK == 1, 1,
                              ifelse(A2_QTD_DESK > 1 & A2_QTD_DESK<999999999, 2,
                              ifelse(A2_QTD_NOTE >=1 & A2_QTD_TAB >=1 & A2_QTD_DESK==999999999,0,999999999))),
                 A2_B_FAIXA = ifelse(A2_QTD_NOTE == 999999999 & A2_QTD_TAB >=1 & A2_QTD_DESK>=1,0,
                              ifelse(A2_QTD_NOTE == 1,1,
                              ifelse(A2_QTD_NOTE > 1  &  A2_QTD_NOTE < 999999999, 2,999999999))),
                 A2_C_FAIXA = ifelse(A2_QTD_NOTE >= 1 & A2_QTD_TAB == 999999999 & A2_QTD_DESK>=1, 0,
                              ifelse(A2_QTD_TAB == 1, 1,
                              ifelse(A2_QTD_TAB > 1 & A2_QTD_TAB <999999999,  2, 999999999))),
                 # INDICADOR A4B - DOMIC�LIOS, POR PRESEN�A DE COMPUTADOR E INTERNET�
                 A1A4 = ifelse(A1_AGREG == 1 & A4 == 1, 1,
                        ifelse(A1_AGREG == 1 & A4 == 0, 2,
                        ifelse(A1_AGREG == 0 & A4 == 1, 3,
                        ifelse(A1_AGREG == 0 & A4 == 0, 4, 4)))),
                 # INDICADOR A5 - DOMIC�LIOS COM ACESSO � INTERNET, POR TIPO DE CONEX�O
                 A7_1 = ifelse(A7 == 1, 1, 99),
                 A7_AGREG = ifelse(A7 == 2 | A7 == 3 | A7 == 4 | A7 == 5, 1, 99),
                 A7_3 = ifelse(A7 == 3, 1, 99),
                 A7_4 = ifelse(A7 == 2, 1, 99),
                 A7_5 = ifelse(A7 == 4, 1, 99),
                 A7_6 = ifelse(A7 == 5, 1, 99),
                 A7_7 = ifelse(A7 == 6, 1, 99),
                 A7_97 = ifelse(A7 == 97, 1, 99),
                 A7_98 = ifelse(A7 == 98, 1, 99),
                 # INDICADOR A10 - DOMIC�LIOS SEM ACESSO � INTERNET, POR MOTIVOS PARA A FALTA DE INTERNET
                 A5_NENHUM = ifelse((A5_A == 0 | A5_A == 97 | A5_A == 98) & 
                                     A5_B != 1 & A5_C != 1 & A5_D != 1 & A5_E != 1 & 
                                     A5_F != 1 & A5_G != 1 & A5_H !=1 & A5_I != 1 & 
                                     A5_OUTRO != 1, 1, 99),
                 # INDICADOR A11 - DOMIC�LIOS COM ACESSO � INTERNET, POR VALOR PAGO PELA PRINCIPAL CONEX�O
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

# Cria os labels de valores das vari�veis derivadas acima
basedom$variables <- basedom$variables  %>% 
         add_value_labels(RENDA_FAMILIAR_2 =  c("At� 1 SM" = 1,
                                                "Mais de 1' SM at� 2 SM" = 2,
                                                "Mais de 2 SM at� 3 SM" = 3,
                                                "Mais de 3 SM at� 5 SM" = 4,
                                                "Mais de 5 SM at� 10 SM" = 5,
                                                "Mais de 10 SM" = 6,
                                                "N�o tem renda" = 7,
                                                "N�o sabe" = 97,
                                                "N�o respondeu" = 98),
                          DIC_TV = c("Televis�o" = 1),
                          DIC_RADIO = c("R�dio" = 1),
                          A1_AGREG = c("Sim" = 1,
                                       "N�o" = 0),
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
                          A7_1 = c("Conex�o discada" = 1),
                          A7_AGREG = c("Total - Banda larga fixa" = 1),
                          A7_3 = c("Conex�o via cabo de TV ou fibra �tica" = 1),
                          A7_4 = c("Conex�o via linha telef�nica (DSL)" = 1),
                          A7_5 = c("Conex�o via r�dio" = 1),
                          A7_6 = c("Conex�o via sat�lite" = 1),
                          A7_7 = c("Conex�o m�vel via modem ou chip 3G ou 4G" = 1),
                          A7_97 = c("N�o sabe" = 1),
                          A7_98 = c("N�o respondeu" = 1),
                          A5_NENHUM = c("Nenhum desses motivos" = 1),
                          A9_FAIXA = c("At� R$ 30" = 1,
                                       "R$ 31 a R$ 40" = 2,
                                       "R$ 41 a R$ 50" = 3,
                                       "R$ 51 a R$ 60" = 4,
                                       "R$ 61 a R$ 70" = 5,
                                       "R$ 71 a R$ 80" = 6,
                                       "R$ 81 a R$ 90" = 7,
                                       "R$ 91 a R$ 100" = 8,
                                       "R$ 101 a R$ 150" = 9,
                                       "Mais de R$ 150" = 10,
                                       "N�o sabe" = 97,
                                       "N�o respondeu" = 98))

# Converte o tipo de todas as vari�veis que possuem label para fator
basedom$variables <- as_factor(basedom$variables,
                               only_labelled = TRUE, 
                               levels = "default", 
                               ordered = FALSE)

# Reproduz os indicadores publicados (resultados devem ser iguais as tabelas do site)
# Indicador: A4 - DOMIC�LIOS COM ACESSO � INTERNET
# Vari�vel utilizada:
# A4 - Este domic�lio tem acesso � Internet?
# Filtro utilizado:
# Este indicador foi calculado para o Total de domic�lios
# Portanto, n�o h� filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
# svyby (combinado com svymean) fornece as propor��es e os erros-padr�o
# svyby (combinado com svytotal) fornece os totais e os erros-padr�o
# margem de erro = erro-padr�o*1.96
A4_AREA <- svyby(formula =~A4, 
                 by =~AREA, 
                 design = basedom, 
                 FUN = svymean)
A4_REGIAO <- svyby(~A4, ~COD_REGIAO, design = basedom, svymean)
A4_RENDA <- svyby(~A4, ~RENDA_FAMILIAR_2, design = basedom, svymean)
A4_CLASSE <- svyby(~A4, ~CLASSE_CB2015, design = basedom, svymean)

# Indicador: A1 - DOMIC�LIOS COM COMPUTADOR�
# Vari�vel utilizada:
# A1_AGREG - Possuir ao menos um tipo de computador (var criada no update acima) 
# Filtro utilizado:
# Este indicador foi calculado para o Total de domic�lios
# Portanto, n�o h� filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
A1_AREA <- svyby(~A1_AGREG, ~AREA, design = basedom, svymean)
A1_REGIAO <- svyby(~A1_AGREG, ~COD_REGIAO, design = basedom, svymean)
A1_RENDA <- svyby(~A1_AGREG, ~RENDA_FAMILIAR_2, design = basedom, svymean)
A1_CLASSE <- svyby(~A1_AGREG, ~CLASSE_CB2015, design = basedom, svymean)

# Indicador: A12 - DOMIC�LIOS COM ACESSO � INTERNET, POR PRESEN�A DE WIFI
# Vari�vel utilizada:
# A7A - Neste domic�lio tem Wi-Fi?
# Filtro utilizado:
# Este indicador foi calculado para o Total de domic�lios com acesso � Internet
# Portanto, deve-se filtrar os que responderam A4 = 1 (utilizando a fun��o subset)

# Verifica o tipo da vari�vel A4
str(basedom$variables$A4)
# Tabela para ver as categorias da vari�vel A4
table(basedom$variables$A4)

# Gera as tabelas (utilizando o desenho amostral)
A12_AREA <- svyby(~A7A, ~AREA, design = subset(basedom, A4 == "Sim"), svymean)
A12_REGIAO <- svyby(~A7A, ~COD_REGIAO, design = subset(basedom, A4 == "Sim"), svymean)
A12_RENDA <- svyby(~A7A, ~RENDA_FAMILIAR_2, design = subset(basedom, A4 == "Sim"), svymean)
A12_CLASSE <- svyby(~A7A, ~CLASSE_CB2015, design = subset(basedom, A4 == "Sim"), svymean)


#*******************************************************************************
#******************************** Indiv�duos ***********************************
#*******************************************************************************

# Cria vari�veis derivadas (que ser�o utilizadas para tabula��o)
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
                 # PARA O INDICADOR C2A - USU�RIOS DE INTERNET - INDICADOR AMPLIADO�
                 C3J3 = ifelse(C3 == 1 | J3 == 1, 1,
                        ifelse(C3 != 1 & (J3 == 0 | J3 == 99), 0,
                        ifelse(C3 != 1 & J3 == 97, 97,
                        ifelse(C3 != 1 & J3 == 98, 98, 999)))),
                 # INDICADOR C15 - INDIV�DUOS QUE NUNCA UTILIZARAM INTERNET, POR MOTIVO DECLARADO PARA NUNCA TER UTILIZADO A INTERNET
                 C2_OUTRO = ifelse(C2_J == 1 | C2_K == 1 | C2_L == 1 | C2_M == 1 | 
                                   C2_N == 1 | C2_O == 1 | C2_P == 1, 1, 0),
                 # INDICADOR C15A - INDIV�DUOS QUE NUNCA UTILIZARAM INTERNET, POR PRINCIPAL MOTIVO DECLARADO PARA NUNCA TER UTILIZADO A INTERNET
                 C2A_2 = ifelse(C2A_1> 9 & C2A_1 < 97, 8, C2A_1),
                 # INDICADOR C16 - USU�RIOS DE INTERNET, POR DISPOSITIVO UTILIZADO
                 C5_AGREG = ifelse(C5_A==1 | C5_B==1 | C5_C==1, 1, 0),
                 # INDICADOR C16A - USU�RIOS DE INTERNET, POR DISPOSITIVO UTILIZADO DE FORMA EXCLUSIVA OU SIMULT�NEA
                 C5_DISPOSITIVOS = ifelse(C5_AGREG == 1 & C5_D != 1, 1,
                                   ifelse(C5_AGREG != 1 & C5_D == 1, 2,
                                   ifelse(C5_AGREG == 1 & C5_D == 1, 3,99))),
                 # INDICADOR G3 - USU�RIOS DE INTERNET, POR ATIVIDADES DE INTERA��O COM AUTORIDADES P�BLICAS
                 C8_NAO = ifelse(C8_F != 1 & C8_G != 1,1,99),
                 # INDICADOR I1 - USU�RIOS DE COMPUTADOR, POR HABILIDADES PARA USO DO COMPUTADOR
                 I1_NENHUM = ifelse(I1_A!=1 & I1_B!=1 & I1_C!=1 & I1_D!=1 & I1_E!=1 & I1_F!=1 &
                                    I1_G!=1 & I1_H!=1 & I1_I!=1, 1, 99),
                 # INDICADOR J2A - INDIV�DUOS, POR QUANTIDADE DE LINHAS DE TELEFONE CELULAR
                 J5_FAIXAS = ifelse(J5 == 0 | J5_QTD_LINHAS == 0, 0,
                             ifelse(J5 == 1 & J5_QTD_LINHAS == 1, 1, 
                             ifelse(J5 == 1 & J5_QTD_LINHAS == 2, 2, 
                             ifelse(J5 == 1 & (J5_QTD_LINHAS > 2 & J5_QTD_LINHAS < 96), 3,
                             ifelse(J5 == 97, 97,
                             ifelse(J5 == 98, 98, 99)))))),
                 # INDICADOR J4 - USU�RIOS DE TELEFONE CELULAR, POR ATIVIDADES REALIZADAS NO TELEFONE CELULAR NOS �LTIMOS TR�S MESES
                 J2_NENHUM = ifelse(J2_H1!=1 & J2_I!=1 & J2_J!=1 & J2_K!=1 & J2_L!=1 & J2_N!=1, 1, 99),
                 # INDICADOR J5 - INDIV�DUOS QUE USARAM A INTERNET NO TELEFONE CELULAR NOS �LTIMOS TR�S MESES�
                 J3_AGREG = ifelse(J3 == 1,1,
                            ifelse(J3 == 0 | J1 == 0,0,
                            ifelse(J3 == 97| J1 == 97, 97,
                            ifelse(J3 == 98| J1 == 97, 98,99)))),
                 # INDICADOR J6A - USU�RIOS DE INTERNET PELO TELEFONE CELULAR, POR TIPO DE CONEX�O UTILIZADA DE FORMA EXCLUSIVA OU SIMULT�NEA�
                 J3AB = ifelse(J3A_A==1 & J3A_B!=1,1,
                        ifelse(J3A_B==1 & J3A_A!=1,2,
                        ifelse(J3A_A==1 & J3A_B==1,3,4))),
                 # INDICADOR L1 - N�O USU�RIOS QUE UTILIZARAM ALGUMA APLICA��O SELECIONADA NOS �LTIMOS TR�S MESES�
                 C1_COB_AGREG = ifelse(C1_COB_A == 99 | C1_COB_B == 99 | C1_COB_C == 99 |
                                       C1_COB_D == 99, 99, 
                                ifelse(C1_COB_A == 1 | C1_COB_B == 1 | C1_COB_C == 1 |
                                       C1_COB_D == 1, 1, 0)),
                 # INDICADOR L5 - N�O USU�RIOS QUE UTILIZARAM ALGUMA APLICA��O SELECIONADA NOS �LTIMOS TR�S MESES, POR DISPOSITIVO UTILIZADO�
                 C5_COB_AGREG = ifelse(C5_COB_A == 1 | C5_COB_B == 1 | C5_COB_C == 1, 1, 0),
                 C5_COB_NENHUM = ifelse(C5_COB_A!= 1 & C5_COB_B!= 1 & C5_COB_C!= 1 & 
                                        C5_COB_D!= 1 & C5_COB_E!= 1 & C5_COB_F!= 1 &
                                        C5_COB_G!= 1,1,0),
                 # INDICADOR B1 - INDIV�DUOS QUE J� UTILIZARAM UM COMPUTADOR�
                 B1 = ifelse(C5_A==1 | C5_B==1 | C5_C==1 | B1==1,1,B1),
                 # INDICADOR B2 - INDIV�DUOS QUE USARAM UM COMPUTADOR, POR �LTIMO ACESSO
                 B2 = ifelse(C5_A==1 | C5_B==1 | C5_C==1 | B2==1,1,B2))

# Cria os labels de valores das vari�veis derivadas acima
baseind$variables <- baseind$variables  %>% 
  add_value_labels(RENDA_FAMILIAR_2 = c("At� 1 SM" = 1,
                                        "Mais de 1 SM at� 2 SM" = 2,
                                        "Mais de 2 SM at� 3 SM" = 3,
                                        "Mais de 3 SM at� 5 SM" = 4,
                                        "Mais de 5 SM at� 10 SM" = 5,
                                        "Mais de 10 SM" = 6,
                                        "N�o tem renda" = 7,
                                        "N�o sabe" = 97,
                                        "N�o respondeu" = 98),
                   GRAU_INST = c("Analfabeto/Educa��o Infantil" = 1,
                                 "Fundamental" = 2,
                                 "M�dio" = 3,
                                 "Superior" = 4),
                   PEA_2 = c("Na for�a de trabalho" = 1,
                             "Fora da for�a de trabalho" = 2),
                   C3J3 = c("Sim" = 1,
                            "N�o" = 0,
                            "N�o sabe" = 97,
                            "N�o respondeu" = 98),
                   C2_OUTRO = c("Por outro motivo" = 1),
                   C2A_2 = c("Por falta de necessidade" = 1,
                             "Por falta de interesse" = 2,
                             "Por falta de habilidade com o computador" = 3,
                             "Por n�o ter onde usar" = 4,
                             "Por ser muito caro" = 5,
                             "Por preocupa��es com seguran�a ou privacidade" = 6,
                             "Para evitar o contato com conte�do perigoso" = 7,
                             "Por n�o ter Internet em casa" = 9,
                             "Por outro motivo" = 8,
                             "N�o sabe" = 97,
                             "N�o respondeu" = 98,
                             "N�o se aplica" = 99),
                   C5_AGREG = c("Total - Computador" = 1),
                   C5_DISPOSITIVOS = c("Apenas computador" = 1,
                                       "Apenas telefone celular" = 2,
                                       "Ambos" = 3),
                   C8_NAO = c("N�o utilizou a Internet para realizar atividades de intera��o com autoridades p�blicas" = 1),
                   I1_NENHUM = c("Nenhuma dessas atividades" = 1),
                   J5_FAIXAS = c("Nenhuma" = 0,
                                 "Uma" = 1, 
                                 "Duas" = 2, 
                                 "Tr�s ou mais" = 3,
                                 "N�o sabe" = 97,
                                 "N�o respondeu" = 98),
                   J2_NENHUM = c("Nenhuma dessas atividades" = 1),
                   J3_AGREG = c("Sim" = 1,
                                "N�o" = 0,
                                "N�o sabe" = 97,
                                "N�o respondeu" = 98),
                   J3AB = c("Apenas 3G ou 4G" = 1,
                            "Apenas WiFi" = 2,
                            "Ambos" = 3,
                            "Indeterminado" = 4),
                   C1_COB_AGREG = c("Sim" = 1,
                                    "N�o" = 0,
                                    "N�o se aplica" = 99),
                   C5_COB_AGREG = c("Total - Computador" = 1),
                   C5_COB_NENHUM = c("Nenhum desses dispositivos" = 1),
                   B1 = c("Sim" = 1,
                          "N�o" = 0,
                          "N�o sabe" = 97,
                          "N�o respondeu" = 98),
                   B2 = c("H� menos de tr�s meses (usu�rio)�" = 1,
                          "Entre tr�s e doze meses atr�s" = 2,
                          "Mais de doze meses atr�s" = 3,
                          "Nunca usou um computador" = 99))
    
# Converte o tipo de todas as vari�veis que possuem label para fator
baseind$variables <- as_factor(baseind$variables,
                               only_labelled = TRUE, 
                               levels = "default", 
                               ordered = FALSE)


# Reproduz os indicadores publicados (resultados devem ser iguais as tabelas do site)
# Indicador: B1 - INDIV�DUOS QUE J� UTILIZARAM UM COMPUTADOR�
# Vari�vel utilizada:
# B1 - O respondente j� usou um computador de mesa, um notebook ou um tablet?
# Filtro utilizado:
# Este indicador foi calculado para o Total da popula��o
# Portanto, n�o h� filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
# svyby (combinado com svymean) fornece as propor��es e os erros-padr�o
# svyby (combinado com svytotal) fornece os totais e os erros-padr�o
# margem de erro = erro-padr�o*1.96
B1_AREA <- svyby(~B1, ~AREA, design = baseind, svymean)
B1_REGIAO <- svyby(~B1, ~COD_REGIAO, design = baseind, svymean)
B1_SEXO <- svyby(~B1, ~SEXO, design = baseind, svymean)
B1_RACA <- svyby(~B1, ~RACA, design = baseind, svymean)
B1_ESCOL <- svyby(~B1, ~GRAU_INST, design = baseind, svymean)
B1_FX_ETA <- svyby(~B1, ~FAIXA_ETARIA, design = baseind, svymean)
B1_RENDA <- svyby(~B1, ~RENDA_FAMILIAR_2, design = baseind, svymean)
B1_CLASSE <- svyby(~B1, ~CLASSE_CB2015, design = baseind, svymean)
B1_PEA <- svyby(~B1, ~PEA_2, design = baseind, svymean)

# Indicador: B2 - INDIV�DUOS QUE USARAM UM COMPUTADOR, POR �LTIMO ACESSO
# Vari�vel utilizada:
# B2 - Quando o respondente usou um computador de mesa, um notebook ou um tablet pela �ltima vez?
# Filtro utilizado:
# Este indicador foi calculado para o Total da popula��o
# Portanto, n�o h� filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
B2_REGIAO <- svyby(~B2, ~COD_REGIAO, design = baseind, svymean)
B2_RACA <- svyby(~B2, ~RACA, design = baseind, svymean)
B2_PEA <- svyby(~B2, ~PEA_2, design = baseind, svymean)

# Indicador: C1 - INDIV�DUOS QUE J� ACESSARAM A INTERNET�
# Vari�vel utilizada:
# C1 - O respondente j� usou a Internet?
# Filtro utilizado:
# Este indicador foi calculado para o Total da popula��o
# Portanto, n�o h� filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
C1_ESCOL <- svyby(~C1, ~GRAU_INST, design = baseind, svymean)
C1_FX_ETA <- svyby(~C1, ~FAIXA_ETARIA, design = baseind, svymean)
C1_RENDA <- svyby(~C1, ~RENDA_FAMILIAR_2, design = baseind, svymean)

# Indicador: C2 - INDIV�DUOS, POR �LTIMO ACESSO � INTERNET
# Vari�vel utilizada:
# C3 - Quando o respondente usou a Internet pela �ltima vez?
# Filtro utilizado:
# Este indicador foi calculado para o Total da popula��o
# Portanto, n�o h� filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
C2_PEA <- svyby(~C3, ~PEA_2, design = baseind, svymean)
C2_REGIAO <- svyby(~C3, ~COD_REGIAO, design = baseind, svymean)
C2_ESCOL <- svyby(~C3, ~GRAU_INST, design = baseind, svymean)

# Indicador: J1 - INDIV�DUOS QUE USARAM TELEFONE CELULAR NOS �LTIMOS TR�S MESES
# Vari�vel utilizada:
# J1 - Nos �ltimos 3 meses, o respondente usou um telefone celular?
# Filtro utilizado:
# Este indicador foi calculado para o Total da popula��o
# Portanto, n�o h� filtro a ser feito

# Gera as tabelas (utilizando o desenho amostral)
J1_AREA <- svyby(~J1, ~AREA, design = baseind, svymean)
J1_REGIAO <- svyby(~J1, ~COD_REGIAO, design = baseind, svymean)
J1_SEXO <- svyby(~J1, ~SEXO, design = baseind, svymean)

# Indicador: C16A - USU�RIOS DE INTERNET, POR DISPOSITIVO UTILIZADO DE FORMA EXCLUSIVA OU SIMULT�NEA
# Vari�vel utilizada:
# C5_DISPOSITIVOS - Nos �ltimos 3 meses, o respondente utilizou a Internet no computador, telefone celular ou ambos? (var criada no update acima) 
# Filtro utilizado:
# Este indicador foi calculado para o Total de de usu�rios de Internet
# Portanto, deve-se filtrar os que responderam C3 = 1 (utilizando a fun��o subset)

# Verifica o tipo da vari�vel C3
str(baseind$variables$C3)
# Tabela para ver as categorias da vari�vel C3
table(baseind$variables$C3)

# Gera as tabelas (utilizando o desenho amostral)
C16A_RACA <- svyby(~as.factor(C5_DISPOSITIVOS), ~RACA, design = subset(baseind, C3 == "H� menos de 3 meses"), svymean)
C16A_ESCOL <- svyby(~as.factor(C5_DISPOSITIVOS), ~GRAU_INST, design = subset(baseind, C3 == "H� menos de 3 meses"), svymean)
C16A_FX_ETA <- svyby(~as.factor(C5_DISPOSITIVOS), ~FAIXA_ETARIA, design = subset(baseind, C3 == "H� menos de 3 meses"), svymean)

