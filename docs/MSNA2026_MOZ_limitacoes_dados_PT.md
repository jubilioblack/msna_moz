---
title: "MSNA 2026 Moçambique – Limitações dos dados, da ferramenta e impacto nas análises"
date: "16/11/2026"
---

## Resumo

Os dados (21 445 agregados, 126 distritos, export de 01/10/2026) são utilizáveis, mas têm cinco limitações que afectam as análises e devem constar da nota metodológica:

1. **O export não tem metadados de entrevista** (hora, data, enumerador, GPS). A qualidade da recolha não pode ser verificada por duração, por dia nem por enumerador.
2. **Os módulos HHS e FIES discordam** em 15–21% dos agregados para o mesmo período de 30 dias. A severidade da fome depende do indicador escolhido.
3. **A pergunta sobre a primeira consulta pré-natal mistura semanas e meses** (72% das respostas ≤ 9). O indicador de cuidados pré-natais precoces não é fiável tal como está.
4. **O quadro populacional da OCHA não cobre 784 entrevistas** (21 estratos com população 0) e tem 8 estratos com mais entrevistas do que agregados. Os pesos dessas zonas dependem de uma decisão metodológica (pool com os não-deslocados) e não de números oficiais.
5. **33 dos 159 distritos não foram inquiridos.** Os resultados nacionais e provinciais representam os 126 distritos cobertos, não o país inteiro.

Os números deste documento vêm da corrida de teste da pipeline de limpeza a 16/11/2026. São **preliminares** até ao fim da revisão dos cleaning logs.

## Abreviaturas

| Abreviatura | Significado |
| --- | --- |
| admin1 / admin2 / admin3 | Província / distrito / posto administrativo (níveis administrativos 1, 2 e 3) |
| DTM | Displacement Tracking Matrix (Matriz de Acompanhamento de Deslocados) da OIM |
| FCS | Food Consumption Score (Pontuação de Consumo Alimentar) |
| FIES | Food Insecurity Experience Scale (Escala de Experiência de Insegurança Alimentar) |
| GPS | Global Positioning System (coordenadas geográficas) |
| HHS | Household Hunger Scale (Escala de Fome no Agregado) |
| IC | Intervalo de confiança |
| IDP | Internally Displaced Person (pessoa deslocada interna) |
| INGD | Instituto Nacional de Gestão e Redução do Risco de Desastres |
| IPC | Integrated Food Security Phase Classification (Classificação Integrada das Fases de Segurança Alimentar) |
| LCS | Livelihood Coping Strategies (estratégias de sobrevivência baseadas nos meios de vida) |
| LSG | Living Standards Gap (lacuna nos padrões de vida) |
| MSNA | Multi-Sector Needs Assessment (Avaliação Multissectorial de Necessidades) |
| OCHA | Escritório das Nações Unidas para a Coordenação dos Assuntos Humanitários |
| OIM | Organização Internacional para as Migrações (IOM em inglês) |
| p-code | Código de lugar padronizado das unidades administrativas (ex.: MZ0101 = Ancuabe) |
| R21 / R22 | Ronda 21 / ronda 22 do DTM |
| rCSI | reduced Coping Strategies Index (Índice Reduzido de Estratégias de Sobrevivência) |
| SA | Segurança alimentar |
| XML | Formato em que o Kobo exporta nomes de variáveis e códigos de resposta ("XML values and headers") |

## Limitações do export de dados

O ficheiro `REACH_MOZ_Data_MSNA_01102026.xlsx` não é um export Kobo padrão: é uma versão processada, rotulada e sem metadados.

| Limitação | Evidência | Consequência |
| --- | --- | --- |
| Cabeçalhos e respostas em rótulos ingleses, não em nomes/códigos XML | 1 029 colunas só identificáveis pela ordem do formulário; rótulos repetidos ("If other, please specify:2") | Foi preciso um passo de conversão (script 0). Uma alteração de rótulo ou de ordem num novo export pode desalinhar colunas sem erro visível; o log de conversão tem de ser revisto a cada export |
| Escolha múltipla: a coluna-mãe vem como texto de rótulos separados por espaços | "Insecurity Armed conflict" não se separa de forma fiável | A coluna-mãe foi reconstruída a partir das colunas 0/1; o texto original não é usado |
| Sem `start`, `end`, `today`, `_submission_time` | Nenhuma coluna de data/hora da entrevista | Sem verificação de duração; sem tracker diário; sem remoção do piloto por data; sem análise por período de recolha |
| Sem `enum_code`, `deviceid`, `audit` | Só existe `enum_gender` | Sem verificações por enumerador (padrões, velocidade, quase-duplicados do mesmo enumerador); `enum_id` vazio nos logs |
| Sem GPS | Nenhuma coordenada | Sem verificação de localização face ao posto declarado; sem mapas a nível de ponto |
| `enum_gender` com dois códigos | M 8 283, F 6 764, male 3 562, female 2 836 | Sinal de que o export junta fontes ou versões diferentes; foi harmonizado para male/female |
| `point_number` inexistente | — | O `_id` do Kobo é usado como identificador nos logs; as equipas de campo têm de localizar o registo pelo `_id` |

Risco geral: como não sabemos que processamento foi feito antes do export (remoções, recodificações, scores calculados), a limpeza não consegue distinguir um erro de recolha de um erro de processamento.

## Limitações do formulário Kobo

Três problemas do formulário afectam directamente indicadores; os outros afectam sobretudo o trabalho de limpeza.

| Limitação | Evidência | Consequência |
| --- | --- | --- |
| `health_antenatal_care_months` pergunta **semanas** de gravidez, mas o nome diz meses | 3 132 respostas: 2 261 (72%) entre 0 e 9, 868 entre 10 e 42, 3 acima de 42 | Muitos enumeradores devem ter registado meses. O indicador "primeira consulta pré-natal no 1.º trimestre" fica enviesado e não é recuperável só com os dados |
| Coluna `health_antenatal_care_weeks_first_visit` no export, fora do formulário | Idêntica a `health_antenatal_care_months` | Duplicado criado no processamento; não é uma segunda medida |
| HHS e FIES perguntam o mesmo para os mesmos 30 dias | Discordância: sem comida em casa 20,8%, dormir com fome 16,0%, dia e noite sem comer 15,2% | A prevalência de fome severa muda com o módulo usado. Provável efeito de formulação, tradução ou formação, não de registos isolados |
| Rótulos de opções mudaram entre a versão recolhida e a versão final | Algumas opções de escolha múltipla têm no export um rótulo diferente do formulário final | Convertido sem perda, mas confirma que houve versões do formulário durante a recolha |
| 52 perguntas "especificar outro" | 4 870 textos no agregado + loops; 76 215 linhas no log de revisão | Volume pesado de recodificação; enquanto não for feita, a categoria "outro" fica inflacionada |
| Restrições largas em perguntas numéricas | Ex.: 0–50 em semanas e consultas pré-natais; idades até 120 | O Kobo não travou valores improváveis; ficam a cargo das verificações lógicas |
| Sem identificação explícita do inquirido no roster | A regra "o 1.º membro é o inquirido" só é verificada a posteriori (`MOZ_ROS_002`) | Indicadores sobre o inquirido dependem dessa regra ter sido seguida |

Para o MSNA seguinte: renomear `health_antenatal_care_months`, fixar a unidade no rótulo e na restrição (0–42 semanas), e rever a tradução das perguntas HHS e FIES lado a lado.

## Limitações do quadro populacional e dos pesos

Os pesos usam a folha "Baseline data" do ficheiro da OCHA (159 distritos; IDPs e retornados do DTM ronda 22; não-deslocados = total − IDPs − retornados), com estratos distrito × grupo populacional.

| Limitação | Evidência | Tratamento / consequência |
| --- | --- | --- |
| Grupos entrevistados que a OCHA dá como população 0 | 784 entrevistas em 21 estratos: IDPs em Maputo Cidade (98), Bilene (93), Boane (90), Marracuene (85), Palma (38)…; retornados em Balama (99), Chókwè (41), Chigubo (30)… | Ponderados com os não-deslocados do mesmo distrito (decisão de 16/11/2026). Os totais de IDPs/retornados nesses distritos vêm da amostra, não de um número oficial |
| Mais entrevistas do que agregados estimados | 8 estratos, 222 entrevistas. Ex.: retornados de Chemba, 40 entrevistas para 36 pessoas (≈ 6 agregados) | O quadro subestima estes grupos; os pesos ficam muito baixos (mínimo ≈ 0,0005) e estes agregados quase não contam nos resultados agregados |
| População em pessoas, não em agregados | Colunas da OCHA são indivíduos | Convertida dividindo pelo tamanho médio do agregado do estrato na amostra; erros no `hh_size` passam para os pesos |
| Distritos sem entrevistas | 33 dos 159 distritos; 37 estratos com população e sem entrevistas | Os resultados representam os 126 distritos inquiridos, não o país |
| Duas versões de população no mesmo ficheiro | Sheet1/Sheet4 (só Cabo Delgado, DTM R21): Ancuabe 199 392; Baseline (R22): 193 878 | Usada a Baseline; confirmar com a OCHA qual é a versão de referência |
| Pesos muito dispersos | 0,0005 a 9,9; efeito de desenho de Kish ≈ 2,3 | A dimensão efectiva da amostra nacional fica em cerca de 9 400 agregados, não 21 445; margens de erro maiores do que a amostra sugere |

Os resultados por distrito × grupo não dependem dos pesos (peso constante dentro do estrato). As limitações acima afectam os resultados que juntam estratos: provincial, nacional, total IDPs.

## Limitações do processo de limpeza

A limpeza cobre a coerência das respostas, mas não a forma como as entrevistas foram feitas.

| Limitação | Evidência | Consequência |
| --- | --- | --- |
| Verificações de comportamento do enumerador impossíveis | Sem duração, data, enumerador nem GPS | Entrevistas curtas, inventadas ou feitas fora do local não são detectáveis; risco principal de qualidade que fica por controlar |
| Quase-duplicados só dentro do mesmo posto | Corrida nos 21 445 de uma vez exigiria dezenas de GB de memória; 10 duplicados de perfil exacto encontrados | Duplicados entre postos diferentes não são detectados |
| Verificações lógicas só cruzam perguntas | 64 verificações; 24 461 registos sinalizados no teste | Apanham incoerências, não respostas falsas mas coerentes |
| Discordância HHS/FIES só verificada num sentido | `MOZ_HH_027`: dia e noite sem comer em HHS mas não em FIES | As outras discordâncias (15–21%) ficam nos dados; tratar na análise, não na limpeza |
| "Faltas ≥ 50%" conta perguntas condicionais | 1 413 agregados (6,6%) sinalizados | Muitas sinalizações são falsos positivos; rever com critério |
| Volume de revisão elevado | 126 logbooks de distrito; 76 215 linhas de "especificar outro" | Risco de revisão apressada (`no_action` em massa) e de prazos; converter "outros" frequentes por regra em vez de linha a linha |

## Impacto nas análises

As limitações com mais peso nos resultados são a discordância HHS/FIES, a pergunta pré-natal e os pesos das zonas fora do quadro OCHA.

| Limitação | Indicadores afectados | Efeito provável | Como reportar |
| --- | --- | --- | --- |
| HHS e FIES discordam (15–21%) | Fome severa (HHS), FIES, compostos de segurança alimentar e LSG de SA | A prevalência muda com o módulo escolhido | Escolher um módulo principal (HHS, por alinhamento com o IPC), reportar o outro em anexo e explicar a diferença |
| Semanas/meses na 1.ª consulta pré-natal | Cuidados pré-natais no 1.º trimestre | Enviesado, sentido desconhecido | Não publicar este indicador; usar só "recebeu cuidados pré-natais" e n.º de consultas |
| 784 entrevistas em estratos sem população OCHA | Totais e % de IDPs e retornados em Maputo, Gaza, Palma, Balama, etc.; agregados provinciais e nacionais | Esses agregados pesam como os não-deslocados do distrito | Resultados por distrito × grupo não ponderados; nota de rodapé nos agregados por grupo |
| 8 estratos com mais entrevistas que agregados | Agregados nacionais e provinciais de IDPs | Estes agregados quase não contam (pesos ≈ 0,0005–0,003) | Nota metodológica; sensibilidade com pesos limitados (trimming) se o grupo for central |
| 33 distritos não inquiridos | Todos os resultados nacionais e provinciais | Representam 126 distritos, não o país | Indicar cobertura em todos os gráficos e tabelas nacionais |
| Pesos dispersos (efeito de desenho ≈ 2,3) | Todos os resultados ponderados | Intervalos de confiança ≈ 1,5 vezes mais largos que numa amostra sem pesos | Calcular sempre com o desenho (estrato + peso, p. ex. srvyr); nunca % simples ponderadas sem IC |
| Sem metadados de entrevista | Todos | Fabricação ou entrevistas apressadas não detectadas | Declarar a limitação; pedir ao parceiro de recolha as evidências de supervisão |
| Scores já calculados no export (FCS, rCSI, HHS, LCS, FIES) | Categorias de segurança alimentar | Método e limiares desconhecidos | Recalcular a partir das perguntas (p. ex. humind) e comparar antes de usar |
| "Especificar outro" por recodificar | Fontes de rendimento, razões de deslocamento, barreiras | Categoria "outro" inflacionada | Só analisar depois de aplicar o log de "especificar outro" |

## Recomendações

As três primeiras resolvem a maior parte do risco e devem ser feitas antes da análise.

- [ ] Pedir ao parceiro de recolha o export Kobo em "XML values and headers" com `start`, `end`, `_submission_time`, `enum_code` e GPS, para correr as verificações de duração e por enumerador
- [ ] Confirmar com a OCHA a versão de referência da população (Baseline R22 vs Sheet1/Sheet4) e pedir números para os 21 estratos com população 0 (INGD ou outra ronda do DTM para Maputo e Gaza)
- [ ] Decidir com a equipa de segurança alimentar qual módulo (HHS ou FIES) é o principal e documentar a discordância
- [ ] Retirar o indicador de 1.ª consulta pré-natal por trimestre das análises e corrigir a pergunta no próximo formulário
- [ ] Recalcular FCS, rCSI, HHS, LCS e FIES a partir das perguntas e comparar com os scores do export
- [ ] Concluir a revisão dos cleaning logs e do "especificar outro" antes de qualquer tabela
- [ ] Incluir na nota metodológica: cobertura (126 de 159 distritos), estratos sem população, efeito de desenho e ausência de metadados

Ficheiros de referência no projecto `MSNA_cleaning_MOZ`: `inputs/REACH_MOZ_MSNA2026_data_xml_conversion_log.xlsx` (conversão), `inputs/REACH_MOZ_MSNA2026_logical_checks.xlsx` (verificações), `docs_test_run/flags_by_check_test_run_2026-11-16.xlsx` (sinalizações) e o ficheiro de pesos gerado por `computing_weights.R`.
