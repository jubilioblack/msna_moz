# MSNA 2026 Moçambique – pipeline de limpeza (adaptada do Uganda)

Abra `MSNA_cleaning_MOZ.Rproj` e corra `R-scripts/Master cleaning script.R` de cima para baixo.
Na primeira vez, corra antes `R-scripts/00_install_packages.R`.

## Ficheiros de entrada (`inputs/`)

| Ficheiro | O que é |
|---|---|
| `REACH_MOZ_MSNA2026_Kobo_tool.xlsx` | Formulário Kobo final de MOZ (`survey`/`choices`) |
| `REACH_MOZ_Data_MSNA_01102026.xlsx` | Export de dados recebido (cabeçalhos e valores **rotulados**) |
| `REACH_MOZ_MSNA2026_logical_checks.xlsx` | Verificações lógicas de MOZ (geradas pelo passo 1b) |
| `REACH_MOZ_MSNA2026_data_xml.rds` / `.xlsx` | Export convertido para nomes/códigos XML (gerado pelo passo 0) |
| `REACH_MOZ_MSNA2026_data_xml_conversion_log.xlsx` | Mapeamento cabeçalho → nome XML e valores sem correspondência (rever) |
| `REACH_MOZ_MSNA2026_sampling_targets.xlsx` *(opcional, não incluído)* | `admin2`, `population_group`, `target` – para o tracker |
| `REACH_MOZ_MSNA2026_population_frame.xlsx` | Quadro populacional da OCHA; os pesos usam a folha `Baseline data` (DTM R22) |
| `REACH_MOZ_MSNA2026_other_translations.xlsx` | Dicionário PT → EN das respostas "outro" (3 264 textos; 1.ª versão é um rascunho automático a verificar). O passo 6 acrescenta os textos novos com `response_en` vazio |
| `REACH_MOZ_MSNA2026_other_recode_rules.xlsx` | Regras de sugestão (lista, expressão regular, opção ou INVALID). Só preenchem `suggested_choice`; nada é alterado sem decisão do revisor |

## Passos

| Passo | Script | Notas MOZ |
|---|---|---|
| 0 | `0.Convert_labelled_export_to_xml_MOZ.R` | **Novo.** O export de MOZ tem as perguntas em inglês como cabeçalhos e os rótulos das opções como valores. O script reconstrói os nomes XML pela ordem do formulário, recodifica select_one para os códigos, refaz as colunas-mãe dos select_multiple a partir das dummies 0/1 e renomeia `uuid`→`_uuid` / `_submission__uuid`. Só volta a correr se o export ou o formulário mudarem. |
| 1b | `1b.Build_MOZ_logical_checks_workbook.R` | **Novo.** 64 verificações lógicas escritas para as variáveis de MOZ (44 no agregado, 20 nos loops), validadas contra o formulário (folha `QA_summary`). |
| 1 | `1.Load and process the data.R` | Lê o `.rds` convertido. Sem filtro de Kampala, sem filtro de piloto por data, sem compostos HumInd (os scores FCS/rCSI/HHS/LCS/FIES já vêm no export). Acrescenta `admin1_name`/`admin2_name`/`admin3_name` e `point_number` = `_id` do Kobo. |
| 2 | `2.Soft duplicate checks.R` | Quase-duplicados por posto administrativo (`admin3`). Correr para os 21 000 agregados de uma vez exigiria dezenas de GB de memória. |
| 3–5 | checks → processamento → export | Um livro de cleaning log por distrito: `outputs/<data>/District cleaning logs/<pcode>_<distrito>/`. |
| 6 | `6.Other_responses_MOZ.R` | **Novo (substitui `6.Specify_other_checks.R`).** Livro `Other responses/MSNA2026_MOZ_other_responses_<data>.xlsx` no formato IMPACT: uma linha por resposta escrita, `response_pt` (texto original), `response_en` (dicionário), `suggested_choice`, opções escolhidas e colunas de decisão TRUE / EXISTING 1-3 / INVALID / FOLLOW-UP com listas pendentes "English \| Português". Folha `Options_PT_EN` com todas as opções nas duas línguas. O passo 3 deixou de pôr linhas "outro" nos logbooks distritais. |
| 7 | `7.Progress_tracker_MOZ.R` | **Novo.** Entrevistas por província/distrito/posto × grupo populacional (e % da meta, se houver ficheiro de metas). |
| 8 | `8.Load_feedback_and_create_clean_data_MOZ.R` | **Novo.** Lê os logbooks revistos (folha `cleaning_log`) e o other_responses revisto (folha `other_responses`) em `inputs/feedback/`, converte as decisões "outro" em linhas de cleaning log, aplica as alterações aos 8 datasets, propaga `remove_survey` aos loops e exporta os dados limpos com a revisão (folha `other_responses_not_applied` = linhas sem decisão, com follow-up ou com erro). |
| pesos | `computing_weights.R` | Estratos distrito × grupo populacional, população da folha `Baseline data` da OCHA convertida em agregados (÷ tamanho médio do agregado no estrato). Entrevistas de grupos com população 0 na OCHA (784 no export de 01/10) são ponderadas com os não-deslocados do mesmo distrito (`pool_district`, decidido a 16/11/2026); `exclude` dá-lhes peso 0. |

### Revisão das respostas "outro" (passo 6 → passo 8)

Em cada linha, preencha **uma** decisão:

| Decisão | Quando | O que o passo 8 faz |
|---|---|---|
| TRUE other | É uma categoria nova | O texto passa a ser o texto em inglês da coluna; "outro" continua escolhido |
| EXISTING other 1-3 | Corresponde a opção(ões) do formulário (lista "EN \| PT") | select_one: a resposta passa a essa opção. select_multiple: `outro` = 0, opção(ões) = 1, coluna-mãe refeita. O texto é apagado. Em `cm_income_source`, a % de "outro" passa para a opção em EXISTING 1 |
| INVALID other = Yes | Não responde à pergunta | "outro" é retirado; se era a única opção, a pergunta fica vazia. O texto é apagado |
| FOLLOW-UP | Dúvida para o terreno | Nada é aplicado até haver decisão |

Linhas com mais de uma decisão, ou com uma opção que não pertence à lista, não são aplicadas e aparecem no relatório.
Logbooks distritais gerados antes de 18/11/2026 ainda têm linhas `recode other` (respostas "outro" em português). Essas respostas estão todas no other_responses; quando há um other_responses em `inputs/feedback/`, o passo 8 ignora as linhas `recode other` dos logbooks, para o mesmo texto não ser alterado duas vezes. Para tirar essas linhas dos logbooks, volte a correr os passos 3–5.
Limitação: ao recodificar para uma opção existente, as perguntas de seguimento dessa opção (relevant) ficam vazias.

## Datasets (nomes internos)

`Submissions` (principal), `roster`, `edu_ind`, `health_ind`, `nutrition_ind` (`nut_ind`), `health_wra`,
`prot_sep_child` (`prot_separated_children_breakdown`), `prot_hosted_child` (`prot_hosted_separated_children_breakdown`).
Moçambique não tem o loop `child_protection_ind` do Uganda.

Colunas de contexto nos logs: `admin1`, `admin2`, `admin3` (p-codes), `setting`, `admin2_name`, `population_group`.
Substituem as colunas do Uganda `admin3_setlement`, `admin4` e `hoh_type`.

## Limitações do export recebido

O ficheiro não tem `start`/`end`, `_submission_time`, `today`, `enum_code`, `deviceid` nem GPS. Por isso:
- não há verificação de duração da entrevista;
- não há tracker diário nem remoção de piloto por data (use `pilot_submission_uuids` no master);
- não há verificações por enumerador; o campo `enum_id` nos logs fica vazio.

Se for possível obter o export Kobo em formato **"XML values and headers"** com os metadados, o passo 0
deixa de ser necessário (aponte `path_data` diretamente para esse ficheiro) e estas verificações voltam a funcionar.

## Memória

Abrir o `.xlsx` de 120 MB no R (readxl) precisa de cerca de 7–8 GB de RAM. Se o R bloquear no passo 0,
grave cada folha como CSV UTF-8 numa pasta e aponte `path_data_labelled` para essa pasta.
Depois da conversão, o resto da pipeline lê o `.rds` e usou cerca de 3–4 GB nos testes.

## Scripts do Uganda não usados

Estão em `R-scripts/Source scripts/_uganda_only_not_used_in_MOZ/`, só para referência: trackers de
refugiados/acolhimento, cache Shiny, Loading/Review/8 do Uganda, 1b do Uganda.
