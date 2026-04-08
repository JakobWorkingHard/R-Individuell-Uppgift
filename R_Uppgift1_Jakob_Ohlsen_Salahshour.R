  library(tidyverse)
  library(corrplot)
  
  insurance_raw <- read_csv("insurance_costs.csv")
  
  # Noterar att customer_id är chr
  glimpse(insurance_raw)
  
  
  # Noterar  lågt antal tidigare olyckor men förmodligen flera outliers
  summary(insurance_raw)
  
  # 3 kolumner innehar ett lågt antal NA värden 
  colSums(is.na(insurance_raw))
  
  
  # Städbehov: region, smoker, plan_type
  columns <- c("sex", "region", "smoker", "chronic_condition", "exercise_level",
               "plan_type")
  
  for (column in columns) {
    print(paste("--", column, "--"))
    print(unique(insurance_raw[[column]]))
  }
  
  # I övrigt ser kategorierna jämna ut, utan att någonting sticker ut.
  for (category_info in columns) {
    print(insurance_raw %>% 
            count(.data[[category_info]], sort = TRUE))
  }
  
  # --- Städar datan ---
  
  # Städar och gör om till faktorer
  insurance_cleaned <- insurance_raw %>%
    mutate(
      region = str_to_title(region),
      smoker = str_to_lower(smoker),
      plan_type = str_to_title(plan_type),
      across(all_of(columns), as.factor)
    )
  
  # Kollar resultatet av städningen: Ser bra ut!
  # Noterar: Datan är jämnt fördelad
  for (category_info in columns) {
    print(insurance_cleaned %>% 
            count(.data[[category_info]], sort = TRUE))
  }
  
  # Ser hur våra NA värden har påverkat försäkringskostnader
  insurance_cleaned %>%
    mutate(has_na = if_any(everything(), is.na)) %>%
    summarise(
      total_charges = sum(charges, na.rm = TRUE),
      na_charges = sum(charges[has_na == TRUE], na.rm = TRUE),
      na_percentage = (na_charges / total_charges) * 100
    )
  
  # Väljer att fylla våra saknade värden med imputer istället för att droppa dessa rader
  # Börjar med att räkna ut den mest vanligt förekommande exercise_level
  most_frequent_exercise <- insurance_cleaned %>%
    drop_na(exercise_level) %>%
    count(exercise_level) %>%
    arrange(desc(n)) %>%
    pull(exercise_level) %>%
    first()
  
  print(paste("Mest vanligt förekommande träningsnivå: ", most_frequent_exercise))
  
  # Ersätter saknade BMI och hälsoundersökningsvärden med medelvärdet respektive medianen
  insurance_imputed <- insurance_cleaned %>%
    mutate(
      bmi = replace_na(bmi, mean(bmi, na.rm = TRUE)),
      annual_checkups = replace_na(annual_checkups, median(annual_checkups, na.rm = TRUE)),
      exercise_level = as.character(exercise_level),
      exercise_level = replace_na(exercise_level, as.character(most_frequent_exercise)),
      exercise_level = as.factor(exercise_level)
    )
  
  # Kontrollerar så slutresultatet stämmer
  colSums(is.na(insurance_imputed))
  
  # Lägger till ny kategorisk variabel för BMI
  insurance_imputed <- insurance_imputed %>%
    mutate(
      bmi_category = case_when(
        bmi < 18.5 ~ "Underweight",
        bmi >= 18.5 & bmi < 25 ~ "Normal",
        bmi >= 25 & bmi < 30 ~ "Overweight",
        bmi >= 30 ~ "Obese"
      ),
      bmi_category = factor(bmi_category, levels = c("Normal", "Underweight", "Overweight", "Obese"))
    )
  
  # -- Undersöker datan --
  
  # Försäkringskostnaderna verkar vara jämna även dessa
  insurance_imputed %>% 
    summarize(
      avg_charge = mean(charges, na.rm = TRUE),
      median_charge = median(charges, na.rm = TRUE),
      sd_charge = sd(charges, na.rm = TRUE),
      min_charge = min(charges, na.rm = TRUE),
      q1_charge = quantile(charges, 0.25, na.rm = TRUE),
      q3_charge = quantile(charges, 0.75, na.rm = TRUE),
      max_charge = max(charges, na.rm = TRUE)
    )
  
  # Boxplot avslöjar att här finns flera outliers!
  ggplot(insurance_imputed, aes(y = charges)) +
    geom_boxplot() +
    labs(
      title = "Boxplot för försäkringskostnader",
      x = "Försäkringskostnader",
      y = "Antal försäkringsfall"
    )  
  
  # Ser om vi kan finna mönster i var dessa outliers har för betalplan
  # Majoriteten av outliers ligger i "Standard"
  ggplot(insurance_imputed, aes(x = plan_type, y = charges)) +
    geom_boxplot() +
    labs(
      title = "Boxplot: Försäkringskostnader och betalplan ",
      x = "Betalplan",
      y = "Antal försäkringsfall"
    )  
  
  # Verkar ej finnas mönster mellan outliers och regioner
  ggplot(insurance_imputed, aes(x = plan_type, y = charges)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(aes(color = region), alpha = 0.5, width = 0.2) +
    labs(
      title = "Försäkringskostnader med regional spridning",
      x = "Betalplan",
      y = "Kostnad",
      color = "Region"
    )
  
  # Ej någon region som sticker ut, utan det är enbart Standard planen som sticker ut.
  region_summary <- insurance_imputed %>%
    group_by(region, plan_type) %>%
    summarise(
      total_charges = sum(charges, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    arrange(desc(total_charges))
  
  region_summary
  
  # -- Antal hälsoundersökningar och försäkringskostnader --
  # Trots min hypotes om att antalet hälsoundersökningar skulle kunna predicera
  # försäkringskostnader så ser det ej ut att finnas något samband.
  ggplot(insurance_imputed, aes(x = as.factor(annual_checkups), y = charges)) +
    geom_boxplot(fill = "lightblue") +
    labs(
      titel = "Antal hälsoundersökningar och försäkringskostnader",
      x = "Antal hälsokontroller",
      y = "Försäkringskostnader"
      )
  
  # I ren frustration över att finna samband för att få behålla platsen på den prestigefyllde yrkesutbildningen på EC så gör Jakob nu någonting somliga hade betraktat som kontroversiellt. Han gör en heatmap!
  
  #  Skapar heatmap
  # Noterar: Rökare och kroniska sjukdomar korrelerar starkt med försäkringskostnader,
  # men även tidigare försäkringsärenden och ålder
  heatmap_data <- insurance_imputed %>%
    select(-region, -customer_id, -plan_type, -exercise_level, -bmi_category) %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    cor(use = "complete.obs")
  
  corrplot(heatmap_data, 
           method = "color",           
           type = "full",              
           addCoef.col = "black",      
           number.cex = 0.7,           
           tl.col = "black",           
           tl.srt = 45,                
           diag = TRUE,                
           title = "Korrelation: Faktorer som påverkar Charges",
           mar = c(0,0,1,0))           
  
  
  # -- Linjär regression --
  
  # Vi väljer prediktorer baserat på de variabler som hade starkast korrelation från vår heatmap,
  # samt inkluderar bmi_category för att se hur övervikt/fetma spelar in.
  
  insurance_model <- lm(charges ~ smoker + chronic_condition + age + prior_claims + bmi_category, 
                        data = insurance_imputed)
  
  # Skriver ut resultatet
  summary(insurance_model)
  
  # Tolknings av resultat:
  
  # * Multiple R-squared avslöjar att vi lyckats fånga ungefär 71% av variansen i vår data
  
  # * Residuals: Medianen ligger på -180, så vår modell gissar nästan helt rätt för den generella kunden
  # * Residuals: Q1 och Q3 är jämna, så mittersta 50% av modellens gissningar är symmetriska med plus/minus 1600
  # * Residuals: Min: Minsta: -9200 (vår modell gissar 9200 för högt som allra högst felgissning)
  # * Residuals: Max <--- INTRESSANT: Vår modell gissar 17500 för lågt på den lägsta felgissningen,
  #   vilket indikerar på att det kan finnas en synnergieffekt bland variablerna,
  #   exempelvis att det blir exponentiellt värre om man både röker och har kroniska sjukdomar.
  #   Denna slutsats drar vi för att skillnaden är så stor mellan Residualen Min och Max!
  
  # * Estimate: Hur mycket en variabel påverkar den direkta försäkringskostnaden. 
  #   Exempelvis om man röker så kostar man i snitt 7750 mer än om man inte röker.
  #   (Förutsatt att alla andra variabler hålls  konstanta)
  #   Så en rimlig slutsats skulle kunna vara att rökare betalar 7750 mer än icke
  #   rökare för försäkringen (men man måste även ta hänsyn till potentiell synnergieffekt)
  
  # * Samtliga variabler förutom underviktiga har ett  p-värde långt under 0.05,
  #   vilket visar på statistisk signifikans.
  
  
  # -- Modell för att utforska synnergieffekter --
  insurance_model_synergy <- lm(charges ~ prior_claims + bmi_category + 
  smoker * chronic_condition + smoker * age, 
  data = insurance_imputed)
  
  summary(insurance_model_synergy)
  
  # Mycket intressant resultat: Vår modell för att testa synnergier blev faktiskt
  # sämre än vår ursprungliga linjära regression, då p värdet för både kroniska
  # sjukdomar tillsammans med rökning samt även ålder och rökning har ett på tok
  # för högt p-värde, samt att våra residualer ser relativt oförändrade ut.
  
  # Vi testar en sista synnergieffekt innan vi tar helg!
  
  insurance_model_synergy_2 <- lm(charges ~ prior_claims * smoker + 
                                    prior_claims * chronic_condition +
                                    age + bmi_category, data = insurance_imputed)
  summary(insurance_model_synergy_2)
  
  # Fortfarande ingen större skillnad bland våra residualer samt att vår nya
  # variabler priorclaims+smokers ligger precis över gränsen 0.05 i p värde, 
  # samt även att prior claims + chronic conditions har ett på tok för högt p värde.
  
  # Slutsatsen är att vi ej lyckades finna några synnergier, men att det kan finnas
  # där, och vidare utforskning skulle behövas. Så vi gör kör en sista LM, där vi
  # istället  undersöker en klassiker: De som lider av fetma som också röker.
  
  insurance_model_synergy_final <- lm(charges ~ prior_claims + age + 
                                        smoker * bmi_category + chronic_condition, 
                                      data = insurance_imputed)
  summary(insurance_model_synergy_final)
  
  # Här ser vi för första gången en ökning i i vår R-squared, så denna modell 
  # förklarar lite  mer än 1% mer av variansen än våra tidigare modeller.
  # Vi ser även att Max residualen har sjunkit med 2000!
  
  # Vi ser även att de som lider av fetma och som också röker kostar 3400 extra.
  # Detta innebär att kalkylen för de som lider av fetma och röker ser ut såhär:
  
  # Rökare: 7000
  # Fetma: 1500
  # Rökare+Fetma (straffavgift pga synnergieffekt): 3400
  
  # Synnergieffekten innehar även ett lågt p-värde vilket gör det statistiskt signifikant.
  
  # Slutsats: Vi fann en synnergieffekt mellan rökare och de som lider av fetma,
  # vilket försäkringspriserna bör ta hänsyn till.
  
  # -- Testar en fullständig modell --
  # Vi inkluderar region, betalplan och träningsnivå för att se om de
  # tillför någonting, samtidigt som vi behåller vår funna synnergieffekt.
  
  insurance_model_full <- lm(charges ~ prior_claims + age + chronic_condition + 
                               region + plan_type + exercise_level + 
                               smoker * bmi_category, 
                             data = insurance_imputed)
  
  summary(insurance_model_full)
  
  # Regionerna kan dra åt pipsvängen med deras höga p-värden
  # R-squared  har ökat till hela 75%! Så 75% av variansen förklaras av vår modell.
  
  # Slutsatser:
  # De som tränar lite kostar 1500 mer än de som tränar mycket
  # De som har köpt Premium-betalplanen kostar 1600 mer än de som valt basic
  # Rökare kostar 7000 mer än icke rökare
  # De som innehar kroniska sjukdomar kostar 3900 mer än de som inte har det
  # De som lider av fetma kostar 1500 mer än de som inte gör det
  # Synnergieffekt finns bland de som lider av fetma och även röker, där denna
  #    grupp kostar ytterliggare  3400 mer.
  # För varje extra försäkringsärende så kostar en kund 1100 mer.
  # För varje levnadsår kostar en kund 75 mer (20 årig kund: 1500 extra, 40 årig  kund: 3000 extra)
  

  
  # Nu tar vi helg!