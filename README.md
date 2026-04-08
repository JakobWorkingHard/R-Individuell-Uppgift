# README: Analys och Prediktion av Försäkringskostnader

Detta projekt analyserar historiska försäkringsdata för att identifiera vilka kundfaktorer som driver kostnader. Genom datastädning, utforskande visualiseringar (inklusive en heatmap) och multipel linjär regression byggs en modell för att predicera priser och hitta dolda synnergieffekter.

Följ stegen nedan för att reproducera analysen på din egen dator.

## 1. Ställ in rätt arbetsmapp (Working Directory)
För att R ska kunna hitta datafilen automatiskt måste du ställa in din arbetsmapp till den mapp där koden ligger. 
Öppna R-skriptet i RStudio och navigera via toppmenyn:
**`Session` ➔ `Set Working Directory` ➔ `To Source File Location`**

## 2. Kontrollera datafilen
Säkerställ att filen **`insurance_costs.csv`** ligger i exakt samma mapp som själva R-skriptet på din dator.

## 3. Installera nödvändiga paket
Detta projekt använder sig av några externa paket för att manipulera och visualisera datan. Om du inte redan har dessa installerade, kopiera och kör följande rad i din R-konsol:

```R
install.packages(c("tidyverse", "dplyr", "corrplot"))