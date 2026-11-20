# treatledger

**Standardised cumulative treatment exposure for childhood cancer survivorship research.**

treatledger turns a simple table of treatment records into the cumulative
exposures used in late-effects research:

- **Doxorubicin-equivalent dose** for anthracyclines and anthraquinones
- **Cyclophosphamide equivalent dose (CED)** for alkylating agents
- **Cumulative radiotherapy dose** by body region

Every conversion factor is stored with its source, and every result records
the factor set and package version used, so studies can report exactly how
exposures were derived and rerun them under another set.

> **Research use only.** treatledger does not give screening or treatment
> recommendations and must not be used to guide individual patient care.

## Why

Published anthracycline conversion factors differ between sources. A
cumulative daunorubicin dose of 300 mg/m² is 150 mg/m² doxorubicin-equivalent
under the Children's Oncology Group guidelines (version 6.0) and 180 mg/m²
under Feijen et al. (2019). Most groups re-implement these calculations in
their own code. treatledger offers one open, tested implementation.

## Installation

```r
# install.packages("remotes")
remotes::install_github("Mrn-selyn-commits/treatledger")
```

## Example

```r
library(treatledger)

treatledger_demo            # invented records for five patients
tl_summarise(treatledger_demo, anthracycline_set = "cog_v6")
```

| Function | Returns |
| --- | --- |
| `tl_dox_equivalent()` | Cumulative doxorubicin-equivalent dose (mg/m²) |
| `tl_ced()` | Cyclophosphamide equivalent dose (mg/m²) |
| `tl_radiation()` | Cumulative radiotherapy dose (Gy) by region |
| `tl_summarise()` | All of the above, one row per patient |
| `tl_factor_sets()` | The available factor sets and their sources |
| `tl_standardise_agent()` | Generic drug names from common synonyms |

## Input

One row per treatment course, with a patient identifier, agent, cumulative
dose and unit (and region for radiotherapy). Drug doses should be in mg/m².
Anthracycline doses in mg/kg are converted by multiplying by 30, as in the COG
guidelines; for other drug classes they are flagged, not converted.

## Factor sets

| Set | Drug class | Source |
| --- | --- | --- |
| `cog_v6` | Anthracyclines | Children's Oncology Group Long-Term Follow-Up Guidelines, version 6.0 (2023), Section 34 |
| `feijen2019` | Anthracyclines | Feijen et al. (2019), JAMA Oncology 5(6):864–871; idarubicin from COG v6.0 |
| `green2014` | Alkylating agents | Green et al. (2014), Pediatric Blood & Cancer 61(1):53–67 |

The factor tables are also in `inst/extdata` as CSV files.

## Status

Version 0.1.0, in development. Before the first release, the CED
coefficients are still to be confirmed against the full text of Green et al.
(2014); they currently match two independent clinical guidance documents.

## References

- Children's Oncology Group (2023). *Long-Term Follow-Up Guidelines for Survivors of Childhood, Adolescent, and Young Adult Cancers*, Version 6.0.
- Feijen EAM et al. (2019). Derivation of anthracycline and anthraquinone equivalence ratios to doxorubicin for late-onset cardiotoxicity. *JAMA Oncology* 5(6):864–871. doi:10.1001/jamaoncol.2018.6634
- Green DM et al. (2014). The cyclophosphamide equivalent dose as an approach for quantifying alkylating agent exposure. *Pediatric Blood & Cancer* 61(1):53–67. doi:10.1002/pbc.24679

## Licence

MIT © 2026 Nayiga Joselyn Mirembe
