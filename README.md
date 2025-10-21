# Japan–Vietnam Research Collborations Projects

## Overview

-   **Query (Scopus):** `AFFILCOUNTRY ( japan AND viet* )`

-   **Timestamp:** 17:30 (GMT+7) 20/10/2025, Sources data are *September* snapshot

-   **Initial hits:** 14,240 documents

-   **Filtered to journals:** 9,982 documents (primary research output across disciplines)

-   **Primary file:** `data/all_1020.csv` → processed to `data/processed_data.csv`

-   **Extras merged in:**

    -   Curated **funder categories** (by region & sector) from manual Scopus filter exports
    -   **SJR** scores and **quartiles** (journal-level); \~6% unmatched due to title misalignments across sources

------------------------------------------------------------------------

## Column dictionary

Below is a concise dictionary for `processed_data.csv` (formerly `reg_data` in code).

| Column | Type | Meaning / Values |
|--------------|--------------|---------------------------------------------|
| `id` | integer | Row id generated after load. |
| `year` | integer | Publication year. |
| `title` | character | Journal title (Scopus `Source.title`). |
| `cited` | integer | Citation count (`Cited.by`). |
| `affiliations` | character | Semicolon-separated author affiliations (raw). |
| `DOI` | character | Digital Object Identifier. |
| `OA` | factor | Open access flag: `Not OA`, `OA` (derived from `Open.Access`). |
| `LS` | integer | Life Sciences macro-area indicator (Scopus source mapping). |
| `SS` | integer | Social Sciences macro-area indicator. |
| `PS` | integer | Physical Sciences macro-area indicator. |
| `HS` | integer | Health Sciences macro-area indicator. |
| `mult` | integer | Multidisciplinary subject flag (journal scope). |
| `agr_bio` | integer | Agriculture & Biological Sciences. |
| `art_hum` | integer | Arts & Humanities. |
| `bio_chem` | integer | Biochemistry / related biological chemistry fields. |
| `buss` | integer | Business, Management & Accounting. |
| `chem_eng` | integer | Chemical Engineering. |
| `chem` | integer | Chemistry. |
| `comp_sci` | integer | Computer Science. |
| `des_sci` | integer | Decision Sciences. |
| `earth` | integer | Earth & Planetary Sciences. |
| `econ` | integer | Economics, Econometrics & Finance. |
| `ener` | integer | Energy. |
| `egin` | integer | Engineering (field code; appears as `egin` in source). |
| `env_sci` | integer | Environmental Science. |
| `immu` | integer | Immunology & Microbiology. |
| `mat_sci` | integer | Materials Science. |
| `math` | integer | Mathematics. |
| `med` | integer | Medicine. |
| `neuro` | integer | Neuroscience. |
| `nurse` | integer | Nursing. |
| `pharm` | integer | Pharmacology, Toxicology & Pharmaceutics. |
| `phys` | integer | Physics & Astronomy. |
| `psy` | integer | Psychology. |
| `soc_sci` | integer | Social Sciences. |
| `vet` | integer | Veterinary. |
| `den` | integer | Dentistry. |
| `heal` | integer | Health Professions / Allied Health. |
| `coop` | factor | Cooperation type inferred from unique countries per paper: `bilateral` (exactly 2), `multilateral` (\>2). |
| `asian` `eu` `int` `jap` `us` `vn` | integer | Funder **region** flags by DOI (1 = present). `int` = international (non-regional). |
| `pub` `uni` `ind` | integer | Funder **sector** flags by DOI: public / university / industry (1 = present). |
| `fund` | factor | Funding presence derived from any region flag: `Not funded`, `Funded`. |
| `n_funder` | factor | Count of regions with non-zero flags among `{asian, eu, int, jap, us, vn}`. |
| `n_ftype` | factor | Count of sectors with non-zero flags among `{ind, pub, uni}`. *(Computed as sum of sector dummies.)* |
| `sjr_score` | numeric | SJR. |
| `quartile` | factor | Best SJR quartile (e.g., `Q1`–`Q4`). |

> ℹ️ All subject-area and macro-area columns are 0/1 indicators mapped from journal-level source metadata.