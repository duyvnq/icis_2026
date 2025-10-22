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
| `LS` | binary | Life Sciences macro-area indicator (Scopus source mapping). |
| `SS` | binary | Social Sciences macro-area indicator. |
| `PS` | binary | Physical Sciences macro-area indicator. |
| `HS` | binary | Health Sciences macro-area indicator. |
| `mult` | binary | Multidisciplinary subject flag (journal scope). |
| `agr_bio` | binary | Agriculture & Biological Sciences. |
| `art_hum` | binary | Arts & Humanities. |
| `bio_chem` | binary | Biochemistry / related biological chemistry fields. |
| `buss` | binary | Business, Management & Accounting. |
| `chem_eng` | binary | Chemical Engineering. |
| `chem` | binary | Chemistry. |
| `comp_sci` | binary | Computer Science. |
| `des_sci` | binary | Decision Sciences. |
| `earth` | binary | Earth & Planetary Sciences. |
| `econ` | binary | Economics, Econometrics & Finance. |
| `ener` | binary | Energy. |
| `egin` | binary | Engineering (field code; appears as `egin` in source). |
| `env_sci` | binary | Environmental Science. |
| `immu` | binary | Immunology & Microbiology. |
| `mat_sci` | binary | Materials Science. |
| `math` | binary | Mathematics. |
| `med` | binary | Medicine. |
| `neuro` | binary | Neuroscience. |
| `nurse` | binary | Nursing. |
| `pharm` | binary | Pharmacology, Toxicology & Pharmaceutics. |
| `phys` | binary | Physics & Astronomy. |
| `psy` | binary | Psychology. |
| `soc_sci` | binary | Social Sciences. |
| `vet` | binary | Veterinary. |
| `den` | binary | Dentistry. |
| `heal` | binary | Health Professions / Allied Health. |
| `coop` | factor | Cooperation type inferred from unique countries per paper: `bilateral` (exactly 2), `multilateral` (\>2). |
| `asian` `eu` `int` `jap` `us` `vn` | binary | Funder **region** flags by DOI (1 = present). `int` = international (non-regional). |
| `pub` `uni` `ind` | binary | Funder **sector** flags by DOI: public / university / industry (1 = present). |
| `fund` | factor | Funding presence derived from any region flag: `Not funded`, `Funded`. |
| `n_funder` | factor | Count of regions with non-zero flags among `{asian, eu, int, jap, us, vn}`. |
| `n_ftype` | factor | Count of sectors with non-zero flags among `{ind, pub, uni}`. *(Computed as sum of sector dummies.)* |
| `sjr_score` | numeric | SJR. |
| `quartile` | factor | Best SJR quartile (e.g., `Q1`–`Q4`). |

> ℹ️ All subject-area and macro-area columns are 0/1 indicators mapped from journal-level source metadata.