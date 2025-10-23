# Japan–Vietnam Research Collaborations Projects

## Overview
- **Query (Scopus):** `AFFILCOUNTRY ( japan AND viet* )`
- **Timestamp:** 17:30 (GMT+7) 20/10/2025, Sources data are *September* snapshot
- **Initial hits:** 14,240 documents
- **Filtered to journals:** 9,982 documents (primary research output across disciplines)
- **Primary file:** `data/all_1020.csv` → processed to `data/processed_data.csv`
- **Extras merged in:**
  - Curated **funder categories** (by region & sector) from manual Scopus filter exports
  - **SJR** scores and **quartiles** (journal-level); ~6% unmatched due to title misalignments across sources
  - **SDG mapping** via text2sdg package (OSDG system)

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
| `LS` `SS` `PS` `HS` | binary | Life Sciences, Social Sciences, Physical Sciences, Health Sciences macro-area indicators (Scopus source mapping). |
| `mult` | binary | Multidisciplinary subject flag (journal scope). |
| `agr_bio` `art_hum` `bio_chem` `buss` `chem_eng` `chem` `comp_sci` `des_sci` `earth` `econ` `ener` `egin` `env_sci` `immu` `mat_sci` `math` `med` `neuro` `nurse` `pharm` `phys` `psy` `soc_sci` `vet` `den` `heal` | binary | Subject-area indicators: Agriculture & Biological Sciences, Arts & Humanities, Biochemistry, Business/Management/Accounting, Chemical Engineering, Chemistry, Computer Science, Decision Sciences, Earth & Planetary Sciences, Economics/Econometrics/Finance, Energy, Engineering, Environmental Science, Immunology & Microbiology, Materials Science, Mathematics, Medicine, Neuroscience, Nursing, Pharmacology/Toxicology/Pharmaceutics, Physics & Astronomy, Psychology, Social Sciences, Veterinary, Dentistry, Health Professions. |
| `coop` | factor | Cooperation type inferred from unique countries per paper: `bilateral` (exactly 2), `multilateral` (>2). |
| `n_countries` | integer | Number of unique countries involved in collaboration. |
| `asian` `eu` `int` `jap` `us` `vn` | binary | Funder **region** flags by DOI (1 = present). `int` = international (non-regional). |
| `pub` `uni` `ind` | binary | Funder **sector** flags by DOI: public / university / industry (1 = present). |
| `fund` | factor | Funding presence derived from any region flag: `Not funded`, `Funded`. |
| `n_funder` | factor | Count of regions with non-zero flags among `{asian, eu, int, jap, us, vn}`. |
| `n_ftype` | factor | Count of sectors with non-zero flags among `{ind, pub, uni}`. |
| `sjr_score` | numeric | SCImago Journal Rank score. |
| `quartile` | factor | Best SJR quartile (e.g., `Q1`–`Q4`). |
| `fa_vn` `fa_jp` `fa_o` | binary | First author affiliation: Vietnam / Japan / Other (1 = yes). |
| `n_vn_authors` `n_jp_authors` | integer | Count of Vietnamese / Japanese author affiliations per paper. |
| `n_vn_jp_authors` | integer | Total Vietnam + Japan authors. |
| `prop_vn_authors` | numeric | Proportion of Vietnamese authors (n_vn_authors / n_vn_jp_authors). |
| `vn_led` `jp_led` | binary | Vietnam-led / Japan-led authorship (based on first author, 1 = yes). |
| `collab_type` | factor | Collaboration intensity: `VN-dominated`, `JP-dominated`, `Balanced`, `Other`. |
| `SDG_1` ... `SDG_17` | binary | Sustainable Development Goal alignment flags (1 = paper addresses this SDG). |
| `n_sdg` | integer | Total number of SDGs addressed per paper. |

> ℹ️ All subject-area and macro-area columns are 0/1 indicators mapped from journal-level source metadata.