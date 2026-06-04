# Report Generator Audit

Inventory of 18 Python report-generator scripts in `python/`. Preparation for consolidation into a registry + template architecture.

## Table of Contents

1. [Per-Script Entries](#per-script-entries)
2. [Summary: Shared Pattern](#shared-pattern)
3. [Summary: Loading Variants](#loading-variants)
4. [Summary: Groupings](#groupings)
5. [Summary: Dependency Graph](#dependency-graph)
6. [Summary: Config Access](#config-access)

---

## Per-Script Entries

### 1. meta_reporter.py

**Purpose:** Pipeline health dashboard. Summarizes test results, type distributions, purity, drift, coupling, Boltzmann signatures, network stability, and orbit families.

**Data Sources:**
- `outputs/pipeline_output.json` — per-constraint classification and diagnostic data
- `outputs/output.txt` — Prolog test pass/fail counts (parsed via regex)
- `outputs/orbit_data.json` — orbit signatures and families

**Loading Method:** Own `pathlib.Path` + `json.load()`. Does NOT use `shared/loader`.

**Filters:** None (processes entire corpus).

**Computations:**
- Test pass/fail counts and rates (regex extraction from output.txt)
- Type distribution (constraint counts by claimed_type)
- Unique omega count and type breakdown
- Purity score average and band distribution
- Drift event counts by severity (critical/warning/watch)
- Coupling analysis (independent/weakly_coupled/strongly_coupled)
- Boltzmann signature tallies (FNL, CI-rope, FCR)
- Network stability assessment
- Gauge orbit families (singleton vs multi-member, unknown-containing orbits)

**Output:** Console only (stdout). No file output.

**Dependencies:**
- `import json, os, re, sys`
- `from collections import Counter`

**Unique Logic:**
- `_parse_test_results()` — regex parsing of `output.txt` for pass/fail counts
- Boltzmann signature aggregation (FNL/CI-rope/FCR counting)
- Network stability heuristic

**Shared Boilerplate:** JSON loading, type counting via Counter, markdown-formatted section output.

**CLI Arguments:** None.

---

### 2. type_reporter.py

**Purpose:** Parameterized reporter for 7 constraint types. Three report families: diagnostic (A), validation (B), false_mountain (C). Replaces older `count_computed_classifications.py` and `high_friction.py`.

**Data Sources:**
- `outputs/pipeline_output.json` — per-constraint data
- `outputs/orbit_data.json` — orbit signatures (via `orbit_utils`)

**Loading Method:** Own `load_pipeline_data()` via `pathlib.Path`. Imports `orbit_utils` for orbit data.

**Filters:** By constraint type: snare, piton, scaffold, rope, mountain (true_mountain), tangled_rope, false_mountain. Tangled_rope uses `_filter_any_perspective()` (any perspective sees tangled_rope). Others use unanimity or claimed_type filters.

**Computations:**
- Classification counts by claimed_type
- Gap count (friction) per constraint
- Top 15 high-friction constraints
- Perspectival agreement checks (unanimity)
- Severity ranking (critical > high > moderate > unknown)
- Orbit signature integration

**Output:** `outputs/{type}_report.md` for each type (7 files: snare_report.md, piton_report.md, scaffold_report.md, rope_report.md, true_mountain_report.md, tangled_rope_report.md, false_mountain_report.md).

**Dependencies:**
- `import argparse, json, sys`
- `from collections import Counter`
- `from orbit_utils import load_orbit_data, get_orbit_signature, format_orbit_signature`

**Unique Logic:**
- Three normalization families: `_normalize_diagnostic()` (per-omega), `_normalize_validation()` (per-constraint), `_normalize_false_mountain()` (per-gap)
- Family-specific deduplication and sorting
- Family-specific markdown writers (`_write_diagnostic_report`, `_write_validation_report`, `_write_false_mountain_report`)
- `summary_counts()` and `summary_friction()` subcommands

**Shared Boilerplate:** JSON loading, markdown table formatting.

**CLI Arguments:**
- `--type {snare|piton|scaffold|rope|mountain|tangled_rope|false_mountain}`
- `--all` — generate all type reports
- `--summary {counts|friction}` — print summary to stdout
- (Mutually exclusive group, one required)

---

### 3. omega_reporter.py

**Purpose:** Extracts omega epistemological gaps from pipeline data. Produces omega-centric records with severity and source gap formatting.

**Data Sources:**
- `outputs/pipeline_output.json` — per-constraint omega data

**Loading Method:** Own `load_pipeline_data()` via `pathlib.Path`. Does NOT use `shared/loader`.

**Filters:** Extracts omega entries from per-constraint data; deduplicates by omega name.

**Computations:**
- Unique omega count
- Omega severity assignment
- Source gap formatting (`gap(gap_type, powerless, institutional)`)
- Resolution strategy per constraint
- Sorting by severity (critical first)

**Output:**
- `outputs/omega_report.md` — markdown report
- `outputs/omega_data.json` — JSON sidecar for downstream enrichment

**Dependencies:**
- `import json, sys`

**Unique Logic:**
- `_format_source_gap()` — gap-to-string formatting
- `dedup_omegas()` — deduplication by omega name

**Shared Boilerplate:** JSON loading, markdown table formatting.

**CLI Arguments:** None.

---

### 4. omega_enricher.py

**Purpose:** Cross-references omega data with corpus metrics, orbit signatures, and domain information. Computes composite severity scores and assigns family IDs.

**Data Sources:**
- `outputs/omega_data.json` — parsed omegas from omega_reporter
- `outputs/corpus_data.json` — constraint metrics, domain, classifications
- `outputs/orbit_data.json` — orbit signatures

**Loading Method:** Uses `shared.loader.load_json` for all three files. Also imports `orbit_utils`.

**Filters:** Processes all omegas; resolves constraint IDs with fallback chain.

**Computations:**
- Epsilon (extractiveness), suppression, orbit span
- Composite severity score: `W_EPSILON(0.5) * eps + W_SUPPRESSION(0.3) * supp + W_ORBIT_SPAN(0.2) * orbit_span`
- Severity thresholds: critical >= 0.70, high >= 0.45, medium >= 0.25, else low
- Gap class: consensus, coordination_washing, severity_amplification, protective_framing, analytical_blind, powerless_blind, both_unknown
- Gap pattern (inferred from omega name prefix)
- Family IDs (F001, F002, ...) by (orbit_signature, gap_class, domain)
- Orbit span: ordinal distance between min/max types in orbit

**Output:**
- `outputs/enriched_omega_report.md` — 7-section markdown report
- `outputs/enriched_omega_data.json` — machine-readable enriched data

**Dependencies:**
- `import json, sys`
- `from collections import defaultdict`
- `from orbit_utils import load_orbit_data, get_orbit_signature`
- `from shared.loader import load_json`

**Unique Logic:**
- `compute_severity_score()` — 3-factor weighted composite
- `assign_families()` — groups by (orbit_sig, gap_class, domain)
- `print_distribution()` — severity distribution + percentile calibration

**Shared Boilerplate:** JSON loading via shared/loader, markdown formatting.

**CLI Arguments:** None.

---

### 5. reform_threshold_report.py

**Purpose:** Energy triage for snare-classified constraints. Determines reform thresholds, computes chi at powerless context, and applies coalition modeling.

**Data Sources:**
- `outputs/fingerprint_report.md` — **MARKDOWN-AS-DATA**: shift pattern families
- `outputs/corpus_data.json` — constraint metrics and domain
- `prolog/testsets/*.pl` — structural predicates, victim counts
- `prolog/probsets/*.pl` — additional predicates
- `prolog/config.pl` — thresholds (via `shared.loader.read_config()`)

**Loading Method:** Mixed. Uses `shared.loader.read_config()` for config. Own `json.load()` for corpus_data. Regex parsing for fingerprint_report.md. Glob for .pl files.

**Filters:** Constraints classified as snare from the powerless perspective (via fingerprint shift patterns).

**Computations:**
- Reform threshold: walks standard contexts (powerless/moderate/institutional/analytical), returns first context achieving rope immutability
- Chi at powerless: `epsilon * pi(power) * sigma(scope)`
- Coalition modeling: if `eps >= 0.46 AND supp >= 0.60 AND victims >= 3`, applies organized pi
- Victim counts per constraint (extracted from .pl files)
- Scope modifiers: local=0.8, national=1.0, global=1.2
- Perspectival divergence patterns (P=snare -> M=... I=... A=...)
- Domain distribution

**Output:** Console only (stdout).

**Dependencies:**
- `import json, sys, re`
- `from collections import defaultdict, Counter`
- `from sigmoid import POWER_MODIFIERS`
- `from shared.loader import read_config`

**Markdown-as-Data Parsing:**
- Regex: `r"^###\s+` `` `shift\([^)]+\)` `` `\s+.*?(\d+)\s+constraints?"` — extracts shift pattern headers
- Regex: `r"^- ` `` `([^`]+)` `` `"` — extracts member constraint IDs
- Regex in `parse_shift_tuple()`: `r"shift\((\w+),\s*(\w+),\s*(\w+),\s*(\w+)\)"` — parses shift tuple

**Unique Logic:**
- `compute_reform_threshold()` — context-walking threshold determination
- Coalition floor checks (COALITION_EPS_FLOOR=0.46, COALITION_SUPP_FLOOR=0.60, COALITION_VICTIM_THRESHOLD=3)
- Victim count extraction from Prolog .pl files

**Shared Boilerplate:** Config loading via shared/loader, sigmoid import.

**CLI Arguments:** None.

---

### 6. boundary_normality.py

**Purpose:** Statistical analysis of boundary population distributions. Tests whether MaxEnt probability distributions at type boundaries are normal, and fits alternative distributions.

**Data Sources:**
- `outputs/enriched_pipeline.json` — raw_maxent_probs per constraint
- Corpus data (via `shared.loader.load_all_data()`)
- `outputs/orbit_data.json` — coalition map from enriched pipeline

**Loading Method:** Uses `shared.loader` (load_json, load_all_data, ENRICHED_PIPELINE_JSON). Uses `shared.schemas.validate_enriched_pipeline` for validation. Imports `maxent_classify` from `tangled_decomposition.py` as fallback.

**Filters:** Constraints at type boundaries (claimed_type != rival_type). Grouping by boundary pair (claimed -> rival).

**Computations:**
- P(rival) distributions (pre-override)
- Boundary population grouping (claimed_type -> rival_type)
- Descriptive stats: n, mean, median, std, min, max, q25, q75, IQR
- Normality tests: Shapiro-Wilk, D'Agostino-Pearson, Anderson-Darling, Lilliefors
- Distribution fitting: normal, beta, uniform, Gaussian mixture (via AIC ranking)
- Skewness and kurtosis
- QQ table at deciles (expected vs observed)
- Override sub-population analysis (affected vs non-affected signatures)
- Coalition type x snare cluster cross-tabulation (chi-squared test of independence)
- Snare cluster threshold: P(snare) >= 0.5 -> high-snare, else low-snare

**Output:**
- `outputs/boundary_normality_data.json`
- `outputs/boundary_normality_report.md`

**Dependencies:**
- `import json, math, sys`
- `from collections import Counter, defaultdict`
- `from shared.loader import load_json, load_all_data, ENRICHED_PIPELINE_JSON`
- `from shared.constants import MAXENT_TYPES`
- `from shared.schemas import validate_enriched_pipeline`
- `from tangled_decomposition import maxent_classify`
- Optional: `scipy.stats`, `sklearn.mixture.GaussianMixture`, `statsmodels.stats.diagnostic.lilliefors`

**Unique Logic:**
- Pure-Python descriptive stats (no numpy dependency)
- `normality_tests()` — four statistical tests with graceful scipy fallback
- `fit_alternative_distributions()` — AIC-based distribution comparison
- `override_subpopulation_analysis()` — splits tangled_rope->snare by override-affected signatures
- `coalition_snare_crosstab()` — contingency table analysis
- `make_ascii_histogram()` — terminal-friendly visualization

**Shared Boilerplate:** Shared loader + constants + schemas, markdown table formatting.

**CLI Arguments:** None.

---

### 7. classification_audit.py

**Purpose:** Classification audit engine. Triages constraints into categories A+ through F3 based on extractiveness, suppression, theater ratio, and structural predicates. Identifies naturalization errors, theater-mountain conflicts, WHO suspects, and corpus bias.

**Data Sources:**
- `outputs/corpus_data.json` — constraint metrics and classifications
- `outputs/false_mountain_report.md` — **MARKDOWN-AS-DATA**: false mountain entries with severity
- `prolog/testsets/*.pl` — structural predicates (theater_ratio, pl_claim_value, template_version)
- `prolog/config.pl` — thresholds (via `shared.loader.read_config()`)

**Loading Method:** Uses `shared.loader.read_config()` for config. Own `json.load()` for corpus_data. Regex parsing for false_mountain_report.md. Glob for .pl files.

**Filters:** Different filters per triage category (A: mountains with high extraction; B: theater-mountain conflicts; C: exoneration candidates; D: WHO suspects; E: structural defects; F: corpus bias).

**Computations:**
- Category A+: Mountain + extractiveness > MOUNTAIN_MAX_EXTRACTIVENESS(0.25) + enforcement
- Category A: Mountain + extractiveness > 0.25 (without enforcement)
- Category B: Theater ratio > THEATER_CONFLICT_THRESHOLD(0.50) conflicts
- Category C: Legitimate gap exoneration
- Category D: WHO suspects requiring human review
- Categories E1-E4: Structural defects (missing predicates, template issues, etc.)
- Categories F1-F3: Corpus bias analysis (domain skew, beneficiary patterns)
- Perspectival type distribution tracking
- False mountain gap pattern classification

**Output:** `outputs/classification_audit_report.md`

**Dependencies:**
- `import argparse, json, re, sys`
- `from dataclasses import dataclass, field`
- `from datetime import datetime`
- `from typing import Dict, List, Optional, Tuple`
- `from shared.loader import read_config`

**Markdown-as-Data Parsing:**
- Regex: `r'###\s*\d+\.\s*False Mountain:\s*` `` `([^`]+)` `` `\s*\n(.*?)(?=###\s*\d+\.|$)'` — extracts false mountain entries
- Regex: `r'\*\*Severity:\*\*\s*` `` `(\w+)` `` `'` — extracts severity field

**Unique Logic:**
- Dataclass-based audit model (`AuditConstraint`, `AuditFinding`)
- Multi-category triage framework (A+ through F3)
- `supplement_from_pl_files()` — extracts theater_ratio, pl_claim_value, template_version from .pl source
- `_render_finding()` — per-finding markdown formatting

**Shared Boilerplate:** Config loading via shared/loader, markdown formatting.

**CLI Arguments:**
- `--corpus-data` (default: `outputs/corpus_data.json`)
- `--false-mountains` (default: `outputs/false_mountain_report.md`)
- `--testsets` (default: `prolog/testsets`)
- `--output` (default: `outputs/classification_audit_report.md`)

---

### 8. classification_confidence.py

**Purpose:** Classification confidence analysis. Computes per-constraint confidence metrics from MaxEnt distributions: confidence, margin, entropy, band, rival type. Compares override vs raw distributions.

**Data Sources:**
- `outputs/enriched_pipeline.json` — MaxEnt distributions per constraint
- Corpus data (via `shared.loader.load_all_data()`)

**Loading Method:** Uses `shared.loader` (load_json, load_all_data, ENRICHED_PIPELINE_JSON, OUTPUT_DIR). Uses `shared.schemas.validate_enriched_pipeline`. Imports `maxent_classify` and `apply_signature_override` from `tangled_decomposition.py`.

**Filters:** All constraints with MaxEnt distributions.

**Computations:**
- Confidence = P(claimed_type) from MaxEnt distribution
- Rival type = argmax excluding claimed_type
- Margin = confidence - rival_prob
- Shannon entropy (normalized, 0=certain, 1=max uncertainty)
- Band classification: "deep" (>0.8 AND margin>0.5), "moderate" (0.5-0.8), "borderline" (<0.5)
- Boundary pairs (claimed_type -> rival_type)
- Override impact analysis (band changes, top type changes)
- FNL tangled_rope cluster analysis
- Confidence-purity correlation
- Confidence-signature correlation

**Output:** `outputs/classification_confidence_report.md` (no JSON sidecar).

**Dependencies:**
- `import json, sys`
- `from collections import Counter, defaultdict`
- `from shared.loader import load_json, load_all_data, ENRICHED_PIPELINE_JSON, OUTPUT_DIR`
- `from shared.constants import MAXENT_TYPES, N_TYPES, shannon_entropy`
- `from shared.schemas import validate_enriched_pipeline`
- `from tangled_decomposition import maxent_classify, apply_signature_override`

**Unique Logic:**
- `compute_confidence_metrics()` — full confidence calculation per constraint
- `compute_override_comparison()` — with-override vs without-override comparison
- FNL cluster detection for tangled_rope constraints

**Shared Boilerplate:** Shared loader + constants + schemas, `make_ascii_histogram()`, markdown formatting.

**CLI Arguments:** None.

---

### 9. corpus_profile.py

**Purpose:** Generates a corpus profile as JSON (no markdown). Aggregates type distributions, signature distributions, signal base rates, verdict distribution, and anomalies. Upstream data source for other reporters.

**Data Sources:**
- `outputs/pipeline_output.json` — per-constraint array

**Loading Method:** Own `load_json()` via `pathlib.Path`. Does NOT use `shared/loader`.

**Filters:** None (processes entire corpus).

**Computations:**
- Type distributions: claimed_type and modal resolved type
- Signature distribution
- Signal base rates:
  - false_ci_rope percentage
  - h1_gt_0 percentage (H1 band > 0)
  - with_drift_events percentage
  - critical_drift percentage
  - broadly_stressed percentage (3+ critical drift types)
  - critical_extraction_accumulation percentage
- Verdict distribution (green/yellow/red)
- Subsystems availability tracking
- Anomalies: null_type_constraints and nonstandard_type_constraints

**Output:** `outputs/corpus_profile.json` (JSON only, no markdown).

**Dependencies:**
- `import json, sys`
- `from collections import Counter`

**Unique Logic:**
- `modal_type()` — returns most common non-null type across perspectives
- Signal base rate computation (false_ci_rope, broadly_stressed, etc.)
- Anomaly detection (null types, nonstandard types)

**Shared Boilerplate:** JSON loading.

**CLI Arguments:** None.

---

### 10. institutional_dissent_analysis.py

**Purpose:** Splits corpus into low-snare vs high-snare populations by P(snare) threshold. Compares populations across metrics, signatures, orbit structure, domains, and beneficiary/victim patterns. Ranks discriminant features.

**Data Sources:**
- `outputs/enriched_pipeline.json` — MaxEnt distributions, metrics
- `outputs/corpus_data.json` — structural predicates, domain
- `outputs/orbit_data.json` — orbit signatures

**Loading Method:** Uses `shared.loader` (load_json, ENRICHED_PIPELINE_JSON, CORPUS_JSON, ORBIT_JSON). Uses `shared.schemas.validate_enriched_pipeline`.

**Filters:** Population split: P(snare) >= 0.5 -> high-snare, < 0.5 -> low-snare.

**Computations:**
- Population split by raw P(snare) threshold (0.5)
- Continuous metrics comparison: base_extractiveness, suppression, theater_ratio, purity_score, confidence, confidence_margin, tangled_psi
- Mann-Whitney U test with rank-biserial correlation
- Chi-squared test with Cramer's V (signature comparison)
- H1 (observer disagreement) = count of distinct types across perspectives
- Orbit signature comparison and perspective patterns
- Domain distribution comparison
- Beneficiary-victim overlap detection
- Discriminant ranking by effect size
- Spot-check extremes (10 lowest and highest P(snare))

**Output:**
- `outputs/institutional_dissent_report.md`
- `outputs/institutional_dissent_data.json`

**Dependencies:**
- `import json, math, sys`
- `from collections import Counter, defaultdict`
- Optional: `scipy.stats`
- `from shared.loader import load_json, ENRICHED_PIPELINE_JSON, CORPUS_JSON, ORBIT_JSON`
- `from shared.schemas import validate_enriched_pipeline`

**Unique Logic:**
- `rank_biserial()` — rank-biserial correlation from Mann-Whitney U
- `compute_h1()` — observer disagreement metric
- `beneficiary_victim_analysis()` — overlap detection
- `rank_discriminants()` — feature ranking by effect size

**Shared Boilerplate:** Shared loader + schemas, pure-Python `descriptive_stats()`, markdown table formatting.

**CLI Arguments:** None.

---

### 11. conflict_map.py

**Purpose:** Perspectival gap analysis by domain. Classifies shift patterns (analytical vs powerless disagreements) and aggregates conflict profiles per domain.

**Data Sources:**
- `outputs/corpus_data.json` — constraint metrics and classifications
- `outputs/fingerprint_report.md` — **MARKDOWN-AS-DATA**: shift pattern families

**Loading Method:** Own `json.load()` and regex parsing. Does NOT use `shared/loader`.

**Filters:** Constraints with shift patterns (perspectival disagreement) from fingerprint_report.md, plus static classifications from corpus_data.json.

**Computations:**
- Severity ordering: mountain(0), rope(1), scaffold(2), piton(3), tangled_rope(4), snare(5), unknown(-1)
- Shift classification:
  - consensus (same type both perspectives)
  - coordination_washing (analyst sees benign, powerless sees extractive)
  - severity_amplification (both see extraction, powerless sees worse)
  - protective_framing (analyst sees worse than powerless)
  - analytical_blind, powerless_blind, both_unknown
- Severity delta: absolute gap magnitude between types
- Domain aggregation: shift class distribution, pair distribution per domain
- Source tracking (fingerprint vs corpus_static)

**Output:** `outputs/conflict_map.md`

**Dependencies:**
- `import json, re, sys`
- `from collections import defaultdict, Counter`

**Markdown-as-Data Parsing:**
- Regex: `r'### ` `` `shift\((\w+), (\w+), (\w+), (\w+)\)` `` `'` — extracts 4-tuple shift patterns
- Regex: `r'- ` `` `'` — extracts member constraint IDs

**Unique Logic:**
- `classify_shift()` — perspectival gap direction classification
- `severity_delta()` — numeric gap magnitude between two types
- Domain-level conflict profiling

**Shared Boilerplate:** JSON loading, markdown table formatting.

**CLI Arguments:** None.

---

### 12. variance_analyzer.py

**Purpose:** Index variance analysis. Determines whether constraints cluster naturally or whether index configurations explain all variance.

**Data Sources:**
- `outputs/corpus_data.json` — variance_ratio, index_configs, types_produced per constraint

**Loading Method:** Own `json.load()` in class constructor. Does NOT use `shared/loader`.

**Filters:** Analyzes all constraints. Highlights high-variance (>0.5), suspicious stability (configs >= 5 AND variance < 0.3).

**Computations:**
- Variance distribution buckets: 1.0 (stable), 0.7-0.9, 0.5-0.6, 0.3-0.4, <0.3, null
- Domain-level variance statistics (average variance, high variance percentage)
- High variance examples (top 10 most volatile constraints)
- Suspicious stability (many configs, low variance — possible modeling issues)
- Data completeness percentages (classifications, variance_ratio, domain)

**Output:** `outputs/variance_analysis.md`

**Dependencies:**
- `import json, argparse`
- `from collections import defaultdict, Counter`

**Unique Logic:**
- `find_suspicious_stability()` — detects constraints that should vary but don't
- Variance bucket analysis with automatic insight generation

**Shared Boilerplate:** JSON loading, class-based report pattern, markdown table formatting.

**CLI Arguments:**
- `--corpus-data` (default: `../outputs/corpus_data.json`)
- `--output` (default: `../outputs/variance_analysis.md`)

---

### 13. pattern_miner.py

**Purpose:** Structural pattern mining. Identifies structural twins, candidate categories, hybrid patterns, and transition markers.

**Data Sources:**
- `outputs/corpus_data.json` — constraint metrics and classifications

**Loading Method:** Own `json.load()` in class constructor. Does NOT use `shared/loader`.

**Filters:** Various heuristic thresholds per candidate category.

**Computations:**
- Structural signatures: metric-based grouping key
- Structural twins: same signature, different types
- Candidate categories with heuristic thresholds:
  - tangled_rope: epsilon >= 0.6, suppression >= 0.6
  - piton: suppression >= 0.7
  - scaffold: 0.3 <= epsilon, suppression <= 0.6
  - wings: epsilon <= 0.3, suppression <= 0.3
- Hybrid patterns: epsilon >= 0.5 AND suppression >= 0.5
- Transition markers: mid-range metric counts
- Prioritized recommendations for new categories

**Output:** `outputs/pattern_mining.md`

**Dependencies:**
- `import json, argparse`
- `from collections import defaultdict, Counter`

**Unique Logic:**
- `structural_signature()` — creates binned metric signatures for grouping
- `identify_candidate_categories()` — heuristic candidate detection
- `generate_recommendations()` — priority-ranked recommendations

**Shared Boilerplate:** JSON loading, class-based report pattern, markdown table formatting.

**CLI Arguments:**
- `--corpus-data` (default: `../outputs/corpus_data.json`)
- `--output` (default: `../outputs/pattern_mining.md`)

---

### 14. boolean_independence.py

**Purpose:** Boolean feature independence analysis. Tests whether 6 boolean features are redundant with type classification or carry independent information.

**Data Sources:**
- `outputs/enriched_pipeline.json` — MaxEnt distributions
- `outputs/corpus_data.json` — boolean feature values

**Loading Method:** Uses `shared.loader` (load_json, ENRICHED_PIPELINE_JSON, CORPUS_JSON). Uses `shared.schemas.validate_enriched_pipeline`. Uses `shared.constants.MAXENT_TYPES`.

**Filters:** All constraints with boolean feature data.

**Computations:**
- 6 boolean features: emerges_naturally, requires_active_enforcement, has_coordination_function, has_asymmetric_extraction, natural_law_without_beneficiary, is_constructed
- Within-type proportions: P(B=true | type=T)
- Independence score: mean of min(P, 1-P) across types (0=redundant, 0.5=maximally independent)
- Chi-squared test of independence
- Cramer's V
- Mutual information / normalized MI
- Mann-Whitney U for metric comparisons (B=true vs B=false)
- Phi coefficient for pairwise boolean correlation
- Within-type quadrant analysis (2x2 grid for two features in one type)
- Orbit family classification (uniform, binary, trio, full_dispersion)
- H1 (observer disagreement) from perspectives
- Confidence band analysis (borderline rate)

**Output:**
- `outputs/boolean_independence_report.md`
- `outputs/boolean_independence_data.json`

**Dependencies:**
- `import json, math, sys`
- `from collections import Counter, defaultdict`
- Optional: `scipy.stats`
- `from shared.loader import load_json, ENRICHED_PIPELINE_JSON, CORPUS_JSON`
- `from shared.constants import MAXENT_TYPES`
- `from shared.schemas import validate_enriched_pipeline`

**Unique Logic:**
- `independence_score()` — mean of min(P, 1-P) metric
- `mutual_information()` — MI(B, type) / H(B)
- `pairwise_correlation()` — phi coefficient between boolean pairs
- `within_type_quadrant_analysis()` — 2x2 feature co-occurrence

**Shared Boilerplate:** Shared loader + constants + schemas, pure-Python stats, markdown table formatting.

**CLI Arguments:** None.

---

### 15. tangled_decomposition.py

**Purpose:** Tangled rope fiber decomposition. Full MaxEnt classifier replication in Python. Computes per-constraint probability distributions, psi metric, band classification, and coalition types. **Critical dependency**: exports `maxent_classify()` and `apply_signature_override()` used by other scripts.

**Data Sources:**
- `outputs/enriched_pipeline.json` — per-constraint metrics
- `outputs/pipeline_output.json` — maxent_probs for cross-validation
- `outputs/orbit_data.json` — orbit contexts for coalition classification
- `outputs/maxent_report.md` — 182 known distributions for validation (markdown-as-data, read-only)
- `prolog/config.pl` — MaxEnt parameters (via `shared.loader.read_config()`)

**Loading Method:** Uses `shared.loader` (load_json, read_config, load_all_data, ENRICHED_PIPELINE_JSON, OUTPUT_DIR). Uses `shared.constants` (MAXENT_TYPES, N_TYPES, BOOLEAN_SPECS, PSI_ROPE_LEANING, PSI_SNARE_LEANING, compute_psi, classify_band, classify_coalition). Uses `shared.schemas.validate_enriched_pipeline`.

**Filters:** All constraints (tangled_rope band analysis focused on tangled_rope subset).

**Computations:**
- Default type profiles: mean/std per metric per type (extractiveness, suppression, theater_ratio)
- Corpus-frequency priors with floor 0.001
- Gaussian log-likelihood per metric
- Boolean log-likelihood contributions
- Log-sum-exp normalization to posterior probabilities
- Signature-based overrides (matching Prolog maxent_override/3)
- Psi: P(snare) / (P(rope) + P(snare) + 0.001)
- Band classification: rope_leaning (psi < 0.3), genuinely_tangled (0.3-0.7), snare_leaning (psi > 0.7)
- Coalition types: uniform_tangled, institutional_dissent, analytical_dissent, split_field, other
- Validation against 182 known distributions (tolerance 0.05)

**Output:** `outputs/tangled_rope_decomposition_report.md` (no JSON sidecar — data lives in pipeline_output.json).

**Dependencies:**
- `import argparse, json, math, sys`
- `from collections import Counter, defaultdict`
- `from shared.loader import load_json, read_config, load_all_data, ENRICHED_PIPELINE_JSON, OUTPUT_DIR`
- `from shared.constants import MAXENT_TYPES, N_TYPES, BOOLEAN_SPECS, PSI_ROPE_LEANING, PSI_SNARE_LEANING, compute_psi, classify_band, classify_coalition`
- `from shared.schemas import validate_enriched_pipeline`

**Exported Functions (used by other scripts):**
- `maxent_classify(constraint, profiles, priors, config)` — full MaxEnt classification
- `apply_signature_override(dist, signature)` — signature-based override

**Unique Logic:**
- Complete MaxEnt classifier replication (`gaussian_ll`, `boolean_ll`, `log_sum_exp_normalize`)
- `compute_profiles()` — empirical mean/std from corpus
- `compute_priors()` — corpus-frequency priors
- `parse_maxent_report()` — parses 182 known distributions from maxent_report.md
- `validate_maxent()` — cross-validation with tolerance 0.05

**Shared Boilerplate:** Full shared infrastructure (loader + constants + schemas), `make_ascii_histogram()`, markdown formatting.

**CLI Arguments:**
- `--validate-only` — runs MaxEnt validation only, no decomposition

---

### 16. powerless_blind_diagnostic.py

**Purpose:** Tests all 6 classification gates at the powerless context. Identifies constraints with powerless-blind patterns and classifies subpopulations.

**Data Sources:**
- `outputs/fingerprint_report.md` — **MARKDOWN-AS-DATA**: shift pattern families
- `outputs/corpus_data.json` — constraint metrics
- `prolog/testsets/*.pl` — structural predicates, victim counts
- `prolog/probsets/*.pl` — additional predicates
- `prolog/config.pl` — thresholds (via `shared.loader.read_config()`)

**Loading Method:** Uses `shared.loader.read_config()` for config. Own `json.load()` for corpus_data. Regex parsing for fingerprint_report.md. Glob for .pl files. Also imports `sigmoid.POWER_MODIFIERS`.

**Filters:** Constraints with powerless-blind shift patterns (unknown -> non-unknown transitions).

**Computations:**
- Chi formula: `epsilon * pi(power) * sigma(scope)`
- Coalition upgrade: if `eps >= 0.46 AND supp >= 0.60 AND victims >= 3`, applies organized pi (0.4)
- Gate testing for all 6 types at powerless context
- Theater ratio extraction from .pl files
- Victim counts per constraint
- Power modifiers via sigmoid function
- Scope modifiers: local=0.8, national=1.0, global=1.2
- Subpopulation classification: coalition_chi_reduction, dead_zone, missing_structural, wings_candidate, high_chi_gap, no_metrics, other

**Output:** Console only (stdout).

**Dependencies:**
- `import json, sys, re`
- `from collections import defaultdict, Counter`
- `from sigmoid import POWER_MODIFIERS`
- `from shared.loader import read_config`

**Markdown-as-Data Parsing:** Same regex patterns as reform_threshold_report.py (#5):
- Shift pattern header regex for `shift(...)` entries
- Member constraint ID regex
- `parse_shift_tuple()` regex

**Unique Logic:**
- `test_powerless_gates()` — tests all 6 gates at powerless
- `classify_subpopulation()` — 7-category subpopulation classification
- `find_powerless_blind()` — identifies unknown->non-unknown transitions

**Shared Boilerplate:** Config loading via shared/loader, sigmoid import, markdown parsing (shared with #5).

**CLI Arguments:** None.

---

### 17. red_spot_check.py

**Purpose:** Post-calibration spot-check of red-verdict constraints. Extracts tension sources, convergent rejections, and absorbed patterns.

**Data Sources:**
- `outputs/pipeline_output.json` — per-constraint diagnostic verdicts

**Loading Method:** Own `json.load()` via `pathlib.Path`. Does NOT use `shared/loader`.

**Filters:** Constraints with `diagnostic_verdict.verdict == "red"`.

**Computations:**
- Modal resolved type from perspectives (mode across all perspective types)
- Tension subsystems and signal details
- Convergent rejections: alternative types and rejecting subsystems
- Expected conflicts (absorbed patterns)
- Verdict distribution across corpus
- Tension and rejection counts per constraint

**Output:** Console only (stdout).

**Dependencies:**
- `import json, sys`

**Unique Logic:**
- `extract_reds()` — diagnostic detail extraction for red constraints
- Modal type computation from perspective dict

**Shared Boilerplate:** JSON loading, formatted text output.

**CLI Arguments:** None.

---

### 18. sufficiency_tester.py

**Purpose:** Index sufficiency testing. Detects index collisions (same index configuration producing multiple types) and classifies them. Produces a verdict on whether the index system is sufficient.

**Data Sources:**
- `outputs/corpus_data.json` — index configurations, classifications
- `outputs/pipeline_output.json` — enrichment data (signatures, purity, omegas)

**Loading Method:** Own `json.load()` in class constructor. Does NOT use `shared/loader`.

**Filters:** Constraints with index collisions (same index -> multiple types). Mountain-filtered stability anomalies.

**Computations:**
- Index configurations: (agent_power, time_horizon, exit_options, spatial_scope)
- Collision detection: same index -> multiple types
- Collision classification:
  - Classification failures (containing unknown/naturalized)
  - Expected perspectival variance (agent_power explains the difference)
  - Genuine collisions (unexplained)
- Domain sufficiency: variance ratio per domain, normalized domain labels
- Stability anomalies: non-mountains with 6+ configs producing same type
- Four-rate verdict system:
  - Classification failure rate
  - Expected variance rate
  - Genuine collision rate
  - Non-mountain anomaly rate
- Overall verdict: SUFFICIENT / MIXED / INSUFFICIENT

**Output:**
- `outputs/index_sufficiency.md`
- `outputs/index_sufficiency.json`

**Dependencies:**
- `import json, argparse`
- `from collections import defaultdict, Counter`

**Unique Logic:**
- `_normalize_domains()` — canonical domain label normalization
- `classify_collisions()` — 3-category collision classification
- `_is_perspectival()` — checks if agent_power profile differences explain collision
- `find_stability_anomalies()` — mountain-filtered stability detection
- `_calculate_verdict()` — 4-rate verdict system

**Shared Boilerplate:** JSON loading, class-based report pattern, markdown table formatting.

**CLI Arguments:**
- `--corpus-data` (default: `../outputs/corpus_data.json`)
- `--pipeline-data` (default: `../outputs/pipeline_output.json`)
- `--output` (default: `../outputs/index_sufficiency.md`)
- `--json-output` (default: `../outputs/index_sufficiency.json`)

---

## Shared Pattern

All 18 scripts follow the same high-level pattern:

1. **Load** — Read one or more JSON (or markdown) data sources
2. **Filter/Transform** — Select a subset of constraints or aggregate by grouping key
3. **Compute** — Calculate metrics, run statistical tests, or classify
4. **Format** — Render results as markdown tables and narrative sections
5. **Write** — Output to file or stdout

Common formatting idioms:
- Markdown tables with `|` delimiters, always preceded by a header separator row
- Section headers at `##` level
- Inline backtick-wrapped constraint IDs
- f-string formatting with fixed-width columns (e.g., `{value:6.1f}%`)
- ASCII histograms (boundary_normality, classification_confidence, tangled_decomposition)

---

## Loading Variants

### Variant A: `shared.loader` functions (10 scripts)

Scripts: omega_enricher (#4), reform_threshold_report (#5), boundary_normality (#6), classification_audit (#7), classification_confidence (#8), institutional_dissent_analysis (#10), boolean_independence (#14), tangled_decomposition (#15), powerless_blind_diagnostic (#16).

Sub-variants:
- **`load_json(path, label, schema)`** — general JSON loading with optional schema validation
- **`load_all_data()`** — merged pipeline+corpus+orbit per-constraint dict
- **`read_config()`** — reads `param/2` from `prolog/config.pl`
- Path constants: `ENRICHED_PIPELINE_JSON`, `CORPUS_JSON`, `ORBIT_JSON`, `PIPELINE_JSON`, `OUTPUT_DIR`

### Variant B: Own `load_json()` / `load_pipeline_data()` (5 scripts)

Scripts: meta_reporter (#1), type_reporter (#2), omega_reporter (#3), corpus_profile (#9), red_spot_check (#17).

Pattern: Direct `pathlib.Path` + `json.load()` in a local helper function or class constructor.

### Variant C: Class constructor loading (3 scripts)

Scripts: variance_analyzer (#12), pattern_miner (#13), sufficiency_tester (#18).

Pattern: `__init__(self, path)` opens and loads JSON. CLI arguments provide the path with relative defaults (`../outputs/...`).

### Variant D: Markdown regex parsing (4 scripts)

Scripts: reform_threshold_report (#5), classification_audit (#7), conflict_map (#11), powerless_blind_diagnostic (#16).

These scripts parse `fingerprint_report.md` or `false_mountain_report.md` as data sources. See [Dependency Graph: Markdown-as-Data Chain](#markdown-as-data-chain) for details.

---

## Groupings

### Group 1: Could share a JSON-filter-and-template pattern

These scripts read JSON, filter/aggregate, and produce markdown. They have no exotic data sources or cross-script imports and could be expressed as `(data_source, filter_fn, template)` triples:

| Script | Data Source | Filter |
|--------|-----------|--------|
| variance_analyzer (#12) | corpus_data.json | variance_ratio buckets |
| pattern_miner (#13) | corpus_data.json | heuristic thresholds |
| red_spot_check (#17) | pipeline_output.json | verdict == "red" |
| sufficiency_tester (#18) | corpus_data.json + pipeline_output.json | index collisions |
| omega_reporter (#3) | pipeline_output.json | omega extraction |
| corpus_profile (#9) | pipeline_output.json | entire corpus |

### Group 2: Shared computation layer (MaxEnt / tangled_decomposition)

These scripts share `maxent_classify()` and/or constants from `shared/constants.py`. Consolidation must preserve the exported function interface:

| Script | Imports From |
|--------|-------------|
| tangled_decomposition (#15) | shared/loader + shared/constants + shared/schemas (defines maxent_classify) |
| boundary_normality (#6) | tangled_decomposition.maxent_classify, shared/constants.MAXENT_TYPES |
| classification_confidence (#8) | tangled_decomposition.{maxent_classify, apply_signature_override}, shared/constants |
| boolean_independence (#14) | shared/constants.MAXENT_TYPES |

### Group 3: Enriched pipeline consumers

These scripts read `enriched_pipeline.json` and use schema validation:

| Script | Additional Sources |
|--------|--------------------|
| boundary_normality (#6) | corpus data via load_all_data() |
| classification_confidence (#8) | corpus data via load_all_data() |
| institutional_dissent_analysis (#10) | corpus_data.json, orbit_data.json |
| boolean_independence (#14) | corpus_data.json |
| tangled_decomposition (#15) | pipeline_output.json, orbit_data.json |

### Group 4: Markdown-as-data consumers (need custom query functions)

These scripts cannot become pure JSON-filter-template — they need a markdown parser or the upstream data exposed as JSON first:

| Script | Markdown Source | What They Extract |
|--------|----------------|-------------------|
| reform_threshold_report (#5) | fingerprint_report.md | shift pattern families + member IDs |
| classification_audit (#7) | false_mountain_report.md | false mountain entries + severity |
| conflict_map (#11) | fingerprint_report.md | shift patterns (4-tuple) + member IDs |
| powerless_blind_diagnostic (#16) | fingerprint_report.md | shift pattern families + member IDs |

### Group 5: Sigmoid/config consumers

These scripts use `sigmoid.POWER_MODIFIERS` and `shared.loader.read_config()`:

| Script | Config Usage |
|--------|-------------|
| reform_threshold_report (#5) | power modifiers, coalition thresholds |
| powerless_blind_diagnostic (#16) | power modifiers, coalition thresholds, gate thresholds |
| tangled_decomposition (#15) | MaxEnt parameters |
| classification_audit (#7) | extractiveness/suppression thresholds |

### Group 6: Multi-source reporters with custom computation

These scripts are complex enough to need custom query functions even after consolidation:

| Script | Why Custom |
|--------|-----------|
| meta_reporter (#1) | Parses output.txt (non-JSON), complex dashboard |
| type_reporter (#2) | 3 report families, 7 types, orbit integration |
| omega_enricher (#4) | Cross-references 3 data sources, composite scoring |
| institutional_dissent_analysis (#10) | Statistical tests, population splitting |

---

## Dependency Graph

### Pipeline Data Flow

```
prolog/ (Prolog engine)
  |
  v
pipeline_output.json  ---------> enriched_pipeline.json (via enrich_pipeline_json.py)
  |                                    |
  |                                    +---> boundary_normality.py (#6)
  |                                    +---> classification_confidence.py (#8)
  |                                    +---> institutional_dissent_analysis.py (#10)
  |                                    +---> boolean_independence.py (#14)
  |                                    +---> tangled_decomposition.py (#15)
  |
  +---> meta_reporter.py (#1)
  +---> type_reporter.py (#2)
  +---> omega_reporter.py (#3)
  +---> corpus_profile.py (#9)
  +---> red_spot_check.py (#17)
  +---> sufficiency_tester.py (#18)

corpus_data.json (via extract_corpus_data.py)
  |
  +---> omega_enricher.py (#4)
  +---> reform_threshold_report.py (#5)
  +---> classification_audit.py (#7)
  +---> conflict_map.py (#11)
  +---> variance_analyzer.py (#12)
  +---> pattern_miner.py (#13)
  +---> boolean_independence.py (#14)
  +---> institutional_dissent_analysis.py (#10)
  +---> powerless_blind_diagnostic.py (#16)
  +---> sufficiency_tester.py (#18)

orbit_data.json
  |
  +---> meta_reporter.py (#1)
  +---> type_reporter.py (#2) (via orbit_utils)
  +---> omega_enricher.py (#4) (via orbit_utils)
  +---> institutional_dissent_analysis.py (#10)
  +---> tangled_decomposition.py (#15)
```

### Report-to-Report Consumption

```
omega_reporter.py (#3)
  |  writes omega_data.json
  v
omega_enricher.py (#4)
  |  reads omega_data.json
  v
enriched_omega_data.json
```

```
type_reporter.py (#2)
  |  writes false_mountain_report.md
  v
classification_audit.py (#7)
  |  reads false_mountain_report.md
```

### Cross-Script Python Imports

```
tangled_decomposition.py (#15)
  |  exports maxent_classify(), apply_signature_override()
  +---> boundary_normality.py (#6)
  +---> classification_confidence.py (#8)

orbit_utils.py
  |  exports load_orbit_data(), get_orbit_signature(), format_orbit_signature()
  +---> type_reporter.py (#2)
  +---> omega_enricher.py (#4)

sigmoid.py
  |  exports POWER_MODIFIERS, sigmoid_f(), power_modifier()
  +---> reform_threshold_report.py (#5)
  +---> powerless_blind_diagnostic.py (#16)
```

### Markdown-as-Data Chain

Four scripts parse **fingerprint_report.md** (generated by `prolog/fingerprint_report.pl`):

| Script | Extraction Method | What's Extracted |
|--------|------------------|-----------------|
| reform_threshold_report (#5) | `parse_fingerprint_report()` | Shift pattern families: header `### \`shift(...)\` ... N constraints`, member IDs under `- \`constraint_id\`` |
| conflict_map (#11) | `parse_shift_patterns()` | 4-tuple shift patterns: `shift(powerless, moderate, institutional, analytical)`, member IDs |
| powerless_blind_diagnostic (#16) | `parse_fingerprint_report()` | Same as #5 — shift families and member IDs |

One script parses **false_mountain_report.md** (generated by `type_reporter.py`):

| Script | Extraction Method | What's Extracted |
|--------|------------------|-----------------|
| classification_audit (#7) | `load_false_mountains()` | False mountain entries: `### N. False Mountain: \`id\``, severity field: `**Severity:** \`level\`` |

**Consolidation implication:** These markdown-as-data dependencies cannot become simple JSON-filter-template patterns. Options:
1. Expose fingerprint data as JSON upstream (preferred)
2. Preserve the markdown parsing step as a dedicated loader function

---

## Config Access

Six scripts read `prolog/config.pl` via `shared.loader.read_config()`:

| Script | Parameters Used |
|--------|----------------|
| reform_threshold_report (#5) | Sigmoid parameters, coalition thresholds |
| classification_audit (#7) | MOUNTAIN_MAX_EXTRACTIVENESS, SNARE_MIN_EXTRACTIVENESS, THEATER_NATURALIZATION_THRESHOLD, THEATER_CONFLICT_THRESHOLD, etc. |
| powerless_blind_diagnostic (#16) | Sigmoid parameters, gate thresholds, coalition thresholds |
| tangled_decomposition (#15) | MaxEnt parameters (profile defaults, boolean weights) |

Additionally, `sigmoid.py` reads config.pl directly at import time to compute `POWER_MODIFIERS`.

The `read_config()` function parses `param(name, value).` facts via regex `r'param\(\s*(\w+)\s*,\s*(-?[\d.]+)\s*\)'` and returns `{str: float}`.

---

## Completeness Check

### Included (18 report generators)

| # | Script | Confirmed |
|---|--------|-----------|
| 1 | meta_reporter.py | Yes |
| 2 | type_reporter.py | Yes |
| 3 | omega_reporter.py | Yes |
| 4 | omega_enricher.py | Yes |
| 5 | reform_threshold_report.py | Yes |
| 6 | boundary_normality.py | Yes |
| 7 | classification_audit.py | Yes |
| 8 | classification_confidence.py | Yes |
| 9 | corpus_profile.py | Yes |
| 10 | institutional_dissent_analysis.py | Yes |
| 11 | conflict_map.py | Yes |
| 12 | variance_analyzer.py | Yes |
| 13 | pattern_miner.py | Yes |
| 14 | boolean_independence.py | Yes |
| 15 | tangled_decomposition.py | Yes |
| 16 | powerless_blind_diagnostic.py | Yes |
| 17 | red_spot_check.py | Yes |
| 18 | sufficiency_tester.py | Yes |

### Excluded (26 scripts + shared/)

| Script | Reason |
|--------|--------|
| `shared/loader.py` | Utility module |
| `shared/constants.py` | Utility module |
| `shared/schemas.py` | Utility module |
| `shared/__init__.py` | Package init |
| `enhanced_report.py` | Excluded per user instructions |
| `run_pipeline.py` | Orchestrator, excluded per user instructions |
| `generate_constraint_pl.py` | Excluded per user instructions |
| `perspective_analysis.py` | Different data flow (reads experiment_log.json) |
| `enrich_pipeline_json.py` | Data transformation, not report generation |
| `extract_corpus_data.py` | Data transformation, not report generation |
| `duplicate_checker.py` | Reads Prolog testsets, not pipeline JSON |
| `find_u2_exemplars.py` | Reads Prolog testsets, not pipeline JSON |
| `domain_priors.py` | Reads Prolog testsets, not pipeline JSON |
| `domain_priors_expander.py` | Reads Prolog testsets, not pipeline JSON |
| `python_test_suite.py` | Builds Prolog test suites |
| `python_gap_suite.py` | Builds Prolog test suites |
| `normalize_orbit_ids.py` | Data normalization |
| `promote_human_readable.py` | Data normalization |
| `promote_topic_domain.py` | Data normalization |
| `prolog_cleaner.py` | Prolog maintenance |
| `golden_file_check.py` | Regression test |
| `regenerate_stories.py` | Story generation |
| `sigmoid.py` | Shared utility (power modifiers) |
| `linter.py` | Shared utility |
| `orbit_utils.py` | Shared utility |
| `query.py` | CLI query tool |
| `config_sensitivity_sweep.py` | Parameter sweep tool |

### Output Path Cross-Reference

| Output File | Producing Script | Consumed By |
|-------------|-----------------|-------------|
| `pipeline_output.json` | Prolog engine / run_pipeline.py | #1, #2, #3, #9, #15, #17, #18 |
| `enriched_pipeline.json` | enrich_pipeline_json.py | #6, #8, #10, #14, #15 |
| `corpus_data.json` | extract_corpus_data.py | #4, #5, #7, #10, #11, #12, #13, #14, #16, #18 |
| `orbit_data.json` | prolog/orbit_report.pl | #1, #2, #4, #10, #15 |
| `output.txt` | Prolog test runner | #1 |
| `fingerprint_report.md` | prolog/fingerprint_report.pl | #5, #11, #16 |
| `false_mountain_report.md` | type_reporter.py (#2) | #7 |
| `omega_data.json` | omega_reporter.py (#3) | #4 |
| `maxent_report.md` | Prolog engine | #15 (validation only) |
