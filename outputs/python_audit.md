# Python Codebase Audit

Generated: 2026-02-17

Scope: All 27 `.py` files in `python/`. Cross-referenced against `Makefile` pipeline phases and `prolog/config.pl` parameter inventory.

---

## Section 1: Summary Table

| # | File | Purpose | Pipeline Phase | Data Sources | Issues |
|---|------|---------|----------------|--------------|--------|
| 1 | `classification_audit.py` | Audit constraint classifications against type-gate thresholds | NOT IN MAKEFILE | `corpus_data.json`, `false_mountain_report.md`, `.pl` files | 3 |
| 2 | `conflict_map.py` | Map severity ordering + classify type shifts | NOT IN MAKEFILE | `corpus_data.json`, `fingerprint_report.md` | 1 |
| 3 | `config_sensitivity_sweep.py` | Sweep config.pl params via swipl subprocess | NOT IN MAKEFILE | `config.pl` (regex), `swipl` subprocess | 2 |
| 4 | `domain_priors.py` | Generate `domain_registry.pl` from testset files | PREP (line 152) | `.pl` files (regex), `config.pl` (regex) | 1 |
| 5 | `domain_priors_expander.py` | Generate expanded domain priors | NOT IN MAKEFILE | `.pl` files (regex), `config.pl` (regex) | 2 |
| 6 | `duplicate_checker.py` | Detect duplicate constraint IDs and module names | CHECK (line 133) | `.pl` files (regex) | 0 |
| 7 | `enhanced_report.py` | Two-stream report: swipl live + Python splice | NOT IN MAKEFILE | `swipl` subprocess, 7 JSON/md files | 2 |
| 8 | `extract_corpus_data.py` | Build `corpus_data.json` from pipeline output | POST (line 384) | `pipeline_output.json`, `orbit_data.json` | 0 |
| 9 | `false_mountain_reporter.py` | Generate false mountain diagnostic report | POST (line 365) | `pipeline_output.json`, `orbit_data.json` | 1 |
| 10 | `linter.py` | Structural linter for `.pl` testset files | CHECK (line 131) | `.pl` files (regex), `config.pl` (regex) | 0 |
| 11 | `meta_reporter.py` | Pipeline health dashboard | POST (line 432) | `pipeline_output.json`, `output.txt`, `orbit_data.json` | 0 |
| 12 | `normalize_orbit_ids.py` | Normalize `orbit_data.json` keys to canonical IDs | POST (line 322) | `orbit_data.json`, `.pl` files (regex) | 0 |
| 13 | `omega_enricher.py` | Enrich omega data with severity + orbit context | POST (line 419) | `omega_data.json`, `corpus_data.json`, `orbit_data.json` | 2 |
| 14 | `omega_reporter.py` | Extract omega records into report + JSON | POST (line 375) | `pipeline_output.json` | 0 |
| 15 | `orbit_utils.py` | Shared loader for `orbit_data.json` | LIBRARY | `orbit_data.json` | 0 |
| 16 | `pattern_miner.py` | Structural pattern mining across corpus | POST (line 401) | `corpus_data.json` | 1 |
| 17 | `powerless_blind_diagnostic.py` | Diagnose powerless-perspective blind spots | NOT IN MAKEFILE | `.pl` files (regex), `fingerprint_report.md`, `corpus_data.json` | 3 |
| 18 | `prolog_cleaner.py` | Fix AI-generated Prolog artifacts | UTILITY (line 139) | `.pl` files (direct write) | 0 |
| 19 | `promote_human_readable.py` | Migrate human_readable from comments to Prolog facts | NOT IN MAKEFILE | `.pl` files (direct write) | 1 |
| 20 | `python_test_suite.py` | Generate `validation_suite.pl` from testsets | PREP (line 153) | `.pl` files (regex) | 0 |
| 21 | `reform_threshold_report.py` | Report on reform threshold accessibility | NOT IN MAKEFILE | `.pl` files (regex), `fingerprint_report.md`, `corpus_data.json` | 3 |
| 22 | `regenerate_stories.py` | Batch-regenerate testsets via Gemini 2.5 Pro API | NOT IN MAKEFILE | `linter` (import), `duplicate_checker` (import), Gemini API | 1 |
| 23 | `sigmoid.py` | Shared sigmoid power modifier f(d) | LIBRARY | None (pure computation) | 1 |
| 24 | `sufficiency_tester.py` | Test whether 4 indices explain constraint variance | POST (line 407) | `corpus_data.json`, `pipeline_output.json` | 1 |
| 25 | `tangled_rope_reporter.py` | Generate tangled rope diagnostic report | POST (line 360) | `pipeline_output.json`, `orbit_data.json` | 1 |
| 26 | `type_reporter.py` | Parameterized reporter for 5 constraint types | POST (line 334) | `pipeline_output.json`, `orbit_data.json` | 0 |
| 27 | `variance_analyzer.py` | Analyze classification variance across perspectives | POST (line 395) | `corpus_data.json` | 0 |

**Totals:** 15 scripts in Makefile pipeline (including 2 libraries), 12 scripts standalone/dead. 26 issues across 14 files.

---

## Section 2: Detailed Per-File Findings

### 1. `classification_audit.py`

- **Purpose:** Audits every constraint against type-gate metric thresholds and reports mismatches, false mountains, and WHO suspects.
- **Data sources:** `corpus_data.json`, `false_mountain_report.md` (regex-parsed), `.pl` testset files (regex for `base_extractiveness`, `suppression_score`)
- **Pipeline phase:** NOT IN MAKEFILE — standalone diagnostic
- **Issues:**
  1. **Hardcoded threshold divergence:** `MOUNTAIN_MAX_EXTRACTIVENESS=0.15` (line 28) vs `config.pl:mountain_extractiveness_max=0.25` (config line 148). Value mismatch — audit uses a stricter gate than the pipeline itself.
  2. **Hardcoded threshold divergence:** `TANGLED_MIN_EXTRACTIVENESS=0.50` (line 36) vs `config.pl:tangled_rope_extraction_floor=0.16` (config line 160) and `tangled_rope_epsilon_floor=0.30` (config line 192). Neither config param matches 0.50.
  3. **Redundant .pl parsing:** Reads `.pl` files directly (regex for `base_extractiveness`, `suppression_score`) when this data is available in `pipeline_output.json` and `corpus_data.json`.

### 2. `conflict_map.py`

- **Purpose:** Exports `SEVERITY` ordinal dict and `classify_shift()` for comparing constraint type across perspectives.
- **Data sources:** `corpus_data.json`, `fingerprint_report.md` (regex-parsed)
- **Pipeline phase:** NOT IN MAKEFILE — library imported by `omega_enricher.py`
- **Issues:**
  1. **Standalone severity ordering:** `SEVERITY` dict (lines 25-33) is a hardcoded ordinal ranking with no config.pl equivalent. Imported by `omega_enricher.py` — could be derived from pipeline data or centralized.

### 3. `config_sensitivity_sweep.py`

- **Purpose:** Sweeps config.pl parameter ranges by patching values and re-running swipl to measure classification stability.
- **Data sources:** `config.pl` (regex-parsed for `param/2`), `swipl` subprocess calls
- **Pipeline phase:** NOT IN MAKEFILE — standalone research tool
- **Issues:**
  1. **swipl subprocess dependency:** Invokes `swipl` directly via `subprocess.run` — tightly coupled to local Prolog installation.
  2. **Regex config parsing:** Parses `config.pl` with regex instead of using a structured intermediary.

### 4. `domain_priors.py`

- **Purpose:** Generates `prolog/domain_registry.pl` by scanning testset files for metric scores and inferring constraint types from thresholds.
- **Data sources:** `.pl` files (regex for `base_extractiveness`, `suppression_score`, `constraint_claim`), `config.pl` (regex for `param/2` with fallback defaults)
- **Pipeline phase:** PREP (Makefile line 152)
- **Issues:**
  1. **Fallback defaults as shadow config:** `_PARAM_MAP` (lines 35-41) embeds fallback defaults that duplicate config.pl values. If config.pl read fails silently, the fallbacks become an invisible shadow configuration. The pattern is sound (config-first + fallback) but the fallback values can drift.

### 5. `domain_priors_expander.py`

- **Purpose:** Generates `domain_priors_expanded.pl` with additional domain context for constraints.
- **Data sources:** `.pl` files (regex), `config.pl` (regex for thresholds)
- **Pipeline phase:** NOT IN MAKEFILE — standalone utility
- **Issues:**
  1. **Hardcoded `BASE_DIR` path** (line 19): Absolute path to project root.
  2. **Redundant .pl parsing:** Reads testset files with regex — could use `pipeline_output.json` if run post-pipeline.

### 6. `duplicate_checker.py`

- **Purpose:** Detects duplicate constraint IDs and module names across testset `.pl` files.
- **Data sources:** `.pl` files (regex for `constraint_claim`, `:- module`)
- **Pipeline phase:** CHECK (Makefile line 133)
- **Issues:** None. Must parse `.pl` files directly — runs before pipeline output exists.

### 7. `enhanced_report.py`

- **Purpose:** Two-stream architecture: runs swipl for live Prolog report, then splices Python-built analysis sections.
- **Data sources:** `swipl` subprocess, `pipeline_output.json`, `corpus_data.json`, `omega_data.json`, `orbit_data.json`, `variance_analysis.md`, `pattern_mining.md`, `index_sufficiency.md`
- **Pipeline phase:** NOT IN MAKEFILE — standalone composite report generator
- **Issues:**
  1. **swipl subprocess dependency:** Runs full Prolog report generation via subprocess.
  2. **Brittle markdown splicing:** Regex-parses multiple markdown reports to combine sections — fragile if report formats change.

### 8. `extract_corpus_data.py`

- **Purpose:** Builds `corpus_data.json` from `pipeline_output.json`, enriched with orbit data.
- **Data sources:** `pipeline_output.json`, `orbit_data.json`
- **Pipeline phase:** POST (Makefile line 384)
- **Issues:** None. Clean JSON-to-JSON pipeline. Domain inference from constraint names (lines 187-225) is a reasonable heuristic fallback.

### 9. `false_mountain_reporter.py`

- **Purpose:** Generates diagnostic report for false mountains (claimed mountains that fail metric gates).
- **Data sources:** `pipeline_output.json`, `orbit_data.json` (via `orbit_utils`)
- **Pipeline phase:** POST (Makefile line 365)
- **Issues:**
  1. **Consolidation candidate:** Near-identical structure to `tangled_rope_reporter.py`. Both follow the same pattern: filter `pipeline_output.json` by type, format per-constraint diagnostics, attach orbit data. Could become a mode of `type_reporter.py`.

### 10. `linter.py`

- **Purpose:** Structural linter enforcing 30 rules on `.pl` testset files.
- **Data sources:** `.pl` files (extensive regex — this is its core purpose), `config.pl` (regex for `param/2` with fallback defaults, lines 57-70)
- **Pipeline phase:** CHECK (Makefile line 131, produces `lint_errors.txt`)
- **Issues:** None. Must parse `.pl` files directly — that is its purpose. Config reading with fallbacks is well-implemented.

### 11. `meta_reporter.py`

- **Purpose:** Pipeline health dashboard showing pass/fail counts, type distributions, and orbit coverage.
- **Data sources:** `pipeline_output.json`, `output.txt` (regex for pass/fail counts), `orbit_data.json`
- **Pipeline phase:** POST (Makefile line 432)
- **Issues:** None. Clean implementation reading from appropriate sources.

### 12. `normalize_orbit_ids.py`

- **Purpose:** Normalizes `orbit_data.json` keys from filename stems to canonical constraint IDs (from `constraint_claim/2`).
- **Data sources:** `orbit_data.json`, `.pl` files (regex for `constraint_claim`)
- **Pipeline phase:** POST (Makefile line 322)
- **Issues:** None. Must parse `.pl` files — maps filename stems to canonical atoms for orbit data alignment.

### 13. `omega_enricher.py`

- **Purpose:** Enriches omega gap records with composite severity scores, corpus metrics, and orbit context.
- **Data sources:** `omega_data.json`, `corpus_data.json`, `orbit_data.json` (via `orbit_utils`), `conflict_map.SEVERITY` (import)
- **Pipeline phase:** POST (Makefile line 419)
- **Issues:**
  1. **Hardcoded severity weights:** `W_EPSILON=0.5`, `W_SUPPRESSION=0.3`, `W_ORBIT_SPAN=0.2` (lines 32-34) and `SEVERITY_THRESHOLDS` (lines 37-42) have no config.pl equivalents.
  2. **Cross-module SEVERITY import:** Imports `SEVERITY` from `conflict_map.py` (a non-pipeline script) — creates a dependency on dead code.

### 14. `omega_reporter.py`

- **Purpose:** Extracts omega epistemological gap records from pipeline output into `omega_report.md` and `omega_data.json`.
- **Data sources:** `pipeline_output.json`
- **Pipeline phase:** POST (Makefile line 375)
- **Issues:** None. Clean JSON-based extraction.

### 15. `orbit_utils.py`

- **Purpose:** Shared utility for loading and querying `orbit_data.json`.
- **Data sources:** `orbit_data.json`
- **Pipeline phase:** LIBRARY (imported by `type_reporter`, `tangled_rope_reporter`, `false_mountain_reporter`, `omega_enricher`)
- **Issues:** None. Well-scoped shared utility.

### 16. `pattern_miner.py`

- **Purpose:** Mines structural patterns from corpus data (tangled rope candidates, piton candidates, scaffold candidates).
- **Data sources:** `corpus_data.json`
- **Pipeline phase:** POST (Makefile line 401)
- **Issues:**
  1. **Hardcoded candidate thresholds:** Inline threshold literals at lines 123-138 (`extractiveness >= 0.6`, `suppression >= 0.7`, `0.3 <= extractiveness <= 0.6`) are standalone heuristics with no config.pl equivalent. Not necessarily wrong, but undocumented.

### 17. `powerless_blind_diagnostic.py`

- **Purpose:** Diagnoses constraints where the powerless perspective produces a different (usually more severe) classification than other perspectives.
- **Data sources:** `.pl` files (regex for metrics), `fingerprint_report.md` (regex), `corpus_data.json`
- **Pipeline phase:** NOT IN MAKEFILE — standalone diagnostic
- **Issues:**
  1. **Hardcoded absolute path:** `BASE = Path("/home/scott/bin/structural_dynamics_model")` (line 66).
  2. **Massive gate threshold block:** 16 hardcoded constants (lines 95-110) duplicate config.pl values. Values currently match, but no config reading mechanism — will drift if config.pl changes.
  3. **Redundant .pl parsing:** Reads `.pl` files and `fingerprint_report.md` with regex for data available in `pipeline_output.json` and `corpus_data.json`.

### 18. `prolog_cleaner.py`

- **Purpose:** Fixes AI-generated Prolog artifacts (stray comments, incorrect module headers, duplicate facts).
- **Data sources:** `.pl` files (direct read/write)
- **Pipeline phase:** UTILITY (Makefile line 139, `lint-fix` target only)
- **Issues:** None. Source file modifier — must operate on `.pl` files directly.

### 19. `promote_human_readable.py`

- **Purpose:** One-time migration: promotes `human_readable` from comment metadata to queryable `human_readable/2` Prolog facts.
- **Data sources:** `.pl` files (direct read/write)
- **Pipeline phase:** NOT IN MAKEFILE — one-time migration script
- **Issues:**
  1. **One-time migration complete:** If all files have been processed, this script is dead code. Idempotent (checks for existing facts) so harmless, but could be archived.

### 20. `python_test_suite.py`

- **Purpose:** Generates `prolog/validation_suite.pl` by collecting `interval/3` patterns from all testset files.
- **Data sources:** `.pl` files (regex for `interval()` patterns)
- **Pipeline phase:** PREP (Makefile line 153)
- **Issues:** None. Must parse `.pl` files — runs before pipeline output exists.

### 21. `reform_threshold_report.py`

- **Purpose:** Reports which constraints become reformable as perspective shifts from powerless toward institutional.
- **Data sources:** `.pl` files (regex for metrics), `fingerprint_report.md` (regex), `corpus_data.json`
- **Pipeline phase:** NOT IN MAKEFILE — standalone diagnostic
- **Issues:**
  1. **Hardcoded absolute path:** `BASE = Path("/home/scott/bin/structural_dynamics_model")` (line 45).
  2. **Duplicated scope/coalition constants:** `SCOPE_MODIFIERS` (lines 66-69), `COALITION_VICTIM_THRESHOLD` (line 73), `COALITION_EPS_FLOOR` (line 74), `COALITION_SUPP_FLOOR` (line 75) — all duplicate config.pl values and `powerless_blind_diagnostic.py` constants, with no config reading.
  3. **Redundant .pl parsing:** Reads `.pl` files and `fingerprint_report.md` with regex for data available in `pipeline_output.json`.

### 22. `regenerate_stories.py`

- **Purpose:** Batch-regenerates constraint testset files using Gemini 2.5 Pro API, with lint and duplicate checks.
- **Data sources:** Gemini API, `linter.lint_file` (import), `duplicate_checker.check_incoming_file` (import)
- **Pipeline phase:** NOT IN MAKEFILE — standalone generation tool
- **Issues:**
  1. **External API dependency:** Depends on `GEMINI_API_KEY` environment variable and Google Generative AI SDK. No Makefile integration (correctly so — manual tool).

### 23. `sigmoid.py`

- **Purpose:** Shared sigmoid power modifier `f(d)` computation with canonical d positions for each power level.
- **Data sources:** None (pure computation module)
- **Pipeline phase:** LIBRARY (imported by `reform_threshold_report.py`, `powerless_blind_diagnostic.py`)
- **Issues:**
  1. **Hardcoded sigmoid params:** `SIGMOID_LOWER`, `SIGMOID_UPPER`, `SIGMOID_MIDPOINT`, `SIGMOID_STEEPNESS` (lines 16-19) and `CANONICAL_D` (lines 24-31) duplicate config.pl section 4C exactly. Comments note this ("match config.pl") but no runtime reading occurs. Will drift if config.pl changes.

### 24. `sufficiency_tester.py`

- **Purpose:** Tests whether the 4 classification indices (extractiveness, suppression, power, scope) sufficiently explain observed variance.
- **Data sources:** `corpus_data.json`, `pipeline_output.json`
- **Pipeline phase:** POST (Makefile line 407)
- **Issues:**
  1. **Hardcoded verdict thresholds:** `failure_rate > 5`, `genuine_rate > 10`, `anomaly_rate > 20`, `perspectival_rate > 50` (lines 406-414) are analyst-judgment constants with no config.pl equivalent.

### 25. `tangled_rope_reporter.py`

- **Purpose:** Generates diagnostic report for tangled rope constraints.
- **Data sources:** `pipeline_output.json`, `orbit_data.json` (via `orbit_utils`)
- **Pipeline phase:** POST (Makefile line 360)
- **Issues:**
  1. **Consolidation candidate:** Near-identical structure to `false_mountain_reporter.py`. Both could become modes of `type_reporter.py` (which already handles 5 types via `--type` parameter).

### 26. `type_reporter.py`

- **Purpose:** Parameterized reporter generating per-type diagnostic markdown for snare, piton, scaffold, rope, and mountain.
- **Data sources:** `pipeline_output.json`, `orbit_data.json` (via `orbit_utils`)
- **Pipeline phase:** POST (Makefile lines 334-355, invoked 5 times with `--type`)
- **Issues:** None. Cleanest reporter implementation — reads JSON, parameterized by type. Model for other reporters.

### 27. `variance_analyzer.py`

- **Purpose:** Analyzes classification variance across perspectives for each constraint.
- **Data sources:** `corpus_data.json`
- **Pipeline phase:** POST (Makefile line 395)
- **Issues:** None. Clean JSON input, markdown output.

---

## Section 3: Consolidation Map

### 3.1 tangled_rope_reporter.py + false_mountain_reporter.py → type_reporter.py

`type_reporter.py` already handles 5 types via `--type snare|rope|scaffold|piton|mountain`. The tangled rope and false mountain reporters follow the same pattern (filter pipeline JSON → format diagnostics → attach orbit data) but are separate scripts with separate Makefile targets.

**Recommended action:** Add `--type tangled_rope` and `--type false_mountain` modes to `type_reporter.py`. Update Makefile lines 358-366 to invoke `type_reporter.py` instead. Delete `tangled_rope_reporter.py` and `false_mountain_reporter.py`.

**Effort:** Low. The structural pattern is identical; only the filter predicate and section headings differ.

### 3.2 SEVERITY dict: conflict_map.py → omega_enricher.py dependency

`omega_enricher.py` (a pipeline script, Makefile line 419) imports `SEVERITY` from `conflict_map.py` (a non-pipeline script). This creates a runtime dependency on dead code.

**Recommended action:** Either:
- (a) Move `SEVERITY` and `classify_shift()` into `omega_enricher.py` directly (inline, ~15 lines), or
- (b) Create a minimal `constants.py` shared module if other scripts also need the ordering.

Option (a) is simpler since `omega_enricher.py` is the only pipeline consumer.

### 3.3 sigmoid.py constants → config.pl section 4C

`sigmoid.py` hardcodes 4 sigmoid parameters and 6 canonical d values (lines 16-31) that exactly duplicate `config.pl` section 4C (config lines 97-112). The comments acknowledge this ("match config.pl") but there is no runtime reading.

**Recommended action:** Add a `_read_sigmoid_from_config()` function (same pattern as `domain_priors.py:_read_config_thresholds()`) that reads config.pl at import time with the current values as fallback defaults.

### 3.4 Dead/utility scripts not in Makefile

12 scripts are not invoked by any Makefile target (or only by the `lint-fix` utility target):

| Script | Status | Recommendation |
|--------|--------|---------------|
| `classification_audit.py` | Standalone diagnostic | Keep; document standalone usage |
| `conflict_map.py` | Library for omega_enricher | Inline into omega_enricher (see 3.2) |
| `reform_threshold_report.py` | Standalone diagnostic | Keep; fix hardcoded path + constants |
| `powerless_blind_diagnostic.py` | Standalone diagnostic | Keep; fix hardcoded path + constants |
| `sigmoid.py` | Library for 2 diagnostics | Keep; add config.pl reading (see 3.3) |
| `config_sensitivity_sweep.py` | Research tool | Keep; document as manual-only |
| `domain_priors_expander.py` | Standalone utility | Assess if still needed; hardcoded path |
| `enhanced_report.py` | Composite report | Keep; document swipl dependency |
| `regenerate_stories.py` | Generation tool | Keep; document API key requirement |
| `promote_human_readable.py` | One-time migration | Archive if migration complete |
| `orbit_utils.py` | Shared library | Keep (actively imported by 4 pipeline scripts) |
| `prolog_cleaner.py` | Utility (lint-fix target) | Keep |

---

## Section 4: Priority Ranking

### Priority 1: Eliminate .pl regex parsing in post-pipeline scripts

**Impact: High | Effort: Medium**

Scripts that run AFTER `pipeline_output.json` exists should not re-parse `.pl` files with regex. The pipeline JSON is the authoritative source.

Affected scripts:
- `classification_audit.py` — reads `.pl` files for `base_extractiveness`, `suppression_score`
- `reform_threshold_report.py` — reads `.pl` files for constraint metrics
- `powerless_blind_diagnostic.py` — reads `.pl` files for constraint metrics
- `domain_priors_expander.py` — reads `.pl` files for metric scores

All of these can read from `corpus_data.json` or `pipeline_output.json` instead. This eliminates a class of regex fragility bugs.

### Priority 2: Consolidate tangled_rope + false_mountain reporters into type_reporter.py

**Impact: Medium | Effort: Low**

`type_reporter.py` is already parameterized for 5 types. Adding 2 more modes is straightforward and eliminates 2 files + 2 Makefile targets. The existing `--type` CLI pattern is clean and proven.

### Priority 3: Centralize hardcoded thresholds

**Impact: Medium | Effort: Medium**

Currently 6+ scripts embed threshold constants that duplicate `config.pl` values. Two approaches:

- **For pipeline scripts** (`domain_priors.py`, `linter.py` pattern): Already reading config.pl with fallbacks. Good. No change needed.
- **For standalone diagnostics** (`powerless_blind_diagnostic.py`, `reform_threshold_report.py`, `classification_audit.py`, `sigmoid.py`): Add config.pl reading with current values as fallback defaults, following the `domain_priors.py` pattern at lines 14-48.

Special concern: `classification_audit.py` has 2 values that **diverge** from config.pl (MOUNTAIN_MAX_EXTRACTIVENESS=0.15 vs config 0.25, TANGLED_MIN_EXTRACTIVENESS=0.50 vs config 0.16/0.30). These need investigation — either the audit uses intentionally different thresholds, or these are stale values.

### Priority 4: Archive dead utility scripts or document standalone purpose

**Impact: Low | Effort: Low**

- `promote_human_readable.py`: If migration is complete, move to a `python/archive/` directory.
- `domain_priors_expander.py`: Assess if still needed or superseded by `domain_priors.py`.
- For remaining standalone diagnostics: Add a docstring note explaining they are run manually and are not part of the automated pipeline.

### Priority 5: Fix hardcoded absolute paths

**Impact: Low | Effort: Low**

Two scripts hardcode `BASE = Path("/home/scott/bin/structural_dynamics_model")`:
- `powerless_blind_diagnostic.py` (line 66)
- `reform_threshold_report.py` (line 45)

Replace with `Path(__file__).resolve().parent.parent` (same pattern used by `promote_human_readable.py` at line 21).

### Priority 6: Replace swipl subprocess calls where JSON contract suffices

**Impact: Low | Effort: High**

Two scripts invoke swipl directly:
- `config_sensitivity_sweep.py`: Needs swipl to re-run classification with modified params. **Cannot be replaced** — the whole point is testing Prolog behavior under param changes.
- `enhanced_report.py`: Runs swipl for the Prolog-native report section. Could potentially be replaced by reading `pipeline_output.json`, but the Prolog report may include live computations not captured in JSON.

Recommendation: Leave as-is. Both are standalone tools where swipl dependency is justified.

---

## Section 5: Hardcoded Constants Inventory

### Sigmoid Parameters (config.pl section 4C)

| Constant | Value | File | Line | config.pl param | Config Line | Match? |
|----------|-------|------|------|-----------------|-------------|--------|
| `SIGMOID_LOWER` | -0.20 | sigmoid.py | 16 | `sigmoid_lower` | 97 | YES |
| `SIGMOID_UPPER` | 1.50 | sigmoid.py | 17 | `sigmoid_upper` | 98 | YES |
| `SIGMOID_MIDPOINT` | 0.50 | sigmoid.py | 18 | `sigmoid_midpoint` | 99 | YES |
| `SIGMOID_STEEPNESS` | 6.00 | sigmoid.py | 19 | `sigmoid_steepness` | 100 | YES |
| `CANONICAL_D[powerless]` | 1.00 | sigmoid.py | 25 | `canonical_d_powerless` | 107 | YES |
| `CANONICAL_D[moderate]` | 0.6459 | sigmoid.py | 26 | `canonical_d_moderate` | 108 | YES |
| `CANONICAL_D[powerful]` | 0.4804 | sigmoid.py | 27 | `canonical_d_powerful` | 109 | YES |
| `CANONICAL_D[organized]` | 0.3990 | sigmoid.py | 28 | `canonical_d_organized` | 110 | YES |
| `CANONICAL_D[institutional]` | 0.00 | sigmoid.py | 29 | `canonical_d_institutional` | 111 | YES |
| `CANONICAL_D[analytical]` | 0.7250 | sigmoid.py | 30 | `canonical_d_analytical` | 112 | YES |

### Mountain Gate Thresholds

| Constant | Value | File | Line | config.pl param | Config Line | Match? |
|----------|-------|------|------|-----------------|-------------|--------|
| `MOUNTAIN_MAX_EXTRACTIVENESS` | 0.15 | classification_audit.py | 28 | `mountain_extractiveness_max` | 148 | **NO** (config=0.25) |
| `MOUNTAIN_MAX_SUPPRESSION` | 0.05 | classification_audit.py | 29 | `mountain_suppression_ceiling` | 146 | YES |
| `MOUNTAIN_EPS_MAX` | 0.25 | powerless_blind_diagnostic.py | 95 | `mountain_extractiveness_max` | 148 | YES |
| `MOUNTAIN_SUPP_MAX` | 0.05 | powerless_blind_diagnostic.py | 96 | `mountain_suppression_ceiling` | 146 | YES |
| `mountain_extractiveness_max` | 0.25 | linter.py | 69 | `mountain_extractiveness_max` | 148 | YES (reads config) |
| `mountain_suppression_ceiling` | 0.05 | linter.py | 70 | `mountain_suppression_ceiling` | 146 | YES (reads config) |

### Snare Gate Thresholds

| Constant | Value | File | Line | config.pl param | Config Line | Match? |
|----------|-------|------|------|-----------------|-------------|--------|
| `SNARE_MIN_EXTRACTIVENESS` | 0.46 | classification_audit.py | 32 | `snare_epsilon_floor` | 187 | YES |
| `SNARE_MIN_SUPPRESSION` | 0.60 | classification_audit.py | 33 | `snare_suppression_floor` | 167 | YES |
| `SNARE_CHI_FLOOR` | 0.66 | powerless_blind_diagnostic.py | 97 | `snare_chi_floor` | 186 | YES |
| `SNARE_EPS_FLOOR` | 0.46 | powerless_blind_diagnostic.py | 98 | `snare_epsilon_floor` | 187 | YES |
| `SNARE_SUPP_FLOOR` | 0.60 | powerless_blind_diagnostic.py | 99 | `snare_suppression_floor` | 167 | YES |
| `snare_epsilon_floor` | 0.46 | linter.py | 66 | `snare_epsilon_floor` | 187 | YES (reads config) |
| `SNARE_EXTRACTION_FLOOR` | 0.46 | domain_priors.py | 39 | `snare_epsilon_floor` | 187 | YES (reads config) |
| `WHO_MIN_EXTRACTIVENESS` | 0.46 | classification_audit.py | 53 | `snare_epsilon_floor` | 187 | YES |

### Rope Gate Thresholds

| Constant | Value | File | Line | config.pl param | Config Line | Match? |
|----------|-------|------|------|-----------------|-------------|--------|
| `ROPE_CHI_CEIL` | 0.35 | powerless_blind_diagnostic.py | 102 | `rope_chi_ceiling` | 182 | YES |
| `ROPE_EPS_CEIL` | 0.45 | powerless_blind_diagnostic.py | 103 | `rope_epsilon_ceiling` | 183 | YES |
| `ROPE_EXTRACTION_CEILING` | 0.15 | domain_priors.py | 36 | `rope_extraction_ceiling` | 154 | YES (reads config) |

### Tangled Rope Gate Thresholds

| Constant | Value | File | Line | config.pl param | Config Line | Match? |
|----------|-------|------|------|-----------------|-------------|--------|
| `TANGLED_MIN_EXTRACTIVENESS` | 0.50 | classification_audit.py | 36 | `tangled_rope_extraction_floor` | 160 | **NO** (config=0.16) |
| `TANGLED_MIN_SUPPRESSION` | 0.40 | classification_audit.py | 37 | `tangled_rope_suppression_floor` | 162 | YES |
| `TANGLED_CHI_FLOOR` | 0.40 | powerless_blind_diagnostic.py | 104 | `tangled_rope_chi_floor` | 190 | YES |
| `TANGLED_CHI_CEIL` | 0.90 | powerless_blind_diagnostic.py | 105 | `tangled_rope_chi_ceil` | 191 | YES |
| `TANGLED_EPS_FLOOR` | 0.30 | powerless_blind_diagnostic.py | 106 | `tangled_rope_epsilon_floor` | 192 | YES |
| `TANGLED_SUPP_FLOOR` | 0.40 | powerless_blind_diagnostic.py | 107 | `tangled_rope_suppression_floor` | 162 | YES |
| `TANGLED_ROPE_EXTRACTION_FLOOR` | 0.16 | domain_priors.py | 37 | `tangled_rope_extraction_floor` | 160 | YES (reads config) |
| `TANGLED_ROPE_EXTRACTION_CEIL` | 0.90 | domain_priors.py | 38 | `tangled_rope_extraction_ceil` | 161 | YES (reads config) |

### Scaffold / Piton Gate Thresholds

| Constant | Value | File | Line | config.pl param | Config Line | Match? |
|----------|-------|------|------|-----------------|-------------|--------|
| `PITON_MIN_THEATER` | 0.70 | classification_audit.py | 40 | `piton_theater_floor` | 200 | YES |
| `SCAFFOLD_MAX_THEATER` | 0.70 | classification_audit.py | 43 | — | — | Standalone (inverse of piton_theater_floor) |
| `SCAFFOLD_CHI_CEIL` | 0.30 | powerless_blind_diagnostic.py | 100 | `scaffold_extraction_ceil` | 196 | YES |
| `SCAFFOLD_THEATER_MAX` | 0.70 | powerless_blind_diagnostic.py | 101 | — | — | Standalone |
| `PITON_CHI_CEIL` | 0.25 | powerless_blind_diagnostic.py | 108 | `piton_extraction_ceiling` | 199 | YES |
| `PITON_EPS_FLOOR` | 0.10 | powerless_blind_diagnostic.py | 109 | `piton_epsilon_floor` | 201 | YES |
| `PITON_THEATER_FLOOR` | 0.70 | powerless_blind_diagnostic.py | 110 | `piton_theater_floor` | 200 | YES |
| `scaffold_extraction_ceil` | 0.30 | linter.py | 67 | `scaffold_extraction_ceil` | 196 | YES (reads config) |
| `piton_theater_floor` | 0.70 | linter.py | 68 | `piton_theater_floor` | 200 | YES (reads config) |
| `FALSE_MOUNTAIN_THRESHOLD` | 0.90 | domain_priors.py | 40 | `false_mountain_extraction_threshold` | 149 | YES (reads config) |

### Scope / Coalition / Power Constants

| Constant | Value | File | Line | config.pl param | Config Line | Match? |
|----------|-------|------|------|-----------------|-------------|--------|
| `SCOPE_MODIFIERS[local]` | 0.8 | reform_threshold_report.py | 67 | `scope_modifier_local` | 83 | YES |
| `SCOPE_MODIFIERS[national]` | 1.0 | reform_threshold_report.py | 68 | `scope_modifier_national` | 85 | YES |
| `SCOPE_MODIFIERS[global]` | 1.2 | reform_threshold_report.py | 69 | `scope_modifier_global` | 87 | YES |
| `SIGMA_LOCAL` | 0.8 | powerless_blind_diagnostic.py | 81 | `scope_modifier_local` | 83 | YES |
| `SIGMA_GLOBAL` | 1.2 | powerless_blind_diagnostic.py | 82 | `scope_modifier_global` | 87 | YES |
| `COALITION_VICTIM_THRESHOLD` | 3 | reform_threshold_report.py | 73 | `critical_mass_threshold` | 73 | YES |
| `COALITION_VICTIM_THRESHOLD` | 3 | powerless_blind_diagnostic.py | 85 | `critical_mass_threshold` | 73 | YES |
| `COALITION_EPS_FLOOR` | 0.46 | reform_threshold_report.py | 74 | `snare_epsilon_floor` | 187 | YES |
| `COALITION_EPS_FLOOR` | 0.46 | powerless_blind_diagnostic.py | 86 | `snare_epsilon_floor` | 187 | YES |
| `COALITION_SUPP_FLOOR` | 0.60 | reform_threshold_report.py | 75 | `snare_suppression_floor` | 167 | YES |
| `COALITION_SUPP_FLOOR` | 0.60 | powerless_blind_diagnostic.py | 87 | `snare_suppression_floor` | 167 | YES |

### Omega Enrichment Constants (no config.pl equivalent)

| Constant | Value | File | Line | config.pl param | Match? |
|----------|-------|------|------|-----------------|--------|
| `W_EPSILON` | 0.5 | omega_enricher.py | 32 | — | Standalone |
| `W_SUPPRESSION` | 0.3 | omega_enricher.py | 33 | — | Standalone |
| `W_ORBIT_SPAN` | 0.2 | omega_enricher.py | 34 | — | Standalone |
| `SEVERITY_THRESHOLDS[critical]` | 0.70 | omega_enricher.py | 38 | — | Standalone |
| `SEVERITY_THRESHOLDS[high]` | 0.45 | omega_enricher.py | 39 | — | Standalone |
| `SEVERITY_THRESHOLDS[medium]` | 0.25 | omega_enricher.py | 40 | — | Standalone |
| `SEVERITY` ordinal dict | 0-5 | conflict_map.py | 25-33 | — | Standalone |

### Pattern Mining Thresholds (no config.pl equivalent)

| Constant | Value | File | Line | config.pl param | Match? |
|----------|-------|------|------|-----------------|--------|
| tangled rope: extractiveness | >= 0.6 | pattern_miner.py | 123 | — | Standalone |
| tangled rope: suppression | >= 0.6 | pattern_miner.py | 124 | — | Standalone |
| piton: suppression | >= 0.7 | pattern_miner.py | 130 | — | Standalone |
| scaffold: extractiveness | 0.3-0.6 | pattern_miner.py | 137 | — | Standalone |
| scaffold: suppression | 0.3-0.6 | pattern_miner.py | 138 | — | Standalone |

### Sufficiency Verdict Thresholds (no config.pl equivalent)

| Constant | Value | File | Line | config.pl param | Match? |
|----------|-------|------|------|-----------------|--------|
| failure_rate gate | > 5 | sufficiency_tester.py | 406 | — | Standalone |
| genuine_rate gate | > 10 | sufficiency_tester.py | 408 | — | Standalone |
| anomaly_rate gate | > 20 | sufficiency_tester.py | 410 | — | Standalone |
| perspectival_rate gate | > 50 | sufficiency_tester.py | 414 | — | Standalone |

### Domain Normalization (no config.pl equivalent)

| Constant | Value | File | Line | config.pl param | Match? |
|----------|-------|------|------|-----------------|--------|
| `DOMAIN_NORMALIZATION` | 5 mappings | sufficiency_tester.py | 19-24 | — | Standalone |

### Audit-Only Thresholds (no config.pl equivalent)

| Constant | Value | File | Line | config.pl param | Match? |
|----------|-------|------|------|-----------------|--------|
| `THEATER_NATURALIZATION_THRESHOLD` | 0.50 | classification_audit.py | 47 | — | Standalone |
| `THEATER_CONFLICT_THRESHOLD` | 0.50 | classification_audit.py | 50 | — | Standalone |

---

### Summary of Mismatches

Two hardcoded constants in `classification_audit.py` **diverge** from their config.pl counterparts:

1. **`MOUNTAIN_MAX_EXTRACTIVENESS = 0.15`** (classification_audit.py:28) vs **`mountain_extractiveness_max = 0.25`** (config.pl:148)
   - The audit uses a stricter threshold than the pipeline. This may be intentional (auditing against a tighter standard) or may be a stale value from an earlier config revision.

2. **`TANGLED_MIN_EXTRACTIVENESS = 0.50`** (classification_audit.py:36) vs **`tangled_rope_extraction_floor = 0.16`** (config.pl:160) / **`tangled_rope_epsilon_floor = 0.30`** (config.pl:192)
   - Neither config.pl param matches 0.50. This is likely a conceptual threshold ("high extraction for tangled") rather than a gate threshold, but it should be documented or reconciled.

All other hardcoded constants either match config.pl values exactly or are standalone constants with no config.pl equivalent.
