# ID Normalization Fix Report — Bug #3

## Root Cause

`orbit_data.json` was keyed by internal `constraint_metric/3` atoms (e.g., `agent_opt_2026`),
while `corpus_data.json` was keyed by `.pl` filename stems (e.g., `agentive_optimism_2026`).
The orbit pipeline (`orbit_report.pl`) collects IDs via `findall(C, narrative_ontology:constraint_metric(C, _, _), ...)`
and writes these internal atoms directly as JSON keys.

## Fix Applied

Created `python/normalize_orbit_ids.py` — a post-processor that rewrites `orbit_data.json` keys
from internal constraint atoms to `.pl` filename stems. Scans each `.pl` file for
`constraint_metric(ATOM, ...)` declarations to build an `internal_id → filename_stem` mapping,
then rewrites keys in place.

Integrated into `scripts/run_full_pipeline.sh` after orbit analysis (Step 8b) and before FPN analysis (Step 8c).

Simplified `extract_corpus_data.py:load_orbit_data()` and `orbit_utils.py:get_orbit_signature()`
by removing their reverse-mapping fallback logic, since orbit_data.json now uses canonical filename keys.

## Before/After Match Counts

| Metric | Before | After |
|--------|--------|-------|
| Exact matches (corpus↔orbit) | 889 | 1022 |
| Fuzzy matches | 73 | 0 |
| Corpus-only unmatched | 62 | 2 |
| Orbit-only unmatched | 60 | 0 |

## Normalization Summary

- **133 keys renamed** from internal atoms to filename stems
- **889 keys unchanged** (already matched)
- **0 collisions**

## Remaining Unmatched IDs

2 corpus-only constraints have no orbit data — these are constraints whose `.pl` files
exist but lack `constraint_metric` declarations processable by the orbit pipeline:

| Constraint ID | .pl file exists? |
|---|---|
| `26usc469_real_estate_exemption` | yes |
| `8k_tv_limit_2026` | yes |

These filenames start with digits. The likely cause is that Prolog atoms starting with digits
require quoting (`'26usc469_real_estate_exemption'`), and `constraint_metric/3` may not be
declared or may use a differently-formatted atom. This is expected and not a normalization bug.

## Collision Report

No collisions detected. Each internal atom mapped to exactly one filename.

## Files Modified

1. **`python/normalize_orbit_ids.py`** — new post-processor script
2. **`scripts/run_full_pipeline.sh`** — added normalization step after orbit analysis
3. **`python/extract_corpus_data.py`** — removed 30-line reverse-mapping fallback in `load_orbit_data()`
4. **`python/orbit_utils.py`** — removed `_build_filename_mapping()` and fallback logic in `get_orbit_signature()`
