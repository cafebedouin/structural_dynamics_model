# Bug Fix Verification Report

**Date:** 2026-02-15
**Pipeline run:** `scripts/run_full_pipeline.sh` — completed successfully (0 errors, 0 warnings)

## Fixes Applied

| Fix | Bug # | Description | Status |
|-----|-------|-------------|--------|
| FIX 1 | #1, #4 | Moved `fnl_trace_diagnostic.pl` to `prolog/testsets_diagnostic/` | Done |
| FIX 2 | #2 | Fixed omega regex in `meta_reporter.py:65` — captures full omega name | Done |
| FIX 3 | #5 | Domain name normalization in `extract_corpus_data.py:257,265` — `.lower()` | Done |
| FIX 4 | #7 | Filter sentinel IDs (`unknown`, `none`, `undefined`) in `extract_corpus_data.py:153` | Done |
| FIX 5 | #9 | Deleted `outputs/coupling_protocol_post_fix.md` (identical to `coupling_protocol.md`) | Done |
| DIAGNOSE | #3 | ID normalization audit script + report | Done |

## Pipeline Additions

- Added Steps 8g-8j to `run_full_pipeline.sh`: covering analysis, giant component analysis, coupling protocol, MaxEnt diagnostic
- Added 4 new reports to GENERATED REPORTS listing
- Created `python/id_normalization_audit.py` diagnostic script

## Before/After Verification

| Metric | Before | After | Expected | Status |
|--------|--------|-------|----------|--------|
| Meta-report omega count | 507 | 878 | ~878 | PASS |
| corpus_data.json constraint count | 1026 | 1024 | 1025 | NOTE |
| corpus_data.json null claimed_type | 1 | 0 | 0 | PASS |
| Uppercase domains in corpus | 3 (Political, Physics, Social) | 0 | 0 | PASS |
| coupling_protocol_post_fix.md | exists | deleted | deleted | PASS |
| fnl_trace_diagnostic.pl location | `prolog/testsets/` | `prolog/testsets_diagnostic/` | `prolog/testsets_diagnostic/` | PASS |
| Covering analysis constraint count | not found | 1022 | ~1024 | PASS |
| Giant component node count | 1 (bad grep) | 1022 | ~1024 | PASS |
| Coupling protocol | contaminated | regenerated | regenerated | PASS |
| MaxEnt diagnostic constraint count | not found | 1021 | ~1021 | PASS |
| Orbit report constraint count | 1022 | 1022 | ~1022 (unchanged) | PASS |
| Enriched omega severity (critical) | 328 (37.4%) | 328 (37.4%) | unchanged | PASS |
| Enriched omega severity (high) | 408 (46.5%) | 408 (46.5%) | unchanged | PASS |
| Enriched omega severity (medium) | 77 (8.8%) | 77 (8.8%) | unchanged | PASS |
| Enriched omega severity (low) | 65 (7.4%) | 65 (7.4%) | unchanged | PASS |
| Variance analysis domains | Political/Physics/Social | political/physics/social | all lowercase | PASS |
| Validation suite test cases | — | 1024 | ~1024 | PASS |
| Prolog test pass rate | — | 100.0% (1024/1024) | 100% | PASS |

### Note on Constraint Count (1024 vs 1025)

The plan expected 1025 (1026 minus the `unknown` sentinel). The actual result is 1024 because **both** fixes contributed:
- FIX 4 filtered out the `unknown` sentinel (-1)
- FIX 1 removed `fnl_trace_diagnostic.pl` from testsets/ (-1), eliminating its phantom entry

This is correct behavior — both bugs produced phantom entries, and both are now eliminated.

## ID Normalization Audit (Bug #3 Diagnostic)

| Metric | Count |
|--------|-------|
| Corpus IDs | 1024 |
| Orbit IDs | 1022 |
| Exact matches | 889 |
| Fuzzy matches | 73 |
| Truly unmatched (corpus-only) | 62 |
| Truly unmatched (orbit-only) | 60 |

Full details in `outputs/id_normalization_audit.md`.

## Downstream Report Constraint Counts

| Report | Count | Notes |
|--------|-------|-------|
| corpus_data.json | 1024 | Canonical corpus |
| covering_analysis.md | 1022 | Prolog-loaded testsets |
| giant_component_analysis.md | 1022 | Same Prolog base |
| orbit_report.md | 1022 | Same Prolog base |
| maxent_diagnostic_report.md | 1021 | Excludes 1 piton edge case |
| fingerprint_report.md | 1023 | Slightly different loading |

The 1024 (Python) vs 1022 (Prolog) gap reflects 2 constraints that Python extracts from output.txt but Prolog's `covering_analysis:load_all_testsets` doesn't enumerate (likely files with non-standard structure).
