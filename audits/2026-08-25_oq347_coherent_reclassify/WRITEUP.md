# WRITEUP — OQ-342 step (3) / OQ-347 steps 2–4: the coherent 19-leg reclassify, the situation-fixed core, and OQ-348

**Executed:** 2026-08-25
**OQ:** OQ-342 step (3); OQ-347 steps 2–4; OQ-348; OQ-345 (shortfall recording)
**Verdict (one line, scoped):** IN EXECUTION at this header's first commit (Phases 0–3 complete:
arms preserved, S6 control passed, sync rescues landed 6/11, harness fixed/built, tree frozen) —
the reclassify verdict lands in this file at close, this session.
**Manifest cite:** the 19 pre-existing leg outputs span 15 engine commits, 16/19 `code_dirty:
True` (re-derived this session, audit_log.md §Substrate S4/S5); the coherent set's own manifests
are cited in the Phase-4/5 sections below at close.
**Fired:** latent — provisional at freeze, re-evaluated at close: the S8 re-derivation already
surfaced a real defect population conditional on an unproduced consumer (27 absent-ε records on
the CANONICAL `outputs/pipeline_output.json`, coercible to 0.0 by any of the OQ-377 census sites
that reads the canonical artifact; no analysis-leg number affected).

## Evidence map

| artifact | what it shows |
|---|---|
| `audit_log.md` | HEAD stamps (open `7fc4b8c59`), sole-writer check, S1–S22 substrate verification incl. the S6 positive control (PASSED) and the S8 qualified-confirm |
| `preserved_arms.md` | the four §9 arms preserved with md5s; the §9 leg→commit pin (`haiku` @ `0f432fb`, `flash` @ `2ce8e18`) |
| `leg_shortfall.md` | Phase-1 rescue results: nemotron 996→1000 (4 landed / 5 short, ids listed), nemotron_think 1003→1005 (complete); OQ-58 sweeps (1 quarantined edge each); read-back witnesses |
| `outputs/_arms_oq345_2026-08-25/*.gz` | the preserved arm blobs (gitignored; md5s committed) |
| `python/audits/oq347_coherent_reclassify.py` | the Phase-4 sweep driver (serial classify_corpus at one frozen HEAD, derived expected_model, coherence assertions) |
| `python/audits/oq347_prereg_diff.py` | the Phase-5 pre-registered diff (per_constraint only, two strata) |
| `python/audits/situation_fixed_core.py` | OQ-347 step 4 (same-model pairs derived from provenance; null column per OQ-51) |
| `python/audits/permutation_null.py` | OQ-348 (seed-label permutation, recorded RNG seed) |

Sections for Phases 4–9 are appended at close.
