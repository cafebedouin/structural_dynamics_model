# OQ-353 Phase 1 — pre-registration frozen, instrument refactored. Nothing measured.

**Executed:** 2026-08-24
**OQ:** OQ-353 — corpus-level statistic floors (size × content × edge-semantics control)
**Verdict (scoped to Phase 1, which runs no arms):** the pre-registration is frozen and
checkable, the instrument emits declared not-yet-measured tokens instead of fabricated verdicts,
and both arm corpora are built and witnessed — **but Phase 2 is blocked on two things this phase
could not clear: the OQ-356 human ruling, and a cut-point ruling this phase newly opened.**
**Fired:** live
**Manifest cite:** no pipeline run was made in this phase. The instrument reads existing per-leg
`classify_corpus` outputs; each row's own `manifest.code_commit` is recorded per leg in
`outputs/leg_diagnostic_table.json` → `classify_stamp`. Freeze-time state: HEAD
`e01951de21d96a4ac5d777bd64a9adea8704b2a5`, clean tree, `python/run_pipeline.py` sha256
`c23d3208a995…`, 2026-08-24T16:35:36Z.

## What this phase produced

- **`PREREGISTRATION.md`** (md5 `c057015d25f73dfff07fd38e4a5f4ef1`) — the frozen artifact: 56 declared classify-side
  statistics + 10 report stages + 5 `PENDING OQ-356` rows, four kinds, both exposure columns
  with their coverage, the B1–B5 bit vector replacing OQ-353's non-partitioning five labels, X1's
  counting rule, the arms, and 12 declared residues.
- **`python/audits/leg_diagnostic_table.py`** — six-change refactor, `--selftest` (30 checks),
  gate row `oq353 floors`.
- **Arm (a′)** `prolog/oq353_arm_astrip_haiku2/` and **arm (c)** `prolog/oq353_arm_c{1,2,3}/`,
  both built and load-witnessed; manifests committed (the dirs are gitignored and reproducible).
- **C1–C4** closed in `audits/INVESTIGATIONS.md` with their `Fired:` bits.

## The three findings that changed something

1. **The chimera seal is VACUOUS on v6** — it does not admit v6, it never looks at it. The clause
   is guarded on `cs_story_uid` being non-empty and v6 carries it on **0/3380**. A three-arm
   positive control shows the seal fires on a planted chimera, declines on coherent data, and is
   **silent on a genuine ε conflict when `cs_story_uid` is absent** — v6's exact shape. Arms
   (b)/(c) inherit an unvetted ε-coherence assumption. **The census makes it a seal defect, not a v6
property:** 14 corpora fully vacuous (7,162 stories), 9 partially checked while producing output
identical to a clean pass — `testsets_failed_…` at 0.9%, `kernel_v2_test` at 15%, and **the LIVE
leg `testsets` at 64.2%** — for **7,829 unchecked readings** in total. The partial case is the
worse one: a 15%-covered corpus is indistinguishable from a 100%-covered one at the read site.
**C1's outcome token is therefore *not evaluated* — a fourth outcome the plan's clean/warns/refuses
table does not contain**, recorded as a pre-freeze note so a later reader does not read
`Fired: live` beside a clean load and infer the seal passed v6.
2. **The cut-points cannot be set from step-0 evidence.** Step 0's implicit `≥8 / <3` band holds
   **10 of 52 statistics**, on a continuous 1.715→∞ distribution with no gap. The plan's
   cut-point stop-and-ask fires. `classify_bits()` raises `CutPointsNotRuled` rather than
   defaulting, and a fixture asserts the refusal.
3. **C2's frozen density rule would have picked the one pair the arm cannot be witnessed on.**
   Density alone selects `stealth2/stealth3`, where `giant_comp` throws. Feasibility leaves a
   singleton (haiku2/haiku3). And the density metric is **saturated at its ceiling** on three of
   five legs — it had no content to contribute on any leg in this one-seed-set roster.

## Two corrections to the plan, and one to a repo checker

- **F5's directive keyword is wrong on every leg**, including its own witness leg: the declaration
  block is `:- multifile`, not `:- discontiguous`. The mechanism F5 warns about is real; the
  named directive is not the one at risk, which makes the plan's stated integrity check one that
  cannot fail.
- **`run_pipeline.py:1092` splits `git diff --name-only` on whitespace** rather than lines;
  paths may contain spaces (37,553 lines → 37,757 tokens here). Fails closed, so it over-refuses
  and miscounts rather than wrongly accepting. **Reported, not fixed** — OQ-352 surface.
- **`pattern_citation_check`'s ±3-LINE recovery window is a ±3-ENTRY window in
  `INVESTIGATIONS.md`**, where one line is one entry. A legitimate `findall` in my C1 close
  armed a false `bound-probe` citation on an unrelated probe label three entries away.

## Declared deviation

`report_corpus`'s **hard** stamp refusal, adopted as written, refuses all 19 legs and empties the
table — including Verification 0's own diff. The two tools differ in what they are doing (join vs
tabulate). The verdict is computed in full through the same `_is_code_path`; its default
disposition here is **record, never silent**, with `--strict-stamp` restoring the hard behaviour.
Both sides are fixture-covered.

## What is still open, and who owns it

| open | owner |
|---|---|
| **OQ-356** — `giant_comp` dies on 17/20 corpora | **operator ruling.** §15 gives the conditional-consequence table per outcome and an explicitly declared interest; **no recommendation is made** |
| **Cut-points R_hi / R_lo + the B1 band** | **operator ruling** (newly opened by this phase, §8.1/§8.3) |
| **OQ-363** — `report_corpus` forwards only `giant_comp_timeout` | implementation |
| Exposure tracing: 44/56 vintage, 41/56 mixture `UNTRACED` | Phase 2 (§6) |
| X1's counting rule validated against a published `n_sibling_edges_stripped` | Phase 2, first (a′) step (§12) |

## Evidence map

| artifact | what it shows |
|---|---|
| `PREREGISTRATION.md` | the frozen list, kinds, bits, exposure, arms, residues |
| `c1_v6_load.stderr.txt.gz`, `c1_class_census.txt.gz` | C1's verbatim capture and its 11-class reduction |
| `c1_control/` + `arm_*.stderr.txt` | the three-arm chimera positive control |
| `c2_density.py`, `c2_density.txt`, `c2_density.json` | the flat ranking + feasibility filter |
| `build_strip_twin.py`, `c3_strip_build.txt`, `c3_strip_manifest.json`, `c3_declaration_lines.txt` | the two-sided strip reconciliation; 996 declaration lines enumerated individually |
| `build_arm_c.py`, `c4_arm_c_build.txt`, `arm_c_*.manifest.txt`, `arm_c_manifests.json` | arm (c) disjointness, seed, membership |
| `v0_baseline/`, `v0_after/`, `v0_diff.txt` | Verification 0 — both TSVs byte-identical (same md5) |
| `gate_run1.txt` | the gate run that surfaced the two reds, both since fixed |
| `audit_log.md` | execution order and the pre-registration md5 |
