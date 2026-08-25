# Audit log — OQ-342 step (3) / OQ-347 steps 2–4: the coherent 19-leg reclassify

Plan: `~/.claude/plans/next-ethereal-clarke.md`. Run row: `2026-08-25-2` in
`.claude/skills/plan-review/RUNS.md`.

## HEAD stamp at OPEN (2026-08-25, Phase 0)

```
HEAD: 7fc4b8c59e1395b4faf7c5f44b077be18e61231d  (branch: main)
git status --porcelain:
 M .claude/skills/plan-review/RUNS.md
?? agent/decompose_manifests/flat/zeuge_witness_protocol_2026_20260824_150509.manifest.json
```

**Planning HEAD was `cae508f1a`; 8 commits landed between planning and open** (the OQ-375
cross-artifact-reconciliation session, `5cb9c2132`…`7fc4b8c59`). Checked:
`git diff --name-only cae508f1a HEAD -- 'prolog/*.pl'` minus `testsets*`/`tests/*` is **empty** —
no engine Prolog changed, so the plan's S5/S6 zero-diff warrant transfers to the new HEAD
unchanged. Changed files are docs/trackers/gate.sh plus two new analysis scripts
(`python/shared/recon.py`, `python/audits/oq375_wu_recon.py`) outside the classify path.

## Sole-writer check

- **Own writes accounted first:** the ` M RUNS.md` line is this executor's own Step-0 append
  (run row `2026-08-25-2`) — excluded from the sole-writer read per the plan.
- The untracked `agent/decompose_manifests/flat/zeuge_…20260824….manifest.json` predates this
  session (dated 2026-08-24, present in the session-start git snapshot) — a leftover topic-run
  manifest, not an active writer.
- **A second writer WAS active today**: the session-start snapshot showed dirty
  `audits/INVESTIGATIONS.md`, `build_discipline.md`, `gate.sh` etc., all now committed as the
  OQ-375 session (RUNS.md row `2026-08-25-1`, post-impl annotation landed at `793b93f99`) — that
  run appears COMPLETE.
- `ps` shows **5 live `claude` processes** (one is this executor). The others' activity state is
  unknowable from here; the OQ-375 run's completion suggests idle sessions, but this is a
  RECORDED RISK for the Phase-4 freeze window, not a cleared check. Mitigations already in the
  plan: `testsets` classified last with its count stamped at the moment of use; per-leg
  `code_dirty` assertions on all 18 coherent-set legs (a concurrent write would flip them and
  stop the run — the check is live, not narrated).

## S6 positive control (Phase 0 step 5) — PASSED

```
$ grep -n "use_module.*giant_component_analysis" prolog/json_report.pl   # test half
(no output, exit 1)
$ grep -n "use_module.*purity_scoring" prolog/json_report.pl             # control half
20::- use_module(purity_scoring, [purity_score/2]).                      (exit 0)
```

The instrument demonstrably finds an import when one exists and reports
`giant_component_analysis` absent. S6 is now a **tested absence**; the zero-diff expectation for
unchanged legs is warranted.

## Phase 0 preservation — DONE

Four §9 arms preserved to `outputs/_arms_oq345_2026-08-25/*.gz`, roundtrip md5s identical;
md5s + manifests + the explicit §9 leg→commit pin (`haiku` @ `0f432fb`, `flash` @ `2ce8e18`) in
`preserved_arms.md` (this directory).

## INVESTIGATIONS line — OPENED

Prepended to `audits/INVESTIGATIONS.md` 2026-08-25: *does a coherent reclassify of all 19 legs at
clean HEAD reproduce the 15-commit dirty-tree artifacts' `per_constraint` exactly, and how large
is the situation-fixed core?*

## Substrate table verification (S1–S22)

- **S1 ✓** LIVE_LEGS imports, 19 entries; on-disk `prolog/testsets*` dirs are exactly that set.
  One nuance vs the plan's S18 phrasing: LIVE_LEGS itself already CONTAINS `testsets`, so
  `declared_roster()`'s union with `{"testsets"}` is idempotent (roster still 19; harmless).
- **S2 ✓** per-leg counts match the plan's table exactly (incl. `testsets` 285, counted not recalled).
- **S3 ✓** seed pool = 1005. **S4 ✓** (16 distinct commits counting the sonnet2 rerun artifact;
  15 across the 18 legs; 16/19 `code_dirty: True`). **S5 ✓** re-run per commit at open-HEAD
  `7fc4b8c59`: engine delta is exactly `prolog/giant_component_analysis.pl` for ALL artifact
  commits (pasted in session). **S6 ✓** tested absence (control half hits `purity_scoring` at
  json_report.pl:20). **S7 ✓** read (fresh swipl per call; shared raw artifact).
- **S8 QUALIFIED-CONFIRM + EXTENSION (reported, not silently adapted):** absent ε = 0 on EVERY
  model-leg artifact (plan's operative claim holds; genuine 0.0s abundant, instrument
  discriminates). NEW: the canonical `outputs/pipeline_output.json` (live testsets leg, n=285)
  carries **27 absent-ε records** (raw twin 27; dead `pipeline_output_testsets_head.json` 11) —
  outside the plan's 22-file glob. Consequence recorded in OQ-377: a coercion site reading the
  CANONICAL artifact has a live absent-ε input today. No analysis-leg number is affected;
  testsets is excluded from Phases 6–8 anyway.
- **S9 re-derived:** 24 sites incl. the two fixed; **22 sites / 14 files remain** (census in
  OQ-377; `phase0_data.py:87` carries both shapes in one line; stale worktree copies under
  `.claude/worktrees/oq-48-recalibration/` excluded from census).
- **S10 ✓** (dead `_head.json` family at :29–34; literal 4-tuple at :98 → OQ-376).
- **S11/S20 ✓** (`--run-tag` in gemini/sonnet/stealth drivers; absent in kimi; `--leg-name` in
  sonnet + stealth). Read-back for nemotron: **PASSED** — field 5 of a landed rescue story reads
  `no_scope_rebuild_nemotron+seed_rescue1`, single `+`.
- **S12 ✓** (AFTER arms @ `0f432fb`/`2ce8e18` — see preserved_arms.md). **S13 ✓** (1 open line,
  the placeholder, before this session's own line). **S14 ✓** (18 entries, no testsets key).
  **S15 ✓ S16 ✓** (read in run_pipeline.py). **S17 ✓** byte-exact signature histogram on sonnet2
  (599/347/39/9/5/3/1; non-null 1003/1003). **S18 ✓** (with the S1 nuance above; new helper
  `output_name_for()` added so the coherent sweep cannot fork the name mapping). **S19 ✓** (read
  `load()`: hardcoded `outputs/pipeline_output.<leg>.json`, print-and-skip, id-keyed).
  **S21 ✓** (rescue1 180 / stakeholder_backfill 661 / seed_rescue1 0 at open).
- **S22 ✓ all three traps re-verified via PROV_RE** (multi-line-safe): flash 754+206 under DRIVER
  name `no_scope_rebuild_gemini*`; haiku 505 bare `no_scope_rebuild` + 455; nemotron 852+144
  (+seed_rescue1 landing live); stealth 969+36.

## Phase 1 log

- T0 = 2026-08-25T06:49:53Z (pilot launch, `--n 1`). Pilot seed
  `divine_legitimacy_substrate__folk_syncretistic_reading` FAILED schema validation 3× (negative
  values below minimum 0) — one of the 9 already-failed-once residue seeds. Driver mechanism
  viable (API round-trips, responses persisted, validation firing).
- **T+15 boundary decision (act-then-report):** at T+15 the only attempted seed was the pilot's
  known-hard one; abandoning Phase 1 on that would misfire the viability check's purpose (the
  plan's own pilot-first read-back sequencing consumed the window). Proceeded to the full pass at
  06:57:38Z with **T+45 = 07:34:53Z as the binding hard stop**.
- Full nemotron pass (`--n 0`, 9 seeds, tag `seed_rescue1` bare): running; read-back PASSED on
  first landed story. nemotron_think (2 seeds) to follow, serialized.
