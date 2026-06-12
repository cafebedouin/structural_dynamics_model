# OQ-110 — Offline residual join + D-fork decision package

**Executed:** 2026-06-11 (local) on worktree branch `oq110-residual-join`.
**Substrate (cited per manifest, never memorized):** `outputs/pipeline_output.json` —
`pipeline_run_at 2026-06-12T00:59:49Z`, `code_commit c22ec561` (`code_dirty: false`),
`n_constraints 62`, `schema_version 2`. The pipeline was re-run fresh for this audit because the
prior output was produced at `25d6a637` on a dirty tree; flip totals were identical across the
two runs (91 context-level flips / 20 fab-adjacent / 34 constraints), so the post-`25d6a637`
code changes (incl. the `transition_paths.pl` determinism guard) did not move the residual.
(*Caveat: that is a TOTALS comparison, not a per-flip identity check — a comfort note only;
do not cite it later as "the runs were equivalent." Everything downstream ran on the fresh
manifest.*)

**Question:** classify backed observer flips as ε-explained vs ε-unexplained; join against
committer drift stages; deliver the author-vs-derive D-fork (branch b: time-varying role/d)
decision package for the operator's ruling. Pre-registered criterion: plan §1.3 (pinned at
plan approval); outcome meanings written before the run.

---

## 1.1 Backed end-to-end verification (inherited from OQ-33/OQ-46 closes)

Trace verified against code at HEAD: `drl_composition.pl:189` `classify_at_time/5` →
`snap(D, Backed, Eps, Supp, Theater)` with `Backed = EpsBacked ∧ SuppBacked` (`:256`) →
`temporal_residual.pl:70–85` `scan_transitions/3` (both endpoints backed, else
`fabrication_adjacent`) → `json_report.pl:807–846` (`json_report.pl` is a **non-module**
script; its predicates live in `user`) → `pipeline_output.json` `temporal_residual` blocks,
exporter-gated on `measurement/5` presence (`json_report.pl:482`; null = no-temporal-data,
distinct from looked-and-found-none).

Witnesses (artifacts in this directory; all controls per-process):

- **Control A (fab_adjacent EXCLUDED)** — `backed_semantic_probe.out`: `agenda_conditioning`,
  powerless ctx. Suppression series authored at T={0,20,40,50}, absent at T={10,30} with scalar
  0.72 present and no static marker → SuppBacked=false (OQ-105 misalignment bucket). The
  tangled_rope→snare change at t=20→30 spans the unbacked t=30 endpoint and is EXCLUDED from
  flips, counted `fab_adjacent=1`. PASS.
- **Control B (backed flip PRESENT)** — same file: `adjunctification_of_university_teaching`,
  powerless ctx, flip(10,20,tangled_rope,snare) present with deltas 0.14/0.04/0.11 matching the
  serialized JSON. PASS.
- **Control C (Backed bit GATES bucketing)** — same file: `probe_harness:with_retracted` of the
  eps measurement @t=20 removes the flip (t=20 type reverts under the flagged ε-default, so the
  pair leaves both buckets); verified restore returns it. PASS.
- **Full-corpus e2e diff** — `backed_e2e_diff.out`: independent in-process recomputation
  (fresh swipl, same serializer) is **identical over all 62** constraints (2 null =
  zero `measurement/5` facts: `employment_boundary_contradictions`,
  `human_dignity_ai_governance_contradictions`). Comparator positive-controlled (planted
  mutation flagged).

## 1.2 The join (`python/audits/oq110_residual_join.py` → `oq110_residual_join.json`)

- **Coverage carried to the read site (n=62):** both-surfaces=11, flips-only=23,
  stages-only=4, neither=24; partition-sum checked. The plan's "~15/62 both" re-derived to
  11/62 on this substrate.
- **Flips:** 91 context-level flip events over 34 constraints; 20 fabrication-adjacent
  EXCLUDED (OQ-46 buckets not re-merged).
- **OQ-105 cross-read:** misaligned suppression rows re-derived from authored grids
  (`metric_grids_export.pl` → `oq110_metric_grids.json`): 23 rows / 11 constraints on this
  corpus (census of record was 21/10 on the 48-file corpus; all named-10 still live and
  re-derived; one new host from corpus growth: `institutional_trust_erosion`). Counted flips
  ON a misaligned row: **0** (armed halt; would contradict the Backed gate verified in 1.1).
  Counted flips ADJACENT to a misaligned row: **0**. No flip in this package carries
  OQ-105 timing risk.
- **Inherited flags carried verbatim** (all three constraints are kernel_v2_test-regime and
  **not in the live corpus**, so none touches a live counted flip):
  `clinical_deskilling_automation` 0→2 = documented exclusion; `milblogger_legitimacy_erosion`
  12→18 = clean; `challenge_as_commons_maintenance` T=5 unflagged (touches no counted flip).
- **Stage join is presence-level:** committer moments are NAMED atoms
  (e.g. `platform_economy_emergence`); no numeric moment↔t mapping exists and none was
  fabricated.

## 1.3 Pinned counterfactuals (91 backed flips × 2 pins)

Per-flip results: `oq110_pin_results.tsv`; buckets: `pin_aggregate.out` /
`oq110_pin_aggregate.json`. Controls, all per-process: overlay-took-effect read-back on every
pin; identity-pin (re-pin eps@T2 with its own value) survives — the overlay path does not
perturb classification; expected-vanish (adjunctification t=10→20, d_eps=0.14) reverts to
tangled_rope under ε-pin. Enumeration identity: the in-process flip set equals the join
inventory (91 = 91, set equality). Zero classify failures; zero third-type pinned outcomes
(the criterion's "does NOT produce the type change" ambiguity never materialized — every
ε-explained flip reverted exactly to its From type).

**Implementation-correction note (criterion NOT amended):** v1 of `pin_aggregate.py` printed a
final verdict keyed to the genuinely-unexplained count; the pre-registered text keys the 0/>0
fork on the **ε-unexplained** count (survivors of ε-pinning). The line was corrected to the
pinned definition; buckets and per-flip verdicts were never affected. This is a
**bug-to-spec repair, not an amendment**: the pinned text was the authority the code was
corrected *against* — the case study for what "halt-and-escalate, never inline-amend" looks
like when the defect is in the implementation rather than the criterion (the buckets stayed
untouched and the package escalated instead of self-closing).

| bucket | count | meaning (pre-registered) |
|---|---|---|
| ε-explained | 82 | flip vanishes under ε-pin |
| supp-explained residual | 9 | survives ε-pin, vanishes under supp-pin — "NOT evidence for time-varying d" (criterion text) |
| genuinely unexplained residual | **0** | survives both single pins — the only bucket that could evidence time-varying role/d |

### The 9 ε-unexplained residuals (all supp-explained)

Every residual is the **same mechanism**: analytical/civilizational/analytical/global context,
tangled_rope→snare, with the authored suppression series crossing the snare suppression floor
(0.60) between T1 and T2 while ε stays sub-critical at that seat. Supp-pin reverts all 9 to
tangled_rope.

| constraint | t1→t2 | supp t1→t2 | eps t1→t2 | committer stages |
|---|---|---|---|---|
| agricultural_contract_grower_lockin | 0→10 | 0.58→0.65 | 0.52→0.64 | none |
| garment_supplychain_audit_theater | 0→5 | 0.58→0.61 | 0.58→0.63 | none |
| optimization_artifact_risk | 3→6 | 0.58→0.66 | 0.54→0.62 | none |
| platform_flexibility_precarity_tradeoff | 6→9 | 0.58→0.62 | 0.52→0.58 | none |
| retirement_security_deficit | 6→9 | 0.55→0.60 | 0.50→0.55 | rf=formal_employment_default, moment=platform_economy_emergence, terminal=husk, gap=practice_drift/substantial/unack |
| technocratic_paradigm_resistance | 0→3 | 0.58→0.64 | 0.52→0.59 | rf=catholic_social_doctrine_pre_digital_era, moment=contemporary_ai_deployment_era, terminal=stable_pattern, gap=practice_drift/substantial/ack |
| truth_as_common_good | 6→9 | 0.58→0.62 | 0.51→0.58 | rf=imago_dei_anthropology, moment=contemporary_digital_era, terminal=husk, gap=practice_drift/substantial/unack |
| veto_asymmetry | 15→30 | 0.58→0.64 | 0.54→0.59 | none |
| work_dignity_automation | 20→30 | 0.56→0.60 | 0.50→0.55 | rf=imago_dei_anthropology_pre_industrial, moment=contemporary_ai_automation_era, terminal=husk, gap=practice_drift/substantial/unack |

Committer-stage alignment of the residual set: 4/9 carry authored drift stages (all
practice_drift / substantial; 3 husk-terminal unacknowledged, 1 stable_pattern acknowledged);
5/9 author no committer stages. Presence-level only — named moments admit no timing comparison
against the integer flip times. **Coverage caveat for any future reopen evaluation:** the
stage-alignment dimension rested on both-surfaces = 11/62 — UNDERPOWERED by coverage, not
null; thin evidence, not absent structure.

## 1.4 D-fork decision package (the ruling is the operator's)

Pre-registered outcome meaning realized: ε-unexplained residual count = 9 (> 0) → the bucketed
residual set escalates as the branch-b decision package; **nothing auto-opens**.

What the package shows: the residual set is exhausted by suppression-floor crossings — the
bucket the criterion pre-declared "NOT evidence for time-varying d" — and the
genuinely-unexplained bucket (the only one that could evidence time-varying role/d) is **empty**.
No flip carries OQ-105 timing risk. If branch b is nonetheless opened, the build shape is the
ruled PAIR: C2 `frame_policy` + C1 time-indexed d via the existing stubs
`constraint_indexing.pl:435–441` (`effective_time/3`, identity, must stay deterministic) and
`:426–434` (`time_indexed_directionality_source/4`, dynamic, zero facts) — source and policy
gate land together or not at all, folded into OQ-109 Phase C scope so the four-tuple-only
minority regens once.

**Operator ruling (2026-06-11):** branch b does NOT open — OQ-110 resolved with derived-d
standing. Rationale of record: the supp-pinned distinction existed in the package precisely so
supp-floor crossings could be ruled not-evidence without amending the criterion, and that is
the entire residual set; genuinely-unexplained = 0 is the criterion's no-open condition in
substance, while the 9-flip package escalated exactly as the residual->0 path required.
Nothing foreclosed: the C1/C2 stubs persist, pair-or-nothing stays satisfiable later.
**Reopen condition (commit-plus-kill-condition):** ≥1 backed flip on a future residual join
(post-regen substrate, manifest cited) surviving BOTH ε-pinning and supp-pinning. The
inherited Backed-verification deposit chain OQ-33 → OQ-46 → OQ-83 → OQ-110 terminates here —
it lives nowhere else. Close note: ISSUES.md OQ-110.

## Artifacts

| file | content |
|---|---|
| `backed_semantic_probe.{pl,out,stderr}` | 1.1 controls A/B/C |
| `backed_e2e_export.{pl,out,stderr}`, `oq110_residual_inprocess.json`, `backed_e2e_diff.{py,out}` | 1.1 full-corpus identity diff |
| `metric_grids_export.pl`, `oq110_metric_grids.json` | authored per-metric time grids (OQ-105 cross-read input) |
| `../../python/audits/oq110_residual_join.py`, `oq110_residual_join.{json,out}` | 1.2 join |
| `pin_counterfactuals.{pl,out,stderr}`, `oq110_pin_results.tsv` | 1.3 pin runs (91 × 2) |
| `pin_aggregate.py`, `oq110_pin_aggregate.json`, `pin_aggregate.out` | 1.3 buckets |
