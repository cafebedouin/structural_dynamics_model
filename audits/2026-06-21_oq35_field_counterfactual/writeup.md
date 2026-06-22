# OQ-35 — cruft-vs-wire adjudication of 6 authored-field wiring gaps (census rows 1–6)

**Execution date:** 2026-06-21
**Method:** recon → proposal (plan `review-oq-35-in-issues-md-stateful-crystal.md`) → execution → writeup.
**Governing stance:** every verdict ships with its witness; where a falsifier is specifiable the
verdict **commits** with the kill condition rather than being held open.

The 2026-05-31 Wiring-Gap Census (`audits/2026-05-31_wiring_gap_census/`) characterized the gaps but
deferred adjudication. Three of its "strip" directions were stale (engine changed underneath): the
routing-sink conversions (OQ-128/OQ-138) re-graded the signatures reading rows 2–3; OQ-109 retired the
only consumers of row 1's hardcoded path; `json_report.pl:590` became a real read site for row 4.

---

## Rows 2–3 — `accessibility_collapse` / `resistance`: **RETAIN (load-bearing router inputs)** — committed

### Why the diff variable is NOT `dr_type` alone (the central methodological point)

Post-OQ-138 the signatures these two fields feed (`false_summit_mountain`, `false_ci_rope`@routed,
`constructed_high_extraction`) were converted RECLASSIFY→ROUTE: they **revert `dr_type` to the metric
type** and emit their effect in `verdict_join.{verdict, alerts, signature_grade}`. So diffing `dr_type`
alone would show a clean 0-diff while the fields are fully load-bearing **on routing**. Witnessed
directly on the live corpus (`testsets`):

```
adjunctification_of_university_teaching_c0
  baseline : obs(snare,[constructed_high_extraction],yellow,[...],commentary)
  treatment: obs(snare,[unknown],            green, [...],commentary)
```

`dr_type` is `snare` in **both** baseline and treatment — a dr_type-only probe reports 0-diff — yet the
**signature** flips `constructed_high_extraction → unknown` and the **verdict** drops `yellow → green`.
Removing the two metrics makes the `constructed_high_extraction` detector (which reads
`accessibility_collapse`/`resistance` at `signature_detection.pl:180,182`) stop firing.

The probe's observation is therefore the full per-constraint tuple:
`obs(dr_type, sorted_signatures, verdict_join.verdict, sorted_alerts, signature_grade)`.

### Probe + controls (three directions)

`prolog/probe_oq35_field_counterfactual.pl`, driven per-corpus by
`python/audits/oq35_field_counterfactual.py` (one swipl process per corpus — controls are per-process;
`probe_harness:with_retracted/2` snapshots, restores, and runs `cache_registry:clear_all_caches/0`,
clearing the Boltzmann memo hazard):

- **Treatment** — retract `constraint_metric(_, accessibility_collapse, _)` + `(_, resistance, _)`.
- **Positive control (probe-can-see-routing)** — retract `constraint_victim/2` (flips
  `false_summit_mountain`/`false_ci_rope` severity moderate↔informational → grade + alerts change) and,
  separately, `constraint_claim(_, mountain)` (flips `constructed_high_extraction` severe↔informational).
  Passes if **either** variant moves the routing observable. Guards a broken probe, a non-cleared cache,
  and a probe blind to routing.
- **Presence control (something-to-retract)** — count `accessibility_collapse`/`resistance` facts per
  corpus; a 0-diff with presence==0 is "field absent here," **not** cosmetic.
- **Null control (observable-is-stable)** — `with_retracted([], …)`; must be byte-identical to baseline.
  Signatures and alerts are `sort`ed in the observable, so list-ordering nondeterminism cannot manufacture
  a false load-bearing verdict. Clean (0) in every corpus → the non-empty direction is witnessed, not
  assumed.

### Result (5 corpora; raw in per-corpus subdirs, `master_summary.txt`)

| corpus | N (`corpus_constraint/1`) | presence acc/res | treatment diff | pos-ctrl victim / claim | null | verdict |
|---|---|---|---|---|---|---|
| testsets (live) | 92 | 86 / 86 | 55 | 11 / 13 | 0 | LOAD-BEARING |
| testsets_haiku | 960 | 960 / 960 | 691 | 202 / 59 | 0 | LOAD-BEARING |
| testsets_flash | 960 | 960 / 960 | 537 | 201 / 67 | 0 | LOAD-BEARING |
| kernel_v1 | 1106 | 44 / 44 | 26 | 453 / 1 | 0 | LOAD-BEARING |
| original_v6 | 3380 | 465 / 465 | 421 | 1142 / 0 | 0 | LOAD-BEARING |

The archives' low presence (44/1106, 465/3380) is exactly the plan's anticipated "predate emission"
exposure — but **where the fields are present, retracting them moves the observation tuple** (26 of 44 in
kernel_v1; 421 of 465 in original_v6). `pc_claim` is 0 in original_v6 (no mountain-claim seat flips a
constructed_high there) but `pc_victim`=1142 carries the positive control; both pass.

### Verdict (committed) + kill condition

**RETAIN — `accessibility_collapse`/`resistance` are load-bearing router inputs.** The census's
"cosmetic (T.1)" was NL-override-specific and was superseded by the routing-sink conversion.

**Canonical falsifier (single statement):** a clean 0-diff on the full observation tuple in *every corpus
where presence>0 AND the positive control passes there*. Corpora where the fields were never emitted
(presence==0) cannot witness "cosmetic" and are recorded "field absent here," so the falsifier is scoped
to presence>0 corpora. **Not met anywhere** — every presence>0 corpus shows a non-empty treatment diff
with a clean null control and a passing positive control.

---

## Row 1 — `is_mandatrophy_resolved/1`: **dead facts → STRIP (operator's seat; evidence gathered, not yet executed)**

Two hardcoded facts at `narrative_ontology.pl:458–459` (`gale_shapley`, `planetary_boundaries`).

- **Zero readers (grep witness).** No goal-body or meta-call (`call/N`, `=..`) read of
  `is_mandatrophy_resolved/1` anywhere in non-archive code; every remaining hit is comment text, the two
  facts themselves, or testset doc-prose. The OQ-109 retirement removed the
  `detect_omega`/`count_unresolved_omegas`/`detect_mandatrophy_omega` consumers.
- **The only mandatrophy analytical surface is independent of the facts (code-read).**
  `enhanced_report.py:407 extract_mandatrophy_gap` scrapes the `MANDATROPHY GAP: delta_chi` line produced
  by `report_generator.pl:476 format_mandatrophy_gap/3`, which computes `delta_chi` purely via
  `compute_chi_v6/6` (base_extractiveness · f(d) · scope_modifier). It never references
  `is_mandatrophy_resolved/1`.
- **That surface is itself dead on the live corpus (Step B branch 3 — dangling consumer).** It produces
  **0** `MANDATROPHY GAP` lines (witnessed full-corpus *and* via the real `run_scenario` path). Its gate
  needs powerless≠institutional disagreement via `constraint_indexing:constraint_classification/3`, which
  holds **0 powerless** facts (1 total fact, a demo constraint) on the live corpus → the gate cannot fire.
  Logged as a dangling consumer for separate follow-up; does **not** block the fact strip.

**Conclusion:** stripping the 2 facts is output-neutral *by construction* (zero readers; producer
independent). The plan's belt-and-suspenders before/after `mandatrophy_gap` diff is unsatisfiable on the
live corpus (no non-empty `mandatrophy_gap` exists to diff) — superseded by the stronger code-read that
the facts have no readers at all.

**This is the operator's seat (revive-vs-strip is roadmap, not evidence-settleable).** D6's escape hatch
partly collapsed: revival now also requires rebuilding the OQ-109-retired consumer, raising revival cost.
The strip edit to `narrative_ontology.pl` is **not** auto-executed — it runs on the operator's go.

---

## Row 4 — `cs_reference_frame/2`: **RETAIN on the OQ-133 bet, kill condition attached**

Emitted (`generate_constraint_pl.py`), read **only** at `json_report.pl:590` — serializes the committer
t0 to JSON; **no join is computed** (offline t0→t1→t2 reconciliation deferred to OQ-133, blocked on
OQ-109/OQ-110). This is **inert consumption** (serialized, not joined), not a clean RETAIN. Rule: retain
on the OQ-133 roadmap bet, labeled a bet. **Kill condition:** when OQ-133 ships, the join either
materializes (vindicates retain) or is cut (then strip the emission). Do not strip now — it would destroy
the authored t0 anchor the deferred tier needs and remove it from the serialized committer output.

**OQ-38 correction:** its "confirmed dead: `cs_reference_frame/2`" is **stale** — `json_report.pl:590`
is a real read site (serialization). Code-read beats the stale document.
(`predict_transformation/3`, OQ-38's other confirmed-dead item, is untouched.)

---

## Rows 5–6 — confirmed by-design, no action

- `uke_scope.*` — schema-only provenance (`schema:719-737`); not emitted, not read. By design.
- `commentary.*` — emitted as `.pl` comment text + a `perspectival_gap` plunit; no facts read. By design.

---

## Disposition

| Row | Field | Verdict |
|---|---|---|
| 1 | `is_mandatrophy_resolved/1` | dead facts → STRIP (operator go pending); `mandatrophy_gap` logged dangling |
| 2–3 | `accessibility_collapse`/`resistance` | **RETAIN — load-bearing router inputs** (committed + kill condition) |
| 4 | `cs_reference_frame/2` | RETAIN on OQ-133 bet + kill condition; OQ-38 corrected |
| 5–6 | `uke_scope.*`, `commentary.*` | by-design, no action |
