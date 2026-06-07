# OQ-85 — Disentangling Same-Power Opposition: feasibility audit (READ-ONLY)

**Date:** 2026-06-07 · **Scope:** read-only; probes compile to temp testsets (removed), no
engine/schema/prompt edits. Evidence: `oq85_audit_probe.py` + `.out`, the two
`*.stakeholder.json`, this report. Stderr surfaced throughout (no `2>/dev/null`).

## Question

OQ-83 step-4 4b: generated same-power antagonists don't land `same-power + opposed-role`, so
`in_contention` is silent on them. Operator reframe: not "add symmetric contention" but "two
co-equal agenda_setters are an under-resolved `tangled_rope`; is there a dimension where one
pays and the other benefits?" The audit: is that dimension **in the substrate** (compute it,
dichotomy dissolved) or **needs new authoring** (relocation)? Plus the operator's prior
question: is the silence a **blind spot** or **correct**?

## 1a — Inventory (the ceiling on any decomposition predicate)

| referent | verdict | witness |
|---|---|---|
| multi-constraint / `cs_kernel` per story | ABSENT in both stories | 1 `constraint_id` each, no `cs_structure`, `network=None` |
| per-metric directionality | ABSENT (doesn't exist) | `constraint_beneficiary/victim` are `/2 (C,Agent)`, per-constraint-global (narrative_ontology.pl:72,103); no `(C,Metric,Agent)` predicate |
| Q3 `transfer_function` / `situation` | PROSE ONLY | emitted as `%` comments (generate_constraint_pl.py:638-655); only `disappearance_verdict` + `founding_problem_status` become facts |
| per-`(C,AgentA,AgentB)` authored relation | ABSENT (doesn't exist) | `in_contention` is COMPUTED; no authored `relation/transfer/pays(C,A,B)` |

**Ceiling:** the only structured per-agent directional referent is role-derived
`constraint_beneficiary/victim`. Nothing carries "same pair, different direction across sub-parts."

## 1b — The two failing stories: dual-level verdict (witnessed W2, W3)

**Constraint-TYPE level — recoverable / already-correct.** W2: both compute `dr_type = snare`
(ε=0.7, supp=0.8), NOT the authored `claimed_type: rope`. The engine sees the extraction from
the metrics; claim-vs-computed divergence is already its diff machinery's job, not a gap.

**Per-PAIR level — the between-antagonist asymmetry is prose-only.** W3: the two antagonists
appear in NEITHER structured list. streaming `STRUCT_BENEFICIARIES=[music_subscribers,
songwriters_and_publishers]`, `STRUCT_VICTIMS=[major_label_artists, independent_artists,
music_subscribers]` — `dominant_streaming_service` and `major_record_labels` in neither (both
agenda_setter, derive nothing). hospital `STRUCT_BENEFICIARIES=[large_hospital_systems]`,
`STRUCT_VICTIMS=[insured_patients, employers, small_providers]` — `large_health_insurers` in
neither. The "service pays / labels benefit", "hospital vs insurer over the rate" relation is in
`situation`+`transfer_function` prose and in no computable referent. Recovering it needs NEW
authoring → **the author-vs-derive fork is RELOCATED, not dissolved at this level.**

(Aside: streaming has `music_subscribers` in both lists — a dual-role authoring artifact;
irrelevant to the antagonist-pair verdict, which turns on the two institutional setters.)

## 1c — Prior question: blind spot or correct? → **silence-is-correct** (witnessed W4 + grep)

The relocation at the pair level only matters if the pair relation is **classification-relevant.**
It is not:

1. **`in_contention` feeds no classifier (grep, witnessed):** zero consumers outside its
   definition; `drl_core`/`constraint_indexing`/`signature_detection` read neither
   `in_contention` nor `constraint_stakeholder`. It is a relational **annotation**.
2. **The type is metric-driven and correct without it (W2 + W4-blindspot):** streaming snare,
   hospital snare; and the constructed no-anchor case `oq85_blindspot` (two co-equals BOTH
   agenda_setter, NO powerless payer, `STRUCT_VICTIMS=[]`) STILL computes `snare` from ε/supp
   alone. The classification never depended on the pair relation.
3. **The powerless anchor is present where it matters (W4):** streaming payer
   `independent_artists` (powerless), hospital payer `insured_patients` (powerless). The
   load-bearing directional fact is institutions→powerless — which the engine HAS — and the
   between-setter rivalry over splitting that surplus is second-order. (streaming even fired
   `in_contention` on a REAL asymmetric organized pair, songwriters(beneficiary) vs
   major-label-artists(payer) — the detector catches genuine asymmetry and correctly stays
   silent on the co-equal setters.)

**Positive controls both fired correctly:** W1 — a clean same-power beneficiary+payer pair fires
`in_contention` with d-split 0.25/0.85 (probe is live where asymmetry is structured); W4-blindspot
— silence + empty victims is located and real, yet the type stays correct.

**Conclusion:** `in_contention`'s silence on co-equal agenda_setters is **correct, not a blind
spot.** It is annotation; the classification (snare) is metric-driven and correct in every case
tested, anchor or not. The only thing genuinely absent is the *identity* of which co-equal
extracts from which when the victim is one of the pair (the no-anchor case) — and that is prose,
i.e. commentary, never a classification input.

## Verdict

The OQ-85 fork was a false binary **twice**: the reframe dissolved symmetric/asymmetric at the
TYPE level (under-resolved tangled_rope/snare — confirmed), and the audit dissolves the relocated
per-PAIR fork too — the relation `in_contention` can't see is one it correctly **shouldn't**
classify on, because it feeds no classifier and the type is already correct without it. Disposition
**(a) silence-is-correct** is what all evidence supports. Disposition **(b) genuine
classification blind spot was NOT found** — even the no-anchor co-equal-extraction case classifies
correctly. Authoring per-dimension between-setter directionality would add an annotation that feeds
nothing and, if ever wired to classification, would inject second-order who-wins-among-extractors
into a layer correct to ignore it.

**A6:** the recoverable-via-multi-constraint branch did not land (multi-constraint absent in both
stories) → the shared-name coupling recount does not come due here.

## Escalate (operator's; I lean (a) but do not rule)

- **Prior question:** all witnessed evidence → **(a) silence-is-correct**; no classification blind
  spot exists. Ship the asymmetric-only escape as **correct, not a limitation**: `in_contention`
  is the straitjacket detector (asymmetric collapse), the engine handles co-equal extractors via
  the metric-driven type + the real powerless payer, and the co-equal rivalry is commentary by
  design.
- **The only live sub-decision:** whether the pairwise who-extracts-from-whom (prose today) is
  worth surfacing as explicit **commentary** (annotation, never classification) — e.g. for the
  rare no-powerless-anchor case where the victim is one of the co-equals and is currently unnamed
  though the snare type is correct. This is a reporting feature, not a classification fix, and is
  the operator's call. If pursued it stays commentary-grade (R3 discipline) and authoring-side,
  never an opposition-directive (4b neutrality pin holds).
