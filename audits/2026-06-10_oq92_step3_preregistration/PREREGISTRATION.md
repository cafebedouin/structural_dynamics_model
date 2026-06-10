# Pre-registration — OQ-92 step 3: the authored gain-flow surface (schema + compiler + prompt)

**Status: DRAFT — awaiting two operator rulings (Q1, Q2 below). Build does not start until both
are answered; everything else here is evidence-settled or derived.** Drafted 2026-06-10.

Step 3 builds, for the first time, the authored surface the step-2 prototype validated
(`audits/2026-06-10_gain_flow_prototype/`, Outcome 1 PASS 8/8): per-constraint `gain_flow`
(named seat | explicit `diffuse` | absent) + `fixing_cost` class, per OQ-92 rulings (a)/(b).
Playbook: OQ-83 Phase A (schema dial-set → compiler emission → prompt; additive, witnessed
per stage). Classification wiring is LAST and separately gated.

## Settled preconditions (no questions here)

1. **Schema rejection of malformed gain.** A `gain_flow` naming a seat not in `stakeholders[]`
   is rejected at authoring time. JSON Schema Draft7 cannot express the cross-field reference,
   so the schema declares the shape and the COMPILER enforces referential integrity fail-loud
   (`generate_constraint_pl.py`, at the stakeholder emission block, lines ~638-648). Runtime
   absorption to fail-closed (witnessed, prototype control 8) remains as backstop, never as
   the primary check.
2. **Fabrication ban (OQ-92 Rulings block, settled by the OQ-90 HALT).** `gain_flow` is never
   synthesized — absent stays absent, fail-closed. Enforcement site: `data_repair.pl`. No
   repair/bridge/imputation clause may infer gain_flow from metrics; the existing
   `constraint_beneficiary` fabrication (`data_repair.pl:124-131`, E>0.46 ∧ S>0.40 →
   `inferred_institutional`) is the named door and must NOT be extended to the new surface.
   Witness at build time: grep data_repair for the new predicates returns only the ban comment
   (positive control: the grep fires on the comment).
3. **Estimator-classifier congruence.** Any change to `drl_core.pl:346` (scaffold clause) or
   `:373` (tangled_rope clause) lands in `maxent_classifier.pl` boolean_spec (:173, :177-179)
   in the same commit.
4. **Tri-valued provenance (OQ-92 rulings).** Authored-gain-to-NAMED-seat / explicit `diffuse`
   / absent-fails-closed. NAF over authored fields is authored-absence in disguise; piton-side
   reads consume `diffuse` positively.
5. **Diffuse-audit gate before classification wiring** (parameters = Q1).
6. **OQ-94 known-interference**: the read-site pass verdicts (`audits/2026-06-10_oq94_readsite_pass/`)
   govern which consumers may change; FORBIDDEN-bucket sites are untouchable by any capture gate.

## The build (three stages, each witnessed before the next)

**Stage A — schema.** `schemas/constraint_story_schema.json`: optional `gain_flow`
(string: stakeholder name, or literal `"diffuse"`) and `fixing_cost` (enum `cheap|prohibitive`)
— one authoring surface, two fields, per ruling (b). Pattern-5 conditionals in the OQ-83 style
(authored-empty ≠ absent). Witness: schema validates the three provenance shapes + rejects a
wrong-typed value (4 cases, Draft7, the pipeline's validator).

**Stage B — compiler.** `generate_constraint_pl.py` emits
`narrative_ontology:stakeholder_gain_flow(C, Receiver)` and
`narrative_ontology:fixing_cost_class(C, Class)`; referential-integrity check (precondition 1)
fails loud naming the ghost seat. Witness: 0-diff old-vs-new compiler on the full live corpus
(additivity — no existing story authors the fields); pilot story exercising named-seat /
diffuse / absent / malformed-REJECTED (the rejection is a witness, not an error to fix).

**Stage C — prompt.** Authoring guidance in the generation prompt: non-leaky (no thresholds
recited), keyed on the stakeholder roles; the one-shot example VARIES or OMITS the gain_flow
value (OQ-70: the example value becomes a template convention). Witness: a small generated
batch (n per Q1) compiles clean; then the diffuse audit (Q1) runs on it BEFORE anything
consumes the fields for classification.

**Stage D — classification wiring (gated on Q1 pass + Q2 ruling).** `seat_captures/2`,
`uncaptured/1`, piton-side reads per the prototype's validated shapes; OQ-90 Steps 2-4
(piton refinement in the FCR branch, `Supp ≤ 0.2` gate retirement) build on it. Benignity-family
gating per Q2. Every consumer change carries its per-site diff (build_discipline rule 3).

## Expected diffs, derived before any write (per the derive-diff-before-run rule)

Stages A-B: `pipeline_output.json` manifest UNCHANGED (no story authors the fields; emission is
conditional on presence); `validation_suite` regenerates with zero behavioral delta; compiler
0-diff is the stage gate. Stage C: new stories carry the two facts; classification UNCHANGED
(nothing consumes them until Stage D). Any deviation from these expectations is a halt-and-
diagnose, not a note.

---

## Q1 — operator ruling: diffuse-audit sample size and tolerance

Authored-`diffuse` is an authored universal negative with no checkable witness — the cheapest
token for a generation model to emit unchecked (OQ-92 Rulings block). Before the fields drive
classification: hand-audit a sample of generated `diffuse` claims for obvious capturing seats.

**The numbers are yours to set — they are a judgment about how much generation-side dishonesty
the corpus can carry, with no evidence-settled default.** Decision-relevant context, not a
recommendation: (i) the OQ-94 sort shrank the gain_flow load-bearing surface relative to
all-split (per-site, not global), which argues the tolerance need not be maximally tight;
(ii) a capture-gate at the benignity family (if Q2 rules yes anywhere) RAISES the stakes of a
false `diffuse` (it would wrongly *preserve* scaffold/CI_Rope certification); (iii) OQ-70
precedent says first-batch prevalence is authoring-convention — the audit reads individual
claims against their own story text, not rates.

**Q1 asks:** sample size N (out of the first generated batch), tolerance K (max diffuse claims
with an obvious capturing seat before HALT), and whether a failed audit halts Stage D only or
also Stage C regeneration.

## Q2 — operator ruling: the benignity-certification family (three rows, split ruling permitted)

The OQ-94 rule sorted every other live site; these three ask "is this benign coordination?" —
neither mountain-likeness nor co-occurrence. Per the format note (operator 2026-06-10): the
family is unified by its question, NOT its evidence — rule per-row; if they sort together, the
ruling says so.

| row | site | proposed gate | evidence on hand |
|---|---|---|---|
| 1 | `drl_core.pl:346` scaffold clause (+ `maxent_classifier:173`, same commit per congruence) | scaffold certification requires NOT-captured (computed from authored gain_flow) | **Witnessed misfire on the gate's side**: the step-2 prototype's capturer seats reached `scaffold` through exactly this clause (`gain_flow_prototype.out`; absent-fact twins fell to `naturalized`). The gate would have stopped a witnessed wrong-direction call. |
| 2 | `signature_detection.pl:1019` CI_Rope certification | CI_Rope requires NOT-captured | **No witness yet.** FSM intercepts mountain-metric beneficiary-bearers (priority 3 < 5), but a captured low-ε non-mountain profile can reach this gate — reachability asserted from clause order, not witnessed. Optional pre-ruling control: construct that profile and run it (cheap, prototype-style). |
| 3 | `signature_detection.pl:1122` `pure_coordination` subtype | subtype label requires NOT-captured | **No witness; commentary-grade stakes** (purity-path label, no classification override). |

**Q2 asks, per row:** gate / don't gate / defer (row 2 optionally: run the reachability control
first, then rule). A "gate" ruling on any row is implemented in Stage D only, with the per-site
diff and a control showing a captured constraint no longer certifies there AND an uncaptured one
still does (two-sided, per the verification discipline).

---

*Cross-refs: OQ-92 (rulings + graduation), OQ-94 (read-site pass + escalation), OQ-90 (piton
steps gated on this build), OQ-93 (data_repair shim family), GAP-10 (closes when the surface
lands). Drafted in worktree `wt-oq94-readsite`; merged to main with the questions OPEN.*
