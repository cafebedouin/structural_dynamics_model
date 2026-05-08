# Phase 1: Flags — Deferred Observations and Hypotheses

These are observations collected during Phase 1 evidence-gathering. They are
deferred from the cross-check and candidate files to avoid contaminating
the empirical record. Hypotheses here are not findings.

---

## F1: Three audit-invisible modules contain temporal indexing predicates

`coercion_projection.pl`, `pattern_analysis.pl`, and `intent_engine.pl` are not
mentioned anywhere in the audit. All three contain predicates whose sole
purpose is to access or aggregate measurement data at specific time points
(`coercion_vector/3`, `coercion_gradient/4`, `system_gradient/3`,
`analyze_interval/4`, `classify_interval/3`). The audit's coverage claim
("one clean Type A subsystem") is made without examining these modules.

Hypothesis for Phase 2: the audit's thinness claim is partly a consequence
of not having looked, not purely an architectural gap.

---

## F2: The drl_composition.pl transformation chain is entirely self-contained

The full temporal chain — `constraint_history/3` → `dr_type_at/4` →
`transformation_detected/5` → `transformation_type/6` →
`canonical_transformation/6` → `predict_transformation/3` — has no
external callers. The chain is defined, exported, and unused from outside
the module. The exception is `non_monotonic_trajectory/2`, which has one
call site in `drift_report.pl`. This raises a question about dead code vs.
intended architecture.

Hypothesis for Phase 2: the transformation chain was built as infrastructure
that was never connected to the reporting pipeline, or it was superseded by
the drift_events detector approach.

---

## F3: Two predicates are explicitly DEPRECATED but still load

`dr_type_at/4` in `drl_composition.pl` (line 170) and `classify_snapshot/3`
in `transition_paths.pl` (line 112) both carry DEPRECATED headers noting
that they use the legacy `power_modifier/2` multiplication path instead of
the sigmoid pipeline used by `drl_core:dr_type/3`. The audit note at the
top of `drl_composition.pl` says "TODO: Migrate to sigmoid pipeline. Audit
date: 2026-03-12."

These predicates are called at runtime (`dr_type_at` inside
`constraint_history/3`, `classify_snapshot` inside `degradation_chain/3`).
A constraint analyzed through the deprecated path may get a different
classification than through the canonical path.

Hypothesis for Phase 2: deprecated predicates that differ from the canonical
path constitute a Type A risk (frame drift via silently-different
classification at historical timestamps). Not a trifurcation-Type-A issue
but a code-level frame consistency issue worth flagging separately.

---

## F4: logical_fingerprint.pl defines a duplicate metric_trend/3 with different output atoms

`drift_events:metric_trend/3` returns `increasing | decreasing | stable`.
`logical_fingerprint:metric_trend/3` returns `rising | falling | stable | unknown`.

Both compute the same algorithm (findall T-V pairs, sort, compute delta,
threshold at ±0.05). The atoms differ. The logical_fingerprint version adds
`unknown` for fewer than 2 measurements.

This means two modules in the same system use different vocabulary for the
same predicate. Any code that compares trend atoms cross-module will silently
fail (e.g., checking `Trend = increasing` on a fingerprint drift component
will fail because fingerprint uses `rising`).

Hypothesis for Phase 2: this is a Type B inconsistency (structural
inconsistency within the same conceptual operation) embedded inside the one
cross-cutting module the audit classifies as containing Type A content.

---

## F5: network_dynamics.pl sits at a classification boundary

`network_drift_velocity/4` computes a temporal rate (velocity = rate of EP
change from neighbor drift). `cascade_prediction/3` computes time-to-crossing
from that rate. Both are temporal progression predicates. The audit classifies
the containing module as "Outside trifurcation" because the phenomenon it
models (network contamination via coupled purity degradation) is not named
in the trifurcation.

But the computation mechanism is Type A: it uses `drift_events:drift_velocity`
(an explicit temporal rate), accumulates contributions, and produces a
time-in-years prediction. The phenomenon is "outside" but the computational
approach is Type A.

Hypothesis for Phase 2: the audit's "Outside" classification is correct at
the phenomenon level but obscures the fact that these predicates depend on
and extend the Type A temporal machinery.

---

## F6: drift_acceleration/3 has no external call sites

`drift_acceleration/3` is defined and exported but has no call sites in the
main prolog modules (excluding testsets and archives). The audit does not
mention it by name despite examining drift_events in detail.

Hypothesis for Phase 2: this is either dead code or very recently added.
Worth checking whether any testset or Python script calls it, or whether it
was a planned extension that was not connected.

---

## F7: The coercion subsystem has a caller in report_generator.pl

`report_generator.pl` calls `coercion_projection:coercion_magnitude/3` at
line 91. The `report_generator.pl` module is the Mandatrophy Gap subsystem,
which the audit classifies as "Partial C, partial B." This means a module
classified for C/B reasons is calling into a temporally-indexed module that
the audit missed entirely.

Hypothesis for Phase 2: if the coercion subsystem is in-scope for Type A,
then the Mandatrophy Gap subsystem (partial C/B) has a hidden Type A input.

---

## F8: The detect_*/1 predicates are all unexported or unused externally

All nine `detect_*/1` predicates in drift_events.pl have zero external call
sites. They function as guards for the `drift_event/3` rules, not as public
API. The public surface of drift_events.pl for external consumers is:
`drift_event/3`, `drift_event/4`, `drift_velocity/3`, `metric_at/4`,
`metric_trend/3`. The detect predicates are implementation detail.

Hypothesis for Phase 2: the audit's description of "nine detectors" is
accurate but may overstate the modularity. The nine events are better
understood as nine rules of drift_event/3, each with a named guard.

---

## F9: infer_structural_coupling/3 is called by a module classified Outside

`drl_purity_network.pl` (classified Outside trifurcation) calls
`drl_counterfactual:infer_structural_coupling/3` at line 82 to discover
inferred edges in the constraint network. `infer_structural_coupling/3`
works by correlating temporal gradient series — it is a fundamentally
temporal computation (requires multiple measurement timestamps per
constraint).

This creates a dependency chain: a module classified Outside the trifurcation
uses a temporal-correlation mechanism to build its network topology.

Hypothesis for Phase 2: the "Outside" category in the audit may itself
have hidden Type A structure in how it constructs its inputs.

---

## F10: The transformation chain and drift events chain are parallel but separate

`drift_events.pl` detects "what is changing" (metric deltas, event types).
`drl_composition.pl` detects "what type-change happened" (constraint went
from rope to snare between T1 and T2). These are parallel Type A operations
at different levels of abstraction — metric level vs. classification label
level.

The audit examined drift_events (metric level) and classified it Type A.
It did not examine drl_composition's transformation chain (label level) at
predicate granularity. If the transformation chain is Type A, the audit
has one example of Type A at the metric level and a separate unmapped
example of Type A at the classification-label level.

Hypothesis for Phase 2: the two chains are not just Type A but represent
two distinct sub-varieties of Type A that the trifurcation description
"frame drift" could be applied to — metric-drift (the apparatus's current
model) and label-drift (what drl_composition implements).

---

## F11: pattern_analysis.pl and intent_engine.pl form a Type A pipeline

`coercion_projection.pl` → `pattern_analysis:analyze_interval/4` →
`intent_engine:classify_interval/3` → `report_generator:classify_interval/3`

This is a 4-module pipeline where each stage performs temporal computation:
- coercion_projection: access measurements at T, compute gradient
- pattern_analysis: classify interval direction
- intent_engine: classify intent pattern
- report_generator: produce report output

None of these modules are mentioned in the audit. Together they form what
appears to be a complete Type A analysis pipeline for interval-level
(multi-constraint, multi-level) temporal patterns. The audit's Type A
coverage claim appears not to have examined this pipeline at all.

---

## F12: The audit's "data availability" explanation for Type A thinness deserves scrutiny

The audit (lines 700-710) explains that Type A is thin because "the majority
of testsets contain point-in-time structural data, not longitudinal measurement
series." This may be true for the standard testsets analyzed by drift_events.pl.

However, `coercion_projection.pl` and the interval subsystem have their own
data model (`interval/3` facts with T_start/T_end, `measurement/5` facts
indexed by time, `level/1` facts). If testsets for that subsystem do exist,
they would contain longitudinal data. The audit's data-availability explanation
may apply to the constraint testsets but not to interval testsets.

Hypothesis for Phase 2: determine whether interval testsets exist and, if
so, whether they contain longitudinal data — which would undercut the
"data availability" explanation for that subsystem.
