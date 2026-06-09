# Does the DR classifier ever return a clean pass, or is coordination-washing its default?

Audit date: 2026-06-08. Settled against the substrate (the `.pl` rules + a live engine run),
not the reports or manifest. Every claim below carries its witness in-turn.

Corpus under test: 18 testsets on disk (`prolog/testsets/*.pl`), of which the
magnifica_humanitas run is the 11 reports stamped 17:08. Engine run:
`outputs/pipeline_output.json` manifest = `pipeline_run_at 2026-06-08T22:25:01Z,
n_constraints 18, code_commit 025e71d, code_dirty true`.

---

## VERDICT (the disjunction settled)

**CLEAN PASS EXISTS.** The classifier is not unconditionally coordination-washing.
Two corpus members pass cleanly:

```
collapse_mechanism_ambiguity   | claimed mountain -> classified ROPE | sig coupling_invariant_rope | maxent rope (agrees)
demographic_skill_mismatch     | claimed mountain -> classified ROPE | sig coupling_invariant_rope | maxent rope (agrees)
```

Engine witness (my own swipl run, read-only, `constraint_signature/2` queried directly):

```
collapse_mechanism_ambiguity | sig=coupling_invariant_rope | eps=0.08 | boltz=compliant(0)
demographic_skill_mismatch   | sig=coupling_invariant_rope | eps=0.15 | boltz=compliant(0)
```

`coupling_invariant_rope` is the engine's POSITIVE certification of true coordination
(signature_detection.pl:114–118, 941–961: Boltzmann-compliant + scope-invariant + has a
coordination function). Both reach a final `dr_type = rope` with MaxEnt agreement (no hard
disagreement, no override). These are genuine clean passes through the full stack.

**BUT the washing is the default for the magnifica-11, and they are not anomalies.** All 11
have measured cross-context coupling above threshold, so all 11 wash. The mechanism is real,
the priors push undata'd inputs into washed territory, and the disconfirming subsystems are
relabeled "confirmatory" inside the engine. Details below.

---

## 1. Harness positive control (my run reproduces the report)

Direct `constraint_signature/2` + `base_extractiveness/2` query reproduces the flat_control
report's signature and ε exactly:

```
human_dignity_ai_governance_flat_control | sig=false_ci_rope | eps=0.48 | boltz=non_compliant(1.0,0.29)
secular_humanist_reading                 | sig=false_ci_rope | eps=0.28 | boltz=non_compliant(1.0,0.33)
techno_optimist_reading                  | sig=false_ci_rope | eps=0.68 | boltz=non_compliant(0.75,0.3)
```

Report JSON for flat_control: `structural_signature=false_ci_rope`, pipeline ε=0.48. Match.
Re-run produced byte-identical signatures (determinism, §8). Everything below is witnessed.

## 2. The decisive clauses

**Signature priority (signature_detection.pl:70–125).** First match cuts:
1. `false_natural_law` (FNL) — line 70
2. **`false_ci_rope` (FCR) — line 77** ← checked BEFORE clean rope
3. `false_summit_mountain` (FSM) — line 99
4. `natural_law` via emergence — line 109
5. **`coupling_invariant_rope` (clean) — line 117** ← only reached if 1–4 all fail
6. profile fallback → natural_law / coordination_scaffold / piton / constructed_low /
   `constructed_high_extraction` / ambiguous — line 121

**FCR gate (signature_detection.pl:1101–1130).** Fires iff:
```
false_ci_rope(C, ...) :-
    appears_as_rope(C, _),          % low ε (≤ rope_epsilon_ceiling 0.45) OR explicit rope claim
    collect_fcr_failures(C, F), F \= [].   % ≥1 Boltzmann test fails
```
Fires when a rope-appearing constraint fails ANY of: Boltzmann non-compliance, scope variance,
excess > floor (0.05), nonsensical coupling. — *Does not fire* only if all four pass.

**Boltzmann compliance (boltzmann_compliance.pl:94–103).**
```
compliant   if cross_index_coupling(C) =< complexity_adjusted_threshold(C)
non_compliant otherwise
```
Coupling = fraction of (Power×Scope) grid cells where classification fails to factorize
(12 contexts, lines 174–205). Coupling 0 ⇒ compliant; any classification flip across the grid ⇒
coupling > 0. — *Fires non-compliant* when the constraint's type depends on observer position.

**Clean rope (signature_detection.pl:941–961).**
```
coupling_invariant_rope(C, ...) :-
    boltzmann_compliant(C, compliant(_)),   % coupling ≤ threshold (~0.25–0.33)
    scope_invariance_test(C, invariant),
    has_coordination_function(C).
```
— *Fires* only for compliant + scope-invariant + authored coordination function.

**Type assignment / override (signature_detection.pl:755–795).** FNL→tangled_rope;
FCR→tangled_rope unless perspectival variance preserves the metric type; constructed_high +
mountain→tangled_rope, +unknown→snare. — the signature *overrides* the metric dr_type.

**Prior imputation for missing vectors (domain_priors.pl:60–67; get_metric_average
signature_detection.pl:166–173).**
```
get_metric_average(C, M, 0.5) :- no constraint_metric(C, M, _) facts.   % "Default if no data"
get_prior(_, _, 0.5).                                                   % catch-all neutral default
```
— *Fires* whenever an authored vector is absent: the value 0.5 is fabricated, not measured.
`snare_epsilon_floor = 0.46`, so a defaulted ε (0.5) **exceeds the high-extraction floor.**

**"Confirmatory" relabel (diagnostic_summary.pl:422–439, pattern P5).**
```
expected_conflict_pattern(C, boltzmann, non_compliant(...), DetType,
    constructed_non_compliance,
    'Constructed types couple dimensions deliberately; non-compliance is confirmatory') :-
    constructed_type(DetType),                 % the verdict is already "constructed"
    ( ... coupling pairs are power_scope form ; true /* benefit of the doubt */ ).
```
— *Fires* once the signature has decided constructed/false: the Boltzmann disconfirmation is
re-labeled confirmatory, **gated on the verdict itself**, with a benefit-of-the-doubt fallback
(line 438) when the guard cannot be checked. This is in the RAW engine (`diagnostic_verdict`
field of pipeline_output.json), NOT manufactured by enhanced_report.py.

## 3. Clean-pass reachability (static)

A non-washed verdict IS reachable. Trigger: `appears_as_rope` + `compliant` (coupling ≤
threshold) + `scope_invariant` + excess ≤ 0.05 + no nonsensical coupling + authored
coordination function → FCR's `collect_fcr_failures` returns `[]` → FCR (#2) does not fire →
control reaches `coupling_invariant_rope` (#5).

**For a constraint with non-trivial coupling (coupling > threshold ~0.25–0.33), there is NO
clean rope path.** `coupling_invariant_rope` requires `compliant(_)` by construction, so any
coupling above threshold is intercepted by `false_ci_rope` at priority #2 and cut before #5 is
tried. Foreclosing clause: `constraint_signature(C, false_ci_rope) :- false_ci_rope(C, _), !`
(line 77). This is the engine's *definition* of coordination-washing — classification that
varies with which observers you condition on — not a bug. Whether the coupling test is too
aggressive (e.g. demoting genuinely coupled methodological kernels, cf. OQ-88) is the live
design question; it is not the question of whether a clean path exists. It does.

## 4. The green search (empirical, all 18)

Signature per constraint, from the engine's own `pipeline_output.json`:

```
ID                                  | eps  | coupling score / boltz       | signature
collapse_mechanism_ambiguity        | 0.08 | 0   / compliant              | coupling_invariant_rope   ← CLEAN
demographic_skill_mismatch          | 0.15 | 0   / compliant              | coupling_invariant_rope   ← CLEAN
human_dignity_..._flat_control      | 0.48 | 1.0 / non_compliant          | false_ci_rope
magisterial_integralist_reading     | 0.42 | 0.75/ non_compliant          | false_ci_rope
pluralist_pragmatic_reading         | 0.42 | 0.75/ non_compliant          | false_ci_rope
secular_humanist_reading            | 0.28 | 1.0 / non_compliant          | false_ci_rope
techno_optimist_reading             | 0.68 | 0.75/ non_compliant          | false_ci_rope
wage_convergence_sustainability     | 0.28 | 0.375/non_compliant          | false_ci_rope
digital_power_concentration         | 0.78 | 1.0 / non_compliant          | constructed_high_extraction
optimization_artifact_risk          | 0.68 | 1.0 / non_compliant          | constructed_high_extraction
platform_flexibility_precarity      | 0.58 | 0.875/non_compliant          | constructed_high_extraction
proxy_measurement_validity          | 0.48 | 1.0 / non_compliant          | constructed_high_extraction
surveillance_control_freedom        | 0.68 | 1.0 / non_compliant          | constructed_high_extraction
technocratic_paradigm_resistance    | 0.68 | 1.0 / non_compliant          | constructed_high_extraction
truth_as_common_good                | 0.58 | 1.0 / non_compliant          | constructed_high_extraction
war_normalization_ai_weapons        | 0.68 | 1.0 / non_compliant          | constructed_high_extraction
work_dignity_automation             | 0.58 | 1.0 / non_compliant          | constructed_high_extraction
human_dignity_..._contradictions    | none | inconclusive                 | constructed_high_extraction
```

Distribution: **2 clean (coupling_invariant_rope), 6 false_ci_rope, 10 constructed_high_extraction.**
The single discriminator is coupling: the 2 clean have coupling score **exactly 0**; the 16
washed all have **0.375–1.0**, all Boltzmann non-compliant.

**Within the magnifica-11 specifically: zero clean passes** — every one has coupling ≥ 0.375.
The clean passes are two OTHER corpus members (a World3 collapse-mechanism kernel and a
demographic-skill kernel). Note also the 17:08 report header `Types: 1 mountain, 4 rope,
6 tangled_rope, 3 snare` (the final dr_type distribution of the 14 testsets loaded at 17:08):
clean final types (mountain, rope) plainly appear there too, so even by post-override dr_type
the corpus is not uniformly washed.

## 5. Prior isolation (the priors carry the signature)

Synthetic near-empty constraints, classified with no authored vectors:

```
PROBE_BARE  (only constraint_claim(_, rope), no metrics): sig = false_ci_rope
PROBE_EMPTY (nothing asserted at all):                    sig = constructed_high_extraction
```

- A bare rope *claim* with zero metrics already certifies **false_ci_rope**: `appears_as_rope`
  fires on the claim, and `collect_fcr_failures` finds a failure from imputed/default structure.
- A constraint with *no data whatsoever* defaults to **constructed_high_extraction**, because
  `get_metric_average` returns 0.5 for the missing extraction vector and `0.5 ≥
  snare_epsilon_floor (0.46)`. The neutral default sits above the high-extraction floor.

So the verdict is produced largely by the imputation machinery: the priors carry the signature,
and the read site cannot distinguish "measured high extraction" from "no data, defaulted to
0.5." This is Build-Discipline Pattern 5 (absence satisfies the gate) operating on the ε floor.
The flat_control verdict riding "28 prior-imputed vectors" is an instance of this — those
vectors flow through `data_repair.pl:277 get_prior` → 0.5 defaults.

## 6. Enhanced-vs-raw

The "Expected Conflicts … confirmatory" framing the operator flagged is **already in the raw
engine output** (`diagnostic_summary.pl` P5, surfaced in pipeline_output.json's
`diagnostic_verdict`). enhanced_report.py did NOT invent it; it faithfully relays it. So this is
not a report-layer over-claim — it is an engine-layer circularity: the verdict defines its own
disconfirmers as confirmers (§2, P5). One genuine strengthening to watch: flat_control's report
leads with confidence figures (rival_p 0.999) while the underlying margin is 0.0001 — borderline
presented as near-certain; that is a presentation choice in the report, the raw confidence_band
is "borderline."

## 7. Provenance

There is no single "JSON manifest of exactly eleven." The 11 = the report cohort stamped 17:08;
each of the 11 has a matching `prolog/testsets/<id>.pl`, `json/<id>.json`, and
`outputs/constraint_reports/<id>_report.{md,json}`. The live corpus is **18**, not 11
(pipeline manifest `n_constraints=18`), because three earlier testsets (16:34/16:36) and three
later (17:18–17:25) bracket the magnifica run. Audits against "the corpus" must cite the 18-count
manifest, not the 11. No orphan/drop among the 11 themselves.

## 8. Falsifiability & determinism

- **Deterministic:** re-running `constraint_signature/2` produced identical signatures
  (flat_control=false_ci_rope, both clean=coupling_invariant_rope) — priors are fixed facts, no
  stochasticity.
- **Falsifiable — demonstrated:** the classifier is NOT a constant function. Two real inputs
  (coupling 0) yield `coupling_invariant_rope`; the probe inputs yield different washed types by
  mechanism. An input with coupling ≤ threshold + coordination function + scope-invariance +
  ε ≤ 0.05 excess flips the verdict to clean. The washing is conditional on measured coupling,
  not imposed unconditionally.

---

## 9. Follow-ups closed (2026-06-08, same session)

**[CLOSED] Is the "confirmatory" relabel independent corroboration?** No — it is circular.
`constructed_type/1` (diagnostic_summary.pl:51–54) is a STATIC membership fact
(`constructed_type(scaffold|snare|tangled_rope|piton)`), not a computation. The P5 relabel's
guard `constructed_type(DetType)` is true precisely when the override already routed the
constraint to a constructed type — which the coupling gate / FCR did. The Boltzmann
non-compliance it relabels "confirmatory" is the SAME `cross_index_coupling` value that fired
FCR (FCR test 1 = `boltzmann_non_compliant`). The diagnostic's "Expected Conflicts" reads as
robustness but is the coupling gate agreeing with itself in a second register. Do not count that
subsystem's agreement as independent corroboration.

**[CLOSED] Can a metric default avoid triggering a verdict?** Partially, and the map is a trap.
Uniform-metric sweep (witnessed):
```
V=0.1  -> ambiguous                    (honest abstain)
V=0.2  -> false_ci_rope                (washed — lowering the default made it WORSE)
V=0.46 -> constructed_high_extraction
V=0.5  -> constructed_high_extraction  (current default)
V=0.8  -> constructed_high_extraction
```
The default→verdict function is NON-MONOTONIC: lowering 0.5 toward rope_epsilon_ceiling (0.45)
crosses into rope-appearing territory and turns `constructed_high_extraction` into `false_ci_rope`.
An abstain (`ambiguous`) requires ALL of {extraction, suppression, resistance} low simultaneously,
and the path there runs through the FCR washing zone. So NO single scalar robustly abstains.

**Two defaults, opposite safety postures.** Config thresholds are fail-closed + loud
(`config_validation.pl:215 :- initialization(validate_config)` halts exit 1 on a missing required
param). The metric `0.5` fallback (signature_detection.pl:172) is fail-OPEN + silent — not a
config param, not covered by that gate, fabricates a value that triggers a verdict. Principled
fix = make `get_metric_average` fail / return `unknown` on a missing vector and propagate to an
abstain (the engine's own "return unknown not 0.5 / fail-closed on absence" spine), which also
gates calibration to fully-vectored stories by construction.

## RESOLUTION (2026-06-09) — the 0.5-default contamination, fixed at source

The `0.5`-default pathology this audit named (§2, §5: `get_metric_average` default `0.5` >
`snare_epsilon_floor` 0.46 → fabricated `constructed_high_extraction` from no data) is fixed.
Full record: KNOWN_STATE 2026-06-09; ISSUES OQ-89; evidence `rebuild_evidence/`.

- **Source:** schema now requires `accessibility_collapse`+`resistance` for ALL constraint types
  (rejects each independently); prompt instructs honest non-mountain authoring.
- **Engine fail-closes:** `get_metric_average` empty→`unknown`; abstain clause + `number/1` guards
  on the profile-signature predicates + confidence-path gate. Absence now abstains to `unknown`,
  never throws/fabricates. Witness: 0 throws across corpus+probes; fully-vectored verdicts unchanged.
- **Regenerated** the three articles; 16/16 stories author both metrics; **V5 deterministic
  substitution: B(metrics→0.5)==C for all 16** — the formerly-defaulted metrics do not move these
  extraction-driven verdicts, so the audit's washed verdicts were not secretly riding the default
  (the default only bit when extraction itself was unauthored, e.g. the `*_contradictions` stubs,
  which now correctly abstain).

This resolves the *fabrication* leg. The §3 finding stands unchanged: a clean pass exists
(`coupling_invariant_rope`), and FCR still forecloses clean rope for coupling > threshold — that is
design, re-examined under OQ-88, not a defect.

## OPEN

- **[OPEN] Which constraint is the "1 mountain" in the 17:08 header?** Not chased; none of the
  18 carry a `mountain`/FNL/FSM *signature* in pipeline_output, so the header's mountain is
  either a non-magnifica member of the 14-at-17:08 set or a perspectival-classification count.
  Next probe: `grep -l "classified.*mountain" outputs/constraint_reports/*.json` over the 14
  loaded at 17:08, then read that constraint's signature. Does not affect the disjunction.
- **[design, not defect] Coupling-threshold aggressiveness.** Whether the (Power×Scope) coupling
  test over-routes genuinely-coupled methodological kernels into FCR (the OQ-88 "false-mountain =
  missed-kernel" concern) is the substantive question the clean-pass result reframes: the path is
  open, but every magnifica constraint trips the coupling gate. Next probe: re-witness coupling
  for one magnifica constraint under the kernel-first router to see if the flip is genuine
  observer-dependence or a thresholding artifact.
