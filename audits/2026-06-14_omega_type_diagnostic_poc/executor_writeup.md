# Executor Writeup — Ω-Type Diagnostic POC (blind executor)

**Role:** blind executor. I built the protocol (`spec.md`), a deterministic baseline
(`deterministic_baseline.py` → `det_results.json`), and an LLM-judge pass
(`build_judge.py` → `judge_results.json`) over the committed `sample_40.json` (40 omegas).
I did **not** read `adjudicator_held_key.json`. Every count below cites computed output in
this directory.

## What the protocol does (one line)

Classify each omega by **which resolution operation discharges it** (measure→Ω_E /
define→Ω_C / decide→Ω_P), where a fired operation only counts if it is **external** at its
own locus (criterion / DECIDER / fact) rather than a restatement of a declared field. Two
orthogonal steps: signature detection (set, stamps nothing) + external-vs-restatement gate
(classifies). Full spec in `spec.md`.

---

## Headline metrics

### (a) Diagnosed-vs-authored, hybrid-aware

**33/40 agree = 0.825.** (`judge_results.json`, `vs_authored=="agree"`.)
A hybrid counts as agreement when the authored type is among the fired externals.

- vs_authored breakdown: `{agree: 33, mistyped->Ω_E: 5, mistyped->Ω_C: 2}`.
- **Hybrid-rate bucket: 1/40 = 0.025** — only sample_id **21** (`reading_boundary_vs_
  sibling_readings`) diagnoses as a genuine `hybrid(define+measure)`: the omega both asks
  which *boundary criterion* counts as electronic-money emergence (define, open conceptual
  choice among declared sibling readings) AND demands *traced institutional/behavioral data*
  on when behavior actually changed (measure, external observation). It is authored Ω_E; the
  measure limb is external, so the authored type is among the fired externals → counts AGREE.

The 7 disagreements are all **single-step mistypes, none restatements**:
- `mistyped->Ω_E` (5): ids **2, 18, 22, 23, 29** — all authored Ω_C whose resolution routes
  through external observation (practitioner-statement textual analysis; court-application
  consistency; historical counterfactuals; motive history), not definition. Conceptual-labeled
  but empirically-resolved.
- `mistyped->Ω_C` (2): id **35** (authored Ω_E, resolved by whether a *limiting criterion*
  exists = define) and id **39** (authored Ω_P, resolved by what *"priority" means* = define,
  not by a stakeholder value-decision).

**Interpretation:** Strong agreement with authored labels (82.5%), and where the protocol
disagrees it does so by the operation-that-discharges test, not by relabeling noise. The
mistypes cluster on the predicted seam: authored-conceptual questions whose actual resolution
is an external observation get re-routed to Ω_E (the "is it a cover story / was it
historically necessary / did motives change" family).

### (b) Deterministic `unknown` rate (the determinism boundary)

**5/40 = 0.125 unknown** (`det_results.json`: ids 2, 7, 18, 22, 27 returned `unknown`).
**Two-sided commit control: BOTH PASS** (`det_results.json.commit_control.both_pass=true`):
- under-commit: a KNOWN-DEFINITIONAL case (biotech "what counts as 'biotech'… conceptual
  analysis to define the framework") → baseline **commits to `conceptual`** (does not
  flinch to unknown).
- over-commit: a GENUINELY-AMBIGUOUS case ("Could this be read differently?… remains
  contested," no sitable locus) → baseline returns **`unknown`** (does not guess).

**Interpretation:** The 12.5% boundary is trustworthy in the sense that matters — the
baseline can neither inflate `unknown` (it commits on a clear definitional case) nor deflate
it (it refuses on a genuinely empty case). The deterministic baseline is a transparent *floor*,
not the verdict: it agrees with the LLM-judge on only **19/40** diagnoses (hybrid-collapsed),
because lexical cues over-fire `measure` on words like "counterfactual"/"compare" and miss
the subordinate-vs-deciding-operation distinction the judge resolves by reasoning. The
baseline's value is the honest `unknown` floor, not its accuracy.

### (c) Restatement rate (validity-critical)

**0/40 = 0.000** diagnose as restatement in the judge pass (`judge_results.json`, no
`diagnosis=="restatement"`, no `vs_authored=="restatement-mislabel"`).

**Interpretation (validity-critical, stated as such):** Zero restatements means **every
sampled omega is a genuine frontier** under this protocol — none merely re-states a declared
field. This is a *positive* validity signal for the corpus's omega-authoring, but it is also
the result most worth distrusting: a 0-count over a gate is exactly the Build-Discipline
"absence satisfies the gate" hazard. The gate is **not vacuous** — it has a live discriminator:
the ε-invariance omegas (ids 27, 31; and the `measure→fact-locus` RESTATE_MEASURE rule in the
baseline) are precisely the cases that *could* have restated (re-deriving an authored ε across
declared readings). I judged them Ω_C, not restatement, because the ε-invariance test there
**individuates constraints by a conceptual criterion** rather than re-reading an already-
declared ε value — the two readings' ε are not both authored, so the comparison defines whether
two constraints exist. That is a real call, not a vacuous pass: the restatement limb fires on a
constructed re-derivation case in the baseline (`RESTATE_MEASURE`), confirming the gate would
flag one. **But the live restatement rate on this 40-sample is 0**, and a reader should treat
"no restatements found" as a fact about this sample under my locus-calls, re-checkable against
the adjudicator's key — not proof none could exist.

### (d) kernel_reading / committer family verdict — PER-OMEGA (not an aggregate)

14 family omegas (`is_family=true`); **all 14 are authored `conceptual`** and **all 14
diagnose Ω_C.** Per-omega:

| id | name | authored | diagnosis |
|----|------|----------|-----------|
| 0  | reading_committer_ambiguity | conceptual | Ω_C |
| 7  | kernel_reading_alternative_framing | conceptual | Ω_C |
| 9  | committer_frame__biomedical_vs_critical_foreclosure | conceptual | Ω_C |
| 10 | kernel_reading_foreclosure | conceptual | Ω_C |
| 11 | kernel_reading_foreclosure_test | conceptual | Ω_C |
| 17 | kernel_reading_plurality | conceptual | Ω_C |
| 19 | kernel_reading_contest | conceptual | Ω_C |
| 20 | kernel_reading_alternative_framing | conceptual | Ω_C |
| 24 | committer_structure__liturgical_vs_native_reading_contest | conceptual | Ω_C |
| 25 | kernel_reading_underdetermination | conceptual | Ω_C |
| 27 | kernel_reading_distinction | conceptual | Ω_C |
| 28 | reading_committer_frame_dependence | conceptual | Ω_C |
| 31 | kernel_reading_alternative_framing | conceptual | Ω_C |
| 38 | kernel_reading_contest | conceptual | Ω_C |

**Verdict: the family is UNIFORM, not split.** All 14 resolve by a define operation at the
criterion-locus (foreclose-vs-coexist of declared readings, kernel identity, what "living"/
"priority"/"distinct constraint" means). This is the discriminating set, judged per-omega and
reported per-omega; the uniformity is an empirical finding of this sample, not an assumption.
Three of them (ids 19, 20, 38) carry a subordinate `measure` limb in their approach (empirical
falsification / generate-and-compare / text-availability); I judged the **deciding** operation
to be the conceptual framework-call in each, with the measure limb as evidence-for, not the
locus. Those are the family's medium-confidence calls (see Undecidable section).

### (e) Ω_E-falsifier rate

Among the **14 authored-Ω_E** omegas, **1** (id **35**, `rights_boundary_expansion`) routes
external via an internal-reasoning operation (define — whether a *limiting criterion* exists)
rather than measure. **Ω_E-falsifier rate = 1/14 = 0.071.** The other 13 authored-Ω_E all
route via measure (12 pure Ω_E; id 21 hybrid(define+measure), still measure-external).

**Interpretation: RARE ⇒ "Ω_E is a status" HOLDS on this sample.** When an omega is authored
empirical, its resolution almost always genuinely routes through external observation (93%);
only one case is an empirically-labeled question that actually wants a definition. The
empirical label is doing real work, not acting as a catch-all "status." (Under-claim: one
sample of 14; this earns "Ω_E label is mostly load-bearing here," not "Ω_E is a kind
everywhere.")

---

## Biotech-triple spec check (SPEC-IMPLEMENTATION ONLY — near-tautological, NOT a positive control)

Running the judge/baseline on the `docs/omega_variables.md` US-China biotech example
reproduces the documented decomposition (`det_results.json.biotech_triple`):

| sub-omega | diagnosis |
|-----------|-----------|
| Ω_C (race vs supply-chain frame) | conceptual |
| Ω_E (supply-chain % depending on China) | empirical |
| Ω_P (security vs progress, which dominates) | preference |

**C / E / P reproduced.** This confirms the protocol *implements* the documented framework —
it is near-tautological because the example was written to teach exactly these three
operations, with the cue vocabulary the detector keys on. **It is NOT a positive control** on
the diagnostic's discriminating power over real omegas; the discriminating evidence is metrics
(a)–(e) above on `sample_40.json`.

---

## Genuinely undecidable / lowest-confidence cases

No case was *fully* undecidable (every omega had a defensible deciding operation), but the
following are the medium-confidence calls where the deciding-vs-subordinate operation is a real
judgment a different reasoner could flip:

- **id 21** (`reading_boundary_vs_sibling_readings`) — the one genuine **hybrid**: define
  (boundary criterion) and measure (when behavior changed) are *both* plausibly the deciding
  operation. I committed to hybrid rather than forcing one.
- **ids 19, 20, 38** (family) — carry a real `measure` limb (empirical falsification /
  generate-and-compare sibling / text-availability survey). I ruled the conceptual
  framework-call the deciding locus; a reader weighting the empirical limb could call these
  hybrids. This is the family's soft edge.
- **id 28** (`reading_committer_frame_dependence`) and **id 38** (`kernel_reading_contest`,
  Hanafi) — surface as `measure` (community survey / text availability) but the deciding
  question is conceptual (frame-dependence / foreclose-vs-coexist). Judged Ω_C with measure
  subordinate.
- **id 39** (`priority_as_ontological_marker_vs_historical_fact`) — authored Ω_P; I judged
  Ω_C because "what *priority* means" settles it (define), not "whose values" (decide). The
  authored label and the operation genuinely diverge here.
- **id 15** (`victim_vindication_mechanism`) — authored Ω_P; decide (can a dead person be
  benefited — a value premise) and measure (interview families) both fire; I ruled the value
  premise the decider → Ω_P, agreeing with authored.

These are noted so the adjudicator can see exactly where my locus-calls sit. I make **no claim**
about what the held key says.

---

## Artifacts in this directory

- `spec.md` — the protocol.
- `deterministic_baseline.py` → `det_results.json` — transparent fail-closed baseline,
  two-sided commit control, biotech triple.
- `build_judge.py` → `judge_results.json` — per-omega LLM-judge verdicts with rationales.
- `executor_writeup.md` — this file.
