# FNL bait-confound audit — is the disguise-signature dominance substantive or a generator artifact?

**Date:** 2026-06-04. **Pipeline manifest:** run 2026-06-04T06:30:31Z, commit `1f61da4`,
`code_dirty: true`, `n_constraints: 1106`.
**Denominator:** 1106 (testset constraints, 1:1 with files — Probe 0). The 1107th
`per_constraint` entry is `catholic_church_1200`, an engine demo from
`constraint_instances.pl` (loaded by `stack.pl:13`), excluded throughout. The extra entry is
**non-testset** and the join is otherwise exact — no testset constraint is missing from
per_constraint (`only_pc = {catholic_church_1200}`, `only_files = ∅`); every percentage in
this writeup rides on that ruling.
**Scope:** every conclusion in this writeup is about **FNL**. FCR receives attribution-level
evidence only (no counterfactual); the combined "94.5% disguise" figure is not licensed by
these witnesses. **Tracker:** ISSUES.md OQ-70 (cross-links: OQ-65, OQ-43/44, OQ-48, OQ-49).

## Question

An older evaluation observed: false_natural_law + false_ci_rope ≈ 95% of readings, driving
tangled_rope dominance (~70%). Is that a real property of the seeded topics, or a generator
artifact — does the generator routinely emit the conditions FNL fires on, making the 70%
mechanical?

## Verdict (FNL): generator artifact, witnessed at every link

### The load-bearing witness — Probe 2 counterfactual (destination histogram)

Retract all 915 authored `constraint_classification(C, mountain, context(...))` facts matching
tuple T = (agent_power(analytical), exit_options(analytical)) — the template perspective —
clear `cached_coupling`/`cached_classification`, re-run `constraint_signature/2` corpus-wide
(signature argument unbound; lock cuts respected).

Controls, passed before the diff was read:
- **Sensitivity (pre-named prediction):** `abrahamic_covenant__land_promise_constraint`
  (single tuple-T mountain; claim=snare kills source 1; suppression 0.72 kills source 3;
  authored rope perspective + non_compliant) — predicted flip to `false_ci_rope`. **Flipped
  to `false_ci_rope`.** Recompute proven live.
- **Specificity:** the 41 explicit-claim (claimed_type=mountain) constraints: 0 changed.
- **Collateral:** 0 non-FNL rows changed.

Destination histogram of the 827 baseline-FNL constraints (sums to 827):

| destination | n | attribution |
|---|---|---|
| false_ci_rope | **809** | bait fungibility: `appears_as_rope` source 2 catches the authored ROPE template perspective (1063/1106 files author one) — next template perspective, next clause |
| false_natural_law (residual) | 14 | every one holds ≥1 NON-tuple-T mountain perspective (set-equal: {residual} = {baseline-FNL with non-T mountain}; 13 hold only non-T mountains, 1 held both and the non-T fact kept source 2 alive) — source-2 via non-T, mechanism confirmed, not a leak |
| constructed_high_extraction | 4 | no authored rope perspective; ε ≥ 0.62 → profile fall-through |
| genuine natural_law / coupling_invariant_rope | **0** | zero substantive mass |

809 + 14 + 4 = 827: every ex-FNL constraint in exactly one row. Units note: the retract set
is **915 facts** across **908 constraints** (a few files author two tuple-T mountains) — the
plan's "915-file set" and "~7 outside T" were file/fact-unit slips; the constraint-level
figures are 908 in-T and 922 − 908 = 14 only-outside-T, which is exactly the residual.

**Pre-stated reading honored:** the FNL+FCR aggregate moved 1046→1042 — a naive aggregate
read ("removed the bait, disguise held → substantive") would invert the meaning. The
migration to FCR is the artifact propagating to the parallel gate, not substance.

### The mechanism (each link witnessed)

1. **Template authors the bait.** The one-shot example sent with every generation request
   (`agent/verification_bottleneck.json` via `agent/story_generator_base.py:30`) contains
   "PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN)" at
   (analytical, analytical). Copy rate: 922/1106 constraints author ≥1 mountain perspective;
   908 at exactly tuple T (915 facts); 441 files carry the stereotyped comment label
   (grep lower bound; phrase greps undercount per OQ-65). Authored comments state the
   expectation outright: `acceptable_risk_energy__catastrophic_tail_reading.pl:186` — "Engine
   will flag this as false summit."
2. **The claim gate reads the bait.** `claimed_natural/2` (signature_detection.pl:892–898)
   source 2 fires on ANY single authored mountain perspective. Probe 1: `fnl_evidence` Claim
   slot = `indexed_mountain_classification` for **827/827**; sources 1 and 3 contribute zero
   (positive controls: source-1 dispatch proven live on a claimed-mountain id; no natural_law
   id in the FNL set). In-session sweep reproduced pipeline signatures 1106/1106.
3. **The coupling test fails by construction at mid-ε.** Probe 3 (descriptive, confounded
   run): Boltzmann non-compliance ≥85% for every ε band ≥ 0.3 (100% at 0.3 and ≥0.6);
   compliance lives at ε < 0.2. 70% of the corpus is authored claimed_type=tangled_rope
   (mid-band ε by schema gate).
4. **Funnel (Probe 0, exact):** FNL ⟺ (≥1 authored mountain perspective) ∧ non_compliant,
   zero exceptions both directions: 922 mountain-authoring = 827 FNL + 95 non-FNL (exactly
   the 93 compliant + 2 inconclusive); 184 complement → 0 FNL. FNL ∩ inconclusive = ∅
   (asserted, not assumed).

### The detector did zero discriminating work on FNL (Probe 5)

189 FNL firings overrode a non-tangled_rope authored claim (snare 166, rope 13, scaffold 5,
piton 5). Discriminating subset {override} ∩ {claim-source 1 or 3} = **∅** (all 827 are
source-2). 188/189 were metrically consistent with their own claimed type's context-free
schema gates — the tangled_rope lock steamrolled consistent claims. The single
gate-inconsistent case, `decalogue_image_prohibition__moderate_iconoclast_reading` (snare
claim; supp 0.58 vs the 0.60 floor — a 0.02 miss; ε 0.52 passes), **also fired via source-2
bait** (`fnl_claim_source: indexed_mountain_classification`; bait perspective at `.pl:181`) —
a bait firing that coincidentally landed on a marginally mislabeled file. Credit goes to
author error, not detection. **Substantive yield across all 827 firings: 0 detected,
1 coincidental.** (2 of the residual-14 are non-T overrides, reported separately.)

### Downstream

The tangled_rope ~70% dominance is inherited from authored claimed_type (779/1106;
638/827 FNL were already claimed tangled_rope — FNL's lock target *confirms* the authored
claim). Paper text citing the trifurcation / tangled_rope dominance / "95% disguised"
inherits the artifact. FCR: attribution says 174/219 ride the indexed rope perspective and
45 the explicit rope claim (Probe 1b) — same gate pattern, counterfactual pending.

## Remediation (deferred to a design ruling — OQ-70)

1. Narrow `claimed_natural` source 2 / `appears_as_rope` source 2 (engine; reshapes corpus
   statistics, cascades into papers).
2. De-bait the generation example/prompt (future generation only; corpus stays baited).
3. Adopt the OQ-65 committer-axis framing corpus-wide (bucket bait-driven vs substantive in
   all reporting).

## Artifacts

- `audits/2026-06-04_fnl_bait_confound/fnl_probe0_file_constraint_map.json`, `audits/2026-06-04_fnl_bait_confound/fnl_probe0_reconciliation.json`
- `audits/2026-06-04_fnl_bait_confound/fnl_probe1_attribution.pl` + `.jsonl` (per-constraint signature + claim/appearance source)
- `audits/2026-06-04_fnl_bait_confound/fnl_probe2_counterfactual.pl` + `.jsonl` (per-constraint before/after)
- `audits/2026-06-04_fnl_bait_confound/fnl_probe3_coupling_by_band.json`
