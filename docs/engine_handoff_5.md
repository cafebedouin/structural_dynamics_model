# Engine Handoff No. 5 — Fabricated-Default Front, Surface Routing, OQ-33

*2026-05-30. A snapshot for the next session. Read Handoffs 1–4 first for the full
thesis (radio telescope for constraints; points, does not adjudicate; nothing
self-certifies; coverage makes a green readable rather than blind; demotion status
is per-instrument not absolute; the witness-tier ledger is the discipline). This doc is
current state, not a narrative. **Re-cut the substrate cold — do not trust this doc over
a run.***

---

## The discipline this session continued (one note, carry forward)

Handoff 4 named the recurring correction: claims sitting one tier above their evidence.
This session repeated it. The fix that held: paste-or-untag, per claim, every time.
**The witness-tier ledger below is the structural memory for this.** Do not re-flatten it.
A claim without a tier is not done.

The stopping note from Handoff 1 still loads: the instrument cannot certify its own
stopping. This session stopped after diagnosis (Surface-3 blocked, fabricated-default
front inventoried) and before construction (Surface-2 primitive). The stopping was
deliberate. The cold read you are about to do is the only thing that can tell whether it
was right.

---

## Current state — one number per question

**Denominator: 191 engine params + 6 authored fields = 197 type-moving predicates.**
*Tier: grep-witnessed + perturb-confirmed (bidirectional dataflow trace; demotion_pass
live output confirms 191; 6 authored fields graduated to perturb-confirmed this session).*

Breakdown: 168 numeric config.pl params (demotion_pass.py regex `param(name, decimal)`;
config.pl has 178 total `param()` declarations, 10 atom-valued, 168 regex-matched
numeric) + 23 supplementary (14 `power_role_heuristic/4` + 9 from `exit_modulation/2`
and `positional_displacement/2` in constraint_indexing.pl) = 191.

**Demotion sort — all 191 have a witnessed status (demotion_pass.py live, 2026-05-30):**

```
  6  shadowed              positional_displacement (inert at profile=uniform; live if profile→positional)
  0  errored-untested      (empty — integer-19 re-swept at ±1; all resolved)
 20  unperturbable         witnessed coverage=0
  0  reachable-but-locked  (empty — lock lives at READING, not param granularity)
 24  perturbed-and-survived  final-type flips, 18 kernels, in _WITNESSED_PARAMS + _WITNESSED
141  perturbable-but-unperturbed  backlog; epsilon-first
───
191  ✓
```

*Tier for sort: grep-witnessed (demotion_pass.py pasted output; re-run to confirm).*

**Epsilon-param sub-tier within 141 backlog (characterized this session):**
- `rope_epsilon_ceiling` (0.45): split-tier. +10% → unperturbable-by-construction
  (config_schema.pl:482–487 `classification_rope_snare` invariant fires → export_failed);
  −10% → reachable-stable (23 kernels, fold_survival=1.0, 0 flips).
  *Tier: +10% grep-witnessed (invariant source code); −10% perturb-confirmed (pasted results).*
- `tangled_rope_epsilon_floor` (0.3): perturbable-but-unperturbed, EARNED. 25–26 kernels
  reached across ±10% band, fold_survival=1.0 on all. Genuine stability, not untouched.
  *Tier: perturb-confirmed (pasted results, witness_backlog_results.json).*
- `fpn_epsilon` (0.001): unreached-at-tested-range. coverage=0 at all ±10% values. Not
  unperturbable-by-construction; flip potential unknown; wider range required.
  *Tier: perturb-confirmed (coverage=0 from witness_backlog_results.json); "flip potential
  unknown" is unwitnessed — do not assert inert.*
- `piton_epsilon_floor` (0.1): unreached-at-tested-range (near-blind; 2/38 kernels at +10%
  only, fold_survival=1.0, 0 flips). Same status as fpn_epsilon.
  *Tier: perturb-confirmed (pasted results); "not governing" is unwitnessed.*

`unreached-at-tested-range` is a new sub-tier of the 141 backlog. It is distinct from
`errored-untested` (which means the sweep errored, not that coverage was zero). Fix
direction for unreached: wider range sweep. Fix direction for errored: integer-step sweep.

**38 kernels total; 18 witnessed (24 survivors cover 18 kernels); 20 unwitnessed on Surface 1.**
*Tier: grep-witnessed (kernel count from demotion_pass.py; 18 witnessed = count of unique
kernels in _WITNESSED dict).*

**OQ-30 resolved to routing: 19 reachable-but-locked, 0 unreached, 1 unlocked-reached-but-held.**

- 19 kernels: reached (MaxCoverage > 0), AnyFlip = NO. Lock is `false_natural_law` or
  `coupling_invariant_rope` signature. Signature fires first; metric changes are eaten.
  Lock guard: `boltzmann_compliant(C, non_compliant(_, _))` at
  `signature_detection.pl:835–836` inside `false_natural_law/2`.
  *Tier: grep-witnessed (signature_detection.pl:835–836 source read; MaxCoverage
  values from witness_backlog_results.json; "AnyFlip=NO" from pasted backlog outputs).*
- 0 kernels: unreached. All 20 were reached at ±10%.
  *Tier: perturb-confirmed (pasted results).*
- 1 reading: `qwerty_persistence_mechanism/naturalization_reading` — reached, unlocked
  (constructed_low_extraction signature), AnyFlip=NO at ±10% rope_chi_ceiling. Wider-
  range sweep is the targeted next step, low priority.
  *Tier: perturb-confirmed (pasted result).*

**CRITICAL-PATH CONSEQUENCE (not in Handoff 4): the boltzmann guard at:835–836 is
Surface-2-displaceable.** The same boltzmann_floor overlay that moved excess_extraction
by −0.52 in the Surface-2 proof-of-life targets the `boltzmann_compliant/2` predicate
that feeds this guard. If the overlay shifts a kernel from `non_compliant` to `compliant`,
`false_natural_law/2` fails, and the reading falls through to the next signature clause.
Surface 2 is therefore the critical path for 19 of 20 remaining kernels — NOT the
low-urgency novelty front Handoff 4 ranked it.

**Caveat to carry:** "Surface-2-displaceable" is *grep-witnessed at the guard*
(source code traces to boltzmann_compliance.pl). "Perturbing the Boltzmann floor
actually flips these 19 final types" is **HYPOTHESIS, not tested** — it is the
Surface-2 primitive's first real verification target. Do not carry this as a
finding before that run.

---

## Three surfaces — current status

**Surface 1 — static type** (`product_site_export` → `dr_type/3` → `classify_from_metrics/6`
then `integrate_signature_with_modal/3`): mature, perturb.py sweeps all 191 params.
**Hit its empirical ceiling on the kernel-witness problem**: all 19 locked kernels were
reached and watched the signature override eat every metric change. Surface 1 cannot
witness them; Surface 2 is required.
*Tier: perturb-confirmed (coverage>0, AnyFlip=NO pasted for all 19).*

**Surface 2 — excess-extraction / PoA** (`boltzmann_compliance.pl:excess_extraction/2`):
SCOPED, proof-of-life witnessed. Observable = `excess_extraction(C, ExcessEps)`. Overlay
= `config:param/2` retract/assertz on a `boltzmann_floor_*` param. Baseline→perturbed
delta pasted (civic_eugenic_reading, `boltzmann_floor_identity_coordination` 0.08→0.60,
delta −0.52). Floor path confirmed as `coordination_type` (not override, not default).
*Tier: perturb-confirmed (delta pasted, proof_of_life_surface2.py run).*
**Primitive NOT built. Now the critical path (see above). Next build.**

**Surface 3 — temporal / drift** (`drl_composition.pl:classify_at_time/4` →
`constraint_history/3`): SCOPED but **BLOCKED by OQ-33**.
Observable = `constraint_history(C, Ctx, Timeline)`. Dynamic overlay confirmed
(`narrative_ontology:measurement/5` retract/assertz). Two structural findings from the
proof-of-life (both load-bearing, see below):

1. `classify_at_time` (line 193) calls `drl_core:classify_from_metrics` directly. It does
   NOT call `integrate_signature_with_modal`. `dr_type/3` calls both. The two surfaces
   diverge by construction on any constraint where a signature override would fire.
   Concrete: `civic_eugenic_reading` at T=4 would get `snare` from `dr_type` (if metrics
   pass snare) but `unknown`/non-snare from `classify_at_time` (which skips the signature
   layer, and the fabricated Supp=0.5 fails the snare floor).
   *Tier: grep-witnessed (drl_composition.pl:193 source; drl_core.pl dr_type/3 source).*
2. Fabricated-default (OQ-33) — see below. Surface-3 primitive is premature until OQ-33
   resolves; perturbing a surface running on fabricated baselines measures noise against
   noise.

---

## Fabricated-default front (new this session, the big one — OQ-33)

Full entry: `ISSUES.md OQ-33`.

**What is fabricated-default (build_discipline.md Pattern 4):** A predicate that lacks
its input fabricates a plausible constant rather than failing or returning `unknown`.
Downstream callers receive a real-looking value and treat it as a measurement. Fires
silently. Distinguishable from a genuine measurement only by perturbation (tripwire).
Sibling of produced-but-not-consumed: P-b-n-c leaves a wire dangling; fabricated-default
connects the wire to a made-up signal so nothing looks broken. Harder to see because the
system appears to work.

**D1 (the confirmed instance, LOAD-BEARING-WRONG):**
`classify_at_time` (`drl_composition.pl:179`):
```prolog
(narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp) -> true ; Supp = 0.5)
```
- `suppression_requirement` temporal `measurement/5` facts: **0/223 testsets** (the
  grep: `grep -l "narrative_ontology:measurement.*suppression_requirement" prolog/testsets/*.pl`
  → 0 results). The fallback fires on 100% of the temporal path.
  *Tier: grep-witnessed (zero-count grep on live testsets + source code).*
- Tripwire (Supp poisoned unconditionally): **443/519 non-unknown temporal classifications
  flipped to unknown.**
  *Tier: instance-reported (inventory session 2026-05-30). Not a persisted script result.
  Graduation step: re-run the tripwire, paste the flip count. Until run: do NOT cite as
  perturb-confirmed.*

**Static-vs-temporal fabrication disagreement (distinct sub-finding):**
`get_raw_suppression` (`drl_core.pl:96`):
```prolog
(narrative_ontology:constraint_metric(Constraint, ActualMetricName, Value) -> true ; Value = 0)
```
Static path fabricates `0`; temporal path fabricates `0.5`. Same missing-data condition,
different invented fillers. Consequence:

| Surface | Fallback | `tangled_rope` (floor 0.40) | `snare` (floor 0.60) |
|---------|----------|---------------------------|----------------------|
| Static (Surface 1) | Supp = 0 | blocked | blocked |
| Temporal (Surface 3) | Supp = 0.5 | **passes** | blocked |

Temporal can never emit `snare` (0.5 < 0.60 snare_suppression_floor); static can (real
authored value can be ≥ 0.60, e.g., `absolutist_reading` = 0.72). Any cross-surface
divergence analysis that attributes static/temporal snare disagreement to observational
difference is compromised — it is artifact.
*Tier: grep-witnessed (both source lines confirmed; table is logic from confirmed values,
no tripwire needed for the asymmetry claim — it is structural).*

**D20/D21 (suspected second instance, `boltzmann_compliance.pl:245/251`, UNSURE tier):**
The Boltzmann path appears to call `classify_from_metrics` with `Supp=0` / `BaseEps=0.5`
fabrication. Path-asserted from code read only, not tripwired.
*Tier: path-asserted. Graduation step: source-patch tripwire on the Boltzmann call site.*

**Inventory claim "40 defaults, 10 tripwired" (from prompt/session):**
*Tier: instance-reported (session conversation, no persisted inventory script or results
file). Do not cite as perturb-confirmed. Graduation step: write the inventory script,
paste counts.*

**OQ-33 resolution options** (three, per ISSUES.md — design decision, no verdict here):
(a) author temporal suppression_requirement measurements into testsets;
(b) align classify_at_time to get_raw_suppression fallback — costs the Surface-1/Surface-3
    independence the three-surface model exists to hold;
(c) formalize "temporal returns indeterminate without its own data" — 443 extant
    tangled_rope emissions reclassified as policy decisions or repair targets.

**Blocks from OQ-33:**
1. Surface-3 perturbation primitive (premature until resolved)
2. Validity of 443 extant temporal classifications in corpus
3. Cross-surface divergence interpretation (static-0/temporal-0.5 sub-finding)

---

## Pattern 3 — bound-probe / query-binding-bypasses-cut

`docs/technical/build_discipline.md` Pattern 3. Witnessed by live count-delta.

**Self-check (run this before any signature-membership census):**
```prolog
?- findall(C, signature_detection:constraint_signature(C, natural_law), Cs), length(Cs, N).
N = 1.  % bound form: over-counts; actual engine count is 0
?- findall(C, (signature_detection:constraint_signature(C, Sig), Sig == natural_law), Cs), length(Cs, N).
N = 0.  % unbound+post-filter: correct
```
`behavioral_competence_reading` satisfies the `:97` clause body in isolation but the
engine assigns it `false_summit_mountain` (lock at `:87` fires first with Sig unbound).
If the bound form returns 1 and the unbound form returns 0, Pattern 3 self-check passes
— the probe is working correctly.

---

## The next move (one thing, by readiness)

**Build the Surface-2 per-param perturbation primitive.** This is the proven critical
path for 19 of 20 remaining unwitnessed kernels, has real (non-fabricated) baselines, and
is ungated by OQ-33. It is a **construction task** — per the Handoff 1 stopping-note, it
belongs to a fresh session, not the tail of a diagnosis-heavy one.

First cut:
1. Build the per-param Surface-2 primitive (observable = `excess_extraction(C, ExcessEps)`;
   do NOT extend `perturb.py`, which is Surface-1 only; carry the boltzmann_floor_override
   path-shadows analog — coverage=0 if override path is taken; follow proof_of_life_
   surface2.py design).
2. Test the **HYPOTHESIS** (currently path-asserted, not witnessed) that Boltzmann
   perturbation flips the 19 locked kernels' final types. That test is the thing that
   converts "critical path in principle" to "witnessed."

Lower-urgency remaining items:
- **Surface-3 primitive**: gated on OQ-33. Do not build until the fabricated-suppression
  design decision is made; a Surface-3 primitive on fabricated baselines cannot produce
  interpretable results.
- **OQ-33 design decision** (a/b/c above): not a construction task — a deliberate repo-
  owner choice that gates Surface-3 and determines whether 443 temporal classifications
  stand or are repaired.
- **qwerty/naturalization_reading** wider-range `rope_chi_ceiling` sweep: one remaining
  Surface-1 candidate, low priority, independent.
- **D20/D21 Boltzmann tripwire**: graduation step for the UNSURE suspected instance;
  worth doing alongside or after the Surface-2 primitive build since you are already
  in that subsystem.

---

## Witness-tier ledger (do not re-flatten)

Carry from Handoff 4; updated for this session.

**Denominator and authored fields (from Handoff 4 + this session):**
- 191 engine params — **grep-witnessed** (backward trace + pasted EXIT-1 exclusions; 
  demotion_pass.py live confirms 191 total).
- 6 authored live fields — **grep-witnessed + perturb-confirmed** (per-field edge greps
  and type flips pasted in Handoff 4; graduated from trace-asserted this session).
  Path split: extractiveness/suppression/theater_ratio/d_value reach `classify_from_metrics/6`
  via argument slots; accessibility_collapse/resistance reach `dr_type/3` via
  `integrate_signature_with_modal/3` (AFTER `classify_from_metrics`, same `dr_type/3`
  call). Both paths live on Surface 1.
  See Handoff 4 field-by-field ledger for pasted edge-greps — not reproduced here.
- 24 survivors — **perturb-confirmed** (per-pair coverage/fold_survival pasted; sample
  before/after types pasted).
- Demotion sort 6/0/20/0/24/141 — **grep-witnessed** (demotion_pass.py pasted output;
  re-run to confirm current state).

**This session (new tiers):**
- Boltzmann guard at signature_detection.pl:835–836 — **grep-witnessed** (source read,
  pasted).
- "19 kernels reachable-but-locked by FNL/CIR" — **perturb-confirmed** (MaxCoverage>0,
  AnyFlip=NO pasted for all 19 in OQ-30 session; plus grep-witnessed for the guard).
- "Surface-2 is critical path for 19 kernels" — **path-asserted** (guard is grep-
  witnessed; "perturbation actually flips final types" is HYPOTHESIS, untested).
- D1 `Supp=0.5` fires 100% — **grep-witnessed** (zero measurement/5 facts for
  suppression_requirement + source line confirmed).
- D1 asymmetry (temporal 0.5 vs static 0) — **grep-witnessed** (both source lines
  confirmed; table is structural logic).
- 443/519 tripwire count — **instance-reported-not-PM-seen** (inventory session
  2026-05-30). Not a persisted script. Graduation step: re-run, paste.
- 40 defaults / 10 tripwired — **instance-reported-not-PM-seen** (session only; no
  inventory script in repo). Graduation step: write the script, paste.
- D20/D21 Boltzmann suspected second instance — **path-asserted** (code read only;
  not tripwired). Graduation step: source-patch tripwire.
- classify_at_time omits integrate_signature_with_modal (Surface-3 structural
  divergence from dr_type) — **grep-witnessed** (drl_composition.pl:193 calls
  classify_from_metrics directly; dr_type source confirmed calling both).

---

## Corrections to the prompt / numbers that moved

The prompt states "suppression_requirement measurement absent in 190/190 testsets." 
Correct as a temporal claim but the count is 0/191 (not 190): 191 testsets have
temporal `measurement/5` facts (any metric); 0 have them for `suppression_requirement`
specifically. 191 testsets DO have the static `constraint_metric/3` fact for
`suppression_requirement` (used by the static path; not by `classify_at_time`). The
prompt's "190" appears to be a typo or off-by-one of the total testset count (223 total;
191 with static suppression fact, 32 lacking even the static fact). The structural claim
is unchanged: Supp=0.5 fires 100% of the temporal path.

The prompt states "168 config.pl." That is the count of params matching the
`load_numeric_params` regex in `demotion_pass.py`. `config.pl` actually has 178 total
`param()` declarations; 10 are atom-valued and not matched by the regex. The 168 is
correct for "numeric params swept by demotion_pass.py."

---

## Pointers

`ISSUES.md OQ-33` (fabricated-default front; blocks Surface 3) ·
`ISSUES.md OQ-30` (20 unwitnessed kernels; Surface-2 routing; updated this session) ·
`docs/technical/build_discipline.md` Pattern 3 (bound-probe) + Pattern 4 (fabricated
default) · `python/sweeps/perturb.py` (Surface-1 primitive, 191 params) ·
`python/sweeps/proof_of_life_surface2.py` (Surface-2 proof-of-life; design template for
the primitive) · `python/sweeps/proof_of_life_surface3.py` (Surface-3 proof-of-life;
confirms observable and overlay; confirms classify_at_time skips signature layer) ·
`python/sweeps/demotion_pass.py` (the sort; run to confirm 6/0/20/0/24/141) ·
`python/sweeps/witness_backlog.py` (backlog sweep; --resume) ·
`python/enhanced_report.py` (`_WITNESSED_PARAMS`, 18 kernels; E5 stability band +
Fisher probe) · `prolog/drl_composition.pl:179` (Supp=0.5 temporal fabrication) ·
`prolog/drl_core.pl:96` (Supp=0 static fabrication) ·
`prolog/signature_detection.pl:835–836` (boltzmann guard inside false_natural_law) ·
`outputs/witness_backlog_results.json` (float ±10% batch, 179 params) ·
`outputs/witness_backlog_integer_results.json` (integer ±1 batch) ·
Handoffs 1–4.
