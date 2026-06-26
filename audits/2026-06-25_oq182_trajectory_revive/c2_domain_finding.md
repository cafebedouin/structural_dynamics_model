# C2 blocker — `cross_domain_twins/3`'s "domain" is a name-prefix heuristic, not an authored field

**Status:** ESCALATED to operator (pre-authorized: "escalate if Part A's domain-field definition is
ambiguous — that's an operator call"). Recon witnessed 2026-06-25, before any C2 spend.

## The finding

`context_profile_mining:cross_domain_twins/3` gates twins on `constraint_domain(C1,D1)`,
`constraint_domain(C2,D2)`, `D1 \= D2`. But `constraint_domain/2` (`context_profile_mining.pl:837`)
is **not** the authored `topic_domain/2`. It is a string parse of the constraint **id**: the substring
before the first `_`.

Witness (testsets/, 104 constraints):
- `constraint_domain` distinct values: **86 over 104** — e.g. `actinide`, `intervention`, `regulated`,
  `bitcoin`, `divine`. Nearly one "domain" per constraint ⇒ the `D1 \= D2` gate is **near-vacuous**.
- Diverges from authored `topic_domain`:
  - `actinide_replenishment_mechanism_contradictions` → prefix `actinide` | topic `<none>`
  - `intervention_target_selection` → prefix `intervention` | topic `moral_psychology/philosophy_of_action/social_psychology`
  - `regulated_stablecoin_reading` → prefix `regulated` | topic `monetary_policy/digital_currency/behavioral_economics`
- `topic_domain` itself (the candidate authored field) is **compound/granular**: 97/104 authored, 61
  distinct values, many sharing prefixes (`astrophysics/nuclear_physics` vs
  `astrophysics/nuclear_physics/stellar_spectroscopy`), so exact-string inequality there is also
  over-permissive.

So the 448 "cross-domain twins" reported on testsets/ are mostly same-structural-family pairs that
merely have different first name-tokens — "cross-domain" currently means "different name-prefix."

## Impact on the operator's frozen C2 design

- **Part A (value-witness via orbit-signature ∩ cross-domain):** the cross-domain filter is ill-defined.
  Under name-prefix it is near-vacuous; under `topic_domain` it is compound/granular. Neither is the
  clean authored domain the design assumed. The "what field defines domain" question the operator told
  me to pre-register has **no clean answer in the current substrate** — itself a finding that bounds
  what "cross-domain" can mean (the operator's stated deferral condition).
- **Part B (same-kernel-through-two-models positive via the paired twins):** the paired
  `testsets_haiku/`↔`testsets_flash/` share **identical base-names** (960/960). Same id ⇒ same
  `constraint_domain` ⇒ rejected by `D1 \= D2`; and same-id files cannot co-load in one `trajectory_run`
  (loader keys by base-name). So the paired twins are a **cross-corpus** comparison signal, NOT an
  in-corpus `cross_domain_twins/3` positive. Part B as framed cannot run against this finder without
  either (a) changing the domain definition, or (b) redefining the positive as a cross-corpus
  same-family recovery rather than an in-corpus twin flag.

## The ruling needed (operator's seat)

1. **What should `cross_domain_twins/3` use as "domain"?** Keep the name-prefix heuristic (near-vacuous;
   C2 then validates "fires on different-prefix same-family pairs" — a mechanism-test only, value OPEN to
   the rebuild)? Or change it to authored `topic_domain` (output-changing engine edit to the subsystem
   under validation — needs its own diff-witness commit; and topic_domain's compound form needs a
   match rule, e.g. top-level token)? This is above the fix-simple-errors threshold (engine behavior +
   genuine ruling).
2. **Given (1), what does C2 validate on existing data**, and does C2-value defer to the cross-domain-
   designed rebuild?

Commentary-only invariant is unaffected: `constraint_domain` feeds only the twin report, never
classification — so C0/C1/C3/C-null are independent of this ruling and can proceed.

---

## OPERATOR RULING (2026-06-25)

**Domain definition: KEEP the name-prefix `constraint_domain/2` as-is (Option 1).** Do NOT switch to
`topic_domain` mid-validation — that is an output-changing edit to the subsystem under validation
(changes the 448 twins), so every twin witness gathered afterward would be witnessing code modified to
pass the witness (apparatus-counterfeits-witness), and "what cross-domain should mean" is an
operator-seat design call, not a CC unblock. If `topic_domain` is the right signal it is its OWN OQ with
its own gate, authored deliberately.

**C2 this round = MECHANISM-TEST ONLY** (finder fires/stays-silent on constructed pairs), labeled
wiring, not value. **C2-VALUE = OPEN**, closer = *"rebuilt corpus with a real authored domain field +
a `constraint_domain/2` that reads it."* This is the absence-as-finding branch the frozen design
pre-registered, reached honestly: the cross-domain *value* question is unwitnessable on existing data.

**The twin-vacuity reframe (carries into the validation SCOPE).** 448 twins over 104 constraints under a
near-vacuous gate (constraint_domain `D1\=D2` ≈ `true`, 86/104 distinct) is itself a C-null-class signal
that the **twin product may be near-meaningless as currently gated** — structure produced from a vacuous
predicate. Therefore **families and twins are DISTINCT products**:
- A clean C0/C1/C3/C-null sweep validates **the FAMILY product** (safe + stable + structure-bearing),
  and says **nothing** about the twin product. **NB (2026-06-25):** "structure-bearing" requires
  **C-null** — C1+C3 alone witness only **stability** (well-defined, reproducible partition), NOT
  **meaning**. A fixed-seed clustering of pure noise clears C1+C3. C1 is a raw size distribution, not
  null-relative. So with C-null unrun, family **meaning is OPEN**, symmetric with twin meaning — do not
  read C1+C3 green as "family validated."
- **Twins = OPEN this round** — gate found vacuous, value deferred to the rebuild. Open question:
  should twins ship at all this round, or stay dormant pending the rebuild while only families ship?
- When C-null runs on families, **also report the twin-pair count + gate vacuity** as a parallel
  observation (the family C-null does not cover the twin product).
- C3's permutation leg: twins gated near-vacuously ⇒ their reorder-stability is **less load-bearing**;
  note it, don't lean on it.
- **KNOWN_STATE scope must be precise:** "families validated (pending C-null verdict); twins OPEN —
  gate found vacuous, value deferred to rebuild." Five green checks must NOT read as "subsystem
  validated" when half of it (twins) was found broken this turn.

**C-gen (distinct from C2).** The paired `testsets_haiku/`↔`testsets_flash/` same-kernel pairs are a
**cross-generation stability** control — does a kernel land in the same structural family across models?
Worth running as its own invariance witness. It is NOT C2 (same id ⇒ same-domain ⇒ rejected by the
cross-domain gate; same id ⇒ cannot co-load in one run). Do not relabel C-gen as C2.

---

## FAMILY-MEANING RESULT — C-null PASS (2026-06-25, testsets/ leg)

The C-null scope-setter has **run and PASSED**. The earlier "family meaning is OPEN pending C-null"
caveat above is now **resolved on the family side**: the family product is **validated as
meaning-bearing**, not merely safe + stable.

- **Verdict:** RealSil = **0.161119** (97 clustered constraints, 11 families) > **P95(null) = −0.026436**
  over 200 per-component-shuffle draws; **0/200** null draws reach RealSil; TEETH PASS (null_median
  −0.0945 < RealSil; standardized gap **+5.01σ**). Reproducible under seed 20260625 (SWI 9.2.9; Python
  cross-check matches). Harness + log + distribution: `c_null_harness.pl`, `c_null_results.log`,
  `c_null_distribution.json`. Control-first: INTERNAL-CHECK / GROUPING-FIDELITY / FIDELITY / JOINT-TOOTHLESS
  / TIE-BREAK all pasted *before* the verdict and gating it; the joint shuffle was demonstrated toothless
  (S_joint = RealSil) exactly as the per-component design predicts.
- **Twin product UNCHANGED — still OPEN.** As required, the family C-null reported the twin-pair
  count/gate-vacuity in parallel (448 twins / 4656 pairs; near-vacuous cross-domain gate). The family
  meaning result says **nothing** about twins. Twin meaning stays OPEN, deferred to the rebuilt corpus
  with a real authored domain field. **The precise scope is now: families validated (safe + stable +
  meaning-bearing); twins OPEN.**
- **Mechanism note:** the frozen "Chimera surgery map" was mechanically wrong (`group_by_shift` keys on
  constraint identity, not `trajectory_cached`); the harness builds shift-groups itself under σ_shift.
  Quantities unchanged — erratum in `c_null_protocol_FROZEN.md`.
