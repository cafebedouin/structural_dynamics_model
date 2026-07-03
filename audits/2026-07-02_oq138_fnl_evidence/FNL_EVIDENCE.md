# OQ-138 false_natural_law — route-vs-keep partition witness (2026-07-02)

**The missing OQ-138 limb.** FNL was deliberately excluded from the 2026-06-21 FSM/FCR/constructed
evidence pass (the OQ-70 bait gate that excluded it has since expired). This census gathers the
same partition witness the other three overrides were ruled on: does the FNL override CHANGE type
at cascade-winner seats, and does the change mask structural tensions the way FSM/FCR did?

**Method.** `fnl_census.pl` (this dir), UNBOUND `constraint_signature/2` (true cascade winner —
the constructed-3 §1 gotcha), per-seat `metric_based_type_indexed → dr_type` diff (FNL has NO
config ablation hook, so the per-seat diff is the override effect), `claimed_natural` source,
boltzmann, vic/ben/eps/supp, verdict decomposition. Corpus overlays via
`retractall + assertz`. **Code state HEAD `8a4a54a7`** (post-FCR-9, post-OQ-128). FNL override =
`signature_detection.pl:924` (unknown→unknown, OQ-37 abstain) + `:925` (else→tangled_rope).

**Positive controls (both PASSED on testsets):**
- `PC_CENSUS ok` — census finds `organization_floor_c0` among FNL winners (the known live firing;
  a census that missed it is broken).
- `PC_DIFF` — the diff probe flags the **3 unconverted piton seats** (scaffold→piton, held on
  OQ-90) as live type-changers — the only known live changers at HEAD once FSM/FCR/constructed are
  converted. A diff blind to them would be byte-identical to one that never looked.

## Per-leg counts (corpus AND code state cited)

| leg | n | FNL winners | TYPE-CHANGED | TYPE-INERT | source |
|---|---|---|---|---|---|
| testsets | 119 | 1 | 0 | 1 | explicit_mountain_claim |
| testsets_haiku | 960 | 13 | 6 | 7 | explicit_mountain_claim (13/13) |
| testsets_flash | 960 | 8 | 8 | 0 | explicit_mountain_claim (8/8) |
| archives/datasets/kernel_v1 | 1106 | **0** | 0 | 0 | — |

**All 22 FNL firings across all legs are `explicit_mountain_claim` (source-1). ZERO
`natural_law_signature_match` (source-2) firings anywhere.** This confirms the OQ-70 class fix
held at HEAD: no signature reads a single authored perspective as a story-level claim. These are
NOT the regime-bound 827/1106-era numbers — they are current-detection at HEAD.

**kernel_v1 = 0 is MEASURED-empty, not didn't-look** (`fnl_kv1_control.log`): the archive has 41
mountain-claims and 973 boltzmann-non-compliant seats, but their **intersection is 0** — every
mountain-claimer on kernel_v1 is boltzmann-compliant, so FNL (claim ∧ non-compliant) cannot fire.
`PC_CLAIMED_NATURAL_REACHABLE ok` (synthetic mountain claim fires the detector). So the archived
corpus yields zero FNL at HEAD by the data, not by a broken probe.

## The type-changing FNL seats repeat the FSM/FCR shape EXACTLY

Every CHANGED FNL seat (6 haiku + 8 flash = 14) shows the identical override signature the FSM and
FCR-9 conversions were ruled on:
- **override effect:** metric type `scaffold` or `snare` → `tangled_rope` (the `:925` overwrite);
- **verdict decomposition:** base `green` → joined `yellow`, grade `correction` (13 of 14; two
  are base `yellow`→`yellow`) — the "green→yellow unmask" the FCR-9 ruling predicted;
- **discriminant available:** all carry `explicit_mountain_claim` (FNL fires definitionally on
  claimed naturality) plus vic/ben — the FSM victim-discriminant is authored here too (vic ranges
  0–4).

The TYPE-INERT FNL seats (1 testsets + 7 haiku) all sit at `unknown→unknown` — they ride the
`:924` abstain clause (OQ-37 honest-unknown), so the override is type-inert on them and conversion
is free either way.

**Unremarked asymmetry (recorded, does NOT gate the ruling):** flash is 8/8 CHANGED while haiku is
6/13 CHANGED (7 inert). Both point CONVERT, so it does not bear on the disposition, but it is
either a probe artifact or a genuine model-difference finding (Flash authors mountain-claims that
resolve to typed scaffold/snare; Haiku authors more that surface `unknown` at the analytical
context). Worth a follow-up read under the twin-comparison harness if the operator wants it typed;
flagged here so it is not silently absorbed. `organization_floor_c0` (the 1 live testsets firing) is exactly this case:
`unknown→unknown`, grade commentary, verdict err (unknown-typed, no verdict rendered — class
behavior).

## What the override MASKS on the CHANGED seats

The `scaffold/snare → tangled_rope` overwrite replaces the metric classification with a
contested-type verdict. On the flash seats the masked metric type is often `snare` at high ε
(`competence_occupation` ε=0.95, `monopoly_rulebook` ε=0.85, `sacrifice_obligation` ε=0.95) — a
snare-at-seat is a real high-extraction structure that the tangled_rope overwrite currently
presents as generic contestation rather than as the routed snare it computed. This is the same
"the override MASKS the structural signal the router should reconsider" pattern that drove
FSM/FCR/constructed to CONVERT.

## Ruling menu

**(a) CONVERT** on the FSM/FCR-9 discriminated-severity template: route (not overwrite) at
FNL cascade-winners so the metric type (scaffold/snare) reads UNMASKED and the FNL diagnostic
becomes a signature annotation carrying the discriminant (authored mountain-claim + vic). By
family consistency this is the obvious answer — the CHANGED FNL seats are structurally
indistinguishable from the FCR-9 routed-9 (same override shape, same green→yellow unmask, same
available discriminant). **A CONVERT ruling owes a later build** (the conversion commit + 5-corpus
invariant sweep + the `abductive_helpers.pl:61,:93` FNL consumers + the UNCONDITIONAL,
not-seat-aware maxent boost at `maxent_classifier.pl:350–351`) — this census delivers only the
partition witness, matching how FSM/FCR were ruled.

**(b) KEEP** — the override stands. Cheap to defend only on the observation that the live
`testsets` leg has just 1 FNL firing and it is type-inert; but the twins show 14 genuine
type-changers with the masking pattern, so KEEP preserves a known FSM/FCR-class defect on the
comparison baseline.

**Weight of evidence:** CONVERT, on family consistency. The one caveat that softens it: on the
LIVE leg the conversion is near-free (1 inert firing), so if the operator wants to defer the build
cost, the live-corpus impact is nil until a twin/rebuild surfaces the changers — but the defect is
real and witnessed on the twins now.
