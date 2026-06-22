# FINDINGS — MaxEnt signature-override made seat-aware (OQ-138 maxent residual)

**Date:** 2026-06-21 · **Code:** edit to `prolog/maxent_classifier.pl` (HEAD `ab8d1d79` baseline)
· **Witness corpus:** live `testsets` n=92 + 20 archive/twin corpora swept.
Analyzed from evidence (pipeline slices, sweep output, stash A/B) — not from documentation.

## What changed

`apply_override_for_sig/3 → /4`: `C` threaded from the single call site (`apply_signature_override/3:318`).
Two converted signatures skip the MaxEnt boost at their **routed** seats, mirroring the type layer's
per-seat routing:
- `false_ci_rope`: `signature_detection:fcr_routed(C) -> DistOut = DistIn` (else keep the
  `fcr_override_enabled`-gated boost).
- `constructed_high_extraction`: `signature_detection:constructed_routed(C) -> DistOut = DistIn`.

All non-converted clauses (`natural_law`, `false_natural_law`, `coupling_invariant_rope`,
`coordination_scaffold`, `constructed_low_extraction`, `constructed_constraint`) ignore `C` —
behavior byte-identical. Affects BOTH serialized surfaces (`maxent_top_type` / classical `maxent_probs`
and `maxent_indexed`) since both `maxent_classify_one/2` and `maxent_classify_one_indexed/2` call
the same `apply_signature_override/3`.

Incidental fix on the way: a pre-existing `[C2]` singleton (maxent_classifier.pl, indexed-run
summary) was renamed `_` so the load-warning gate's line-keyed allowlist entry (`...:852`) no longer
drifts on edits above it; the stale allowlist line was pruned. Behavior-preserving.

## Pass-1 premise refinement (surfaced, not absorbed)

The plan's Context attributed routed seats' `maxent_top_type = tangled_rope` to the boost
("manufacturing where the type layer stopped"). **The substrate refines this:**

- `maxent_top_type` = argmax(post-override `maxent_dist`); `raw_maxent_probs` = pre-override
  `maxent_dist_raw`. **For all 12 routed seats, `maxent_top_type` already == argmax(raw)** — the
  conditional ×3 boost moves mass but never flips the CLASSICAL argmax at any routed seat.
- Positive control (corpus-wide): only **2** seats have `maxent_top_type != raw_argmax`, both
  **non-converted unconditional-override** signatures (`demographic_skill_mismatch_c0`/
  coupling_invariant_rope → scaffold→rope; `organization_floor_c0`/false_natural_law →
  scaffold→tangled_rope). The conditional FCR/constructed boosts flip **zero** classical tops.

So the residual was real but lived in (a) classical `maxent_probs` **mass** distortion and (b) the
**indexed** top — NOT a classical-top revert. The fix is correct regardless (it mirrors the type
layer); only the witness narrative changed. The genuinely-manufactured verdict the fix corrects is
`shinbutsu`'s indexed top (see below).

## Witness — routed seats (full diff `diff_witness.out`)

| check | result |
|-------|--------|
| routed classical `maxent_top_type` changed | **0 / 12** (all already == raw argmax) |
| routed `maxent_probs` now == raw (boost removed) | **12 / 12** |
| routed seats genuinely boosted in baseline (`maxent_probs != raw`) | 6 / 12 (divine, equal_protection, fictional, institutional_trust_erosion, press_strategic, shinbutsu) |
| routed **indexed**-top flips | **1**: `shinbutsu…incoherence_reading` tangled_rope (0.652) → **snare** (0.616) |
| routed `verdict_join` changed | **0 / 12** |

The single categorical flip — `shinbutsu` indexed top — matches the pre-registered raw estimate
(snare 0.616 / tangled_rope 0.384) to 3 decimals: the constructed ×3 boost had lifted tangled_rope
over snare on the indexed path; removing it restores the raw argmax (snare). This is the manufactured
verdict the fix corrects. Its `verdict_join` stays yellow/yellow (the join floors severity from
base + alerts; the indexed top feeds the non-headline `probe_maxent` signal), confirming the gap was
verdict-**capable** but benign-by-outcome on this seat — exactly the pre-registered prediction.

`equal_protection` stayed tangled_rope (reverted 0.604/0.396, as pre-registered — borderline but no
flip). `institutional_trust_erosion` stayed snare (already snare-dominant).

## Witness — negative half (the too-wide-selector guard)

Across **all 80 non-routed seats** (16 non_routed_fcr + 47 non_routed_constructed + inert/non-converted):

- `raw_maxent_probs` MOVED: **0** (input distributions byte-stable → no fix reached a non-routed seat)
- `maxent_probs` moved: **0** · classical top moved: **0** · indexed top moved: **0** ·
  `verdict_join` moved: **0**

The skip fired on **exactly the 12 routed seats and nowhere else** — zero ensemble ripple. The
non-converted unconditional boosts (`organization_floor_c0`/false_natural_law,
`demographic_skill_mismatch_c0`/coupling_invariant_rope) were NOT in the moved set: their boosts are
intact. **WITNESS RESULT: PASS.**

## Generality sweep — 21 corpora (`generality_sweep.out`)

Mechanism check per corpus: every routed FCR/constructed seat has the boost SKIPPED
(`maxent_dist == maxent_dist_raw`); non-routed boost-bearing seats KEEP the boost.

- **routed_STILL_boosted = 0 in ALL 21 corpora** (fcr_routed counts 0–78, constructed 0–63). The
  skip fires on routed seats everywhere; non-converted boosts untouched everywhere.
- **`original_v5` is PARTIAL, not clean** (`v5_coverage.txt`): `maxent_run` FAILS on v5
  (7/19 routed seats populated a dist; 12 unverified). **Confirmed pre-existing via stash A/B —
  baseline (edit removed) also `maxent_run=no`**, so NOT a regression from this change. Recorded as
  partial coverage, not "swept clean" (Pattern-6: didn't-look ≠ measured-clean).
- **SKIP-LOGGED (0 flat `.pl`, cannot load via flat glob):** `archives/datasets/{sotu,
  original_json, kernel_v1_json, kernel_v2_test2, iran_essay_2026-06-11, ab_pilot_pair_2026-06-07}`.
  Their absence is logged, not read as swept-clean.
- Archive caveat: chimera/pre-reset regimes (ID reuse, FNL bait) — the sweep tests **mechanism
  generality**, NOT corpus statistics; archive prevalence is not a detection result.

## Gates

- `validation_suite` DR-AUDIT SUITE: **Passed 92 / Failed 0** (with edit and stash-baseline — identical).
- `[FAIL] Schema mismatch` (data-validation sub-suite) + check_stack `data_repair` warnings:
  **pre-existing** (confirmed via stash A/B; structurally independent of maxent overrides).
- `check_stack`: clean vs baseline (no new wrong-qualifier findings; the new `fcr_routed`/
  `constructed_routed` calls are module-qualified, matching the `seat_overrides/2` runtime pattern).
- `./scripts/gate.sh`: **GREEN** (issues_status, omega check/selftest/index, known_state).
- Load-warning gate: 3 allowlisted, 0 unexpected.

## §1 unbound-cascade key (operator pin) — confirmed

`apply_signature_override/3:317` resolves `Sig` via `constraint_signature(C, Sig)` with `Sig`
UNBOUND (true cascade winner), passes the bound winner to `apply_override_for_sig/4`. The skip guards
`fcr_routed/1` / `constructed_routed/1` re-key on their OWN unbound cascade calls
(signature_detection.pl:1670/1689). The threaded edit introduces **no** bound-arg
`constraint_signature(C, false_ci_rope)`-style mis-key (the superheavy_decay shape). Verified by
inspection + the corpus-wide sweep (superheavy_decay_reading appears in both signature winner lists
and is correctly NOT routed under either predicate).
