# Pre-registration — MaxEnt seat-aware override skip (OQ-138 maxent residual)

Written BEFORE editing `maxent_classifier.pl`. Baseline = `outputs/pipeline_output.json`
manifest `pipeline_run_at 2026-06-22T01:23:47Z`, HEAD `ab8d1d79`, n=92.
Routed-seat lists pinned from `baseline_routed_seats.txt` (live probe): **9 fcr_routed + 3
constructed_routed**.

## Premise refinement found in Pass 1 (surface, do not absorb)

The plan's Context says the boost makes routed seats' `maxent_top_type = tangled_rope` (the
override target), "manufacturing where the type layer stopped." **The substrate shows this is
imprecise for the CLASSICAL path:**

- `maxent_top_type` = argmax of post-override `maxent_dist`; `raw_maxent_probs` = pre-override
  `maxent_dist_raw` (json_report.pl:616/633, maxent_classifier.pl:414/622-624).
- **For all 12 routed seats, `maxent_top_type` already == argmax(`raw_maxent_probs`)** — the
  conditional ×3 boost moves mass but never flips the classical argmax at any routed seat.
- Positive control (corpus-wide): only **2** seats have `maxent_top_type != raw_argmax`, both
  **non-converted unconditional-override** signatures (`demographic_skill_mismatch_c0`/
  coupling_invariant_rope, `organization_floor_c0`/false_natural_law). The conditional FCR/
  constructed boosts flip **zero** classical tops.

So the residual manifests as (a) **mass distortion** in the full `maxent_probs` dist at routed
seats, and (b) an **indexed-top flip** on the indexed path — NOT a classical-top revert. The fix
is still correct (it mirrors the type layer's per-seat routing) and the operator's design ruling
stands; only the witness narrative changes.

## Positive half — movers (predicted CHANGES)

| seat | path | baseline | predicted post-fix | mechanism |
|------|------|----------|--------------------|-----------|
| (all 12 routed) | `maxent_probs` (classical full dist) | post-override | == `raw_maxent_probs` | boost skipped → raw dist passes through |
| shinbutsu…incoherence_reading | `maxent_indexed.top_type` | tangled_rope (0.652) | **snare** | constructed ×3 boost lifted tr over sn; raw est sn 0.62 > tr 0.38 |

## Negative half — non-movers (predicted UNCHANGED)

**Classical `maxent_top_type` — UNCHANGED for ALL 12 routed seats** (already == raw argmax):

- fcr_routed: basic_law…parliamentary=rope, conceptual_framework=rope, divine…syncretistic=scaffold,
  fictional_construct=scaffold, jewish…cultural_zionist=rope, lausanne…guarantor=rope,
  llm_synthesis_capacity=scaffold, neutron_star…=rope, press…strategic_deployment=snare.
- constructed_routed: equal_protection…colorblind=tangled_rope, institutional_trust_erosion_c0=snare,
  shinbutsu…incoherence=tangled_rope.

**`maxent_indexed.top_type` — UNCHANGED for 11 of 12 routed seats** (shinbutsu is the only flip):

- All 9 fcr_routed: indexed tangled_rope mass ≈ 0 (boost target has no mass to amplify) → tops
  rope/scaffold/snare unchanged.
- equal_protection…colorblind: tangled_rope, raw est tr 0.60 > sn 0.40 → **predict UNCHANGED**
  (borderline — verify; if it flips it is still a mover, record mechanism).
- institutional_trust_erosion_c0: snare (raw est sn 0.86) → UNCHANGED.

**Non-routed FCR/constructed + every inert/non-converted seat — `raw_maxent_probs` UNCHANGED and
`maxent_top` UNCHANGED.** The skip fires on exactly the 12 routed seats and **nowhere else**.
- 16 non_routed_fcr, 47 non_routed_constructed seats (baseline_slice.json) keep their boost.
- Discriminator for any non-routed delta: a moved non-target seat is **ensemble-explained only if
  its `raw_maxent_probs` are byte-unchanged pre/post** (input dist did not move; delta is
  downstream renorm) AND the renorm path is named. If a non-routed seat's `raw_maxent_probs`
  moved, that is the fix reaching too wide (flag #2) — fix, do not explain.

## verdict_join (the operator's mandatory witness pin)

`maxent_top` is the INPUT to the join, not the join. Predict: **verdict_join UNCHANGED for all 12**
(the join floors severity from base_verdict + alerts; maxent feeds the non-headline `probe_maxent`
signal). Baseline verdict_join for the 12 routed (verdict / base):

- fcr_routed: basic_law=yellow/yellow, conceptual=yellow/yellow, divine=yellow/yellow,
  fictional=yellow/yellow, jewish_cz=yellow/yellow, lausanne=yellow/yellow,
  **llm_synthesis=red/red**, neutron=yellow/yellow, **press_strategic=red/yellow**.
- constructed_routed: equal_protection=yellow/yellow, **institutional_trust_erosion=red/yellow**,
  shinbutsu=yellow/yellow.

Post-fix: **enumerate every verdict_join delta and attach mechanism.** A moved join is the gap
having been verdict-live and the fix surfacing it correctly — explain, do not suppress.

## FSM scope-gate (Pass 1 confirmation)

CONFIRMED against substrate: `false_summit_mountain` has **no** `apply_override_for_sig` clause
(maxent_classifier.pl:323-347) and no alias/mapping to a boost target — it falls to the catch-all
no-op. FSM needs no MaxEnt change. Scope holds; no fourth untracked residual.
