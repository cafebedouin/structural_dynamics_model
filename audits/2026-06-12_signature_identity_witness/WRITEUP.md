# Signature-identity witness — does the engine's structural typing carry story identity across generation draws?

**Date:** 2026-06-12. **Substrate:** `archives/datasets/kernel_v1` (corpus_path overlay,
fresh process; 1,106 testsets). **Code:** main @ `9a992459`. **Probe:** `probe.pl`; raw
output `probe_output.txt` (this directory).

## Question (pre-registered in probe.pl before the run)

The kernel_v1 naming-drift triple — press/Reformation topic, three independent
generation runs, three names for nominally "the same" reading
(`press_reformation_causality__technological_inevitability`,
`press_reformation_causation__technological_determinism`,
`technology_reformation_causality__technological_determinism_reading`) — do the three
draws land NEAR each other in the engine's structural-identity space
(`logical_fingerprint/2`, 7 dimensions: shift/properties/voids/actors/drift/zone/coupling),
and do topic-distinct stories land APART? Either outcome was declared the finding:
separation ⇒ the math carries cross-draw identity; overlap ⇒ the math identifies
story-kinds, not stories.

This is the free natural experiment for the OQ-109 cohort-zero question "what carries
constraint identity across the regen" — drift already realized, no API spend.

## Probe controls (all passed)

- **Membership:** all 6 ids confirmed via `corpus_loader:corpus_constraint/1` (pasted in output).
- **Vacuity guard:** all 6 full fingerprints printed and populated — matches are not
  unknown-filled. (Caveat recorded below: the triple's *within* agreements include
  agreement-in-absence.)
- **Matcher-can-say-false:** all 9 between-pairs returned falses (0/7 each).

## Result

| pair class | match counts (of 7 dims) |
|---|---|
| WITHIN triple (3 pairs) | **3, 6, 3** |
| BETWEEN triple × 3 topic-distinct controls (9 pairs) | **0, 0, 0, 0, 0, 0, 0, 0, 0** |
| CONTROL × CONTROL (3 pairs) | **3, 3, 6** |

Dimension-level detail (raw output is authoritative):

1. **Draws 1 & 3 cluster tightly: 6/7**, including an *identical* perspectival shift
   pattern `[tangled_rope, scaffold, scaffold, tangled_rope]`, identical properties,
   actors (concentrated/none), zone, coupling. Only drift differs.
2. **Draw 2 is a different mechanism class.** Shift `[mountain, rope, rope, mountain]`;
   properties `[has_temporal_data, natural]` vs `[coordination, has_beneficiaries,
   natural]`; actors none/none vs concentrated/none. Its 3/7 "agreement" with each
   sibling is **entirely agreement-in-absence**: `voids [] = []`, zone
   negligible=negligible, coupling independent=independent (score 0.000 both). Pattern-5
   shadow: nothing positive is shared. Same material, re-drawn, produced a story the
   engine reads as a different mechanism.
3. **Topic-distinct ≠ structurally distinct:** control pair
   `blockchain_settlement_finality | neural_interface_standardization` matched **6/7** —
   as close as the closest within-triple pair. This is by design: the
   `logical_fingerprint.pl` header states two constraints with the same fingerprint are
   "LOGICALLY ISOMORPHIC — they operate through the same mechanism regardless of domain."
4. The clean 0/7 on all between-pairs reflects this triple sitting in a quiet corner
   (negligible zone, independent coupling, natural-claimed) while all three controls are
   high-extraction coupled stories — real separation on this draw of controls, but with
   n=3 controls it is not a tolerance calibration.

## Verdict

**The fingerprint space types KINDS, not stories — witnessed in both directions on the
first natural experiment:** (a) one of three same-material draws escaped its siblings'
kind entirely (no positive dimension shared); (b) two different-material stories share a
kind at 6/7. A signature-match tolerance that re-links draw 2 to its siblings does not
exist at any threshold: draw 2 is *closer to nothing-in-common* with its siblings than
blockchain is to neural_interface.

Consequences, scoped:

- **Kind-level meta-analysis survives generation stochasticity** — this is what the
  apparatus (fingerprint/orbit/Boltzmann/maxent) is FOR, and on this triple the
  draw-stable dimensions were {zone, coupling, voids} while {shift, properties, actors,
  drift} swung. Note what swung includes **shift = the classification itself**: type
  prevalence over n=1-per-story draws samples generation noise (consistent with OQ-26).
  The OQ-109 replicate probe's stability table is exactly the instrument that turns this
  one data point into a calibration; this triple is data point zero for it, from the
  OLD-prompt regime (upper bound on drift: whole-pipeline re-run, not fixed-spec redraw).
- **Story-level identity across regen cannot ride the math.** Per-story mechanisms in
  OQ-109 Phase C — the fail-closed exclusion list (institutional_trust_erosion), the
  named re-witness pair (organization_floor, demographic_skill_mismatch) — must anchor
  through the seed link (`seeded_from: <archived story_uid>` in provenance) recorded at
  regen time, then key on whatever cohort zero produces. Signature-keying the exclusion
  list is ruled out by this witness (it would have lost draw 2). The seed link is
  provenance/ground-truth, not an identity *mechanism* — identity of the new draw is just
  the new draw's own id.
- **Altitude control:** this witnesses the 7-dim categorical fingerprint with unanimous
  per-dimension matching, on one triple and three controls, kernel_v1 regime. It licenses
  "identity-by-signature fails HERE"; it does not license "no metric over engine features
  could ever separate" — but the burden now sits on any proposed metric to pass this same
  natural experiment (re-link draw 2; separate blockchain|neural) before being trusted
  with identity.

## Relation to the OQ-109 perspective problem

Phase B's witness battery is conditioned on the committed 62-story corpus; the
determinism-frontier ruling (CLAUDE.md, 2026-06-12) already states regenerated stories
are new draws. This probe adds the missing half: the engine's typing machinery does not
recover cross-draw identity after the fact, so anything name- or identity-keyed across
the regen boundary (exclusion lists, named re-witnesses, per-story dispositions) needs
the seed link authored at generation time — identity is shaped *forward* at generation,
never recovered *backward* by analysis. Deterministic re-runs behind the frontier
(byte-identical pipeline diffs, gauntlet reconciliations) certify the snapshot, not the
next cohort.
