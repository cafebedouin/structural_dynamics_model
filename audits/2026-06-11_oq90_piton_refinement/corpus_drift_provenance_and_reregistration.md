# Corpus drift + re-registered OQ-90 expected delta (2026-06-11)

## The drift (provenance — operator-required record)

The OQ-90 plan pre-registered its expected delta against a **48-testset** corpus "verified live
this session, 2026-06-11" and predicted the refinement flips **exactly one** constraint
(`regulatory_measurement_gap`), declaring "extra rows are a failure." Phase-0 recon found the live
pipeline corpus is **52**, and the delta math now disagrees: a second constraint
(`institutional_trust_erosion`) flips by identical logic.

Resolved seam (git, 2026-06-11):

- **HEAD (`411db0e7`) tracks 48 top-level testsets** — this is the plan's "48".
- The **live pipeline corpus is 52** = those 48 tracked **+ 4 untracked working-tree files**
  (pipeline loads the working tree; manifest `code_dirty=true`):
  - `generational_value_divergence.pl` — not diffuse
  - `institutional_trust_erosion.pl` — **diffuse + prohibitive + false_ci_rope** (the 2nd flip)
  - `intra_party_fragmentation.pl` — not diffuse (authors a named seat)
  - `representation_legitimacy_gap.pl` — not diffuse (authors a named seat)
- All four are `?? ` (untracked) in `git status`; zero modified-tracked testsets. They are not on
  any branch line — at session-start HEAD (`6c2ae0e2`) and current HEAD (`411db0e7`) the file
  `prolog/testsets/institutional_trust_erosion.pl` does not exist; the only commits that ever
  added it (`bbee831e`, `eba5a22b`) are pre-reset (the corpus was reset 2026-06-05). They are
  restored/untracked working-tree artifacts.

**Why mid-unit:** the corpus is deliberately small and churning because it is the substrate being
used to fix exactly these problems (operator context note, OQ-92 PREREGISTRATION). The drift is
authoring activity, not a bug — but a silently-growing live corpus under a pre-registered diff is
a seam that reads as a bug later, hence this record.

**Reproducibility flag (surfaced, not self-resolved):** the OQ-90 Phase-3 output-changing diff
toggles `piton_refinement_enabled` 0→1 over the *same* working tree both arms, so the untracked
testsets do not corrupt the diff (both arms see the identical 52-corpus). But a fresh clone at
`411db0e7` would see only 48 testsets and would NOT reproduce the 2-row delta — it would reproduce
a 1-row delta (`regulatory_measurement_gap` only). For the OQ-90 result to be reproducible on a
clean clone, the 4 untracked testsets must be committed. They are not mine (I did not author
them); flagged for the operator to commit rather than committed unilaterally.

## Re-registered expected delta (witnessed by the hand-audit)

Per operator ruling (2026-06-11), after `institutional_trust_erosion` passed the K=0 hand-audit
(`diffuse_audit_institutional_trust_erosion.md`, 0/1 observed):

**Expected Phase-3 delta = exactly TWO rows, `tangled_rope → piton`:**
1. `regulatory_measurement_gap` (all FCR-reaching perspectives)
2. `institutional_trust_erosion` (all FCR-reaching perspectives)

**Leak controls (MUST stay `rope` regardless):** `organization_floor`,
`reprogramming_safety_toxicity`. If either flips, that is a cascade-ordering bug upstream of the
refinement, NOT extra piton recovery.

**Failure conditions (unchanged in spirit, updated for the 2-row substrate):**
- zero-delta is a FAILURE (both flips are pre-witnessed reachable);
- any row other than the two named above is a FAILURE;
- either leak control flipping is a FAILURE;
- corpus-fitted ripple (maxent_probs / arakelov heights) is expected and not a stop signal.

This registration supersedes the plan's 1-row figure **on the substrate ground stated above**, not
by inline amendment of the prediction — the substrate changed (48→52, +`institutional_trust_erosion`
diffuse), the hand-audit gate was extended to cover it, and the new figure is derived from that
audit. Had the audit flagged a capturing seat, the registration would have stayed 1 row and
`institutional_trust_erosion` would route to the regeneration path instead.
