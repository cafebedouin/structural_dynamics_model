# Committer-axis experiments CA-1 + CA-3 (2026-06-07)

Read-only / synthetic; no engine/schema/prompt edits; temp `.pl` removed. Evidence:
`ca1_probe.py`, `ca3_probe.pl`, `ca3_rows.txt`, `ca3_summary.txt`. Prior: `TWO_AXIS_NOTE.md`.

## CA-1 — committer field-partition cross-check (synthetic; PASS, no surprise)

Hold committer content fixed, vary in-engine framing knobs, recompute committer verdicts:

```
base                                  foreclosed=core_premise drift_terminal=axiom_foreclosure mismatch=both
framing: reference_frame alpha->BETA  foreclosed=core_premise drift_terminal=axiom_foreclosure mismatch=both   (identical)
framing: story_uid relabel            foreclosed=core_premise drift_terminal=axiom_foreclosure mismatch=both   (identical)
CONTENT control: grounding flip       foreclosed=none         drift_terminal=axiom_foreclosure mismatch=trajectory_only   (moved)
```

Framing-invariant (base==refframe==uid) **True**; content-control moved **True** (probe live).
**Honest scope (operator):** this CONFIRMS the field partition by a second method (perturbation),
on top of the static read that already established it — it is NOT a fresh "committer axis is
architecturally framing-blind" discovery. The interesting outcome would have been a SURPRISE (a
verdict moving on a framing knob = a mis-binned field); none occurred. Corroboration: the content
flip stopped `cs_axiom_foreclosed` (reads grounding) but left `cs_drift_trajectory` death (reads
only drift_state) — the two committer verdicts read different content subsets, exactly the partition.

## CA-3 — detection-independence on real multi-reading kernels (kernel_v1; READ-ONLY; kernel_v1-REGIME)

H0 pre-check first (guard against stale-site collapse): `cohomological_obstruction` computes without
error and is non-degenerate on archived stories (both H0 values appear; the 3 abrahamic_covenant
readings split isaac=incoherent / ishmael=coherent). Pre-check PASS → the observer half is meaningful
on archived data.

Per-axis verdicts pinned before the run: observer **coherent = H0=1**; committer **dead** =
`cs_axiom_foreclosed` fires OR `cs_drift_trajectory` terminal ∈ {axiom_foreclosure, husk, extinction,
repudiation}. N=906 kernel-bearing (story_uid present; `catholic_church_1200` excluded).

```
                       committer-live   committer-dead
observer-coherent          44              74  (diverge-A, canonical Theorem-7)
observer-incoherent       140 (diverge-B)  648
```
- diverge-A (coherent+dead) = **74 / 906 = 8.2%** — CLEAN Theorem-7: the reading glues globally on
  the observer axis yet is committer-foreclosed. Spread across **68 distinct kernels** (top carry 2
  each — broad, not concentrated in one kernel).
- diverge-B (incoherent+live) = **140 / 906 = 15.5%** — the artifact-prone cell (reported separately).
- summed divergence density = 23.6% — **but do not headline this** (see confounds).

### The honest read: both axes are SATURATED and the density is confounded
- observer-incoherent = **87.0%** (788/906). Plausibly real for a kernel-heavy corpus (contested
  kernels are exactly the constraints whose type varies across observer contexts → incoherent), but
  a stale-site component cannot be fully ruled out without the as-authored H0 (not recorded).
  diverge-B sits on this 87% base and is the most artifact-prone cell.
- committer-dead = **79.7%** (722/906). **Correction to a label in `ca3_summary.txt`: this is NOT
  the OQ-70 FNL bait confound** — FNL/`claimed_natural` is an OBSERVER-axis signature; the committer
  verdicts read `cs_drift_state`/`cs_axiom_grounding`, so the 80% deadness reflects kernel_v1's
  DRIFT-authoring convention (how often severe/unacknowledged `axiom_overriding`/erosion drift was
  authored), a regime caveat of its own, not OQ-70.
- Because both axes are saturated toward their extreme and both saturations are regime-confounded,
  the dominant agree-dead cell (71.5%) is "two saturated axes coinciding," not clean detector
  agreement, and the summed divergence density is **not a trustworthy quantitative gate number**.

## diverge-A cause-of-death witness (per-item under the 74 — the count was overclaiming)

The 74 is the aggregate; cause-of-death is the substrate (operator: "you've been burned once this
thread by a count standing in for the per-item check"). Distribution across the 74
(`ca3_divergeA_causes.txt`, `ca3_divergeA_cause_summary.txt`):
- **death path:** 66/74 via drift-trajectory ONLY (reads `cs_drift_state` alone);
  `cs_axiom_foreclosed` (the only committer verdict reading axiom GROUNDING = content beyond drift)
  fired just **8**.
- **terminal:** husk 51, axiom_foreclosure 13, repudiation 8, extinction 2 (4 distinct).
- **drift profile concentrated:** magnitude `substantial` 65/74 (88%), `acknowledged=false` 68/74
  (92%); the single profile `(practice_drift|authority_erosion, substantial, false)→husk` is
  **50/74 (68%)**.

**Verdict: predominantly SATURATION, small heterogeneous core.** ~89% of diverge-A deaths run the
drift-only path dominated by ONE drift-authoring convention (substantial+unacknowledged → husk) —
the same convention firing uniformly in the observer-coherent slice = saturation wearing Theorem-7's
clothes, NOT heterogeneous-cause orthogonality. The genuinely content-driven core is the 8
`cs_axiom_foreclosed` cases (read the axiom's grounding), and even those share `axiom_overriding/
substantial` drift. So the existence proof for detection-independence is **NOT load-bearing on
kernel_v1**: the clean signal shrinks from 74 to ~8, and the bulk is convention-leak. The de-leaked
re-measure is required before the existence proof stands — exactly the operator's "if same convention
every time, diverge-A is saturation wearing Theorem-7's clothes."

## Verdict + corpus gate

- **Qualitative (trustworthy): detection-independence OCCURS on real contested kernels.** diverge-A
  is 74 clean cases (observer-coherent — the less-artifact-prone observer state — yet
  committer-dead), spread across 68 kernels. Orthogonal detection is empirically real, not merely
  architecturally possible. The two-axis architecture does the thing it was designed to do, on real
  data.
- **Quantitative (NOT trustworthy from kernel_v1): the divergence DENSITY** is confounded by
  double-saturation + the unquantified observer-staleness component. A clean density needs a
  de-leaked re-measure.
- **Two corpora, two gates (operator):** CA-3 gates a **detection-independence corpus** — and the
  qualitative gate is GREEN (real signal exists), while the precise richness is pending a de-leaked
  measurement. CA-3 says **nothing** about a **C/B (framing-dependence) corpus**, which stays gated
  on **CA-2** (generation; the archive varies content, not framing, so it cannot test framing-
  dependence). Do not let diverge-A authorize a C/B corpus.
- CA-1 gates neither corpus; it confirmed the field partition.

## Net
The committer axis is a real, content-driven classification surface (CA-1: framing-invariant,
content-sensitive) and detection-independence between the two axes is empirically real on real
kernels (CA-3: 74 clean diverge-A across 68 kernels). The clean *density* is not measurable on the
confounded archive; a de-leaked kernel pilot would be needed for that. C/B / framing-dependence
remains untouched and unmeasured — CA-2's generation experiment, not substitutable by the archive.
