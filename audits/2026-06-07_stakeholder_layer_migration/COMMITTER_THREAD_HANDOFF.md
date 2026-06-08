# Committer-axis thread — handoff (banked 2026-06-08)

Cold-read entry point for the committer-axis investigation. Tracker: **ISSUES.md OQ-87**
(+ OQ-83 stakeholder migration, OQ-86 standalone). Reports in this dir:
`TWO_AXIS_NOTE.md`, `CA_COMMITTER_AXIS.md`, `PILOT_STEP01_REPORT.md`. Status: **BANKED / PARKED** —
the experimental sub-questions are answered; the overarching existence proof is UNPROVEN and its
next move is a fresh-decision larger study, NOT a parameter change.

## The question
The DR engine is two-axis (v7): the OBSERVER axis (`dr_type` orbit over P,T,E,S) and the COMMITTER
axis (`cs_structure` → `cs_axiom_engine`/`cs_drift_engine`). The thread asked: is there a
framing-sensitive / Type-B classification layer, and is detection-independence (Theorem 7 —
observer-coherent readings can be committer-foreclosed) empirically real?

## Settled findings (each witnessed; pointer in parens)
1. **Two-axis architecture is real and the axes are separate** (`TWO_AXIS_NOTE.md`,
   `two_axis_witness.py`). The observer orbit is framing-blind — it ignores authored perspectives
   (A1) AND `cs_structure` (witnessed: identical observer orbit, different committer verdict). The
   committer axis is a SEPARATE structure-sensitive classification surface. **Consequence:
   observer-axis Type-B is architecturally foreclosed**; the genuine C/B question is committer-axis
   only. An observer-axis corpus run measures generation-resolution variance, NOT ontology.
2. **CA-1 — committer field partition confirmed** (`CA_COMMITTER_AXIS.md`, `ca1_probe.py`).
   Committer verdicts are framing-INVARIANT (vary reference_frame / story_uid → identical) and
   content-SENSITIVE (grounding flip moves `cs_axiom_foreclosed`). This is a cross-check on the
   field partition by perturbation, NOT a fresh architectural finding (the static read already
   established it). No mis-binned field.
3. **CA-3 — detection-independence on kernel_v1 is NOT load-bearing** (`CA_COMMITTER_AXIS.md`,
   `ca3_*`). Per-axis verdicts pinned (observer coherent=H0=1; committer dead=`cs_axiom_foreclosed`
   OR drift terminal ∈ {axiom_foreclosure,husk,extinction,repudiation}). 74 diverge-A (coherent+dead)
   of 906 LOOKED like a real existence proof — **but the per-item cause-of-death witness showed
   ~89% is ONE drift convention** (substantial+unacknowledged → husk); clean content-driven core ≤8.
   The count misidentified the effect. → banked the standing rule *a gating count is not a finding
   without its composition* (build_discipline.md).
4. **Step 0 — observer claim-drift is MODEL-STABLE** (`PILOT_STEP01_REPORT.md`, `step0_model_omega.py`).
   The 4c claim-layer framing effect (stakeholder→`rope`, four-tuple→`tangled_rope`) reproduces 3/3
   under Sonnet, identical to Gemini. Clean bankable result; observer-axis loose end closed.
5. **Pilot Steps 1/1b/matched — the kernel_v1 husk-saturation is reading-set + magnitude-authoring,
   NOT a model artifact, NOT removable by model swap** (`PILOT_STEP01_REPORT.md`). The drift example
   anchors DIRECTION (authority_erosion 14→4 on removal) but not the husk-driving magnitude/ack.
   The MATCHED run (same manifests/backend/prompt, vary only GEN model) showed **the Haiku→Sonnet
   bump is NOT confirmed**: substantial 76%→62% (below threshold), ack-false 49%→80% (wrong
   direction). `ack-false` ranges 49–92% across arms = **reading-set-dominated**, not model;
   `substantial` is robustly 62–88% (persistent authoring feature). The matched control OVERTURNED
   the unmatched Step 1b ("Sonnet de-saturates") — caught a wrong conclusion before it became a
   GEN_MODEL edit.

## Open / unproven
- **Detection-independence existence proof: UNPROVEN.** kernel_v1's diverge-A is saturation
  (substantial-magnitude + that corpus's reading-set), not ground truth; and it is NOT fixable by
  switching GEN model. A clean proof needs a MUCH LARGER de-leaked kernel study to average out the
  dominant reading-set variance, then the CA-3 measurement + diverge-A cause-composition against the
  pinned 3-conjunct criterion (no single terminal×drift-profile >40%; ≥25% via `cs_axiom_foreclosed`;
  among those, no single grounding-profile >40%). This is rebuild-scale — a fresh decision, NOT a
  continuation.
- **Committer-axis C/B (framing-dependence): UNTESTED.** Needs CA-2 (generation: same commitment,
  two framings → does the committer verdict move). The kernel_v1 archive CANNOT substitute — its
  sibling readings vary CONTENT (different axioms), not framing. A separate experiment.
- **OQ-86** — pairwise who-extracts-from-whom as report commentary; standalone, unblocked, no
  migration dependency.

## The single next move (and why it's gated as a fresh decision)
A LARGER de-leaked kernel study is the prerequisite for the existence proof (kernel_v1 too saturated;
model swap doesn't fix it; small-N pilots are dominated by reading-set variance). It is rebuild-scale
generation. Decide whether the answer (does detection-independence occur cleanly on real contested
kernels) is worth the cost as a fresh decision with the thread closed behind you — do not roll into
it because the machinery is warm. CA-2 (committer C/B) is an independent second experiment.

## Method rules banked this thread (for the next instance)
- **A gating count carries its composition in the same pass** (build_discipline.md). Witnessed
  twice: 4b consumer-grep, CA-3 diverge-A cause distribution. The count alone is a different and
  usually wrong result.
- **Model-confound guard:** a "persists under example-removal" result on one model is not "real
  property" — confirm on a second model (the guard caught Step 1b's misread).
- **Matched control over separate runs:** independent re-SCOPE introduces reading-set variance that
  can DOMINATE small-N distribution comparisons (it overturned Step 1b). Vary one thing; hold the
  substrate (same manifests).

## Artifacts / housekeeping
- All measurements + scripts committed (commits on `kernel-first-router`: two-axis `583b93b3`,
  CA `042be7ba`/`10abfce4`, build-discipline `b268cf4c`, Step0 `79a30cd0`, Steps1/1b `79606796`,
  matched `53d51b5f`). Cost spent ≈ 284 generation calls.
- **Run-tagged pilot stories** (`prolog/testsets/pilot_{s1_current,s1_neutral,s1b_sonnet,matched_haiku,matched_sonnet}/`
  + matching `json/`) are UNTRACKED, glob-isolated from the live corpus (the loader's non-recursive
  glob excludes run-tag subdirs), and DISPOSABLE — the measurements are banked in the reports; the
  raw stories are reproducible. Safe to delete; safe to leave.
