# Engine Handoff No. 7 — Handoff 6 Re-Cut Cold, Its Headline Corrected, Surface-2 Still the Build

*2026-05-31. A snapshot for the next session. **This handoff is self-contained** — the
section below carries everything load-bearing from Handoffs 1–6; the prior docs are the
deeper substrate (full thesis, per-session witness ledgers) if you want them, not a
prerequisite. **The witness of record is the committed substrate, not this doc.** This
session's corrections live in `KNOWN_STATE.md` (commits on branch `audit-doc-corrections`);
where this doc and a committed file disagree, the file wins. Re-cut cold — do not trust this
doc over a run.*

---

## What this session was, in one line

It did not build. It **re-cut Handoff 6 cold** — the read-only verify-or-correct pass the
prior handoff demanded before any Surface-2 work — and the pass overturned Handoff 6's own
headline. That is the method working, not failing: Handoff 6 said "distrust this doc, re-run
before building," and the re-run found a perturb-confirmed tag sitting one layer above its
witness. The substrate is now re-cut and the corrections are committed. The build (Surface-2)
is unchanged as the next task and is owed a fresh session.

---

## Handoffs 1–6 in brief (everything load-bearing, so you don't need the other docs)

**The thesis — what the engine is FOR.** The engine is a *radio telescope for constraints*:
one lens (extraction ε, directionality, the χ = ε·f(d)·σ(S) classification, H¹ perspectival
fracture, the CS commitment layer) applied to a signal — an argument, a policy, a paper — to
make visible the folds inside it: the cover story a constraint tells about itself, the seat a
claim is issued from, the beneficiary a framing routes out of view. It **points; it does not
adjudicate.** Many diagnostics run; none overrides the base classification; the *divergence
between lenses is the signal*, agreement across them is the robust band. **Engine points,
human stakes.**

**Why it cannot self-certify (the load-bearing structural limit).** Every check the engine
runs to test one of its own verdicts is itself a reading — a cut, a seat. No perturbation
steps outside all readings to certify a verdict as seat-free. So grounding is undecidable
*from inside the engine*; the only honest trust is **external perturbation that survives —
claim by claim, never the whole at once.** A verdict that has been perturbed and held is an
instrument reading. A verdict not perturbed is fabrication with the engine's typography on it.
**Reasoning about what drives an output is unreliable here; perturb-and-observe is the only
ground truth. If you catch yourself predicting what a param does, stop and run it.**

**The witness-tier ledger (the discipline).** The recurring failure is *claims sitting one
tier above their evidence* — and it drifts most invisibly when the surrounding work is strong
enough to vouch for it. **Paste-or-untag, per claim, no exceptions.** Every claim carries a
tier — grep-witnessed / perturb-confirmed / path-asserted / instance-reported-not-seen /
HYPOTHESIS — and a claim without a tier is not done. A guard is witnessed by a run that makes
it *fire*, never by a code-read confirming it is present.

**The spine (the generalization, now in `docs/technical/build_discipline.md`).** *Every defect
here is an absence that presents as a presence.* Something is missing — a consumer, a canonical
fact, a measurement, an authored datum — and a *success-shaped token* fills the hole so the
read site cannot tell it from the real thing. **The single fix everywhere:** carry the
provenance bit with the value (return `unknown` not `0.5`; fail-closed on absence;
wire-or-fail-loud; let the engine dispatch). **Diagnostics are not exempt:** a clean read is
byte-identical to a read that never looked. Every diagnostic needs a *positive control* — run
it against a case it must flag and confirm it flags — or its green is unfalsifiable. This holds
for reasoning too. (This session it held for the *tooling*: see the scramble note below.)

**The three observable surfaces.**
- **Surface 1 — static type.** Mature; `perturb.py` sweeps all 191 engine params + 6 authored
  fields. **At its ceiling:** ~19–20 unwitnessed kernels are *reached but signature-locked* —
  the metric flips and a signature override (`false_natural_law` / `coupling_invariant_rope`)
  eats it. Surface 1 cannot witness them.
- **Surface 2 — excess-extraction / PoA** (`boltzmann_compliance:excess_extraction/2`).
  Proof-of-life witnessed, **primitive not built.** The **critical path** for those kernels;
  the boltzmann guard that locks them (`signature_detection.pl:835–836`) is
  Surface-2-displaceable. *The claim that perturbing the Boltzmann floor flips those locked
  kernels' final types is HYPOTHESIS, not tested* — the Surface-2 primitive's first
  verification target, and the claim that still gates the project's headline.
- **Surface 3 — temporal / drift** (`drl_composition.pl:classify_at_time/4`). Skips
  `integrate_signature_with_modal`, so Surfaces 1 and 3 diverge by construction. Its
  fabricated-`Supp=0.5` block was fixed in Handoff 6 (row-23). Still premature for a primitive
  until regen authors temporal series.

**The denominator.** 191 engine params + 6 authored fields = **197 type-moving predicates on
Surface 1**, by *bidirectional dataflow trace* (residual zero both directions). Completeness is
edge-closure (following dataflow), not node-search (grepping names).

*Full thesis and per-session pasted witnesses: Handoffs 1–6. Witness of record for this
session's corrections: `KNOWN_STATE.md` (committed).*

---

## What this session changed (all committed; verify against the blobs, not this doc)

The verify-or-correct pass ran read-only first (deciding pass), then a write pass landed three
docs-only commits on branch `audit-doc-corrections` (off `3f6c0c52`, **unpushed as of this
writing**). Each was verified against the committed blob — SHA resolves, one `.md` each, zero
code files, signature present in the commit (not tree-only).

**Correction 1 — Handoff 6's headline was wrong. The NL-gate fix is diagnostic-layer, NOT
classification-changing.** (`a0163405`, `KNOWN_STATE.md` B1.)
Handoff 6 (lines 219–222) claimed the NL-gate fail-close was "classification-changing for the
3-case tail" and used that to scope T.1's "cosmetic" verdict ("do not cite cosmetic
unqualified"). The re-cut falsified it. The fix declined 3 raw `natural_law_signature`
certifications (raw match 5→2) — TRUE, a **diagnostic-layer** change. But final `dr_type` held
at `tangled_rope` for all 3 (`behavioral_competence_reading`, `disparity_as_depth_signal`,
`generational_economic_decline`) at **both** `39630182` (parent of the NL-gate fix) and HEAD.
Mechanism: all 3 claim naturality via `explicit_mountain_claim`; the cascade resolves them to
`false_summit_mountain`, which sits *higher* in the priority cascade than the `natural_law`
clause and reads `constraint_beneficiary` directly — so the raw `natural_law` match was
*shadowed* for these 3 before and after. The gate now discriminates honestly **at the raw
diagnostic layer**, and that layer is cosmetic to final type. The accurate split:
*declined-a-raw-certification* (TRUE, 5→2) ≠ *classification-changing* (FALSE, 0 final-type
moves). Handoff 6 conflated them. So "do not cite cosmetic unqualified" itself inverts: at the
final-type level the change is fully cosmetic; non-cosmetic only at the raw-diagnostic layer.
*Tier: perturb-confirmed (final dr_type at two commits, cascade mechanism pasted). The prior
"perturb-confirmed" tag was raw-count evidence standing in for a final-type claim — witness one
layer below the claim. This is the spine, fired on the prior handoff.*

**Correction 2 — `demotion_pass.py` is engine-blind.** (`b0436cdf`, `KNOWN_STATE.md`.)
Its 6/0/20/0/24/141 buckets are a regex param-count over `config.pl`/`constraint_indexing.pl`
+ hand-maintained `_WITNESSED`/`_UNPERTURBABLE`/`_SHADOWED` dicts; no `swipl`, no classifier
call. A "block matches" result **cannot witness any engine change** — so Handoff 4/5/6's
"re-run the demotion sort before trusting the block" is mis-routed through it. The block HELD
here *by construction*, which says nothing about post-fix engine behavior. The block's real
validity rests on whether those dicts still match live `perturb.py` survival on the post-fix
engine — **UNVERIFIED / OPEN.** Route item-1-type verification through `perturb.py`, not this
script. *Tier: buckets grep-witnessed; engine-blindness path-asserted (import list pasted).*

**Correction 3 — Row-26 "all six NEUTRAL" is 2–3-of-6 witnessed.** (`bfc0f5a2`, `ISSUES.md`
OQ-41.)
`outputs/tripwire_row26_results.json` carries 3 rows (`purity_scoring`,
`drl_boltzmann:coupling_factor`, `drl_boltzmann:excess_extraction_factor`), and `ROW26_SITES`
lists exactly those 3. Handoff 6's prose attributes 6 sites to the artifact, adding
`covering_analysis:486`, `gap_diagnostic:120`, `omega1_audit:102`, and a `drl_fpn:197`
LIVE-COSMETIC verdict — **none in the artifact.** So "no second classification-changing
fabricated-default beyond row 23" is perturb-confirmed for the covered sites, **OPEN for 4.**
Sharper: 2 of 3 covered sites are NEUTRAL via *dead else-branch* (`cross_index_coupling` total),
not live-but-empty. The 999.9 tripwire's own positive control fired clean (synthetic no-data
constraint → `excess_extraction_factor` emits 0.5 when reached). *Tier: perturb-confirmed for
covered sites; coverage gap grep-witnessed (both files pasted).*

**Confirmed-and-strengthened (no correction needed):**
- **The +642 set move is now perturb-confirmed at both ends.** Pre-row-23 worktree CTX_SET =
  6542; HEAD CTX_SET = 7184; delta +642 *observed*, not doc-asserted. Pair count HELD at 79
  (the count-identity trap — count holds while the set moves). 268-row timeline correction
  reproduced exactly (185 TR→snare, 58 unk→snare, 10 TR→unk, 9 scaf→mtn, 6 rope→mtn) by diffing
  656 rows at both commits. *Tier: perturb-confirmed (both endpoints + a witnessed
  `cs_kernel_divergence/4` solution).*
- **Live NL = 2** (`explanatory_closure_mechanism`, `state_role_time_collapse`), confirmed via
  Pattern-3 unbound census with a 6-type histogram positive control. The 3 declined all carry
  ≥1 authored `constraint_beneficiary` (1/3/3); survivors carry 0 (the negative control).
  Falsification passed. *Tier: perturb-confirmed.*
- **Branch `53be26f2` is moot** — not on main, superseded by `5e3d9dc6` (the reconciled
  emission map; line-numbers dropped is the intended durability fix, census-consistent). No
  merge action; effective squash-with-reconciliation is the correct disposition.

**Item 1b — the load-bearing finding for the build.** The signature-locked population is HELD
(override histogram byte-identical pre/post NL-gate: FNL 147, false_ci_rope 37,
coupling_invariant_rope 4). **But it is a path-asserted proxy that over-includes.**
`false_natural_law` fires on `claimed_natural` + Boltzmann-noncompliance — it is **not** gated
on an absorbed metric flip. So "final sig ∈ {`false_natural_law`, `coupling_invariant_rope`}"
is *not* the Surface-2 lock condition (which is *reached* ∧ *flip-absorbed*). The signature
read identifies the override's presence, not the lock. **The build must derive its own target;
it cannot inherit this list.** *Tier: path-asserted (clause read).*

---

## Witness-integrity note (record it — the spine fired on the tooling)

Mid-write-pass, the harness replayed earlier tool results out of order and handed back **phantom
commit SHAs** (`0c40c5be`, `5d227bd0`, `7de7cc7b`) with fabricated "committed" output for two
edits that had not landed (`git show` → "unknown revision"). It was caught by re-deriving state
from a clean `git log` + per-file greps against committed blobs — not by trusting the running
notes. This is the spine on the audit's own substrate: a success-shaped token (a plausible SHA
with a success message) filling the slot of a commit that did not exist; `git show → unknown
revision` was the positive control proving the absence. **Lesson for the next write pass: when
the witness is git state, serialize the operations (parallel Bash batches triggered the
scramble) and verify "landed" against committed blobs, never against tool-reported success.**

---

## Corpus drift (verify, it may move the build target)

Corpus loaded this session = **226 testsets / 194 corpus-constraints / 38 kernels** (Handoff 6
said 223). Both pre-fix worktrees also showed 226 (no drift across the fix commits). The
unwitnessed-kernel set may have grown with the corpus — another reason the build derives its
target rather than inheriting Handoff 6's "19 of 20."

---

## The next move (fronts, by readiness)

1. **Surface-2 per-param primitive — the proven critical path, ungated, a construction task,
   a fresh session.** Observable = `excess_extraction(C, ExcessEps)`; overlay =
   `boltzmann_floor_*` retract/assertz; follow `proof_of_life_surface2.py`; do NOT extend
   `perturb.py` (Surface-1 only). **Derive the target list itself** — `perturb.py` (or the
   Surface-2 overlay) filtered to the unwitnessed kernels, recording *(reached: metric-flipped)*
   ∧ *(locked: type-held)* **with the coverage field**. Do not inherit 1b's signature-read list
   (proven by clause-read to over-include). Then **test the HYPOTHESIS** that Boltzmann
   perturbation flips the locked kernels' final types — the test that converts
   critical-path-in-principle to witnessed, and the project's headline. "19 of 20" stays
   path-asserted / OPEN until that perturb derivation runs; it gates nothing but the build.
2. **`get_raw_suppression` static `Supp=0` fabrication (`drl_core.pl:96`)** — the static-side
   sibling of the row-23 fix, still unfixed. Same G6 treatment (fallback or fail-closed). Known
   live fabricated-default; lower urgency than Surface 2.
3. **Row-26 `ROW26_SITES` 6-site expansion (the first hygiene write to close OQ-41).** Add
   `covering_analysis:486`, `gap_diagnostic:120`, `omega1_audit:102`, `drl_fpn:197`, re-run the
   999.9 tripwire so all six get a witness instead of 2–3. Converts the half-supported safety
   claim to fully witnessed (or surfaces the second classification-changing default if there is
   one). Small; can ride ahead of the build.
4. **Lower-urgency carry-overs:** Surface-3 primitive (gated on OQ-46 authoring temporal
   series — a primitive on stopgap-scalar-fallback baselines measures the fallback, not the
   drift); full-rubric prompt+schema redesign (estimator-classifier, all six types) + the
   regen (OQ-46/47, gated on deleting `ab_test/stripped_schema.json`); `rope_chi_ceiling` wider
   sweep; D20/D21 Boltzmann tripwire graduation (alongside the Surface-2 build, same subsystem).

---

## Verify-or-correct for the next session (paste real output; a code-read does not close these)

1. **Branch state.** Confirm `audit-doc-corrections` (3 commits off `3f6c0c52`:
   `a0163405`/`b0436cdf`/`bfc0f5a2`) is where you expect — pushed/merged or still local — and
   that the 3 blobs still carry their signatures. The scramble made "landed" false once this
   session; verify against committed blobs, not any summary.
2. **The Surface-2 target — derive, do not inherit.** Run `perturb.py` (or the overlay)
   filtered to the unwitnessed kernels with the coverage field, and produce the
   *(reached ∧ flip-absorbed)* list. Whatever number it yields is the target; the "19 of 20"
   and the 1b signature-read list are both superseded the moment this runs.
3. **`build_discipline.md` reconcile.** A parallel-session paragraph ("name the level" — the
   diagnostic-vs-classification layering) was uncommitted in the tree at handoff. Confirm it
   does not fork from `KNOWN_STATE.md` B1 (Correction 1) on the same finding, then commit it.
   If it already landed, confirm the two docs agree.
4. **Re-confirm the lock is real, not just the override.** Before building the target on
   *(reached ∧ flip-absorbed)*, perturb one `false_natural_law` constraint and show
   type-holds-without-absorption — i.e. that the signature can fire flip-independently (the 1b
   clause-read claim), so you know the proxy over-includes by how much.

---

## Substrate pointers

`KNOWN_STATE.md` (**witness of record**: B1 NL-gate correction `a0163405`; demotion-blindness
`b0436cdf`) · `ISSUES.md` OQ-41 row-26 coverage correction (`bfc0f5a2`) + OQ-46/47 (regen) ·
`docs/technical/build_discipline.md` (the spine + Patterns 1–5 + "name the level" qualifier,
reconcile/commit pending) · `prolog/signature_detection.pl` (NL-gate fail-close `3116ac08`;
`false_natural_law` flip-independent at the cascade; boltzmann guard `:835–836` = Surface-2
critical path) · `prolog/drl_composition.pl` (row-23 fix `39630182`) · `prolog/drl_core.pl:96`
(static `Supp=0` — unfixed sibling, front 2) · `python/sweeps/proof_of_life_surface2.py` (the
Surface-2 template — the next build) · `python/sweeps/tripwire_fabricated_defaults.py`
(`ROW26_SITES` = 3, expand to 6 = front 3) · `python/sweeps/demotion_pass.py` (engine-blind —
do NOT route engine-fix verification through it) · `docs/engine_handoff_6.md` (untracked by
design; superseded on its headline by `KNOWN_STATE.md` B1 — read it for the full prior thesis,
not for the NL-gate verdict) · Handoffs 1–5 (optional substrate).

---

## Stopping note (carry, per every prior handoff)

This session was an audit, not a build, and it stopped before the Surface-2 construction it
re-confirmed as next — a deliberate cut between a cold-re-cut arc and the build that should get
a fresh session. The cut is chosen, not detected. The method earned its keep this time the way
it is supposed to: the cold re-cut overturned the prior handoff's headline (a perturb-confirmed
tag one layer above its witness), the substrate re-derivation caught the harness fabricating
its own commit SHAs, the +642 was witnessed at both ends instead of trusted at one, and the
signature-lock proxy was refused as the build target before it could contaminate the build. None
of that certifies the corrected catalogue is sound — it closes the doors this pass found open
and says nothing about doors behind the ones it did not open. The cold read you are about to do
is the only thing that can tell whether this cut landed well. Distrust this doc; the committed
`KNOWN_STATE.md` is the witness, and the four verify-or-correct items are where to start.
