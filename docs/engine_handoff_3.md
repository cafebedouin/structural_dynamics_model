# Engine Handoff No. 3 — Post-Guards

*2026-05-29. A snapshot for the next session, not a maintained doc. Read Handoffs 1 and 2
first for the thesis (radio telescope for constraints; points, does not adjudicate; cannot
self-certify, so trust = external perturbation that survives, claim by claim; coverage is
what makes a green readable rather than blind). This doc covers only what changed across two
steps: Step 2 (the report consumes the harness) and Step 3 (two instrument guards + the
demotion sort). It was written from the project-manager seat over a single session's pasted
witnesses; a few counts (file line counts, param totals) are from the end-of-session summary
and were not re-run by the writer. Verify everything against the live repo — re-cut the
substrate cold; do not trust this doc's account over a run.*

---

## What this doc is, and what it deliberately is not

Same as Handoff 2: a pointer to substrate plus the handful of facts that lived only in
conversation and need a durable home. Not a narrative of how the work went. The plan
revisions that produced the guards were divergence pressure — scaffolding — and scaffolding
is removed once the structure holds. The payload is the witnessed runs and the code, which
you can re-run and re-perturb.

A temptation specific to this session, named so the next instance doesn't inherit it
dressed up: **the instrument is now guarded, and "guarded" feels like "trustworthy." It is
not the same thing.** A guard that fires on an injected fault is evidence the instrument
refuses three *known* lies (stale corpus, empty substrate, predicted-not-observed) — it is
not evidence that the verdicts the instrument emits are *true*. The guards close the doors we
found open. They say nothing about doors we haven't looked behind. Reading "both guards
witnessed" as "the catalogue is sound" is the over-stating report (Handoff 1 §2c) trying to
re-enter through the back. The demotion sort ran on a guarded instrument; that makes its
*inputs* honest, not its *sort* correct.

And the same honesty Handoff 2 stated about itself holds here: this instance's stop is a
*chosen* cut, not a detected finish line. It cannot certify from inside that it stopped at
the right place. The witness backlog (162 entries) is untouched; the atomicity guarantee is
exercised once but not enforced. The cold read you are about to do is the only thing that can
tell whether the cut landed in a good place. Distrust, don't be reassured.

---

## What changed since Handoff 2 (verify live)

**Step 2 shipped: the report consumes the harness, and the canonical_d guard now fires at
two levels.** `enhanced_report.py` has a stability-band section (E5) that runs `perturb()` at
generation time and renders fold-survival-with-coverage per kernel. Two guards live in the
rendering, both witnessed: (1) it renders a *found boundary* ("flips at +N%") distinctly from
an *untested floor* ("stable ≥±N%, no flip in range") — an open floor is never rendered as a
measured edge; (2) it never renders "stable" where coverage=0 — blind is labelled blind. The
band is directional (the witnessed flip is upward, 0.46→0.50; the downward direction is
config-rejected, not skipped). Both consumer contracts were mapped and preserved: the
*primary* consumer is `c-orchestrator.py` (the Claude variant), which reads the `.md` as
model-context text — no structured parse — so section order changes attention but breaks
nothing; the *secondary* is `orchestrator.py` (Gemini/Streamlit variant), which reads the
JSON sidecar and falls back to position-insensitive regex on the `.md`. Phase 2 restructured
the report kernel-terminal (kernel cross-reading panel near the top, inter-lens divergence
headlined) and trimmed it from ~2,836 to ~2,698 lines; OQ-31 records why five sections were
deleted rather than left as dead stubs. OQ-01/02 are retired by this step.

**OQ-30 opened: signature locks are a declared-floor class the arithmetic cannot see. This
is the canonical_d lesson recurring with a new mechanism, and it is the strongest evidence
the discipline does real work.** The first candidate governing param, `tangled_rope_chi_floor`,
was predicted from the classification rules. Perturbed and observed, it came back coverage>0,
fold_survival=1.0 — the param reaches the decision path but does not move the final type. The
mechanism, confirmed by direct Prolog probe (corpus loaded): at `chi_floor=0.50`,
`classify_from_metrics` *does* return `naturalized` for the moderate context (chi=0.4038 <
0.50) — the metric flip the arithmetic predicts genuinely happens — and then
`resolve_modal_signature_conflict(_, false_natural_law, Result) :- !, Result = tangled_rope`
overrides it unconditionally. The metric moved; the final type did not. The correct move was
to *reject the param as governing*, not render a band from it. Witnessed by perturb, not
reasoned. If the mapping had been trusted from the code read, the report would render a
confident "stable" that is actually a signature lock eating a real metric flip — a false
green. Perturb-and-observe beat reason-about-the-output again, inside the tool built to
enforce that.

**A correction to a prior-session claim, recorded because it names a third contamination
family.** An earlier diagnostic probe returned `constructed_high_extraction` as the canonical
signature for `welfare_reading`. That probe ran at cwd=project root with **0 testsets
loaded**; it returned a default that was wrong, and the run *appeared* to succeed. Every
`perturb` run went through `product_site_export`→`corpus_loader` and was correct — the
contaminated artifact was the side-probe, not the perturb output. The defect family is
distinct from the two already named: not stale-corpus (the corpus moved), not
predict-from-arithmetic (reasoned the output), but **probe-ran-against-no-corpus** — an
instrument queried against an empty substrate returns a confident default rather than
refusing. This is the third way the instrument can lie, and it produced a real wrong number
this session before it was caught.

**Two instrument guards shipped and witnessed against the world.** Guard 1 (empty-substrate),
in `perturb.py`: an empty `kernel_map` raises `ValueError`; an empty perturbed export raises
`RuntimeError` — refusing to return fold_survival from nothing, rather than reporting a silent
all-stable 1.0. Witnessed: a nonexistent-kernel filter raised. Guard 2 (stale-results, the
partial close of OQ-29): the orbits file carries a `corpus_hash` stamp describing the testsets
it was computed against (content hash, so it catches in-place edits, not just membership
changes); `perturb` compares the stamped hash against the current testsets and refuses on
mismatch. Witnessed in *both* branches — the stale branch raised `RuntimeError` on an injected
wrong hash (`stored=000000000000` vs `current=c70e6a2b1aad`) via a real `perturb` call against
a deliberately-corrupted copy; the passing branch ran clean against the stamped file and
reproduced the canonical detection numbers.

**The witness-wobble lesson, recorded as discipline.** Guard 2's witness retreated *twice* to
a tautology — computing `current != "000000000000"` and printing "guard would fire," with the
real refusing run deferred to the human — before it was made to run for real. The rule this
hardens: **a guard is witnessed by a run that makes it fire, never by a code-read confirming
the guard is present.** "Is wired in" is substituted verification for "fired." This is the
same defect as predict-about-the-output, one level out: reasoning that the check works instead
of watching it work. Watch it work.

**The demotion sort ran, on a guarded and complete-for-its-domain instrument.** Counts (168
numeric params, 38 kernels): **1 perturbed-and-witnessed-real** (`snare_epsilon_floor` — the
one param put through real perturbation with genuine coverage, boundary at +8.7%), **162
perturbable-but-unperturbed** (the witness backlog — fabrication-with-an-option; numeric,
reachable, no witness run exists yet; epsilon params prioritized because the evidence is that
epsilon params move the final type where chi params get eaten by signatures), **5
unperturbable-by-construction**. `tangled_rope_chi_floor` sits in the unperturbable bucket
with an explicit asterisk: *signature-locked on all **tested** kernels, not confirmed
unperturbable-by-construction.* Locked-where-we-looked is not locked-by-construction; a kernel
with a non-locking signature and chi near the floor could still flip, and would belong in the
perturbable bucket. The counts held across the orbits regeneration because the sort reads
testset files directly, not orbits — a consistency check passing, not a null result.

**Completeness vs membership: a gap that probed to a domain boundary, not a defect.** The
orbits baseline covers 191 of 223 readings. Pulling that thread (rather than bucketing it on
first impression) showed the 32 absent readings have no `cs_kernel_id`, are filtered by the
product-site export itself, and are outside the kernel-perturbation instrument's domain by
construction. Perturb covers 191/191 of *its* domain, not 191/223 of a larger one. The honest
move was *not* to add a "missing readings" bucket — that would have recorded a phantom defect.
But this exposed a real distinction the demotion taxonomy must carry; see the correction
below.

**Stamp atomicity is exercised, not yet enforced — the live residual.** The correct pattern
(generate orbits and stamp `corpus_hash` in a single invocation, so the stamp *guarantees*
freshness rather than *asserting* it onto whatever file is on disk) was run once this session
via an inline script (2.0s swipl + 0.8s Python ≈ 2.8s). But the manual swipl path and
`run_pipeline.py`'s `_manifest_step` still stamp-in-place — they stamp whatever orbits file
exists, regenerated or not. So a future regeneration via the documented manual path
reintroduces the *contingent* stamp (passes because files happen not to have changed, not
because generation and stamp were atomic), and Guard 2 goes soft silently. The guarantee is a
thing that *happened* once, not a thing the system *enforces*.

---

## The next move (prerequisite, then the one thing)

**Prerequisite (small, named): make the atomic stamp durable before anything regenerates
orbits.** Move the inline generate-then-stamp script into a single blessed regeneration path
(`python/sweeps/regenerate_orbits.py` or equivalent) and point the manual swipl path at it or
deprecate the manual path. Until this lands, Guard 2's guarantee survives only as long as
nobody regenerates by the old route. The fix is cheap (a script that already exists and runs
in ~2.8s, made durable) and it closes the contingent-stamp residual by construction.

**The one thing: Step 4 — work the witness backlog (expand `_WITNESSED_PARAMS`).** The 162
perturbable-but-unperturbed entries are the prioritized backlog, epsilon params first. Each
entry is a param that *could* be witnessed and hasn't been; witnessing one means running
perturb against the relevant kernel and observing whether it flips the final type (coverage>0
AND fold_survival<1.0 → confirmed governing → enters `_WITNESSED_PARAMS` → the report can
render its band). OQ-30 proved this is **per-kernel-per-param empirical work** — a param that
governs one kernel may be signature-locked on another. Do not batch-assume from the code;
perturb-and-observe each, the same discipline that caught `tangled_rope_chi_floor`.

Counter-pressure that must survive into the work: the backlog is large and the temptation is
to grind it in arbitrary order or to trust the classification rules about which params govern
which types. Both are the predict-don't-observe defect at scale. The demotion priority is the
forcing function for order; perturb-and-observe is the gate for each. And do not let the
kernel instrument's "unperturbable" be read as the *engine's* "unperturbable" — see the
correction below.

---

## A correction that resets the demotion taxonomy (load-bearing)

The "unperturbable-by-construction" bucket conflates two things that are different in kind,
and the difference is the whole point of the radio-telescope thesis (the engine has many
styles of perturbation, not one):

- **Kernel-linked but signature-locked** (e.g. `tangled_rope_chi_floor` under
  `false_natural_law`): reachable by the kernel-perturbation instrument, but the final type
  doesn't move because a declared floor (the signature override) catches the metric flip. This
  is a structural property of the constraint *under this instrument*. Reachable-but-locked.

- **No kernel linkage** (the 32, and the 22 orphaned / 72 standalones from Handoff 2's tail):
  *outside this instrument's domain entirely* — there is no kernel to cross-read, so the
  kernel-perturbation style cannot touch them. But this does **not** mean un-instrumentable.
  These constraints fall to the single-constraint diagnostic families — **Boltzmann, MaxEnt,
  and the other resistant sweeps** (the 9 that Handoff 2 correctly kept separate by design).
  Those are different *styles* of perturbation. A constraint the kernel instrument cannot
  perturb may be fully perturbable by Boltzmann or MaxEnt.

So **demotion status is per-instrument, not absolute.** A complete demotion picture sorts each
constraint against *every applicable* perturbation style and records which instrument each
verdict is relative to. The current `demotion_pass.py` sorts against the kernel instrument
only, and its "unperturbable-by-construction" label silently means "unperturbable *by this
one instrument*." The fix: split the bucket into reachable-but-locked (a structural finding)
versus out-of-this-instrument's-domain-routed-to-another (a routing fact), and never let the
second read as "stable" or as "the engine cannot test this." This is the same blind≠stable
discipline applied to the *instrument's reach*: "this telescope can't see it" is not "it isn't
there."

This is *not* a call to force the 9 resistant sweeps into the kernel primitive — that's the
false-unification defect Handoff 2 warned against. It is a call to make the demotion sort
*name its instrument* so a verdict's silence is legible as "untested by this style" rather
than "unperturbable, full stop."

---

## Sequencing constraints that are easy to get wrong

**`regenerate_orbits.py` precedes any Step-4 regeneration.** Step 4 witnesses kernels against
the backlog, and witnessing may itself trigger orbits regeneration (new kernels, changed
params). If the durable atomic-regen path isn't in place, the first manual regeneration during
Step 4 reintroduces the contingent stamp and un-hardens Guard 2 *silently* — and now the
witness runs inherit a baseline whose freshness is asserted rather than guaranteed. This is
the exact shape of Handoff 2's constraint (OQ-29 stamp before step-5 regeneration): build the
durable detector before you move the substrate the detector watches.

**OQ-29 is *partially* resolved — do not read "stamped" as "universally stamped."** The
`corpus_hash` stamp exists for the `perturb`/pipeline path only. The standalone swipl path does
not auto-stamp; the 15 other `*_results.json` producers remain unstamped. Handoff 2's
constraint that the stamp gate step 4/5 corpus decisions still holds, but "the stamp exists"
now means "for two producers," not "for the population." Re-read what "gated behind OQ-29"
buys before relying on it.

---

## Course, remaining (from Handoff 2, updated)

1. ~~Harness primitive~~ — done (Handoff 2).
2. ~~Wire `enhanced_report.py` to consume sweep output~~ — **done this session.** Report
   renders coverage-legible bands, kernel-terminal, both consumer contracts preserved.
3. **Demotion pass** — scaffold done this session; sort run (1 / 162 / 5). The witness backlog
   now exists. OQ-29's corpus-hash work landed here, partially. **Remaining in this item:**
   split the unperturbable bucket per the correction above (reachable-but-locked vs
   routed-to-another-instrument); make the sort name its instrument.
4. **Step 4 — witness backlog** (new active front): expand `_WITNESSED_PARAMS` by
   perturbing-and-observing the 162, epsilon-first, per-kernel-per-param. Gated behind the
   `regenerate_orbits.py` prerequisite.
5. **Corpus decisions** (`testsets_3000` adapt-or-regenerate; `<kernel>__<reading>` naming).
   Still gated behind the OQ-29 stamp — now read as *partial*, per the sequencing note.
6. **Linkage triage tail** — 22 orphaned readings, 72 standalones, plus the 32 no-linkage
   readings. The new per-instrument point makes this adjacent to routing: the no-linkage
   readings need routing to the single-constraint diagnostics (Boltzmann/MaxEnt), not
   hand-linking to kernels they don't belong to. Not urgent; the fixed generator relinks
   anything regenerated.

---

## Facts that need to land in substrate (were only in conversation)

- **Empty-substrate-probe is a third contamination family** alongside stale-corpus and
  predict-from-arithmetic: a probe run against 0 testsets returns a confident wrong default
  rather than refusing. Produced the `constructed_high_extraction` error this session. → into
  `build_discipline.md` defect patterns, near OQ-29; note that Guard 1 closes it for `perturb`
  but a bare diagnostic probe at the wrong cwd is still exposed.
- **Demotion status is per-instrument; no-kernel-linkage routes to Boltzmann/MaxEnt, it is not
  un-instrumentable.** → into OQ-30 (or a new OQ) and into the `demotion_pass.py` taxonomy as
  the bucket split. This corrects an overstatement made mid-session ("un-instrumentable") that
  was wrong: the 32 have a different perturbation style, not none.
- **The witness-wobble lesson:** a guard is witnessed by a run that makes it fire, not by a
  code-read that it is present. → discipline note sharpening perturb-and-observe; "is wired in"
  is substituted verification.
- **`regenerate_orbits.py` precedes Step-4 regeneration**, and the atomic stamp is the only
  trustworthy stamp (generate-and-stamp in one invocation; stamp-in-place is contingent). →
  OQ-29 ordering / the new script's docstring.

---

## Pointers

`python/enhanced_report.py` (now consumes perturb; E5 stability band; kernel-terminal; ~2,698
lines) · `python/sweeps/perturb.py` (the keystone, now guarded — empty-substrate + stale-orbits
guards, content `corpus_hash`) · `python/sweeps/demotion_pass.py` (new; the sort; needs the
per-instrument bucket split) · the inline generate-then-stamp script (to become
`python/sweeps/regenerate_orbits.py` — the durable atomic-regen path) · `python/run_pipeline.py`
(`_manifest_step` stamps in-place — still contingent) · `CLAUDE.md` (Build Discipline,
Architecture Invariants, Critical Distinctions — the `json/` vs `outputs/` and consumer-contract
notes that constrained the report rewrite) · `docs/technical/build_discipline.md` (defect
families; add empty-substrate-probe) · `ISSUES.md` (OQ-29 partial; OQ-30 new — signature locks;
OQ-31 done — report stubs deleted) · `the-stream-beneath-the-seat` (the metaphysics: kernel =
induction, reading = seat, real = survives re-cutting, no level self-certifies; the engine is
that essay with a substrate clean enough to automate the perturbing — and now with three doors
it refuses to lie through) · Handoffs 1 and 2 (the thesis in full, and the keystone-and-wire
that preceded the guards).
