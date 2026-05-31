# Engine Handoff No. 2 — Post-Harness

*2026-05-29. A snapshot for the next session, not a maintained doc. Read Handoff No. 1
first for the thesis (radio telescope for constraints; points, does not adjudicate; cannot
self-certify, so trust = external perturbation that survives, claim by claim). This doc
covers only what changed and what's next. Verify numbers against the live repo — this was
written partly from a clone that lags the most recent work.*

---

## What this doc is, and what it deliberately is not

It is a pointer to substrate plus the two or three facts that lived only in conversation and
need a durable home. It is **not** a narrative of how the work went. The reviews that
produced the harness were divergence pressure — scaffolding that helped the structure stand
— and scaffolding is removed once the structure holds, not handed forward. The payload is
the green numbers and the code, which you can re-run and re-perturb. A prose synthesis of
the collaboration would be the over-stating report (§2c of Handoff 1): authoritative enough
to defer to, unfalsifiable, exactly the artifact the engine exists to resist. So this is
short on purpose. **Re-cut the substrate cold; do not trust this doc's account over a run.**

A note on that, stated honestly so the next instance doesn't inherit a dressed-up version:
the prior instance's stop was a *chosen* cut, not a detected finish line, and it could not
certify from inside that it stopped at the right place. Neither can this one. The cold read
you are about to do is the only thing that can tell whether the harness foundation was
stopped too early. Treat that as license to distrust, not as reassurance.

---

## What changed since Handoff 1 (verify live)

**The keystone landed: `python/sweeps/perturb.py` exists and is witnessed.** This is the
unified perturbation primitive — `perturb(param, values) → re-export → fold-survival per
kernel` — that Handoff 1 §4 step 2 called the keystone. It carries a **coverage field**
(`touched`, `coverage`) per kernel, which is the part that matters: it distinguishes
`fold_survival = 1.0 because nothing was perturbed / nothing was reached` (blind) from
`= 1.0 because the type genuinely held across touched contexts` (real). Without that field a
clean number is ambiguous; with it, a green is readable.

Verification that ran, with the numbers (re-confirm by re-running, don't trust the paste):
- **Determinism:** two `run_product_export_to` calls at the same value → byte-identical
  (diff exit 0). Prerequisite for any fold_survival meaning anything; established first.
- **Identity (no-op, `snare_epsilon_floor=0.46`):** `end_of_life_decision_authority`
  fold_survival=1.0, **coverage=0.0**, 0 flips, 0/35 kernels affected. Coverage 0.0 here is
  *correct, not blind* — a param set to its own current value enters no decision differently.
  The proof it's real-stable and not blind is the next line.
- **Detection (`snare_epsilon_floor=0.50`):** same kernel, fold_survival=0.917,
  **coverage=0.167**, 39 flips, 78 touched. `vulnerability_protection_reading` flips 39
  institutional contexts tangled_rope→naturalized; the other two readings don't move. Same
  param, same kernel, coverage moves 0→nonzero exactly when the value actually changes. The
  coverage field doing its job.
- **Cross-reading (`cross_reading_diff.py end_of_life_decision_authority`):** 3 readings,
  swipl clean, **COLLAPSE verdict** (all three identical tangled_rope orbits at baseline).
  Note the real finding: identical at baseline, divergent under perturbation. That's a true
  fold the engine surfaced.

**The detection-witness correction is the canonical_d lesson, live.** The first witness
candidate (`tangled_rope_chi_floor` / `welfare_reading`) was *predicted from chi arithmetic*
and came back coverage>0, fold_survival=1.0 — the ε dual-threshold check intercepted the
flip the arithmetic predicted. Reasoning about the output was wrong; the coverage gate caught
it; the fallback found a witness (`snare_epsilon_floor=0.50`) that actually flips. Perturb-
and-observe beat reason-about-the-output inside the tool built to enforce that. If you find
yourself predicting what a param does, stop and run it.

**Scope honesty (don't let this re-inflate):** the primitive unifies the **type-stability
sweep family — 5 of the ~14 sweeps**, not all of them. The other 9 measure structurally
different things (topology persistence, Fisher info, Nash, test-health, metric correlation)
and stay separate *by design, not deferred*. The keystone is narrower than "every sweep" and
narrower is correct. Do not force the 9 in; that's false unification, the opposite defect.

**OQ-29 opened (new defect class).** 19/19 `*_results.json` carry no record of which corpus
they were computed against. `bifurcation_results.json` is confirmed stale — it reports flips
for 7 constraints that live only in `testsets_3000/` (archive) and are absent from the live
`testsets/`. This is a third defect family the build-discipline doc didn't name:
**produced-against-a-substrate-that-moved** — a result outliving the corpus it describes
(distinct from produced-but-not-consumed and silent-fork). Candidate fix: corpus-hash stamp
on every results file + a staleness check. Lives in the demotion pass (course step 4).

**Wiring fact that cost a run to learn (now recorded in CLAUDE.md):** `[stack]` does NOT
load `product_site_export`; the harness overlay must load it explicitly. Environment fact,
not derivable by reading — recorded as a checked fact.

---

## The next move (one thing)

**Wire `enhanced_report.py` to consume the sweep output (Handoff 1 §4 step 3).** The harness
now produces fold-survival-with-coverage; nothing reads it into the report yet. This is the
produced-but-not-consumed wire, now closable because the producer exists. Concretely:

- The report (~2,836 lines, ends per-constraint on a value-stack) should annotate each
  classification with its **stability band** from `perturb.py` output: *survives ε∈[…], flips
  under snare_epsilon_floor≥0.50*, with the **coverage** shown so a stable reading is
  legible as "stable across N touched contexts," never as bare 1.0.
- It should end on the **kernel cross-reading panel** (the fold lives at the kernel, not the
  constraint), and **headline inter-lens divergence** rather than stack values.
- Counter-pressure that must survive into the work: the report is probably **over-built** at
  2,836 lines. Its job is to produce *one clean cut the next layer flinches against*, not to
  be the comprehensive verdict. The redesign likely makes it **shorter and kernel-terminal**.
  An over-stating report suppresses the flinch. If the rewrite gets longer, something went
  wrong.

Audit → implement → verify, same as the harness. "Verify" = run it, paste real output, no
code-read standing in for a run. The report consuming a *blind* perturb result (coverage=0)
and showing it as a stability band would be the canonical_d trap one layer out — so the
report must surface coverage, not hide it behind a green.

## Sequencing constraint that is easy to get wrong

The OQ-29 **corpus-hash stamp is a prerequisite for step 5 (the `testsets_3000` adapt-or-
regenerate decision), not its cleanup.** 19/19 results files have no corpus provenance.
Regenerating the corpus (step 5) instantly stales an unknown fraction of those 19 with no
detector watching. So: stamp + staleness-check **before** any regeneration that moves the
substrate, or you rebuild the exact defect OQ-29 names at the moment the course was meant to
move past it. This is produced-against-a-substrate-that-moved trying to recur. Build the
detector before you move the substrate.

## Course, remaining (from Handoff 1 §4, updated)

1. ~~Harness primitive~~ — **done, witnessed.**
2. **Wire `enhanced_report.py` to consume sweep output** — next, above.
3. **Demotion pass** — now has the harness to run against. Sort every verdict and paper claim
   into perturbed-and-survived / perturbable-but-unperturbed / unperturbable-by-construction.
   OQ-29's corpus-hash work lives here.
4. **Corpus decisions** (`testsets_3000` adapt-or-regenerate; `<kernel>__<reading>` naming).
   Reality-test-gated: try a small retrofit batch, don't reason about whether it's cheap.
   **Gated behind the OQ-29 stamp** per the sequencing constraint above.
5. **Linkage triage tail** — 22 orphaned readings, 72 standalones. Mostly moot: the fixed
   generator relinks anything regenerated. Hand-link only permanent keepers that won't be
   regenerated. Not urgent.

## Two facts that need to land in substrate (were only in conversation)

- The sequencing constraint above (OQ-29 stamp precedes step-5 regeneration) → into OQ-29's
  "what resolution changes" or §4 ordering, as a checked fact.
- The identity-test reading (coverage=0.0 at a no-op is *correct, not blind*) → a one-line
  comment at perturb.py's coverage field, so a future instance doesn't "fix" a zero that's
  right.

## Pointers

`CLAUDE.md` (Build Discipline, Architecture Invariants, the new `[stack]` note) ·
`docs/technical/build_discipline.md` (defect patterns + diagnostics) · `ISSUES.md` (OQ-29
new; OQ-01/02 retired by step 2 when the report consumes sweep output) · `python/sweeps/
perturb.py` (the keystone) · `the-stream-beneath-the-seat` (the metaphysics: kernel =
induction, reading = seat, real = survives re-cutting, no level self-certifies — the engine
is that essay with a substrate clean enough to automate the perturbing) · Handoff No. 1 (the
thesis in full).
