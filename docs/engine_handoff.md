# Engine Handoff — Purpose, State, and Course

*Written 2026-05-29 as a handoff. Read this to understand what the engine is FOR before
touching what it does. The thesis here is the load-bearing part; the state and course
follow from it. Where numbers are cited, verify against the live repo — a clone used to
draft this lagged the most recent migration.*

---

## 1. The thesis — what the engine is for

The engine is a **radio telescope for constraints**. It applies one lens — the constraint
lens (extraction ε, directionality, the χ = ε·f(d)·σ(S) classification, H¹ perspectival
fracture, the CS commitment layer) — to a *signal* and makes visible a band that is
invisible without it: the folds inside an argument, a policy, a news article, a paper. The
cover story a constraint tells about itself, the seat a claim is issued from, the
beneficiary a framing routes out of view.

What it is **not**, and the distinction is the whole point:

- It does **not** produce seat-free verdicts about what constraints "really are." It cannot,
  for a structural reason (below), and the attempt is the failure mode the whole apparatus
  exists to resist.
- It **points; it does not adjudicate.** It runs many diagnostics that do not override the
  base classification — each says "via this method you get a different result." The
  *divergence between lenses* is the signal; agreement across them is the robust band. The
  report's job is to surface that disagreement, not resolve it.

**Why it cannot self-certify (the structural limit).** Every check the engine runs to test
whether one of its own verdicts is grounded is itself a reading — a cut, a seat. There is no
perturbation that steps outside all readings to certify a verdict as seat-free, because the
certifying would be one more seated act. The engine's grounding is therefore undecidable
*from inside the engine*, the same way a system cannot decide a fact about its own operation
by means of that operation. This is not a defect to fix. It is the reason the engine's only
honest form of trust is **external perturbation that survives** — claim by claim, never the
whole at once. A verdict that has been perturbed and held is an instrument reading at that
point. A verdict that has not been perturbed is fabrication with the engine's typography on
it — indistinguishable in confidence from a real reading until you test it. (The failure is
real and has bitten in this project: a fluent, mathematized causal story about
`canonical_d_analytical` was confidently wrong; the parameter is inert on the live
classification path. It was caught by perturbation — re-export at swept values, byte-identical
output — not by reading the code, which *looked* sound. Reasoning about what drives an output
is unreliable here; perturb-and-observe is the only ground truth.)

**The operating distinction that falls out:** the engine works on what can be perturbed.
Substrate-bearing objects (parameters, the corpus) perturb mechanically. Arguments perturb
by re-cutting their fold from a seat the author didn't occupy — which is what the
"who benefits / how does it look from the worst-positioned seat" battery already does by
hand. So the engine and the analytic essays are one operation at two grains: the essays are
the lens applied by hand to others' folds; the engine is the same lens automated. The
input domain is therefore **any argument, anyone's** — and in practice the dominant use is
exactly that (`agent/p-orchestrator.py` pointed at a paper or article → constraint reports →
a model conversation), not self-authored test material.

**The governing constraint when pointed outward:** stay interrogative, but do not mistake
that for never concluding. The engine surfaces *candidate* folds and perturbs them; it does
not certify what someone's hidden seat *is* (that's a fold it can't get behind). But the
*human* using it stakes conclusions — declares, publishes, signs. Declining to ever conclude
is its own cover story (the sophisticated-critic pose that stays safe by never committing).
The division of labor: **engine points (interrogative), human stakes (verdictive, owned).**
The layered defense — engine diagnostics, then a second model's *flinch* (where it diverges
from the summary, used as signal the same way inter-lens divergence is), then publication,
expert review, the UKE skills — is a cascade where each layer's *disagreement with the prior
layer* is the payload. No single layer adjudicates; the aggregate points.

---

## 2. Current state — measured against that thesis

Three findings. The capability is largely present; the wiring and the honesty-about-wiring
are the gaps.

**(a) The fold-tightness meters exist but were mostly unconsumed.** ~12 sensitivity sweeps
in `python/sweeps/` (ε, directionality, structural config, H¹ fragility) perturb parameters
and measure fold-survival — this is exactly the perturb-and-watch the thesis requires. But
they were bespoke (three different perturbation methods across three sweeps, no shared
harness) and their `*_sensitivity_results.json` outputs had no consumer. The engine measured
fold-tightness and dropped the measurement. The meter had no dial.

**(b) The cross-reading layer is now wired (this session).** The kernel→readings linkage
was the prerequisite for the engine's most important verdict — reading-robust vs
reading-specific, the operational form of "only the real abides." A kernel is an induction;
its readings are the cuts; the band that survives across all readings is the part that
abides; COLLAPSE means the authoring induction was loose (readings named differently but cut
the same). The cross-reading diff measures this. As of this session: the join step stamps
`cs_kernel_id`/`cs_story_uid` into generated `.pl` files from the SCOPE manifest (idempotent —
verified 103 skips / 0 changes on re-run); `cross_reading_diff` returns linked kernels'
readings with no "tagged but absent" warnings; the ~83 historically-unlinked stories were
triaged into 32 contradiction-files (auto-stamped `cs_contradiction_of`), 22 orphaned
readings (hand-confirm pending), 72 standalones (eyeball pending).

**(c) The report (`enhanced_report.py`, ~2,836 lines) ends on the wrong unit and over-states.**
It terminates per-constraint with a stack of diagnostic *values* (purity, coupling,
Boltzmann, live-type). Under the thesis it should: (i) end on the **kernel-level cross-reading
panel** (here are the cuts, here's the robust band, here's where they diverge, here's the
COLLAPSE-or-differentiated verdict), because the fold lives at the kernel; (ii) headline
**inter-lens divergence** rather than a value-stack, because divergence is the signal;
(iii) annotate each value with its **stability band** from the sweep JSONs (survives ε∈[…],
flips under sigmoid_upper<1.2), because a value without its perturbation result is the
`canonical_d` trap waiting. AND — counter-pressure — it is probably *over-built* at 2,836
lines. Its role is to produce *one clean cut that the next layer flinches against*, not to be
the comprehensive verdict. An over-stating report suppresses the flinch (the next model defers
to its authority instead of diverging). The redesign likely makes it **shorter and
kernel-terminal**, not richer.

---

## 3. The recurring defect to design against

Documented in full at `docs/technical/build_discipline.md`; summarized because it will
recur in every move below. Two patterns, one root (fast solo build: producing is the
interesting part, reconciling is deferred and invisible because the producer looks done):

- **Produced-but-not-consumed** (orphaned sweep JSONs; manifest grouping not stamped into
  `.pl`). Rule: a producer isn't done until something consumes its output; wire the consumer
  in the same change or add a check that fails when output is unconsumed.
- **Silent fork** (the duplicated `generate_kernel_corpus.py`, resolved this session). Rule:
  one canonical location, recorded as a checked fact, not a memory.

And the meta-rule both serve: **build for the corpus you want, not the one on disk.** A
naming scheme that *cannot* collide by construction beats one that *happens not to* today.
The present corpus is one generation; design for thousands, regeneration under schema change,
found-article ingestion.

---

## 4. The course — in dependency order

Each step is a fold the next stands on. The order matters: enforcement before cleanup, because
hand-cleanup rots the moment the next generation runs.

1. **Finish the linkage triage (small, now).** Walk the 22 bucket-B orphaned readings —
   per story, does its kernel exist (link it) or is it a true single-read (leave, or assign a
   one-cut kernel if SCOPE marked it contested). Eyeball the 72 bucket-C standalones for
   miscategorization. This is judgment, not mechanism; do it while context is fresh.

2. **Unify the perturbation harness.** Collapse the ~12 bespoke sweeps' three perturbation
   dialects into one primitive: `perturb(param, values) → re-export → fold-survival per
   kernel`. The `dval_sweep` / `cross_reading_diff` pattern is the seed. This is the keystone:
   it makes every fold-measurement mean the same thing and makes adding a parameter a call,
   not a new script.

3. **Wire consumption — reconceive `enhanced_report.py`.** Close the orphaned-JSON wire: the
   report reads sweep output and shows the stability band beside each value, ends on the
   kernel cross-reading panel, headlines inter-lens divergence. Probably *shorter*. This
   retires OQ-01/OQ-02 (is this knob load-bearing?) mechanically, as a query against consumed
   sweep output.

4. **The demotion pass.** With the harness (step 2) running, sort every verdict the engine
   emits and every paper claim into: *perturbed-and-survived* (instrument),
   *perturbable-but-unperturbed* (fabrication-with-an-option), *unperturbable-by-construction*
   (declared floor, labeled as such — like the stream in the essay, or `site_contexts_product`
   scope exclusion). The `canonical_d` and `power_modifier_analytical` fossils get found and
   marked here mechanically. **Order note:** this comes AFTER the harness, not before — done by
   hand first, it rots on the next generation. Build the gate, then the archaeology is a query.

5. **Corpus decisions (parallel track, partly reality-test-gated).**
   - `testsets_3000` (3,380 pre-kernel found-discourse analyses) and `testsets_sotu` (189):
     adapt-or-regenerate. The biggest latent asset in the repo. The adapt/regenerate call
     depends on whether kernel retrofit is per-story cheap — a thing you find out by trying a
     small batch, not by reasoning. DeepSeek-generated fresh corpus is a live option given
     pricing.
   - Naming scheme: move toward `<kernel>__<reading>` so collision is impossible by
     construction, not absent by luck. (Not urgent — no live collision — but it's a
     corpus-you-want decision, so decide before the next large generation, not after.)

---

## 5. Stopping notes (read before another long design pass)

Two cautions the session earned, both about the engine's relationship to its maker:

- **Reasoning has a stopping point.** Both linkage defects this session were found by running
  greps, not by thinking. Claims like "the UUID survives regeneration" or "this naming scheme
  holds at scale" cannot be settled by argument — only by building the thin version and
  watching it break. When a design question has been reasoned to where further turns produce
  elaboration rather than resolution, that is the signal to build and test, not to think
  harder. The first move this session that went diagnosis→verified-in-repo (the linkage join)
  returned clean numbers; that contact with reality is worth more than another design turn.

- **The instrument can't certify its own stopping.** "Far enough" — when the engine is
  trustworthy enough, when a design is done — cannot be computed from inside the work; the
  drive to continue is worst-placed to judge its own halting. Stopping is a chosen cut, not a
  detected finish line. Exempting the stopping-rule from this is the precise error the engine
  exists to name. Applies to the maker as much as the engine: the high-velocity build that
  produced both the strength and the recurring defect is the same engine, and it runs on
  hardware that, unlike the Prolog, is allowed to rest.

---

## 6. Pointers

- Purpose / behavioral invariants: `CLAUDE.md` (Build Discipline section, Architecture
  Invariants, Critical Distinctions).
- Defect patterns + diagnostics: `docs/technical/build_discipline.md`.
- Open questions: `ISSUES.md` (OQ-01/02 = is-this-knob-load-bearing, retired by step 3;
  OQ-25 = chimera seal, resolved).
- Canonical framework: `docs/deferential_realism_paper_v7.md` (two-axis: observer +
  committer).
- Orientation for a cold-entering model: `docs/project_orientation.md`.
- The fold/seat/induction theory underneath all of this (written this session, no apparatus):
  `the-stream-beneath-the-seat`. It is the engine's own metaphysics — a kernel is an induction,
  a reading is a seat, the real is what survives re-cutting, and no level (including this one)
  can certify itself from inside. The engine is that essay with a substrate clean enough to
  automate the perturbing.
