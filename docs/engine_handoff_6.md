# Engine Handoff No. 6 — Wiring-Gap Arc Closed, Surface-2 Still the Critical Path

*2026-05-31. A snapshot for the next session. **This handoff is self-contained** — the
section below carries everything from Handoffs 1–5 you need to act; the prior docs are the
deeper substrate (full thesis, per-session witness ledgers) if you want them, not a
prerequisite. This doc is current state, not a narrative. **Re-cut the substrate cold — do
not trust this doc over a run.***

---

## Handoffs 1–5 in brief (everything load-bearing, so you don't need the other five docs)

**The thesis — what the engine is FOR.** The engine is a *radio telescope for constraints*:
it applies one lens (extraction ε, directionality, the χ = ε·f(d)·σ(S) classification, H¹
perspectival fracture, the CS commitment layer) to a signal — an argument, a policy, an
article, a paper — and makes visible the folds inside it: the cover story a constraint tells
about itself, the seat a claim is issued from, the beneficiary a framing routes out of view.
It **points; it does not adjudicate.** Many diagnostics run; none overrides the base
classification; the *divergence between lenses is the signal*, agreement across them is the
robust band. The report surfaces disagreement, it does not resolve it. The human using the
engine stakes the conclusions (verdictive, owned); the engine stays interrogative — but
"never conclude" is itself a cover story, so the division is: **engine points, human stakes.**

**Why it cannot self-certify (the structural limit, the load-bearing idea).** Every check the
engine runs to test one of its own verdicts is itself a reading — a cut, a seat. No
perturbation steps outside all readings to certify a verdict as seat-free, because the
certifying would be one more seated act. So the engine's grounding is undecidable *from inside
the engine*. This is not a defect to fix. The only honest form of trust is **external
perturbation that survives — claim by claim, never the whole at once.** A verdict that has
been perturbed and held is an instrument reading at that point. A verdict that has *not* been
perturbed is fabrication with the engine's typography on it — indistinguishable in confidence
from a real reading until you test it. (This bit hard: a fluent, mathematized causal story
about `canonical_d_analytical` was confidently wrong — the param is inert on the live path. It
was caught by perturbation, not by reading the code, which looked sound. **Reasoning about
what drives an output is unreliable here; perturb-and-observe is the only ground truth. If you
catch yourself predicting what a param does, stop and run it.**)

**The discipline that falls out — the witness-tier ledger.** The recurring failure across
every session is *claims sitting one tier above their evidence* — characterized-not-pasted,
asserted-path-not-grepped, errored-counted-as-swept. It drifts most invisibly when the
surrounding work is strong enough to vouch for it. The fix that holds: **paste-or-untag, per
claim, no exceptions.** Every claim carries a tier — grep-witnessed / perturb-confirmed /
path-asserted / instance-reported-not-PM-seen / HYPOTHESIS — and a claim without a tier is not
done. A guard is witnessed by a run that makes it *fire*, never by a code-read confirming it is
present ("is wired in" is substituted verification). Do not re-flatten the ledger.

**Coverage makes a green readable rather than blind.** The perturbation primitive
(`perturb.py`, Surface-1, all 191 params) reports `fold_survival` *with a coverage field*:
`fold_survival=1.0, coverage=0` means "nothing was perturbed / nothing was reached" (blind);
`=1.0, coverage>0` means "the type genuinely held across touched contexts" (real-stable). A
clean number without coverage is ambiguous; never render "stable" where coverage=0 — blind is
labelled blind, and a found boundary ("flips at +N%") is rendered distinctly from an untested
floor ("stable ≥±N%, no flip in range").

**Demotion status is per-instrument, not absolute.** The engine has many *styles* of
perturbation (the kernel-cross-reading instrument is one; Boltzmann, MaxEnt, and ~9 resistant
single-constraint sweeps are others, kept separate *by design* — do not force them into one
primitive, that is the false-unification defect). A constraint the kernel instrument cannot
touch may be fully perturbable by another. So "this telescope can't see it" is not "it isn't
there"; a demotion verdict must name its instrument. The demotion sort buckets every param:
*perturbed-and-survived* (instrument reading) / *perturbable-but-unperturbed* (fabrication-
with-an-option, the witness backlog) / *unperturbable-by-construction* (a declared floor,
labeled).

**The three observable surfaces (named so they don't collapse into one).**
- **Surface 1 — static type** (`product_site_export` → `dr_type/3` → `classify_from_metrics/6`
  then `integrate_signature_with_modal/3`). Mature; `perturb.py` sweeps all 191 engine params +
  6 authored fields. **Hit its ceiling:** 19 of 20 remaining unwitnessed kernels are *reached
  but signature-locked* — the metric flip happens and a signature override
  (`false_natural_law` / `coupling_invariant_rope`) eats it. Surface 1 cannot witness them.
- **Surface 2 — excess-extraction / PoA** (`boltzmann_compliance:excess_extraction/2`).
  Proof-of-life witnessed, **primitive not built.** It is the **critical path** for those 19
  kernels: the boltzmann guard that does the locking (`signature_detection.pl:835–836`) is
  Surface-2-displaceable. *The claim that perturbing the Boltzmann floor actually flips those
  19 final types is HYPOTHESIS, not tested* — it is the Surface-2 primitive's first
  verification target.
- **Surface 3 — temporal / drift** (`drl_composition.pl:classify_at_time/4` →
  `constraint_history/3`). `classify_at_time` calls `classify_from_metrics` directly and
  **skips** `integrate_signature_with_modal` — so Surfaces 1 and 3 diverge by construction on
  any constraint a signature would override. Was blocked by OQ-33 (the fabricated Supp=0.5);
  this session fixed that site (see below).

**The denominator (so you don't re-derive it).** 191 engine params + 6 authored fields = **197
type-moving predicates on Surface 1**, established by *bidirectional dataflow trace* (backward
from `classify_from_metrics/6`, forward from the schema's authored fields, residual zero both
directions). The lesson that matters more than the number: **completeness is edge-closure
(following dataflow), not node-search (grepping names)** — every prior counting method
committed the "narrow substrate labeled as the whole" defect; the trace cannot, because its
substrate *is* the dataflow.

**The maker-facing stopping note (it recurs every handoff).** The instrument cannot certify its
own stopping; "far enough" cannot be computed from inside the work, and the drive to continue is
worst-placed to judge its own halting. Every handoff's stop is a *chosen cut, not a detected
finish line.* Reasoning has a stopping point too — both linkage defects in the early sessions
were found by running greps, not by thinking; when a design question produces elaboration rather
than resolution, build the thin version and watch it break. The high-velocity solo build that
produces both the strength and the recurring defect runs on hardware that, unlike the Prolog, is
allowed to rest.

*If you want the full thesis or a session's pasted witnesses, Handoffs 1–5 are the substrate.
Everything above is what you need to act on the current state below.*

---

## The two threads, reconciled (read this first — it is the orientation)

Handoffs 1–5 run one course: the perturbation harness → the three observable surfaces →
the demotion sort → the witness backlog. That course's open front is **Surface 2 (the
critical path for 19 of 20 unwitnessed kernels)**, and it is unchanged by this session —
still the next construction task, still ungated, still HYPOTHESIS-not-witnessed at its
load-bearing claim.

This session ran a **second thread** that converges with the first: a complete
**wiring-gap census** (prompt↔schema↔engine disagreements) and the adjudication+fix of
what it found. That thread and the surface course meet at one point: **OQ-33's
fabricated-default Supp=0.5 (Handoff 5) is the same defect this session fixed as "row
23."** The fix landed. So one of Handoff 5's three Surface-3 blocks is now partially
discharged, and the fabricated-default front graduated from inventoried to fixed-at-one-
site. Details below.

The reconciliation matters because a cold reader of Handoffs 1–5 would expect to pick up
at Surface 2 and would not know the corpus/engine moved underneath the harness this
session. **It did. Re-run the demotion sort before trusting Handoff 4/5's
6/0/20/0/24/141 block — row-23 changed the temporal classification path, and the
NL-gate fix changed signature behavior on the 404.**

---

## The spine (new this session — the generalization the prior handoffs were circling)

Handoffs 2–5 named defect families one at a time: produced-but-not-consumed, silent-fork,
predict-from-arithmetic, empty-substrate-probe, stale-corpus, fabricated-default,
errored≠inert≠swept, bound-probe-bypasses-cut. This session found the invariant under all
of them, now written in `docs/technical/build_discipline.md`:

**Every defect here is an absence that presents as a presence.** Something is missing — a
consumer, a canonical fact, a clause dispatch, an authored datum, a measurement — and a
*success-shaped token* fills the hole so the read site cannot tell it from the real thing.
The fillers are the only thing that differs:
- consumer absent → a producer that passes its own checks fills it
- canonical fact absent → a file that looks right fills it
- engine dispatch absent from a probe → a clause body that unifies in isolation fills it
- measurement absent → a fabricated constant (0.5 / 0) fills it
- authored datum absent → a vacuous truth (`BeneficiaryCount==0` over an empty table) fills it

**The single fix, everywhere:** carry the provenance bit with the value so absence and
success stop collapsing to one token at the read site. A bare value is a lie of omission
the consumer cannot detect. Concretely: return `unknown` not `0.5`; fail-closed on
absence; wire-or-fail-loud; checked-canonicity not memory; let the engine dispatch.

**Diagnostics are not exempt.** A clean read is byte-identical to a read that did not look:
an empty grep, a `findall` of `[]`, a count of 0, an "I found it nowhere" each can mean
"nothing there" or "didn't dispatch / queried wrong / never ran." **Every diagnostic needs
a positive control** — run it against a case you know it must flag and confirm it flags —
or its green is unfalsifiable. This holds for *reasoning* too: a claim of the form "X
appears nowhere / is unique" is an unfalsified diagnostic until run against a known-positive.
This rule fired twice this session and overturned its own premise both times (see below);
that is the rule working, not failing.

This is the same principle as the witness-tier ledger and perturb-and-observe, stated at
the level that indexes all of them. The ledger says "a claim without a witness is one tier
above its evidence." The spine says "the missing witness is filled by a success-shaped
token, so the gap is invisible." Same disease; the ledger is the discipline, the spine is
the diagnosis.

---

## What changed this session (verify live)

**The wiring-gap census ran to completion.** `audits/2026-05-31_wiring_gap_census/wiring_gap_census.md`: 27 gaps across
prompt↔schema↔engine, classified by type (G1 demand-no-consumer, G2 consume-never-authored,
G3 compute-no-consumer, G4 rule-no-enforcer, G5 scalar-vs-temporal, G6 fabricated-default),
with a forced reconciliation count (unaccounted = 0), a cross-axis-live subset, and a raw-
witness appendix. Routed to `ISSUES.md` OQ-35–44. The census's own discipline: every count
re-grepped (it caught a prior "0 facts" claim that was actually 421/441/520), the 217 G3
"candidates" refused as an orphan list (false-orphan trap, the `mandatrophy_resolved` canary
validated the read-vs-declare discipline first).
*Tier: grep-witnessed (census appendix); reconciliation count is the completeness witness.*

**The 27 gaps collapsed to ~8 decisions, adjudicated, and the rulings executed in two
commits on branch `wiring-gap-fixes`.**

**Commit A (`39630182`, output-changing, `prolog/drl_composition.pl` only) — the row-23 /
OQ-33-D1 fix.** `classify_at_time` no longer fabricates `Supp=0.5` on absent temporal
suppression. New order: temporal `measurement/5` → authored scalar `suppression_requirement`
→ `unknown`.
- The positive control overturned the literal ruling: 650/656 temporal rows lack a temporal
  measurement but **all 650 carry an authored scalar** (genuine-no-data = 0 rows). Returning
  bare `unknown` (the ruling) would have discarded real data — the absence-as-value sin in
  the other direction. Scalar-fallback-then-unknown reads the data that exists.
- **Witnessed impact: 268 timeline rows corrected** (185 tangled_rope→snare, 58
  unknown→snare, 9 scaffold→mountain, 6 rope→mountain, 10 tangled_rope→unknown).
- **Set-not-count finding (load-bearing):** the persisted `cs_kernel_divergence` count held
  at 79, but the **per-context divergence set moved 6542→7184 (+642)**. The fabricated 0.5
  was *homogenizing* real per-context divergence into false agreement — and for a framework
  whose product is the inter-lens diff, homogenizing the divergence is the quietest and worst
  half of the bug. A count-identity check would have shipped "no change"; set-identity caught
  the 642. **Carry: when a fabricated/absent value is fixed, check the divergence SET, not
  the aggregate — fabricated constants homogenize, so their damage lives in the variance the
  count suppresses.**
- Closed a latent **G5** as a side effect: `snapshot_type` already used scalar fallback while
  `classify_at_time` fabricated 0.5, so the two temporal paths silently disagreed; the fix
  converges them.
- The scalar fallback is a **STOPGAP** (labeled in-code, with its death condition: it fires
  only on stories lacking a temporal series; a corpus where it fires 0× is the signal to
  strip it). Retired by OQ-46 (generation authors the temporal series). The `unknown` floor
  fires 0× on the live corpus, witnessed via a **synthetic** no-data case (the floor had 0
  natural test instances, so it needed a constructed positive control or it shipped untested).
*Tier: perturb-confirmed (268-row diff, +642 set move, 0/0 validation pasted); unknown floor
perturb-confirmed via synthetic case.*

**Commit B (`3116ac08`, behavior-preserving) — NL-gate fail-close + strips + schema/prompt
+ docs.**
- **D3 NL-gate fail-close (the finding that matters):** `count_power_beneficiaries` now reads
  the populated authored `constraint_beneficiary` table instead of the empty
  `intent_power_change` join that made `BeneficiaryCount==0` vacuously true corpus-wide. The
  gate now discriminates and **DECLINED 3 raw natural_law_signature certifications (raw match
  5→2)** — three constraints with authored beneficiaries the vacuous gate had been certifying as
  natural. **CORRECTION (VERIFY-OR-CORRECT pass, see KNOWN_STATE 2026-05-31 entry): this is a
  diagnostic-layer decline, NOT classification-changing.** Final `dr_type` of all 3
  (`behavioral_competence_reading`, `disparity_as_depth_signal`, `generational_economic_decline`)
  held at `tangled_rope` at BOTH `39630182` (parent-of-`3116ac08`) and HEAD — `false_summit_mountain`
  sits higher in the cascade (reads `constraint_beneficiary` directly) and captured them before and
  after, so the raw 5→2 never reached final classification. T.1 "cosmetic" therefore holds **fully
  at the final-type level** (the metric-agreeing majority AND the 3-case tail); it is non-cosmetic
  **only** at the raw natural_law_signature certification layer (a diagnostic, not a classification).
  (The earlier "classification-changing for the 3-case tail" was the conflation this pass corrected.)
  *Tier: the prior "perturb-confirmed (NL 5→2)" was a raw-count witness standing in for a final-type
  claim; the final-type claim is now perturb-confirmed by the two-commit `dr_type` query — held.*
- D2: stripped `inevitability` dead clause; **deferred** `internalization_depth` (its reader
  is in a module `stack.pl` never loads — dead-MODULE removal, a different task) and
  `resistance_to_change` (reached by a live report path, `json_report:237` emits a field).
- D5: removed the unenforced scaffold "suppression must decline" prompt line (a G4 rule the
  engine reduced to a static flag — a prompt rule shaping authoring toward nothing).
- D7: stripped the mountain `accessibility_collapse ≥0.85` / `resistance ≤0.15` thresholds
  from the **canonical** schema gate; kept emitting the fields as documentation; the gate now
  requires only `emerges_naturally`/extractiveness/suppression.
- D6: `mandatrophy_resolved` document-and-defer (near-zero touch; hardcoded path matches 0
  live constraints).
- D8: corrected the stale `affects_constraint`-is-empty note (it is populated, 520 live /
  9305 archive; the empty-table finding holds only for the `intent_*` family).

**Commit C (`5ef9294e`, docs) + a parallel-session commit (`53be26f2`,
`generator_emission_map.md`).** C reconciled three docs to post-A/B state and converted the
emission map to durable form (line numbers dropped — they rot on the next edit; field→fact
mapping, renames, emitted-to-nothing list, and gotchas kept; provenance header stamping
`3116ac08` with a one-command re-check). The emission map names itself a Pattern-2 derived
copy. `53be26f2` is the repo-owner's parallel-session doc; it sits between B and C on the
branch — **decide squash-vs-preserve at merge knowing it is in the set, and read its 89 lines
against the census emission findings for content-fork before merging.**

**The positive-control rule fired twice and overturned its own inputs both times** — recorded
because it is the rule working: (1) my "return unknown" ruling was falsified by tracing it
(650/656 carry a scalar); (2) CC's first row-26 positive control (a guard-falsity count) was
itself caught vacuous by *its own* control (the guards succeed for a bogus constraint), and
it switched to a sound 999.9 branch-reachability tripwire. A diagnostic checking whether
another diagnostic discriminates. That is the spine's "diagnostics are not exempt" clause,
live.

**Row-26 fabricated-default sweep (the rest of OQ-41's G6 family): all NEUTRAL.** The 0.5
defaults at `covering_analysis:486`, `gap_diagnostic:120`, `omega1_audit:102` (BaseEps),
`purity_scoring`, `drl_boltzmann_analysis`, `drl_fpn:197` were tripwired (999.9 branch-
reachability + per-site guard-falsity counts). Verdicts: NEUTRAL (guard never false →
default unreached) or LIVE-COSMETIC (`drl_fpn:197` — moves FPN contamination, never
`dr_type`). **No second classification-changing fabricated-default beyond row 23.** The
`domain_priors` 0.5s are sanctioned neutral priors (by-design, marked).
*Tier: perturb-confirmed (`outputs/tripwire_row26_results.json`, per-site verdict +
positive-control count).*

---

## Where this leaves Handoff 5's open fronts

**Surface 2 is STILL the critical path and STILL the next construction task — unchanged.**
This session did not touch it. The 19-of-20 locked-kernel claim, the boltzmann guard at
`signature_detection.pl:835–836`, and the HYPOTHESIS that perturbing the Boltzmann floor
flips those 19 final types all stand exactly as Handoff 5 left them. **Build the Surface-2
per-param primitive; test the hypothesis. That converts "critical path in principle" to
"witnessed."** It is ungated, has non-fabricated baselines, and is a construction task — a
fresh session per the stopping-note, not the tail of this diagnosis-heavy one.

**OQ-33 (Surface-3 block) is PARTIALLY discharged.** Handoff 5 listed three blocks from
OQ-33; this session resolved the engine half of block 1 and recharacterized the rest:
- **Block "443 extant temporal classifications" — recount and re-verify.** Handoff 5's
  443/519 flip count was instance-reported-not-PM-seen. This session's fix corrected 268
  timeline rows via scalar-fallback (not the 443-to-unknown the unconditional tripwire
  predicted — because the tripwire poisoned Supp unconditionally, whereas the fix falls back
  to the real authored scalar). **The 443 number is now superseded by the 268 actual
  correction; re-run any temporal-validity census against the fixed `classify_at_time`, not
  the pre-fix tripwire.**
- **The static-0 / temporal-0.5 asymmetry (the table in Handoff 5) is partly closed:** the
  temporal side no longer fabricates 0.5 (it falls back to the authored scalar, which is what
  the static side reads too). The two surfaces now read the same authored suppression value
  when no temporal series exists, so the snare-disagreement-is-artifact finding no longer
  applies to the fixed path. **`get_raw_suppression`'s static `Supp=0` fabrication
  (`drl_core.pl:96`) is NOT fixed** — it is the same defect on the static side and is a
  candidate next G6 fix (it was out of this session's row-23 scope).
- **The OQ-33 (a/b/c) design decision is now narrower:** option (a) "author temporal
  suppression into testsets" is the OQ-46 regeneration path; the stopgap holds until then.
  The Surface-3 primitive is still premature until the regen authors temporal series — a
  primitive on stopgap-scalar-fallback baselines measures the fallback, not the drift.

**The regeneration arc (OQ-46/47) is the other major open front, and its gate is now light.**
The plan: old corpus → kernel seeds → scoped reading → constraint seeds → regenerate at
100-story batched increments. This session confirmed the three leak surfaces that would
re-contaminate a regen are **clean on the regen path**:
- SCOPE prompt: clean of the stripped fields (grep-witnessed).
- Schema: the regen loads `python/constraint_story_schema.json` (canonical, B4-stripped) via
  `DR_SCHEMA`/`_load_schema`; the `agent/data/` copy is an orphan no generator loads; the
  `ab_test/` over-stripped schema is reachable only via explicit `DR_SCHEMA` override —
  **delete it so it can't be picked up by accident.**
- Example/few-shot: **the leak surface the principle did not catch until the path-map.**
  `generate_kernel_corpus` (the regen path) uses the clean exemplar
  (`agent/verification_bottleneck.json`); `c-orchestrator` uses `json/antifragility.json`
  which hard-codes `accessibility_collapse:0.9, resistance:0.08` and would re-teach the stamp.
  **The regen path is clean (kernel-corpus generator), so the example leak is c-orchestrator
  hygiene, not a regen blocker** — but record the lesson: a few-shot exemplar carrying a gate-
  satisfying value shows the author the decision rule more strongly than a stated threshold.
  The estimator-classifier contamination surface is {instructions, schema, **examples**}.

**The de-stamp witness is the first regen batch, not a separate A/B.** The one claim the whole
prompt/schema/example strip rests on — that stripping the cutpoint actually de-stamps the
authored value — is **still unwitnessed** (all evidence to date is correlational: git-stable
84%-at-0.92 under one prompt regime, the logic_rationale citing the threshold, engine-
insensitivity). The first 100-story regen batch's AC histogram IS that witness. **Gate batch
two on it:** AC fragments off 0.92 → the strip works, proceed; AC still clusters at 0.92 →
the stamp is a training prior, not the prompt, and the fix moves to generator post-processing.
Do not regenerate the full corpus before reading batch one.

---

## The estimator-classifier principle (the schema/generation answer, settled)

The question that opened this arc — *do we need the numerical guidance in the schema/
generation prompts* — is answered: **no.** Boundary logic lives in the engine; the author
estimates substrate and never sees the decision rule. The type-defining cutpoints come out
(decided, validated, engine proven insensitive at T.1 / row-23 count-holds); the structural-
fact requirements stay (`emerges_naturally`, victims, beneficiaries, sunset); the evidence-
forcing gates rephrase to judgment-triggered (no number). The four-class rubric for any
author-visible threshold (`docs/technical/build_discipline.md`, estimator-classifier section):
- **D-both** (definitional in direction AND value — keep: mountain ε≤0.25)
- **D-dir/MI-val** (definitional direction, engine's-business value — keep the direction as
  prose, strip the number: snare "high-extraction" yes, "ε≥0.46" no)
- **MI** (measurement-independent decision rule — strip: any χ threshold, χ is engine-computed)
- **Structural** (a required fact, not a band — keep)

OQ-34 generalizes this to an audit of every remaining author-visible threshold (χ rows are MI
by derivation; the stamp grep must target the authored INPUT field, e.g. ε, not the computed
threshold field, e.g. χ). The full-rubric redesign of prompt+schema across all six types is
**decided but not built** — it is the generation-straightening work that precedes the regen,
and it can be built in parallel with the Surface-2 construction since one touches the
generator and the other the engine.

---

## The next move (fronts, by readiness)

1. **Surface-2 per-param primitive — the proven critical path, ungated, a construction task.**
   Observable = `excess_extraction(C, ExcessEps)`; overlay = `boltzmann_floor_*` param
   retract/assertz; follow `proof_of_life_surface2.py`; do NOT extend `perturb.py` (Surface-1
   only). Then **test the HYPOTHESIS** that Boltzmann perturbation flips the 19 locked kernels'
   final types — the test that converts critical-path-in-principle to witnessed. Fresh session.
2. **Full-rubric prompt+schema redesign (estimator-classifier, all six types)** — decided, not
   built; parallel to (1). Produce to a `redesign/` dir, do not promote; the type-definition
   section likely inverts from metric-bands to substrate-estimation. Then the regen.
3. **Regeneration (OQ-46/47)** — gated on (2) and on deleting the `ab_test/` schema. First
   batch is the de-stamp witness; gate batch two on the AC histogram. Its own planning session.
4. **`get_raw_suppression` static `Supp=0` fabrication (`drl_core.pl:96`)** — the static-side
   sibling of row 23, not fixed this session. Same G6 treatment (fallback or fail-closed); lower
   urgency than Surface 2 but it is a known live fabricated-default.
5. **Lower-urgency carry-overs from Handoff 5:** Surface-3 primitive (gated on OQ-46 authoring
   temporal series); `qwerty/naturalization_reading` wider-range `rope_chi_ceiling` sweep;
   D20/D21 Boltzmann tripwire graduation (do alongside the Surface-2 build — same subsystem).

---

## Verify-or-correct for the next session (paste real output; a code-read does not close these)

1. **Re-run the demotion sort** — Handoff 4/5's 6/0/20/0/24/141 block predates row-23 and the
   NL-gate fix. Confirm it still reproduces or record what moved. The temporal path and
   signature behavior both changed this session.
2. **The 3 declined NL certs (5→2)** — re-run the NL signature census (unbound + post-filter,
   Pattern 7 self-check) and confirm 2, not 5. Identify the 3 declined and confirm each has an
   authored `constraint_beneficiary` fact (the reason they were false-naturals).
3. **The 268-row correction and +642 set move** — re-run against the fixed `classify_at_time`;
   confirm the count and, critically, the SET delta (6542→7184), not just the pair count (79).
4. **The row-26 NEUTRAL verdicts** — re-run `tripwire_row26_results.json`; confirm no site
   moved off NEUTRAL/LIVE-COSMETIC into a classification trap.
5. **The branch merge** — `53be26f2` (parallel session) sits between B and C; decide squash-vs-
   preserve; read its 89 lines against the census emission findings for content-fork.

---

## Substrate facts (land where noted; most already landed this session)

- **The spine** ("absence presents as presence"; carry the provenance bit; diagnostics need a
  positive control, reasoning included) — landed in `CLAUDE.md` Build Discipline summary
  (`2fcd82dd`) and `docs/technical/build_discipline.md` (full table + instances).
- **T.1 "cosmetic" is scoped** — cosmetic for the metric-agreeing majority, classification-
  changing for the 3 false-naturals the vacuous gate hid. Landed (`build_discipline.md`,
  KNOWN_STATE B1 entry). Do not cite "cosmetic" unqualified.
- **Few-shot example is a third estimator-classifier leak surface** — demonstration beats
  instruction; contamination surface is {instructions, schema, examples}. Landed
  (`build_discipline.md`, estimator-classifier section); regen exemplar confirmed clean.
- **Set-not-count** — a fabricated/absent value homogenizes, so its damage lives in the
  variance the aggregate suppresses; check the divergence SET when fixing one. → carry into
  the OQ-46 regen witness (check per-context divergence, not just classification types).
- **Self-description fork** (a stale comment is a derived copy that forks) — the generator's
  `% required for mountain constraints` comment is stale post-B4; filed as a fix-on-next-edit
  disposition in `build_discipline.md` Pattern 2, no OQ row (one-liners get a disposition, not
  a tracking row).
- **`ab_test/stripped_schema.json` over-strips** (removed ε/supp too) — delete before regen so
  `DR_SCHEMA` can't pick it up by accident.

---

## Pointers

`audits/2026-05-31_wiring_gap_census/wiring_gap_census.md` (27 gaps, G1–G6, reconciliation=0) ·
`outputs/wiring_gap_adjudication_prep.md` (8 decisions, blank ruling sheet) ·
`outputs/tripwire_row26_results.json` (G6 family, all NEUTRAL/LIVE-COSMETIC) ·
`ISSUES.md` OQ-35–44 (census-routed) + OQ-45 (404 NL content audit, opened) + OQ-46
(D4-for-suppression = generation-template requirement) + OQ-47 (SCOPE→seed seam, confirmed
clean) · `prolog/drl_composition.pl` (row-23 fix, branch `wiring-gap-fixes` commit `39630182`) ·
`prolog/signature_detection.pl` (NL-gate fail-close, `3116ac08`; boltzmann guard :835–836
still Surface-2 critical path) · `prolog/drl_core.pl:96` (static `Supp=0` — unfixed sibling) ·
`python/sweeps/proof_of_life_surface2.py` (the Surface-2 template — the next build) ·
`python/sweeps/demotion_pass.py` (re-run; pre-dates this session's changes) ·
`docs/technical/build_discipline.md` (Patterns 1–5 + the spine + estimator-classifier section +
positive-control rule) · `docs/technical/generator_emission_map.md` (`53be26f2`, reconciled by
`5ef9294e`; durable, provenance-stamped, names itself a Pattern-2 copy) · `CLAUDE.md` (spine
capstone `2fcd82dd`; KNOWN_STATE now split out per the token-load decision) ·
`KNOWN_STATE.md` (row-23 entry, NL-gate entry, the session log) · Handoffs 1–5 (optional
substrate — the "in brief" section above carries what you need; reach for these only for the
full thesis or a session's pasted witnesses).

---

## Stopping note (carry, per every prior handoff)

This session closed the wiring-gap thread and stopped before the Surface-2 construction it
points to — a deliberate cut between a diagnosis-and-fix arc and the construction that should
get a fresh session. The cut is chosen, not detected; this instance cannot certify from inside
that it stopped in the right place. The discipline held where it usually slips: the positive
control overturned a ruling, the set-not-count check found 642 divergences the aggregate hid,
the false-orphan trap was refused, the one-liner was not ceremonialized. None of that certifies
the catalogue is sound — it closes the doors we found open and says nothing about doors we have
not looked behind. The cold read you are about to do is the only thing that can tell whether
the cut landed well. Distrust this doc; re-run the four verify-or-correct items before building
on them.
