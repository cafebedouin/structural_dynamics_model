# Design Gaps

**A ledger of capabilities the engine does not have, where the absence is a known limitation
rather than an accident — and, where one was attempted, the implementation that was built and
removed so the gap could be deferred honestly rather than half-occupied.**

**Version: v1.0**

**Status:** Companion to `design_discipline.md` and `build_discipline.md`, referenced from
`CLAUDE.md`. Where `design_discipline.md` governs *what the engine is for* and `build_discipline.md`
governs *how we build and verify*, this document records *what the engine deliberately does not yet
do.* It exists so that a gap is a visible, named entry — not an orphaned predicate, a dangling wire,
or a comment buried in one module that the next agent must re-derive.

---

## Why this ledger exists

The recurring failure mode in this repo (see `build_discipline.md`, "the spine under all of these")
is **an absence that presents as a presence**: a declared-but-unfed predicate, a gate over an empty
table, a reader with no producer. A success-shaped token fills the hole and the read site cannot
tell it from the real thing.

A *design gap* is the honest form of that absence. The distinction:

- A **defect** is an absence the code does not admit to — it reads as working. It belongs in
  `KNOWN_STATE.md` / `ISSUES.md` and gets *fixed*.
- A **design gap** is an absence the design *declares* — a capability we have chosen not to build
  yet, recorded so no one mistakes the empty placeholder for a working feature. It belongs *here*.

The rule this ledger enforces: **when intended functionality is deferred, the deferral is written
down where a cold read finds it, and any half-built apparatus for it is removed rather than left
declared-but-unfed.** A predicate that exists but is never fed is the defect; a documented gap with
the apparatus cleanly removed is the honest deferral.

Each entry records: what the capability would be, why it is absent, what (if anything) was built and
removed, and what closing the gap would require.

---

## GAP-01 — The system does not systematically track derivation chains

**The capability:** A constraint can be *generated from* another constraint — most naturally when a
single constraint fails to hold one stable base extractiveness (ε) across the observables used to
evaluate it, and so decomposes into linked components (the ε-invariance / DP-001 principle). The
engine has no first-class, queryable record of *which constraint was derived from which, and by what
mechanism.* Provenance of decomposition is not tracked.

**Why it is absent:** The corpus is authored constraint-by-constraint; nothing in the generation
pipeline emits a derivation edge with a typed reason. The nearest live relation,
`narrative_ontology:affects_constraint/2`, is a bare arity-2 causal/network edge (consumed by the
purity network, counterfactual analysis, composition, signature detection, and giant-component
analysis). The generator's DP-001 comment block instructs authors to link decomposed constraints
*via `affects_constraint/2`* — which flattens a provenance relation onto a causal one and **drops the
mechanism** (the *why* of the derivation). So such decomposition links as exist are present, but
untyped and indistinguishable from ordinary causal coupling.

**What was built and removed (deferred):** `dirac_classification.pl` once carried a lightweight
annotation layer implementing exactly this, as **Axis 1 of the Dirac mapping** (primary vs. secondary
constraints — secondary constraints being those Dirac generates by requiring a primary constraint to
stay consistent under evolution, {φ,H}≈0; the DR analog is ε-invariance as the consistency
condition):

```prolog
:- multifile derived_from/3.
:- dynamic   derived_from/3.

%% derived_from(+Secondary, +Primary, +Reason)
%  Reason ∈ { epsilon_invariance, temporal_decomposition, ... }

constraint_generation_order(C, primary)  :- \+ derived_from(C, _, _), !.
constraint_generation_order(C, secondary(Source, Reason)) :- derived_from(C, Source, Reason).
```

This was removed (2026-06-02) because it was an absence presenting as a presence: **zero producers**
anywhere (no testset asserted it, no generator emitted it, no engine code asserted it), so
`constraint_generation_order/2` returned `primary` for every constraint in the corpus by the
`\+ derived_from` cut — reading as "the corpus is entirely primary constraints" when the truth was
"the secondary-detection path was never fed." The module's own header had already sorted this axis
into *"merely relabels"* (vs. the first-class/second-class axis's four genuine wins), noting "the
primary/secondary axis is trivially 'primary' for most constraints since the system does not
systematically track derivation chains." The predicate, its sole reader
`constraint_generation_order/2`, and the `generation_order(...)` field of `full_dirac_report/3` were
removed; nothing external consumed any of them.

**The one capability it had that the live channel lacks:** the `Reason` slot — *typed
generation-mechanism* (`epsilon_invariance` vs. `temporal_decomposition`). `affects_constraint/2`
cannot carry it. This is the part worth preserving if the gap is ever closed.

**What closing the gap would require:**

1. A **producer**: the generation pipeline (`generate_constraint_pl.py`, or a hand-authoring
   convention) emits a typed derivation edge whenever a constraint is produced by decomposing
   another under a named consistency condition — wired in the same change as its consumer, per
   `build_discipline.md` Pattern 1.
2. A **schema decision**: either revive `derived_from/3` (provenance, arity-3, mechanism-typed) as a
   relation distinct from `affects_constraint/2` (causal, arity-2), or extend `affects_constraint`
   with a reason slot and a sub-type marking derivation edges. The two relations are *not*
   interchangeable; the DP-001 comment's instruction to overload `affects_constraint` for
   decomposition links should be corrected when this is decided.
3. A **consumer** that uses derivation order for something the analysis actually reads — otherwise
   the gap should stay deferred rather than re-occupied by an unfed placeholder.

**Status:** Deferred. Apparatus removed; gap recorded here. Re-opening is a framework-direction
decision (is derivation-chain provenance a first-class axis?), not a code fix.

**Reframe (2026-07-17, from `blog/2026-07/no-four-oclock-v8.md` + `docs/debugging_philosophy.md`):**
derivation-*origin* is not merely untracked but, for a completed naturalization, unrecoverable from
any single time-slice probe — the counterfactual "otherwise" an origin claim needs has been
eliminated by the drift itself. Two things narrow the gap. (1) The *mountain-vs-naturalized* boundary
does **not** need origin at all: it is cut by the beneficiary leg (`false_natural_law` — a present
otherwise checked structurally, not remembered), so it is not part of this gap. (2) The
*temporally-invariant* case splits further into **forced** (genuine mountain — no beneficiary, no
consistent negation) and **chosen** (a declared axiom, invariant like Euclid's parallel postulate but
with a live otherwise); that split is also structural (beneficiary present ⇒ chosen). What is left —
and all that is left — is **declared-choice vs unacknowledged-foreclosure** (Euclid-as-known-choice vs
Euclid-that-forgot): both are seated choices, temporally reconstructible only where the pre-closure
snapshot was authored, and separable only by the `cs_drift` acknowledged bit, which is authored not
detected. So the residual of GAP-01 is an **evidence-base limit, not a meter limit**: origin stays
unrecoverable exactly for terminal foreclosures whose earlier snapshot was never written down — the
population the essay argues correlates with power (most-administered ⇒ least paper). If the gap is
closed, the `Reason`-slot (typed generation-mechanism) is still the part worth preserving; the new
observation is that a *temporal* mechanism tag (`temporal_decomposition` / drift-snapshot lineage) is
the one that would carry the reconstructed otherwise.

---

## GAP-02 — The engine does not compute husk trajectory-shape (born-saturated vs glided)

**The capability:** distinguishing a commitment that *decayed into* a hollow/saturated state from
one that was *born* already there — the diachronic **born-vs-glide** read. It is irreducibly
temporal: it needs ≥2 time-points on an extraction/purity series, because a single snapshot cannot
tell a constraint that hollowed out over time from one that started hollow. The word "while" in
"stable *while* decaying" is the load-bearing part, and a snapshot has no "while."

**Why it is absent:** an observer-axis implementation existed —
`saturation_floor = max(ε_series) − 0.50`, with `born_saturated = saturation_floor < native_floor`
(i.e. the series peak never reached the saturation crossover, so the constraint started at/near
saturation rather than gliding there) — but it was **report-only**: zero engine consumers,
terminating in the `enhanced_report.py` `--- HUSK SIGNATURE ---` display string, which itself
disclaimed the value as "*ε authoring, not an observed property of the underlying phenomenon.*" It
was removed in `ef92a61d` (2026-06-02). It was also a *superseded duplicate*: the metric/trajectory
husk landed 2026-05-25 06:43 (`e56bc18c`) and was superseded ~4h later by the categorical
committer-axis husk attractor (`624e3b66`, 10:41); the first draft was never deleted.

**What was built and removed (deferred):** `husk_series/3`, `ep_native_series/3`, `husk_exists/3`,
`husk_point/5` (`drl_composition.pl`); the `husk_report.pl` standalone + `husk_data.json`;
`saturation_floor`/`born_saturated`/`husk_metrics` enrichment (`enrich_pipeline_json.py`);
`build_husk_signature` + the HUSK SIGNATURE report section (`enhanced_report.py`); the `_prolog_husk`
pipeline step. Full inventory + blast radius in `KNOWN_STATE.md` (2026-06-02).

**The one capability it had that the live channel lacks:** the committer-axis husk
(`cs_terminal_attractor(..., husk)` in `cs_drift_engine.pl`, kept) is a **prospective attractor**
computed from an authored `(direction, magnitude, acknowledged)` gap vector — it answers "*where
does this drift end up*," not "*what shape was the path to here.*" Born-vs-glide is the
**retrospective trajectory-shape** read, which the CS attractor table does not carry. Its nearest CS
analog is the `acknowledged` bit — an *epistemic* axis (was the drift acknowledged), not a
trajectory-shape one. So the capability is genuinely absent, not merely relocated.

**What closing the gap would require:**

1. A **framework-direction decision** that retrospective trajectory-shape is wanted at all — the
   removed version fed nothing, so it may be genuinely unwanted rather than merely deferred.
2. If wanted, build it on the **committer axis** over an authored CS series, **not** the observer
   ε-measurement series. Reading observer ε into a committer-labeled diagnostic is the cross-axis
   reduction `two_axis_architecture_v7.md` (§ non-goals, "No reduction of committer-axis diagnostics
   to observer-axis structures") forbids — and it is exactly what the removed version did: it was
   observer-computed (powerless canonical DR context) yet consumed as a per-CS-reading property.
3. A **consumer** that reads the result, per `build_discipline.md` Pattern 1 — otherwise it returns
   to being the unfed placeholder this ledger exists to prevent.

**Status:** Deferred. Apparatus removed (`ef92a61d`); gap recorded here. Whether born-vs-glide is
wanted is a framework-direction decision (does the committer axis want a retrospective
trajectory-shape diagnostic beside its prospective attractor?), not a code fix.

---

## GAP-03 — `orbit_data.json` cannot carry an in-file manifest (provenance is sidecar-only)

**The capability:** The repo's manifest convention (see `CLAUDE.md`, "Pipeline Output Manifest
Convention") is that a pipeline-output JSON carries a top-level `manifest` key stamping run
provenance (timestamp, corpus counts, commit, dirty flag), injected by `run_pipeline.py`'s
`inject_manifest`. `pipeline_output.json` has it. The intended uniform capability is: *every pipeline
output is self-describing — open the file, read its manifest, know the run.* `orbit_data.json` does
**not** have this, and by construction **cannot**.

**Why it is absent:** `orbit_data.json` is a pure `id → {orbit_signature, contexts}` dict consumed by
**7 readers that iterate it with bare `.items()`** (`game_theory_nash.py:158`,
`game_theory_mixed_strategy.py:89`, `python/audits/sheaf_audit.py:310`,
`container_typology_analysis.py:259`, `python/reports/queries/meta_reporter.py:100`,
`extract_corpus_data.py:250`, `normalize_orbit_ids.py:43`). `inject_manifest` prepends a sibling
`"manifest"` key; into this file that key would be silently iterated as a **fake constraint** by all
7 — the exact absence-presenting-as-presence failure this ledger guards against, except here the
*presence* (a junk "manifest" pseudo-constraint) is what would be injected. The id-keyed schema has
no reserved namespace for metadata, so no sibling key is safe.

**What was built (the honest deferral):** rather than restructure the schema (a nested
`{"manifest":…, "orbits":{…}}` would break *all* consumers, including the `.get(cid)` ones) or patch
7 iterating call sites with an old-vs-new diff each (large blast radius for a read-only provenance
need), provenance was externalized to a **sidecar**: `run_pipeline.py:_manifest_step` writes
`orbit_data.manifest.json` carrying the *same* `build_manifest(run_at)` dict in the same step, so the
two files are provably the same run. Consumers that need the join-provenance read the sidecar
(`python/w1_sheaf_join.py` asserts sidecar-manifest == pipeline-manifest before merging); the 7
iterators are untouched.

**What closing the gap would require:** a reserved-namespace decision for orbit-style id-keyed
outputs — either (a) migrate all id-keyed pipeline JSONs to a `{"manifest":…, "<payload-key>":{…}}`
envelope and update every consumer (a corpus-wide schema migration, justified only if more id-keyed
outputs need in-file provenance), or (b) ratify the sidecar as the standing convention for id-keyed
outputs and document it beside the manifest convention. Until then the sidecar is the rule for
`orbit_data.json`.

**Status:** Deferred by sidecar. The capability (uniform in-file manifest) is absent for id-keyed
outputs by consumer constraint, not by oversight. Tripwire in `KNOWN_STATE.md` (2026-06-02) so no one
re-attempts the inline injection.

---

## GAP-04 — No first-class kernel/reading axis: the two axes are conflated in the classifier

**The capability:** A *kernel* is an idea-space (e.g. `rule_of_law`, `animal_status`); a *reading* is a
declared standpoint on it (e.g. natural / positivist / realist; property / welfare / abolitionist). The
engine should be able to (a) hold a kernel fixed and compare its readings *as a set*, and (b) hold a
reading-stance fixed and sweep it across kernels (the transpose). Neither is a first-class operation.

**Why it is absent:** The kernel/reading corpus did not exist until 2026-06-02. Readings are currently
represented only as sibling constraint files sharing a `kernel__reading_name` prefix, linked by
`cs_kernel_registry`; the kernel is an implicit string prefix, not a queryable object with enumerable
readings. Every reading is classified *independently* through the observer pipeline (power×scope), so the
two axes — **observer** (power/scope index, live) and **reading** (which standpoint on the kernel) — are
not distinguished by the classifier. There is no predicate that takes a kernel and returns its readings
as a compared set, and no reading-stance label comparable *across* kernels to enable the transpose.

**Evidence (manifest `ae10e7e`, run 2026-06-02T18:54:31Z, n=772):** 542 readings sit under 183
multi-reading kernels; 231 constraints are standalone (no reading decomposition). Within a kernel,
readings differentiate (ε differs in 176/183 kernels, victim-set in 183/183, `claimed_type` in 113/183),
so the distinction is real and carried in the data but not surfaced as an axis. Example `animal_status`:
`property_reading` (ε 0.05, rope) / `welfare_reading` (ε 0.45, tangled_rope) / `abolitionist_reading`
(ε 0.92, snare) — three standpoints, three seats, classified as three unrelated constraints.

**What closing the gap would require:** (1) promote the kernel to a queryable object with enumerable
readings; (2) a reading-stance vocabulary comparable across kernels (the selection-seat of OQ-56 / Seat
Theorem Cor 2b — a declared, contestable premise, not derivable, and blocked on the cross-kernel
clustering analysis only now possible); (3) a kernel-fixed reading-comparison predicate and its
transpose, each with a consumer. Open question + resolution criteria: **OQ-53**.

**Status:** Partially built (transpose increment, 2026-06-26). The **reading-stance transpose**
— requirement (3)'s second half, "hold a reading-stance fixed and sweep it across kernels, with a
consumer" — is now live: `cs_kernel_registry:cross_kernel_stance_profile/2` (+ `reading_stance/2`,
`stance_cohort/2`) over the `declared_stance/2` seat, grounded in the kernel-independent
`logical_fingerprint:fingerprint_shift/2` signature (now serialized into `pipeline_output.json`),
with consumer `python/cross_kernel_stance_report.py` (writes `outputs/cross_kernel_stance.{json,md}`).
The cohort is **declared, not derived** (requirement (2)'s seat, partial): morphology only suggests
candidates — witnessed on the abolition cohort, an exact-stem rule catches 4/7 (stems fragment) and a
substring rule over-admits an anti-abolition reading — so each cohort member carries
morphology-suggested-vs-hand-declared provenance, and the verdict carries it too. Witnessed result:
abolition converges on `shift(*,snare,rope,snare)` (5/7) on both live twins; originalist is
kernel-divergent. **Still open under OQ-53:** requirement (1) (kernel as a first-class queryable
object), the kernel-fixed↔transpose pair as a formal paired operation, and the full curated stance
vocabulary (this build seeds an initial declared table for the exercised stances only).

**The missing population layer (noted 2026-07-23, operator-framed).** Readings are discrete
idealized standpoints; real holders of a reading (e.g. positivist on a `rule_of_law` kernel) hold
it across a *range of attitudes* — and nothing in the axis models that range. Two halves, sharply
different in kind. (a) The COMPUTED half already exists: a reading's gauge orbit over the observer
index (power × scope × time × exit) IS the engine's predicted attitude-range toward it, per
position — see `blog/2026-05_or_before/diagnostic_value_of_perspectival_gaps.md` for the
index-position account. (b) The AUTHORING half is the gap, and **for an LLM-authored corpus it
cannot be factual population data — it is an existential/generative question about the authoring
position itself**: can variation in the perspective the model is given change what gets authored?
Is handing the generator a stakeholder or other position enough to produce a genuinely different
(better) story, or does the model's disposition swamp the assigned seat? Adjacent evidence, both
directions: OQ-228 (cross-model corpora are dispositional fingerprints; kimi collapses to ~2
perspective molds regardless of material) suggests disposition dominates; OQ-73 (does the
reading-set move when the generation frame moves?) is the controlled probe that would answer it and
is already authored — this note is a second consumer for OQ-73's result. Not a new tracker;
population-layer work, if taken up, extends OQ-53/OQ-73 rather than minting a parallel question.

---

## GAP-05 — The reading axis has no cross-index gluing test (Boltzmann is observer-axis only)

**The capability:** "Do a kernel's readings glue into a global section, or are they irreducibly plural?"
— the sheaf/H¹ question on the *reading* axis. The engine has one cross-index test, `boltzmann_compliant/2`
(the Power×Scope factorization / coupling test in `boltzmann_compliance.pl`), and it runs **only on the
observer axis**. Nothing tests factorization or gluing across the readings of a kernel.

**Why it is absent:** Same root as GAP-04 — until 2026-06-02 there was no kernel/reading corpus to run a
reading-axis test against. The Boltzmann coupling test predates it and was built for the observer index
(`coupling_test_powers/1`, `coupling_test_scopes/1`). Note the corrected framing: Boltzmann
non-compliance is **not a pathology** — by the Seat Theorem (Coupling Theorem, §4) an index-invariant
verdict is seat-free/contentless, so Boltzmann invariance is a *partial test for Mountain-ness*, and
non-compliance = the verdict is seated on the observer index. The reading axis needs its *own* such test;
it cannot be read off the observer one. The `w1_sheaf_join` / `sheaf_status` / `h1_band` machinery is the
natural home for the reading-axis cohomology but is not yet wired to the kernel/reading grouping as a
gluing test.

**What closing the gap would require:** (1) define the reading-axis site (a kernel's readings as the cover)
and a gluing/obstruction measure on it, distinct from observer coupling; (2) decide how it relates to the
existing H¹/W1 obstruction (cf. OQ-51) and to seat-orthogonality / detection-independence — Seat Theorem
§3's correction is that two seats can be mutually non-representable (Theorem 7), so a reading-axis
obstruction may be gradient-orthogonal to the observer one and must not be reduced to it; (3) a consumer.
Open question + operationalization: **OQ-54**.

**Status:** CLOSED (2026-06-02) — built as `cs_kernel_obstruction/4` in `cs_kernel_registry.pl`,
observer-blind (reads only `cs_reading_relation`), orthogonality pre-discharged by Theorem 7. It is a
**distinct axis** from OQ-51's observer H¹/W1, not the same object. The build surfaced and repaired a
data defect (86 name-form-mismatched edges under-counting closure across this probe, `cs_corpus_analysis`,
and `json_report`), with a generator canonicalization (`generate_constraint_pl.py`) preventing recurrence;
dangling residue is OQ-58. Remaining for full wiring (a JSON field + the OQ-55 router) tracked under
**OQ-54** / **GAP-06**. Provenance + witnesses: KNOWN_STATE 2026-06-02, OQ-54.

---

## GAP-06 — Reading-disagreement is not trifurcated (no Type A/B/C router)

**The capability:** When two readings of a kernel disagree, classify *why*, per
`docs/debugging_philosophy.md` §6: **Type C** (index ambiguity — the readings answer under different
declared seats; the common, correct case for genuine plurality like natural/positivist/realist → specify
index, do not collapse); **Type A** (frame drift — a criterion slides within what should be one seat →
frame-fix); **Type B** (structural — the kernel's own commitments are inconsistent → genuine fracture).
The engine has the raw pieces (`sheaf_status`, `h1_band`, `cs_pattern`, `cs_axiom_foreclosed`,
`metric_drift_events`) but no predicate that takes a kernel's reading-set and returns {ambiguity, drift,
structure}.

**Why it is absent:** Same root — no kernel/reading corpus to test a disagreement router against until
2026-06-02. The trifurcation is the operational core of the kernel/reading engine and is the natural
consumer of GAP-05's reading-axis gluing test (H¹≠0 with each reading internally coherent ⇒ Type C;
internal incoherence ⇒ Type B; same-seat criterion drift ⇒ Type A).

**What closing the gap would require:** (1) GAP-05's reading-axis obstruction as input; (2) the
three-stage diagnostic of `debugging_philosophy.md` §6 implemented over a kernel's readings, mapping
existing diagnostics onto A/B/C; (3) a consumer that reads the verdict. Open question: **OQ-55**.

**Status:** Built (within-kernel) 2026-06-20 via OQ-55 — `prolog/cs_trifurcation.pl`
(`cs_reading_trifurcation/3`), serialized as `reading_trifurcation`, controls 8/8 green, all four
branches firing on the live corpus. This entry's status had sat stale at "Deferred" until
2026-07-23. What remains gap-shaped is UPSTREAM of the router: the router types a disagreement
(A/B/C/unknown) but nothing discriminates *diagnostic* from *erroneous* disagreement before typing
applies.

**Upstream candidate (added 2026-07-23): the three-feature test** from the operator's essay
`blog/2026-05_or_before/diagnostic_value_of_perspectival_gaps.md` — disagreement is diagnostic
(structure-revealing) rather than erroneous iff it is (1) **structurally predictable** from the
observers' index positions, (2) **persistent across instances**, and (3) **irreducible by
information** (sharing data does not close it). Mapping onto the router: irreducible-by-information
disagreement ≈ Type C genuine plurality; reducible ≈ a communication error the router should never
see; non-persistent ≈ case-specific error. Not yet an implemented predicate — a design candidate
recorded here so the router's next revision considers a pre-filter; also binds essay-side work
(uke_write §2.4: a disparity failing the three-feature test is not a load-bearing tension member).

---

## GAP-07 -- affects_constraint targets are not resolved into the (bounded) structural-class space

**The capability:** A network edge whose target names no existing constraint (a *dangling*
`affects_constraint/2` target) could be resolved to the structural class it denotes -- the
`logical_fingerprint` isomorphism class the engine already computes -- instead of being matched by
exact atom identity. The engine matches targets string-first; it has no resolver from a target onto
the structural space, and no first-class notion that that space is bounded and closed.

**Why it is absent:** `affects_constraint/2` is consumed string-first by the purity network,
counterfactual, composition, signature detection, giant-component, and coupling layers -- each treats
a target atom as simply present or absent. On the current corpus **1710 of 2548 edges (67%) dangle**;
9 are one-character delimiter typos against an existing sibling reading (e.g.
`john_1_1_logos_orthodox_christological` for `..._logos__orthodox_christological`), the remaining
~1701 are fresh-minted descriptive names for constraints never authored (**OQ-58**; **OQ-59** is the
sibling within-kernel duplication). There is no referential-integrity guard on the network layer (the
OQ-58 hard-fail was built for `cs_reading_relation` only), so the danglers are silent. Treating them
as an *open frontier* (a worklist to auto-generate against) is falsified: per-story dangle-fraction is
flat-to-rising across a 15x size range and four generators (~0.63-0.74), and 95-98% of dangle edges
land on a singleton target string no other story coined -- it is exhaust of open-vocabulary
name-minting, not a closing boundary (`python/dangle_curve.py`, `python/dangle_indegree.py`).

**Why resolution is nonetheless well-founded -- the structural space is a bounded attractor
(witnessed).** Two generator-independent witnesses carry the bounded verdict:

1. **shift saturates <1% of its hard cap.** `fingerprint_shift/2` is the constraint's type-response
   over a *fixed* 4-point observer grid (`standard_context_for_power/2`), so its combinatorial ceiling
   is `8 types ^ 4 contexts = 4096` (8 cascade type atoms, witnessed). Realized: ~59 = **~1.4%**. A space with a known enumerable
   ceiling that closes at ~1.4% of it is closed by structure, not by enumeration stopping.
2. **Coupon-collector estimator gives a finite intercept.** Regressing per-story discovery rate on the
   *current realized count* fits `dS/dn = r*(1 - S/S_max)` linearly with a **finite S-axis intercept**
   for both the structural 5-dim space (S_max~108) and shift (S_max~58), R2~0.84-0.89. A finite
   intercept is the bounded-attractor signature; it is the test that separates bounded from log/open
   (rate-vs-n could not -- both fall as 1/n; rate-vs-S can). See `python/territory_test.py`,
   `python/fingerprint_rarefaction.py`, `python/separate_spaces.py`.

*Illustration, not proof:* realized occupancy of the 5-marginal product cap is ~0.07% (116/155,848
current; 99/149,688 v5), generator-stable -- demoted from proof because MI (below) shows it overstates
richness.

*Mechanism (MI, `python/mi_structure.py`):* the five structural dimensions carry **45% (current) - 56%
(v5) redundant information** (total correlation / sum of marginal entropies), and the strongest
couplings are **generator-invariant** (`voids<->zone` normalized-MI ~0.83, `props<->actors` ~0.78 in
both corpora). So the boundedness is achieved *substantially through dimensional redundancy plus an
entropy-inflated nominal cap* (effective independent cap ~2^7.2 ~ 146; effective joint ~2^2.6-4.0 ~
6-16 cells) -- **not**, on current evidence, primarily through a rich multi-way forbidding of
independent dimensions. The bounded attractor is real; the strong "0.07% forbidden by structure"
reading is not witnessed. The one genuinely structural, authoring-independent fact MI does establish is
that the *coupling pattern itself* is generator-invariant -- the redundancy lives in the engine's
predicates, not the corpus.

**What closing the gap would require:** (1) a structural-class resolver mapping a target to its
fingerprint class; (2) fail-closed consumers -- a network consumer must distinguish edge-to-interior,
edge-to-resolved-class, and edge-to-unresolved (terminus), never silently traverse a phantom; (3)
repair the delimiter typos *first* so the frontier set is clean (else a typo resolves to / would
regenerate an existing node -- straight into OQ-59) -- **DONE 2026-06-03**: 10 `affects_constraint`
edges (9 distinct targets; `paris_article_4_ndc__supranational_reading` is targeted twice) repaired,
dangling 1710->1700, witnessed target-arg-only diff (branch `repair-affects-constraint-typos`); the
same 9 strings in 6 `cs_reading_relation` edges are held as a separate OQ-58 batch. (4) **RULED
2026-06-03 -- key on the five structural dims; exclude `shift` and `coupling`.** `coupling` is
near-injective (239/772; the full 7-dim fingerprint is 872 classes at v5, ~3.9 constraints/class),
giving almost no populated resolution targets. `shift` is excluded *not* as redundant -- it adds real
splits (99->199 classes at v5) -- but because those splits are the wrong kind: it is a different
(constraint x fixed-4-context) space, and adding it **doubles singleton-stranding** (v5 1.5%->3.0%,
current 9.8%->14.4%) and halves class size, degrading exactly the populated-class property a resolver
needs. The five structural dims give 99 classes at v5 with **98.5% of constraints in populated
(non-singleton) classes**, and singleton-stranding *falls* with scale (current 9.8% -> v5 1.5%), so the
resolver improves as the corpus grows. The ~half redundancy among the five (MI, `mi_structure.py`) is
acceptable/robust, not a reason to drop dims. Witness: the 5-vs-6 singleton table, re-derivable from the
fingerprint dump. Open
questions: **OQ-58**, **OQ-59**; related untyped-edge gap: **GAP-01**.

**Sibling-layer corroboration (`cs_reading_relation`, OQ-58 cross-corpus census, 2026-06-20).** The
bounded-not-open verdict extends to the committer-edge layer, split into two distinct invariants:
- **Dangling RATE is bounded ~2-5% across independent lineages** (testsets_haiku 3.7%, testsets_flash
  2.3%, archives/datasets/kernel_v1 4.8%) — a cross-lineage invariant; absolute counts (94-127) do
  NOT scale with 700->3380 corpus growth.
- **Defensible (in-degree>=2) COUNT ~40 is reproducible within a lineage** (haiku 39 ~= flash 41,
  haiku∩flash = 39; same kernel population, second-model twin) but is NOT universal (kernel_v1 = 8, a
  different kernel population; the tri-lineage common core is 1).
- ⇒ the dangling space is bounded, not an open frontier. The LIVE testsets/ rate of 93.5% is a
  *sparsity* artifact (1.03 readings/kernel, 97% singletons), NOT a frontier — it tracks kernel
  sparsity, not minting. Witness: `audits/2026-06-20_oq58_cross_corpus_incompleteness/` (read-only
  `census_driver.py` + saved output; selftest PASS).

**Status:** Partially closed. Bounded-attractor basis analyzed 2026-06-02; typo repair done and the
resolver keying ruled 2026-06-03 (items 3-4 above); `cs_reading_relation` sibling layer
characterized as bounded 2026-06-20 (above). **Remaining to close:** the resolver itself
(item 1) and its fail-closed consumers (item 2). Scripts above re-runnable on a fingerprint dump.
**Drift framing held deliberately:** the post-saturation corpus is a *bounded state
set*; whether change over it has memory (Markov vs higher-order) is an **open measurement** pending
`fingerprint_drift` velocity/acceleration -- not yet asserted.

## GAP-08 — The `intent_*` evidence layer is designed but never fed (and its sole bulk consumer is now retired)

**Declared:** 2026-06-05 (operator ruling, pre-rebuild ISSUES triage Item 2 — option A:
declare the absence; defer populate-vs-delete until a research question consumes intent data).
**Amended:** 2026-06-12 (OQ-106 retire, see below).

**The absence:** seven authored predicates — `intent_viable_alternative/3`,
`intent_alternative_rejected/3`, `intent_beneficiary_class/2`, `intent_power_change/3`,
`intent_suppression_level/4`, `intent_resistance_level/4`, `intent_norm_strength/3` — have
**zero facts in every corpus ever generated** (witnessed 2026-06-05: 0 in the live rebuild,
0 in kernel_v1, 0 in original_v6). The schema has no authoring surface for them; the
generation prompt never mentions them. Do not treat any `intent_*`-derived output as data:
it is a read over an empty table.

**The live residual (OQ-43 class, FIFTH instance) — CLOSED FAIL-CLOSED 2026-06-11:**
`signature_detection:has_viable_alternatives/2` formerly defaulted `false` on the empty
`intent_viable_alternative/3` table, and `natural_law_signature` requires
`HasAlternatives == false` — so absence SUPPORTED natural-law certification (pass-open).
The OQ-43/OQ-44 policy ruling (operator, 2026-06-11) changed the default to `unknown`:
`false` now requires authored evidence, NL's `== false` check fails until an intent layer
or authored alternatives table exists, and the resulting un-certification of
`thermal_dissipation_constraint` was accepted. See `signature_detection.pl`
`has_viable_alternatives/2` header. Logged on OQ-43.

> **CORRECTION 2026-08-17 (OQ-251 audit) — "the resulting un-certification … was accepted"
> describes an event that did not occur, and the predicate is worse off than this entry says.**
>
> **(a) Nothing was traded away.** `has_viable_alternatives/2` clause 1 requires BOTH
> `affects_constraint/2` AND `intent_viable_alternative/3`. The second has **0 facts on every
> corpus** (this entry's own finding), so `HasAlternatives == false` was **never once satisfied by
> data** — it was always the catch-all default. `thermal_dissipation_constraint` authors *neither*
> field; `maxwell_demon_impossibility` authors 4 `affects_constraint` facts and dies on the empty
> table. The fail-close therefore did not **cost** a certification, it **revealed** that none had
> been earned. The FAIL-CLOSED disposition is unchanged and correct — Pattern 5 working — but
> anyone citing this line as a cost-of-fail-closing is citing a non-event.
>
> **(b) BOTH branches are dead, not just `false`, so this is not a `natural_law` story.** Measured
> across all seven legs (8,688 constraints — testsets 276, haiku 960, flash 960, kimi 1005, sonnet
> 1001, kernel_v1 1106, original_v6 3380): range is the **singleton `[unknown]`** on every one,
> `count(true) = 0` and `count(false) = 0` everywhere. `has_viable_alternatives/2` is a **constant
> function**. Two different deaths: `false` is dead **by construction** (no clause can emit it),
> `true` is dead **by empty table** (a clause can emit it; its input never exists). Note
> `affects_constraint` is richly authored (9,523 facts across the legs) — clause 1's first conjunct
> succeeds abundantly and the predicate dies entirely on the second.
>
> **(c) The consequence this entry does not draw: `coordination_scaffold` is dead too.**
> `coordination_scaffold_signature/1` requires `HasAlternatives == true` (`signature_detection.pl:458`),
> so it fires **0 times** on both legs measured (live 0/276, kernel_v1 0/1106) — by empty table, not
> by construction. The `== true` consumers this residual's sibling text calls "unchanged … they never
> fired on the empty table and still don't" are accurately described but under-labelled: *unchanged*
> reads as reassurance when the fact is that a named signature in the cascade cannot fire at all.
> Full consumer surface: **OQ-296**. Witness:
> `audits/2026-08-17_oq251_natural_law_reachability/probe_hva_constant.pl` + `audit_log.md`.

**Consumer retirement (OQ-106, operator ruling 2026-06-12):** `intent_engine`'s
`structural_coercive_intent/4` top verdict — the only consumer that read five of these
tables in bulk — was DELETED with its five config params (range-dead threshold, never
fired, no downstream consumer; witness `audits/2026-06-12_oq106_retire/`). The remaining
live reader is the fail-closed `has_viable_alternatives/2` gate above. The retired
verdict's unique capability — the interval-level intent conjunction, "the coercion
increase is by design" — is part of THIS declared absence, not destroyed: the same ruling
ratified **capture-as-design** as the piton intension (the computed `constraint_captured/1`
gate carries the designed/decayed axis), with a recorded kill condition in the OQ-106
close. If that proxy is ever falsified, the falsifying case is the consuming research
question that arms revival.

**What revival requires (unchanged, generic — armed-not-scheduled was explicitly
declined):** a research question that consumes intent evidence; then schema block + prompt
guidance + compiler emission + threshold re-derivation as its own staged work item under a
fresh preregistration, fail-closed over these tables per OQ-44 (do NOT add authoring
surface mid-baseline — the rebuild's ε-idiom and diff baselines are forming).

**Amended 2026-06-16 (provenance correction + a candidate consuming research question arrived):**
- **Provenance correction (finding, witnessed).** "Zero facts in every corpus ever generated"
  above is imprecise. By-corpus count of `intent_viable_alternative`: **original_v1 43/43, v3 14/18,
  v4 23/39**, then **v5 1/702, v6 0, kernel_v1 0, live 0**. The layer was *fed in the early corpora
  and abandoned from v5 onward* — not never-fed, and the drop predates the 2026-06-05 reset (so it is
  NOT a de-leak-rebuild regression). Revival is therefore **restore-shaped**: an early authoring pattern
  exists to seed the schema block + prompt.
- **Candidate consuming research question: OQ-128.** Gating `natural_law` on *no stakeholder asserts a
  viable alternative* (`\+ intent_viable_alternative(I,_,_)` — the per-stakeholder sheaf criterion for
  `HasAlternatives==false`) is exactly the "research question that consumes intent evidence" this gap
  names as the revival trigger. Recorded as a **finding, not a scheduling**: arming revival remains the
  staged, preregistered, fail-closed work item above, and the mid-baseline caution still governs the
  OQ-128 witness-set authoring. Evidence + checks: ISSUES.md OQ-128 + `audits/2026-06-13_oq50_power_scaling_residue/`.
  **VINDICATED 2026-06-17 (converged-control validation, OQ-128 VALIDATION block):** alternative-existence
  is the *correct, structural* discriminator — `thermodynamics_entropy` (authored no-alternative) is wrongly
  exit-degraded and *should* restore via the dead `natural_law` override; `radiative` (authored competing
  mechanisms) correctly stays non-mountain. Not a candidate anymore — a real consuming research question.
  **CAVEAT (2026-06-17b, `audits/2026-06-17_mountain_authoring_sweep/`):** the trigger must be read from
  CONTENT-STRUCTURAL signal (authored per-seat type-diversity + contingency/false-summit omega), NOT prose
  framing — 370/627 engine-declared mountains carry "impossible alternative"-style prose AND a contested
  reading, so a prose-sourced override would re-certify them. The mid-seat degradation is universal (hits
  clean `fermats_last_theorem` identically to contested `axiom_of_choice`), so revival rescues only the
  ~166 content-clean mountains; the 370 contested are an upstream authoring question (operator's seat).
  **DISCRIMINATOR LIMIT (2026-06-17c):** the content-classifier has two legs of unequal trust. The
  **type-diversity leg** (story authors a non-mountain `constraint_classification` seat) is a *fact* — gate
  on it (161/370 contested were caught here, 10 with same-seat contradictions). The **contingency-omega
  leg** is author self-report and misfires both ways: false-positive (witnessed — `radiative`/`actinide`
  trip the "false summit" keyword on omegas that resolve to *"still Mountain"*) and false-negative
  (all-mountain seats + genuinely contingent substance + no confessing omega passes as clean = the AC case
  minus its omega). Do not gate the engine on the omega leg; treat it as advisory review-flagging only. Also:
  the live-testsets schema authors **no** per-seat types, so the strong leg is unavailable there — those
  mountains (incl. the original `radiative`/`actinide`) cannot be hard-classified and stay an authoring call.
  **SUPERSEDED 2026-06-17g (operator — routing architecture; see ISSUES.md OQ-128 SUPERSESSION):** this whole
  GAP-08 thread was framed as "revive the intent layer to FEED the `natural_law` exit-immunity OVERRIDE." That
  override is now ruled the wrong design: **`natural_law` is diagnostic — it comments and routes, it does not
  reclassify.** The engine never overrules its own degradation back to mountain; the author↔engine diff is the
  product, consumed by a routing sink, ruled by review. So GAP-08's restoration corpse is to be **deleted, not
  revived**, and the intent-layer-as-override-trigger consuming-research-question is **dead** — the override
  was wrong, not under-fed. The `intent_*` layer absence stands as a declared gap on its own terms (other
  consumers), but its OQ-128 motivation is withdrawn.
  **REFINED 2026-06-17h (demote, not delete):** precisely, the *resolver overwrite*
  `resolve_modal_signature_conflict(_, natural_law, mountain)` (signature_detection.pl:867) dies; the
  *detector* `natural_law_signature` (:359) LIVES, demoted to a router input (a third, author-independent
  reading). So GAP-08's true residual is **not** "feed the override" — it is the still-open
  **author-independent immovability signal**: what structural fact, written by neither the author (uniformity
  is contaminated — the seat-blind agent flattens) nor the degradation (the exit-table is the checkee) nor
  Boltzmann (orthogonal), reads "immovable"? Routing makes a *weak* detector usable (flag, don't certify), so
  this is non-fatal — but unsolved, and it shades into what the generator should emit (typed-invariance).
  Design: `audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md`.
  **OQ-113 closed against this residual (2026-06-18).** OQ-113 settled the detector's gate leg
  (`has_viable_alternatives/2` is range `{true, unknown}` — `false` builder-unreachable, fork (b))
  AND its limb 2: `determine_pure_subtype/2`'s `pure_natural_law` subtype is provably unreachable
  for the same reason and now `throw/1`s a tripwire if a future corpus/schema ever powers it (the
  §9b.2 KILL, made loud). So this §7 residual is the SINGLE remaining capability gap for BOTH
  limbs — the live marker that the author-independent immovability signal is unbuilt. Until it
  exists, `natural_law_signature` and `pure_natural_law` both stay 0-firing by construction.
  **Candidate substrate (logged as hypothesis, not asserted — cross-sibling discipline):** the
  live corpus authors the CS/committer-axis schema (`narrative_ontology:cs_axiom_contradiction/2`,
  reading-relation edges `forecloses`/`coexists_with`) instead of `constraint_classification`
  seats. That schema is a *candidate* author-independent reading of foundational immovability —
  but it needs its own validation (does a foreclosure structure separate immovable-by-structure
  from flattened-uniform?) when §7 is actually worked; it is NOT a drop-in for `HasAlternatives`.
  **Empirical constraints on any §7 re-powering (OQ-45 content audit, 2026-07-01/02 —
  `audits/2026-07-01_oq45_oq52_hidden_winners/WRITEUP.md`):** (1) the beneficiary leg must
  distinguish **gain-AROUND-persistence from gain-from-authoring** — genuinely natural constraints
  carry named ecosystem winners in prose (religious institutions ↔ mortality; extraction-system
  beneficiaries ↔ carrying capacity), so a prose-winner criterion false-positives on real
  mountains, while `BeneficiaryCount==0` over authored facts false-negatives nothing (0/404
  authored) and misses content-level winners entirely; (2) the gate is **victim-blind** — NL
  certified twin stories carrying authored `constraint_victim` facts (2 per twin, witnessed);
  (3) content-vs-facts divergence is REAL, not hypothetical: 6/35 read 404-members + 4/26
  kernel_v1 + 1/5 flash NL stories describe systematic winners in their own prose (quote-verified,
  all social constraints; the formal/math members read clean) — so a re-powered detector
  restricted to formal/physical domains has empirical support that a domain-blind one lacks.
- **Retire the Pattern-2 duplicate.** The archival `oq50-power-scaling-residue` branch drafted a standalone
  `no_viable_alternatives/1` flag as the GAP-08 fill; it duplicates this declared `intent_*` layer and
  bypasses the revival protocol. The declared surface should be revived, not paralleled — the flag should
  be retired, not landed (OQ-128 records this).

**DECLINED ON PRINCIPLE 2026-06-23 (operator ruling — supersedes the "deferred revival" posture for the
intent-attribution predicates).** The layer's six imputed-intent predicates — `intent_power_change`,
`intent_suppression_level`, `intent_resistance_level`, `intent_norm_strength`, `intent_beneficiary_class`,
`intent_alternative_rejected` — attribute a *mental state to a third party* (what some actor *intended*).
A logic engine cannot know another agent's intent; only an agent can declare its own, and this system's
sole self-declaration channel is the authored commitment/CS structure (committer axis). So these predicates
are **out of scope by construction, not unscheduled** — revival is ruled out and the empty tables are
correct, not unfinished. **Retire rather than revive.** The principle is promoted to
`docs/design/design_discipline.md` §4 ("reads effect and self-declaration, never imputed intent"). What the
layer was *reaching for* — the designed-vs-decayed distinction — survives in structural form (the piton
`constraint_captured/1` axis, computed from effect over time) and as the still-open author-independent
immovability signal (§7 residual / CS committer axis), neither of which imputes intent. Carve-out:
`intent_viable_alternative/3` is arguably *not* an intent attribution — an alternative's *existence* is a
fact about the world, not a mental state — but its consuming motivation is already withdrawn by the
routing-not-override supersession above (2026-06-17g) and its surviving need points at the CS axis, so it
needs no intent layer either. Net: the `intent_*` surface is to be **deleted, not fed**; only `data_verification.pl`
referencing diagnostics and the fail-closed `has_viable_alternatives/2` gate remain to clean up at rebuild.

**SUNSET ATTACHED 2026-08-18 (OQ-296 close; the sunset itself is OQ-317, due 2026-11-17).** OQ-296
ruled to KEEP `natural_law_signature/1` and `coordination_scaffold_signature/1` as unpowered
sockets rather than retire them, and the justification rests entirely on this section: §7's
author-independent immovability signal would revive **both signatures at once** — one capability,
two detectors. That payoff is the whole reason dead code is being kept. **If §7 is still unpowered
on 2026-11-17, the keep-sockets disposition returns for re-ruling with `retire` as the DEFAULT, and
the burden sits on whoever wants to keep: a positive case that §7 has moved, not an absence of
objection.** A deferral that renews itself by silence is how an unpowered socket becomes permanent
furniture.

**The Ω this section inherits, sharpened by OQ-296 (hold it open until §7 is specified).** OQ-296's
whole finding is that `has_viable_alternatives/2` is a constant because its `true` branch reads an
empty authored table and its `false` branch is unreachable — i.e. the "is this immovable?" question
currently bottoms out in authorship. Building §7 does not automatically escape that; it **relocates
the question one layer down**:

> *Does the author-independent immovability signal produce a non-authored `false`, or does it bottom
> out in authored inputs one layer down?*

If the latter, §7 re-creates OQ-296's defect with more machinery — the drift OQ-251 worried about,
performed deliberately. **This question must be answered in the §7 design, not after it**, because a
signal that fails it will look exactly like a signal that passes: both emit a real-looking token.

---

## GAP-09 — No kernel-dominance stage: the kernel set is unfiltered for dominance (by ruling, not accident)

**Declared:** 2026-06-06 (operator ruling LIBERAL, kernel-first router; OQ-79).

**The absence:** the kernel-first router (`c-orchestrator._step_decompose`, primed scope prompt)
routes a topic to kernel whenever a foundational reading is *constructible*. Phase 0
(`outputs/kernel_first_phase0/PHASE0_READOUT.md`) measured this as **kernel-liberal**: loud
means-disputes that share a foundational axiom (nuclear-as-climate-solution, the phonics/whole-language
"reading wars") route to kernel alongside genuine kernels (magnifica, Zionism), because they are
*contentful* (the situation does not settle them — `docs/seat-theorem-v1.md` Coupling Theorem) and
contentful ⇒ seated ⇒ admits a foundational construction. There is **no stage that judges whether a
kernel-positive is *dominantly* a foundational contest** (the main structure) versus a means-dispute
with an available-but-minority foundational strand. So the kernel set is unfiltered for dominance.

**Why it is absent (deliberate, two reasons):**
1. **A seat-FREE dominance ranking is forbidden** by the seat theorem (§6: the framework cannot
   deliver a seat-free ranking of rival selection-premises). So a dominance stage cannot be an
   "objective" filter; it can only be a *declared, seated* curation. The kernel-positive label is
   demoted accordingly ("admits a foundational construction," dominance unjudged) — see CLAUDE.md
   Critical Distinctions; that label demotion is the honest substitute for the absent filter.
2. **A seated dominance stage is permitted but deferred.** Building it now would be designing the
   discriminator blind, ahead of a witnessed pile of kernel-positives — the measure-first mistake
   Phase 0 just avoided (it is what falsified the pre-built A3 grounding leg). Kernels accrue
   uncurated first; a dominance stage, if ever wanted, is designed *against* the accrued pile.

**What was NOT built (and why that's correct here):** the A3 grounding leg (per-reading
real-constituency check, the planned 0b backstop) was dropped, not built — Phase 0 showed it is the
wrong instrument: over-routed readings have real constituencies, so grounding would *confirm* them,
not flag the over-routing. No half-apparatus is left declared-but-unfed.

**What closing the gap would require:** (a) a research question that needs dominance-filtered
kernels; (b) a witnessed pile of kernel-positives to study; (c) a *declared seated* discriminator
(not a grounding check, not a manifest-text predicate — both shown insufficient), e.g. a human
curation pass or a learned dominance signal, with its selection-seat declared per the seat theorem.
Interim safety: the failure direction is bounded — an over-routed topic yields a construction *pair*
(kernel readings + the auto forced-flat control), never a silent flat-loss; over-routing costs
generation, not lost signal.

---

## GAP-10 — No gain-flow / authored-receipt surface: the engine cannot tell capture from non-harm — **CLOSED 2026-06-10**

**CLOSED (OQ-92 resolved, all four stages witnessed same day):** the authored receipt surface
exists end-to-end — `gain_flow`/`fixing_cost` schema fields → compiler emission with fail-loud
referential integrity → live-prompt authoring guidance → first generated batch authored 6/6 →
`narrative_ontology:constraint_captured/1` computed positively → benignity gates live
(drl_core scaffold clause, CI_Rope, pure_coordination + maxent mirror) with two-sided
controls (`audits/2026-06-10_oq92_step3_preregistration/stage_d_controls.out`). The engine
now tells capture from non-harm by AUTHORED receipt — never by role, metrics, or absence.
Original declaration kept below for provenance.

**Declared:** 2026-06-09. **Witness:** `audits/2026-06-09_capture_axis_cut_control/`
(PREREGISTRATION.md + FINDINGS.md + step1_capturer_cut_control.out). **Drives:** OQ-92 (the surface),
OQ-90 (piton/capture split that halted on it).

**The absence:** there is no authored fact for **who the extraction accrues to** (gain-flow /
receipt). The engine's χ (`constraint_indexing:extractiveness_for_agent_d/4`,
`χ = ε · sigmoid_f(d) · σ(S)`) is **extraction *from* a seat's directional view, not gain *to* a
seat.** A beneficiary-side seat has low role-`d` (`config.pl:156–160`: agenda_setter 0.12,
beneficiary 0.25) → low/negative χ → a favorable computed type — **regardless of whether the payers'
extraction actually accrues to it.** So a genuine capturer and a merely-unharmed/benignly-aligned
beneficiary seat are **computed-identical**. The one authored fact in the vicinity,
`constraint_beneficiary/2`, is not a gain-flow signal: it feeds `has_coordination_function/1`
(`narrative_ontology.pl:303`), which pushes a capturer's seat toward *scaffold* (benign coordination)
— the wrong direction for capture detection (homed as OQ-94, 2026-06-10 — opposite-direction calls
go live together at the OQ-92 step-3 classification wiring).

**Why it is absent (deliberate):** the snare/piton split was ruled to turn on *capture* (OQ-90,
operator 2026-06-10: snare = a seat captures the extraction; piton = uncaptured dead-weight), and the
no-capture test was ruled **must be computed, never authored-absence** (OQ-83 R3 / Pattern 5). The
Step-1 discriminating control (2026-06-09) established that the best computed proxy
(`has_computed_capturer/1` = beneficiary-side seat with favorable `dr_type_for_stakeholder`)
**false-positives on a mild-favorable non-capturer** (witnessed two-part: candidate-set membership
TRUE *and* cut TRUE on a non-capturer) and even fires on an uncaptured designed DMV's agenda_setter.
The proxy is upstream-broken (the d-derivation), so it is not shipped — building the piton refinement
on it would be shipping the loose proxy the review flagged.

**What closing the gap would require:** an **authored gain-flow surface** — a per-(C, seat) or
per-constraint fact recording who *receives* the extraction (not who is unharmed by it), consumed
*positively* (a seat computes as capturer because gain is authored to it), never by authored-absence.
Plausibly the same surface answers `fixing_cost`/benefit-of-fixing (OQ-90's other open term) — both
are missing authored scalars in the receipt/accrual family — but the two are **distinct scalars**
(accrual-of-gain vs cost-to-fix) and whether one surface covers both is a design ruling, not a
settled equivalence (see OQ-92; distinction-check owed before merging — build_discipline rule #2).

**Interim safety:** the capture axis is simply not computed — the engine does not *claim* to
distinguish snare from piton by capture; the `Supp ≤ 0.2` piton gate stays in place (OQ-90), and the
piton type stays where OQ-90 left it (subsumed under false_ci_rope), not falsely refined on a broken
proxy. No half-apparatus is left declared-but-unfed.

**Ruling update (2026-06-10):** OQ-92 rulings recorded — (a) build the surface, prototype-first
(OQ-93 precedent: hand-authored control stories before any schema/prompt change); (b) ONE authoring
surface, TWO distinct fields (gain-flow + fixing_cost), never one scalar or merged enum — the
distinction-check the paragraph above owed is DISCHARGED on design grounds (semantically independent
axes; the captured+cheap-fix fourth cell is live; a merged enum would re-weld provenance to value).
Tri-valued provenance design (authored-gain-to-named-seat / explicit-diffuse / absent-fails-closed),
the malformed-gain runtime default, and the step-3 generated-diffuse audit gate: see OQ-92 Rulings
block (single tracking surface). The gap closes when the surface lands; step-2 prototype:
`audits/2026-06-10_gain_flow_prototype/`.

---

## GAP-11 — No frontier-identity organ: omega-question identity is neither structural-kind nor topic

**The capability:** The omega corpus (4,430 authored `omega_variable` facts over 960 stories) needs a
way to ask *"which omegas are the same reasoning frontier?"* — the dedup operation any omega agenda
depends on, and the third of three identity axes the omega-resolver work (OQ-130) showed are distinct:
**Kind** (structural type — `constraint_signature`/orbit/`logical_fingerprint`), **Topic** (authored
subject — `cs_kernel_id`), **Frontier** (the recurring *question*). The engine has organs for the first
two and **none aimed at the frontier axis.**

**Why it is absent:** the corpus organs were built for constraints, not for the omega questions attached
to them. Frontier-identity is question-semantic; the engine machinery is structure-semantic; they are
witnessed-orthogonal (see Evidence). Name-keyed dedup overstates distinct frontiers (one frontier recurs
under many lexically-distinct names).

**Evidence (POC, `audits/2026-06-14_corpus_omega_soundness_poc/`, read-only over `testsets_haiku/`,
2026-06-14):** three recovered surfaces scored against the `cs_kernel_id` partition (331 kernels):
- **KIND ⊥ topic** — `logical_fingerprint`-shift and `gauge_orbit` both give **ARI ≈ −0.0004** vs the
  kernel partition (same-kernel→same-cluster 7.65% ≈ chance). The two KIND surfaces came out *identical*
  (orbit and fingerprint-shift encode the same structure) — one KIND organ, not two.
- **frontier ⊥ topic** — the `suppression_*structural_vs_internalized*` frontier family spans **225–264
  distinct kernels** (333 omegas / 333 stories, adjudicator-recomputed), far above its ~85 top-name
  count; ARI ≈ −0.001 vs kernel. The same family is *also* the unsound class probe 3 flagged (restating
  authored deltas, fails Irreducibility) — so the dedup organ and the soundness gate are coupled.
- Name-keyed "unique" = 3,755; semantic-dedup **lower bound = 2,901** distinct frontiers (6 families
  absorb 1,485). The lexical clustering used is a LOWER BOUND (misses synonyms).

**What closing the gap would require:** a **semantic clustering organ over the omega question text** —
embeddings are the real instrument (the POC's lexical proxy is a floor); positive control = a planted
known-duplicate omega pair must merge and a known-distinct sibling pair must stay apart (OQ-130 §C gate
(1)). Distinct from GAP-04/05/06 (those are the kernel/reading *topic* axis). The same three-axis
structure recurs at ISSUES.md scale: the missing axis *there* is **kind made computable** — §A (the
defect-pattern taxonomy) is hand-authored, not a queryable `kind` field on OQs (carried in OQ-130 as a
resolver refinement, distinct from `concerns_predicate`/topic and the per-OQ frontier).

**Status:** Deferred. Whether to *build* the organ or log-and-stop is itself an Ω_C/Ω_P the POC informs
but does not rule (OQ-130). Named here on first measurement of the orthogonality (2026-06-14).

---

## GAP-12 — The observer/committer one-seat invariant is not machine-enforced

**CLOSED 2026-06-23 (commit `fd1ee561`, OQ-15 Phase 1).** The dataflow/taint guard now exists:
`prolog/check_axis_boundary.pl` (reachability over the loaded call graph) + `python/check_axis_boundary.py`
(allowlist diff, `--selftest`), gate-wired in `scripts/gate.sh`. Both required positive controls fire
(payload-widening / non-influences seam). The invariant is machine-enforced; the census confirmed only the
sanctioned `influences` bridge + the bucket-3 `cs_kernel_id` exclusion as observer-verdict reads. The
*adoption* of v8 over v7 and the vocabulary migration remain open (OQ-135 / OQ-15 Phase 2) — but the
*enforcement* absence this gap names is filled. Witnesses: `audits/2026-06-23_oq15_crossaxis_witnesses/`.
The original absence is preserved below for provenance.

**The capability that is absent:** a guard that *enforces* cross-axis non-coupling — the property that
detection-independence (v7 Theorem 7) and the v8 seat/gauge/orientation ontology rest on: **no committer
field reaches observer computation by any path except entailment-typed payload on the single forward
`influences` → `detect_necessity_inheritance` bridge.**

**Why it is absent:** the invariant holds today by **prose + manual review only**. v7 §4.5 states it as
"the count of data bridges is the invariant"; the decision is recorded in `docs/design/two_axis_architecture_v7.md`
(OQ-14, resolved). Nothing in the engine *checks* it. A refactor that widened the `influences` payload past
entailment, or wired a (B) read-only seam to *feed* observer computation, would pass silently — the count
stays 1 and no per-edge check on `influences` sees the second path (the v8 spec §3 paths (b)/(c)).

**The mis-citation this prevents:** do **not** read v7's/v8's "(A)-bridge invariant" or "detection
independence" as something the engine enforces. It is a documented design commitment, not a checked fact.

**Distinct from GAP-04/05/06** (kernel/reading *topic* axis) and from **OQ-15** (cross-axis comparison/
mediator layer — that *surfaces* disagreement; this would *enforce* non-coupling).

**What closing the gap would require:** the v8 spec §8 priority-1 artifact — a **dataflow/taint guard** over
the whole cross-axis surface (label-agnostic reachability, not a per-edge type check) with two positive
controls (payload-injection on `influences`; (B)-seam-promotion off `influences`). Tracked: **OQ-135**.
Named here 2026-06-16 (seat/orientation audit; `docs/design/v8_seat_gauge_orientation_design_spec.md`).

---

## GAP-13 — No omega bridging for v3.4-legacy *unpaired* testsets (5-arity → 3-arity)

**The capability:** synthesize a `narrative_ontology:omega_variable/3` (3-arity) fact from a testset's
authored 5-arity `omega_variable(OID, Question, ResolutionMethod, Implications,
confidence_without_resolution(_))` protocol, so an *unpaired* testset — one that authors the 5-arity
protocol in its own module but NO matching 3-arity `narrative_ontology` sibling — still has its omegas
enumerated in reports (report_generator.pl:709 reads the 3-arity; :776-794 renders the 5-arity protocol).

**Why it is absent:** the engine has no v3.4-legacy inputs to serve. The live corpus
(`prolog/testsets/*`) is 100% *paired* (every 5-arity OID has a same-file `narrative_ontology:omega_variable/3`
sibling authored directly), so no synthesis is needed — authored omegas already reach reports without a
bridge. The only unpaired inputs are in `prolog/archives/datasets/*`, which the operator ruled OUT OF SCOPE
(no backward-compatibility, 2026-06-18).

**What was built and removed (deferred):** `data_repair:bridge_omega_variables_pure/3` attempted exactly
this synthesis but keyed its module lookup on the **bare** interval id while testsets live in module
`constraint_<id>` — so it always missed and imported zero (OQ-99's wrong-module twin; Build Discipline
Pattern 6). RETIRED 2026-06-18 (OQ-111) with a tombstone in `data_repair.pl`; the now-dead
`persist_single(omega_variable(...))` dispatch clause was removed with it. Removal was behavior-preserving
(zero-diff witnessed on three omega-authoring reports) precisely because the predicate already returned `[]`.
A secondary defect was retired with it: the /5 branch fabricated type `empirical` for a 5-arity fact that
carries no type field.

**What closing the gap would require:** re-introduce the synthesis keyed on `atom_concat(constraint_, Id,
Module)` per the OQ-99 template (`report_generator.pl:776-794`), with (a) an unpaired positive control showing
a NEW 3-arity import, (b) a paired control showing the dedup guard suppresses duplicates, and (c) a principled
type for the synthesized 3-arity fact (NOT a fabricated `empirical`). It is output-changing and lands alone.
Named here 2026-06-18 (OQ-111 close).

---

## GAP-14 — No suppression-overreach dynamic: enforcement does not erode its own grip

**The capability:** A temporal/drift law where *escalating suppression of dissent erodes the control it
seeks* — the operator's principle (2026-06-19) and the Litany's "Enforcement draws on power and exhausts
it; power revealed can be subverted." Suppression buys short-term compliance by spending three things at
once: the power reserve (enforcement cost), naturalization (you cannot look natural while visibly
suppressing), and quiescence (suppression breeds resistance). So escalation trades *cheap* grip
(uncontested, naturalized) for *expensive* grip (visible coercion that depletes itself). The engine has
no law expressing this.

**Why it is absent:** `suppression_requirement` and `resistance` exist as metrics with temporal
measurement series, but there is **no coupling** between them (grep 2026-06-19 for
`suppression→resistance` / `grip` / `backfire` / `overreach` / `exhaust` found nothing). `resistance` is
consumed only as a *static* signature/vector input and as classification caps
(`coordination_resistance_max` etc.); nothing makes accumulating suppression drive a control variable
down over time. Distinct from `mandatrophy` (`narrative_ontology.pl:157`), which is mandate-*atrophy* — a
*state* (founding problem dead ∧ extraction persists), not this overreach *feedback*.

**What was built and removed (deferred):** nothing — this was never built; it is a declared absence, not
a retired apparatus.

**What closing the gap would require (three pieces, hardest first):**
1. **A declared "grip"/control variable** — there is no metric for it. Candidate proxies, each measuring
   a different failure: `theater_ratio` rising (grip going *performative* — maintaining the show, not the
   control) or `resistance` rising. Choosing/constructing it is the hard part.
2. **An inverted-U / threshold law, not a monotone term** — *some* suppression maintains; *escalating*
   suppression backfires. A single coefficient cannot express "works then backfires"; the law needs a
   threshold past which ∂grip/∂suppression flips negative.
3. **Validation on data already present** — the corpus carries temporal series for suppression, theater,
   and extraction, so the hypothesis is testable *before* encoding: within constraints, does rising
   suppression track rising theater (grip→performative) / rising resistance over time?

The static cross-section of this dynamic is split out as ISSUES.md **OQ-152** — a *commentary-grade*
diagnostic of the **per-seat naturalization-collapse** under suppression (at what suppression level each
seat loses the cover story; the verdict is the ordering, beneficiary last). Note OQ-152 **annotates, never
re-gates the type** — re-adding a suppression maximum to the classifier would undo the 2026-06-05 de-leak
(metric/claim independence; divergence is the signal). This GAP is the full *temporal feedback*; OQ-152 is
its seat-resolved static snapshot. Named here 2026-06-19.

**Update 2026-07-24 — OQ-152 DISPOSED (answered-negative); the static side is foreclosed, the temporal
law here is NOT.** The static cross-section is unfingerprintable under the baseline calibration:
suppression is a **constraint-level gate**, not a seat dial (absent from seat χ and from the
`naturalized` clause; a step at the 0.60 snare floor), and the seat-χ ordering is **fixed by `role→d`
config** (beneficiary d≤0.30 vs high-d d≥0.69, unbridgeable by `exit_modulation`/`scope_modifier`
across the affine sigmoid; 0/158 within-constraint crossings). **Revival class — config-level only,
never a corpus authoring pass:** (i) a positional-δ calibration (which would also make `agent_power`
live for seat χ — see GAP-27), (ii) the `role→d` map itself, (iii) the `exit_modulation`/`scope_modifier`
ranges. This forecloses the *static seat-ordering* instrument; it does **not** foreclose GAP-14's
temporal grip-decay law (a different object — over-time series, not a fixed-time seat cross-section).
Evidence: `audits/2026-07-24_oq152_seat_crosssection/`.

---

## GAP-15 — gkc `--scope` is not routed through the unified generation backend (deferred one-path cleanliness)

**The capability:** A single generation backend — `generate_kernel_corpus.generate_from_manifests`
— that every manifest-capable generation route flows through, so wave-ordering, supplementary-axis
framing, and injection policy live in exactly one place. Today the c-orchestrator routes through it;
the legacy gkc `--scope` flow (`main()`, run-tagged) still runs its own path (`flatten_manifests` →
`build_batch_requests` → one batch, no waves → `process_batch_results`). Two manifest-capable
implementations coexist.

**Why it is absent (deferred, not defective):** the silent-fork *bug* — c-orchestrator dropping
recognized kernel readings — was fixed (OQ-79 mech-1); both paths now handle kernels. What remains
is pure one-path cleanliness plus an *enhancement* (gkc kernel runs would gain waves: supplementary
axes with reading-deps move to a later wave). The route is a working, currently-dormant legacy path
(c-orchestrator is the live authoring loop; the live corpus has zero kernels, so the rewire cannot
even be witnessed end-to-end now). Under the alpha→beta posture (operator ruling 2026-06-20: extract
value from the corpus we have, defer cleanliness and rebuild), this is low-value to do now and folds
into the eventual rebuild. **The injection-governance worry that once made this urgent is gone:** the
`--scope` path was confirmed *structurally injection-free* by code-read (no waves, never calls
`upstream_context`, never builds `generated_by_id`) — so the deferral costs no safety. (Full witness:
ISSUES.md OQ-82, closed 2026-06-21.)

**What was built and removed (deferred):** nothing removed — `generate_from_manifests` exists and is
the polymorphic backend; the `--scope` path was simply never re-pointed at it. No half-built
apparatus is left declared-but-unfed.

**What closing the gap would require (the focused witnessed pass OQ-82 specified):**
1. Route gkc `--scope` through `generate_from_manifests(model=GEN_MODEL, system=<gkc list>, ...)`.
2. Handle the integration seams: `emit_axiom_contradiction_facts` is emitted by *both* the unified
   backend and the `--scope` flow (after `coherence_eyeball`) → remove the duplicate or it
   double-emits; keep `kernel_grouping.json` + `coherence_eyeball` wrapped around the new call.
3. Witness it as a *behavior change*, not assumed parity: a small live kernel-seed run (1 kernel) →
   readings + flat controls + integrity sweep produced, AND the wave change visible (supplementary
   axes ordered after readings), framed as a CHANGE from kernel_v1 (which had no waves).

The *governance* sliver — that injection-suppression lives in a backend predicate rather than a
bypass-proof invariant — was tracked separately as ISSUES.md **OQ-172** and **resolved 2026-06-21**
by retiring the live injection site: the legacy `agent/orchestrator.py` and its dormant Streamlit
drivers (`app.py`/`c-app.py`) were deleted (`git rm`). OQ-172 leaves a standing tripwire — any new
or revived generation front-end must route through `generate_from_manifests`, never a hand-rolled
prompt-assembly loop that threads `claimed_type` over `downstream_of` deps. Named here 2026-06-21.

## GAP-16 — The MaxEnt signature-override boost is not seat-aware (a converted override still fires on its routed seats in the MaxEnt layer) — **CLOSED 2026-06-21**

**CLOSED (OQ-173 resolved, witnessed).** The MaxEnt boost is now the third seat-aware layer.
`apply_override_for_sig/3→/4`: `C` threaded from `apply_signature_override/3` (maxent_classifier.pl:318);
the two converted signatures skip the boost at their routed seats — `false_ci_rope` ∧
`signature_detection:fcr_routed/1`, `constructed_high_extraction` ∧ `constructed_routed/1` (reused
verbatim; `DistOut = DistIn` reverts the seat to its pre-override raw distribution). One edit covers
both serialized surfaces (classical `maxent_top_type`/`maxent_probs` and `maxent_indexed`). Witness
(`audits/2026-06-21_maxent_seat_aware/diff_witness.out`): exactly the 12 routed seats revert to raw;
**0** non-routed seats move; **1** indexed-top flip (`shinbutsu` tangled_rope→snare, the one
genuinely-manufactured verdict); **0** verdict_join changes. 21-corpus generality sweep clean
(`routed_STILL_boosted=0` everywhere; `original_v5` partial via a pre-existing `maxent_run` failure,
stash-confirmed not a regression); `validation_suite` 92/0/0; `check_stack` baseline-clean; `gate.sh`
GREEN. **Correction to the declaration below:** the boost does NOT flip the routed seats' CLASSICAL
`maxent_top` (the conditional ×3 boost never flips a classical argmax — positive control: only 2
corpus-wide flips, both non-converted UNCONDITIONAL overrides); the constructed-3 "flip to tangled_rope
at the pipeline surface" was the classical raw argmax already being tangled_rope plus the indexed-path
boost — the real manufactured flip was the indexed top, now corrected. The deferred design question
("should the MaxEnt override mechanism exist at all for converted signatures") was settled by the
operator's OQ-173 ruling in favor of the **minimal seat-aware skip** (mirror the type layer's per-seat
routing, do not delete the mechanism — it stays live for piton/inert/future callers). Original
declaration kept below for provenance.

**The capability:** A signature override that has been converted from RECLASSIFY to ROUTE/COMMENT
(OQ-138: `false_summit_mountain`, `false_ci_rope`/FCR-9, `constructed_high_extraction`/constructed-3)
should stop acting as an override at *every* layer for its routed seats — not only at the type
dispatch. Today the conversion is seat-aware at two of the three layers an override lives in: the
**type layer** (`resolve_modal_signature_conflict` / `resolve_with_perspectival_check`) and the
**override-artifact consumers** (`diagnostic_summary` `probe_signature/3` + the P1/P7
`expected_conflict_pattern` arms, via `abductive_helpers:seat_overrides/2`). The third layer — the
**MaxEnt distribution boost** (`maxent_classifier.pl` `apply_override_for_sig/3`, e.g. `false_ci_rope`
→ tangled_rope ×3 at `:331`, `constructed_high_extraction` → tangled_rope ×3 at `:341`) — is
**signature-level and still boosts the routed seats** toward their old override target. So a routed
seat whose `dr_type` reverted to scaffold/unknown can still have its MaxEnt distribution pushed toward
tangled_rope, leaving the MaxEnt layer asserting a type the rest of the engine no longer routes to.

**Why it is absent (deferred, not defective — and benign at the moment):** `apply_override_for_sig/3`
takes only the signature and the distribution, not the constraint `C`, so it cannot consult the
seat-level routed predicates (`signature_detection:fcr_routed/1`, `constructed_routed/1`) that the
other two layers use. Witnessed effect (2026-06-21): for FCR-9 the boost did not change the routed
seats' MaxEnt top (it stayed `rope`, not `tangled_rope`) — benign by luck, not design; for
constructed-3 it *did* flip the routed tangled_rope-claimed seats' `maxent_top` to `tangled_rope` at
the pipeline surface, but the headline verdict was unchanged (yellow, the boost-driven MaxEnt
disagreement subsumed by the honest base; the mountain-claimed seat is red via its severe signature
floor regardless). So the inconsistency is real but currently changes no verdict — an admitted
absence, not a defect that reads as working.

**What was built and removed:** nothing removed. The seat-aware infrastructure the fix would reuse
(`fcr_routed/1`, `constructed_routed/1`, `seat_overrides/2`, `converted_at_seat/2`) all exist and are
live for the other two layers; the MaxEnt layer was simply not threaded. No half-built apparatus is
left declared-but-unfed.

**What closing the gap would require:** thread `C` from `apply_signature_override(C, …)`
(`maxent_classifier.pl:316`, where `C` is already in scope) into `apply_override_for_sig`, and skip
the boost for routed seats (`false_ci_rope` ∧ `fcr_routed(C)`; `constructed_high_extraction` ∧
`constructed_routed(C)`). Treat it as an output-changing conversion with its own witness (removing the
boost changes `probe_maxent` → possibly base verdicts): full-pipeline before/after diff + the 5-corpus
generality sweep + decompose MaxEnt at the **pipeline surface** (the `[stack]` `maxent_top_type`
diverges from the pipeline's — see `engine_measurement_gotchas.md`). Open design question to surface
first: whether the MaxEnt override mechanism should exist *at all* for converted signatures under
route-not-reclassify (the type layer no longer manufactures, so should the MaxEnt layer?) — an
operator-seat call, not the minimal seat-aware skip. **Currently tracked as a residual under OQ-138**
(the FCR-9 / constructed-3 build records, `audits/2026-06-21_oq138_fsm_route_conversion/`); the
next-instance build prompt is drafted there. Named here 2026-06-21.

---

## GAP-17 — The committer t0 anchor (`cs_reference_frame`) is serialized but never joined (the deferred t0→t1→t2 tier is unbuilt)

**The capability:** an offline committer-drift reconciliation that joins the authored t0 reference
frame (`cs_reference_frame/2`) against the computed t1/t2 present structure — the diachronic tier that
would give a committer-axis verdict its traction (OQ-133).

**Why it is absent (deferred, not defective):** `cs_reference_frame/2` is **emitted** by the generator
and **read** at `json_report.pl:590`, where it is serialized into the committer JSON — but **no join is
computed**. The t0→t1→t2 reconciliation is blocked behind the temporal-series machinery
(OQ-109/OQ-110). So the field is inert *consumption*: present in the serialized output, consumed by no
analysis. This is the honest form of the absence — the authored anchor is preserved so the tier *can*
be built, not silently dropped.

**Adjudication (OQ-35, 2026-06-21):** RETAIN on the OQ-133 bet (the "unwired ≠ worthless" rule —
unfinished value, not cruft). **Kill condition:** when OQ-133's t0-anchor tier ships, the join either
materializes (vindicates the retain → close this gap) or is cut (then strip the `cs_reference_frame`
emission). Do not strip now: it would destroy the t0 anchor the deferred tier needs and remove it from
the serialized committer output. Cross-link: OQ-133, OQ-35. Named here 2026-06-21.

---

## GAP-18 — The `mandatrophy_gap` analytical surface is a dangling consumer (its gate cannot fire on the live corpus)

**The capability:** the mandatrophy `delta_chi` gap analysis — `report_generator.pl:476
format_mandatrophy_gap/3`, scraped by `enhanced_report.py:407 extract_mandatrophy_gap` into
`sidecar["mandatrophy_gap"]` — reports the extraction gap between the powerless and institutional
power positions for constraints where those positions disagree on type.

**Why it is absent (dangling consumer, Pattern 1):** the producer's gate
(`mandatrophy_only_report/1`) requires `constraint_indexing:constraint_classification/3` to hold for
*both* a powerless and an institutional context with differing types. On the live corpus
`constraint_classification/3` holds **0 powerless facts** (1 fact total, a demo constraint) — so the
gate cannot fire and the surface produces **0** `MANDATROPHY GAP` lines (witnessed full-corpus and via
the real `run_scenario` path, 2026-06-21). The consumer `extract_mandatrophy_gap` therefore always
returns `None`. The producer is independent of the (separately dead) hardcoded `is_mandatrophy_resolved/1`
facts (OQ-35 row 1) — this gap is about the *gap surface*, not those facts.

**What closing the gap would require:** establish whether `constraint_classification/3` is meant to be
populated post-reset (it appears to be a legacy stored-classification predicate the live pipeline no
longer feeds — the engine classifies via `dr_type/3`). Either wire the gap surface onto the live
classification path (`dr_type/3` at powerless/institutional contexts), or retire the
`format_mandatrophy_gap`/`extract_mandatrophy_gap` pair. Surfaced by OQ-35; tracked there. Named here
2026-06-21.

---

## GAP-19 — `maxent_boundary_analysis/3` is built but unwired (the per-constraint nearest-edge fragility view has no consumer)

**The capability:** `maxent_classifier:maxent_boundary_analysis(C, Context, Analysis)`
(`maxent_classifier.pl:544`) returns, per constraint, an `msort`ed list of
`Distance-Boundary` pairs over the 7 classification thresholds (ε/suppression/χ —
`threshold_boundary/5`). It is the **per-constraint dual** of the live per-boundary
"Threshold Proximity Analysis" report section: a fragility/robustness profile —
"for *this* constraint, which classification edge is it nearest to, and how near"
(a small metric perturbation away from a type flip = fragile; far = robust).

**Why it is absent (dangling producer, Pattern 1 — unwired ≠ worthless):** the predicate
has **zero callers** anywhere in the codebase (full grep, 2026-06-23). Its underlying
`maxent_threshold_proximity/4` *is* live — consumed by `maxent_report.pl:211` and
`maxent_diagnostic.pl:395` — but both consume it in the **per-boundary** orientation
("near `snare_chi_floor`, who is borderline?"). The **per-constraint** orientation
(`boundary_analysis`) is produced by no live subsystem. Adjudicated **unfinished value, not
cruft** (the three-part test: yields a per-constraint nearest-edge view; not a duplicate of
the per-boundary report; interpretable as a fragility score). Witnessed in
`audits/2026-06-23_oq112_round3/`.

**What closing the gap would require:** wire `boundary_analysis` into a per-constraint surface
(natural homes: `enhanced_report.py`'s per-constraint section as a "nearest classification edge
/ type-flip margin" field, or `json_report.pl` per-constraint output). Cheap — one `msort` over
data the threshold layer already computes and ships. **Hardening already landed (OQ-112 item 4,
2026-06-23):** `maxent_threshold_proximity/4` now carries a `number/1` fail-closed guard so the
`unknown` metric sentinel (introduced by the same commit at `get_constraint_metrics/4`) fails
closed instead of throwing `abs(unknown - Thresh)` — so a future wire does not inherit a crash.
Named here 2026-06-23.

---

## GAP-20 — Two cross-domain-twin producers exist; `isomorphism_engine.pl` is a loaded-but-dead fork (deletion deferred)

**The canonical capability (live):** `context_profile_mining:cross_domain_twins/3` — integrated
cross-domain structural twins, produced inside the HAC trajectory subsystem and reached via
`context_profile_report:run_trajectory_report`. This is the producer to keep (OQ-182; the subsystem
itself is being revived + validated as commentary-grade).

**The fork (loaded but non-executing, Pattern 2):** `isomorphism_engine.pl` is a *second*
cross-domain-isomorphism producer. It is **loaded** — `constraint_bridge.pl:11` and
`report_generator.pl:31` both `use_module(isomorphism_engine)`, and both are in `stack.pl` — but every
one of its 4 call sites is dead, so it never executes on any live path (full grep, 2026-06-25):
- `constraint_bridge:check_for_social_twins/2` — NOT in the `constraint_bridge` export list
  (`constraint_bridge.pl:2–5`) and never called.
- `report_generator:cross_domain_audit/0` (`report_generator.pl:1018`) — defined, never called.
- `isomorphism_report:generate_isomorphism_report/0` — `isomorphism_report.pl` is NOT in `stack.pl`
  (unwired); its `isomorphism_engine:generate_cross_domain_index/1` therefore has no live caller.

**Why this is a gap, not a defect to fix now:** the fork is harmless (it computes nothing on the live
path) but is genuine duplicate clutter — two producers for one capability, with canonicity now a
*checked fact* (this entry + KNOWN_STATE 2026-06-25) rather than a memory. **Deleting it is NOT a
one-line behavior-preserving cleanup:** it touches 2 `use_module` directives + 3 dead call sites across
3 files, so it needs its own old-vs-new diff-witness (the build-discipline witness rule
*prove before you replace*). Per operator ruling
(OQ-182 plan, Step 2) the fork is **log-only** here, not bundled onto the OQ-182 gate-flip commit.

**What closing the gap would require:** mint a dedicated output-neutral cleanup OQ that removes
`isomorphism_engine.pl` + `isomorphism_report.pl` + the 3 dead call sites, witnessed by a
`pipeline_output.json` byte-identical diff (the fork firing nowhere ⇒ removal must be a no-op).
Until then, the duplicate stays declared here so no future agent revives the dead path as if it were
the canonical twin-finder. Named here 2026-06-25.

---

## GAP-21 — No faithful full-series acceleration (the endpoint/first-3-points reductions were removed, not replaced)

**Declared:** 2026-06-25 (OQ-18 close).

**The capability:** a *series-faithful* acceleration read over a full `measurement/5` time series —
the second derivative (rate-of-change-of-rate) of an extraction/purity trajectory, computed over all
timepoints, not a fixed-window endpoint reduction. The diachronic question "is the drift speeding up
or settling?" needs every point: a three-point window cannot distinguish a genuine acceleration from
noise at the chosen sample points.

**Why it is absent:** the only implementation, `metric_drift_events:drift_acceleration/3`
(+ `compute_acceleration/2`), reduced the whole series to its **first three points**
(`Sorted = [T1-V1, T2-V2, T3-V3|_]`, comparing only the first two inter-point rates) and was
**removed 2026-06-25** (OQ-18): it had **zero callers** anywhere (witnessed), the first-3-points
reduction was a trajectory-faithfulness hazard, and the name `drift_acceleration` actively invited
the silent misuse OQ-18 exists to prevent. Deleting the misleading export removed the hazard; nothing
consumed it, so removal was behavior-preserving (`pipeline_output.json` byte-identical).

**The capability the deleted code did NOT provide (so this is a genuine gap, not a relocation):** a
full-series acceleration. The removed predicate only ever looked at three points; even its intended
read was unfaithful. The faithful version does not exist anywhere in the engine — `drift_velocity/3`
is endpoint-only too (OQ-18), and the live faithful primitive `drl_composition:linear_slope/2` is a
*first* derivative (velocity), not a second.

**What closing the gap would require:** (1) a full-series second-derivative computation (e.g. a
least-squares fit of the per-interval slopes, reusing `linear_slope/2` as the inner velocity
primitive); (2) a **consumer** that reads it (per `build_discipline.md` Pattern 1 — otherwise it
returns to being the unfed placeholder this ledger guards against); (3) most cheaply, fold it into the
faithful-velocity rebuild **OQ-184**, which already replaces the endpoint `drift_velocity` with a
least-squares slope and is the natural home for its acceleration sibling. Until a consumer wants it,
the gap stays deferred — the deletion was the honest move, not a regression.

**Status:** Deferred. Apparatus removed (OQ-18, 2026-06-25); gap recorded here; faithful rebuild
folded into OQ-184's migration list.

---

## GAP-22 — The engine surfaces per-observer classification variation but not its deciding hub

**Declared:** 2026-06-28 (OQ-22 resolved Verdict B; follow-up OQ-192).

**The absence:** classification has two independent hubs — Hub 1 (the χ-gates, power-scaled) and
Hub 2 (`effective_immutability`, observer-indexed). `pipeline_output.json` serializes the per-observer
outcome (`perspectives`, `perspective_chi`) but NOT which hub decided it. OQ-22 measured that a real
subset of constraints are "starved": their observer-χ span sits within one band of their realized
per-constraint χ→type map, so Hub 1 cannot move the type by changing observer, and the ENTIRE
cross-observer type variation is Hub-2-sourced (grid-witnessed Hub-2, every member with a grid
witness: testsets 5/109, haiku 23/960, flash 100/960, kernel_v1 49/1106;
`audits/2026-06-28_oq22_hub_starvation/`). A consumer reading per-observer type variation as two-hub /
χ-discrimination signal therefore mis-reads a Hub-2-only decision — the absence reads as a presence
(the per-observer disagreement *looks like* χ-discrimination it isn't).

**Why it is absent:** the engine was never asked to carry the bit. The two-hub structure is documented
(`drl_core.pl` two-hub comment; `docs/logic.md`) but per-classification hub-provenance is neither
computed nor serialized. No current consumer keys on the deciding hub, so nothing forced the field
into being — the standard "absence reads as presence" shape this ledger guards.

**What closing the gap would require (OQ-192):** per (constraint, context) decide whether the type
changes when χ is swept across the constraint's observed span at fixed immutability (Hub-1 contributes)
vs only when immutability changes (Hub-2-decided); serialize a `deciding_hub ∈ {hub1_chi,
hub2_immutability, both}` sibling of `perspective_chi`, and surface it in `enhanced_report`. The audit's
`oq22_grid.py` is a working prototype of the discriminator (the (observer × immutability) grid:
vanish-under-pin = Hub-2, persist-under-pin = Hub-1), O(observers × {mountain,rope}) per constraint —
cheap enough to wire into `run_pipeline` per Pattern 1 if a consumer wants it.

**Status:** Declared-acceptable absence (operator ruling, 2026-06-29; OQ-192 resolved document-only).
The add-the-field option (a) was considered and DECLINED in favour of documentation (OQ-22 note +
the corrected `drl_core.pl:205–211` two-hub comment); no current consumer keys on the hub, so the
absence is deliberate, not pending work. Recorded here so a future provenance/tracking-surface
proposal finds it. **Reopen condition (= OQ-192's):** a downstream consumer that reads per-observer
type variation AS two-hub / χ-discrimination signal revives option (a) — the prototype
(`oq22_grid.py`) and per-leg anchor counts above are ready to wire.

---

## GAP-23 — The generation schema does not author three structural-contradiction tells (the OQ-37 read-but-unauthored metrics)

**Declared:** 2026-06-30 (OQ-37 census re-disposition; evidence
`audits/2026-06-30_oq37_census_redispose/`).

**The absence:** the json→prolog compiler `python/generate_constraint_pl.py:608-635` emits a
**fixed** `constraint_metric` set (extractiveness, suppression_requirement, theater_ratio always;
accessibility_collapse + resistance for mountains; `has_sunset_clause` when flagged). Several engine
predicates *read* metric names the compiler **never emits** — authored-zero across all four corpora
(testsets/haiku/flash/kernel_v1 = 3,142 stories; positive controls resistance/extractiveness fire
everywhere; census in the audit dir). Each names an on-target extraction tell (a place where
structure could contradict the cover story) that the generation front does not yet supply. The
absence reads as a presence: the detector exists and looks live, but its `safe_metric/3` lookup
fails silently on the never-authored input, so it can never fire — measured-empty is
indistinguishable from never-asked at the read site.

**Why it is absent:** these tells were scoped out of the emit set at the generation front, not at
the read site — the confound OQ-37 names (forgot-to-wire vs deliberately-scoped-out) lives in the
compiler, and the compiler simply never grew the field. No consumer forced the field into being.

**The three deferred tells (priced; each an operator-seat liven, output-changing):**

1. **`sunset_time` — the self-supplied falsification tell.** A constraint declares "expires at year
   Y" (`has_sunset_clause`, already live and emitted), Y passes, extraction continues →
   `detect_sunset_violation` / `drift_event(sunset_violation)` (`metric_drift_events.pl:182,250`).
   `sunset_time` is the second input and is **never emitted**, so the detector cannot fire even when
   a story wants it. **Non-redundant** (probe b): `scaffold_suppression_escalating`
   (`cs_pattern_detection.pl:207`) fires on a rising suppression *series* — a metric-TREND verdict
   orthogonal to a declared-EXPIRY violation. **Price:** add `sunset_time` to the schema + compiler
   emission + generation prompt; the validator already requires `has_sunset_clause` for enforced
   scaffolds. **Positive control on liven:** the previously-dark `sunset_violation` fires on a
   constructed story (declared Y in the past, still extracting). *Highest-priority of the three.*

2. **`internalization_depth` — the manufactured-consent quadrant.** Names the structure that
   `suppression_requirement` alone cannot separate: consent that was *manufactured* (high
   internalization, low overt suppression) from consent that is *genuine*. **Two wiring breaks**
   (both confirmed): (i) the home module `psych_bridge.pl` is **never loaded** — not in `stack.pl`,
   and the `drl_core.pl:129`/`data_repair.pl:69` references are comments, not `use_module`; (ii) the
   input is never emitted. **Price:** wire `psych_bridge` into `stack.pl` + add the field to schema +
   compiler emission + prompt. **Kill-condition (run before livening):** dies if
   `suppression_requirement` alone already separates manufactured-consent from genuine-consent on a
   constructed pair — if so, log scope-out instead of wiring. *Highest cost.*

3. **`function_obsolescence` — a two-input dead detector, not a one-line repoint.**
   `detect_function_obsolescence` (`metric_drift_events.pl:170`) reads `alternatives_available`
   (never emitted) **and** `resistance_to_change` (never emitted) **and** theater_ratio. It dies at
   its FIRST goal (`safe_metric(C, alternatives_available, _)` — `safe_metric/3` has no default,
   `metric_drift_events.pl:66`). Livening needs **both** unauthored inputs supplied. **A
   `resistance_to_change → resistance` repoint was considered and DECLINED (OQ-64 morphology trap):**
   `resistance` is the NL/coercion-grid metric (`grid_first_contact_gate.py:48`; mountain-signature
   feature), a *distinct referent* from drift-domain `resistance_to_change` (resistance-to-abolition)
   — the shared stem is not a shared meaning. The repoint buys zero behavior (detector dead at the
   prior goal) while baking a latent wrong-metric identification that would activate the moment
   `alternatives_available` is livened. **Price:** add `alternatives_available` + `resistance_to_change`
   to schema/compiler/prompt as a unit. *Lower priority than `sunset_time`.*

**Not gaps (recorded so the next reader doesn't re-open them):**
- **`inevitability` (scalar) — superseded, not deferred.** Its sole consumer (`constraint_status/3`
  `binding_limit`) was already removed (D2 strip, `constraint_bridge.pl:20-25`); the *capability*
  (detect "there is no alternative" inevitability fraud) is carried **structurally** by
  `false_natural_law` (`signature_detection.pl:1018` documents exactly this; gate
  `claimed_natural + boltzmann_compliant(non_compliant)`, `:1040`). The scalar metric is off the FNL
  path and unneeded. Reviving a scalar "inevitable" authoring cue that *feeds FNL confidence* would
  be a fresh, low-priority capability — not a re-instatement of the removed read.
- **`accumulation_speed` — OQ-38 dead-code, not OQ-37.** Sole read `utils.pl:211` sits inside
  `safe_get_profile_components/2` which has **zero callers** — a dead orphan helper, routed to the
  OQ-38 clause-level orphan pass (do not blind-strip; false-orphan discipline).

**Status:** Declared-deferred absences (operator-seat livens, staged post-rebuild per the
"extract value from the corpus we have before any rebuild" posture). **Reopen condition:** an
analytical product needs declared-expiry-violation (→ liven `sunset_time`) or manufactured-vs-genuine
consent separation (→ wire `internalization_depth`/`psych_bridge`). Each liven is output-changing —
land separately under manual approval, split from behavior-preserving work, with the detector's own
firing as the positive control.

---

## GAP-24 — SCOPE-time concept-slot emission not built (axiom concept labels are post-hoc only)

**Declared at the OQ-72 close (2026-07-04).** The ratified `axiom_concept/2` seat exists
(`prolog/axiom_concept_registry.pl`, baked post-hoc from ratified assignment TSVs), but the
generation schema does NOT emit a concept label per axiom at SCOPE time — the documented drop-in
shape (optional per-axiom schema field, ratification-gated, flipped on by config at rebuild) is
recorded in the OQ-72 resolution and unbuilt. Until built, every corpus tranche needs the post-hoc
PROPOSAL → ratify → bake path; an empty concept map for a new tranche means NOT-YET-RATIFIED,
never "no shared subjects." Fill condition: the corpus rebuild (beta posture ruling, CLAUDE.md
Critical Distinctions) or the OQ-75(b) parity ruling demanding SCOPE-time footing.

---

## Deferred triggers (not yet gaps)

A trigger is a capability that is **not** committed future work — it has too few real users to
justify a gap entry, which would read as planned work. It is recorded here so that if the
triggering condition is met, a cold read finds the prior reasoning rather than re-deriving it.
Promote a trigger to a full GAP-NN only when its condition fires.

### TRIGGER — Acknowledgment-authority as a depletable stock (self-consuming standing)

**Surfaced 2026-07-18 (fiction stress-test — the `blog/2026-07/` *Hearts of Glass* review; not a
corpus case).** The orientation / `cs_*` axis models **standing** — who may acknowledge drift or
ratify a reading — as a **stable positional property**: a party has standing or does not, and
exercising it *expresses* it (the `cs_drift` acknowledged bit, the `cs_drift_ack_witness`
confrontation-path, seat-theorem Corollary 3 honor-vs-reabsorb). There is no state for standing
that is a **stock exercise depletes** — a principal the system defers to *because* its authority is
latent and unreachable, whose act of exercising it converts it from principal to witness (the
authority the system orbits becomes a party it cross-examines; exercising the standing destroys it).
Its **terminal** form is `acknowledgment_collapse` — the authority that can no longer tell its faith
from the perfect performance of its faith (OQ-227): the standing is not refused but *spent*.
The nearest live surface is the **inverse**: dormant-container reactivation (Meiji, the
Estates-General) *credits* latent standing by switching it on; self-consuming standing would *debit*
live standing by using it. **Declined, not gapped:** zero live-corpus instances — the only case is a
fiction thought-experiment (a charter that defers to sleepers who lose ultimate standing the moment
they are woken to exercise it). A standing-as-stock state variable + a debit-on-exercise transition
+ a consumer, justified by no corpus data, would read as planned work (the same reasoning that keeps
the 1:N reading layer below a trigger).

**Promotion condition:** a live-corpus kernel whose acknowledgment authority is genuinely *consumed*
by exercise — a ratifying party that loses standing by ratifying — not merely activated (the
reactivation inverse) or transferred. When that fires, promote to a GAP-NN. Until then the property
model of standing (`cs_drift_engine.pl`, seat-theorem Cor 3) is correct for every case the corpus
actually contains.

**Cross-ref (OQ-227 Leg C2, 2026-07-24):** `acknowledgment_collapse` — surfaced in OQ-227 alongside
`sealed_closure` — is routed **here** as the standing-axis terminal of this trigger, **never a
`cs_terminal_attractor/4` commitment terminal** (that table is the commitment axis; standing is a
distinct axis). OQ-227's surviving-referent precondition on the commitment terminals is now header-
documented and change-detected by `tests/test_cs_drift_engine.pl terminal_set_pinned` (a tripwire).
This trigger's "Declined, not gapped" status is unchanged.

### TRIGGER — 1:N reading-object layer (one reading covers N constraints)

**Deferred per OQ-04 (2026-06-23, design-cut ruling).** A predicate (`cs_reading_covers/2` and
`cs_reading_enumeration_status/2`) that would let a single reading own N constraints, instead of
the current 1:1 reading-to-constraint schema. **Declined, not gapped:** currently N=1 — the only
case is the archived cyclopean-point kernel (`prolog/archives/datasets/kernel_test/`), dropped
from the live corpus by the 2026-06-05 reset. A 1:N predicate justified by one archived case would
be woven through four surfaces (ontology decls, generator emission, registry validation, report
rendering) that cannot be cheaply un-shipped; 1:1→1:N is a clean promotion when a *second* case
appears, but un-weaving schema is not. The existing `cs_kernel_id/2` + `cs_reading_relation/3`
apparatus already expresses "N constraints of one kernel" as sibling readings.

**Promotion condition:** a SECOND kernel (live corpus, not archive) genuinely needs one reading to
own multiple constraints. When that fires, promote to a GAP-NN. Until then this stays a trigger.
The one-reading-vs-three *ontology* question (whether the cyclopean constraints are one reading or
three) is left open by OQ-04 and is orthogonal to this schema deferral.

---

## GAP-25 — No external calibration for cross-model corpus differences (the marginals are dispositional fingerprints, not capability scores)

**Declared:** 2026-07-21. **Witness:** `audits/2026-07-20_five_leg_twin_comparison/` (WRITEUP +
DEEPER_CUTS + BATTERY_WRITEUP + PARITY_WRITEUP). **Drives:** OQ-228.

**The absence:** the engine can fingerprint how models differ in *authored structure* (the five-leg
twin corpora — haiku/flash/sonnet/kimi over the shared seed pool — yield per-model marginals: H¹-band
distribution, type mix, perspective-pattern diversity, signature mix). It has **no anchor that
licenses reading those differences as model *quality*.** "Model X authors thinner structure than
model Y" is a real, reproducible measurement; "model X is *weaker*" is a different claim the corpus
cannot make on its own.

**Why the upgrade is unlicensed (witnessed dissociation, not just caution):** the constraint-story
corpus measures **unprompted authoring disposition** — what a model reaches for across 1005 stories
with no instruction to differentiate (kimi-k2.6 → ~2 perspective molds → 63% H¹ band-3, the
"thinnest" leg). The 2026-07-20 stance battery + thinking-parity re-run measured **elicited
capability** — the same model asked once to differentiate/critique — and it **dissociated cleanly**:
kimi is top-of-set there (a sharp Critic reaching for non-stock analytical lenses), while
gemini-flash stays thin *even at max reasoning*. So low structural differentiation is a *disposition*,
not a capability ceiling, and the two do not co-vary. A cross-model corpus difference is therefore a
**projective fingerprint** (like a Rorschach read — see `docs/profiling/`), not a benchmark score.

**What closing the gap would require:** an external calibration axis — e.g. correlating the
DR-derived "differentiation" with an independent capability measure or human ratings of the same
stories, or a matched-regime design (see OQ-228's thinking-parity confound) — that gives
"differentiation" an *up*. Absent that, the marginals are cited as dispositional/regime-bound
descriptive stats only (the OQ-26/OQ-70 rule, applied to the model axis).

**Interim discipline:** report cross-model corpus differences as fingerprints, never as a
model-quality ranking. The five-leg writeups already carry this caveat; OQ-228 tracks the regime
confound that must also be closed before any capability reading. Related: OQ-228 (regime confound +
dispositional caveat), OQ-75 (twin corpora), `docs/profiling/README.md` (the two-read-out framing:
mechanical structure vs interpretive stance).

## GAP-26 — No rating/report channel in the engine (deliberate: reports are position-derived appearances)

**Declared:** 2026-07-23. **Witness:** `audits/2026-07-23_oq232_falsifier_redesign/`
(`discrimination_probe.py` models the channel standalone). **Drives:** OQ-234.

**The absence:** the engine computes experienced extraction (χ per position) and models
`suppression`/`accessibility_collapse` as constraint-level authored scalars, but carries NO
χ→rating link — no model of what a rater at a position would *report*. The OQ-232 falsifier
probe needed one and built it as a declared, invented modeling assumption in a standalone
script (two link forms, linear and threshold, sensitivity-tested), deliberately outside the
engine.

**Why deliberate, not unbuilt:** v8 §3.2 treats every rating as a position-indexed appearance;
wiring a testimony/report channel into the engine would smuggle reports into the χ computation
— exactly the evidentiary-bridge inversion OQ-232/OQ-234 exist to prevent (a rating counts as
measurement only when perturbation controls cover every channel adaptation can index). The
probe also showed the invented link is load-bearing (the magnitude criterion is licensed only
under an approximately linear channel and breaks under a saturating one), so any engine-side
link would be a strong empirical commitment wearing an infrastructure costume.

**Interim discipline:** a future instantiation of the OQ-232 falsifier arms, or the OQ-234
structural-observable question, may need a report model — build it beside the engine (the
probe is the template: declared link, ≥2 forms, criterion-level assertions), never into it.
Promoting a report channel to engine surface is an operator ruling, not a wiring task.
Related: OQ-232 (resolved), OQ-234 (the class), v8 §9.5 item 3 (the evidentiary bridge).

---

## GAP-27 — A stakeholder's authored `agent_power` is inert for its seat χ under the baseline δ=0

**The absence:** In the per-seat (stakeholder) path, a seat's authored `agent_power` atom does **not**
enter its χ. Seat χ is `constraint_indexing:extractiveness_for_agent_d/4` = `ε · f(D_eff) · σ(scope)`,
where `D_eff = clamp(D + Δ)`, `D` comes from `role→d` + `exit_modulation` (not power), and
`Δ = resolve_displacement(Power, Δ)`. Under the live baseline (`cognitive_displacement = 0.0`, profile
`uniform`; and every `positional_displacement = 0.0`) **Δ = 0 regardless of Power**, so `agent_power`
drops out of seat χ entirely — only role, exit, and scope move it.

**Why it reads as a footgun, not just a fact:** the seat path *accepts* an authored power atom and
threads it through a context term, so a consumer that keys on stakeholder power expecting seat-χ
modulation is a **silent no-op** (success-shaped inertness, Build-Discipline Pattern 6). Distinct from
the canonical-context path (`dr_type/3` via `context→d`), where power *does* set d.

**Witness (2026-07-24):** identical χ = 0.442 for `power=powerless` vs `power=institutional` at fixed
D=0.50, scope=national (`audits/2026-07-24_oq152_seat_crosssection/verify.pl`, control (c)).

**What closing it would require:** a non-zero displacement calibration — `cognitive_displacement ≠ 0`
(uniform, shifts all seats equally, preserves ordering) or a `positional_displacement` per-power-atom
profile (would make the authored power atom live for seat χ, and could reorder seats). This is a
**config/calibration** decision, not a per-story authored field. Surfaced by the OQ-152 leg; it is the
reason the OQ-152 static cross-section is config-fixed (GAP-14 update, 2026-07-24).

**Status:** Declared absence (baseline calibration). Not a defect to fix on sight — δ=0 is the deliberate
"no perceptual bias" baseline; the gap is that consumers must not assume seat-power modulation without a
δ calibration in place.

---

## GAP-28 — The five-condition husk annotation is not built: the husk conjunction is empty and condition-5 independence is untestable on this corpus

**The capability:** A commentary-grade annotation firing when all five husk conditions co-occur —
(1) kernel, (2) reading layer, (3) naturalization, (4) dead founding-problem/moving world, (5) frozen
update-authority. OQ-153 built condition 5 as the `update_authority` field (the only one lacking an
authored surface) and validated it, but the annotation itself is deferred.

**Why it is absent (evidence, not preference):** two independent findings (OQ-153 step 3,
`audits/2026-07-24_oq153_step3_blind_pass/`):
1. **`dead ∧ frozen = 0/8`** in a sample enriched for both halves — the conjunction the annotation
   keys on is empty. A *mechanism* for `husk_signature_read.py`'s synchronic K=0.
2. **Condition-5 independence from conditions 2+3 is UNTESTED** — the corpus cannot populate non-canon
   `frozen` (a live kernel with foreclosed amendment, outside closed canon); a valid shape test could
   not be assembled (3/4 attempted items failed to instantiate the shape). NOT disproven as a proxy —
   untested.

**What closing the gap would require (REOPENING CONDITION — documentation, NOT a trigger):** a corpus
containing **≥3 live-kernel-foreclosed-amendment instances outside closed canon**. On such a corpus:
(a) the shape test becomes valid (condition-5 independence testable); (b) the husk conjunction may be
non-empty. **This condition is UNFIREABLE as a monitor** — it is only evaluable by authoring
`update_authority` on candidates, which the field's dormancy disposition means nobody will do; so it
is checkable only by someone already doing the work it would prompt (same self-referential dead-end as
`sealed_closure`, OQ-227). It is therefore documentation for a REVIVER, not a monitored trigger — do
not read GAP-28 as watched. **The occasion that would actually surface it:** a corpus-expansion /
generation-scope decision that adds live standards-body or entrenched-clause material — a scoping
decision, not a query. Until such an expansion, the annotation would fire on nothing and gain no
*established*-independent signal.

**Status:** Declared absence. The `update_authority` field (condition 5) is BUILT and validated
(`narrative_ontology.pl` + `data_validation.pl`); the annotation over all five is deferred to a corpus
that meets the reopening condition. A reviver must NOT read the deferral as "condition 5 is a proxy"
(that is untested) nor rebuild it on the current corpus (the conjunction is empty). Related: OQ-153
(resolved), `husk_signature_read.py` (K=0), `cs_pattern_detection.pl:336-351` (the naturalized∧forecloses
coupling that already computes conditions 2+3).

---

## GAP-29 — No terminal-severity cell above `snare`: the "no-exit corner" (ε≈1 ∧ χ≈1) is inexpressible in the type lattice

**Declared:** 2026-07-25 (OQ-67 close, the retirement of `drl_audit_core.pl`).

**The absent capability:** a *terminal* classification cell sitting above `snare` — the corner where
extraction and power-scaled extraction are both maximal and the agent has no remaining move. The type
lattice cannot express it because **`snare` is unbounded above**: `drl_core.pl:389-398`
(`classify_from_metrics/6`) gates `snare` on three *floors* (`snare_chi_floor`, `snare_epsilon_floor`,
`snare_suppression_floor`) and no ceiling, so a constraint at ε = 0.99, χ = 1.4 and one at ε = 0.70,
χ = 0.70 land in the same cell with no distinguishing token. Nothing downstream can say "this one is
worse in kind, not merely in degree."

**Why it is named here rather than silently dropped:** the retired `drl_audit_core.pl` carried the
only detector in the tree that attempted this cell — `ontological_fraud_check(_, z_alert)`, the one
export of the four that had no live duplicate (`structural_signature/3` duplicates
`omega1_audit:determine_primary_gate/11`; `fm_alert` duplicates `drl_core:type_1_false_summit`;
`omega_risk/4` duplicates `drl_core` + `transition_paths`). The module is deleted on *value* grounds —
unreachable **and** thrice-duplicated — but this one product went with it, so it is declared, not
assumed re-derivable. The retired code verbatim (`drl_audit_core.pl:65-71` at commit `ee025a0c`):

```prolog
% Zero Degrees of Freedom (Z): The absolute trap
% Logic: epsilon -> 1.0 AND chi -> 1.0
ontological_fraud_check(Data, z_alert) :-
    member(suppression_score(Epsilon), Data),
    Epsilon >= 0.90,
    member(base_extractiveness(X_base), Data),
    X_base >= 0.85, !.
```

**Why it must NOT simply be revived as written** (three independent defects, each disqualifying):
1. **The comment does not describe the code.** `:66` claims "ε → 1.0 AND χ → 1.0"; `:68-71` reads
   `suppression_score ≥ 0.90 ∧ base_extractiveness ≥ 0.85`. There is **no χ term at all** — it is an
   ε∧suppression conjunction wearing a χ label.
2. **It binds the wrong metric to ε.** It binds `suppression_score` to a variable *named* `Epsilon`.
   In the canonical engine these are distinct arguments of `classify_from_metrics/6` (`BaseEps` vs
   `Supp`); ε is `base_extractiveness`. (The same mis-binding is why the sibling `fm_alert` clause
   diverged from `logic.md:749` Rule FM, which specifies ε and additionally requires the
   `∃I(¬■C[I])` leg that the retired clause dropped entirely.)
3. **Both thresholds are bare literals outside governance.** `0.90` and `0.85` appear nowhere in
   `config.pl` / `config_schema.pl`, so they were unperturbable by the sensitivity sweeps and
   unvalidated by `config_validation.pl`.

**Spec status — there is no rule to implement against.** No "Rule Z / Zero Degrees of Freedom" exists
in `logic.md`, `logic_extensions.md`, `logic_index.md`, or `logic_thresholds.md`. **`logic.md:3267`'s
"Rule Z" is the *Piton* rule** (`χ ≤ 0.25 ∧ ε > 0.10 ∧ Theater ≥ 0.70`) — a letter collision, not this
capability. A reviver must not read that rule as the spec for this cell.

**What closing the gap would require:** (a) a spec decision — is the no-exit corner a new *type* (a
lattice cell above snare, which changes the priority cascade), or a *severity annotation* on snare
(commentary-grade, no cascade change)? That is a seat-declaration ruling, not an evidence question.
(b) Whichever is chosen, thresholds authored into `config.pl` + `config_schema.pl`, and a χ term that
actually reads χ. Until then the corner stays declared-absent so that an empty
`ontological_fraud_check` result is never mistaken for "no constraint is terminal." Related: OQ-67
(resolved), `drl_core.pl:389-398` (the unbounded snare cell), GAP-20 (the sibling
retire-a-duplicate-fork entry).

## GAP-30 — No externally-grounded ε: ε is author-assigned by construction, and the design declines the "real corpus, external ε" pipeline (operator ruling, 2026-07-25)

**Declared:** 2026-07-25 (operator ruling, relayed in-session; question raised by an
independent no-context review of `docs/deferential_realism_paper_v8.md` the same day).

**The absent capability:** an ε-grounding pipeline against real documents — ε (and by
extension every downstream measurement: the six-type classification, H¹ structure, drift
terminals) sourced from externally-measured properties of real legal/doctrinal/institutional
text rather than assigned by the authoring model during story generation. The reviewer's
framing: until ε is externally sourced, the apparatus validates internal consistency of a
formal model over a synthetic corpus, and the loaded vocabulary (extraction, coordination,
snare) is earned only at the level of "a formal system with these properties exists," not
"this tells us something about actual labor law." The v8 §9.2 ε confession and §9.4
citation prohibitions are the existing fence around exactly this gap.

**The ruling and its grounds (operator, 2026-07-25):** ruled AGAINST pursuing, on two
grounds — (a) no known method: there is no procedure on the table that takes a real
document and returns an ε with better provenance than an author's judgment; (b) no
established advantage: it is not clear why a human's (or any external assigner's) ε would
be epistemically better if you could get one. Ground (b) has a seat-theorem shape worth
recording: under `docs/seat-theorem-v1.md` there is no seat-free ε — "external grounding"
relocates the assigning seat (to a rater, a rubric, a market), it does not remove it; the
honest form of the current design is that the authoring seat is *declared* (model
provenance is recorded per story), which is the only status the law permits any ε to have.

**Standing falsifier (attached per the ruling; both conjuncts required to fire):** the
ruling reopens if someone exhibits (i) a CONCRETE, runnable grounding procedure — named
document class in, ε value out, with the assigning seat and rubric declared — not a
proposal or a vocabulary ("crowdsource it," "use a rater panel" unspecified do not fire);
AND (ii) evidence that the resulting ε carries epistemic value the author-assigned ε lacks
— e.g. inter-rater convergence on held-out documents, or predictive validity against an
out-of-corpus outcome the author-assigned ε fails to predict. Difference alone does not
fire it: an external ε that merely *disagrees* with the authored ε is two seats
disagreeing, which the framework already prices. A fired falsifier routes to a new OQ; the
gap is then a build question, not a ruling question.

**Until then:** the fence holds as designed — synthetic-corpus findings are cited per the
v8 §9.4 prohibitions, model provenance stays first-class, and a fresh instance proposing
"ground ε in real documents" should be pointed here rather than treated as surfacing a
novel gap. Related: v8 §9.2/§9.4, `docs/seat-theorem-v1.md` §2 (P2 defense) and §6.2 (the
real-but-local warrant shape this ruling shares).

## GAP-31 — No cross-story seat identity: stakeholder seats are story-local, so no family-level seat presheaf exists to section

**Declared:** 2026-08-08 (OQ-261 close + post-hoc symmetric read,
`audits/2026-08-07_oq261_forced_gluing/`).

**The absent capability:** a seat that persists ACROSS the stories of a kernel family —
e.g. a per-round performance object (judge/ballot record) authored once and referenced by
multiple readings, or any stakeholder identity shared between sibling stories. Every seat
is authored per-story and typed per-story (`dr_type_for_stakeholder` is position-indexed
within its own story; the CS identity model keys instances by per-story UID), so a seat
name recurring across two readings is two seats, not one.

**Why declared rather than a defect:** the absence is what the OQ-261 experiment ran
into, not a broken wire. Its measurable consequence (witnessed): ANY sub-vector pooled
across a family's stories inherits each story's typing, so pooled seat-frame H¹ reduces
to the family's bloc structure — in the fiat family every pooled set (performance /
topic-community / all-agent) carried H¹ = (#rope)·(#scaffold) exactly, and 15/16
`real_closure` families "obstruct" on the pooled agent-seat read. A pooled-across-story
H¹ is therefore not evidence about the seat set; treating it as such is the trap
(build_discipline → *Pooled-across-story H¹ inherits story-level typing*).

**What would discharge it:** authoring cross-story objects with identity — per-round
performance records seats can share, or a seat-identity join table — which is the
substrate OQ-267 names as required before the "ballot = forcing over a second obstructed
presheaf" thesis is testable. A fresh instance proposing to "compute the family-level
stakeholder H¹" should be pointed here: the quantity exists arithmetically but has no
object under it.

## GAP-32 — No outcome surface for cost redistribution at constant terminal ("the structure holds, and the cost changes")

**Declared:** 2026-08-09 (world-bible review; `agent/narrative_transform/THE-GRAIN-world-bible.md`
Module 11 §4, "a seventh ending the table does not produce").

**The absent capability:** an outcome descriptor for a commitment system whose institutional
fate is unchanged — same terminal attractor, same drift state — while the *per-position cost
distribution* under it changes. The bible names the shape exactly: nothing repaired, the
arrangement outlasts everyone, and it takes less out of the people in one position than it did
before. The attractor table's codomain is institution-fates only (`stable_pattern` / `husk` /
`extinction` / `revival` / `repudiation` / `axiom_foreclosure`); nothing on the CS axis can
express "terminal constant, incidence redistributed." Observer-axis per-position reads (χ,
extraction, per-context type) exist synchronically per story, but nothing joins them to CS
drift as a *within-terminal trajectory of who bears the cost*.

**Why declared rather than a defect:** nothing is broken — the taxonomy was built to type
institutional fates, and cost-incidence-over-time was never in its codomain. Declared so a
fresh instance proposing "the attractor table misses an ending" is pointed here rather than
bolting a seventh terminal onto the table: it is not a terminal, it is orthogonal to the
terminal axis, and adding it as a row would corrupt the enumeration (which is gate-pinned by
`spec_enum_check.py`).

**What would discharge it:** a per-position cost/incidence series joined to the drift state.
Substrate honestly stated: positional extraction is authored synchronically per story, so a
within-terminal redistribution claim needs either snapshot-series positional data or a
cross-story comparison with seat identity — the latter runs directly into GAP-31's
no-cross-story-seat-identity wall. Any build proposal routes through the OQ-37-style "does it
earn its keep" trial on `testsets/` (the evolving-schema leg), not through a spec change first.

## GAP-33 — Edge-validity (severance/intrinsicness) annotations are audit-artifact-only: no authored in-corpus field exists

**Declared:** 2026-08-09 (OQ-262 close;
`audits/2026-08-09_oq262_coexists_severance/WRITEUP.md` → Residue).

**The absent capability:** a per-edge validity annotation surface in the corpus itself.
The OQ-262 audit produced per-pair severance/intrinsicness verdicts with mutation text
for every fiat-family `coexists_with` pair, but the annotations live in the audit
artifact `edge_audit.json` only — no schema slot, no authored fact, no generator
emission. A consumer wanting per-edge validity reads that file, fiat family only, at
its recorded altitude ("reading of the authored text under a frozen grammar," RULED
minority, `genuine` class uncalibrated).

**Why declared rather than a defect:** promotion into an authored in-corpus field is a
schema change, which routes through the test-bed posture (exercise on `testsets/`
first) and is the operator's seat — explicitly deferred at OQ-262 close, not
forgotten. A fresh instance should neither hunt the schema for the field nor propose
building it unaware the deferral was deliberate.

**What would discharge it:** an operator adoption decision, then a schema slot (e.g. a
`cs_edge_validity/4`-shaped fact or a JSON sidecar the generator emits), a re-judgment
path (the annotations survive re-judgment by a different instrument — that was the
design condition), and registration/gating per the reading-registry and spec-enum
opt-in rules (both are silent-escape shapes if skipped).

---

## GAP-34 — No role-projected obstruction gauge: the role-site H¹ is DECLINED by ruling, not unbuilt by accident

**Declared:** 2026-08-09 (OQ-151 close, operator ruling;
`audits/2026-08-09_oq151_dual_gauge/WRITEUP.md`).

**The absent capability:** an H¹/disagreement measure computed over the role-projected
site `[beneficiary, payer, excluded, observer]` (OQ-151's "six-questions gauge"). The
engine deliberately does not have one. What exists instead: the per-role SET projection
`stakeholder_seats:role_type_set/3` / `role_type_sets/2` (intra-role fracture surfaced,
no obstruction number), the seat-frame `h1_stakeholder`
(`stakeholder_obstruction/5` — the role site's refinement), and the typed empty-chair
detector `empty_chair_state/2`.

**Why declared rather than a defect (three independent grounds, OQ-151 close):** (a)
the role-gauge geometry is config-forced (role→d + exit is the only live seat dial —
GAP-27; seat-χ ordering fixed by role→d — GAP-14), so a role-H¹ would largely measure
config, not the story; (b) a role projection is a coarsening of the seat vector
`h1_stakeholder` already measures — a coarsening cannot outperform its refinement; (c)
OQ-56 recorded the reopen tripwire for exactly this gauge and no live consumer needs
it. The decline deliberately does NOT rest on the 0.245 twin-agreement (mis-keyed;
OQ-275).

**What would discharge it:** the OQ-151/OQ-56-aligned reopen condition — a live
consumer that requires per-role disagreement structure `role_type_sets/2` +
`h1_stakeholder` cannot express. Re-evaluated by a human at consumer-build time;
nothing arms it automatically. A fresh instance proposing a role-projected H¹ should
route through this entry and OQ-151's close, not build it as a missing feature.

## GAP-35 — No authored cross-corpus seed identity: matched-seed pairing exists only as a filename convention on the twin legs

**Declared:** 2026-08-10 (OQ-78 idiom close + the OQ-281 single-pair verification,
`audits/2026-08-10_oq78_idiom_close/`). **Drives:** OQ-281.

**The absent capability:** a first-class authored relation asserting *"this story instantiates the
same seed as that story,"* stable across regeneration and across corpus legs. Paired / matched-seed
designs — the only designs that hold topic and claimed_type mix fixed by construction — require it.

**What exists, and why none of it serves:**

| field | status | joins across legs? |
|---|---|---|
| `cs_kernel_id` | authored, present | **yes at KERNEL level** — 331/331 kernel ids shared across the twin legs |
| `cs_story_uid` | authored, present (960/1000) | **no, by design** — a per-story surrogate UUID; **0/956** match across twins |
| `seeded_from` | **not emitted** | n/a — 0/960 haiku, 0/1001 sonnet |
| `constraint_id` (filename) | authoring convention, not a declared identity | **de facto yes** — 957 four-way matched ids |

So the only *reading-level* pairing mechanism is **filename equality**, and the project's own standing
rule holds that **names are not identity across a regeneration boundary** (`CLAUDE.md` Critical
Distinctions; OQ-264 — kernel ids churned at reproduce-rate 1.0; `seeded_from` is explicitly
generation-time plumbing, never identity recovered backward). The field that most looks like it
should serve, `cs_story_uid`, specifically does not.

**And the kernel-level join does not rescue the general case (verified 2026-08-10):** the twin legs
share 331/331 kernel ids, but default-leg derived sonnet-4.5 (64 stories → 8 kernel ids) shares
**0** with the sonnet leg, and the archive `kernel_v2_test2` carries **0 kernel ids at all** (it
predates the CS schema). Pairing fails at *both* join levels outside the twins.

**Why declared rather than a defect:** nothing is broken. The twin legs were genuinely built by
re-authoring one seed set, and their pairing is real (spot-checked: same `constraint_id`, different
`human_readable`, same subject matter re-authored per model). The absence bites only when a design
*assumes* pairing is generally available — which is invisible until the id intersection comes back
zero, typically a session in.

**The structural consequence — the reason this earns a ledger entry.** Outside the twin legs,
**marginal-independence and paired-comparability are MUTUALLY EXCLUSIVE.** A population independent
enough to serve as a clean *marginal* known-positive is, by that same independence, **unusable for a
paired read.** Witnessed in both directions in a single pass (OQ-78): the archive (n=60) and
default-leg derived sonnet-4.5 (n=64) landing on the same ε digit at nearly the same concentration
from a **zero-story-id intersection** was the strongest evidence in the calibration pass — and that
same zero forecloses any paired within-family check between them. This is a property of how the
corpus was built, not a shortage of data, so no amount of further authoring *into existing legs*
relieves it.

**`testsets_haiku|flash|kimi|sonnet` are therefore the ONLY matched-seed structure in the project**
(957 four-way matched ids) — and the only reason OQ-78's primary paired statistic could exist at all.

**What closing the gap would require:** emit a **seed/lineage id at generation time** — a stable
token naming the seed a story was authored from, distinct from both the per-story UUID and the
kernel id, carried into every leg built from that seed pool. It is **cheap at generation time and
impossible to add afterward**: it records an authoring *event*, not a property of the artifact, so
it is not recoverable by inspection — and per the OQ-264 redraw standard it is not recoverable by
signature matching either. **This is the entry to weigh whenever the rebuild question comes up**
(`CLAUDE.md` Critical Distinctions, FIVE LIVE LEGS / beta posture): matched-seed structure is a
generation-time decision that a later session cannot reverse.

**Interim discipline:** check the **id intersection FIRST** when designing any paired probe — outside
the twin legs there is nothing to pair, and a paired design over any other population pair is dead on
arrival. Report matched-seed joins as *filename-convention* joins and say so. A new matched-seed leg
is a **generation spend, never a re-read**.

**Related:** GAP-31 (no cross-story *seat* identity — the seat-level analogue of this gap), GAP-25
(cross-model differences are dispositional fingerprints), OQ-281 (whose branch (B) costing rests on
this entry), OQ-78 (close), `CLAUDE.md` Critical Distinctions.

---

## GAP-37 — Sampling parameters are not queryable: 71% of the live corpus carries no temperature term, and the slot that would carry it is an unconsumed, delimiter-colliding string

**Declared:** 2026-08-18 (OQ-190 close, `audits/2026-08-17_oq190_blast_radius/`, from the OQ-118
Limb-3 sweep ruling). **Drives:** OQ-118 Limb 3 (closed partly as *unspendable-as-designed*),
OQ-202.

**Numbering note:** GAP-36 is claimed-but-unlanded — proposed by
`audits/2026-08-17_oq285_mode3_measurement_arm/` and not yet written here. This entry takes **37**
rather than 36 so a landed and a proposed entry never share an index. A visible gap is a checked
fact; a silent reuse is a fork (the OQ-278 numbering principle).

**The absent capability:** the ability to attribute any corpus property to a sampling parameter.
Not "hard" — **not possible from substrate** for most of the corpus, because the parameter was
never recorded per story.

**Measured (live leg `prolog/testsets/`, n=279, 2026-08-18;
`limb3_temperature_aliasing_CORRECTED.out`):**

| `sampling_params` content | stories |
|---|---|
| carries a **numeric** temperature (`0.1`, `0.2`, `1.0`) | 42 |
| carries a **symbolic** temperature (`api_default`, `default`) | 38 |
| `'unspecified'` — no sampling information at all | 173 |
| no `story_provenance` fact at all (the GAP-adjacent `*_contradictions` stratum, OQ-306) | 26 |

So **199 of 279 (71%) carry no temperature term**, and only **42 (15%) carry a numeric one.**

**Why this is a declared absence and not a defect:** nothing misreports. `story_provenance/8` says
`'unspecified'` honestly. The gap is that a whole class of Ω_E question — *"is this corpus property
attributable to a sampling parameter?"* — is unanswerable for most of the corpus, **and that fact is
not discoverable without running this count.** OQ-118's original Limb-3 framing presumed a
temperature sweep was purchasable against existing material; for 71% of the corpus there is no
baseline to sweep against. Declaring it here is what stops the next person scoping that work before
learning it.

**Second half of the gap — the slot itself is not a field, it is a bag, and it collides with its own
container's delimiter.** `story_provenance/8` arg 8 is schema-typed only as
`{"type": "string", "description": "Sampling parameters at generation (e.g. 'temperature=1.0')"}`.
In practice it holds nine distinct shapes, most of them comma-separated `k=v` bags
(`'max_tokens=16384,temperature=0.1,thinking_budget=0'`) — **inside a comma-delimited Prolog term.**

- **Consumers today: ZERO** (verified 2026-08-18). Every reader takes arg **7** (`Model`):
  `json_report.pl:1134`, `python/audits/oq136_bucket_provenance.py:70`, `run_pipeline.py:199`.
  Arg 8 is emitted (`generate_constraint_pl.py:857`), declared, and never read — **T5b
  inert-unconsumed** in OQ-190's vocabulary.
- **The hazard is armed for its first reader, and it is Pattern 4.** A naive arg-split on `,`
  truncates the value at its first comma and yields `'max_tokens=16384'` — a **well-formed,
  plausible, silently wrong** value, not an error. This is not hypothetical: **OQ-190's own first
  probe did exactly this**, dropped every temperature term in a compound value, and reported
  "temperature unrecorded in the deciding cell" when the cell records
  `temperature=api_default`. The correction reversed which branch of the sweep ruling applied.

**What closing it would require** (none of it done here): a structured sampling-parameter surface
(per-key fields or an authored map) rather than a string bag; a back-stamp for the 173
`'unspecified'` stories where the value is recoverable from generation logs, or an honest
declaration that it is not; and a parse contract for arg 8 for as long as it stays a string —
if a reader is ever written, it owes a control on a compound value.

**Do not read the OQ-118 Limb-3 close as answering the temperature question corpus-wide.** It was
answered on one 28-story cell where temperature is *constant*, plus a mechanism attribution. This
GAP is the scope of what remains unanswerable.

## GAP-38 — No domain-prior expectation checking: the authored category→signature table was retired dead in both senses

**Declared:** 2026-08-18 (OQ-296 D3 retirement, `audits/2026-08-18_oq296_consumer_honesty/`).
**Drives:** OQ-316 (the `category_of/2` disposition), OQ-317 (the GAP-08 §7 sunset).

**The absent capability:** an *expectation check* on the signature layer — a declared statement of
what signature a constraint of a given domain category OUGHT to receive, against which the engine's
actual verdict could be compared. `should_be_natural_law/1` / `expected_signature/2` /
`validate_signature/2` were an implementation of exactly that, and were retired 2026-08-18 rather
than repaired. This entry exists so the capability is a **declared absence** and not a silent one,
and so the authored content survives the code.

**BOTH deadnesses, stated — the routing_sink dark-declaration model does not cover consumer-less
code, which is why this is a gap entry and not a site comment:**

1. **0 firings.** `should_be_natural_law/1` fired **0** times on the live leg (n=279). It requires
   `expected_signature(Cat, natural_law)`, i.e. `Cat ∈ {physical_natural, formal_logic}`, and
   `category_of/2` returns `unknown_novel` on every constraint of every corpus measured
   (279/279 live, 1106/1106 kernel_v1 — see OQ-316).
2. **0 consumers.** All 17 references to the three predicates repo-wide were inside
   `domain_priors.pl` itself. Control for that sweep: the same grep shape returns 9 external hits
   for the sibling `category_of/2`, which was therefore KEPT. The trio was exported at
   `domain_priors.pl:5-7` and called nowhere.

**The authored table, preserved verbatim** (this is the content worth keeping — a considered
mapping from domain kind to expected signature, and the only place the project ever wrote one
down):

```prolog
%% expected_signature(?Category, ?Signature)
expected_signature(physical_natural,  natural_law).
expected_signature(formal_logic,      natural_law).
expected_signature(election_cycle,    constructed_constraint).
expected_signature(statutory_formal,  constructed_constraint).
expected_signature(extractive_market, constructed_constraint).
expected_signature(narrative_history, constructed_constraint).
expected_signature(unknown_novel,     ambiguous).
```

**Why the table could not work at HEAD, in three layers** (measured 2026-08-18; note the
retirement was NOT a judgement that the idea is worthless — per *Unwired ≠ worthless* — but that
this implementation could not deliver it):

- **5 of 7 rows are unreachable by construction.** `category_of/2` has only two clauses and can
  emit only `physical_natural` or `unknown_novel`. The categories `formal_logic`, `election_cycle`,
  `statutory_formal`, `extractive_market`, `narrative_history` were `domain_registry`-era values;
  that module was deleted Feb 2026 (OQ-96) and nothing replaced its classifier.
- **6 of 7 are unreachable on any authored corpus.** Adding `physical_natural`, which is reachable
  by dispatch — a planted `constraint_claim(_, natural_law | physical_law)` yields it — but whose
  claim vocabulary is authored **0 times across all five live legs and kernel_v1** (~5,311 files).
- **The one live row is the least informative one.** Every constraint routes
  `unknown_novel → ambiguous`, so the check could only ever have said "expected ambiguous".
  Worse, `constructed_constraint` — the expectation for four of the seven rows — is **not in the
  live signature vocabulary at all** (the live cascade emits `constructed_high_extraction` /
  `constructed_low_extraction`), so even a repaired `category_of/2` would have compared against an
  atom the engine no longer produces.

**What would have to exist to revive it:** a working domain classifier (OQ-316 — either restore one
or author the claim vocabulary), AND a re-derivation of the expectation table against the *current*
signature vocabulary. Reviving the predicates alone would reproduce the dead state.

**Do not re-mint a similar surface without reading this entry first.** The failure here was not the
idea but a table that outlived both its classifier and its target vocabulary while continuing to
export cleanly.

## GAP-39 — No framing carriage in the corpus schema: a constraint story cannot state the selection rule its reading was formed under

**Declared:** 2026-08-19 (OQ-284 ruling, operator, second-instance reviewed — the author-vs-declare
choice was made, not defaulted).
**Drives:** nothing — that is the point of the declaration. OQ-284 is resolved on this disposition.

**The absent capability:** an authored field on a constraint story declaring the selection rule /
framing under which its reading was produced — the corpus-side analogue of what the apparatus
already carries everywhere (the pipeline manifest convention, coverage-in-band, the OQ-60 dual
absence tokens, the staleness ladder). The nearest existing surface, `cs_reference_frame/2`, is
serialized (`json_report.pl:720`) and never joined — GAP-17, the standing demonstration of what
happens when a field is authored without a consumer.

**Why declared rather than built.** (1) **No consumer exists.** No current analysis joins or wants
this field; the schema-test-bed rule ("test whether a metric earns its keep on `testsets/`")
measures earning-its-keep against a consumer, and there is none. (2) **Self-certification at
birth:** the field would be authored by the same generation pass whose framing it purports to
declare. (3) The churn rider below makes any citable use expensive before it is meaningful.

**Revival conditions — all three, before any schema field is authored:**

- **(a) A consumer that EXISTS and currently wants the data, or a specific analysis BLOCKED on the
  absence.** Not an analysis that could be written if the field existed — proposing a consumer is
  cheap and would arrive from the same reasoning that wants the field. The bar is an existing join
  site or a named, blocked analysis.
- **(b) The self-certification problem solved STRUCTURALLY — and note why the obvious answer is
  not obviously available.** The declaring party must not be the authoring pass. A sidecar will be
  proposed; note that OQ-71's lineage sidecar worked precisely because it never reached the
  generator — which is also why it could carry nothing the generator knew. A framing sidecar has
  the INVERSE requirement: it must describe the selection rule the generation pass operated under,
  so something outside that pass has to know the rule independently. Whether such an independent
  knower exists is the actual open question behind this condition — start there, not at "write a
  sidecar."
- **(c) The OQ-264 churn rider honored as written:** per-reading redraw identity is unstable with
  NO global floor (2/6–3/6 at 340K arsenal; file-structure-dependent), so an authored framing
  value is citable only under the k=3-unanimous standard, and reporting may never be finer than
  the denominator's own churn.

**The churn rider is GENERAL, not framing-specific — read this even if you are not reviving this
gap.** Condition (c) binds ANY authored per-reading field whose value someone would want to cite,
not just a framing field. It is recorded here because this is where the ruling happened, but its
scope is every future per-reading schema addition. Candidate for the OQ-295 STANDING section when
that ships (a general rule with no natural `Files:` line).
