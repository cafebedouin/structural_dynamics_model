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

**Status:** Deferred. Named here on first availability of a corpus that exercises it (2026-06-02).

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
`drift_events`) but no predicate that takes a kernel's reading-set and returns {ambiguity, drift,
structure}.

**Why it is absent:** Same root — no kernel/reading corpus to test a disagreement router against until
2026-06-02. The trifurcation is the operational core of the kernel/reading engine and is the natural
consumer of GAP-05's reading-axis gluing test (H¹≠0 with each reading internally coherent ⇒ Type C;
internal incoherence ⇒ Type B; same-seat criterion drift ⇒ Type A).

**What closing the gap would require:** (1) GAP-05's reading-axis obstruction as input; (2) the
three-stage diagnostic of `debugging_philosophy.md` §6 implemented over a kernel's readings, mapping
existing diagnostics onto A/B/C; (3) a consumer that reads the verdict. Open question: **OQ-55**.

**Status:** Deferred. Named on first availability of the corpus that exercises it (2026-06-02).

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
repair the 9 delimiter typos *first* so the frontier set is clean (else a typo resolves to / would
regenerate an existing node -- straight into OQ-59); (4) decide whether the resolver keys on five
structural dims or six -- `shift` is a different (constraint x fixed-context) space, and `coupling` is
high-cardinality and possibly over-resolved (239/772), so it may be excluded or quantized. Open
questions: **OQ-58**, **OQ-59**; related untyped-edge gap: **GAP-01**.

**Status:** Deferred; bounded-attractor basis analyzed 2026-06-02 (scripts above, all re-runnable on a
fingerprint dump). **Drift framing held deliberately:** the post-saturation corpus is a *bounded state
set*; whether change over it has memory (Markov vs higher-order) is an **open measurement** pending
`fingerprint_drift` velocity/acceleration -- not yet asserted.
