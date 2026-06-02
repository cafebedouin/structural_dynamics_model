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
