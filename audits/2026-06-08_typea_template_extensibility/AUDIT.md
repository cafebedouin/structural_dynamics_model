# Pre-Rebuild Audit — Template Extensibility & Type A (Temporal) Authoring

**Date:** 2026-06-08 · **Branch:** `kernel-first-router` · **Corpus:** live rebuild, 100 testsets
(witnessed: `BASELINE pass=100 fail=0`) · **Mode:** READ-ONLY. `git diff --stat` empty; all schema
experiments on `/tmp/dr_scratch/`, removed after. Every claim tagged **[W]** witnessed (with the
pasted probe) or **[A]** assumed/inferred (with what would graduate it).

**Governing operator constraint (received mid-audit, stated three times, load-bearing):**
**CS and DR are SEPARATE HUBS, not to be joined** — and `cs_structure` is the CS hub, which v7
"should outline why it's independent." This is not a style note; it is *the central commitment of
v7*, with a theorem behind it. Grounding in `docs/deferential_realism_paper_v7.md` **[W]**:

- §1: "The central new commitment — and it is a commitment, stated as such — is that the two axes
  are *not* unified: their mechanisms have incompatible mathematical characters … v7 refuses this
  fold, and the refusal is not a preference."
- **Theorem 7 (Detection Independence)**, §3: observer-coherence (H¹) and committer-foreclosure are
  "computed from disjoint inputs … neither's computation can see the other's inputs." Their
  independence is *structural*, "not that they happen not to correlate."
- §4.5: the committer modules "run *beside* the observer-axis pipeline, not inside it."

**⚠ The v7 bridge paragraph is STALE as a census — corrected against the substrate (operator
flagged; do not cite the paper's count).** v7 §4.5 claims "**exactly one intentional bridge between
the axes: `influences` → `detect_necessity_inheritance` … and nothing else**," with
`cs_drift_mismatch` as the sole cross-axis diagnostic. The witnessed coupling surface is larger.
Two **distinct** categories must be kept apart (the paper conflates them under "bridge"):

- **(A) DATA bridge — one axis's output feeds the other's *computation/model*.** Still exactly
  **one** [W]: `cs_reading_relation(_,_,influences)` → `detect_necessity_inheritance`
  (`drl_composition.pl:141`). `forecloses` is *excluded by decision* (OQ-24,
  `drl_purity_network.pl:224–235`: gradient-orthogonal, would invert the monotone-flow proof). The
  intra-kernel `cs_kernel_id` read (`drl_purity_network.pl:97`) is a **severing filter**, not a
  feed. The recent stakeholder migration added **no** new data bridge. *This is the invariant that
  actually protects the hubs, and it holds.*
- **(B) READ-ONLY seam diagnostics — a module reads the other axis's *output* to surface
  disagreement; nothing feeds back.** The paper names only `cs_drift_mismatch`; the substrate has
  **at least three** [W]: `cs_drift_mismatch`; `cs_kernel_registry` cross-reading divergence →
  `classify_at_time` (`:66–67`, Time=0); `cs_pattern_detection` grounding-vs-structure →
  `signature_detection:constraint_signature` + `constraint_metric` (`:262,296,310,326,378`). These
  read DR *output* to compare against CS authored grounding; **no DR computation reads their
  result** ([A], graduation: grep DR modules for `cs_grounding_contradiction`/`cross_reading_*`),
  so they do not violate separation — but the paper's "exactly one … and nothing else" is not an
  accurate enumeration of the seam.
- Code corroboration of the invariant **[W]**: `cs_kernel_registry.pl:78–84` ("OBSERVER-BLIND BY
  CONSTRUCTION … never classify_at_time, χ, or live_index" *for the committer-edge cohomology*);
  `cs_drift_engine`/`cs_axiom_engine` read no DR predicate (grep returns only comments).

**The operative rule for Type A (substrate, not paper): no new (A)-category data bridge.** The DR
time-varying-d computation must read only observer-axis inputs; CS computation must not take d/χ as
input. A new (B)-category read-only seam diagnostic comparing observer-drift vs committer-drift
would be in the *sanctioned* category (the `cs_drift_mismatch` pattern) — permitted, not a join.

**[W]** This restructures the Type A verdict below: Type A is **not one cross-product field
spanning both axes** — it is **two independent per-axis temporal stories that must not be wired to
each other.** Any schema shape where the DR classifier reads a `cs_structure` field (or a CS
diagnostic reads d/χ) would be an **unsanctioned second bridge** — foreclosed by v7 §4.5 +
Theorem 7, independent of whether it validates. **And the committer-axis Type A is already built**
(§C2): `cs_structure`'s `reference_frame` (t0) → `drift_state` (t1) → attractor (t2) IS the
committer's temporal drift model; the gap is observer-axis-only.

---

## 0. Headline verdicts (for the critic, before evidence)

| # | Question | Verdict | Tag |
|---|---|---|---|
| A | What makes a soft fork additive here? | Named pattern, 4 properties, all witnessed | [W] |
| B | Is the template extensible as a general property? | YES — 100/100 validate + compile with added optional fields; `additionalProperties:false` is the *mechanism*, not a ceiling | [W] |
| C1 | Time-indexed directionality — soft-fork-able? | YES on schema + compiler; engine needs an **additive** time-aware sibling of `derive_directionality`. DR-axis-internal. | [W] schema/compiler; [A] engine wiring |
| C2 | Reading-level frame-policy — where, soft-fork-able? | Soft-fork-able, BUT attachment point is forced by hub-separation: a **DR-axis** policy must NOT attach to `cs_structure` (CS). CS already owns its t0→t1→t2 model. | [W] |
| D | Author vs derive time-indexed d | **ESCALATED — operator's call.** Substrate supports both; trade-off stated §5. | [W] both paths exist |
| E | Foreclosure risks | One real one, and it is **architectural, not schema**: coupling the hubs. Schema/compiler themselves do not force a break. Plus the dangling-wire trap (authored-but-inert). | [W] |

---

## A. The soft-fork pattern (named, witnessed)

The just-completed stakeholder migration (commit `9c347e57`) is the worked example. A change here
is a **velvet/soft fork** iff it has all four properties:

1. **Optional schema field** — added to `properties`, *not* to `required`; old stories validate
   without it. **[W]** `stakeholders[]` is at `schema:549` outside the top-level `required`
   (`header, base_properties, perspectives, interval`, `:7–12`). Baseline: all 100 live stories
   validate (`BASELINE pass=100 fail=0`) and none authors `stakeholders[]`
   (`constraint_stakeholder/7` = **0 facts corpus-wide**, witnessed §C1).
2. **`multifile`/`dynamic` fact declarations** so an absent read fails *soft*, not *error*. **[W]**
   `derive_directionality/3` is a 3-stage **fallback cascade** (override → structure → canonical,
   `constraint_indexing.pl:406–413`): missing inputs fall through, they don't throw.
3. **Computed classification path ignores authored fields it doesn't read** (the keystone). **[W]**
   Re-confirmed against the prior session's controlled-null: `a1_probe.pl` flips an authored
   perspective `snare→mountain` and the 160 computed type/χ rows + signature + H¹ are
   **byte-identical** (`AUDIT.md` §A1, `a1_mut_perspective.txt`); the **same probe** moves every
   register under an ε mutation (positive control, 120/160 types flip). The computed path reads
   metrics/structure, not authored seats.
4. **Compiler tolerates fields it has no emission code for** (silent drop, no crash). **[W] (new
   this audit)** Compiling a story carrying an unknown-to-compiler `directionality_series` field
   exits 0 and emits a `.pl` **byte-identical (307/307 lines)** to the no-field compile — the field
   is silently dropped. See §B and §E.

**Pattern-A, one line:** *optional + fail-soft fact reads + computed-path-blind-to-authored +
compiler-drops-unknown.* A change with all four is additive; old corpus validates, compiles, and
classifies unchanged.

---

## B. Extensibility as a general property — WITNESSED, with the validator positive-controlled

Procedure: scratch-copy the live schema, add two optional fields, validate all 100 live stories
against it (compiler honors `DR_SCHEMA` env override, so the live schema is untouched).

Added (scratch only): top-level optional `directionality_series[]` (C1 shape) and an optional
`frame_policy` enum inside `cs_structure` (C2 shape, *for the test only — see §C2 for why this
attachment is wrong*).

```
BASELINE (live schema):  pass=100 fail=0
SCRATCH  (two optional fields added):  pass=100 fail=0
```
**[W]** Additive: the two new optional fields invalidate zero existing stories.

**Validator positive control** (so "pass" is not vacuous — `additionalProperties:false` is set at
top level and on `cs_structure`):
```
(1) junk field vs LIVE schema         -> FAIL as expected  (validator enforces additionalProperties:false)
(2) directionality_series vs LIVE     -> FAIL as expected  (field genuinely unknown to live)
(3) directionality_series vs SCRATCH  -> PASS as expected  (the scratch addition is what admits it)
```
**[W]** The validator fires; the field is genuinely new; the schema edit is what admits it.

**The ceiling that isn't a ceiling.** `additionalProperties:false` means you **cannot** sneak a new
field past the schema — the schema edit declaring the optional field is **mandatory** and must ship
with (or before) any data that carries it. But declaring it optional invalidates **no old data**.
So the schema edit is a *velvet* fork (additive), never a *breaking* one. **Keep this line sharp
for the critic:** schema/engine extensibility (optional additions stay soft — the architectural
property the operator wants) is distinct from corpus regeneration (the rebuild *populates* the new
optional fields — expected, not a "break"). Conflating them is the easy error; the evidence above
separates them — old data stays valid (extensibility); new data merely *adds* the field
(population).

---

## C. Type A input fit

### Reframing forced by hub-separation (read before C1/C2)

The conceptual frame called Type A "the temporal cross-product of the two axes." Against the
substrate **and** the operator's CS≠DR invariant, that resolves into **two disjoint temporal
stories**, each internal to one hub:

- **DR/observer Type A** — does a fixed seat's **d** (hence type/χ) drift across the timeline?
  Today: **partially built and partially frozen.** `classify_at_time/4` already reads time-varying
  ε/suppression/theater from `measurement/5` (`drl_composition.pl:201,193,208`) **[W]**, but for
  directionality it calls `derive_directionality(C, Context, D)` — **no Time argument**
  (`drl_composition.pl:203`, `transition_paths.pl:130`) **[W]**. So d is **timeless-frozen**: at
  every T the same d. The engine is doing *implicit frozen-d* and cannot express a d that re-derives
  per stage. This is the DR-axis Type A gap. C1 fills it.
- **CS/committer Type A** — does the **kernel** drift from its reference frame? **Already built,
  CS-internal.** `cs_reference_frame/2` (t0, 7 facts) → `cs_drift_state/3` (t1, 7 facts) →
  `cs_terminal_attractor/4` (t2) (`cs_drift_engine.pl:4–6,44–48`; v7 §4.5 "t0→t1→t2 trajectories
  via the attractor table") **[W]**. The Originalist/Living distinction on the *committer* side is
  **already the reference-frame declaration**: v7 §2 (Axiom 7) makes t0 a *declared* baseline —
  "*which* state counts as t0 is position-dependent … two analysts may author different baselines
  for the same kernel"; reference_frame = the fixed founding axioms (Originalist), drift_state →
  attractor = the re-evaluated trajectory (Living), and `cs_axiom_foreclosed` is the verdict on
  whether the founding axiom still holds. **The committer-side Type A needs no new field** — the
  worked Constitution example's *committer* reading ("do the founding axioms still hold or have they
  been overridden") is already expressible. **[W]**

**The forbidden shape:** a single frame-policy field that the CS axis authors and the DR
classifier consumes (or vice versa). `cs_drift_engine` reads **no** DR predicate (grep for
`classify_at_time|dr_type|chi|derive_directionality|measurement` in the CS engines returns only
comments) **[W]**. Wiring Type A across the hubs would violate the `two_axis_architecture_v7.md`
non-goal and Theorem 7. **Type A inputs must be offered on each axis separately.**

### C1 — Time-indexed directionality (DR axis). SOFT-FORK-ABLE.

- **Schema** **[W]**: add optional `directionality_series[]` (or extend the `MeasurementMetric`
  enum — currently `{theater_ratio, base_extractiveness, suppression_requirement}`, `schema:108–115`
  — with a directionality metric). Validated additive in §B.
- **Compiler** **[W]**: tolerates the new field today (silent drop). Emission code is new work but
  *additive* — it writes a new fact predicate, like `constraint_stakeholder/7` did; old emission
  unchanged.
- **Engine** **[A]** (graduation: implement + re-run the A1-style null): `derive_directionality/3`
  is a fail-soft cascade. A time-aware sibling — `derive_directionality_at(C, Context, Time, D)`
  that checks a time-indexed series first and **falls back to `derive_directionality/3` on
  absence** — is additive: stories without the series behave exactly as now. The consumers
  `classify_at_time/4` and `snapshot_type/3` already have `Time` in scope, so re-pointing them at
  the `/4` sibling is a localized edit, **DR-internal, touching no CS predicate.**
- **Consumer status** **[W]**: `classify_at_time` is reached only via `cs_kernel_registry` **at
  Time=0** and tests; `snapshot_type`/`constraint_history`/`degradation_chain` have **zero** live
  callers (the only grep hit is an archived story's module name). The multi-time trajectory is
  **dormant** — C1's natural consumer is dormant infrastructure that must be revived in the same
  change (else C1 is a dangling wire, §E). *Caveat:* `cs_kernel_registry` calling `classify_at_time`
  is a CS→DR *diagnostic* read that already exists and is explicitly fenced as observer-blind at the
  committer-edge layer; C1 must not deepen that into a dependency.

**"0 time-indexed directionality facts" — re-verified with positive control [W]:**
`directionality_override` is 3-ary `(C, PowerAtom, D)` (33 facts, no Time); `constraint_beneficiary`
/`constraint_victim` are 2-ary (no Time). `measurement/5` (time-indexed) is found **1315×** — so
grep *would* surface time-indexed directionality if any existed. It does not. Static only.

### C2 — Reading-level frame-policy. SOFT-FORK-ABLE, but attachment is hub-constrained.

- **It validates as an optional field** (§B test added it to `cs_structure` and 100/100 passed).
- **But the §B attachment point is WRONG under hub-separation.** The Originalist/Living frame-policy
  that governs the **DR-axis d-trajectory** (fix-d-at-t0 vs re-derive-d-at-tn) is a **DR/observer**
  policy. Putting it in `cs_structure` (the CS hub) and having `classify_at_time` (DR) read it is a
  CS→DR join — forbidden. **[W]** It must attach to the DR/observer authoring surface (alongside
  `directionality_series`, or the perspective/stakeholder layer) and be read only by the
  time-indexed DR classifier.
- **The CS axis does not need a new frame-policy field for *its* Type A** — `cs_reference_frame` +
  `cs_drift_state` already encode the committer-side t0/t1, and `cs_axiom` foreclosure is the
  Originalist/Living-equivalent verdict on the committer axis. Adding a CS frame-policy would
  likely duplicate existing CS temporal structure (verify before adding — graduation: read
  `cs_drift_engine` terminal-attractor semantics against the Originalist/Living question). **[A]**
- **Consumer caveat (dangling-wire):** even attached correctly on the DR surface, `frame_policy`
  has **no consumer until C1 exists** (a frozen-d classifier has nothing to branch). C2 must land
  *with* C1 and a `classify_at_time` branch, or it is inert (§E).

---

## D. Author-vs-derive fork — ESCALATED (operator's call; determines schema shape)

Both realizations of time-indexed DR directionality exist in the substrate as natural extensions;
neither is forced by the code. **This is a genuine fork the evidence does not settle.**

- **(a) AUTHORED series.** A `directionality_series[]` (the §B/§C1 shape) or a time-extended
  `stakeholder_d_override` (today `stakeholder_d_override/3` is a "probe surface, nothing in the
  corpus asserts it", `stakeholder_seats.pl:41–43` **[W]**). d is hand-authored at each checkpoint.
  - *For:* maximal expressiveness (any d trajectory); minimal engine change (read the series, fall
    back).
  - *Against:* d becomes a free authored parameter at every stage — a **cover-story surface** (the
    author can paint any drift), and it **breaks the standing invariant that d is *derived* from
    structure, never authored** (R1 / "per-seat perception is COMPUTED", `schema:250`,
    `stakeholder_seats.pl:108–112`).
- **(b) DERIVED from time-indexed roles.** Give the stakeholder/role its **own** time index
  (`constraint_stakeholder` → time-aware, or a role-change series), then reuse the **existing**
  `role_base_d` → `exit_modulation` → clamp machinery (`stakeholder_seats.pl:55–71`) at each Time.
  This is the deferred **"time-varying role/d" OQ** (OQ-83, schema:237 — explicitly "backgrounds the
  time-index of role… roles are static in this schema… deferred"), and its precondition (the static
  per-(C,Name) seat layer) is now **built**.
  - *For:* preserves "d is a consequence of authored structure, not a free dial"; aligns with R1
    and the no-authored-perception discipline; reuses witnessed derivation code.
  - *Against:* a role that flips is a **coarser** instrument than a continuous d (5 role atoms vs a
    [0,1] series); requires authoring role *changes* over the timeline.

**The substrate leans (b)-able:** the role→d derivation has a clean temporal extension point (the
same `role_base_d` map applies at each Time), and the seat layer that (b) needs already exists.
But **(a)** is strictly less engine work and strictly more expressive. **This is the operator's
ruling — it sets whether the new schema field is an authored d-series or a time-index on roles.**
Not self-resolved.

---

## E. Foreclosure risks

**The decisive finding: the schema and compiler do NOT force any breaking change for Type A.**
Adding the two inputs is additive at every layer witnessed (§B: validate; §A.4/§E-drop: compile).
The real foreclosure risks are **not** "old corpus goes invalid" — they are:

1. **ARCHITECTURAL (the one that matters): a new (A)-category data bridge.** **[W]** The protected
   invariant (witnessed census, §0/governing block — *not* the paper's stale "one bridge" count) is:
   exactly one DR←CS data bridge (`influences`), and no DR computation reads a `cs_structure` field
   as input. Any Type A schema shape where the DR time-varying-d classifier reads a `cs_structure`
   field (e.g. `frame_policy`-in-`cs_structure`, the shape §B happened to test) is a **new (A)
   bridge** — foreclosed by Theorem 7's disjoint-inputs requirement and the monotone-flow basis,
   regardless of validity. **Must be rejected** in favor of per-axis attachment (§C2): the DR
   frame-policy on the observer surface, the committer frame already carried by `reference_frame`.
   A new (B)-category read-only seam diagnostic (observer-drift vs committer-drift disagreement)
   is *permitted* — it is the `cs_drift_mismatch` pattern, not a join. *This is the foreclosure the
   rebuild plan most needs to honor,* and it does not fail loudly: the wrong shape validates,
   compiles, and classifies; it violates a theorem, silently.

2. **DANGLING-WIRE (Build-Discipline Pattern 1, made *more* likely by the very extensibility proved
   in §B).** **[W]** Because the compiler **silently drops** fields it has no emission code for
   (307/307 byte-identical compile), a Type A field added to the schema and **authored** but **not
   wired** (compiler emit + revived `classify_at_time` consumer) is **silently inert** — the exact
   `mandatrophy_resolved` defect (schema boolean, 0 compiler emissions, dead since the JSON-template
   migration; CLAUDE.md Critical Distinctions). Extensibility and the dangling-wire trap are the
   **same property** (the read site can't tell authored-absent from authored-inert). *Mitigation
   (not a fork):* land schema + compiler-emit + revived consumer in one change, or add a loud
   "authored-but-unconsumed" check; carry the provenance bit. C1's consumer (`classify_at_time`
   trajectory) is **dormant** today (§C1) — so C1 is born one revival away from inert.

3. **No schema-structural foreclosure found.** **[W]** Searched for: required-field additions
   (none needed — both inputs are optional), enum closure conflicts (the `MeasurementMetric` and
   `StakeholderRole` enums are closed, but *extending* an enum is additive — old values stay valid;
   only *removing* one breaks, which Type A does not require), and `additionalProperties:false`
   lockout (handled by the mandatory-but-additive schema edit, §B). If the operator chooses
   author-path (a) in §D, even the role-timeline complication of (b) is avoided. Stated as a
   witnessed absence, not reassurance: the probes that *would* catch a structural break (the 100/100
   validate, the byte-identical compile, the enum read) all ran and all came back additive.

---

## OPEN items (graduation steps)

1. **C1 engine wiring** [A] — implement `derive_directionality_at/4` (fallback to `/3`) + re-point
   `classify_at_time`/`snapshot_type`; graduate by re-running the A1 controlled-null (series-absent
   stories must stay byte-identical) **and** confirming no CS predicate enters the call path.
2. **C2 CS-duplication check** [A] — read `cs_drift_engine` terminal-attractor semantics against the
   Originalist/Living question; confirm whether a CS-side frame-policy is already expressible via
   `cs_reference_frame`/`cs_drift_state` before adding any CS field.
3. **D ruling** — operator's call (author-series vs time-indexed-roles); blocks schema shape.
4. **Trajectory-classifier revival** [W dormant] — `snapshot_type`/`constraint_history`/
   `degradation_chain` have zero live callers; C1's value depends on reviving the multi-time path,
   not just authoring the series.
5. **v7 §4.5 bridge paragraph is stale** [W] (operator-flagged, substrate-confirmed) — "exactly one
   intentional bridge … and nothing else" undercounts the read-only seam diagnostics (≥3:
   `cs_drift_mismatch`, `cs_kernel_registry`→`classify_at_time`, `cs_pattern_detection`→
   `constraint_signature`). The **data-bridge** count (1, `influences`) is still correct; the
   conflation of *data bridge* with *seam diagnostic* is the inaccuracy. Graduation: amend v7 §4.5
   (and/or `two_axis_architecture_v7.md`) to state the (A) data-bridge / (B) seam-diagnostic
   distinction, or confirm the seam diagnostics are intended to be uncounted. Belongs in KNOWN_STATE
   + the paper, not just here.

---

## Appendix — probe commands (reproducible, read-only)

```
# B baseline + scratch (DR_SCHEMA override keeps live schema untouched)
for f in json/*.json; do python3 python/generate_constraint_pl.py --validate-only "$f"; done
DR_SCHEMA=/tmp/dr_scratch/scratch_schema.json python3 python/generate_constraint_pl.py --validate-only "$f"
# E silent-drop: compile a story carrying directionality_series, diff line count
DR_SCHEMA=/tmp/dr_scratch/scratch_schema.json python3 python/generate_constraint_pl.py /tmp/dr_scratch/with_series.json
# C1 frozen-d witness
grep -n "derive_directionality(C, Context, D)" prolog/drl_composition.pl prolog/transition_paths.pl
# C1 0-facts positive control
grep -rh "measurement(" prolog/testsets/*.pl | wc -l   # 1315 (probe would find time-indexed if present)
grep -rh "directionality_override(" prolog/testsets/*.pl | head   # 3-ary, no Time
# Hub separation
grep -n "classify_at_time\|dr_type\|chi\|derive_directionality\|measurement" prolog/cs_drift_engine.pl  # comments only
```
