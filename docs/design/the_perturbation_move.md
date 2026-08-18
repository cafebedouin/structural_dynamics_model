# The Perturbation Move (with code)

*The one operation the engine runs everywhere: hold the rest fixed, perturb one axis, read invariant
vs variant. This is the code-facing companion to `docs/the_perturbation_principle.md` (the theory).
Read that for why the move is the framework's commitments in operational form; read this for how the
substrate already makes it, in four places, with the same signature.*

**Status:** v0.1 draft — design sketch. Scope: name the shared shape of `reading_diff.pl`,
`axiom_diff.pl`, the drift machinery, and `python/sweeps/perturb.py`, so the next operator built is
recognized as the *fifth instance of one move* and given the same shape, not invented fresh.

---

## The move as a code pattern

Every instrument below is this, specialized:

```
perturb(Axis, held_fixed=everything_else):
    cells_A := read_authored(object, at perturbation_setting_1)
    cells_B := read_authored(object, at perturbation_setting_2)
    align   := declared_key(Axis)          # the SEAT — explicit, never defaulted
    partition:
        invariant := { v | aligned(v) and value_A(v) == value_B(v) }   # situation-fixed → defer
        variant   := { v | aligned(v) and value_A(v) != value_B(v) }   # seated → the depth
        uncovered := { v | not aligned across both }                   # coverage gap
    verdict := stability over a *declared set* of keys (order-independent)
```

Two invariants of the pattern itself, enforced in code (see §5):
1. **The alignment key is a required argument** — it is the seat (`the_perturbation_principle.md`
   §3c). Baking it is the no-seat pose.
2. **Self-diff is the positive control** — perturbing an object against itself must return
   all-invariant under the strict key, or the probe cannot be trusted to find variance.

---

## Axis C — perturb the observer (`reading_diff.pl`)

The object is a reading; the perturbed axis is the observer position, the closed tuple
`context(agent_power(P), time_horizon(T), exit_options(E), spatial_scope(S))`. The authored cells are
`constraint_indexing:constraint_classification/3` — the value at each vantage is the *type*. Two
readings (or one reading at two settings) are diffed under a declared alignment key:

```prolog
% reading_diff/6 — the partition; Key is the declared seat (exact | fuzzy_agent_power | weighted(...))
reading_diff(RA, RB, Key, Agreement, Disparity, Blind).

% the seat is explicit and cannot be silently defaulted:
%   exact             — same (P,T,E,S) vantage
%   fuzzy_agent_power — same agent_power, T/E/S collapsed
%   weighted(Ws,Thr)  — tolerance relation; HAS NO vantage partition, so reading_diff/6 THROWS on it
%                       (you must route it through aligned_pairs/5) — refusing to fake a seat
```

`Agreement` is what no observer-perturbation moves: situation-fixed, the cross-seat invariant you may
state about the kernel. `Disparity` is the perspectival gap — the depth, the standpoint-set content.
`Blind` is coverage the perturbation could not reach. The headline verdict is the stability of the
*regime* across a declared key set — never a bare per-pair label, because the regime is a property of
*pair × key* (the same pair reads undersampled under `exact`, binocular under `fuzzy_agent_power`):

```prolog
stability_verdict(RA, RB, Verdict).   % robustly_binocular | key_fragile | robustly_undersampled
% order-independent: ∀ keys disparity≥1 → robustly_binocular; ∀ keys disparity=0 → robustly_undersampled;
%                    else → key_fragile  (the seat decides the regime — that flip IS the finding)
```

Corpus reading: 615 within-kernel pairs sort 39.5 % robustly_binocular / 53.7 % key_fragile / 6.8 %
robustly_undersampled (`reading_diff_census.pl`, `audits/2026-06-03_reading_diff_census/reading_diff_census.md`) — i.e. for a
majority of reading-pairs, *whether the two observers disagree at all* is set by the alignment seat.
That is cyclopean-point at corpus scale: the seat is doing the depth computation.

## Axis B — perturb the axiom (`axiom_diff.pl`)

Same partition, lifted from type-cells to a reading's **axioms**. The authored cells are
`cs_axiom(UID, Tier, Name)` reached via `cs_story_uid`; the value compared is the **grounding**
(`cs_axiom_grounding`, per-story, so it can vary) — *not* `cs_axiom_status` (keyed on the axiom name,
hence global, hence cannot vary across readings: comparing it would be a degenerate probe).

```prolog
axiom_diff(RA, RB, Key, Agreement, Disparity, Blind).   % Key ∈ {exact_name, concept}
```

The seat here is *sharper than at the observer layer*, and the substrate makes the point: **0 of 935
within-kernel reading-pairs share even one axiom name** — every reading authors bespoke names. So
`exact_name` is structurally all-blind across readings: unlike the observer axis (where exact
`(P,T,E,S)` is a real mechanical key, because readings reuse canonical context tuples), the axiom
axis has **no mechanical alignment key at all.** Aligning axioms requires a *declared semantic
equivalence* — the caller's seat, never baked:

```prolog
:- multifile axiom_concept/2.   % EMPTY by default. The caller declares which axioms are "the same".
:- dynamic   axiom_concept/2.   % concept key reads this; empty map ⇒ all-blind, and the report says so.
```

Declare the seat (the Westphalia absolute pair, 4 bespoke names → 2 concepts) and the perturbation
reads a **grounding inversion**: both readings hold a sovereignty-absolute axiom and a non-interference
axiom, but A grounds them `conventional`/`deontological` and B inverts to `deontological`/`conventional`.
Neither reading alone discloses that the *grounding structure* is the contested parameter; the diff
computes it. (`tests/test_axiom_diff.pl` freezes this.) The cross-kernel case is the same operator
with `RA`, `RB` in different kernels — the Westphalia near-kernels (OQ-58/59) are a *positive*: two
near-identical kernels held side by side make the kernel-level invariant legible as their agreement
and the divergence legible as their disparity.

## Axis A — perturb time (the drift machinery)

The object is a constraint; the perturbed axis is the time index. The authored cells are the temporal
measurements (`narrative_ontology:measurement/5`, `interval/3`); the value is the classification (or
the metric) at each t. The same partition appears as **stable-across-time vs drifting**, computed by
`transition_paths.pl` / `metric_drift_events.pl` (drift velocity = first-order rate, acceleration =
second-order), with the variant resolving toward **terminal attractors** (`husk`,
`axiom_foreclosure`) — the temporal disparity's structure. `classify_at_time/4` /
`snapshot_type/3` give the per-t value the diff reads.

The discipline mirror here is already a `build_discipline.md` instance: a temporal predicate that
fabricates a default for an absent measurement (the `Supp=0.5` fallback, Pattern 4) injects a
*phantom invariant* — a value that does not move because it was never authored, masquerading as a
value that does not move because it is fixed. Perturb the fallback (tripwire it to an out-of-range
value) and the phantom invariants flip: that is the positive control for axis A.

## The reflexive axis — perturb the apparatus (`python/sweeps/perturb.py`)

Turn the move on the engine's own constants:

```python
# python/sweeps/perturb.py — vary a config threshold, re-export, measure per-kernel fold-survival
perturb("tangled_rope_chi_floor", [0.40, 0.41, 0.42])   # → which kernels keep their type, which flip
```

Invariant = classifications that survive the calibration perturbation (the constraint's verdict).
Variant = classifications that flip under a small threshold nudge (the *threshold's* verdict, not the
constraint's — `design_discipline.md` §5: a band boundary is a calibration, not a discovery). The
stability band in `enhanced_report.py` (E5) and the sensitivity sweeps are its consumers; the
`ε`-stability rule (`design_discipline.md` §7) is the same control for the one authored primitive.
Per its own docstring `perturb.py` is *the single primitive that unifies the type-stability sweep
family* — the move recognizing that the bespoke sweeps were already the same move (the consolidation
that collapses them onto it is the in-progress half; the consolidation owes the old-vs-new
diff `build_discipline.md`'s *prove before you replace* requires — a faith-merge otherwise). See `docs/technical/perturb_substrate.md`.

---

## The shared signature (why the next operator should look like these)

| instance | object | perturbed axis | authored value read | invariant = | variant = | seat (declared key) |
|----------|--------|----------------|---------------------|-------------|-----------|---------------------|
| `reading_diff` | reading | observer (P,T,E,S) | classification type | agreement cells | disparity cells | exact / fuzzy_agent_power / weighted |
| `axiom_diff` | reading | axiom | grounding | agreeing concepts | grounding mismatch | exact_name / concept (`axiom_concept/2`) |
| drift machinery | constraint | time t | type / metric at t | stable trajectory | drift toward attractor | the t-window / frame-at-t0 |
| `perturb.py` | the engine | a config constant | classification | survives the sweep | flips under the sweep | the param + value range |

A new diagnostic that does not fit this table is either a genuinely new kind of question (worth
flagging) or — more often — this move not yet recognized as this move. Before building it bespoke,
ask: what is the object, what axis am I perturbing, what authored value do I read, and what is the
declared key? If those four are answerable, build it as the fifth row, with the same partition and
the same positive control.

---

## The discipline (carried from the principle, enforced in code)

- **Declare the axis/key as an argument; never bake it.** `reading_diff` throws on a seat it cannot
  honestly form (`weighted`); `axiom_diff`'s `axiom_concept/2` is empty by default. A perturbation
  operator that picks its own axis silently is the no-seat pose in code.
- **Self-diff is the mandatory positive control.** `reading_diff(X,X,exact,_,[],[])`;
  `axiom_diff(X,X,exact_name,Ag,[],[])` with `Ag` = all axioms. An operator that cannot see "no
  difference" cannot be trusted to see difference (`build_discipline.md`, *Every diagnostic needs a
  positive control*). A claimed invariant is unfalsified until a known-variant case moves the probe.
- **One axis at a time.** Hold the rest fixed; vary one. The held dimensions are what make the varied
  one's contribution legible — vary two and the partition blurs and attribution is lost.
- **Read authored substrate, not recomputed/derived values, unless the design says otherwise.**
  `reading_diff` and `axiom_diff` read the authored cells, never the computed `product_site_orbits.json`
  (full coverage there makes blind-spots impossible — it would erase the coverage signal). Perturbing
  a derived value measures the derivation, not the object.
- **Beware the phantom invariant.** Zero variation has two causes that look identical: a genuinely
  fixed axis, and a perturbation never run / an absent datum defaulted (`build_discipline.md`
  Patterns 4–5). An invariant you *found* and an invariant you *failed to probe* present the same flat
  result — the positive control is what tells them apart.

---

*Pointers: `docs/the_perturbation_principle.md` (theory); `cyclopean-point.md` (disparity-as-signal,
situation-fixed vs open); `debugging_philosophy.md` (the A/B/C trifurcation); `seat-theorem-v1.md`
and `docs/design/design_discipline.md` (verdicts are seated; Axiom R; §3 recurrence);
`docs/technical/build_discipline.md` (positive controls, the absence-as-presence spine).*
