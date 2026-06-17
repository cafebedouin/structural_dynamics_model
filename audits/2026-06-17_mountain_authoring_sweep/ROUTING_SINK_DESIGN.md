# Routing Sink Design — the natural_law author↔engine diff

**Status:** DESIGN ONLY (no code). Authorized by operator 2026-06-17, scoped to the **natural_law diff**.
Explicitly NOT in scope (drawn, not built): type_1's RED-cap, the other `resolve_modal_signature_conflict`
clauses, FNL/FCR/FSM — each a separate ruling.

## 0. The principle (operator architecture)
The engine **routes** disagreement; it **never reclassifies**. Only review reclassifies. The author↔engine
diff is the product, not an error to suppress. Naming it: *engine never reclassifies; it routes
disagreement* — a ruling about the routing **mechanism** and about `natural_law`; it does **not** license
retiring any other adjudicator.

## 1. Pre-build confirmations (operator's two conditions) — BOTH PASS, witnessed
1. **The :614 diff is tappable pre-cap.** `dr_claim_mismatch(C, Ctx, type_1_false_summit, severe)`
   (`drl_core.pl:614`) is a standalone predicate that *returns* the mismatch term; `severe` is a returned
   **severity field**, not an applied cap. The RED-cap is a downstream consumer (verdict layer reads
   severity → caps). ⇒ a router consumer can read the per-seat diff without modifying `:614` and without
   triggering the cap. "Verb change, not new machinery" holds: the diff already exists, miswired to an
   adjudicator.
2. **Detector and resolver are separable code paths.** Detector = `natural_law_signature(profile(...))`
   (`signature_detection.pl:359`) — a pure reading. Resolver = `resolve_modal_signature_conflict(_,
   natural_law, mountain)` (`signature_detection.pl:867`, called at `:823`) — the overwrite (rope→mountain).
   Retiring `:867` neutralizes the overwrite and leaves the detector firing.

## 2. The three readings
- **Author** — per-seat `constraint_classification(C, Type, Context)`. Authority on the **claim**. May be
  SILENT at a seat; may CONTRADICT itself at a seat (`[mountain,rope]`).
- **Engine `dr_type`** — computed per seat (χ = ε·f(d)·σ + signature layer). The structural reading.
- **natural_law DETECTOR** — `natural_law_signature` fires/not: "reads immovable-by-structure." A THIRD,
  author-independent reading. **Demoted** here from override-trigger to pure router input.

## 3. The leaf unit: the per-SEAT diff (a TYPE, not a convention)
The sink's atomic record is `diff(Constraint, Seat)` — **never** per-constraint. A constraint emits N
seat-diffs (N = `standard_context` count). The aggregate-merge failure that recurred **three times** in this
arc (confirmed-artifact → the 370 union → the three-constraint witness) is structurally impossible iff the
type stays per-seat to emission. **No predicate in the sink may collapse seats into one constraint verdict.**

```
diff(Constraint, Seat) = {
  author:    mountain | rope | ... | [mountain,rope] (self-contradiction) | AUTHOR_SILENT
  engine:    <dr_type at Seat> | ENGINE_SILENT
  detector:  nl_fired | nl_absent
  address:   <router label — §4>
  provenance: <transparency record — §5>
}
```

## 4. The router address (a LABEL, not a gate — certifies nothing)
- **engine/exit-table review** — author-uniform-mountain at the seat + engine degrades (thermo
  moderate/institutional). **Caveat:** author-uniformity is CONTAMINATED — the seat-blind authoring agent
  flattens to uniform-mountain whether or not the thing is immovable, so this address is a *candidate*, not a
  finding. Disambiguating "uniform because immovable" from "uniform because flattened" is exactly what an
  author-independent immovability signal would do — see §7.
- **authoring review** — author self-contradicts at the seat (`[mountain,rope]`), or uniform-mountain seats +
  a contingency omega (topological analytical; AC). The 161 hard-contested land here **by construction**.
- **generation-gap** — AUTHOR_SILENT at a seat the engine classified (topological moderate).
- **no route** — author authored divergence and the engine reproduces it (presheaf working), or all readings
  agree.

**Why a noisy detector is now usable (the unlock):** routing ≠ certifying. The GAP-08 re-leak (the natural_law
detector firing on `price_formation__naturalist`) produces a review **item**, not a miscertification — volume
in the queue, not a wrong answer in the substrate. The prose-trust / unauthorable-`HasAlternatives` problems
were fatal to a **gate** (which certifies); they are survivable in a **router** (which flags).

## 5. Transparency (operator add-on) — the diff states what happened
Every routed item carries an explicit provenance record, for the adjudicating consumer AND for
troubleshooting. Absence is **typed**, never blank (connects OQ-137):
- `author: supplied=<v>` **or** `AUTHOR_SILENT("no constraint_classification at this seat")`
- `engine: dr_type=<v>` **or** `ENGINE_SILENT("dr_type produced no solution at this seat")`
- `detector: nl_fired | nl_absent`
- "author supplied but engine silent" and "engine commented but author silent" are each NAMED states.

## 6. What is RETIRED vs LEFT IN PLACE
- **RETIRED:** `resolve_modal_signature_conflict(_, natural_law, mountain)` (`signature_detection.pl:867`) —
  the overwrite. `natural_law_signature` (detector) stays, as a router input.
- **LEFT IN PLACE — wired, NOT disabled (the next rulings, drawn as dashed boxes):**
  - **type_1's RED-cap** (the consumer that caps mountain→RED on `severe`). A cap **withholds** certification;
    the natural_law overwrite **manufactured** it. Opposite verbs — "never reclassify" may be right for the
    overwrite and wrong for the cap. **Separate ruling, own kill condition:** *what gets certified during the
    route-and-review window that the cap currently blocks?* Do not fold into this build.
  - the other `resolve_modal_signature_conflict` clauses (`false_natural_law`→tangled_rope,
    `coupling_invariant_rope`→rope, `false_ci_rope`) and FNL/FCR/FSM — same: next ruling.

## 7. The named OPEN (non-fatal now; was fatal as a gate)
The detector needs an **author-independent immovability reading**, and the candidates are burned: NOT
author-uniformity (contaminated, §4), NOT the exit-table (it is the checkee — can't be checker and checkee),
NOT Boltzmann (orthogonal: extraction-not-contingency, witnessed). *What structural fact, written by neither
the author nor the degradation, reads "immovable"?* = GAP-08 in new clothes — still unsolved, now merely
non-fatal (routing lowers the bar from "certify" to "worth a look"). Shades into what the generator should be
made to emit (the typed-invariance guidance).

## 8. Diagram
```
   AUTHOR              ENGINE dr_type        natural_law DETECTOR
 (per-seat claim)    (structural reading)   (immovable-by-structure?)
        \                   |                        /
         +------- per-SEAT diff (the leaf TYPE) ----+
                            |
                   [ router : address = LABEL, certifies nothing ]
              /          |             |              \
     engine/exit    authoring     generation-      no-route
      review         review          gap         (presheaf ok)
                            |
                   REVIEW  (the ONLY reclassifier)

   +-- - - - - - - - - - - - - - - - - - - - - - - - - - - --+
   |  type_1 RED-cap : LEFT ON  (next ruling, own kill cond) |   dashed = wired, not disabled
   |  other resolvers / FNL / FCR / FSM : next ruling        |
   +-- - - - - - - - - - - - - - - - - - - - - - - - - - - --+
```

## 9. Acceptance / kill conditions (for the eventual build)
1. Router consumer taps `dr_claim_mismatch/4`; the predicate is **UNMODIFIED**.
2. `resolve_modal_signature_conflict(_, natural_law, mountain)` (`:867`) retired; `natural_law_signature`
   still fires (detector lives) — witness: a natural_law-detected constraint no longer reads mountain via
   overwrite, but appears as a router input.
3. type_1's RED-cap behavior **UNCHANGED** this pass (regression: existing RED verdicts byte-identical).
4. **Sink unit is per-seat at emission.** KILL: if any routed item is keyed per-constraint, the build is wrong.
5. Transparency: every item states author/engine/detector state; silence typed, never blank.
```
```
