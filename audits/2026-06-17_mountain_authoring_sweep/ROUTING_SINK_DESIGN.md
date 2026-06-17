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

## 2. Two LIVE inputs + a socketed third
The sink has **one live diff** today, not three readings. Be precise about each input's state:

- **Author** (LIVE) — per-seat `constraint_classification(C, Type, Context)`. Authority on the **claim**. May
  be SILENT at a seat; may CONTRADICT itself at a seat (`[mountain,rope]`).
- **Engine `dr_type`** (LIVE) — computed per seat (χ = ε·f(d)·σ + signature layer). The structural reading.
- The author↔engine `dr_type` diff is **the one LIVE diff** the sink routes.
- **natural_law DETECTOR** (*socketed — wired but UNPOWERED*) — `natural_law_signature`: "reads
  immovable-by-structure," a third author-independent reading IN PRINCIPLE. **Demoted** here from
  override-trigger to pure router input. But it fires **0/72 on the live corpus** (witnessed §9(i)) because
  every live profile arrives `HasAlternatives = unknown` (72/72), closing the gate before its discriminating
  content is ever consulted. It is a **socket**, not a live input: the slot is wired, the signal that would
  power it does not yet exist (§7, §9(iii)). A builder must not read it as a third live reading.

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
- **LEFT IN PLACE — wired, NOT disabled (the next rulings, drawn as deferred boxes):**
  - **type_1's RED-cap** (the consumer that caps mountain→RED on `severe`). A cap **withholds** certification;
    the natural_law overwrite **manufactured** it. Opposite verbs — "never reclassify" may be right for the
    overwrite and wrong for the cap. **Separate ruling, own kill condition:** *what gets certified during the
    route-and-review window that the cap currently blocks?* Do not fold into this build.
    **PERMANENT-SAFE (design completeness):** nothing in the sink's wiring assumes the cap will ever route.
    The design is correct and COMPLETE with the type_1 RED-cap left on **permanently** — if routing type_1 is
    later ruled wrong, the sink is still a finished thing. The cap is *left on*, not *load-bearing-deferred*;
    the type_1 ruling stays genuinely open and the sink does not wait on it.
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
   AUTHOR              ENGINE dr_type      [ natural_law DETECTOR ]
 (per-seat claim)    (structural reading)  (immovable-by-structure?)
        ║                   ║              ┊ wired-but-UNPOWERED:    ┊
        ║                   ║              ┊ 0/3098 fires, gate shut ┊
        ╚═══════╗   ╔═══════╝              ┊ on ha=unknown (§9 i)    ┊
                ║   ║                      ┊............╌╌╌╌╌╌╌......┊
         +--- per-SEAT diff (the leaf TYPE) ---+      (socket, no signal yet)
                       ║                    ┊
              [ router : address = LABEL, certifies nothing ]
              /          |             |              \
     engine/exit    authoring     generation-      no-route
      review         review          gap         (presheaf ok)
                            |
                   REVIEW  (the ONLY reclassifier)

   +== ruled-but-DEFERRED (a ruling exists; held off) ===================+
   ‖  type_1 RED-cap : LEFT ON  (next ruling, own kill cond)             ‖
   ‖  other resolvers / FNL / FCR / FSM : next ruling                    ‖
   +=====================================================================+

   LEGEND — three DISTINCT input/box states, do not collapse:
     ═══  LIVE         : author + engine dr_type; the one live diff the sink routes.
     ┊┄┄  UNPOWERED    : natural_law detector — wired into the leaf type but fires 0
                         (no ha=false signal exists in any corpus); a socket, not an input (§7, §9-iii).
     ‖═‖  DEFERRED     : type_1 cap / other resolvers — a ruling drawn but held off; LEFT ON, not relied on.
```

## 9. Acceptance / kill conditions (for the eventual build)

### 9a. Does the natural_law detector earn "live third input"? — an INPUT-PROVENANCE question, NOT an output-agreement one
Three findings, each with its own witness; together they say *socketed, not live*. The judgment is about the
detector's **inputs**, not its firing pattern — co-firing or co-disagreement with another reading is irrelevant
if the input that powers it does not exist.

- **(i) UNPOWERED — witnessed.** `natural_law_signature` fires **0/72 on the live corpus** — and **0/3098**
  across four independently-generated corpora (live 72 + testsets_haiku 960 + testsets_flash 960 + kernel_v1
  1106), with `HasAlternatives = unknown` at **100% (3098/3098)** in every one. The **positive control fires**:
  `natural_law_signature(profile(0.92,0.02,0.04,0,false,stable,_))` → succeeds, so `0` is a *real absence*, not a
  dead/malformed read. **Overdetermined:** the clause requires `HasAlternatives == false`; live profiles arrive
  `unknown`, so the gate shuts *before* any discriminating content is consulted — 0 fires no matter how good the
  reading is. The signal it needs is authored by **no generation pipeline** (all four corpora, two distinct
  twin models, agree). Cite the count *and* the control.
- **(ii) INPUT-INDEPENDENT of the exit-table — witnessed.** Detector inputs `{accessibility_collapse,
  suppression, resistance, beneficiary_count, HasAlternatives, temporal_stability}`
  (`signature_detection.pl:359`) are **disjoint** from the exit-table's `{time_horizon, exit_options}`
  (`effective_immutability`, `constraint_indexing.pl:195+`). So the detector is **not** the exit-table's second
  opinion by provenance — co-firing/co-disagreement is irrelevant. Independence is a property of *inputs*,
  witnessed against `effective_immutability`'s inputs, not of firing pattern.
- **(iii) DISCRIMINATING CONTENT — UNSOLVED, ≡ §7.** The detector's only *discriminating* leg is
  `HasAlternatives` (the §7 dead signal); its other conditions are the burned metrics
  (accessibility/suppression/resistance) the delta-control/Boltzmann probes already showed cannot separate
  physics from naturalization. So **§9(iii) and §7 are the same question:** the detector earns "live third
  reading" **only** when §7's author-independent immovability signal replaces `HasAlternatives`.
  - Name all three outcomes so **silence ≠ redundancy ≠ divergence**:
    *unpowered* (current — gate shut on `ha=unknown`) /
    *powered-but-non-discriminating* (gate opens but only the burned metrics carry it) /
    *powered-and-discriminating* (needs the §7 signal).
  - **KILL:** do **not** count the detector a live input until (iii) — until an author-independent immovability
    signal replaces `HasAlternatives` and is shown to discriminate. Until then it is a wired socket.

### 9b. Build acceptance / kill conditions
1. Router consumer taps `dr_claim_mismatch/4`; the predicate is **UNMODIFIED**.
2. `resolve_modal_signature_conflict(_, natural_law, mountain)` (`:867`) retired; `natural_law_signature`
   still fires (detector lives) — witness: a natural_law-detected constraint no longer reads mountain via
   overwrite, but appears as a router input.
3. type_1's RED-cap behavior **UNCHANGED** this pass (regression: existing RED verdicts byte-identical).
   **Permanent-safe:** the sink is COMPLETE with the cap left on forever; no wiring assumes it ever routes (§6).
4. **Sink unit is per-seat at emission.** KILL: if any routed item is keyed per-constraint, the build is wrong.
5. Transparency: every item states author/engine/detector state; silence typed, never blank.
```
```
