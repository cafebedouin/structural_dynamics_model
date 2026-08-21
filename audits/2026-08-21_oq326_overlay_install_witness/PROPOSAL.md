# OQ-326 Phase 2 — PROPOSAL

**Written:** 2026-08-21, before execution. **OQ:** OQ-326. **Parent:** OQ-302 (resolved 2026-08-19).
**Anchor:** HEAD `8efd8be7`. **Plan:** `~/.claude/plans/review-and-present-a-adaptive-wigderson.md`.

## The question

At HEAD, which `probe_harness:with_{overlay,retracted,asserted}` call sites would **throw** under
the six-check strict harness Phase 3 installs — and, of those, which declare their zero/partial
**in the committed artifact** (→ dated retrofit wrapper) versus which do not (→ mint an OQ against
that probe's published finding, operator's call).

Phase 1 (2026-08-19) answered a *narrower* question: did any prior audit overlay a RULE. Answer:
no. Phase 2 widens it to all six checks, of which **dynamism is entirely new** — Phase 1 tested
rule-ness and never tested whether the target could be mutated at all.

## What runs

**Static arm.** `audits/2026-08-19_oq302_bound_false_repair/extract_overlay_templates.py`, reused
**verbatim**. It parses *argument positions* with a balanced-paren scan. A functor-proximity grep
is the wrong instrument and its false-positive shape is already recorded (Phase-1 census §1: six
rule-bearing predicates surfaced that way, all six goal-position — read, not overlaid). The
44/27/13 figure is two days old; a census starts decaying the day it is taken, so it is re-taken,
not cited.

**Mechanical arm.** Every distinct retract-side template **generalized to fresh arguments**, and
every assert-side predicate, evaluated against a loaded program on five columns:

| column | test | verdict meaning |
|---|---|---|
| resolvable | `predicate_property(M:T, defined)`; on failure `undefined` vs `arity_mismatch(A2)` via `current_predicate/1` | not resolvable ⇒ always a defect, no escape |
| rule-bearing | the harness's own detector **with `copy_term`** | Phase 1 ran the *leaking* form; re-run non-leaking and confirm the Phase-1 verdict survives |
| generalized fact-match count | `findall` over `clause(M:T, true)` | **0 ⇒ no instantiation can match ⇒ the site measured nothing** (conservative in the right direction). >0 ⇒ not voided *by this mechanism*; per-instance binding residue **declared, never assumed away** |
| shadowable | would an asserted fact land behind a unifying surviving clause, **at template shape** (R8) | fact-shape testing is permissive — the false-green direction |
| dynamic | `predicate_property(M:T, dynamic)` | static ⇒ cannot be overlaid at all; new column, no Phase-1 counterpart |

Each site runs under **the load chain the probe itself declares**. Where the chain is not
determinable from the file, the site is marked **`unevaluated`** and treated as *not
artifact-declared* in classification — declared, not guessed.

## Controls — two-sided, one pair per check, all five pasted

A control that only fires licenses nothing. Planned pairs:

| check | FIRES on | DECLINES on |
|---|---|---|
| dynamism | `boltzmann_compliance:boltzmann_invariant_mountain/2` (static — verified by query) | `narrative_ontology:constraint_metric/3` (dynamic — verified) |
| rule detector | `boltzmann_compliance:boltzmann_invariant_mountain/2` | `config:param/2` (Phase 1's pair, reused) |
| resolvability | bogus functor ⇒ `undefined`; `constraint_metric/2` ⇒ `arity_mismatch(3)` | `constraint_metric/3` |
| empty snapshot | a declared-dynamic-but-unpopulated template | `config:param(corpus_path, _)` |
| shadow | assert over an unretracted unifying fact | a retract+assert pair |

## Classification — TWO ORTHOGONAL AXES (R4), not one four-way partition

- **Axis 1 — self-witnessed? (y/n).** Does the probe assert *inside the overlay* that the change
  took effect? Model: `audits/2026-06-11_oq110_residual_join/backed_semantic_probe.pl` Control C,
  which **fails loudly** if the flip survives retraction. Decides whether the probe's **published
  finding is voided**. **Not mechanically decidable** — read each call site's Goal argument and ask
  *does anything in here fail if the overlay did not install?* Grep (`\+`, `-> true ; fail`,
  `format.*FAIL`) is a starting point, never the verdict: a Goal that merely *observes* is not
  self-witnessing. **Record the deciding `file:line` for every `y`; a site I cannot point at a line
  for is `n`.**
- **Axis 2 — throws under the strict harness? (y/n).** Mechanical, from the arm above. Decides
  whether the site needs **migration**. Self-witnessing exempts a probe from being *voided*, not
  from being *strict-harness-clean*.

For every **throws = y** site, the intent test: the committed artifact **itself** declares the
zero/partial (a syntactic declaration like `with_retracted([], …)`, or an in-probe assertion) →
dated retrofit wrapper. Intent **not derivable from the artifact**, or the site is `unevaluated` →
**no wrapper**; mint an OQ against that probe's published finding and add a pointer comment,
leaving it throwing. *"It looks like they meant it"* is the assumption move this OQ exists to
prohibit.

## Reported separately (F8/F10/F11)

Bare-`with_asserted/2` sites, and any fact no template covers, throw
`probe_overlay_reach_undecidable` **by construction** — unconditionally, before any site-specific
analysis — because there is no declared query shape to test reachability at. They are one uniform
class with one already-ruled migration (`reach_undeclared`, **not** `allow_shadowed` — the latter
means *I checked and accept the shadowing*, and these sites never had a check to accept). Mixed
into the general throw count they would read as scattered defects. **If this class comes back
large, it is reported BEFORE any wrapper is written** — a mechanically-determined class is a
re-scoping question, not a migration (F11).

## Stopping conditions (operator's, not mine)

1. Any site that throws whose intent is **not derivable from the committed artifact** — that means
   minting an OQ against a published audit's finding. Bring the list; write no wrapper.
2. The bare-assert / template-uncovered class comes back large.
3. HEAD moves onto any of R6's five named paths.

## What this proposal does NOT authorize

No harness edit. Phase 2 is a read-only deciding pass; landing Phase 3 first would destroy the
pre-change state that is this census's own negative control.
