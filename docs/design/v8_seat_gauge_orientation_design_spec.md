# v8 Design Spec — Seat / Gauge / Orientation (the worked one-seat ontology)

**Status:** DESIGN SPEC (draft for review) — not the v8 paper, the precursor to it.
**Authored:** 2026-06-16 (Claude Code), from the seat/orientation invariant audit + the R3
presentation-vs-structure probe (`audits/2026-06-16_seat_invariant_vs_prolog/`).
**Intended handoff:** review by the originating reasoning instance → a new Claude Code instance
drafts an implementation plan from §8.
**Supersedes:** nothing on disk yet — it *unifies* three existing artifacts under one vocabulary
(see §0). It does **not** retire `docs/seat-theorem-v1.md`; it operationalizes it.

> **Provenance discipline.** Every v7 claim below is cited to a verified line in
> `docs/deferential_realism_paper_v7.md` (checked 2026-06-16). One correction to the originating
> ruling is folded in: it leaned on v7's phrase *"exactly one intentional bridge … and nothing
> else,"* which **v7 line 109 explicitly disowns** as an earlier draft that "wrongly conflated"
> two kinds of cross-axis surface. The corrected (A)/(B) distinction is used throughout (§3) — it
> is *sharper* for one-seat, not weaker.

---

## 0. Why v8 (and why not an OQ)

The seat/orientation invariant is not a finding inside the existing framework; it re-describes the
framework's core ontology. Three artifacts already exist and say the same thing in three
vocabularies that collide on the word **seat**:

- **`docs/seat-theorem-v1.md`** — the *law*, abstract and example-free: the **Coupling Theorem**
  (a verdict is seat-free iff contentless), the **no-seat pose** as the unique inconsistency,
  **declared-vs-concealed** as the live distinction, **Corollary 3** (a staked seat must be
  *honored under confrontation*, not reabsorbed), §8 (the situation itself is framed → framing is
  a seat).
- **`docs/deferential_realism_paper_v7.md`** — the *two-axis realization*: an observer axis
  (`dr_type` over positions) and a committer axis (`cs_*`), promoted to "co-equal" with its own
  anchor (Axiom 7), detection-independence proof (Theorem 7), and conflict dichotomy (Theorem 8).
- **the CS engine** (`cs_pattern_detection.pl`, `cs_axiom_engine.pl`, `cs_drift_engine.pl`, …) —
  the *mechanism*.

v8's job: state the **single ontology** (seat / gauge / orientation) that all three instantiate,
draw the seat/face line **operationally** (§2), make the line a **checkable invariant** (§3),
resolve the **vocabulary collision** (§4), and fold v7 + the engine in as the worked realization.
That is a version step, not an issue. An OQ tracks an open question inside an ontology; this fixes
the ontology. v8 gets to define its own terms, and adopts the cleaner seat/gauge/orientation
vocabulary over v7's overloaded "seat."

**What is retired:** only the conversational *two-seat / "lattice of content-seats"* hypothesis
floated during the audit. It was never a file (there is **no** `two_seat_theorem.md`); nothing on
disk is deleted by v8.

---

## 1. The ontology: three things, one of them singular

For any **contentful** question about a constraint (one the situation does not by itself settle —
`seat-theorem-v1` §2):

- **Seat** — *the one content-position the verdict answers to: the reference reality it is
  audited against.* There is exactly one per contentful question. In the engine this is the
  **ε-anchored metric reality** (`constraint_signature` / the `dr_type` content), the
  who-benefits / what-binds structure. Vary it and the *content verdict* changes. (`seat-theorem-v1`
  Coupling Theorem: contentful ⇒ seated.)
- **Gauge** — *positions over the one content.* The observer orbit U₁…U₄: the same content read
  from many standpoints (`dr_type` across contexts; H¹ measures whether the standpoints agree).
  Varying the gauge rotates the *reading of* the one content; it does not introduce a second
  content. **This is what v7 unfortunately calls "the seat"** (§4).
- **Orientation** — *the holder's relation to its one seat,* a **co-equal independently-metered**
  relation (not a second seat), with two structured faces (§5): **showing** (how the selection is
  presented) and **keeping** (whether it is retained vs updated over time). In the engine this is
  the entire committer/`cs_*` axis.

The whole claim of v8 in one line: **one content-seat, gauge-rotated and orientation-audited.**

---

## 2. The operational discriminator — audit direction, not input independence

The seat/face line cannot be drawn by "does varying this axis change a verdict?" — that test
(input-independence; the audit's 2×2) is passed by **gauge and orientation alike**, so it cannot
tell a second seat from a face. (Witnessed: the audit's 2×2 showed `dr_type` and `cs_pattern`
respond to disjoint inputs — true, and insufficient.)

**The discriminator is the direction of the audit:**

> The **seat** is the reference reality that is *audited against*. A **face** is what is *audited*.
> The audit is **one-directional**. To classify any axis: does the verdict machinery treat it as
> the reference (other things checked against it → seat) or as the audited claim (checked against
> the reference → face)?

Witnessed in the engine (`audits/2026-06-16…/REPORT.md` §6; `probe_r3_presentation_vs_structure.pl`):

1. `cs_pattern`/`cs_classify` (cs_pattern_detection.pl:108–169) is a **pure function of the
   authored presentation labels** (`cs_kernel_codification`, `cs_authority_grounding`) and is
   **structurally blind to what binds** — re-presenting alone moves the pattern; varying the
   binding reality (beneficiary, suppression) does not.
2. The `cs_verdict` false-X layer (cs_pattern_detection.pl:192–249) **audits the presentation-
   pattern against** the binding reality, **directionally** — grounding-claim checked against the
   metric/beneficiary reality, **never the reverse**. The metric axis is the reference; the
   grounding axis is the audited claim.

⇒ The grounding/committer axis is the **showing-face of orientation**, not a second seat. The
metric/beneficiary axis is the seat.

This is exactly `seat-theorem-v1`'s **no-seat pose** made mechanical: `false_natural_law` fires
when a constraint claims self-enforcing / natural (no seat) while a beneficiary (a seat) is
present — "asserting content while denying a standpoint," the unique inconsistency, caught at the
showing-face.

---

## 3. The standing invariant and its kill-condition

v7 §4.5 (line 109, corrected text) gives the invariant in engine terms. The cross-axis surface
has two kinds:

- **(A) Data bridges** — committer-axis data feeding observer-axis **computation**. v7: **exactly
  one** — `influences` → `detect_necessity_inheritance` (entailment), forward (committer→observer).
- **(B) Read-only seam diagnostics** — committer-side consumers that **read** observer output and
  **feed nothing back**. v7: **at least three** (incl. the grounding-vs-structure diagnostic that
  reads `constraint_signature`). "(B) seams may multiply freely — a one-way read of observer
  output cannot couple the axes."

**Invariant (v8):** the seat is singular **iff the (A) data-bridge count stays 1 and the audit
stays one-directional** — orientation is audited against the seat, never the seat against
orientation.

**Standing kill-condition (the falsifier for any successor version):** one-seat **falls** the day a
version adds a **reverse (A)-type data bridge** — observer-axis *computation* that consumes
grounding/commitment fields to **override the metric verdict** (metrics ruled wrong *by*
grounding). That makes the audit bidirectional and the two axes co-equal **as seats**. It does
**not** exist in v7 or the engine today. *Precision:* this is **not** "any reverse read" — (B)
read-only seams that read observer output are permitted and plural; they cannot couple the axes.
The invariant is on the **(A) count and direction**, not on reads.

---

## 4. Vocabulary bridge (the collision, resolved)

| v8 term | `seat-theorem-v1` | v7 | engine locus |
|---|---|---|---|
| **seat** (the one content-position; reference reality audited against) | the seat (Coupling Thm) | the **ε-invariant content** (NOT v7's word "seat") | `constraint_signature` / `dr_type` content; ε is its load-bearing primitive |
| **gauge** (positions over the one content) | — (orthogonal to the law) | **v7's "seat"** — observer position U₁…U₄; "varies the seat" | observer orbit / `dr_type` across contexts; H¹ |
| **orientation** (relation to the seat; co-equal meter, two faces) | declared-vs-concealed (showing) + Corollary 3 honor-vs-reabsorb (keeping) | the **committer axis** | `cs_pattern`, `cs_axiom_engine`, `cs_drift_engine` |

**The load-bearing collision v8 fixes:** v7's "seat" = v8's **gauge** (an observer position);
v8's **seat** = v7's ε-content. Read across corpora without this table and the two "seats" get
miscounted as two content-seats — the exact error that produced the conversational two-seat
hypothesis. The v8 paper must carry this table.

---

## 5. Orientation is a co-equal *meter* (two structured faces), not a second *seat*

v7 Theorem 7 (Detection Independence, lines 61–85) proves the committer axis registers a failure
class the content-seat's cohomology (H¹) **cannot** — "computed from disjoint inputs … neither's
computation can see the other's." That makes orientation **co-equal as a meter** (its own anchor
Axiom 7, its own state-space, its own math), **not co-equal as a seat** (it is audited against the
seat, §2–3). The two readings are not in tension once "co-equal" is read as *meter*.

Orientation's two faces, each with engine structure:

- **Showing-face (external):** how the selection is presented; **audited against the seat,
  one-directional.** Healthy = declared-as-a-seat; pathological = posed-as-fact/natural.
  Engine: `cs_pattern` (the offered legitimation) + `cs_verdict` false-X (the pose-detector). v7
  **Theorem 8** (licensed-plurality `coexists_with` vs real-closure `forecloses`, lines 89–93) is
  showing-face *state structure*. = `seat-theorem-v1` declared-vs-concealed; the no-seat pose.
- **Keeping-face (internal/temporal):** whether the selection is retained vs updated; **drift over
  time to terminal attractors** (husk / axiom_foreclosure / extinction / revival / repudiation).
  Pathology = **unacknowledged drift** (`cs_drift_unacknowledged`). Engine: `cs_drift_engine`. =
  `seat-theorem-v1` **Corollary 3** (honor-vs-reabsorb under confrontation) turned into an
  authored field.

So orientation is not a tag on the seat; it is a structured, independently-metered relation to it,
which is precisely why v7 could promote it to "co-equal" while one-seat holds.

---

## 6. What the engine already realizes (⇒ small code delta)

The R3 probe (§2) and v7 §4.5 (§3) show the engine **already** implements one-seat / gauge /
orientation with a one-directional audit. v8's content is largely *re-description*, not behavior
change:
- the seat (ε-metric content), the gauge (observer orbit), and orientation (`cs_*`) are all built;
- the audit-direction asymmetry **is** v7 §4.5's existing (A)/(B) architecture;
- Theorems 7–8 already supply the co-equal-meter and showing-face-state-structure results.

The only thing not yet *enforced* as a fact is the §3 invariant itself (the (A)-bridge count is a
prose invariant in v7, not a guard). See §8.

---

## 7. The R1 cap — the seat's substrate-independence is the standing open work

The audit runs **presentation against an *authored* reality**, not against the world. v7 line 147
confesses this directly: ε is "the framework's least-grounded and most load-bearing primitive …
authored by judgment … not computed from anything beneath it." The whole observer axis is a
deterministic transform of ε; ε is hand-set. So:

- the **architecture** (audit-direction, one-seat) is sound and verified;
- the **substrate** the seat is built from is authored on both sides of the audit (the metric
  reality AND the grounding claim) — this is the **R1 / Killer-1** finding of the audit, here shown
  to be the *same* gap v7 already names.

v8 inherits this as **standing open work**, not a defect to paper over: the partial discipline v7
offers (ε-stability: §6, lines 147, 82–85, the rejected `hanbali` anchor) is the fix-*direction*,
not the fix. **Naming the seat's substrate-independence — a world-anchored ε, or an honest
declaration that there is none and the seat is itself declared (seat-theorem-v1 §8) — is the open
problem v8 hands forward.**

---

## 8. Implementation scope (for the downstream Claude Code plan)

**Read this before scoping.** v8 is a large *conceptual* step and a small *code* delta (§6). Do
**not** plan an engine rebuild. The plan should cover:

1. **Documentation / vocabulary migration (the bulk).** Introduce seat / gauge / orientation;
   carry the §4 bridge table; update cross-references in `seat-theorem-v1.md`,
   `design_discipline.md`, `metrics_as_routing`, and the memories that use "seat" loosely. Author
   `docs/deferential_realism_paper_v8.md` from this spec (or rename/extend v7 → v8 per operator
   choice).
2. **Promote the §3 invariant to a guard (the one genuinely new engineering artifact).** A check /
   test asserting the **(A) data-bridge count stays 1 and forward** — i.e. no observer-axis
   computation reads `cs_*` to override a metric verdict. This formalizes v7 §4.5's "the count of
   data bridges is the invariant that protects … Theorem 7's detection independence," and makes the
   v8 kill-condition mechanically checkable (a positive control: it must *fire* if a reverse bridge
   is introduced). Candidate home: a stack-consistency-style check or a `plunit` test over the
   cross-axis surface.
3. **Retire the conversational two-seat / lattice hypothesis** (in prose only — no file exists to
   delete; just ensure no doc asserts it).
4. **Fold v7 + the engine in as worked realization** — cite, do not re-derive; Axioms 1–6 /
   Theorems 1–4 / the observer engine are untouched (v7 line 13: "strictly additive").

**Out of scope / behavior-preserving:** no change to `classify_from_metrics`, the signature layer,
the contamination network, or any verdict threshold. If the plan proposes one, it has left v8's
remit (which is ontology + the one guard) and should be split out.

---

## 9. What review should pressure-test (open questions for the reviewer)

- **Q1 — Is the audit-direction discriminator (§2) really seat-complete?** It classifies the CS
  axis cleanly; does it classify *every* engine axis, or is there an axis that is neither audited
  nor audited-against (a genuine third role)? (Cross-check against the audit's Killer-2: the
  network layer is out-of-scope, not a third seat — confirm that holds under the v8 ontology.)
- **Q2 — Does the (A)-bridge invariant (§3) actually carry the full kill-condition,** or can a
  second seat sneak in through a (B) seam that is later *promoted* to feed computation? (i.e. is
  the A/B boundary itself stable under refactor?)
- **Q3 — R1/§7:** is "the seat is itself declared (no world-anchor)" an acceptable resting place
  (seat-theorem-v1 §8 says all framings are seated), or does v8 owe a world-anchoring attempt for
  ε before it can claim the architecture is *trustworthy* and not merely *sound*?
- **Q4 — Vocabulary:** adopt seat/gauge/orientation wholesale, or keep v7's "seat" with a
  disambiguating qualifier? (This spec recommends wholesale; the bridge table is the migration.)

---

## 10. Provenance and honest assessment

- **v7 citations verified** against `deferential_realism_paper_v7.md` 2026-06-16: Axiom 7 (l.51),
  Theorem 7 (ll.61–85), Theorem 8 (ll.89–93), §4.5 (A)/(B) bridges (l.109), the ε caveat (l.147),
  the `hanbali` self-falsification (l.151).
- **One correction folded in:** the originating ruling cited v7's *"exactly one bridge … nothing
  else"*; v7 l.109 disowns that phrasing. The corrected (A)/(B) distinction is used and is stronger
  for one-seat (no-disowned-name discipline applied).
- **The method note worth keeping** (v7 §6, l.151, and this conversation): the framework, the
  theory, and both author-sets have each run the same staking discipline and *lost* the productive
  round — v7's drafted Theorem 7 falsified by its own `hanbali` anchor; the audit's content-stake
  for R3 falsified by the presentation-vs-structure probe. v8 should preserve this as a property,
  not smooth it: the seat/face line is held by a falsifier (§3), not asserted.

**Net:** on the verified substrate, the engine architecture **votes one-seat** (orientation is the
audited face, the metric reality is the seat, the audit is one-directional). v8 states that
ontology, makes its boundary checkable (§3), and hands forward the one thing the substrate does
**not** settle: the seat's own substrate-independence (§7). That open problem is the seat's, and
it is declared — which is the only status seat-theorem-v1 permits any contentful claim to have.
