# v8 Design Spec — Seat / Gauge / Orientation (the worked one-seat ontology)

**Status:** DESIGN SPEC (draft for review), **rev2** — incorporates the first review pass. Not the
v8 paper, the precursor to it.
**Authored:** 2026-06-16 (Claude Code), from the seat/orientation invariant audit + the R3
presentation-vs-structure probe (`audits/2026-06-16_seat_invariant_vs_prolog/`); rev2 folds in the
reviewer's taint-property catch and three sharpenings (see §10).
**Intended handoff:** review by the originating reasoning instance → a new Claude Code instance
drafts an implementation plan from §8.
**Supersedes:** nothing on disk yet — it *unifies* three existing artifacts under one vocabulary
(see §0). It does **not** retire `docs/seat-theorem-v1.md`; it operationalizes it.

> **Provenance discipline.** Every v7 claim below is cited to a verified line in
> `docs/deferential_realism_paper_v7.md` (checked 2026-06-16). The invariant (§3) is a **two-part
> taint property**, not a count: v7 line 109 carries *two* "nothing else"s — a disowned
> total-surface one and a surviving payload one — and the live guard is the surviving payload claim
> (the one (A) bridge carries entailment "and nothing else"). Rev1 dropped both and stated a count;
> rev2 restates the taint property (§3/§8.1, §10). The discriminator (§2) is scoped to the
> per-constraint surface; the ε world-anchor question is settled by the Coupling Theorem (§7).

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

**Declared scope limit (stated here, where it can cost — not parked in review).** The
audit-direction discriminator is complete for the **per-constraint surface**: any axis that makes a
claim *about one constraint* is either audited or audited-against, hence either face or seat. It does
**not** range over the **relational network layer** (`affects_constraint` and the contamination
network) — relations *between* constraints are a different *kind* of object, neither audited nor
audited-against a single seat. That is why the audit's Killer-2 routed `cs_drift_mismatch`'s network
conjunct out of scope: not "not a third seat," but "not the kind of thing the per-constraint seat/face
distinction classifies at all." v8's ontology is scoped to the per-constraint surface; the network
layer is a declared *exterior*, not a candidate third role inside it. (Declared up front so it cannot
later be invoked only to dismiss an inconvenient axis.)

This is exactly `seat-theorem-v1`'s **no-seat pose** made mechanical: `false_natural_law` fires
when a constraint claims self-enforcing / natural (no seat) while a beneficiary (a seat) is
present — "asserting content while denying a standpoint," the unique inconsistency, caught at the
showing-face.

---

## 3. The standing invariant and its kill-condition

v7 §4.5 (line 109) gives the engine-level structure. The cross-axis surface has two kinds:

- **(A) Data bridges** — committer-axis data feeding observer-axis **computation**. v7: **exactly
  one** — `influences` → `detect_necessity_inheritance`, "carrying entailment … **and nothing
  else**," forward (committer→observer).
- **(B) Read-only seam diagnostics** — committer-side consumers that **read** observer output and
  **feed nothing back**. v7: **at least three** (incl. the grounding-vs-structure diagnostic that
  reads `constraint_signature`). "(B) seams may multiply freely — a one-way read of observer
  output cannot couple the axes."

**The invariant is a taint property, not a count — and trusting the count is exactly the
distrust-the-aggregate failure.** v7 line 109 carries *two* "nothing else"s, and v8 needs the
surviving one. The **disowned** "nothing else" was the *total-surface* claim ("exactly one
intentional bridge … and nothing else") — correctly dropped, it wrongly excluded the plural (B)
seams. The **surviving** "nothing else" is the *payload* claim: the one (A) bridge carries
**entailment-typed content only**. v7 itself states the invariant as "the count of data bridges"
and leaves the payload constraint in prose — so the count is the aggregate, the payload-type is the
witness, and an earlier draft of this very spec trusted the aggregate. The complete invariant is
**two-part**:

> **(i)** committer→observer *computation dependency* = exactly **one** bridge (count + forward
> direction); **AND**
> **(ii)** that one bridge's **payload is entailment-typed only** — no grounding / foreclosure /
> drift content crosses into observer computation.

**Standing kill-condition (the falsifier for any successor version) — now with a count-silent
path:** one-seat **falls** if *either* (a) a **reverse (A)-type data bridge** appears (observer
computation consuming grounding to **override the metric verdict** — the audit-reversal path), *or*
(b) **`influences` widens its payload** to carry grounding / foreclosure / drift content that
observer computation consumes — **the count stays 1, no reverse bridge is added, no metric verdict
need be overridden, yet committer-commitment data now flows into observer computation,** precisely
the coupling Theorem 7 forbids. Path (b) is invisible to a count check; it is the hole the payload
taint property closes. *Precision (unchanged):* this is **not** "any reverse read" — (B) read-only
seams are permitted and plural and cannot couple the axes. The invariant is on the **(A) dependency
and its payload-type**, not on reads. (This two-part form sharpens v7, which states only the count.)

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
seat, §2–3). The precise shape — not "no tension" — is **independent-object-plus-subordinate-audit**:
orientation has its *own* object and empirical anchor (Axiom 7), *and* a subordinate audit-direction
(its showing-face is checked against the seat, never the reverse). Both halves are real; "co-equal
as a meter, audited as a face" is the non-rounded statement.

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

The only thing not yet *enforced* as a fact is the §3 invariant itself (a prose invariant in v7,
not a guard). See §8.

**This is also where v8 risks becoming unfalsifiable, so read it carefully:** if "theory = current
engine," the engine cannot vote *against* the theory and v8 collapses to a tautology. The only two
things that keep v8 falsifiable are the **§3 kill-condition** (a concrete future configuration that
would refute one-seat) and the **§7 open problem** (the seat's substrate-independence, unsolved).
Re-description is the honest bulk of the *work*; the kill-condition and the open problem are the
entire *epistemic spine*. §8 is ordered by that, not by effort.

---

## 7. The R1 cap — the seat's substrate-independence (a declaration discipline, not a world-anchor)

The audit runs **presentation against an *authored* reality**, not against the world. v7 line 147
confesses this directly: ε is "the framework's least-grounded and most load-bearing primitive …
authored by judgment … not computed from anything beneath it." The whole observer axis is a
deterministic transform of ε; ε is hand-set. So:

- the **architecture** (audit-direction, one-seat) is sound and verified;
- the **substrate** the seat is built from is authored on both sides of the audit (the metric
  reality AND the grounding claim) — this is the **R1 / Killer-1** finding of the audit, here shown
  to be the *same* gap v7 already names.

**The Coupling Theorem already settles the in-principle question — it is not the operator's
discretionary call.** "World-anchor ε *or* admit the seat is declared" is a false choice: a
world-anchored ε would be a *seat-free seat*, which the Coupling Theorem forbids (a contentful
verdict is seated; ε is contentful). So the declared-seat resting place is not a fallback — it is
the **only law-consistent answer**, and v8 owes **no world-anchoring attempt** (that is the
impossible horn). What v8 *does* owe is a **declaration discipline for ε**: provenance (who/what
authored it from which reading), and ε-stability (v7 §6, lines 147, 82–85, the rejected `hanbali`
anchor). This is **the showing-face applied to the framework's own primitive** — and it closes the
recursion cleanly: *the no-seat pose the framework detects in its constraints is one the framework
must not strike about its own ε.* So §7's open problem converts from "find a world-anchor
(impossible)" to "**build ε's declaration discipline (tractable)**." That is the standing open work
v8 hands forward — a design task, not an in-principle one.

---

## 8. Implementation scope (for the downstream Claude Code plan)

**Read this before scoping.** v8 is a large *conceptual* step and a small *code* delta (§6). Do
**not** plan an engine rebuild. **Effort-bulk is not value-bulk** — the items below are ordered by
*epistemic load*, not by lines of work. Item 1 is small and load-bearing (it is what keeps one-seat
falsifiable, §6); item 4 is large and low-stakes. Plan accordingly, and do **not** optimize for the
migration at the guard's expense.

1. **[LOAD-BEARING — priority 1] Promote the §3 *two-part* invariant to a guard.** Not a count
   check — a **dataflow / taint check on the (A) bridge's payload**: assert (i) committer→observer
   computation dependency = exactly one bridge, forward, **and** (ii) that bridge (`influences` →
   `detect_necessity_inheritance`) carries **entailment-typed content only** — no grounding /
   foreclosure / drift field reaches observer computation. **Positive control (required): inject a
   grounding field into the `influences` payload and confirm the guard *fires*** — a count check
   would pass this injection (count still 1), which is exactly the §3 path-(b) hole. This is the one
   genuinely new engineering artifact and the entire reason v8 is falsifiable rather than tautological
   (§6). Candidate home: a stack-consistency-style check or a `plunit` test over the cross-axis
   surface payloads, not just the edge count.
2. **Fold v7 + the engine in as worked realization** — cite, do not re-derive; Axioms 1–6 /
   Theorems 1–4 / the observer engine are untouched (v7 line 13: "strictly additive").
3. **Retire the conversational two-seat / lattice hypothesis** (in prose only — no file exists to
   delete; just ensure no doc asserts it).
4. **[LOW-STAKES BULK] Documentation / vocabulary migration.** Introduce seat / gauge / orientation;
   carry the §4 bridge table; update cross-references in `seat-theorem-v1.md`,
   `design_discipline.md`, `metrics_as_routing`, and the memories that use "seat" loosely. Author
   `docs/deferential_realism_paper_v8.md` from this spec (or rename/extend v7 → v8 per operator
   choice). This is the most *work* and the least *stakes*; finish it after the guard, not before.

**Out of scope / behavior-preserving:** no change to `classify_from_metrics`, the signature layer,
the contamination network, or any verdict threshold. If the plan proposes one, it has left v8's
remit (ontology + the one guard) and should be split out.

---

## 9. What review should pressure-test (open questions for the reviewer)

*(Two questions from the prior draft are now resolved in the body: discriminator completeness is
declared as a scoped limit in §2; the ε world-anchor question is settled by the Coupling Theorem in
§7. They are no longer open.)*

- **Q2 — Is the A/B boundary stable under refactor, and does the taint guard (§3/§8.2) actually
  catch payload widening?** Two sub-threats: (a) a (B) read-only seam later *promoted* to feed
  computation (becomes an unwatched (A) bridge); (b) the existing `influences` (A) bridge widening
  its payload past entailment. The §8.1 guard is specified to catch (b) via the injection positive
  control — pressure-test whether it also catches (a), and whether "entailment-typed only" is
  decidable as a static check or needs a dataflow trace.
- **Q4 — Vocabulary:** adopt seat/gauge/orientation wholesale, or keep v7's "seat" with a
  disambiguating qualifier? (This spec recommends wholesale; the bridge table is the migration.)

---

## 10. Provenance and honest assessment

- **v7 citations verified** against `deferential_realism_paper_v7.md` 2026-06-16: Axiom 7 (l.51),
  Theorem 7 (ll.61–85), Theorem 8 (ll.89–93), §4.5 (A)/(B) bridges (l.109), the ε caveat (l.147),
  the `hanbali` self-falsification (l.151).
- **Two corrections, in two directions (rev1 → rev2):** *(1)* The originating ruling cited v7's
  *"exactly one bridge … nothing else"*; v7 l.109 disowns *that* phrasing (the total-surface claim),
  so rev1 dropped it for the (A)/(B) distinction. *(2)* But l.109 carries a **second** "nothing
  else" — the *payload* claim (the one (A) bridge carries "entailment … and nothing else"), which
  **survives** and is the actual content-guard. Rev1 dropped both and stated the invariant as a
  **count**; the reviewer caught this (the distrust-the-aggregate failure, in the spec's own
  vocabulary), and rev2's §3/§8.1 restate it as the **two-part taint property** with a payload
  injection positive control. The correction made in (1) is precisely what opened the gap closed in
  (2) — recorded as such.
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
