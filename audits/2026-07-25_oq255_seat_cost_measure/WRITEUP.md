# OQ-255 — The seat-cost measure: gate + grade, defined and controlled

**Date:** 2026-07-25. **Status of this audit:** complete — candidate measure delivered,
kinship verified against the classifier, kill condition answered. Adoption of the measure
into `docs/seat-theorem-v1.md` remains OQ-253's `blocked_on_human` ruling; nothing here
pre-empts it.

**Corpus/code state (cite this, not memory):** `outputs/pipeline_output.json` manifest
`pipeline_run_at: 2026-07-25T07:20:20Z`, `n_constraints: 199`, `code_commit_short: ee025a0`,
`code_dirty: true`. All counts below are **context-slots** (199 × 4 contexts = 796; totality
check passed, `evidence/census.json`). Types read from `per_constraint.perspectives` —
`per_constraint.classifications` is empty corpus-wide (the known OQ-148 regression; schema
inspected before concluding, not assumed).

---

## 1. What was asked

OQ-253's edit 3 replaces the interrogative type exemption with "a question's seat-cost is
what it structurally cannot return." The original kill condition was unfireable by identity;
OQ-255 was minted to define the measure as a **function from foreclosure-sets to a coarser
ordering** (proposed shape: gate + grade), verify the v8 §6.3 engine-kinship claim against
the actual classifier, and answer the gate/grade kill condition.

## 2. Kinship verification (the declared first step)

v8 §6.3 (`deferential_realism_paper_v8.md:803-805`) asserts: Q3's coordination-to-transfer
ratio ↔ the rope/snare/tangled_rope axis; Q5's rearrangement test ↔ mountain-versus-
arrangement. Verified against `classify_from_metrics/6` (`prolog/drl_core.pl:364-459`):

**Q5 ↔ `emerges_naturally`/`requires_active_enforcement`** (the maintenance pair,
`drl_core.pl:87-88`, delegating to authored `domain_priors` facts). The kinship is REAL but
**looser than the source instance's enumeration, in one direction**:

- *Confirmed:* `emerges_naturally` is **necessary** for mountain (`:372`) — Q5's "world
  would rearrange" answer (en=false) genuinely excludes mountain. Confirmed likewise: Q5
  cannot separate snare from tangled_rope (both live entirely in the en=false region on
  every gate path; census: tangled_rope 133/133 slots en=false, snare 179/187 en=false).
- *Refuted:* "Q5 partitions {mountain} against the rest." The rope gate carries an
  `emerges_naturally` **bypass disjunct** (`:423`), snare's gate never checks the flag
  (only `\+ nlwb`, `:391`, blockable by `agent_beneficiary` alone per `:334-337`), and
  scaffold checks neither. On the live corpus the en=true pole is inhabited by **five
  types**: mountain 6, rope 32, scaffold 18, snare 8, unknown 8 slots (witnesses:
  `architectural_pattern_validity` authors `emerges_naturally` and classifies rope;
  full table `evidence/census.json`). **Q5 is a one-sided excluder, not a certifier.**

**Q3 ↔ the χ-band + boolean pair.** Confirmed structurally: rope = χ ≤ 0.35 ceiling
(`:415-424`), tangled_rope = χ ∈ (0.35, 0.90] + `has_coordination_function` +
`has_asymmetric_extraction` (`:426-438`), snare = χ/ε/suppression floors (`:390-398`).
The coordination-to-transfer ratio IS this partition in engine form. Q3's foreclosure:
the maintenance boundary (en=true snares exist, 8 slots — same ratio, different origin),
theater/vitality (piton's content), temporality (scaffold's).

**Verdict on the OQ-253 addendum's conditional** ("if the kinship is looser, the
run-witness path collapses"): the kinship IS looser than the source's enumeration, but the
path does **not** collapse — it sharpens. Foreclosure-sets must be **computed from the gate
structure, not from the question's intuitive gloss**: the gloss overstated Q5's resolving
power, which is precisely the false-precision the measure exists to price. The
mis-enumeration is evidence *for* the measure.

## 3. The candidate measure

Relative to a token-set T (here the engine's: six core types + `naturalized` + `unknown`),
a terminator subset T⁺ (the battery-terminating verdicts: `mountain` = natural fact,
justified `rope` = cost is the price of a still-needed good), and a battery B:

- **foreclosure(Q)** = the set of distinctions {t, t′} over T that Q's answer-partition
  cannot separate. For engine-kin questions this is *computed* from the gate structure
  (§2); for non-kin questions (Q2 frame-rotation, Q4 missing-stakeholder) it is
  *hand-enumerated and declared* — the measure stays enumerable-not-felt either way, but
  only the kin subset is machine-checkable.
- **Gate(Q)** (binary, checkable): Q admits at least one **positively-shaped** answer that
  lands in T⁺. Positively-shaped = an authored/witnessed presence, not an exhausted search.
  This imports Build Discipline Pattern 5 into the measure: Q1/Q2/Q4's terminator-shaped
  answers ("found no payer / no suppressed vantage / no absent stakeholder") are negative
  existentials and do NOT pass; Q3 (a measured all-coordination ratio; χ ≤ 0 net-beneficiary
  cells are inhabited, e.g. `ability_ceiling_reading` institutional χ = −0.029), Q5 (authored
  `emerges_naturally`), and Q6 (an origin story that survives, reason still live) do pass.
  Battery-level gate: ∃ Q ∈ B with Gate(Q) = 1 — "the battery must admit at least one
  answer that terminates the battery," now mechanical.
- **Grade(Q)** = foreclosure(Q), ordered by **set inclusion** (a partial order). The
  coarsening to cardinality is rejected: the witnessed Q3/Q5 incomparability (§4) is real
  information a total order would destroy. (The choice of inclusion-order is itself a
  declared seat of the measure — recorded, contestable.)

## 4. The kill condition, answered: it does NOT fire

**Kill condition:** exhibit two questions that both pass the gate and whose grades differ,
where the grade difference changes which seat a practitioner selects. (If no such pair
exists, the grade is ornamental and the gate is the whole revision.)

**Exhibited pair: (Q3, Q5)** — chosen over the initially-planned (Q5, Q6) because Q6's
unique channel is authored-empty on the live corpus (§5); the exhibit must not ride an
empty table.

- Both pass the gate (§3).
- Grades differ, incomparably — each forecloses a distinction the other resolves, all
  four witness cells inhabited on the live corpus:
  - rope-vs-snare ∈ foreclosure(Q5) \ foreclosure(Q3): both types live in en=false
    (rope 147, snare 179 slots) where Q5 returns one answer; Q3's χ-band separates them.
  - the maintenance boundary ∈ foreclosure(Q3) \ foreclosure(Q5): en=true snares (8 slots)
    vs en=false snares return the same Q3 ratio; Q5 separates them.
- The difference changes seat selection: given candidate seats A = a maintained
  low-transfer rope story and B = a maintained high-transfer snare story (both cells
  inhabited; `ability_ceiling_reading` is itself snare-at-three-contexts /
  rope-at-institutional), a Q5-only practitioner gets identical answers on A and B and
  cannot rank them; a Q3-only practitioner selects B. The grade difference is the
  selection difference.

**Therefore the grade is load-bearing on the engine-kin subset; the pre-declared weaker
landing (type exemption survives as a gate) is NOT forced.** Scope declared: this is an
existence result over engine-token foreclosure-sets on the live corpus — it licenses "the
grade carries weight *here*," not "the grade carries weight for every question pair
everywhere."

## 5. Finding surfaced en route: Q6's unique engine channel is authored-empty post-reset

The genealogical guard's engine-kin is the dead-coordination piton path
(`coordination_dead/1`, `drl_core.pl:354-357`, reading authored
`narrative_ontology:coordination_vitality/2`) — "built for a problem that no longer
exists, yet persisting" is exactly `coordination_vitality(C, dead|degrading)`. That fact
is authored by **zero** stories on all five live legs AND `kernel_v1`; only the legacy
`original_json` archive authors it (grep positive control: the pattern fires there and on
the two engine files). All 32 live piton slots ride the theater fallback clause
(`:440-448`), not the dead-coordination clause (witness: `adjacency_reading`, no
vitality fact). The gate fails closed on the absence (falls through the priority chain),
so this is dormant-not-defective — but it means the one question the essay calls the
battery's guard maps to an engine surface generation stopped feeding at the 2026-06-05
reset. If Q6's drift-verdict is ever wanted as a measured quantity on the live corpus,
the generation frontier (`generate_constraint_pl.py` emit set) must re-emit
`coordination_vitality`, or the absence should be declared in `design_gaps.md`.

## 6. Controls run

1. **Probe schema control:** first census attempt read `per_constraint.classifications`
   and returned {} — recognized as the OQ-148 regression, schema inspected, probe rebuilt
   on `perspectives`. The empty read was NOT reported as "no types."
2. **Totality:** 796 slots = 199 × 4. Passed.
3. **Grep control ladder** for the `coordination_vitality` absence: live legs 0 →
   twins/kimi/sonnet 0 → kernel_v1 0 → whole-tree grep fires on `original_json` + engine
   files. The absence claim is scoped to where the probe was shown to fire.
4. **Witness facts checked at source:** `architectural_pattern_validity` (authored
   `emerges_naturally`, rope), `adjacency_reading` (no vitality fact, fallback piton).

## 7. What this hands to OQ-253

The measure is adjudicable: gate is mechanical (and strictly stronger than the doc's
virtue-requirement), grade is witnessed load-bearing, and the type exemption's honest
replacement is graded-with-a-positively-shaped-gate rather than gate-only. The residue the
operator still owes a ruling on (OQ-253): adopt/reject the measure, and the three-site
sweep decision for the exemption sentence.

---

## 8. Same-day addendum: §5's inference CORRECTED; the discriminator run; Gate v1.1

**Trigger:** the source instance read §5 as "the guard has one live carrier (Q3) and it's
the wrong class" and specified the discriminating check: *on a case where Q3 and Q6 should
diverge, can Q3's positive pole return the still-needed-good verdict Q6 would have
returned?* Pre-registered meanings: can → the doc's structural claim about cost-finding
questions was too strong; can't → the guard is unmanned.

**The correction first (set-level claim re-witnessed and falsified).** §5's *facts* stand
— `coordination_vitality/2` is authored-empty on all live legs, the dead-coordination
piton clause is dormant. §5's *inference* ("Q6's unique engine channel is authored-empty")
was WRONG: I identified the wrong channel. Q6's genealogical kin is
**`founding_problem_status/2`** (live/dead/contested — literally "is the founding reason
still live?"), authored on **164/199** live-leg stories (89 contested / 14 dead / 61
live), near-total on the kimi/sonnet legs, with live consumers — including the wired
Q5×Q6 conjunction `has_mandatrophy_declaration(C) :- founding_problem_status(C, dead),
disappearance_verdict(C, world_rearranges)` (`narrative_ontology.pl:168-170`, the OQ-83
R5 rewire). `disappearance_verdict/2` (Q5's kin, richer than `emerges_naturally`) is
likewise authored 165/199 (119 world_rearranges / 33 contested / 13 world_unchanged).

**The discriminator, run** (join in `evidence/`; same manifest 2026-07-25T07:20:20Z,
n=199, ee025a0):

- The diverging cell — cost-present × reason-still-live — is INHABITED: tangled_rope ×
  fps=live 18 slots, snare × live 43, naturalized × live 2 (witness:
  `alignment_constraint_narrowing`). Full type × fps table in `evidence/census.json`
  (fps join added).
- Q3's answer on that cell is a non-terminator **by construction**: every cost-present
  cell requires χ above the rope ceiling and/or `has_asymmetric_extraction`
  (`drl_core.pl:426-438`, `:390-398`), which ejects it from Q3's all-coordination
  terminator cell. Q3 sees the cost and structurally cannot say what the cost is buying.
- Q6's verdict on that cell is returnable AND authored: `founding_problem_status(C, live)`
  on those very stories.

**Verdict: a third branch neither pre-registered option anticipated.** The doc's
structural claim about cost-finding questions STANDS (Q3 cannot do the guard's job on the
diverging case) — and the guard is NOT unmanned: it is manned by `founding_problem_status`.
The "one live carrier, wrong class" reading was built on §5's channel misidentification.
What §5 correctly found demotes to: the *dead-coordination piton clause specifically*
rides a dormant surface; the genealogical guard itself rides fps and is alive.

**Gate v1.1 (refinement worth keeping even though the alarm dissolved).** The source's
Pattern-6 worry was right about Gate v1 as an instrument: "∃Q admitting a positively-shaped
terminator anywhere" is satisfiable by Q3 alone, and would emit "battery guarded"
identically whether the genealogical channel carried 199 values or zero. Refinement:
**Gate v1.1 — the battery must contain a question that can positively return a terminator
ON COST-PRESENT CASES** (the diverging region, where termination is informative). Only
genealogical-class questions pass v1.1 (Q3 provably fails it; Q5 fails it — a one-sided
excluder certifies nothing); live batteries pass it through Q6 via fps. The gate now
measures guard-*function*, not terminator-*admission*.
