# σ/seat field classification — PRE-REGISTERED PREDICTION (freezes at this commit)

**Sources consulted for the classification: `docs/seat-theorem-v1.md` (v2.4) and
`schemas/constraint_story_schema.json` at this commit — NOTHING ELSE.** Epistemic
precedence per the operator's sequencing note: this pass derives from the theorem text and
the schema file and cites only those. Temporal precedence: no replicate draw exists;
freeze-at-first-commit (OQ-114 pattern). A field the theorem underdetermines is
halt-and-escalate — none was hit; the closest call (measurements) carries its derivation
below.

**Contamination disclosure (known-in-advance fields — hits on these may NOT count as blind
confirmations):** the classifying instance carries, from public record and this session:
(a) `shift`/mechanism-class draw-instability on the naming triple (public, CLAUDE.md +
the signature-identity witness); (b) OQ-26 — ε is generated, not observer-invariant
(public since the v6.13.1 amendment); (c) press/Reformation naming drift —
ids/scopings vary per run (public). Consequence: `header.constraint_id`,
`base_properties.claimed_type`, and `base_properties.extractiveness` are flagged
KNOWN-IN-ADVANCE below. ε receives dual treatment: the theorem+schema derivation says σ;
the public record says seat — both recorded so the eventual table cannot award a blind hit
either way.

## Declared seed spec (the driver is HELD to this; deviation = halt, fields silently
## changing buckets is the failure this declaration exists to prevent)

The regen driver hands the model, per story: (1) the topic/title material
(`human_readable`, `topic_domain`), (2) a one-paragraph situation summary derived from the
archived `commentary.narrative_context`, (3) nothing else from the archived story — no
metrics, no types, no stakeholder lists, no six-questions content, no omegas. Mechanical
stamps (`provenance`, `seeded_from`, `draw`) are driver-written, not model-generated.

**Seed-supplied bucket** (stability = input echo, never σ evidence):
`base_properties.human_readable`, `base_properties.topic_domain`, and the situation-summary
content insofar as it echoes into `commentary.narrative_context`.

## Out of scope (not verdicts on σ — the theorem's partition does not apply)

`header.*` (constraint_id is an artifact label — noted KNOWN-IN-ADVANCE unstable; version,
generated_date, status are bookkeeping), `provenance` + `seeded_from`/`draw`
(driver-stamped), `uke_scope` (manifest passthrough), and all free-prose fields
(`commentary.*` bodies, `stakeholders[].situation`, `*_rationale`/`*_corroboration` text)
— prose is trivially draw-variable; the table evaluates structured fields only. Prose
fields contribute only via their structured derivates (e.g. stakeholder role multisets).

## The prediction (three buckets, derived)

**Derivation rule used throughout:** P1 — a property OF the situation has its value fixed
by it → predicted-σ. Coupling Theorem — anything that is a VERDICT on an open question
(the schema's own enums of assessment) is contentful, hence seat-dependent → predicted-seat.
Authorial selection (which/how many of an open-ended set to author) is an index of
evaluation → predicted-seat.

| field | bucket | derivation |
|---|---|---|
| `base_properties.suppression` | **σ** | P1: coercion the situation exerts |
| `base_properties.theater_ratio` | **σ** | P1: performative/functional ratio of the operation |
| `base_properties.accessibility_collapse` | **σ** | P1: how completely alternatives collapsed |
| `base_properties.resistance` | **σ** | P1: resistance the constraint actually meets |
| `base_properties.extractiveness` | **σ** by theorem+schema (schema: "how much the constraint extracts") — **KNOWN-IN-ADVANCE public record says seat (OQ-26)**; dual-recorded, no blind credit either way | P1 vs public record |
| `base_properties.requires_active_enforcement` | **σ** | P1: enforcement is or is not required |
| `base_properties.emerges_naturally` | **σ** | P1: naturality of origin |
| `base_properties.has_sunset_clause` | **σ** | P1: the clause exists or not |
| `base_properties.beneficiaries[]` / `victims[]` | **σ** (set/cardinality level; compare name-normalized — naming morphology is artifact-label territory) | P1: who collects / who pays are facts of σ |
| `base_properties.vindicated_propositions[]` | **σ** (weak) | P1: what the operation vindicates |
| `base_properties.claimed_type` | **seat** — KNOWN-IN-ADVANCE | schema text: "the type you BELIEVE is structurally true" — a verdict; Coupling Theorem |
| `stakeholders[]` roster + `role` (+ `secondary_role`, `agent`) | **σ** (roster cardinality + role multiset) | P1: the scenario contains the agents it contains; role = structural relationship |
| `stakeholders[].power/time_horizon/exit_options/spatial_scope` | **σ** | P1: structural position of a real actor |
| `six_questions.coordination_function` / `transfer_function` | **σ** | P1 + schema: "stated without evaluation" / what it moves, from whom to whom |
| `six_questions.absent_voices` | **σ** (structured derivate: pairs with excluded seats) | P1: who is not in the conversation |
| `six_questions.disappearance_verdict` | **seat** | the schema names it a VERDICT; enum includes `contested`; Coupling Theorem |
| `six_questions.founding_problem` | **σ** | P1: what it was built to solve (historical) |
| `six_questions.founding_problem_status` | **seat** | live/dead/contested is an assessment-verdict on an open question; Coupling Theorem |
| `six_questions.founding_problem_corroboration` | **σ** (structured derivate: outside-attestation exists or not) | P1: who attests is a fact |
| `gain_flow` | **σ** | schema/prompt: "facts about the situation, not classifications" |
| `fixing_cost` | **σ** (weak — cost class of a counterfactual act, but schema files it as fact) | same |
| `omegas[]` (count + ids + type_class) | **seat** | which uncertainties the author finds open is an index of evaluation |
| `measurements[]` (presence, metric selection, grid shape, point count) | **seat** | authorial modeling selection over an open-ended set; closest call — series VALUES are σ-anchored at endpoints (they must agree with base_properties), so the table should report shape-stability and endpoint-stability separately |
| `interval` | **seat** | arbitrary modeling units |
| `boltzmann` / `network` / `directionality_overrides` / `cs_structure` / `coercion_grid` (presence) | **seat** | optional-apparatus selection |

## Survival criterion (restating the pin, ISSUES.md OQ-109 item 4)

The σ/seat synthesis survives iff the observed stability partition over replicate draws
matches this predicted split beyond chance ON THE NON-SEED-SUPPLIED, IN-SCOPE fields, with
positive-agreement and agreement-in-absence reported separately. The noise hypothesis
predicts instability tracking field entropy / prose-proximity and smearing across the
σ/seat line. A mismatch is a finding about where the seat boundary sits — never a license
to redraw predictions. KNOWN-IN-ADVANCE fields (`claimed_type`, `extractiveness`,
`constraint_id`) are reported but earn no blind-confirmation credit.
