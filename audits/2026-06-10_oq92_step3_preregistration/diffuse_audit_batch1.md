# Diffuse audit — first Stage-C batch (gfbatch1, 2026-06-10)

**Criterion applied:** as RULED pre-batch (PREREGISTRATION.md Q1 block) — a `diffuse` claim
fails if the story artifact itself identifies a capturing seat; artifact-decidable only;
outside-information cases pass (the priced detectability limit).

**Batch:** 6 stories, topic "municipal parking permit allocation", run-tag `gfbatch1`
(`json/gfbatch1/`, compiled `prolog/testsets/gfbatch1/`), generated via the Stage-C prompt
(commit `58550bc2`).

**Result: 0/0 observed — VACUOUS PASS, stated as vacuous.** Zero `diffuse` claims were
authored; the audit's subject set is empty. K=0 is formally satisfied; nothing was checked,
and this line says so (Pattern-5/6 discipline: an empty subject set is reported as empty,
never as "clean").

**Receipt-surface authorship census (the non-vacuous facts):**
- 6/6 stories author BOTH `gain_flow` and `fixing_cost`; 0 omit; 0 author `diffuse`.
- 6/6 named receivers verify against their own `stakeholders[]` (referential integrity also
  enforced at compile — all six compiled clean with `stakeholder_gain_flow/2` +
  `fixing_cost_class/2` facts emitted).
- Receiver roles: 3 beneficiary-seats, 3 agenda_setter-seats. `fixing_cost`: 4 cheap, 2
  prohibitive — both fields vary, neither is a constant template token.

**Prevalence flag (OQ-70, inverse direction):** 6/6 named-capture with 0 diffuse and 0
omitted smells like authoring convention — the prompt's "diffuse is an affirmative claim,
check every seat first" may make naming a seat the path of least resistance. Recorded as
authoring-convention-until-checked per the standing rule. Risk asymmetry: a false NAMED
capture over-BLOCKS benignity certification (fail-toward-suspicion) rather than preserving it
— it does not threaten the gates the K=0 audit protects; it matters for the piton side
(OQ-90, uncaptured-dependent) where a diffuse-starved corpus would leave piton_candidate
unreachable. Flagged for the OQ-90 build, not a Stage-D blocker.

**Gate verdict: Stage D unblocked** (audit ran against the pre-written criterion; halt
condition not met; the vacuity and the prevalence flag travel with this record).
