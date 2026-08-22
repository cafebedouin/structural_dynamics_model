% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Norm Climb: Bottom-Up Adoption Then State Coordination
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint embodies the endogenous-climb reading of a contested
 *   kernel about how new norms gain legitimacy during historical transitions.
 *   In this reading, norms achieve legitimacy through bottom-up voluntary
 *   adoption by populations and cultural carriers before — and sometimes
 *   independently of — state recognition. The state's role is coordination
 *   and formalization, not imposition. Extractiveness is low (0.15) because
 *   the constraint operates through alignment of incentives and cultural
 *   logic, not through coercion. Suppression is minimal (0.12) because
 *   adoption is self-reinforcing once critical mass is reached. Theater ratio
 *   is very low (0.08) because the state's enforcement activity is genuine
 *   coordination of an already-established practice, not theatrical
 *   maintenance of a degraded function. This stands in contrast to the
 *   exogenous-override reading (where state mandates precede adoption and
 *   extraction is high) and the hybrid reading (where symbolic authority
 *   transfer and institutional incentives combine).
 *
 * KEY AGENTS:
 *   - norm_beneficiary_populations: Early and broad adopters; genuine beneficiaries of coordination
 *   - cultural_carriers: Status-bearing modelers whose adoption drives the climb
 *   - state_administrative_apparatus: Ratifier and formalizer of emergent consensus
 *   - marginal_dissenters: Structurally excluded holdouts
 *   - historical_analyst: Observer of adoption timelines and causal order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.15).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Norm Climb: Bottom-Up Adoption Then State Coordination").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, 'a0f4a8c6-0147-4847-832a-3f0a13e5f238').
narrative_ontology:cs_kernel_codification('a0f4a8c6-0147-4847-832a-3f0a13e5f238', distributed).
narrative_ontology:cs_authority_grounding('a0f4a8c6-0147-4847-832a-3f0a13e5f238', practice).
narrative_ontology:cs_interpretation_layer_present('a0f4a8c6-0147-4847-832a-3f0a13e5f238').
narrative_ontology:cs_reading_relation('a0f4a8c6-0147-4847-832a-3f0a13e5f238', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0f4a8c6-0147-4847-832a-3f0a13e5f238', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('a0f4a8c6-0147-4847-832a-3f0a13e5f238', foundational, voluntary_adoption_precedes_state_mandate).
narrative_ontology:cs_axiom_status(voluntary_adoption_precedes_state_mandate, holdable).
narrative_ontology:cs_axiom_grounding('a0f4a8c6-0147-4847-832a-3f0a13e5f238', voluntary_adoption_precedes_state_mandate, empirically_contingent).
narrative_ontology:cs_axiom('a0f4a8c6-0147-4847-832a-3f0a13e5f238', foundational, coordination_problem_solved_by_emergent_practice).
narrative_ontology:cs_axiom_status(coordination_problem_solved_by_emergent_practice, holdable).
narrative_ontology:cs_axiom_grounding('a0f4a8c6-0147-4847-832a-3f0a13e5f238', coordination_problem_solved_by_emergent_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('a0f4a8c6-0147-4847-832a-3f0a13e5f238', norm_emergence_without_state_enforcement).
narrative_ontology:cs_drift_state('a0f4a8c6-0147-4847-832a-3f0a13e5f238', post_state_formalization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('a0f4a8c6-0147-4847-832a-3f0a13e5f238', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_beneficiary_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, cultural_carriers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Early adopters and the broad populations that voluntarily embrace new norms because they solve coordination problems or align with emergent values. They benefit from the norm's existence and participate in its spread. The state's later recognition formalizes and stabilizes what they have already chosen.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_beneficiary_populations, beneficiary,
    organized, generational, mobile, national).

% Intellectuals, merchants, clergy, and other status-bearing agents who model the new norm and influence peer adoption. They are neither coerced nor incentivized by the state; their adoption and advocacy drive the climb itself. They benefit from the norm's legitimacy and social prestige.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, cultural_carriers, beneficiary,
    moderate, generational, mobile, national).

% Observes that a norm has already achieved broad adoption, recognizes its coordination value, and formally recognizes or codifies it into law or administrative practice. Acts as a coordinator and ratifier of emergent consensus rather than as an imposer or enforcer. Legitimacy flows from following popular practice, not from imposing from above.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Holdouts and communities that do not adopt the norm even after state recognition. They are not central to the constraint's operation; their non-adoption does not destabilize the norm because adoption was endogenous and remains voluntary for them. Their voices would object to universalization, but they are structurally marginalized in the legitimacy narrative.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, marginal_dissenters, excluded,
    powerless, biographical, constrained, local).

% Examines the constraint from outside the historical moment: observes adoption timelines, measures resistance, reconstructs the causal order (climb before mandate or mandate before climb), and assesses whether state coordination followed cultural legitimacy or preceded it.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__endogenous_climb_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem by establishing a shared practice that coordinates behavior across a population. The norm reduces transaction costs, resolves ambiguity about acceptable conduct, or aligns individual incentives with group welfare. Populations voluntarily adopt because the coordination benefit is real and immediate.
% TRANSFER_FUNCTION: No systematic transfer from losers to winners. Beneficiaries are those who adopt and benefit from coordination; the state formalizes what they have already chosen. The constraint itself does not move resources from one party to another — it stabilizes a practice that emerges endogenously and carries distributed gains.
% ABSENT_VOICES: Marginal dissenters and communities whose practices conflict with the norm are excluded from the consensus narrative. They would object to characterizing the norm as universal; but their objection is not part of the legitimacy story because adoption outpaced them. Institutional competitors (rival authority structures, alternative norm-carriers) may also be absent from the formal record.
% DISAPPEARANCE_RATIONALE: If the norm and its state recognition vanished, populations would likely re-establish it because the coordination problem it solved remains live. The state's formal codification accelerated stabilization and reduced recurrence costs, but the norm's deep roots are in endogenous adoption — removal of state recognition would not eliminate the practice, only its legal standing.
% FOUNDING_PROBLEM: A coordination problem requiring shared practice — a boundary of acceptable conduct, a shared ritual, a common measure, or a behavioral standard that groups need to solve interaction problems at scale. The problem exists whether or not any single authority recognizes it.
% FOUNDING_PROBLEM_CORROBORATION: Historical and ethnographic evidence confirms that the norm spread through voluntary adoption networks BEFORE state recognition (e.g., merchant adoption of a measure before metrication law, linguistic norm adoption before language policy, hygiene practice adoption before public health mandate). Scholars outside the state apparatus attest the climb preceded the mandate in multiple historical cases (Weber on authority types; Kuran on preference falsification; Sunstein on norm cascades in the absence of central enforcement).
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness remains low across the interval (0.08→0.15) because the norm's persistence depends on voluntary continued adoption, not on enforcement machinery. The constraint lacks the suppression signature of a snare (high suppression, high resistance) and the theater signature of a piton (high theater ratio, low resistance despite performance). Measurements show suppression rising slightly (0.06→0.12) as marginal dissenters experience pressure to conform, but this pressure is social rather than state-administered — it emerges from the coordinated behavior itself. Theater remains flat and minimal (0.04→0.08) because the state's formal recognition of the norm does not require ongoing performance; it is a one-time ratification. The small rise in extractiveness over time reflects modest enforcement costs as the state institutionalizes the norm (legal codification, administrative machinery) — costs that are low relative to snare/tangled-rope constraints and that beneficiaries absorb without serious resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, the constraint is pure coordination: it observes a norm that has already achieved social consensus and formalizes it to reduce transaction costs and stabilize expectations. From the perspective of marginal dissenters, the constraint is extractive — the state amplifies majority practice into universal law, overriding local alternatives. Cultural carriers see the state's formalization as validation of their own modeling; they experience low extraction. Early adopter populations see low extraction because they initiated the practice and benefit from its stabilization. The divergence is structural: the constraint carries different directionalities for different agents depending on whether they were early adopters (low d, low effective extraction) or late resisters (higher d, higher felt pressure). The engine's per-seat computation captures this.
 *
 * DIRECTIONALITY LOGIC:
 *   Norm-beneficiary populations: these are the primary beneficiaries. They initiated adoption (low d toward beneficiary end); their exit options are mobile (they could revert to non-norm conduct if it proved harmful, but rarely do after critical mass). Directionality is near the beneficiary end (~0.15-0.25). Cultural carriers: also beneficiaries; their status rises with adoption of the norm they model. Directionality near beneficiary end (~0.20-0.30). State apparatus: acts as coordinator and formalizer, not as the primary beneficiary. It collects modest legitimacy gains from recognizing consensus but bears administrative costs. Directionality near symmetric (~0.45-0.55). Marginal dissenters: bear the cost of universalization; their exit is constrained (identity-locked into rejected practices or geographically local, making exit costly). Directionality toward target end (~0.70-0.85), but their small number and peripheral status means the constraint's effective extraction measured globally remains low.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy signature. The founding problem (need for coordination) remains live and the norm's formal recognition continues to solve it. The state's mandate followed rather than preceded popular acceptance, so there is no zombie-like persistence of a dead mandate. Theater ratio stays low, indicating the state's enforcement is functional coordination, not theatrical maintenance. A mandatrophy reading would emerge if the state continued to enforce the norm after it had lost voluntary adoption — if populations reverted to non-norm conduct but the state maintained legal penalties for this reversion. The measurement series would show extraction remaining high even as resistance fell (false enforcement of a dead norm). This constraint does not exhibit that pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_order_ambiguity,
    'Did state recognition truly follow popular adoption, or did state promotion precede and drive adoption, with retrospective narratives misremembering the causal order?',
    'Documentary evidence of adoption timelines: newspaper adoption, merchant record adoption, linguistic attestation, artifact production dates relative to legislation dates. Cross-cultural comparison of norm-adoption sequences in cases where documentary precision is high.',
    'If documentation shows state action preceded adoption (even if the state framed it as ratification), this reading transforms into the hybrid or exogenous reading; effective extraction rises as the state''s role shifts from coordinator to initiator. If adoption predates state recognition by clear margins (years or decades), the endogenous-climb reading holds and extraction stays low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_order_ambiguity, empirical, 'Whether state recognition followed or preceded popular adoption.').

omega_variable(
    voluntary_adoption_vs_cascading_conformity,
    'Did populations adopt norms because they genuinely perceived coordination benefits, or because they conformally adopted them after seeing others adopt, with no independent benefit assessment?',
    'Ethnographic work on adoption narratives; experiment data on preference falsification in threshold-cascade dynamics (Kuran''s work); analysis of adoption rate curves (S-curve cascades vs. flat slow adoption suggests different mechanisms).',
    'Pure voluntary adoption for coordination benefit supports the endogenous-climb framing. Cascade dynamics with preference falsification (people adopt not because they prefer the norm but because they fear standing out) introduce coercive elements even in the absence of state enforcement — extractiveness rises from 0.15 toward 0.25-0.30. The constraint remains rope-classified but with higher suppression from internalized conformity pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_vs_cascading_conformity, empirical, 'Whether adoption was autonomous benefit-seeking or conformity cascade.').

omega_variable(
    reading_narrative_circularity,
    'Is the endogenous-climb reading itself a committer narrative: a story historians and sociologists tell to emphasize agency and organic cultural change, possibly selected over exogenous narratives because it flatters modernization-as-progress ideology?',
    'Historiographic critique: compare dominant narratives about the same norm across time periods and disciplinary traditions. Look for shifts in framing (exogenous 50 years ago, now endogenous) that track not the evidence but the discipline''s changing values. Examine whether specific cases are consistently coded as endogenous or exogenous across independent scholarship or whether the coding drifts with partisan/theoretical commitments.',
    'If the endogenous-climb narrative is itself a reading (a preferred interpretation, not a discovered fact), then this constraint is explicitly a committer frame and the whole kernel remains contested indefinitely. Mandatrophy would manifest not in the constraint''s metrics but in the disciplinary commitment to the reading despite evidence to the contrary. This omega documents the reflexivity of the reading itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_narrative_circularity, conceptual, 'Whether the endogenous-climb reading is a committer narrative shaped by disciplinary values.').

omega_variable(
    state_formalization_extraction_cost,
    'What proportion of the measured extractiveness (0.15) is the cost of state formalization (law-writing, administrative machinery, enforcement of universal application) vs. the cost of suppressing competing norms?',
    'Budget analysis of formalization costs; comparison of suppression spend on holdouts vs. administrative overhead for norm codification; measurement of conformity pressure before vs. after state formalization.',
    'If formalization costs dominate (say, 0.10 of 0.15), the constraint remains rope: low-extraction coordination. If suppression of competing norms dominates (say, 0.09 of 0.15), the exogenous-override reading gains traction — the state is enforcing preference for one norm over alternatives, not merely formalizing consensus. High extraction would suggest the constraint is a snare or tangled-rope, not rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_formalization_extraction_cost, empirical, 'Whether measured extractiveness reflects formalization cost or suppression cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(impo_tr_t0, projected).
narrative_ontology:measurement(impo_tr_t5, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement_basis(impo_tr_t5, observed).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(impo_tr_t20, observed).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(impo_tr_t30, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(impo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(impo_be_t0, projected).
narrative_ontology:measurement(impo_be_t5, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 5, 0.11).
narrative_ontology:measurement_basis(impo_be_t5, observed).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(impo_be_t20, observed).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement_basis(impo_be_t30, observed).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(impo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(impo_su_t0, projected).
narrative_ontology:measurement(impo_su_t5, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement_basis(impo_su_t5, observed).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(impo_su_t20, observed).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(impo_su_t30, observed).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(impo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, information_standard).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family decomposing the contested kernel 'imposition_mechanism_kernel' into three structurally distinct claims about how norms gain state legitimacy. Each reading instantiates a different constraint with its own ε, stakeholder structure, and beneficiary/victim declarations. The endogenous-climb reading (this file) asserts that norms achieve legitimacy through bottom-up adoption before state recognition; ε is low (0.15), extraction is minimal, and the state coordinates rather than coerces. The exogenous-override reading asserts that state coercion precedes adoption; ε would be high, suppression high, and the state enforces. The hybrid reading asserts that symbolic authority transfer and institutional incentives combine. The three readings are linked via network.affects_constraints to enable contamination analysis — if one reading's causal narrative is falsified, how does that alter the others?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
