% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree as Sufficient Authority for Practice Displacement (Exogenous Override Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint represents the 'exogenous override' reading of how state
 *   decrees displace prior practices. It posits that state authority is
 *   sufficient to enforce compliance, regardless of whether the new practices
 *   are internalized by the population. This reading emphasizes legal mandate
 *   and coercive power as the primary drivers of change, often leading to
 *   high extraction and suppression for affected populations. The state's
 *   modernization agenda is the primary beneficiary, while rural populations
 *   bear the costs of adjustment without genuine consent. This is one reading
 *   of the 'legitimacy_of_imposed_practice' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.7).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.85).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree as Sufficient Authority for Practice Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33').
narrative_ontology:cs_kernel_codification('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', formalized).
narrative_ontology:cs_authority_grounding('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', extraction).
narrative_ontology:cs_interpretation_layer_present('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33').
narrative_ontology:cs_reading_relation('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', foundational, state_mandate_is_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(state_mandate_is_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', state_mandate_is_sufficient_for_compliance, conventional).
narrative_ontology:cs_axiom('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', foundational, internalization_is_not_a_prerequisite_for_practice_change).
narrative_ontology:cs_axiom_status(internalization_is_not_a_prerequisite_for_practice_change, holdable).
narrative_ontology:cs_axiom_grounding('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', internalization_is_not_a_prerequisite_for_practice_change, empirically_contingent).
narrative_ontology:cs_reference_frame('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', rational_state_sovereignty).
narrative_ontology:cs_drift_state('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', contemporary_human_rights_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2d0ac8d2-a03a-43a0-b5e9-1bcf66f84c33', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract goal of the state to modernize and rationalize society, which benefits from the rapid, top-down imposition of new practices, regardless of local acceptance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).

% The administrative apparatus responsible for drafting, promulgating, and enforcing state decrees. Benefits from the perceived efficiency and authority of top-down implementation, and from the expansion of its own power and scope.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the direct costs of adjusting to new practices (e.g., changing agricultural calendars, dress codes) without consultation or compensation. Face coercive enforcement and social disruption, with limited means to resist or exit.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, immediate, trapped, local).

% Local leaders whose authority and status are undermined by the displacement of traditional practices. They may attempt to negotiate or subtly resist, but ultimately face the superior coercive power of the state.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites, payer,
    moderate, biographical, constrained, regional).

% Academics, NGOs, and other external actors who analyze the efficacy and human rights implications of state-imposed cultural changes. Their analysis may influence international opinion but has no direct enforcement power.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint aims to coordinate societal practices (e.g., calendars, dress codes) under a single, rationalized state standard, replacing diverse local customs with a uniform system.
% TRANSFER_FUNCTION: Transfers authority and legitimacy from traditional local practices and institutions to the central state, and imposes adjustment costs (social, economic, cultural) onto local populations.
% ABSENT_VOICES: Local community leaders, cultural preservationists, and those whose livelihoods are tied to traditional practices are excluded from the decision-making process. Their objections regarding cultural erosion, economic disruption, and lack of consent are ignored.
% DISAPPEARANCE_RATIONALE: If the state's decree authority to override prior practice vanished, local communities would likely revert to traditional practices, or develop new ones organically. The state's modernization agenda would stall, and its legitimacy in these domains would collapse, leading to a significant reorganization of social and political life.
% FOUNDING_PROBLEM: The state perceived traditional, diverse local practices as obstacles to national unity, economic development, and modern governance, leading to a desire for rapid, top-down standardization.
% FOUNDING_PROBLEM_CORROBORATION: The state bureaucracy and its proponents attest that the problem of 'backward' or 'fragmented' practices remains live, justifying continued top-down intervention. International observers and rural populations, however, contest this, arguing that the 'problem' is a construct of the state's centralizing ambition, not an inherent flaw in local practices.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) reflects the significant costs imposed on rural populations and traditional elites, who are forced to abandon established ways of life. Suppression (0.85) is high due to the reliance on coercive state power to enforce compliance, often against local resistance. The theater ratio (0.4) indicates that while some genuine modernization occurs, a substantial portion of the state's effort is performative, aimed at demonstrating its authority and control rather than achieving deep, internalized change. Resistance (0.75) is high, reflecting ongoing, often subtle, non-compliance and pushback from affected communities.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, this is a necessary, if sometimes difficult, process of modernization and nation-building. From the perspective of rural populations, it is an imposition that extracts cultural autonomy and imposes hardship. The engine's classification will highlight this divergence, likely classifying it as a Snare for the affected populations, despite the state's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and its modernization agenda are clear beneficiaries (low directionality), as they gain power, legitimacy, and the perceived benefits of standardization. Rural populations and traditional elites are targets (high directionality), bearing the costs of displacement and facing direct enforcement. International observers are analytical, neither directly benefiting nor paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (rapid, top-down modernization) is considered 'live' by the state, but 'contested' by others. This reading prevents mislabeling the coercive imposition as genuine coordination by emphasizing the high suppression and extractiveness, and the lack of internalization. It highlights that compliance is driven by mandate and enforcement, not by collective benefit or voluntary adoption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_compliance,
    'To what extent does observed compliance reflect genuine internalization of new practices versus mere outward conformity under duress?',
    'Longitudinal ethnographic studies tracking post-enforcement behavior, and analysis of cultural artifacts for signs of genuine adoption versus superficial adherence.',
    'If compliance is largely superficial, the constraint''s effective suppression and extractiveness are higher than measured, as the underlying resistance remains strong. If some internalization occurs, the constraint might be less extractive over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_compliance, empirical, 'Distinguishing coerced compliance from genuine adoption.').

omega_variable(
    long_term_stability_of_imposed_practices,
    'Will practices imposed solely by exogenous decree persist in the long term without continuous, active enforcement, or will they revert to prior forms?',
    'Historical analysis of similar top-down reforms in other contexts, and observation of periods where enforcement capacity wanes.',
    'If practices revert without enforcement, the constraint''s long-term viability is low, and its ''success'' is purely a function of suppression. This would reinforce its classification as a Snare or even a Piton if enforcement costs become unsustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_stability_of_imposed_practices, empirical, 'Sustainability of practices imposed by decree.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s authority to impose practices derived from a genuine social contract or from its monopoly on coercive force?',
    'Analysis of historical documents, public discourse, and the presence/absence of mechanisms for popular consent or redress.',
    'If authority is purely coercive, the constraint is a clear Snare. If a genuine (even if weak) social contract exists, it might have elements of a Tangled Rope, implying some coordination function, however asymmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Source of state''s legitimacy for cultural imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimacy_of_imposed_practice' kernel, focusing on the state's capacity for exogenous override. It is linked to sibling readings that emphasize endogenous adoption or hybrid approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
