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
 *   authority displaces prior practice. It posits that state decree is
 *   sufficient for compliance, regardless of whether the new practices are
 *   internalized by the population. This reading emphasizes legal mandate and
 *   coercive enforcement, leading to high extraction and suppression,
 *   particularly for rural populations who bear the adjustment costs. The
 *   state's modernization agenda is the primary beneficiary, while
 *   traditional elites and rural communities are victims. The claimed type is
 *   'snare' because the coordination story (modernization) is cover for the
 *   coercive imposition and extraction of compliance.
 *
 * KEY AGENTS:
 *   - state_modernization_agenda: Primary beneficiary (institutional/analytical) — benefits from rapid, top-down change.
 *   - state_bureaucracy: Agenda setter (institutional/constrained) — enforces decrees, reinforces its own authority.
 *   - rural_populations: Primary victim (powerless/trapped) — bears adjustment costs, faces coercive enforcement.
 *   - traditional_elites: Secondary victim (moderate/constrained) — loses authority, faces pressure to conform.
 *   - international_observers: Analytical observer (analytical/analytical) — monitors and critiques the process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree as Sufficient Authority for Practice Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '1cd2346a-ab44-4dc8-a398-8e32f3d015c5').
narrative_ontology:cs_kernel_codification('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', formalized).
narrative_ontology:cs_authority_grounding('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', extraction).
narrative_ontology:cs_interpretation_layer_present('1cd2346a-ab44-4dc8-a398-8e32f3d015c5').
narrative_ontology:cs_reading_relation('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', foundational, state_mandate_is_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(state_mandate_is_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', state_mandate_is_sufficient_for_compliance, conventional).
narrative_ontology:cs_axiom('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', secondary, internalization_is_irrelevant_to_legal_compliance).
narrative_ontology:cs_axiom_status(internalization_is_irrelevant_to_legal_compliance, holdable).
narrative_ontology:cs_axiom_grounding('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', internalization_is_irrelevant_to_legal_compliance, conventional).
narrative_ontology:cs_reference_frame('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', unilateral_state_sovereignty).
narrative_ontology:cs_drift_state('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', contemporary_human_rights_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1cd2346a-ab44-4dc8-a398-8e32f3d015c5', '').
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

% The abstract goal of the state to modernize and rationalize society, which benefits from the immediate, top-down imposition of new practices without lengthy consultation or adaptation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).

% The administrative apparatus responsible for issuing decrees and enforcing compliance. Benefits from the perceived efficiency and directness of top-down mandates, reinforcing its own authority and control.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_bureaucracy, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the direct costs of adjusting to new practices (e.g., changing agricultural calendars, dress codes) without prior consultation or sufficient resources for adaptation. Experience coercive enforcement and practical workarounds due to non-compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, immediate, trapped, local).

% Leaders of traditional communities whose authority and social standing are undermined by the state's direct imposition of new practices, bypassing their customary roles in mediating change. They face a choice between resistance and co-optation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_elites, payer,
    moderate, biographical, constrained, regional).

% Monitor state actions for human rights compliance and effective governance. Their analysis often highlights the gap between decreed compliance and actual internalization, and the social costs of coercive imposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to rapidly coordinate societal practices with a state-defined modernization agenda, bypassing slow, decentralized processes of cultural change.
% TRANSFER_FUNCTION: Transfers authority over social norms and practices from traditional community structures to the centralized state, extracting compliance and resources (e.g., labor, taxes) in the process.
% ABSENT_VOICES: Local community leaders, cultural preservationists, and human rights advocates are often excluded from the decree-making process; they would argue for bottom-up participation, cultural sensitivity, and voluntary adoption over coercive imposition.
% DISAPPEARANCE_RATIONALE: If the state's authority to unilaterally impose practices vanished, traditional practices would likely re-emerge or evolve organically, and the state's modernization agenda would face significant delays and require new strategies for implementation.
% FOUNDING_PROBLEM: The perceived inefficiency and 'backwardness' of traditional practices hindering national development and integration into the modern world.
% FOUNDING_PROBLEM_CORROBORATION: The state bureaucracy and its proponents attest that the problem of 'backwardness' is still live and requires decisive action. International development agencies and some academics, while acknowledging the initial problem, often criticize the methods of imposition as counterproductive, suggesting the problem is 'live' but the solution is flawed.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because the state imposes significant costs on populations without their consent or adequate compensation, leveraging its monopoly on legitimate force. Suppression is very high (0.88) as compliance is achieved through active coercion and the suppression of dissent or alternative practices. Theater ratio is moderate (0.45) because while the state genuinely seeks modernization, a significant portion of its effort is performative (demonstrating control, projecting an image of modernity) rather than genuinely functional (facilitating internalization or adaptation). Resistance is high (0.75) due to the coercive nature of the imposition, leading to active and passive forms of non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   The state bureaucracy perceives this as a necessary, if sometimes difficult, process of national development, where compliance is a measure of its effectiveness. Rural populations and traditional elites experience it as an arbitrary, extractive imposition that disrupts their way of life and undermines their autonomy. International observers often highlight the human rights implications and the long-term instability caused by such methods.
 *
 * DIRECTIONALITY LOGIC:
 *   The state bureaucracy and its modernization agenda are clear beneficiaries, as the constraint directly serves their goals and enhances their power (low d). Rural populations and traditional elites are targets, bearing the costs and facing severe restrictions on their autonomy (high d). International observers are analytical, neither directly benefiting nor suffering, but evaluating the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly rejects the idea of mandatrophy in the sense of a mandate outliving its function. For this reading, the 'function' is the imposition of state authority and modernization, which is considered an ongoing, live problem. The high extractiveness and suppression are seen as necessary tools for this function, not signs of decay. The classification as 'snare' prevents mislabeling this coercive imposition as genuine coordination, even if the state claims a coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_compliance,
    'To what extent does observed compliance reflect genuine internalization of new practices versus mere outward conformity under duress?',
    'Longitudinal ethnographic studies and post-enforcement surveys measuring attitudes, beliefs, and spontaneous practice in the absence of direct surveillance.',
    'If compliance is primarily external, the constraint''s long-term effectiveness is lower than its immediate suppression suggests, requiring perpetual enforcement. This would shift the classification towards a more unstable ''snare'' or even ''piton'' if enforcement costs outweigh benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalization_vs_compliance, empirical, 'Distinguishing between coerced compliance and genuine adoption of new practices.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s authority for practice displacement grounded in a genuine social contract or in its monopoly on coercive force?',
    'Analysis of historical documents, public discourse, and citizen participation in governance, particularly regarding the formation of the state and its relationship with diverse populations.',
    'If grounded in coercion, the constraint is a pure ''snare'' with no underlying coordination function. If a social contract exists, it might be a ''tangled_rope'' where some coordination is present but extraction is asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'The fundamental source of the state''s legitimacy in imposing new practices.').

omega_variable(
    long_term_social_cost,
    'What are the long-term social and cultural costs of rapid, coercive practice displacement, beyond immediate economic adjustment?',
    'Intergenerational studies on cultural loss, social cohesion, and psychological well-being in affected communities.',
    'High unacknowledged long-term costs would further solidify the ''snare'' classification by revealing hidden victims and amplifying the effective extraction, even if not immediately apparent to the state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_social_cost, empirical, 'Unmeasured long-term social and cultural impacts of imposed practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 1950, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1960, 0.38).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1970, 0.45).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(legi_tr_t1990, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1990, 0.43).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 2000, 0.45).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1970, 0.78).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1980, 0.76).
narrative_ontology:measurement(legi_be_t1990, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1990, 0.77).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 2000, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1960, 0.85).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1970, 0.88).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1980, 0.87).
narrative_ontology:measurement(legi_su_t1990, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1990, 0.87).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 2000, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
