% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Hybrid Scaffolding of Imposed Practices (e.g., Dress Reform)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint describes the process by which a top-down state mandate
 *   for cultural change (e.g., dress reform in early 20th-century Turkey)
 *   achieves partial success through a 'hybrid scaffolding' approach. Pure
 *   decree (like calendar reform) often fails due to lack of internalization,
 *   while purely endogenous change is slow. This reading focuses on how
 *   ideological messaging and elite modeling create a 'quasi-endogenous pull'
 *   that reinforces the mandate, leading to hybrid practices rather than full
 *   displacement. The constraint is claimed as a Tangled Rope because it
 *   involves both a coordination function (aligning society with a modern
 *   vision) and asymmetric extraction (costs borne disproportionately by
 *   rural and traditional groups).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Hybrid Scaffolding of Imposed Practices (e.g., Dress Reform)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e0fb3346-f008-429b-adfd-32843b68d753').
narrative_ontology:cs_kernel_codification('e0fb3346-f008-429b-adfd-32843b68d753', formalized).
narrative_ontology:cs_authority_grounding('e0fb3346-f008-429b-adfd-32843b68d753', lineage).
narrative_ontology:cs_interpretation_layer_present('e0fb3346-f008-429b-adfd-32843b68d753').
narrative_ontology:cs_reading_relation('e0fb3346-f008-429b-adfd-32843b68d753', legitimacy_of_imposed_practice__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('e0fb3346-f008-429b-adfd-32843b68d753', legitimacy_of_imposed_practice__endogenous_climb_reading, influences).
narrative_ontology:cs_axiom('e0fb3346-f008-429b-adfd-32843b68d753', foundational, legitimacy_requires_quasi_endogenous_pull).
narrative_ontology:cs_axiom_status(legitimacy_requires_quasi_endogenous_pull, holdable).
narrative_ontology:cs_axiom_grounding('e0fb3346-f008-429b-adfd-32843b68d753', legitimacy_requires_quasi_endogenous_pull, empirically_contingent).
narrative_ontology:cs_axiom('e0fb3346-f008-429b-adfd-32843b68d753', secondary, pure_decree_insufficient_for_cultural_change).
narrative_ontology:cs_axiom_status(pure_decree_insufficient_for_cultural_change, holdable).
narrative_ontology:cs_axiom_grounding('e0fb3346-f008-429b-adfd-32843b68d753', pure_decree_insufficient_for_cultural_change, empirically_contingent).
narrative_ontology:cs_reference_frame('e0fb3346-f008-429b-adfd-32843b68d753', state_led_cultural_transformation).
narrative_ontology:cs_drift_state('e0fb3346-f008-429b-adfd-32843b68d753', post_colonial_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e0fb3346-f008-429b-adfd-32843b68d753', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_cultural_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state apparatus driving the modernization agenda, seeking to transform society through top-down mandates. It benefits from the perceived success of these reforms, which legitimizes its authority and vision. Exit means abandoning the project, which is politically costly.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_project, agenda_setter,
    institutional, generational, constrained, national).

% Adopt the new practices (e.g., Western dress) as markers of modernity and alignment with the state. They gain social status, access to state resources, and reinforce their position within the new social hierarchy. They have relatively easy 'exit' from traditional practices due to their social capital.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, beneficiary,
    powerful, biographical, mobile, local).

% Bear the costs of imposed practices, often lacking the resources or social incentives to adopt them fully. They face social stigma, economic penalties, or direct coercion for non-compliance, while being excluded from the 'scaffolding' infrastructure (e.g., education, media) that facilitates adoption for elites. Their identity is often tied to traditional practices.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, generational, trapped, regional).

% Actively resist or passively subvert imposed practices, viewing them as an assault on their cultural identity and heritage. They pay the cost in terms of cultural erosion, loss of autonomy, and sometimes direct persecution. Their exit from traditional practices is identity-locked.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_cultural_groups, payer,
    organized, civilizational, identity_locked, regional).

% Craft and disseminate messaging that links the imposed practices to national progress, modernity, or a desired future identity. They benefit from the success of the ideological project, which validates their role and influence. Their role is crucial for generating 'quasi-endogenous pull'.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, ideological_propagandists, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To rapidly shift societal norms and practices towards a state-defined vision of modernity, coordinating collective action around new cultural markers and behaviors.
% TRANSFER_FUNCTION: Transfers social capital, legitimacy, and resources from traditional cultural practices and their adherents to new, state-sanctioned practices and the elites who adopt them.
% ABSENT_VOICES: Scholars and practitioners of traditional cultural forms, who are often marginalized or silenced, would articulate the value and coherence of the practices being displaced, and the social costs of their erosion.
% DISAPPEARANCE_RATIONALE: If the state's mandate and ideological scaffolding vanished, the hybrid practices would likely revert towards traditional forms in many areas, and the social hierarchy based on adherence to 'modern' practices would destabilize. The cultural landscape would re-diversify.
% FOUNDING_PROBLEM: The state perceived a need to rapidly modernize and align with global powers, viewing traditional practices as obstacles to progress and national strength.
% FOUNDING_PROBLEM_CORROBORATION: The state continues to assert the necessity of modernization for national survival and progress. Independent historians and sociologists corroborate that the initial problem was perceived as live by the state, though they may contest the methods or the necessity of cultural imposition.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the state leverages its authority to impose practices that benefit a segment of the population (urban elites) at the expense of others. Suppression (0.7) is significant, as the state actively enforces compliance and suppresses traditional alternatives, but it's not absolute due to the 'scaffolding' aspect. Theater ratio (0.4) reflects that while there's genuine effort to transform society, a portion of the enforcement is performative, maintaining the image of success even where compliance is superficial or hybrid. Accessibility collapse is moderate (0.45) because alternatives are suppressed but not entirely eliminated, leading to hybrid practices. Resistance is high (0.75) due to the cultural and identity costs borne by victims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and urban elites, this is a necessary, albeit challenging, coordination effort for national progress. From the perspective of rural and traditional groups, it is an extractive imposition that undermines their way of life. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state modernization project and urban elites are beneficiaries, gaining legitimacy and status from the imposed practices (low directionality). Rural populations and traditional cultural groups are victims, bearing the costs of cultural disruption and coercion (high directionality). Ideological propagandists are agenda-setters, actively shaping the narrative to support the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (modernization) is still 'live' from the state's perspective, preventing it from being a Piton. However, the 'scaffolding' aspect means it's not a pure Snare, as there's a genuine, albeit unevenly distributed, coordination function in aligning a segment of society with new norms. The hybrid nature prevents mislabeling it as pure extraction or pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_ideological_scaffolding,
    'To what extent does ideological messaging genuinely generate ''quasi-endogenous pull'' versus merely providing a veneer for continued coercion?',
    'Longitudinal ethnographic studies tracking changes in individual beliefs and practices post-mandate, distinguishing between outward compliance and internalized acceptance.',
    'If the pull is largely superficial, the constraint''s effective extractiveness and suppression are higher, pushing it closer to a Snare. If genuine, it reinforces the Tangled Rope classification by highlighting the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_ideological_scaffolding, empirical, 'Distinguishing genuine internalization from performative compliance.').

omega_variable(
    scope_of_scaffolding_infrastructure,
    'What is the actual reach and effectiveness of the ''scaffolding'' infrastructure (education, media, elite modeling) across different social strata?',
    'Detailed historical analysis of resource allocation for reform efforts, and sociological studies on access to and impact of modernization initiatives in urban vs. rural areas.',
    'If scaffolding is highly concentrated among elites, the constraint''s extractiveness from rural populations is higher than currently estimated, as they bear costs without access to the benefits of facilitated adoption. This would strengthen the asymmetric extraction component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_scaffolding_infrastructure, empirical, 'Uneven distribution of support for adopting new practices.').

omega_variable(
    cultural_identity_vs_state_identity,
    'Is the adoption of new practices a genuine fusion of identities, or a strategic performance of state identity while traditional cultural identity persists privately?',
    'Oral histories and cultural studies focusing on private vs. public adherence to practices, and the evolution of hybrid cultural forms.',
    'If traditional identity persists strongly in private, the ''identity_locked'' exit option for traditional groups is more severe, and the suppression of their cultural expression is more complete than outward appearances suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_identity_vs_state_identity, conceptual, 'Depth of cultural identity shift versus performative compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 1920, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1930, 0.3).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1950, 0.38).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 1960, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1950, 0.63).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 1960, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 1960, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimacy_of_imposed_practice' kernel, focusing on the hybrid scaffolding mechanism. It is linked to other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
