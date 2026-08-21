% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: Legitimacy of Practice Standardization via Exogenous State Decree
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint describes the legitimacy of practice change when it is
 *   decreed by state authority for perceived collective benefit (e.g.,
 *   modernization, fiscal stability, international alignment). This
 *   'exogenous override' reading emphasizes abrupt legal imposition, active
 *   enforcement, and the resulting surface compliance that often masks
 *   persistent underground traditional practices. The 'double life' led by
 *   affected populations is seen as a stable equilibrium, not a transitional
 *   phase, with rural communities maintaining traditional calendars and dress
 *   for decades despite official mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.78).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.85).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "Legitimacy of Practice Standardization via Exogenous State Decree").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '8de4db0c-347d-4434-af50-4388eba6e7de').
narrative_ontology:cs_kernel_codification('8de4db0c-347d-4434-af50-4388eba6e7de', formalized).
narrative_ontology:cs_authority_grounding('8de4db0c-347d-4434-af50-4388eba6e7de', extraction).
narrative_ontology:cs_interpretation_layer_present('8de4db0c-347d-4434-af50-4388eba6e7de').
narrative_ontology:cs_reading_relation('8de4db0c-347d-4434-af50-4388eba6e7de', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('8de4db0c-347d-4434-af50-4388eba6e7de', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('8de4db0c-347d-4434-af50-4388eba6e7de', foundational, state_sovereignty_over_practice).
narrative_ontology:cs_axiom_status(state_sovereignty_over_practice, holdable).
narrative_ontology:cs_axiom_grounding('8de4db0c-347d-4434-af50-4388eba6e7de', state_sovereignty_over_practice, conventional).
narrative_ontology:cs_axiom('8de4db0c-347d-4434-af50-4388eba6e7de', foundational, collective_benefit_justifies_imposition).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_imposition, holdable).
narrative_ontology:cs_axiom_grounding('8de4db0c-347d-4434-af50-4388eba6e7de', collective_benefit_justifies_imposition, instrumental).
narrative_ontology:cs_reference_frame('8de4db0c-347d-4434-af50-4388eba6e7de', rational_state_modernization).
narrative_ontology:cs_drift_state('8de4db0c-347d-4434-af50-4388eba6e7de', post_colonial_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8de4db0c-347d-4434-af50-4388eba6e7de', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernization_agenda_setters).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_elites).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditionalists).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, cultural_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate and enforce decrees for practice standardization, framing them as essential for national progress, fiscal stability, or international standing. They benefit from increased state control and the symbolic capital of 'modernity'.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernization_agenda_setters, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Often align with state modernization efforts, adopting new practices (e.g., Gregorian calendar, Western dress) that facilitate their integration into global systems and distinguish them from traditional rural populations. They benefit from the perceived social and economic advantages of standardization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_elites, beneficiary,
    powerful, biographical, mobile, national).

% Bear the direct costs of forced practice change, often maintaining traditional practices (e.g., lunar calendar, customary dress) underground or in private, leading a 'double life'. Their identity is deeply tied to these practices, making genuine exit unthinkable, despite state coercion.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_traditionalists, payer,
    powerless, generational, identity_locked, local).

% Experience state-decreed standardization as an assault on their distinct cultural identity and autonomy. They face severe penalties for non-compliance but often resist through passive means or by preserving practices in hidden contexts, leading to persistent, low-level conflict.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, cultural_minorities, payer,
    powerless, generational, identity_locked, regional).

% Analyze the impact of state-led modernization on human rights, cultural diversity, and social cohesion. They can influence international opinion and provide critical perspectives on the legitimacy and effects of such policies.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize diverse local practices (e.g., calendars, dress codes) across a national territory to foster national unity, improve administrative efficiency, facilitate economic integration, and align with international norms.
% TRANSFER_FUNCTION: Transfers authority over social and cultural practices from local, traditional, or religious bodies to the central state. It imposes the costs of compliance, cultural disruption, and identity suppression onto traditional and minority populations, while centralizing power and symbolic capital within the state and its aligned elites.
% ABSENT_VOICES: Traditional leaders, local community elders, and cultural preservationists whose authority is directly challenged by state decrees are often excluded from the decision-making process. Their perspectives on the value and function of traditional practices are systematically marginalized.
% DISAPPEARANCE_RATIONALE: If state authority to decree practice standardization vanished, the imposed practices would rapidly lose legitimacy. Traditional practices would likely resurface openly, leading to a re-fragmentation of social norms and potentially challenging the state's administrative coherence and national identity narratives.
% FOUNDING_PROBLEM: Perceived national disunity, economic backwardness, and international isolation stemming from a multitude of diverse, unstandardized local and regional practices that hindered state administration, fiscal collection, and modern economic development.
% FOUNDING_PROBLEM_CORROBORATION: State archives, official histories, and modernization theorists attest to the founding problem's historical urgency and ongoing relevance. However, historical accounts from affected communities, cultural anthropologists, and post-colonial critics contest this framing, arguing that the 'problem' was often a pretext for cultural assimilation and state power consolidation, rather than a genuine collective-action failure.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the state imposes significant costs on populations forced to abandon or hide traditional practices, often without genuine consent. Suppression (0.85) is very high due to the legal force and enforcement mechanisms deployed by the state to ensure compliance. The theater ratio (0.60) is substantial because a significant portion of observed compliance is performative, with traditional practices continuing in private or informal spheres, creating a 'double life' where official adherence is theatrical rather than genuine. Resistance (0.70) is also high, reflecting the persistent, often subtle, forms of non-compliance and cultural preservation.
 *
 * PERSPECTIVAL GAP:
 *   The state and urban elites perceive this constraint as a necessary and legitimate tool for national development and progress, viewing resistance as backwardness. Conversely, rural traditionalists and cultural minorities experience it as an illegitimate imposition that extracts cultural autonomy and imposes a foreign way of life, leading to a deep and unacknowledged perspectival chasm.
 *
 * DIRECTIONALITY LOGIC:
 *   State modernization agenda-setters and urban elites are beneficiaries, gaining power, symbolic capital, and economic integration from standardization (low directionality). Rural traditionalists and cultural minorities are clear targets, bearing the costs of cultural disruption, forced compliance, and identity suppression (high directionality), often trapped by their deep cultural ties.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'collective benefit' justification for this constraint often serves as a cover story for state power consolidation and cultural assimilation. While some coordination benefits (e.g., unified timekeeping) may exist, the high extractiveness and suppression, coupled with persistent underground resistance, indicate that the constraint's primary function has drifted from genuine collective benefit to maintaining state authority and extracting cultural conformity. The 'double life' phenomenon is a key indicator of this mandatrophy, where the official mandate is theatrically maintained while its original coordination function is undermined by its extractive operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the legitimacy of practice change truly derived from collective benefit, or primarily from the coercive power of the state?',
    'Analysis of historical records for evidence of genuine popular consent or voluntary adoption versus reliance on legal mandates and enforcement. Comparative studies of similar reforms in contexts with varying state capacity and popular participation.',
    'If legitimacy is primarily coercive, the constraint''s extractiveness and suppression are higher than acknowledged, and its coordination function is largely a cover story, pushing it closer to a Snare. If genuine collective benefit is demonstrable, the Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Ambiguity regarding the true source of the constraint''s legitimacy.').

omega_variable(
    compliance_depth_ambiguity,
    'To what extent is observed compliance with state-decreed practices genuine adoption versus performative surface adherence masking persistent underground traditional practices?',
    'Ethnographic studies, oral histories, and analysis of informal social networks to uncover the persistence and vitality of traditional practices in private or non-official spheres, contrasting with official reports of full compliance.',
    'If compliance is largely performative, the ''theater_ratio'' is higher than currently estimated, and the ''suppression'' is more effective at creating a facade of change than actual transformation, reinforcing the Tangled Rope classification and highlighting the ''double life'' phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_depth_ambiguity, empirical, 'Uncertainty about the depth and sincerity of compliance with imposed practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.6).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 60, 0.59).
narrative_ontology:measurement(legi_tr_t70, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 70, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 60, 0.77).
narrative_ontology:measurement(legi_be_t70, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 70, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.84).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(legi_su_t70, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 70, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
