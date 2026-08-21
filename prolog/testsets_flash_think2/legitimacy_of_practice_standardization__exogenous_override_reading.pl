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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Legitimacy of Practice Standardization: Exogenous Override Reading
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint represents the 'exogenous override' reading of the
 *   'legitimacy of practice standardization' kernel. It asserts that practice
 *   change is legitimate when decreed by state authority for collective
 *   benefit (e.g., modernization, fiscal stability, international alignment).
 *   This reading is characterized by abrupt legal imposition, active
 *   enforcement mechanisms, and often results in surface compliance masking
 *   persistent underground traditional practices, creating a 'double life'
 *   for affected populations. The metrics reflect this high-extraction,
 *   high-suppression dynamic, with significant theatricality.
 *
 * KEY AGENTS:
 *   - State_authority: Primary agenda_setter (institutional/arbitrage) — decrees and enforces changes.
 *   - Modernization_elites: Primary beneficiary (powerful/mobile) — architects of change, benefit from alignment.
 *   - Rural_populations: Primary payer (powerless/identity_locked) — bear costs, maintain underground practices.
 *   - Traditional_practice_adherents: Primary payer (powerless/identity_locked) — cultural practices suppressed.
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
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "Legitimacy of Practice Standardization: Exogenous Override Reading").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'f02bbf06-e9a9-467f-89da-faa7eb4add07').
narrative_ontology:cs_kernel_codification('f02bbf06-e9a9-467f-89da-faa7eb4add07', formalized).
narrative_ontology:cs_authority_grounding('f02bbf06-e9a9-467f-89da-faa7eb4add07', extraction).
narrative_ontology:cs_interpretation_layer_present('f02bbf06-e9a9-467f-89da-faa7eb4add07').
narrative_ontology:cs_reading_relation('f02bbf06-e9a9-467f-89da-faa7eb4add07', legitimacy_of_practice_standardization__endogenous_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('f02bbf06-e9a9-467f-89da-faa7eb4add07', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('f02bbf06-e9a9-467f-89da-faa7eb4add07', foundational, state_sovereignty_over_practice).
narrative_ontology:cs_axiom_status(state_sovereignty_over_practice, holdable).
narrative_ontology:cs_axiom_grounding('f02bbf06-e9a9-467f-89da-faa7eb4add07', state_sovereignty_over_practice, conventional).
narrative_ontology:cs_axiom('f02bbf06-e9a9-467f-89da-faa7eb4add07', foundational, collective_benefit_justifies_imposition).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_imposition, holdable).
narrative_ontology:cs_axiom_grounding('f02bbf06-e9a9-467f-89da-faa7eb4add07', collective_benefit_justifies_imposition, instrumental).
narrative_ontology:cs_reference_frame('f02bbf06-e9a9-467f-89da-faa7eb4add07', centralized_state_control_of_social_order).
narrative_ontology:cs_drift_state('f02bbf06-e9a9-467f-89da-faa7eb4add07', post_colonial_critique_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f02bbf06-e9a9-467f-89da-faa7eb4add07', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_authority).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_aligners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_practice_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees and enforces practice changes (e.g., calendar, dress codes) for perceived collective benefits like national unity or fiscal stability. Defines the 'collective benefit' and wields legal and coercive power to ensure compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, state_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Benefit from the alignment of national practices with international norms, economic reforms, and the consolidation of state power. They are often the architects and proponents of such standardization efforts.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_elites, beneficiary,
    powerful, generational, mobile, national).

% International bodies, financial institutions, or allied states whose interests are served by a nation's adoption of standardized practices (e.g., common legal frameworks, fiscal reporting standards). They provide diplomatic or economic incentives for compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_aligners, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the direct costs of forced practice change, including social disruption, economic penalties for non-compliance, and the psychological burden of maintaining traditional practices underground. Their daily lives are dictated by state decrees that often conflict with long-standing customs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, identity_locked, local).

% Their cultural and religious practices are delegitimized, suppressed, or criminalized by state decrees. They face a choice between surface compliance and maintaining their identity through covert adherence to traditions, leading to a 'double life'.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_practice_adherents, payer,
    powerless, generational, identity_locked, local).

% Analyze the long-term social, cultural, and political effects of state-led practice standardization, often documenting the gap between official narratives of collective benefit and the lived experiences of affected populations.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, institutional_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize diverse practices across a national population, aiming for administrative efficiency, national cohesion, fiscal stability, or international alignment, as defined and enforced by the state.
% TRANSFER_FUNCTION: Transfers legitimacy and control over daily, cultural, and economic practices from local/traditional authorities and norms to the central state authority. It also transfers resources (time, effort, social capital) from adhering to traditional practices to complying with state decrees.
% ABSENT_VOICES: Traditional leaders, local community elders, and those whose cultural identity is deeply tied to the practices being overridden are often marginalized or suppressed. They would advocate for the preservation of traditional practices or for a more inclusive, bottom-up approach to change.
% DISAPPEARANCE_RATIONALE: If state decrees for practice standardization and their enforcement vanished overnight, the suppressed traditional practices would likely resurface more openly, and the state's legitimacy in these domains would be severely challenged. Social and cultural life would reorganize around pre-existing or re-emergent local norms.
% FOUNDING_PROBLEM: Fragmented, diverse, or 'backward' practices hindering national cohesion, economic development, or international standing, as perceived by the state and modernization elites.
% FOUNDING_PROBLEM_CORROBORATION: The state authority and modernization elites attest that the founding problem (e.g., need for modernization) is still live. However, anthropological studies, local oral histories, and critical historical analyses from outside the benefiting parties often contest this, arguing the problem was a pretext for control or has been substantially solved, with the arrangement persisting as a mechanism of power.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the state imposes changes that often incur significant costs on populations without their consent, defining 'collective benefit' unilaterally. Suppression is very high due to the 'abrupt legal imposition' and 'enforcement mechanisms' required to override deeply ingrained practices. The 'surface compliance masking persistent underground practice' directly translates to a high theater_ratio, as a significant portion of observed compliance is performative rather than genuine adoption. Resistance is high, evidenced by the persistence of underground practices. Accessibility collapse is moderate, as alternatives are legally suppressed but not entirely eliminated, leading to the 'double life' phenomenon.
 *
 * PERSPECTIVAL GAP:
 *   The state authority and modernization elites perceive this as a legitimate and necessary coordination mechanism for national progress. In contrast, rural populations and traditional practice adherents experience it as an extractive and suppressive imposition that undermines their cultural identity and autonomy. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority, modernization elites, and international aligners are beneficiaries, as they gain power, legitimacy, and alignment from the standardization. Rural populations and traditional practice adherents are victims, bearing the costs of forced change and suppression of their practices. Their identity_locked exit options amplify their target status.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'collective benefit' (modernization, fiscal stability) serves as the coordination story. However, the high extractiveness and suppression, coupled with the persistence of underground practices, suggest that the constraint functions more as a Snare or Tangled Rope, where the coordination narrative is a cover for state power consolidation and rent extraction (in terms of control and legitimacy). The 'double life' indicates that the mandate for genuine collective benefit is not fully realized, and the constraint persists through coercion rather than voluntary coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_benefit_definition_ambiguity,
    'Who defines ''collective benefit'' in this context, and does this definition genuinely align with the welfare of the affected populations, or primarily with state/elite interests?',
    'Independent sociological studies, public opinion surveys among affected populations, and economic analyses that disaggregate benefits and costs across different social strata.',
    'If ''collective benefit'' is found to primarily serve state/elite interests, the constraint''s extractiveness is higher than initially assessed, and its coordination function is largely theatrical, pushing it closer to a Snare. If genuinely collective, the Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_definition_ambiguity, empirical, 'Ambiguity in the definition and distribution of ''collective benefit''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (state enforcement, legal penalties) or internalized (fear, belief in state''s right to rule, social pressure to conform)?',
    'Post-decree ethnographic studies: if traditional practices persist openly or resurface rapidly after enforcement relaxation, suppression is primarily structural. If compliance persists even without active enforcement, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This would make the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in practice standardization.').

omega_variable(
    reading_distinction_clarity,
    'Is the distinction between ''exogenous override'' and ''dual practice equilibrium'' readings sufficiently clear, or do they describe different phases/aspects of the same phenomenon?',
    'Further historical and anthropological research into the long-term stability of ''double life'' practices. If the ''double life'' is a stable, enduring outcome rather than a transitional phase, the ''dual practice equilibrium'' gains more independent structural weight.',
    'If the readings are too conflated, it suggests a single, more complex constraint rather than distinct ones, potentially requiring a re-evaluation of epsilon invariance and decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_distinction_clarity, conceptual, 'Clarity of distinction between exogenous override and dual practice equilibrium readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 1920, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(legi_tr_t1925, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1925, 0.4).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1930, 0.5).
narrative_ontology:measurement(legi_tr_t1935, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1935, 0.55).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1940, 0.6).
narrative_ontology:measurement(legi_tr_t1945, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1945, 0.6).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1950, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1920, 0.65).
narrative_ontology:measurement(legi_be_t1925, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1925, 0.7).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1930, 0.73).
narrative_ontology:measurement(legi_be_t1935, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1935, 0.75).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1940, 0.77).
narrative_ontology:measurement(legi_be_t1945, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1945, 0.78).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1950, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(legi_su_t1925, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1925, 0.8).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1930, 0.83).
narrative_ontology:measurement(legi_su_t1935, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1935, 0.85).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1940, 0.85).
narrative_ontology:measurement(legi_su_t1945, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1945, 0.85).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1950, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, national_identity_formation).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, state_fiscal_policy).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimacy_of_practice_standardization' kernel, each representing a distinct structural claim about how practice change becomes legitimate. They are linked to capture the contested nature of institutional change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
