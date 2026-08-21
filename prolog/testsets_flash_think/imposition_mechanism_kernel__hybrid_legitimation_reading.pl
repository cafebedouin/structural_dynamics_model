% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Imperial Hybrid Legitimation of New Norms
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint describes the historical process by which new social
 *   norms achieved legitimacy and widespread adoption within an empire, not
 *   through pure coercion or purely grassroots emergence, but through a
 *   hybrid mechanism. This mechanism combined the symbolic authority and
 *   charismatic example of the emperor with tangible institutional incentives
 *   (e.g., administrative appointments, tax breaks) for those who adopted the
 *   new norms. This reading emphasizes the stratified adoption, with elites
 *   leading the way, and the moderate enforcement costs compared to outright
 *   military imposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.55).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Imperial Hybrid Legitimation of New Norms").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '90e531f6-e052-498a-9043-febf5180d402').
narrative_ontology:cs_kernel_codification('90e531f6-e052-498a-9043-febf5180d402', implicit).
narrative_ontology:cs_authority_grounding('90e531f6-e052-498a-9043-febf5180d402', lineage).
narrative_ontology:cs_interpretation_layer_present('90e531f6-e052-498a-9043-febf5180d402').
narrative_ontology:cs_reading_relation('90e531f6-e052-498a-9043-febf5180d402', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('90e531f6-e052-498a-9043-febf5180d402', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('90e531f6-e052-498a-9043-febf5180d402', foundational, imperial_charisma_legitimates_norms).
narrative_ontology:cs_axiom_status(imperial_charisma_legitimates_norms, holdable).
narrative_ontology:cs_axiom_grounding('90e531f6-e052-498a-9043-febf5180d402', imperial_charisma_legitimates_norms, conventional).
narrative_ontology:cs_axiom('90e531f6-e052-498a-9043-febf5180d402', foundational, institutional_incentives_drive_adoption).
narrative_ontology:cs_axiom_status(institutional_incentives_drive_adoption, holdable).
narrative_ontology:cs_axiom_grounding('90e531f6-e052-498a-9043-febf5180d402', institutional_incentives_drive_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('90e531f6-e052-498a-9043-febf5180d402', imperial_unity_through_shared_norms).
narrative_ontology:cs_drift_state('90e531f6-e052-498a-9043-febf5180d402', post_imperial_collapse, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('90e531f6-e052-498a-9043-febf5180d402', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_state).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, elites_adopting_norms).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_factions).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, unwilling_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that initiates the new norms, leveraging its symbolic power (e.g., the emperor's example) and institutional capacity (e.g., tax incentives, administrative appointments) to encourage adoption. Benefits from increased social cohesion and centralized control.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_state, agenda_setter,
    institutional, generational, arbitrage, continental).

% Local and regional elites who adopt the new norms early, often gaining favor, appointments, or economic advantages from the imperial state. They benefit from the stability and opportunities provided by the new order, but are constrained by their dependence on imperial patronage.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, elites_adopting_norms, beneficiary,
    powerful, biographical, constrained, national).

% The general population, particularly in remote or culturally distinct regions, who are pressured to adopt the new norms through a combination of symbolic example, institutional incentives, and subtle coercion. They bear the cost of abandoning traditional practices and conforming to the new order, with limited options for resistance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, unwilling_populace, payer,
    powerless, immediate, trapped, local).

% Groups or communities deeply invested in pre-existing norms and practices. They resist the new norms, facing social marginalization, loss of influence, or direct institutional penalties. Their resistance is often localized and fragmented, making exit from the imperial system difficult.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, traditionalist_factions, payer,
    moderate, biographical, constrained, regional).

% Scholars who analyze the historical processes of norm imposition and legitimation, seeking to understand the interplay of power, culture, and social change in state formation. They provide an external, analytical perspective on the constraint's operation and effects.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_state).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the adoption of new social and political norms across a diverse imperial territory, fostering a shared cultural identity and administrative coherence, by blending top-down symbolic authority with institutional incentives.
% TRANSFER_FUNCTION: Transfers social compliance, cultural alignment, and political loyalty from diverse local populations to the imperial state and its aligned elites, in exchange for institutional benefits, social order, and access to imperial resources.
% ABSENT_VOICES: Local cultural leaders, religious figures, and community elders whose traditional authority was undermined by the new norms. Their perspectives, emphasizing the value of existing practices and the disruption caused by imperial imposition, were systematically excluded from the legitimation discourse.
% DISAPPEARANCE_RATIONALE: If this hybrid legitimation mechanism had not existed, the imperial state would have faced much greater resistance to its new norms, potentially leading to widespread social unrest, fragmentation, or the failure of its state-building project. The norms would not have diffused effectively, and the empire's stability would have been severely compromised.
% FOUNDING_PROBLEM: The imperial state faced the challenge of integrating diverse populations with varied local customs and loyalties into a unified political and cultural entity, requiring the establishment of new, universally accepted norms to strengthen central authority and prevent fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Historical chronicles from various regions, archaeological evidence of administrative changes, and analyses by independent historians and anthropologists (outside the imperial state's own narratives) corroborate the existence of this problem and the imperial efforts to address it through such mechanisms.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).
:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while compliance was incentivized, it wasn't purely voluntary; there was a cost to non-compliance, but also benefits to adoption. Suppression (0.55) is also moderate, reflecting the use of institutional pressure and social marginalization rather than overt violence, which decreased over time as norms became internalized. Theater ratio (0.15) is low because the legitimation process, while strategic, was genuinely aimed at cultural integration and not merely performative. Accessibility collapse (0.40) and resistance (0.30) reflect the limited but not entirely absent options for non-compliance, which diminished as the norms became entrenched.
 *
 * PERSPECTIVAL GAP:
 *   The imperial state would perceive this as a successful and necessary coordination mechanism for empire-building, emphasizing the benefits of unity and progress. The unwilling populace and traditionalist factions, however, would experience it as an extractive imposition, a loss of cultural autonomy, and a burden on their traditional way of life. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial state and adopting elites are clear beneficiaries, gaining social cohesion, administrative efficiency, and political power. The unwilling populace and traditionalist factions are the payers, bearing the costs of cultural disruption and forced conformity. The directionality for the populace is high (near target) due to limited exit options and the imposition of new cultural costs, while elites benefit from aligning with the imperial agenda.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'What was the true balance between symbolic authority, institutional incentives, and direct coercion in the imposition of new norms?',
    'Detailed historical case studies comparing regions with varying degrees of imperial presence and local resistance, analyzing the specific mechanisms of norm diffusion and the costs of non-compliance.',
    'If symbolic authority and incentives were dominant, this ''hybrid_legitimation_reading'' is strongly supported. If direct coercion was more prevalent, the ''exogenous_override_reading'' gains strength, implying higher extraction and suppression. If grassroots adoption was primary, the ''endogenous_climb_reading'' would be favored, implying lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, empirical, 'Distinguishing the primary mechanism of norm imposition among hybrid, endogenous, and exogenous readings.').

omega_variable(
    legitimacy_source_ambiguity,
    'To what extent did the new norms achieve genuine internalized legitimacy versus mere instrumental compliance driven by incentives?',
    'Longitudinal studies of norm persistence after the decline of imperial power, or analysis of cultural artifacts and narratives reflecting popular sentiment rather than official discourse.',
    'If internalized legitimacy was high, the constraint''s long-term stability and lower suppression are more robust. If compliance was primarily instrumental, the constraint was more fragile and its ''extraction'' was higher, as it required continuous external reinforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Assessing the depth of norm legitimation beyond surface-level compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement(impo_tr_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(impo_be_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(impo_su_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 80, 0.47).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
