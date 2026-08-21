% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Statehood requires recognition by the existing community of states (Constitutive Reading)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'constitutive reading' of the
 *   Montevideo Statehood Criteria, which posits that statehood is not merely
 *   a factual condition but requires recognition by the existing community of
 *   states to be legally effective. This reading grants existing states a
 *   structural veto over new state creation, leading to significant barriers
 *   for unrecognized polities. The claimed type is 'rope' (reflecting the
 *   theoretical framing of maintaining international order), but the metrics
 *   reflect its highly extractive and suppressive operation in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.85).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.9).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Statehood requires recognition by the existing community of states (Constitutive Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, 'e589cef8-4d3e-40d1-b83d-0ebebc12abe7').
narrative_ontology:cs_kernel_codification('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', formalized).
narrative_ontology:cs_authority_grounding('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', extraction).
narrative_ontology:cs_interpretation_layer_present('e589cef8-4d3e-40d1-b83d-0ebebc12abe7').
narrative_ontology:cs_reading_relation('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', montevideo_statehood_criteria__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', foundational, recognition_as_prerequisite).
narrative_ontology:cs_axiom_status(recognition_as_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', recognition_as_prerequisite, conventional).
narrative_ontology:cs_axiom('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', secondary, community_of_states_authority).
narrative_ontology:cs_axiom_status(community_of_states_authority, holdable).
narrative_ontology:cs_axiom_grounding('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', community_of_states_authority, conventional).
narrative_ontology:cs_reference_frame('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', post_montevideo_order).
narrative_ontology:cs_drift_state('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', contemporary_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e589cef8-4d3e-40d1-b83d-0ebebc12abe7', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, established_international_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the existing community of states who collectively determine whether to recognize new entities. They benefit from maintaining control over the international system's membership and stability, and from the structural veto over new state creation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Entities that meet objective criteria for statehood (territory, population, government, capacity to enter relations) but lack recognition from the community of states. They are denied full diplomatic access, treaty participation, and economic integration, severely limiting their development and security.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, generational, trapped, regional).

% Organizations like the UN, World Bank, and IMF whose membership and operational legitimacy are predicated on the existing state system. They benefit from the clarity and stability provided by a controlled process of state creation, even if it means excluding some entities.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, established_international_organizations, beneficiary,
    institutional, generational, constrained, global).

% Citizens of unrecognized polities who suffer from the lack of international legal standing, including limited travel rights, difficulty accessing international aid, and vulnerability to external aggression without the protection of international law. Their identity is often tied to the aspiration of statehood.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories, payer,
    powerless, generational, identity_locked, local).

% Academics and legal experts who analyze the criteria and practice of state recognition, often debating the merits of constitutive, declaratory, and hybrid theories. They observe the effects of non-recognition but do not directly participate in the decision-making process.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain the stability and order of the international system by controlling the proliferation of new state actors and ensuring that only viable and legitimate entities gain full membership in the community of states.
% TRANSFER_FUNCTION: Transfers legitimacy, diplomatic access, treaty rights, and economic integration from unrecognized polities to existing states, which retain the power to grant or withhold these benefits.
% ABSENT_VOICES: Unrecognized polities and their populations are largely excluded from the formal international forums where statehood is debated and decided. They would argue for self-determination and the primacy of objective criteria over political recognition.
% DISAPPEARANCE_RATIONALE: If recognition were no longer a prerequisite for statehood, numerous entities would immediately claim full statehood, leading to widespread border disputes, challenges to existing international agreements, and a breakdown of diplomatic norms as the international system would lack a clear definition of its members.
% FOUNDING_PROBLEM: To prevent arbitrary claims to statehood and ensure a stable, orderly international system, particularly after periods of conflict or decolonization, by establishing clear (albeit contested) criteria for new state creation.
% FOUNDING_PROBLEM_CORROBORATION: Existing states and some traditional international legal scholars argue the problem of maintaining international order and preventing chaos is still live. Unrecognized polities and critical international relations scholars argue the founding problem is largely solved, and the constraint now serves to maintain existing power structures and deny self-determination, with corroboration from historical analyses of state practice.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is very high because unrecognized polities are denied fundamental rights and access in the international system. Suppression (0.90) is also very high, as the collective non-recognition by powerful states creates an almost insurmountable barrier to full statehood. The theater ratio (0.10) is low because the act of non-recognition is direct and impactful, not merely performative. Accessibility collapse (0.95) is near total, as there are virtually no alternative paths to full statehood without recognition. Resistance (0.75) is high, as unrecognized entities actively campaign for recognition and challenge the legitimacy of the constitutive theory.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existing states, the constitutive reading is a necessary mechanism for international order and stability. From the perspective of unrecognized polities, it is a tool of oppression and a denial of self-determination. The engine's classification will likely reflect this divergence, computing a highly extractive type for the payer seats despite the claimed 'rope' type.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing states and established international organizations are clear beneficiaries, as they control the system's membership and maintain the status quo. Unrecognized polities and their populations are the primary targets, bearing the full cost of exclusion. The directionality for existing states is near 0.0 (full beneficiary), while for unrecognized polities it is near 1.0 (full target).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_as_power_projection,
    'Is the requirement of recognition a genuine coordination mechanism for international order, or primarily a tool for existing states to project power and maintain geopolitical advantage?',
    'Empirical analysis of recognition patterns: if recognition correlates more strongly with geopolitical interests of powerful states than with objective statehood criteria or normative legitimacy, it supports the power projection hypothesis.',
    'If primarily power projection, the constraint''s effective extraction is higher and its coordination function is largely cover, pushing classification towards Snare. If genuine coordination, it supports the Rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_as_power_projection, empirical, 'Whether recognition serves order or power.').

omega_variable(
    community_of_states_definition,
    'Who constitutes the ''existing community of states'' whose recognition is required, and how is their collective will determined?',
    'Analysis of historical state practice and international legal instruments to identify the de facto and de jure mechanisms of collective recognition, and the influence of individual powerful states within this process.',
    'If the ''community'' is effectively a small group of powerful states, the constraint''s suppression is more concentrated and its legitimacy more easily challenged. If it''s a broad, democratic consensus, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_of_states_definition, conceptual, 'Ambiguity in the ''community of states'' definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement(mont_tr_t1950, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(mont_tr_t1970, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1970, 0.11).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1933, 0.75).
narrative_ontology:measurement(mont_be_t1950, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1950, 0.8).
narrative_ontology:measurement(mont_be_t1970, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1970, 0.82).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1990, 0.83).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1933, 0.8).
narrative_ontology:measurement(mont_su_t1950, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement(mont_su_t1970, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1970, 0.87).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
