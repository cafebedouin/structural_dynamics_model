% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement as Primary Integration Principle
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the principle of free movement within the
 *   European Union, specifically as interpreted through the lens of
 *   'integration primary.' Under this reading, free movement is a
 *   foundational element of EU citizenship and the single market, requiring
 *   member states to yield their national welfare boundaries to ensure
 *   mobility rights. This often results in receiving member states bearing
 *   the costs of providing welfare and public services to mobile EU citizens,
 *   with ECJ authority expanding through case law to enforce these rights.
 *   This story is one reading of the 'federation_membership_obligations'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement as Primary Integration Principle").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '8929e7bc-2e3a-4919-80cc-092a770de40e').
narrative_ontology:cs_kernel_codification('8929e7bc-2e3a-4919-80cc-092a770de40e', formalized).
narrative_ontology:cs_authority_grounding('8929e7bc-2e3a-4919-80cc-092a770de40e', lineage).
narrative_ontology:cs_interpretation_layer_present('8929e7bc-2e3a-4919-80cc-092a770de40e').
narrative_ontology:cs_reading_relation('8929e7bc-2e3a-4919-80cc-092a770de40e', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('8929e7bc-2e3a-4919-80cc-092a770de40e', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('8929e7bc-2e3a-4919-80cc-092a770de40e', foundational, free_movement_is_unconditional_right).
narrative_ontology:cs_axiom_status(free_movement_is_unconditional_right, holdable).
narrative_ontology:cs_axiom_grounding('8929e7bc-2e3a-4919-80cc-092a770de40e', free_movement_is_unconditional_right, deontological).
narrative_ontology:cs_axiom('8929e7bc-2e3a-4919-80cc-092a770de40e', foundational, single_market_requires_labor_mobility).
narrative_ontology:cs_axiom_status(single_market_requires_labor_mobility, holdable).
narrative_ontology:cs_axiom_grounding('8929e7bc-2e3a-4919-80cc-092a770de40e', single_market_requires_labor_mobility, instrumental).
narrative_ontology:cs_reference_frame('8929e7bc-2e3a-4919-80cc-092a770de40e', maastricht_treaty_citizenship_framework).
narrative_ontology:cs_drift_state('8929e7bc-2e3a-4919-80cc-092a770de40e', post_brexit_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8929e7bc-2e3a-4919-80cc-092a770de40e', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, single_market_businesses).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_member_states).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Commission, European Parliament, and European Court of Justice (ECJ) actively promote and enforce free movement rights, viewing them as fundamental to EU citizenship and the single market. They benefit from deeper integration and expanded authority through case law.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals who exercise their right to live and work in any EU member state, gaining access to labor markets, social security, and public services in receiving states, often on equal terms with nationals. They are direct beneficiaries of the mobility rights.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% National governments and their welfare systems bear the costs of providing social benefits, healthcare, education, and other public services to mobile EU citizens. Their ability to restrict access to these services is legally constrained by EU law and ECJ rulings.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_member_states, payer,
    institutional, generational, constrained, national).

% Workers in receiving member states who may face increased competition for jobs, potential wage depression, or strain on local public services due to the influx of mobile EU labor. Their concerns are often subordinated to the broader integration agenda.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, constrained, local).

% Companies operating across the EU benefit from a larger, more flexible, and diverse labor pool, allowing them to recruit talent and manage labor costs more efficiently across borders. They are strong advocates for maintaining free movement.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, single_market_businesses, beneficiary,
    organized, biographical, arbitrage, global).

% Organizations and political parties in member states that prioritize the integrity and sustainability of national welfare systems. They often argue for greater national control over welfare access and express concerns about the fiscal and social impacts of unconditional free movement, but their policy proposals are often legally challenged by EU institutions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_welfare_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, eu_institutions).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates the functioning of the EU single market by ensuring the free movement of persons, capital, goods, and services, and establishes a common framework for EU citizenship rights across member states.
% TRANSFER_FUNCTION: Transfers the costs of providing welfare and public services for mobile EU citizens from their home states to receiving member states. It also transfers labor market flexibility and access benefits to businesses operating across the EU.
% ABSENT_VOICES: National welfare advocates and local labor unions in receiving states, whose concerns about welfare strain, wage depression, and social cohesion are often overridden by EU legal principles and institutional priorities.
% DISAPPEARANCE_RATIONALE: If free movement rights and their enforcement vanished overnight, the EU single market would fragment, national borders would re-emerge as significant barriers to labor mobility, and the concept of EU citizenship would be fundamentally undermined. This would lead to profound economic and political reorganization across the continent.
% FOUNDING_PROBLEM: The post-World War II desire to prevent future conflicts and foster economic prosperity in Europe through deep integration, creating interdependence and shared identity among member states.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions, pro-integration political parties, and many economists consistently attest to the ongoing relevance of integration for peace and prosperity. Critics acknowledge the historical context but dispute the current form's efficacy or fairness, particularly regarding welfare state sustainability.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high, reflecting the significant fiscal and social costs borne by receiving member states' welfare systems. Suppression (0.75) is high because member states' ability to restrict welfare access for EU citizens is severely curtailed by EU law and ECJ rulings, requiring active enforcement by EU institutions. The theater ratio (0.20) is low-moderate; while there is genuine legal enforcement, some political rhetoric from member states against 'welfare tourism' can be performative, given their limited legal avenues for restriction. Accessibility collapse (0.80) is high for member states seeking to restrict access, while resistance (0.60) is moderate, manifesting primarily through legal challenges and political discourse rather than outright defiance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions, this constraint is a successful coordination mechanism for deepening integration and fostering a cohesive single market. However, from the perspective of receiving member states and their local labor, it functions as an extractive mechanism, imposing costs and limiting national policy autonomy in welfare provision.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and mobile EU citizens are clear beneficiaries, gaining expanded authority/integration and direct access to welfare/labor markets, respectively. Single market businesses also benefit from a flexible labor pool. Receiving member states and displaced local labor are the primary payers, bearing the fiscal and social adjustment costs. The ECJ's role ensures that the directionality consistently favors integration and mobile citizens over national welfare state autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint—deepening European integration and fostering a single market—is still very much 'live.' However, the specific *form* of its implementation, particularly regarding unconditional welfare access, is subject to ongoing contestation. The constraint is not suffering from mandatrophy in its core function, but rather from a persistent tension between its integrationist goals and the sovereign interests of member states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_strain_empirical_significance,
    'Is the actual fiscal and social strain on receiving member states'' welfare systems due to mobile EU citizens empirically significant, or is it primarily a matter of political perception and rhetoric?',
    'Comprehensive, independent economic and social impact assessments across multiple member states, disaggregating costs and contributions of mobile EU citizens.',
    'If empirically significant, it strengthens the ''extraction'' argument for receiving states and could lead to calls for EU-level burden-sharing or revised welfare access rules. If not, it would weaken member states'' resistance and reinforce the integration primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_strain_empirical_significance, empirical, 'Empirical assessment of welfare strain from free movement.').

omega_variable(
    subsidiarity_principle_interpretation,
    'Where is the legitimate boundary between EU competence (free movement) and national competence (welfare policy) under the principle of subsidiarity, and how should this principle be interpreted in practice?',
    'Further ECJ jurisprudence clarifying the limits of EU competence, or intergovernmental agreements establishing clearer divisions of responsibility, potentially through treaty revision.',
    'A reinterpretation favoring national competence could reduce the extractiveness on receiving states, potentially shifting the constraint towards a more balanced ''rope'' or even ''scaffold'' if temporary measures are introduced. A continued broad interpretation reinforces the current ''tangled_rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidiarity_principle_interpretation, conceptual, 'Conceptual ambiguity regarding subsidiarity in free movement vs. welfare.').

omega_variable(
    identity_lock_eu_citizenship_strength,
    'How deeply is the identity of ''EU citizen'' fused with the right to free movement for mobile individuals, and how would a curtailment of welfare access affect this identity-lock?',
    'Sociological studies and surveys on identity formation among mobile EU citizens, and analysis of behavioral responses to policy changes affecting welfare access.',
    'If identity-lock is strong, mobile citizens might tolerate some curtailment of welfare access to preserve the broader identity and mobility right, dampening resistance. If weak, curtailment could lead to significant exit or political backlash, challenging the constraint''s stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_eu_citizenship_strength, empirical, 'Strength of identity-lock for mobile EU citizens regarding free movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_obligations__integration_primary, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_obligations__integration_primary, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(fede_tr_t2003, federation_membership_obligations__integration_primary, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_obligations__integration_primary, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(fede_tr_t2013, federation_membership_obligations__integration_primary, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(fede_tr_t2018, federation_membership_obligations__integration_primary, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(fede_tr_t2023, federation_membership_obligations__integration_primary, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_obligations__integration_primary, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(fede_be_t1998, federation_membership_obligations__integration_primary, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(fede_be_t2003, federation_membership_obligations__integration_primary, base_extractiveness, 2003, 0.58).
narrative_ontology:measurement(fede_be_t2008, federation_membership_obligations__integration_primary, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(fede_be_t2013, federation_membership_obligations__integration_primary, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(fede_be_t2018, federation_membership_obligations__integration_primary, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement(fede_be_t2023, federation_membership_obligations__integration_primary, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_obligations__integration_primary, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(fede_su_t1998, federation_membership_obligations__integration_primary, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(fede_su_t2003, federation_membership_obligations__integration_primary, suppression_requirement, 2003, 0.7).
narrative_ontology:measurement(fede_su_t2008, federation_membership_obligations__integration_primary, suppression_requirement, 2008, 0.73).
narrative_ontology:measurement(fede_su_t2013, federation_membership_obligations__integration_primary, suppression_requirement, 2013, 0.75).
narrative_ontology:measurement(fede_su_t2018, federation_membership_obligations__integration_primary, suppression_requirement, 2018, 0.78).
narrative_ontology:measurement(fede_su_t2023, federation_membership_obligations__integration_primary, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
