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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement: Integration Primary Reading
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'integration primary' reading of EU
 *   federation membership obligations, where free movement is a constitutive
 *   right of EU citizenship and single market functioning. Under this
 *   reading, member state welfare boundaries must yield to mobility rights,
 *   leading to mobile workers entering the full welfare beneficiary set in
 *   receiving states, and displaced local labor bearing adjustment costs. ECJ
 *   authority expands via case law to enforce this principle. The constraint
 *   is classified as a Tangled Rope due to its genuine coordination function
 *   (single market) coupled with asymmetric extraction from receiving states
 *   and local labor, requiring active enforcement by EU institutions.
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
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement: Integration Primary Reading").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '71b7e151-2270-484a-a8ed-0ae9082c838f').
narrative_ontology:cs_kernel_codification('71b7e151-2270-484a-a8ed-0ae9082c838f', formalized).
narrative_ontology:cs_authority_grounding('71b7e151-2270-484a-a8ed-0ae9082c838f', lineage).
narrative_ontology:cs_interpretation_layer_present('71b7e151-2270-484a-a8ed-0ae9082c838f').
narrative_ontology:cs_reading_relation('71b7e151-2270-484a-a8ed-0ae9082c838f', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('71b7e151-2270-484a-a8ed-0ae9082c838f', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('71b7e151-2270-484a-a8ed-0ae9082c838f', foundational, free_movement_as_constitutive_right).
narrative_ontology:cs_axiom_status(free_movement_as_constitutive_right, holdable).
narrative_ontology:cs_axiom_grounding('71b7e151-2270-484a-a8ed-0ae9082c838f', free_movement_as_constitutive_right, deontological).
narrative_ontology:cs_axiom('71b7e151-2270-484a-a8ed-0ae9082c838f', foundational, primacy_of_eu_law_in_single_market).
narrative_ontology:cs_axiom_status(primacy_of_eu_law_in_single_market, holdable).
narrative_ontology:cs_axiom_grounding('71b7e151-2270-484a-a8ed-0ae9082c838f', primacy_of_eu_law_in_single_market, conventional).
narrative_ontology:cs_reference_frame('71b7e151-2270-484a-a8ed-0ae9082c838f', ever_closer_union_principle).
narrative_ontology:cs_drift_state('71b7e151-2270-484a-a8ed-0ae9082c838f', contemporary_eurozone_crises_and_migration_pressures, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('71b7e151-2270-484a-a8ed-0ae9082c838f', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_institutions).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, receiving_member_states).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to live and work in any EU member state, accessing social security and welfare benefits in the receiving state on equal terms with nationals. Their mobility is a core right under this reading.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Interpret and enforce free movement as a foundational principle of EU citizenship and the single market. They expand ECJ authority through case law to ensure member state welfare boundaries yield to mobility rights, driving deeper integration.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Bear the fiscal and social costs of providing welfare and public services to mobile EU citizens, often without commensurate tax contributions, leading to pressure on national budgets and social cohesion. Their sovereignty over welfare policy is constrained by EU law.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_member_states, payer,
    institutional, generational, constrained, national).

% Experience downward pressure on wages and increased competition for jobs and social housing in sectors with high influxes of mobile EU workers. They bear the adjustment costs of free movement without direct compensatory mechanisms.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, immediate, trapped, local).

% Advocate for national sovereignty over borders and welfare, viewing free movement as an erosion of national identity and control. Their arguments are often dismissed as anti-EU or xenophobic within the dominant integrationist discourse.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_nationalists, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates the free movement of persons, goods, services, and capital across member states, enabling a deeper single market and fostering a sense of shared European citizenship by removing internal barriers.
% TRANSFER_FUNCTION: Transfers social and economic benefits (welfare access, labor market opportunities) to mobile EU citizens, and fiscal/social costs (welfare burden, labor market competition) to receiving member states and their local populations.
% ABSENT_VOICES: Nationalist and anti-EU political movements, as well as segments of local labor in receiving states, are often marginalized in the EU's integrationist discourse. They would argue for stronger national control over borders and welfare access.
% DISAPPEARANCE_RATIONALE: If free movement obligations vanished, member states would immediately reassert border controls and welfare restrictions, leading to a fragmentation of the single market, a collapse of EU citizenship as a meaningful concept, and a fundamental reordering of economic and social relations across the continent.
% FOUNDING_PROBLEM: Post-WWII Europe sought to prevent future conflicts and foster economic prosperity through integration, requiring the removal of barriers to movement and trade between nations.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and pro-integration political parties consistently affirm the founding problem of fragmentation and conflict prevention as live. While some member states and nationalist parties contest the extent to which free movement contributes to this, the core goal of integration remains widely accepted by a broad consensus of political and economic actors outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.65) reflects the uncompensated fiscal and social costs borne by receiving member states and local labor. Suppression (0.75) is high because member states' attempts to restrict welfare access for mobile citizens are actively challenged and overturned by EU institutions and the ECJ, requiring continuous enforcement to maintain the integrationist interpretation. The theater ratio (0.20) is low, indicating that the EU's enforcement of free movement is largely functional, though some rhetoric about 'abuse' of welfare systems by mobile citizens may be performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of mobile EU citizens and EU institutions, this constraint is a Rope, enabling fundamental rights and market efficiency. From the perspective of receiving member states and displaced local labor, it operates as a Snare or Tangled Rope, imposing uncompensated costs and eroding national sovereignty over welfare policy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU citizens are clear beneficiaries (d=0.0-0.2) as they gain access to new labor markets and welfare systems. EU institutions are also beneficiaries (d=0.0-0.1) as their authority and the integration project are advanced. Receiving member states and displaced local labor are targets (d=0.7-0.9) as they bear the direct costs. Member state nationalists are excluded, their concerns suppressed by the dominant integrationist narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (deepening integration, preventing conflict) is still live, but its implementation has shifted from pure coordination to a mechanism that also facilitates significant transfers and imposes uncompensated costs. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine coordination function of the single market).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_burden_quantification,
    'What is the precise fiscal burden on receiving member states due to mobile EU citizens'' welfare access, net of their economic contributions?',
    'Comprehensive, independent economic studies across multiple member states, disaggregating contributions and costs by origin and destination.',
    'If the net burden is demonstrably high, it would strengthen arguments for compensatory mechanisms or re-evaluation of welfare access rules, potentially shifting the constraint towards a more balanced Tangled Rope or even a Snare from the receiving state''s perspective. If low, it would support the integrationist narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_burden_quantification, empirical, 'Quantification of the net fiscal impact of free movement on receiving states.').

omega_variable(
    labor_market_impact_differentiation,
    'How do the labor market impacts of free movement differ across skill levels and sectors within receiving member states?',
    'Granular econometric analysis of wage and employment data, controlling for other economic factors, to isolate the effect of mobile EU labor on specific segments of the local workforce.',
    'If impacts are concentrated on low-skilled local labor, it would highlight the asymmetric costs and strengthen the ''victim'' status of this group, pushing the constraint towards a higher extractiveness for them. If impacts are diffuse or positive, it would weaken this claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_differentiation, empirical, 'Detailed analysis of free movement''s differentiated labor market effects.').

omega_variable(
    integration_vs_sovereignty_framing,
    'Is the tension between free movement and national welfare sovereignty an inherent structural conflict, or a solvable policy design problem?',
    'Conceptual analysis of federalist theory and comparative welfare state models, alongside policy experiments with compensatory or harmonizing mechanisms.',
    'If inherent, the constraint will remain a Tangled Rope, requiring continuous enforcement against national resistance. If solvable, policy reforms could reduce extraction and suppression, potentially moving it towards a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_framing, conceptual, 'Conceptual framing of the conflict between EU integration and national welfare state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__integration_primary, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__integration_primary, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__integration_primary, theater_ratio, 30, 0.18).
narrative_ontology:measurement(fede_tr_t40, federation_membership_obligations__integration_primary, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__integration_primary, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__integration_primary, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__integration_primary, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(fede_be_t40, federation_membership_obligations__integration_primary, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__integration_primary, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__integration_primary, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__integration_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(fede_su_t40, federation_membership_obligations__integration_primary, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.1).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_single_market_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('integration_primary') of the 'federation_membership_obligations' kernel. It emphasizes the constitutive nature of free movement and the primacy of EU law over national welfare boundaries. Sibling readings ('member_sovereignty_primary', 'selective_solidarity') offer alternative interpretations of the balance between EU integration and national control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
