% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty over Free Movement (EU Context)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'member sovereignty' reading of the EU's
 *   federation membership kernel, asserting that national welfare state
 *   capacity and labor market protection should bound free movement rights.
 *   Member states retain authority to exclude economically inactive migrants
 *   and protect social solidarity institutions. This reading prioritizes
 *   national control over supranational integration in migration policy,
 *   leading to constrained mobility for certain migrant groups.
 *
 * KEY AGENTS:
 *   - receiving_member_states: Agenda setter (institutional/constrained) — asserts national control
 *   - national_labor_unions: Beneficiary (organized/constrained) — protects domestic labor
 *   - economically_inactive_migrants: Payer (powerless/trapped) — bears exclusion costs
 *   - sending_member_state_workers: Payer (moderate/constrained) — faces restricted access
 *   - migrant_families: Payer (powerless/identity_locked) — affected by family separation/denial of social support
 *   - european_commission: Observer (institutional/analytical) — monitors compliance, balances national/supranational authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty over Free Movement (EU Context)").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'd2f19681-345d-49d3-85e5-d9251f8e4d0d').
narrative_ontology:cs_kernel_codification('d2f19681-345d-49d3-85e5-d9251f8e4d0d', formalized).
narrative_ontology:cs_authority_grounding('d2f19681-345d-49d3-85e5-d9251f8e4d0d', lineage).
narrative_ontology:cs_interpretation_layer_present('d2f19681-345d-49d3-85e5-d9251f8e4d0d').
narrative_ontology:cs_reading_relation('d2f19681-345d-49d3-85e5-d9251f8e4d0d', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2f19681-345d-49d3-85e5-d9251f8e4d0d', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('d2f19681-345d-49d3-85e5-d9251f8e4d0d', foundational, national_welfare_state_primacy).
narrative_ontology:cs_axiom_status(national_welfare_state_primacy, holdable).
narrative_ontology:cs_axiom_grounding('d2f19681-345d-49d3-85e5-d9251f8e4d0d', national_welfare_state_primacy, conventional).
narrative_ontology:cs_axiom('d2f19681-345d-49d3-85e5-d9251f8e4d0d', foundational, member_state_control_over_borders).
narrative_ontology:cs_axiom_status(member_state_control_over_borders, holdable).
narrative_ontology:cs_axiom_grounding('d2f19681-345d-49d3-85e5-d9251f8e4d0d', member_state_control_over_borders, conventional).
narrative_ontology:cs_reference_frame('d2f19681-345d-49d3-85e5-d9251f8e4d0d', post_maastricht_national_sovereignty).
narrative_ontology:cs_drift_state('d2f19681-345d-49d3-85e5-d9251f8e4d0d', contemporary_migration_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d2f19681-345d-49d3-85e5-d9251f8e4d0d', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_labor_unions).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_member_state_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, migrant_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert their right to control access to their welfare systems and labor markets, implementing policies to exclude economically inactive migrants and protect domestic social solidarity. They benefit from reduced welfare expenditure and perceived protection of national labor markets.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_member_states, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for policies that protect domestic workers from perceived downward pressure on wages and working conditions due to unrestricted labor mobility. They benefit from reduced competition in specific labor sectors.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_labor_unions, beneficiary,
    organized, biographical, constrained, national).

% Are directly targeted by exclusion policies, facing barriers to entry or residence based on their economic status. They bear the cost of restricted access to welfare benefits and social services, often leading to precarity.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, regional).

% Experience restricted access to labor markets in wealthier member states, limiting their opportunities for economic advancement and contributing to brain drain in their home countries. Their mobility is constrained by the receiving states' policies.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_member_state_workers, payer,
    moderate, biographical, constrained, regional).

% Are affected by policies that can separate families or deny access to social support based on the economic activity of one member. Their identity is often tied to the aspiration of family unity and stability across borders, making exit from the system difficult.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, migrant_families, payer,
    powerless, biographical, identity_locked, regional).

% Monitors member state compliance with EU law, including free movement principles. While it can initiate infringement procedures, its authority is balanced against member state sovereignty, leading to complex legal and political negotiations.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, european_commission, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the protection of national welfare systems and labor markets within a broader framework of free movement, allowing member states to manage the social and economic impacts of migration.
% TRANSFER_FUNCTION: Transfers the burden of welfare provision and labor market competition away from receiving member states and national workers, onto economically inactive migrants and workers from sending states, by restricting their access and rights.
% ABSENT_VOICES: Advocates for universal human rights and migrant solidarity, as well as businesses seeking flexible labor, are often marginalized in policy debates dominated by national welfare concerns. They would argue for more expansive and unconditional free movement.
% DISAPPEARANCE_RATIONALE: If this reading of the kernel vanished, member states would lose a key justification for restricting migrant access to welfare and labor markets. This would likely lead to increased migration flows, greater pressure on national welfare systems, and a significant shift in the balance of power between national and supranational authorities regarding free movement.
% FOUNDING_PROBLEM: The tension between the EU's principle of free movement and member states' sovereign right to manage their national welfare states and labor markets, particularly concerning the fiscal and social sustainability of welfare provisions.
% FOUNDING_PROBLEM_CORROBORATION: Academic literature on EU integration and welfare state sustainability, national political debates, and ongoing legal challenges at the European Court of Justice all corroborate the persistent tension. The problem is widely acknowledged by political scientists, economists, and legal scholars outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial, as it imposes significant costs on migrants and sending states by limiting access to opportunities and welfare. Suppression (0.70) is high due to active legal and administrative enforcement by member states to restrict entry and access. Theater ratio (0.20) is relatively low, as the stated goal of protecting national welfare is genuinely pursued, though with extractive consequences. The rising extractiveness and suppression over time reflect increasing nationalistic pressures and tightening migration policies within the EU.
 *
 * PERSPECTIVAL GAP:
 *   Receiving member states and national labor unions perceive this as a legitimate exercise of sovereignty and a necessary protection of national interests. Economically inactive migrants, sending state workers, and migrant families experience it as a highly extractive and suppressive barrier to their rights and opportunities. The European Commission observes a complex legal and political balancing act.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving member states and national labor unions are beneficiaries (low d) as they gain control and protection. Economically inactive migrants, sending state workers, and migrant families are targets (high d) as they bear the direct costs of exclusion and restricted access. The 'identity_locked' exit for migrant families reflects the deep personal and social costs of challenging or leaving the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the significant extraction from migrants) or a pure Snare (which would ignore the genuine, albeit contested, coordination function of protecting national welfare systems). The constraint's mandate to protect national welfare is still 'live', but its implementation has become increasingly extractive, indicating a drift towards a more Snare-like operation over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_welfare_sustainability_empirical_basis,
    'To what extent do economically inactive migrants genuinely pose an unsustainable burden on national welfare states, as opposed to being a politically convenient justification for restriction?',
    'Empirical studies on the net fiscal contribution of different migrant groups, disaggregated by economic activity and duration of stay, across various member states.',
    'If the burden is empirically negligible, the ''protection of welfare state capacity'' justification weakens, reclassifying the constraint closer to a Snare. If the burden is substantial, it reinforces the coordination aspect, keeping it a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_welfare_sustainability_empirical_basis, empirical, 'Empirical basis for claims of welfare state burden by migrants.').

omega_variable(
    labor_market_protection_efficacy,
    'Do restrictions on free movement for sending state workers genuinely protect national labor markets and wages, or do they primarily create labor shortages and informal economies?',
    'Comparative economic analysis of labor market outcomes in member states with varying degrees of free movement restriction, controlling for other economic factors.',
    'If protection is ineffective or counterproductive, the ''labor market protection'' justification weakens, increasing the perceived extractiveness and suppression. If effective, it supports the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_protection_efficacy, empirical, 'Efficacy of free movement restrictions in protecting national labor markets.').

omega_variable(
    sovereignty_vs_integration_conceptual_boundary,
    'At what point does the assertion of member state sovereignty over free movement fundamentally contradict the foundational principles of EU integration and citizenship?',
    'Legal and political philosophy analysis of the ''red lines'' where national control becomes an existential threat to the federal project, potentially leading to a ''forecloses'' relationship with the integration reading.',
    'If a clear contradiction is established, this reading could be seen as foreclosing the ''integration_reading'' within a coherent EU legal framework, rather than merely coexisting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_conceptual_boundary, conceptual, 'Conceptual boundary between national sovereignty and EU integration principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1992, 0.4).
narrative_ontology:measurement(fede_be_t2000, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement(fede_su_t2000, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2008, 0.63).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_kernel'. This 'member_sovereignty_reading' emphasizes national control, contrasting with the 'integration_reading' (supranational authority) and 'welfare_coordination_reading' (inter-state coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
