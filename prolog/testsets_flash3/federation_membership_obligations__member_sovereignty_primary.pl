% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Sovereignty Primary in EU Free Movement
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'member sovereignty primary' reading of
 *   federation membership obligations, particularly within the European Union
 *   context. It asserts that national welfare states retain significant
 *   authority to control access to their social security systems and protect
 *   their labor markets, even for mobile EU citizens. Free movement is thus
 *   conditional on these national priorities, leading to a system where
 *   mobile workers may be excluded from full welfare benefits. This reading
 *   is actively enforced by member state governments to ensure fiscal
 *   sustainability and domestic labor protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.45).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.6).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.45).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Sovereignty Primary in EU Free Movement").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, 'a775d4f0-fa84-4868-83c0-56d4959f1d3f').
narrative_ontology:cs_kernel_codification('a775d4f0-fa84-4868-83c0-56d4959f1d3f', formalized).
narrative_ontology:cs_authority_grounding('a775d4f0-fa84-4868-83c0-56d4959f1d3f', lineage).
narrative_ontology:cs_interpretation_layer_present('a775d4f0-fa84-4868-83c0-56d4959f1d3f').
narrative_ontology:cs_reading_relation('a775d4f0-fa84-4868-83c0-56d4959f1d3f', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('a775d4f0-fa84-4868-83c0-56d4959f1d3f', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('a775d4f0-fa84-4868-83c0-56d4959f1d3f', foundational, national_welfare_autonomy_is_primary).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('a775d4f0-fa84-4868-83c0-56d4959f1d3f', national_welfare_autonomy_is_primary, conventional).
narrative_ontology:cs_axiom('a775d4f0-fa84-4868-83c0-56d4959f1d3f', foundational, free_movement_is_conditional_on_sustainability).
narrative_ontology:cs_axiom_status(free_movement_is_conditional_on_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('a775d4f0-fa84-4868-83c0-56d4959f1d3f', free_movement_is_conditional_on_sustainability, instrumental).
narrative_ontology:cs_reference_frame('a775d4f0-fa84-4868-83c0-56d4959f1d3f', westphalian_welfare_state_model).
narrative_ontology:cs_drift_state('a775d4f0-fa84-4868-83c0-56d4959f1d3f', contemporary_eu_jurisprudence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a775d4f0-fa84-4868-83c0-56d4959f1d3f', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_labor_forces).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_citizens_seeking_welfare).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, non_contributory_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These governments prioritize the fiscal sustainability of their national welfare systems and the protection of their domestic labor markets. They actively enforce policies that restrict welfare access for mobile EU citizens who have not made sufficient contributions or are not economically active, viewing this as a core aspect of national sovereignty within the federation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from policies that limit competition from mobile workers in certain sectors and protect the integrity of national social security systems. They support their governments' efforts to ensure that free movement does not undermine national employment standards or welfare provisions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_labor_forces, beneficiary,
    organized, biographical, constrained, national).

% These individuals face restrictions on accessing welfare benefits in host member states, often requiring proof of economic activity or sufficient resources. They bear the direct cost of these limitations, which can lead to precarity and exclusion, despite their EU citizenship status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_citizens_seeking_welfare, payer,
    powerless, immediate, constrained, regional).

% These migrants, often with limited or no prior contributions to the host state's welfare system, are the primary targets of restrictive welfare access policies. Their options are severely limited, often leading to destitution or forced return, as they lack the economic activity or resources required for integration.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, non_contributory_migrants, payer,
    powerless, immediate, trapped, local).

% The Commission advocates for the principle of free movement and non-discrimination, often challenging national restrictions on welfare access. However, under this reading, its authority to enforce broader integration principles is constrained by member states' asserted sovereignty over welfare and labor market policies.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_commission, excluded,
    institutional, generational, constrained, continental).

% Interprets EU law regarding free movement and social rights. While it has historically expanded the scope of EU citizenship rights, this reading emphasizes the limits of its jurisdiction when national welfare state sustainability and labor market protection are invoked by member states.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_court_of_justice, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between national welfare state autonomy and the principle of free movement within a federal or quasi-federal system, aiming to prevent 'welfare shopping' and protect national fiscal integrity.
% TRANSFER_FUNCTION: Limits the transfer of welfare benefits from host member states to mobile EU citizens and non-contributory migrants, effectively retaining resources within national systems for domestic populations.
% ABSENT_VOICES: Advocates for universal social rights and full EU citizenship equality would argue that national welfare boundaries undermine the spirit of European integration. They are often marginalized in national policy debates dominated by fiscal and labor market concerns.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, member states would lose a key mechanism for managing the fiscal and social impacts of free movement. There would likely be a rapid increase in welfare claims by mobile citizens, leading to significant fiscal strain and political backlash, forcing a renegotiation of federal principles or a collapse of national welfare systems.
% FOUNDING_PROBLEM: The tension between national welfare state models and the EU's commitment to free movement of persons, particularly concerning the fiscal burden on receiving states and the protection of national labor markets.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and national labor unions consistently attest to the ongoing nature of this problem, citing public opinion, budgetary pressures, and the need to maintain social cohesion. Independent economic analyses often corroborate the potential for fiscal strain if welfare access is entirely unconditional.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).
:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (balancing national autonomy with federal principles) but also involves asymmetric extraction. Member state governments and national labor forces benefit from the protection of national systems, while mobile EU citizens and non-contributory migrants bear the costs of restricted welfare access. The extractiveness (0.45) reflects the significant, but not total, burden placed on mobile individuals, while suppression (0.6) indicates the active legal and administrative barriers maintained by member states. Theater ratio (0.2) is low, as the stated goals of welfare sustainability and labor protection are genuinely pursued, though they also serve as cover for nationalistic preferences.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member state governments, this constraint is a necessary coordination mechanism to preserve national welfare states. From the perspective of mobile EU citizens, it is an extractive barrier that undermines the principle of free movement and equal citizenship. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments and national labor forces are beneficiaries, as the constraint protects their interests (low directionality). Mobile EU citizens and non-contributory migrants are targets, facing direct costs and limited access (high directionality). The European Commission and ECJ are excluded or observers, with their integrationist agenda constrained by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_threshold,
    'What is the empirically verifiable threshold at which unconditional welfare access for mobile citizens would genuinely threaten national welfare state sustainability?',
    'Longitudinal economic modeling and comparative studies across member states with varying welfare access policies, accounting for demographic shifts and economic cycles.',
    'If the current restrictions are found to be far below a genuine fiscal threat threshold, the extraction component of this constraint would be reclassified as higher, indicating less coordination and more pure rent-seeking. If the threat is imminent, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_threshold, empirical, 'Determining the true fiscal impact of welfare access for mobile citizens.').

omega_variable(
    sovereignty_vs_integration_framing,
    'Is the assertion of national welfare sovereignty a fundamental, irreducible principle, or a policy choice that could be re-framed within a broader integrationist framework without undermining core national interests?',
    'Conceptual analysis of federal theory and comparative constitutional law, alongside political science studies of national identity and European integration narratives.',
    'If framed as an irreducible principle, this reading''s ''member_sovereignty_primary'' stance is robust. If framed as a policy choice, it becomes more amenable to revision through political negotiation and legal challenge, potentially shifting the constraint towards a more ''integration_primary'' or ''selective_solidarity'' type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_framing, conceptual, 'Ambiguity in the conceptual grounding of national welfare state autonomy within a federal system.').

omega_variable(
    labor_market_protection_efficacy,
    'How effective are current restrictions on welfare access for mobile citizens in genuinely protecting national labor forces from adverse competition, versus merely creating a vulnerable underclass?',
    'Empirical studies on labor market segmentation, wage depression, and employment rates for both national and mobile workers in sectors affected by free movement, disaggregated by welfare access status.',
    'If restrictions are found to be ineffective in protecting national labor forces but create significant precarity for mobile workers, the ''suppression'' and ''extractiveness'' metrics would be re-evaluated upwards, and the coordination claim weakened. If effective, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_protection_efficacy, empirical, 'Assessing the actual impact of welfare restrictions on labor market protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 5, 0.17).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 10, 0.19).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 15, 0.2).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_obligations' kernel. This 'member_sovereignty_primary' reading emphasizes national control over welfare and labor markets, contrasting with 'integration_primary' (prioritizing free movement) and 'selective_solidarity' (tiered rights based on contribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
