% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty: Sovereignty Primary Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primary' reading of a
 *   federation membership treaty, where free movement is conditional on
 *   member state consent, and states retain authority to protect national
 *   labor markets and welfare systems. This reading prioritizes national
 *   regulatory autonomy over full federation integration, leading to a
 *   tangled rope dynamic where member states coordinate their national
 *   interests at the expense of mobile workers and federation-level
 *   coherence. The metrics reflect a moderately extractive and actively
 *   enforced constraint, with a low but rising theater ratio as national
 *   justifications for restrictions become more performative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.7).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty: Sovereignty Primary Reading").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '235c4113-eb5c-4ef7-a2a8-d6e472efddae').
narrative_ontology:cs_kernel_codification('235c4113-eb5c-4ef7-a2a8-d6e472efddae', fixed_text).
narrative_ontology:cs_authority_grounding('235c4113-eb5c-4ef7-a2a8-d6e472efddae', lineage).
narrative_ontology:cs_interpretation_layer_present('235c4113-eb5c-4ef7-a2a8-d6e472efddae').
narrative_ontology:cs_reading_relation('235c4113-eb5c-4ef7-a2a8-d6e472efddae', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('235c4113-eb5c-4ef7-a2a8-d6e472efddae', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('235c4113-eb5c-4ef7-a2a8-d6e472efddae', foundational, national_sovereignty_precedes_federation_competence).
narrative_ontology:cs_axiom_status(national_sovereignty_precedes_federation_competence, holdable).
narrative_ontology:cs_axiom_grounding('235c4113-eb5c-4ef7-a2a8-d6e472efddae', national_sovereignty_precedes_federation_competence, conventional).
narrative_ontology:cs_axiom('235c4113-eb5c-4ef7-a2a8-d6e472efddae', foundational, protection_of_national_labor_markets_is_legitimate_state_interest).
narrative_ontology:cs_axiom_status(protection_of_national_labor_markets_is_legitimate_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('235c4113-eb5c-4ef7-a2a8-d6e472efddae', protection_of_national_labor_markets_is_legitimate_state_interest, instrumental).
narrative_ontology:cs_reference_frame('235c4113-eb5c-4ef7-a2a8-d6e472efddae', westphalian_state_autonomy).
narrative_ontology:cs_drift_state('235c4113-eb5c-4ef7-a2a8-d6e472efddae', contemporary_federation_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('235c4113-eb5c-4ef7-a2a8-d6e472efddae', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, federation_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over national borders, labor market access, and welfare eligibility. They interpret the treaty to prioritize national interests, allowing them to impose conditions on free movement to protect domestic systems. They actively enforce these conditions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced competition from mobile workers, particularly in sectors sensitive to wage depression. This reading allows for policies that protect domestic employment and wage levels, even if it restricts labor mobility within the federation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Are protected from perceived strain due to immediate access by new arrivals. This reading allows member states to impose residency or contribution requirements before mobile workers can access social benefits, preserving the fiscal integrity of national systems.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_welfare_systems, beneficiary,
    institutional, generational, constrained, national).

% Face significant barriers to exercising free movement rights, including conditional entry, work permit requirements, and delayed access to social benefits. Their mobility is restricted, and they bear the costs of navigating diverse national regulations.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, regional).

% Bear the cost of fragmented internal borders and reduced policy coherence. Their mandate for deeper integration is challenged by member states asserting national sovereignty, leading to complex legal disputes and slower progress on common policies.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_institutions, payer,
    institutional, generational, constrained, continental).

% Argue for stronger free movement rights and reduced national barriers. Their arguments are often sidelined in favor of national sovereignty concerns, and they struggle to influence policy against entrenched member state interests.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, pro_integration_advocates, excluded,
    moderate, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to coordinate their national policies on migration and welfare, ensuring that each state retains sufficient autonomy to manage its internal affairs while participating in a broader federation.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy discretion from the federation level back to member states, at the cost of restricting the free movement rights of individuals and the coherence of federation-wide policies.
% ABSENT_VOICES: Pro-integration advocates and mobile workers' rights organizations are often excluded from the core decision-making processes where national sovereignty is asserted. They would argue for a more expansive interpretation of free movement and less restrictive national policies.
% DISAPPEARANCE_RATIONALE: If this reading of the treaty vanished, member states would lose their primary legal justification for restricting free movement based on national interests. This would likely lead to a rapid increase in labor mobility, significant pressure on national welfare systems, and a shift towards more centralized federation-level migration policies, fundamentally altering the balance of power.
% FOUNDING_PROBLEM: The original treaty sought to balance the benefits of economic integration and free movement with the need for member states to retain control over their national borders, labor markets, and social policies.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments consistently attest that the founding problem of balancing national sovereignty with federation integration remains live, citing ongoing concerns about migration flows and welfare system sustainability. This is corroborated by public opinion polls showing strong national preferences for border control, even if federation institutions and pro-integration scholars contest the necessity of such restrictions.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the costs imposed on mobile workers and the federation's institutions due to fragmented policies. Suppression (0.70) is high because member states actively enforce national restrictions, often through legal and administrative barriers. The theater ratio (0.20) is relatively low, as the national sovereignty claims are genuinely held, but there's a growing performative element as the justifications for restrictions are increasingly challenged by economic realities and federation law. Accessibility collapse (0.40) is moderate, as alternatives (e.g., moving to another member state with fewer restrictions) exist but are constrained. Resistance (0.55) is also moderate, coming from mobile worker advocacy groups and federation institutions.
 *
 * PERSPECTIVAL GAP:
 *   Member state governments and national labor/welfare systems experience this as a legitimate coordination mechanism that protects their interests. Mobile workers and federation institutions, however, experience it as an extractive and suppressive barrier to free movement and deeper integration. The engine will compute these divergent classifications from the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments, national labor markets, and national welfare systems are beneficiaries (d near 0.0) as they gain regulatory control and protection. Mobile workers and federation institutions are victims (d near 1.0) as they bear the costs of restricted movement and fragmented policy. The constraint subsidizes national autonomy by extracting from mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling national sovereignty as pure extraction by acknowledging the genuine coordination function of allowing states to manage their internal affairs. However, the rising extractiveness and suppression over time, coupled with the 'contested' status of the founding problem, suggest a drift towards a more extractive dynamic where the original coordination function is increasingly overshadowed by rent-seeking from national governments at the expense of federation principles. The constraint is a tangled rope because it genuinely coordinates national interests but does so with significant asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_balance,
    'Is the current balance between national sovereignty and federation integration optimal for overall welfare, or does this reading disproportionately favor national interests at the expense of federation-wide benefits?',
    'Comprehensive economic and social impact assessments comparing outcomes under different treaty interpretations, including counterfactual modeling of a more integrated scenario.',
    'If the balance is found to be suboptimal, it would strengthen arguments for reinterpreting the treaty towards greater integration, potentially reclassifying the constraint as more extractive than currently assessed. If optimal, it would reinforce the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_balance, empirical, 'Assessing the welfare implications of the sovereignty-primary balance.').

omega_variable(
    national_interest_definition,
    'How are ''national labor markets'' and ''welfare systems'' defined and protected under this reading? Are these definitions genuinely about protecting vulnerable populations, or are they used as cover for protectionist policies?',
    'Detailed analysis of specific national policies and their actual impact on labor market dynamics and welfare system sustainability, distinguishing between genuine protective measures and discriminatory practices.',
    'If definitions are found to be protectionist, it would increase the measured extractiveness and suppression, pushing the constraint closer to a snare. If genuinely protective, it would reinforce the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_interest_definition, conceptual, 'Clarifying the scope and intent of national interest protections.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''sovereignty primary'' reading of the federation treaty, or is it a strategic interpretation used by member states to resist deeper integration?',
    'Analysis of member state voting records, public statements, and legal arguments over time, compared against the historical intent of the treaty''s drafters and the evolving jurisprudence of the federation''s highest court.',
    'If it''s a strategic interpretation, the ''claimed_type'' of tangled_rope would be further validated, and the extractiveness might be higher than currently assessed, as the coordination story would be more of a cover. If it''s a genuine reading, the coordination function is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Distinguishing genuine reading from strategic interpretation of the treaty kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.16).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.17).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.18).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__sovereignty_primary, theater_ratio, 40, 0.19).
narrative_ontology:measurement(fede_tr_t50, federation_membership_treaty__sovereignty_primary, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__sovereignty_primary, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__sovereignty_primary, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__sovereignty_primary, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__sovereignty_primary, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.1).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_single_market_rules).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_social_policy_harmonization).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_treaty' kernel. This 'sovereignty_primary' reading emphasizes national autonomy, contrasting with 'integration_primary' (emphasizing free movement) and 'subsidiarity_balance' (seeking proportional limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
