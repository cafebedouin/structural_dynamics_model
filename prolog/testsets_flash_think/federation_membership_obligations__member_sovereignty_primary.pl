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
 *   human_readable: National Welfare State Closure Authority (Member Sovereignty Primary Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the assertion of national welfare state closure
 *   authority, where free movement within a federal or supranational entity
 *   (like the EU) is conditional on protecting national labor markets and
 *   welfare system sustainability. This is presented as the
 *   'member_sovereignty_primary' reading of the broader
 *   'federation_membership_obligations' kernel, emphasizing national control
 *   over social policy. The constraint is actively enforced through
 *   legislation, court rulings, and administrative practices that limit
 *   mobile workers' access to benefits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "National Welfare State Closure Authority (Member Sovereignty Primary Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '9f406422-21ef-4bd1-b8e0-e2ac4c21a435').
narrative_ontology:cs_kernel_codification('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', formalized).
narrative_ontology:cs_authority_grounding('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', lineage).
narrative_ontology:cs_interpretation_layer_present('9f406422-21ef-4bd1-b8e0-e2ac4c21a435').
narrative_ontology:cs_reading_relation('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', federation_membership_obligations__integration_primary, forecloses).
narrative_ontology:cs_reading_relation('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', foundational, national_sovereignty_over_welfare).
narrative_ontology:cs_axiom_status(national_sovereignty_over_welfare, holdable).
narrative_ontology:cs_axiom_grounding('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', national_sovereignty_over_welfare, deontological).
narrative_ontology:cs_axiom('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', foundational, welfare_system_sustainability_imperative).
narrative_ontology:cs_axiom_status(welfare_system_sustainability_imperative, holdable).
narrative_ontology:cs_axiom_grounding('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', welfare_system_sustainability_imperative, instrumental).
narrative_ontology:cs_reference_frame('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', westphalian_welfare_state).
narrative_ontology:cs_drift_state('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', contemporary_eu_migration_debates, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9f406422-21ef-4bd1-b8e0-e2ac4c21a435', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_labor_force).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, eu_institutions).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, pro_integration_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These governments assert and enforce national control over welfare access and labor market regulations for non-citizens, arguing it is essential for national sovereignty and welfare system sustainability. They actively legislate and litigate to maintain this authority within federal structures like the EU.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the perceived protection of their welfare systems and labor markets, as mobile workers are restricted from full access. They often support policies that prioritize national citizens' access to social benefits and employment.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_citizens, beneficiary,
    organized, biographical, mobile, national).

% Benefits from reduced competition in certain labor market segments and the protection of national labor standards, which are seen as vulnerable to downward pressure from unrestricted free movement. They advocate for policies that prioritize their employment and working conditions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_labor_force, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of restricted welfare access and potential labor market discrimination in receiving states. Despite free movement rights, their ability to fully integrate and access social safety nets is conditional and often delayed, leading to precarity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, regional).

% Experience their integration agenda challenged and constrained by member states' assertion of welfare sovereignty. They bear the political cost of internal disputes and the administrative burden of mediating between national and supranational legal principles.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, eu_institutions, payer,
    institutional, generational, constrained, continental).

% Argue for stronger mobility rights and greater welfare solidarity across member states, seeing national closure as undermining the foundational principles of the federation. Their arguments are often sidelined in national political debates focused on sovereignty.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, pro_integration_advocates, excluded,
    moderate, generational, constrained, continental).

% Analyze the legal and political implications of national welfare state closure within federal structures, assessing its compatibility with human rights, non-discrimination principles, and the evolution of international and supranational law.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To protect the fiscal and social sustainability of national welfare states and the stability of national labor markets from perceived pressures arising from unrestricted free movement within a federal or supranational entity.
% TRANSFER_FUNCTION: Transfers social security, healthcare, and unemployment benefits from mobile workers (who contribute but face restricted access) to national citizens and labor forces (who receive full access and protection). It also transfers political authority over social policy from supranational bodies back to national legislatures.
% ABSENT_VOICES: Mobile workers' rights organizations, EU integrationist civil society groups, and international human rights advocates are often marginalized in national debates, where the focus is on national interests and sovereignty. They would argue for universal rights and greater solidarity.
% DISAPPEARANCE_RATIONALE: If national welfare states lost their closure authority overnight, there would be immediate and profound fiscal and social pressures on national systems, potentially leading to rapid integration of welfare policies at the federal level or a collapse of national systems. Labor markets would also experience significant shifts, reorganizing around new mobility patterns.
% FOUNDING_PROBLEM: The perceived threat to the fiscal and social sustainability of national welfare states and the stability of national labor markets posed by the principle of free movement within a federal or supranational union.
% FOUNDING_PROBLEM_CORROBORATION: National governments, some economists, and populist political movements consistently attest to the ongoing nature of this problem, citing demographic shifts and economic disparities. While EU institutions and pro-integration scholars contest the severity or framing of the problem, the national perspective remains a powerful force in policy debates.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because mobile workers contribute to the economy but face significant restrictions on accessing social benefits, effectively subsidizing national systems. Suppression is also high (0.75) due to the active legal and administrative enforcement mechanisms that prevent full integration and access for mobile workers. Theater ratio is low (0.15) as the enforcement is genuine and directly serves the stated goal of national protection, rather than being merely performative. Resistance is moderate (0.55) from pro-integration groups and mobile workers, but often outmatched by national political will. Accessibility collapse is moderate (0.60) because while free movement exists, full access to the benefits of citizenship in a receiving state is significantly curtailed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member state governments and national citizens, this constraint is a legitimate and necessary defense of national interests and social cohesion, framing it as a 'tangled_rope' that coordinates national welfare. From the perspective of mobile workers and EU institutions, it is a 'snare' that undermines fundamental rights and the principles of free movement, extracting resources while denying full participation. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments, national citizens, and the national labor force are the primary beneficiaries, as the constraint protects their perceived interests and resources. Mobile workers, EU institutions, and pro-integration advocates are the targets, bearing the costs of restricted access, political friction, and a challenged integration agenda. The directionality for mobile workers is high (near 1.0) due to their limited exit options and direct experience of extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'How does this ''member_sovereignty_primary'' reading structurally differ from its sibling readings, ''integration_primary'' and ''selective_solidarity''?',
    'Comparative legal and policy analysis of each reading''s core tenets, stakeholder beneficiaries/victims, and enforcement mechanisms.',
    'Understanding the precise structural deltas clarifies the points of contestation and the specific mechanisms of extraction or coordination unique to this reading, informing the classification of each sibling constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Clarifies the specific structural instantiation of this kernel reading.').

omega_variable(
    welfare_sustainability_empirical_vs_political,
    'Is the claim of ''welfare system sustainability'' an empirically verifiable economic necessity for closure, or a politically constructed justification for restricting free movement?',
    'Independent economic modeling of welfare system resilience under various migration scenarios, disaggregated by contribution and benefit usage, compared against political discourse analysis.',
    'If primarily political, the ''coordination'' function of this constraint is weakened, increasing its effective extractiveness and pushing its classification closer to a ''snare''. If empirically robust, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_sustainability_empirical_vs_political, empirical, 'Distinguishes between genuine economic necessity and political rhetoric in justifying welfare closure.').

omega_variable(
    labor_market_protection_efficacy,
    'To what extent does national closure authority genuinely protect the national labor force from adverse effects of free movement, versus merely creating barriers for mobile workers without significant national benefit?',
    'Longitudinal studies comparing labor market outcomes (wages, employment, working conditions) for national citizens in sectors with high vs. low mobile worker presence, controlling for other economic factors.',
    'If protection is minimal or non-existent, the ''coordination'' function for the national labor force is weakened, increasing the perceived extraction from mobile workers and potentially reclassifying the constraint as more purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_protection_efficacy, empirical, 'Assesses the actual protective effect of closure authority on national labor markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2004, 0.14).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement(fede_be_t1998, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(fede_be_t2004, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2004, 0.62).
narrative_ontology:measurement(fede_be_t2010, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(fede_be_t2016, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2016, 0.7).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1992, 0.6).
narrative_ontology:measurement(fede_su_t1998, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(fede_su_t2004, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2004, 0.7).
narrative_ontology:measurement(fede_su_t2010, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(fede_su_t2016, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
