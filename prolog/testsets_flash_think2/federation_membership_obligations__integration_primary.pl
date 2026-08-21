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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: EU Free Movement and Welfare Integration Mandate
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint,
 *   'federation_membership_obligations__integration_primary', represents one
 *   reading of the contested kernel of EU federation membership. It asserts
 *   that free movement is a fundamental component of EU citizenship and
 *   single market functioning, requiring member state welfare boundaries to
 *   yield to mobility rights. This reading emphasizes the deepening of
 *   integration and the supremacy of EU law over national policies in this
 *   domain. The constraint is claimed as a Tangled Rope, reflecting its dual
 *   function of coordinating the single market while imposing significant,
 *   asymmetric costs on receiving member states and local labor.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement and Welfare Integration Mandate").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, 'ec71a889-cb4e-4734-b9ae-1440fc577a7c').
narrative_ontology:cs_kernel_codification('ec71a889-cb4e-4734-b9ae-1440fc577a7c', formalized).
narrative_ontology:cs_authority_grounding('ec71a889-cb4e-4734-b9ae-1440fc577a7c', lineage).
narrative_ontology:cs_interpretation_layer_present('ec71a889-cb4e-4734-b9ae-1440fc577a7c').
narrative_ontology:cs_reading_relation('ec71a889-cb4e-4734-b9ae-1440fc577a7c', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('ec71a889-cb4e-4734-b9ae-1440fc577a7c', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('ec71a889-cb4e-4734-b9ae-1440fc577a7c', foundational, eu_citizenship_indivisible).
narrative_ontology:cs_axiom_status(eu_citizenship_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('ec71a889-cb4e-4734-b9ae-1440fc577a7c', eu_citizenship_indivisible, deontological).
narrative_ontology:cs_axiom('ec71a889-cb4e-4734-b9ae-1440fc577a7c', foundational, single_market_supremacy).
narrative_ontology:cs_axiom_status(single_market_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('ec71a889-cb4e-4734-b9ae-1440fc577a7c', single_market_supremacy, conventional).
narrative_ontology:cs_reference_frame('ec71a889-cb4e-4734-b9ae-1440fc577a7c', ever_closer_union).
narrative_ontology:cs_drift_state('ec71a889-cb4e-4734-b9ae-1440fc577a7c', post_brexit_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ec71a889-cb4e-4734-b9ae-1440fc577a7c', '').
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

% The European Commission and European Court of Justice (ECJ) actively interpret and enforce free movement rights, often expanding their scope and limiting national discretion over welfare access. They benefit from a more integrated EU and the expansion of its legal authority.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% As EU citizens, they gain the right to reside and access social benefits in any member state, regardless of prior contributions in that state, significantly enhancing their mobility and security across the Union.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Businesses operating across the EU benefit from a larger, more flexible labor pool and reduced administrative barriers to hiring from other member states, lowering labor costs and increasing efficiency within the single market.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, single_market_businesses, beneficiary,
    organized, biographical, arbitrage, global).

% These states are legally obliged to provide welfare benefits to mobile EU citizens, incurring significant fiscal costs and perceived loss of sovereignty over their national welfare systems, often without commensurate fiscal transfers from the EU or other member states.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_member_states, payer,
    institutional, generational, constrained, national).

% Local workers in receiving states may face increased competition for jobs and downward pressure on wages, particularly in low-skilled sectors, bearing the adjustment costs of increased labor supply without direct compensation or adequate retraining programs.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, constrained, local).

% Groups advocating for the integrity and sustainability of national welfare states, often arguing for stricter conditions on welfare access for non-contributing migrants, find their arguments legally constrained by EU free movement principles and ECJ rulings.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_welfare_advocates, excluded,
    organized, biographical, constrained, national).

% The European Court of Justice is the ultimate arbiter of EU law, consistently ruling in favor of broader free movement rights and deeper integration, thereby expanding its own authority and the scope of EU law over national policies and welfare provisions.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, ecj, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates a unified single market and EU citizenship by ensuring labor mobility and non-discrimination across member states, solving collective action problems related to cross-border economic activity and social rights.
% TRANSFER_FUNCTION: Transfers welfare responsibilities and associated costs from mobile EU citizens' home states (or their own prior contributions) to the receiving member states. It also transfers economic adjustment costs to local labor in receiving states.
% ABSENT_VOICES: National welfare advocates and local labor unions are structurally excluded from the primary legal and institutional processes that shape this constraint; they would argue for stronger national control over welfare access and labor market protections, but their positions are often overridden by EU law.
% DISAPPEARANCE_RATIONALE: If free movement and welfare integration vanished overnight, the single market would fragment, labor mobility would cease, and national welfare systems would reassert full control over access, leading to significant economic, social, and political reorganization across the continent.
% FOUNDING_PROBLEM: Early European integration faced fragmented national markets, significant barriers to labor mobility, and discrimination against citizens from other member states, hindering economic growth, social cohesion, and the concept of a unified European identity.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and pro-integration economists consistently attest that the founding problems of market fragmentation and discrimination remain live, arguing that free movement is essential for continued economic prosperity and political stability. However, national governments and some economists contest the extent to which the original problem remains paramount over new challenges related to welfare sustainability and social dumping; legislative hearings and independent economic analyses from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is substantial due to the fiscal burden on receiving member states to provide welfare to mobile citizens without full reciprocity or compensatory transfers, and the adjustment costs borne by local labor. Suppression (0.75) is high because member states' ability to restrict free movement and welfare access is severely curtailed by EU treaties and ECJ jurisprudence, requiring active enforcement by EU institutions. The theater ratio (0.25) is moderate, indicating some performative resistance from national governments, but ultimate compliance with EU legal obligations. Accessibility collapse (0.7) reflects the significant reduction in member states' ability to control their welfare borders. Resistance (0.6) is present from national political actors and affected social groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions and mobile citizens, this constraint is a necessary and beneficial coordination mechanism for a unified Europe. However, from the perspective of receiving member states and displaced local labor, it operates as an extractive mechanism, imposing costs and eroding national sovereignty over welfare policy. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and mobile EU citizens are clear beneficiaries, gaining expanded authority and rights, respectively. Single market businesses also benefit from a flexible labor pool. Receiving member states and displaced local labor are the primary targets, bearing the fiscal and social costs of integration. The ECJ, while an agenda-setter, also acts as an analytical observer, interpreting the law to expand the constraint's scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, to foster integration and a single market, is still live. However, the *balance* of its operation is contested. While the founding problem of fragmented markets is largely addressed, the ongoing costs and perceived loss of sovereignty for some member states raise questions about whether the current implementation of the mandate has become overly extractive, rather than purely coordinative. The 'contested' status of the founding problem reflects this ongoing debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_balance,
    'Is the current balance between EU integration principles and national welfare state sovereignty optimal, or does it disproportionately burden receiving member states?',
    'Comprehensive fiscal impact studies on receiving member states, coupled with public referenda or intergovernmental negotiations on the acceptable level of welfare integration and burden-sharing mechanisms.',
    'If a disproportionate burden is confirmed, it would support re-negotiation of welfare access rules, potentially shifting the constraint towards a ''selective_solidarity'' reading or even ''member_sovereignty_primary'' by re-emphasizing national control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_balance, empirical, 'The ongoing tension between EU integration and national sovereignty over welfare.').

omega_variable(
    economic_benefit_distribution,
    'Are the economic benefits of free movement (e.g., increased labor supply, single market efficiency) equitably distributed among member states and social groups, or do they primarily accrue to businesses and specific regions?',
    'Detailed economic modeling and empirical studies on wage impacts, employment rates, and regional economic growth across the EU, disaggregated by skill level, sector, and nationality.',
    'If benefits are highly concentrated while costs are diffuse or borne by specific vulnerable groups, it would strengthen arguments for compensatory mechanisms, EU-level social funds, or a re-evaluation of the ''integration_primary'' reading''s fairness and sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_benefit_distribution, empirical, 'Fairness of economic benefit distribution from free movement.').

omega_variable(
    ecj_authority_scope,
    'Is the ECJ''s expansive interpretation of free movement rights a legitimate evolution of treaty principles, or an overreach that undermines democratic accountability in member states?',
    'Legal-constitutional analysis comparing ECJ jurisprudence with original treaty intent and national constitutional traditions, alongside political science studies on the democratic legitimacy of judicial activism in federal systems.',
    'If deemed an overreach, it could lead to calls for judicial reform or treaty amendments to rebalance power between EU and national institutions, potentially shifting the ''authority_grounding'' of the constraint or strengthening the ''member_sovereignty_primary'' reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecj_authority_scope, conceptual, 'Legitimacy of ECJ''s expansive interpretation of free movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t6, federation_membership_obligations__integration_primary, theater_ratio, 6, 0.17).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__integration_primary, theater_ratio, 12, 0.2).
narrative_ontology:measurement(fede_tr_t18, federation_membership_obligations__integration_primary, theater_ratio, 18, 0.22).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__integration_primary, theater_ratio, 24, 0.24).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__integration_primary, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t6, federation_membership_obligations__integration_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__integration_primary, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(fede_be_t18, federation_membership_obligations__integration_primary, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__integration_primary, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__integration_primary, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fede_su_t6, federation_membership_obligations__integration_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__integration_primary, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(fede_su_t18, federation_membership_obligations__integration_primary, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__integration_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__integration_primary, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('integration_primary') of the 'federation_membership_obligations' kernel. Sibling readings include 'member_sovereignty_primary' and 'selective_solidarity', which offer alternative interpretations of the balance between EU integration and national autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
