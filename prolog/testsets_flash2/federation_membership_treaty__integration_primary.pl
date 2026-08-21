% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Single Market Free Movement (Integration Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'integration primary' reading of a federal
 *   membership treaty, where free movement is considered a foundational and
 *   largely unrestricted right within the single market. Restrictions by
 *   member states are viewed as presumptively illegitimate and require narrow
 *   justification. The constraint is classified as a Tangled Rope because it
 *   genuinely coordinates economic activity across a large area, but also
 *   involves significant asymmetric extraction from local labor markets and
 *   national welfare systems, maintained by active federal enforcement. The
 *   metrics reflect a growing assertiveness of federal institutions in
 *   upholding this interpretation, leading to increased extractiveness and
 *   suppression over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.78).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Single Market Free Movement (Integration Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '6d458722-03eb-4df5-989d-fcbbd6ced9ff').
narrative_ontology:cs_kernel_codification('6d458722-03eb-4df5-989d-fcbbd6ced9ff', fixed_text).
narrative_ontology:cs_authority_grounding('6d458722-03eb-4df5-989d-fcbbd6ced9ff', lineage).
narrative_ontology:cs_interpretation_layer_present('6d458722-03eb-4df5-989d-fcbbd6ced9ff').
narrative_ontology:cs_reading_relation('6d458722-03eb-4df5-989d-fcbbd6ced9ff', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('6d458722-03eb-4df5-989d-fcbbd6ced9ff', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('6d458722-03eb-4df5-989d-fcbbd6ced9ff', foundational, free_movement_constitutive_of_single_market).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_single_market, holdable).
narrative_ontology:cs_axiom_grounding('6d458722-03eb-4df5-989d-fcbbd6ced9ff', free_movement_constitutive_of_single_market, deontological).
narrative_ontology:cs_axiom('6d458722-03eb-4df5-989d-fcbbd6ced9ff', secondary, restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('6d458722-03eb-4df5-989d-fcbbd6ced9ff', restrictions_presumptively_illegitimate, conventional).
narrative_ontology:cs_reference_frame('6d458722-03eb-4df5-989d-fcbbd6ced9ff', ever_closer_union_principle).
narrative_ontology:cs_drift_state('6d458722-03eb-4df5-989d-fcbbd6ced9ff', contemporary_political_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6d458722-03eb-4df5-989d-fcbbd6ced9ff', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_corporations).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, federal_institutions).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to seek employment and reside anywhere within the single market, accessing broader economic opportunities and social benefits. Their mobility is a core tenet of this reading.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from a flexible, continent-wide labor pool, reducing labor costs and simplifying cross-border operations. They actively lobby for the enforcement of free movement principles.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Are the primary enforcers and proponents of free movement, viewing it as fundamental to the single market's integrity and the federation's political project. They interpret treaty law to prioritize integration.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federal_institutions, agenda_setter,
    institutional, civilizational, analytical, continental).

% Bear the costs of increased competition, wage depression in certain sectors, and strain on local public services due to rapid influxes of workers. They have limited mechanisms to restrict entry or manage impacts.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% Experience increased demand for social benefits, healthcare, and housing, leading to fiscal pressure and political tension. Restrictions on access are often challenged by federal institutions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    organized, biographical, constrained, national).

% Are constrained in their ability to control national borders and manage migration flows, often facing political backlash from their electorates while being legally bound by federal treaty obligations. They must justify any restrictions very narrowly.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).

% Represent a significant segment of the electorate that opposes unrestricted free movement, advocating for stronger national border controls and protection of national interests. Their policy proposals are often deemed incompatible with federal law by federal institutions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_populist_movements, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates the efficient allocation of labor and capital across the single market, enabling businesses to find workers and individuals to find opportunities without national barriers, thereby boosting overall economic integration and growth.
% TRANSFER_FUNCTION: Transfers economic opportunities and social benefits to mobile workers and multinational corporations, while transferring costs (e.g., increased competition, strain on public services) to local labor markets and national welfare systems.
% ABSENT_VOICES: National populist movements and local communities experiencing direct negative impacts of rapid migration are often marginalized in the federal discourse, their concerns framed as protectionist or anti-integrationist rather than legitimate interests to be balanced.
% DISAPPEARANCE_RATIONALE: If free movement as a constitutive principle vanished, the single market would fragment, national borders would reassert control over labor flows, and the entire federal project would undergo a fundamental re-evaluation, leading to a significant reorganization of economic and political structures.
% FOUNDING_PROBLEM: The original problem was to overcome national protectionism and create a unified economic area to foster peace and prosperity through interdependence.
% FOUNDING_PROBLEM_CORROBORATION: Federal institutions and multinational corporations attest that the problem of economic fragmentation and the need for integration remain live. Member state governments and local labor markets, while acknowledging the original intent, argue that the current interpretation overshoots the original problem, creating new imbalances. Independent economic analyses corroborate the benefits of integration but also highlight the uneven distribution of costs.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the costs borne by specific local and national entities due to the broad application of free movement principles, which are not fully compensated or mitigated. Suppression (0.78) is high because federal institutions actively challenge and overturn national attempts to restrict movement or manage its impacts, making it difficult for member states to assert their sovereignty in this area. Theater ratio is low (0.15) as the enforcement is genuinely aimed at upholding the integration principle, not merely performing it. The rising extractiveness and suppression over the interval reflect the increasing judicial and political power of federal institutions in prioritizing integration.
 *
 * PERSPECTIVAL GAP:
 *   Federal institutions and mobile workers perceive this as a beneficial Rope, enabling prosperity and opportunity. Member state governments and local labor markets, however, experience it as a Snare or Tangled Rope, where their ability to manage local impacts is suppressed for the benefit of the broader federal project. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and multinational corporations are clear beneficiaries, gaining access to broader markets and labor pools (low directionality). Federal institutions are the agenda-setters and primary beneficiaries of this reading, as it strengthens their authority and the federal project (low directionality). Local labor markets and national welfare systems are victims, bearing the costs of increased competition and demand for services (high directionality). Member state governments are caught between federal obligations and national interests, often acting as payers (moderate-high directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (economic integration and peace) is still live, but its application has shifted from pure coordination to a mechanism that also facilitates significant transfers and imposes unmitigated costs. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the genuine coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_distribution_equity,
    'Are the benefits of free movement equitably distributed, or are the costs disproportionately borne by specific regions or social groups without adequate compensatory mechanisms?',
    'Comprehensive federal impact assessments that disaggregate costs and benefits by region and demographic, coupled with proposals for federal compensatory funds or adjustment mechanisms.',
    'If costs are found to be highly inequitable and uncompensated, the effective extractiveness of the constraint would be re-evaluated upward for victim groups, potentially shifting the classification closer to a Snare for those seats. If compensatory mechanisms are implemented, extractiveness would decrease.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_distribution_equity, empirical, 'Equity of cost distribution for free movement.').

omega_variable(
    legitimacy_of_national_restrictions,
    'What constitutes a ''narrowly justified'' restriction on free movement, and how does this balance against national sovereignty and subsidiarity principles?',
    'Further judicial clarification from the federal court, or a political re-negotiation of the treaty framework to explicitly define the scope of national derogations and the principle of proportionality.',
    'A broader interpretation of ''narrowly justified'' would reduce suppression on member states, potentially lowering extractiveness for national victims. A stricter interpretation would reinforce the current Tangled Rope classification, possibly increasing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_national_restrictions, conceptual, 'Defining the legitimate scope of national restrictions on free movement.').

omega_variable(
    federal_vs_national_identity_lock,
    'To what extent are member state governments identity-locked into the federal project, making exit or strong resistance to federal interpretations politically unfeasible?',
    'Analysis of public opinion and political discourse in member states regarding federal membership, and the electoral consequences for parties advocating for stronger national sovereignty over federal integration.',
    'If identity-lock is strong, the effective suppression on member state governments is higher than structural measures suggest, as their ''constrained'' exit options are further limited by political identity. This would amplify the extractive nature of the constraint from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_vs_national_identity_lock, empirical, 'Identity-lock of member states to the federal project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.14).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.15).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, global_infrastructure).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federal_budget_contributions).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, common_agricultural_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_treaty' kernel. This 'integration_primary' reading emphasizes free movement as constitutive of the single market, influencing and being influenced by other readings and related federal policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
