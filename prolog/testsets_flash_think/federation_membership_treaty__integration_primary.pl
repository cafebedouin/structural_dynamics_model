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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Federation Single Market Free Movement (Integration Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'integration_primary' reading of the
 *   federation's membership treaty, where free movement is considered a
 *   foundational principle of the single market. Restrictions on movement are
 *   presumptively illegitimate and require narrow justification, placing the
 *   burden of proof heavily on member states. This reading emphasizes the
 *   benefits of deep integration while acknowledging the costs borne by
 *   national and local interests.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.8).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Federation Single Market Free Movement (Integration Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '993a6b4c-aa9c-4515-92f3-656bbb7b553e').
narrative_ontology:cs_kernel_codification('993a6b4c-aa9c-4515-92f3-656bbb7b553e', fixed_text).
narrative_ontology:cs_authority_grounding('993a6b4c-aa9c-4515-92f3-656bbb7b553e', lineage).
narrative_ontology:cs_interpretation_layer_present('993a6b4c-aa9c-4515-92f3-656bbb7b553e').
narrative_ontology:cs_reading_relation('993a6b4c-aa9c-4515-92f3-656bbb7b553e', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('993a6b4c-aa9c-4515-92f3-656bbb7b553e', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('993a6b4c-aa9c-4515-92f3-656bbb7b553e', foundational, free_movement_is_fundamental).
narrative_ontology:cs_axiom_status(free_movement_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('993a6b4c-aa9c-4515-92f3-656bbb7b553e', free_movement_is_fundamental, deontological).
narrative_ontology:cs_axiom('993a6b4c-aa9c-4515-92f3-656bbb7b553e', foundational, single_market_indivisibility).
narrative_ontology:cs_axiom_status(single_market_indivisibility, holdable).
narrative_ontology:cs_axiom_grounding('993a6b4c-aa9c-4515-92f3-656bbb7b553e', single_market_indivisibility, instrumental).
narrative_ontology:cs_reference_frame('993a6b4c-aa9c-4515-92f3-656bbb7b553e', ever_closer_union_principle).
narrative_ontology:cs_drift_state('993a6b4c-aa9c-4515-92f3-656bbb7b553e', contemporary_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('993a6b4c-aa9c-4515-92f3-656bbb7b553e', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, federation_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_corporations).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary enforcers and proponents of free movement, viewing it as essential for the federation's existence and prosperity. They gain legitimacy and power from deeper integration and the successful operation of the single market.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federation_institutions, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Benefit directly from the ability to seek employment and reside in any member state, accessing diverse labor markets and social benefits. Their mobility is a core right under this reading.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from a larger, unified labor pool and reduced administrative burdens for cross-border operations. They advocate for minimal restrictions on movement to optimize their supply chains and talent acquisition.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Bear the cost of reduced autonomy over national borders, labor market regulation, and social policy. They must justify any restrictions on free movement with a high bar, often facing legal challenges from federation institutions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).

% Experience downward pressure on wages or increased competition in certain sectors due to an influx of mobile workers. They have limited means to influence policy or exit the integrated market dynamics.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    powerless, biographical, trapped, local).

% Face increased demand for social services and benefits from mobile workers, leading to fiscal strain or political pressure to adapt. Their ability to restrict access is severely limited by federal law.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    organized, generational, constrained, national).

% Represent segments of the population critical of free movement's impact on national sovereignty and social cohesion. While politically active, their arguments for restrictions are largely excluded from the dominant integrationist legal and policy discourse.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_populist_parties, excluded,
    organized, biographical, mobile, national).

% Academics and policy analysts who study the economic, social, and political impacts of free movement, often highlighting the trade-offs between integration benefits and national costs without direct participation in enforcement or payment.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, federation_institutions).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates a unified economic area by removing barriers to labor and capital mobility, fostering economic growth and efficiency across the federation by ensuring a single, integrated market.
% TRANSFER_FUNCTION: Transfers regulatory autonomy and control over borders from member states to the federal level, enabling mobile workers to access labor markets and welfare systems, while imposing costs on local labor and welfare systems.
% ABSENT_VOICES: National populist parties and local communities concerned about social dumping or strain on public services are often marginalized in the discourse that prioritizes integration. Their concerns are framed as protectionist rather than legitimate interests.
% DISAPPEARANCE_RATIONALE: If free movement as a primary principle vanished, the single market would fragment, national borders would re-emerge as significant economic barriers, and the entire federal project would fundamentally alter or collapse, leading to widespread economic and political reorganization.
% FOUNDING_PROBLEM: Fragmented national markets, trade barriers, and inefficient allocation of labor and capital across the continent post-WWII, hindering economic recovery and political stability.
% FOUNDING_PROBLEM_CORROBORATION: Federation institutions and multinational corporations consistently attest to the ongoing need for free movement to ensure economic prosperity and competitiveness. Independent economists often corroborate the efficiency gains, though some also highlight distributional costs and social impacts.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is moderately high (0.65) because the principle of free movement, while coordinating the single market, imposes significant costs on member states' autonomy and local welfare systems. Suppression is high (0.80) due to the active legal and political enforcement by federation institutions against national restrictions. Theater ratio is low (0.15) as the constraint is genuinely and actively enforced, not merely performed. Accessibility collapse is high (0.75) because national alternatives to free movement are largely foreclosed. Resistance is moderate-high (0.60) reflecting ongoing political and social pushback from national interests.
 *
 * PERSPECTIVAL GAP:
 *   Federation institutions and mobile workers perceive this constraint as a beneficial 'Rope' or even a 'Mountain' (a natural economic law), essential for prosperity. Conversely, member state governments, local labor markets, and national welfare systems experience it as a 'Snare' or 'Tangled Rope' due to the imposed costs and loss of autonomy. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Federation institutions, mobile workers, and multinational corporations are clear beneficiaries, gaining from expanded markets and reduced barriers. Member state governments, local labor markets, and national welfare systems are targets, bearing the costs of integration and loss of regulatory control. The high suppression ensures that the costs are effectively borne by the targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (economic integration) is still live from the perspective of its beneficiaries. However, the high extractiveness and suppression, coupled with significant resistance, indicate that while it serves a coordination function, it also operates as an extractive mechanism for national autonomy. This prevents mislabeling it as a pure 'Rope' despite its coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_sovereignty_balance,
    'Is the current level of free movement, as interpreted by the ''integration_primary'' reading, optimal for overall federation welfare, or does it excessively infringe on national sovereignty and local interests?',
    'Comprehensive, independent cost-benefit analyses that account for both economic efficiency and social cohesion, alongside public referenda on specific aspects of free movement policy.',
    'If found to be excessively infringing, it could lead to re-negotiation of treaty articles or a shift towards a ''subsidiarity_balance'' reading, potentially reducing extractiveness and suppression from member states. If optimal, it reinforces the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_balance, conceptual, 'The optimal balance between federal integration and national autonomy.').

omega_variable(
    economic_efficiency_vs_social_cohesion,
    'Does the economic efficiency gained from free movement, as championed by this reading, genuinely outweigh the potential costs to social cohesion, public services, and local labor markets in host countries?',
    'Longitudinal empirical studies comparing economic growth and social indicators in regions with high and low immigration, controlling for other variables, and incorporating qualitative data on social impacts.',
    'Strong evidence of net negative social impacts could fuel political pressure for policy adjustments, potentially leading to a re-evaluation of the ''integration_primary'' axiom''s instrumental grounding. Strong evidence of net positive impacts would strengthen its legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_efficiency_vs_social_cohesion, empirical, 'Trade-off between economic efficiency and social cohesion due to free movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.17).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.16).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__integration_primary, theater_ratio, 40, 0.15).
narrative_ontology:measurement(fede_tr_t50, federation_membership_treaty__integration_primary, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__integration_primary, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__integration_primary, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__integration_primary, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__integration_primary, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, global_infrastructure).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, single_market_trade_rules).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_budget_contributions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
