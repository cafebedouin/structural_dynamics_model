% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement (Integration Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'integration reading' of EU free movement,
 *   where supranational authority (ECJ) interprets free movement expansively
 *   to maximize labor mobility and equal treatment, often overriding national
 *   concerns. It is a reading of the 'federation_membership_kernel'. This
 *   reading emphasizes the fundamental right of EU citizenship and single
 *   market completion. The metrics reflect the increasing extractiveness and
 *   suppression as this interpretation has gained dominance, leading to
 *   identifiable victims among local labor and national welfare systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.75).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '4d9368fb-8552-47cb-aacf-1915caf79734').
narrative_ontology:cs_kernel_codification('4d9368fb-8552-47cb-aacf-1915caf79734', fixed_text).
narrative_ontology:cs_authority_grounding('4d9368fb-8552-47cb-aacf-1915caf79734', lineage).
narrative_ontology:cs_interpretation_layer_present('4d9368fb-8552-47cb-aacf-1915caf79734').
narrative_ontology:cs_reading_relation('4d9368fb-8552-47cb-aacf-1915caf79734', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('4d9368fb-8552-47cb-aacf-1915caf79734', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('4d9368fb-8552-47cb-aacf-1915caf79734', foundational, free_movement_fundamental_right).
narrative_ontology:cs_axiom_status(free_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('4d9368fb-8552-47cb-aacf-1915caf79734', free_movement_fundamental_right, deontological).
narrative_ontology:cs_axiom('4d9368fb-8552-47cb-aacf-1915caf79734', foundational, single_market_completion_requires_mobility).
narrative_ontology:cs_axiom_status(single_market_completion_requires_mobility, holdable).
narrative_ontology:cs_axiom_grounding('4d9368fb-8552-47cb-aacf-1915caf79734', single_market_completion_requires_mobility, instrumental).
narrative_ontology:cs_reference_frame('4d9368fb-8552-47cb-aacf-1915caf79734', treaty_of_maastricht_vision).
narrative_ontology:cs_drift_state('4d9368fb-8552-47cb-aacf-1915caf79734', post_2004_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4d9368fb-8552-47cb-aacf-1915caf79734', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, multinational_corporations).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_tax_bases).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to live and work anywhere in the EU, accessing labor markets and social benefits across borders. Their mobility is a core tenet of this reading.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% The European Commission and the European Court of Justice (ECJ) actively interpret and enforce free movement rights, expanding their scope to deepen integration and ensure equal treatment, often overriding national objections.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from a larger, more flexible labor pool across the single market, allowing them to optimize labor costs and talent acquisition without national restrictions.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Face increased competition for jobs and downward pressure on wages in receiving states, particularly in low-skill sectors, without adequate retraining or social support mechanisms.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, immediate, trapped, local).

% Bear the costs of providing social benefits and public services to new arrivals without corresponding fiscal transfers or sufficient time for integration into the tax base, leading to strain on national budgets and public services.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, biographical, constrained, national).

% Experience 'brain drain' as skilled workers leave, eroding their tax base and human capital without adequate compensation or mechanisms to attract return migration.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_tax_bases, payer,
    institutional, generational, constrained, national).

% Are compelled to implement ECJ rulings that expand free movement, often against domestic political pressure to protect national labor markets or welfare systems. They administer the system but have limited power to shape its fundamental direction under this reading.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_governments, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, member_state_governments, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor mobility and ensures non-discrimination across the EU single market, facilitating economic integration and preventing national protectionism from fragmenting the internal market.
% TRANSFER_FUNCTION: Transfers labor, human capital, and associated social costs from sending states to receiving states, and economic benefits to mobile citizens and multinational corporations, while centralizing interpretive authority in EU institutions.
% ABSENT_VOICES: National labor unions and local communities in receiving states, who would argue for stronger protections against wage depression and strain on public services, are often marginalized in the supranational legal discourse. Economically inactive citizens in sending states, who bear the costs of brain drain, also lack direct representation in this framing.
% DISAPPEARANCE_RATIONALE: If free movement as interpreted by the ECJ vanished, national borders would immediately reassert control over labor migration, leading to significant economic disruption for mobile citizens and multinational corporations, and a fundamental re-evaluation of the EU's federal project. Labor markets would re-nationalize, and welfare systems would face different pressures.
% FOUNDING_PROBLEM: The original problem was to prevent national protectionism from hindering economic integration and to create a common market for goods, services, capital, and people, ensuring non-discrimination based on nationality.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and mobile citizens attest that the problem of market fragmentation and discrimination remains live, requiring continuous supranational enforcement. Member state governments and displaced local labor, while acknowledging the original problem, argue that the current interpretation has overshot its original mandate and created new problems.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high because the expansive interpretation of free movement creates significant uncompensated costs for receiving state welfare systems and displaces local labor, while benefiting mobile citizens and multinational corporations. Suppression (0.75) is also high, as national governments' attempts to limit free movement or protect their welfare systems are consistently overridden by ECJ rulings, requiring active enforcement of the supranational interpretation. The theater ratio is low (0.15) because the EU institutions genuinely believe in and actively pursue the integrationist mandate; there is little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions and mobile citizens, this is a successful Rope, coordinating a fundamental right and deepening integration. From the perspective of displaced local labor and national welfare systems, it operates as a Snare or Tangled Rope, extracting resources and imposing costs without adequate compensation or democratic input. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and mobile EU citizens are primary beneficiaries, as the constraint directly enables their goals. Multinational corporations also benefit from expanded labor pools. Displaced local labor, receiving state welfare systems, and sending state tax bases are victims, bearing the costs of increased competition, fiscal strain, and brain drain, respectively. Member state governments are in a dual role, administering the system but often acting as payers when their national policies are overridden.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (deepening integration, preventing market fragmentation) is still live, but its implementation under this reading has created new, unaddressed problems (fiscal strain, social dumping concerns). The high extractiveness and suppression indicate that while coordination exists, it is heavily skewed, preventing it from being a pure Rope. The classification as Tangled Rope reflects this hybrid nature, where a genuine coordination function is intertwined with asymmetric extraction and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_compensation_mechanism,
    'What would be the fiscal cost and political feasibility of a robust EU-level fiscal compensation mechanism for receiving state welfare systems?',
    'Detailed economic modeling of fiscal transfers and political negotiation among member states to establish such a mechanism.',
    'If a feasible and effective compensation mechanism existed, the extractiveness from receiving state welfare systems would decrease, potentially shifting the constraint towards a more balanced Rope or Scaffold, as the coordination function would be less asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_compensation_mechanism, empirical, 'Uncertainty about the possibility of mitigating fiscal costs on receiving states.').

omega_variable(
    labor_market_impact_differentiation,
    'How do the labor market impacts of free movement differentiate across skill levels and sectors within receiving states, and are these impacts adequately measured and addressed?',
    'Granular, longitudinal studies of labor market dynamics in specific sectors and regions, disaggregated by skill level and origin of migrant workers.',
    'If impacts are highly concentrated on specific vulnerable groups, the ''displaced_local_labor'' victim group''s extractiveness and suppression are underestimated, pushing the constraint further towards a Snare for those specific groups. If impacts are diffuse and temporary, the current assessment holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_differentiation, empirical, 'Uncertainty regarding the precise distribution and severity of labor market impacts.').

omega_variable(
    supranational_vs_national_legitimacy,
    'Is the ECJ''s expansive interpretation of free movement perceived as legitimate by a majority of EU citizens, or is it seen as an imposition on national democratic sovereignty?',
    'Longitudinal public opinion surveys across member states, combined with analysis of national parliamentary debates and electoral outcomes related to EU integration and free movement.',
    'If legitimacy is low, the ''suppression'' metric is more accurately read as coercion, and the constraint''s stability depends more on institutional power than shared consent, pushing it towards a Snare. If legitimacy is high, the coordination function is stronger, supporting a Tangled Rope or even Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supranational_vs_national_legitimacy, conceptual, 'Ambiguity regarding the perceived legitimacy of supranational authority in this domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_kernel__integration_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_kernel__integration_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__integration_reading, theater_ratio, 2004, 0.13).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_kernel__integration_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__integration_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__integration_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__integration_reading, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(fede_be_t1998, federation_membership_kernel__integration_reading, base_extractiveness, 1998, 0.52).
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__integration_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement(fede_be_t2010, federation_membership_kernel__integration_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__integration_reading, base_extractiveness, 2016, 0.66).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__integration_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__integration_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(fede_su_t1998, federation_membership_kernel__integration_reading, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__integration_reading, suppression_requirement, 2004, 0.65).
narrative_ontology:measurement(fede_su_t2010, federation_membership_kernel__integration_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__integration_reading, suppression_requirement, 2016, 0.73).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__integration_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, welfare_coordination_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_single_market_regulation).

% DUAL FORMULATION NOTE:
% This constraint is the 'integration_reading' of the 'federation_membership_kernel'. Its expansive interpretation of free movement directly influences the operational space and legitimacy claims of the 'member_sovereignty_reading' and 'welfare_coordination_reading' by setting the default legal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
