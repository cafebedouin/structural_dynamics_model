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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint represents the 'integration primary' reading of a
 *   federation's membership treaty, where free movement is considered a
 *   foundational and largely unrestricted right, essential for the single
 *   market. Restrictions are viewed as presumptively illegitimate and require
 *   narrow justification. This reading prioritizes federal integration over
 *   national sovereignty concerns, leading to a high degree of suppression of
 *   national-level policy autonomy regarding migration. The constraint is
 *   claimed as a Tangled Rope because it genuinely coordinates economic
 *   activity but does so with significant asymmetric extraction from local
 *   labor markets and national welfare systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.75).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Single Market Free Movement (Integration Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, '6e425632-4c78-46c0-b2c6-bf417ad6134e').
narrative_ontology:cs_kernel_codification('6e425632-4c78-46c0-b2c6-bf417ad6134e', formalized).
narrative_ontology:cs_authority_grounding('6e425632-4c78-46c0-b2c6-bf417ad6134e', lineage).
narrative_ontology:cs_interpretation_layer_present('6e425632-4c78-46c0-b2c6-bf417ad6134e').
narrative_ontology:cs_reading_relation('6e425632-4c78-46c0-b2c6-bf417ad6134e', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('6e425632-4c78-46c0-b2c6-bf417ad6134e', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('6e425632-4c78-46c0-b2c6-bf417ad6134e', foundational, free_movement_is_foundational_right).
narrative_ontology:cs_axiom_status(free_movement_is_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('6e425632-4c78-46c0-b2c6-bf417ad6134e', free_movement_is_foundational_right, deontological).
narrative_ontology:cs_axiom('6e425632-4c78-46c0-b2c6-bf417ad6134e', foundational, single_market_requires_unrestricted_mobility).
narrative_ontology:cs_axiom_status(single_market_requires_unrestricted_mobility, holdable).
narrative_ontology:cs_axiom_grounding('6e425632-4c78-46c0-b2c6-bf417ad6134e', single_market_requires_unrestricted_mobility, empirically_contingent).
narrative_ontology:cs_reference_frame('6e425632-4c78-46c0-b2c6-bf417ad6134e', ever_closer_union_framework).
narrative_ontology:cs_drift_state('6e425632-4c78-46c0-b2c6-bf417ad6134e', post_brexit_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('6e425632-4c78-46c0-b2c6-bf417ad6134e', '').
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

% Benefit from the right to seek employment and reside in any member state without significant administrative barriers. Their economic opportunities are expanded beyond national borders.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from a larger, more flexible labor pool and reduced costs for cross-border operations. They can optimize their workforce across the single market, increasing efficiency and profits.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Are the primary enforcers and proponents of free movement, viewing it as fundamental to the integration project. They interpret treaties and issue directives that prioritize market freedoms over national prerogatives.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federal_institutions, agenda_setter,
    institutional, civilizational, analytical, continental).

% Bear the costs of increased competition, wage depression in certain sectors, and potential strain on local public services due to influxes of workers. They have limited means to resist these pressures.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% Experience increased demand for social services and benefits, leading to fiscal strain or political pressure to restrict access. They are constrained by federal law from imposing significant restrictions on access for mobile citizens.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    organized, generational, constrained, national).

% Are obligated to implement and enforce free movement rules, often against domestic political pressure to protect national interests. They face legal challenges from federal institutions if they attempt to impose restrictions not narrowly justified.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, agenda_setter).

% Advocate for stronger national borders and restrictions on free movement, often framing it as a threat to national sovereignty and cultural identity. Their policy proposals are largely incompatible with the integration_primary reading and are actively resisted by federal institutions.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_populist_movements, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified economic space where labor and capital can move efficiently, fostering economic growth and reducing transaction costs across national borders within the federation.
% TRANSFER_FUNCTION: Transfers economic opportunities and social benefits to mobile workers and multinational corporations, while transferring costs (e.g., increased competition, welfare strain) to local labor markets and national welfare systems.
% ABSENT_VOICES: National populist movements and local communities disproportionately affected by rapid demographic shifts are often marginalized in the federal discourse, their concerns dismissed as protectionist or xenophobic, rather than legitimate interests to be balanced against integration.
% DISAPPEARANCE_RATIONALE: If free movement vanished overnight, the single market would fragment, supply chains would re-localize, and national economies would re-orient inward. The federal project itself would be fundamentally undermined, leading to a profound reorganization of political and economic structures.
% FOUNDING_PROBLEM: The original problem was to overcome national protectionism and create a larger, more efficient economic bloc to foster peace and prosperity after devastating wars.
% FOUNDING_PROBLEM_CORROBORATION: Federal institutions and pro-integration economists consistently attest that the problem of national protectionism and economic fragmentation remains a live threat, requiring continuous commitment to free movement. While some member states contest the degree of threat, the core problem is widely acknowledged by independent economic bodies and historical analysis.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because the benefits of free movement are concentrated among mobile workers and multinational corporations, while the costs are diffused across local communities and national welfare systems. Suppression (0.75) is high due to the robust legal and institutional mechanisms at the federal level that actively challenge and overturn national attempts to restrict movement. The theater ratio (0.1) is low, indicating that the enforcement is genuinely functional in maintaining free movement, with little performative pretense. The rising extractiveness and suppression over time reflect the deepening of integration and the increasing federal capacity to enforce this reading.
 *
 * PERSPECTIVAL GAP:
 *   Federal institutions and multinational corporations experience this as a highly beneficial Rope, enabling efficient market operation. Conversely, local labor markets and national welfare systems experience it as a Snare, bearing significant costs with limited recourse. Member state governments are caught between federal obligations and domestic pressures, experiencing it as a Tangled Rope where they must enforce rules that extract from their own populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and multinational corporations are clear beneficiaries (low d) as the constraint directly enables their mobility and profit optimization. Federal institutions are agenda-setters and beneficiaries (low d) as they gain power and legitimacy from the integration project. Local labor markets and national welfare systems are victims (high d) as they bear the direct costs. Member state governments are payers (high d) as they are compelled to absorb costs and enforce rules that may be unpopular domestically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (economic integration and prosperity) is still live, but its operation has accumulated significant extraction. The classification as Tangled Rope, rather than a pure Rope, prevents mislabeling the asymmetric costs as mere coordination overhead. The rising extractiveness and suppression indicate an enforcement ratchet where the 'coordination' function increasingly serves to maintain an extractive status quo, rather than purely solving a collective action problem. The 'integration primary' reading itself contributes to this drift by systematically de-prioritizing national-level concerns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_vs_sovereignty_balance,
    'Is the current balance between federal integration and national sovereignty optimal for long-term stability, or does the ''integration primary'' reading create unsustainable pressures on member states?',
    'Longitudinal studies of social cohesion and political stability in member states, coupled with analysis of policy outcomes from alternative readings (e.g., ''subsidiarity_balance'') implemented in other federations.',
    'If unsustainable, the constraint''s long-term viability as a Tangled Rope is questionable, potentially leading to political fragmentation or a reclassification towards Snare if resistance escalates. If optimal, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_balance, conceptual, 'The optimal balance point between federal integration and national sovereignty.').

omega_variable(
    cost_distribution_equity,
    'Are the costs of free movement (e.g., strain on welfare systems, wage depression) equitably distributed or adequately compensated across member states and local communities?',
    'Detailed economic analysis of fiscal transfers, social impact assessments, and comparative studies of compensation mechanisms. This would require granular data on the actual costs borne by different regions and demographics.',
    'If costs are inequitably distributed and uncompensated, the ''extraction'' component of the Tangled Rope is more severe than currently measured, potentially pushing the constraint closer to a Snare for the victim seats. If compensation mechanisms are effective, the extractiveness might be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_distribution_equity, empirical, 'Equity of cost distribution for free movement.').

omega_variable(
    founding_problem_drift,
    'Has the ''founding problem'' of national protectionism genuinely remained ''live'' as asserted by federal institutions, or has the constraint''s primary function drifted towards rent extraction under the guise of solving an increasingly attenuated problem?',
    'Independent historical and economic analysis comparing the severity of protectionism in the founding era versus contemporary challenges, and assessing whether current enforcement mechanisms are proportional to the actual threat.',
    'If the founding problem is largely ''dead'' or significantly attenuated, the constraint''s persistence and high extractiveness would be reclassified closer to a Snare or Piton, as its coordination function would be largely theatrical cover for ongoing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_drift, empirical, 'Whether the founding problem remains genuinely live or has attenuated, leading to functional drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_treaty__integration_primary, theater_ratio, 1993, 0.05).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_treaty__integration_primary, theater_ratio, 1998, 0.07).
narrative_ontology:measurement(fede_tr_t2003, federation_membership_treaty__integration_primary, theater_ratio, 2003, 0.08).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_treaty__integration_primary, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(fede_tr_t2013, federation_membership_treaty__integration_primary, theater_ratio, 2013, 0.1).
narrative_ontology:measurement(fede_tr_t2018, federation_membership_treaty__integration_primary, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(fede_tr_t2023, federation_membership_treaty__integration_primary, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_treaty__integration_primary, base_extractiveness, 1993, 0.4).
narrative_ontology:measurement(fede_be_t1998, federation_membership_treaty__integration_primary, base_extractiveness, 1998, 0.48).
narrative_ontology:measurement(fede_be_t2003, federation_membership_treaty__integration_primary, base_extractiveness, 2003, 0.55).
narrative_ontology:measurement(fede_be_t2008, federation_membership_treaty__integration_primary, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(fede_be_t2013, federation_membership_treaty__integration_primary, base_extractiveness, 2013, 0.63).
narrative_ontology:measurement(fede_be_t2018, federation_membership_treaty__integration_primary, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(fede_be_t2023, federation_membership_treaty__integration_primary, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_treaty__integration_primary, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(fede_su_t1998, federation_membership_treaty__integration_primary, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(fede_su_t2003, federation_membership_treaty__integration_primary, suppression_requirement, 2003, 0.65).
narrative_ontology:measurement(fede_su_t2008, federation_membership_treaty__integration_primary, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(fede_su_t2013, federation_membership_treaty__integration_primary, suppression_requirement, 2013, 0.73).
narrative_ontology:measurement(fede_su_t2018, federation_membership_treaty__integration_primary, suppression_requirement, 2018, 0.74).
narrative_ontology:measurement(fede_su_t2023, federation_membership_treaty__integration_primary, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, global_infrastructure).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.15).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('integration_primary') of the 'federation_membership_treaty' kernel. Other readings include 'sovereignty_primary' and 'subsidiarity_balance', which represent different interpretations of the balance between federal integration and national autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
