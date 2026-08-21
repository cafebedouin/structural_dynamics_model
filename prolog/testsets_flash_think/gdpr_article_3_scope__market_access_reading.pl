% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Scope: Market Access Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint models GDPR Article 3's scope from a 'market access'
 *   perspective, where its extraterritoriality is understood as a 'Brussels
 *   Effect' standard-setting mechanism rather than a direct assertion of
 *   effects-based jurisdiction. Non-EU businesses comply to gain access to
 *   the EU's single market, making compliance a strategic choice rather than
 *   pure coercion. The claimed type is 'rope' because it facilitates market
 *   coordination under a common standard, despite imposing significant costs
 *   on non-EU actors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.45).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.55).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Scope: Market Access Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, 'bc5ac184-b0cf-46df-99df-1b2a8a4279ea').
narrative_ontology:cs_kernel_codification('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', formalized).
narrative_ontology:cs_authority_grounding('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', lineage).
narrative_ontology:cs_interpretation_layer_present('bc5ac184-b0cf-46df-99df-1b2a8a4279ea').
narrative_ontology:cs_reading_relation('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', foundational, market_access_conditions_are_sovereign_prerogative).
narrative_ontology:cs_axiom_status(market_access_conditions_are_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', market_access_conditions_are_sovereign_prerogative, conventional).
narrative_ontology:cs_axiom('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', secondary, regulatory_standards_diffuse_globally_via_market_power).
narrative_ontology:cs_axiom_status(regulatory_standards_diffuse_globally_via_market_power, holdable).
narrative_ontology:cs_axiom_grounding('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', regulatory_standards_diffuse_globally_via_market_power, empirically_contingent).
narrative_ontology:cs_reference_frame('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', eu_single_market_regulatory_power).
narrative_ontology:cs_drift_state('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bc5ac184-b0cf-46df-99df-1b2a8a4279ea', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_regulatory_bodies).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_citizens).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_businesses).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_businesses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce GDPR, thereby extending EU regulatory influence globally by setting standards for market access. They benefit from the increased legitimacy and power of EU law.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Comply with GDPR to access the lucrative EU market, incurring significant costs for data processing, legal counsel, and operational changes. Leaving the EU market is often not a viable option.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_businesses, payer,
    powerful, biographical, constrained, global).

% Benefit from strong privacy protections for their data, regardless of where the processing company is located. They experience increased trust in digital services.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_citizens, beneficiary,
    organized, biographical, mobile, regional).

% Benefit from a level playing field where non-EU competitors must meet the same high privacy standards, reducing competitive disadvantages that might arise from lower regulatory burdens elsewhere.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_businesses, beneficiary,
    powerful, biographical, mobile, regional).

% Legal scholars and some regulators who argue that GDPR's reach is primarily about protecting EU residents wherever they are, not just market access. They analyze the legal implications of the 'targeting' and 'monitoring' clauses.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, effects_jurisdiction_advocates, observer,
    analytical, generational, analytical, universal).

% National governments and legal bodies from outside the EU who argue that GDPR's extraterritorial application is an overreach of traditional territorial sovereignty. Their arguments are largely sidelined in the market-access framing of GDPR.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, territorial_sovereignty_advocates, excluded,
    institutional, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, eu_regulatory_bodies).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, high standard for data protection for all entities wishing to operate in the EU market, ensuring a baseline of trust and fair competition for digital services.
% TRANSFER_FUNCTION: Moves compliance costs from non-EU businesses to EU citizens (via better privacy) and EU businesses (via a level playing field). It also transfers regulatory influence from national jurisdictions to the EU.
% ABSENT_VOICES: Businesses that choose to entirely forgo the EU market rather than comply are not part of the conversation about GDPR's scope. Advocates for strict territorial sovereignty are also largely excluded from this market-access framing.
% DISAPPEARANCE_RATIONALE: If GDPR's market access requirement vanished overnight, non-EU businesses would likely revert to lower privacy standards, creating an uneven playing field for EU businesses and eroding trust in the digital economy for EU citizens. The global standard for data protection would likely fragment and lower.
% FOUNDING_PROBLEM: Fragmented and weak data protection laws across jurisdictions, leading to a race to the bottom in privacy and an inability for the EU to protect its citizens' data when processed by foreign entities.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions, privacy advocacy groups, and many EU citizens corroborate the ongoing need for strong data protection. While some non-EU businesses might contest the necessity of the current solution, the broad consensus on the problem's existence and the need for a robust framework is strong, supported by legislative hearings and public surveys.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).
:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because compliance costs are substantial, but the choice to enter the EU market mitigates pure extraction. Suppression is moderate as businesses can technically opt out of the EU market, but for many, it's economically unfeasible. Theater ratio is low because the compliance required for market access is genuinely functional, not performative. The increasing extractiveness and suppression over time reflect the maturing enforcement mechanisms and the deepening entrenchment of GDPR as a global standard.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-EU businesses, GDPR's market access requirement is a costly barrier, while from the EU's perspective, it's a necessary coordination mechanism to protect its citizens and market integrity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   EU regulatory bodies, citizens, and businesses are beneficiaries, gaining regulatory influence, privacy protections, and a level playing field, respectively. Non-EU businesses are payers, bearing the compliance costs. Advocates for alternative jurisdictional readings are observers or excluded, as this framing prioritizes market dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_access_vs_effects_jurisdiction,
    'To what extent is GDPR''s extraterritorial application primarily a market access requirement, versus an assertion of effects-based jurisdiction over non-EU entities?',
    'Analysis of enforcement actions: if enforcement primarily targets entities actively serving the EU market, it supports the market access reading. If it targets entities with minimal EU market presence but significant effects on EU residents, it supports the effects jurisdiction reading.',
    'If primarily market access, the constraint is more ''rope-like'' (conditional coordination). If primarily effects jurisdiction, it is more ''snare-like'' (coercive assertion of power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_access_vs_effects_jurisdiction, conceptual, 'Ambiguity in the primary mechanism of GDPR''s extraterritorial reach.').

omega_variable(
    standard_setting_vs_jurisdictional_overreach,
    'Is the ''Brussels Effect'' a benign form of standard-setting, or an unacknowledged form of jurisdictional overreach by the EU?',
    'International legal consensus and reciprocal regulatory actions by other major powers: if other nations adopt similar standards or acknowledge the EU''s right to set such conditions, it supports standard-setting. If it leads to significant international legal disputes and non-cooperation, it suggests overreach.',
    'If benign standard-setting, the constraint''s suppression is lower and its coordination function is stronger. If overreach, suppression is higher and the constraint is more extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(standard_setting_vs_jurisdictional_overreach, preference, 'Normative framing of the ''Brussels Effect''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__market_access_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__market_access_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__market_access_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__market_access_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(gdpr_tr_t10, gdpr_article_3_scope__market_access_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__market_access_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__market_access_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__market_access_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(gdpr_be_t10, gdpr_article_3_scope__market_access_reading, base_extractiveness, 10, 0.51).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__market_access_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__market_access_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__market_access_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__market_access_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(gdpr_su_t10, gdpr_article_3_scope__market_access_reading, suppression_requirement, 10, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
