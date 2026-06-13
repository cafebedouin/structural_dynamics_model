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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement as Primary Integration Principle
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the 'integration primary' reading of EU
 *   federation membership obligations, where free movement is a
 *   non-negotiable, constitutive element of EU citizenship and the single
 *   market. Member state welfare boundaries are expected to yield to mobility
 *   rights, leading to mobile workers entering the full welfare beneficiary
 *   set in receiving states. This reading emphasizes the expansion of ECJ
 *   authority via case law and acknowledges that displaced local labor bears
 *   adjustment costs. The constraint is framed as a Tangled Rope, reflecting
 *   its genuine coordination function (single market) alongside asymmetric
 *   extraction (costs borne by national welfare states and local labor).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.65).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.7).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement as Primary Integration Principle").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, 'bae20921-e7bb-4f2b-8f28-90650418dd90').
narrative_ontology:cs_kernel_codification('bae20921-e7bb-4f2b-8f28-90650418dd90', formalized).
narrative_ontology:cs_authority_grounding('bae20921-e7bb-4f2b-8f28-90650418dd90', lineage).
narrative_ontology:cs_interpretation_layer_present('bae20921-e7bb-4f2b-8f28-90650418dd90').
narrative_ontology:cs_reading_relation('bae20921-e7bb-4f2b-8f28-90650418dd90', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('bae20921-e7bb-4f2b-8f28-90650418dd90', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('bae20921-e7bb-4f2b-8f28-90650418dd90', foundational, free_movement_is_foundational_eu_right).
narrative_ontology:cs_axiom_status(free_movement_is_foundational_eu_right, holdable).
narrative_ontology:cs_axiom_grounding('bae20921-e7bb-4f2b-8f28-90650418dd90', free_movement_is_foundational_eu_right, deontological).
narrative_ontology:cs_axiom('bae20921-e7bb-4f2b-8f28-90650418dd90', foundational, welfare_access_follows_citizenship_not_contribution).
narrative_ontology:cs_axiom_status(welfare_access_follows_citizenship_not_contribution, holdable).
narrative_ontology:cs_axiom_grounding('bae20921-e7bb-4f2b-8f28-90650418dd90', welfare_access_follows_citizenship_not_contribution, conventional).
narrative_ontology:cs_reference_frame('bae20921-e7bb-4f2b-8f28-90650418dd90', ever_closer_union_principle).
narrative_ontology:cs_drift_state('bae20921-e7bb-4f2b-8f28-90650418dd90', contemporary_eurozone_crises_and_brexit_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('bae20921-e7bb-4f2b-8f28-90650418dd90', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, multinational_corporations).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, national_welfare_states).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, taxpayers_in_receiving_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Commission and the European Court of Justice actively interpret and enforce free movement as a foundational principle, expanding its scope through directives and case law. They benefit from deeper integration and increased authority.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from the right to live, work, and access social benefits in any member state, regardless of prior contributions. This enhances their economic and social opportunities across the Union.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from a larger, more flexible labor pool across member states, allowing them to optimize labor costs and talent acquisition without national border restrictions. They lobby for stronger free movement enforcement.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, multinational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Bear the costs of extending social benefits and public services to mobile EU citizens, which can strain national budgets and social security systems, especially in high-welfare states. Their ability to restrict access is limited by EU law.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_welfare_states, payer,
    institutional, generational, constrained, national).

% Experience increased competition for jobs and downward pressure on wages in sectors with high mobile labor influx. They often lack the political power or mobility to mitigate these effects, bearing direct adjustment costs.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    powerless, biographical, trapped, local).

% Fund the welfare provisions and public services accessed by mobile EU citizens through their national tax contributions. They perceive a fiscal burden without direct control over immigration policy.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, taxpayers_in_receiving_states, payer,
    organized, biographical, constrained, national).

% Are formally responsible for implementing EU free movement law but often face domestic political pressure to limit welfare access for non-nationals. They are caught between EU obligations and national electoral mandates.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_governments, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the free movement of labor and citizens across the EU, enabling a single market for goods, services, capital, and people, and fostering a sense of European citizenship.
% TRANSFER_FUNCTION: Transfers social welfare benefits and public services from the tax bases of receiving member states to mobile EU citizens, and transfers labor market adjustment costs to local labor in receiving states.
% ABSENT_VOICES: Nationalist political parties and anti-EU movements, who would advocate for stronger national border controls and welfare state protectionism, are marginalized in the EU's institutional discourse on free movement.
% DISAPPEARANCE_RATIONALE: If the principle of free movement as primary integration vanished, the EU single market would fragment, national borders would reassert control over labor flows, and the concept of EU citizenship would be fundamentally altered. Economic and social structures across the continent would undergo significant reorganization.
% FOUNDING_PROBLEM: Post-WWII Europe sought to prevent future conflicts and foster economic prosperity through deep integration, requiring the removal of barriers to trade and movement, including for labor.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and pro-integration academics attest that the founding problem of preventing conflict and fostering prosperity through integration remains live. While some member states and national political parties contest the specific mechanisms, the overarching goal of integration is widely accepted as a continuous project.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because the principle mandates that national welfare systems absorb costs from mobile citizens without full reciprocity or compensatory mechanisms, creating a fiscal burden. Suppression (0.70) is high due to the legal supremacy of EU law, which actively suppresses national attempts to restrict welfare access for mobile citizens. The theater ratio (0.20) is low, indicating that the constraint is genuinely functional in promoting integration, though some rhetoric around 'benefit tourism' might be performative. The rising extractiveness and suppression over time reflect the deepening of integration and the expansion of EU legal authority through case law.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions and mobile citizens, this constraint is a beneficial Rope, facilitating integration and opportunity. However, from the perspective of national welfare states and local labor, it operates as a Snare or Tangled Rope, imposing uncompensated costs and suppressing national policy autonomy. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and multinational corporations are clear beneficiaries (d near 0.0) as they gain from deeper integration and a flexible labor market. Mobile EU citizens are also beneficiaries (d near 0.1-0.2) as they access broader opportunities. National welfare states, displaced local labor, and taxpayers in receiving states are targets (d near 0.8-1.0) as they bear the direct and indirect costs of extending welfare and facing increased labor competition. Member state governments are caught in the middle, acting as both agenda-setters (implementing EU law) and payers (managing national costs and political backlash).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the asymmetric costs) or a pure Snare (ignoring the genuine integration benefits). The 'integration primary' reading acknowledges the coordination function but highlights the substantial, actively enforced extraction from specific national actors. The founding problem of fostering integration remains live, but its implementation has led to significant distributive conflicts, indicating a shift from pure coordination to a hybrid extractive form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_burden_quantification,
    'What is the precise fiscal burden on national welfare states due to free movement, net of contributions from mobile citizens?',
    'Comprehensive, harmonized data collection and economic modeling across member states, distinguishing between contributory and non-contributory benefits.',
    'If the net burden is negligible, the constraint leans more towards a Rope; if substantial and persistent, it reinforces the Tangled Rope or Snare classification for national welfare states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_burden_quantification, empirical, 'Quantifying the fiscal impact of free movement on national welfare systems.').

omega_variable(
    labor_market_displacement_causality,
    'To what extent is local labor displacement and wage depression directly attributable to free movement, versus other factors like automation or global trade?',
    'Detailed econometric studies controlling for confounding variables and focusing on specific sectors and regions with high mobile labor influx.',
    'Strong causal link reinforces the extraction from local labor; weak link suggests other factors are dominant, potentially shifting the burden away from free movement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_displacement_causality, empirical, 'Disentangling the causal impact of free movement on local labor markets.').

omega_variable(
    integration_vs_sovereignty_framing,
    'Is the ''integration primary'' reading a genuine interpretation of EU treaties, or a strategic framing by EU institutions to expand their authority?',
    'Historical analysis of treaty negotiations, legal scholarship on original intent, and comparative analysis of federal systems'' approaches to mobility and welfare.',
    'If primarily strategic, the constraint''s extractiveness is higher, as the coordination story is more cover than function; if genuine, the extraction is an unavoidable cost of a shared political project.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_vs_sovereignty_framing, conceptual, 'Ambiguity between genuine integration principle and institutional power expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_obligations__integration_primary, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__integration_primary, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fede_tr_t2007, federation_membership_obligations__integration_primary, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(fede_tr_t2014, federation_membership_obligations__integration_primary, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_obligations__integration_primary, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__integration_primary, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_obligations__integration_primary, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__integration_primary, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(fede_be_t2007, federation_membership_obligations__integration_primary, base_extractiveness, 2007, 0.58).
narrative_ontology:measurement(fede_be_t2014, federation_membership_obligations__integration_primary, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(fede_be_t2020, federation_membership_obligations__integration_primary, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__integration_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_obligations__integration_primary, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__integration_primary, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fede_su_t2007, federation_membership_obligations__integration_primary, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement(fede_su_t2014, federation_membership_obligations__integration_primary, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement(fede_su_t2020, federation_membership_obligations__integration_primary, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__integration_primary, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_single_market_regulations).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, national_social_security_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('integration_primary') of the 'federation_membership_obligations' kernel. Other readings include 'member_sovereignty_primary' and 'selective_solidarity', which emphasize national control over welfare or tiered access to benefits, respectively. This reading prioritizes EU-level integration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
