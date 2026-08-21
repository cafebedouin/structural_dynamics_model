% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems: Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'balanced coexistence' reading of the 'One
 *   Country, Two Systems' framework, where neither PRC sovereignty nor Hong
 *   Kong's autonomy is absolute. Instead, the framework is understood as a
 *   dynamic process of political accommodation and negotiation, with
 *   contested boundaries resolved through ongoing dialogue rather than strict
 *   legal supremacy. This reading acknowledges a medium level of extraction
 *   and suppression, reflecting the inherent tension and the need for active
 *   enforcement to maintain the balance, but also recognizes the genuine
 *   coordination function of allowing two distinct systems to operate. The
 *   temporal measurements reflect periods of increased tension and subsequent
 *   re-accommodation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.45).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.55).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems: Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '2f2f2754-05c3-4e3c-a093-84bd57415e8d').
narrative_ontology:cs_kernel_codification('2f2f2754-05c3-4e3c-a093-84bd57415e8d', formalized).
narrative_ontology:cs_authority_grounding('2f2f2754-05c3-4e3c-a093-84bd57415e8d', lineage).
narrative_ontology:cs_interpretation_layer_present('2f2f2754-05c3-4e3c-a093-84bd57415e8d').
narrative_ontology:cs_reading_relation('2f2f2754-05c3-4e3c-a093-84bd57415e8d', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f2f2754-05c3-4e3c-a093-84bd57415e8d', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('2f2f2754-05c3-4e3c-a093-84bd57415e8d', foundational, functional_division_of_powers_negotiated).
narrative_ontology:cs_axiom_status(functional_division_of_powers_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('2f2f2754-05c3-4e3c-a093-84bd57415e8d', functional_division_of_powers_negotiated, conventional).
narrative_ontology:cs_axiom('2f2f2754-05c3-4e3c-a093-84bd57415e8d', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2f2f2754-05c3-4e3c-a093-84bd57415e8d', neither_sovereignty_nor_autonomy_absolute, conventional).
narrative_ontology:cs_reference_frame('2f2f2754-05c3-4e3c-a093-84bd57415e8d', political_accommodation_framework).
narrative_ontology:cs_drift_state('2f2f2754-05c3-4e3c-a093-84bd57415e8d', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f2f2754-05c3-4e3c-a093-84bd57415e8d', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elites).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, international_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_special_administrative_region_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereignty over Hong Kong, but acknowledges the need for accommodation to maintain economic stability and international standing. Engages in political negotiation to resolve boundary disputes, seeking to balance control with functional autonomy.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates as the local administrative authority, mediating between Beijing's sovereignty claims and Hong Kong's desire for autonomy. Its legitimacy depends on maintaining a functional balance, often bearing the costs of political accommodation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_special_administrative_region_government, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_special_administrative_region_government, payer).

% Advocates for greater autonomy and civil liberties, but recognizes the reality of PRC sovereignty. Engages in protests and political action, seeking to influence the negotiation process and retain bargaining power through public pressure and international attention.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, biographical, constrained, local).

% Benefits from the economic stability and access to mainland markets that the framework provides, while also valuing Hong Kong's distinct legal and financial systems. Seeks to maintain the 'two systems' for economic advantage, often acting as a bridge between Beijing and local interests.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elites, beneficiary,
    powerful, biographical, mobile, global).

% Invest in Hong Kong due to its unique status as a gateway to China with a common law system. They bear the costs of political instability and uncertainty when the balance shifts, but can reallocate capital if the framework becomes too extractive or unpredictable.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_investors, payer,
    powerful, immediate, mobile, global).

% Monitors the implementation of 'One Country, Two Systems' due to its implications for international law, trade, and human rights. Exerts diplomatic pressure and offers commentary, but has limited direct enforcement power over the framework's internal dynamics.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the complex relationship between a sovereign state and a highly autonomous region, allowing for distinct legal, economic, and political systems to coexist under a single national identity, facilitating economic integration while preserving unique regional advantages.
% TRANSFER_FUNCTION: Transfers political authority and ultimate decision-making power to the PRC Central Government, while transferring economic benefits and international legitimacy to Hong Kong through its continued distinctiveness. It also transfers the burden of political accommodation and uncertainty to Hong Kong's civil society and international stakeholders.
% ABSENT_VOICES: Those advocating for full independence for Hong Kong are structurally excluded from the framework's negotiation process; their demands are deemed outside the 'One Country' principle and are suppressed. They would argue for self-determination and complete separation.
% DISAPPEARANCE_RATIONALE: If the 'One Country, Two Systems' framework disappeared overnight, Hong Kong would either be fully integrated into mainland China (losing its distinct systems) or attempt to declare independence (triggering a major geopolitical crisis). In either scenario, the economic, legal, and political landscape of Hong Kong and its relationship with the PRC would fundamentally reorganize.
% FOUNDING_PROBLEM: To facilitate the peaceful transfer of sovereignty over Hong Kong from the United Kingdom to the People's Republic of China, ensuring Hong Kong's stability and prosperity by preserving its capitalist system and way of life for 50 years, while affirming China's sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The PRC Central Government and Hong Kong SAR Government attest that the problem of managing sovereignty and autonomy is ongoing. International observers and Hong Kong civil society also corroborate that the framework continues to address this fundamental tension, albeit with shifting interpretations and increasing pressure on autonomy.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).
:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs borne by Hong Kong in accommodating Beijing's sovereignty, but not yet reaching the level of pure extraction. Suppression (0.55) is present as Beijing actively enforces its 'One Country' principle, but civil society retains some capacity for resistance and negotiation. The theater ratio (0.20) indicates that while there is genuine functional division, some aspects of autonomy are performative, designed to manage international perception. The cyclical nature of measurements reflects the periodic crises and subsequent political accommodations that characterize this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC's perspective, this framework is a necessary and legitimate exercise of sovereignty with appropriate concessions for Hong Kong's unique status. From Hong Kong civil society's perspective, it is a constant struggle to preserve autonomy against encroaching central authority, with periods of greater or lesser success. The 'balanced coexistence' reading attempts to capture this ongoing tension as a structural feature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government is a primary beneficiary, securing sovereignty and stability. Hong Kong's business elites also benefit from the economic opportunities and stability. Hong Kong civil society and international investors bear costs through political uncertainty and limitations on autonomy. The Hong Kong SAR Government acts as an agenda-setter but also bears costs in mediating these tensions. This reading emphasizes the dynamic interplay and mutual (though asymmetric) dependence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negotiation_power_imbalance,
    'Is the ''negotiation'' truly substantive, or is it increasingly a formality where Beijing''s will ultimately prevails, making the ''balance'' illusory?',
    'Analysis of outcomes from recent political crises: if all major disputes are resolved in favor of Beijing''s stated position without significant concessions to Hong Kong''s autonomy, the negotiation is likely formal.',
    'If the negotiation is formal, the constraint shifts closer to the ''sovereignty primacy'' reading, increasing extractiveness and suppression, potentially reclassifying as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_power_imbalance, empirical, 'Assesses the genuine power dynamics within the ''negotiated'' framework.').

omega_variable(
    international_leverage_decay,
    'To what extent does international attention and economic leverage still provide bargaining power for Hong Kong civil society and business elites?',
    'Tracking the impact of international statements and economic sanctions on PRC policy decisions regarding Hong Kong over time.',
    'If international leverage significantly declines, Hong Kong''s exit options become more constrained, increasing effective extraction and suppression, pushing the constraint towards a ''sovereignty primacy'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_leverage_decay, empirical, 'Evaluates the external factors influencing Hong Kong''s bargaining position.').

omega_variable(
    framing_underdetermination_coexistence,
    'Is the ''balanced coexistence'' reading the most defensible framing, or does the evidence increasingly support either the ''sovereignty primacy'' or ''autonomy primacy'' readings?',
    'Longitudinal analysis of legal interpretations, policy shifts, and civil society responses, compared against the core tenets of each reading. This is a conceptual choice guided by empirical trends.',
    'Adopting the ''sovereignty primacy'' reading would reclassify this constraint as more extractive (Snare or Tangled Rope with higher epsilon); adopting the ''autonomy primacy'' reading would reclassify it as less extractive (Rope or even Mountain, if the guarantees were truly unassailable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_coexistence, conceptual, 'The choice of framing for the ''One Country, Two Systems'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(one__tr_t20, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(one__be_t20, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 25, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(one__su_t20, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 25, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'One Country, Two Systems' kernel. This 'balanced coexistence' reading emphasizes ongoing political accommodation, distinct from the 'sovereignty primacy' (Beijing's ultimate authority) and 'autonomy primacy' (Hong Kong's guaranteed self-governance) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
