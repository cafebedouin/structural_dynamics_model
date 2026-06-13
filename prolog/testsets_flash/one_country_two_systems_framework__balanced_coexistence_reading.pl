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
 *   The 'One Country, Two Systems' framework, as interpreted by the 'balanced
 *   coexistence' reading, describes a dynamic where neither the PRC's
 *   sovereignty nor Hong Kong's autonomy is absolute. Instead, their
 *   relationship is defined by ongoing political negotiation and
 *   accommodation, with contested boundaries resolved through dialogue rather
 *   than strict legal supremacy. This reading acknowledges the functional
 *   division of powers and the role of civil society and international
 *   leverage in shaping outcomes, leading to a medium-epsilon constraint
 *   regime with periodic crises and renegotiations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.45).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.35).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.35).
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
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '8731faae-5b9c-4e9c-aad6-201e0a209c4b').
narrative_ontology:cs_kernel_codification('8731faae-5b9c-4e9c-aad6-201e0a209c4b', fixed_text).
narrative_ontology:cs_authority_grounding('8731faae-5b9c-4e9c-aad6-201e0a209c4b', lineage).
narrative_ontology:cs_interpretation_layer_present('8731faae-5b9c-4e9c-aad6-201e0a209c4b').
narrative_ontology:cs_reading_relation('8731faae-5b9c-4e9c-aad6-201e0a209c4b', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8731faae-5b9c-4e9c-aad6-201e0a209c4b', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8731faae-5b9c-4e9c-aad6-201e0a209c4b', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('8731faae-5b9c-4e9c-aad6-201e0a209c4b', neither_sovereignty_nor_autonomy_absolute, conventional).
narrative_ontology:cs_axiom('8731faae-5b9c-4e9c-aad6-201e0a209c4b', foundational, political_accommodation_resolves_boundaries).
narrative_ontology:cs_axiom_status(political_accommodation_resolves_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('8731faae-5b9c-4e9c-aad6-201e0a209c4b', political_accommodation_resolves_boundaries, instrumental).
narrative_ontology:cs_reference_frame('8731faae-5b9c-4e9c-aad6-201e0a209c4b', basic_law_original_intent_dynamic_balance).
narrative_ontology:cs_drift_state('8731faae-5b9c-4e9c-aad6-201e0a209c4b', post_national_security_law_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8731faae-5b9c-4e9c-aad6-201e0a209c4b', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, international_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereignty over Hong Kong, but acknowledges the need for accommodation to maintain economic stability and international standing. Engages in political negotiation to resolve boundary disputes, seeking to balance control with functional autonomy.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates as the local administrative authority, mediating between Beijing's sovereignty claims and Hong Kong's autonomous institutions. Its legitimacy depends on maintaining a functional balance, often requiring difficult political compromises.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, agenda_setter,
    organized, biographical, constrained, local).

% Experiences the contested boundaries as a constant pressure on civil liberties and democratic aspirations. Engages in protests and advocacy, leveraging international attention and economic importance to maintain bargaining power, but ultimately subject to central government authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer,
    organized, generational, identity_locked, local).

% Benefit from Hong Kong's unique legal and economic status, but face uncertainty from the contested boundaries. Their continued presence provides economic leverage for Hong Kong, but they can exit if political instability or erosion of autonomy threatens their interests.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_investors, payer,
    powerful, immediate, mobile, global).

% Operates under the Basic Law, interpreting local laws and maintaining judicial independence within the framework. Its authority is respected as long as its rulings do not directly challenge central government sovereignty, making it a key institution in the 'balanced coexistence'.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_judiciary, beneficiary,
    institutional, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the complex relationship between a sovereign state and a highly autonomous region, allowing for distinct legal and economic systems to operate within a single national framework, preventing direct conflict through political accommodation.
% TRANSFER_FUNCTION: Transfers some degree of sovereign authority from the central government to the autonomous region in exchange for economic stability and international legitimacy, while also transferring some local autonomy to central government oversight in matters of national security and foreign policy.
% ABSENT_VOICES: Hardline sovereignty advocates within the PRC who view any autonomy as a temporary concession, and radical independence advocates in Hong Kong who reject any central government authority. Both are marginalized by the 'balanced coexistence' framework, which seeks a middle ground.
% DISAPPEARANCE_RATIONALE: If the framework vanished, Hong Kong would either be fully integrated into the PRC system (losing its distinct legal and economic identity) or attempt full independence (triggering a major geopolitical crisis). The current arrangements, however imperfect, prevent these more extreme outcomes.
% FOUNDING_PROBLEM: To facilitate the peaceful transfer of sovereignty over Hong Kong from the United Kingdom to the People's Republic of China, preserving Hong Kong's capitalist system and way of life for 50 years, while asserting China's national unity.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live as the 50-year period approaches and geopolitical tensions rise. International observers, legal scholars, and both the PRC and Hong Kong governments (albeit with different interpretations) corroborate the ongoing challenge of managing this unique arrangement.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).

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
 *   Extractiveness (0.45) is moderate because both sides make concessions, but the central government ultimately holds more power. Suppression (0.35) is present to manage dissent and maintain the 'one country' principle, but it's not absolute, allowing for some resistance and negotiation. The 'balanced coexistence' reading implies a dynamic equilibrium, which can be disrupted by events (e.g., 2019 protests, National Security Law), leading to fluctuations in extractiveness and suppression, as shown in the temporal measurements. The slight decrease in extractiveness and suppression by 2024 reflects a period of relative stabilization after the initial impact of the National Security Law, with a new equilibrium of accommodation being sought.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the PRC Central Government, this is a necessary and legitimate framework for national unity. From Hong Kong civil society, it's a constant struggle to preserve freedoms against encroaching central authority. The 'balanced coexistence' reading attempts to capture this ongoing tension and negotiation, where neither side fully dominates but both must adapt.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government and Hong Kong Government are beneficiaries as they maintain a functional system, albeit with compromises. Hong Kong Civil Society and International Investors are payers, bearing the costs of uncertainty and reduced autonomy, but also retaining some leverage. The Hong Kong Judiciary benefits from its institutional role in maintaining the legal distinction, but its autonomy is constrained by the overarching framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negotiation_vs_imposition,
    'To what extent are boundary disputes resolved through genuine political accommodation and negotiation, versus unilateral imposition by the central government?',
    'Analysis of specific policy decisions and legal interpretations over time, assessing the degree of input from Hong Kong stakeholders and the responsiveness of the central government to local concerns.',
    'If resolution is primarily through imposition, the constraint leans towards a Snare, with the ''coexistence'' narrative serving as cover. If genuine negotiation is evident, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_vs_imposition, empirical, 'Distinguishing genuine negotiation from performative consultation.').

omega_variable(
    civil_society_leverage_durability,
    'How durable is Hong Kong civil society''s bargaining power, given increasing central government control and international geopolitical shifts?',
    'Longitudinal study of protest effectiveness, electoral outcomes, and the impact of international pressure on central government policy towards Hong Kong.',
    'If civil society''s leverage significantly diminishes, the constraint''s extractiveness and suppression would likely increase, pushing it closer to a Snare. If leverage persists, it maintains the ''balanced'' aspect of the coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_society_leverage_durability, empirical, 'The long-term viability of civil society''s influence.').

omega_variable(
    reading_framing_bias,
    'Is this ''balanced coexistence'' reading an accurate description of the framework''s operation, or a normative ideal that masks a de facto shift towards sovereignty primacy?',
    'Comparison of this reading''s predictions (e.g., periodic renegotiation, civil society influence) against empirical outcomes, particularly in periods of crisis, and against the ''sovereignty primacy'' reading''s predictions.',
    'If empirical evidence consistently aligns with the ''sovereignty primacy'' reading, this ''balanced coexistence'' reading would be reclassified as a conceptual omega, representing a contested interpretation rather than a structural reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_bias, conceptual, 'Assessing the descriptive accuracy of the ''balanced coexistence'' framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.1).
narrative_ontology:measurement(one__tr_t2005, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.3).
narrative_ontology:measurement(one__be_t2005, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.2).
narrative_ontology:measurement(one__su_t2005, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2005, 0.25).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'One Country, Two Systems' kernel, focusing on the dynamic balance between sovereignty and autonomy. Other readings (e.g., 'sovereignty primacy', 'autonomy primacy') represent distinct constraints with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
