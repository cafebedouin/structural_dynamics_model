% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Digital Money Emergence: Consumer Holdings Definition
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money through the lens
 *   of consumer holdings: money exists only when individuals can directly
 *   hold and transact with digital instruments outside traditional bank
 *   accounts (e.g., e-purses, Electronic Money Directives). This reading
 *   establishes a specific boundary for what counts as 'money' in the digital
 *   realm, distinguishing it from mere electronic transfers or conceptual
 *   ideas. It is a 'Tangled Rope' because it coordinates regulatory efforts
 *   and enables new financial products, but also extracts from and suppresses
 *   alternative forms of digital value that do not fit this definition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.45).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.6).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Emergence: Consumer Holdings Definition").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453').
narrative_ontology:cs_kernel_codification('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', formalized).
narrative_ontology:cs_authority_grounding('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', lineage).
narrative_ontology:cs_interpretation_layer_present('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453').
narrative_ontology:cs_reading_relation('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', foundational, direct_consumer_holding_is_money).
narrative_ontology:cs_axiom_status(direct_consumer_holding_is_money, holdable).
narrative_ontology:cs_axiom_grounding('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', direct_consumer_holding_is_money, conventional).
narrative_ontology:cs_axiom('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', secondary, non_bank_intermediation_is_distinct).
narrative_ontology:cs_axiom_status(non_bank_intermediation_is_distinct, holdable).
narrative_ontology:cs_axiom_grounding('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', non_bank_intermediation_is_distinct, conventional).
narrative_ontology:cs_reference_frame('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', regulated_e_money_framework).
narrative_ontology:cs_drift_state('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', contemporary_crypto_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4a2c3ad7-c194-4e22-9f69-dfe2a2ae3453', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unregulated_digital_asset_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, consumers_seeking_unintermediated_digital_value).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, consumers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the legal categories for digital money, such as Electronic Money Institutions (EMIs) in Europe. They benefit from a clear, controllable definition that supports financial stability and monetary policy, and from the fees/oversight associated with regulated entities.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Issue e-money products that fit within the regulatory definition, gaining market access and legitimacy. They benefit from the clarity and trust established by the regulatory framework, even with compliance costs.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms, beneficiary,
    powerful, biographical, mobile, global).

% Must adapt to new definitions of money and compete with fintech firms in the digital money space. They bear costs of compliance and potential market share erosion, but also have opportunities to issue e-money themselves.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, payer,
    institutional, biographical, constrained, global).

% Gain access to new, regulated digital payment instruments with consumer protection. They benefit from trust and convenience but may indirectly bear costs through fees or limited choice compared to unregulated alternatives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, consumers, payer).

% Issue digital assets (e.g., many cryptocurrencies) that do not meet the legal definition of 'digital money' under this framework. They are excluded from the regulated financial system and face legal/reputational barriers, limiting their market and legitimacy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unregulated_digital_asset_issuers, excluded,
    powerless, immediate, trapped, global).

% Analyze the implications of this definition for monetary policy, financial stability, and the evolution of money. They provide critical commentary and alternative conceptualizations.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, legally recognized boundary for what constitutes 'digital money' that consumers can directly hold outside traditional bank accounts, enabling regulatory oversight, fostering trust, and facilitating the development of new financial instruments.
% TRANSFER_FUNCTION: Transfers definitional authority and market access from potentially broader or unregulated concepts of digital value to specific, regulated e-money products and their issuers. It also transfers regulatory compliance burdens to new market entrants.
% ABSENT_VOICES: Advocates for broader definitions of digital money (e.g., including many cryptocurrencies as 'money' regardless of issuer or regulatory status) are structurally excluded from the definitional process. They would argue for a more inclusive or decentralized approach.
% DISAPPEARANCE_RATIONALE: If this definition vanished, the regulatory landscape for digital finance would collapse, leading to widespread confusion about legal tender, consumer protection, and monetary policy. The distinction between bank deposits, e-money, and other digital assets would blur, forcing a fundamental reorganization of the digital financial economy.
% FOUNDING_PROBLEM: The emergence of new digital payment instruments (e-purses, e-money) that allowed consumers to hold value outside traditional bank accounts, creating a need for clear legal definitions and regulatory frameworks to ensure financial stability, consumer protection, and effective monetary policy.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies (e.g., central banks, financial supervisors) and mainstream financial institutions consistently corroborate the ongoing need for clear definitions and regulation of digital money, citing financial stability and consumer protection concerns. Fintech firms also corroborate, as it provides a clear operating environment. Critics (e.g., some crypto advocates) contest the *scope* and *exclusivity* of the definition, but not the underlying problem of new digital value forms.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).
:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) stems from the market power granted to regulated entities and the costs imposed on those excluded. Suppression (0.6) is moderate because while other forms of digital value exist, they are actively suppressed from being recognized as 'money' within the formal financial system. The theater ratio is low (0.1) as the definition is actively applied and enforced, not merely performative. Accessibility collapse is high (0.8) because for those operating within the formal financial system, alternatives to this definition of digital money are largely unavailable. Resistance (0.5) comes from advocates of broader or alternative definitions, particularly from the cryptocurrency space.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of regulatory bodies and fintech firms, this definition is a necessary coordination mechanism for a stable and innovative financial system. From the perspective of unregulated digital asset issuers, it is an extractive and suppressive mechanism designed to protect incumbent interests and regulatory control. The engine's classification as 'Tangled Rope' reflects this inherent tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech firms are beneficiaries, gaining definitional authority, market access, and legitimacy. Traditional banks are payers, needing to adapt to new competitive landscapes. Consumers are both beneficiaries (protection, convenience) and payers (indirect costs, limited choice). Unregulated digital asset issuers are victims, as their products are explicitly excluded from 'money' status by this definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_scope_ambiguity,
    'Is this definition of digital money primarily a coordination mechanism for financial stability, or an extractive mechanism to maintain regulatory control and benefit specific financial actors?',
    'Comparative analysis of regulatory outcomes in jurisdictions with different definitions, assessing impacts on innovation, competition, and consumer welfare. Examination of lobbying efforts and regulatory capture in the definitional process.',
    'If primarily extractive, the constraint''s effective extraction (χ) would be higher, potentially reclassifying it closer to a Snare. If primarily coordination, χ would be lower, reinforcing a Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, conceptual, 'Ambiguity between coordination and extraction in the definition of digital money.').

omega_variable(
    alternative_emergence_framing,
    'How would the classification of ''digital money emergence'' change if the ''conceptualization_reading'' or ''infrastructure_reading'' were adopted as the primary definition?',
    'Analysis of the structural properties (beneficiaries, victims, enforcement) that would arise from adopting a different primary definition, leading to distinct constraint stories for each reading.',
    'Adopting a different reading would instantiate a different constraint with potentially different base properties and classifications. For example, the ''conceptualization_reading'' might be closer to a Mountain, while the ''infrastructure_reading'' might be a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_emergence_framing, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(digi_tr_t2005, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(digi_tr_t2010, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(digi_tr_t2015, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(digi_tr_t2020, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(digi_be_t2005, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(digi_be_t2010, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(digi_be_t2015, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(digi_be_t2020, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(digi_su_t2005, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(digi_su_t2010, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(digi_su_t2015, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(digi_su_t2020, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
