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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Digital Money Defined by Consumer Direct Holdings
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint is the `consumer_holdings_reading` of the
 *   `digital_money_emergence_boundary` kernel. It defines digital money's
 *   emergence by the point consumers could directly hold and transact with
 *   digital instruments outside traditional bank accounts, distinguishing it
 *   from earlier conceptual or infrastructural developments. Sibling readings
 *   include `conceptualization_reading` and `infrastructure_reading`. This
 *   reading, adopted by regulatory bodies, establishes a specific boundary
 *   for what constitutes 'digital money' in a legal and operational sense,
 *   creating a framework for new financial products but also imposing
 *   regulatory burdens and market exclusions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.65).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.75).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Defined by Consumer Direct Holdings").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '729bf77d-411d-4cb7-9933-807872c77e5a').
narrative_ontology:cs_kernel_codification('729bf77d-411d-4cb7-9933-807872c77e5a', formalized).
narrative_ontology:cs_authority_grounding('729bf77d-411d-4cb7-9933-807872c77e5a', lineage).
narrative_ontology:cs_interpretation_layer_present('729bf77d-411d-4cb7-9933-807872c77e5a').
narrative_ontology:cs_reading_relation('729bf77d-411d-4cb7-9933-807872c77e5a', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_reading_relation('729bf77d-411d-4cb7-9933-807872c77e5a', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('729bf77d-411d-4cb7-9933-807872c77e5a', foundational, direct_consumer_control_is_money_criterion).
narrative_ontology:cs_axiom_status(direct_consumer_control_is_money_criterion, holdable).
narrative_ontology:cs_axiom_grounding('729bf77d-411d-4cb7-9933-807872c77e5a', direct_consumer_control_is_money_criterion, conventional).
narrative_ontology:cs_axiom('729bf77d-411d-4cb7-9933-807872c77e5a', foundational, non_bank_digital_is_distinct_from_deposits).
narrative_ontology:cs_axiom_status(non_bank_digital_is_distinct_from_deposits, holdable).
narrative_ontology:cs_axiom_grounding('729bf77d-411d-4cb7-9933-807872c77e5a', non_bank_digital_is_distinct_from_deposits, conventional).
narrative_ontology:cs_reference_frame('729bf77d-411d-4cb7-9933-807872c77e5a', non_bank_digital_instrument_distinction).
narrative_ontology:cs_drift_state('729bf77d-411d-4cb7-9933-807872c77e5a', contemporary_crypto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('729bf77d-411d-4cb7-9933-807872c77e5a', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unregulated_digital_asset_issuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the legal categories of money, including e-money, influencing monetary policy and financial stability. They benefit from a clear framework for oversight.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, universal).

% Issue e-money products that fit within this definition, gaining legitimacy and market access. They navigate the regulatory framework but benefit from the clarity it provides for their business model.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_firms, beneficiary,
    powerful, biographical, mobile, global).

% Their traditional bank deposits are explicitly distinguished from 'digital money' under this definition, potentially leading to new competitive pressures or regulatory distinctions. They bear the cost of adapting to a new monetary landscape.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banks, payer,
    institutional, biographical, constrained, national).

% Gain the ability to directly hold and transact with digital instruments outside traditional bank accounts, often with enhanced consumer protection. They benefit from new payment options.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, consumers, beneficiary,
    moderate, immediate, mobile, global).

% Issue digital assets that do not fit the formal definition of 'digital money' under this reading, facing exclusion from regulated financial systems and potential legal challenges. Their products are not recognized as money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unregulated_digital_asset_issuers, excluded,
    powerless, immediate, trapped, global).

% Analyze the conceptual and practical implications of this definition for monetary theory, policy, and the evolution of financial systems. They observe the structural effects.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__consumer_holdings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, legally recognized definition of digital money that enables regulatory oversight, consumer protection, and market development for non-bank digital instruments, distinguishing them from traditional bank deposits.
% TRANSFER_FUNCTION: Transfers definitional authority and market legitimacy towards non-bank digital issuers and their regulators, while imposing new distinctions and potential competitive pressures on traditional banks.
% ABSENT_VOICES: Advocates for broader definitions of money (e.g., including all forms of digital value, or purely private digital currencies without central oversight) are structurally excluded from the formal definitional process, as their claims fall outside this reading's scope.
% DISAPPEARANCE_RATIONALE: If this definitional boundary vanished, the regulatory landscape for digital finance would become chaotic, consumer protection for e-money would collapse, and the distinction between bank deposits and other digital instruments would blur, leading to a fundamental reorganization of financial markets and regulatory approaches.
% FOUNDING_PROBLEM: The rise of non-bank digital payment instruments (e-purses, e-money) in the 1990s and 2000s created ambiguity about what constitutes 'money' outside traditional bank accounts, posing challenges for monetary policy, financial stability, and consumer protection.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and fintech firms attest to the ongoing relevance of defining and regulating non-bank digital money. Monetary economists and financial historians corroborate the historical emergence of this problem with the advent of new digital instruments, citing policy papers and legislative debates from the period.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness (0.65) stems from the power granted to regulatory bodies and the market advantage conferred on fintech firms operating within this definition, at the expense of traditional banks and unregulated digital asset issuers. Suppression (0.75) is high because this definition actively excludes alternative forms of digital value from being recognized as 'money' within the formal financial system, requiring continuous enforcement against new innovations. Theater ratio (0.25) is low-to-moderate, as the definitional work is genuinely functional, though some regulatory efforts may be performative in defending the established boundary against evolving technology. Accessibility collapse (0.8) is high because once this definition is adopted, alternatives that don't fit are largely excluded from the formal monetary system. Resistance (0.5) is moderate, as traditional banks and crypto advocates contest the narrowness of the definition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of regulatory bodies and fintech firms, this definition provides essential clarity and a framework for innovation and stability. From the perspective of traditional banks and unregulated digital asset issuers, it represents an arbitrary boundary that creates competitive disadvantages or outright exclusion. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and fintech firms are beneficiaries, gaining definitional authority, market legitimacy, and new business opportunities. Traditional banks and unregulated digital asset issuers are victims, facing new competitive distinctions and exclusion from the 'digital money' category. Consumers are beneficiaries through new options and protections, but also indirectly bear costs through market structuring. The definition itself, through its regulatory adoption, channels benefits and costs asymmetrically.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_emergence_point_ambiguity,
    'Is the ''true'' emergence of digital money best defined by its conceptualization, infrastructural enablement, or consumer-level direct holdings?',
    'Historical consensus among monetary historians and economists, or a shift in regulatory focus to prioritize one aspect over others.',
    'If an earlier reading (conceptualization or infrastructure) were adopted, the perceived ''naturalness'' of this constraint would decrease, and its classification might shift towards a more constructed type, highlighting the policy choice inherent in the definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(true_emergence_point_ambiguity, conceptual, 'Ambiguity regarding the fundamental criteria for digital money''s emergence.').

omega_variable(
    regulatory_capture_of_definition,
    'To what extent does the current definition of digital money reflect genuine public interest (financial stability, consumer protection) versus the interests of incumbent fintech firms and regulatory bodies in maintaining control?',
    'Independent audits of regulatory lobbying, analysis of policy outcomes favoring specific industry players, or a shift in public discourse challenging the definition''s impartiality.',
    'If significant capture is demonstrated, the constraint''s extractiveness and suppression would be re-evaluated upwards, potentially shifting its classification towards a Snare, as the coordination story would be revealed as cover for private benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_definition, empirical, 'Whether the definition serves public good or private interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(digi_tr_t6, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(digi_tr_t12, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(digi_tr_t18, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(digi_tr_t24, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(digi_tr_t30, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(digi_be_t6, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(digi_be_t12, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(digi_be_t18, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(digi_be_t24, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(digi_be_t30, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(digi_su_t6, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(digi_su_t12, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(digi_su_t18, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(digi_su_t24, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(digi_su_t30, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'digital_money_emergence_boundary' kernel, focusing on consumer direct holdings. It is distinct from the 'conceptualization_reading' and 'infrastructure_reading' which define emergence by theoretical possibility and technological enablement, respectively. Each reading yields a different structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
