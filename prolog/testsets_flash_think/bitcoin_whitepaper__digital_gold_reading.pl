% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__digital_gold_reading, []).

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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold and Inflation Hedge
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint story models the 'digital gold' reading of Bitcoin, where
 *   its primary function is understood as a scarce, decentralized store of
 *   value and an inflation hedge. This reading prioritizes asset appreciation
 *   and network security over transaction throughput or low fees. The claimed
 *   type is 'rope' reflecting the proponents' view of it as a beneficial
 *   coordination mechanism, but the authored metrics reflect the structural
 *   extraction inherent in this prioritization, particularly for late
 *   entrants and small transaction users.
 *
 * KEY AGENTS:
 *   - early_adopters: Primary beneficiary (powerful/arbitrage) — benefits from appreciation
 *   - long_term_holders: Primary beneficiary (powerful/mobile) — benefits from appreciation
 *   - mining_operators: Agenda setter (institutional/constrained) — enforces rules, benefits from fees/new coins
 *   - late_entrants: Primary target (powerless/constrained) — bears cost of appreciation
 *   - small_transaction_users: Primary target (powerless/trapped) — bears cost of high fees
 *   - p2p_cash_advocates: Excluded (organized/constrained) — marginalized by this reading's focus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.65).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.7).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold and Inflation Hedge").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '47b7acb2-27cd-4e74-a849-3c9eca1a162e').
narrative_ontology:cs_kernel_codification('47b7acb2-27cd-4e74-a849-3c9eca1a162e', fixed_text).
narrative_ontology:cs_authority_grounding('47b7acb2-27cd-4e74-a849-3c9eca1a162e', practice).
narrative_ontology:cs_interpretation_layer_present('47b7acb2-27cd-4e74-a849-3c9eca1a162e').
narrative_ontology:cs_reading_relation('47b7acb2-27cd-4e74-a849-3c9eca1a162e', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('47b7acb2-27cd-4e74-a849-3c9eca1a162e', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('47b7acb2-27cd-4e74-a849-3c9eca1a162e', foundational, fixed_supply_is_paramount).
narrative_ontology:cs_axiom_status(fixed_supply_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('47b7acb2-27cd-4e74-a849-3c9eca1a162e', fixed_supply_is_paramount, deontological).
narrative_ontology:cs_axiom('47b7acb2-27cd-4e74-a849-3c9eca1a162e', foundational, security_via_decentralization).
narrative_ontology:cs_axiom_status(security_via_decentralization, holdable).
narrative_ontology:cs_axiom_grounding('47b7acb2-27cd-4e74-a849-3c9eca1a162e', security_via_decentralization, conventional).
narrative_ontology:cs_reference_frame('47b7acb2-27cd-4e74-a849-3c9eca1a162e', satoshi_vision_scarce_asset).
narrative_ontology:cs_drift_state('47b7acb2-27cd-4e74-a849-3c9eca1a162e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('47b7acb2-27cd-4e74-a849-3c9eca1a162e', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, long_term_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transaction_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, fiat_currency_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired Bitcoin at low prices and have seen substantial asset appreciation, benefiting directly from its store-of-value narrative and scarcity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopters, beneficiary,
    powerful, generational, arbitrage, global).

% Hold Bitcoin as a primary store of value, believing in its long-term appreciation and inflation-hedging properties. They benefit from network effects and perceived scarcity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, long_term_holders, beneficiary,
    powerful, generational, mobile, global).

% Secure the network and validate transactions, receiving newly minted Bitcoin and transaction fees. They have significant capital investment and influence protocol stability, aligning with the digital gold narrative that values security over throughput.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_operators, agenda_setter,
    institutional, biographical, constrained, global).

% Acquire Bitcoin at significantly higher prices, facing a higher barrier to entry for store-of-value benefits. They bear the cost of prior appreciation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrants, payer,
    powerless, immediate, constrained, global).

% Attempt to use Bitcoin for everyday transactions, but face high and volatile transaction fees, making it impractical for small payments. They are effectively priced out of this use case by the digital gold prioritization.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transaction_users, payer,
    powerless, immediate, trapped, global).

% Indirectly bear costs if Bitcoin's success as an inflation hedge draws capital from traditional markets or if its narrative undermines confidence in fiat currencies. They have alternatives but are affected by the broader monetary shift.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, fiat_currency_users, payer,
    moderate, biographical, mobile, global).

% Advocate for Bitcoin's primary use as a medium of exchange for everyday transactions, but their vision is marginalized by the digital gold narrative which prioritizes scarcity and security over transaction throughput and low fees. They can fork the protocol but lose network effects.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates, excluded,
    organized, biographical, constrained, global).

% Maintain and propose changes to the Bitcoin protocol. Those aligned with the digital gold reading prioritize stability, security, and scarcity, often resisting changes that might compromise these attributes for increased transaction capacity.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, protocol_developers, agenda_setter,
    organized, biographical, mobile, global).

% Observe Bitcoin's impact on traditional monetary systems, inflation, and financial stability. They analyze its properties and may consider regulatory responses, but do not directly control its operation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, monetary_authorities, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a globally recognized, decentralized, and scarce digital asset that can serve as a store of value, coordinating belief in its long-term value and resistance to inflation.
% TRANSFER_FUNCTION: Transfers wealth from those who acquire Bitcoin later at higher prices (or pay high transaction fees for limited block space) to early adopters and long-term holders, as well as to mining operators.
% ABSENT_VOICES: Advocates for Bitcoin as a peer-to-peer electronic cash system (p2p_cash_advocates) are structurally excluded from the primary discourse of this reading, as are those who would prefer lower fees and higher transaction throughput for everyday use.
% DISAPPEARANCE_RATIONALE: If Bitcoin vanished, the global financial system would lose a significant alternative asset class and a major inflation hedge, leading to a scramble for other safe-haven assets, a re-evaluation of decentralized digital scarcity, and potentially increased demand for other cryptocurrencies. The narrative of 'digital gold' would collapse, and significant wealth would be destroyed.
% FOUNDING_PROBLEM: The perceived instability and inflationary tendencies of fiat currencies, and the desire for a decentralized, censorship-resistant store of value outside traditional financial systems, particularly after the 2008 financial crisis.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists, financial analysts, and a segment of the public (outside the direct beneficiary group) corroborate ongoing concerns about fiat inflation, central bank policies, and the demand for alternative, non-sovereign stores of value.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__digital_gold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bitcoin_whitepaper__digital_gold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is substantial, reflecting the wealth transfer from late entrants to early holders and the high transaction fees that make small payments impractical. Suppression (0.70) is high due to the strong network effects, the difficulty of creating a truly decentralized alternative, and the active defense of the 'digital gold' narrative against competing visions. Theater ratio is low (0.10) because the network's core functions (scarcity, security) are genuinely maintained. Accessibility collapse (0.40) is moderate; while other assets exist, Bitcoin's unique properties make it a distinct, if costly, alternative. Resistance (0.50) is moderate, coming from advocates of alternative uses and competing cryptocurrencies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of early adopters and long-term holders, Bitcoin is a highly successful 'rope' that coordinates a valuable store of wealth. From the perspective of late entrants and small transaction users, the same structure operates as a 'tangled rope' or 'snare', extracting value through appreciation and high fees. The engine's classification will likely diverge from the claimed 'rope' due to the high extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and long-term holders are clear beneficiaries, experiencing low directionality. Mining operators, while agenda setters, also benefit significantly from transaction fees and block rewards. Late entrants and small transaction users are clear targets, experiencing high directionality due to the costs they bear and their constrained exit options. Fiat currency users are indirect targets. P2P cash advocates are excluded from the primary benefits of this reading and face structural barriers to their preferred use case.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'digital gold' reading prevents mislabeling the asset as a pure coordination mechanism (rope) by highlighting the substantial extraction from late entrants and small transaction users. It also avoids mislabeling it as a pure snare by acknowledging the genuine coordination function of providing a decentralized, scarce store of value. The contest over its primary purpose (store of value vs. medium of exchange) is central to understanding its extractive dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primary_purpose_ambiguity,
    'Is Bitcoin''s primary purpose fundamentally a store of value (''digital gold'') or a medium of exchange (''peer-to-peer electronic cash'')?',
    'Long-term observation of transaction patterns, average transaction value, and fee structures; analysis of community consensus shifts over time.',
    'If resolved as primarily a medium of exchange, the high fees and low throughput become a severe structural flaw, reclassifying the constraint as more extractive (e.g., Snare). If resolved as primarily a store of value, the current structure is more consistent with its intended function, potentially supporting a Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(primary_purpose_ambiguity, conceptual, 'Ambiguity over Bitcoin''s core function.').

omega_variable(
    scarcity_natural_vs_constructed,
    'Is Bitcoin''s scarcity a ''natural law'' of its protocol, or a socially constructed and maintained consensus that could theoretically be altered?',
    'Analysis of the technical feasibility and social coordination required for a hard fork to alter the supply cap; observation of community response to such proposals.',
    'If scarcity is purely a social construct, its ''mountain-like'' properties are weaker, and the extraction it enables is more clearly a product of human choice and enforcement, potentially shifting classification towards Snare if the enforcement is coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_natural_vs_constructed, empirical, 'The naturalness of Bitcoin''s fixed supply.').

omega_variable(
    protocol_ossification_necessity,
    'Is the current degree of protocol ossification (resistance to change) a necessary condition for Bitcoin''s ''digital gold'' properties, or an extractive mechanism that prevents adaptation?',
    'Comparative analysis with other cryptocurrencies that have undergone more frequent or significant protocol changes; theoretical modeling of security vs. adaptability tradeoffs.',
    'If ossification is found to be unnecessarily rigid, it would highlight a suppression mechanism that benefits existing holders by limiting innovation and competition, potentially increasing the effective suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_ossification_necessity, conceptual, 'Necessity of protocol ossification for digital gold properties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.1).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, fiat_currency_inflation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, traditional_asset_markets).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'bitcoin_whitepaper' kernel. This 'digital_gold_reading' emphasizes scarcity and store of value, while 'p2p_cash_reading' focuses on transaction utility and 'protocol_ossification_reading' on immutability. Each reading yields a distinct constraint with different beneficiaries, victims, and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
