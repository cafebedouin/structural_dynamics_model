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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Digital Gold (Store of Value Reading)
 *   domain: Cryptocurrency Economics / Monetary Systems / Technology Governance
 *
 * SUMMARY:
 *   This constraint represents the 'digital gold' reading of the Bitcoin
 *   whitepaper, emphasizing its role as a scarce, censorship-resistant store
 *   of value and inflation hedge. This reading prioritizes asset appreciation
 *   and network security over transaction throughput and low fees, leading to
 *   a specific set of beneficiaries and victims. It is one of several
 *   contested interpretations of Bitcoin's fundamental purpose.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.75).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold (Store of Value Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "Cryptocurrency Economics / Monetary Systems / Technology Governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'd9b22d89-6b28-4b46-9257-5517da7b87c9').
narrative_ontology:cs_kernel_codification('d9b22d89-6b28-4b46-9257-5517da7b87c9', fixed_text).
narrative_ontology:cs_authority_grounding('d9b22d89-6b28-4b46-9257-5517da7b87c9', practice).
narrative_ontology:cs_interpretation_layer_present('d9b22d89-6b28-4b46-9257-5517da7b87c9').
narrative_ontology:cs_reading_relation('d9b22d89-6b28-4b46-9257-5517da7b87c9', bitcoin_whitepaper__p2p_cash_reading, forecloses).
narrative_ontology:cs_reading_relation('d9b22d89-6b28-4b46-9257-5517da7b87c9', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('d9b22d89-6b28-4b46-9257-5517da7b87c9', foundational, bitcoin_as_inflation_hedge).
narrative_ontology:cs_axiom_status(bitcoin_as_inflation_hedge, holdable).
narrative_ontology:cs_axiom_grounding('d9b22d89-6b28-4b46-9257-5517da7b87c9', bitcoin_as_inflation_hedge, empirically_contingent).
narrative_ontology:cs_axiom('d9b22d89-6b28-4b46-9257-5517da7b87c9', foundational, scarcity_drives_value).
narrative_ontology:cs_axiom_status(scarcity_drives_value, holdable).
narrative_ontology:cs_axiom_grounding('d9b22d89-6b28-4b46-9257-5517da7b87c9', scarcity_drives_value, empirically_contingent).
narrative_ontology:cs_reference_frame('d9b22d89-6b28-4b46-9257-5517da7b87c9', bitcoin_as_scarce_digital_commodity).
narrative_ontology:cs_drift_state('d9b22d89-6b28-4b46-9257-5517da7b87c9', contemporary_macroeconomic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d9b22d89-6b28-4b46-9257-5517da7b87c9', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_bitcoin_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_investors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_bitcoin_adopters).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals or entities acquired Bitcoin at low prices and benefit significantly from its appreciation, viewing it primarily as a long-term store of value and inflation hedge. They are incentivized to maintain the digital gold narrative.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_bitcoin_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Miners secure the network and are compensated with newly minted Bitcoin and transaction fees. High transaction fees, a consequence of the digital gold narrative's prioritization of block space for high-value transfers, directly benefit them.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_miners, beneficiary,
    organized, biographical, mobile, global).

% Large financial institutions and corporations that have invested in Bitcoin as a strategic asset for portfolio diversification and inflation hedging. They reinforce the digital gold narrative through their public statements and investment strategies.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% Individuals and smaller investors entering the Bitcoin market after significant price appreciation. They face high entry costs and may struggle to realize substantial gains, effectively paying into the system without the same benefits as early holders.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_bitcoin_adopters, payer,
    moderate, biographical, constrained, global).

% Users who wish to use Bitcoin for everyday, low-value transactions. They are disproportionately affected by high transaction fees and slow confirmation times, making Bitcoin impractical for its original peer-to-peer cash use case.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transaction_users, payer,
    powerless, immediate, trapped, global).

% Groups and individuals who believe Bitcoin's primary purpose should be as a censorship-resistant medium of exchange for direct electronic transactions. Their vision is marginalized by the digital gold narrative, and their proposals for scaling solutions are often rejected.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, p2p_cash_advocates, excluded,
    organized, biographical, constrained, global).

% The small group of individuals who maintain and propose changes to the Bitcoin protocol. Their technical decisions and social influence heavily shape the network's direction, often aligning with the digital gold narrative by prioritizing stability and security over transaction throughput.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, core_developers, agenda_setter,
    powerful, biographical, identity_locked, global).

% Researchers, economists, and critics who analyze Bitcoin's economic and social implications without direct financial stake. They observe the dynamics of the digital gold narrative and its effects on different user groups.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_bitcoin_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate belief in Bitcoin as a reliable, scarce, censorship-resistant store of value, attracting capital and providing a hedge against inflation and monetary debasement.
% TRANSFER_FUNCTION: Transfers wealth from late adopters and those needing frequent, low-value transactions to early holders, miners, and institutional investors through asset appreciation, high transaction fees, and network effects.
% ABSENT_VOICES: Advocates for Bitcoin as a peer-to-peer electronic cash system (p2p_cash_advocates) are structurally excluded from the dominant discourse and development priorities, their concerns about high fees and slow confirmations dismissed as secondary to the store-of-value narrative. Their proposals for scaling are often met with resistance.
% DISAPPEARANCE_RATIONALE: If the digital gold narrative for Bitcoin collapsed overnight, it would trigger a massive sell-off, destabilizing global cryptocurrency markets, causing significant capital flight, and forcing a fundamental re-evaluation of digital asset roles and monetary policy worldwide.
% FOUNDING_PROBLEM: The original problem Bitcoin was built to solve was to create a decentralized, censorship-resistant digital currency that could not be inflated by central authorities, offering an alternative to traditional fiat money and enabling peer-to-peer electronic transactions.
% FOUNDING_PROBLEM_CORROBORATION: Early adopters and digital gold proponents attest that the problem of fiat inflation and central bank control is still live and that Bitcoin effectively solves it. P2P cash advocates and some economists attest that the founding problem was broader (peer-to-peer electronic cash) and that the digital gold narrative has narrowed its utility, a claim supported by transaction data and fee trends, and by the existence of alternative cryptocurrencies focused on transaction speed and low cost.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is driven by the asset appreciation that benefits early holders and the high transaction fees that disproportionately affect small users. Suppression (0.75) stems from the social and technical consensus that prioritizes the store-of-value narrative, effectively marginalizing alternative uses and scaling solutions. The theater ratio (0.25) is relatively low, as the core security and decentralization functions are real, but some rhetoric around 'permissionless innovation' or 'inclusive finance' can be performative given the high barriers to entry and use. The increasing trend in extractiveness and suppression reflects the hardening of this narrative over time.
 *
 * PERSPECTIVAL GAP:
 *   Early Bitcoin holders and institutional investors perceive this as a highly effective 'rope' for wealth preservation and coordination against inflation. In contrast, late adopters and small transaction users experience it as a 'snare' due to prohibitive costs and limited utility for everyday transactions. Core developers, while often aligned with this reading, may view it as a necessary 'tangled rope' to maintain network security and decentralization.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders, miners, and institutional investors are clear beneficiaries (low directionality) as they profit from appreciation and fees. Late adopters and small transaction users are targets (high directionality) as they bear the costs of entry and transaction. P2P cash advocates are excluded, their vision suppressed by the dominant narrative. Core developers, as agenda-setters, shape the protocol to reinforce this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of Bitcoin as 'peer-to-peer electronic cash' has undergone significant reinterpretation. While the founding problem of decentralized money remains, the 'digital gold' reading has shifted the solution's focus, arguably leading to a form of mandatrophy where the original function for everyday transactions has atrophied in favor of a new, more extractive function for wealth storage. The contested status of the founding problem reflects this shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine and stable reading of the Bitcoin whitepaper, or a transient market-driven narrative?',
    'Long-term observation of market cycles, developer consensus, and user adoption patterns across different use cases. If the narrative persists through bear markets and continues to shape protocol development, it is stable.',
    'If transient, the constraint''s extractiveness and suppression may be less stable and more susceptible to shifts in market sentiment or alternative narratives. If stable, its classification as a Tangled Rope is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Stability of the digital gold narrative as a reading of the Bitcoin kernel.').

omega_variable(
    p2p_cash_reading_impact,
    'To what extent does the ''digital gold'' reading actively foreclose or merely marginalize the ''p2p cash'' reading?',
    'Analysis of protocol changes and social discourse: if proposals for scaling that would enable p2p cash are consistently rejected due to ''digital gold'' priorities, it''s foreclosure. If they are simply less prioritized, it''s marginalization.',
    'If foreclosure, the ''p2p cash'' reading is structurally impossible within the current Bitcoin framework, making the ''digital gold'' reading more extractive by eliminating alternatives. If marginalization, the ''p2p cash'' reading retains latent potential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(p2p_cash_reading_impact, conceptual, 'Relationship between digital gold and p2p cash readings.').

omega_variable(
    store_of_value_necessity,
    'Are high transaction fees and limited throughput structurally necessary for Bitcoin to function as a secure, decentralized store of value, or could it achieve this with lower fees and higher throughput?',
    'Empirical evidence from alternative scaling solutions (e.g., Lightning Network adoption) or other cryptocurrencies that balance security with higher transaction capacity. Technical analysis of trade-offs between decentralization, security, and scalability.',
    'If not necessary, the current high fees represent excess extraction not justified by coordination function. If necessary, the extraction is an unavoidable cost of the desired coordination, making the ''tangled rope'' classification more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(store_of_value_necessity, empirical, 'Necessity of high fees for store of value function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2010, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(bitc_tr_t2016, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(bitc_tr_t2019, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement(bitc_tr_t2022, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2022, 0.24).
narrative_ontology:measurement(bitc_tr_t2025, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2010, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2013, 0.45).
narrative_ontology:measurement(bitc_be_t2016, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(bitc_be_t2019, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(bitc_be_t2022, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2022, 0.65).
narrative_ontology:measurement(bitc_be_t2025, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2010, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2013, 0.55).
narrative_ontology:measurement(bitc_su_t2016, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(bitc_su_t2019, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(bitc_su_t2022, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2022, 0.73).
narrative_ontology:measurement(bitc_su_t2025, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'bitcoin_whitepaper' kernel, each with different ε values and structural properties. This 'digital_gold_reading' prioritizes scarcity and store of value, contrasting with the 'p2p_cash_reading' (focused on transactions) and the 'protocol_ossification_reading' (focused on immutability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
