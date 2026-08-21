% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__p2p_cash_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bitcoin_whitepaper__p2p_cash_reading, []).

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
 *   constraint_id: bitcoin_whitepaper__p2p_cash_reading
 *   human_readable: Bitcoin as Peer-to-Peer Electronic Cash
 *   domain: cryptocurrency_economics/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'peer-to-peer electronic cash'
 *   reading of the Bitcoin whitepaper, emphasizing its function as a
 *   censorship-resistant medium of exchange. This reading prioritizes low
 *   transaction fees and scalability for broad transactional use, viewing
 *   high fees or limited transaction capacity as a failure of its core
 *   purpose. The constraint is claimed as a Rope, reflecting its potential
 *   for genuine coordination, but the metrics acknowledge the increasing
 *   extractiveness and suppression from competing interpretations and network
 *   dynamics that have pushed against this vision.
 *
 * KEY AGENTS:
 *   - users_seeking_censorship_resistance: Primary beneficiary (moderate/constrained) — values low fees and transactional freedom.
 *   - merchants_accepting_bitcoin: Beneficiary (moderate/mobile) — seeks efficient, low-cost payments.
 *   - bitcoin_miners: Agenda setter (organized/mobile) — secures the network, influenced by fee structure.
 *   - bitcoin_core_developers: Agenda setter (powerful/constrained) — protocol maintainers, key to scaling decisions.
 *   - users_priced_out_by_high_fees: Primary victim (powerless/trapped) — denied access due to fee markets.
 *   - financial_institutions_and_speculators: Excluded (institutional/arbitrage) — their store-of-value focus conflicts with p2p cash vision.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__p2p_cash_reading, 0.4).
domain_priors:suppression_score(bitcoin_whitepaper__p2p_cash_reading, 0.2).
domain_priors:theater_ratio(bitcoin_whitepaper__p2p_cash_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bitcoin_whitepaper__p2p_cash_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__p2p_cash_reading, rope).
narrative_ontology:human_readable(bitcoin_whitepaper__p2p_cash_reading, "Bitcoin as Peer-to-Peer Electronic Cash").
narrative_ontology:topic_domain(bitcoin_whitepaper__p2p_cash_reading, "cryptocurrency_economics/monetary_systems/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__p2p_cash_reading, '7011ac62-96f7-4e9a-865a-f9c302cd67d6').
narrative_ontology:cs_kernel_codification('7011ac62-96f7-4e9a-865a-f9c302cd67d6', fixed_text).
narrative_ontology:cs_authority_grounding('7011ac62-96f7-4e9a-865a-f9c302cd67d6', distributed).
narrative_ontology:cs_reading_relation('7011ac62-96f7-4e9a-865a-f9c302cd67d6', bitcoin_whitepaper__digital_gold_reading, coexists_with).
narrative_ontology:cs_reading_relation('7011ac62-96f7-4e9a-865a-f9c302cd67d6', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('7011ac62-96f7-4e9a-865a-f9c302cd67d6', foundational, low_transaction_fees_are_essential).
narrative_ontology:cs_axiom_status(low_transaction_fees_are_essential, holdable).
narrative_ontology:cs_axiom_grounding('7011ac62-96f7-4e9a-865a-f9c302cd67d6', low_transaction_fees_are_essential, instrumental).
narrative_ontology:cs_axiom('7011ac62-96f7-4e9a-865a-f9c302cd67d6', foundational, block_size_expansion_is_legitimate).
narrative_ontology:cs_axiom_status(block_size_expansion_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('7011ac62-96f7-4e9a-865a-f9c302cd67d6', block_size_expansion_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('7011ac62-96f7-4e9a-865a-f9c302cd67d6', satoshi_vision_p2p_cash).
narrative_ontology:cs_drift_state('7011ac62-96f7-4e9a-865a-f9c302cd67d6', contemporary_fee_market_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7011ac62-96f7-4e9a-865a-f9c302cd67d6', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, users_seeking_censorship_resistance).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin).
narrative_ontology:constraint_victim(bitcoin_whitepaper__p2p_cash_reading, users_priced_out_by_high_fees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ability to transact without intermediaries or state control, especially in contexts of capital controls or political repression. Values low transaction fees to make micro-transactions viable.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, users_seeking_censorship_resistance, beneficiary,
    moderate, biographical, constrained, global).

% Benefits from direct, irreversible payments with lower fees than traditional payment processors, and access to a global customer base. Requires transaction reliability and low confirmation times.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, merchants_accepting_bitcoin, beneficiary,
    moderate, biographical, mobile, global).

% Process transactions and secure the network. Their incentives are aligned with transaction volume and fees, but this reading emphasizes keeping fees low to encourage broad usage, potentially through block size increases.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_miners, agenda_setter,
    organized, biographical, mobile, global).

% Maintain the reference implementation of the Bitcoin protocol. This reading implies a responsibility to implement changes that facilitate its use as electronic cash, such as scaling solutions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, bitcoin_core_developers, agenda_setter,
    powerful, generational, constrained, global).

% Cannot afford to use Bitcoin for small transactions when fees are high, effectively denying them access to the censorship-resistant medium of exchange. This reading identifies them as victims of a system that fails its core purpose.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, users_priced_out_by_high_fees, payer,
    powerless, immediate, trapped, global).

% Primarily interested in Bitcoin as a store of value or speculative asset, not as a transactional currency. Their influence on protocol development often pushes against changes that would facilitate low-fee transactions, making them 'excluded' from the p2p cash vision.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__p2p_cash_reading, financial_institutions_and_speculators, excluded,
    institutional, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, trustless system for direct electronic transactions, enabling individuals to exchange value without relying on financial institutions or government oversight.
% TRANSFER_FUNCTION: Facilitates the transfer of digital value (Bitcoin) directly between parties, bypassing traditional financial intermediaries and their associated fees and controls.
% ABSENT_VOICES: Those who advocate for Bitcoin primarily as a store of value or a 'digital gold' are often absent from discussions about scaling for transactional use, as their interests diverge from prioritizing low fees and high transaction throughput. Similarly, traditional financial institutions are excluded from this direct peer-to-peer model.
% DISAPPEARANCE_RATIONALE: If Bitcoin ceased to function as a censorship-resistant medium of exchange, a significant portion of the global population (especially in regions with unstable currencies or authoritarian regimes) would lose a vital tool for economic freedom and privacy. Alternative systems would emerge, but the immediate impact would be a substantial rearrangement of informal and formal economies.
% FOUNDING_PROBLEM: The problem of trusted third parties in electronic transactions, where financial institutions introduce costs, delays, and the potential for censorship or seizure of funds.
% FOUNDING_PROBLEM_CORROBORATION: The problem of trusted third parties remains live, as evidenced by ongoing financial surveillance, capital controls, and the exclusion of unbanked populations from traditional financial systems. This is corroborated by human rights organizations, privacy advocates, and individuals in politically unstable regions, who attest to the continued need for censorship-resistant money.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__p2p_cash_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__p2p_cash_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__p2p_cash_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bitcoin_whitepaper__p2p_cash_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__p2p_cash_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bitcoin_whitepaper__p2p_cash_reading_tests).
:- end_tests(bitcoin_whitepaper__p2p_cash_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) reflects the increasing transaction fees that have made Bitcoin less viable for everyday transactions, extracting value from users who need low-cost transfers. Suppression (0.2) is present in the form of protocol-level resistance to changes (like block size increases) that would reduce fees, effectively suppressing the 'cash' use case. Theater ratio (0.1) is low, as the network's core function is still robust, but there's a growing performative aspect to claims of 'cash' utility when fees are high. Resistance (0.7) is high, as many users and developers actively push for scaling solutions to restore the p2p cash vision. Accessibility collapse (0.6) is moderate; alternatives exist (other cryptocurrencies, traditional systems), but none offer the same combination of decentralization and censorship resistance at scale.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of users priced out by high fees, the system is extractive, failing its promise of accessible electronic cash. From the perspective of those who benefit from censorship resistance, it remains a valuable, albeit imperfect, tool. The agenda setters (miners, developers) face a tension between maintaining network security/decentralization and enabling low-cost transactions, leading to different interpretations of the 'correct' path.
 *
 * DIRECTIONALITY LOGIC:
 *   Users seeking censorship resistance and merchants are beneficiaries, as the system provides a unique service. Users priced out by high fees are victims, as the system's current state extracts their ability to participate. Miners and developers are agenda setters, whose decisions directly shape the constraint's operation. Financial institutions and speculators are excluded from the core 'cash' vision, as their interests lie elsewhere.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Rope acknowledges the genuine coordination function of providing censorship-resistant electronic cash. However, the rising extractiveness and suppression metrics, coupled with the 'contested' status of the founding problem, indicate a potential drift towards a Tangled Rope or even Snare if the 'cash' function continues to be undermined by high fees and limited scalability. The Mandatrophy analysis would focus on whether the original mandate of 'peer-to-peer electronic cash' is being actively subverted by other interests, or if the technical challenges are genuinely insurmountable without compromising core properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    block_size_scalability,
    'Is it technically feasible to increase Bitcoin''s transaction capacity (e.g., via block size increases) without compromising decentralization or security, thereby reducing transaction fees and restoring its ''cash'' utility?',
    'Empirical testing of scaling solutions (e.g., Lightning Network adoption, sidechains, or future hard forks) and their impact on node count, mining centralization, and transaction costs.',
    'If feasible, the constraint''s extractiveness (high fees) would decrease, reinforcing its Rope classification. If not, the ''p2p cash'' reading becomes increasingly untenable, pushing the constraint towards a Snare for those priced out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(block_size_scalability, empirical, 'Technical feasibility of scaling Bitcoin for transactional use.').

omega_variable(
    interpretive_drift_of_whitepaper,
    'To what extent has the dominant interpretation of the Bitcoin whitepaper shifted from ''p2p electronic cash'' to ''digital gold'' among key stakeholders (developers, miners, investors)?',
    'Content analysis of developer discussions, mining pool statements, investor sentiment, and media coverage over time. Analysis of protocol changes and their stated justifications.',
    'If the ''digital gold'' reading has become dominant, this ''p2p cash'' reading is increasingly marginalized, and the constraint''s effective extractiveness for transactional users is higher due to lack of support for scaling. This would indicate a conceptual shift in the kernel''s meaning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_drift_of_whitepaper, conceptual, 'Shift in the dominant interpretation of Bitcoin''s purpose.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (resistance to scaling changes) structural (inherent technical trade-offs) or internalized (ideological commitment to small blocks, even if technically suboptimal)?',
    'Analysis of technical arguments vs. ideological rhetoric in scaling debates. Post-implementation analysis of scaling solutions: if resistance persists even after technical concerns are addressed, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the ''cash'' vision is suppressed by an unyielding ideology. If structural, the suppression is a necessary trade-off for other desired properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in scaling debates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__p2p_cash_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 3, 0.06).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 6, 0.07).
narrative_ontology:measurement(bitc_tr_t9, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 9, 0.08).
narrative_ontology:measurement(bitc_tr_t12, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__p2p_cash_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(bitc_be_t9, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 9, 0.35).
narrative_ontology:measurement(bitc_be_t12, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__p2p_cash_reading, base_extractiveness, 15, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 3, 0.12).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 6, 0.15).
narrative_ontology:measurement(bitc_su_t9, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 9, 0.17).
narrative_ontology:measurement(bitc_su_t12, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 12, 0.19).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__p2p_cash_reading, suppression_requirement, 15, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__p2p_cash_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__digital_gold_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__p2p_cash_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Bitcoin whitepaper. The 'p2p_cash_reading' emphasizes transactional utility, while 'digital_gold_reading' focuses on scarcity and store of value, and 'protocol_ossification_reading' prioritizes immutability. Each reading generates a structurally distinct constraint, linked here as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
