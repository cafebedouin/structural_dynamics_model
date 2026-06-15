% ============================================================================
% CONSTRAINT STORY: crypto_permissionless_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crypto_permissionless_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: crypto_permissionless_reading
 *   human_readable: Permissionless Cryptocurrency as Legitimate Money
 *   domain: monetary_policy/digital_currency/behavioral_economics
 *
 * SUMMARY:
 *   This constraint represents the permissionless reading of digital money
 *   legitimacy: that legitimate money can emerge from decentralized consensus
 *   without state permission or backing. It is one of three sibling readings
 *   of the digital_money_legitimacy kernel. The sovereign_cbdc_reading holds
 *   that legitimate digital money must be state-issued and state-controlled.
 *   The regulated_stablecoin_reading holds that private digital money is
 *   legitimate only when backed by reserves and subject to regulatory
 *   oversight. This reading asserts that cryptographic proof and network
 *   consensus are sufficient for monetary legitimacy, and that state
 *   permission is neither necessary nor desirable. The claim/metric gap is
 *   deliberate: the constraint is claimed as rope (genuine coordination
 *   solving the permissionless-transaction problem) while the metrics show
 *   moderate extraction (early-adopter seigniorage, energy costs) and rising
 *   suppression (state attempts to control adoption). The engine measures
 *   that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crypto_permissionless_reading, 0.42).
domain_priors:suppression_score(crypto_permissionless_reading, 0.68).
domain_priors:theater_ratio(crypto_permissionless_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crypto_permissionless_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(crypto_permissionless_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(crypto_permissionless_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(crypto_permissionless_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(crypto_permissionless_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crypto_permissionless_reading, rope).
narrative_ontology:human_readable(crypto_permissionless_reading, "Permissionless Cryptocurrency as Legitimate Money").
narrative_ontology:topic_domain(crypto_permissionless_reading, "monetary_policy/digital_currency/behavioral_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(crypto_permissionless_reading, 'e69bed7b-d66f-4b2f-b894-840f6c6dd99d').
narrative_ontology:cs_kernel_codification('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', distributed).
narrative_ontology:cs_authority_grounding('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', distributed).
narrative_ontology:cs_reading_relation('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', digital_money_legitimacy__sovereign_cbdc_reading, coexists_with).
narrative_ontology:cs_reading_relation('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', digital_money_legitimacy__regulated_stablecoin_reading, coexists_with).
narrative_ontology:cs_axiom('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', foundational, consensus_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(consensus_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', consensus_suffices_for_legitimacy, conventional).
narrative_ontology:cs_axiom('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', foundational, state_permission_unnecessary).
narrative_ontology:cs_axiom_status(state_permission_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', state_permission_unnecessary, deontological).
narrative_ontology:cs_reference_frame('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', cypherpunk_monetary_sovereignty).
narrative_ontology:cs_drift_state('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', post_institutional_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e69bed7b-d66f-4b2f-b894-840f6c6dd99d', '').
narrative_ontology:cs_kernel_id(crypto_permissionless_reading, digital_money_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crypto_permissionless_reading, individual_holders).
narrative_ontology:constraint_beneficiary(crypto_permissionless_reading, cross_border_remitters).
narrative_ontology:constraint_beneficiary(crypto_permissionless_reading, censorship_resistant_actors).
narrative_ontology:constraint_victim(crypto_permissionless_reading, capital_control_regimes).
narrative_ontology:constraint_victim(crypto_permissionless_reading, sanctions_enforcement_authorities).
narrative_ontology:constraint_victim(crypto_permissionless_reading, monetary_policy_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold cryptocurrency as a store of value or medium of exchange outside state-controlled banking systems. They gain financial sovereignty and censorship resistance but bear volatility risk and regulatory uncertainty. Exit means converting back to fiat through regulated exchanges or peer-to-peer markets.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, individual_holders, beneficiary,
    moderate, biographical, mobile, global).

% Use cryptocurrency to send money across borders, bypassing high remittance fees and slow settlement times of traditional banking. They depend on network availability and local conversion infrastructure. Exit means returning to wire transfers or money-transfer services with higher costs.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, cross_border_remitters, beneficiary,
    powerless, immediate, constrained, global).

% Rely on cryptocurrency to transact when excluded from traditional financial systems—whether for political dissent, journalism in authoritarian contexts, or operating in sanctioned jurisdictions. Their identity is fused with the need for permissionless access; returning to state-mediated finance means accepting surveillance or exclusion.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, censorship_resistant_actors, beneficiary,
    moderate, biographical, identity_locked, global).

% Lose the ability to enforce capital controls when citizens can move value across borders via cryptocurrency without permission. They attempt to suppress adoption through exchange regulation and internet restrictions, but enforcement is costly and incomplete. Exit means accepting reduced monetary sovereignty.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, capital_control_regimes, payer,
    institutional, generational, constrained, national).

% Face degraded effectiveness of financial sanctions when targeted actors can transact via permissionless networks. They invest in blockchain forensics and exchange chokepoints but cannot prevent peer-to-peer transactions. The constraint erodes a primary tool of geopolitical coercion.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, sanctions_enforcement_authorities, payer,
    institutional, generational, constrained, global).

% Experience reduced transmission effectiveness of monetary policy when citizens can hold wealth in non-sovereign currencies. They cannot directly control cryptocurrency supply or interest rates. Exit means accepting a multi-currency environment or attempting to ban cryptocurrency adoption entirely.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, monetary_policy_institutions, payer,
    institutional, generational, constrained, national).

% Design and maintain the consensus protocols that enable permissionless money. They set technical standards and upgrade paths but do not control individual transactions or holdings. Their authority is epistemic and voluntary—users can fork or exit to alternative protocols.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, protocol_developers, agenda_setter,
    organized, generational, mobile, global).

% Watch cryptocurrency adoption as both competitive threat and potential integration opportunity. They provide on-ramps and custody services where regulation permits, but the core permissionless layer operates outside their control. They lobby for regulatory clarity that would bring cryptocurrency under traditional financial oversight.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, regulated_financial_institutions, observer,
    institutional, biographical, mobile, global).

% Study cryptocurrency as a monetary experiment testing theories of money emergence, network effects, and the necessity of state backing. They produce empirical analysis of adoption patterns, price stability, and macroeconomic effects without direct stake in the outcome.
narrative_ontology:constraint_stakeholder(crypto_permissionless_reading, academic_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables peer-to-peer value transfer without requiring permission from financial intermediaries or state authorities. Solves the double-spend problem through decentralized consensus, allowing strangers to transact without trusted third parties.
% TRANSFER_FUNCTION: Moves monetary sovereignty from state institutions to individual holders. Transfers enforcement capacity from sanctions authorities to protocol rules. Shifts seigniorage from central banks to early adopters and miners.
% ABSENT_VOICES: Unbanked populations in regions with poor internet infrastructure are structurally excluded from the conversation—they would benefit from financial inclusion but lack the technical access to participate. Their absence means the legitimacy debate is dominated by actors who already have some form of financial access.
% DISAPPEARANCE_RATIONALE: If permissionless cryptocurrency disappeared overnight, cross-border remitters would return to expensive wire transfers, censorship-resistant actors would lose their primary financial tool, capital control regimes would regain full monetary sovereignty, and sanctions enforcement would return to pre-2009 effectiveness. The global financial system would re-centralize around state-mediated channels.
% FOUNDING_PROBLEM: The 2008 financial crisis exposed the fragility of state-backed financial systems and the power of intermediaries to freeze accounts and censor transactions. The founding problem was: how can individuals transact and store value without depending on institutions that can fail or exclude them?
% FOUNDING_PROBLEM_CORROBORATION: Protocol developers and individual holders attest the problem remains live, citing ongoing bank failures, account freezes, and financial censorship. Monetary policy institutions and regulated financial institutions attest the founding problem was overstated and that cryptocurrency introduces more instability than it solves. Independent analysis from academic monetary economists shows mixed evidence: permissionless systems do provide censorship resistance but at the cost of volatility and limited scalability.
narrative_ontology:disappearance_verdict(crypto_permissionless_reading, world_rearranges).
narrative_ontology:founding_problem_status(crypto_permissionless_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(crypto_permissionless_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-15',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(crypto_permissionless_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crypto_permissionless_reading_tests).
:- end_tests(crypto_permissionless_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because early adopters and miners capture seigniorage that would otherwise accrue to central banks, and transaction costs (energy, fees) are borne by users. Suppression is higher (0.68) and rising because state actors increasingly attempt to control cryptocurrency adoption through exchange regulation, internet restrictions, and legal prohibitions—the constraint's persistence depends on resisting these suppression attempts. Theater is low-moderate (0.28) and rising: some cryptocurrency activity is performative (speculation dressed as monetary revolution), but the core censorship-resistance function remains real. Accessibility collapse is moderate (0.48): alternatives exist (fiat, gold, barter) but are less accessible for censorship-resistant use cases. Resistance is high (0.72): the constraint faces active opposition from capital-control regimes and sanctions authorities.
 *
 * PERSPECTIVAL GAP:
 *   From the individual-holder seat, the constraint operates as genuine coordination enabling financial sovereignty. From the capital-control-regime seat, the same structure operates as an attack on monetary sovereignty that must be suppressed. From the sanctions-authority seat, it is a tool for evading geopolitical enforcement. The engine computes these divergent classifications from the structural data—the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual holders, cross-border remitters, and censorship-resistant actors are structural beneficiaries—they gain financial sovereignty and transaction access. Capital-control regimes, sanctions authorities, and monetary policy institutions are structural victims—they lose enforcement capacity and policy effectiveness. Protocol developers are agenda-setters with mobile exit (they can fork or switch protocols). The directionality derivation should place beneficiaries near d=0.0 (low effective extraction) and institutional victims near d=0.8-0.9 (high effective extraction, as they bear the cost of reduced state capacity).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Does monetary legitimacy derive from state authority, or can it emerge from decentralized consensus and network effects alone?',
    'This is a conceptual question about the nature of money that cannot be resolved by empirical data. Different readings rest on different axioms: state-authority grounding vs. emergent-consensus grounding. The question is which axiom better predicts long-term adoption and stability.',
    'If legitimacy requires state backing, permissionless cryptocurrency remains a niche tool for censorship resistance but never achieves broad monetary status. If legitimacy can emerge from consensus, cryptocurrency could displace state-backed money in some use cases. The classification hinges on which axiom the observer holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Whether monetary legitimacy requires state authority or can emerge from decentralized consensus.').

omega_variable(
    scalability_vs_decentralization_tradeoff,
    'Can permissionless cryptocurrency scale to global transaction volumes without sacrificing decentralization, or does scalability require reintroducing trusted intermediaries?',
    'Empirical: measure transaction throughput, node distribution, and degree of intermediation as layer-2 solutions and protocol upgrades deploy. If throughput scales while node count remains distributed, the tradeoff is resolvable. If scaling requires consolidation, the permissionless property degrades.',
    'If the tradeoff is unresolvable, permissionless cryptocurrency remains limited to high-value, low-frequency transactions, and the coordination function is bounded. If resolvable, the constraint could scale to compete with state-backed payment systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_vs_decentralization_tradeoff, empirical, 'Whether permissionless systems can scale without reintroducing centralization.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the seigniorage captured by early adopters and miners a necessary cost of bootstrapping a permissionless network, or is it extractive rent-seeking that could be reduced with better protocol design?',
    'Compare seigniorage distribution across different consensus mechanisms (proof-of-work, proof-of-stake, alternative designs). If seigniorage concentration is invariant to mechanism choice, it is a necessary coordination cost. If it varies widely, some mechanisms are more extractive than others.',
    'If seigniorage is a necessary cost, the measured extraction is inherent to the coordination function. If it is reducible, the constraint is more extractive than it needs to be, and protocol competition should drive extraction downward over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether early-adopter seigniorage is necessary coordination cost or reducible extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crypto_permissionless_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cryp_tr_t0, crypto_permissionless_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cryp_tr_t3, crypto_permissionless_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement(cryp_tr_t6, crypto_permissionless_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement(cryp_tr_t9, crypto_permissionless_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement(cryp_tr_t12, crypto_permissionless_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(cryp_tr_t15, crypto_permissionless_reading, theater_ratio, 15, 0.28).

% Extraction over time
narrative_ontology:measurement(cryp_be_t0, crypto_permissionless_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cryp_be_t3, crypto_permissionless_reading, base_extractiveness, 3, 0.31).
narrative_ontology:measurement(cryp_be_t6, crypto_permissionless_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement(cryp_be_t9, crypto_permissionless_reading, base_extractiveness, 9, 0.39).
narrative_ontology:measurement(cryp_be_t12, crypto_permissionless_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(cryp_be_t15, crypto_permissionless_reading, base_extractiveness, 15, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cryp_su_t0, crypto_permissionless_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cryp_su_t3, crypto_permissionless_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(cryp_su_t6, crypto_permissionless_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(cryp_su_t9, crypto_permissionless_reading, suppression_requirement, 9, 0.63).
narrative_ontology:measurement(cryp_su_t12, crypto_permissionless_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(cryp_su_t15, crypto_permissionless_reading, suppression_requirement, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crypto_permissionless_reading, global_infrastructure).
narrative_ontology:affects_constraint(crypto_permissionless_reading, sovereign_cbdc_reading).
narrative_ontology:affects_constraint(crypto_permissionless_reading, regulated_stablecoin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the digital_money_legitimacy kernel. The readings differ in their axioms about the source of monetary legitimacy: state authority (sovereign_cbdc_reading), regulated private issuance (regulated_stablecoin_reading), or decentralized consensus (this reading). Each reading has a different beneficiary/victim structure and different ε. They are linked via network.affects_constraints because adoption of one reading creates structural pressure on the others—if permissionless cryptocurrency achieves broad adoption, it erodes the necessity of state-backed digital money; if CBDCs achieve dominance, they crowd out permissionless alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
