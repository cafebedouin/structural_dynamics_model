% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Bitcoin Digital Gold Reading
 *   domain: cryptocurrency_economics/monetary_systems
 *
 * SUMMARY:
 *   The Bitcoin network is interpreted through the 'digital gold' reading: a
 *   fixed-supply, scarce digital asset prioritized as a non-sovereign store
 *   of value and inflation hedge. This reading coordinates holders around
 *   scarcity and disincentivizes protocol changes that would increase
 *   throughput or alter monetary policy. It generates asymmetric extraction
 *   by transferring purchasing power from late entrants to early holders via
 *   appreciation dynamics, and by accepting a high-fee market that prices
 *   small transactors out of main-chain participation. The constraint is
 *   actively enforced through protocol conservatism, social narrative
 *   control, and the economic interests of incumbents.
 *
 * KEY AGENTS:
 *   - early_holders: Primary beneficiary (powerful/arbitrage) â accumulated low-cost supply and benefit from scarcity premium.
 *   - institutional_investors: Primary beneficiary (powerful/mobile) â allocate capital to the store-of-value narrative and benefit from regulatory legitimacy.
 *   - mining_incumbents: Secondary beneficiary (organized/constrained) â collect fees and subsidy under a fee-market regime justified by the digital gold framing.
 *   - core_developers_conservatives: Agenda setter (organized/mobile) â enforce protocol conservatism and resist changes threatening the scarcity consensus.
 *   - late_entrants_retail: Primary payer (moderate/constrained) â enter at high cost basis and transfer wealth to earlier holders.
 *   - small_transactors: Payer (powerless/trapped) â priced out of block space by high fees, unable to move small balances.
 *   - p2p_cash_community: Excluded voice (organized/constrained) â advocates for medium-of-exchange utility marginalized by the dominant narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.62).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.55).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin Digital Gold Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency_economics/monetary_systems").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '708de271-e3af-4bde-89d4-824eb3a848fb').
narrative_ontology:cs_kernel_codification('708de271-e3af-4bde-89d4-824eb3a848fb', fixed_text).
narrative_ontology:cs_authority_grounding('708de271-e3af-4bde-89d4-824eb3a848fb', distributed).
narrative_ontology:cs_reading_relation('708de271-e3af-4bde-89d4-824eb3a848fb', bitcoin_whitepaper__p2p_cash_reading, coexists_with).
narrative_ontology:cs_reading_relation('708de271-e3af-4bde-89d4-824eb3a848fb', bitcoin_whitepaper__protocol_ossification_reading, influences).
narrative_ontology:cs_axiom('708de271-e3af-4bde-89d4-824eb3a848fb', foundational, scarcity_premium_over_transaction_efficiency).
narrative_ontology:cs_axiom_status(scarcity_premium_over_transaction_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('708de271-e3af-4bde-89d4-824eb3a848fb', scarcity_premium_over_transaction_efficiency, instrumental).
narrative_ontology:cs_axiom('708de271-e3af-4bde-89d4-824eb3a848fb', foundational, fixed_supply_monetary_policy_as_immutable).
narrative_ontology:cs_axiom_status(fixed_supply_monetary_policy_as_immutable, holdable).
narrative_ontology:cs_axiom_grounding('708de271-e3af-4bde-89d4-824eb3a848fb', fixed_supply_monetary_policy_as_immutable, conventional).
narrative_ontology:cs_reference_frame('708de271-e3af-4bde-89d4-824eb3a848fb', capped_supply_sovereign_store).
narrative_ontology:cs_drift_state('708de271-e3af-4bde-89d4-824eb3a848fb', post_institutional_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('708de271-e3af-4bde-89d4-824eb3a848fb', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_investors).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_incumbents).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants_retail).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_transactors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulated positions at low cost basis during the network's early years. Benefit from scarcity premium and narrative-driven appreciation. Can exit to fiat or other liquid assets at will, realizing gains transferred from later entrants.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Allocate treasury or fund capital to Bitcoin as a non-sovereign inflation hedge and portfolio diversifier. Benefit from regulatory and macro narrative legitimacy of the digital gold framing. Exit via deep, liquid markets without significant friction.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_investors, beneficiary,
    powerful, biographical, mobile, global).

% Earn block subsidy and transaction fees. The digital gold reading justifies a high-fee settlement market as economically necessary and acceptable. Capital is physically locked in specialized ASIC hardware and long-term energy contracts, making exit costly and slow.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_incumbents, beneficiary,
    organized, biographical, constrained, global).

% Maintain the reference protocol implementation and resist consensus changes that would increase on-chain throughput, alter the 21M supply cap, or reduce settlement costs. Their social legitimacy and influence depend on preserving the scarcity consensus and store-of-value properties.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, core_developers_conservatives, agenda_setter,
    organized, generational, mobile, global).

% Enter during bull-market phases at high cost basis, transferring purchasing power to earlier holders. Advised to hold for generational wealth. Exiting typically means realizing significant losses, creating a sunk-cost lock-in that reinforces continued participation.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrants_retail, payer,
    moderate, biographical, constrained, global).

% Hold small balances or attempt to use Bitcoin for payments. Priced out of main-chain block space by high fees justified under the digital gold framing. Unable to economically move funds, rendering their balances practically frozen.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_transactors, payer,
    powerless, immediate, trapped, local).

% Advocates for peer-to-peer electronic cash utility and low-fee access. Structurally marginalized by the dominant digital gold narrative and protocol conservatism. Attempts to fork or build alternative layers have failed to capture the base-layer network effect.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, p2p_cash_community, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global economic actors around a credibly scarce, non-sovereign digital asset with a fixed supply schedule, creating a decentralized store of value and inflation hedge outside state monetary systems.
% TRANSFER_FUNCTION: Moves purchasing power from late entrants and small transactors to early holders, institutional investors, and miners via asset appreciation dynamics and fee-based prioritization of block space.
% ABSENT_VOICES: Small transactors priced out by fees and the p2p cash community advocating for medium-of-exchange utility are structurally excluded from core protocol development and narrative formation; they would argue for base-layer capacity expansion and fee minimization.
% DISAPPEARANCE_RATIONALE: If the digital gold reading vanished, the incentive to hold for scarcity premium would collapse, institutional treasury allocations would reallocate, mining economics would face a severe security-budget crisis, and the protocol would likely drift toward functional obsolescence or a competing consensus reading.
% FOUNDING_PROBLEM: Sovereign currency debasement and the absence of a non-sovereign, censorship-resistant store of value with a credibly fixed monetary policy.
% FOUNDING_PROBLEM_CORROBORATION: Macroeconomic critics of sovereign debt and currency debasement outside the concentrated Bitcoin beneficiary set attest to the live problem of inflation. However, payment-systems researchers and users in developing economies who need transactional utility rather than storage attest that the digital gold framing misidentifies the optimal solution. No corroborating source outside all benefiting parties unambiguously supports this specific reading's priority over its siblings.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the store-of-value framing structurally requires continuous capital inflow from new entrants to realize appreciation for early holders, creating a wealth-transfer dynamic. The fee market under this reading treats small transactions as economically unviable, extracting access from the powerless. Suppression (0.55) reflects the active social and technical resistance to block-size increases and protocol alterations that would enable cheaper transactions, effectively suppressing the p2p-cash alternative within the Bitcoin kernel. Theater (0.25) is moderate: the scarcity and hashrate security are functionally real, but the 'digital gold' narrative includes performative elements (hodl culture, institutional marketing) that exceed the technical necessity of the coordination. Accessibility collapse (0.45) is moderate: alternatives (altcoins, Layer 2s) exist but suffer from network-effect disadvantages and legitimacy deficits relative to Bitcoin's first-mover position. Resistance (0.40) is moderate: the p2p-cash community continues to object, and fork attempts have occurred, though they have lost market share.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (core developers, early holders, miners) experience the constraint as genuine coordination around a priceless public goodâsound money. The payer seats (late entrants, small transactors) experience the same structure as an extraction mechanism where their participation is either a wealth transfer or an impossibility. The engine will compute divergent per-seat classifications from this structural asymmetry: the powerful early holder computes near rope/beneficiary, while the small transactor computes near snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders and institutional investors are declared beneficiaries with arbitrage and mobile exit options; their directionality sits near the full-beneficiary end, damping effective extraction. Mining incumbents are beneficiaries but with constrained exit (ASIC lock-in), giving them a slightly less favorable d. Late entrants are payers with constrained exit (sunk cost, loss realization), pushing d toward the target end. Small transactors are payers with trapped exit (cannot afford fees to move funds), placing them nearest the full-target end and amplifying effective extraction despite their individual small balances.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâsovereign debasement and lack of censorship-resistant savingsâremains live, preventing simple mandatrophy classification. However, the digital gold reading's prioritization of appreciation over transactional utility creates a mismatch between the original coordination intent (peer-to-peer electronic cash per the whitepaper) and the current extraction profile. The framework prevents mislabeling by requiring both beneficiaries and victims for tangled rope: the scarcity consensus genuinely coordinates non-sovereign savings, while the fee market and appreciation structure asymmetrically extract from late and small participants. If the coordination function died but the structure persisted, it would migrate toward piton or snare; currently the coordination remains live for the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the digital gold reading a legitimate interpretation of the Bitcoin whitepaper kernel, or an extraction narrative that forecloses the p2p cash reading?',
    'Historical and textual analysis of the whitepaper, early communications, and the 2015-2017 block-size debate to determine original authorial intent and community understanding.',
    'If the kernel was unambiguously p2p cash, this reading is an extractive overlay migrating toward snare; if ambiguous, it coexists legitimately and classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the digital gold reading is a legitimate kernel reading or extractive overlay.').

omega_variable(
    fee_market_extraction_vs_security,
    'Are main-chain transaction fees under the digital gold reading set at a level necessary for long-term security, or do they function as extractive gatekeeping that enriches miners while excluding small transactors?',
    'Empirical comparison of fee revenue to miner operational costs and security budget models; observation of whether small-balance UTXOs become economically unspendable.',
    'If fees vastly exceed security needs, extraction is higher than structurally justified, pushing classification toward snare; if tightly coupled to security, extraction is inherent coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_market_extraction_vs_security, empirical, 'Whether the fee market extracts beyond necessary coordination cost.').

omega_variable(
    late_entrant_wealth_transfer,
    'Does the store-of-value appreciation mechanism structurally require continuous new entrant inflow, creating a zero-sum wealth transfer from late to early participants?',
    'Longitudinal analysis of holder cost-basis distribution and price impact of institutional inflow plateaus; assessment of whether early-holder gains are realized through exit or merely notional.',
    'If the dynamic is zero-sum inflow-dependent, late entrants are structurally victimized and extraction is higher; if backed by independent utility or cash flow, the constraint is more coordinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(late_entrant_wealth_transfer, empirical, 'Whether appreciation is inflow-dependent zero-sum transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(btc_dg_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(btc_dg_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(btc_dg_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(btc_dg_tr_t9, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(btc_dg_tr_t12, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(btc_dg_tr_t15, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 15, 0.25).

% Extraction over time
narrative_ontology:measurement(btc_dg_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(btc_dg_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(btc_dg_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(btc_dg_be_t9, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(btc_dg_be_t12, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(btc_dg_be_t15, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 15, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(btc_dg_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(btc_dg_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(btc_dg_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(btc_dg_su_t9, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 9, 0.52).
narrative_ontology:measurement(btc_dg_su_t12, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(btc_dg_su_t15, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the bitcoin_whitepaper kernel, decomposed from p2p_cash_reading and protocol_ossification_reading due to structural divergence in beneficiary/victim structure and coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
