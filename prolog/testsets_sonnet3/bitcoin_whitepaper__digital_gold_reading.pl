% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Bitcoin as Digital Gold: Store-of-Value / Inflation-Hedge Reading
 *   domain: cryptocurrency economics/monetary systems
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested Bitcoin whitepaper
 *   kernel: the digital-gold reading, under which Bitcoin's value proposition
 *   centers on scarcity-driven store-of-value and inflation-hedge properties,
 *   with transaction throughput and fee levels treated as an acceptable
 *   secondary cost of preserving the scarcity property. This is a distinct
 *   constraint from the p2p-cash reading (which prioritizes cheap, fast
 *   electronic transactions and would treat high fees as a coordination
 *   failure) and from the protocol-ossification reading (which centers on the
 *   legitimacy conditions for protocol change). Each reading is authored as
 *   its own file with its own epsilon; this file's epsilon (0.58) reflects
 *   extraction specific to the digital-gold framing — appreciation-driven
 *   wealth transfer to early holders and fee-tolerant institutional
 *   intermediaries — not an average across readings.
 *
 * KEY AGENTS:
 *   - early_holders_and_miners: primary beneficiary (organized/arbitrage) — captures appreciation
 *   - custodial_exchanges_and_etf_issuers: institutional beneficiary (institutional/arbitrage) — monetizes the narrative as a financial product
 *   - mining_pool_operators: agenda_setter (institutional/arbitrage) — sets fee-market tolerance consistent with scarcity framing
 *   - late_retail_entrants: primary target (powerless/constrained) — buys in after appreciation, bears volatility
 *   - small_value_transactors: primary target (powerless/trapped) — priced out of payment use by fee competition
 *   - unbanked_populations_seeking_payment_utility: excluded voice — the p2p-cash constituency structurally sidelined
 *   - macro_narrative_analysts: analytical observer — assesses whether the hedge narrative empirically holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.58).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.35).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold: Store-of-Value / Inflation-Hedge Reading").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency economics/monetary systems").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'b4956404-8a74-41cb-8424-1ef6b66f4627').
narrative_ontology:cs_kernel_codification('b4956404-8a74-41cb-8424-1ef6b66f4627', fixed_text).
narrative_ontology:cs_authority_grounding('b4956404-8a74-41cb-8424-1ef6b66f4627', distributed).
narrative_ontology:cs_reading_relation('b4956404-8a74-41cb-8424-1ef6b66f4627', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('b4956404-8a74-41cb-8424-1ef6b66f4627', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('b4956404-8a74-41cb-8424-1ef6b66f4627', foundational, scarcity_preservation_takes_priority_over_throughput).
narrative_ontology:cs_axiom_status(scarcity_preservation_takes_priority_over_throughput, holdable).
narrative_ontology:cs_axiom_grounding('b4956404-8a74-41cb-8424-1ef6b66f4627', scarcity_preservation_takes_priority_over_throughput, instrumental).
narrative_ontology:cs_axiom('b4956404-8a74-41cb-8424-1ef6b66f4627', secondary, fee_market_pressure_is_acceptable_cost_of_settlement_assurance).
narrative_ontology:cs_axiom_status(fee_market_pressure_is_acceptable_cost_of_settlement_assurance, holdable).
narrative_ontology:cs_axiom_grounding('b4956404-8a74-41cb-8424-1ef6b66f4627', fee_market_pressure_is_acceptable_cost_of_settlement_assurance, instrumental).
narrative_ontology:cs_reference_frame('b4956404-8a74-41cb-8424-1ef6b66f4627', whitepaper_electronic_cash_intent).
narrative_ontology:cs_drift_state('b4956404-8a74-41cb-8424-1ef6b66f4627', post_2017_institutional_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b4956404-8a74-41cb-8424-1ef6b66f4627', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_holders_and_miners).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges_and_etf_issuers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_retail_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, small_value_transactors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, unbanked_populations_seeking_payment_utility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquired or mined coins when price and difficulty were low. Benefit disproportionately from the digital-gold narrative because it drives price appreciation independent of transactional utility. Can exit into fiat or other assets at will; their holdings appreciate as new capital enters seeking a hedge.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_holders_and_miners, beneficiary,
    organized, generational, arbitrage, global).

% Package Bitcoin as an investable store-of-value product (spot ETFs, custodial accounts), collecting fees on assets under management. Actively promote the digital-gold framing in marketing and regulatory filings because it fits existing securities/commodities frameworks better than a payments framing does. Face no meaningful exit cost from the narrative they helped construct.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges_and_etf_issuers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, custodial_exchanges_and_etf_issuers, agenda_setter).

% Set and enforce the fee-market dynamics that follow from a fixed block-size policy consistent with the store-of-value reading: high fees during demand spikes are treated as an acceptable feature of a scarce settlement asset rather than a defect to fix. Their revenue rises with fee pressure, so they have no incentive to advocate capacity changes that would undercut the scarcity narrative.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, mining_pool_operators, beneficiary).

% Buy in after substantial price appreciation, often during hype cycles amplified by the digital-gold narrative. Pay elevated entry prices and volatility risk with no structural claim on the network's original coordination-cost advantages. Exit means realizing losses; staying means continued exposure to a narrative-driven asset whose price no longer tracks any usage metric they can verify.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_retail_entrants, payer,
    powerless, biographical, constrained, global).

% Attempt to use Bitcoin for ordinary payments and remittances but face transaction fees that can exceed the value being transferred during congestion, a direct consequence of treating fee pressure as tolerable friction on a settlement-of-scarce-value network rather than a problem to solve. Have no seat in shaping fee-market policy and few practical substitutes once committed to the network for a given transfer.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, small_value_transactors, payer,
    powerless, immediate, trapped, global).

% Represent the population the p2p-cash vision of Bitcoin was meant to serve — people without reliable banking access who need cheap, fast electronic transactions. Under the digital-gold reading their needs are structurally deprioritized: fee levels and volatility that make Bitcoin impractical as everyday money are treated as acceptable costs of preserving scarcity value for holders. They have no representation in the venues (mining pool governance, ETF product design) that shape the reading's dominance.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, unbanked_populations_seeking_payment_utility, excluded,
    powerless, biographical, trapped, global).

% Track how the store-of-value framing performs against inflation benchmarks, correlate Bitcoin price with macro liquidity conditions, and assess whether the asset behaves more like a risk-on speculative instrument than an inflation hedge. Their findings feed back into whether institutional capital treats the reading as vindicated or discredited, without themselves holding a stake in either outcome.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, macro_narrative_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, diffuse).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a fixed, verifiably scarce, censorship-resistant unit that a large and growing pool of holders can use as a common store of value, substituting for scarce commodities like gold when trust in fiat monetary policy weakens.
% TRANSFER_FUNCTION: Moves purchasing power from later entrants (who buy at appreciated prices and pay fee-market costs) to earlier holders and infrastructure operators (miners, custodians, ETF issuers) who benefit from appreciation and fee revenue without a corresponding increase in transactional utility delivered to new users.
% ABSENT_VOICES: Users who need Bitcoin as functional payment infrastructure — remittance senders, unbanked populations, small merchants — are structurally absent from the venues (mining economics, ETF product design, institutional custody) where the digital-gold framing is reinforced; the p2p-cash reading is their voice, and it loses influence as this reading dominates capital allocation and protocol-change politics.
% DISAPPEARANCE_RATIONALE: Early holders, custodians, and miners would argue the world rearranges catastrophically — trillions in market capitalization and an entire ETF product ecosystem depend on the store-of-value narrative holding. Payment-utility advocates would argue the underlying protocol and its coordination value persist regardless of which narrative frames it, and that abandoning the digital-gold framing would let the p2p-cash function re-emerge. The parties genuinely disagree on which is true.
% FOUNDING_PROBLEM: The original whitepaper set out to solve double-spending for peer-to-peer electronic cash without a trusted third party — a payments problem, not a store-of-value problem.
% FOUNDING_PROBLEM_CORROBORATION: Early cypherpunk mailing-list archives and the whitepaper's own title and abstract, cited independently by economic historians and payment-systems researchers outside any Bitcoin-holding constituency, corroborate that the founding problem was electronic cash. Institutional holders and ETF issuers attest the more pressing problem it now solves is monetary debasement hedging, but that attestation comes entirely from parties who benefit financially from the store-of-value framing gaining precedence — no outside corroboration supports the founding problem having been store-of-value from inception.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, contested).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects a real but partial extraction: the reading does coordinate a genuine scarce-asset function for holders while transferring appreciation-driven gains from later entrants to earlier ones, and fee-market dynamics function as a toll that falls disproportionately on small transactors who derive no store-of-value benefit from holding tiny, transient balances. Suppression (0.35) is moderate rather than high — no single actor coercively forecloses the p2p-cash alternative; rather, capital allocation, narrative dominance in media and regulatory filings, and mining-pool fee-tolerance jointly marginalize it. Theater ratio rises from 0.05 to 0.28 over the interval as institutional marketing (ETF prospectuses, 'digital gold' branding) increasingly substitutes for demonstrated inflation-hedge performance, which macro analysts note correlates more with risk-on liquidity cycles than with inflation prints.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (early holders, custodians, mining pools) this reading is straightforward coordination: a scarce, verifiable asset serving a real hedging demand, with fees as a minor friction cost. From the payer seats (late entrants, small transactors) the same structure is asymmetric extraction: they absorb appreciation-driven entry costs and fee-market tolls that exist specifically because the network prioritizes scarcity-preservation over transactional throughput. The engine should compute these as different seat-level types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Early holders and infrastructure operators are declared beneficiaries because the digital-gold framing directly increases the value of what they already hold or the fees they collect, with mobile/arbitrage exit options insulating them from any narrative failure. Late entrants and small transactors are declared victims because they bear the costs the reading treats as acceptable (entry-price risk, fee competition) without a correspondingly increased claim on the network's founding payments utility. The unbanked payment-utility population is excluded rather than victimized directly by this reading's mechanics — they are the constituency the sibling p2p-cash reading would serve, marginalized by resource and attention allocation rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peer-to-peer electronic cash) is contested as live vs. dead specifically because this reading's dominance has NOT solved it — fee volatility and price volatility both work against everyday payment use — while claiming the network's success as vindication of a different, later-arrived-at problem (inflation hedging). This is a mandatrophy-adjacent pattern: an arrangement whose original coordination justification has been substantially supplanted by a different justification that better serves current beneficiaries, without an explicit acknowledgment that the mandate shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bitcoin_kernel_reading_dominance,
    'Is the digital-gold reading''s current dominance over the p2p-cash reading a natural consequence of the protocol''s technical properties (limited throughput, verifiable scarcity), or a contingent outcome of capital-allocation and narrative dynamics that could have gone differently?',
    'Comparative analysis of alternative UTXO-based cryptocurrencies that retained low fees and high throughput at the cost of narrative-level scarcity messaging — do they exhibit different beneficiary/victim distributions, and did network effects or capital flows determine which reading captured mainstream Bitcoin discourse?',
    'If dominance is a contingent capital/narrative outcome rather than technical necessity, the extraction attributed to this reading is more clearly a constructed choice rather than an inherent property of the protocol, strengthening the tangled_rope classification over a more mountain-like ''this is just how blockchains scale'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bitcoin_kernel_reading_dominance, conceptual, 'Whether digital-gold dominance over p2p-cash is technically necessitated or contingently constructed.').

omega_variable(
    inflation_hedge_empirical_validity,
    'Does Bitcoin actually function as an inflation hedge in the sense the digital-gold reading claims, or does its price behavior track speculative risk appetite more than monetary debasement?',
    'Longitudinal correlation analysis between Bitcoin price, CPI/monetary-base measures, and risk-asset indices (equities, growth stocks) across multiple macro regimes including tightening cycles.',
    'If the hedge property is empirically weak, the digital-gold reading''s coordination-function claim (a genuine service to holders seeking inflation protection) is substantially undermined, shifting the classification toward snare (extraction dressed as a service that does not reliably deliver on its coordination promise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_hedge_empirical_validity, empirical, 'Whether Bitcoin empirically delivers the inflation-hedge function this reading claims to provide.').

omega_variable(
    reading_framing_disambiguation,
    'Given the SCOPE manifest''s decomposition of ''the Bitcoin whitepaper'' into three readings, is the digital_gold_reading the correct locus for authoring the fee-tolerance/scarcity-priority structural claim, or should fee-market policy itself be a fourth, separately-decomposed constraint shared across readings?',
    'Trace whether fee-market outcomes (block-size policy, mempool congestion pricing) are causally downstream of the digital-gold reading''s dominance specifically, or are an independent technical variable that both readings inherit and interpret differently.',
    'If fee-market policy is better modeled as an independent upstream constraint, this story''s extractiveness score may currently conflate two structurally distinct claims (narrative dominance vs. technical throughput policy) that the epsilon-invariance principle would require decomposing further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_disambiguation, conceptual, 'Whether fee-market structure should be its own constraint rather than embedded in this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2012, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(bitc_tr_t2015, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(bitc_tr_t2018, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2009, 0.1).
narrative_ontology:measurement(bitc_be_t2012, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2012, 0.18).
narrative_ontology:measurement(bitc_be_t2015, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(bitc_be_t2018, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2021, 0.53).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2024, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(bitcoin_whitepaper__digital_gold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.12).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings decomposed from the natural-language label 'the Bitcoin whitepaper's purpose.' Digital_gold_reading and p2p_cash_reading compete directly for the resource that determines throughput/fee policy (block-space allocation), so this story influences p2p_cash_reading's viability: capital and developer attention flowing toward store-of-value framing structurally starves the payments-optimization agenda the sibling reading depends on. Digital_gold_reading and protocol_ossification_reading coexist and reinforce each other, since resistance to block-size-increasing protocol changes serves both scarcity-narrative preservation and consensus-rule stability, but neither reading logically forecloses the other — different factions of the Bitcoin community hold each without contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
