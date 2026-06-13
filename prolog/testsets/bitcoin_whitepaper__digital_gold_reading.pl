% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bitcoin_whitepaper__digital_gold_reading
 *   human_readable: Bitcoin as Scarce Digital Gold (Store of Value Reading)
 *   domain: economic/technological/monetary
 *
 * SUMMARY:
 *   Bitcoin's white paper (2008) presented a peer-to-peer electronic cash
 *   system without relying on trusted intermediaries. The digital-gold
 *   reading is ONE contested interpretation: Bitcoin is a scarce digital
 *   asset optimized for store-of-value and inflation hedging, where the
 *   21-million supply cap is the primary value proposition, transaction
 *   capacity is a secondary concern, and rising on-chain fees are acceptable
 *   costs of maintaining scarcity. This reading emerged gradually (2013–2017)
 *   as institutional adoption increased, large hodlers accumulated capital,
 *   and protocol design choices (block-size caps, fee-market competition)
 *   prioritized scarcity over transaction throughput. The constraint under
 *   this reading is TANGLED ROPE: it provides genuine coordination (immutable
 *   ledger, decentralized settlement) AND asymmetric extraction (early
 *   adopters appreciate, late entrants pay fees, transaction-dependent users
 *   are excluded). The extraction is ACTIVELY ENFORCED through mining-pool
 *   consensus, protocol rules, and community narratives that defend scarcity
 *   against proposals to increase transaction capacity. Critically, the claim
 *   and metrics are authored independently: this story CLAIMS tangled_rope
 *   based on the presence of beneficiaries, victims, and active enforcement;
 *   the metrics (extractiveness 0.68, suppression 0.52, theater 0.41) are
 *   authored as honestly descriptive of the constraint's current operation.
 *   The engine's computation will determine whether the structural data
 *   supports the claim or reveals a different type.
 *
 * KEY AGENTS:
 *   - early_adopters: beneficiaries — acquired BTC pre-2015 at low cost; benefit from appreciation driven by scarcity narrative
 *   - institutional_hodlers: beneficiaries and agenda-setters — corporations, sovereign funds; drive digital-gold framing; influence protocol governance toward stability
 *   - large_bitcoin_holders: agenda-setters — miners, exchange operators; enforce scarcity through network rules and fee markets
 *   - late_entrants: victims — priced out by appreciation; face transaction fees ($5–$50/tx in peak periods); trapped by sunk costs
 *   - transaction_dependent_users: victims and excluded — merchants, remittance senders; cannot afford on-chain fees; excluded from white-paper use case
 *   - unbanked_populations: excluded (not victims, as they were never included) — promised financial access; blocked by high transaction costs
 *   - protocol_developers: agenda-setters with constrained latitude — maintain Bitcoin Core; face resistance from hodlers when proposing fee reductions or capacity increases
 *   - layer_2_operators: beneficiaries — profit from high on-chain fees that drive users to sidechains and Lightning
 *   - monetary_authorities: observers — analyze Bitcoin as alternative store-of-value and systemic risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.52).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Scarce Digital Gold (Store of Value Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "economic/technological/monetary").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c').
narrative_ontology:cs_kernel_codification('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', fixed_text).
narrative_ontology:cs_authority_grounding('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', extraction).
narrative_ontology:cs_interpretation_layer_present('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c').
narrative_ontology:cs_reading_relation('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', bitcoin_whitepaper__p2p_cash_reading, forecloses).
narrative_ontology:cs_reading_relation('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', foundational, scarcity_determines_value).
narrative_ontology:cs_axiom_status(scarcity_determines_value, holdable).
narrative_ontology:cs_axiom_grounding('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', scarcity_determines_value, instrumental).
narrative_ontology:cs_axiom('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', foundational, transaction_throughput_secondary_to_settlement_security).
narrative_ontology:cs_axiom_status(transaction_throughput_secondary_to_settlement_security, holdable).
narrative_ontology:cs_axiom_grounding('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', transaction_throughput_secondary_to_settlement_security, deontological).
narrative_ontology:cs_axiom('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', secondary, peer_to_peer_payment_function_subordinate_to_store_of_value).
narrative_ontology:cs_axiom_status(peer_to_peer_payment_function_subordinate_to_store_of_value, overridden).
narrative_ontology:cs_axiom_grounding('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', peer_to_peer_payment_function_subordinate_to_store_of_value, empirically_contingent).
narrative_ontology:cs_reference_frame('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', digital_scarcity_value_proposition).
narrative_ontology:cs_drift_state('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', contemporary_institutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f24eb05-c2f4-47bf-a5a6-a00dc1a4989c', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, large_bitcoin_holders).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, transaction_dependent_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, unbanked_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).

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
 *   Extractiveness rises from 0.12 (2009: negligible early-stage extraction) to 0.68 (2024: high institutional-grade extraction). The curve reflects the reading's maturation: early Bitcoin was low-stakes, no one was priced out, and transaction costs were immaterial. By 2021–2024, asset appreciation created a two-tier economy (early holders with massive gains, late entrants with capital losses), on-chain fees became economically prohibitive for small transactions (~$5–$50 per tx), and the white-paper use case was functionally inaccessible. Theater ratio rises from 0.05 (2009: minimal performative activity) to 0.41 (2024: substantial theater). Early enforcement was genuine security (fighting fraud, spam). By 2024, a significant share of enforcement activity is theatrical: narrative maintenance (digital-gold mythology), community management (repelling cash-use advocates), and technical theater (claiming scarcity is natural law rather than engineered). Suppression requirement rises from 0.15 (early: minimal opposition) to 0.52 (2024: moderate active suppression). Suppression exists because the p2p-cash reading and protocol-ossification reading offer structural alternatives; the digital-gold reading's persistence requires continuous defense against these alternatives through hashpower concentration, community discourse control, and technical obstacles to capacity expansion. The measurement series spans one aligned time grid (same time points for all three metrics) so the engine can analyze drift patterns.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (large hodlers, institutional investors) experience the constraint as genuine coordination: a decentralized ledger they have built and actively defend against attacks and alternatives. From their perspective, scarcity is the value proposition, transaction fees are necessary to prioritize security, and late entrants are making rational choices to enter a transparent market. The target seats (late entrants, transaction users, unbanked) experience the same constraint as extraction: they arrived after value was created, pay fees they did not authorize, and are excluded from the system's original promise. The beneficiary seats (early adopters, layer-2 operators) experience it as fortuitous gain: they arrived early or built infrastructure that captures value from the constraint's operation. The observer seat (monetary authorities) experiences it as a potential rival to fiat currency, neither beneficiary nor victim but analyzing systemic implications. This perspectival gap is the constraint operating at full strength: it coordinated early on (genuine peer-to-peer), then stratified (benefits became asymmetric), then extracted (extraction became active enforcement). The engine measures this through power-atom directionality: institutional rule-setters compute lower d than powerless late entrants, so the same constraint produces different classifications per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality divergence is structural and substantial. Early adopters and institutional holders sit at d ≈ 0.05–0.15 (strong beneficiaries: asset appreciation, no fees, influence over rules). Large hodlers sit at d ≈ 0.15–0.25 (beneficiaries AND agenda-setters: they set rules, extract indirectly through fee markets). Late entrants sit at d ≈ 0.75–0.85 (targets: pay fees, absorb losses, face closing transaction costs). Transaction-dependent users sit at d ≈ 0.8–0.95 (targets: fees prohibitive, excluded from use case). Unbanked sit at d = 1.0 (complete targets: never included, permanently excluded by the constraint). Layer-2 operators sit at d ≈ 0.2–0.3 (secondary beneficiaries: high on-chain fees drive user adoption). Protocol developers sit at d ≈ 0.45–0.55 (near-symmetric: genuinely maintain the system, but constrained by hodler resistance). This directionality profile is ASYMMETRIC across seats: the same constraint computes as a rope/coordination tool from the beneficiary seats and as a snare/extraction mechanism from the target seats. The engine's per-seat computation should reveal this divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would be triggered if founding_problem_status = dead AND disappearance_verdict = world_unchanged. Here, founding_problem_status = contested (not dead), so mandatrophy does not apply by the strict gate. However, the underlying dynamic is present: the digital-gold reading's founding mandate (provide alternative to fiat inflation) is still LIVE, but a competing mandate (peer-to-peer cash for financial inclusion) has been ABANDONED. The question is whether abandonment of the secondary mandate (cash use) constitutes mandatrophy of the primary mandate (inflation hedge). The commentary argues: the inflation-hedge mandate is live and functional; the cash-use mandate was actively displaced by protocol choices that prioritized scarcity. This is not mandatrophy but rather a deliberate reinterpretation of the kernel, where one reading (digital-gold) forecloses another reading (p2p-cash) and eliminates a use case. The mandatrophy logic does not capture this dynamic because mandatrophy requires the founding problem to be DEAD (no longer relevant), whereas here the founding problem has SHIFTED (from inflation + intermediary risk to scarcity + store-of-value). The constraint should NOT declare mandatrophy_resolved=true, because the constraint's primary function (inflation hedge through scarcity) remains live and defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scarcity_enforcement_naturalness,
    'Is Bitcoin''s 21-million supply cap a natural feature of mathematics and cryptography, or a designed policy choice that benefits early adopters and requires active enforcement by rule-setters?',
    'Analyze whether the 21-million cap is mathematically inevitable (like the speed-of-light limit in physics) or a choice point where alternative rules (inflation schedule, dynamic supply) were available and actively rejected. Examine protocol governance logs showing rule-defenders explicitly choosing scarcity over alternative designs.',
    'If the scarcity cap is a natural law, Bitcoin is a mountain; if it is an engineered constraint maintained by rule-setters with asymmetric benefits, Bitcoin is a tangled_rope or snare. The extraction reading depends on the distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scarcity_enforcement_naturalness, conceptual, 'Whether Bitcoin''s scarcity is a natural limit or a designed constraint maintained by beneficiaries.').

omega_variable(
    digital_gold_vs_peer_to_peer_cash_kernel,
    'Which reading of the Bitcoin white-paper kernel was the authentic founding intent: store-of-value (digital gold) or medium-of-exchange for everyday transactions (peer-to-peer cash)?',
    'Examine Satoshi Nakamoto''s original writings, early mining incentives, initial transaction volumes, and protocol design choices (block size, fee structure). Determine whether the system was architected for settlement or for high-frequency transactions. Analyze the block-size wars (2015–2017) as a contested reinterpretation of the kernel.',
    'The digital-gold reading is defensible under the store-of-value interpretation; the p2p-cash reading is defensible under the transaction-efficiency interpretation. This constraint (digital-gold) excludes the cash reading; the cash reading forecloses the gold reading''s extraction claims. The question determines which reading is authentic and which is a post-hoc reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_gold_vs_peer_to_peer_cash_kernel, conceptual, 'The contested kernel: did Bitcoin white-paper prioritize gold-like scarcity or cash-like transaction utility?').

omega_variable(
    late_entrant_victimhood_ambiguity,
    'Are late entrants victims of the digital-gold reading''s extraction, or are they making an informed choice to enter a transparent market with known constraints?',
    'Analyze whether late entrants have adequate information about fee structures and scarcity constraints at entry time; whether they can exit without prohibitive losses; and whether the reading actively suppresses information about transaction costs or alternative use cases. Compare to cases where market entry terms are transparently disclosed.',
    'If late entrants are suppressed from understanding constraints (theater_ratio applies), they are victims and extraction is coercive. If constraints are transparent and entry is voluntary, extraction is rent-collection but not necessarily coercive. The suppression metric depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_entrant_victimhood_ambiguity, empirical, 'Whether late entrants are victims of information suppression or making transparent choices.').

omega_variable(
    unbanked_use_case_abandonment,
    'Did the digital-gold reading deliberately foreclose Bitcoin''s unbanked-inclusion use case, or is the exclusion an incidental side effect of rational fee-market dynamics?',
    'Examine protocol governance discourse and mining-pool incentive structures. If large hodlers and miners explicitly chose transaction-fee elevation and scarcity-preservation over transaction-capacity expansion, the foreclose is deliberate (snare-flavored extraction). If fee elevation emerged from decentralized incentive competition with no coordination to exclude unbanked users, it is an incidental side effect.',
    'Deliberate foreclose implicates agenda-setter complicity; incidental side effect implicates tragedy-of-commons coordination failure. The constraint''s extraction character and agenda-setter identification depend on this distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unbanked_use_case_abandonment, empirical, 'Whether unbanked-user exclusion was intentional or emergent from fee-market dynamics.').

omega_variable(
    mining_centralization_supply_constraint,
    'Does the proof-of-work security model require continuous centralization of mining power to enforce scarcity constraints, and if so, does this create a structural vulnerability where large miners become de-facto rule-setters?',
    'Analyze mining-pool concentration over time; determine whether solo mining remains economically viable; assess whether large pools can coordinate fee policies or block-size constraints. Examine alternative consensus mechanisms (proof-of-stake, hybrid) and their implications for scarcity enforcement.',
    'If scarcity enforcement requires mining centralization, the constraint''s enforcement depends on continuous threat of miner coordination, making it more snare-like. If scarcity emerges naturally from decentralized incentives, it is more rope-like. The agenda-setter identification depends on whether enforcement is concentrated or distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mining_centralization_supply_constraint, empirical, 'Whether scarcity constraints require centralized mining coordination or emerge from decentralized incentives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t2009, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2009, 0.05).
narrative_ontology:measurement(bitc_tr_t2013, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2013, 0.12).
narrative_ontology:measurement(bitc_tr_t2017, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(bitc_tr_t2021, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(bitc_tr_t2023, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2023, 0.41).
narrative_ontology:measurement(bitc_tr_t2024, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(bitc_be_t2009, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2009, 0.12).
narrative_ontology:measurement(bitc_be_t2013, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2013, 0.28).
narrative_ontology:measurement(bitc_be_t2017, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2017, 0.45).
narrative_ontology:measurement(bitc_be_t2021, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(bitc_be_t2023, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement(bitc_be_t2024, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t2009, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2009, 0.15).
narrative_ontology:measurement(bitc_su_t2013, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2013, 0.28).
narrative_ontology:measurement(bitc_su_t2017, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement(bitc_su_t2021, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2021, 0.48).
narrative_ontology:measurement(bitc_su_t2023, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2023, 0.52).
narrative_ontology:measurement(bitc_su_t2024, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% Bitcoin white-paper kernel (bitcoin_whitepaper) is instantiated by three structurally distinct readings: digital_gold_reading (this constraint) prioritizes scarcity and inflation-hedge value; p2p_cash_reading prioritizes transaction efficiency and financial inclusion; protocol_ossification_reading prioritizes consensus stability over innovation. Each reading carries a different epsilon (extractiveness), beneficiary set, and constraint type. The digital_gold_reading forecloses the p2p_cash_reading (scarcity prioritization makes transaction capacity a sacrificial cost) and coexists_with the protocol_ossification_reading (both defend Bitcoin's current structure, though on different grounds). These are not three perspectives on one constraint — they are three different constraints instantiated from one contested kernel. Each instantiation has its own stakeholders, measurements, and governance dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__digital_gold_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
