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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Bitcoin as Digital Gold: Scarcity-First Asset Constraint
 *   domain: cryptocurrency/monetary-systems/technology-governance
 *
 * SUMMARY:
 *   Bitcoin's whitepaper describes a peer-to-peer electronic cash system;
 *   this constraint captures one reading of what that system has become: a
 *   scarce digital asset optimized for store of value and inflation hedging.
 *   The digital gold reading prioritizes Bitcoin's monetary properties (fixed
 *   supply, predictable issuance, immutability) over its cash properties
 *   (speed, cost, accessibility for small transactions). The constraint
 *   embeds asymmetry: early adopters benefit from appreciation and
 *   institutional adoption; late entrants face high acquisition cost and
 *   transaction fees; payment-use cases are deprioritized through protocol
 *   design choices that ratify scarcity. The claim/metric gap is intentional:
 *   the constraint is CLAIMED as tangled_rope (coordinating a trust-worthy
 *   ledger while extracting value through scarcity) while the authored
 *   metrics describe substantial extraction with moderate suppression—the
 *   engine measures this gap rather than resolving it.
 *
 * KEY AGENTS:
 *   - early_adopters_and_hodlers: benefit from appreciation and protocol stability (power=organized, exit=arbitrage)
 *   - late_entrant_users: face high acquisition and transaction costs (power=powerless, exit=constrained)
 *   - core_developers: set protocol priorities through consensus governance (power=institutional, exit=analytical)
 *   - bitcoin_maximalists_and_ideologues: defend scarcity narrative and identity-lock to digital gold reading (power=organized, exit=identity_locked)
 *   - transaction_fee_payers: bear costs of artificial block space scarcity (power=moderate, exit=constrained)
 *   - institutional_investors: benefit from scarcity hedge narrative (power=powerful, exit=arbitrage)
 *   - payment_network_operators: deprioritized by digital gold governance (power=organized, exit=trapped, partially)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.52).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold: Scarcity-First Asset Constraint").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency/monetary-systems/technology-governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, '0fd2fd4a-2956-4891-9991-fc0b4b4d992f').
narrative_ontology:cs_kernel_codification('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', fixed_text).
narrative_ontology:cs_authority_grounding('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', extraction).
narrative_ontology:cs_interpretation_layer_present('0fd2fd4a-2956-4891-9991-fc0b4b4d992f').
narrative_ontology:cs_reading_relation('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', bitcoin_whitepaper__p2p_cash_reading, influences).
narrative_ontology:cs_reading_relation('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', foundational, scarcity_is_primary_virtue).
narrative_ontology:cs_axiom_status(scarcity_is_primary_virtue, holdable).
narrative_ontology:cs_axiom_grounding('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', scarcity_is_primary_virtue, deontological).
narrative_ontology:cs_axiom('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', secondary, institutional_adoption_validates_design).
narrative_ontology:cs_axiom_status(institutional_adoption_validates_design, holdable).
narrative_ontology:cs_axiom_grounding('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', institutional_adoption_validates_design, instrumental).
narrative_ontology:cs_reference_frame('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', censorship_resistant_electronic_cash).
narrative_ontology:cs_drift_state('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', institutional_adoption_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0fd2fd4a-2956-4891-9991-fc0b4b4d992f', '').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters_and_hodlers).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, protocol_stability_advocates).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrant_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, transaction_fee_payers).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, rejected_transaction_volume).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, bitcoin_maximalists_and_ideologues).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, institutional_investors).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, price_volatility_absorbers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulated Bitcoin in the first 5-8 years when computational difficulty was low and price was negligible (<$1-$100). They benefit from appreciation as institutional adoption accelerated and the digital gold narrative hardened. Their holdings represent 1-2 orders of magnitude gain. They have maximum exit optionality: can sell at any price point, hold across multiple assets, diversify into alternatives. The constraint's persistence is optional for them; they can exit profitably at any time.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopters_and_hodlers, beneficiary,
    organized, generational, arbitrage, global).

% Individuals and small institutions attempting to acquire or use Bitcoin after 2017, when the asset reached mainstream awareness but price had appreciated 10,000x from its origin. Entry cost is now substantial ($40,000-$60,000 per coin as of 2024). They face multiple cost barriers: acquisition price is high, transaction fees are high (median fee $2-5 per transaction, unacceptable for small payments), and confirmation time is slow (10 minutes average, plus network congestion delays). The digital gold narrative frames these costs as reflective of genuine scarcity and security; late entrants internalize this framing or migrate to alternatives (other cryptocurrencies, fiat savings). Their exit options are constrained by regulatory uncertainty, technical learning curve, and liquidity depth of alternatives.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrant_users, payer,
    powerless, biographical, constrained, global).

% Users who need Bitcoin for settlement or transfers (merchants converting to fiat, institutional movers between wallets, traders executing strategies). Block space is artificially scarce (1 MB blocks, 10-minute intervals create ~7 transactions per second maximum). Fee markets allocate this scarce space; high-value transactions subsidize the network while low-value transactions become economically unviable. A merchant processing $100 in Bitcoin faces a $10-20 fee (10-20% of transaction value); a retail transaction becomes uneconomical. Exit options: use second-layer networks (Lightning, accepting custodial risk and protocol dependency), convert to stablecoins (accepting counterparty risk), or use alternative payment systems (fiat, credit cards, other cryptocurrencies).
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, transaction_fee_payers, payer,
    moderate, biographical, constrained, global).

% Economic transactions that never reach the blockchain because block space rationing makes inclusion uneconomical or impossible. These are coordinated exchanges (person A wants to send to person B) that would occur without the constraint but are suppressed by the protocol. The digital gold reading treats this suppression as necessary: limited throughput is the cost of decentralization. The suppressed transactions represent coordination value (peer-to-peer settlement without intermediaries) that the constraint deliberately sacrifices.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, transaction_rejection_queue, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(bitcoin_whitepaper__digital_gold_reading, transaction_rejection_queue).

% Institutional investors, hedge funds, and conservative developers who benefit from Bitcoin's stability and brand consistency. They view proposed changes (larger blocks, smart contracts, privacy features) as protocol corruption that threatens the 'digital gold' positioning. Their benefit is legitimacy and institutional adoption anchored on the immutability and scarcity narrative. Exit: shift institutional adoption to alternative assets (Ethereum, commodities, fiat); their analytical exit means they evaluate Bitcoin against alternatives and can recommend others without personal cost.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, protocol_stability_advocates, beneficiary,
    institutional, civilizational, analytical, global).

% Coordinate protocol development through distributed consensus forums (Bitcoin Core GitHub, mailing lists, Bitcoin Improvement Proposals). The digital gold reading shapes what 'consensus' means: changes that enhance store-of-value (security hardening, privacy, fee efficiency through better transaction formats) find support; changes that increase throughput or enable new use cases face resistance framed as protocol bloat and centralization risk. Their authority derives from perceived technical competence and the social consensus around Bitcoin's purpose. Exit: write alternative code (fork, sidechain, or entirely new project); but their voice and influence are tied to Bitcoin's governance, creating semi-sticky investment in the current reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, core_developers, agenda_setter,
    institutional, civilizational, analytical, global).

% Community members whose professional identity, ideological commitment, and social standing fuse with Bitcoin as digital gold. Their identity encompasses the philosophical claim that Bitcoin embodies sound money independent of state control and that any deviation from the original design is corruption. Accepting alternative readings (payment network, protocol flexibility) would require dismantling their worldview, reputational risk, and loss of community standing. They shape community discourse, media narratives, and developer priorities through intellectual leadership and gatekeeping. Exit: ideological defection (publicly renouncing maximalism creates reputational cost and professional isolation within the community).
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, bitcoin_maximalists_and_ideologues, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, bitcoin_maximalists_and_ideologues, agenda_setter).

% Pension funds, corporate treasuries, sovereign wealth funds, and hedge funds that adopted Bitcoin as a portfolio diversifier and inflation hedge. They benefit from the scarcity narrative, which justifies holding an asset with limited functional utility and extreme volatility. High entry price ($40k-$60k per coin) is acceptable for entities with large capital reserves; they have capital diversification options. Exit: divest from Bitcoin and reallocate to alternatives (commodities, other cryptocurrencies, fiat instruments); their financial constraints allow unconstrained exit.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, institutional_investors, beneficiary,
    powerful, generational, arbitrage, global).

% Layer-2 and alternative-layer operators (Lightning Network, Stacks, Liquid) building scaling solutions. They are not contractually excluded but are structurally deprioritized: development resources and governance attention flow toward mainchain security and store-of-value properties; their requests for mainchain features (larger blocks, faster confirmation, script extensions to enable smart contracts on L2) are evaluated through the digital gold lens and often rejected. Full exclusion from Bitcoin development is contested; partial deprioritization and resource starvation is structural. Exit: build on alternative blockchains (Ethereum, Solana) with more permissive feature sets; but their investment in Bitcoin infrastructure becomes sunk cost if they exit.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, payment_network_operators, excluded,
    organized, generational, trapped, global).

% Merchants and ordinary users who want to use Bitcoin for actual transactions (wage payment, retail sales, service invoicing) face price volatility that makes settlement value unpredictable. A merchant quoting a price in Bitcoin faces price drift of 5-15% intraday; they must hedge through conversion to fiat or stablecoins or absorb variance. The digital gold narrative treats volatility as a byproduct of genuine scarcity and market discovery, not a design flaw; remedies (settlement guarantees, stablecoin integration, faster confirmation) are discouraged as centralization risks. Their costs are structural friction in the constraint's operation, not a problem for the asset reading to solve.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, price_volatility_absorbers, payer,
    moderate, biographical, mobile, global).

% Economists, technologists, policy analysts, and academic researchers studying Bitcoin's effects on monetary systems, financial inclusion, energy consumption, and wealth distribution. They observe the constraint's operation across multiple readings and measure divergence between the digital gold framing and actual usage patterns. They lack direct power to alter the constraint but influence institutional perception, regulatory response, and academic consensus. Their observations feed into risk assessments and policy recommendations that indirectly shape the constraint's environment.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, academic_and_policy_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_adopters_and_hodlers).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a globally accessible, permissionless ledger of ownership claims backed by cryptographic proof-of-work, solving the double-spend problem without a central authority. The scarcity constraint (capped supply, proof-of-work difficulty) enables coordination around a durable store of value with known monetary policy.
% TRANSFER_FUNCTION: Transfers value accumulation from late-entrant users and transaction-volume participants to early adopters and holders. The scarcity narrative justifies fees and throughput constraints by reframing them as features (rarity, security) rather than design limitations. Layer-1 block space is allocated by fee market, not use case optimization.
% ABSENT_VOICES: Users who need Bitcoin primarily as a medium of exchange (merchants, remittance corridors, low-income populations in high-inflation regions) are structurally excluded from mainchain utility by fees and throughput constraints. They have economic interest in the p2p_cash_reading but are not seated in governance conversations dominated by holders and protocol conservators. Second-layer and alternative-layer scaling advocates are present but deprioritized.
% DISAPPEARANCE_RATIONALE: If the digital gold constraint—the scarcity-first design and protocol stability doctrine—were abandoned in favor of the p2p_cash reading (increased throughput, lower fees, feature enhancements), Bitcoin's market positioning would shift. Institutional adoption predicated on 'digital gold' narrative might retreat; payment-layer activity would accelerate. A subset of value that accumulated under the expectation of scarcity maintenance would disperse. The protocol would reorganize around different priorities (transaction finality speed, smart contract capability, supply elasticity). The constraint's disappearance triggers institutional and technical reconfiguration.
% FOUNDING_PROBLEM: Centralized payment networks and fiat monetary policy subject money to authority control and censorship. A decentralized, scarce, authority-free ledger of value enables independent store of value, protection from inflation, and resistance to financial censorship.
% FOUNDING_PROBLEM_CORROBORATION: Early adopters and protocol conservatives (Satoshi Nakamoto whitepaper, core developers, Bitcoin Foundation statements) attest the problem remains live: central bank monetary expansion, financial surveillance, and geopolitical control of payment systems justify scarcity and decentralization. Competing readings dispute the framing: payment-use advocates argue the founding problem was peer-to-peer cash without intermediaries, not necessarily asset scarcity; economists and policy observers attest Bitcoin's actual use case has shifted dramatically from the 2009 problem statement to 2024 institutional asset hedging, and the original problem (censorship of small transactions) is unresolved while new problems (price volatility, energy consumption, wealth concentration) have emerged. No neutral party outside the benefiting constituency confirms the founding problem is still the constraint's primary function.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bitcoin_whitepaper__digital_gold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(bitcoin_whitepaper__digital_gold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the protocol's design choices (1 MB block limit, 10-minute block time, capped supply) create artificial scarcity that benefits early holders while suppressing transaction volume and late-entrant access. The constraint's initial extractiveness (0.45) is lower because early Bitcoin had low institutional adoption and price volatility was extreme; as institutional investors adopted the asset and the digital gold narrative solidified (t=4-14), extractiveness increased as the late-entrant victim set enlarged and fees rose. Suppression is moderate (0.52) because the constraint relies on protocol consensus and narrative persuasion rather than explicit coercion—users can exit to alternatives, but the network effects and accumulated institutional positioning make exit costly. Theater rises from 0.15 to 0.28 as the claim of immutability and decentralization becomes increasingly theatrical: the protocol exhibits real decentralization properties, but the governance discourse around 'what Bitcoin is for' concentrates around the holder/maximalist constituencies and marginalizes payment-use arguments. Theater stabilizes at 0.28 as the digital gold narrative becomes institutionally entrenched (institutional investor adoption, corporate treasury holdings, regulatory recognition of Bitcoin as commodity rather than currency) and the theatrical gap between 'decentralized sound money' claims and actual governance concentration becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (late entrants, transaction-volume users) and the beneficiary/agenda-setter seats (early adopters, core developers, maximalists) should compute differently under per-seat classification. From the payer position, the constraint is enforced extraction: high fees, throughput limits, and appreciation scarcity are structural barriers maintained through protocol design and governance control, with no offsetting coordination benefit—the 'ledger of value' was solved in 2009; the constraint persists for wealth concentration. From the beneficiary/developer position, the constraint is genuine tangled coordination: scarcity is essential to the ledger's trust model, throughput limits preserve decentralization (larger blocks require more computational resources, centralizing mining and validation), and the protocol must remain stable to preserve its properties. The engine computes this divergence: the same structure (fixed supply, block-size limit) is experienced as coordination overhead by developers and as extractive scarcity by late entrants. The authored structural data (beneficiary/victim declarations, exit modulation) feeds directionality; the engine produces the per-seat type.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and holders are structural beneficiaries (d near 0.0-0.2): they benefit from appreciation and have maximum exit optionality (arbitrage-grade: can sell at any price, buy alternatives). Late entrants and transaction-volume users are structural targets (d near 0.8-1.0): they face high entry costs, constrained exit (network effects and regulatory uncertainty make migration costly), and bear the costs of the fee market and throughput constraints. Core developers sit asymmetrically (d near 0.5): they coordinate the protocol and benefit from its stability, but they also face responsibility for consensus and pressure from divergent constituencies; their exit is analytical (they could write alternative code, but their authority derives from Bitcoin's governance). Institutional investors are strong beneficiaries (d near 0.1-0.3): they have arbitrage exit and benefit from appreciation narrative. Bitcoin maximalists are identity-locked beneficiaries (d near 0.3-0.5): they benefit from the scarcity narrative but their identity is fused with the constraint, making exit costly despite high directionality toward the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (censorship-resistant value storage independent of state control) and the constraint's current function (institutional asset hedging and wealth concentration) show mandatrophy drift. The founding_problem_status is 'contested': early Bitcoin addressed peer-to-peer transactions for censorship resistance (remittances, cross-border payments without banking intermediaries); institutional Bitcoin addresses portfolio diversification for entities with no censorship risk (pension funds, corporations). The two problems have different victim sets. The tangled_rope classification captures this: genuine coordination (a ledger of value with known supply) rides alongside substantial extraction (late entrants paying for early adopters' appreciation, transaction volume sacrificed to preserve decentralization properties the institutional investors don't need). Mandatrophy would be triggered if the founding problem (censorship resistance for low-value transactions) is dead but the constraint persists for institutional hedging (a different problem not named in the founding). The founding_problem_corroboration reveals no external attestation—only the benefiting parties (maximalists, early adopters) claim the original problem is still live; competing parties attest it has shifted. This split suggests mandatrophy is material, but the determination requires the engine's mismatch consumer to cross-check (founding_problem_status=contested x disappearance_verdict=world_rearranges) against theater ratio and computed piton evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    design_necessity_vs_structural_choice,
    'Are the 1 MB block size and 10-minute block interval structurally necessary to preserve Bitcoin''s decentralization properties, or are they engineering choices that could be relaxed without sacrificing decentralization?',
    'Empirical testing on sidechains and alternative implementations; economic analysis of validator hardware requirements and geographic distribution under various throughput levels; historical analysis of protocol governance decisions.',
    'If the limits are engineering choices, the suppression of transaction volume is extractive (serving the interests of holders and miners, not decentralization requirements). If they are design necessities, the constraint is genuine tangled coordination with real decentralization costs. This determines whether the victim set (late_entrant_users, transaction_fee_payers) are genuinely necessary costs or artificial extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_necessity_vs_structural_choice, empirical, 'Whether throughput and block-size constraints are decentralization-necessary or value-concentrating design choices.').

omega_variable(
    institutional_adoption_foundational_or_contingent,
    'Is Bitcoin''s institutional adoption (pension funds, corporate treasuries, spot ETFs) a natural result of the digital gold narrative, or a contingent outcome that could be otherwise with different protocol priorities?',
    'Counterfactual scenario analysis: if the p2p_cash reading had dominated governance (8 MB blocks, 1-minute confirmation, layer-1 smart contracts), would institutional adoption have followed the same trajectory or shifted to alternative assets? Market surveys of institutional adoption drivers.',
    'If institutional adoption follows the digital gold narrative necessarily, the beneficiary set (early_adopters_and_hodlers, institutional_investors) is structural. If contingent, the constraint''s beneficiary structure was reinforced by governance choices that deprioritized alternatives, and the extraction is more deliberately enforced than the digital gold reading admits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_adoption_foundational_or_contingent, conceptual, 'Whether digital gold framing enabled or merely capitalized on institutional adoption demand.').

omega_variable(
    price_discovery_legitimacy,
    'Is the 15-year price appreciation trajectory of Bitcoin a legitimate discovery of its intrinsic scarcity value, or does it reflect information asymmetry and wealth concentration that extracted value from late entrants?',
    'Economic analysis of price elasticity and adoption curves; comparison to other scarce digital assets (Ethereum supply cap, other PoW coins); survey data on late-entrant perception of fairness and exit barriers.',
    'If legitimate discovery, late entrants chose to enter at high price with knowledge of the constraint; extraction is minimal. If information asymmetry dominates, late entrants faced artificially constrained information about alternatives and alternatives'' viability, and the extraction is substantial. This affects whether victims are genuine market participants or trapped participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_discovery_legitimacy, preference, 'Whether price appreciation reflects intrinsic value discovery or extractive information asymmetry.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can the digital_gold_reading and the p2p_cash_reading coexist as simultaneous instantiations of the whitepaper kernel, or does the digital gold reading''s dominance in protocol governance functionally foreclose the p2p_cash reading?',
    'Monitor whether layer-2 scaling solutions (Lightning, Stacks) achieve payment-use viability and institutional adoption independent of mainchain priorities. Track whether protocol upgrade proposals supporting cash-use cases gain governance traction or remain marginalized.',
    'Coexistence would place the readings as sibling constraints competing for governance resources. Foreclosure would mean the digital gold reading has achieved structural dominance and the p2p_cash reading persists only in marginal implementations. This determines the reading_relations field: coexists_with or influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, empirical, 'Whether the two readings remain live options or whether digital gold has functionally dominated.').

omega_variable(
    maximalist_identity_lock_structural,
    'Is the identity-lock of Bitcoin maximalists (fusion of professional identity, ideological commitment, and community standing with the digital gold reading) a structural feature of the network, or a contingent social arrangement that could dissolve if core development priorities shifted?',
    'Historical comparison to other technology communities where ideological identity shifts (e.g., GNU/Linux licensing debates); survey of maximalists'' stated exit costs and commitment irreversibility; analysis of professional incentives for developers and community leaders.',
    'If structural, the identity-lock amplifies the digital gold reading''s durability and makes alternative readings harder to instantiate—the social cost of switching is prohibitive. If contingent, the constraint could be redesigned if governance priorities shifted and identity investments reallocated. This affects the suppression metric and the agenda-setter power of maximalists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximalist_identity_lock_structural, empirical, 'Whether maximalist identity-lock is structural or contingent to current governance state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bitc_tr_t2, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(bitc_tr_t4, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(bitc_tr_t8, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(bitc_tr_t14, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bitc_be_t2, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(bitc_be_t4, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(bitc_be_t8, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(bitc_be_t14, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 14, 0.68).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bitc_su_t2, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 2, 0.4).
narrative_ontology:measurement(bitc_su_t4, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(bitc_su_t8, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(bitc_su_t14, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% The bitcoin_whitepaper kernel decomposes into three constraint stories: digital_gold_reading (this file) prioritizes scarcity and value storage; p2p_cash_reading prioritizes transaction speed and censorship-resistant payments; protocol_ossification_reading prioritizes immutability and decentralization as structural properties. Each reading instantiates a different constraint with different ε values (extraction levels), different victim sets, and different type classifications. The digital_gold_reading authors ε=0.68 (substantial extraction through appreciation scarcity and fee suppression); the p2p_cash_reading would author lower ε (the same design is coordinating cash without intermediaries, a genuine public good); the protocol_ossification_reading treats stability as the primary virtue and would author different suppression/theater values. The three are structurally related: digital gold constrains p2p cash by deprioritizing throughput; protocol ossification constrains both by treating protocol changes as delegitimacy; p2p cash advocates would enable protocol evolution that digital gold constrains. Network edges model these relationships: digital_gold affects (influences and partially forecloses) the p2p_cash_reading; both are influenced by protocol_ossification doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__digital_gold_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
