% ============================================================================
% CONSTRAINT STORY: bitcoin_whitepaper__digital_gold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Bitcoin as Digital Gold (Whitepaper Reading)
 *   domain: cryptocurrency/monetary_systems/technology_governance
 *
 * SUMMARY:
 *   Bitcoin, under the digital-gold reading, is a scarce digital asset
 *   optimized for store of value and inflation hedging. The reading
 *   instantiates a specific constraint: the 21-million-coin cap and
 *   immutability consensus rule enforce scarcity, which drives appreciation,
 *   which transfers wealth from late entrants to early holders. This is ONE
 *   reading of the Bitcoin whitepaper kernel; sibling readings (p2p-cash,
 *   protocol-ossification) would decompose the constraint differently, naming
 *   different victims and beneficiaries. The digital-gold reading privileges
 *   asset appreciation over transaction utility; the cost of that choice is
 *   reflected in the victim set (late entrants, transaction users, the
 *   economically excluded) and the suppression mechanism (consensus defense
 *   of immutability and the 21-million cap against alternative
 *   interpretations).
 *
 * KEY AGENTS:
 *   - Early adopters and holders: Beneficiary seat. Accumulated bitcoin cheaply; appreciation transfers wealth to them. Exit is open via liquidation at market prices.
 *   - Wealthy capital accumulators: Beneficiary seat. Institutional investors using bitcoin as inflation hedge and store of value. High power, mobile exit — bitcoin is one portfolio item among many.
 *   - Core developers and maintainers: Agenda-setter seat. Enforce the 21-million cap and immutability rule. Identity-locked to the digital-gold reading; exit would mean abandoning the steward role.
 *   - Late entrants: Victim seat. Face escalating entry prices and the appreciation barrier. Trapped: cannot exit without losses; the reading frames bitcoin as the only safe hedge.
 *   - Transaction users: Victim seat. Pushed out by rising transaction fees. Structurally excluded from on-chain use; secondary layers and competing assets are framed as inferior.
 *   - Economically excluded: Victim seat. Cannot afford bitcoin at current prices. Trapped participation barrier: the inflation-hedge narrative markets bitcoin as solving financial exclusion, yet the constraint operation excludes them.
 *   - Alternative asset designers: Excluded seat. Cannot compete with Bitcoin's network effect or the digital-gold narrative dominance. Trapped in subordinate roles.
 *   - Monetary policy authorities: Observer seat. Challenge the reading's founding-problem framing; have analytical standing but cannot reinterpret the reading from within.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bitcoin_whitepaper__digital_gold_reading, 0.68).
domain_priors:suppression_score(bitcoin_whitepaper__digital_gold_reading, 0.42).
domain_priors:theater_ratio(bitcoin_whitepaper__digital_gold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(bitcoin_whitepaper__digital_gold_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bitcoin_whitepaper__digital_gold_reading, tangled_rope).
narrative_ontology:human_readable(bitcoin_whitepaper__digital_gold_reading, "Bitcoin as Digital Gold (Whitepaper Reading)").
narrative_ontology:topic_domain(bitcoin_whitepaper__digital_gold_reading, "cryptocurrency/monetary_systems/technology_governance").

domain_priors:requires_active_enforcement(bitcoin_whitepaper__digital_gold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bitcoin_whitepaper__digital_gold_reading, 'da85b5ee-947b-4fe1-8055-9bf3fd12c7d2').
narrative_ontology:cs_kernel_codification('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', fixed_text).
narrative_ontology:cs_authority_grounding('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', lineage).
narrative_ontology:cs_interpretation_layer_present('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2').
narrative_ontology:cs_reading_relation('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', bitcoin_whitepaper__p2p_cash_reading, forecloses).
narrative_ontology:cs_reading_relation('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', bitcoin_whitepaper__protocol_ossification_reading, coexists_with).
narrative_ontology:cs_axiom('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', foundational, scarcity_enforced_appreciation).
narrative_ontology:cs_axiom_status(scarcity_enforced_appreciation, holdable).
narrative_ontology:cs_axiom_grounding('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', scarcity_enforced_appreciation, empirically_contingent).
narrative_ontology:cs_axiom('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', foundational, immutability_non_negotiable).
narrative_ontology:cs_axiom_status(immutability_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', immutability_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', whitepaper_sound_money_intent).
narrative_ontology:cs_drift_state('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', contemporary_institutional_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da85b5ee-947b-4fe1-8055-9bf3fd12c7d2', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, early_adopters_and_holders).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, wealthy_capital_accumulators).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, late_entrants).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, transaction_users).
narrative_ontology:constraint_victim(bitcoin_whitepaper__digital_gold_reading, economically_excluded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bitcoin_whitepaper__digital_gold_reading, mainstream_adoption_proponents).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, monetary_scarcity_maximalism).
narrative_ontology:constraint_vindicates(bitcoin_whitepaper__digital_gold_reading, inflation_hedge_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accumulated bitcoin at low prices before mainstream adoption. Benefit from appreciation driven by scarcity narrative and protocol-enforced 21-million-coin cap. Their wealth compounds as later entrants compete for fixed supply. Exit options remain open: can liquidate at market prices, hedge via derivatives, or integrate holdings into institutional portfolios.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, early_adopters_and_holders, beneficiary,
    organized, generational, arbitrage, global).

% Large institutional investors (family offices, pension funds, corporations) accumulate bitcoin as a store of value and inflation hedge. The 21-million-coin scarcity constraint ensures their holdings appreciate against both fiat debasement and later entrants' demand. Can reallocate capital freely to competing assets; bitcoin is one portfolio instrument among many.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, wealthy_capital_accumulators, beneficiary,
    powerful, generational, mobile, global).

% Steward the protocol and its immutability commitment. Their authority rests on consensus that the 21-million-coin cap and fixed emission schedule are non-negotiable. They enforce the reading's core premise: appreciation through scarcity is the design intent. Exit would mean abandoning the identity of 'Bitcoin steward' and would require ceding authority to those who would alter the reading.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, core_developers_and_maintainers, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Acquire bitcoin after mainstream adoption when price has risen substantially. Face a two-fold extraction: first, the appreciation barrier (entry price is high because earlier holders benefited from scarcity); second, transaction fees rise as on-chain throughput is capped. Cannot exit to alternative stores of value without realizing losses; the reading's framing makes bitcoin the 'only safe' hedge, trapping later entrants into unfavorable accumulation positions.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, late_entrants, payer,
    powerless, biographical, trapped, global).

% Wish to use bitcoin for payments (its original stated purpose). Face escalating transaction fees as block space becomes scarce and competition for on-chain settlement intensifies. The digital-gold reading deprioritizes transaction throughput in favor of immutability and scarcity, making on-chain use economically unfeasible for small values. Secondary role: structurally excluded from the design's primary optimization (their payments are pushed to secondary layers or alternative assets).
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, transaction_users, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(bitcoin_whitepaper__digital_gold_reading, transaction_users, excluded).

% Cannot afford bitcoin at current prices and cannot accumulate it via transaction use (fees prohibit small payments). The scarcity-based reading creates a participation barrier: bitcoin is marketed as a hedge against financial exclusion, yet the constraint's operation (fixed supply, rising fees) structurally excludes those without existing capital. Their only path to holding bitcoin is through speculative purchase at appreciated prices, replicating the late-entrant trap.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, economically_excluded, payer,
    powerless, biographical, trapped, global).

% Design competing stores of value (alt-L1 blockchains, commodity-backed assets, digital currencies). Excluded by Bitcoin's network-effect dominance and the digital-gold reading's rhetorical primacy. Their alternative designs cannot command the same scarcity-premium or store-of-value narrative. Trapped: their only path to relevance is to accept subordinate roles (layer-2 scaling, niche use cases) or to challenge Bitcoin's reading directly (which requires overturning the consensus the reading has achieved).
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, alternative_asset_designers, excluded,
    powerful, generational, trapped, global).

% Central banks and treasuries observe Bitcoin as a potential challenge to fiat monetary authority. The digital-gold reading explicitly positions bitcoin as an inflation hedge against fiat debasement, directly undermining the authority of monetary policy. Their analytical position: they can observe, regulate at the margin, and attempt to counter via CBDC and capital controls, but cannot change the reading from within (it is not their story to reinterpret).
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, monetary_policy_authorities, observer,
    institutional, civilizational, analytical, national).

% Companies and individuals whose business models depend on Bitcoin adoption as digital gold (hardware wallets, custodians, wealth advisory firms, mining operations). Benefit from the reading's dominance because it drives accumulation demand and legitimizes institutional participation. Their exit is partial: they can shift to other cryptocurrencies or traditional finance, but their current competitive advantage depends on Bitcoin's primacy.
narrative_ontology:constraint_stakeholder(bitcoin_whitepaper__digital_gold_reading, mainstream_adoption_proponents, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bitcoin_whitepaper__digital_gold_reading, early_adopters_and_holders).
narrative_ontology:fixing_cost_class(bitcoin_whitepaper__digital_gold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bitcoin coordinates a scarce, verifiable, censorship-resistant store of value by enforcing a fixed emission schedule (21 million coins) and immutable transaction history on a decentralized ledger. The coordination problem solved: absent such a system, individuals face uncertainty about monetary stability and fiat debasement — Bitcoin provides a technological solution to the coordination problem of agreeing on a supply-capped asset.
% TRANSFER_FUNCTION: Transfers wealth from late entrants and transaction users to early adopters and capital accumulators. The mechanism: scarcity-driven appreciation (early adopters acquire cheap, later entrants buy dear) and transaction-fee concentration (on-chain blockspace becomes valuable to holders; network use is priced out). The reading frames this as justified: the digital-gold holder subsidizes security (via node operation and proof-of-work cost); later participants pay the appreciation premium as the cost of admission to a now-valuable network.
% ABSENT_VOICES: Voices advocating for Bitcoin as a medium of exchange (rather than store of value) are structurally excluded by the reading's design choices — they would argue for higher transaction throughput, lower fees, and faster settlement, but these goals conflict with the immutability and scarcity priorities. Alternative monetary systems and those who depend on monetary policy tools (central banks, development economists) are also absent from the reading's legitimacy conversation, though their interests are directly challenged.
% DISAPPEARANCE_RATIONALE: If the digital-gold reading and its enforcement (the 21-million-cap consensus rule, the immutability commitment, the node-operator coalition defending it) vanished overnight, capital flows would redirect from Bitcoin accumulation to competing stores of value; on-chain transaction throughput could increase (removing the fee constraint); the narrative legitimacy of Bitcoin as inflation hedge would evaporate, triggering potential price collapse; and the wealth transfer mechanism (appreciation) would halt, benefiting late entrants and transaction users at the expense of early holders.
% FOUNDING_PROBLEM: Fiat currency is subject to debasement through monetary expansion; governments and central banks lack enforceable constraints on money supply; individuals and institutions have no neutral, non-political store of value immune to inflation and currency manipulation.
% FOUNDING_PROBLEM_CORROBORATION: Early Bitcoin adopters and developers attest the founding problem remains live and that Bitcoin solves it. Mainstream economists and central banks contest the diagnosis: they argue monetary stability is achievable through fiat policy frameworks and that Bitcoin's scarcity is enforced only by consensus, not by physical law. The digital-gold reading's founding narrative is attested only by its beneficiaries and those ideologically aligned with monetary scarcity maximalism; monetary policy authorities and transaction-use advocates provide external corroboration that they *disagree* with the reading's founding-problem framing, not that they agree the problem exists.
narrative_ontology:disappearance_verdict(bitcoin_whitepaper__digital_gold_reading, world_rearranges).
narrative_ontology:founding_problem_status(bitcoin_whitepaper__digital_gold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bitcoin_whitepaper__digital_gold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.38 to 0.68 over the interval, driven by three mechanisms: (1) appreciation concentrates wealth as entry prices rise and late entrants face higher barriers; (2) transaction fees accumulate as block space becomes valuable; (3) the reading's rhetorical dominance hardens the consensus constraint, making alternatives harder to pursue. Theater ratio rises to 0.28 (moderate): the reading invokes 'sound money' and 'inflation hedge' rhetoric, but actual function shifts toward wealth concentration. Suppression rises to 0.42 (moderate-low): the constraint persists primarily through consensus agreement among holders and developers, not through external coercion, but active defense occurs against alternative readings and fee-reduction proposals (SegWit scaling debates, block-size wars, etc.). Accessibility collapse is 0.71: the scarcity narrative and network effects make alternatives feel distant; late entrants calculate they must hold bitcoin despite high prices. Resistance is 0.54: moderate resistance from transaction-use advocates and economically excluded groups, but insufficient to alter the dominant reading.
 *
 * PERSPECTIVAL GAP:
 *   The digital-gold reading creates a classic wealth-transfer structure masked as coordination: early adopters and wealthy accumulators benefit from scarcity-driven appreciation and transaction-fee revenue; late entrants and transaction users bear the appreciation barrier and fee costs; the economically excluded are trapped in a participation barrier. The core developers sit at the pivot: they have agency (consensus rule enforcement), but their identity is fused to the reading — challenging it would dissolve their role. Monetary authorities observe that the reading directly challenges their legitimacy (Bitcoin positions itself against fiat debasement) but cannot reinterpret the reading from within the Bitcoin community.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declaration maps to directionality as follows: Early adopters and wealthy accumulators (beneficiaries) get low d (high subsidy from appreciation). Late entrants and transaction users (victims) get high d (extraction via fees and appreciation barrier). Economically excluded (victims) get maximum d (full target: cannot afford entry, cannot use on-chain, trapped in the narrative). Core developers sit at moderate d (0.35): they enforce the rules, collect authority and recognition, but their identity locks them to the reading. Alternative asset designers are excluded (their exit options are trapped: they cannot compete with network effects) but are not named in the victim set because their exclusion is structural (network effects) rather than extractive in the constraint's operation. Monetary policy authorities are analytical observers — their directionality is excluded from the effective extraction calculation because they do not participate in the constraint; they face downstream pressure from it (their monetary authority is challenged) but are not primary seats in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The digital-gold reading exhibits a contested mandatrophy boundary. The founding problem (fiat debasement, monetary instability, lack of neutral store of value) is attested by early adopters and developers as live. However, monetary policy authorities and transaction-use advocates attest that the founding problem is substantially solved: inflation is manageable through fiat policy frameworks, central banks have demonstrated ability to maintain stability, and Bitcoin's scarcity is enforced only by consensus (not physical law). The measurement series shows extractiveness rising while the founding-problem status remains contested. This suggests the constraint persists not because the founding problem is live, but because the reading has achieved consensus dominance among Bitcoin community members and capital accumulators. The theater ratio rising (0.08 → 0.28) indicates increasing performative maintenance: the 'sound money' and 'inflation hedge' narratives are invoked even as the actual operation (wealth concentration through appreciation and fees) dominates. The divergence between claimed type (tangled_rope, coordination + extraction) and metrics (rising extractiveness, moderate theater) suggests mandatrophy may be arriving: the coordination function (store-of-value provision) is achieving its purpose (Bitcoin is widely recognized as a store of value), but the extraction mechanism (wealth transfer to early holders) persists and intensifies. If the founding problem is deemed dead or solved, the constraint transitions from justified coordination to zombie constraint (persisting without purpose, maintained by inertia and beneficiary capture).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monetary_scarcity_vs_political_consensus,
    'Is Bitcoin''s 21-million-coin scarcity a physical constraint (like gold''s geological scarcity) or a political consensus constraint (the consensus rule can be changed, just with difficulty)?',
    'Empirical test: observe whether consensus could be achieved to increase the coin cap if major stakeholders (developers, miners, holders) unanimously agreed. If consensus could shift the cap, scarcity is enforced only by social agreement; if consensus cannot shift it despite unanimous intent, scarcity is effectively physical.',
    'If scarcity is purely consensual, the store-of-value promise depends on sustained consensus about immutability — a political rather than physical property. This would reframe Bitcoin as a ''commons with governance risk'' rather than ''digital gold.'' It would support victim claims that late entrants face extraction from early holders'' political power to enforce immutability, not from natural scarcity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_scarcity_vs_political_consensus, empirical, 'Whether Bitcoin''s scarcity is physical or political.').

omega_variable(
    digital_gold_vs_p2p_cash_reading_contest,
    'Is the dominance of the digital-gold reading over the p2p-cash reading the result of genuine functional superiority (Bitcoin really is better as a store of value than as cash) or beneficiary capture by early holders (those who benefit from appreciation dominate narrative and development resources)?',
    'Counterfactual historical analysis: estimate whether a 2-MB or larger block size would have prevented price appreciation and institutional adoption. Observe jurisdictions where alternative readings dominate (e.g., BCH/BSV communities): do they achieve functional p2p-cash use at meaningful scale? Natural experiment from hard forks: BSV attempted larger blocks; observe whether it achieved store-of-value adoption comparable to Bitcoin.',
    'If the digital-gold reading dominates due to beneficiary capture rather than functional superiority, the constraint''s claim (coordination + extraction) becomes harder to defend: the coordination might be achievable through p2p-cash design, and the extraction (wealth transfer to early holders) is the primary function. This would push classification toward snare (pure extraction, coordination cover) or increase the victim set''s confidence in mandatrophy framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_gold_vs_p2p_cash_reading_contest, empirical, 'Whether digital-gold dominance reflects functional fit or beneficiary capture.').

omega_variable(
    inflation_hedge_empirical_claim,
    'Does Bitcoin actually function as an inflation hedge in practice, or does its price track speculative demand and risk appetite more closely than inflation expectations?',
    'Econometric analysis: regress Bitcoin price changes against inflation surprise, inflation expectations, and risk-on/risk-off indices. Compare beta to gold (accepted inflation hedge) and real assets. Observe periods of high inflation (2021–2023): did Bitcoin price track inflation or speculative positioning?',
    'If Bitcoin''s price tracks speculation more than inflation, the founding-problem narrative (fiat debasement risk) becomes contested empirically. The digital-gold reading is vindicated by the monetary-scarcity-maximalism axiom, not by empirical hedge function. Late entrants would have stronger grounds to claim extraction: they paid prices driven by speculation, not by inflation-protection value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_hedge_empirical_claim, empirical, 'Whether Bitcoin empirically functions as an inflation hedge.').

omega_variable(
    scarcity_narrative_vs_network_effect,
    'Is the appreciation mechanism driven by the reading''s scarcity narrative (people believe Bitcoin is scarce, so they accumulate, driving price up) or by network effects (Bitcoin is valuable because everyone uses it, so everyone wants to hold it)?',
    'Behavioral economics: survey Bitcoin holders about their reasons for accumulation; separate ''scarcity store-of-value'' motivations from ''network adoption'' motivations. Observe price dynamics relative to adoption milestones (institutional entry, regulatory clarity, payment integrations): does price respond more to scarcity-narrative events (halving, supply-cut proposals) or adoption events (corporate treasury purchases, payment integrations)?',
    'If network effects (not scarcity narrative) drive appreciation, the digital-gold reading is post-hoc rationalization of a network-based value accumulation. This would shift the constraint''s extraction analysis: the mechanism is not ''scarcity enforces appreciation'' but ''network adoption raises value.'' The victim set might shift (later adopters still pay high prices, but through network lock-in, not scarcity). The reading''s foundation would weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_narrative_vs_network_effect, empirical, 'Whether appreciation is driven by scarcity narrative or network effects.').

omega_variable(
    reading_kernel_relationship_ambiguity,
    'Is the digital-gold reading a faithful interpretation of Satoshi Nakamoto''s whitepaper intent, or a post-hoc reading imposed by later community factions?',
    'Textual and historical analysis: examine the whitepaper''s emphasis on ''peer-to-peer electronic cash'' vs. ''store of value''; observe Satoshi''s early statements and forum posts; compare to the Block Wars era (2015–2017) when digital-gold advocates used ''immutability'' and ''scarcity'' language to defend small blocks. Did Satoshi emphasize scarcity or transaction utility?',
    'If digital-gold is post-hoc (not original-intent), the reading''s legitimacy rests on consensus-achieved dominance, not on fidelity to the kernel. This weakens the appeal to ''this is what Bitcoin is'' and strengthens the interpretation-as-choice framing. The constraint becomes more explicitly political: a coalition (early holders, developers, miners) enforced a reading against competing readings. This supports mandatrophy analysis and victim claims of enforced extraction under guise of ''following the protocol.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_relationship_ambiguity, conceptual, 'Whether digital-gold reading is original-intent or post-hoc interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bitcoin_whitepaper__digital_gold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bitc_tr_t0, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(bitc_tr_t0, projected).
narrative_ontology:measurement(bitc_tr_t3, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement_basis(bitc_tr_t3, observed).
narrative_ontology:measurement(bitc_tr_t6, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(bitc_tr_t6, observed).
narrative_ontology:measurement(bitc_tr_t10, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(bitc_tr_t10, observed).
narrative_ontology:measurement(bitc_tr_t15, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(bitc_tr_t15, observed).
narrative_ontology:measurement(bitc_tr_t20, bitcoin_whitepaper__digital_gold_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(bitc_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(bitc_be_t0, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(bitc_be_t0, projected).
narrative_ontology:measurement(bitc_be_t3, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 3, 0.47).
narrative_ontology:measurement_basis(bitc_be_t3, observed).
narrative_ontology:measurement(bitc_be_t6, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(bitc_be_t6, observed).
narrative_ontology:measurement(bitc_be_t10, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(bitc_be_t10, observed).
narrative_ontology:measurement(bitc_be_t15, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(bitc_be_t15, observed).
narrative_ontology:measurement(bitc_be_t20, bitcoin_whitepaper__digital_gold_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(bitc_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(bitc_su_t0, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(bitc_su_t0, projected).
narrative_ontology:measurement(bitc_su_t3, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 3, 0.3).
narrative_ontology:measurement_basis(bitc_su_t3, observed).
narrative_ontology:measurement(bitc_su_t6, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(bitc_su_t6, observed).
narrative_ontology:measurement(bitc_su_t10, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(bitc_su_t10, observed).
narrative_ontology:measurement(bitc_su_t15, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement_basis(bitc_su_t15, observed).
narrative_ontology:measurement(bitc_su_t20, bitcoin_whitepaper__digital_gold_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(bitc_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bitcoin_whitepaper__digital_gold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(bitcoin_whitepaper__digital_gold_reading, 0.18).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__p2p_cash_reading).
narrative_ontology:affects_constraint(bitcoin_whitepaper__digital_gold_reading, bitcoin_whitepaper__protocol_ossification_reading).

% DUAL FORMULATION NOTE:
% Bitcoin whitepaper kernel constraint family (3 readings): digital_gold_reading (this story) positions Bitcoin as scarcity-enforced store of value, naming early holders as beneficiaries and late entrants as victims. p2p_cash_reading positions Bitcoin as censorship-resistant medium of exchange, naming transaction users as beneficiaries and high-fee victims. protocol_ossification_reading positions immutability as the design's primary virtue, naming protocol stewards as beneficiaries. Each reading instantiates a different constraint with different ε, different victim/beneficiary structure, different classification. They are linked by network.affects_constraints because protocol design choices that enforce one reading's assumptions directly constrain the others' feasibility. The digital-gold reading forecloses sustained p2p-cash function by constraining block size; the p2p-cash reading would undermine the scarcity narrative by enabling high-throughput, low-fee transactions; the ossification reading forecloses innovation in either direction by privileging immutability above functional goals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bitcoin_whitepaper__digital_gold_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
