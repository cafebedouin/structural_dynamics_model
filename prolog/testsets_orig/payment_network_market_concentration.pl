% ============================================================================
% CONSTRAINT STORY: payment_network_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_payment_network_market_concentration, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: payment_network_market_concentration
 *   human_readable: Payment Network Market Concentration
 *   domain: economic/infrastructure/financial_systems
 *
 * SUMMARY:
 *   Payment network market concentration represents a structural constraint
 *   on global commerce where two networks (Visa and Mastercard) control
 *   approximately 83% of card payment volume worldwide. The constraint
 *   exhibits genuine coordination function (solving merchant-customer
 *   matching, managing fraud, maintaining settlement reliability) alongside
 *   asymmetric extraction through interchange fees, mandatory participation
 *   rules, and network switching costs. The same structural phenomenon
 *   appears as essential infrastructure coordination (Rope perspective from
 *   beneficiaries), intolerable extraction (Snare perspective from powerless
 *   merchants and unbanked populations), temporary problem with decentralized
 *   solutions (Scaffold perspective from emerging alternative
 *   infrastructure), mixed coordination and gatekeeping (Tangled Rope from
 *   constrained competitors and organized merchant coalitions), and degraded
 *   regulatory theater (Piton perspective from oversight bodies). The
 *   constraint's history shows extractiveness rising over 20 years (from 0.35
 *   to 0.58) as payment processing has become more essential and switching
 *   costs have hardened, while theater ratio (regulatory oversight) remains
 *   modest (0.48), indicating that regulation has not substantially reduced
 *   underlying extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Visa and Mastercard: Primary beneficiaries (institutional/arbitrage) — capture dominant market share, set interchange fee rules, control interoperability standards. Net extraction flow toward these agents.
 *   - Underbanked Merchants: Primary victims (powerless/trapped) — absorb 2-4% transaction fees with no negotiating power. Forced into payment network participation for economic survival.
 *   - Unbanked Populations: Primary victims (powerless/trapped) — structurally excluded from formal payment networks; cannot access credit, build financial history, or participate in digital commerce.
 *   - Competing Payment Providers: Secondary victims (powerful/constrained) — confined to niche markets; limited interoperability, high switching costs, face gatekeeping by incumbent networks.
 *   - Merchant Associations: Organized secondary victims (organized/constrained) — negotiate collectively but remain structurally dependent on incumbent networks. Some agency but exit remains economically unfeasible.
 *   - Digital Currency and Open Infrastructure Movements: Alternative pathway providers (organized/constrained) — blockchain, distributed ledgers, CBDCs represent technical routes around incumbent concentration. Currently niche but scaling.
 *   - Financial Regulators: Institutional oversight (institutional/arbitrage) — maintain interchange fee caps and interoperability mandates but see regulation as increasingly degraded. Theater ratio reflects performative oversight.
 *   - Central Banks and Treasury Departments: Policy actors (institutional/arbitrage) — CBDC initiatives and regulatory frameworks being developed as long-term alternatives to incumbent networks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(payment_network_market_concentration, 0.58).
domain_priors:suppression_score(payment_network_market_concentration, 0.65).
domain_priors:theater_ratio(payment_network_market_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(payment_network_market_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(payment_network_market_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(payment_network_market_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(payment_network_market_concentration, tangled_rope).
narrative_ontology:human_readable(payment_network_market_concentration, "Payment Network Market Concentration").
narrative_ontology:topic_domain(payment_network_market_concentration, "economic/infrastructure/financial_systems").

domain_priors:requires_active_enforcement(payment_network_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(payment_network_market_concentration, incumbent_payment_networks).
narrative_ontology:constraint_beneficiary(payment_network_market_concentration, financial_system_stability).
narrative_ontology:constraint_victim(payment_network_market_concentration, competing_payment_providers).
narrative_ontology:constraint_victim(payment_network_market_concentration, merchants).
narrative_ontology:constraint_victim(payment_network_market_concentration, underbanked_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERBANKED MERCHANT (SNARE) — Trapped in payment network dependency with no viable alternatives. High transaction fees (2-4% for card payments), forced acceptance of major networks, no ability to negotiate terms. Bears full extraction cost with zero exit capacity. Network effects make refusing Visa/Mastercard economically unviable.
constraint_indexing:constraint_classification(payment_network_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNBANKED POPULATION (SNARE) — Structurally excluded from formal payment networks. Cannot build credit history, access digital payments, or participate in mainstream commerce without network access. Trapped by infrastructure concentration and verification requirements. Maximum extraction through exclusion.
constraint_indexing:constraint_classification(payment_network_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPETING PAYMENT PROVIDER (TANGLED ROPE) — Constrained by network effects and interoperability barriers. Benefits from standardized payment infrastructure that enables any provider to participate; also bears extraction through gatekeeping fees and switching costs imposed by incumbents. Some agency through regulatory arbitrage and niche markets, but exit from network participation is not viable.
constraint_indexing:constraint_classification(payment_network_market_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: VISA/MASTERCARD NETWORK OPERATORS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: connecting merchants, cardholders, and banks creates genuine mutual benefit and solves the double-coincidence problem. Net beneficiary of concentration — market power enables coordination subsidies and infrastructure investment. Exit options via arbitrage (currency switching, regulatory jurisdiction selection).
constraint_indexing:constraint_classification(payment_network_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MERCHANT ASSOCIATIONS (TANGLED ROPE) — Organized groups with some negotiating power but structurally trapped. Benefits from standardized, interoperable payment infrastructure; extraction occurs through mandatory acceptance rules and rising interchange fees. Coalition organization creates agency for negotiation but exit from network participation remains economically unfeasible for members.
constraint_indexing:constraint_classification(payment_network_market_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN INFRASTRUCTURE MOVEMENTS (SCAFFOLD) — Distributed ledger technologies (Bitcoin, Ethereum), CBDC initiatives, and open payment standards (ISO 20022) represent alternatives to incumbent networks. Currently lower market share but rising. Sees traditional payment concentration as temporary coordination problem being solved by decentralized protocols. Sunset mechanism: as distributed alternatives mature and regulatory frameworks clarify, incumbent network dominance declines. Estimated sunset: 15-30 years depending on regulatory adoption.
constraint_indexing:constraint_classification(payment_network_market_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY OVERSIGHT (PITON) — Traditional regulation treats payment networks as natural monopolies requiring rate oversight, but regulation itself has become largely performative. Regulatory caps on interchange fees exist in many jurisdictions (EU, India) yet market concentration persists and extraction continues through alternative mechanisms (network fees, member assessments). Theater ratio high because regulation appears to constrain networks while actual extraction continues. Regulators maintain oversight apparatus but see it as degraded — concentration persists despite regulatory intervention.
constraint_indexing:constraint_classification(payment_network_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing contingent arrangements: 'Payment networks are naturally monopolistic due to network effects.' This framing treats market concentration as an inherent property of payment infrastructure. However, structural data reveals this as potential false summit — the concentration is maintained through switching costs, interoperability barriers, and regulatory capture, not as immutable law.
constraint_indexing:constraint_classification(payment_network_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(payment_network_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(payment_network_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(payment_network_market_concentration, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(payment_network_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(payment_network_market_concentration, TR),
    TR >= 0.70.

:- end_tests(payment_network_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts substantial rents through interchange fees (averaging 1.5-2.5% of transaction value in developed markets, higher in developing markets), mandatory network participation, and switching costs. Extraction is not as severe as pure Snare would suggest because genuine coordination benefits exist (fraud prevention, settlement reliability, merchant-customer matching). The trajectory (0.35→0.58 over 20 years) reflects increasing reliance on digital payments and hardening of switching costs as payment networks become infrastructure. Suppression (0.65): High. Multiple barriers prevent exit: network effects (merchants must accept where customers expect to pay), interoperability barriers (lack of open standards), regulatory fragmentation (different rules per jurisdiction), and technological switching costs (merchant terminal equipment, software integration). These barriers are structural rather than legal, making them harder to regulate directly. Theater ratio (0.48): Moderate. Regulatory oversight (interchange caps, interoperability mandates) is visible and material but has not substantially reduced underlying extraction. The theater reflects that regulation targets specific mechanisms (interchange fees) while extraction continues through alternative mechanisms (network assessment fees, compliance costs, premium network pricing). Regulation maintains appearance of control without achieving structural change.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between beneficiaries and victims. Visa/Mastercard classify the constraint as pure Rope from the institutional/arbitrage perspective: they solve the double-coincidence problem, enable commerce, and invest in fraud prevention and security. This perspective is not wrong — the coordination function is real. Unbanked populations classify the same constraint as Snare: they are excluded entirely, cannot participate, and cannot exit the exclusion. Underbanked merchants classify as Snare: they must accept the networks, cannot negotiate, and have no alternative. Competing providers classify as Tangled Rope: they benefit from the open payment standard but are gatekept by interoperability barriers and switching costs. Merchant coalitions classify as Tangled Rope: they coordinate with the networks on standardization while extracting from interchange fees. Regulators classify as Piton: they maintain oversight and rate-setting authority, but the regulation appears performative — concentration and extraction persist despite regulatory intervention. The Analytical observer at civilizational scale risks classifying as Mountain: 'Payment networks are naturally monopolistic due to network effects.' But the structural data reveals this as a false summit — the concentration is maintained through contingent institutional arrangements (switching costs, regulatory frameworks, interoperability barriers) rather than inherent physical or mathematical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural position within the payment network. Beneficiaries of market concentration (Visa, Mastercard) have arbitrage exit options and derive d ≈ 0.05-0.15, producing low or negative effective extraction. Trapped agents (unbanked populations, underbanked merchants with no payment alternative) have d ≈ 0.95, producing maximum experienced extraction. Constrained agents (competing payment providers, merchant coalitions) have intermediate d ≈ 0.55-0.70, experiencing moderate-high extraction with some agency. Organized agents (regulatory bodies, CBDC initiatives) have d ≈ 0.50-0.65, reflecting mixed beneficiary/victim status — they see both coordination value and extraction mechanisms. The Analytical observer uses canonical d ≈ 0.73 for analytical power. Suppression scaling: suppression is unscaled (0.65 raw). Effective extractiveness χ scales suppression via scope: σ(national) = 1.0, σ(global) = 1.2. Global-scope victims experience χ ≈ 0.58 × f(d) × 1.2, amplifying the felt extraction at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how genuine coordination coexists with real extraction, validating the Tangled Rope classification as analytically central. The beneficiary perspective (Rope) observes the coordination function truthfully — payment networks do solve genuine problems. The victim perspective (Snare) observes the extraction mechanism truthfully — mandatory participation and high switching costs are real constraints. Neither perspective is lying; they are observing different aspects of the same structure. The mandatrophy is resolved not by choosing one type over another but by recognizing that the constraint simultaneously coordinates and extracts, and that the ratio between these functions varies by agent. From the beneficiary's position, coordination dominates (Rope classification). From the powerless agent's position, extraction dominates (Snare classification). From positions of moderate power and constrained exit (competitors, merchant associations), both functions are structurally present and neither dominates (Tangled Rope classification). The analytical observer at civilizational scale must resist the Mountain temptation — treating network effects as natural law — and instead recognize that the observed concentration is structurally sustained by institutional choices (regulatory framework, technical standards, interoperability requirements) that are changeable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_magnitude,
    'What portion of current market concentration derives from genuine network effects vs switching costs and regulatory barriers?',
    'Empirical analysis of alternative payment network adoption rates; comparison of consumer switching friction across jurisdictions; measurement of value proposition differences between incumbents and challengers',
    'If dominated by genuine network effects: concentration may be inherently stable (Mountain-like). If dominated by switching costs: concentration is maintainable extraction (Snare/Tangled Rope). Classification shifts between Mountain and Snare depending on answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_magnitude, empirical, 'Degree to which market concentration reflects network effects vs switching costs').

omega_variable(
    incumbent_innovation_subsidy,
    'Do incumbent payment networks leverage extraction rents to subsidize infrastructure innovation that competitors cannot match, creating a sustainable competitive advantage?',
    'Financial analysis of incumbent R&D spending, fraud prevention infrastructure costs, and cross-subsidy patterns; comparison with challenger capabilities; measurement of whether network improvements require incumbents'' scale',
    'If yes: extraction may be welfare-improving (rents fund public goods). If no: extraction is pure rent-seeking with negligible innovation benefit. Shifts classification between justified Tangled Rope and extractive Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_innovation_subsidy, empirical, 'Whether incumbent scale subsidizes innovation that benefits ecosystem').

omega_variable(
    distributed_alternative_scalability,
    'Can distributed payment infrastructure (blockchain, CBDC) scale to global transaction volumes while maintaining decentralization and security guarantees?',
    'Technical analysis of throughput, latency, energy consumption, and attack surface for distributed vs centralized networks; real-world stress testing under peak load; security audit comparisons',
    'If scalable: Scaffold sunset is plausible within generational timescale. If not: distributed alternatives remain niche, concentration persists. Determines validity of Scaffold perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_alternative_scalability, empirical, 'Whether distributed alternatives can scale to replace incumbent networks').

omega_variable(
    regulatory_arbitrage_exhaustion,
    'As major jurisdictions impose interchange fee caps and interoperability mandates, can incumbent networks maintain extraction through regulatory arbitrage?',
    'Comparative analysis of network profitability across regulatory regimes (EU vs US vs Asia); tracking of alternative extraction mechanisms post-regulation; measurement of capital flight to less-regulated jurisdictions',
    'If arbitrage exhausted: extraction declines under regulatory pressure (Scaffold/Piton trajectory). If arbitrage continues: concentration and extraction persist despite regulation (Piton/Snare persistence). Determines long-term classification stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_exhaustion, empirical, 'Whether regulatory arbitrage can sustain incumbent extraction').

omega_variable(
    unbanked_population_accessibility,
    'What structural barriers (identity verification, minimum balance, device access, literacy) are inherent to financial inclusion vs maintainable through incumbent gatekeeping?',
    'Decomposition of unbanked exclusion factors; measurement of inclusion rates in jurisdictions with alternative infrastructure; analysis of mobile money and informal payment system accessibility patterns',
    'If barriers inherent: unbanked Snare classification reflects unavoidable coordination cost. If maintainable gatekeeping: exclusion is extraction mechanism (Snare from extraction rather than technical necessity). Shifts interpretation of victim status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbanked_population_accessibility, empirical, 'Degree to which financial exclusion reflects technical necessity vs incumbent gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(payment_network_market_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pnmc_tr_t0, payment_network_market_concentration, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pnmc_tr_t10, payment_network_market_concentration, theater_ratio, 10, 0.42).
narrative_ontology:measurement(pnmc_tr_t20, payment_network_market_concentration, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(pnmc_be_t0, payment_network_market_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pnmc_be_t10, payment_network_market_concentration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pnmc_be_t20, payment_network_market_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(payment_network_market_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(payment_network_market_concentration, 0.18).
narrative_ontology:affects_constraint(payment_network_market_concentration, merchant_fee_extraction).
narrative_ontology:affects_constraint(payment_network_market_concentration, credit_card_interest_rate_structure).
narrative_ontology:affects_constraint(payment_network_market_concentration, cross_border_remittance_pricing).

% DUAL FORMULATION NOTE:
% Payment network market concentration is upstream of multiple sectoral extraction constraints. Merchant fee extraction (interchange fees specifically), credit card interest structures (enabled by network switching costs and merchant lock-in), and cross-border remittance pricing (where incumbent networks extract rents from international transfers) are all downstream consequences of the primary concentration constraint. Each has its own extractiveness value reflecting domain-specific mechanisms, but all depend on the fundamental market structure established by network concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(payment_network_market_concentration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
