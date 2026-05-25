% ============================================================================
% CONSTRAINT STORY: visa_mastercard_interchange_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_visa_mastercard_interchange_extraction, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: visa_mastercard_interchange_extraction
 *   human_readable: Visa/Mastercard Interchange Fee Extraction System
 *   domain: financial/payment_systems
 *
 * SUMMARY:
 *   The Visa/Mastercard interchange fee system extracts rents from merchants
 *   and consumers through a structurally asymmetric fee arrangement that
 *   vendors cannot negotiate. Interchange fees—amounts paid by acquiring
 *   banks to issuing banks for each card transaction—represent approximately
 *   1.5-3.5% of transaction value in unregulated markets. This constraint
 *   exhibits genuine coordination function (fraud prevention, settlement,
 *   cardholder benefits) combined with significant asymmetric extraction,
 *   making it a canonical Tangled Rope. The system is defended as immutable
 *   network effect (mountain view), but regulatory evidence from EU and
 *   Australia demonstrates the extraction component is contingent: capped
 *   interchange maintains coordination function while reducing extraction.
 *   The constraint's low theater ratio (0.35) reflects that interchange fees
 *   are functionally transparent, not performative—they directly subsidize
 *   issuer operations. However, the visibility of their necessity (fraud
 *   prevention, settlement) obscures the monopoly rent component (lack of
 *   negotiation, network lock-in), creating a category error: calling all
 *   interchange 'coordination cost' naturalizes what regulators can and have
 *   separated into 'necessary cost' (lower) and 'extraction rent'
 *   (reducible).
 *
 * KEY AGENTS:
 *   - Card-Issuing Banks: Primary beneficiaries (institutional/arbitrage) — receive interchange fee revenues; have arbitrage options and can adjust offerings; lowest friction exit from system
 *   - Visa/Mastercard Networks: Primary beneficiaries (institutional/arbitrage) — set interchange standards, collect network fees; maintain duopoly control; arbitrage options through fee schedule adjustments
 *   - Small Merchants: Primary victims (powerless/trapped) — cannot exit card system without losing customer access; face fixed fees with zero negotiating power; no alternative payment infrastructure sufficiently mature
 *   - Consumers: Secondary victims (moderate/constrained) — don't directly pay interchange but bear costs through merchant price increases; convenience lock-in constrains ability to reject cards; not trapped, merely constrained
 *   - Large Merchants/Retailers: Secondary victim-beneficiaries (powerful/mobile) — have negotiating power with banks; can implement alternative payment channels (Apple Pay, proprietary systems); constrained by customer expectations but mobile enough to shape payments landscape
 *   - Regulatory Coalitions: Organized agents (organized/constrained) — see extraction as regulable; have constrained but real power to cap fees; operate within jurisdictional limits
 *   - Analytical Observer: Risks naturalizing contingent institutional arrangements as immutable network effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(visa_mastercard_interchange_extraction, 0.58).
domain_priors:suppression_score(visa_mastercard_interchange_extraction, 0.72).
domain_priors:theater_ratio(visa_mastercard_interchange_extraction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(visa_mastercard_interchange_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(visa_mastercard_interchange_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(visa_mastercard_interchange_extraction, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(visa_mastercard_interchange_extraction, tangled_rope).
narrative_ontology:human_readable(visa_mastercard_interchange_extraction, "Visa/Mastercard Interchange Fee Extraction System").
narrative_ontology:topic_domain(visa_mastercard_interchange_extraction, "financial/payment_systems").

domain_priors:requires_active_enforcement(visa_mastercard_interchange_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(visa_mastercard_interchange_extraction, card_issuing_banks).
narrative_ontology:constraint_beneficiary(visa_mastercard_interchange_extraction, visa_mastercard_networks).
narrative_ontology:constraint_victim(visa_mastercard_interchange_extraction, merchants).
narrative_ontology:constraint_victim(visa_mastercard_interchange_extraction, consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MERCHANT (SNARE) — Cannot exit the payment card system without losing access to customers; faces fixed interchange fees (1.5-3.5% of transaction value) with no negotiating power. Bears extraction costs directly with no coordination benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSUMER (TANGLED ROPE) — Constrained by the near-universal adoption of card payments; benefits from payment convenience and fraud protection but bears hidden costs through higher prices that merchants pass on to cover interchange fees. Extraction is real but not total — consumer is not trapped, merely constrained by convenience lock-in.
constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CARD-ISSUING BANK (ROPE) — Primary beneficiary; receives interchange fees as revenue stream. Experiences the system as pure coordination: managing card accounts, fraud prevention, and settlement. Net beneficiary with arbitrage options (can issue cards, adjust offerings, change networks). Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CARD NETWORK (ROPE) — Primary beneficiary; sets interchange fee standards and collects fees from issuers. Coordinates multi-sided platform (merchants, consumers, banks) while capturing significant rents. Arbitrage options extensive (can adjust fee schedules, set rules, branch into new segments). Experiences constraint as coordination mechanism with substantial embedded extraction.
constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE MERCHANT (TANGLED ROPE) — Has negotiating power and mobile exit options (can accept alternative payment methods, push against-card transactions, develop proprietary systems like Amazon Pay). Still constrained by customer expectations for card acceptance but has genuine agency. Experiences extraction with coordination benefits — payment system coordination enables transactions but with asymmetric fee structure.
constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY COALITION (TANGLED ROPE) — Organized agents (regulators, merchant associations, consumer advocates) see the interchange system as requiring active enforcement against extraction. Experience shows regulated caps (EU 0.3% credit/0.1% debit) reduce extraction while maintaining coordination function. Sees the system as solvable through regulatory boundaries rather than technological exit.
constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, multi-sided platform coordination for payment systems faces an apparently immutable network effect constraint: merchants must accept cards because consumers expect them; consumers expect cards because merchants accept them; networks extract rents from this lock-in as a structural inevitability. However, structural data contradicts the mountain classification — regulatory caps and alternative payment systems demonstrate the network effect is contingent, not natural law.
constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visa_mastercard_interchange_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(visa_mastercard_interchange_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(visa_mastercard_interchange_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(visa_mastercard_interchange_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. The system performs genuine coordination function — fraud prevention, transaction settlement, cardholder benefits. But empirical evidence (EU regulation at 0.3% credit/0.1% debit maintaining functionality) demonstrates 1.5-3.5% fees include significant extraction rent above coordination cost. The intermediate value (0.58 rather than 0.70+) reflects the genuine coordination component alongside extraction. Suppression (0.72): High. Merchants face near-total inability to exit: card acceptance is nearly mandatory for retail viability; alternative payment systems lack critical mass; no negotiation possible for individual merchants; even large retailers are constrained by consumer expectations. Regulatory and network barriers are substantial. Theater ratio (0.35): Moderate-low. Interchange fees are functionally transparent and directly tied to real services (fraud prevention, settlement). This is not performative like peer review theater. However, the justification framing obscures the monopoly rent component — claiming 'all interchange is necessary cost' when regulation shows 'half could be eliminated without losing coordination' is a rhetorical theater masking the actual fee structure. The theater has increased slightly over the interval as networks add new fee categories (assessment fees, network fees) to recapture regulatory savings.
 *
 * PERSPECTIVAL GAP:
 *   The powerless merchant sees a snare: complete extraction with no coordination benefit from their perspective (they would function identically with lower fees). The issuing bank sees rope: coordination mechanism that serves their core function (managing accounts, fraud, settlement) while providing revenue. The consumer sees tangled rope: genuine benefits (payment convenience, fraud protection) alongside hidden extraction (price increases from merchants). The card network sees rope: their networks genuinely coordinate a multi-sided platform. Large merchants see tangled rope: they have mobile options and negotiating power, experience both coordination benefits and extraction constraints. Regulators see tangled rope with solvable extraction component: demonstrate that coordination function persists at lower fee caps (EU evidence). The analytical observer risks seeing mountain (network effect immutable) but the EU regulation counterfactual shows the extraction component is regulable while preserving coordination. The perspectival gap reveals that the same set of fees is experienced as pure extraction (powerless merchant), necessary coordination cost (issuing bank), unavoidable overhead (consumer), and controllable rent (regulator).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (card-issuing banks, card networks) have arbitrage exit options and are net recipients of interchange flows — they derive d from low exit cost and beneficiary status, producing negative f(d), absorbing extraction toward themselves. Victims (small merchants) have trapped or constrained exit, derive high d from victim + trapped combination, producing high f(d) and maximum experienced extraction. Large merchants have powerful status + mobile exit options, moderating their experienced extraction despite victim status — mobile exit options reduce their d value. Consumers are constrained but not trapped, producing moderate d and moderate experienced extraction. The regulatory coalition has organized status + constrained (jurisdictional) exit, producing moderate d and providing a structural check on extraction escalation. Each perspective's chi value emerges from the base extractiveness (0.58) scaled by f(d) and scope modifier: a powerless trapped merchant at global scope experiences high chi; an institutional arbitrage issuer experiences low or negative chi despite identical base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the interchange system into its coordination and extraction components empirically: EU regulation and other capped-fee jurisdictions demonstrate that coordination function (fraud prevention, settlement, cardholder benefits) persists at materially lower fee rates. This falsifies the claim that all 1.5-3.5% fees are coordination cost. It also falsifies the mountain view that network effects necessarily produce this extraction level — network effects explain why the extraction persists despite being reducible, not why it must be at current levels. The constraint is Tangled Rope: genuine coordination (fraud prevention, settlement) combined with genuine extraction (monopoly rents from network lock-in), and the tangled structure is confirmed by the regulatory evidence that one component can be removed without destroying the other. The constraint cannot be classified as pure coordination (Rope) because the monopoly rent component is real and harms powerless merchants. It cannot be classified as pure extraction (Snare) because coordination function is genuinely necessary and valuable. The Tangled Rope classification is the only one consistent with all observed perspectives and the regulatory counterfactual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_contingency,
    'Is the network effect lock-in an immutable structural property or a contingent institutional outcome?',
    'Comparison of EU-regulated markets (capped interchange) with unregulated markets (US); time-series analysis of payment method diversity and merchant acceptance rates pre/post regulation; emergence of alternative systems (mobile wallets, crypto, stablecoins) as genuine exits',
    'If contingent: mountain classification is false summit; constraint is Tangled Rope from all perspectives. If immutable: network effect justifies extraction as coordination cost unavoidable by any payment system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_contingency, empirical, 'Whether network lock-in is structural or institutional').

omega_variable(
    alternative_payment_viability,
    'Can decentralized payment systems (blockchain, mobile money) provide genuine exit from Visa/Mastercard extraction without requiring universal adoption?',
    'Market penetration analysis of alternative systems; cost structure comparison; consumer friction measurement in multi-payment environments; merchant profitability correlation with payment method mix',
    'If viable: scaffold perspective correct — alternative systems have sunset logic. If not viable: extraction constraint remains mountain-like despite alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_payment_viability, empirical, 'Viability of alternative payment systems as genuine exits').

omega_variable(
    interchange_necessity_for_coordination,
    'What minimum interchange rate is structurally necessary for the coordination function (fraud prevention, settlement, cardholder benefits) vs what is pure extraction rent?',
    'Cost accounting of card issuer operations (fraud losses, processing, rewards); comparison with actual interchange rates; empirical analysis of service degradation at lower fee caps; counterfactual analysis of payments markets with zero interchange fees',
    'If high minimum (> 0.8%): most extraction claimed by banks is coordination cost; constraint better classified as Rope. If low minimum (< 0.3%): current fees (1.5-3.5%) are primarily extraction; Snare/Tangled Rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interchange_necessity_for_coordination, empirical, 'Minimum interchange necessary for coordination function').

omega_variable(
    regulatory_arbitrage_escalation,
    'As jurisdictions cap interchange fees, do card networks escalate extraction through alternative mechanisms (assessment fees, network fees, scheme fees)?',
    'Fee structure tracking across jurisdictions; correlation of regulatory caps with emergence of new fee categories; merchant and issuer revenue impact analysis; comparison of total payment system costs pre/post regulation',
    'If escalation observed: regulation removes one extraction channel but does not constrain the underlying asymmetric relationship; constraint persists as Tangled Rope with shifted extraction vectors. If no escalation: regulation reduces total extraction and demonstrates constraint is not immutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_escalation, empirical, 'Whether card networks escalate extraction through alternative fee mechanisms after regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(visa_mastercard_interchange_extraction, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmc_ifx_tr_t0, visa_mastercard_interchange_extraction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vmc_ifx_tr_t15, visa_mastercard_interchange_extraction, theater_ratio, 15, 0.28).
narrative_ontology:measurement(vmc_ifx_tr_t30, visa_mastercard_interchange_extraction, theater_ratio, 30, 0.35).
narrative_ontology:measurement(vmc_ifx_tr_t45, visa_mastercard_interchange_extraction, theater_ratio, 45, 0.38).

% Extraction over time
narrative_ontology:measurement(vmc_ifx_be_t0, visa_mastercard_interchange_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vmc_ifx_be_t15, visa_mastercard_interchange_extraction, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(vmc_ifx_be_t30, visa_mastercard_interchange_extraction, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(vmc_ifx_be_t45, visa_mastercard_interchange_extraction, base_extractiveness, 45, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(visa_mastercard_interchange_extraction, resource_allocation).
narrative_ontology:affects_constraint(visa_mastercard_interchange_extraction, payment_network_duopoly_control).
narrative_ontology:affects_constraint(visa_mastercard_interchange_extraction, merchant_acquiring_bank_power_asymmetry).

% DUAL FORMULATION NOTE:
% The interchange extraction system is downstream of two upstream structural constraints: the payment network duopoly (Visa/Mastercard control ~85% of global card volume) and the power asymmetry between acquiring banks and individual merchants. The interchange constraint has its own extractiveness (0.58) reflecting the specific fee extraction mechanism; the upstream constraints have different extractiveness values reflecting the structural duopoly and power asymmetry. Decomposition clarifies that reducing interchange while preserving duopoly control may shift extraction mechanisms rather than eliminate them (captured in the regulatory_arbitrage_escalation omega variable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(visa_mastercard_interchange_extraction, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
