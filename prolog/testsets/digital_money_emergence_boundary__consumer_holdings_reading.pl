% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Digital Money Emergence Boundary (Consumer Holdings Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   The consumer-holdings reading identifies the emergence of digital money
 *   with the moment consumers could hold and transact digital instruments
 *   directly, outside traditional bank accounts. This reading draws a
 *   boundary at 1990s e-purses and formalizes it at 2000 EMD (Electronic
 *   Money Directive). The constraint captures the structural asymmetry
 *   created when regulatory and fintech authorities define 'money' to include
 *   direct consumer holdings: regulators gain supervisory domain; fintech
 *   issuers gain market access and customer relationships; traditional banks
 *   lose deposit monopoly; consumers gain payment optionality but lose
 *   counterparty-risk protection. The extractiveness trajectory (0.18 → 0.42)
 *   reflects the accumulation of fintech issuers without corresponding
 *   consumer protections, reaching a stable plateau once regulatory
 *   frameworks (MiFID, PSD2) established baseline requirements. The theater
 *   ratio (0.38 → 0.55) reflects that early e-money (1990s e-purses,
 *   pre-2000) functioned operationally but lacked regulatory recognition —
 *   once EMD formalized the category in 2000, the same products became
 *   'official' money. The increase in theater ratio captures the added
 *   procedural and compliance layer that regulatory classification imposed.
 *   Suppression (0.32 → 0.48) reflects the progression from early optionality
 *   (consumers could choose e-purses or bank accounts) to growing dependency
 *   (e-money became necessary for online commerce, mobile payments,
 *   cross-border transfers, with limited exit paths). This is a Tangled Rope
 *   constraint: genuine coordination function (e-money enables payment
 *   innovation, interoperability, financial inclusion) paired with asymmetric
 *   extraction (fintech and regulatory capture of customer relationships and
 *   authority).
 *
 * KEY AGENTS:
 *   - Fintech Issuers / E-Money Providers: Beneficiaries (institutional/arbitrage) — define consumer-holdings boundary, capture customer relationships, operate with regulatory arbitrage
 *   - Regulatory Bodies (ECB, FCA, PRA): Beneficiaries (institutional/arbitrage) — gain supervisory domain, classify e-money into monetary aggregates, license issuers
 *   - Consumer Holding E-Money: Victims (powerless/trapped) — depend on issuer for redemption, face counterparty risk, limited exit options
 *   - Traditional Banking Sector: Mixed (moderate/constrained) — lose deposit monopoly but maintain regulatory preference, constrained to integrate with e-money rails
 *   - Central Bank / Monetary Authority: Mixed (organized/constrained) — coordinate payment infrastructure but lose control of monetary aggregates and money supply
 *   - Legacy Payment Operators: Victims (institutional/constrained) — lose transaction volume, persist through inertia (piton dynamic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.42).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.48).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Digital Money Emergence Boundary (Consumer Holdings Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, '2b081d00-5c1e-4b82-92dd-5f0331a9c152').
narrative_ontology:cs_kernel_codification('2b081d00-5c1e-4b82-92dd-5f0331a9c152', formalized).
narrative_ontology:cs_authority_grounding('2b081d00-5c1e-4b82-92dd-5f0331a9c152', extraction).
narrative_ontology:cs_interpretation_layer_present('2b081d00-5c1e-4b82-92dd-5f0331a9c152').
narrative_ontology:cs_reading_relation('2b081d00-5c1e-4b82-92dd-5f0331a9c152', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_reading_relation('2b081d00-5c1e-4b82-92dd-5f0331a9c152', digital_money_emergence_boundary__conceptualization_reading, influences).
narrative_ontology:cs_axiom('2b081d00-5c1e-4b82-92dd-5f0331a9c152', foundational, consumer_direct_custody_necessary).
narrative_ontology:cs_axiom_status(consumer_direct_custody_necessary, holdable).
narrative_ontology:cs_axiom_grounding('2b081d00-5c1e-4b82-92dd-5f0331a9c152', consumer_direct_custody_necessary, deontological).
narrative_ontology:cs_axiom('2b081d00-5c1e-4b82-92dd-5f0331a9c152', secondary, regulatory_licensing_legitimacy).
narrative_ontology:cs_axiom_status(regulatory_licensing_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2b081d00-5c1e-4b82-92dd-5f0331a9c152', regulatory_licensing_legitimacy, conventional).
narrative_ontology:cs_reference_frame('2b081d00-5c1e-4b82-92dd-5f0331a9c152', bank_mediated_monetary_accounts).
narrative_ontology:cs_drift_state('2b081d00-5c1e-4b82-92dd-5f0331a9c152', contemporary_fintech_proliferation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2b081d00-5c1e-4b82-92dd-5f0331a9c152', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, traditional_banking_sector).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, monetary_authority_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER HOLDING DIGITAL MONEY (SNARE) — Once a consumer holds e-money outside a traditional bank account, they face operational entrapment: the issuer controls redemption, conversion, and liquidity. Exit from the constraint (converting back to fiat) depends entirely on issuer willingness. Suppression is structural — no alternative infrastructure exists for holding consumer-level digital instruments outside regulated issuers. Maximum experienced extraction: the consumer bears counterparty risk and has zero recourse beyond regulatory complaint.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRADITIONAL BANKING SECTOR (TANGLED ROPE) — Banks benefit from consumer settlement finality and float (the time lag in clearing). But the emergence of direct consumer e-money holdings disrupts their monopoly on deposit accounts. They face constraints: regulatory pressure to offer e-money services, competitive pressure from fintech issuers, and loss of low-cost deposit funding. The constraint produces both coordination (banks must integrate with digital rails) and extraction (fintech competitors extract margin and customer relationships from traditional banking).
constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINTECH ISSUERS / E-MONEY PROVIDERS (ROPE) — These agents define and benefit from the consumer-holdings boundary. They can issue e-money instruments, capture customer relationships, and operate with regulatory arbitrage (lighter capital requirements than traditional banks). The constraint is pure coordination for them: defining what counts as 'holdable' money enables a new market. They experience the constraint as enabling, not extractive. Effective extraction runs AWAY from this group.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MONETARY AUTHORITY / CENTRAL BANK (TANGLED ROPE) — Central banks experience a hybrid: genuine coordination need (e-money must interoperate with payment rails, reserve systems, settlement infrastructure) paired with extraction loss (they lose control over monetary aggregates, demand for banknotes declines, and regulatory authority must stretch to non-bank issuers). They are constrained to accommodate e-money but retain some enforcement power through capital requirements and licensing. Their exit options are constrained — abandoning e-money regulation is not viable once it enters circulation.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY BODIES (ECB, FCA, etc.) (ROPE) — These institutions benefit directly from the consumer-holdings boundary: it creates a new asset class they can regulate, classify (M4/M5 separation), and supervisory space they can occupy. The constraint is pure coordination for them: defining when 'money' transitions from theoretical concept to regulated product enables regulatory capture of fintech. They experience low extraction cost — enforcement is delegated to issuers through licensing conditions. Effective chi is negative (they benefit).
constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational analytical position, the consumer-holdings boundary appears as a natural consequence of technological capability: once cryptographic signing, electronic wallets, and digital verification became feasible (inherent to computation and communication), direct consumer holding of digital instruments became inevitable. This perspective risks naturalizing what is actually a contested institutional boundary. The false-summit detector will flag this: the boundary is contingent on regulatory choices (which entities are licensed to issue), not inherent to technology.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: LEGACY PAYMENT RAIL OPERATORS (PITON) — ATM networks, check clearing houses, and traditional payment processors continue to function but are increasingly ornamental. Consumer e-money holdings bypass them entirely. Their operational status persists through regulatory inertia and installed base (decades of integration cost), but their core function has degraded. Theater ratio high: they maintain procedures and clearing houses that process fewer transactions annually while e-money transfers occur directly. Piton classification reflects institutional inertia, not functional necessity.
constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_money_emergence_boundary__consumer_holdings_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, TR),
    TR >= 0.70.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate-high. The constraint produces significant value capture by fintech issuers and regulatory authorities: fintech issuers extract customer relationships and margin from consumer e-money holdings (counterparty risk premiums, currency conversion spreads, dormancy fees). Regulatory authorities extract supervisory authority and classification power. But extractiveness is not maximal (>0.70) because consumers retain optionality to use alternative payment methods (cards, bank transfers, cash in lower-income contexts) and regulatory frameworks have imposed baseline protections (capital reserves, deposit insurance surrogates, consumer complaint mechanisms). Suppression (0.48): Moderate-high. Significant barriers exist: network effects (e-money value depends on merchant acceptance), technical barriers (wallet software, internet access), regulatory barriers (licensing requirements lock out non-approved issuers), and operational barriers (once in circulation, exit from e-money dependency requires alternative payment infrastructure). Not total suppression — formal alternatives exist and some populations retain cash or card-based optionality. Theater ratio (0.55): Moderate. The regulatory classification (EMD, PSD2) added procedural layers that appear to protect consumers (licensing, capital reserves, prudential oversight) but do not substantively reduce counterparty risk — the issuer retains control of redemption and liquidity. Early e-purses (1990s) were purely functional; modern e-money has a substantial regulatory theater component. Theater exceeds functionality because procedural compliance (compliance officers, documentation, audits) outweighs the actual reduction in consumer loss risk.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's six perspectives produce all six classification types from a single set of base properties, demonstrating how indexical positioning determines classification. The fintech issuer sees Rope (they benefit from the boundary they define). The regulatory body sees Rope (they supervise the category they created). The consumer sees Snare (trapped optionality and counterparty risk). The traditional bank sees Tangled Rope (forced integration plus customer loss). The central bank sees Tangled Rope (payment coordination plus monetary control loss). The legacy payment operator sees Piton (operational persistence through inertia). The analytical observer risks Mountain (technological inevitability) — a false summit when the boundary is actually contingent on regulatory choice. This perspectival diversity is the analytical signature of Tangled Rope: genuine coordination function (e-money solves payment friction) contaminated by asymmetric extraction (fintech and regulatory capture).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural relationship to the extraction flow. Fintech issuers have d ≈ 0.10 (beneficiaries with arbitrage exit) — they define the boundary and profit from it with minimal external coercion. Regulatory bodies have d ≈ 0.05 (institutional beneficiaries with arbitrage) — they supervise the category they created. Consumers have d ≈ 0.88 (powerless victims with trapped exit) — they depend on issuers for redemption and face counterparty risk. Traditional banks have d ≈ 0.65 (moderate victims with constrained exit) — they bear customer relationship loss but retain regulatory preference and capital requirements advantage. Central banks have d ≈ 0.55 (organized agents with constrained exit) — they lose monetary control but retain payment system coordination authority. The derived d values map through the sigmoid f(d) to produce experience chi: fintech/regulatory agents experience negative chi (benefit from the constraint); consumer agents experience high chi (extract loss and trapped optionality). This asymmetry is the structural basis for the Tangled Rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_holding_empirical_scope,
    'Does ''direct consumer holding'' require technical custody (keys in wallet software) or is regulatory license sufficient (issuer holds assets but consumer has legal claim)?',
    'Regulatory mapping across EU, US, Asia: which jurisdictions require issuer custody vs. consumer key management for e-money classification. Technical audit of deployed e-money systems (PayPal, M-Pesa, stablecoins) vs. regulatory classification.',
    'If technical custody required: boundary is much narrower (excludes most current e-money, includes only self-custodial wallets). If regulatory license sufficient: boundary is broader (includes all e-money products), and the constraint''s extractiveness increases (consumers are trapped in issuer-dependent systems even if ''holding'' is legal status, not technical control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_holding_empirical_scope, empirical, 'Scope of ''direct consumer holding'' — technical custody vs. regulatory license').

omega_variable(
    definitional_kernel_ambiguity,
    'Is the kernel ''when did digital money emerge?'' asking for the FIRST moment when it was technologically possible, the first moment when regulators acknowledged it, or the first moment when it became economically significant?',
    'This is the committer-frame ambiguity: three readings coexist because the kernel is under-specified. Historical record shows all three moments (Chaum 1985 theory → ATM infrastructure 1967 → consumer e-purses 1990s → regulatory EMD classification 2000 → modern stablecoins 2017+). The three readings correspond to three different answers: conceptualization reading picks theoretical moment, infrastructure reading picks operational moment, consumer-holdings reading picks consumer-access moment.',
    'This omega documents that the three sibling readings are NOT empirical disagreement about the same observable. They are THREE DIFFERENT ANSWERABLE QUESTIONS about THREE DIFFERENT MOMENTS. No empirical evidence will resolve this to pick one ''correct'' reading — the kernel is ambiguous by design. Each reading is internally coherent but answers a different question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definitional_kernel_ambiguity, conceptual, 'Kernel under-specification: three readings correspond to three different moments (theoretical, infrastructural, consumer-access)').

omega_variable(
    regulatory_benefit_circularity,
    'Do regulatory bodies define the consumer-holdings boundary because it is economically natural, or do they define it to create regulatory space for themselves?',
    'Causal inference: did regulators anticipate the need to classify e-money as money BEFORE fintech products emerged, or did they retroactively create the classification when fintech created a regulatory gap? Compare timing of e-money product launches vs. regulatory framework publication (EMD Directive 2000/46/EC, Payment Services Directive 2007). Counterfactual: if e-money had remained unregulated, would the boundary have been conceptually necessary?',
    'If natural anticipation: the boundary is a genuine coordination mechanism and the rope classification for regulators is appropriate. If retroactive: the boundary is extraction-enabling and the tangled_rope classification is correct. High-confidence empirical resolution is possible through regulatory history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_benefit_circularity, empirical, 'Whether regulatory classification of consumer holdings is natural coordination or self-interested domain creation').

omega_variable(
    monetary_aggregate_necessity,
    'Does the consumer-holdings reading require a redefinition of monetary aggregates (M4/M5), or is that a separate policy choice?',
    'Comparison of ECB monetary aggregate definitions pre- and post-EMD. Analysis of whether M4 (broad money) could include e-money without creating a separate M5 classification. Econometric test: does inclusion of e-money in M4 change the relationship between money growth and inflation?',
    'If redefinition is necessary: the constraint''s suppression value increases (monetary authorities are forced to accommodate the new boundary or lose empirical control of monetary quantities). If redefinition is optional: suppression is lower (monetary authorities retain discretion). Affects classification boundary between tangled_rope and rope for central banks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_aggregate_necessity, empirical, 'Whether monetary aggregate redefinition is necessary consequence of consumer-holdings boundary').

omega_variable(
    false_summit_natural_law_candidate,
    'Is the consumer-holdings boundary a natural consequence of technology (inevitable digital capability) or a contingent regulatory choice?',
    'Counterfactual regulatory analysis: could regulators have prevented consumer direct holding of e-money through licensing restrictions while allowing bank-mediated digital transfers? Historical comparison: why did some jurisdictions adopt the consumer-holdings boundary (EU EMD 2000) while others maintained bank-monopoly models longer (some Asian markets)?',
    'If contingent regulatory choice: the mountain perspective is a false summit (naturalizing institutional choice as technological law). This triggers FSM reclassification via the signature detection chain. If genuinely technologically inevitable: mountain classification is appropriate. High stakes for analytical observer positioning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Consumer-holdings boundary as natural technological consequence vs. contingent regulatory choice (false summit candidate)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmeb_ch_tr_t0, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dmeb_ch_tr_t5, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(dmeb_ch_tr_t10, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(dmeb_ch_tr_t15, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(dmeb_ch_be_t0, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(dmeb_ch_be_t5, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(dmeb_ch_be_t10, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement(dmeb_ch_be_t15, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 15, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dmeb_ch_su_t0, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(dmeb_ch_su_t5, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(dmeb_ch_su_t10, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(dmeb_ch_su_t15, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).

% DUAL FORMULATION NOTE:
% The digital money emergence boundary is one kernel with three readings. This file (consumer_holdings_reading) instantiates the latest boundary — money exists when consumers hold direct digital instruments. The infrastructure_reading traces operational enablement (ATM, ACH, SWIFT). The conceptualization_reading traces theoretical formalization (Chaum cryptography). All three readings affect each other through the network: the conceptualization established the theoretical framework that infrastructure could implement; the infrastructure enabled the consumer-access boundary; the consumer boundary created regulatory necessity for the other two readings' frameworks. The three readings are linked via affects_constraints to show the dependency chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__consumer_holdings_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
