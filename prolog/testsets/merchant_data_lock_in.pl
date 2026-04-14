% ============================================================================
% CONSTRAINT STORY: merchant_data_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_merchant_data_lock_in, []).

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
 *   constraint_id: merchant_data_lock_in
 *   human_readable: Merchant Data Lock-In in Payment Processing Ecosystems
 *   domain: financial_services/digital_commerce
 *
 * SUMMARY:
 *   Merchant data lock-in in payment processing creates structural extraction
 *   through the concentration of transactional, customer, and operational
 *   data in proprietary platforms. Small merchants (retail, restaurants,
 *   e-commerce) accumulate years of transaction history, customer
 *   relationship data, and reconciliation records within payment processor
 *   ecosystems. Switching to alternative processors requires abandoning this
 *   accumulated data or paying substantial integration costs to migrate.
 *   Payment processors and platform operators (Stripe, Square, Shopify,
 *   PayPal) benefit from lock-in through sustained fee extraction, reduced
 *   price competition, and ability to cross-sell ancillary services. The
 *   constraint exhibits all six DR types depending on merchant size and
 *   market position. For powerless small merchants, it appears as a snare —
 *   exit is economically impossible. For mid-market merchants, it is tangled
 *   rope — genuine coordination services bundled with extraction. For payment
 *   processors, it is rope — they genuinely solve merchant problems (fraud,
 *   settlement, reconciliation) while capturing asymmetric value. For the
 *   open finance movement, it is a temporary problem with a sunset — data
 *   portability regulations and interoperability standards are reducing
 *   lock-in through policy intervention. For legacy banking infrastructure,
 *   it is a piton — regulatory requirements maintain the connection despite
 *   functional obsolescence. The analytical observer risks treating data
 *   lock-in as an inevitable feature of digital platforms (mountain),
 *   naturalizing what is actually a contingent regulatory and architectural
 *   choice.
 *
 * KEY AGENTS:
 *   - Small Merchants: Primary victims (powerless/trapped) — accumulate data within proprietary ecosystems; face switching costs exceeding available alternatives
 *   - Mid-Market Merchants: Secondary victims (moderate/constrained) — can migrate at significant operational cost; benefit from integrated payment services
 *   - Payment Processors: Primary beneficiaries (institutional/arbitrage) — extract value through sustained customer lock-in and data accumulation; maintain genuine coordination functions
 *   - Platform Ecosystem Builders: Mixed (powerful/mobile) — coordinate merchant ecosystems while locking in through integrated services; can migrate but face ecosystem friction
 *   - Legacy Banking Infrastructure: Institutional inertia (institutional/arbitrage) — maintained through regulation despite low functional contribution
 *   - Open Finance Movement: Organized agents (organized/constrained) — regulators and standards bodies building data portability mechanisms with sunset logic
 *   - Analytical Observer: Risk of naturalizing contingent arrangements (analytical/analytical) — treats data lock-in as inevitable feature of platforms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(merchant_data_lock_in, 0.58).
domain_priors:suppression_score(merchant_data_lock_in, 0.65).
domain_priors:theater_ratio(merchant_data_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(merchant_data_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(merchant_data_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(merchant_data_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(merchant_data_lock_in, tangled_rope).
narrative_ontology:human_readable(merchant_data_lock_in, "Merchant Data Lock-In in Payment Processing Ecosystems").
narrative_ontology:topic_domain(merchant_data_lock_in, "financial_services/digital_commerce").

domain_priors:requires_active_enforcement(merchant_data_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(merchant_data_lock_in, payment_processors).
narrative_ontology:constraint_beneficiary(merchant_data_lock_in, platform_operators).
narrative_ontology:constraint_victim(merchant_data_lock_in, small_merchants).
narrative_ontology:constraint_victim(merchant_data_lock_in, competitive_innovation).
narrative_ontology:constraint_victim(merchant_data_lock_in, market_contestability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MERCHANT (SNARE) — Trapped by data portability barriers and switching costs. Transaction history, customer relationships, inventory records, and reconciliation data are held hostage by platform lock-in. Exit requires abandoning accumulated customer data and operational continuity. No effective alternatives — cannot migrate without accepting substantial friction and business disruption. Maximum extraction from agents with zero bargaining power.
constraint_indexing:constraint_classification(merchant_data_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-MARKET MERCHANT (TANGLED ROPE) — Constrained but not trapped. Can migrate data at significant cost (IT integration, staff retraining, operational disruption). Benefits from coordination features: payment reconciliation, fraud detection, inventory management. Extraction is asymmetric — benefits are real but gains from lock-in accrue to platform. Exit is possible but expensive.
constraint_indexing:constraint_classification(merchant_data_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PAYMENT PROCESSOR (ROPE) — Net beneficiary. Extracts value from merchant switching costs and data accumulation. But must maintain genuine coordination functions (settlement, fraud prevention, customer service) to retain merchants. Experiences the constraint as coordination — solving merchant-acquirer problems while capturing asymmetric value. Can arbitrage merchant data across customers.
constraint_indexing:constraint_classification(merchant_data_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM ECOSYSTEM BUILDER (TANGLED ROPE) — Powerful agents (Shopify, Square, Toast) coordinate merchant ecosystems (payment, inventory, analytics, loyalty). Generate genuine value through integrated tools. Also lock merchants in through data and API dependencies. Can migrate but face substantial ecosystem friction. Extract through both coordination (ecosystem services) and lock-in (switching costs). Powerful exit options keep extraction below snare threshold.
constraint_indexing:constraint_classification(merchant_data_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY BANKING INFRASTRUCTURE (PITON) — Incumbent payment systems (ACH, wire transfer, traditional acquiring) offer minimal lock-in value. Merchants use them due to regulatory requirements and institutional inertia, not because alternatives have failed. The theater ratio is high — compliance and reconciliation processes are largely performative, maintained through regulation rather than function. The constraint persists through institutional path-dependence.
constraint_indexing:constraint_classification(merchant_data_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN FINANCE MOVEMENT (SCAFFOLD) — Organized agents (regulators, open-source projects, interoperability standards) are building data portability mechanisms with sunset logic. PSD2, Open Banking, FHIR-like standards in commerce, and open-source payment APIs reduce lock-in by enabling merchant data migration. Suppression is declining as regulation mandates data access. Extraction mechanism loses force as portability becomes standard.
constraint_indexing:constraint_classification(merchant_data_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From the civilizational level, data lock-in can appear to be an inevitable feature of digital commerce ('networks create lock-in naturally'). But the structural data contradicts this: lock-in is contingent on regulatory choices (data portability standards vs restrictions), API architecture decisions (proprietary vs interoperable), and market concentration. The mountain classification is a false summit — a naturalization of what is actually institutional lock-in.
constraint_indexing:constraint_classification(merchant_data_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(merchant_data_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(merchant_data_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(merchant_data_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(merchant_data_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(merchant_data_lock_in, TR),
    TR >= 0.70.

:- end_tests(merchant_data_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Payment processors capture sustained extraction through data lock-in, fee structure asymmetry, and ability to deny merchant exit. Trajectory shows growth from 0.35 to 0.58 over the interval as platforms have consolidated market share and increased data dependencies. Not as severe as pure snare (0.72+) because some merchants can exit at a cost, and integrated services provide genuine coordination benefits. Suppression (0.65): High. Barriers to merchant exit include accumulated customer data inaccessibility, incompatible data schemas, lack of open-source alternatives, technical integration friction, switching downtime risk, and career risk (small merchants lack IT expertise for platform migration). Regulation mandating data portability (PSD2, Open Banking) is beginning to reduce suppression, but merchant awareness and technical capacity remain barriers. Theater ratio (0.48): Moderate. Data lock-in is structurally real — it creates genuine switching friction — but some performance is theatrical: marketing narratives of proprietary 'intelligence' and 'security' obscure relatively standard payment processing. As interoperability standards mature, some of the claimed uniqueness becomes increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (data lock-in) produces divergent classifications across different observer positions. A powerless small merchant sees snare because exit is impossible and extraction is maximal. A powerful platform builder sees tangled rope because they coordinate valuable services while also locking merchants in through data dependencies — and they can exit this particular constraint if competitive pressure increases. A regulatory agent sees scaffold — the constraint is temporary because policy interventions (data portability standards) are building exit pathways. The gap reveals that lock-in's character (snare vs tangled rope vs scaffold) is contingent on market structure (concentration), regulatory environment (data portability mandates), and technological maturity (open-source alternatives), not inherent to payment processing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: power level, exit capacity, and relationship to the extraction flow. Small merchants are trapped (high d → high f(d) → high χ experienced extraction) and are victims of the constraint (concentrated extraction on powerless agents). Payment processors are beneficiaries (low d → low f(d) → negative χ, i.e., they receive extraction). Mid-market merchants are constrained (moderate d), experiencing moderate extraction because some exit capacity exists. Platform builders are powerful and mobile (lower d despite being partial beneficiaries), which keeps their experienced extraction below snare threshold. The analytical observer is positioned outside the extraction flow (d ≈ 0.72 canonical for analytical) and observes the structure rather than experiencing extraction directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution in this constraint requires distinguishing genuine coordination (payment processing, fraud detection, customer service) from extractive lock-in (data portability barriers, switching costs, vendor lock). The tangled_rope classification captures this: payment processors genuinely solve merchant coordination problems (settlement, reconciliation, fraud prevention are valuable services) while also capturing extraction through data lock-in. The extracted value exists alongside real coordination value. The snare classification from the small merchant perspective reveals that for agents with zero bargaining power, the coordination value becomes irrelevant — they experience only the extraction. The scaffold classification from the regulatory perspective reveals that the extraction mechanism (data lock-in) is policy-contingent and has a sunset: as data portability regulations mandate open APIs and data transfer rights, the lock-in mechanism degrades. The false summit classification at the analytical level identifies that treating data lock-in as inevitable ('networks create lock-in') naturalizes a contingent regulatory choice. The mandatrophy resolves by acknowledging that all six classifications are legitimate perspectival readings — the constraint genuinely is tangled rope, snare, rope, scaffold, piton, and false mountain depending on observer position and structural relationship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_portability_technical_feasibility,
    'Can merchant transaction data be ported between payment systems without loss of integrity or analytical capacity?',
    'Technical audit of data schema portability; testing merchant migration workflows with full data transfer; comparison of analytics capabilities pre- and post-migration',
    'If feasible: lock-in is primarily regulatory/market choice (suppression < 0.50). If infeasible: lock-in has genuine technical foundations (suppression justifies snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_technical_feasibility, empirical, 'Technical feasibility of merchant data portability').

omega_variable(
    switching_cost_ratio_to_revenue,
    'What proportion of small merchant revenue is consumed by switching costs to alternative payment platforms?',
    'Survey of actual migration costs (integration labor, downtime, staff retraining, customer communication); comparison to merchant gross profit margins',
    'If > 20% of annual profit: trapped classification appropriate. If < 5%: constrained classification more accurate. Affects whether snare or tangled_rope is structural reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_ratio_to_revenue, empirical, 'Ratio of switching costs to merchant profitability').

omega_variable(
    data_lock_in_vs_service_lock_in_disentanglement,
    'Is merchant lock-in driven primarily by inability to port data, or by inability to replicate integrated services (fraud detection, inventory management, reconciliation)?',
    'Controlled comparison: merchants with full data portability but service integration friction vs merchants with service portability but data restrictions. Measure switching rates by friction type.',
    'If data-driven: open-source data standards and APIs solve the problem. If service-driven: data portability alone does not reduce lock-in; ecosystem maturity matters more.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_lock_in_vs_service_lock_in_disentanglement, empirical, 'Decomposition of data lock-in vs integrated service lock-in').

omega_variable(
    platform_consolidation_feedback_loop,
    'Does merchant lock-in concentration reinforce platform market power, creating a feedback loop where dominant platforms can raise extraction without losing volume?',
    'Time-series analysis of payment processor fee increases correlated with merchant switching rates and data portability restrictions; comparison of competitive markets with/without portability mandates',
    'If confirmed: lock-in is structurally extractive (snare). If refuted: platforms can extract only as much as competitive alternatives permit (constrains to tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_consolidation_feedback_loop, empirical, 'Whether lock-in reinforces extractive market power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(merchant_data_lock_in, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdli_tr_t0, merchant_data_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mdli_tr_t5, merchant_data_lock_in, theater_ratio, 5, 0.4).
narrative_ontology:measurement(mdli_tr_t10, merchant_data_lock_in, theater_ratio, 10, 0.48).
narrative_ontology:measurement(mdli_tr_t15, merchant_data_lock_in, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(mdli_be_t0, merchant_data_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mdli_be_t5, merchant_data_lock_in, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mdli_be_t10, merchant_data_lock_in, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mdli_be_t15, merchant_data_lock_in, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(merchant_data_lock_in, resource_allocation).
narrative_ontology:affects_constraint(merchant_data_lock_in, payment_processing_fee_extraction).
narrative_ontology:affects_constraint(merchant_data_lock_in, api_dependency_lock).
narrative_ontology:affects_constraint(merchant_data_lock_in, data_portability_standards).

% DUAL FORMULATION NOTE:
% Merchant data lock-in is downstream of platform ecosystem consolidation but represents a distinct structural constraint. The upstream consolidation constraint has its own dynamics; this story focuses on the data lock-in mechanism specifically. Network linkage to data portability standards reflects that regulatory interventions directly reduce this constraint's extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(merchant_data_lock_in, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
