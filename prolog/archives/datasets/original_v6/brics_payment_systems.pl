% ============================================================================
% CONSTRAINT STORY: brics_payment_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brics_payment_systems, []).

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
 *   constraint_id: brics_payment_systems
 *   human_readable: BRICS Payment Systems and De-dollarization
 *   domain: geopolitical_economics/international_finance
 *
 * SUMMARY:
 *   BRICS payment systems represent a contestable coordination mechanism that
 *   exhibits genuine coordination function (reducing forex friction among
 *   member states, enabling trade settlement without intermediation through
 *   Western financial centers) alongside extractive elements (rent-capturing
 *   through currency conversion spreads, exclusion of smaller participants,
 *   geopolitical coercion through payment system access control). The
 *   constraint's extractiveness has increased from 0.35 (early pilot phase
 *   emphasis on coordination) to 0.58 (institutionalized system with embedded
 *   rent-extraction mechanisms). Theater ratio has risen from 0.40 to 0.55 as
 *   the system's promotional narratives have diverged from actual settlement
 *   capacity — media coverage emphasizes de-dollarization momentum while
 *   actual trade settlement remains marginal relative to claims. The system
 *   coordinates genuine intra-BRICS settlement needs while simultaneously
 *   extracting from smaller currency blocs forced to navigate BRICS
 *   participation or dollar dependency.
 *
 * KEY AGENTS:
 *   - BRICS Member States (China, India, Russia, Brazil, South Africa, new members): Powerful institutional actors (powerful/constrained) — benefit from settlement coordination and sanctions circumvention capacity; constrained by dollar ecosystem dependence and internal contradictions.
 *   - Smaller Currency Blocs (African, Southeast Asian regional economies): Powerless peripheral actors (powerless/trapped) — bear extraction through unfavorable conversion rates and payment system rent; no exit option.
 *   - USD Hegemonic System (US Treasury, Federal Reserve, dollar-based correspondent banking): Institutional beneficiary (institutional/arbitrage) — experiences BRICS as coordination pressure rather than extraction; maintains arbitrage through system redundancy.
 *   - De-dollarization Coalition (Organized state actors, central banks, alternative finance advocates): Organized participants (organized/constrained) — see payment system as temporary scaffold toward normalization of alternative settlement.
 *   - Bretton Woods Institutions (IMF, World Bank, legacy coordination frameworks): Institutional legacy holder (institutional/arbitrage) — maintains performative relevance through institutional inertia despite reduced functional use.
 *   - Analytical Observer: Civilizational viewpoint (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to multipolar geopolitics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brics_payment_systems, 0.58).
domain_priors:suppression_score(brics_payment_systems, 0.62).
domain_priors:theater_ratio(brics_payment_systems, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brics_payment_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(brics_payment_systems, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(brics_payment_systems, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brics_payment_systems, tangled_rope).
narrative_ontology:human_readable(brics_payment_systems, "BRICS Payment Systems and De-dollarization").
narrative_ontology:topic_domain(brics_payment_systems, "geopolitical_economics/international_finance").

domain_priors:requires_active_enforcement(brics_payment_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brics_payment_systems, brics_member_states).
narrative_ontology:constraint_beneficiary(brics_payment_systems, alternative_payment_infrastructure_operators).
narrative_ontology:constraint_victim(brics_payment_systems, usd_hegemonic_stability).
narrative_ontology:constraint_victim(brics_payment_systems, smaller_currency_blocs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BRICS MEMBER STATE (TANGLED ROPE) — Powerful agent constrained by dollar dependency and sanctions risk. The BRICS payment system coordinates genuine regional settlement needs (reducing forex friction, enabling intra-BRICS trade) while simultaneously extracting from smaller currency users and creating new rent-capturing mechanisms. Constrained exit because departing would restore immediate dollar dependence; coordination benefit is real but asymmetric extraction accrues upward.
constraint_indexing:constraint_classification(brics_payment_systems, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: SMALLER CURRENCY BLOC (SNARE) — Trapped between dollar and BRICS systems, bears extraction from both. Limited bargaining power; must accept BRICS payment terms or remain in exclusive dollar ecosystem. No arbitrage option; trapped in regional payment hierarchy regardless of choice.
constraint_indexing:constraint_classification(brics_payment_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: USD-HEGEMONIC FINANCIAL SYSTEM (ROPE) — Experiences BRICS payment infrastructure as pure coordination from its vantage point: settles international trades, reduces transaction costs for participating members, enables financial transparency. No extraction experienced because the constraint extracts FROM this system's hegemonic position, not toward it. Arbitrage option is structural — alternative systems increase USD settlement competition without destroying the dollar ecosystem.
constraint_indexing:constraint_classification(brics_payment_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DE-DOLLARIZATION COALITION (SCAFFOLD) — Organized actors (BRICS, Shanghai Cooperation Organization, Non-Aligned Movement) see payment system diversification as a temporary coordination problem with a sunset. The constraint has genuine sunset: as alternative systems mature (BRICS+ Pay, digital currencies, bilateral settlement arrangements), the need for unified BRICS payment infrastructure diminishes. Sunset horizon: 15-25 years as alternative settlement networks normalize.
constraint_indexing:constraint_classification(brics_payment_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRETTON WOODS INSTITUTIONAL LEGACY (PITON) — IMF and World Bank frameworks for currency coordination persist through institutional inertia despite reduced functional relevance. The original Bretton Woods coordination function (post-WWII currency stability) has atrophied, but the institutional machinery remains, maintained by sunk legitimacy and organizational interests rather than demonstrated effectiveness. Theater ratio high because compliance with IMF frameworks is largely performative in the age of floating currencies and capital flows.
constraint_indexing:constraint_classification(brics_payment_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, some form of multilateral settlement mechanism is inherent to interstate trade: the void that BRICS payment systems fill is an irreducible requirement of a multipolar world. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'necessity of settlement coordination' naturalizes what is actually a contestable institutional arrangement. The coordination function is genuine; the specific extraction mechanisms are contingent.
constraint_indexing:constraint_classification(brics_payment_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brics_payment_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brics_payment_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brics_payment_systems, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brics_payment_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brics_payment_systems, TR),
    TR >= 0.70.

:- end_tests(brics_payment_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. BRICS payment systems exhibit genuine coordination function (reducing settlement costs for member states) alongside significant extraction mechanisms. The increase from 0.35 to 0.58 reflects the system's institutional maturation — initial emphasis on coordination (pilot phase) has shifted to embedded rent-extraction (operational phase) as the infrastructure becomes routine. Extraction flows primarily through currency conversion spreads, preferential settlement terms for major participants, and exclusion of smaller currencies from favorable rates. Suppression (0.62): Moderate-high. Significant barriers exist for exit: dollar dependence carries sanctions risk and capacity loss; BRICS system participation requires political alignment and technical interoperability. Smaller participants face suppression through asymmetric information (opaque fee structures) and market power concentration (larger BRICS members set terms). Theater ratio (0.55): Moderate. De-dollarization rhetoric emphasizes payment system as harbinger of multipolar financial architecture; actual settlement capacity remains modest (~2-5% of global trade, concentrated in BRICS + trusted regional partners). The gap between promotional narrative and operational reality reflects performative elements — the system functions as legitimacy mechanism for de-dollarization strategy, not yet as primary alternative settlement network.
 *
 * PERSPECTIVAL GAP:
 *   The verification gap is substantial: BRICS member states perceive the payment system as Tangled Rope (genuine coordination of settlement needs with embedded but justifiable rent-extraction); smaller currency blocs perceive Snare (extraction with minimal choice); USD system perceives Rope (coordination pressure that maintains system redundancy); de-dollarization coalition perceives Scaffold (temporary coordination with sunset as alternatives mature); Bretton Woods legacy perceives Piton (performative relevance maintained through institutional inertia); analytical observer at civilizational scale risks perceiving Mountain (settlement coordination as inherent to multipolar systems) — but structural data reveals this as false summit. The perspectival gap reveals that BRICS payment systems are genuinely contestable institutions: coordination function is real, extraction mechanisms are real, and which dominates depends on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations encode the extraction flow: BRICS member states benefit from coordination function (reduced forex costs, settlement alternatives, sanctions mitigation); smaller currency blocs bear costs (unfavorable conversion rates, exclusion from preferred terms, payment system rent); USD system bears pressure on hegemonic position (marginal trade settlement loss, de-dollarization momentum) but maintains arbitrage through reserve status. The derivation chain (beneficiary + exit → low d → negative chi for beneficiaries; victim + trapped → high d → high chi for victims) produces differential extraction experience. BRICS members experience moderate effective extraction (they benefit from coordination more than they lose to rent-capturing); smaller states experience high effective extraction (rent-capturing with minimal coordination benefit); USD system experiences low effective extraction (pressure on hegemonic position offset by reserve currency arbitrage).
 *
 * MANDATROPHY ANALYSIS:
 *   BRICS payment systems resolve the mandatrophy by decomposing coordination and extraction into structurally distinct mechanisms. The genuine coordination function (reducing settlement costs for legitimate trade) is separable from the rent-extraction mechanism (currency conversion spreads, exclusion rents, geopolitical access control). Different observers prioritize these functions differently based on their structural position: BRICS members emphasize coordination (genuine benefit to them), peripheral states emphasize extraction (real cost to them), analytical observer risks naturalizing the system as inherent to multipolar finance (false summit). The mandatrophy is resolved not by choosing 'which type is correct' but by recognizing that the system exhibits BOTH coordination and extraction, and the classificatory outcome depends on which mechanism dominates from the observer's vantage point. The tangled_rope classification at the primary (powerful/constrained) perspective reflects this hybrid structure: genuine coordination function exists alongside asymmetric extraction, active enforcement is required (member state alignment), and the distinction between coordination benefit and extraction cost is empirically testable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payment_system_network_effects,
    'Do BRICS payment networks exhibit genuine network effects that constitute coordination, or are they primarily mechanisms for rent extraction through currency conversion spreads and fintech monopolies?',
    'Comparative analysis of transaction costs and settlement times: BRICS Pay vs USD correspondent banking vs alternative bilateral arrangements. Measure whether the system reduces costs for low-volume traders or only benefits large participants.',
    'If genuine coordination: classification shifts toward Rope for all perspectives. If primarily extraction: classification shifts toward Snare for peripheral participants, reducing the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payment_system_network_effects, empirical, 'Whether network effects justify classification as coordination vs extraction').

omega_variable(
    brics_member_alignment,
    'Do BRICS member states share sufficient strategic alignment on de-dollarization to maintain unified payment infrastructure, or will individual state interests fragment the system?',
    'Longitudinal analysis of BRICS meeting outcomes and actual implementation commitment; correlation between de-dollarization rhetoric and actual bilateral payment flows; measurement of defection risk (Brazil, India exploring alternative partners).',
    'If alignment holds: scaffold sunset is structural (alternative systems mature on schedule). If alignment fragments: system becomes extractive theatre maintaining legitimacy despite functional failure (shifts toward Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brics_member_alignment, empirical, 'Structural alignment among BRICS members on payment system maintenance').

omega_variable(
    dollar_replacement_timeline,
    'What is the realistic timeline for alternative currencies (RMB, digital settlement, commodity-backed mechanisms) to replace USD in 50%+ of global trade settlement?',
    'Extrapolation from current de-dollarization rates, central bank reserve rebalancing patterns, and adoption curves for alternative settlement mechanisms; scenario modeling with 5-, 10-, 20-year horizons.',
    'If timeline < 15 years: BRICS system is transitional (scaffold classification holds). If timeline > 30 years or indefinite: BRICS system is becoming institutional (shifts toward Tangled Rope with extended horizon).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dollar_replacement_timeline, empirical, 'Timeline for meaningful USD trade settlement replacement').

omega_variable(
    sanctions_enforcement_mechanism,
    'To what extent does the BRICS payment system''s extractive capacity depend on its role as a sanctions-evasion mechanism, and how much is coordination-genuine?',
    'Analysis of payment flows: which portion serves legitimate trade coordination vs sanctions circumvention. Measurement of correlation between BRICS payment volume and Western sanctions regime intensity.',
    'If primarily sanctions evasion: extraction vector is enforcement against US-designated entities, and suppression mechanisms become secondary (coercive). If primarily coordination: extraction vector is more structural (rent-capturing). Different mechanisms imply different sunset timelines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctions_enforcement_mechanism, empirical, 'Role of sanctions evasion in BRICS payment system sustainability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brics_payment_systems, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brics_tr_t0, brics_payment_systems, theater_ratio, 0, 0.4).
narrative_ontology:measurement(brics_tr_t3, brics_payment_systems, theater_ratio, 3, 0.48).
narrative_ontology:measurement(brics_tr_t6, brics_payment_systems, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(brics_be_t0, brics_payment_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(brics_be_t3, brics_payment_systems, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(brics_be_t6, brics_payment_systems, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brics_payment_systems, resource_allocation).
narrative_ontology:affects_constraint(brics_payment_systems, usd_hegemonic_settlement).
narrative_ontology:affects_constraint(brics_payment_systems, international_sanctions_evasion).
narrative_ontology:affects_constraint(brics_payment_systems, emerging_market_currency_stability).

% DUAL FORMULATION NOTE:
% BRICS payment systems are downstream of geopolitical de-dollarization strategy and upstream of specific currency coordination mechanisms (RMB settlement, digital currency adoption, bilateral arrangements). The payment system constraint has its own extractiveness (0.58) reflecting institutional maturity and embedded rent-extraction, distinct from the parent de-dollarization strategy constraint (which exhibits lower extractiveness reflecting genuine coordination motivation) and the child currency coordination constraints (which may exhibit higher or lower extractiveness depending on bilateral vs multilateral mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brics_payment_systems, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
