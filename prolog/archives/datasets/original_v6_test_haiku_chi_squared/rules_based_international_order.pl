% ============================================================================
% CONSTRAINT STORY: rules_based_international_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rules_based_international_order, []).

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
 *   constraint_id: rules_based_international_order
 *   human_readable: The Rules-Based International Order
 *   domain: political/economic
 *
 * SUMMARY:
 *   The rules-based international order comprises the institutional
 *   architecture (United Nations, World Trade Organization, International
 *   Monetary Fund, World Bank, NATO) and associated norms established
 *   primarily by the United States and Western allies after 1945. Nominally,
 *   it provides predictability, dispute resolution, and coordination
 *   mechanisms for global commerce and security. Structurally, however, it
 *   encodes the power distributions of its founding moment: permanent
 *   Security Council seats for 1945 victors, IMF/World Bank governance
 *   reflecting 1944 contributions, GATT/WTO rules designed around Cold War US
 *   advantages. As power has shifted (rise of China and India, emergence of
 *   new regional powers), the order has become increasingly extractive for
 *   excluded actors while maintaining its coordination functions for the
 *   original beneficiaries. The constraint exhibits all six classification
 *   types from different structural perspectives, revealing that the same
 *   institutional architecture functions as pure coordination for some
 *   actors, pure extraction for others, and a complex hybrid for those
 *   straddling inclusion/exclusion boundaries. The theater ratio (0.68)
 *   reflects that much multilateral activity (UN votes, WTO panel procedures,
 *   IMF board meetings) is substantially performative while actual
 *   enforcement mechanisms (US sanctions authority, dollar dominance, NATO
 *   military capacity) operate outside institutional processes.
 *
 * KEY AGENTS:
 *   - The United States and NATO Allies: Primary beneficiary (institutional/arbitrage) — rule-setters with de facto veto power; benefit from dollar hegemony, intellectual property enforcement, security guarantees; can violate rules with limited consequence
 *   - Rising Powers (China, India, Russia): Mixed victim and challenger (powerful/mobile) — nominally integrated into WTO/UN but excluded from governance; bear extraction through IP restrictions, currency controls, sanctions; building alternative institutional pathways
 *   - Developing Nations and Global South: Primary victims (powerless/trapped) — subject to IMF conditionality, structural adjustment programs, debt obligations, trade restrictions; no exit options without sovereign default or institutional rupture
 *   - Multilateral Institutions (UN, WTO, IMF, World Bank): Institutional intermediaries (institutional/constrained) — nominally sovereign but functionally degraded; maintain performative processes while lacking enforcement capacity independent of hegemonic power
 *   - Global South Coalition (G77+China, Non-Aligned Movement, BRICS): Organized challengers (organized/mobile) — building alternative institutions and institutional pathways with explicit exit logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the order as inherent to international politics rather than as contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rules_based_international_order, 0.55).
domain_priors:suppression_score(rules_based_international_order, 0.65).
domain_priors:theater_ratio(rules_based_international_order, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rules_based_international_order, extractiveness, 0.55).
narrative_ontology:constraint_metric(rules_based_international_order, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rules_based_international_order, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rules_based_international_order, tangled_rope).
narrative_ontology:human_readable(rules_based_international_order, "The Rules-Based International Order").
narrative_ontology:topic_domain(rules_based_international_order, "political/economic").

domain_priors:requires_active_enforcement(rules_based_international_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rules_based_international_order, hegemonic_power_usa).
narrative_ontology:constraint_beneficiary(rules_based_international_order, allied_advanced_economies).
narrative_ontology:constraint_beneficiary(rules_based_international_order, institutional_governance_apparatus).
narrative_ontology:constraint_victim(rules_based_international_order, rising_powers_excluded_from_rule_setting).
narrative_ontology:constraint_victim(rules_based_international_order, developing_nations_debt_obligations).
narrative_ontology:constraint_victim(rules_based_international_order, global_south_trade_restrictions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATION BORROWER (SNARE) — Trapped within IMF conditionality frameworks, World Bank loan requirements, and structural adjustment programs. Cannot exit without sovereign default costs. Bears extraction through austerity mandates, privatization demands, and currency devaluation. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.91.
constraint_indexing:constraint_classification(rules_based_international_order, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RISING POWER (TANGLED ROPE) — Nominally integrated into WTO/UN frameworks but systematically excluded from rule-making (permanent Security Council seats, IMF governance). Experiences coordination benefits (trade access, investment certainty) but also extraction (intellectual property enforcement, currency restrictions, tech sanctions). Mobile exit option (Belt and Road alternatives, BRICS institutional building) reduces effective extraction. d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(rules_based_international_order, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: US-ALLIED CORE (ROPE) — Primary beneficiary and rule-setter. NATO, dollar reserve status, institutional gatekeeping enable arbitrage: benefit from rule enforcement while maintaining escape clauses (e.g., US can violate WTO rules with limited consequence). Experiences the order as pure coordination mechanism. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(rules_based_international_order, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UN AND MULTILATERAL INSTITUTIONS (PITON) — Nominally sovereign decision-making bodies but functionally degraded: Security Council is deadlocked by veto power, IMF/World Bank governance reflects 1944 power distributions, WTO dispute resolution is increasingly ignored. Theater_ratio=0.68 reflects that institutional processes (committee meetings, consensus-building, voting) are substantially performative while actual power flows through bilateral enforcement. Institutions persist through inertia and because no unified alternative has emerged. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.49.
constraint_indexing:constraint_classification(rules_based_international_order, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL SOUTH COALITION (SCAFFOLD) — Organized actors (G77+China, Non-Aligned Movement, BRICS) are building alternative institutions (New Development Bank, Regional Comprehensive Economic Partnership, AfCFTA) with explicit sunset logic: these alternatives are temporary scaffolding supporting exit from the rules-based order. Coordination benefits exist (trade negotiations) but are paired with intentional institutional bypass. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.30.
constraint_indexing:constraint_classification(rules_based_international_order, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, some degree of hegemonic order is seen as inherent to international politics: power vacuums cannot exist, coordination requires a dominant power, rules require enforcement power. This view naturalizes the current order as inevitable. However, the structural data (ε=0.55, suppression=0.65, beneficiary/victim declarations) contradicts mountain classification — the engine will identify this as a false summit, revealing that what appears as natural law is actually contingent institutional design favoring specific actors.
constraint_indexing:constraint_classification(rules_based_international_order, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rules_based_international_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rules_based_international_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rules_based_international_order, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rules_based_international_order, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rules_based_international_order, TR),
    TR >= 0.70.

:- end_tests(rules_based_international_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The order imposes significant constraints on rising and developing nations (technology transfer requirements, currency restrictions, IP enforcement asymmetries) while enabling unilateral exit for the hegemon. The extraction is not maximal (0.70+) because alternative institutions are emerging and some developing nations have sufficient scale to negotiate outside the framework. The value reflects that extraction is real and growing (measurement shows progression from 0.35 to 0.55 over the interval) but not yet total. Suppression (0.65): Moderate-high. Exit costs are substantial: countries cannot simply abandon WTO access, dollar-denominated trade, or IMF-contingent lending without economic shock. However, suppression is declining as alternatives mature. The value reflects that while exit is costly, it is possible (BRICS exists, RMB is internationalized, regional trade pacts operate). Theater ratio (0.68): High. UN voting procedures, WTO dispute resolution, IMF board meetings are largely performative. Actual enforcement comes through bilateral US pressure, sanctions authority under US law (OFAC), and dollar hegemony outside institutional processes. The theater has increased over time as institutional authority has declined relative to hegemonic enforcement (measured progression from 0.45 to 0.68 reflects this drift).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental perspectival divergence between structural positions. The US-allied core sees a Rope (coordination mechanism for managing global commerce and security) with significant escape clauses (withdrawal from TPP, JCPOA, WTO appellate body, Paris Agreement). Rising powers see a Tangled Rope (coordination benefits from trade access and investment certainty paired with extraction through exclusion from rule-setting and enforcement mechanisms). Developing nations see a Snare (structural coercion through debt obligations, conditionality, and zero exit options). The Global South coalition sees a Scaffold with a real sunset (BRICS, Belt and Road, regional pacts are intentional alternatives designed to provide redundancy and lower the hegemon's unilateral power). Multilateral institutions see themselves as Piton (performing governance functions through committees and procedures while lacking independent enforcement capacity — the performance persists through inertia and because no unified alternative has fully emerged). The analytical observer risks seeing a Mountain (international order requires hegemonic enforcement; power vacuums cannot exist; some degree of hegemonic extraction is inevitable). The engine's false summit detector will flag this — the order is contingent institutional design, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   US-allied core: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; institutional rule-setting and enforcement authority enable escape clauses (withdraw from frameworks, block appellate panels, trigger national security exceptions). Rising powers: Victim + mobile → d≈0.65, f(d)≈0.95. Significant extraction; bear costs of IP enforcement, technology restrictions, currency limits, but have sufficient scale and alternatives to partially exit (BRICS, Belt and Road, RMB internationalization). Developing nations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no viable alternative to IMF financing, dollar-trade, or WTO access without default or destitution. Multilateral institutions: Institutional + constrained → d≈0.55, f(d)≈0.75. Trapped between nominal sovereignty and functional dependence on hegemon enforcement; cannot exit without losing legitimacy, cannot enforce without hegemon backing. Global South coalition: Organized + mobile → d≈0.45, f(d)≈0.45. Low effective extraction despite nominal inclusion in order; coalition has agency and is actively building alternatives (BRICS infrastructure, Belt and Road, regional trade pacts). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is observer-dependent; engine identifies as false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by demonstrating that the rules-based order functions simultaneously as coordination mechanism (Rope for beneficiaries), extraction framework (Snare for powerless nations), institutional hybrid (Tangled Rope for rising powers), performative degradation (Piton for UN/WTO), temporary scaffold for exit (Scaffold for Global South coalition building), and false natural law (Mountain from analytical view). The classification is not unified because the structural reality is not unified: the order IS coordination for those who set the rules and ARE extracted from those excluded from rule-setting. The apparent contradiction dissolves when we recognize that the constraint encodes asymmetric power into institutional form. The order's durability does not depend on all perspectives agreeing it is beneficial — it depends on the hegemon and allied core perceiving sufficient benefit (Rope) to maintain enforcement and on alternative institutions not yet providing credible exit for developing nations (Snare persistence despite theater rising). The theater increase (0.45 → 0.68) signals Piton-direction drift: as actual enforcement increasingly relies on bilateral US action (sanctions, dollar control) rather than institutional process, the institutional performances lose functional content. The classification remains Tangled Rope for the order as a whole because extraction coexists with real coordination functions — the hybrid classification is structurally accurate, not a failure to decide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_compliance_mechanism,
    'Do countries comply with rules-based order mechanisms (WTO, UN resolutions, IMF mandates) because the rules are legitimate/efficient or because enforcement (sanctions, exclusion) makes exit costlier than compliance?',
    'Correlation analysis of compliance rates with enforcement capacity across issue-areas; study of voluntary compliance during periods of enforcement breakdown; comparison of compliance for rules that benefit vs harm compliant nation',
    'If legitimacy-driven: order is Rope. If enforcement-driven: order is Snare/Tangled Rope. This omega determines whether the order has genuine coordination function or pure extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_mechanism, empirical, 'Whether compliance is voluntary or coerced').

omega_variable(
    alternative_order_viability,
    'Can alternative multilateral orders (BRICS, Belt and Road, regional arrangements) actually provide the coordination functions (dispute resolution, trade access, investment certainty) that the rules-based order provides, or do they merely redistribute extraction?',
    'Comparative analysis of institutional effectiveness: dispute resolution speed/fairness, trade dispute outcomes, investment protection enforcement across orders; longitudinal tracking of whether alternatives reduce extraction for members vs merely changing extractors',
    'If alternatives are viable: Scaffold/Piton perspectives confirmed — sunset is real. If alternatives fail: Scaffold is aspirational; the order''s extraction mechanism persists because alternatives cannot deliver coordination functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_order_viability, empirical, 'Whether alternative orders can replicate coordination functions').

omega_variable(
    hegemon_structural_dependency,
    'Does the US actually benefit from maintaining the rules-based order, or does the order constrain US power relative to unilateral action?',
    'Analysis of US policy: instances of violation/exit from rules (TPP withdrawal, JCPOA exit, WTO appellate body blocking); comparison of US gains under rules vs hegemonic alternatives; cost accounting of enforcing rules on allies vs freedom of action under hegemony',
    'If US gains exceed costs: order is stable Rope for hegemon. If unilateral extraction exceeds order benefits: hegemon sees order as constraint; order''s classification flips from Tangled Rope to Snare for the hegemon despite being Rope for beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hegemon_structural_dependency, empirical, 'Whether hegemon benefits from maintaining rules-based order').

omega_variable(
    institutional_performance_vs_theater,
    'What fraction of multilateral institution activity is functional (resolves disputes, enforces agreements, enables trade) vs performative (statements, committees, symbolic actions)?',
    'Process analysis of UN, WTO, IMF: track percentage of resolutions actually implemented; measure dispute resolution effectiveness; compare institutional processes to actual enforcement mechanisms (US veto, bilateral pressure, sanctions)',
    'If theater > 0.70: Piton classification confirmed for institutions. If theater < 0.40: institutions are functional and may be better classified as Rope/Tangled Rope depending on distribution of benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_performance_vs_theater, empirical, 'Functional vs performative content of multilateral institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rules_based_international_order, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rules_based_international_order, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rbio_tr_t25, rules_based_international_order, theater_ratio, 25, 0.58).
narrative_ontology:measurement(rbio_tr_t50, rules_based_international_order, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rules_based_international_order, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rbio_be_t25, rules_based_international_order, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(rbio_be_t50, rules_based_international_order, base_extractiveness, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rules_based_international_order, enforcement_mechanism).
narrative_ontology:affects_constraint(rules_based_international_order, us_dollar_hegemony).
narrative_ontology:affects_constraint(rules_based_international_order, intellectual_property_regime).
narrative_ontology:affects_constraint(rules_based_international_order, wto_trade_dispute_resolution).
narrative_ontology:affects_constraint(rules_based_international_order, imf_structural_adjustment).
narrative_ontology:affects_constraint(rules_based_international_order, un_security_council_veto).

% DUAL FORMULATION NOTE:
% The rules-based international order is downstream of specific institutional mechanisms (WTO, IMF, UN) but represents a unified structural constraint on state behavior. Each component (trade rules, financing conditions, security arrangements) has its own ε and can be analyzed separately (e.g., IP regime as pure extraction, WTO as mixed coordination-extraction). The overarching order story integrates these components' structural relationship: they function collectively to enable hegemon benefit while constraining alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rules_based_international_order, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
