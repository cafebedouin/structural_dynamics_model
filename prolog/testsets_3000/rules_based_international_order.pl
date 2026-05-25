% ============================================================================
% CONSTRAINT STORY: rules_based_international_order
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The rules-based international order emerged from the post-WWII settlement
 *   as a coordination mechanism: institutions (UN, WTO, Bretton Woods, NATO)
 *   and norms that enabled global commerce, reduced great-power conflict, and
 *   provided frameworks for dispute resolution. This constraint exhibits the
 *   defining characteristic of Tangled Rope — it performs genuine
 *   coordination functions while simultaneously embedding the power
 *   asymmetries of its designers. The hegemon and allies benefit from both
 *   the coordination gains and from rules written to preserve their
 *   advantages. Weak states are trapped within a system that promises neutral
 *   rule-based governance but systematically enforces rules asymmetrically.
 *   Rising middle powers are constrained by rules designed before their
 *   emergence. The mandatrophy is resolved by recognizing that both the
 *   coordination function and the asymmetric extraction are real and
 *   structural, not contradictory.
 *
 * KEY AGENTS:
 *   - Hegemon (US) and Allied Bloc: Primary beneficiary (institutional/arbitrage) — designed the rules, can modify or ignore them at strategic convenience. Benefits from both coordination and power-preservation dimensions.
 *   - Rule-Taker Weak States: Primary victim (powerless/trapped) — must follow rules they did not write, lack exit options, bear full cost of asymmetric enforcement. No ability to arbitrage or exit.
 *   - Rising Middle Powers: Secondary victim (moderate/constrained) — harmed by rules designed before their ascent but cannot exit entirely. Some benefits from trade liberalization but constrained by technology controls, financial restrictions. Can form coalitions but cannot change fundamental rule structure.
 *   - Global Trade Losers: Tertiary victim (moderate/constrained) — workers and communities in developed economies losing to manufacturing relocations; benefit from cheap imports but bear concentrated adjustment costs. Constrained by rule-based trade commitments.
 *   - Multilateral Institutions (WTO, IMF, UN): Quasi-institutional (institutional/constrained) — designed to enforce the order but increasingly seen as theater. Theater ratio rising as power distribution shifts but institutions are not reformed. Scaffold perspective: they are supposed to sunset and be replaced by more representative structures.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — must model both the genuine coordination function and the genuine asymmetric extraction without collapsing one into the other.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rules_based_international_order, 0.58).
domain_priors:suppression_score(rules_based_international_order, 0.65).
domain_priors:theater_ratio(rules_based_international_order, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rules_based_international_order, extractiveness, 0.58).
narrative_ontology:constraint_metric(rules_based_international_order, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rules_based_international_order, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rules_based_international_order, tangled_rope).
narrative_ontology:human_readable(rules_based_international_order, "The Rules-Based International Order").
narrative_ontology:topic_domain(rules_based_international_order, "political/economic").

domain_priors:requires_active_enforcement(rules_based_international_order).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rules_based_international_order, hegemon_military_alliance).
narrative_ontology:constraint_beneficiary(rules_based_international_order, rules_architecture_designers).
narrative_ontology:constraint_victim(rules_based_international_order, rule_takers_weaker_states).
narrative_ontology:constraint_victim(rules_based_international_order, non_aligned_actors).
narrative_ontology:constraint_victim(rules_based_international_order, global_trade_losers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RULE-TAKER WEAK STATE (SNARE) — Small nations have no exit from the rules-based order. Trapped by resource dependencies, debt structures, and lack of military alternatives. Rules appear neutral but embed advantages of rich/powerful states. Maximum extraction experience: must absorb external shocks (financial crises, sanctions, trade restrictions) with no ability to write or enforce rules.
constraint_indexing:constraint_classification(rules_based_international_order, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RISING MIDDLE POWER (TANGLED ROPE) — Constrained by existing power distribution and rule-set designed before their rise. Benefits from some aspects of the order (trade liberalization for manufactured exports) but harmed by others (technology export controls, financial sanctions threats). Can organize coalitions (BRICS, regional blocs) but cannot exit entirely. Mixed coordination-extraction dynamic: the rules partly enable their integration, partly constrain their ascent.
constraint_indexing:constraint_classification(rules_based_international_order, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEGEMON AND ALLIED BLOC (ROPE) — Designed and enforces the rules-based order. Primary beneficiary. Experiences the constraint as coordination mechanism enabling global commerce, alliance management, and strategic advantage. Can arbitrage between rules (invoking or suspending them based on strategic interest). Net benefit from both coordination function and ability to extract exceptions. Low experienced extraction because the rules are aligned with their interests.
constraint_indexing:constraint_classification(rules_based_international_order, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTILATERAL INSTITUTION COMPLEX (SCAFFOLD) — WTO, IMF, World Bank, UN system represent a transitional coordination framework. Designed post-WWII with sunset logic: as global actors gained power (China, India, Brazil), institutions were supposed to become more representative. But power rebalancing has been resisted, creating a scaffold that is increasingly theatrical but not yet replaced. The institutions' theater ratio is high (diplomatic ritual, consensus theater, implementation theater) because their enforcement power is declining. A genuine sunset would involve reform to reflect new power distribution or replacement by regional orders.
constraint_indexing:constraint_classification(rules_based_international_order, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LIBERAL INTERNATIONAL LAW IDEAL (PITON) — The normative claim that a rule-based order constrains the powerful and protects the weak through law has substantially atrophied. Enforcement is selective (sanctions only against weaker actors; powerful states ignore decisions they dislike). The legal architecture persists through institutional inertia and rhetorical appeal ('rules-based order' language used by all parties) but its functional constraint on power has diminished. Theater is high: continued invocation of the 'rules-based order' even as states systematically violate or redefine rules. A piton maintained by performance and path dependency rather than structural force.
constraint_indexing:constraint_classification(rules_based_international_order, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the rules-based order is fundamentally hybrid: it genuinely solves coordination problems (global supply chains, avoided nuclear wars during Cold War, multilateral trade gains) but simultaneously embeds and enforces the power asymmetries that created it. The coordination function is real but asymmetric. The extraction is real but enabled by the coordination framework. Both functions must be modeled. The system is neither a pure rope (benefits flow broadly) nor a pure snare (pure extraction). It is tangled because the beneficiaries use the coordination function itself as an extraction mechanism.
constraint_indexing:constraint_classification(rules_based_international_order, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rules_based_international_order_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rules_based_international_order, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rules_based_international_order, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high and rising. The order extracts value from weak actors (forced compliance with rules they cannot change, asymmetric enforcement of sanctions, restrictions on technology and capital flows) while enabling coordination gains that concentrate with powerful actors. The measurement shows extractiveness rising from 0.32 to 0.58 over the 50-year interval, reflecting increasing violations of the 'rules-based' ideal as power distributions shift and the order becomes less representative. Suppression (0.65): Moderate-high. Significant barriers to exit include financial dependency structures (IMF conditionality, debt traps), military alliance dependencies, and lack of alternative institutions with comparable scope. Smaller states face extreme suppression (sanctions threats, investment freezes, military encirclement). Suppression is not total — some actors (BRICS, regional blocs) have created partial alternatives, but exit costs remain very high for most weak states. Theater ratio (0.68): Moderate-high and rising. The diplomatic ritual surrounding the 'rules-based order' has increasingly become theater: powerful states invoke rules selectively, rules are reinterpreted unilaterally (China tariff wars, COVID vaccine export controls, sanctions on major economies), and institutional processes (UN Security Council votes, WTO dispute resolution) are performative because enforcement depends on power, not rules. Theater has risen from 0.35 to 0.68 as the order's legal authority has declined relative to power dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The rules-based order presents extreme perspectival divergence. The hegemon sees coordination (Rope) — they designed it, benefit from it, can work within it or modify it as needed. Weak states see pure extraction (Snare) — trapped by rules they cannot change, enforcement is asymmetric, no exit options. Rising powers see mixed extraction and coordination (Tangled Rope) — the rules partly enable their integration into global markets but constrain their military/technological ascent. The liberal international law ideal (Piton) sees a degraded version of its own aspirations — the law was supposed to constrain power equally but has become theater as power openly violates or reinterprets rules. The multilateral institutions (Scaffold) see their own obsolescence looming — designed for a world of five permanent powers; now struggling to remain relevant as that distribution has shifted, with no formal reform mechanism. The analytical observer (Tangled Rope) recognizes that all these perspectives are correct simultaneously: the order genuinely coordinates AND genuinely extracts, asymmetrically, by design.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position in the order. Beneficiaries with arbitrage options (hegemon, allies) have low d: they can invoke or ignore rules, shape their meaning, and capture coordination gains. Trapped victims (weak states) have high d: they must follow rules, cannot negotiate exceptions, and absorb costs of enforcement. Constrained middle powers have moderate d: they benefit from some rules but are harmed by others, can organize coalitions but cannot change the fundamental structure. The piton classification for the liberal ideal derives from theater ratio (0.68) exceeding functionality — the normative claim that law constrains power persists through rhetoric even as states routinely violate rules without consequences proportional to their power. The analytical perspective recognizes that d varies across actors, making the order a genuine hybrid: coordination + extraction cannot be separated.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves the mandatrophy by modeling both coordination and extraction as real structural functions of the same system. The false choice between 'the order is a coordination mechanism' (Rope interpretation) and 'the order is a power-preservation device' (Snare interpretation) is resolved by recognizing that it is both simultaneously, with asymmetric benefits. The beneficiaries (hegemon, allies) experience primarily coordination function (Rope perspective). The victims (weak states) experience primarily extraction (Snare perspective). The analytical observer must model both at once (Tangled Rope). The extractiveness value (0.58) reflects that asymmetric enforcement and rule-writing advantages constitute meaningful extraction, while coordination gains (avoided wars, global supply chains, reduced transaction costs) are real. The theater ratio (0.68) reflects that the order's legitimacy increasingly depends on rhetorical invocation ('rules-based order') of an ideal that is not empirically matched by symmetric enforcement. The rising theater and rising extractiveness over the interval indicate Goodhart drift: as power distributions shifted and the order became less representative, states responded by either exiting (partial, via regional alternatives) or by maintaining theater while ignoring rules (major power behavior in Ukraine, Taiwan, Arctic). The piton classification for the liberal ideal and scaffold classification for the institutional framework both reflect this degradation: the order is increasingly maintained by performance (continuing to call it rules-based) rather than by structural enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_vs_predatory_rule_usage,
    'When does invoking rules constitute enforcement versus when does selective application constitute predatory extraction?',
    'Longitudinal analysis of sanctions/trade enforcement patterns: comparison of enforcement rates for similar violations across power tiers; documentation of rule changes made unilaterally vs through consensus',
    'If enforcement is roughly symmetric: the order is primarily coordination-plus-fairness (Rope from more perspectives). If enforcement is systematically asymmetric: the order is primarily power-preservation (Snare/Tangled Rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_vs_predatory_rule_usage, empirical, 'Whether rule enforcement is symmetric or systematically biased toward powerful states').

omega_variable(
    alternative_order_viability,
    'Are emerging alternative orders (Belt and Road, BRICS institutions, regional trade blocs) structurally different constraint systems or merely weakened versions of the existing order?',
    'Comparative analysis: do alternative institutions encode different power distributions, different beneficiary/victim structures, or do they replicate the same extraction patterns with different actors? Measurement of rule-following vs rule-bending in each system.',
    'If alternative orders are structurally different: the scaffold perspective is validated — sunset is real, replacement is underway. If they replicate existing patterns: the order is more robust/durable than the scaffold suggests; the replacements are not functional alternatives but weaker copies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_order_viability, empirical, 'Whether emerging alternative orders are functionally different or merely parallel systems').

omega_variable(
    coordination_gains_distribution,
    'How are the gains from global coordination (reduced trade barriers, avoided great-power conflict) distributed between powerful and weak actors?',
    'Economic analysis: comparison of GDP growth, trade participation gains, and conflict avoidance benefits across power tiers; measurement of who captures the coordination surplus vs who bears adaptation costs',
    'If gains are broadly distributed: the order is Rope from many perspectives. If gains concentrate with powerful states and weak states absorb most costs: the order is Snare/Tangled Rope. This resolves the mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_gains_distribution, empirical, 'Distribution of coordination gains across power tiers').

omega_variable(
    rule_creation_barriers,
    'Can non-hegemonic actors successfully propose and implement new rules, or do all significant institutional innovations originate from the hegemon and allied powers?',
    'Historical documentation of rule-change proposals: which came from weaker states, which were adopted, which were blocked or reinterpreted; analysis of voting power distribution vs actual influence in rule-making forums',
    'If rule creation is open: the order enables voice for all (closer to Rope). If rule creation is effectively closed to non-powerful actors: the order is a power-preservation mechanism (closer to Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rule_creation_barriers, empirical, 'Whether rule-making is accessible to non-hegemonic actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rules_based_international_order, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rules_based_international_order, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rbio_tr_t25, rules_based_international_order, theater_ratio, 25, 0.52).
narrative_ontology:measurement(rbio_tr_t50, rules_based_international_order, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rules_based_international_order, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rbio_be_t25, rules_based_international_order, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(rbio_be_t50, rules_based_international_order, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rules_based_international_order, global_infrastructure).
narrative_ontology:affects_constraint(rules_based_international_order, trade_rule_asymmetry).
narrative_ontology:affects_constraint(rules_based_international_order, sanctions_regime_selectivity).
narrative_ontology:affects_constraint(rules_based_international_order, currency_hegemony).
narrative_ontology:affects_constraint(rules_based_international_order, technology_export_controls).
narrative_ontology:affects_constraint(rules_based_international_order, debt_dependency_traps).

% DUAL FORMULATION NOTE:
% The rules-based order is a constraint family decomposable into multiple structural claims: (1) enforcement of trade rules by WTO, (2) financial conditionality by IMF/World Bank, (3) security guarantees by NATO/bilateral alliances, (4) technological access controls by US-led regimes. Each sub-claim has its own extractiveness value and affected actors. The parent constraint captures the overarching architectural feature — that all these mechanisms share a common power distribution — while child constraints in the affects_constraints array model domain-specific extraction mechanisms. Rising extractiveness in the parent (0.32→0.58) correlates with rising asymmetry in enforcement across child constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rules_based_international_order, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
