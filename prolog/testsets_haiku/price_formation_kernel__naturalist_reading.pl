% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Price Formation as Natural Equilibrium (Naturalist Reading)
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   This constraint story instantiates the naturalist reading of price
 *   formation: the claim that prices in decentralized markets naturally
 *   equilibrate at levels reflecting objective scarcity and preference. Under
 *   this reading, price formation is a property of reality, not an
 *   institutional construction or extraction mechanism. There are no
 *   beneficiaries, no victims, no active enforcement — price discovery is
 *   self-sustaining and requires no party to maintain it. Policy
 *   interventions that attempt to set prices administratively are claimed to
 *   disrupt the natural process and produce deadweight loss. This is ONE
 *   reading of a contested kernel; sibling readings (institutional, georgist,
 *   financialization) decompose price formation into constructed mechanisms,
 *   distributional asymmetries, and credit-driven dynamics respectively. The
 *   kernel contest is documented in omega variables; this story claims the
 *   naturalist pole and maintains it independently of the other readings.
 *
 * KEY AGENTS:
 *   - No stakeholders: this reading treats price formation as a natural law, not an arrangement with agents who benefit or bear costs. Agents participate in price formation, but the constraint itself is not a human construction they maintain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'e5ff4bec-b10e-4894-8612-7345b1774e4d').
narrative_ontology:cs_kernel_codification('e5ff4bec-b10e-4894-8612-7345b1774e4d', distributed).
narrative_ontology:cs_authority_grounding('e5ff4bec-b10e-4894-8612-7345b1774e4d', expertise).
narrative_ontology:cs_interpretation_layer_present('e5ff4bec-b10e-4894-8612-7345b1774e4d').
narrative_ontology:cs_reading_relation('e5ff4bec-b10e-4894-8612-7345b1774e4d', price_formation_kernel__institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('e5ff4bec-b10e-4894-8612-7345b1774e4d', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5ff4bec-b10e-4894-8612-7345b1774e4d', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('e5ff4bec-b10e-4894-8612-7345b1774e4d', foundational, price_equilibrium_exists_and_is_unique).
narrative_ontology:cs_axiom_status(price_equilibrium_exists_and_is_unique, holdable).
narrative_ontology:cs_axiom_grounding('e5ff4bec-b10e-4894-8612-7345b1774e4d', price_equilibrium_exists_and_is_unique, empirically_contingent).
narrative_ontology:cs_axiom('e5ff4bec-b10e-4894-8612-7345b1774e4d', foundational, decentralized_preference_aggregation_via_price_signal).
narrative_ontology:cs_axiom_status(decentralized_preference_aggregation_via_price_signal, holdable).
narrative_ontology:cs_axiom_grounding('e5ff4bec-b10e-4894-8612-7345b1774e4d', decentralized_preference_aggregation_via_price_signal, instrumental).
narrative_ontology:cs_reference_frame('e5ff4bec-b10e-4894-8612-7345b1774e4d', decentralized_price_discovery_equilibrium).
narrative_ontology:cs_drift_state('e5ff4bec-b10e-4894-8612-7345b1774e4d', contemporary_financialized_housing, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5ff4bec-b10e-4894-8612-7345b1774e4d', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Price formation coordinates decentralized knowledge of scarcity and preference without central direction: each transaction reveals information about supply and demand that propagates through the price signal, allowing efficient allocation without a planner.
% TRANSFER_FUNCTION: No transfer; price is discovery of equilibrium, not extraction.
% ABSENT_VOICES: None — this reading treats price formation as a property of reality, not a negotiated arrangement with constituencies.
% DISAPPEARANCE_RATIONALE: Price formation is a structural property of resource allocation. If the mechanism disappeared, it would re-emerge: agents making voluntary exchanges under scarcity would naturally discover equilibrium prices. The constraint is not an arrangement anyone maintains; it is a fact about how decentralized systems work.
% FOUNDING_PROBLEM: How do decentralized actors coordinate resource allocation without central authority or complete information?
% FOUNDING_PROBLEM_CORROBORATION: Adam Smith (Wealth of Nations, invisible hand); modern general equilibrium theory (Walras, Arrow-Debreu); empirical microeconomics documenting price discovery in markets with minimal regulation (commodity markets, financial exchange benchmarks). Corroboration comes from economists and market data outside any party seeking special benefit from the reading.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero: a natural equilibrium process transfers nothing to any party; it is purely discovery. Suppression is zero: the constraint requires no coercion to maintain; it re-emerges whenever agents exchange under scarcity. Theater ratio is zero: there is no performative component; the process either works or fails to equilibrate, with no gap between ostensible and actual function. Accessibility collapse is extremely high (0.95): once a decentralized market is understood via price signals, alternative coordination mechanisms (barter, central planning, direct allocation) are practically inaccessible without the signal structure. Resistance is near zero (0.02): the only 'resistance' to price discovery is the transactional friction of finding counterparties, which is a practical cost, not ideological opposition to the constraint itself. The measurement series is flat across all time points: the naturalist reading claims price formation is invariant — it works the same way in medieval markets, 18th-century agriculture, and contemporary housing, and it works the same way in boom and bust periods. If this flat series proves false empirically (if extractiveness accumulates, if theater rises, if the process fails to equilibrate), that falsification is data against the naturalist reading, not against the measurement discipline.
 *
 * PERSPECTIVAL GAP:
 *   The naturalist reading predicts no perspectival gap: price equilibrium is the same phenomenon from every seat. A landlord and a tenant both face the same equilibrium price; their distributional positions differ, but the price itself is not constructed for their benefit. Contrast this with a tangled_rope or snare reading, where different seats would compute as coordinated vs. target depending on power and exit. The absence of perspectival gap is diagnostic: if empirical research reveals that agents at different power levels experience the price system differently (e.g., institutional investors have information advantages that retail buyers do not), that finding contradicts the naturalist reading and supports the institutional or financialization reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable — no stakeholders are authored because the naturalist reading denies that agents are positioned as beneficiaries or victims of a price equilibrium. Price is a fact about scarcity and preference, not a constructed transfer. If a question arises about directionality (e.g., 'housing prices have risen faster than wages, benefiting landlords and harming renters'), that question points toward an institutional or georgist reading, not the naturalist reading. Directionality logic belongs to readings where human choice constructs an arrangement; it does not apply to a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable. Mandatrophy arises when a constraint's original function atrophies but the constraint persists due to institutional inertia. The naturalist reading claims price formation HAS no function beyond discovery itself — it is not built to solve a problem that could become obsolete. The founding problem (coordinating decentralized actors under scarcity) is claimed to be permanently live, not a historical condition that might be superseded. If price formation did become theatre (if the process ceased to equilibrate but was maintained performatively by authorities), that would be falsification of the naturalist reading, not mandatrophy — it would be evidence that price is constructed and maintained by institutional choice, supporting a different reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_construction,
    'Is price formation a natural law of decentralized exchange, or is every observable price the output of constructed institutional frameworks (zoning, lending standards, tax treatment, informational access)?',
    'Comparative institutional analysis: examine price formation in markets with different regulatory architectures (e.g., free land sales vs. zoned jurisdictions, peer-to-peer lending vs. regulated banking, transparent exchanges vs. opaque intermediaries). If the institutional framework changes but price discovery mechanisms persist, naturalist reading holds; if prices shift fundamentally with institution type, institutional reading holds.',
    'If natural law, policy interventions create deadweight loss and unintended consequences; if constructed, interventions are design choices and can be optimized. This is the foundational ontological divergence: whether price IS or whether price is MADE.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_construction, conceptual, 'Whether price formation is a brute fact of scarcity or a constructed social outcome.').

omega_variable(
    equilibrium_existence_and_stability,
    'Does a unique, stable equilibrium exist in real markets, or are real prices path-dependent, influenced by initial conditions, information cascades, and non-convexities?',
    'Empirical study of price trajectories in newly-formed markets (new asset classes, new platforms) and markets after regulatory shocks (price controls, deregulation). If paths converge to a unique equilibrium regardless of history, naturalist reading supported; if paths diverge and persist, institutional/financial reading supported.',
    'Existence and uniqueness are foundational to the naturalist reading''s claim that price IS discovered rather than constructed. Non-uniqueness undermines the claim that ''the price'' reflects ''the equilibrium''; instead prices would reflect path-contingent institutional choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_existence_and_stability, empirical, 'Whether observable prices converge to a unique natural equilibrium or remain path-dependent.').

omega_variable(
    information_asymmetry_and_preference_visibility,
    'Can preferences and scarcity be observed and aggregated decentrally via price signals, or are real markets characterized by severe asymmetric information and hidden preferences that price cannot reveal?',
    'Study of price discovery failures: lemons markets, adverse selection in insurance and lending, information cascades in asset markets. If information asymmetries are empirically small or markets develop mechanisms to overcome them, naturalist reading is supported; if asymmetries are large and persistent, institutional reading is supported.',
    'The naturalist reading depends on the claim that prices reveal true scarcity and preference. If revealed preferences are systematically biased by informational gaps, then observed prices are not equilibrium prices but artifacts of the information structure — supporting the institutional reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_and_preference_visibility, empirical, 'Whether real markets have sufficient information transparency for price to function as a natural equilibrium signal.').

omega_variable(
    scarcity_objectivity,
    'Is scarcity an objective property of resources, or is scarcity itself socially constructed (e.g., artificial scarcity from intellectual property, zoning, monopoly control)?',
    'Historical and comparative analysis of the same resource under different institutional regimes. Water (scarce under gravity-fed systems, abundant under public piping); land (scarce under zoning, less scarce under dense development); data (artificially scarce under encryption, abundant under open protocols). If scarcity is invariant across regimes, it is objective; if it shifts with institutional design, it is constructed.',
    'If scarcity is constructed, then ''price reflects objective scarcity'' is circular: prices reflect the scarcity that institutions create. The naturalist reading requires scarcity to be given and external to the institutional system; constructed scarcity collapses the reading''s foundational claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_objectivity, conceptual, 'Whether scarcity is an objective feature of the world or a socially constructed property.').

omega_variable(
    preference_exogeneity,
    'Are preferences exogenous (given, stable, independent of institutional context), or are preferences endogenous (shaped by advertising, status signaling, institutional options)?',
    'Behavioral economics, neuroeconomics, and cross-cultural preference studies. If preferences are consistent across contexts and resistant to manipulation, exogenous framing is supported; if preferences vary systematically with information access, social proof, and available options, endogenous reading is supported.',
    'The naturalist reading treats price as equilibrium of given preferences. If preferences are shaped by the institutions whose outcomes the reading claims to explain, the explanation is circular. Endogenous preferences support the institutional reading (institutions construct preferences, which construct demand, which determines price).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preference_exogeneity, empirical, 'Whether preferences driving price formation are exogenous or shaped by institutional context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__naturalist_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__naturalist_reading, theater_ratio, 40, 0.0).
narrative_ontology:measurement(pric_tr_t60, price_formation_kernel__naturalist_reading, theater_ratio, 60, 0.0).
narrative_ontology:measurement(pric_tr_t80, price_formation_kernel__naturalist_reading, theater_ratio, 80, 0.0).
narrative_ontology:measurement(pric_tr_t100, price_formation_kernel__naturalist_reading, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__naturalist_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__naturalist_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement(pric_be_t60, price_formation_kernel__naturalist_reading, base_extractiveness, 60, 0.0).
narrative_ontology:measurement(pric_be_t80, price_formation_kernel__naturalist_reading, base_extractiveness, 80, 0.0).
narrative_ontology:measurement(pric_be_t100, price_formation_kernel__naturalist_reading, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__naturalist_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__naturalist_reading, suppression_requirement, 20, 0.0).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__naturalist_reading, suppression_requirement, 40, 0.0).
narrative_ontology:measurement(pric_su_t60, price_formation_kernel__naturalist_reading, suppression_requirement, 60, 0.0).
narrative_ontology:measurement(pric_su_t80, price_formation_kernel__naturalist_reading, suppression_requirement, 80, 0.0).
narrative_ontology:measurement(pric_su_t100, price_formation_kernel__naturalist_reading, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% Price formation kernel is decomposed into four readings: naturalist (this constraint), institutional, georgist, and financialization. Each reading has different ε, different beneficiary/victim structure, different type. The readings coexist in public discourse but compete for explanatory authority. ε-invariance principle (DP-001): each reading instantiates a different constraint with stable ε within its own framework. The institutional reading decomposes price as constructed, yielding higher ε and identified beneficiaries (zoning boards, lenders, platform operators). The georgist reading separates earned (capital/labor value) from unearned (location rent), potentially yielding moderate ε concentrated on land speculators. The financialization reading treats price as feedback loop of credit expansion, yielding high ε concentrated on asset-price rent. The naturalist reading (this constraint) claims zero ε because price is discovery, not construction. Measurable divergence between readings signals that observables differ — readers are looking at different constraints. All four readings share the same kernel (price formation in housing) but instantiate different constraints via different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
