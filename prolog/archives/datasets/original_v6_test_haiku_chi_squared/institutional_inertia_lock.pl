% ============================================================================
% CONSTRAINT STORY: institutional_inertia_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_inertia_lock, []).

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
 *   constraint_id: institutional_inertia_lock
 *   human_readable: The Sunk-Cost Regulatory Trap
 *   domain: political/technological
 *
 * SUMMARY:
 *   The sunk-cost regulatory trap arises when the institutional cost of
 *   dismantling and rebuilding a regulatory framework exceeds the annual
 *   friction of maintaining an obsolete one. This is a classical tangled
 *   rope: the framework originally served a genuine coordination function
 *   (establishing predictability for legitimate market participants), but
 *   incumbent firms and the regulatory bureaucracy have jointly developed
 *   dependencies that make reform politically infeasible even when the net
 *   efficiency loss is enormous. The constraint's theater_ratio of 0.68
 *   reflects that much regulatory activity becomes performative compliance
 *   ritual rather than substantive protection. Compliance audits follow the
 *   letter of outdated rules; workarounds and shadow markets flourish;
 *   innovation adapts around the framework rather than being enabled by it.
 *   The extractiveness of 0.52 reflects moderate but real rent extraction
 *   through barrier-to-entry effects, though the framework retains some
 *   genuine coordination function for incumbents. Over a 20-year interval,
 *   both theater and extractiveness increase as technology and consumer
 *   preferences diverge from the framework's original design, while sunk
 *   costs accumulate, making reform progressively more expensive.
 *
 * KEY AGENTS:
 *   - Emerging Market Entrants: Primary victims (powerless/trapped) — face full compliance costs designed for incumbents; cannot exit without abandoning market participation
 *   - Consumer Efficiency Gains (Abstract): Primary victim (powerless/trapped) — permanently deferred innovation and cost reductions that would benefit consumers but have no political advocate
 *   - Incumbent Regulated Entities: Primary beneficiary (powerful/constrained) — extract rent through regulatory moat while experiencing coordination benefits; constrained by regulatory dependence, powerful enough to resist change
 *   - Regulatory Bureaucracy: Secondary beneficiary (institutional/arbitrage) — benefits from maintaining existing compliance infrastructure and staff allocation; can arbitrage by updating rules without fundamental restructuring
 *   - Legislative Intent / Original Statutory Mandate: Institutional actor (institutional/constrained) — original statute achieved legitimate goals; now persists through inertia despite theater ratio indicating performative compliance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — can detect hybrid coordination-extraction character and identify sunk cost trap as contingent, not natural
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_inertia_lock, 0.52).
domain_priors:suppression_score(institutional_inertia_lock, 0.65).
domain_priors:theater_ratio(institutional_inertia_lock, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_inertia_lock, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_inertia_lock, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_inertia_lock, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_inertia_lock, tangled_rope).
narrative_ontology:human_readable(institutional_inertia_lock, "The Sunk-Cost Regulatory Trap").
narrative_ontology:topic_domain(institutional_inertia_lock, "political/technological").

domain_priors:requires_active_enforcement(institutional_inertia_lock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_inertia_lock, incumbent_regulated_entities).
narrative_ontology:constraint_beneficiary(institutional_inertia_lock, regulatory_bureaucracy).
narrative_ontology:constraint_victim(institutional_inertia_lock, emerging_market_entrants).
narrative_ontology:constraint_victim(institutional_inertia_lock, consumer_efficiency_gains).
narrative_ontology:constraint_victim(institutional_inertia_lock, innovation_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING MARKET ENTRANT (SNARE) — Cannot exit the legacy framework without absorbing full compliance costs designed for incumbents. High barriers to entry, no waiver path, no alternative jurisdiction within market. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(institutional_inertia_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER EFFICIENCY GAINS / INNOVATION POTENTIAL (SNARE) — Abstract collective benefit perpetually deferred. No agent advocates for the unrealized efficiency. Trapped indefinitely by sunk costs of legacy framework. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(institutional_inertia_lock, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT REGULATED ENTITIES (TANGLED ROPE) — Benefit from regulatory moat (coordination function: predictable rules for incumbents). Also extract rent through compliance cost imposition on entrants. Constrained by regulatory dependence but powerful enough to resist change. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.31.
constraint_indexing:constraint_classification(institutional_inertia_lock, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY BUREAUCRACY (ROPE) — Benefits from maintaining existing compliance infrastructure, staff allocation, and jurisdictional authority. Experiences framework redesign as coordination problem (can be solved by updating rules). d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Negative extraction = net beneficiary.
constraint_indexing:constraint_classification(institutional_inertia_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE INTENT / ORIGINAL MANDATE (PITON) — The framework was designed to achieve specific legislative goals (consumer protection, market stability). Over time, the statute persists through inertia despite achieving only theater—performative compliance audits, rule-following rituals—while the underlying statutory objectives are not met. theater_ratio=0.68 reflects widespread workarounds, shadow markets, and compliance theater. The constraint is maintained through institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(institutional_inertia_lock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (COMPARATIVE POLITICAL ECONOMY) (TANGLED ROPE) — Sees both coordination (incumbents need predictability) and extraction (barriers impose deadweight loss). The constraint exhibits classical regulatory capture: initial rule established legitimate coordination function, but sunk costs and incumbent lobbying have locked in extraction mechanism. χ≈0.52 (moderate extractiveness) reflects hybrid nature. Does not classify as Mountain—the framework is contingent institutional choice, not natural law.
constraint_indexing:constraint_classification(institutional_inertia_lock, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_inertia_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_inertia_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_inertia_lock, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_inertia_lock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_inertia_lock, TR),
    TR >= 0.70.

:- end_tests(institutional_inertia_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework creates genuine barriers to entry (compliance cost, legal complexity, licensing requirements) that incumbent firms do not face due to grandfathering or established protocols. This is extraction in the form of rent imposed on entrants. However, the extraction is not total (χ ≤ 0.90) because the framework retains some real coordination function: it does provide predictability and baseline standards for incumbents, which has genuine value. The increasing trajectory from 0.35 to 0.52 reflects that as technology diverges from the framework's original design, the ratio of extraction to coordination increases; the framework becomes less about genuine market coordination and more about protecting incumbent market position. Suppression (0.65): High. Entrants face multiple barriers: (a) regulatory compliance costs proportional to incumbent scale, not startup scale; (b) licensing and approval delays that advantage incumbents with existing relationships; (c) legal/technical complexity that requires regulatory expertise; (d) lack of alternative pathways or waivers. These are not natural scarcity but deliberate structural features. Theater ratio (0.68): Moderately high. Regulatory audits, compliance documentation, and rule-following rituals constitute performative activity that does not substantively achieve the framework's original statutory objectives. Actual enforcement varies; workarounds proliferate; innovation occurs in shadow markets. The rising trajectory from 0.38 to 0.68 reflects that as the framework ages, the gap between the rule-following ritual and the substantive coordination goal widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural phenomenon appears as pure extraction (Snare) from the entrant perspective, mixed coordination-extraction (Tangled Rope) from the incumbent perspective, and degraded ritual (Piton) from the statutory intent perspective. Emerging entrants see no coordination benefit—they encounter only barriers. Incumbents see both coordination (predictable rules) and extraction (competitive moat). The regulatory bureaucracy sees primarily coordination (maintenance of existing rule structure). Legislative intent observes that the statute's original purposes (consumer protection, market stability) are no longer being served; the framework is theater. The analytical observer sees all four: a hybrid coordination-extraction mechanism with increasing theater as the framework ages. The perspectival gap reveals that 'sunk cost trap' is not a fixed property but a structural relationship between the framework's original purpose, the current institutional dependencies, and the efficiency loss that accumulates over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Emerging entrants: Victim + trapped → d≈0.92, f(d)≈1.40. Near-maximum extraction; no exit option except market abandonment. Consumer efficiency: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; no collective exit option. Incumbent entities: Beneficiary + constrained (regulatory dependence) → d≈0.48, f(d)≈0.60. Moderate extraction burden despite beneficiary status because they are also partially trapped by regulatory dependence. If exit capacity were higher (d→0.35, arbitrage exit), they would shift toward pure Rope. Regulatory bureaucracy: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; can implement framework changes without fundamental restructuring. Legislative intent: Constrained institutional perspective → d≈0.65, f(d)≈1.00. Moderate extraction; the original statute's intent is being violated by the constraint's continuation. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Neutral analysis of structural pattern; sees both coordination and extraction components.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE CONFLICT ANALYSIS: The sunk-cost regulatory trap demonstrates classic mandatrophy between the original statutory goal (consumer/market protection) and the actual institutional outcome (incumbent protection through compliance cost barriers). The statute mandates the coordination function (predictability, baseline standards); the constraint produces extraction (barrier to entry, innovation suppression). Resolution mechanism: This is Tangled Rope precisely because both coordination and extraction are structurally present. The framework is not a false summit (not a pure extraction Snare masquerading as coordination). The coordination function is real—incumbents genuinely benefit from predictable rules. The extraction is also real—entrants genuinely face barriers. The mandatrophy occurs because the original legislative mandate (consumer protection, market stability) is no longer satisfied by the framework that ostensibly implements it. Theater has increased faster than functionality. The system persists not because it works but because the institutional cost of replacement exceeds the recognized inefficiency cost. Reform is possible through legislative sunset mechanisms or regulatory modernization, but the sunk cost trap makes reform politically disfavored unless external shocks (competitive pressure from unregulated sectors, consumer defection, technological obsolescence) exceed the threshold of incumbent lobbying resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunk_cost_threshold,
    'What reorganization cost threshold would trigger legislative/regulatory reform? At what point does the efficiency gain from redesign exceed the one-time institutional restructuring cost?',
    'Cost-benefit analysis of regulatory redesign; comparison with similar framework transitions in adjacent jurisdictions; legislative committee cost estimates for modernization',
    'If threshold low (< 2 years of incumbent compliance spending): framework should have been reformed already, indicating political lock (not just cost lock). If threshold high (> 10 years): sunk cost trap is genuinely structural and may persist indefinitely without external shock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunk_cost_threshold, empirical, 'Cost threshold at which regulatory redesign becomes economically rational').

omega_variable(
    incumbent_exit_capacity,
    'Can incumbent entities realistically migrate to alternative regulatory frameworks (neighboring jurisdictions, regulatory arbitrage, functional equivalence with lighter regimes)?',
    'Analysis of incumbent adaptation to jurisdictional change; cost of relocating compliance infrastructure; measurement of actual defection if framework is liberalized',
    'If high exit capacity: incumbents are not truly dependent on the framework; extract through active enforcement, not lock. Classification shifts toward pure Snare. If low exit capacity: incumbents are also partially trapped, changing directionality analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_exit_capacity, empirical, 'Incumbent capacity to migrate to alternative regulatory regimes').

omega_variable(
    consumer_preference_revelation,
    'Do consumers value the regulatory framework''s purported protections, or would they accept lighter regulation with lower compliance cost pass-through?',
    'Revealed preference studies (actual choices when alternative is available); willingness-to-pay surveys; comparison with lightly-regulated competitors in other markets for same services',
    'If consumers strongly prefer protections: framework serves real coordination function even if inefficient. If consumers indifferent or prefer cheaper unregulated alternative: framework is pure extraction theater. Determines whether framework is truly Tangled Rope (mixed benefit/cost) or Snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_preference_revelation, empirical, 'Consumer demand for regulatory protections versus lighter-touch regimes').

omega_variable(
    legislative_sunset_mechanism,
    'Does the regulatory statute include automatic sunset/reauthorization clauses that would force periodic review? Or is the framework permanent by default?',
    'Statutory text analysis; legislative history; comparative review of sunset-enabled vs permanent regulatory frameworks',
    'If permanent-by-default: sunk cost trap is structurally embedded; framework persists through inertia even if consensus reform exists. If sunset-enabled: trap can be broken by legislative calendar. This is the lynchpin of whether Scaffold perspective is feasible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_sunset_mechanism, conceptual, 'Whether statute includes automatic reauthorization/sunset mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_inertia_lock, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inertia_tr_t0, institutional_inertia_lock, theater_ratio, 0, 0.38).
narrative_ontology:measurement(inertia_tr_t10, institutional_inertia_lock, theater_ratio, 10, 0.52).
narrative_ontology:measurement(inertia_tr_t20, institutional_inertia_lock, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(inertia_be_t0, institutional_inertia_lock, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inertia_be_t10, institutional_inertia_lock, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(inertia_be_t20, institutional_inertia_lock, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_inertia_lock, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_inertia_lock, regulatory_capture).
narrative_ontology:affects_constraint(institutional_inertia_lock, incumbent_rent_seeking).
narrative_ontology:affects_constraint(institutional_inertia_lock, innovation_suppression_via_compliance).

% DUAL FORMULATION NOTE:
% The sunk-cost regulatory trap is structurally downstream of regulatory capture (which establishes the framework favoring incumbents) but represents a distinct phase: the lock-in via accumulated institutional costs. Separate ε values distinguish the initial capture mechanism (higher extraction, lower theater) from the long-term inertial lock (lower extraction, higher theater as the framework becomes increasingly performative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_inertia_lock, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
