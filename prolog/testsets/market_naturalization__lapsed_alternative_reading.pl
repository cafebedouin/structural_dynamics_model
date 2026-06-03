% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (Alternative Reading)
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the 'lapsed alternative reading' of the
 *   market_naturalization kernel. In this reading, market dominance as a
 *   closure persists without active maintenance by identifiable
 *   beneficiaries. The mechanism is institutional atrophy of alternatives:
 *   command economies have discredited themselves; gift economies and
 *   commons-based production have been pushed to margins; cooperative
 *   movements have been absorbed into market logics; informal economies have
 *   been formalized into market participation. The closure is lapsed because
 *   the forces that would challenge market dominance (organized labor,
 *   cooperative networks, gift-economy practitioners, commons-stewards) have
 *   lost structural capacity to mount viable counter-institutions. This is
 *   distinct from the sibling 'beneficiary_maintained_reading' where market
 *   dominance is actively defended by incumbent capital holders through
 *   antitrust exemptions, IP regimes, regulatory capture, and state
 *   enforcement. It is also distinct from the 'hybrid_reading' which sees
 *   market dominance as combining both lapsed elements (atrophied
 *   alternatives) and active maintenance (beneficiary defense). In the lapsed
 *   reading, market dominance appears as a self-sustaining rope —
 *   coordination equilibrium without coercion, because alternatives have been
 *   unmade and their re-making is not blocked actively but by the sheer
 *   difficulty of institutional revival.
 *
 * KEY AGENTS:
 *   - Market Participants (Moderate Power/Mobile): Experience market dominance as stable coordination; no agents perceive active suppression
 *   - Precarious Participants (Powerless/Constrained): Experience market as constraining but not coercive; atrophied alternatives make exit costly but not impossible
 *   - Market Institution (Institutional/Mobile/Global): Self-sustaining through functional coordination benefits (price aggregation, resource allocation); low maintenance overhead
 *   - Market Ideology (Institutional/Mobile/Global): Pro-market philosophy and neoclassical economics maintain intellectual legitimacy of market closure; performative function (piton view)
 *   - Organized Opposition (Organized/Constrained/National): Labor unions, cooperatives, commons movements; historically counter-institutional but now institutionalized or marginalized
 *   - Analytical Observer (Analytical/Analytical/Global): Sees market dominance as contingent temporary equilibrium with potential sunset as new coordination mechanisms emerge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.18).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, rope).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (Alternative Reading)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, 'a50e81f5-45d9-41d8-b0c0-a91dd51ac01d').
narrative_ontology:cs_kernel_codification('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', implicit).
narrative_ontology:cs_authority_grounding('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', extraction).
narrative_ontology:cs_interpretation_layer_present('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d').
narrative_ontology:cs_reading_relation('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', market_naturalization__beneficiary_maintained_reading, forecloses).
narrative_ontology:cs_reading_relation('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', foundational, institutional_alternatives_genuinely_atrophied).
narrative_ontology:cs_axiom_status(institutional_alternatives_genuinely_atrophied, holdable).
narrative_ontology:cs_axiom_grounding('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', institutional_alternatives_genuinely_atrophied, empirically_contingent).
narrative_ontology:cs_axiom('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', foundational, market_dominance_requires_no_beneficiary_class).
narrative_ontology:cs_axiom_status(market_dominance_requires_no_beneficiary_class, holdable).
narrative_ontology:cs_axiom_grounding('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', market_dominance_requires_no_beneficiary_class, empirically_contingent).
narrative_ontology:cs_reference_frame('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', market_as_self_sustaining_equilibrium).
narrative_ontology:cs_drift_state('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', contemporary_financialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a50e81f5-45d9-41d8-b0c0-a91dd51ac01d', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARKET PARTICIPANT (ROPE) — Experiences market dominance as stable coordination equilibrium. Moderate agents (mid-tier firms, regional producers) benefit from standardized exchange mechanisms and accepted price signals. No coercion required; alternatives have atrophied through disuse. Extraction is minimal because the constraint functions as a genuine coordination mechanism with low overhead.
constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS PARTICIPANT (ROPE) — Experiences market as stable but constraining. Limited exit options (constrained: can exit markets but at high cost to livelihood), yet perceives no active suppression — the constraint persists through entrenchment, not enforcement. Sees market dominance as coordinate-or-perish, not coerce-or-perish. The binding mechanism is structural atrophy of alternatives, not visible coercion.
constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARKET INSTITUTION (ROPE) — The institution of market exchange (price system, property rights, contract law) maintains itself through coordination benefits, not through active defense of alternatives or visible suppression. Market dominance appears as self-sustaining institutional equilibrium. Low theater — the mechanism is functional, not performative. Low extractiveness because the institution's benefit is broadly distributed (information aggregation, resource allocation efficiency).
constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: MARKET IDEOLOGY (PITON) — Viewing market dominance through the lens of explicit pro-market ideology reveals performative maintenance. Neoclassical economics, libertarian philosophy, and market-fundamentalist discourse operate as theater that maintains the market closure's intellectual legitimacy without adding functional coordination capacity. Theater ratio high (0.65+) because ideological justification carries much weight; actual coordination function could persist without it. Piton classification reflects degraded original function (ideology as substitution for structural defense).
constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED OPPOSITION (PITON) — Labor unions, cooperative movements, and organized economic alternatives represent historically live counter-institutions. These organizations perceive market dominance as a lapsed closure — the alternatives they represent require no active suppression because unions have been institutionalized within the market (losing counter-institutional force) and cooperative movements exist as marginal niche sectors. The opposition maintains itself through inertia and cultural memory (theater) rather than structural force. Piton classification reflects degraded capacity to mount viable alternative coordination.
constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TEMPORAL VIEW (SCAFFOLD) — From a long-term analytical perspective, market dominance is a contingent institutional arrangement with a potential sunset. This reading sees the market as a temporary coordination solution that solved genuine coordination problems (vs. feudal barter, command economies) but may be superseded by alternative coordination mechanisms (platform economies, algorithmic allocation, post-scarcity coordination). The constraint appears as Scaffold — functional but temporary, with no enforcement required precisely because alternatives remain underdeveloped. The sunset is empirical/potential, not formalized.
constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_naturalization__lapsed_alternative_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, TR),
    TR >= 0.70.

:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. This reading asserts that market dominance extracts minimal rent beyond genuine coordination costs. No identifiable beneficiary class hoards gains — income distribution is determined by market mechanisms, not by closure protection. The low value reflects that measured extractiveness in this reading captures only coordination overhead (transaction costs, information asymmetries, some degree of path-dependent lock-in). Suppression (0.12): Very low. This reading asserts that alternatives are not actively suppressed. Unions are legal; cooperatives can operate; alternatives exist at margins. The suppression reflects only the structural difficulty of mounting viable counter-institutions, not coercive barriers. Theater ratio (0.35): Moderate-low. Market ideology carries significant weight in maintaining closure legitimacy, but the market mechanism itself functions without requiring performative justification — it works because it coordinates information and resource allocation. The 35% theater reflects the ideological work required to prevent challenges, not the core coordination function. Measurement trajectory shows modest growth: extractiveness rises slightly (0.08→0.18) as financialization and monopoly concentration occur, and theater rises (0.20→0.35) as market-ideological justification becomes more elaborate. But the core claim persists: no active beneficiary maintenance is required — the closure is lapsed, held by inertia.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this reading is minimal — most perspectives yield rope, scaffold, or piton, not because the observers disagree sharply but because the lapsed closure reading asserts structural uniformity. The gap appears between the market institution's functional rope view (coordination works) and the organized opposition's piton view (alternatives are degraded but not suppressed). The organized opposition perspective is diagnostic: if they perceived active suppression (snare or tangled_rope), the reading would be false — beneficiary maintenance would be evident. That they perceive piton (degraded alternative, maintained through inertia) confirms the lapsed reading's core claim. The analytical scaffold perspective introduces potential sunset — market dominance is not eternal, merely current — which distinguishes this reading from the beneficiary_maintained reading (which would see market dominance as actively preserved indefinitely).
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares NO beneficiaries in base_properties because the lapsed closure reading asserts that no identifiable agent class captures supernormal rents from market dominance. This structural choice (zero beneficiaries) is the kernel reading's core empirical claim — it differs from the beneficiary_maintained_reading which would list incumbent capital holders, financial oligopolists, and rent-capturing monopolists as beneficiaries. With zero beneficiaries and no victims, directionality derivation produces uniform low d values across all perspectives — no agent is systematically targeted for extraction, and no agent unusually benefits. The constraint appears as coordination (rope) rather than extraction (snare or tangled_rope) from all perspectives. This is the structural signature of the lapsed closure reading: the absence of beneficiary/victim asymmetry in the base properties propagates into perspectival uniformity (all rope or scaffold or piton, but not snare or tangled_rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy because the claimed type (rope) is consistent with all measured perspectives. Rope classification is appropriate for low-extraction coordination mechanisms without identifiable beneficiaries. The constraint becomes mandatrophic only if we compare it against the sibling beneficiary_maintained_reading, which would classify the same market dominance phenomenon as tangled_rope or snare (high extraction, active maintenance). The mandatrophy is not within this story but between readings of the kernel. This is by design: the kernel contest is precisely where mandatrophy lives. Resolving which reading is correct (lapsed vs. beneficiary-maintained) is the engine's job when empirical data becomes available to discriminate the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_atrophy_irreversibility,
    'Is the atrophy of alternative coordination systems (cooperative networks, gift economies, commons-based production) genuinely irreversible, or can it be reversed through institutional reconstruction?',
    'Historical case studies of institutional revival (cooperative re-emergence, commons reconstitution); analysis of institutional learning curves and path-reversal costs. Comparison of revival success rates across domains.',
    'If irreversible: the lapsed closure is sticky — alternatives cannot re-emerge without massive structural investment, making market dominance de facto eternal. If reversible: the lapsed reading is accurate but unstable — alternatives could re-constitute, and the closure is genuinely contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_atrophy_irreversibility, empirical, 'Whether institutional atrophy of alternatives is reversible').

omega_variable(
    beneficiary_identification_problem,
    'Does the absence of a clearly identifiable beneficiary class in this reading represent genuine lack of beneficiaries, or does it reflect analytical blind spots about who captures rents from market dominance?',
    'Trace actual wealth accumulation and rent distribution in market-dominant economies; identify agents whose income/power would collapse if market closure ended. Compare with sibling reading''s beneficiary identification.',
    'If beneficiaries exist but this reading obscures them: the lapsed reading is analytically false — market dominance is actively maintained by beneficiaries hiding behind ''naturalization'' framing. If no clear beneficiary class: the lapsed reading is accurate, and market dominance persists through institutional entrenchment rather than active defense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identification_problem, empirical, 'Identifiability of beneficiary class in market dominance').

omega_variable(
    active_maintenance_threshold,
    'What level of institutional maintenance counts as ''active'' vs. ''lapsed''? Is antitrust enforcement, patent law, corporate law, or IP regime maintenance evidence of active market closure, or are these routine institutional functions not counting as ''active defense''?',
    'Decompose institutional maintenance into routine coordination cost (lapsed closure maintenance) vs. extraordinary enforcement (active defense). Identify which market-supporting legal/policy instruments are counterfactually necessary — what would market dominance lose if removed?',
    'If enforcement regimes are necessary: market dominance is actively maintained (sibling beneficiary_maintained_reading is correct). If enforcement regimes are peripheral: market dominance is lapsed (this reading is accurate). This threshold determines which reading of the kernel is empirically sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_threshold, conceptual, 'Threshold for distinguishing active maintenance from lapsed institutional inertia').

omega_variable(
    kernel_framing_reading_dependency,
    'Does this reading depend on a specific framing of ''market dominance'' as a closure that requires defense, or is the very concept of ''dominance requiring maintenance'' already the beneficiary reading smuggled in?',
    'Test alternative kernel framings: (1) market dominance as natural inevitable equilibrium (no closure, no maintenance needed); (2) market dominance as contingent institutional package; (3) market dominance as actively defended extraction mechanism. Each framing instantiates a different reading. Determine which kernel framing this constraint actually instantiates.',
    'If the kernel is inherently ''dominance-as-defended-closure'': this reading (lapsed closure) is coherent. If the kernel is ''market mechanism per se'': this reading mislabels the constraint — there is no closure to lapse. The omega reveals whether this reading is one valid reading or a conceptual confusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_reading_dependency, conceptual, 'Kernel framing dependency for the lapsed closure reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mkt_lapsed_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mkt_lapsed_tr_t40, market_naturalization__lapsed_alternative_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(mkt_lapsed_tr_t80, market_naturalization__lapsed_alternative_reading, theater_ratio, 80, 0.35).

% Extraction over time
narrative_ontology:measurement(mkt_lapsed_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mkt_lapsed_be_t40, market_naturalization__lapsed_alternative_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(mkt_lapsed_be_t80, market_naturalization__lapsed_alternative_reading, base_extractiveness, 80, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.1).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% Market dominance is analyzed through three readings of a single kernel. This constraint (lapsed_alternative_reading) models market dominance as institutional equilibrium without active beneficiary maintenance. Sibling constraints model the same phenomenon as either actively defended (beneficiary_maintained_reading) or hybrid (hybrid_reading). The three stories share identical base observed markets but differ in explaining the mechanism of persistence. Each has its own epsilon value reflecting the reading's empirical content. Network links enable comparative analysis of how readings affect each other's plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
