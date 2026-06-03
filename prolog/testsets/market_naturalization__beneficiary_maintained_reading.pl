% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Defended Extraction (Beneficiary-Maintained Reading)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel
 *   'market_naturalization': the beneficiary-maintained reading. This reading
 *   asserts that market dominance by incumbent capital holders is actively
 *   defended through enforcement mechanisms (regulatory capture, patent
 *   strategy, supply-chain control, predatory pricing, infrastructure
 *   lock-in) rather than emerging passively from immutable scale economies or
 *   inevitable competitive dynamics. The core claim is that identifiable
 *   beneficiaries (incumbent capital holders) sustain dominance through
 *   active suppression of alternatives, and that without this enforcement
 *   machinery, market structure would be substantially different. This
 *   contrasts with sibling readings: the lapsed_alternative_reading treats
 *   dominance as a stable outcome of past competitive elimination requiring
 *   minimal ongoing enforcement, and the hybrid_reading frames dominance as
 *   combining lapsed structural advantages with active maintenance on the
 *   margins. The beneficiary-maintained reading is distinguished by its
 *   attribution of causality: dominance exists primarily because
 *   beneficiaries continuously defend it, not because alternatives have
 *   become structurally obsolete or because the dominant position rests on
 *   passive scale advantages.
 *
 * KEY AGENTS:
 *   - Incumbent Capital Holders: Primary beneficiary class (institutional/arbitrage) — extract rents through market dominance; coordinate enforcement machinery
 *   - Excluded Market Entrants: Primary victim class (powerless/trapped) — face insurmountable barriers; accumulate opportunity costs; systematically denied access to market participation
 *   - Labor Suppliers: Secondary victim class (moderate/constrained) — face monopsony power; wage suppression; limited bargaining capacity
 *   - Alternative Economic Organization: Structural victim (powerless/trapped) — cooperative, decentralized, and non-capitalist economic forms are actively suppressed through legal, financial, and regulatory barriers
 *   - Competitive Incumbents (Surviving Challengers): Mixed beneficiary/victim (organized/constrained) — benefit from coordination mechanisms (established supply chains, standardized platforms); bear extraction costs (competitive pressure, predatory tactics, patent litigation)
 *   - Regulatory State (Captured Regulator): Mixed beneficiary/victim (powerful/mobile) — benefits from stable business environment and predictable tax base; bears extraction costs (political accountability, antitrust exposure, regulatory burden-shifting)
 *   - Neoclassical Economics Apparatus: Legitimacy structure (institutional/arbitrage) — performs theoretical justification for dominance; maintains institutional inertia; shields enforcement machinery from scrutiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.68).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.72).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, snare).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Defended Extraction (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '0ab72f47-8e4c-4774-a065-2e3e0fd76414').
narrative_ontology:cs_kernel_codification('0ab72f47-8e4c-4774-a065-2e3e0fd76414', distributed).
narrative_ontology:cs_authority_grounding('0ab72f47-8e4c-4774-a065-2e3e0fd76414', extraction).
narrative_ontology:cs_interpretation_layer_present('0ab72f47-8e4c-4774-a065-2e3e0fd76414').
narrative_ontology:cs_reading_relation('0ab72f47-8e4c-4774-a065-2e3e0fd76414', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ab72f47-8e4c-4774-a065-2e3e0fd76414', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('0ab72f47-8e4c-4774-a065-2e3e0fd76414', foundational, capital_holders_actively_defend_dominance).
narrative_ontology:cs_axiom_status(capital_holders_actively_defend_dominance, holdable).
narrative_ontology:cs_axiom_grounding('0ab72f47-8e4c-4774-a065-2e3e0fd76414', capital_holders_actively_defend_dominance, empirically_contingent).
narrative_ontology:cs_axiom('0ab72f47-8e4c-4774-a065-2e3e0fd76414', foundational, suppression_of_alternatives_is_necessary).
narrative_ontology:cs_axiom_status(suppression_of_alternatives_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0ab72f47-8e4c-4774-a065-2e3e0fd76414', suppression_of_alternatives_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('0ab72f47-8e4c-4774-a065-2e3e0fd76414', incumbent_capital_dominance_maintained_through_active_enforcement).
narrative_ontology:cs_drift_state('0ab72f47-8e4c-4774-a065-2e3e0fd76414', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ab72f47-8e4c-4774-a065-2e3e0fd76414', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, rent_extraction_apparatus).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, excluded_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, labor_suppliers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, alternative_economic_organization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED ENTREPRENEUR (SNARE) — Faces structural barriers to market entry: capital requirements, regulatory capture, incumbent price-cutting, patent accumulation, supply-chain gatekeeping. No exit option; must accept subordinate role or abandon economic participation. Maximum experienced extraction — the constraint extracts opportunity cost and accumulated wealth disparity.
constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR SUPPLIER (SNARE) — Faces monopsony or oligopsony power: limited employment options, wage suppression, reduced bargaining power. Exit is theoretically possible (migrate, retrain) but costs are high (geographic, social, human-capital sunk). Significant extraction of surplus value through asymmetric bargaining.
constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT CAPITAL HOLDER (ROPE) — Experiences the constraint as coordination: maintaining dominant position through strategic capital allocation, network effects, and regulatory positioning is how markets 'work.' Sees enforcement machinery (antitrust avoidance, lobbying, integration) as normal business coordination. Net beneficiary; effective extraction runs toward this agent.
constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETITIVE INCUMBENT / SURVIVING CHALLENGER (TANGLED ROPE) — A firm that has scaled into competitiveness but remains below market-dominant position. Experiences genuine coordination benefits (access to supply chains, standardized platforms, established distribution) AND significant extraction costs (competitive pressure, predatory pricing, patent litigation risk). Has some agency (can negotiate, can exit to adjacent markets) but constrained by dominant incumbent's power.
constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NEOCLASSICAL ECONOMICS APPARATUS (PITON) — 'Perfect competition' and 'efficient markets' axioms perform legitimacy for the dominance structure while denying the machinery that maintains it. Antitrust enforcement is theatrical: high-profile cases followed by lax enforcement and settlement. The economic theory persists through institutional inertia (textbooks, academic incentives, policymaker training) despite empirical failure to predict market concentration outcomes. Theater ratio is high because the constraint's enforcement appears as market-natural when theoretically framed as inevitable equilibrium.
constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY STATE / CAPTURED REGULATOR (TANGLED ROPE) — State apparatus nominally charged with preventing monopolistic extraction. Actually enforces the constraint through selective enforcement, patent extension, merger approval, and regulatory barriers to entry disguised as consumer protection. Experiences genuine coordination benefits (predictable business environment, reduced cutthroat competition, stable tax base) AND real extraction costs (political accountability, regulatory burden-shifting, antitrust exposure). Has agency but is constrained by incumbent capital's political power.
constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, market concentration is treated as inevitable: larger firms are more efficient (scale economies), winners consolidate advantages (network effects), competition is naturally self-limiting. This perspective naturalizes the beneficiary-maintained extraction as a law of economics. However, this reading's structural data declares active suppression of alternatives and identifiable beneficiaries — signatures that contradict genuine natural law. The engine's false summit detector will flag this as naturalization of a contingent institutional arrangement rather than an immutable constraint.
constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_naturalization__beneficiary_maintained_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, TR),
    TR >= 0.70.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The reading asserts that incumbent capital holders extract substantial rents (monopolistic pricing, supernormal profits) AND bear enforcement costs (lobbying, legal strategy, regulatory positioning, predatory competitive acts). The sum of extraction plus enforcement cost yields 0.68 — in the high-extraction range for snare classification. The measurement trajectory shows extractiveness accumulating over time (0.45 → 0.68), consistent with the reading's narrative that enforcement machinery has intensified and become more sophisticated over the 50-unit interval. Suppression (0.72): High. Active suppression of alternatives is central to this reading's core claim. Barriers include: capital concentration (excluded entrants cannot raise financing), regulatory capture (rules written to favor incumbents), patent strategy (IP enforcement limits innovation), supply-chain lock-in (exclusive dealing, vertical integration), predatory pricing (below-cost competition), infrastructure monopolies (network effects, switching costs), and ideological suppression (market-naturalization mythology that frames dominance as inevitable). The suppression trajectory (0.52 → 0.72) indicates that suppression mechanisms have been progressively strengthened and systematized. Theater ratio (0.38): Low-moderate. This reading minimizes the performative content of dominance maintenance. Unlike the piton perspective (which emphasizes the theatrical economics apparatus), this reading treats the enforcement machinery as fundamentally functional rather than theatrical. Antitrust enforcement is theatrical (high-profile cases, minimal impact), but incumbent capital's enforcement machinery is substantive (actually works to maintain dominance). Theater remains present because market-naturalization mythology obscures enforcement as deliberate action, but the ratio is lower than if the enforcement were purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the incumbent capital holder (Rope) and excluded entrant (Snare) is maximal. The beneficiary experiences the constraint as normal market coordination — the machinery of dominance appears as business-as-usual. The victim experiences the constraint as extraction with no exit. The regulatory state (Tangled Rope) sees genuine coordination benefits alongside real extraction costs, giving it agency to resist but incentives to comply. The neoclassical economics apparatus (Piton) naturalizes the beneficiary's experience into theory, performing legitimacy that obscures the enforcement machinery. The surviving challenger (Tangled Rope) has more agency than the powerless entrant but less than the incumbent — they benefit from coordination but bear asymmetric extraction. The analytical observer at civilizational scale risks seeing a mountain (inevitable market dynamics) when the structural data declares an active snare.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading's directionality assignment depends on the beneficiary/victim structure it declares. Incumbent capital holders are declared beneficiaries with arbitrage exit options — they can divest, relocate, shift to alternative markets, or negotiate exit terms. Their d-value derives as low (~0.15 for beneficiary + arbitrage), producing negative f(d), indicating they experience the constraint as subsidizing them. Excluded entrants are declared victims with trapped exit — no meaningful way out except abandoning economic participation. Their d-value derives as high (~0.95 for victim + trapped), producing high f(d) ~1.42, indicating they experience maximum extraction. Labor suppliers (victim + constrained) derive moderate-high d (~0.80), indicating significant extraction. The regulatory state (beneficiary status debatable; constrained exit) occupies an intermediate position — it benefits from predictable business environment but is constrained by antitrust exposure and political accountability. The reading's snare classification emerges from: high ε (0.68), high suppression (0.72), and the derived directionality showing substantial extraction concentrated on trapped and constrained agents. The piton perspective's lower theater ratio (0.38) reflects that this reading does not rely on performative machinery — the enforcement is substantive, which is why the reading frames dominance as 'actively defended' rather than 'theatrically maintained.'
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the snare classification (high extraction, high suppression, identifiable victims, weak or absent coordination function) is stable across multiple perspectives from different power positions. The piton perspective (neoclassical economics apparatus) is the mandatrophy resolution: it demonstrates that the constraint can be classified as piton (low functional extraction, high theater) only from within the legitimating theory. From every structural perspective outside the theory (excluded entrant, labor supplier, alternative organization), the constraint classifies as snare. The analytical observer's mountain (market dominance as inevitable) is a false summit — it is natural-law framing imposed on a constraint with declared beneficiaries and active suppression. The mandatrophy does not dissolve because the piton and snare are not contradictory; they are perspectival: from inside the neoclassical frame, dominance appears as natural law (hence piton as degraded natural law); from outside the frame, the enforcement machinery is visible and dominance appears as active extraction (hence snare). The resolution is not 'which is correct?' but 'the classification depends on whether you can see the enforcement machinery, which the legitimating theory prevents you from seeing.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_economic_organization_feasibility,
    'Under what conditions would cooperative, decentralized, or non-capitalist economic organization prove viable at scale?',
    'Comparative institutional analysis of successful alternative structures (Mondragon, OpenStack, municipal broadband); modeling of coordination costs for large-scale alternatives; historical analysis of periods of rapid organizational innovation',
    'If alternatives are structurally viable: suppression mechanism is active (snare classification holds). If alternatives are fundamentally unviable at scale: constraint approaches mountain classification (scale economies are immutable). The reading''s core claim depends on this omega being resolvable toward ''suppression, not impossibility.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_economic_organization_feasibility, empirical, 'Whether alternative economic organization is structurally feasible at scale').

omega_variable(
    enforcement_visibility_and_attribution,
    'How much of the incumbent''s dominance maintenance is due to deliberate enforcement (lobbying, exclusionary practices, predatory pricing) versus passive market dynamics (scale economies, consumer preference, network effects)?',
    'Forensic documentation of antitrust cases and settlements; analysis of lobbying expenditure and legislative outcomes; econometric decomposition of market concentration drivers; historical counterfactual: what would sector concentration look like with zero incumbent enforcement effort?',
    'If enforcement is major (>50%): snare classification is robust. If enforcement is minor (<20%): constraint approaches rope or mountain (coordination rather than extraction-plus-suppression). This omega discriminates between this beneficiary-maintained reading and the lapsed-alternative reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_visibility_and_attribution, empirical, 'Proportion of incumbent dominance attributable to active enforcement vs passive market dynamics').

omega_variable(
    kernel_contested_within_authority,
    'What is the status of the ''market dominance must be actively maintained'' axiom within mainstream economic authority structures?',
    'Content analysis of economic textbooks, central bank policy documents, antitrust authority guidelines; interviews with regulatory economists; trend analysis of academic publication on market structure and enforcement',
    'If the axiom is holdable (acknowledged as live debate): this reading coexists with sibling readings in a genuine contest. If the axiom is overridden (explicitly rejected within economic mainstream): this reading is minoritarian and requires documenting where the override occurred. If the axiom is foreclosed (treated as unchallengeable): this reading cannot be held within orthodox economic authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contested_within_authority, conceptual, 'Status of the ''active enforcement'' axiom within economic authority structures').

omega_variable(
    beneficiary_class_identification_and_stability,
    'Is ''incumbent capital holders'' a coherent beneficiary class, or do internal conflicts (competing firms, short-term vs long-term capital, financial vs industrial capital) undermine the class coherence?',
    'Analysis of political coalitions and lobbying alignment; measurement of intra-capital conflict intensity; identification of defections and cross-class alliances; historical analysis of whether capital holders have unified around dominance maintenance or fragmented',
    'If coherent class: snare classification is supported (unified beneficiary). If fragmented: constraint may distribute into multiple separate constraints (beneficiary_capital_A vs beneficiary_capital_B), or classify as tangled_rope (intra-beneficiary conflicts create enforcement instability). The hybrid_reading may be more accurate if class coherence is weak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_class_identification_and_stability, empirical, 'Coherence and stability of incumbent capital holders as unified beneficiary class').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mktnb_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mktnb_tr_t25, market_naturalization__beneficiary_maintained_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(mktnb_tr_t50, market_naturalization__beneficiary_maintained_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(mktnb_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mktnb_be_t25, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(mktnb_be_t50, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mktnb_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mktnb_su_t25, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(mktnb_su_t50, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, wage_suppression_labor_market).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, financial_extraction_debt_cycles).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, patent_strategy_innovation_lock).

% DUAL FORMULATION NOTE:
% Market dominance as 'beneficiary-maintained' constrains and sustains multiple downstream extraction mechanisms: labor-market monopsony, financial-sector rent extraction, and patent-based innovation control. Each downstream constraint has its own ε and perspectives but is structurally enabled by this upstream dominance structure. The upstream constraint's enforcement machinery (regulatory capture, supply-chain control) reduces enforcement costs for the downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
