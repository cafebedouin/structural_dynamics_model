% ============================================================================
% CONSTRAINT STORY: knowledge_action_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_action_gap, []).

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
 *   constraint_id: knowledge_action_gap
 *   human_readable: The Informational Friction Barrier
 *   domain: social/technological
 *
 * SUMMARY:
 *   The knowledge-action gap represents a structural constraint where
 *   information about climate risk and other systemic threats is widely
 *   available, verified by scientific consensus, and accessible to
 *   decision-makers and the general public, yet coordinated action to
 *   mitigate these risks remains severely constrained. This is not a failure
 *   of information provision — it is a failure of the gap between knowing and
 *   doing. The constraint operates through multiple simultaneous mechanisms:
 *   switching costs that lock individuals and institutions into
 *   carbon-intensive systems, cognitive load from information overload that
 *   produces decision paralysis, attention economics that favor incumbent
 *   industries' disinformation over climate action framing, and institutional
 *   inertia that maintains performative consensus-building rather than
 *   action. The constraint exhibits tangled rope structure because it bundles
 *   genuine coordination problems (collective action free-rider dilemmas,
 *   multi-stakeholder negotiation) with extraction mechanisms (suppression of
 *   alternative framings, capture of policy by carbon-intensive interests,
 *   and maintenance of infrastructure lock-in). The gap between what is known
 *   and what is done is not accidental — it is actively maintained through
 *   enforcement: incumbent industries fund distraction campaigns, algorithmic
 *   systems amplify engaging-but-paralytic content, and institutions invest
 *   in theatrical consensus-building rather than mobilization.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) — bear climate costs despite having no decision-making role; temporal asymmetry ensures extraction
 *   - Climate-Vulnerable Populations: Primary victim (powerless/trapped) — geographic and economic immobility traps them in harm's way despite knowledge of risk
 *   - Individual Consumers: Secondary victim (moderate/constrained) — have information and some choice capacity, but face switching costs and cognitive load that suppress action
 *   - Carbon-Intensive Industries: Primary beneficiary (institutional/arbitrage) — maintain economic rents through suppression mechanisms including disinformation, regulatory capture, and attention capture
 *   - Green Finance Institutions: Beneficiary (institutional/arbitrage) — capture emerging markets in renewable energy and carbon credits enabled by the gap
 *   - Climate Movement Organizations: Mixed (organized/mobile) — provide coordination function but constrained by incumbent suppression and limited attention share
 *   - Climate Science Institutions: Theatrical maintainer (institutional/arbitrage) — perform consensus-building rituals whose original function has atrophied; constrained by professional norms from directness
 *   - Analytical Observer: External view (analytical/analytical) — observes both genuine coordination problems and genuine suppression mechanisms operating simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_action_gap, 0.52).
domain_priors:suppression_score(knowledge_action_gap, 0.68).
domain_priors:theater_ratio(knowledge_action_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_action_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(knowledge_action_gap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(knowledge_action_gap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_action_gap, tangled_rope).
narrative_ontology:human_readable(knowledge_action_gap, "The Informational Friction Barrier").
narrative_ontology:topic_domain(knowledge_action_gap, "social/technological").

domain_priors:requires_active_enforcement(knowledge_action_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_action_gap, incumbent_carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(knowledge_action_gap, attention_commodifiers).
narrative_ontology:constraint_beneficiary(knowledge_action_gap, status_quo_institutions).
narrative_ontology:constraint_victim(knowledge_action_gap, future_generations).
narrative_ontology:constraint_victim(knowledge_action_gap, climate_vulnerable_populations).
narrative_ontology:constraint_victim(knowledge_action_gap, cognitive_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Cannot exit or negotiate the constraint; bear the full cost of inaction despite having no role in its maintenance. Trapped by temporal asymmetry: information about climate risk exists today, but response capacity compressed into a narrow window. d≈0.98, f(d)≈1.43, σ=1.2 → χ≈0.88. Pure extraction masquerading as information provision.
constraint_indexing:constraint_classification(knowledge_action_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Low-income communities in flood-prone regions, drought-affected agricultural areas, and heat-vulnerable urban neighborhoods have detailed knowledge of climate impacts but cannot exit the geographic or economic constraint that traps them in harm's way. Information + immobility = pure extraction. d≈0.96, f(d)≈1.41, σ=0.9 → χ≈0.85.
constraint_indexing:constraint_classification(knowledge_action_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDIVIDUAL CONSUMERS (TANGLED ROPE) — Have access to climate information and can make individual choices (diet, transportation, consumption), but face switching costs (habit, infrastructure dependency, price differentials) and cognitive load (decision paralysis from information overload). The constraint enforces coordination function (collective action problems require info-sharing) while extracting behavioral surplus through friction. d≈0.62, f(d)≈0.80, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GREEN FINANCE INSTITUTIONS (ROPE) — Banks, investment funds, and ESG-certified firms benefit from the knowledge-action gap by capturing emerging markets in renewable energy, carbon credits, and climate adaptation services. The constraint is coordination mechanism: information asymmetries create arbitrage opportunities; firms are incentivized to bridge gaps. d≈0.12, f(d)≈0.15, σ=1.2 → χ≈0.09. Net beneficiary; sees the constraint as market structure to exploit, not burden.
constraint_indexing:constraint_classification(knowledge_action_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CARBON-INTENSIVE INDUSTRIES (ROPE) — Oil, coal, and high-emission manufacturers benefit from the knowledge-action gap's suppression mechanisms (lobbying, attention capture, regulatory capture). From their view, the constraint is a coordination problem they are solving: maintaining public acceptance of carbon-intensive systems despite awareness of climate impacts. They experience the constraint as coordination because they are actively enforcing it and capturing economic rents. d≈0.08, f(d)≈0.08, σ=1.2 → χ≈0.05. Net beneficiary through active enforcement.
constraint_indexing:constraint_classification(knowledge_action_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE MOVEMENT ORGANIZATIONS (TANGLED ROPE) — NGOs, activist coalitions, and climate-focused nonprofits experience the constraint as both coordination mechanism (information they provide enables collective action) and extraction (they must expend resources competing for attention against incumbent industries' disinformation and distraction). The constraint enforces a specific framing: only certain types of climate solutions get attention, while systemic solutions face suppression. d≈0.52, f(d)≈0.65, σ=1.2 → χ≈0.40. Mixed: they are organized agents with some exit capacity (can reallocate resources) but constrained by the very communication infrastructure they are trying to use.
constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE SCIENCE INSTITUTIONS (PITON) — Universities, research centers, and scientific bodies maintain elaborate peer review, publication, and assessment rituals (IPCC reports, consensus statements, model refinements) whose primary function has become performative: the information is already conclusive about warming and human causation, yet the institutions continue theatrical consensus-building. theater_ratio=0.65 reflects the gap between what climate science knows and how it communicates (hedging language, probabilistic framing, conservative messaging) to maintain institutional legitimacy. The institutions are trapped by inertia — they cannot communicate with maximal directness without destabilizing professional norms.
constraint_indexing:constraint_classification(knowledge_action_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the knowledge-action gap is a hybrid constraint combining genuine coordination problems (collective action in the presence of free-rider incentives, multi-stakeholder negotiation) with genuine extraction mechanisms (incumbent industry suppression, attention economics favoring distracting content, infrastructure lock-in). The constraint bundles coordination with suppression in a way that makes separation difficult. d≈0.58, f(d)≈0.72, σ=1.2 → χ≈0.45. The analytical view confirms tangled rope: real coordination function, real asymmetric extraction, active enforcement through multiple channels.
constraint_indexing:constraint_classification(knowledge_action_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_action_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_action_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_action_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_action_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_action_gap, TR),
    TR >= 0.70.

:- end_tests(knowledge_action_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over the 30-year measurement interval. Initial extractiveness (0.28) reflects genuine coordination costs and switching expenses inherent to transition. Final extractiveness (0.52) reflects the accumulation of deliberately enforced friction: disinformation campaigns, algorithmic amplification of paralyzing content, regulatory capture preventing carbon pricing, infrastructure lock-in maintained through subsidy and zoning. The rise in extractiveness indicates that the constraint has shifted from a genuine coordination challenge to an enforced extraction mechanism. Suppression (0.68): High. Multiple channels suppress action: incumbent industry lobbying and legal obstruction, media narratives emphasizing individual responsibility and technological solutions (shifting burden from systemic action), algorithmic recommendation systems that amplify engaging-but-paralyzing content, institutional messaging that frames climate risk in probabilistic/hedged language, and infrastructure that makes low-carbon choices expensive and inconvenient. Theater ratio (0.65): Moderate-high, increasing over time. Initial theater (0.42) reflects legitimate scientific consensus-building and policy deliberation. Final theater (0.65) reflects the gap between institutional performance (IPCC reports, national climate pledges, corporate net-zero commitments) and actual emissions trajectories — the rituals are increasingly performative, serving institutional legitimacy rather than action.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as six different types from different structural positions. Future generations and vulnerable populations experience pure extraction (Snare) — they have no exit option and bear all costs. Individual consumers experience mixed coordination-extraction (Tangled Rope) — the constraint both solves a real collective action problem and suppresses their ability to coordinate solutions. Incumbent industries experience pure coordination (Rope) — they are actively solving the problem of maintaining public acceptance of carbon-intensive systems. Green finance experiences coordination (Rope) — they are arbitraging the gap. Climate movement experiences mixed burden (Tangled Rope) — they must provide coordination while suppressed by incumbent forces. Climate science institutions experience degraded ritual (Piton) — their consensus-building has become performative. The analytical observer sees the tangled structure: genuine coordination problems bound together with genuine suppression mechanisms, making separation difficult and resolution ambiguous. The perspectival gap reveals that the constraint cannot be resolved by information alone — the powerless victim perspectives show pure extraction (Snare), while the beneficiary perspectives show that they are actively maintaining the constraint through enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations: Victim + trapped → d≈0.98, f(d)≈1.43. No exit, bear all costs. Climate-vulnerable populations: Victim + trapped → d≈0.96, f(d)≈1.41. Geographic/economic immobility enforces extraction. Individual consumers: Victim + constrained → d≈0.62, f(d)≈0.80. Can make some choices but high switching costs limit true exit. Carbon-intensive industries: Beneficiary + arbitrage → d≈0.08, f(d)≈0.08. Can exit the constraint entirely (could transition business models) but choose not to; arbitrage through enforcement. Green finance: Beneficiary + arbitrage → d≈0.12, f(d)≈0.15. Arbitrage opportunity from gap. Climate movement organizations: Mixed + mobile → d≈0.52, f(d)≈0.65. Organized agents with some exit capacity; constrained by the communication landscape they are trying to use. Climate science institutions: Institutional + arbitrage → d≈0.08, f(d)≈0.08. Piton classification from theater gate, not directionality; they could communicate more directly (could exit institutional inertia) but maintain theatrical norms. Analytical observer: analytical → d≈0.58, f(d)≈0.72. Confirms tangled rope structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through multi-perspective analysis that shows both genuine coordination function and genuine extraction mechanism operating simultaneously. Future generations experience Snare (no mitigation coordination function visible from their position). Individual consumers experience Tangled Rope (the constraint both enables collective action framing and suppresses individual action). Incumbent industries experience Rope (they are solving the coordination problem of maintaining legitimacy). The analytical observer sees Tangled Rope at civilizational scope: the constraint bundles a real coordination problem (free-rider dilemmas in climate action, multi-stakeholder negotiation, tragedy of the commons dynamics) with a real extraction mechanism (suppression through disinformation, regulatory capture, infrastructure lock-in, and attention capture). Neither classification alone is correct — the constraint is neither pure coordination nor pure extraction. The rising extractiveness over time (0.28 → 0.52) indicates that the extraction component has been deliberately amplified: early in the interval, the gap reflected genuine transition costs; later, it reflects enforced friction designed to maintain incumbent rents. This trajectory confirms Tangled Rope: a coordination mechanism that has been captured and weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_load_attribution,
    'Is the primary barrier to action genuine cognitive load from information overload, or is it manufactured distraction from incumbent industries?',
    'Experimental studies comparing climate action rates under conditions of (a) high-quality information + low-distraction vs (b) equivalent information + high-distraction. Cross-national analysis of action rates vs advertising spend by carbon-intensive industries.',
    'If genuine load dominates: constraint is primarily coordination problem (Rope classification more justified). If distraction dominates: constraint is primarily extraction mechanism (Snare classification more justified for consumer perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load_attribution, empirical, 'Attribution of inaction to cognitive load vs manufactured distraction').

omega_variable(
    switching_cost_malleability,
    'Are the switching costs (infrastructure, price differentials, habit entrenchment) inherent to the transition, or are they artificially inflated by policy and market structure?',
    'Historical comparison to prior infrastructure transitions (fossil fuels replacing whale oil, electricity replacing gas lamps). Analysis of jurisdictions with different carbon pricing, renewable subsidies, and zoning policies to identify cost reductions. Behavioral experiments isolating habit-loop strength from actual monetary barriers.',
    'If inherent: transition is genuinely costly, and gap reflects rational economic calculation (Rope/Tangled Rope). If inflated: gap is enforced rent-seeking (Snare/Tangled Rope with higher extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_malleability, empirical, 'Whether switching costs are inherent or artificially inflated').

omega_variable(
    collective_action_solution_feasibility,
    'Can decentralized coordination mechanisms (local climate action, community energy, individual carbon pricing) solve the collective action problem without central enforcement, or is government mandate required?',
    'Longitudinal tracking of grassroots climate initiatives (transition towns, community solar, local divestment). Analysis of scaling dynamics: what percentage of emissions reductions come from voluntary action vs regulated/mandated action in various jurisdictions.',
    'If decentralized coordination works: constraint is primarily informational (Rope/Scaffold classification justified). If mandate is required: constraint embeds a suppression mechanism preventing non-coercive solutions (Snare/Tangled Rope with higher suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_solution_feasibility, empirical, 'Feasibility of decentralized solutions vs necessity of government mandate').

omega_variable(
    attention_economy_structural_necessity,
    'Is attention scarcity and the resulting competition for saliency a structural feature of human cognition and media systems, or is it an artificially maintained rent-extraction mechanism?',
    'Neuroscience on attentional capacity limits; comparative media analysis of jurisdictions with different advertising regulations, public broadcasting models, and algorithmic curation. Historical analysis of periods of high collective action despite information scarcity (WWII mobilization, moon landing, civil rights) vs present paralysis despite information abundance.',
    'If structural: knowledge-action gap is inevitable, and suppression is quasi-natural (Mountain classification risk). If maintained: attention hoarding is deliberate extraction mechanism (Snare/Tangled Rope with higher suppression, theater ratio as distraction metric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_economy_structural_necessity, conceptual, 'Whether attention scarcity is structural or manufactured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_action_gap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kag_tr_t0, knowledge_action_gap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(kag_tr_t15, knowledge_action_gap, theater_ratio, 15, 0.54).
narrative_ontology:measurement(kag_tr_t30, knowledge_action_gap, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(kag_be_t0, knowledge_action_gap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kag_be_t15, knowledge_action_gap, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(kag_be_t30, knowledge_action_gap, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_action_gap, resource_allocation).
narrative_ontology:affects_constraint(knowledge_action_gap, infrastructure_lock_in).
narrative_ontology:affects_constraint(knowledge_action_gap, attention_economy_asymmetry).
narrative_ontology:affects_constraint(knowledge_action_gap, regulatory_capture_fossil_fuels).

% DUAL FORMULATION NOTE:
% The knowledge-action gap decomposes into three structurally distinct sub-constraints: (1) infrastructure_lock_in (ε≈0.35, physical/economic barriers to switching), (2) attention_economy_asymmetry (ε≈0.48, incumbent advantage in saliency capture), and (3) regulatory_capture_fossil_fuels (ε≈0.55, policy suppression). These three constraints reinforce each other through the network. The knowledge-action gap story captures the unified effect; individual stories capture the structural mechanisms. Each sub-constraint has different potential solutions and different omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_action_gap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
