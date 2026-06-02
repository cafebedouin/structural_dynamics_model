% ============================================================================
% CONSTRAINT STORY: temporal_mismatch_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_mismatch_governance, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_mismatch_governance
 *   human_readable: Temporal Mismatch Governance
 *   domain: institutional_design/coordination
 *
 * SUMMARY:
 *   Temporal Mismatch Governance is the structural constraint created by the
 *   fundamental misalignment between institutional decision-making cycles and
 *   the timescales of ecological, climate, and long-term resource processes.
 *   Governments operate on electoral cycles (2-5 years). Markets operate on
 *   earnings cycles (quarterly). Biological systems operate on generational
 *   timescales (decades to centuries). Climate systems operate on centennial
 *   and millennial timescales. This mismatch is not incidental — it creates
 *   systematic extraction from long-term interests in favor of short-term
 *   ones. The constraint exhibits all six DR types depending on the
 *   observer's temporal position: future generations see a snare; short-term
 *   decision-makers see coordination; long-term stakeholders see mixed
 *   extraction-coordination; organized movements see constrained governance;
 *   central banks with long mandates experience the hybrid directly;
 *   corporate sustainability reporting has become piton-level theater; and an
 *   analytical observer might mistake it for a natural law. The theater ratio
 *   has risen from 0.35 to 0.58 over twenty years as institutions have added
 *   long-term planning machinery (sustainability departments, ESG reporting,
 *   climate councils) without fundamentally reorganizing decision-making
 *   around those timescales. The constraint is a canonical tangled rope:
 *   genuine coordination function (short-term actors need to maintain
 *   legitimacy; long-term actors need baseline resource stability) combined
 *   with asymmetric extraction (short-term actors capture benefits; long-term
 *   actors bear costs and have no exit).
 *
 * KEY AGENTS:
 *   - Future Generations: Primary powerless victim (civilizational/trapped) — cannot participate; will bear full cost of temporal discounting
 *   - Ecosystems and Biotic Commons: Primary powerless victim (generational/trapped) — degrade at mismatched timescales; cannot represent themselves
 *   - Long-Horizon Stakeholders: Secondary moderate victim (generational/constrained) — participate in governance but structurally outweighed by short-term actors; constrained by resource barriers
 *   - Short-Term Decision Makers: Primary institutional beneficiary (immediate/arbitrage) — political systems and markets aligned with their timescales; high exit flexibility
 *   - Organized Environmental Movements: Organized actor (biographical/mobile) — coordinate around long-term concerns; benefit from governance attention but constrained within short-term institutional frames
 *   - Central Banks and Long-Horizon Regulators: Institutional hybrid (biographical/constrained) — mandates designed for longer horizons; pressured to serve short-term political cycles; both beneficiary and victim
 *   - Corporate Sustainability Reporting: Institutional performance mechanism (biographical/arbitrage) — creates appearance of long-term coordination without changing decision-making structure; piton classification reveals degradation
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent institutional choice (quarterly/electoral cycles) as unchangeable law of rational choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_mismatch_governance, 0.58).
domain_priors:suppression_score(temporal_mismatch_governance, 0.62).
domain_priors:theater_ratio(temporal_mismatch_governance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_mismatch_governance, extractiveness, 0.58).
narrative_ontology:constraint_metric(temporal_mismatch_governance, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(temporal_mismatch_governance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_mismatch_governance, tangled_rope).
narrative_ontology:human_readable(temporal_mismatch_governance, "Temporal Mismatch Governance").
narrative_ontology:topic_domain(temporal_mismatch_governance, "institutional_design/coordination").

domain_priors:requires_active_enforcement(temporal_mismatch_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_mismatch_governance, short_term_decision_makers).
narrative_ontology:constraint_beneficiary(temporal_mismatch_governance, institutional_incumbents).
narrative_ontology:constraint_victim(temporal_mismatch_governance, long_term_stakeholders).
narrative_ontology:constraint_victim(temporal_mismatch_governance, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Cannot participate in current decision-making; bear consequences of temporal discounting in governance. No exit option and no voice in institutions designed around short time horizons. Maximum extraction with maximum suppression.
constraint_indexing:constraint_classification(temporal_mismatch_governance, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ECOSYSTEMS AND BIOTIC COMMONS (SNARE) — Cannot represent themselves; degradation occurs at timescales mismatched to institutional governance cycles. Trapped in receiving extraction while governance operates on election cycles and quarterly earnings reports.
constraint_indexing:constraint_classification(temporal_mismatch_governance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LONG-HORIZON STAKEHOLDERS (TANGLED ROPE) — Communities dependent on long-term resource stability (water management, agriculture, fisheries) participate in governance but face severe constraints: their interests are structurally outweighed by short-term actors, yet they also benefit from coordination mechanisms that maintain baseline resource access. Mixed extraction and coordination.
constraint_indexing:constraint_classification(temporal_mismatch_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SHORT-TERM DECISION MAKERS (ROPE) — Political systems, quarterly earnings reports, and election cycles create alignment between institutional actors and immediate-term governance. Experience the constraint as coordination: solving the collective action problem of maintaining legitimacy across short decision windows. Net beneficiaries with high exit flexibility.
constraint_indexing:constraint_classification(temporal_mismatch_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED ENVIRONMENTAL MOVEMENTS (TANGLED ROPE) — Benefit from governance attention and policy mechanisms (regulatory frameworks, enforcement structures) that coordinate around long-term environmental concerns. Simultaneously extract costs: constrained to work within institutions designed for short-term optimization, forced to continually mobilize against institutionalized temporal discounting. Can exit (relocate advocacy, change targets) but at cost.
constraint_indexing:constraint_classification(temporal_mismatch_governance, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: CENTRAL BANKS AND LONG-HORIZON REGULATORS (TANGLED ROPE) — Institutional actors with mandates longer than political cycles (30-50 year horizons for central bank stability, climate regulation at decadal scales). Experience the constraint as mixed: their longer institutional memory and mandate enable coordination across temporal mismatches, but they are constrained by political pressure to serve short-term priorities. Both beneficiary (authority to coordinate) and victim (pressure to subordinate long-term mandates to political cycles).
constraint_indexing:constraint_classification(temporal_mismatch_governance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CORPORATE SUSTAINABILITY REPORTING (PITON) — The institutional apparatus (ESG metrics, sustainability reports, impact investing) is substantially theatrical: creates performance of long-term concern while decision-making remains tied to quarterly cycles. The reporting ritual has degraded into performative compliance; genuine long-horizon coordination is minimal. High theater ratio, low functional coordination.
constraint_indexing:constraint_classification(temporal_mismatch_governance, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational scope, temporal discounting appears as a mathematical law: rational actors maximize utility within their time horizons, institutions respond to electoral/market incentives on observable timescales, and mismatches between institutional decision windows and ecological/climate timescales are features of the system, not bugs. However, this naturalizes what is actually a contingent institutional design choice.
constraint_indexing:constraint_classification(temporal_mismatch_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_mismatch_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_mismatch_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_mismatch_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_mismatch_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_mismatch_governance, TR),
    TR >= 0.70.

:- end_tests(temporal_mismatch_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The temporal mismatch creates systematic advantages for short-term actors and costs for long-term stakeholders. The magnitude is not as severe as pure snares (ε ≥ 0.66) because many institutions have added long-term planning mechanisms (regulatory frameworks, environmental agencies, sustainability departments) that genuinely coordinate around longer horizons. But extraction is real and measurable: documented in resource depletion trajectories, biodiversity loss, climate overshoot, and institutional underinvestment in long-term infrastructure. The increase from 0.42 to 0.58 reflects accumulating extraction as short-term actors have optimized within the temporal frame they control. Suppression (0.62): High. Barriers to long-term voice include: (1) structural — future generations literally cannot participate; (2) epistemic — long-term consequences are uncertain and discounted in decision-making; (3) institutional — all formal governance structures (legislatures, boards, markets) are optimized for short decision windows; (4) economic — capital markets price in only immediate and near-term risks. Theater ratio (0.58): Moderate-high and rising. Institutional responses to temporal mismatch have increasingly become performative rather than structural: sustainability reporting without decision-making change, climate pledges without emissions reductions, intergenerational councils without actual power, ESG metrics without behavioral change. The rise from 0.35 to 0.58 reflects the expansion of the sustainability theater apparatus. Traditional governance remains locked in short-term cycles; the long-term machinery is bolted on without reorganizing the core decision structure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap emerges from the fundamental conflict between temporal scales. No agent experiences the same constraint identically because their time horizon determines what they perceive as extraction. To a short-term actor, long-term planning is a drag on efficiency (they perceive coordination costs as constraint). To a long-term stakeholder, short-term optimization is extraction (they perceive it as theft of their future). To future generations, the entire system is a snare (they have no choice but to accept the temporal structure short-term actors chose). To organized movements, it's a constrained hybrid (they can influence but not control). To central banks, it's a contradiction (their mandate says long-term; their political environment says short-term). To corporate sustainability, it's piton (we maintain the ritual because we've invested in the machinery, not because it works). The analytical observer's mountain view is the false summit — it mistakes the institutional choice (short-term optimization cycles) for a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each actor's structural position in the extraction flow. Future generations and ecosystems have d ≈ 0.95 (full targets) — they cannot organize, cannot exit, cannot represent themselves. Long-horizon stakeholders have d ≈ 0.70 (primary targets with some agency) — they participate in governance but are structurally outweighed. Short-term decision-makers have d ≈ 0.10 (beneficiaries with high exit flexibility) — they are aligned with institutional decision windows and have many alternatives if one governance structure fails. The derived f(d) values produce high χ for powerless agents (experiencing strong extraction) and low/negative χ for institutional beneficiaries (experiencing the constraint as coordination). Central banks and long-horizon regulators occupy an intermediate position (d ≈ 0.45) because their mandates align with long-term interests but they are institutionally subordinate to political actors optimized for short-term cycles. Organized movements have d ≈ 0.55 (moderate targets) — they can organize and exit but face high institutional costs for doing so.
 *
 * MANDATROPHY ANALYSIS:
 *   TEMPORAL MISMATCH GOVERNANCE resolves its mandatrophy by showing that the constraint is genuinely tangled: (1) coordination function is real — all actors need some mechanism to coordinate across time horizons, even if asymmetrically; (2) asymmetric extraction is real — short-term actors capture benefits while long-term actors bear costs and have no escape; (3) active enforcement is real — institutional structures, capital markets, electoral systems, and reporting frameworks are actively maintained to preserve short-term optimization. The mandatrophy question is not whether the constraint is coordination or extraction, but whether the coordination function requires the asymmetric extraction. Empirically: No. Institutional designs that align decision-making timescales (central banks with 30+ year mandates, indigenous governance with seven-generation principles, long-horizon investment vehicles like endowments and sovereign wealth funds) demonstrate that genuine long-term coordination is possible without extracting from the future. The theater ratio (0.58) indicates that much of the institutional response has degraded into performance — sustainability departments, ESG metrics, climate pledges exist without restructuring the underlying decision cycles. The mandatrophy is resolved by recognizing that the constraint is a tangled rope that could be unwound: institutions could be redesigned to genuinely coordinate across timescales (low-theater coordination) rather than maintain short-term optimization with performative long-term planning attached. Current trajectory points toward increasing piton dynamics as institutions invest more in long-term machinery without changing short-term decision structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_actor_assumption,
    'Is temporal discounting an inherent feature of rational decision-making or a contingent artifact of institutional structures that measure success on short cycles?',
    'Comparative analysis of institutions with different time horizons: central banks (50+ year mandates), indigenous governance (seven-generation principles), corporate entities (with vs without long-term incentive structures). Determine whether discount rates correlate with institutional measurement window or with economic fundamentals.',
    'If contingent: temporal mismatch governance is a tangled rope with solvable design flaws. If inherent: it may approach mountain status — an unchangeable law of rational choice under uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption, conceptual, 'Whether temporal discounting is rational necessity or institutional contingency').

omega_variable(
    representation_mechanism_sufficiency,
    'Can procedural mechanisms (citizen assemblies, intergenerational councils, long-term budgeting) adequately represent the interests of those without voice (future generations, ecosystems)?',
    'Empirical analysis of outcomes from institutions using proxy-representation mechanisms (e.g., environmental trusts, intergenerational justice commissions) vs. standard representative democracy. Track whether proxy representation actually improves long-term outcome trajectories or merely improves optics.',
    'If mechanisms work: the snare perspective can be upgraded toward tangled rope — trapped agents gain partial agency. If mechanisms are theater: the constraint remains snare-level for future generations, and piton dynamics dominate (performative long-termism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_mechanism_sufficiency, empirical, 'Effectiveness of proxy representation for voiceless long-term stakeholders').

omega_variable(
    temporal_coordination_coupling,
    'What is the empirical relationship between the misalignment of institutional decision cycles and ecological/climate timescales, and the magnitude of extraction from long-term stakeholders?',
    'Multi-sector analysis correlating institutional governance window length (election cycles, budget periods, corporate reporting intervals) with outcomes for long-term interests (biodiversity loss rates, climate tipping points, resource depletion trajectories). Identify whether tighter temporal alignment produces better long-term outcomes.',
    'High coupling suggests the temporal mismatch is the primary extraction mechanism (validate tangled rope classification). Low coupling suggests other institutional failures dominate and temporal mismatch is secondary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_coordination_coupling, empirical, 'Correlation between institutional time horizons and long-term stakeholder outcomes').

omega_variable(
    sustainability_theater_boundary,
    'At what threshold of institutional effort devoted to long-term planning does the piton classification (theater ≥ 0.70) transition to genuine coordination (theater ≤ 0.50)?',
    'Longitudinal tracking of institutional investment in long-term governance mechanisms (climate agencies, environmental regulators, intergenerational councils) and correlation with actual outcome improvement vs. metric improvement. Distinguish between gaming metrics and genuine ecological/climate stabilization.',
    'If low threshold: major institutional reforms needed immediately. If high threshold: current institutional scaffolding may transition out of piton range as investment accumulates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_theater_boundary, empirical, 'Theater-to-coordination transition threshold for sustainability institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_mismatch_governance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tmg_tr_t0, temporal_mismatch_governance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tmg_tr_t10, temporal_mismatch_governance, theater_ratio, 10, 0.48).
narrative_ontology:measurement(tmg_tr_t20, temporal_mismatch_governance, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(tmg_be_t0, temporal_mismatch_governance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tmg_be_t10, temporal_mismatch_governance, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(tmg_be_t20, temporal_mismatch_governance, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_mismatch_governance, enforcement_mechanism).
narrative_ontology:affects_constraint(temporal_mismatch_governance, discount_rate_lock_in).
narrative_ontology:affects_constraint(temporal_mismatch_governance, electoral_cycle_dominance).
narrative_ontology:affects_constraint(temporal_mismatch_governance, quarterly_capitalism).
narrative_ontology:affects_constraint(temporal_mismatch_governance, ecological_tipping_point_lag).

% DUAL FORMULATION NOTE:
% Temporal Mismatch Governance is upstream of domain-specific constraints (electoral cycles in politics, quarterly earnings in markets, generational turnover in institutions). Each downstream constraint inherits the temporal mismatch structure but manifests it differently. The core extractiveness (0.58) applies across all domains; domain-specific stories can decompose how the mismatch manifests in particular institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
