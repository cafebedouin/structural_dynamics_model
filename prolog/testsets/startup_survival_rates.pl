% ============================================================================
% CONSTRAINT STORY: startup_survival_rates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_startup_survival_rates, []).

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
 *   constraint_id: startup_survival_rates
 *   human_readable: Startup Survival Rate Constraint
 *   domain: entrepreneurship/venture_capital/economic_policy
 *
 * SUMMARY:
 *   The startup constraint encompasses the structural apparatus by which
 *   entrepreneurial capital is allocated, founders are selected, and failure
 *   is distributed. Nominally a coordination mechanism (matching capital with
 *   opportunity), it functions as a mixed coordination-extraction hybrid that
 *   concentrates rewards among a minority (successful founders and venture
 *   investors) while distributing losses broadly (90% of startups fail;
 *   founders bear sunk cost; workers lose salary and equity). The constraint
 *   exhibits high perspectival variance: from the founder's view it is a
 *   snare (trapped, no exit), from the venture firm's view it is a rope (pure
 *   coordination), from the startup worker's view it is tangled rope (mixed),
 *   and from the institutional piton view it is a degraded ritual (high
 *   theater, low function). The theater ratio (0.68) reflects extensive
 *   performative activity: pitching, due diligence, board governance, and
 *   narrative framing maintain the appearance of meritocratic capital
 *   allocation while empirical outcomes show extreme path dependency (founder
 *   background, geography, prior success are stronger predictors than market
 *   opportunity). The extractiveness value (0.58) reflects that the
 *   constraint systematically extracts from founders and workers to benefit
 *   venture capital and successful survivors. However, this extraction is not
 *   total (snare-level) because the venture structure does solve genuine
 *   coordination problems: it aggregates capital, creates accountability
 *   mechanisms, and enables risk-sharing that individual capital sources
 *   cannot. The constraint is most accurately classified as tangled_rope from
 *   the analytical perspective: genuine coordination function (capital
 *   allocation) with significant asymmetric extraction (beneficiary
 *   selection, control concentration, failure distribution).
 *
 * KEY AGENTS:
 *   - Venture Capital Firms: Primary beneficiary (institutional/arbitrage) — controls capital allocation, can exit losing positions, benefits from portfolio concentration and 90% failure rate that creates extreme upside for 10% successes
 *   - Successful Founders: Secondary beneficiary (moderate/mobile) — capture equity upside and network effects; selective rewards create aspiration and participation in the constraint
 *   - Failed Entrepreneurs: Primary victim (powerless/trapped) — sunk costs (time, capital, identity, reputation), social stigma, debt obligations, no exit pathway within venture structure
 *   - Early-Stage Founders: Constrained participant (moderate/constrained) — experience both coordination (capital access, validation) and extraction (dilution, control loss, pressure for rapid growth)
 *   - Startup Workers: Secondary victim (moderate/constrained) — benefit from growth-phase participation but bear disproportionate downside risk, equity lock-in, foregone stable employment
 *   - Startup Ecosystem Institutions: Organized agents (organized/constrained) — accelerators, incubators, government programs building alternative pathways with sunset logic for the venture monopoly
 *   - Venture Capital Model (Institutional): Path-dependent actor (institutional/arbitrage) — persists through network effects and narrative authority despite potential degradation of primary function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees constraint as contingent institutional arrangement, not inherent startup property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(startup_survival_rates, 0.58).
domain_priors:suppression_score(startup_survival_rates, 0.65).
domain_priors:theater_ratio(startup_survival_rates, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(startup_survival_rates, extractiveness, 0.58).
narrative_ontology:constraint_metric(startup_survival_rates, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(startup_survival_rates, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(startup_survival_rates, tangled_rope).
narrative_ontology:human_readable(startup_survival_rates, "Startup Survival Rate Constraint").
narrative_ontology:topic_domain(startup_survival_rates, "entrepreneurship/venture_capital/economic_policy").

domain_priors:requires_active_enforcement(startup_survival_rates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(startup_survival_rates, venture_capital_firms).
narrative_ontology:constraint_beneficiary(startup_survival_rates, successful_founders).
narrative_ontology:constraint_victim(startup_survival_rates, failed_entrepreneurs).
narrative_ontology:constraint_victim(startup_survival_rates, early_stage_founders).
narrative_ontology:constraint_victim(startup_survival_rates, workers_in_failed_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAILED ENTREPRENEUR (SNARE) — Trapped by sunk costs, social stigma of failure, and debt obligations. Bears full extraction cost. Cannot escape: invested personal capital, time, identity, and reputation. The constraint offers no coordination benefit — only selective success for a minority. Maximum extraction from the perspective of those who fail (90% of startups). No alternative pathways available within the same capital and social structures.
constraint_indexing:constraint_classification(startup_survival_rates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-STAGE FOUNDER (TANGLED ROPE) — Constrained by funding dependency, market uncertainty, and need for venture validation. Experiences both coordination (pitch process clarifies business model) and extraction (dilution, control loss, pressure for rapid growth over sustainable operations). Can exit but at significant cost: forgone equity upside, restart penalty, network damage. Partial agency — some control over direction but constrained by capital structure.
constraint_indexing:constraint_classification(startup_survival_rates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VENTURE CAPITAL FIRM (ROPE) — Net beneficiary (arbitrage exit). Controls allocation of capital and can walk away from losing positions. Experiences constraint as coordination: portfolio approach spreads risk across many startups; venture structure coordinates between founders and capital sources. Low effective extraction — can exit any single position. Extraction flows toward this agent, not away. Benefits from the constraint structure itself (the economics of 90% failure enable the 10% success payoff).
constraint_indexing:constraint_classification(startup_survival_rates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STARTUP WORKER (TANGLED ROPE) — Constrained by labor market uncertainty and equity compensation lock-in. Benefits from participation in growth phase (learning, network effects) but bears disproportionate downside risk (equity worthless at failure, foregone stable-job salary). More mobility than founders but constrained by sunk relocation costs and opportunity cost of leaving before vesting. Mixed extraction and coordination.
constraint_indexing:constraint_classification(startup_survival_rates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STARTUP SUPPORT ECOSYSTEM (SCAFFOLD) — Organized actors (accelerators, incubators, government startup programs, angel investor networks) see the high-failure environment as a temporary coordination problem with diminishing suppression over time. These institutions are building parallel pathways: mentorship networks reduce information asymmetries, pitch competitions democratize capital access, government grants reduce equity dilution, and corporate innovation labs create alternative exit pathways. Effective extraction declines as the ecosystem matures and provides non-venture alternatives. Has sunset logic: as these alternatives gain legitimacy, the venture-only bottleneck loses force. Theater is moderate — genuine coordination activity (mentorship, education) alongside performative elements (pitch events, demo days).
constraint_indexing:constraint_classification(startup_survival_rates, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: VENTURE CAPITAL MODEL (PITON) — The institutional framework persists through path dependency and network effects, even as its core function (efficient capital allocation to high-growth ventures) has degraded. The 90% failure rate is often characterized as 'just how startup risk works,' but this naturalizes what is actually a contingent capital structure: the venture model concentrates capital and decision-making, creating information asymmetries that increase failure risk. The model persists because (a) successful outcomes are highly visible, (b) survivors have captured narrative authority, and (c) institutional capital is locked into venture infrastructure. Theater ratio is high: extensive performative activity (pitching, networking, due diligence theater) maintains the appearance of meritocracy while outcomes show extreme path dependency (founder background, prior success, geographic location).
constraint_indexing:constraint_classification(startup_survival_rates, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From a universal perspective, startup failure rates reflect underlying economic coordination problems: how to allocate scarce capital efficiently, how to test uncertain business models, how to align founder and investor incentives. These are genuine coordination challenges. The constraint structure (venture capital model) is one solution to these problems, and it does solve them — but asymmetrically, concentrating benefits among a minority while distributing costs widely. The high failure rate is not inherent to entrepreneurship but contingent on the specific institutional arrangement. Alternative models (cooperative funding, public venture banks, substrate diversity) could produce different failure distributions.
constraint_indexing:constraint_classification(startup_survival_rates, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(startup_survival_rates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(startup_survival_rates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(startup_survival_rates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(startup_survival_rates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(startup_survival_rates, TR),
    TR >= 0.70.

:- end_tests(startup_survival_rates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint systematically extracts from founders and workers to benefit venture capital and successful survivors. The measurement reflects: (a) failure concentration (90% fail, bearing sunk costs), (b) control consolidation (founders lose decision authority), (c) dilution dynamics (founders' initial ownership is progressively diluted), (d) exit asymmetry (VCs can exit at loss, founders cannot). However, extractiveness is not snare-level (0.66+) because genuine coordination occurs: capital does get allocated, business models do get tested, accountability mechanisms do exist. The venture structure solves a real problem, which means some of the extracted value is payment for coordination service. The measurement reflects this mixed character. Suppression (0.65): High. Significant barriers prevent founders from exiting or pursuing alternative capital: (a) sunk cost fallacy (psychologically hard to abandon invested effort), (b) social stigma (failure is culturally marked as permanent), (c) equity lock-in (restricted stock requires vesting), (d) information monopoly (venture capital controls access to due diligence, term sheets, investor networks), (e) narrative capture (survivor-bias media creates false success expectations), (f) legal structures (founder agreements, board control). Theater ratio (0.68): Moderate-high. Significant performative activity maintains the constraint: pitching (theater for founder evaluation), board governance (theater for accountability), due diligence (theater for capital protection), ecosystem events (theater for community/legitimacy). The ratio increased over the 10-year interval (from 0.55 to 0.68) as the ecosystem professionalized and institutionalized its theatrical components. This reflects Goodhart drift: as metrics (pitch quality, board composition, founder background diversity) became more visible and optimized, the actual coordination function (matching capital to high-growth opportunity) became harder to verify, and theater increased to fill the verification gap.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental perspectival divergence. The venture capital firm sees a pure coordination mechanism (Rope): we aggregate capital, reduce individual risk through portfolio effects, and create accountability through equity alignment. This is their genuine experience — they do coordinate and they do benefit. The failed entrepreneur sees a snare: we invested everything, played by the rules, and lost it all. We cannot exit. We bear 100% of the extraction. The early-stage founder sees tangled rope: we get capital access and validation (coordination benefit) but we lose decision authority and face dilution pressure (extraction cost). The startup worker sees tangled rope: we learn, network, and participate in growth (benefit) but our equity is likely worthless and we forgoed stable salary (cost). The ecosystem institutions see a scaffold: the venture monopoly is real but temporary; our programs are building alternatives (accelerators, government funding, corporate spin-offs) that will eventually substitute for venture capital, reducing the extraction mechanism. The piton perspective notes that the venture model persists even as its core function (efficient capital allocation) has degraded — it's maintained by path dependency and narrative authority rather than genuine comparative advantage. The analytical perspective notes that the 90% failure rate is contingent on the venture structure, not inherent to entrepreneurship — alternative models (public venture banks, cooperative funding, substrate diversity) could produce different failure distributions. No single perspective is 'wrong' — each captures a real structural aspect. The perspectival gap reveals that what looks like meritocratic capital allocation from one angle looks like extractive concentration from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural relationship to the extraction flow and their exit options. Venture capital firms have d ≈ 0.05 (full beneficiary with arbitrage exit): they control the constraint, can walk away from losing positions, and capture disproportionate upside from successful outcomes. Successful founders have d ≈ 0.25 (beneficiary with some extraction): they receive rewards but had to surrender control and dilution on the way up; they are selective beneficiaries. Failed entrepreneurs have d ≈ 0.95 (full target with trapped exit): they bear all costs, cannot exit, and have no recovery pathway within the venture structure. Early-stage founders have d ≈ 0.65 (mixed victim/beneficiary with constrained exit): they access capital and validation but lose decision authority and face significant extraction through dilution and control consolidation. Startup workers have d ≈ 0.80 (victim with constrained exit): they bear downside risk but can walk away by accepting loss of equity and finding stable employment elsewhere. Ecosystem institutions have d ≈ 0.55 (organized victim with constrained exit): they see the venture monopoly as extraction but are building alternatives with long timelines. The piton perspective (institutional/arbitrage) has d ≈ 0.15 (beneficiary through institutional inertia): the model persists not because it extracts deliberately but because institutions are locked in and narrative authority is established. These directionality values feed the sigmoid function f(d) to produce perceived extractiveness (chi), which is scaled by scope modifier σ(S). Because this constraint operates at national/global scope (σ=1.0 to 1.2), the effective extraction chi is not dampened by locality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disambiguating coordination function from extraction mechanism. The venture capital model DOES coordinate (matches capital to opportunity, allocates risk, creates accountability) — this is genuine. It ALSO extracts asymmetrically (concentrates rewards, distributes losses, suppresses alternatives) — this is also genuine. The mandatrophy question is: 'Is this a coordination mechanism with extraction overhead, or an extraction mechanism wearing a coordination mask?' The empirical answer: both are true simultaneously. The venture structure solves a real coordination problem (how to allocate scarce capital with uncertain outcomes). The solution is extractive by design: it concentrates decision-making, creates information monopolies, and distributes failure costs to minimize loss concentration for capital providers. This is optimal from a capital-protection perspective and suboptimal from a founder-welfare perspective. The tangled_rope classification holds because: (a) beneficiaries exist (venture firms, successful founders), (b) victims exist (failed founders, startup workers, non-venture startups), (c) active enforcement exists (term sheets, board control, equity structures). The constraint is not a pure snare (which would have minimal coordination function) nor a pure rope (which would have minimal asymmetric extraction). The mandatrophy is resolved by accepting that genuine coordination and asymmetric extraction can coexist in the same structure — in fact, the extraction mechanism (concentration of decision-making, control through equity, failure distribution) is what makes the coordination function work from the capital provider's perspective. The perspectival gap reveals that from the founder's perspective, the extraction is more salient than the coordination; from the capital provider's perspective, the coordination is more salient than the extraction. Neither perspective is false — each captures a real structural aspect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    failure_rate_inherence,
    'Is the 90% startup failure rate an inherent feature of early-stage business risk or a function of venture capital''s specific capital structure and selection criteria?',
    'Comparative analysis of failure rates across capital models: bootstrapped startups vs venture-funded, government-supported startups, cooperative models, and corporate spin-offs. Geographic and temporal variation in failure rates across different institutional environments.',
    'If inherent: constraint is mountain-adjacent (structural limit on business formation). If contingent on VC structure: constraint is pure extraction mechanism (Snare) when measured from founder perspective, and the venture model''s persistence is Piton. This resolves the mandatrophy by determining whether high failure is cost-of-coordination or extractive rent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(failure_rate_inherence, empirical, 'Whether high failure rates are inherent to startup risk or structural to venture capital model').

omega_variable(
    information_asymmetry_magnitude,
    'How much of the venture-founder failure gap is attributable to genuine information asymmetry (capital allocation problem) vs deliberate extraction (venture-founder misalignment of incentives)?',
    'Analysis of venture portfolio allocation patterns: do ventures with better information environments (founder with prior exits, strong domain expertise, transparent market data) have proportionally lower failure rates? Do ventures with aligned incentives (founder maintains meaningful control, downside protection, longer runway) have different failure distributions?',
    'If asymmetry dominates: tangled rope classification is appropriate (genuine coordination with extraction overhead). If deliberate misalignment dominates: snare classification more accurate for most founders. This determines whether the constraint''s suppression is a necessary cost of capital access or an extractive feature of the model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_magnitude, empirical, 'Extent of information asymmetry vs deliberate venture-founder misalignment').

omega_variable(
    ecosystem_substitution_threshold,
    'At what maturity level do alternative startup pathways (government programs, accelerators, corporate innovation, cooperative funding) genuinely substitute for venture capital, triggering the scaffold''s sunset logic?',
    'Tracking: proportion of founders raising venture capital vs pursuing alternative pathways over time, success rates by pathway, founder satisfaction and outcome distribution by capital source. Network effects: do founders in strong ecosystem regions experience lower failure and better terms?',
    'If threshold low (< 20% market share of alternatives): sunset is structural (scaffold framework holds). If threshold high (> 60%): alternatives remain marginal and the venture constraint maintains monopoly control (remains Snare/Tangled Rope even as ecosystem grows). This determines whether the scaffold perspective is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_substitution_threshold, empirical, 'Maturity threshold for alternative startup pathways to substitute for venture capital').

omega_variable(
    survivor_bias_in_narrative,
    'Does the celebration of successful founder narratives (survivor bias) constitute part of the constraint''s suppression mechanism, preventing honest assessment of failure patterns?',
    'Content analysis: proportion of founder media coverage dedicated to success vs failure stories, tonal framing of failure narratives, visibility of failed founder support vs success celebration. Cognitive impact: measure founder perception of failure likelihood before and after exposure to survivor-biased media. Correlation between narrative consumption and founder commitment to ''fake it till you make it'' mythology.',
    'If narrative bias is suppression component: the constraint''s effective suppression is higher than structural measurement suggests, because it prevents accurate risk assessment at the point of entry. Founders enter with false success-likelihood beliefs, increasing their willingness to accept unfavorable terms. This would raise the theater_ratio and lower the measured resistance to exit (founders believe they can succeed, so exit barriers appear lower than they are).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivor_bias_in_narrative, empirical, 'Role of survivor-bias narratives in the constraint''s suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(startup_survival_rates, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(startup_tr_t0, startup_survival_rates, theater_ratio, 0, 0.55).
narrative_ontology:measurement(startup_tr_t5, startup_survival_rates, theater_ratio, 5, 0.62).
narrative_ontology:measurement(startup_tr_t10, startup_survival_rates, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(startup_be_t0, startup_survival_rates, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(startup_be_t5, startup_survival_rates, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(startup_be_t10, startup_survival_rates, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(startup_survival_rates, resource_allocation).
narrative_ontology:affects_constraint(startup_survival_rates, founder_career_path_dependency).
narrative_ontology:affects_constraint(startup_survival_rates, venture_capital_geographic_concentration).
narrative_ontology:affects_constraint(startup_survival_rates, corporate_innovation_alternative_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(startup_survival_rates, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
