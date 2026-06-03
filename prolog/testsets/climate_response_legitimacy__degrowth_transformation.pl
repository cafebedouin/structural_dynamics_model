% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation as Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth-transformation reading of climate response legitimacy claims
 *   that adequate warming stabilization requires wealthy nations to dismantle
 *   the growth imperative through structural economic transformation:
 *   universal basic services (decommodifying survival), working time
 *   reduction (distributing available work and ecological carrying capacity),
 *   and democratic firm ownership (shifting capital claims). This reading
 *   accepts that current generations in wealthy nations will bear significant
 *   costs (income reduction, structural economic dislocation, redistribution
 *   to global south) as the price of legitimate climate action that does not
 *   burden future generations with catastrophic adaptation requirements or
 *   technological dependency on unproven solutions. The constraint exhibits
 *   the full tangled-rope structure: genuine coordination function
 *   (intergenerational justice, climate stabilization, coherent
 *   transformation pathway) and asymmetric extraction (incumbent capital and
 *   high-income workers bear concentrated costs; future generations and
 *   climate-vulnerable populations receive benefit without reciprocal
 *   burden). The measurement trajectory shows rising suppression over time
 *   (enforcement machinery must intensify to overcome incumbent capital
 *   opposition and worker resistance) and rising extractiveness (as the
 *   transformation deepens, the burden on current-generation cost-bearers
 *   increases). Theater ratio declines over time, indicating that
 *   performative pledges (Net Zero 2050 commitments, carbon pricing without
 *   redistribution) are replaced by functional transformation (actual income
 *   redistribution, working-time enforcement, firm ownership change)—the
 *   constraint becomes more real, less theatrical. This is a kernel reading:
 *   'legitimate climate response' is contested across three incommensurable
 *   readings (degrowth_transformation, mitigation_priority,
 *   adaptation_priority), each instantiating different constraints with
 *   different beneficiary/victim structures. The degrowth reading forecloses
 *   the mitigation_priority reading's core claim (growth can be preserved
 *   through technological decoupling) while coexisting with the
 *   adaptation_priority reading (both can be held simultaneously, but with
 *   different emphasis and resource allocation).
 *
 * KEY AGENTS:
 *   - Future Generations: Primary beneficiary (institutional/analytical) — receive reduced warming trajectory, lower adaptation burden, structural economic stability without technological-fix dependency
 *   - Global South / Climate-Vulnerable Populations: Secondary beneficiary (institutional/mobile) — receive mitigation benefit (reduced future warming) without bearing transformation costs; benefit from North-South burden inversion
 *   - Wealthy-Nation Workers (Non-Elite): Primary victim (powerless/trapped) — face income reduction, skill displacement, working-time uncertainty during transition; cannot exit individual level
 *   - Incumbent Capital Holders (Fossil Fuel, Finance, Growth-Dependent Sectors): Secondary victim (powerful/arbitrage-attempted) — face asset devaluation, capital-flight constraints, forced participation in redistribution; arbitrage exit is explicitly foreclosed by constraint design
 *   - Organized Labor / Labor Coalition: Mixed agent (organized/constrained) — benefits from coordination function (collective security, working-time reduction, firm democracy) while bearing income-reduction costs; can negotiate but not block transformation
 *   - Wealthy-Nation Democratic State: Enforcing agent (institutional/constrained) — implements transformation against internal opposition; bears political cost and electoral risk; coordinating agent for legitimacy claim
 *   - Analytical Observer / Committer-Frame Reader: Sees structure as reading-dependent; recognizes this is ONE legitimate interpretation of climate response, not THE response; notes kernel contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.72).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '3816a0b1-c721-47b7-943a-6cdc74a0211d').
narrative_ontology:cs_kernel_codification('3816a0b1-c721-47b7-943a-6cdc74a0211d', distributed).
narrative_ontology:cs_authority_grounding('3816a0b1-c721-47b7-943a-6cdc74a0211d', extraction).
narrative_ontology:cs_reading_relation('3816a0b1-c721-47b7-943a-6cdc74a0211d', climate_response_legitimacy__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('3816a0b1-c721-47b7-943a-6cdc74a0211d', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('3816a0b1-c721-47b7-943a-6cdc74a0211d', foundational, growth_incompatible_with_stabilization).
narrative_ontology:cs_axiom_status(growth_incompatible_with_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('3816a0b1-c721-47b7-943a-6cdc74a0211d', growth_incompatible_with_stabilization, empirically_contingent).
narrative_ontology:cs_axiom('3816a0b1-c721-47b7-943a-6cdc74a0211d', foundational, intergenerational_burden_inversion_legitimate).
narrative_ontology:cs_axiom_status(intergenerational_burden_inversion_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3816a0b1-c721-47b7-943a-6cdc74a0211d', intergenerational_burden_inversion_legitimate, deontological).
narrative_ontology:cs_reference_frame('3816a0b1-c721-47b7-943a-6cdc74a0211d', intergenerational_commons_protection).
narrative_ontology:cs_drift_state('3816a0b1-c721-47b7-943a-6cdc74a0211d', contemporary_2020s_policy_stasis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3816a0b1-c721-47b7-943a-6cdc74a0211d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, ecological_commons).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, wealthy_nation_labor_classes).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, incumbent_capital_holders).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, incumbent_financial_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEALTHY NATION WORKER (SNARE) — Faces structural income reduction, working-hour uncertainty, and skill displacement through economic transformation. Cannot exit: relocation is costly, retraining is years-long, and the constraint applies within their domestic framework. Experiences maximum extraction with minimal alternative: transformation imposes costs without offering clear individual escape route. Power is powerless because the structural change bypasses individual choice.
constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT CAPITAL HOLDERS (SNARE) — Asset holders in fossil fuel, finance, and growth-dependent sectors face devaluation of capital stock. Exit option (arbitrage) is theoretically available — capital can flee to less-regulated jurisdictions or pivot to new sectors — but the reading's legitimacy claim directly targets this arbitrage channel by requiring democratic/structural enforcement. Experiences high extraction because the constraint's enforcement mechanism explicitly forecloses capital flight.
constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LABOR COALITION / ORGANIZED WORKERS (TANGLED ROPE) — Organized labor experiences genuine coordination function (collective wage floors, reduced working time, democratic firm ownership enable worker power and material security) alongside extraction (income reduction during transition, structural instability). Exit is constrained: unions can negotiate within the transformation but cannot block it entirely. The constraint simultaneously extracts (income loss) and coordinates (collective security), making it tangled rope rather than pure snare.
constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL SOUTH STATES AND CLIMATE-VULNERABLE POPULATIONS (ROPE) — Pure coordination function: wealthy-nation degrowth reduces future warming, reducing adaptive burden on vulnerable populations. Structural benefit with minimal extraction—these actors are not targets of the cost-bearing mechanism. Exit option (mobile) is real: they can adopt their own climate strategies. The constraint benefits them without coercing them; it is coordination from their perspective.
constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FUTURE GENERATIONS (ROPE) — Receive cumulative benefit (reduced warming trajectory, lower adaptation costs across centuries) with no cost-bearing in the short term. Their 'exit option' is analytical—they cannot choose whether to inherit a transformed economy, but the transformation's outcomes are structurally beneficial. Rope classification because the constraint is purely coordinative from their perspective: it solves the intergenerational coordination problem of climate stabilization.
constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: WEALTHY NATION DEMOCRATIC STATE (TANGLED ROPE) — Faces genuine coordination function (legitimate climate response, intergenerational justice, coherent transformation path) alongside extraction (political cost, electoral risk, capital flight threat). The state must enforce the constraint against internal opposition—incumbent capital, high-income workers—creating enforcement burden. Exit is constrained: states cannot unilaterally opt out of climate physics, but they face pressure from capital and upper classes. The state extracts via redistribution but also coordinates via transformation legitimacy.
constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — READING-AWARE (TANGLED ROPE) — This perspective instantiates the committer frame: recognizes that 'legitimate climate response' is contested across three readings (degrowth_transformation, mitigation_priority, adaptation_priority), each producing different beneficiary/victim sets and constraint types. The degrowth reading coordinates climate stabilization and intergenerational justice while extracting from wealthy-nation incumbents and workers—hence tangled rope. Alternative readings would classify differently. This perspective sees the constraint as contingent on accepting specific axioms about growth necessity, intergenerational duty, and global burden-sharing.
constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_legitimacy__degrowth_transformation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The transformation imposes concentrated costs on incumbent capital (asset devaluation, capital-flow constraints) and on wealthy-nation labor (income reduction, skill displacement). The extraction is not maximal (0.90+) because genuine coordination benefits exist—reduced working time improves labor conditions, basic services provision addresses material security, firm democracy enables worker power. The beneficiary set (future generations, global south) is also abstract/long-term, reducing the immediate felt extraction. The extractiveness increases over time (0.35 → 0.72) as transformation deepens and incumbent opposition hardens, requiring stronger enforcement. Suppression (0.72): High and rising. The constraint requires active enforcement against three vectors of opposition: incumbent capital flight (must be blocked), worker wage-compression (must be enforced), and wealthy-nation consumption expectations (must be culturally reset). Suppression is not totalitarian (0.95+) because the coordination function is genuine—workers accept income reduction if it comes with security and democracy, not merely coercion. Rising over time reflects intensifying enforcement machinery as opposition hardens. Theater ratio (0.45): Below 0.50, indicating the transformation is functional rather than performative. This distinguishes degrowth reading from mitigation_priority reading (which achieves high theater through pledges without redistribution). Theater declines over time as rhetorical commitments are replaced by actual institutional change. The low theater ratio indicates this is not a piton (degraded, mostly performative)—it is tangled rope with real functional content, even though it requires high suppression.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. Incumbent capital sees pure snare (extraction, coercion, no coordination benefit for themselves). Wealthy-nation workers see snare or tangled rope depending on union strength (trapped workers see snare; organized workers see tangled rope with some power). Labor coalitions see tangled rope (both extraction and coordination). Global south and future generations see rope (pure coordination, no extraction). The democratic state sees tangled rope (enforcing both coordination and extraction). The analytical observer at the committer level sees the entire structure as reading-dependent: the constraint is legitimate ONLY if you accept the axiom that growth must be dismantled. Observers who believe technological decoupling is sufficient (mitigation_priority axiom) or that adaptation is the primary concern (adaptation_priority axiom) will reclassify the constraint as illegitimate coercion. The perspectival gap is not empirical disagreement—it is axiom disagreement about what 'legitimate climate response' means.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d value) emerges from the agent's structural position relative to the cost/benefit flow. Beneficiaries (future generations, global south) have low d because the constraint subsidizes them—the cost-bearing is inverted toward incumbents and wealthy-nation workers. Incumbent capital has high d (0.85+) because they are explicitly targeted for extraction via asset devaluation and capital-flow restriction; their arbitrage options are foreclosed by the constraint's design. Workers have high d (0.75+) on income reduction but moderate d (0.55) when organizational power (labor coalition) is factored in—organized workers extract some power through the democratic-firm mechanism, reducing net experienced extractiveness. The state has intermediate d (0.65) because it bears political cost but gains legitimacy gains and future-generation benefit. The analytical observer has high d (0.72) because the reading requires accepting the axiom that growth must be dismantled—observers who reject this axiom experience the constraint as coercive ideological imposition rather than legitimate response. Perspective directionality tracks structural relationships, not moral judgments: a beneficiary with arbitrage option (global south) experiences low extraction regardless of whether the constraint is justified.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA AXIOM STRUCTURE: The apparent mandatrophy—'Is this coordination (Rope) or extraction (Snare)?'—dissolves when the axiom structure is made explicit. The degrowth reading answers: IT IS BOTH, and the both-ness is justified by the axiom that growth must be dismantled and the axiom that intergenerational justice requires current-generation burden-bearing. This is why it is tangled rope, not snare. The snare readings (from incumbent capital perspective, from mitigation_priority reading that rejects the dismantling axiom) are perspectival alternatives, not corrections. The mandatrophy is managed by recognizing that 'legitimate climate response' is not an empirical fact but a political constitution: each reading defines legitimacy differently, and the constraints flow from the axiom set. The resolution is NOT to pick one reading as 'correct,' but to understand why reasonable observers with different axioms classify the same structure differently. This is the core of the committer frame: the kernel (what constitutes legitimate response) is contested, each reading instantiates different constraints, and the engine's task is to classify each reading accurately, not to adjudicate the contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_implementation_gap,
    'Can democratic institutions in wealthy nations implement degrowth transformation against incumbent capital opposition without authoritarian escalation or democratic backsliding?',
    'Historical case analysis (post-WWII European transformation, Nordic welfare-state construction, Green New Deal pilot programs); institutional capacity assessment for wealth redistribution at scale; correlation between environmental constraint severity and democratic resilience',
    'If feasible: constraint classification stands (tangled rope with real transformation path). If infeasible: classification shifts toward snare (extraction without coordination, because transformation becomes coercive fantasy) or piton (performance without function, because pledges lack enforcement capacity). Implementation gap is the crux of this reading''s uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_implementation_gap, empirical, 'Whether democratic institutions can implement degrowth transformation against incumbent opposition').

omega_variable(
    growth_decoupling_technological_frontier,
    'Is complete decoupling of emissions from growth economically achievable, or does degrowth reading''s claim that growth must be dismantled represent a correct diagnosis that technological mitigation is insufficient?',
    'Long-term empirical tracking of emissions-per-GDP trends across sectors; analysis of rebound effects in efficiency improvements; modeling of renewable energy scaling constraints and material cycle limits; comparison of mitigation scenarios with/without behavioral demand reduction',
    'If decoupling is achievable: mitigation_priority reading''s axioms become viable, and degrowth reading''s core extraction claim (growth must be dismantled) becomes optional rather than necessary. Classification shifts toward rope (coordination rather than forced transformation). If decoupling is impossible at required pace: degrowth reading''s extraction claim is vindicated, and the snare/tangled-rope boundary is sharper (coercive necessity vs. legitimate coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_technological_frontier, empirical, 'Whether technological decoupling can achieve climate targets without degrowth').

omega_variable(
    reading_legitimacy_contestation_kernel,
    'Which reading of climate response legitimacy (degrowth_transformation vs mitigation_priority vs adaptation_priority) correctly identifies the operative constraint on climate action?',
    'This is the kernel-level omega: recorded for committer-frame transparency. Cannot be resolved empirically — it is the axiom conflict that defines the kernel. Resolution mechanism is political: whichever reading''s axioms are institutionalized in policy (carbon pricing + innovation, or transformation + redistribution, or adaptation + resilience) becomes the ''operative'' constraint. Other readings persist as counternarratives. See cs_structure.axioms and cs_structure.reading_relations for structural relationships between readings.',
    'Determines which agent sets classify as beneficiary/victim. Degrowth reading puts future generations + global south as beneficiaries; mitigation_priority puts innovation-sector workers and capital as beneficiaries; adaptation_priority puts vulnerable-population security as beneficiary. Victim sets are mutually exclusive across readings. The kernel determines which extraction flow is ''legitimate climate response'' and which is ''unjust burden-shifting.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy_contestation_kernel, conceptual, 'Kernel-level contestation: which reading of legitimate climate response is operative').

omega_variable(
    intergenerational_discount_rate_validity,
    'Is the intergenerational cost-bearing asymmetry (current generation pays, future generations benefit) ethically coherent, or does it mask a failure to value future welfare adequately?',
    'Philosophical analysis of intergenerational justice frameworks (utilitarian discounting vs. rights-based approaches vs. capabilities approach); empirical assessment of whether current-generation cost is actually justified by future-generation benefit magnitude; welfare economics comparison of transformation pathway vs. adaptation-only pathway across time horizons',
    'If asymmetry is justified: degrowth reading''s extraction of current-generation workers is ethically grounded in intergenerational duty. If asymmetry is unjustified (future welfare is not adequately valued): current-generation extraction becomes morally arbitrary, and the constraint reclassifies as snare (pure coercion without proportional justification). Affects whether victims classify as legitimate burden-bearers or exploited groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_validity, preference, 'Whether intergenerational cost-bearing asymmetry is ethically justified').

omega_variable(
    global_coordination_enforcement_capacity,
    'Can wealthy nations enforce degrowth transformation while preventing capital flight and maintaining policy coherence across borders, or will unilateral transformation lead to capital relocation and policy failure?',
    'Institutional analysis of capital controls, trade agreements, and international financial frameworks; case studies of regional economic transformation with open borders (EU transitions, Nordic model sustainability); modeling of defection incentives and coordination mechanisms for multi-nation transformation',
    'If enforcement is possible: constraint classification stands (tangled rope with real enforcement mechanism). If enforcement fails: classification shifts toward piton (transformation rhetoric without functional implementation) or snare (coercive against workers while capital escapes). Global coordination is the critical enforcement bottleneck.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_coordination_enforcement_capacity, empirical, 'Whether degrowth transformation can be enforced across open global economy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degrowth_theater_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.62).
narrative_ontology:measurement(degrowth_theater_t10, climate_response_legitimacy__degrowth_transformation, theater_ratio, 10, 0.45).
narrative_ontology:measurement(degrowth_theater_t20, climate_response_legitimacy__degrowth_transformation, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(degrowth_extractiveness_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(degrowth_extractiveness_t10, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(degrowth_extractiveness_t20, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(degrowth_suppression_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(degrowth_suppression_t10, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(degrowth_suppression_t20, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, intergenerational_justice_distribution).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, capital_accumulation_limits).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_legitimacy' kernel. The sibling readings (mitigation_priority, adaptation_priority) are separate constraint stories with different ε values, different beneficiary/victim declarations, and different perspectives. All three are 'constraints on legitimate climate response,' but they decompose the kernel differently. Network links indicate that accepting THIS reading (degrowth_transformation) structurally forecloses or influences the sibling readings' viability. See cs_structure.reading_relations for typed edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, powerful, 0.88).
constraint_indexing:directionality_override(climate_response_legitimacy__degrowth_transformation, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
