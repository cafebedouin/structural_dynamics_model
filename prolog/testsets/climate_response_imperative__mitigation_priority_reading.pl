% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Climate Response Imperative: Mitigation-First Reading (Technological Innovation & Market Mechanisms)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested climate
 *   response imperative kernel. The mitigation-priority reading frames
 *   climate response as primarily emissions reduction through technological
 *   innovation, market mechanisms (carbon pricing, offsets, green finance),
 *   and decarbonization pathways, with adaptation to unavoidable damages
 *   treated as a secondary, residual concern. Under this reading, the logic
 *   is: (1) emissions are the root cause, (2) reducing emissions is the
 *   direct solution, (3) technological innovation at scale can achieve deep
 *   decarbonization, (4) market mechanisms efficiently allocate
 *   emissions-reduction investments, and (5) adaptation in exposed regions
 *   will adjust to whatever damages mitigation does not prevent. This reading
 *   has captured dominant institutional space in climate finance, technology
 *   policy, and corporate climate commitments globally. The structural delta
 *   this reading produces is: future generations and currently-vulnerable
 *   regions (particularly in the Global South) enter the victim set through
 *   deferred adaptation costs; Global North innovation sectors and carbon
 *   capital managers are primary beneficiaries through access to
 *   emissions-reduction markets, carbon credits, technology-transfer fees,
 *   and green investment flows; the constraint exhibits high suppression
 *   because it marginalizes adaptation-focused discourse and locks out
 *   alternative framings; it exhibits moderate theater because market
 *   mechanisms perform efficiency while actual emissions reductions lag
 *   headline commitments, and adaptation needs are acknowledged in formal
 *   documents but starved of resources. The extractiveness trajectory shows
 *   increase over the measurement interval as the constraint's institutional
 *   capture deepens: early mitigation commitments (2015-2020) left more
 *   adaptation space, but recent emphasis on net-zero targets and CDR scaling
 *   crowds out adaptation budgets.
 *
 * KEY AGENTS:
 *   - Global North Innovation Sectors: Primary beneficiary (institutional/arbitrage) — benefit from green bonds, carbon credits, technology-transfer rents, patent protections on decarbonization technologies. High exit capacity—capital can flow to alternative markets if mitigation-priority framing weakens.
 *   - Future Generations: Primary victim (powerless/trapped) — cannot exit temporal structure; will inherit compounded climate damages while mitigation windows were available. No voice in current decision-making.
 *   - Vulnerable Regions (Small Islands, Sub-Saharan Africa, South Asia): Primary victim (powerless/trapped, some constrained) — geographically locked into exposure zones; capital constraints limit independent adaptation. Deferral of adaptation costs compounds their structural vulnerability.
 *   - Current-Generation Developing-Nation Governments: Secondary victim (moderate/constrained) — theoretically can participate in climate finance and carbon markets but at disadvantageous terms; carry adaptation burdens for their constituents.
 *   - Climate-Justice & Adaptation-First Advocacy Networks: Organized victims (organized/constrained) — push for loss-and-damage recognition and adaptation prioritization but marginalized by mitigation-priority institutional framing.
 *   - UNFCCC Adaptation Machinery: Performative institution (institutional/arbitrage) — Green Climate Fund adaptation window, NAPA processes persist at policy level but capture <10% of climate finance; theater-heavy, functionally subordinated.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the mitigation-first institutional choice as a law of physics rather than a contested policy reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.58).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Climate Response Imperative: Mitigation-First Reading (Technological Innovation & Market Mechanisms)").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '82c8205b-8e49-4ba5-aad5-1e916dc47156').
narrative_ontology:cs_kernel_codification('82c8205b-8e49-4ba5-aad5-1e916dc47156', distributed).
narrative_ontology:cs_authority_grounding('82c8205b-8e49-4ba5-aad5-1e916dc47156', extraction).
narrative_ontology:cs_reading_relation('82c8205b-8e49-4ba5-aad5-1e916dc47156', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('82c8205b-8e49-4ba5-aad5-1e916dc47156', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('82c8205b-8e49-4ba5-aad5-1e916dc47156', foundational, emissions_reduction_technological_sufficiency).
narrative_ontology:cs_axiom_status(emissions_reduction_technological_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('82c8205b-8e49-4ba5-aad5-1e916dc47156', emissions_reduction_technological_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('82c8205b-8e49-4ba5-aad5-1e916dc47156', foundational, market_mechanism_allocative_efficiency).
narrative_ontology:cs_axiom_status(market_mechanism_allocative_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('82c8205b-8e49-4ba5-aad5-1e916dc47156', market_mechanism_allocative_efficiency, instrumental).
narrative_ontology:cs_created_at('82c8205b-8e49-4ba5-aad5-1e916dc47156', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_capital_managers).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, adaptation_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS & CLIMATE-EXPOSED (SNARE) — Locked into receiving adaptation deficits as mitigation is prioritized. Geographically trapped in exposed regions (small island states, sub-Saharan Africa, South Asia) with no exit. Cannot exit the temporal trap — climate damages compound while mitigation timelines extend. Structurally unable to participate in market mechanisms or innovation economics that define the constraint's beneficiary set. Maximum experienced extraction: bear full cost of deferred adaptation while others capture innovation rents.
constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING-NATION GOVERNMENTS (TANGLED ROPE) — Constrained by debt, technology access, and capital flows that route innovation finance to Global North actors. Benefit structurally from decarbonization pathways but carry asymmetric adaptation burdens. Can participate in carbon markets and technology transfer agreements but at disadvantageous terms. High suppression (capital constraints, IP barriers) limits agency. Experience both coordination function (market mechanisms do coordinate global emissions reductions) and extraction (adaptation costs deferred to their constituents and future periods).
constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GLOBAL NORTH INNOVATION & CARBON CAPITAL (ROPE) — Primary beneficiaries. Experience the constraint as enabling coordination: mitigation-first framing legitimizes carbon markets, clean-tech investment, carbon removal markets, and technology-transfer financialization. Can arbitrage between emissions reduction credits, CDR options, and green bonds. High exit capacity (can redirect capital to alternative markets). Net extraction flows toward this agent—experienced as pure coordination because the extraction is not suppressive for them. The market mechanism is their escape valve.
constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE JUSTICE & ADAPTATION ADVOCACY (TANGLED ROPE) — Organized but constrained by institutional power asymmetries. Benefit from increased visibility and funding as climate action accelerates, but constrained by the mitigation-priority framing that marginalizes their adaptation arguments. Experience the constraint as hybrid: genuine coordination function (all agree emissions must fall) paired with suppression (their loss-and-damage frame loses legitimacy in favor of efficiency-metrics and CDR scaling). Suppression operates through narrative framing rather than explicit prohibition. High theater: many declarations of adaptation commitment paired with mitigation funding dominance.
constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UNFCCC ADAPTATION FUND (PITON) — The formal adaptation machinery (Green Climate Fund adaptation window, National Adaptation Programmes of Action, loss-and-damage mechanisms) persists through institutional inertia while mitigation capture dominates resource flows. Declared as co-equal to mitigation at policy level but operates at <10% the budget and institutional weight. Theater-ratio high: extensive documentation and commitment statements paired with minimal resource commitment and agency. The adaptation apparatus is performed, not functional—maintained because it is required by treaty language, not because it drives resource allocation or priority-setting.
constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From physics perspective, emissions must fall to stabilize climate: this is energetically necessary, not a policy choice. Physics-based decarbonization imperative appears as a constraint independent of institutional choices. This perspective risks naturalizing the mitigation-first *reading* as a natural law of climate response. The engine will detect this as false-summit candidate: the natural law is 'emissions must fall,' not 'emissions fall via market mechanisms and innovation while adaptation is deferred.' The institutional framing naturalizes one solution path as inherent to the physical problem.
constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_imperative__mitigation_priority_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, TR),
    TR >= 0.70.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint exhibits substantial asymmetric extraction: beneficiary set (Global North innovation capital) captures resource flows and rents, while victim set (future generations, vulnerable regions) defers costs. The extraction is not maximal (snare-level) because genuine coordination function exists—market mechanisms do coordinate global emissions reductions, and decarbonization is physically necessary. But the coordination is paired with substantial asymmetry: adaptation is subordinated, and the temporal deferral is significant (30-50 year horizons where adaptation delays compound). Suppression (0.62): Moderately high. Institutional suppression operates through: (1) funding allocation that crowds out adaptation (mitigation receives ~90% of climate finance), (2) narrative framing that treats adaptation as passive adjustment rather than proactive investment, (3) IP barriers and capital concentration that limit Global South actors' ability to develop independent solutions, (4) temporal discounting mechanisms in carbon accounting that favor present-day emissions reductions over future adaptation costs, and (5) epistemic suppression of loss-and-damage frameworks that would challenge the mitigation-priority hierarchy. Theater ratio (0.51): Moderate, rising. Trend reflects increasing gap between headline commitments (net-zero pledges, Paris Accord rhetoric) and actual emissions reductions (measured emissions still rising despite 2015+ commitments). Carbon market theater particularly evident: claimed carbon removals and offset integrity have measurability problems; adaptation commitments are documented but underfunded. Theater rises as institutional performance intensifies (more declarations, more pledges) while material outcomes lag.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a classic extraction-disguised-as-coordination perspectival gap. The beneficiary (Global North innovation) sees pure coordination—market mechanisms are solving the global problem efficiently. The adaptation-focused organized actors see tangled rope—they participate in the system but are subordinated and suppressed. The UNFCCC adaptation machinery sees itself as piton—it performs through ritual documents while actual function migrates to mitigation. Vulnerable regions see snare—they are structurally locked in and bear costs while others capture benefits. Future generations see extraction as pure (snare) because they have no exit or voice. The analytical observer risks seeing mountain (physical necessity of decarbonization) while missing the institutional choices (market mechanism vs. state planning vs. degrowth redistribution) that define the constraint. The perspectival gap reveals that 'climate response is primarily mitigation' is not a natural law but a contested institutional reading—the adaptation-priority and degrowth readings would produce different beneficiary/victim structures and different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from the agent's structural position: beneficiaries with arbitrage options (institutional/arbitrage) experience low d (approximately 0.15-0.20, producing negative χ via f(d)); organized victims with constrained exit experience higher d (0.55-0.70, producing moderate χ); powerless victims trapped in geographic or temporal exposure experience maximum d (0.90+, producing maximum χ). The beneficiaries' low d coupled with the institutional power atom produces the rope-type classification from their perspective—they experience the constraint as coordination because extraction runs toward them and they have exit options. The victims' high d produces snare or tangled-rope depending on the degree of suppression and participation in benefit flows. The piton perspective (adaptation machinery) has arbitrage options (institutional power) but theater dominates function (high theater_ratio), producing the degradation classification. The mountain perspective at the analytical context risks naturalizing the institutional reading as a law of physics—the engine's false-summit detector should flag this as a candidate where 'natural necessity of decarbonization' is used to naturalize 'market-mechanism mitigation-priority' choices.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that extractiveness (0.58) genuinely pairs coordination with asymmetry. The coordination function is real: market mechanisms do coordinate emissions reductions across actors, and decarbonization is necessary. The extraction is also real: benefits flow toward Global North innovation capital, while costs are deferred to future generations and vulnerable regions. The classification as tangled_rope is justified by the simultaneous presence of both functions. The perspectival gap does not indicate misclassification but rather that the same constraint generates different experienced types depending on structural position: beneficiaries experience rope (pure coordination), victims experience snare (pure extraction), organized actors experience tangled_rope (mixed). The analytical observer's mountain perspective reveals the false-summit risk: naturalizing 'emissions must fall' into 'technological-innovation mitigation-priority is the only/natural response' obscures that adaptation-priority and degrowth readings are equally viable climate response framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cdrt_technological_feasibility,
    'Can currently-experimental carbon dioxide removal technologies (direct air capture, biochar sequestration, ocean alkalinity enhancement) scale to gigatons-per-year by mid-century without catastrophic side effects or unrecoverable failure?',
    'Pilot-to-deployment cost curves, environmental impact monitoring, thermodynamic feasibility reassessment, and field results from megaton-scale CDR deployment by 2050',
    'If feasible: mitigation-first reading holds—adaptation deferral is rational risk-taking. If infeasible: the constraint reclassifies toward snare for future generations (extraction becomes unambiguous). If partially feasible: constraint remains tangled_rope but with substantially higher omega uncertainty around terminal climate damages.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cdrt_technological_feasibility, empirical, 'Whether CDR technologies scale to required gigatons with acceptable side effects').

omega_variable(
    adaptation_cost_acceleration,
    'Do climate damages compound nonlinearly such that $1 spent on adaptation now prevents >$3 in damages 30 years hence, making present-day adaptation deferral economically irrational even from a pure cost-benefit frame?',
    'Longitudinal cost data from adaptation projects (coastal defense, water infrastructure, heat-resilient agriculture); empirical tipping points in climate system; compound damage modeling against adaptive capacity depletion',
    'If true: mitigation-first logic fails even on its own economic terms—deferred adaptation becomes tragic extraction from future generations. Constraint reclassifies toward snare. If false: cost-deferral remains defensible within mitigation-first framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_cost_acceleration, empirical, 'Whether nonlinear adaptation cost acceleration makes present-day deferral economically irrational').

omega_variable(
    market_mechanism_carbon_accounting_integrity,
    'Do carbon markets (offsets, credits, CDR accounting, REDD+ schemes) actually deliver equivalent emissions reductions to headline claims, or do baseline-shifting, leakage, and additionality gaming produce net atmospheric harm?',
    'Satellite-verified forest cover monitoring, emissions measurement audits, comparative analysis of claimed vs. observed reductions, and cross-system leakage tracking',
    'If integrity is high: market mechanisms are a genuine coordination solution—mitigation-first logic holds. If integrity is low: market mechanism is partly performative, and the constraint''s extractiveness rises (beneficiaries capture rents while actual reductions lag). Reclassification toward higher suppression and theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_mechanism_carbon_accounting_integrity, empirical, 'Whether carbon markets deliver claimed atmospheric reductions or suffer accounting failures').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the climate response imperative kernel—mitigation-priority, adaptation-priority, and degrowth—logically coexistent as simultaneous policy frameworks held by different parties, or does adoption of one reading preclude the coherent adoption of another within a single institutional authority?',
    'Institutional analysis of policy frameworks that attempt to hold multiple readings; identification of decision-point contradictions where commitment to one reading forces contradiction with another; case study of negotiation dynamics at UNFCCC and national climate governance',
    'If coexistent: readings are legitimately plural, and the constraint family is a map of genuine political contest. If incommensurable: at least one pair of readings forecloses the other, reducing the kernel to a binary or unitary structure. Guides whether reading_relations should include foreclosure edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three climate response readings are logically coexistent or mutually foreclosing').

omega_variable(
    innovation_path_lock_in,
    'Does mitigation-first institutional prioritization (funding, regulation, innovation incentives favoring decarbonization over resilience) structurally lock out non-technological adaptation and resilience innovation that might be more effective in high-vulnerability regions?',
    'Comparative innovation funding allocation across mitigation vs. adaptation sectors; institutional barriers to adaptation-focused entrepreneurship; patent and IP concentration analysis; case studies of blocked or underfunded adaptation innovation pathways',
    'If yes: mitigation-first reading becomes extractive through opportunity cost—high-value adaptation solutions are starved of development capital. Suppression mechanism becomes institutional lock-in rather than explicit prohibition. Constraint suppression rises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_path_lock_in, empirical, 'Whether mitigation-first framing structurally suppresses non-technological adaptation innovation').

omega_variable(
    global_north_innovation_monopoly,
    'Can adaptation innovation pathways remain accessible to Global South actors under mitigation-first framing, or does the technology-centricity of mitigation solutions create IP and capital monopolies that foreclose low-cost, locally-generated adaptation solutions?',
    'Analysis of technology transfer mechanisms, patent protection regimes, capacity-building efficacy, and case studies of adaptation innovation adoption in vulnerable regions vs. Global North markets',
    'If monopoly exists: beneficiary set narrows to Global North innovation capital—constraint becomes more snare-like for victims. If accessible: constraint remains tangled_rope with genuine coordination benefits alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_north_innovation_monopoly, empirical, 'Whether Global North innovation monopolies foreclose local adaptation pathways').

omega_variable(
    this_reading_vs_siblings_foreclosure_status,
    'Does the mitigation-priority reading logically foreclose the adaptation-priority and degrowth readings, or do they coexist as incommensurable but simultaneously-holdable frameworks?',
    'Formal logical analysis of each reading''s core premises and their contradictions; institutional analysis of parties attempting to hold multiple readings simultaneously; empirical study of whether policy commitment to one reading precludes commitment to another',
    'If foreclosure exists (e.g., mitigation-priority precludes adaptation-priority): reading_relations should use ''forecloses.'' If coexistent: reading_relations should use ''coexists_with.'' If structural pressure without foreclosure: ''influences.'' This determination guides the cs_structure.reading_relations declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_siblings_foreclosure_status, conceptual, 'Whether this reading forecloses, influences, or coexists with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_mit_theater_t0, climate_response_imperative__mitigation_priority_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(clim_mit_theater_t5, climate_response_imperative__mitigation_priority_reading, theater_ratio, 5, 0.47).
narrative_ontology:measurement(clim_mit_theater_t10, climate_response_imperative__mitigation_priority_reading, theater_ratio, 10, 0.51).

% Extraction over time
narrative_ontology:measurement(clim_mit_extract_t0, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_mit_extract_t5, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(clim_mit_extract_t10, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_mit_suppress_t0, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(clim_mit_suppress_t5, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(clim_mit_suppress_t10, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, carbon_market_integrity).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, technology_transfer_gatekeeping).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, intergenerational_justice_temporal_discounting).

% DUAL FORMULATION NOTE:
% The climate response imperative kernel decomposes into three structurally distinct constraint stories, each with its own ε, beneficiary/victim structure, and institutional architecture. This story (mitigation-priority) has ε=0.58 (tangled_rope). The adaptation-priority sibling has a distinct ε reflecting adaptation focus's different extraction mechanics. The degrowth sibling has a distinct ε reflecting systemic transformation requirements. All three stories are valid readings of the same kernel; the network edges document interdependencies rather than hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
