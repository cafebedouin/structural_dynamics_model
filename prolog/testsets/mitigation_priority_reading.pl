% ============================================================================
% CONSTRAINT STORY: mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mitigation_priority_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mitigation_priority_reading
 *   human_readable: Mitigation-Priority Climate Response (Technological Innovation & Market Mechanisms)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The mitigation-priority reading frames climate response as primarily an
 *   emissions reduction challenge to be solved through technological
 *   innovation, market mechanisms (carbon pricing, green bonds, technology
 *   transfer), and institutional coordination. This reading is one of three
 *   contested framings of the climate response imperative kernel. It has
 *   dominated policy discourse since the UNFCCC Framework Convention,
 *   codifying emissions reduction as the primary climate action while
 *   treating adaptation as a residual response to un-mitigated warming. The
 *   constraint instantiates a structural extraction mechanism: wealthy
 *   nations and innovation sectors benefit from carbon finance, green
 *   technology markets, and first-mover advantage in renewable energy;
 *   vulnerable regions and future generations bear the cost of deferred
 *   adaptation investments and the risk that global emissions targets will
 *   not be met. The reading exhibits tangled rope structure: it provides
 *   genuine coordination benefit (global emissions reduction lowers climate
 *   risk for all parties) while extracting (adaptation costs are pushed to
 *   vulnerable actors and future time periods; technology access remains
 *   conditional on Global North terms). The theater ratio (0.64) reflects
 *   that significant policy activity—UNFCCC negotiations, Nationally
 *   Determined Contributions, carbon credit accounting—consists of
 *   performative commitment and accounting manipulation rather than actual
 *   emissions reductions or adaptation funding.
 *
 * KEY AGENTS:
 *   - Global North Innovation Sectors: Primary beneficiary (institutional/arbitrage) — capture green technology markets, carbon finance flows, and subsidized research funding
 *   - Vulnerable Regions: Primary victim (powerless/trapped) — bear adaptation costs deferred by mitigation priority; lack capital for autonomous climate response
 *   - Future Generations: Primary victim (powerless/trapped) — inherit both cumulative emissions and deferred adaptation deficit
 *   - Carbon Finance Intermediaries: Beneficiary with mixed extraction (institutional/constrained) — coordinate capital flows while extracting fees and conditionality leverage
 *   - Mid-Income Climate-Vulnerable Nations: Mixed victim-beneficiary (moderate/constrained) — some emissions reduction benefit but also constrained by adaptation finance scarcity and technology dependency
 *   - Climate Justice Movements: Organized victim (organized/constrained) — organize resistance to adaptation deferral but operate under power asymmetries; voices marginalized in mitigation-priority framework
 *   - UNFCCC Adaptation Committee: Institutional degradation (institutional/constrained) — formally mandated adaptation oversight lacks enforcement authority and funding; piton structure
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing mitigation priority as physical law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mitigation_priority_reading, 0.58).
domain_priors:suppression_score(mitigation_priority_reading, 0.68).
domain_priors:theater_ratio(mitigation_priority_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mitigation_priority_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(mitigation_priority_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mitigation_priority_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(mitigation_priority_reading, "Mitigation-Priority Climate Response (Technological Innovation & Market Mechanisms)").
narrative_ontology:topic_domain(mitigation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(mitigation_priority_reading, formalized).
narrative_ontology:cs_authority_grounding(mitigation_priority_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(mitigation_priority_reading).
narrative_ontology:cs_kernel_id(mitigation_priority_reading, climate_response_imperative).
narrative_ontology:cs_reading_relation(mitigation_priority_reading, adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation(mitigation_priority_reading, degrowth_reading, coexists_with).
narrative_ontology:cs_axiom(mitigation_priority_reading, foundational, emissions_reduction_primary_imperative).
narrative_ontology:cs_axiom_status(emissions_reduction_primary_imperative, holdable).
narrative_ontology:cs_axiom_grounding(mitigation_priority_reading, emissions_reduction_primary_imperative, empirically_contingent).
narrative_ontology:cs_axiom(mitigation_priority_reading, foundational, technology_transfer_sufficient_for_global_participation).
narrative_ontology:cs_axiom_status(technology_transfer_sufficient_for_global_participation, holdable).
narrative_ontology:cs_axiom_grounding(mitigation_priority_reading, technology_transfer_sufficient_for_global_participation, empirically_contingent).
narrative_ontology:cs_reference_frame(mitigation_priority_reading, scientific_emissions_reduction_imperative).
narrative_ontology:cs_drift_state(mitigation_priority_reading, contemporary_net_zero_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(mitigation_priority_reading, carbon_finance_intermediaries).
narrative_ontology:constraint_beneficiary(mitigation_priority_reading, wealthy_nations).
narrative_ontology:constraint_victim(mitigation_priority_reading, vulnerable_regions).
narrative_ontology:constraint_victim(mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(mitigation_priority_reading, climate_adaptation_dependent_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE REGIONS (SNARE) — Trapped by geographic climate exposure and lack of capital for autonomous adaptation. The mitigation-priority reading defers adaptation investments while betting everything on unproven global emissions reductions that may not materialize in time. No exit: these regions cannot migrate their populations or escape climate impacts. Extraction is maximum — full cost of climatic change borne while benefits (innovation capture, carbon finance fees) flow to Global North.
constraint_indexing:constraint_classification(mitigation_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Structurally trapped: have no voice in current policy decisions; will inherit both the cumulative emissions trajectory AND the deferred adaptation deficit. The mitigation-priority reading commits current actors to a technological wager that may fail, leaving future actors with compounded climate damages and outdated mitigation infrastructure. Maximum extraction from powerless agents with no exit option.
constraint_indexing:constraint_classification(mitigation_priority_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-INCOME CLIMATE-VULNERABLE NATIONS (TANGLED ROPE) — Constrained by debt dependency and technology access barriers. The mitigation framework provides some coordination benefit (global emissions reductions reduce their climate risk), but also extracts: they must meet emissions targets while financing adaptation themselves, and compete for scarce green finance against wealthier actors. Mixed experience of coordination and asymmetric extraction.
constraint_indexing:constraint_classification(mitigation_priority_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: GLOBAL NORTH INNOVATION SECTORS (ROPE) — Primary beneficiary. The mitigation-priority reading coordinates investment into renewable energy, battery technology, direct air capture, and emissions trading infrastructure. Benefits flow directly: subsidies, carbon credit markets, first-mover technology monopolies, high-skill job creation. Experiences the constraint as coordination: the mitigation framework solves the collective action problem of underinvestment in green innovation by mobilizing capital. Net beneficiary with exit option (can arbitrage green tech globally).
constraint_indexing:constraint_classification(mitigation_priority_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CARBON FINANCE INTERMEDIARIES (TANGLED ROPE) — World Bank, bilateral development banks, carbon trading platforms. The mitigation framework is their primary operating domain: they coordinate capital flows between high-emitting nations and mitigation projects, while extracting fees, management overhead, and conditionality leverage. They experience genuine coordination function (channeling climate finance) alongside extraction (fees, project selection bias toward Global North contractors, conditionality requirements that serve lender interests).
constraint_indexing:constraint_classification(mitigation_priority_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UNFCCC ADAPTATION COMMITTEE (PITON) — Formally mandated to oversee adaptation but structurally subordinate in resource allocation and political priority to mitigation-focused negotiation tracks. The adaptation committee persists through institutional inertia and normative commitment (loss & damage recognition, adaptive capacity building) but lacks enforcement authority or substantial funding mechanisms. Theater ratio high: extensive reports and technical assessments that rarely translate to implementation. The institutional role is degraded — maintained because the treaty requires it, not because it functions as a primary climate response mechanism.
constraint_indexing:constraint_classification(mitigation_priority_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE JUSTICE MOVEMENTS (TANGLED ROPE) — Organized but structurally constrained by power asymmetries in climate governance. The mitigation-priority reading coordinates a global response (which serves the movement's core interest in reducing climate damages), but also extracts: the framework marginalizes demands for reparations, local adaptation autonomy, and just transition labor protections. Movements have some agency (can organize, protest, demand renegotiation) but operate under structural constraints (resource disparities, geopolitical leverage asymmetries). Mixed experience: genuine coordination function + significant extraction of voice and resources.
constraint_indexing:constraint_classification(mitigation_priority_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: NET-ZERO TRANSITION WITH SUNSET LOGIC (SCAFFOLD) — Organized climate-conscious investors and policy coalitions see the mitigation-priority framework as a temporary coordination mechanism designed to sunset: carbon pricing is intended to rise high enough to make fossil fuels uncompetitive, stranded asset writedowns are expected, and the framework's enforcement (carbon tariffs, subsidy withdrawal) is designed to phase as alternatives mature. The suppression is high (requires behavioral change, incumbent resistance), but the framework has an explicit exit logic. Structured as temporary support with declining coercive overhead — classic scaffold.
constraint_indexing:constraint_classification(mitigation_priority_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL / PHYSICS VIEW (MOUNTAIN) — From a civilizational/universal perspective, any climate response must ultimately reduce atmospheric CO2 concentration or increase planetary reflectivity. The mitigation-priority reading reflects a physical reality: once emissions are in the atmosphere, they must be removed or their effects mitigated through planetary-scale processes. This perspective risks treating the mitigation-priority reading as an immutable natural law ('emissions reduction is the only viable climate response'), when in fact it is one institutional reading of how to allocate resources between mitigation, adaptation, and prevention.
constraint_indexing:constraint_classification(mitigation_priority_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mitigation_priority_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mitigation_priority_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mitigation_priority_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mitigation_priority_reading, TR),
    TR >= 0.70.

:- end_tests(mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mitigation-priority reading creates three extraction mechanisms: (1) temporal deferral—adaptation costs are pushed to future time periods when climate damages compound; (2) geographic mismatch—emissions are reduced in wealthy nations while adaptation burdens fall on vulnerable regions that did not create the emissions; (3) technological dependency—access to green technology remains conditional on Global North licensing and financing terms. However, extraction is not maximal (snare-level 0.66+) because some genuine mitigation coordination exists and some mid-income nations can negotiate technology transfer. The value increased from 0.38 to 0.58 over the interval as: (a) green technology costs fell, concentrating benefits with Global North first-movers; (b) adaptation deficits accumulated in vulnerable regions, making deferred adaptation more expensive; (c) carbon finance institutions matured, stabilizing extraction fees. Suppression (0.68): Moderate-high. Multiple barriers constrain vulnerable regions' exit from mitigation-priority allocation: geopolitical power asymmetries in climate negotiation, fiscal capacity constraints that make independent adaptation financing impossible, technology access restrictions via patent regimes, and institutional lock-in via debt conditionality. However, suppression is not total because some adaptation alternatives exist (local ecosystem restoration, traditional water management) and some vulnerable regions have successfully negotiated climate finance. Theater ratio (0.64): Moderate-high. Significant mitigation policy activity is performative: Nationally Determined Contributions include speculative CDR assumptions and accounting gimmicks; carbon credit systems enable offset shell games rather than real reductions; green finance reporting inflates adaptation allocations through creative accounting. Theater has risen over the interval as: (a) actual emissions reductions have lagged policy pledges, requiring more accounting manipulation; (b) CDR technology promises have become more central to net-zero narratives, introducing speculative elements into current policy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival disagreement. Global North innovation sectors see the mitigation-priority framework as solving a collective action problem (rope), experiencing genuine coordination benefit. Vulnerable regions see pure extraction (snare)—deferred adaptation that raises future damages. Mid-income nations see mixed coordination and extraction (tangled_rope). Climate justice movements see extraction masked as development (tangled_rope). The UNFCCC Adaptation Committee sees its own degradation (piton)—formally mandated but operationally subordinate. The civilization-scale analytical observer risks seeing mitigation priority as a law of physics (mountain) rather than an institutional choice. This perspectival divergence reveals that the mitigation-priority reading is contingent, not inevitable, and that alternative readings (adaptation-priority, degrowth) would reorganize beneficiaries and victims significantly.
 *
 * DIRECTIONALITY LOGIC:
 *   The mitigation-priority reading's directionality structure (d values) is determined by beneficiary/victim declarations and exit options. Global North innovation sectors are declared beneficiaries with arbitrage exit options (d ≈ 0.08–0.15): they can trade green technology globally and capture monopoly rents. This produces low effective extraction (chi) from their perspective—they experience rope (coordination). Vulnerable regions are declared victims with trapped exit (d ≈ 0.95): they cannot exit geographic climate exposure or acquire adaptive capacity independently. This produces high effective extraction (chi)—they experience snare. Mid-income nations are simultaneously beneficiaries (from global emissions reduction) and victims (from adaptation finance scarcity); their exit is constrained (d ≈ 0.60), producing moderate extraction and tangled_rope classification. Carbon finance intermediaries are beneficiaries with constrained exit (d ≈ 0.40): they benefit from the mitigation institutional apparatus but cannot arbitrage beyond it. The piton classification of the UNFCCC Adaptation Committee derives from theater_ratio ≥ 0.70 + low extractiveness (0.25–0.35 effective), indicating degraded institutional function rather than extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that the mitigation-priority reading contains a genuine coordination function (global emissions reduction benefits all parties by reducing climate damages) alongside asymmetric extraction (benefits flow to Global North, costs to vulnerable regions and future generations). The tangled_rope classification is correct: the constraint simultaneously solves a collective action problem and extracts. The mandatrophy resolution is not 'choose coordination or extraction'—it is to recognize that this institutional reading bundles coordination and extraction together, and that alternative readings would unbundle them differently. The adaptation-priority reading would move coordination function to local adaptation capacity-building; the degrowth reading would move coordination to reduced metabolic throughput. Each reading redistributes beneficiaries and victims. The false summit signature should NOT fire (no misleading naturalization of a contingent institutional arrangement as physical law), but the kernel context omega should flag that the mitigation-priority reading's legitimacy depends on empirical outcomes (CDR scalability, emissions trajectory, adaptation cost curves) that remain uncertain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_dioxide_removal_feasibility,
    'Can carbon dioxide removal (CDR) technologies (direct air capture, bioenergy with carbon capture and storage, enhanced weathering) scale to offset hard-to-decarbonize sectors and provide negative emissions this century?',
    'Engineering analysis of energy requirements and deployment costs for CDR vs. mitigation costs; pilot deployment data on efficiency and permanence; lifecycle emissions accounting including infrastructure embedded carbon',
    'If feasible at scale: mitigation-priority reading is validated—mitigation investments can defer adaptation costs. If not feasible: the reading''s bet on CDR is a false premise, leaving vulnerable regions stranded with deferred adaptation and failed emissions targets; reshuffles constraint toward adaptation-priority reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_dioxide_removal_feasibility, empirical, 'Feasibility of CDR technologies at scale to offset residual emissions').

omega_variable(
    adaptation_learning_curve_velocity,
    'How fast can adaptation technologies, infrastructure, and institutional capacity scale in vulnerable regions once investment occurs? Does the time lag between adaptation need and adaptation capacity create an irreversible lock-in?',
    'Historical analysis of climate adaptation deployment timelines (irrigation expansion, mangrove restoration, urban cooling infrastructure, early warning systems); comparison with emissions reduction cost curves; modeling of compound climate damages during adaptation lag',
    'If adaptation can scale quickly (< 5-10 years): deferral is viable. If slow (> 15-20 years) or locked by fiscal constraints: current deferral creates permanent adaptation deficit; empirically validates the snare classification from vulnerable regions'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_learning_curve_velocity, empirical, 'Timescale and scalability of climate adaptation capacity in vulnerable regions').

omega_variable(
    global_emissions_trajectory_under_mitigation_priority,
    'Will the mitigation-priority policy framework actually achieve emissions reductions sufficient to prevent 2°C+ warming, or will technology adoption remain outpaced by energy demand growth and incumbent carbon lock-in?',
    'Comparison of policy-pledged emissions reductions vs. actual reduction trajectories; accounting for rebound effects (efficiency gains increasing consumption), renewable energy capacity deployment rates, and carbon lock-in from infrastructure investment',
    'If sufficient: the framework''s bet on mitigation succeeds and adaptation deferral is reasonable. If insufficient: emissions trajectory still exceeds 2°C, and adaptation becomes urgent and increasingly expensive; the deferral becomes extractive cost-shifting to future generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_emissions_trajectory_under_mitigation_priority, empirical, 'Feasibility of achieving net-zero via mitigation-priority policy framework').

omega_variable(
    adaptation_vs_mitigation_cost_ratio_evolution,
    'What is the true cost ratio between mitigation today vs. adaptation later? Does deferring adaptation investments create compound interest on climate damages (negative adaptation learning curves)?',
    'Integrated assessment modeling (IAM) comparison of mitigation cost curves vs. future adaptation cost curves; empirical data on how delayed adaptation increases damage trajectory; accounting for adaptation infrastructure lifetimes and maladaptation costs',
    'If adaptation costs rise superlinearly (due to compound damages, infrastructure lock-in, ecosystem tipping points): deferral is a bet against future actors, validating the snare perspective. If costs scale predictably: deferral is a reasonable temporal allocation decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_vs_mitigation_cost_ratio_evolution, empirical, 'Cost-benefit ratio of mitigation today vs. adaptation later, accounting for path dependency').

omega_variable(
    kernel_framing_authority,
    'Which reading of the climate response imperative kernel should ground climate policy authority: the mitigation-priority reading (emissions reduction as primary), the adaptation-priority reading (localized resilience as primary), or the degrowth reading (reducing overall metabolic throughput)?',
    'Normative analysis of which reading reflects the legitimate interests of all stakeholders; empirical analysis of which policy frame produces just outcomes and durable coalitions; historical analysis of which framings gain institutional codification',
    'If mitigation-priority is the legitimate reading: this constraint is resolved as intended. If adaptation-priority gains legitimacy: this constraint reclassifies as false summit (naturalizing a contingent allocation). If degrowth gains ground: both mitigation and adaptation readings become residual, contingent on global North growth assumptions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_authority, preference, 'Legitimacy of the mitigation-priority reading relative to sibling readings of climate response').

omega_variable(
    green_colonialism_risk,
    'Does the mitigation-priority framework instantiate a new form of colonialism: wealthy nations directing climate finance to Global South mitigation projects (solar farms, hydropower) that serve Northern emissions reduction targets while leaving local communities dependent on externally-controlled infrastructure?',
    'Empirical analysis of climate finance flows and project ownership; documentation of local labor conditions and technology transfer in green projects; tracking of whether mitigation-financed infrastructure increases local agency or deepens technological dependency',
    'If green colonialism is present: the beneficiary status of Global North innovation sectors is understood as extraction masquerading as development; reshapes the constraint toward snare from vulnerable regions'' perspective. If mitigated by genuine technology transfer and local ownership: extraction is reduced and the tangled_rope classification for mid-income nations becomes more apt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(green_colonialism_risk, empirical, 'Whether mitigation-priority framework instantiates green colonialism or enables genuine technology transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mitigation_priority_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(miti_tr_t0, mitigation_priority_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(miti_tr_t10, mitigation_priority_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(miti_tr_t20, mitigation_priority_reading, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(miti_be_t0, mitigation_priority_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(miti_be_t10, mitigation_priority_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(miti_be_t20, mitigation_priority_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mitigation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(mitigation_priority_reading, adaptation_priority_reading).
narrative_ontology:affects_constraint(mitigation_priority_reading, degrowth_reading).
narrative_ontology:affects_constraint(mitigation_priority_reading, carbon_finance_extraction).
narrative_ontology:affects_constraint(mitigation_priority_reading, technology_transfer_conditionality).
narrative_ontology:affects_constraint(mitigation_priority_reading, intergenerational_discounting).

% DUAL FORMULATION NOTE:
% The climate response imperative kernel contains three structurally distinct constraints with different epsilon values and different beneficiary/victim distributions. This constraint is the mitigation-priority instantiation. The adaptation-priority and degrowth readings are separate constraint stories linked via network.affects_constraints. All three must be compiled to represent the full contested space; no single story represents the 'true' climate response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
