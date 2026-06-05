% ============================================================================
% CONSTRAINT STORY: degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_degrowth_transformation, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: degrowth_transformation
 *   human_readable: Degrowth Transformation as Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth transformation reading asserts that legitimate climate
 *   response requires wealthy nations to dismantle growth imperatives through
 *   structural economic changes: universal basic services, working time
 *   reduction, and democratic firm ownership. This is one reading of a
 *   contested kernel — the definition of 'legitimate climate response' — that
 *   competes with mitigation_priority (technological decoupling can solve
 *   climate crisis without degrowth) and adaptation_priority (adaptation is
 *   the primary response, transformation secondary). The degrowth reading
 *   claims that growth capitalism's structural logic makes sufficient
 *   emissions reduction impossible without transformation. Unlike the other
 *   readings, degrowth reading places current wealthy populations in the
 *   victim/cost-bearer set, requiring immediate structural sacrifice for
 *   civilizational stability. This reading instantiates a specific claim
 *   about the relationship between economic structures and climate physics:
 *   that consumption patterns in wealthy nations are thermodynamically
 *   coupled to emissions in ways that technological substitution cannot fully
 *   decouple. The constraint exhibits high extractiveness (0.68) because the
 *   transformation imposes concentrated costs on powerful actors (fossil fuel
 *   sectors, growth-dependent states, high-consumption populations) while
 *   diffusing benefits across future generations and ecosystems. Theater
 *   ratio is low (0.38) — the transformation is functionally direct, not
 *   performative — because degrowth mechanisms (working time, basic services,
 *   firm ownership) directly change economic structures rather than
 *   performing symbolic compliance.
 *
 * KEY AGENTS:
 *   - Future Generations & Ecosystems: Primary beneficiaries (analytical/analytical) — benefit from climate stability without technological dependency risk; cannot organize to claim benefits within current political economy
 *   - Current Wealthy Populations: Primary victims (institutional/trapped) — income reduction, consumption constraints, identity disruption for growth-dependent identities; no exit option short of violent resistance or capital flight (which itself extracts)
 *   - Fossil Fuel & High-Consumption Sectors: Institutional victims (institutional/trapped) — asset stranding, business model dissolution, no internal reorganization pathway; pure extraction from their perspective
 *   - Working-Class Workers in Wealthy Nations: Mixed victims/beneficiaries (moderate/constrained) — bear costs (wage reduction, skill obsolescence) but benefit from coordination function (working time reduction, basic services, firm ownership); exit options constrained but not absent
 *   - Labor Movements & Cooperatives: Organized beneficiaries (organized/constrained) — degrowth aligns with labor demands; perceive constraint as coordination mechanism with constrained but collectivized exits
 *   - Wealthy Nation States: Institutional actors (institutional/constrained) — benefit from intergenerational legitimacy and climate stability but lose growth-dependent power structures; civilizational horizon reveals constraint is truly binding
 *   - Climate-Vulnerable Populations: Powerless victims (powerless/trapped) — benefit from climate stability but receive no immediate relief; trapped in double extraction: growth dependency created crisis + transformation offers no alternative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(degrowth_transformation, 0.68).
domain_priors:suppression_score(degrowth_transformation, 0.72).
domain_priors:theater_ratio(degrowth_transformation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(degrowth_transformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(degrowth_transformation, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(degrowth_transformation, snare).
narrative_ontology:human_readable(degrowth_transformation, "Degrowth Transformation as Legitimate Climate Response").
narrative_ontology:topic_domain(degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(degrowth_transformation, '8facd1fe-961c-4026-b5bc-d607a471622d').
narrative_ontology:cs_created_at('8facd1fe-961c-4026-b5bc-d607a471622d', '').
narrative_ontology:cs_kernel_codification('8facd1fe-961c-4026-b5bc-d607a471622d', formalized).
narrative_ontology:cs_authority_grounding('8facd1fe-961c-4026-b5bc-d607a471622d', lineage).
narrative_ontology:cs_interpretation_layer_present('8facd1fe-961c-4026-b5bc-d607a471622d').
narrative_ontology:cs_kernel_id(degrowth_transformation, climate_response_legitimacy).
narrative_ontology:cs_reading_relation('8facd1fe-961c-4026-b5bc-d607a471622d', mitigation_priority, influences).
narrative_ontology:cs_reading_relation('8facd1fe-961c-4026-b5bc-d607a471622d', adaptation_priority, influences).
narrative_ontology:cs_axiom('8facd1fe-961c-4026-b5bc-d607a471622d', foundational, growth_decoupling_insufficient).
narrative_ontology:cs_axiom_status(growth_decoupling_insufficient, holdable).
narrative_ontology:cs_axiom('8facd1fe-961c-4026-b5bc-d607a471622d', foundational, structural_transformation_necessary).
narrative_ontology:cs_axiom_status(structural_transformation_necessary, holdable).
narrative_ontology:cs_reference_frame('8facd1fe-961c-4026-b5bc-d607a471622d', thermodynamic_carbon_budget).
narrative_ontology:cs_drift_state('8facd1fe-961c-4026-b5bc-d607a471622d', contemporary_post_paris_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(degrowth_transformation, ecosystems).
narrative_ontology:constraint_victim(degrowth_transformation, current_wealthy_population).
narrative_ontology:constraint_victim(degrowth_transformation, high_consumption_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Powerless agents (low-income populations in developed and developing nations) face immediate climate impacts and bear suppression through both material barriers (lack of resources for adaptation) and institutional barriers (decision-making exclusion). Degrowth transformation offers no immediate relief; vulnerable populations are trapped in the constraint. They experience pure extraction: the wealthy nations' growth dependency created the climate emergency; the degrowth response requires structural change that offers no alternative exit path for the most vulnerable.
constraint_indexing:constraint_classification(degrowth_transformation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKING-CLASS WORKERS IN WEALTHY NATIONS (TANGLED ROPE) — Moderate power agents with constrained exits experience mixed coordination and extraction. Working-time reduction and universal basic services provide genuine coordination benefits — real material security and freedom from overwork. But the structural transformation imposes costs: income reduction, skill obsolescence in extraction industries (fossil fuels, high-consumption manufacturing), and identity disruption for workers whose professional identity is bound to growth-dependent sectors. Generational horizon: working-age cohort bears costs; next generation benefits from both reduced working time and stable climate. Effective extraction is asymmetric but not total.
constraint_indexing:constraint_classification(degrowth_transformation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LABOR MOVEMENTS & WORKER COOPERATIVES (ROPE) — Organized agents with constrained but collectivized exits perceive the constraint as coordination. Working-time reduction and democratic firm ownership are direct labor movement demands that solve collective action problems (work-sharing, wage stability, participatory governance). The constraint aligns with labor's structural interests. Coordination function is high; extraction is experienced as redistribution rather than surplus seizure. Generational scope: union power and worker ownership build across a generation.
constraint_indexing:constraint_classification(degrowth_transformation, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: FOSSIL FUEL & HIGH-CONSUMPTION SECTORS (SNARE) — Institutional beneficiaries of growth face complete structural transformation. From their perspective, degrowth is pure extraction: capital assets (coal reserves, refineries, petrochemical plants, luxury goods supply chains) lose all value. No arbitrage exit; no internal reorganization preserves the sector's structure. The constraint forces these institutions into the victim set despite their current power. Suppression is total for these actors — the transformation requires their dissolution, not reform. Pure snare: maximum extraction, no coordination benefit.
constraint_indexing:constraint_classification(degrowth_transformation, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: WEALTHY NATION STATES (TANGLED ROPE) — Institutional power facing constrained exits over civilizational scope. The constraint coordinates genuine functions: climate stabilization, intergenerational legitimacy, domestic political stability through redistribution. But it also extracts from the state's capacity to maintain growth-dependent power structures (military-industrial complex, geopolitical leverage based on consumption capacity, capital accumulation mechanisms). Civilizational scope captures that nation-states as institutional forms may not survive degrowth transformation intact — exit options are truly constrained, not merely costly. Both coordination and asymmetric extraction are structural.
constraint_indexing:constraint_classification(degrowth_transformation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal analytical perspective, degrowth transformation is a coordination mechanism solving collective action problems: climate stabilization, intergenerational equity, work-life balance, and democratic firm ownership all address real coordination failures of growth capitalism. The analytical view de-emphasizes the extraction costs borne by wealthy-nation populations and fossil fuel sectors, treating these as contingent transition costs rather than structural constraints. This perspective risks underestimating suppression and treating the transformation as more feasible than middle-power perspectives suggest.
constraint_indexing:constraint_classification(degrowth_transformation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(degrowth_transformation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(degrowth_transformation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(degrowth_transformation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The transformation concentrates costs on powerful actors (wealthy-nation populations averaging 5-10x global average consumption, fossil fuel sectors, growth-dependent capital). Benefits are diffused temporally (future generations) and categorically (ecosystems cannot claim benefits). The directionality from institutional fossil fuel perspective (d ≈ 0.98) produces maximum χ; from analytical observer perspective (d ≈ 0.50) produces lower χ. Base extractiveness reflects the structural reality that transformation transfers welfare from present wealthy to future poor and non-human systems. Suppression (0.72): High. Multiple binding mechanisms enforce the constraint's necessity: thermodynamic limits on decoupling (energy return ratios decline for renewable systems), carbon budget constraints (remaining emissions budget < business-as-usual pathway requires structural change), and lock-in dynamics (infrastructure and financial systems are structured around growth). Agents cannot exit via arbitrage (capital flight triggers degrowth elsewhere), via technological substitution (decoupling is insufficient per the reading's premises), or via geographic relocation (climate is global). Theater ratio (0.38): Low. Degrowth mechanisms are functionally direct: working-time reduction directly reduces consumption; universal basic services directly redistribute; democratic firm ownership directly changes decision-making structures. Unlike carbon pricing or green growth narratives, degrowth offers no performance dimension — the constraint requires actual structural change, not performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The widest perspectival gap separates the analytical observer (Rope) from the powerless and institutional victim perspectives (Snare). The analytical view de-emphasizes transition costs and treats degrowth as a coordination solution to climate instability. The powerless perspective sees immediate extraction with no benefit path; the wealthy-nation institutional perspective sees their power base dissolving. The working-class perspective reveals the crucial distinction: degrowth is Tangled Rope (mixed costs and benefits) from the worker perspective because labor movement demands (working time, firm democracy) are embedded in the transformation. This gap reveals that the constraint's classification depends critically on which class of actors is evaluated. The fossil fuel sector perspective is unambiguously Snare — total asset loss, no reorganization pathway. The worker perspective is Tangled Rope — real coordination gains alongside real costs. The wealthy consumer perspective is Snare — identity disruption, consumption reduction, no exit. The analytical perspective is Rope — sees primarily the coordination function. This perspectival distribution is diagnostic: Snare dominates for powerful actors in wealthy nations; Tangled Rope dominates for organized labor; Rope dominates for disembedded analytical view.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each perspective's structural relationship to the transformation. Fossil fuel sectors: d ≈ 0.98 (full target, zero beneficiary, trapped exit) → f(d) ≈ 1.42 → χ = 0.68 × 1.42 × 1.2 (global scope) ≈ 1.16 (excess extraction confirmed). Current wealthy consumers: d ≈ 0.82 (net target despite some coordination benefits, constrained exit) → f(d) ≈ 1.15 → χ = 0.68 × 1.15 × 1.0 (national scope) ≈ 0.78. Working-class labor: d ≈ 0.55 (roughly balanced between coordination benefit and cost, constrained exit) → f(d) ≈ 0.75 → χ = 0.68 × 0.75 × 1.0 ≈ 0.51 (moderate extraction). Future generations: d ≈ 0.05 (full beneficiary, analytical exit) → f(d) ≈ -0.12 → χ = 0.68 × (-0.12) × 1.2 ≈ -0.10 (no extraction; coordinate via negative chi). The directionality variation explains the perspectival gap: the constraint's effective extractiveness ranges from 1.16 (fossil fuels) to -0.10 (future generations), with working-class perspectives in between.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED. The degrowth reading resolves the mandatrophy by explicitly accepting Snare classification for powerful actors in wealthy nations. The constraint is not an attempt to hide extraction beneath coordination language. It is a direct claim: meaningful climate response requires structural redistribution FROM wealthy-nation consumption to climate stabilization and future welfare, and this redistribution IS extraction from the perspective of those who bear costs. The reading avoids the false choice between 'this is extraction' and 'this is coordination' by recognizing that the constraint is BOTH: it coordinates intergenerational climate stability while extracting from current wealthy populations. The Tangled Rope classification for working-class labor reflects genuine embedding of labor movement demands within the transformation — it is coordination in that dimension. The analytical reading as Rope reflects the analytical observer's detachment from the immediate extraction costs. Mandatrophy is resolved not by reclassifying the constraint but by accepting the multiplex structure: Snare for fossil fuels, Snare for wealthy consumers, Tangled Rope for organized labor, Rope for analytical observer, Snare for climate-vulnerable populations, Rope for future generations. The 'mandate' that transformation must be labeled 'good growth' or 'green coordination' is explicitly rejected in favor of structural honesty about redistribution costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_vs_structural_necessity,
    'Does the structural necessity of degrowth for climate stabilization carry political and economic feasibility, or is the requirement structurally sound but politically impossible?',
    'Historical analysis of analogous structural transformations (post-war conversion, post-Soviet transition); cost-benefit modeling of degrowth pathways vs. delayed action + catastrophic adaptation; political economy analysis of anti-degrowth coalition power',
    'If feasible: Tangled Rope constraints can transition to Rope (coordination becomes dominant). If infeasible: Snare classification holds across all perspectives except analytical; constraint becomes a clarification of impossible choice rather than a viable response.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_vs_structural_necessity, preference, 'Political and economic feasibility of degrowth transformation').

omega_variable(
    growth_decoupling_falsifiability,
    'Can technological decoupling (renewable energy + circular economy + efficiency gains) provide sufficient emissions reduction WITHOUT structural degrowth, making the degrowth transformation unnecessary?',
    'Empirical tracking of energy-return-on-energy-invested (EROI) for renewable systems; carbon budget modeling under different decoupling scenarios; historical analysis of past decoupling claims vs. rebound effects',
    'If decoupling succeeds: constraint disappears entirely; mitigation_priority reading becomes structurally sound. If decoupling fails: degrowth transformation becomes unavoidable; suppression rises as the trap becomes visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_falsifiability, empirical, 'Whether technological decoupling can substitute for degrowth').

omega_variable(
    global_degrowth_coordination,
    'Can wealthy nations implement degrowth unilaterally while developing nations pursue growth, or does degrowth require coordinated global implementation?',
    'Trade flow analysis under unilateral degrowth; modeling of capital flight and carbon leakage; political economy of competitive advantage under different growth regimes',
    'If unilateral degrowth is feasible: Wealthy nation states experience lower suppression (exit via closed borders exists). If global coordination is necessary: Suppression rises (trap is global); constraint becomes civilizational rather than national.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_degrowth_coordination, empirical, 'Global coordination requirements for degrowth implementation').

omega_variable(
    kernel_reading_contest_degrowth_vs_techno_mitigation,
    'This constraint is one reading of the climate response legitimacy kernel. The degrowth reading claims structural necessity; the mitigation_priority sibling reading claims technological sufficiency. Which reading''s core premises foreclose the other, and which coexist or influence?',
    'Structural analysis of whether decoupling success resolves the degrowth reading''s core claim. If decoupling is impossible, degrowth forecloses mitigation_priority in single framework. If decoupling is contingent/uncertain, readings coexist across different factual beliefs. If decoupling is possible but politically infeasible, degrowth influences (creates pressure on) mitigation_priority without foreclosing it.',
    'If readings foreclose: one reading must be chosen; constraint classification changes based on which prevails. If coexistent: both readings remain live; hybrid responses become possible. If influential: degrowth shapes mitigation_priority''s boundary conditions without eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_degrowth_vs_techno_mitigation, conceptual, 'Structural relationship between degrowth transformation and technological mitigation priority readings').

omega_variable(
    democratic_firm_ownership_scaling,
    'Can democratic firm ownership (worker cooperatives, participatory governance) scale to the complexity and coordination demands of modern economic infrastructure (semiconductor supply chains, pharmaceutical R&D, power grid management), or does scale require centralized capital and hierarchical decision-making?',
    'Empirical analysis of cooperative scaling (Mondragon''s industrial capacity, worker cooperative performance in complex sectors); organizational theory modeling of governance scalability; historical comparison with centralized socialist coordination failures',
    'If democratic ownership scales: degrowth transformation preserves functional coordination. If scaling fails: degrowth transformation faces governance bottleneck; suppression rises as the required hierarchical structures reassert themselves under functional pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_firm_ownership_scaling, empirical, 'Scalability of democratic firm ownership to modern economic complexity').

omega_variable(
    intergenerational_extraction_asymmetry,
    'Does the degrowth transformation constitute extraction from the current wealthy generation or legitimate intergenerational redistribution? Where is the boundary between necessary reallocation and unjust confiscation?',
    'Philosophical analysis of intergenerational justice frameworks; empirical assessment of current generation''s consumption vs. carbon budget share; modeling of welfare trade-offs (reduced consumption now vs. climate stability later)',
    'If perceived as extraction: current generation bears Snare classification; legitimacy of transformation degrades. If perceived as redistribution: Tangled Rope classification holds; transformation becomes ethically justified even if costly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_extraction_asymmetry, preference, 'Intergenerational extraction vs. legitimate redistribution boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(degrowth_transformation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degr_tr_t0, degrowth_transformation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(degr_tr_t10, degrowth_transformation, theater_ratio, 10, 0.4).
narrative_ontology:measurement(degr_tr_t20, degrowth_transformation, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(degr_be_t0, degrowth_transformation, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(degr_be_t10, degrowth_transformation, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(degr_be_t20, degrowth_transformation, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(degrowth_transformation, climate_mitigation_priority).
narrative_ontology:affects_constraint(degrowth_transformation, climate_adaptation_priority).
narrative_ontology:affects_constraint(degrowth_transformation, fossil_fuel_stranding_asset).
narrative_ontology:affects_constraint(degrowth_transformation, working_time_reduction).
narrative_ontology:affects_constraint(degrowth_transformation, universal_basic_services).
narrative_ontology:affects_constraint(degrowth_transformation, cooperative_firm_ownership).

% DUAL FORMULATION NOTE:
% Degrowth transformation is the upstream constraint shaping the viability and extractiveness of specific downstream constraints: fossil fuel assets can be stranded (separate constraint with different ε), working time can be reduced (separate constraint with separate victims/beneficiaries), basic services can be universalized (separate constraint). Each downstream constraint has its own measurement of extractiveness and suppression. The degrowth transformation story provides the structural context that determines whether these downstream constraints are feasible or create cascading secondary extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(degrowth_transformation, institutional, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
