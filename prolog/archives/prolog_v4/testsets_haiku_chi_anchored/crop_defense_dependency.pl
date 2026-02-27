% ============================================================================
% CONSTRAINT STORY: crop_defense_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crop_defense_dependency, []).

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
 *   constraint_id: crop_defense_dependency
 *   human_readable: Dependency on External Pesticides due to Crop Biological Limitations
 *   domain: technological/agricultural
 *
 * SUMMARY:
 *   The dependency on external pesticides arises from a structural mismatch
 *   between crop genetics and agronomic practice. Industrial breeding in the
 *   mid-20th century optimized for yield and uniformity at the explicit
 *   expense of defensive traits (secondary metabolites, disease resistance,
 *   herbivory tolerance) that reduce marketable biomass. This created a
 *   technological dependency: defenseless cultivars require external chemical
 *   protection to survive pest and pathogen pressure. The constraint exhibits
 *   the full range of DR types from different structural positions.
 *   Smallholder farmers are trapped (snare), bearing extraction costs with no
 *   exit. The agrochemical and seed industries experience the constraint as
 *   solved coordination (rope), capturing rents from the artificially
 *   sustained demand. Large commercial agriculture experiences mixed
 *   coordination and extraction (tangled rope), benefiting from
 *   chemical-based efficiency but constrained by input costs and regulatory
 *   burden. Food supply chains experience increasing extraction as residue
 *   liabilities grow. The agroecological transition movement sees this as a
 *   temporary engineering problem solvable through genetic recovery and
 *   integrated pest management (scaffold). The civilizational analytical
 *   observer risks naturalizing the design choice as inherent biological law
 *   (false summit mountain). The constraint's theater ratio (0.52) reflects
 *   that much pesticide use is genuinely functional (targeting real pest
 *   pressure), not purely performative, but with increasing performative
 *   elements (prophylactic application, insurance dosing) as pest resistance
 *   develops.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers: Primary victim (powerless/trapped) — economically dependent on commodity crops, cannot switch cultivars or stop pesticide use without income collapse
 *   - Agricultural Soil and Pest Ecology: Primary victim (moderate/constrained) — accumulating toxin burden, losing natural pest control predators and parasitoids as chemical intensity increases
 *   - Agrochemical Companies: Primary beneficiary (institutional/arbitrage) — secured stable, inelastic demand through crop genetics; can exit if markets shift
 *   - Seed Breeding Programs: Primary beneficiary (organized/arbitrage) — locked in licensing/patent architecture; beneficiary of continued high-yield, low-defense breeding direction
 *   - Large Commercial Agriculture: Mixed (powerful/mobile) — benefits from chemical simplicity and economies of scale, but increasingly constrained by input costs and regulatory tightening
 *   - Food Processing and Supply Chains: Mixed (organized/constrained) — benefit from uniform crop inputs, constrained by pesticide residue liability and consumer concerns
 *   - Agroecological Transition Agents: Organized transitional actor (organized/constrained) — see structural sunset through genetic recovery and IPM, but face scaling and economic barriers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design choice as inherent biological limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crop_defense_dependency, 0.58).
domain_priors:suppression_score(crop_defense_dependency, 0.68).
domain_priors:theater_ratio(crop_defense_dependency, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crop_defense_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(crop_defense_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(crop_defense_dependency, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crop_defense_dependency, tangled_rope).
narrative_ontology:human_readable(crop_defense_dependency, "Dependency on External Pesticides due to Crop Biological Limitations").
narrative_ontology:topic_domain(crop_defense_dependency, "technological/agricultural").

domain_priors:requires_active_enforcement(crop_defense_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crop_defense_dependency, agrochemical_companies).
narrative_ontology:constraint_beneficiary(crop_defense_dependency, industrial_seed_breeding_programs).
narrative_ontology:constraint_victim(crop_defense_dependency, smallholder_farmers).
narrative_ontology:constraint_victim(crop_defense_dependency, agricultural_soil_health).
narrative_ontology:constraint_victim(crop_defense_dependency, ecological_pest_predator_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped in dependency loop. Cannot switch crops (market-dependent, local climate constraints), cannot stop using pesticides (defenseless cultivars require them), cannot afford integrated pest management alternatives. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Maximal extraction with minimal exit.
constraint_indexing:constraint_classification(crop_defense_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOIL HEALTH AND PEST ECOLOGY (SNARE) — Constrained victim: cannot opt out of pesticide exposure, bears accumulating toxin burden, loses natural pest control mechanisms as predator networks collapse. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.76. Extraction appears as ecological debt.
constraint_indexing:constraint_classification(crop_defense_dependency, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: AGROCHEMICAL INDUSTRY (ROPE) — Institutional beneficiary. Experiences constraint as pure coordination: defenseless crops require pesticides, creating stable demand. No extraction component from this actor's perspective; they see a solved problem. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary. Has arbitrage exit: can shift product lines if demand changes.
constraint_indexing:constraint_classification(crop_defense_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SEED BREEDING PROGRAMS (ROPE) — Organized institutional actor. Benefits from continued high-yield, low-defense breeding strategy: product differentiation, patent/licensing architecture, locked-in customer base. Experiences constraint as coordination achieved — solved the yield problem. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Arbitrage exit: can retool to disease-resistant varieties if profitable.
constraint_indexing:constraint_classification(crop_defense_dependency, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE-SCALE COMMERCIAL AGRICULTURE (TANGLED ROPE) — Powerful agents with mobile exit (can adopt integrated pest management, crop rotation, precision application). But also benefit from the current system: pesticides are cheaper than labor-intensive alternatives, economies of scale apply to chemical application infrastructure. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.36. Mixed experience: benefits from simplicity, constrained by chemical cost volatility and regulatory tightening.
constraint_indexing:constraint_classification(crop_defense_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FOOD PROCESSING AND RETAIL SUPPLY CHAINS (TANGLED ROPE) — Organized institutional actors benefit from cheap, uniform crop inputs (high-yield monocultures are logistics-optimized). But constrained by pesticide residue limits, retailer brand risk from contamination scandals, and regulatory compliance burden. d≈0.52, f(d)≈0.70, σ=1.1 → χ≈0.45. Hybrid: benefits from current system but increasingly extracted from via liability and compliance costs.
constraint_indexing:constraint_classification(crop_defense_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: AGROECOLOGICAL TRANSITION MOVEMENT (SCAFFOLD) — Organized agents (extension services, regenerative agriculture coalitions, organic certification bodies) see the dependency as temporary and solvable: crop defense can be rebuilt through genetic recovery and integrated pest management protocols. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.26. Low effective extraction because the movement has agency and sees a structural sunset (the constraint can be engineered away). Theater ratio ≤0.70, so scaffold gate passes with sunset implication.
constraint_indexing:constraint_classification(crop_defense_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, high-yield agriculture may appear to require a tradeoff between yield and defense. This perspective risks naturalizing the constraint as inherent to crop biology: 'you cannot have both high yield and robust defenses.' However, the structural data (ε=0.58, suppression=0.68) contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent tradeoff' framing naturalizes what is actually a contingent design choice made in mid-20th-century breeding.
constraint_indexing:constraint_classification(crop_defense_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crop_defense_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crop_defense_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crop_defense_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(crop_defense_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(crop_defense_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The dependency is real and imposes measurable costs on farmers and ecosystems, but is not as severe as a pure snare (extraction ≥0.66). Much of the pesticide use is genuinely required for current cultivars; the extraction is built into the breeding architecture rather than parasitic. The trajectory from 0.35 to 0.58 over 70 years reflects the deepening lock-in as pest resistance develops and chemical alternatives multiply. Suppression (0.68): High. Barriers to exit are substantial: farmers cannot switch crops (market structure, climate), cannot stop pesticides (defenseless cultivars), cannot access integrated pest management (requires training, labor, ecological infrastructure). But suppression is not total (some IPM exists, some organic farming succeeds). Theater ratio (0.52): Moderate. Pesticide application is partly functional (responding to real pest pressure), partly insurance/prophylactic (applied even when pest pressure is low). The theater increases as pest resistance requires increasing dosages to achieve the same efficacy, creating a treadmill where application becomes ritualized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates deep perspectival divergence. Smallholder farmers and soil ecology experience snare-level extraction with no visible exit. Agrochemical and seed companies experience rope-level coordination with benefits. Large agriculture experiences tangled rope — benefiting from the system but increasingly constrained by its costs and rigidity. The agroecological transition sees a solvable engineering problem (scaffold) that can be resolved through genetic recovery. The civilizational analytical observer risks seeing an immutable biological law (mountain) — that high yield requires low defense — but the structural data reveals this as a false summit: the tradeoff is a contingent design choice, not a law of nature. The perspectival gap widens as climate change and pest resistance accelerate, making the old optimization (yield at defense expense) increasingly fragile.
 *
 * DIRECTIONALITY LOGIC:
 *   Smallholder farmers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction: cannot exit, bear full cost. Agricultural soil and ecology: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction: cannot opt out of exposure, suffer accumulating damage. Agrochemical companies: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary: have full exit optionality, benefit from continued dependency. Seed programs: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary: locked-in licensing benefits them, arbitrage exit available. Large agriculture: Mixed + mobile → d≈0.48, f(d)≈0.62. Moderate extraction: can exit (IPM, diversification), but benefits from current simplicity. Food chains: Mixed + constrained → d≈0.52, f(d)≈0.70. Moderate extraction: constrained by liability but benefiting from uniform inputs. Agroecological agents: Organized + constrained → d≈0.45, f(d)≈0.50. Low extraction: organized agency and visible exit path reduce effective extraction even though actors are constrained by transition barriers. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification rejected by false summit detector.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by disambiguating the natural law claim from the technological lock-in. The false summit (mountain perspective) claims that 'high-yield crops cannot have robust defenses' — this is a mid-20th-century breeding choice presented as law. The actual constraint is the technological lock-in (tangled rope): crops have been bred into genetic defenselessness, creating sustained demand for external protection. This is neither immutable nor purely extractive — it is a hybrid that can be engineered away (scaffold sunset) through deliberate breeding for dual-optimized varieties. The mandatrophy resolves by showing that the perspectival gap is not about disagreement on facts but about whether the constraint is treated as inevitable (false summit) or solvable (scaffold/tangled rope). The agroecological transition perspective proves the constraint is engineering-solvable: many landraces and wild relatives combine moderate yield with robust defense. The contemporary breeding direction (precision breeding for resilience, CRISPR enhancement of defense traits) is recovering this dual optimization. The constraint's extractiveness will decline as genetics enables defense recovery without yield penalty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    yield_defense_tradeoff_law,
    'Is the yield-defense tradeoff a fundamental biological law or a contingent outcome of 20th-century breeding choices?',
    'Genetic analysis of wild crop relatives and landraces; screening for varieties combining high yield with robust defense; directed breeding experiments toward dual optimization',
    'If law: constraint is mountain (immutable). If contingent: constraint is tangled rope / scaffold (engineerable). This determines whether the dependency is permanent or temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(yield_defense_tradeoff_law, empirical, 'Whether yield-defense tradeoff is biological law or design choice').

omega_variable(
    smallholder_exit_feasibility,
    'Can smallholder farmers economically viably exit pesticide dependency through integrated pest management without external subsidy?',
    'Cost-benefit analysis of IPM protocols vs chemical dependency; longitudinal farmer income tracking in IPM transition zones; market price premium realization for pesticide-reduced crops',
    'If viable: snare classification weakens (exit becomes possible). If not viable: snare persists and extraction deepens as global commodity prices pressure farmers toward chemical intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smallholder_exit_feasibility, empirical, 'Economic viability of IPM exit for smallholders').

omega_variable(
    genetic_defense_recovery_rate,
    'How quickly can crop defense mechanisms be recovered through directed breeding without sacrificing yield gains?',
    'Historical analysis of crop improvement projects emphasizing disease resistance (e.g., Aphanomyces resistance in peas, bacterial wilt tolerance in tomato); genetic mapping of defense loci; breeding cycle time projections',
    'If recovery < 10 years: scaffold sunset is credible. If recovery > 30 years: scaffold becomes pipe dream, and constraint remains snare/tangled rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_defense_recovery_rate, empirical, 'Timeline for genetic defense recovery without yield loss').

omega_variable(
    alternative_pest_control_scaling,
    'Can biological pest control (predatory insects, parasitoids, entomopathogenic fungi) scale to industrial-scale monocultures without pesticide support?',
    'Analysis of biological control success rates in field-scale deployments; comparison of pest pressure in organic vs conventional systems; habitat provisioning requirements per hectare',
    'If scaling is feasible: agroecological transition becomes structural option (scaffold becomes rope). If scaling requires pesticide-free refugia (incompatible with monoculture): constraint remains tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pest_control_scaling, empirical, 'Scalability of biological pest control to monoculture systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crop_defense_dependency, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cropdef_tr_t0, crop_defense_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cropdef_tr_t35, crop_defense_dependency, theater_ratio, 35, 0.45).
narrative_ontology:measurement(cropdef_tr_t70, crop_defense_dependency, theater_ratio, 70, 0.52).

% Extraction over time
narrative_ontology:measurement(cropdef_be_t0, crop_defense_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cropdef_be_t35, crop_defense_dependency, base_extractiveness, 35, 0.47).
narrative_ontology:measurement(cropdef_be_t70, crop_defense_dependency, base_extractiveness, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crop_defense_dependency, resource_allocation).
narrative_ontology:affects_constraint(crop_defense_dependency, agricultural_chemical_resistance_treadmill).
narrative_ontology:affects_constraint(crop_defense_dependency, monoculture_fragility_ecosystem).
narrative_ontology:affects_constraint(crop_defense_dependency, smallholder_farmer_debt_trap).

% DUAL FORMULATION NOTE:
% The crop defense dependency decomposes into three structurally distinct constraints: (1) the breeding choice that eliminated defenses (technological lock-in, ε=0.58, this story), (2) the resistance treadmill that amplifies pesticide intensity over time (ε=0.72, upstream), and (3) the farmer debt trap created by recurring chemical purchase obligations (ε=0.68, downstream institutional). Each has different ε values because they measure different structural properties: the breeding lock-in, the ecological feedback loop, and the financial extraction mechanism. All three are linked — solving any one weakens the others — making this a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(crop_defense_dependency, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
