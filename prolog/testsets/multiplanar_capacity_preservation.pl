% ============================================================================
% CONSTRAINT STORY: multiplanar_capacity_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_multiplanar_capacity_preservation, []).

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
 *   constraint_id: multiplanar_capacity_preservation
 *   human_readable: Multiplanar Capacity Preservation Through Deliberate Training Variation
 *   domain: exercise_physiology/gerontology/preventive_medicine
 *
 * SUMMARY:
 *   The multiplanar capacity preservation constraint emerges from the
 *   intersection of three structural forces: (1) age-related physiological
 *   decline in rotational power and lateral stability (upstream mountain
 *   constraint), (2) use-dependent atrophy from sagittal-plane-dominant
 *   modern movement patterns (walking, running, cycling, traditional strength
 *   training), and (3) the fitness industry's response through specialized
 *   multiplanar programming. The constraint exhibits genuine coordination
 *   function — deliberate training across all movement planes does preserve
 *   capacities that would otherwise erode — but embeds significant extraction
 *   through complexity barriers, equipment requirements, and professional
 *   gatekeeping. The base extractiveness (0.48) reflects moderate extraction:
 *   the coordination benefit is real, but the delivery mechanism creates
 *   asymmetric costs. Time-constrained general population exercisers face a
 *   choice between accepting capacity erosion (staying with familiar sagittal
 *   routines) or paying the complexity tax (learning unfamiliar movement
 *   patterns, accessing specialized equipment, potentially hiring
 *   professional guidance). The theater ratio (0.38) is moderate: some
 *   multiplanar programming is genuinely functional (addressing real
 *   physiological needs), but some is performative complexity (exotic
 *   exercises that don't meaningfully differ from simpler alternatives). The
 *   constraint's extractiveness has increased over the interval as the
 *   fitness industry has layered commercial complexity onto the genuine
 *   coordination function.
 *
 * KEY AGENTS:
 *   - Time-Constrained General Population Exerciser: Primary victim (powerless/trapped) — limited training time, unfamiliarity with complex movements, gym environments not designed for multiplanar work; faces extraction through knowledge barrier and equipment access requirements
 *   - Motivated Self-Directed Trainer: Secondary victim (moderate/constrained) — can learn multiplanar programming but faces trial-and-error costs and injury risk; also benefits from genuine capacity preservation
 *   - Fitness Industry Specialists: Primary beneficiary (institutional/arbitrage) — personal trainers, functional fitness coaches monetize specialized knowledge; the constraint's complexity is their value proposition
 *   - Physical Therapy Profession: Secondary beneficiary (institutional/arbitrage) — benefits through both preventive programming and corrective rehabilitation; experiences as coordination service
 *   - Movement Culture Community: Organized agents (organized/mobile) — parkour, MovNat, dance-based fitness building alternative ecosystems where multiplanar capacity is default; sees constraint as temporary with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (physiological benefit is real) and embedded extraction (complexity barrier creates gatekeeping)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(multiplanar_capacity_preservation, 0.48).
domain_priors:suppression_score(multiplanar_capacity_preservation, 0.52).
domain_priors:theater_ratio(multiplanar_capacity_preservation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(multiplanar_capacity_preservation, extractiveness, 0.48).
narrative_ontology:constraint_metric(multiplanar_capacity_preservation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(multiplanar_capacity_preservation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(multiplanar_capacity_preservation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(multiplanar_capacity_preservation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(multiplanar_capacity_preservation, tangled_rope).
narrative_ontology:human_readable(multiplanar_capacity_preservation, "Multiplanar Capacity Preservation Through Deliberate Training Variation").
narrative_ontology:topic_domain(multiplanar_capacity_preservation, "exercise_physiology/gerontology/preventive_medicine").

domain_priors:requires_active_enforcement(multiplanar_capacity_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(multiplanar_capacity_preservation, fitness_industry_specialists).
narrative_ontology:constraint_beneficiary(multiplanar_capacity_preservation, athletic_performance_sector).
narrative_ontology:constraint_beneficiary(multiplanar_capacity_preservation, physical_therapy_profession).
narrative_ontology:constraint_victim(multiplanar_capacity_preservation, general_population_training_comfort).
narrative_ontology:constraint_victim(multiplanar_capacity_preservation, time_constrained_exercisers).
narrative_ontology:constraint_victim(multiplanar_capacity_preservation, traditional_gym_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TIME-CONSTRAINED EXERCISER (SNARE) — Trapped by limited training time, unfamiliarity with complex movement patterns, and gym environments optimized for sagittal-plane machines. Faces extraction through requirement for specialized knowledge, equipment access, and coordination capacity that wasn't necessary for basic fitness maintenance. The constraint presents as: 'your comfortable routine is inadequate for healthy aging.'
constraint_indexing:constraint_classification(multiplanar_capacity_preservation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SELF-DIRECTED TRAINER (TANGLED ROPE) — Constrained by learning curve and injury risk when self-programming multiplanar work, but genuinely benefits from the coordination function: deliberate plane variation does preserve capacities that pure sagittal training loses. Experiences both the extraction (complexity tax, trial-and-error costs) and the coordination benefit (maintained rotational power, lateral stability into older age).
constraint_indexing:constraint_classification(multiplanar_capacity_preservation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FITNESS SPECIALIST (ROPE) — Personal trainers, functional fitness coaches, and sports performance professionals benefit from the constraint through specialized knowledge monetization. The complexity of multiplanar programming creates a coordination service market: clients need expert guidance to navigate plane-specific loading, progression schemes, and injury risk. Net beneficiary — the constraint's complexity is their value proposition.
constraint_indexing:constraint_classification(multiplanar_capacity_preservation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHYSICAL THERAPY PROFESSION (ROPE) — Benefits from the constraint through both preventive programming (teaching multiplanar capacity before injury) and corrective work (rehabilitating use-dependent atrophy after injury). The constraint creates a professional service niche that didn't exist when sagittal-plane training was considered sufficient. Experiences as coordination: helping clients maintain functional capacity across the lifespan.
constraint_indexing:constraint_classification(multiplanar_capacity_preservation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MOVEMENT CULTURE COMMUNITY (SCAFFOLD) — Organized communities (parkour, MovNat, animal flow, dance-based fitness) see the constraint as temporary: they are building alternative training ecosystems where multiplanar capacity is the default, not a specialized add-on. As these movement practices diffuse into mainstream fitness culture, the knowledge barrier drops and the extraction mechanism weakens. Estimated sunset: 15-25 years for movement literacy to become standard in general fitness education.
constraint_indexing:constraint_classification(multiplanar_capacity_preservation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination (multiplanar training does preserve capacities that age-related and use-dependent atrophy would otherwise erode) and embedded extraction (the complexity barrier creates professional gatekeeping and excludes populations with limited time, money, or motor learning capacity). The constraint is not a natural law (training could be simpler) but also not pure extraction (the physiological benefit is real).
constraint_indexing:constraint_classification(multiplanar_capacity_preservation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multiplanar_capacity_preservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(multiplanar_capacity_preservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multiplanar_capacity_preservation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(multiplanar_capacity_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(multiplanar_capacity_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint creates genuine coordination value (multiplanar training does preserve capacities that sagittal-only training loses), but the delivery mechanism embeds significant extraction. The complexity barrier excludes populations with limited time, money, or motor learning capacity. The fitness industry has layered commercial complexity (exotic exercises, specialized equipment, certification hierarchies) onto what could be a simpler intervention. The value reflects that roughly half the constraint's cost is necessary coordination overhead and half is extractive rent-seeking. Suppression (0.52): Moderate. Barriers include: unfamiliarity with non-sagittal movement patterns, gym equipment designed for sagittal-plane work, lack of accessible programming resources, injury risk narrative creating fear of self-direction, and professional gatekeeping through certification requirements. But suppression is not total — movement culture communities are building alternative pathways, and some self-directed trainers successfully navigate the complexity. Theater ratio (0.38): Moderate. Some multiplanar programming is genuinely functional (addressing real use-dependent atrophy), but some is performative (exotic exercises that don't meaningfully differ from simpler alternatives, equipment dependency that isn't physiologically necessary, complexity for complexity's sake). The theater has increased as the fitness industry has commercialized the genuine coordination insight.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic tangled rope perspectival structure. The fitness specialist sees pure coordination (Rope) — they are solving the legitimate problem of preserving functional capacity across the lifespan through expert programming. The time-constrained exerciser sees pure extraction (Snare) — a new requirement layered onto what used to be simple (just go to the gym and move). The self-directed trainer sees the hybrid (Tangled Rope) — genuine benefit but at significant cost. The movement culture community sees a temporary problem with a sunset (Scaffold) — they are building ecosystems where the knowledge barrier dissolves. The analytical observer confirms the tangled rope classification: both coordination and extraction are structurally present, and the perspectival gap is not a measurement error but a reflection of different structural positions relative to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reflects asymmetric knowledge and resource distribution. Fitness industry specialists are primary beneficiaries — they possess the specialized knowledge and can monetize it through training services, certifications, and equipment sales. The general population are victims — they face the choice between accepting capacity erosion or paying the complexity tax. The physical therapy profession benefits through both preventive and corrective service niches. Movement culture communities have organized to build alternative pathways that reduce the extraction (making multiplanar capacity the default rather than a specialized add-on). The analytical observer sees the hybrid structure: genuine coordination layered with extractive complexity. The directionality values are derived from these structural positions — beneficiaries with arbitrage options experience low effective extraction, trapped victims with no exit bear maximum extraction, constrained agents experience moderate extraction alongside genuine benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that multiplanar capacity preservation is neither pure coordination (Rope) nor pure extraction (Snare) but a genuine hybrid (Tangled Rope). The coordination function is real: age-related and use-dependent atrophy do erode rotational power and lateral stability, and deliberate multiplanar training does preserve these capacities. The extraction is also real: the fitness industry has layered complexity barriers, equipment requirements, and professional gatekeeping onto what could be a simpler intervention. The mandatrophy question 'Is this necessary complexity or extractive overhead?' is resolved by: both. The minimal effective dose is likely lower than current specialist programming suggests (omega variable 1), and some equipment dependency is commercial rather than physiological (omega variable 3), but the core insight (deliberate plane variation preserves capacity) is valid. The constraint is tangled rope because it cannot be cleanly separated into a coordination component and an extraction component — they are structurally intertwined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimal_effective_dose_threshold,
    'What is the minimum multiplanar training volume required to preserve functional capacity, and does current specialist programming exceed this threshold?',
    'Dose-response studies comparing minimal multiplanar protocols (e.g., 10 minutes 2x/week of lateral/rotational work) vs comprehensive programs (60+ minutes 3x/week). Longitudinal tracking of fall risk, rotational power, and lateral stability across dosing levels.',
    'If minimal dose is low: much of the complexity is extractive overhead, not physiological necessity. If minimal dose is high: the coordination function justifies the complexity, and the constraint is less extractive than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimal_effective_dose_threshold, empirical, 'Minimal effective dose for multiplanar capacity preservation').

omega_variable(
    self_directed_injury_risk,
    'Does self-directed multiplanar training carry significantly higher injury risk than specialist-supervised programming, or is the risk narrative a professional gatekeeping mechanism?',
    'Comparative injury rates: self-programmed multiplanar training vs specialist-supervised vs traditional sagittal-only training. Control for training age, load progression, and movement complexity.',
    'If injury risk is genuinely elevated: specialist supervision is a coordination service, not extraction. If injury risk is comparable: the safety narrative is a cover story for professional rent-seeking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_directed_injury_risk, empirical, 'Whether injury risk justifies specialist supervision requirement').

omega_variable(
    equipment_dependency_necessity,
    'Is specialized equipment (cable machines, landmines, suspension trainers) necessary for effective multiplanar training, or can bodyweight and simple implements achieve equivalent outcomes?',
    'Outcome comparison: equipment-based multiplanar programs vs bodyweight/minimal-equipment programs matched for plane distribution and intensity. Measure rotational power, lateral stability, and functional capacity outcomes.',
    'If equipment is necessary: gym access barrier is structural, not extractive. If bodyweight is sufficient: equipment dependency is a commercial extraction mechanism layered onto the genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equipment_dependency_necessity, empirical, 'Whether specialized equipment is necessary for multiplanar training outcomes').

omega_variable(
    cultural_movement_literacy_baseline,
    'Do populations with high baseline movement literacy (dancers, martial artists, traditional games cultures) show lower age-related capacity erosion without deliberate multiplanar programming?',
    'Cross-cultural comparison of rotational power, lateral stability, and fall risk in aging populations with different movement culture backgrounds. Control for overall activity level and strength training exposure.',
    'If movement-literate populations show preserved capacity: the constraint is partly a product of Western sagittal-dominant training culture, not a universal physiological requirement. If all populations show similar erosion: the constraint is more fundamental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_movement_literacy_baseline, empirical, 'Whether cultural movement literacy provides multiplanar preservation without deliberate programming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multiplanar_capacity_preservation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcp_tr_t0, multiplanar_capacity_preservation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mcp_tr_t5, multiplanar_capacity_preservation, theater_ratio, 5, 0.32).
narrative_ontology:measurement(mcp_tr_t10, multiplanar_capacity_preservation, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(mcp_be_t0, multiplanar_capacity_preservation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mcp_be_t5, multiplanar_capacity_preservation, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(mcp_be_t10, multiplanar_capacity_preservation, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(multiplanar_capacity_preservation, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of two structurally distinct upstream constraints: age_related_capacity_erosion (mountain — physiological decline is a natural law) and training_volume_dose_response (rope — coordination mechanism for matching training stimulus to adaptation). The multiplanar constraint has its own extractiveness value (0.48) reflecting the complexity barrier and professional gatekeeping layered onto the genuine coordination function. The upstream constraints have different epsilon values reflecting their distinct structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
