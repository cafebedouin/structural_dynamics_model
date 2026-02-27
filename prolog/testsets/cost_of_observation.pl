% ============================================================================
% CONSTRAINT STORY: cost_of_observation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cost_of_observation, []).

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
 *   constraint_id: cost_of_observation
 *   human_readable: The Evolutionary Cost of Observation
 *   domain: philosophical/evolutionary
 *
 * SUMMARY:
 *   The cost of observation is a meta-constraint describing the irreducible
 *   tax paid by any situated observer — biological, cognitive, or physical.
 *   It operates across multiple registers: metabolic (energy consumption in
 *   sensory apparatus), evolutionary (trade-offs between observation
 *   investment and reproduction), ecological (predation vulnerability during
 *   vigilance), and thermodynamic (information processing as energy
 *   dissipation). The constraint is not purely natural law (thermodynamics
 *   provides floor, not the full story) nor purely institutional (no human
 *   artifact created it, though technology is reshaping its distribution).
 *   Instead, it manifests as a tangled hybrid: organisms depend on
 *   observation to survive (coordination benefit), yet observation depletes
 *   finite resources (extraction cost). The constraint's theater ratio has
 *   increased over evolutionary time as consciousness-reporting layers
 *   accumulated on top of actual sensory processing, and as technological
 *   systems increasingly decouple the phenomenological experience of
 *   observation from its underlying costs.
 *
 * KEY AGENTS:
 *   - Metabolic Budget Holder: Primary victim (powerless/trapped) — energy is finite, sensory apparatus consumes it, no escape
 *   - Prey Species: Primary victim/secondary beneficiary (organized/constrained) — must coordinate observation to avoid predation but each individual pays energy costs
 *   - Evolutionary Lineage: Primary beneficiary (institutional/arbitrage) — accumulated observation investment produced adaptive advantage
 *   - Developing Organism: Secondary victim (moderate/mobile) — observation apparatus cost is front-loaded in development but declines as apparatus matures
 *   - Phenomenal Consciousness: Theatrical observer (powerless/trapped) — subjective experience of effort may be performative layer on actual processing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — thermodynamic physics establishes universal floor on observation cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cost_of_observation, 0.35).
domain_priors:suppression_score(cost_of_observation, 0.42).
domain_priors:theater_ratio(cost_of_observation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cost_of_observation, extractiveness, 0.35).
narrative_ontology:constraint_metric(cost_of_observation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cost_of_observation, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cost_of_observation, tangled_rope).
narrative_ontology:human_readable(cost_of_observation, "The Evolutionary Cost of Observation").
narrative_ontology:topic_domain(cost_of_observation, "philosophical/evolutionary").

domain_priors:requires_active_enforcement(cost_of_observation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cost_of_observation, adaptive_organisms).
narrative_ontology:constraint_beneficiary(cost_of_observation, information_processors).
narrative_ontology:constraint_victim(cost_of_observation, metabolic_budget).
narrative_ontology:constraint_victim(cost_of_observation, predation_vulnerability).
narrative_ontology:constraint_victim(cost_of_observation, reproductive_opportunity_cost).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: METABOLIC BUDGET (SNARE) — The organism cannot opt out of observation costs. Sensory apparatus, neural processing, and attention mechanisms consume calories, oxygen, and developmental resources with no escape. The metabolic burden is locked-in: eyes evolved, therefore eyes must be fed. Maximum experienced extraction from an energy-constrained system.
constraint_indexing:constraint_classification(cost_of_observation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PREY SPECIES (TANGLED ROPE) — Must observe predators (coordination benefit: avoid death) but observation is costly (predator detection, focus withdrawal, metabolic drain). Benefits from vigilance coordination with herd members but constrained by individual metabolic trade-offs. Organized at species level yet each individual pays extraction; the coordination mechanism (early warning systems) is inseparable from the extraction mechanism (energy costs).
constraint_indexing:constraint_classification(cost_of_observation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUCCESSFUL EVOLUTIONARY LINEAGE (ROPE) — From the million-year view, observation costs are pure coordination: organisms that developed better sensory integration survived, reproduced, and set the framework for their descendants. The lineage benefits from accumulated observation investment. No sense of extraction — observation is the enabler of adaptive success itself.
constraint_indexing:constraint_classification(cost_of_observation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPING ORGANISM (SCAFFOLD) — Early development trades off sensory investment against growth and mobility. Young organisms often have constrained sensory systems (eyes closed at birth, hearing delayed, olfactory systems immature) — observation apparatus itself has a sunset clause. As development proceeds, sensory capacity ramps up and metabolic returns on observation improve. High theater early (incomplete sensory apparatus, parental protection reducing need for vigilance), declining over developmental trajectory.
constraint_indexing:constraint_classification(cost_of_observation, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: PHENOMENAL CONSCIOUSNESS (PITON) — Subjective awareness of observation costs (pain, fatigue, hunger, boredom, attention strain) is functionally important for early hominins but increasingly theater in technological contexts. We experience the felt burden of paying attention — this phenomenological 'watching' is performative, a reporting layer on actual information processing. The constraint persists (you still pay energy costs) but the conscious experience of observation exhaustion becomes inertial as external technologies (glasses, hearing aids, smartphones) bypass the evolved apparatus.
constraint_indexing:constraint_classification(cost_of_observation, piton,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a thermodynamic standpoint, observation is irreducibly costly: any system that detects and processes information about its environment must dissipate energy. This is not a property of biology but of information itself. Shannon entropy, Landauer's principle, and the second law make observation a fundamental tax, not a contingent institutional arrangement. The constraint emerges from physics, not from evolutionary happenstance.
constraint_indexing:constraint_classification(cost_of_observation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cost_of_observation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cost_of_observation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cost_of_observation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cost_of_observation, TR),
    TR >= 0.70.

:- end_tests(cost_of_observation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts metabolic resources from organisms but not maximally — many organisms have found niches where observation benefits exceed costs, suggesting extraction is context-dependent rather than absolute. The value reflects that observation is sometimes beneficial and sometimes extractive depending on ecological context. Suppression (0.42): Moderate. While organisms cannot opt out of observation entirely (sensory apparatus is evolved), they can modulate vigilance behavior, invest selectively in different sensory modalities, and develop energy-efficient perception strategies. Some niche differentiation exists (blind cave fish, bacteria), so suppression is real but not total. Theater ratio (0.38): Moderate. Consciousness-reporting systems add performative layer to actual sensory processing. Humans experience observation as effortful and meaningful, even when the actual computational cost is minimal. Technological systems have further decoupled the felt cost (which remains) from actual energy consumption (which can be externalized).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (evolutionary lineage) sees pure coordination—observation as the mechanism of adaptation. The victim (metabolic budget) sees pure extraction—observation as a drain on finite resources. The prey species sees hybrid constraint—observation solves the predation problem but at energy cost. The developing organism sees sunset constraint—observation apparatus matures over developmental time. The consciousness system sees inertial theater—the subjective experience of effort persists even as technology externalizes actual costs. The analytical observer risks a false summit—treating observation cost as a law of thermodynamics when much of the constraint is evolutionary contingency (sensory apparatus design, consciousness reporting) or institutional (research requiring attention, information-economy valuing observer labor). The gap is maximal: the same structural phenomenon appears as necessity, extraction, coordination, development, theater, and false law depending on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation: Metabolic budget holders are victims (trapped exit, powerless position) → d ≈ 0.95 → high f(d) → high experienced extraction. Prey species are organized victims with some constrained exit (can flee, can join herds) → d ≈ 0.55 → moderate f(d) → moderate extraction. Evolutionary lineages are beneficiaries with arbitrage options (can speciate, adapt, abandon lineages) → d ≈ 0.05 → negative f(d) → negative/zero extraction. Developing organisms are victims but mobile (can transition developmental stages) → d ≈ 0.40 → low-moderate f(d). Phenomenal consciousness is theater-constrained → d derived from trapped + moderate power → d ≈ 0.70. Analytical observer is observation-independent (can model without experiencing) → d ≈ 0.72 → mild f(d). The presheaf over these perspectives reveals a coordinatedly distributed extraction: no single agent bears the full cost, but every agent pays something.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY: The apparent contradiction between 'observation is necessary for survival' (mountain/rope) and 'observation is extractive' (snare/tangled_rope) is resolved by recognizing that these are legitimate perspectival readings from different structural positions. From the organism's energy budget perspective, observation is extraction (Snare). From the evolutionary lineage's perspective, observation is the coordination mechanism of adaptation (Rope). From the species-level predator-avoidance perspective, observation is both: coordination (herd vigilance) coupled with individual extraction (energy cost). The mandatrophy is not 'which type is correct?' but 'at which scale is the constraint primarily coordination vs extraction?' At the metabolic/individual scale, extraction dominates. At the evolutionary/population scale, coordination dominates. Both are real. The Tangled Rope classification (the middle ground) correctly identifies that the constraint cannot be fully described as either pure coordination or pure extraction—it is structurally a hybrid where the coordination benefit (surviving predation) is inseparable from the extraction mechanism (metabolic cost). The increasing theater ratio reflects that consciousness-reporting systems have made the subjective experience of observation cost more salient than the actual energetic cost, introducing a performance layer (Piton) that can be managed independently of the underlying constraint. Technological scaffolding (glasses, hearing aids, prosthetics, AI assistants) creates a sunset pathway where humans gradually externalize observation costs to technological systems, reducing the experienced extraction over generational timescales (Scaffold perspective). The thermodynamic mountain perspective is partially valid (information processing does have a physical cost floor) but naturalizes what is also a contingent evolutionary choice (the particular design of human sensory apparatus) as a universal law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observation_necessity_threshold,
    'Below what environmental complexity threshold can an organism survive with zero sensory observation apparatus?',
    'Comparative analysis of organisms across gradient of sensory completeness (blind cave fish, bacteria, single-celled motile vs non-motile organisms). Measurement of survival/reproduction rates at each complexity level.',
    'If threshold = zero: observation is universally necessary (Mountain). If threshold > zero: some organisms genuinely escape extraction (reframes victims group).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observation_necessity_threshold, empirical, 'Threshold complexity below which observation is optional').

omega_variable(
    metabolic_return_on_observation_investment,
    'What is the net energy ROI of sensory investment across different ecological niches?',
    'Comparative bioenergetics: energy cost of sensory apparatus vs improved foraging/predator avoidance efficiency. Calculate per-organism lifetime fitness delta with vs without each sensory modality.',
    'If ROI uniformly positive: observation is always beneficial (Rope from all perspectives). If ROI highly variable or negative in some niches: extraction mechanism is real and context-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metabolic_return_on_observation_investment, empirical, 'Energy ROI of sensory apparatus across ecologies').

omega_variable(
    technological_bypass_completeness,
    'Can technological extensions (prosthetics, AI systems, digital interfaces) fully replace evolved sensory apparatus without replicating its energetic costs?',
    'Comparison of energy cost profiles: evolved sensory systems vs equivalent technological systems (e.g., human vision ~2-3W vs machine vision implementations). Analysis of whether technology moves the cost to a different agent (energy grid, manufacturer).',
    'If replaceable and cheaper: technological scaffold perspective is real and near-term sunset is possible. If costs are merely transferred: constraint persists in network form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_bypass_completeness, empirical, 'Whether technology can bypass evolved observation costs').

omega_variable(
    consciousness_theater_separation,
    'Is subjective experience of observation costs (fatigue, attention strain, phenomenal effort) functionally separable from the actual metabolic costs of sensory processing?',
    'Neuroscience of phenomenal consciousness vs information processing load. Study dissociations: cases where subjective effort diverges from actual processing cost (inattentional blindness, flow states, meditation, anesthesia).',
    'If fully separable: consciousness layer is pure piton theater, maintainable through technological distraction. If coupled: theater cannot be escaped without changing underlying biology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consciousness_theater_separation, conceptual, 'Whether conscious effort is separable from actual information processing cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cost_of_observation, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(obs_cost_tr_t0, cost_of_observation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(obs_cost_tr_t100, cost_of_observation, theater_ratio, 100, 0.3).
narrative_ontology:measurement(obs_cost_tr_t1000, cost_of_observation, theater_ratio, 1000, 0.38).

% Extraction over time
narrative_ontology:measurement(obs_cost_be_t0, cost_of_observation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(obs_cost_be_t100, cost_of_observation, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(obs_cost_be_t1000, cost_of_observation, base_extractiveness, 1000, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cost_of_observation, resource_allocation).
narrative_ontology:affects_constraint(cost_of_observation, attention_allocation_problem).
narrative_ontology:affects_constraint(cost_of_observation, predator_prey_arms_race).
narrative_ontology:affects_constraint(cost_of_observation, consciousness_binding_problem).

% DUAL FORMULATION NOTE:
% The cost of observation decomposes into at least three structurally distinct sub-constraints: (1) metabolic energy allocation (individual-level extraction), (2) predator-avoidance coordination (population-level coordination), (3) consciousness-reporting overhead (institutional/cultural theater). Each can be modeled separately with different epsilon values, but they are coupled through the observation apparatus itself. This story captures the hybrid at the meta level; separate stories for metabolic_observation_extraction (ε~0.60, Snare), predator_detection_coordination (ε~0.25, Rope), and consciousness_performance_theater (ε~0.55, Piton) would provide finer structural resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
