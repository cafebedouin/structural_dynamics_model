% ============================================================================
% CONSTRAINT STORY: predator_prey_arms_race
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_predator_prey_arms_race, []).

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
 *   constraint_id: predator_prey_arms_race
 *   human_readable: Predator-Prey Arms Race Constraint
 *   domain: evolutionary_biology/ecology
 *
 * SUMMARY:
 *   The predator-prey arms race is a coevolutionary constraint where
 *   escalating predatory adaptations (speed, sensory acuity, lethal
 *   efficiency) drive corresponding escalations in prey defenses (escape
 *   velocity, detection mechanisms, protective morphology). This constraint
 *   operates across ecological scales—from individual organisms bearing the
 *   cost of weapon development to populations coordinating adaptive
 *   innovation to ecosystems managing the energy intensity of the arms race.
 *   The constraint is neither purely coordinative (it creates mutual
 *   entrapment) nor purely extractive (both parties benefit at population
 *   level from the selective dynamic). It exhibits tangled rope
 *   characteristics: genuine coordination function (population fitness
 *   improves through arms race selection) paired with asymmetric extraction
 *   (individuals bear costs of weapon development with diminishing returns).
 *   The theater ratio reflects that the mechanism is not performative—the
 *   arms race is a direct, material coercive dynamic, not an institutional
 *   ritual or narrative construct. Evolutionary biologists have historically
 *   viewed this constraint as a natural law (Mountain), but structured
 *   analysis reveals it as contingent on specific ecological configurations:
 *   niche overlap, reproductive timescales, energy availability, and absence
 *   of alternative equilibria.
 *
 * KEY AGENTS:
 *   - Individual Prey Organisms: Primary victims (powerless/trapped) — face escalating predator lethality with no exit; each generation must evolve faster/sharper/tougher or die
 *   - Individual Predator Organisms: Primary victims (powerless/trapped) — face escalating prey defenses with no exit; starvation pressure drives costlier hunting adaptations
 *   - Prey Populations: Organized beneficiary (organized/constrained) — benefit from selection for viable defensive traits; population fitness improves through arms race dynamic
 *   - Predator Populations: Organized beneficiary (organized/constrained) — benefit from selection for viable hunting traits; population fitness improves through arms race dynamic
 *   - Ecological System: Powerful beneficiary (powerful/mobile) — benefits from stable predator-prey cycles preventing population collapse; experiences extraction through energy intensification
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing a contingent dynamic as universal law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(predator_prey_arms_race, 0.55).
domain_priors:suppression_score(predator_prey_arms_race, 0.65).
domain_priors:theater_ratio(predator_prey_arms_race, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(predator_prey_arms_race, extractiveness, 0.55).
narrative_ontology:constraint_metric(predator_prey_arms_race, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(predator_prey_arms_race, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(predator_prey_arms_race, tangled_rope).
narrative_ontology:human_readable(predator_prey_arms_race, "Predator-Prey Arms Race Constraint").
narrative_ontology:topic_domain(predator_prey_arms_race, "evolutionary_biology/ecology").

domain_priors:requires_active_enforcement(predator_prey_arms_race).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(predator_prey_arms_race, predator_population).
narrative_ontology:constraint_beneficiary(predator_prey_arms_race, prey_population).
narrative_ontology:constraint_victim(predator_prey_arms_race, prey_individual_welfare).
narrative_ontology:constraint_victim(predator_prey_arms_race, ecosystem_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PREY (SNARE) — The prey organism faces escalating predator adaptations with no exit option. Each generation must evolve defensive traits or perish. The arms race is an extractive cycle with no off-ramp: faster predators require faster prey, sharper senses require sharper prey senses, all driven by survival pressure. Maximum suppression (trapped) and high experienced extraction.
constraint_indexing:constraint_classification(predator_prey_arms_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL PREDATOR (SNARE) — Symmetrically trapped. The predator must escalate hunting adaptations or starve as prey defenses improve. No exit from the evolutionary treadmill. Both predator and prey experience the constraint as pure coercion: invest in weaponry or be eliminated. High extraction from both branches of the arms race.
constraint_indexing:constraint_classification(predator_prey_arms_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PREDATOR POPULATION (TANGLED ROPE) — At the population level, the arms race coordinates predator adaptive evolution (genuine coordination function: hunting efficiency selects for viable predators) while extracting from individual predator organisms (generations of individuals invest in costly weapons with diminishing returns as prey defenses scale). The population benefits from the arms race dynamic; individuals suffer.
constraint_indexing:constraint_classification(predator_prey_arms_race, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PREY POPULATION (TANGLED ROPE) — Prey populations coordinate defensive innovation (genuine coordination function: survival pressure selects for viable prey), while extracting from individual prey organisms (generations of individuals invest in costly defenses). Population-level benefit masks individual-level suffering. Constrained exit: prey cannot leave the ecological niche without extinction.
constraint_indexing:constraint_classification(predator_prey_arms_race, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ECOLOGICAL SYSTEM (TANGLED ROPE) — The ecosystem benefits from the arms race (genuine coordination: predator-prey dynamics stabilize population cycles, preventing overgrazed collapse). But the constraint also extracts from system stability through resource intensification: escalating weaponry requires increased energy flows, metabolic costs accumulate, and the system becomes fragile to perturbation. The system has some mobility (alternative equilibria exist) but is constrained by evolutionary path-dependency.
constraint_indexing:constraint_classification(predator_prey_arms_race, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DARWINIAN VIEW (MOUNTAIN) — From a universal civilizational view, the predator-prey arms race appears as an immutable law of natural selection. Any predator-prey system must generate selective pressure for escalating adaptations—this is inherent to differential reproduction. The constraint emerges naturally from reproductive fitness logic with no exit option visible. However, the structured data contradicts the mountain classification: the arms race is contingent on specific ecological configurations, not universal. Alternative equilibria exist (coevolutionary stasis, niche separation). The mountain perspective risks naturalizing a contingent dynamic.
constraint_indexing:constraint_classification(predator_prey_arms_race, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(predator_prey_arms_race_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(predator_prey_arms_race, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(predator_prey_arms_race, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(predator_prey_arms_race, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(predator_prey_arms_race_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The arms race imposes real metabolic and reproductive costs on individuals—weapon development requires energy, time, and genetic allocation that cannot be invested in reproduction. The cost escalates over evolutionary time as each generation must outpace the previous adaptive step. However, extractiveness is not extreme (0.70+) because the constraint does provide genuine selective benefit at the population level: prey that fail to evolve defenses are eliminated, predators that fail to evolve hunting capability starve. Both parties survive through the arms race dynamic, suggesting some net benefit despite individual suffering. Suppression (0.65): High. Exit options are severely limited. Individual organisms cannot opt out of the evolutionary arms race—their fitness is directly tied to weapon capability. Populations cannot partition the niche without behavioral or morphological change that itself requires investment. The constraint is enforced by immediate reproductive/survival pressure: organisms that fail to compete are removed from the population. However, suppression is not absolute (0.85+) because alternative equilibria theoretically exist: niche separation, coevolutionary stasis, mutually-assured predation. The fact that these alternatives are rarely achieved suggests they are unstable rather than impossible. Theater ratio (0.35): Low. This is a material, non-performative constraint. The arms race mechanism is direct: predators catch prey or starve, prey evade predators or die. There is no narrative substitution (as in piton), no institutional ritual (as in tangled rope enforcement mechanisms), no aspirational signal (as in scaffold). The mechanism is transparent and immediate. The theater ratio has increased over evolutionary time (from 0.15 to 0.35) not because the mechanism has become performative, but because secondary mechanisms have emerged: behavioral innovation, signaling displays, population-level density effects, and ecosystem-mediated indirect effects. These add complexity without replacing the core material dynamic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival split between individual and population levels. Individual prey and predators see pure extraction (Snare)—they are trapped on an evolutionary treadmill with no exit. Predator and prey populations see tangled rope—the arms race coordinates adaptive evolution (genuine benefit) while extracting from individuals. The ecological system sees tangled rope at a larger scale—population-level stability benefits are paired with resource intensification extraction. The analytical observer risks seeing mountain—naturalizing the constraint as inevitable outcome of natural selection—but the structural data reveals this as a false summit. Real predator-prey systems have achieved coevolutionary stasis, niche separation, and alternative equilibria. The universality of arms races is contingent, not necessary. The perspectival gap reveals a critical insight: naturalization of contingent dynamics as universal laws is itself an extraction mechanism. It justifies individual suffering as inevitable and prevents innovation seeking alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is complex in this constraint because both predator and prey are simultaneously beneficiaries (at population level) and victims (at individual level). The constraint's structure creates role asymmetry despite functional symmetry. From the perspective of individual organisms: both are trapped targets (d ≈ 0.95 for both), experiencing high extraction. From the perspective of populations: both are beneficiaries of the selective dynamic (d ≈ 0.20 for both), experiencing low extraction because population fitness improves through arms race. From the ecosystem perspective: predators and prey are jointly coordinating stable dynamics (d ≈ 0.45, symmetric), but the ecological system bears extraction costs through energy intensification (d ≈ 0.75 for ecosystem stability as victim). The no canonical power atom captures this multi-level complexity. The constraint demonstrates why single-perspective analysis fails: the same organism is both beneficiary and victim depending on which timeframe and organizational level is analyzed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the six types are not competing answers but valid descriptions at different analytical levels. The constraint is not 'really' a Mountain being falsely called Tangled Rope—it is Mountain at the universal civilizational level if we condition on 'predator-prey systems with complete ecological overlap.' It is Snare at the individual organism level. It is Tangled Rope at the population level. It is Mountain-adjacent (almost naturally immutable) but not actually Mountain because genuine alternative equilibria exist. The mandatrophy resolution is: specify the level of analysis explicitly. The constraint cannot be classified without declaring whether we are analyzing individual organisms, populations, ecosystems, or hypothetical alternative configurations. Once the level is specified, a unique classification follows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arms_race_exit_pathway,
    'Do coevolutionary stasis, niche partitioning, or mutually-assured predation create genuine exits from the arms race escalation, or are these merely temporary pauses before renewed escalation?',
    'Long-term paleontological and ecological data on predator-prey systems that achieved stable equilibrium vs those that show continuous escalation; identification of cases where arms race halted without extinction',
    'If exits exist: constraint is Tangled Rope from all perspectives (constrained rather than trapped). If no exits: constraint approaches Mountain (trapped at all levels). Classification sensitivity is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_race_exit_pathway, empirical, 'Whether predator-prey arms races have genuine exit pathways or equilibrium states').

omega_variable(
    extraction_mechanism_identity,
    'Is the extraction in the arms race the *actual genetic/metabolic cost* of weapon escalation, or is it the *opportunity cost* of energy not invested in reproduction or survival of non-competitive individuals?',
    'Comparative energetics analysis: measure actual metabolic burden of evolved defensive/offensive traits vs baseline phenotype; distinguish between absolute cost and relative fitness cost',
    'If extraction is absolute cost: suppression ≥ 0.70 (Mountain-adjacent). If extraction is relative fitness: suppression ≈ 0.55 (Tangled Rope confirmed). This determines whether individuals are genuinely trapped or constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_identity, empirical, 'Whether arms race cost is absolute metabolic burden or relative fitness trade-off').

omega_variable(
    predator_prey_asymmetry,
    'Is the arms race symmetric (predator gains = prey loss, zero-sum) or asymmetric (predator gains more energy from successful hunt than prey loses from evolutionary defense investment)?',
    'Energy accounting: compare caloric cost of predator adaptations to caloric benefit of successful predation vs caloric cost of prey defensive adaptations to probability of predator capture prevention',
    'If symmetric: extraction flows equally bidirectional. If asymmetric toward predators: predators are net beneficiaries (institutional/arbitrage in population view). If asymmetric toward prey: prey evolution imposes larger absolute costs. Directionality derivation depends on this asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predator_prey_asymmetry, empirical, 'Energy flow asymmetry in predator-prey arms race').

omega_variable(
    human_intervention_constraint_topology,
    'When humans intervene in arms races (introducing invasive predators, culling prey, hunting predators), does the intervention exit the constraint or create a new extractive constraint on top of the existing one?',
    'Case study analysis: invasive cane toads, rabbit myxomatosis control, trophy hunting systems, predator reintroduction programs; tracking of constraint topology pre/post-intervention',
    'If intervention exits: constraint is contingent, not universal. If intervention creates new extraction: humans become institutional beneficiaries (using prey as resource or predator control as tool) while populations bear cost. Scaffold perspective with sunset may be analytically relevant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_intervention_constraint_topology, empirical, 'Whether human intervention can exit or only restructure predator-prey arms races').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(predator_prey_arms_race, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pparms_tr_t0, predator_prey_arms_race, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pparms_tr_t5, predator_prey_arms_race, theater_ratio, 5, 0.28).
narrative_ontology:measurement(pparms_tr_t10, predator_prey_arms_race, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(pparms_be_t0, predator_prey_arms_race, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pparms_be_t5, predator_prey_arms_race, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pparms_be_t10, predator_prey_arms_race, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(predator_prey_arms_race, resource_allocation).
narrative_ontology:affects_constraint(predator_prey_arms_race, evolutionary_fitness_treadmill).
narrative_ontology:affects_constraint(predator_prey_arms_race, ecological_stability_energy_costs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(predator_prey_arms_race, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
