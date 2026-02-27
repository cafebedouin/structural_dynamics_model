% ============================================================================
% CONSTRAINT STORY: carrying_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carrying_capacity, []).

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
 *   constraint_id: carrying_capacity
 *   human_readable: Management of Ecological Carrying Capacity (K)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   Ecological carrying capacity (K) is the maximum population and
 *   consumption an environment can sustain. While rooted in biophysical
 *   limits, its 'management' is a socio-economic constraint. The current
 *   global system operates in 'ecological overshoot,' liquidating natural
 *   capital for short-term economic growth. This creates a structural
 *   conflict between beneficiaries of this overshoot and victims who bear its
 *   long-term costs. This constraint story models this conflict, showing how
 *   the same set of facts about ecological limits can be interpreted as all
 *   six constraint types depending on the observer's structural position.
 *
 * KEY AGENTS:
 *   - The Biosphere/Non-Human Species: Primary victim (powerless/trapped) — bears the full cost of habitat destruction and resource depletion.
 *   - Industrialized Economies: Primary beneficiary (institutional/arbitrage) — leverages technology and global markets to exceed local K by importing resources and externalizing waste.
 *   - Climate-Vulnerable Nations: Secondary victim (moderate/constrained) — suffers disproportionately from the consequences of overshoot (e.g., sea-level rise) while being economically constrained.
 *   - The Techno-Optimist Coalition: Organized actors (organized/mobile) — believe technology can indefinitely expand K, viewing limits as temporary problems to be solved.
 *   - International Governance Bodies: Institutional actors (institutional/constrained) — charged with managing the global commons, but lack enforcement power, leading to performative rather than functional regulation.
 *   - The Malthusian Analyst: Analytical observer (analytical/analytical) — frames K as a simple, immutable natural law, ignoring the mediating role of technology, economics, and politics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carrying_capacity, 0.48).
domain_priors:suppression_score(carrying_capacity, 0.75).
domain_priors:theater_ratio(carrying_capacity, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carrying_capacity, extractiveness, 0.48).
narrative_ontology:constraint_metric(carrying_capacity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(carrying_capacity, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carrying_capacity, tangled_rope).
narrative_ontology:human_readable(carrying_capacity, "Management of Ecological Carrying Capacity (K)").
narrative_ontology:topic_domain(carrying_capacity, "economic/technological/social").

domain_priors:requires_active_enforcement(carrying_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carrying_capacity, industrialized_economies).
narrative_ontology:constraint_beneficiary(carrying_capacity, high_consumption_lifestyles).
narrative_ontology:constraint_victim(carrying_capacity, future_generations).
narrative_ontology:constraint_victim(carrying_capacity, non_human_species).
narrative_ontology:constraint_victim(carrying_capacity, climate_vulnerable_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE BIOSPHERE (SNARE) — Experiences ecological overshoot as pure, uncompensated extraction. It is trapped and has no agency to resist the depletion of its capital. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.82. This is a clear Snare.
constraint_indexing:constraint_classification(carrying_capacity, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDUSTRIALIZED ECONOMY (ROPE) — Experiences the constraint as a pure coordination problem: securing resources and externalizing costs. Benefits from technologies that appear to expand K. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07. Negative effective extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(carrying_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CLIMATE-VULNERABLE NATION (TANGLED ROPE) — Experiences both the coercive extraction of global overshoot (climate impacts, resource depletion) and the coordination benefits of the global economy it cannot easily exit. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.55. This falls squarely in the Tangled Rope range.
constraint_indexing:constraint_classification(carrying_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECHNO-OPTIMIST (SCAFFOLD) — Views current ecological limits as temporary engineering problems. The constraint is a scaffold driving innovation (e.g., fusion, carbon capture) that will eventually make the limit obsolete. This perspective assumes a technological sunset clause. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.23. Low effective extraction, meeting the Scaffold gate.
constraint_indexing:constraint_classification(carrying_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL GOVERNANCE (PITON) — The function of enforcing ecological limits has atrophied, but the performative aspects (conferences, non-binding treaties, reports) remain. The theater_ratio of 0.75 meets the Piton gate (≥0.70). The institution persists through inertia despite its inability to enforce the constraint.
constraint_indexing:constraint_classification(carrying_capacity, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MALTHUSIAN VIEW (MOUNTAIN) — Frames carrying capacity as an immutable, natural law of population dynamics, ignoring the socio-political structures that mediate it. The engine will flag this as a 'false summit' because the base properties (ε=0.48, suppression=0.75) violate the Mountain classification gates, revealing that this 'natural law' is in fact a contingent institutional arrangement.
constraint_indexing:constraint_classification(carrying_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carrying_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carrying_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carrying_capacity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carrying_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carrying_capacity, TR),
    TR >= 0.70.

:- end_tests(carrying_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.48): Represents the significant, but not total, rate at which natural capital is being converted into economic capital without replenishment. Suppression (0.75): High. The global economic system, predicated on growth, makes it extremely difficult for societies or individuals to adopt steady-state or degrowth models. Alternatives are structurally suppressed. Theater Ratio (0.75): High. International agreements (e.g., Paris Accord) and corporate sustainability initiatives are largely performative, lacking the enforcement mechanisms to alter the trajectory of overshoot, thus satisfying the Piton gate.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. For the biosphere, overshoot is a Snare. For the industrialized consumer, it's a Rope (a resource coordination problem). For a vulnerable nation, it's a Tangled Rope (caught between costs and benefits). For the tech sector, it's a Scaffold for future innovation. For international bodies, it's a Piton (a failed, inertial process). For a Malthusian analyst, it's a Mountain (a natural law). The base metrics support all these views, revealing that the 'problem of K' is fundamentally a problem of perspective and structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Industrialized Economies) with arbitrage exit see the system as a net benefit (negative χ), classifying it as a Rope. Victims (Biosphere) who are trapped experience maximal extraction (high positive χ), classifying it as a Snare. Agents with mixed roles and constrained exit options (Vulnerable Nations) experience moderate extraction, classifying it as a Tangled Rope. The system's logic correctly maps these structural positions to divergent classifications from a single set of base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating that 'carrying capacity' is not a single type of constraint. Labeling it solely as a Mountain (natural law) is a 'false summit' that obscures the extraction. Labeling it solely as a Snare ignores the genuine coordination functions and perceived benefits that sustain the system. The Deferential Realism framework correctly identifies it as a complex object whose classification is an indexical function of the observer, preventing the collapse into a single, inadequate label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_substitutability,
    'To what extent can technological capital substitute for natural capital?',
    'Long-term empirical studies on the efficacy and second-order effects of large-scale geo-engineering, closed-loop recycling, and synthetic biology.',
    'High substitutability validates the Scaffold perspective. Low substitutability confirms the Snare perspective for future generations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_substitutability, empirical, 'Whether technology can truly replace depleted natural resources and ecosystem services.').

omega_variable(
    system_collapse_tipping_point,
    'Where are the non-linear tipping points in the Earth system beyond which recovery is impossible on human timescales?',
    'Improved Earth system modeling and paleo-climatic data analysis.',
    'If tipping points are near and irreversible, the constraint is a hard Mountain. If they are distant or reversible, it behaves more like a manageable Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_collapse_tipping_point, empirical, 'The proximity and reversibility of biophysical tipping points.').

omega_variable(
    intergenerational_discount_rate,
    'What is the ethically appropriate discount rate to apply to the welfare of future generations?',
    'Philosophical and ethical consensus-building, not empirical measurement.',
    'A near-zero discount rate frames current consumption as extreme extraction (Snare). A high discount rate justifies it as optimal investment (Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'The ethical weight assigned to future welfare versus present consumption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carrying_capacity, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carr_tr_t1950, carrying_capacity, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(carr_tr_t1990, carrying_capacity, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(carr_tr_t2024, carrying_capacity, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(carr_be_t1950, carrying_capacity, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(carr_be_t1990, carrying_capacity, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(carr_be_t2024, carrying_capacity, base_extractiveness, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carrying_capacity, resource_allocation).
narrative_ontology:affects_constraint(carrying_capacity, global_food_security).
narrative_ontology:affects_constraint(carrying_capacity, climate_stability).
narrative_ontology:affects_constraint(carrying_capacity, geopolitical_conflict).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
