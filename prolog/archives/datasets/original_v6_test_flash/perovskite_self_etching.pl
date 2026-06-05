% ============================================================================
% CONSTRAINT STORY: perovskite_self_etching
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perovskite_self_etching, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perovskite_self_etching
 *   human_readable: The 2D Perovskite Machinability Constraint
 *   domain: technological/semiconductors
 *
 * SUMMARY:
 *   This constraint models the dominance of traditional, high-cost, and
 *   destructive lithography techniques when applied to soft lead halide
 *   perovskites. This limits the range of device architectures that can be
 *   manufactured. The high cost is the barrier that limits adoption by
 *   startups. Established lithography facilities are constrained by existing
 *   infrastructure.
 *
 * KEY AGENTS:
 *   - Lithography Equipment Manufacturers: Beneficiaries (institutional/arbitrage) - Benefit from ongoing demand and new innovations to support perovskites.
 *   - Established Fabrication Facilities: Constrained users (moderate/constrained) - Benefit from existing tools, but limited resolution and throughput slows development.
 *   - Perovskite Startups: Primary victim (powerless/trapped) - Trapped by high costs and limited throughput of existing solutions.
 *   - Emerging Applications: Victim (abstract/trapped) - the applications are trapped by the cost and resolution limitations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perovskite_self_etching, 0.6).
domain_priors:suppression_score(perovskite_self_etching, 0.7).
domain_priors:theater_ratio(perovskite_self_etching, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perovskite_self_etching, extractiveness, 0.6).
narrative_ontology:constraint_metric(perovskite_self_etching, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(perovskite_self_etching, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perovskite_self_etching, tangled_rope).
narrative_ontology:human_readable(perovskite_self_etching, "The 2D Perovskite Machinability Constraint").
narrative_ontology:topic_domain(perovskite_self_etching, "technological/semiconductors").

domain_priors:requires_active_enforcement(perovskite_self_etching).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perovskite_self_etching, lithography_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(perovskite_self_etching, established_fabrication_facilities).
narrative_ontology:constraint_victim(perovskite_self_etching, perovskite_startups).
narrative_ontology:constraint_victim(perovskite_self_etching, emerging_applications).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perovskite startups are trapped by the high cost and limited resolution of existing lithography techniques, hindering their ability to develop and scale novel device architectures.
constraint_indexing:constraint_classification(perovskite_self_etching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Established fabrication facilities benefit from existing lithography infrastructure but are constrained by its limitations when adapting it for perovskites. There is coordination in that they can continue to use their tools, but extraction in that they can't achieve the same level of resolution or throughput.
constraint_indexing:constraint_classification(perovskite_self_etching, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Lithography equipment manufacturers benefit from continued demand for their existing tools, but have the arbitrage option of developing novel lithography techniques specifically for perovskites. They experience it as a coordination mechanism
constraint_indexing:constraint_classification(perovskite_self_etching, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees a complex interplay between established infrastructure, emerging materials, and the limitations of current fabrication techniques. Extraction occurs, active enforcement maintains the dominance of existing approaches.
constraint_indexing:constraint_classification(perovskite_self_etching, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perovskite_self_etching_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perovskite_self_etching, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perovskite_self_etching, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perovskite_self_etching, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(perovskite_self_etching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.6) reflects the cost and throughput limitations of adapting existing lithography techniques, which limit the accessibility for emerging companies and novel perovskite device structures. Suppression of 0.7 captures the lack of alternatives due to materials instability to traditional dry etching processes. The theater ratio is low at 0.3 because the high cost of lithography is genuinely a limitation not merely for show.
 *
 * PERSPECTIVAL GAP:
 *   The lithography equipment manufacturers view the existing constraint as a coordination mechanism that allows for continued sales and innovation to support the novel material. While established fabs are constrained by existing machinery limitations, new startups are trapped without resolution to the cost. The analytical observer sees extraction happening and the market favoring a particular technique.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic is calculated using the exit options. The powerful agents are the big lithography companies who have arbitrage. The victims are powerless because they are trapped by the current process. The fabrication facilities are constrained but are larger companies. The system maintains the extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_etching_viability,
    'Will self-aligned etching or other alternative methods become viable at scale?',
    'Demonstration of high-resolution patterning and scalability using alternative etching techniques.',
    'If viable: constraint shifts towards rope, facilitating perovskite adoption. If not: snare persists, limiting perovskite applications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_etching_viability, empirical, 'Viability of alternative etching methods for perovskites.').

omega_variable(
    materials_stability_limits,
    'What are the fundamental materials stability limits hindering dry etching?',
    'Detailed studies on decomposition pathways during various dry-etch processes.',
    'If limits are surmountable: can lead to new etch chemistries, and better adoption. If fundamental limitation: require changes in patterning strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(materials_stability_limits, empirical, 'Material stability against dry-etch process in perovskites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perovskite_self_etching, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pero_tr_t0, perovskite_self_etching, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pero_tr_t5, perovskite_self_etching, theater_ratio, 5, 0.3).
narrative_ontology:measurement(pero_tr_t10, perovskite_self_etching, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(pero_be_t0, perovskite_self_etching, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(pero_be_t5, perovskite_self_etching, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(pero_be_t10, perovskite_self_etching, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(perovskite_self_etching, solar_cell_efficiency_limit).
narrative_ontology:affects_constraint(perovskite_self_etching, flexible_electronics_throughput).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
