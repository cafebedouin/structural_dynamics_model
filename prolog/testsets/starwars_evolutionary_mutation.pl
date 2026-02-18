% ============================================================================
% CONSTRAINT STORY: starwars_evolutionary_mutation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_starwars_evolutionary_mutation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: starwars_evolutionary_mutation
 * human_readable: Jedi as Systemic Evolutionary Outliers
 * domain: social/political/biological
 * * SUMMARY:
 * This constraint models the Jedi not as moral paragons, but as systemic
 * outliers—mutations that test which institutional "necessities" are genuine
 * physical constraints and which are merely bureaucratic artifacts.
 * The Jedi Council's embedding within the Senate bureaucracy created a
 * structural blindness, where the "Domestication Gradient" turned a beneficial
 * mutation into a rigid apparatus. What began as a Rope (Jedi as functional
 * coordination for peace) ossified, ultimately becoming a Snare as the
 * Council's inability to metabolize dissenting "mutations" (like Anakin)
 * extracted the entire order's survival in favor of institutional conformity.
 * * KEY AGENTS:
 * - Individual Jedi (e.g., Qui-Gon, Anakin): Subject (Powerless) - Possesses latent diagnostic capacity but is excluded or filtered by the institution.
 * - Jedi Council: Beneficiary (Institutional) - Rule-making power blinded by its own bureaucratic embedding.
 * - The Sith (e.g., Palpatine): Auditor (Analytical) - External force that applies the "fitness test" the Council failed to perform internally.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY] High-extraction identified as a structural Snare.
% The institution "extracted" the individual diagnostic potential of its
% members to feed its bureaucratic survival until total collapse.
domain_priors:base_extractiveness(starwars_evolutionary_mutation, 0.75).
% The Council actively suppressed boundary-testing and alternative diagnostic
% methods in favor of "conformity" filters.
domain_priors:suppression_score(starwars_evolutionary_mutation, 0.80).
% Bureaucratic ritual increased over time but was not the primary function.
domain_priors:theater_ratio(starwars_evolutionary_mutation, 0.15).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, extractiveness, 0.75).
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(starwars_evolutionary_mutation, theater_ratio, 0.15).

% The system presents itself as a Snare from an analytical view, extracting
% potential for the sake of conformity.
narrative_ontology:constraint_claim(starwars_evolutionary_mutation, snare).
narrative_ontology:human_readable(starwars_evolutionary_mutation, "Jedi as Systemic Evolutionary Outliers").
narrative_ontology:topic_domain(starwars_evolutionary_mutation, "social/political/biological").

% Binary flags
% The Jedi Code, Senate oversight, and the rigid master-apprentice training
% loop constitute active enforcement.
domain_priors:requires_active_enforcement(starwars_evolutionary_mutation).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(starwars_evolutionary_mutation, bureaucratic_inertia).
narrative_ontology:constraint_beneficiary(starwars_evolutionary_mutation, the_sith_plan).
narrative_ontology:constraint_victim(starwars_evolutionary_mutation, individual_diagnostic_ability).
narrative_ontology:constraint_victim(starwars_evolutionary_mutation, the_jedi_lineage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE ROGUE JEDI (QUI-GON/ANAKIN) - MOUNTAIN
% To a Jedi who sees a threat the Council ignores, the Council's denial is a
% Mountain. It is an unchangeable feature of the institutional landscape that
% no amount of evidence can shift because the Council has delegated its
% "diagnostic sensors" to the Senate.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE JEDI COUNCIL - ROPE
% The Council views its rules and Senate relationship as a Rope—a functional
% coordination mechanism that ensures the Jedi serve the "greater good" of the
% Republic. They view deviations as failures of discipline rather than vital signals.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SITH (PALPATINE) - SNARE
% From the Sith perspective, the Council's own "domestication" is a Snare.
% By tethering the Jedi to a decaying political system, they have turned the
% Order's own "Rope" of coordination into the mechanism of its execution.
constraint_indexing:constraint_classification(starwars_evolutionary_mutation, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(starwars_evolutionary_mutation_tests).

test(perspectival_gap_mountain_rope) :-
    % Verify the gap between the individual Jedi (Mountain) and the Council (Rope).
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_view_is_snare) :-
    % Verify the analytical observer correctly identifies the constraint as a Snare.
    constraint_indexing:constraint_classification(starwars_evolutionary_mutation, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeAnalytical == snare.

test(snare_thresholds_met) :-
    % High extraction and suppression confirm the Snare classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(starwars_evolutionary_mutation, ExtMetricName, E),
    narrative_ontology:constraint_metric(starwars_evolutionary_mutation, SuppMetricName, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(starwars_evolutionary_mutation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the "Domestication Gradient" theory: an initially beneficial
 * coordination mechanism (Rope) becomes rigid and extractive over time.
 * - Extraction (0.75): High, representing the squandered diagnostic potential of individual Jedi, which was "extracted" in favor of institutional conformity.
 * - Suppression (0.80): High, representing the Council's active filtering of dissenting or non-conformist views (e.g., Qui-Gon's, Anakin's).
 * The Perspectival Gap is stark: The Council sees a functional Rope, the individual Jedi sees an immovable Mountain of bureaucracy, and the external adversary (the Sith) sees a perfect Snare to exploit.
 * * MANDATROPHY ANALYSIS:
 * This is not a Tangled Rope because the coordination function had atrophied to the point of being counter-productive. The system's primary function became self-preservation through conformity, making it a pure Snare from an objective, analytical viewpoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_starwars_evolutionary_mutation,
    "Does Jedi training primarily select for obedience (Snare) or for boundary-testing diagnostic ability (adaptive Rope)?",
    "Analysis of Padawan failure rates versus their success in identifying external systemic threats that the Council missed.",
    "If Obedience: The Order is a terminal Snare. If Boundary-Testing: It was an adaptive Rope that merely failed a specific fitness test.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(starwars_evolutionary_mutation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the Jedi Order's ossification over time.
% Extraction (of potential) and theater (of ritual) both increased as the
% Order became more entangled with galactic bureaucracy.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(sw_evo_tr_t0, starwars_evolutionary_mutation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sw_evo_tr_t5, starwars_evolutionary_mutation, theater_ratio, 5, 0.10).
narrative_ontology:measurement(sw_evo_tr_t10, starwars_evolutionary_mutation, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(sw_evo_ex_t0, starwars_evolutionary_mutation, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(sw_evo_ex_t5, starwars_evolutionary_mutation, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(sw_evo_ex_t10, starwars_evolutionary_mutation, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The Jedi Code and Council oversight function as a mechanism to enforce a
% specific behavioral and ideological standard across the Order.
narrative_ontology:coordination_type(starwars_evolutionary_mutation, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */