% ============================================================================
% CONSTRAINT STORY: average_is_over_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_average_is_over_2026, []).

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
 * * constraint_id: average_is_over_2026
 * human_readable: The AI-Talent Barbell Economy
 * domain: economic/technological
 * * SUMMARY:
 * As of 2026, the economy increasingly rewards a combination of natural talent
 * and "internal pressure" to work with intelligent machines.
 * Models like the Alpha School utilize AI-first tutoring to bypass traditional
 * classroom structures, creating a "Rope" for high achievers while
 * hollowing out the middle-skill class. This creates a bifurcated "barbell"
 * economy with high rewards at the top and a squeezed middle.
 * * KEY AGENTS:
 * - Middle-Skill Workers / Traditional Students: Subject (Powerless)
 * - AI-Augmented High Achievers / Alpha School: Beneficiary (Institutional)
 * - Economic Analysts (Arnold Kling/Tyler Cowen): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.68). The "Barbell Economy" extracts wealth and status
% from those whose skills compete directly with machines, concentrating it
% among those whose skills are complementary.
domain_priors:base_extractiveness(average_is_over_2026, 0.68).

% Suppression is moderate-high (0.75). The requirement for extreme
% conscientiousness and adaptability suppresses those without high internal pressure
% or access to elite training, limiting their alternatives.
domain_priors:suppression_score(average_is_over_2026, 0.75).

% Theater ratio is moderate (0.42). While the "outstanding" mindset is
% functional, the performance of "keeping up" by the middle class is often
% theater masking economic stagnation.
domain_priors:theater_ratio(average_is_over_2026, 0.42).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(average_is_over_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(average_is_over_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(average_is_over_2026, theater_ratio, 0.42).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a pure meritocratic coordination mechanism.
narrative_ontology:constraint_claim(average_is_over_2026, tangled_rope).
narrative_ontology:human_readable(average_is_over_2026, "The AI-Talent Barbell Economy").
narrative_ontology:topic_domain(average_is_over_2026, "economic/technological").

% Binary flags
% The market dynamics and credentialing systems that enforce this barbell
% structure require continuous reinforcement.
domain_priors:requires_active_enforcement(average_is_over_2026).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(average_is_over_2026, ai_complementary_workforce).
narrative_ontology:constraint_victim(average_is_over_2026, routine_cognitive_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISPLACED WORKER (SNARE)
% For those competing with algorithms, the economy is a Snare: a trap of
% declining wages and hollowing middle-class opportunities.
constraint_indexing:constraint_classification(average_is_over_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ALPHA STUDENT (ROPE)
% High achievers view AI-tutors and internal pressure as a Rope: essential
% tools to accomplish far more than was previously possible.
constraint_indexing:constraint_classification(average_is_over_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analysts see a system with both a genuine coordination function (for the
% elite) and severe asymmetric extraction (from the middle). This hybrid
% nature, requiring active enforcement, defines a Tangled Rope.
constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(average_is_over_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(average_is_over_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(average_is_over_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_observer_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(average_is_over_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(average_is_over_2026, E),
    E >= 0.46.

:- end_tests(average_is_over_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.68) is driven by "technological complementarity"—
 * where those who cannot leverage AI face a "Sovereignty Gap" in income
 * and status. The Alpha School's model acts as a functional Rope for those
 * with high conscientiousness, creating a stark perspectival gap with those
 * displaced by the same technology, who experience it as a Snare.
 * The analytical classification is Tangled Rope because the system is not
 * purely extractive; it has a genuine, powerful coordination function for
 * its beneficiaries, which coexists with its extractive function. This
 * resolves the LOW_THEATER_RATIO error by correctly identifying the system
 * as functional but asymmetric, rather than non-functional (Piton).
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system correctly identifies this as a Tangled Rope, recognizing both the
 * genuine coordination function for AI-augmented workers (the Rope aspect) and
 * the severe asymmetric extraction from the routine cognitive labor class (the
 * Snare aspect). This prevents misclassifying a hyper-functional, albeit
 * polarizing, economic system as purely extractive or purely coordinative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_conscientiousness_scaling,
    'Can "internal pressure" and adaptability be taught at scale, or are they non-modifiable traits?',
    'Longitudinal studies of graduates from alternative education models (e.g., Alpha School) vs. traditional cohorts.',
    'If teachable, the constraint could evolve into a Scaffold. If not, it solidifies as a permanent demographic Snare for a large population segment.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(average_is_over_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the drift from the "Middle Class Era" to the "Barbell Economy."
% Theater ratio rises as "average" success becomes performative rather than functional.
narrative_ontology:measurement(avg_tr_t0, average_is_over_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(avg_tr_t5, average_is_over_2026, theater_ratio, 5, 0.28).
narrative_ontology:measurement(avg_tr_t10, average_is_over_2026, theater_ratio, 10, 0.42).

% Extraction rises as high earners cluster and capture productivity gains.
narrative_ontology:measurement(avg_ex_t0, average_is_over_2026, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(avg_ex_t5, average_is_over_2026, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(avg_ex_t10, average_is_over_2026, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint allocates high-skill talent to high-leverage AI tools.
narrative_ontology:coordination_type(average_is_over_2026, resource_allocation).

% Network relationships (structural influence edges)
% The barbell economy directly influences the perceived value and function of
% traditional higher education.
narrative_ontology:affects_constraint(average_is_over_2026, university_credentialism_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */