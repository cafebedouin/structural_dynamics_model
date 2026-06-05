% ============================================================================
% CONSTRAINT STORY: antifragility_u2_sed_r2
% ============================================================================
% Version: 3.4 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_sed_r2, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility_u2_sed_r2
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that increase in capability, resilience,
 *   or robustness in response to stressors, shocks, volatility, and
 *   randomness. While this appears to be a natural law at the evolutionary
 *   scale (Mountain), its application by specific agents within a system
 *   creates a stark perspectival gap. For the informed practitioner who can
 *   adopt a 'barbell' strategy (exposing themselves to massive upside while
 *   capping downside), it is a coordination tool (Rope). For the subject who
 *   cannot opt-out and whose fragility provides the fuel for others'
 *   antifragility, it is a highly extractive trap (Snare).
 *
 * KEY AGENTS:
 *   - The Optimized Serf: Primary target (powerless/trapped) - bears the costs of volatility.
 *   - The Barbell Practitioner: Primary beneficiary (moderate/arbitrage) - harvests upside from volatility.
 *   - The Fragilista/Bureaucrat: Institutional enforcer (institutional/constrained) - creates fragile systems that can be exploited.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_sed_r2, 0.75).
domain_priors:suppression_score(antifragility_u2_sed_r2, 0.65).
domain_priors:theater_ratio(antifragility_u2_sed_r2, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_sed_r2, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_sed_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_sed_r2, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_sed_r2, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_sed_r2, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_sed_r2, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_sed_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_sed_r2, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_sed_r2, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_sed_r2, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OPTIMIZED SERF (SNARE)
constraint_indexing:constraint_classification(antifragility_u2_sed_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BARBELL PRACTITIONER (ROPE)
constraint_indexing:constraint_classification(antifragility_u2_sed_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE BUREAUCRAT (TANGLED ROPE)
constraint_indexing:constraint_classification(antifragility_u2_sed_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE EVOLUTIONARY OBSERVER (MOUNTAIN)
constraint_indexing:constraint_classification(antifragility_u2_sed_r2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_sed_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_sed_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_sed_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_sed_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_sed_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (0.75) reflects the core mechanism of antifragility in practice: a 'convexity bias' where one agent harvests the upside from volatility while the downside is externalized to another agent or the collective. Suppression (0.65) is high because the structure of modern optimized systems often removes the buffers and redundancies that would allow individuals to avoid being 'the fragile part' of the equation.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The practitioner sees a tool for personal thriving (Rope). The subject of their practice sees an inescapable system of risk transfer (Snare). The institutional actor sees a complex system they are trying to manage, not realizing they are creating the conditions for extraction (Tangled Rope). The long-term analytical view sees a fundamental law of nature (Mountain), abstracting away the suffering of the individuals who are sacrificed for the system's survival.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: benefits flow to the 'antifragile practitioner' who can selectively engage with risk. Costs are borne by the 'optimized serfs' and 'fragile institutions' who are forced into a position where they absorb negative shocks. The practitioner is the beneficiary; the serf is the victim.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by refusing to accept the 'natural law' (Mountain) or 'personal strategy' (Rope) framings as complete. The Tangled Rope classification from the institutional perspective is key, as it acknowledges both the stated goal of system stability (coordination) and the actual result of asymmetric risk transfer (extraction). This prevents the mislabeling of a predatory dynamic as a simple force of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the 0.75 extraction a functional necessity for evolution or predatory?',
    'Audit of Skin in the Game metrics across participating agents.',
    'If necessity: Mountain. If predatory: Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or predatory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_sed_r2, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_sed_r2, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_sed_r2, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_sed_r2, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_sed_r2, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_sed_r2, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_sed_r2, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
