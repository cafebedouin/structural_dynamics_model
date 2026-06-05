% ============================================================================
% CONSTRAINT STORY: academic_tenure_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_tenure_system, []).

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
 *   constraint_id: academic_tenure_system
 *   human_readable: Academic Tenure System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The academic tenure system is a complex arrangement that aims to protect
 *   academic freedom while also structuring the career paths of faculty. It
 *   incentivizes productivity in junior faculty but may lead to stagnation in
 *   tenured faculty. It also creates a reliance on adjunct labor, impacting
 *   students' educational experiences.
 *
 * KEY AGENTS:
 *   - Tenured Faculty: Primary beneficiary (institutional/constrained) - receives job security and academic freedom
 *   - Junior Faculty: Primary victim (powerless/trapped) - experiences pressure to publish and secure grants
 *   - University Administration: Secondary beneficiary (institutional/constrained) - receives stability and prestige
 *   - Adjunct Faculty: Secondary victim (powerless/constrained) - receives low pay and little job security
 *   - Students: Indirect victims (moderate/constrained) - affected by reliance on adjunct labor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_tenure_system, 0.6).
domain_priors:suppression_score(academic_tenure_system, 0.5).
domain_priors:theater_ratio(academic_tenure_system, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_tenure_system, extractiveness, 0.6).
narrative_ontology:constraint_metric(academic_tenure_system, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(academic_tenure_system, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_tenure_system, tangled_rope).
narrative_ontology:human_readable(academic_tenure_system, "Academic Tenure System").
narrative_ontology:topic_domain(academic_tenure_system, "economic/social").

domain_priors:requires_active_enforcement(academic_tenure_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_tenure_system, tenured_faculty).
narrative_ontology:constraint_beneficiary(academic_tenure_system, university_administration).
narrative_ontology:constraint_victim(academic_tenure_system, junior_faculty).
narrative_ontology:constraint_victim(academic_tenure_system, adjunct_faculty).
narrative_ontology:constraint_victim(academic_tenure_system, students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Junior faculty face intense pressure to publish and secure grants to achieve tenure, often at the expense of work-life balance and pedagogical quality. Limited job mobility outside academia further traps them.
constraint_indexing:constraint_classification(academic_tenure_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Tenure is perceived as a protection of academic freedom, enabling research and teaching without fear of reprisal. However, it also creates a system where some faculty are less productive after achieving tenure, contributing less to the university than before.
constraint_indexing:constraint_classification(academic_tenure_system, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% The tenure system simultaneously coordinates the protection of academic freedom while enabling the extraction of hyper-productivity from junior faculty. This creates a tangled rope where both coordination and extraction occur.
constraint_indexing:constraint_classification(academic_tenure_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Tenure provides stability and prestige to the university. However, it also limits flexibility in staffing and resource allocation, potentially leading to institutional stagnation and increased reliance on adjunct labor.
constraint_indexing:constraint_classification(academic_tenure_system, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_tenure_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_tenure_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_tenure_system, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_tenure_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_tenure_system, TR),
    TR >= 0.70.

:- end_tests(academic_tenure_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High, reflecting the significant pressure on junior faculty. Suppression (0.50): Moderate, reflecting limited job mobility and publication bias. Theater Ratio (0.40): Moderate, reflecting the performative aspects of tenure review processes.
 *
 * PERSPECTIVAL GAP:
 *   The tenure system is a Snare from the perspective of junior faculty due to the pressure to achieve tenure and the lack of alternative career paths within academia. However, from the perspective of tenured faculty, it's a system (Rope) that protects academic freedom, enabling research and teaching without fear of reprisal. The analytical observer recognizes that the system is a hybrid, balancing these two aspects (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the beneficiaries and victims declared. Tenured faculty and university administration, as beneficiaries, receive low 'd' values, leading to a Rope classification. Junior faculty and adjunct faculty, as victims, receive high 'd' values, leading to a Snare classification. The analytical observer's 'd' value balances these opposing forces, resulting in a Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The tenure system is designed to coordinate academic freedom, but this often comes at the cost of extracting productivity from junior faculty, making it appear as a pure extraction (Snare). However, labeling it as pure extraction fails to account for the stated purpose and perceived benefits of the system, specifically from the perspective of tenured faculty and the protection of academic freedom. The perspectives show both aspects, and the analytical observer classifies the situation as tangled rope, acknowledging both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alt_eval_metrics,
    'Are there alternative evaluation metrics (beyond publications and grants) that better assess a faculty member''s contributions to the university and academic community?',
    'Pilot programs with alternative evaluation systems, longitudinal studies comparing outcomes under different evaluation metrics.',
    'If viable metrics are found, the extractiveness of the tenure system for junior faculty could be reduced. If no viable metrics are found, the current system may be justified as the ''least bad'' option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alt_eval_metrics, empirical, 'Existence of alternative evaluation metrics').

omega_variable(
    academic_freedom_scope,
    'To what extent does tenure effectively protect academic freedom, and are there alternative mechanisms that could provide similar protection?',
    'Legal studies, surveys of faculty experiences, comparative analysis of academic freedom in tenured vs. non-tenured positions.',
    'If tenure is shown to be essential for academic freedom, its benefits may outweigh its costs. If alternative mechanisms are found, the need for tenure may be re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_scope, conceptual, 'Efficacy of tenure in protecting academic freedom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_tenure_system, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t0, academic_tenure_system, theater_ratio, 0, 0.3).
narrative_ontology:measurement(acad_tr_t10, academic_tenure_system, theater_ratio, 10, 0.4).
narrative_ontology:measurement(acad_tr_t20, academic_tenure_system, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(acad_be_t0, academic_tenure_system, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(acad_be_t10, academic_tenure_system, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(acad_be_t20, academic_tenure_system, base_extractiveness, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_tenure_system, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
