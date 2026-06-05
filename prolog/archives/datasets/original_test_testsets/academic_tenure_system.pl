% ============================================================================
% CONSTRAINT STORY: academic_tenure_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-15
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
 *   constraint_id: academic_tenure_system
 *   human_readable: Academic Tenure System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The academic tenure system is a quintessential Tangled Rope. It provides
 *   a genuine coordination function: protecting academic freedom to allow for
 *   long-term, high-risk, or controversial research. However, this benefit is
 *   primarily realized by tenured faculty and is structurally dependent on a
 *   highly extractive and coercive probationary period for junior faculty.
 *   This pre-tenure phase functions as a Snare, demanding immense
 *   productivity under precarious conditions, with exit options often
 *   requiring a full career change. The system's stability relies on this
 *   asymmetry.
 *
 * KEY AGENTS:
 *   - Junior Faculty: Primary target (powerless/trapped) — bears the cost of hyper-productivity and precarity.
 *   - Tenured Faculty: Primary beneficiary (institutional/arbitrage) — receives protection for academic freedom.
 *   - University Administration: Institutional beneficiary (institutional/constrained) — benefits from a low-cost, high-output junior labor pool and a stable senior faculty.
 *   - Adjunct Faculty: Excluded victims (powerless/trapped) — experience the system as a source of precarious labor with no access to its coordinating benefits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_tenure_system, 0.55).
domain_priors:suppression_score(academic_tenure_system, 0.65).
domain_priors:theater_ratio(academic_tenure_system, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_tenure_system, extractiveness, 0.55).
narrative_ontology:constraint_metric(academic_tenure_system, suppression_requirement, 0.65).
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
narrative_ontology:constraint_victim(academic_tenure_system, epistemic_risk_taking).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR FACULTY (SNARE) — Faces an 'up or out' system with few alternatives within academia. The high workload and precarious position constitute a coercive extraction of labor in exchange for a chance at security. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(academic_tenure_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TENURED FACULTY (ROPE) — Experiences tenure as a pure coordination mechanism that protects academic freedom, enabling long-term, potentially controversial research. The extractive aspects are filtered out or seen as a necessary rite of passage. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(academic_tenure_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSITY ADMINISTRATION (TANGLED ROPE) — Manages the dual function: it coordinates a stable, high-prestige senior faculty (Rope) while leveraging the system to secure a low-cost, high-productivity, and flexible junior/adjunct labor pool (Snare). Exit is constrained by institutional norms and competition. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(academic_tenure_system, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ADJUNCT FACULTY (PITON) — For adjuncts, the promise of academic freedom and stability associated with tenure is a hollowed-out, inaccessible ideal. The system's primary function has atrophied to pure labor extraction, with the 'ideal' of tenure maintained as a theatrical justification for a multi-tiered system. The system they interact with is a degraded form of the one junior faculty face.
constraint_indexing:constraint_classification(academic_tenure_system, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's claimed purpose (academic freedom) is a genuine coordination function, but it is structurally coupled to an asymmetric extraction mechanism targeting junior and contingent faculty. Both functions are real and inseparable. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(academic_tenure_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (ε=0.55): High. The value of labor extracted from pre-tenure and adjunct faculty (in terms of publications, teaching, and service) far exceeds their compensation and job security, with the surplus captured by the institution's prestige and budget. Suppression (0.65): High. The 'up or out' nature of the tenure track and the scarcity of tenure-track positions create a high-coercion environment. Alternatives within academia are limited, making exit costly. Theater Ratio (0.40): Moderate. While much of the work is substantive, a significant portion of activity is performative, aimed at satisfying tenure committee metrics which may not align with actual scholarly impact.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a junior faculty member, the system is a Snare defined by coercion and extraction. For a tenured professor, it is a Rope that enables their life's work. For the administration, it is a complex management tool (Tangled Rope) balancing competing interests. For an adjunct, it is a Piton, where the ideals of the system are inert and only the labor extraction remains. The analytical view must acknowledge that all these perspectives are structurally valid readings of the same underlying system.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tenured faculty, administration) have institutional power and low directionality (d), experiencing the system as coordination or a manageable hybrid. Victims (junior/adjunct faculty) are powerless and trapped, leading to high directionality (d) and experiencing the system as a Snare. The structural relationship—who holds power and who bears risk—directly determines the classification from their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic resolution of mandatrophy. Labeling tenure as purely a Rope for 'academic freedom' ignores the severe, systematic extraction it imposes. Labeling it purely a Snare ignores the genuine, historically important coordination function it serves. The Tangled Rope classification correctly identifies it as a hybrid system where a desirable coordination goal is achieved via an inseparable and asymmetric extraction mechanism. The system's stability depends on this very ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_discipline,
    'Is the primary structural function of tenure the protection of academic freedom or the enforcement of labor discipline and intellectual conformity on junior scholars?',
    'Analysis of research topic divergence pre- and post-tenure; statistical analysis of tenure denial cases correlated with research controversy vs. productivity metrics.',
    'If primarily for freedom, the system is closer to a Rope with unfortunate side effects. If primarily for discipline, it is a Snare with a Rope-like justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_vs_discipline, empirical, 'Distinguishing the primary function between academic freedom and labor discipline.').

omega_variable(
    selection_criterion,
    'Does the high-pressure tenure track select for the most innovative scholars or for those most tolerant of risk-aversion and conformity?',
    'Longitudinal studies comparing the long-term impact of scholars tenured in high-pressure vs. lower-pressure systems or historical periods.',
    'If it selects for innovation, the extraction has a plausible justification. If it selects for conformity, the ''epistemic_risk_taking'' victim class is validated, and the system is more extractive than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_criterion, empirical, 'Determining if tenure selects for innovation or conformity.').

omega_variable(
    alternative_system_viability,
    'Could alternative systems, such as renewable long-term contracts, provide equivalent protection for academic freedom with less extraction?',
    'Comparative analysis of academic systems in countries without a US-style tenure system (e.g., parts of Europe), controlling for funding and cultural factors.',
    'If viable alternatives exist, the suppression score (0.65) is a contingent feature of the US system, not an inherent property of academic labor. This would strengthen the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_viability, conceptual, 'Viability of alternative academic employment systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_tenure_system, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_tenure_system, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(acad_tr_t1995, academic_tenure_system, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(acad_tr_t2020, academic_tenure_system, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_tenure_system, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(acad_be_t1995, academic_tenure_system, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(acad_be_t2020, academic_tenure_system, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_tenure_system, resource_allocation).
narrative_ontology:affects_constraint(academic_tenure_system, academic_publishing_system).
narrative_ontology:affects_constraint(academic_tenure_system, university_funding_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
