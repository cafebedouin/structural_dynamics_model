% ============================================================================
% CONSTRAINT STORY: ulysses_chp02
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp02, []).

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
 *   constraint_id: ulysses_chp02
 *   human_readable: The Nightmare of History (Dalkey School)
 *   domain: economic/social/political
 *
 * SUMMARY:
 *   For Stephen Dedalus, the school at Dalkey is a Snare defined by the
 *   'nightmare of history' and the futility of teaching privileged boys who
 *   lack innocence. This constraint highlights Stephen's trapped position and
 *   the weight of the past on his present.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary victim (powerless/trapped) - feels trapped by economic necessity and the weight of Irish history.
 *   - Dalkey School: Institutional Actor (institutional/constrained) - represents the status quo and degraded purpose of education.
 *   - Ireland: Nation (moderate/constrained) - trapped by history, looking for ways to coordinate a path forward.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp02, 0.7).
domain_priors:suppression_score(ulysses_chp02, 0.8).
domain_priors:theater_ratio(ulysses_chp02, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp02, extractiveness, 0.7).
narrative_ontology:constraint_metric(ulysses_chp02, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ulysses_chp02, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp02, snare).
narrative_ontology:human_readable(ulysses_chp02, "The Nightmare of History (Dalkey School)").
narrative_ontology:topic_domain(ulysses_chp02, "economic/social/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(ulysses_chp02, stephen_dedalus).
narrative_ontology:constraint_victim(ulysses_chp02, irish_intellectual_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Stephen, trapped by economic necessity, experiences the school as a snare. He lacks power and sees no immediate escape from the cycle of history and the intellectual constraints it imposes.
constraint_indexing:constraint_classification(ulysses_chp02, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The school, as an institution, is a piton. It once served a purpose (education of the elite), but now its function is degraded, mainly theatrical, and supports a system of privilege that Stephen finds stifling.
constraint_indexing:constraint_classification(ulysses_chp02, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Ireland, struggling with its own history and colonial past, experiences the school as a tangled rope. It represents both a constraint on intellectual freedom and a potential coordination point for future generations, though deeply flawed. There are extraction dynamics and a need to maintain some sort of historical narrative, while simultaneously grappling with it.
constraint_indexing:constraint_classification(ulysses_chp02, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the analytical observer views the situation as a tangled rope, demonstrating the complex interplay of historical forces, individual agency, and systemic constraints. It showcases the tension between the burden of the past and the potential for future liberation. There are elements of coordination (education) and extraction (maintaining the status quo) within this system.
constraint_indexing:constraint_classification(ulysses_chp02, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp02_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp02, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp02, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp02, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp02, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp02_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70): High - Stephen's intellectual and creative energies are drained by the oppressive atmosphere of the school and the perceived futility of his work. He must conform to the system's expectations and suppress his own thoughts and feelings. Suppression (0.80): High - The school environment stifles intellectual freedom and reinforces existing social hierarchies. Stephen faces limited opportunities for genuine expression or upward mobility. Theater ratio (0.30): Low - The school still offers a nominal education, even if it's degraded.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different positions of the actors involved. Stephen sees a snare because he's trapped. Ireland sees a tangled rope because it's a mix of tradition and stifling the future. The school sees itself as part of the system, and the analytical observer sees the whole picture as a tangle, not just one thing.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality value is determined by power dynamics. Stephen has little power, Ireland a bit more, and the observer has the most. This is reflected in how they experience the weight of history and the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    history_determinism,
    'To what extent is Stephen''s fate predetermined by historical forces vs. his own agency?',
    'Analyzing Stephen''s future choices and their consequences within the narrative.',
    'If deterministic: School is an inescapable snare. If agentic: School is a temporary piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(history_determinism, conceptual, 'The degree to which Stephen''s future is already determined by history.').

omega_variable(
    school_agency,
    'Can the school reform from within, or is its primary purpose to enforce the status quo?',
    'Looking at if the school changes over time within the narrative.',
    'If reformable: School could shift into a scaffold (temporary support). If unyielding: School is a snare for its teachers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(school_agency, empirical, 'To what extent the school has any real flexibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp02, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp02, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp02, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp02, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp02, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp02, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp02, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
