% ============================================================================
% CONSTRAINT STORY: quellcrist_falconer_justice
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quellcrist_falconer_justice, []).

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
 *   constraint_id: quellcrist_falconer_justice
 *   human_readable: The Machinery of Justice (Quellist)
 *   domain: political
 *
 * SUMMARY:
 *   The Machinery of Justice, as envisioned in the Quellist philosophy, is a
 *   system that ostensibly aims to maintain order and resolve disputes.
 *   However, it can become a tool of oppression, extracting resources and
 *   suppressing dissent. The system's effectiveness and perceived fairness
 *   dictate its classification on the DR index, ranging from a functional
 *   coordination mechanism (Rope) to a coercive snare. This story analyzes
 *   the inherent tensions and potential for abuse within such a system.
 *
 * KEY AGENTS:
 *   - Accused Individuals: The primary targets, bearing the cost of the system's enforcement.
 *   - Dissenting Population: Groups that challenge the system, often facing suppression.
 *   - Governing Elite: The beneficiaries who control and utilize the system.
 *   - Judicial System: The institution that executes the 'justice,' experiencing constraints and benefits.
 *   - Analytical Observer: A perspective to evaluate the system objectively.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quellcrist_falconer_justice, 0.65).
domain_priors:suppression_score(quellcrist_falconer_justice, 0.7).
domain_priors:theater_ratio(quellcrist_falconer_justice, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quellcrist_falconer_justice, extractiveness, 0.65).
narrative_ontology:constraint_metric(quellcrist_falconer_justice, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(quellcrist_falconer_justice, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quellcrist_falconer_justice, snare).
narrative_ontology:human_readable(quellcrist_falconer_justice, "The Machinery of Justice (Quellist)").
narrative_ontology:topic_domain(quellcrist_falconer_justice, "political").

domain_priors:requires_active_enforcement(quellcrist_falconer_justice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quellcrist_falconer_justice, governing_elite).
narrative_ontology:constraint_beneficiary(quellcrist_falconer_justice, judicial_system).
narrative_ontology:constraint_victim(quellcrist_falconer_justice, accused_individuals).
narrative_ontology:constraint_victim(quellcrist_falconer_justice, dissenting_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The accused individual, lacking power and trapped within the system, experiences the justice system as a snare. Limited exit options and high suppression lead to significant extraction.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The judicial system benefits from maintaining order and upholding laws, but is also constrained by the very rules it enforces, resulting in a Tangled Rope classification. High extraction but also a coordination function.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The governing elite, while nominally benefiting, find the 'justice' system has calcified and no longer serves its stated function of ensuring justice. High theater ratio.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% A population who dissents to the existing system. They are moderately constrained by the system, but also benefit from the existing order. High extraction.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From a broad analytical perspective, the justice system presents a complex interplay of coordination and extraction. The system aims to maintain order and enforce laws (coordination), but also extracts resources and suppresses dissent (extraction). High extraction and suppression with some coordination benefit.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quellcrist_falconer_justice_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quellcrist_falconer_justice, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quellcrist_falconer_justice, TR),
    TR >= 0.70.

:- end_tests(quellcrist_falconer_justice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness score reflects the system's capacity to extract resources from individuals and groups, including legal fees, fines, and the suppression of economic opportunities. The suppression score indicates the system's ability to limit alternatives and maintain control, suppressing dissent and limiting personal freedoms. The theater ratio captures the extent to which the system's actions are performative, driven by political considerations rather than genuine justice.
 *
 * PERSPECTIVAL GAP:
 *   The accused individual views the system as a snare, while the governing elite might perceive it as a necessary tool for maintaining order. The analytical observer recognizes the system's inherent contradictions, acknowledging both its coordination and extraction functions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's position within the system. Accused individuals and dissenting populations experience high directionality, as they are subject to the system's power. The governing elite and judicial system benefit from the system's stability and control, thus their d value is low. The exact position on the scale is influenced by factors such as individual agency, access to resources, and the ability to exit the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold,
    'What level of perceived fairness would shift the classification from snare to rope?',
    'Sociological surveys and historical analysis of public trust in the judicial system.',
    'Higher legitimacy leads to rope; lower legitimacy confirms snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_threshold, empirical, 'Legitimacy threshold determines system classification.').

omega_variable(
    suppression_alternatives,
    'Are there viable alternative means of conflict resolution available outside the formal system?',
    'Comparative legal studies and analysis of informal justice systems.',
    'More alternatives reduce suppression, potentially shifting from snare to tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_alternatives, empirical, 'Alternatives reduce the system''s suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quellcrist_falconer_justice, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quel_tr_t0, quellcrist_falconer_justice, theater_ratio, 0, 0.1).
narrative_ontology:measurement(quel_tr_t5, quellcrist_falconer_justice, theater_ratio, 5, 0.4).
narrative_ontology:measurement(quel_tr_t10, quellcrist_falconer_justice, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(quel_be_t0, quellcrist_falconer_justice, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(quel_be_t5, quellcrist_falconer_justice, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(quel_be_t10, quellcrist_falconer_justice, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quellcrist_falconer_justice, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
