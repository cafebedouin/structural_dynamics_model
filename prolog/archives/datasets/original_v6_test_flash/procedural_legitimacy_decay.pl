% ============================================================================
% CONSTRAINT STORY: procedural_legitimacy_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_legitimacy_decay, []).

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
 *   constraint_id: procedural_legitimacy_decay
 *   human_readable: The Hollow Formality Trap
 *   domain: social/political
 *
 * SUMMARY:
 *   This constraint represents a bureaucratic process that has lost its
 *   justifying social consensus but maintains its legal force. The process,
 *   initially designed to address a specific problem or achieve a legitimate
 *   goal, becomes detached from its original purpose due to changing social
 *   norms, technological advancements, or evolving priorities. As a result,
 *   compliance with the process becomes a hollow formality, imposing costs on
 *   the affected population without delivering meaningful benefits. The
 *   process continues to be enforced due to institutional inertia, legal
 *   mandates, and the self-interest of the incumbent bureaucracy, creating a
 *   'hollow formality trap' that undermines public trust and hinders societal
 *   progress.
 *
 * KEY AGENTS:
 *   - Affected Population: Primary target (powerless/trapped) – bears the costs of compliance without receiving commensurate benefits.
 *   - Incumbent Bureaucracy: Primary beneficiary (institutional/constrained) – maintains control and resources through the continued enforcement of the process.
 *   - Analytical Observer: Sees the full structure and can analyze the costs and benefits from a neutral perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_legitimacy_decay, 0.6).
domain_priors:suppression_score(procedural_legitimacy_decay, 0.7).
domain_priors:theater_ratio(procedural_legitimacy_decay, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_legitimacy_decay, extractiveness, 0.6).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_legitimacy_decay, piton).
narrative_ontology:human_readable(procedural_legitimacy_decay, "The Hollow Formality Trap").
narrative_ontology:topic_domain(procedural_legitimacy_decay, "social/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_legitimacy_decay, incumbent_bureaucracy).
narrative_ontology:constraint_victim(procedural_legitimacy_decay, affected_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The affected population experiences this as a snare because they are forced to comply with procedures that no longer serve a legitimate purpose, and they have no practical means of exiting the system.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The incumbent bureaucracy sees the process as a piton. It maintains the process due to inertia and the perceived need to uphold the law, even though the original justification has eroded. They are constrained because dismantling the process would require significant effort and could create political risk.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical observer sees this as a tangled rope because it involves both a form of (degraded) coordination and extraction. The process still provides some level of predictability and order, but it also extracts resources from the affected population without delivering commensurate benefits. The coordination aspect is the nominal adherence to legal frameworks.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_legitimacy_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(procedural_legitimacy_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(procedural_legitimacy_decay, TR),
    TR >= 0.70.

:- end_tests(procedural_legitimacy_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The process extracts significant time, resources, and effort from the affected population, without delivering substantial benefits in return. Suppression (0.7): High. The affected population has limited means of avoiding compliance, due to legal mandates and the power of the bureaucracy. Theater ratio (0.8): Very High. The process is largely performative, with a significant gap between its stated goals and its actual impact.
 *
 * PERSPECTIVAL GAP:
 *   The affected population views the process as a snare, as they are forced to comply with procedures that no longer serve a legitimate purpose. The incumbent bureaucracy views the process as a piton, maintaining it due to inertia and legal mandates. The analytical observer sees a tangled rope, recognizing both the degraded coordination function and the extraction of resources.
 *
 * DIRECTIONALITY LOGIC:
 *   The affected population has a high 'd' value (close to 1) because they are the primary target of the process and have limited exit options. The incumbent bureaucracy has a low 'd' value (close to 0) because they benefit from the process and have the power to maintain it. The analytical observer has a moderate 'd' value because they can analyze the process from a neutral perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold,
    'At what point does the erosion of social consensus render a bureaucratic process illegitimate?',
    'Quantitative surveys and qualitative analyses of public opinion, combined with expert assessments of the process''s effectiveness.',
    'Determines whether the constraint is primarily a piton (if legitimacy is above the threshold) or a snare (if legitimacy is below the threshold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_threshold, empirical, 'The threshold of social consensus needed to justify the bureaucratic process.').

omega_variable(
    enforcement_capacity,
    'What is the actual capacity of the bureaucracy to enforce the procedure, and how does this affect compliance rates?',
    'Statistical analysis of enforcement actions and compliance rates, combined with field research on the practical challenges of implementation.',
    'Impacts the perceived extractiveness. Lower enforcement -> Lower perceived extraction, and vice-versa. Also affect the victims'' power',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity, empirical, 'The bureaucracy''s actual capacity to enforce the procedure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_legitimacy_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_tr_t0, procedural_legitimacy_decay, theater_ratio, 0, 0.3).
narrative_ontology:measurement(proc_tr_t5, procedural_legitimacy_decay, theater_ratio, 5, 0.6).
narrative_ontology:measurement(proc_tr_t10, procedural_legitimacy_decay, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(proc_be_t0, procedural_legitimacy_decay, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(proc_be_t5, procedural_legitimacy_decay, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(proc_be_t10, procedural_legitimacy_decay, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_legitimacy_decay, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
