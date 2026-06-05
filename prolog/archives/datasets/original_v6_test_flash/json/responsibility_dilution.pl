% ============================================================================
% CONSTRAINT STORY: responsibility_dilution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_dilution, []).

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
 *   constraint_id: responsibility_dilution
 *   human_readable: The Accountability Fog
 *   domain: organizational/legal/technological
 *
 * SUMMARY:
 *   A scenario where a critical decision is fragmented across so many
 *   autonomous agents, bureaucratic layers, and algorithmic filters that the
 *   'locus of responsibility' effectively vanishes. This creates an
 *   'accountability fog' where no single entity can be held fully accountable
 *   for negative outcomes. The diffusion protects individual actors and
 *   institutions from direct blame, but it also weakens overall oversight and
 *   enables harmful practices to persist.
 *
 * KEY AGENTS:
 *   - Diffuse Decision Makers: Beneficiaries who avoid direct blame.
 *   - Process Owners: Beneficiaries who control segments of the process.
 *   - Affected Individuals: Victims who bear the consequences of the decision.
 *   - Society at Large: Victims who suffer from systemic failures.
 *   - Executive Leadership: Institutional actors who maintain control.
 *   - Regulatory Agencies: Organized groups tasked with oversight.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_dilution, 0.6).
domain_priors:suppression_score(responsibility_dilution, 0.5).
domain_priors:theater_ratio(responsibility_dilution, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_dilution, extractiveness, 0.6).
narrative_ontology:constraint_metric(responsibility_dilution, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(responsibility_dilution, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_dilution, tangled_rope).
narrative_ontology:human_readable(responsibility_dilution, "The Accountability Fog").
narrative_ontology:topic_domain(responsibility_dilution, "organizational/legal/technological").

domain_priors:requires_active_enforcement(responsibility_dilution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_dilution, diffuse_decision_makers).
narrative_ontology:constraint_beneficiary(responsibility_dilution, process_owners).
narrative_ontology:constraint_victim(responsibility_dilution, affected_individuals).
narrative_ontology:constraint_victim(responsibility_dilution, society_at_large).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individuals directly impacted by the diffused responsibility, with no recourse or ability to influence the decision-making process.
constraint_indexing:constraint_classification(responsibility_dilution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Managers who are nominally responsible but lack the authority or information to effectively control outcomes. They benefit from the diffusion of blame but are also constrained by the system.
constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Top-level leadership who benefit from the lack of clear accountability, as it shields them from direct blame while enabling them to maintain control.
constraint_indexing:constraint_classification(responsibility_dilution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Agencies tasked with oversight but rendered ineffective by the complexity and diffusion of responsibility. They go through the motions but lack the power to hold anyone accountable.
constraint_indexing:constraint_classification(responsibility_dilution, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analyzing the system holistically, the observer recognizes the tangled web of responsibility and the incentives that perpetuate it.
constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_dilution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(responsibility_dilution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_dilution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(responsibility_dilution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(responsibility_dilution, TR),
    TR >= 0.70.

:- end_tests(responsibility_dilution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score reflects the degree to which the accountability fog allows negative consequences to be externalized onto affected individuals and society at large. The suppression score reflects the barriers to holding anyone accountable. The theater_ratio represents the degree to which oversight mechanisms are performative rather than effective.
 *
 * PERSPECTIVAL GAP:
 *   The affected individuals experience the situation as a snare, with no recourse. Executive leadership benefit from the diffusion of blame, experiencing it as a rope. Mid-level managers are constrained by the system but also benefit from the lack of clear accountability, resulting in a tangled rope. Regulatory agencies see their own ineffectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries benefit from the lack of clear accountability, experiencing it as coordination. Victims bear the consequences of the decision, experiencing it as extraction. Regulatory agencies try to oversee the processes, they are constrained by the system and experience this as piton.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_opacity,
    'To what extent are algorithmic decision-making processes transparent and auditable?',
    'Technical audits, legal mandates for explainability.',
    'If opaque, accountability further diluted; if transparent, responsibility can be traced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_opacity, empirical, 'Transparency of algorithmic decision-making.').

omega_variable(
    legal_liability_standards,
    'Are existing legal standards adequate to assign liability in cases of diffused responsibility?',
    'Legal test cases, legislative reforms.',
    'If inadequate, accountability gap persists; if adequate, liability can be assigned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_liability_standards, conceptual, 'Adequacy of legal liability standards.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_dilution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resp_tr_t0, responsibility_dilution, theater_ratio, 0, 0.2).
narrative_ontology:measurement(resp_tr_t5, responsibility_dilution, theater_ratio, 5, 0.5).
narrative_ontology:measurement(resp_tr_t10, responsibility_dilution, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(resp_be_t0, responsibility_dilution, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(resp_be_t5, responsibility_dilution, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(resp_be_t10, responsibility_dilution, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_dilution, enforcement_mechanism).
narrative_ontology:affects_constraint(responsibility_dilution, regulatory_capture).
narrative_ontology:affects_constraint(responsibility_dilution, information_asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
