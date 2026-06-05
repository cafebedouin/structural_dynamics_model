% ============================================================================
% CONSTRAINT STORY: taxonomy_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taxonomy_drift, []).

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
 *   constraint_id: taxonomy_drift
 *   human_readable: The Semantic Slippage Trap
 *   domain: social/linguistic/bureaucratic
 *
 * SUMMARY:
 *   The Semantic Slippage Trap describes a scenario where the definitions
 *   used by a governing system drift away from the ground-truth reality of
 *   the subjects. This drift, often unintentional, can lead to
 *   misclassification, unfair treatment, and a general disconnect between the
 *   governing body and those they govern. The governing body may benefit in
 *   the short term through increased control, but the long-term consequences
 *   include inefficiencies and a loss of legitimacy.
 *
 * KEY AGENTS:
 *   - Governed Subjects: Primary target (powerless/trapped) - Bear the cost of misclassification and disconnect from reality.
 *   - Governing Body: Primary beneficiary (institutional/constrained) - Benefits from increased control but is also constrained by the long-term consequences of the drift.
 *   - Analytical Observer: Analytical observer - Sees the full structure and can analyze the consequences of the drift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taxonomy_drift, 0.55).
domain_priors:suppression_score(taxonomy_drift, 0.7).
domain_priors:theater_ratio(taxonomy_drift, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taxonomy_drift, extractiveness, 0.55).
narrative_ontology:constraint_metric(taxonomy_drift, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(taxonomy_drift, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taxonomy_drift, tangled_rope).
narrative_ontology:human_readable(taxonomy_drift, "The Semantic Slippage Trap").
narrative_ontology:topic_domain(taxonomy_drift, "social/linguistic/bureaucratic").

domain_priors:requires_active_enforcement(taxonomy_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taxonomy_drift, governing_body).
narrative_ontology:constraint_victim(taxonomy_drift, governed_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The governed subjects find themselves trapped as the taxonomy drifts. Their actions are increasingly misclassified and the definitions used by the governing body no longer align with their reality. They have no exit.
constraint_indexing:constraint_classification(taxonomy_drift, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The governing body benefits from the semantic drift, as it reinforces their power and allows for easier control. However, this drift can also lead to inefficiencies and a disconnect from reality, constraining their actions in the long run. They actively enforce the new definitions.
constraint_indexing:constraint_classification(taxonomy_drift, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the semantic slippage is a tangled rope. It benefits the governing body in the short term but leads to a long-term decline in the system's effectiveness. This creates a complex interplay of coordination and extraction.
constraint_indexing:constraint_classification(taxonomy_drift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taxonomy_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taxonomy_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taxonomy_drift, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taxonomy_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(taxonomy_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is rated at 0.55 because the semantic drift extracts value from the governed subjects by misclassifying their actions and needs. The suppression is at 0.70 due to the governing body's enforcement of the drifting definitions, which restricts the subjects' ability to challenge or resist. The theater ratio is 0.60 because there's a performative aspect to maintaining the drifting taxonomy even when it's no longer effective.
 *
 * PERSPECTIVAL GAP:
 *   The governed subjects perceive the semantic drift as a snare because they are trapped within a system that no longer accurately represents their reality. The governing body sees it as a tangled rope because they benefit from the increased control but are also constrained by the long-term consequences. The analytical observer also views it as a tangled rope because it involves both coordination (maintaining a governing system) and extraction (the misclassification of the subjects).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the relationship between the agents and the constraint. The governed subjects are the primary target and have no exit option, leading to a high directionality value. The governing body benefits from the increased control, leading to a lower directionality value. The analytical observer sees both benefits and costs, resulting in an intermediate directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification of this constraint addresses the mandatrophy by recognizing the complex interplay of benefits and costs. It's not simply a case of pure extraction or pure coordination, but a hybrid scenario where both exist. The tangled rope classification captures this complexity and prevents the constraint from being mislabeled as a pure snare or a pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ground_truth_measurability,
    'How accurately can the ''ground truth'' reality of the subjects be measured and defined?',
    'Developing metrics and data collection methods that capture the subjects'' experiences and realities effectively.',
    'If measurable: the drift can be quantified and corrected. If not measurable: the drift is difficult to detect and address.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ground_truth_measurability, empirical, 'Measurability of the ground truth reality').

omega_variable(
    governing_body_awareness,
    'To what extent is the governing body aware of the semantic drift and its consequences?',
    'Conducting surveys, interviews, and audits to assess the governing body''s understanding and awareness of the drift.',
    'If aware: the drift can be addressed proactively. If unaware: the drift is likely to continue and worsen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governing_body_awareness, empirical, 'Awareness of the drift within the governing body').

omega_variable(
    alternative_taxonomies,
    'Are there alternative taxonomies or classification systems that better represent the subjects'' reality?',
    'Researching and evaluating alternative taxonomies that are more aligned with the subjects'' experiences and needs.',
    'If available: the drift can be mitigated by adopting a more appropriate system. If unavailable: the drift may be unavoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_taxonomies, conceptual, 'Availability of alternative taxonomies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taxonomy_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taxo_tr_t0, taxonomy_drift, theater_ratio, 0, 0.4).
narrative_ontology:measurement(taxo_tr_t5, taxonomy_drift, theater_ratio, 5, 0.5).
narrative_ontology:measurement(taxo_tr_t10, taxonomy_drift, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(taxo_be_t0, taxonomy_drift, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(taxo_be_t5, taxonomy_drift, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(taxo_be_t10, taxonomy_drift, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taxonomy_drift, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
