% ============================================================================
% CONSTRAINT STORY: theatrical_neutrality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theatrical_neutrality, []).

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
 *   constraint_id: theatrical_neutrality
 *   human_readable: Theatrical Neutrality: The View from Nowhere as Epistemic Extraction
 *   domain: general
 *
 * SUMMARY:
 *   Theatrical neutrality, the presentation of asymmetric claims as equally
 *   valid 'perspectives,' extracts the reader's ability to discern truth and
 *   reinforces the status quo. This dynamic, often employed to avoid
 *   accusations of bias, allows misinformation to flourish under the guise of
 *   balanced reporting or viewpoint diversity. The effect is not merely a
 *   passive allowance of different opinions but an active degradation of the
 *   epistemic commons.
 *
 * KEY AGENTS:
 *   - Misinformed Public: Primary victim (powerless/trapped) - loses the ability to discern truth.
 *   - Epistemic Commons: Secondary victim (powerless/trapped) - degraded by misinformation.
 *   - Perpetrators of Misinformation: Primary beneficiary (powerful/arbitrage) - advances agendas through confusion.
 *   - Institutions Maintaining Status Quo: Secondary beneficiary (institutional/arbitrage) - protects itself from criticism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theatrical_neutrality, 0.6).
domain_priors:suppression_score(theatrical_neutrality, 0.7).
domain_priors:theater_ratio(theatrical_neutrality, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theatrical_neutrality, extractiveness, 0.6).
narrative_ontology:constraint_metric(theatrical_neutrality, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(theatrical_neutrality, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theatrical_neutrality, tangled_rope).
narrative_ontology:human_readable(theatrical_neutrality, "Theatrical Neutrality: The View from Nowhere as Epistemic Extraction").
narrative_ontology:topic_domain(theatrical_neutrality, "general").

domain_priors:requires_active_enforcement(theatrical_neutrality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theatrical_neutrality, perpetrators_of_misinformation).
narrative_ontology:constraint_beneficiary(theatrical_neutrality, institutions_maintaining_status_quo).
narrative_ontology:constraint_victim(theatrical_neutrality, misinformed_public).
narrative_ontology:constraint_victim(theatrical_neutrality, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The misinformed public, lacking specialized knowledge and critical thinking skills, is trapped by the presentation of misinformation as equally valid viewpoints.
constraint_indexing:constraint_classification(theatrical_neutrality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The epistemic commons is degraded by the proliferation of misinformation, making it difficult to distinguish truth from falsehood. It is defenseless against this degradation. There is no exit.
constraint_indexing:constraint_classification(theatrical_neutrality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Institutions that benefit from the status quo use theatrical neutrality to protect themselves from criticism and maintain their power. They see this as helpful to coordinate and enforce that status quo.
constraint_indexing:constraint_classification(theatrical_neutrality, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Those who spread misinformation benefit from the confusion and doubt created by theatrical neutrality. They can use it to advance their own agendas and increase their power.
constraint_indexing:constraint_classification(theatrical_neutrality, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, theatrical neutrality presents a tangled rope. It seems initially like an attempt to present different viewpoints neutrally. However, it actively extracts the ability to discern the truth from the public, and requires the active maintenance of the performance of neutrality.
constraint_indexing:constraint_classification(theatrical_neutrality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theatrical_neutrality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theatrical_neutrality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theatrical_neutrality, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(theatrical_neutrality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(theatrical_neutrality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Significant extraction of the ability to discern truth. Suppression (0.70): High. Limits on critical thinking and promotion of misinformation. Theater ratio (0.80): High. The focus on apparent neutrality overshadows the need for accuracy.
 *
 * PERSPECTIVAL GAP:
 *   The misinformed public and epistemic commons experience this as a Snare - trapping them with misinformation and degrading truth. The perpetrators and those maintaining status quo experience it as a Rope - using the dynamic to help them continue existing. However, the analyst recognizes the Tangled Rope - the appearance of neutrality obscures extraction of the truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Those victimized by this theatrical performance bear high d values, those benefiting have low d values, the analyst's perspective balances this.
 *
 * MANDATROPHY ANALYSIS:
 *   Theatrical neutrality is only superficially similar to actual neutrality, as it actively harms the commons and provides cover for malicious actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truth_discernability,
    'What is the threshold for truth discernability where extraction turns into a collapse of epistemology?',
    'Correlation between exposure to theatrical neutrality and the ability to discern facts.',
    'The level of extraction will affect the severity of the impact on the general population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_discernability, empirical, 'Determine the level to which truth can be extracted before collapsing the truth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theatrical_neutrality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thea_tr_t0, theatrical_neutrality, theater_ratio, 0, 0.5).
narrative_ontology:measurement(thea_tr_t5, theatrical_neutrality, theater_ratio, 5, 0.7).
narrative_ontology:measurement(thea_tr_t10, theatrical_neutrality, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(thea_be_t0, theatrical_neutrality, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(thea_be_t5, theatrical_neutrality, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(thea_be_t10, theatrical_neutrality, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theatrical_neutrality, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
