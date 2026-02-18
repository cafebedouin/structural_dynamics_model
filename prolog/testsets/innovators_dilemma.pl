% ============================================================================
% CONSTRAINT STORY: innovators_dilemma
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_innovators_dilemma, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: innovators_dilemma
 * human_readable: The Innovator's Dilemma
 * domain: economic/technological
 * * SUMMARY:
 * The Innovator's Dilemma describes how successful companies, by rationally
 * listening to their best customers and investing in high-margin products,
 * create a structural inability to respond to "disruptive" technologies that
 * start in low-margin, niche markets. It is a structural conflict between
 * sustaining current success and embracing future viability.
 * * KEY AGENTS:
 * - Incumbent Manager: Subject (Powerless), trapped by internal resource allocation processes.
 * - Institutional Investor: Beneficiary (Institutional), arbitrages between incumbents and disruptors.
 * - Market Historian: Auditor (Analytical), observes the pattern of market disruption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(innovators_dilemma, 0.55). % Extracts future viability from the incumbent firm.
domain_priors:suppression_score(innovators_dilemma, 0.70).   % Resource allocation processes actively suppress low-margin disruptive projects.
domain_priors:theater_ratio(innovators_dilemma, 0.10).       % The processes are highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(innovators_dilemma, extractiveness, 0.55).
narrative_ontology:constraint_metric(innovators_dilemma, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(innovators_dilemma, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The internal logic of the firm frames its resource allocation as rational coordination.
narrative_ontology:constraint_claim(innovators_dilemma, tangled_rope).
narrative_ontology:human_readable(innovators_dilemma, "The Innovator's Dilemma").
narrative_ontology:topic_domain(innovators_dilemma, "economic/technological").

% Binary flags
% The dilemma is enforced by the firm's own rational processes.
domain_priors:requires_active_enforcement(innovators_dilemma).

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(innovators_dilemma, disruptive_entrants).
narrative_ontology:constraint_victim(innovators_dilemma, incumbent_firms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE INCUMBENT MANAGER (SNARE)
% Trapped by KPIs and resource allocation systems that prevent investment in
% the firm's long-term survival.
constraint_indexing:constraint_classification(innovators_dilemma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL INVESTOR (ROPE)
% Views the dilemma as a coordination mechanism for capital efficiency,
% demanding high returns from incumbents while funding disruptors separately.
constraint_indexing:constraint_classification(innovators_dilemma, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes the dilemma as a hybrid system: it has a genuine coordination
% function (focusing resources) but also produces asymmetric extraction
% (transferring market value from incumbents to disruptors) and requires
% active enforcement (budgeting processes that kill innovation).
constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovators_dilemma_tests).

test(perspectival_gap) :-
    % Verify the manager (Snare) and investor (Rope) have different views.
    constraint_indexing:constraint_classification(innovators_dilemma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovators_dilemma, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(innovators_dilemma, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_tangled_rope) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(innovators_dilemma, ExtMetricName, E),
    assertion(E >= 0.46).

:- end_tests(innovators_dilemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Innovator's Dilemma is a canonical example of a Tangled Rope. It is not a
 * Mountain (a law of physics), but a constructed feature of a specific type of
 * market economy. Its internal logic is presented as pure coordination (a Rope
 * for focusing on profitable customers), but this coordination produces a
 * highly predictable, asymmetric extraction of value from incumbents to new
 * entrants, making it a Snare for those trapped within the incumbent firm.
 *
 * The Perspectival Gap is stark:
 * - The Manager (powerless, trapped) experiences the system as a Snare that
 *   extracts their agency and the firm's future.
 * - The Investor (institutional, mobile) uses the system as a Rope to coordinate
 *   capital, extracting returns from the incumbent's focus while simultaneously
 *   funding the disruptor that will replace it.
 * - The Analyst sees both functions and classifies it as a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope prevents the system from making two key
 * errors. It avoids mislabeling it as a Mountain, which would imply the outcome
 * is inevitable and unchangeable. It also avoids reducing it to a simple Snare,
 * which would ignore the very real, rational, and initially beneficial
 * coordination function that makes the dilemma so difficult to solve. The
 * Tangled Rope classification correctly identifies that the mechanism of
 * coordination is also the mechanism of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_innovators_dilemma,
    'Is the dilemma a fundamental feature of shareholder capitalism, or a contingent artifact of 20th-century management theory?',
    'Comparative analysis of disruption patterns in non-shareholder economic models (e.g., state-owned enterprises, large cooperatives).',
    'If fundamental (Mountain-like), then solutions are limited to managing decline. If contingent (Tangled Rope), then governance structures can be redesigned to solve it.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(innovators_dilemma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The dilemma's extractive force intensifies as a firm becomes more successful
% and its processes become more rigid. Theater ratio remains low as the
% processes are genuinely efficient at their stated (sustaining) goals.
%
% Theater ratio over time (stable):
narrative_ontology:measurement(innovators_dilemma_tr_t0, innovators_dilemma, theater_ratio, 0, 0.10).
narrative_ontology:measurement(innovators_dilemma_tr_t5, innovators_dilemma, theater_ratio, 5, 0.10).
narrative_ontology:measurement(innovators_dilemma_tr_t10, innovators_dilemma, theater_ratio, 10, 0.10).

% Extraction over time (intensifying):
narrative_ontology:measurement(innovators_dilemma_ex_t0, innovators_dilemma, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(innovators_dilemma_ex_t5, innovators_dilemma, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(innovators_dilemma_ex_t10, innovators_dilemma, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The dilemma's coordination mechanism is the firm's resource allocation process.
narrative_ontology:coordination_type(innovators_dilemma, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */