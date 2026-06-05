% ============================================================================
% CONSTRAINT STORY: fnl_shadow_probe
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-10-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fnl_shadow_probe, []).

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
 *   constraint_id: fnl_shadow_probe
 *   human_readable: FNL Shadow Mode Probe (Physics-Washed Construction)
 *   domain: investigation/testing
 *
 * SUMMARY:
 *   A synthetic constraint designed to model a constructed system that could
 *   be mistaken for a natural law (a False Natural Law, or FNL). The
 *   "physics-washed construction" refers to the system being presented or
 *   framed in a way that gives it undue legitimacy, making it appear as a
 *   naturally occurring phenomenon rather than a deliberately constructed
 *   one. This probe is meant to facilitate the study and detection of these
 *   FNL-type misclassifications. Independent investigators may be trapped,
 *   epistemic reliability is victimized, and model proponents benefit.
 *
 * KEY AGENTS:
 *   - Model Proponents: Primary beneficiary (institutional/arbitrage) — gains from perceived scientific legitimacy.
 *   - Independent Investigators: Primary victim (powerless/trapped) — suffers from resource disadvantages and suppressed alternatives.
 *   - Epistemic Commons: Abstract victim bearing the cost of false information.
 *   - The Scientific Community: Constrained by pre-existing literature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fnl_shadow_probe, 0.45).
domain_priors:suppression_score(fnl_shadow_probe, 0.6).
domain_priors:theater_ratio(fnl_shadow_probe, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fnl_shadow_probe, extractiveness, 0.45).
narrative_ontology:constraint_metric(fnl_shadow_probe, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(fnl_shadow_probe, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fnl_shadow_probe, tangled_rope).
narrative_ontology:human_readable(fnl_shadow_probe, "FNL Shadow Mode Probe (Physics-Washed Construction)").
narrative_ontology:topic_domain(fnl_shadow_probe, "investigation/testing").

domain_priors:requires_active_enforcement(fnl_shadow_probe).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fnl_shadow_probe, model_proponents).
narrative_ontology:constraint_victim(fnl_shadow_probe, independent_investigators).
narrative_ontology:constraint_victim(fnl_shadow_probe, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Independent investigators are trapped by the dominant framing and lack resources to challenge the 'physics-washed' construction, making it a snare.
constraint_indexing:constraint_classification(fnl_shadow_probe, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The scientific community is constrained by existing literature and career incentives, but also benefits from the perceived advances. Tangled rope reflects mixed coordination and extraction.
constraint_indexing:constraint_classification(fnl_shadow_probe, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Model proponents benefit from increased funding, recognition, and influence, experiencing the constraint as a rope.
constraint_indexing:constraint_classification(fnl_shadow_probe, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical observer sees a degraded system where investigation and testing are overshadowed by physics-washed narratives. The system persists due to inertia despite low functional utility.
constraint_indexing:constraint_classification(fnl_shadow_probe, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fnl_shadow_probe_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fnl_shadow_probe, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fnl_shadow_probe, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fnl_shadow_probe, TR),
    TR >= 0.70.

:- end_tests(fnl_shadow_probe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): The system extracts resources and attention from alternative lines of investigation. Suppression (0.60): The dominant narrative suppresses alternative explanations through perceived authority. Theater ratio (0.75): There is some genuine attempt at investigation, although overshadowed by framing, and the theater has increased over time.
 *
 * PERSPECTIVAL GAP:
 *   Independent investigators see a snare, scientific community a tangled rope, and model proponents a rope, reflecting differing power, exit options and structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Model Proponents) experience a rope due to resource capture. Victims (Independent Investigators) experience a snare because the dominant narrative constrains their exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dominant_narrative_decay,
    'What factors will weaken the grip of physics-washed explanations?',
    'Detailed analysis of past paradigm shifts; identification of falsifiable predictions; development of novel investigative methodologies.',
    'Shift from Snare/Tangled Rope toward Scaffold if alternative explanations gain traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_narrative_decay, empirical, 'Factors that would weaken dominant physics-washed narratives').

omega_variable(
    investigative_resources,
    'How to allocate resources to challenge dominant, yet untested/falsified narratives?',
    'Funding models that prioritize falsification over confirmation; promoting open-source investigation; establishing independent testing agencies.',
    'Improved investigation and testing could reclassify the system towards rope or scaffold',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investigative_resources, preference, 'Methods for allocating resources to test dominant narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fnl_shadow_probe, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fnl__tr_t0, fnl_shadow_probe, theater_ratio, 0, 0.55).
narrative_ontology:measurement(fnl__tr_t5, fnl_shadow_probe, theater_ratio, 5, 0.65).
narrative_ontology:measurement(fnl__tr_t10, fnl_shadow_probe, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(fnl__be_t0, fnl_shadow_probe, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fnl__be_t5, fnl_shadow_probe, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(fnl__be_t10, fnl_shadow_probe, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fnl_shadow_probe, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
