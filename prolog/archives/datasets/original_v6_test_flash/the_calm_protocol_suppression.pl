% ============================================================================
% CONSTRAINT STORY: the_calm_protocol_suppression
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_calm_protocol_suppression, []).

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
 *   constraint_id: the_calm_protocol_suppression
 *   human_readable: The 'Calm' of Antarctic Protocol
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The 'calm' in Antarctic Protocol refers to the subtle yet pervasive
 *   pressure to conform to established research norms and viewpoints within
 *   the Antarctic research community. This pressure, while often unspoken,
 *   can create a chilling effect on new researchers and alternative research
 *   paradigms, limiting innovation and diversity of thought. It's described
 *   in the narrative of Soh and Mbatha.
 *
 * KEY AGENTS:
 *   - New Researchers: Victims of the 'calm', experiencing suppression and limited career prospects.
 *   - Alternative Research Paradigms: Victims of the 'calm', finding it difficult to gain traction and acceptance.
 *   - Established Antarctic Research Institutions: May be both beneficiaries and constrained by the 'calm', seeking order but potentially stifling innovation.
 *   - Analytical Observer: Sees the 'calm' as a piton, a once-functional mechanism now hindering progress.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_calm_protocol_suppression, 0.6).
domain_priors:suppression_score(the_calm_protocol_suppression, 0.7).
domain_priors:theater_ratio(the_calm_protocol_suppression, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_calm_protocol_suppression, extractiveness, 0.6).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(the_calm_protocol_suppression, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_calm_protocol_suppression, snare).
narrative_ontology:human_readable(the_calm_protocol_suppression, "The 'Calm' of Antarctic Protocol").
narrative_ontology:topic_domain(the_calm_protocol_suppression, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_victim(the_calm_protocol_suppression, new_researchers).
narrative_ontology:constraint_victim(the_calm_protocol_suppression, alternative_research_paradigms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% New researchers entering the field experience the 'calm' as a suppression of alternative viewpoints and research approaches, creating a snare that limits their career prospects if they deviate from established norms.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Alternative research paradigms find it difficult to gain traction and acceptance due to the dominance of established norms and methodologies. This is experienced as a constraint on their development and application within the Antarctic research community.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Established institutions may see the 'calm' as a necessary coordination mechanism for maintaining order and focus in research efforts, but also recognize the potential for stifling innovation and limiting diverse perspectives. They are both beneficiaries and partially constrained.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer may view the 'calm' as a piton - a once-functional mechanism for coordinating research efforts that has become a source of inertia and resistance to change, persisting due to institutional inertia.
constraint_indexing:constraint_classification(the_calm_protocol_suppression, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_calm_protocol_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_calm_protocol_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_calm_protocol_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_calm_protocol_suppression, TR),
    TR >= 0.70.

:- end_tests(the_calm_protocol_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): The 'calm' extracts intellectual diversity and innovation from the research community by suppressing alternative viewpoints and limiting career prospects for those who deviate from established norms. Suppression (0.70): The pressure to conform is high, creating a significant barrier to entry for new researchers and alternative research paradigms. Theater Ratio (0.30): The performative aspect is relatively low, as the 'calm' operates more through subtle pressure and unspoken expectations than through formal rules or procedures.
 *
 * PERSPECTIVAL GAP:
 *   New researchers and alternative research paradigms experience the 'calm' as a snare, limiting their career prospects and suppressing their development. Established institutions may see it as a tangled rope, balancing the need for order with the potential for stifling innovation. Analytical observers might view it as a piton, a degraded coordination mechanism persisting due to inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   The victims (new researchers, alternative paradigms) experience the 'calm' as high extraction, as they are forced to conform or risk their careers and development. Established institutions experience it as a mix of coordination and extraction, as they benefit from the order it creates but are also constrained by the limitations it imposes.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'calm' prevents mislabeling coordination as pure extraction (or vice versa) by recognizing that the suppression of alternative viewpoints is not solely a coordination problem (ensuring order and focus) or a pure extraction mechanism (deliberately suppressing dissent). Instead, it represents a complex interplay of both, with unintended consequences for new researchers and alternative research paradigms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_intentionality,
    'Is the suppression of alternative viewpoints an intentional strategy employed by established researchers, or an unintended consequence of institutional norms and power structures?',
    'Conducting surveys and interviews with researchers at different career stages, examining historical documents and institutional policies, and analyzing power dynamics within the Antarctic research community.',
    'If intentional: supports the classification as a deliberate snare. If unintended: suggests the presence of an emergent tangled rope or a degraded coordination mechanism (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_intentionality, empirical, 'The degree of intentionality behind the suppression of alternative viewpoints.').

omega_variable(
    impact_on_scientific_progress,
    'To what extent does the ''calm'' hinder scientific progress and innovation within Antarctic research?',
    'Analyzing the diversity of research topics and methodologies employed over time, comparing research outcomes with those in other fields, and assessing the impact of alternative research paradigms on scientific understanding.',
    'If significant: reinforces the classification as a snare and underscores the need for change. If limited: suggests the presence of a functional coordination mechanism (rope) or a temporary problem (scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_scientific_progress, empirical, 'The impact of the ''calm'' on scientific progress within Antarctic research.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_calm_protocol_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(the__tr_t0, the_calm_protocol_suppression, theater_ratio, 0, 0.2).
narrative_ontology:measurement(the__tr_t5, the_calm_protocol_suppression, theater_ratio, 5, 0.3).
narrative_ontology:measurement(the__tr_t10, the_calm_protocol_suppression, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(the__be_t0, the_calm_protocol_suppression, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(the__be_t5, the_calm_protocol_suppression, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(the__be_t10, the_calm_protocol_suppression, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_calm_protocol_suppression, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
