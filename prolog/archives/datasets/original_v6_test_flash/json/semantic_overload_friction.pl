% ============================================================================
% CONSTRAINT STORY: semantic_overload_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semantic_overload_friction, []).

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
 *   constraint_id: semantic_overload_friction
 *   human_readable: The Semantic Saturation Threshold
 *   domain: technological/social
 *
 * SUMMARY:
 *   The "Friction of Jargon" arises as specialized domains (e.g., law, tech,
 *   or academia) mature, leading to increasingly dense language. This creates
 *   barriers to entry for new entrants and the uninitiated public, while
 *   benefiting domain experts and incumbent institutions who leverage the
 *   exclusive language for signaling and power. The constraint embodies the
 *   trade-off between efficient communication within the domain and
 *   accessibility to those outside it.
 *
 * KEY AGENTS:
 *   - domain_experts: primary beneficiaries (moderate/constrained)
 *   - incumbent_institutions: primary beneficiaries (institutional/arbitrage)
 *   - new_entrants: primary victims (powerless/trapped)
 *   - uninitiated_public: secondary victims (powerless/trapped)
 *   - analytical_observer: sees mixed effects (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semantic_overload_friction, 0.55).
domain_priors:suppression_score(semantic_overload_friction, 0.65).
domain_priors:theater_ratio(semantic_overload_friction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semantic_overload_friction, extractiveness, 0.55).
narrative_ontology:constraint_metric(semantic_overload_friction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(semantic_overload_friction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semantic_overload_friction, tangled_rope).
narrative_ontology:human_readable(semantic_overload_friction, "The Semantic Saturation Threshold").
narrative_ontology:topic_domain(semantic_overload_friction, "technological/social").

domain_priors:requires_active_enforcement(semantic_overload_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semantic_overload_friction, domain_experts).
narrative_ontology:constraint_beneficiary(semantic_overload_friction, incumbent_institutions).
narrative_ontology:constraint_victim(semantic_overload_friction, new_entrants).
narrative_ontology:constraint_victim(semantic_overload_friction, uninitiated_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of new entrants and the uninitiated public, who are trapped by the high barrier to entry created by the jargon.
constraint_indexing:constraint_classification(semantic_overload_friction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective of domain practitioners who are both beneficiaries and victims of the jargon. They benefit from the exclusivity it creates, but are also constrained by the need to constantly learn and use it.
constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of incumbent institutions who benefit from the jargon as it reinforces their position and creates barriers to entry for competitors. They can arbitrage this position globally.
constraint_indexing:constraint_classification(semantic_overload_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical perspective sees a tangled rope: jargon facilitates specialized communication but also creates a barrier to entry and understanding.
constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semantic_overload_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semantic_overload_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semantic_overload_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semantic_overload_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(semantic_overload_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Jargon extracts from new entrants in the form of time/effort to learn the language and lost opportunities from misunderstanding. Suppression (0.65): High. Alternatives are suppressed because precision is valued within the domain and accessibility is undervalued. Theater ratio (0.30): Low. The primary function is to communicate efficiently with experts; performance takes a backseat.
 *
 * PERSPECTIVAL GAP:
 *   The new entrant sees a snare: they are trapped by the jargon and cannot participate. The domain expert sees a tangled rope: they benefit from the exclusivity it creates but are also constrained by the need to constantly learn and use it. Incumbent institutions see a rope: it reinforces their position and creates barriers to entry. The analytical observer sees a tangled rope: it facilitates specialized communication but also creates a barrier to entry and understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (domain experts and incumbents) benefit by leveraging exclusive jargon, leading to lower derived d values and extractiveness. Victims (new entrants and uninitiated public) are trapped by the jargon, resulting in higher d values and greater perceived extraction. Analytical observers sees a mixed effect based on exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's classification as a Tangled Rope prevents mislabeling the jargon as either pure coordination or pure extraction. The jargon, while enabling communication, also creates a barrier to entry. Recognizing this dual nature resolves the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exoteric_vs_esoteric,
    'What is the ratio of exoteric to esoteric language in the target domain?',
    'Quantitative analysis of text and speech in the domain to determine the frequency of specialized terms vs. common vocabulary.',
    'If the ratio is low, the constraint will be more of a snare; if high, it will be more of a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exoteric_vs_esoteric, empirical, 'Quantifies the balance between accessible and specialized language.').

omega_variable(
    cognitive_load,
    'How much cognitive load does mastering the jargon impose on new entrants?',
    'Psychological studies to measure the time and effort required to learn and use the jargon effectively.',
    'High cognitive load strengthens the snare aspect; lower load may indicate effective scaffolding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load, empirical, 'Measures the effort required to internalize specialized language.').

omega_variable(
    alternative_communication,
    'How effective are alternative communication methods (e.g., plain language summaries) in conveying the same information without jargon?',
    'Comparative studies to assess the accuracy and completeness of information transfer using different communication methods.',
    'More effective alternatives weaken the snare; ineffective alternatives reinforce the extraction by incumbent institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_communication, empirical, 'Assesses the viability of jargon-free communication methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semantic_overload_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sema_tr_t0, semantic_overload_friction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sema_tr_t5, semantic_overload_friction, theater_ratio, 5, 0.2).
narrative_ontology:measurement(sema_tr_t10, semantic_overload_friction, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sema_be_t0, semantic_overload_friction, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sema_be_t5, semantic_overload_friction, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(sema_be_t10, semantic_overload_friction, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semantic_overload_friction, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
