% ============================================================================
% CONSTRAINT STORY: ontological_friction_resolution
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [DRAFT]
% ============================================================================

:- module(constraint_ontological_friction_resolution, []).

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
 *   constraint_id: ontological_friction_resolution
 *   human_readable: The Chaste Fire of Truth
 *   domain: metaphysics/identity_resolution
 *
 * SUMMARY:
 *   This constraint represents the "chaste fire of pain" that arises when the
 *   "vaporous veil of smiles" (the mask) is stripped away to reveal the "bare
 *   simplicity of truth". The confrontation with reality often entails
 *   discomfort as illusions are shattered and preconceived notions are
 *   challenged. This process, while painful, is essential for ontological
 *   clarity and self-knowledge.
 *
 * KEY AGENTS:
 *   - Ontological Clarity: Primary beneficiary (institutional/arbitrage) - the end result of the process
 *   - Self-Knowledge: Secondary beneficiary (moderate/constrained) - grows through the process
 *   - Ontological Illusions: Primary victim (powerless/trapped) - are destroyed by the process
 *   - Self-Deception: Secondary victim (powerless/trapped) - diminishes, even if unwillingly.
 *   - Individual seeking truth (moderate/constrained) - can benefit, but may also be hurt by the process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ontological_friction_resolution, 0.6).
domain_priors:suppression_score(ontological_friction_resolution, 0.7).
domain_priors:theater_ratio(ontological_friction_resolution, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ontological_friction_resolution, extractiveness, 0.6).
narrative_ontology:constraint_metric(ontological_friction_resolution, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ontological_friction_resolution, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ontological_friction_resolution, tangled_rope).
narrative_ontology:human_readable(ontological_friction_resolution, "The Chaste Fire of Truth").
narrative_ontology:topic_domain(ontological_friction_resolution, "metaphysics/identity_resolution").

domain_priors:requires_active_enforcement(ontological_friction_resolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, ontological_clarity).
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, self_knowledge).
narrative_ontology:constraint_victim(ontological_friction_resolution, ontological_illusions).
narrative_ontology:constraint_victim(ontological_friction_resolution, self_deception).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The self-deceived are trapped within their own illusions, bearing the full cost of ontological friction when those illusions are challenged. They have no easy exit and often resist truth due to psychological defenses.
constraint_indexing:constraint_classification(ontological_friction_resolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% The individual seeking truth is both constrained by the difficulty of overcoming ingrained beliefs and benefits from the clarity and self-knowledge gained through ontological friction. There is active enforcement through cognitive dissonance and the need for internal consistency.
constraint_indexing:constraint_classification(ontological_friction_resolution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% Philosophical traditions benefit from the chaste fire of truth, as it refines and strengthens ontological frameworks. They can arbitrage between different interpretations and adapt to new insights. The benefits outweigh the cost.
constraint_indexing:constraint_classification(ontological_friction_resolution, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% From an analytical perspective, the resolution of ontological friction is a complex process involving both extraction (from illusion) and coordination (towards truth). The analytical observer understands the inherent tension between the need for stability and the imperative for intellectual honesty.
constraint_indexing:constraint_classification(ontological_friction_resolution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ontological_friction_resolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ontological_friction_resolution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ontological_friction_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High, as the process is often painful. Suppression (0.70): High, due to resistance to change. Theater Ratio (0.20): Low, the process is not performative but genuine transformation is intended.
 *
 * PERSPECTIVAL GAP:
 *   The self-deceived perceive this constraint as a snare, because they are trapped and cannot escape the friction of challenged beliefs. The individual seeking truth is constrained but can benefit, seeing the constraint as tangled rope. Philosophical traditions can adapt and benefit, viewing it as a rope that strengthens and refines understanding. The analytical observer recognizes the complex interplay of extraction and coordination, understanding the tangled rope nature of the phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality depends on the relationship to the truth. Those who resist bear the cost, those who embrace benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_truth,
    'Is truth an objective reality or a subjective construct?',
    'Ongoing philosophical debate and empirical investigation into the nature of consciousness and reality.',
    'If truth is objective, ontological friction is a necessary process of aligning with reality. If truth is subjective, ontological friction is a negotiation of shared meanings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_truth, conceptual, 'The fundamental nature of truth itself.').

omega_variable(
    cognitive_biases,
    'To what extent are cognitive biases inherent limitations or malleable aspects of human cognition?',
    'Neuroscientific research on the neural basis of cognitive biases and psychological interventions to mitigate them.',
    'If biases are inherent, ontological friction is a constant struggle against intrinsic limitations. If biases are malleable, ontological friction can be reduced through education and self-awareness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_biases, empirical, 'The degree to which cognitive biases can be overcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ontological_friction_resolution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(onto_tr_t0, ontological_friction_resolution, theater_ratio, 0, 0.3).
narrative_ontology:measurement(onto_tr_t5, ontological_friction_resolution, theater_ratio, 5, 0.25).
narrative_ontology:measurement(onto_tr_t10, ontological_friction_resolution, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(onto_be_t0, ontological_friction_resolution, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(onto_be_t5, ontological_friction_resolution, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(onto_be_t10, ontological_friction_resolution, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
