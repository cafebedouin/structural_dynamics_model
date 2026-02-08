% ============================================================================
% CONSTRAINT STORY: harm_principle_liberty
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_harm_principle_liberty, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: harm_principle_liberty
 * human_readable: Mill's Harm Principle as a Social Constraint
 * domain: political/social
 * * SUMMARY:
 * John Stuart Mill's "On Liberty" proposes the Harm Principle as a meta-constraint:
 * society may only coerce an individual to prevent harm to others. This story
 * models the "tyranny of the majority" and the "despotism of custom" that the
 * principle is designed to regulate. The constraint is the social pressure for
 * conformity itself, which Mill's principle attempts to frame as a coordination
 * mechanism (a Rope) but which is often experienced as a coercive trap (a Snare).
 * * KEY AGENTS:
 * - The Non-Conformist: Subject (Powerless) victim of social stigma.
 * - The Majority (Public Opinion): Institutional enforcer of custom.
 * - The Liberal Statesman: Analytical observer attempting to apply the principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction: Society extracts conformity and a "fair share" of burdens for
% collective security. This is significant but not totalizing.
domain_priors:base_extractiveness(harm_principle_liberty, 0.50).
% Suppression: Mill argues the "despotism of custom" is a powerful force that
% suppresses originality and experiments in living.
domain_priors:suppression_score(harm_principle_liberty, 0.80).
% Theater: The principle is largely functional, not performative.
domain_priors:theater_ratio(harm_principle_liberty, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(harm_principle_liberty, extractiveness, 0.50).
narrative_ontology:constraint_metric(harm_principle_liberty, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(harm_principle_liberty, theater_ratio, 0.10).

% Constraint self-claim: The principle claims to be a pure coordination rule for liberty.
narrative_ontology:constraint_claim(harm_principle_liberty, tangled_rope).

% Binary flags
% Enforcement is required both by law (for direct harm) and "moral repression"
% (for social custom). This is a key condition for Tangled Rope.
domain_priors:requires_active_enforcement(harm_principle_liberty).

% Structural property derivation hooks:
% The system has a clear coordination function (social stability) and
% asymmetric extraction (stifling of eccentrics).
narrative_ontology:constraint_beneficiary(harm_principle_liberty, collective_stability).
narrative_ontology:constraint_victim(harm_principle_liberty, eccentrics_and_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE NON-CONFORMIST (SNARE)
% For the eccentric individual, social custom is a coercive trap. The high
% suppression and extraction are felt directly, with no viable exit.
constraint_indexing:constraint_classification(harm_principle_liberty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MAJORITY (MOUNTAIN)
% For the institutional power of the majority, custom is not a choice but a
% self-evident law of nature. It is an unchangeable feature of the social world.
constraint_indexing:constraint_classification(harm_principle_liberty, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (preventing harm) and the
% asymmetric extraction (suppressing non-conformity). It requires active
% enforcement and has clear beneficiaries and victims, making it a Tangled Rope.
constraint_indexing:constraint_classification(harm_principle_liberty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harm_principle_liberty_tests).

test(perspectival_gap) :-
    % Verify the gap between the Non-Conformist (Snare), the Majority (Mountain),
    % and the Analyst (Tangled Rope).
    constraint_indexing:constraint_classification(harm_principle_liberty, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harm_principle_liberty, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(harm_principle_liberty, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three conditions for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(harm_principle_liberty),
    narrative_ontology:constraint_beneficiary(harm_principle_liberty, _),
    narrative_ontology:constraint_victim(harm_principle_liberty, _).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(harm_principle_liberty, extractiveness, E),
    E >= 0.46.

:- end_tests(harm_principle_liberty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of Mill's analysis is the perspectival gap. What the majority
 * experiences as an invisible, natural "Mountain" of custom is felt by the
 * non-conformist as a highly coercive "Snare." The base extractiveness (0.50)
 * reflects Mill's admission that society can demand "labour and sacrifices"
 * for collective defense. The high suppression (0.80) reflects his warning
 * about the "despotism of custom" being as powerful as law.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Classifying this as a Tangled Rope from the analytical perspective is
 * crucial. A naive analysis might see only the coordination function and
 * label it a Rope, or see only the coercion and label it a Snare. The
 * Tangled Rope classification correctly identifies that it is *both*: a
 * necessary coordination mechanism that carries an inherent, asymmetric
 * cost of suppressing individuality. This prevents misclassification and
 * captures the central tension of liberal governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_harm_principle_liberty,
    "Where is the boundary between 'self-regarding' and 'other-regarding' acts in a hyper-connected, interdependent society?",
    "Empirical analysis of network effects and externalities of seemingly private actions (e.g., public health choices, financial decisions).",
    "If the boundary is highly porous, the principle's scope for liberty shrinks, and it functions more like a Snare. If the boundary is firm, it functions more like a Rope.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(harm_principle_liberty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint models a slight increase
% in both extraction and performative aspects over time, as social norms
% become more entrenched and their original justifications fade.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(harm_principle_liberty_tr_t0, harm_principle_liberty, theater_ratio, 0, 0.05).
narrative_ontology:measurement(harm_principle_liberty_tr_t5, harm_principle_liberty, theater_ratio, 5, 0.08).
narrative_ontology:measurement(harm_principle_liberty_tr_t10, harm_principle_liberty, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(harm_principle_liberty_ex_t0, harm_principle_liberty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(harm_principle_liberty_ex_t5, harm_principle_liberty, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(harm_principle_liberty_ex_t10, harm_principle_liberty, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The Harm Principle functions as a high-level rule governing other rules,
% making it a form of enforcement mechanism for a liberal social order.
narrative_ontology:coordination_type(harm_principle_liberty, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */