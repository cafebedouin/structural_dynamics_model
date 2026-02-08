% ============================================================================
% CONSTRAINT STORY: narrative_overfitting
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_narrative_overfitting, []).

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
 * * constraint_id: narrative_overfitting
 * human_readable: The Procrustean Plot
 * domain: social/technological
 * * SUMMARY:
 * A scenario where complex real-world data is forced into a simple, compelling
 * narrative structure to satisfy engagement algorithms or cognitive biases.
 * This "Rope" for making information legible and sharable becomes a "Snare"
 * as the nuances of reality are liquidated to fit the story arc, trapping
 * the subject in a "fictionalized" territory where functional decisions
 * become impossible because the map has been optimized for "likability"
 * rather than accuracy.
 * * KEY AGENTS:
 * - Public Participant: Subject (Powerless)
 * - Narrative Architect (Media/AI): Beneficiary (Institutional)
 * - Epistemic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) reflects the terminal liquidation of objective nuance
% to feed the institutional need for viral "storytelling."
domain_priors:base_extractiveness(narrative_overfitting, 0.87).
domain_priors:suppression_score(narrative_overfitting, 0.76). % Contradictory evidence is suppressed as "plot holes."
domain_priors:theater_ratio(narrative_overfitting, 0.94).    % Extreme theater: performative "story arcs" masking systemic complexity.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(narrative_overfitting, extractiveness, 0.87).
narrative_ontology:constraint_metric(narrative_overfitting, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(narrative_overfitting, theater_ratio, 0.94).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination tool for making sense of the world.
narrative_ontology:constraint_claim(narrative_overfitting, tangled_rope).

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(narrative_overfitting). % Algorithmic amplification and social pressure enforce the narrative.
narrative_ontology:constraint_beneficiary(narrative_overfitting, narrative_architects). % Derives has_coordination_function/1
narrative_ontology:constraint_victim(narrative_overfitting, public_participants). % Derives has_asymmetric_extraction/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The participant is trapped: they must interpret their life through the
% "overfitted" narrative to remain socially legible, liquidating their
% primary sensory agency. χ = 0.87 * 1.5 * 1.0 = 1.305.
constraint_indexing:constraint_classification(narrative_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the narrative as a Rope—the only way to coordinate
% millions of attention-spans in a world of infinite, noisy data.
% χ = 0.87 * -0.2 * 1.2 = -0.2088 (net benefit).
constraint_indexing:constraint_classification(narrative_overfitting, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) and high suppression (0.76) masking as
% essential coordination (Rope). The presence of beneficiaries, victims, and
% active enforcement confirms the Tangled Rope classification.
constraint_indexing:constraint_classification(narrative_overfitting, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_overfitting_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless subject vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(narrative_overfitting, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_overfitting, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(narrative_overfitting, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(narrative_overfitting),
    narrative_ontology:constraint_beneficiary(narrative_overfitting, _),
    narrative_ontology:constraint_victim(narrative_overfitting, _).

:- end_tests(narrative_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the
 * "coordination" of social meaning is achieved by liquidating the
 * informational complexity of reality. Suppression (0.76) is high because
 * algorithms and social dynamics actively punish nuance that contradicts the
 * simplified, viral narrative. The extreme theater ratio (0.94) indicates that
 * the narrative's function is almost entirely performative, yet its high
 * extraction prevents it from being an inert Piton. It is an active,
 * extractive, and highly degraded coordination mechanism.
 *
 * * PERSPECTIVAL GAP:
 * The Public Participant feels a Snare because their reality is hollowed out
 * into a series of tropes. The Narrative Architect sees a Rope because
 * the "story" is the only coordinate system that works for mass mobilization
 * and engagement. The Analytical Observer sees a Tangled Rope, recognizing
 * both the coordination claim and the severe asymmetric extraction it enables.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system resolves this by classifying it as a Tangled Rope. A simpler
 * model might misclassify it as a pure Snare (ignoring the coordination
 * function) or a Piton (ignoring the massive, active extraction due to the
 * high theater score). The Tangled Rope classification correctly identifies
 * it as a coordination mechanism that has been corrupted by extreme,
 * asymmetric extraction, preventing Mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_narrative_overfitting_1,
    'Is the liquidation of nuance a reversible policy choice (Tangled Rope) or an irreversible emergent property of complex information systems (a new kind of Mountain)?',
    'Longitudinal study of information ecosystems after major platform policy changes or de-platforming events to observe if complexity can be restored.',
    'If reversible: Tangled Rope. If irreversible: Mountain, implying a fundamental limit on societal complexity at scale.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(narrative_overfitting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified with the rise of algorithmic media.
% The initial state reflects a pre-social media era with simpler narrative forcing.
% The final state reflects the current hyper-optimized environment.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(narrative_overfitting_tr_t0, narrative_overfitting, theater_ratio, 0, 0.50).
narrative_ontology:measurement(narrative_overfitting_tr_t5, narrative_overfitting, theater_ratio, 5, 0.80).
narrative_ontology:measurement(narrative_overfitting_tr_t10, narrative_overfitting, theater_ratio, 10, 0.94).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(narrative_overfitting_ex_t0, narrative_overfitting, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(narrative_overfitting_ex_t5, narrative_overfitting, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(narrative_overfitting_ex_t10, narrative_overfitting, base_extractiveness, 10, 0.87).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's coordination function is to create a shared standard for
% interpreting reality, however distorted.
narrative_ontology:coordination_type(narrative_overfitting, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */