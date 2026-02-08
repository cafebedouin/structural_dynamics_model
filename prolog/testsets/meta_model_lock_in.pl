% ============================================================================
% CONSTRAINT STORY: meta_model_lock_in
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_meta_model_lock_in, []).

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
 * * constraint_id: meta_model_lock_in
 * human_readable: The Ontological Cage
 * domain: technological/cognitive
 * * SUMMARY:
 * A foundational AI model or classification framework (the Meta-Model) becomes
 * so deeply integrated into social and technical infrastructure that
 * alternative ways of perceiving or organizing reality are suppressed. This
 * "Rope" for achieving global semantic interoperability becomes a "Snare" for
 * the user, as the system liquidates the agency to use non-standard logic or
 * novel ontologies, trapping the subject in a territory where only "model-legible"
 * thoughts can be processed or economically rewarded.
 * * KEY AGENTS:
 * - Independent Thinker: Subject (Powerless)
 * - Model Provider: Beneficiary (Institutional)
 * - Epistemic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction reflects the siphoning of cognitive diversity and agency.
domain_priors:base_extractiveness(meta_model_lock_in, 0.86).
% Divergent logic is suppressed as "incompatible" or "noise."
domain_priors:suppression_score(meta_model_lock_in, 0.79).
% High theater: "Fine-tuning" options that mask the rigid underlying ontology.
domain_priors:theater_ratio(meta_model_lock_in, 0.88).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(meta_model_lock_in, extractiveness, 0.86).
narrative_ontology:constraint_metric(meta_model_lock_in, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(meta_model_lock_in, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination tool for global semantic interoperability.
narrative_ontology:constraint_claim(meta_model_lock_in, tangled_rope).

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(meta_model_lock_in). % Enforcement via API deprecation, incompatibility, and network effects.
narrative_ontology:constraint_beneficiary(meta_model_lock_in, model_providers). % Derives coordination function
narrative_ontology:constraint_victim(meta_model_lock_in, independent_thinkers). % Derives asymmetric extraction

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The thinker is trapped: they must use the Meta-Model to communicate or
% work, but doing so liquidates their primary intellectual agency.
constraint_indexing:constraint_classification(meta_model_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Provider views the model as a Rope—the essential coordination
% substrate for global-scale human-AI alignment and data liquidity.
constraint_indexing:constraint_classification(meta_model_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: a genuine coordination function (beneficiaries exist)
% coupled with severe asymmetric extraction (victims exist) and active enforcement.
constraint_indexing:constraint_classification(meta_model_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_model_lock_in_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(meta_model_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_model_lock_in, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(tangled_rope_structural_conditions) :-
    % Verify that the conditions for a Tangled Rope classification are met.
    narrative_ontology:constraint_beneficiary(meta_model_lock_in, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(meta_model_lock_in, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(meta_model_lock_in).

:- end_tests(meta_model_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extraction score (0.86) is high because the constraint siphons off
 * the user's cognitive agency and the value of alternative ontologies,
 * converting them into model coherence and market power for the provider.
 * The suppression score (0.79) reflects how network effects and API dominance
 * make it prohibitively costly to operate outside the model's logic. The high
 * theater ratio (0.88) points to performative "customization" features that
 * offer an illusion of control while the core ontology remains rigid.
 *
 * The Perspectival Gap is stark: for the provider, it's a Rope that creates a
 * vast, interoperable market. For the user, it's a Snare that forces them to
 * think in "model-legible" terms, sacrificing novel thought for compatibility.
 *
 * * MANDATROPHY ANALYSIS:
 * This constraint is a classic case of Mandatrophy, where a system claiming to be
 * pure coordination (Rope) is actually high-extraction (Snare). The ambiguity is
 * resolved by the Tangled Rope classification from the analytical perspective.
 * This classification correctly identifies that the system possesses BOTH a genuine
 * coordination function (benefiting providers) AND severe, asymmetrically applied
 * extraction (harming independent thinkers). By acknowledging the hybrid nature,
 * the system avoids misclassifying it as a pure Rope (ignoring the victims) or a
 * pure Snare (ignoring the coordination benefits that sustain it). This is a
 * [RESOLVED MANDATROPHY].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_model_plasticity,
    'Is the lock-in a temporary artifact of the current technology (a constructed Snare) or an emergent, inescapable property of large-scale semantic networks (a Mountain of information economics)?',
    'Longitudinal study of forked/alternative models to see if they can achieve sustainable economic niches or are inevitably re-absorbed or extinguished by the dominant model.',
    'If alternatives thrive -> Snare. If all alternatives fail -> Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(meta_model_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint exhibits extraction_accumulation and metric_substitution (rising
% theater) as network effects solidified its dominance. It began as a useful
% coordination tool and degraded into an extractive trap.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(mmli_tr_t0, meta_model_lock_in, theater_ratio, 0, 0.20).
narrative_ontology:measurement(mmli_tr_t5, meta_model_lock_in, theater_ratio, 5, 0.60).
narrative_ontology:measurement(mmli_tr_t10, meta_model_lock_in, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(mmli_ex_t0, meta_model_lock_in, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(mmli_ex_t5, meta_model_lock_in, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(mmli_ex_t10, meta_model_lock_in, base_extractiveness, 10, 0.86).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The model provides a global standard for structuring information.
narrative_ontology:coordination_type(meta_model_lock_in, information_standard).

% Network relationships (structural influence edges)
% The model's dominance directly impacts how knowledge is measured and valued.
narrative_ontology:affects_constraint(meta_model_lock_in, academic_publishing_metrics).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */