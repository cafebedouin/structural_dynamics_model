% ============================================================================
% CONSTRAINT STORY: taiwan_strait_hegemony_shift
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_taiwan_strait_hegemony_shift, []).

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
 * * constraint_id: taiwan_strait_hegemony_shift
 * human_readable: The Taiwan Strait Energy & Logistics Chokepoint
 * domain: political/economic
 * * SUMMARY:
 * This constraint models the geopolitical "lock" created by a hypothetical
 * hegemonic control of Taiwan. By occupying this anchor of the "First Island
 * Chain," the hegemon gains direct control over the sea lines of communication
 * (SLOCs) that provide the vast majority of energy and trade for neighboring
 * states like Japan and South Korea. This physical reality creates a
 * "Client State Gravitational Pull," forcing neighbors to subordinate their
 * foreign policy to the hegemon to ensure national survival.
 * * KEY AGENTS:
 * - Neighboring State Leadership (e.g., Japan): Subject (Powerless)
 * - Hegemonic Naval Command: Beneficiary (Institutional)
 * - Geopolitical Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale (Extraction): 0.90. Represents the near-total extraction of
% strategic autonomy. A nation's existence becomes "permission-based."
domain_priors:base_extractiveness(taiwan_strait_hegemony_shift, 0.90).
% Rationale (Suppression): 0.80. High suppression of alternative security
% architectures (e.g., alliances) as the physical barrier renders them
% logistically unviable.
domain_priors:suppression_score(taiwan_strait_hegemony_shift, 0.80).
% Rationale (Theater): 0.10. This is a highly functional system of control,
% not a performative one.
domain_priors:theater_ratio(taiwan_strait_hegemony_shift, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, extractiveness, 0.90).
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The hegemon claims this is a necessary mechanism for regional stability.
narrative_ontology:constraint_claim(taiwan_strait_hegemony_shift, tangled_rope).

% Binary flags
% Requires active enforcement (naval patrols, ADIZ control, etc.).
domain_priors:requires_active_enforcement(taiwan_strait_hegemony_shift).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(taiwan_strait_hegemony_shift, regional_hegemon).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, japanese_sovereign_autonomy).
narrative_ontology:constraint_victim(taiwan_strait_hegemony_shift, south_korean_security_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% For Japan, the hegemon's control of Taiwan is an unchangeable feature of
% geography. They cannot negotiate with a blockade at their doorstep; they
% must accept the new terrain. The extraction is felt as a natural law.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The hegemon views this control as a Rope—a functional coordination mechanism.
% It allows them to "weave" the regional economy into a new hierarchy, using
% the threat of SLOC disruption to keep neighbors aligned.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a system that has a genuine coordination function (for the
% hegemon) but also imposes severe asymmetric extraction on its victims. It
% requires active enforcement to maintain. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_strait_hegemony_shift_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    assertion(TypePowerless == mountain),
    assertion(TypeInstitutional == rope).

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict into a Tangled Rope.
    constraint_indexing:constraint_classification(taiwan_strait_hegemony_shift, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(taiwan_strait_hegemony_shift, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(taiwan_strait_hegemony_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a scenario of maximum geopolitical leverage. The base
 * extractiveness of 0.90 represents the extraction of national sovereignty
 * itself. The suppression of 0.80 reflects the foreclosure of all meaningful
 * strategic alternatives for affected nations.
 *
 * The key insight is the perspectival gap. For Japan (powerless), the new
 * reality is a Mountain—an immutable fact of geography they must navigate.
 * For the hegemon (institutional), it's a Rope—a tool for coordinating
 * regional order. The analytical observer must reconcile these views. Because
 * the system has a clear coordination function (for the beneficiary), clear
 * asymmetric extraction (on the victim), and requires active enforcement, it
 * is classified as a Tangled Rope. This prevents the system from mislabeling
 * the hegemon's coordination as pure extraction (Snare) or ignoring the
 * victim's plight.
 *
 * [RESOLVED MANDATROPHY] The high extraction is not a simple Snare because
 * it is coupled to a functional (albeit coercive) system of regional
 * coordination, making it a Tangled Rope from an objective standpoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_taiwan_strait_hegemony_shift,
    'Can affected nations develop sufficient energy/economic autonomy (e.g., nuclear power, alternative trade routes) to mitigate the chokepoint''s leverage?',
    'Analysis of 2050 energy independence plans and viability of non-Strait logistics (e.g., Arctic routes) vs. baseline industrial import requirements.',
    'If yes, the constraint degrades from Tangled Rope to a less coercive Rope. If no, it solidifies as a permanent feature of the regional order.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(taiwan_strait_hegemony_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the constraint intensifying over a 10-year period following
% the initial geopolitical shift. Extraction grows as the hegemon solidifies
% control and institutionalizes its leverage. Theater remains low.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(tshs_tr_t0, taiwan_strait_hegemony_shift, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tshs_tr_t5, taiwan_strait_hegemony_shift, theater_ratio, 5, 0.08).
narrative_ontology:measurement(tshs_tr_t10, taiwan_strait_hegemony_shift, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(tshs_ex_t0, taiwan_strait_hegemony_shift, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(tshs_ex_t5, taiwan_strait_hegemony_shift, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(tshs_ex_t10, taiwan_strait_hegemony_shift, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The coordination is achieved through the threat and application of force.
narrative_ontology:coordination_type(taiwan_strait_hegemony_shift, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */