% ============================================================================
% CONSTRAINT STORY: technocratic_overreach
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_technocratic_overreach, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: technocratic_overreach
 * human_readable: The Rule of the Expert
 * domain: political/technological
 * * SUMMARY:
 * A scenario where administrative or technical experts impose restrictive
 * constraints under the guise of "optimization" or "safety," effectively
 * removing the subject's ability to participate in decision-making.
 * * KEY AGENTS:
 * - The Citizen: Subject (Powerless) - Required to follow "optimized" mandates they cannot audit.
 * - The System Architect: Beneficiary (Institutional) - Enforces the technical Rope.
 * - The Ethicist: Auditor (Analytical) - Identifies the extraction of agency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(technocratic_overreach, 0.64). % High: Extracts agency and political liberty.
domain_priors:suppression_score(technocratic_overreach, 0.78).   % High: Complexity makes alternatives invisible.
domain_priors:theater_ratio(technocratic_overreach, 0.35).       % Moderate: The technical function is real, but the "intent" is often theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(technocratic_overreach, extractiveness, 0.64).
narrative_ontology:constraint_metric(technocratic_overreach, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(technocratic_overreach, theater_ratio, 0.35).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(technocratic_overreach, tangled_rope).
narrative_ontology:human_readable(technocratic_overreach, "The Rule of the Expert").
narrative_ontology:topic_domain(technocratic_overreach, "political/technological").

% Binary flags
domain_priors:requires_active_enforcement(technocratic_overreach). % Required for Tangled Rope

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(technocratic_overreach, system_architects).
narrative_ontology:constraint_victim(technocratic_overreach, citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the individual, the overreach is a Snare—a trap of "necessary" rules that
% cannot be challenged without expert credentials.
constraint_indexing:constraint_classification(technocratic_overreach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% To the state/architect, this is a Rope—the only way to manage modern
% complexity safely and efficiently.
constraint_indexing:constraint_classification(technocratic_overreach, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid: The technical coordination is functional (Rope), but
% the removal of democratic consent is extractive (Snare).
constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_overreach_tests).

test(perspectival_gap) :-
    % Verify the shift from Snare (Powerless) to Rope (Institutional).
    constraint_indexing:constraint_classification(technocratic_overreach, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_overreach, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(technocratic_overreach),
    narrative_ontology:constraint_beneficiary(technocratic_overreach, _), % Implies has_coordination_function
    narrative_ontology:constraint_victim(technocratic_overreach, _).       % Implies has_asymmetric_extraction

:- end_tests(technocratic_overreach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.64) reflects the "agency tax." The subject pays for
 * "optimization" by losing the right to choose sub-optimal but free paths. The
 * high suppression (0.78) comes from the opacity of the technical systems, which
 * makes alternatives seem non-existent or dangerously naive.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the "Tangled Rope" classification for auditors.
 * This prevents the system from mislabeling the overreach as an unavoidable
 * "Mountain" of technical necessity; it identifies that the complexity is being
 * used as a tool for extraction, even while providing a genuine coordination benefit.
 * The presence of both beneficiaries and victims is the key structural indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_algorithmic_bias,
    'Is the technical constraint truly objective, or is it a "Snare" designed to favor the Architect?',
    'Audit of algorithmic weights vs. public benefit metrics.',
    'If biased: Pure Snare; If objective: A high-tension Rope/Scaffold.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(technocratic_overreach, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution. This system began with
% lower extraction and functional focus, but over time, extraction increased
% as the system became entrenched and its maintenance more performative.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(to_tr_t0, technocratic_overreach, theater_ratio, 0, 0.15).
narrative_ontology:measurement(to_tr_t5, technocratic_overreach, theater_ratio, 5, 0.25).
narrative_ontology:measurement(to_tr_t10, technocratic_overreach, theater_ratio, 10, 0.35).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(to_ex_t0, technocratic_overreach, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(to_ex_t5, technocratic_overreach, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(to_ex_t10, technocratic_overreach, base_extractiveness, 10, 0.64).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system is an enforcement mechanism for "optimized" rules.
narrative_ontology:coordination_type(technocratic_overreach, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */