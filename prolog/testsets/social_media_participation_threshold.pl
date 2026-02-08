% ============================================================================
% CONSTRAINT STORY: social_media_participation_threshold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_social_media_participation_threshold, []).

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
 * * constraint_id: social_media_participation_threshold
 * human_readable: The 2025 Digital Participation Threshold
 * domain: social/technological
 * * SUMMARY:
 * As of 2025, major social media platforms function as de facto mandatory
 * infrastructure for social and informational participation. High network
 * effects create a significant barrier to exit, making non-participation
 * socially and professionally costly. The constraint is the implicit
 * requirement to maintain a presence on these platforms to access social
 * networks, news, and professional opportunities.
 * * KEY AGENTS:
 * - Young Adult User: Subject (Powerless), for whom platform usage is a
 *   non-negotiable landscape for social survival.
 * - Institutional Marketer: Beneficiary (Institutional), who uses platforms
 *   as a functional coordination mechanism to target demographic segments.
 * - Sociological Researcher: Auditor (Analytical), who tracks the systemic
 *   shifts and extractive properties of the ecosystem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(social_media_participation_threshold, 0.60). % Rationale: High extraction of attention and behavioral data.
domain_priors:suppression_score(social_media_participation_threshold, 0.50).   % Rationale: Moderate suppression of non-digital alternatives due to network effects.
domain_priors:theater_ratio(social_media_participation_threshold, 0.15).       % Rationale: Primarily functional, but with some performative engagement.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(social_media_participation_threshold, extractiveness, 0.60).
narrative_ontology:constraint_metric(social_media_participation_threshold, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(social_media_participation_threshold, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Platforms claim to be pure coordination tools for connecting people.
narrative_ontology:constraint_claim(social_media_participation_threshold, tangled_rope).

% Binary flags
% Rationale: Social enforcement via network effects requires continuous user
% participation, which constitutes a form of active (though decentralized) enforcement.
domain_priors:requires_active_enforcement(social_media_participation_threshold).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(social_media_participation_threshold, platform_architects).
narrative_ontology:constraint_beneficiary(social_media_participation_threshold, demographic_advertisers).
narrative_ontology:constraint_victim(social_media_participation_threshold, user_attention_spans).
narrative_ontology:constraint_victim(social_media_participation_threshold, non_digital_social_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Feels like a Mountain (inescapable) but is metrically a Snare due to high extraction.
% χ = 0.60 (ε) * 1.5 (π(powerless)) * 0.9 (σ(regional)) = 0.81
constraint_indexing:constraint_classification(social_media_participation_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential coordination infrastructure with negligible personal cost.
% χ = 0.60 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.12
constraint_indexing:constraint_classification(social_media_participation_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes both the coordination function and the asymmetric extraction.
% This is the canonical classification for this constraint.
% χ = 0.60 (ε) * 1.15 (π(analytical)) * 1.2 (σ(global)) = 0.828
constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_media_participation_threshold_tests).

test(perspectival_gap) :-
    % Verify the powerless user sees a Snare while the institution sees a Rope.
    constraint_indexing:constraint_classification(social_media_participation_threshold, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_media_participation_threshold, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict by identifying the hybrid nature.
    constraint_indexing:constraint_classification(social_media_participation_threshold, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify that all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(social_media_participation_threshold, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(social_media_participation_threshold, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(social_media_participation_threshold).

:- end_tests(social_media_participation_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores (E=0.60, S=0.50) reflect a system that is highly extractive
 * of user data and attention, while moderately suppressing alternatives through
 * powerful network effects.
 *
 * The Perspectival Gap is stark:
 * - For a powerless user, the high base extraction is amplified by their lack
 *   of power (π=1.5), resulting in an effective extraction (χ) that is firmly
 *   in the Snare category. While it feels like an unchangeable Mountain, its
 *   extractive nature is its defining metric characteristic.
 * - For an institutional beneficiary, their power inverts the extraction (π=-0.2),
 *   making the platform feel like a pure, beneficial coordination tool (Rope).
 *
 * * MANDATROPHY ANALYSIS:
 * The analytical classification of Tangled Rope is critical. A simpler model
 * might classify this as a Snare, ignoring its genuine and powerful coordination
 * function (which beneficiaries experience as a Rope). The Tangled Rope
 * classification correctly identifies the hybrid nature of the constraint: it
 * simultaneously provides a coordination benefit to one group while imposing
 * coercive, asymmetric extraction on another. This prevents the system from
 * mistakenly dismissing the platform's utility while still flagging its harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_social_media_participation_threshold,
    'Is the high suppression score a result of an emergent social consensus (closer to a Mountain) or a deliberately engineered system of behavioral feedback loops (closer to a Snare)?',
    'Comparative analysis of user retention metrics on platforms with and without algorithmic amplification and notification-based engagement systems.',
    'If emergent, policy interventions are less effective. If engineered, regulatory changes to platform design could significantly reduce the constraint''s power.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(social_media_participation_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from a coordination tool
% into an extractive system over a 10-year interval.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(smpt_tr_t0, social_media_participation_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(smpt_tr_t5, social_media_participation_threshold, theater_ratio, 5, 0.10).
narrative_ontology:measurement(smpt_tr_t10, social_media_participation_threshold, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(smpt_ex_t0, social_media_participation_threshold, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(smpt_ex_t5, social_media_participation_threshold, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(smpt_ex_t10, social_media_participation_threshold, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint functions as a piece of global digital infrastructure.
narrative_ontology:coordination_type(social_media_participation_threshold, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */