% ============================================================================
% CONSTRAINT STORY: dark_patterns_manipulation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_dark_patterns_manipulation, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: dark_patterns_manipulation
 * human_readable: Dark Patterns (Interface Coercion)
 * domain: technological/economic
 * * SUMMARY:
 * Dark patterns exploit cognitive biases (like loss aversion or the default
 * effect) to steer user behavior toward outcomes that benefit the platform
 * but harm the user. They are the "shadow side" of Information Foraging Theory,
 * turning the interface from a transparent tool into a psychological Snare.
 * * KEY AGENTS:
 * - The User: Subject (Powerless)
 * - The Platform/Growth Hacker: Beneficiary (Institutional)
 * - The Ethical Designer/Regulator: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(dark_patterns_manipulation, 0.85). % High; designed to extract value (money, data, attention) through deception.
domain_priors:suppression_score(dark_patterns_manipulation, 0.90).   % High; exit options (e.g., 'Cancel Subscription') are intentionally hidden or made difficult.
domain_priors:theater_ratio(dark_patterns_manipulation, 0.10).       % Low; the manipulation is functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(dark_patterns_manipulation, extractiveness, 0.85).
narrative_ontology:constraint_metric(dark_patterns_manipulation, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(dark_patterns_manipulation, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Claims to be a constructed feature for optimization or better user flow.
narrative_ontology:constraint_claim(dark_patterns_manipulation, tangled_rope).
narrative_ontology:human_readable(dark_patterns_manipulation, "Dark Patterns (Interface Coercion)").

% Binary flags
domain_priors:requires_active_enforcement(dark_patterns_manipulation). % Requires active design and implementation by the platform.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(dark_patterns_manipulation, platform_operator).
narrative_ontology:constraint_victim(dark_patterns_manipulation, platform_user).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE USER (SNARE)
% For a user trying to delete an account, the interface is a "Snare."
% The "Confirm Deletion" button is grayed out, hidden behind five menus,
% or requires a physical phone call. The platform strangles their
% exit option to keep their "active user" metric high.
constraint_indexing:constraint_classification(dark_patterns_manipulation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PLATFORM / GROWTH HACKER (ROPE)
% To the product manager, these patterns are a "Rope." They are the
% coordination tools used to "nudge" users toward the "desired"
% business outcome (e.g., higher subscription retention). They see it as a
% necessary strategy for survival in a hyper-competitive attention economy.
constraint_indexing:constraint_classification(dark_patterns_manipulation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ETHICAL DESIGNER / REGULATOR (MOUNTAIN)
% The analytical observer sees the potential for these patterns as a "Mountain" of
% psychological vulnerability. Human cognitive biases (loss aversion, default bias)
% are immutable. The fact that they *can* be exploited is a feature of our biological
% nature that designers must either respect or abuse.
constraint_indexing:constraint_classification(dark_patterns_manipulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dark_patterns_manipulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dark_patterns_manipulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_patterns_manipulation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope).

test(analytical_perspective_is_mountain) :-
    constraint_indexing:constraint_classification(dark_patterns_manipulation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(dark_patterns_manipulation, E),
    domain_priors:suppression_score(dark_patterns_manipulation, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(dark_patterns_manipulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a purely extractive mechanism. Extraction (0.85) is high
 * because dark patterns are, by definition, deceptive and non-reciprocal.
 * Suppression (0.90) is high because their function is to obstruct user agency
 * and prevent exit. The perspectival gap is stark: the platform's "Rope" for
 * achieving business goals is the user's "Snare". The analytical view classifies
 * it as "Mountain" because the underlying cognitive biases being exploited are
 * fixed, unchangeable features of human psychology.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction is resolved because the perspectives clearly map the power
 * dynamic. The institutional agent wields the 'Rope' to achieve targets, which
 * becomes a 'Snare' for the powerless user. The 'Mountain' is the underlying,
 * unchangeable human psychology that makes the exploitation possible. The system
 * correctly identifies this as a constructed Snare, not a mis-classified Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dark_patterns_intent,
    "Is the user harm caused by dark patterns an intended predatory outcome or an unforeseen consequence of aggressive optimization?",
    "Internal review of A/B testing documentation and executive communications to determine if user harm was a known and accepted trade-off.",
    "If intended: Confirms a predatory Snare. If unforeseen: It's a recklessly handled Rope that degraded into a Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dark_patterns_manipulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint models the intensification
% of manipulative designs over time as platforms became more aggressive in
% optimizing for retention and conversion.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(dpm_tr_t0, dark_patterns_manipulation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(dpm_tr_t5, dark_patterns_manipulation, theater_ratio, 5, 0.10).
narrative_ontology:measurement(dpm_tr_t10, dark_patterns_manipulation, theater_ratio, 10, 0.10).

% Extraction over time (shows accumulation as techniques are refined):
narrative_ontology:measurement(dpm_ex_t0, dark_patterns_manipulation, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(dpm_ex_t5, dark_patterns_manipulation, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(dpm_ex_t10, dark_patterns_manipulation, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is purely extractive and lacks a genuine coordination function,
% so no Boltzmann data (coordination_type) is declared.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */