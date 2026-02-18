% ============================================================================
% CONSTRAINT STORY: naming_as_control
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_naming_as_control, []).

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
 * * constraint_id: naming_as_control
 * human_readable: The Ontological Hegemony
 * domain: social/political/linguistic
 * * SUMMARY:
 * This constraint occurs when a dominant institution exercises power by
 * defining the legal and social vocabulary through which reality is
 * interpreted. By naming a behavior "deviant," "efficient," or "essential,"
 * the institution constrains the possible actions of subjects who must use
 * that same vocabulary to advocate for themselves.
 * * KEY AGENTS:
 * - Unnamed Subject: Subject (Powerless)
 * - Naming Authority: Beneficiary (Institutional)
 * - Semantic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.77) as the subject's ability to conceptualize
% alternatives is siphoned into the institution's linguistic framework.
domain_priors:base_extractiveness(naming_as_control, 0.77).
domain_priors:suppression_score(naming_as_control, 0.82).   % High suppression; "unnamed" concepts are invisible.
domain_priors:theater_ratio(naming_as_control, 0.55).       % Moderate theater; labels are presented as objective facts.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(naming_as_control, extractiveness, 0.77).
narrative_ontology:constraint_metric(naming_as_control, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(naming_as_control, theater_ratio, 0.55).

% Constraint self-claim (what does the constraint claim to be?)
% The authority claims this is a necessary tool for social order.
narrative_ontology:constraint_claim(naming_as_control, tangled_rope).
narrative_ontology:human_readable(naming_as_control, "The Ontological Hegemony").
narrative_ontology:topic_domain(naming_as_control, "social/political/linguistic").

% Binary flags & Structural properties for Tangled Rope
% Enforcement is social, legal, and economic; using unapproved terms
% leads to marginalization or punishment.
domain_priors:requires_active_enforcement(naming_as_control).

% Structural property derivation hooks for Tangled Rope:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(naming_as_control, naming_authority).
narrative_ontology:constraint_victim(naming_as_control, unnamed_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped in a linguistic snare: they cannot describe their
% oppression without using the very words that justify it.
constraint_indexing:constraint_classification(naming_as_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views naming as a vital Rope for social coordination and
% administrative legibility across a complex population.
constraint_indexing:constraint_classification(naming_as_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The combination of a genuine coordination function (beneficiary exists) with
% high, asymmetric extraction (victim exists) and active enforcement defines a Tangled Rope.
constraint_indexing:constraint_classification(naming_as_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naming_as_control_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional authority.
    constraint_indexing:constraint_classification(naming_as_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naming_as_control, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(tangled_rope_conditions_met) :-
    % Verify that the analytical observer correctly classifies as tangled_rope.
    constraint_indexing:constraint_classification(naming_as_control, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction (0.77) triggers high-extraction (>= 0.46) logic gates.
    domain_priors:base_extractiveness(naming_as_control, E),
    E >= 0.46.

:- end_tests(naming_as_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.77) reflects the 'Mandatrophy' threshold where
 * the "coordination" benefit of shared language is secondary to the
 * predatory removal of conceptual optionality. The high suppression (0.82)
 * represents the difficulty of operating outside the dominant lexicon.
 *
 * The Perspectival Gap is stark: The Unnamed Subject feels a Snare because
 * their lived experience is "illegalized" or "erased" by the dominant
 * taxonomy. The Authority sees a Rope because standardized naming reduces
 * the cost of large-scale societal management.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This is resolved via the Tangled Rope classification. The system recognizes
 * that while the naming authority provides a genuine coordination function
 * (the "Rope" aspect, evidenced by the beneficiary), the 0.77 extraction
 * score and the presence of a victim identify the predatory nature of the
 * linguistic monopoly (the "Snare" aspect). This prevents misclassification
 * as a pure Snare (which would ignore the coordination) or a pure Rope
 * (which would ignore the harm).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_linguistic_drift,
    'Is the vocabulary a fixed biological limit (Mountain) or a mutable policy choice (Snare)?',
    'Historical tracking of "slang" adoption into official language as a proxy for bottom-up semantic leverage.',
    'If slang redefines law: proves the constraint is a mutable Snare. If law consistently suppresses slang: suggests a more rigid, Mountain-like hegemony.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(naming_as_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a high-extraction constraint. Models how a system of
% naming control can start as a coordination tool and accumulate extractive
% power over time.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(nac_tr_t0, naming_as_control, theater_ratio, 0, 0.20).
narrative_ontology:measurement(nac_tr_t5, naming_as_control, theater_ratio, 5, 0.40).
narrative_ontology:measurement(nac_tr_t10, naming_as_control, theater_ratio, 10, 0.55).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(nac_ex_t0, naming_as_control, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(nac_ex_t5, naming_as_control, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(nac_ex_t10, naming_as_control, base_extractiveness, 10, 0.77).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Naming conventions are a form of information standard.
narrative_ontology:coordination_type(naming_as_control, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */