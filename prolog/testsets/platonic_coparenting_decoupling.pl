% ============================================================================
% CONSTRAINT STORY: PLATONIC_COPARENTING_DECOUPLING
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_platonic_coparenting, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: platonic_coparenting
 * human_readable: The Platonic Co-Parenting Modularization
 * domain: social/familial
 * * SUMMARY:
 * This constraint tracks the shift from traditional "Romantic-Parental"
 * integration to "Modular" parenting. It addresses the coordination problem
 * of the biological clock vs. romantic search.
 * * KEY AGENTS:
 * - The Searcher (Rave Reid): Subject (Moderate Power/Seeking Exit from Romantic Pressure).
 * - The Platform (Modamily): Beneficiary (Institutional/Extracting Subscription Value).
 * - The Traditionalist (Heritage Foundation): Auditor (Organized/Defending Biological-Married standard).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.48) due to platform subscription fees and legal/medical
% overhead (IVF, contracts).
domain_priors:base_extractiveness(platonic_coparenting, 0.48).
domain_priors:suppression_score(platonic_coparenting, 0.35).
domain_priors:theater_ratio(platonic_coparenting, 0.15).       % Functional, not theatrical.

% Binary flags
domain_priors:requires_active_enforcement(platonic_coparenting). % Legal contracts required.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE BIOLOGICAL-CLOCK SUBJECT (ROPE)
% Viewed as a "teammate" coordination mechanism that relieves romantic pressure.
constraint_indexing:constraint_classification(platonic_coparenting, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTIONAL PLATFORM (TANGLED ROPE)
% Provides essential coordination (100k users) while extracting capital.
constraint_indexing:constraint_classification(platonic_coparenting, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE TRADITIONALIST CRITIC (SNARE)
% Argues that adults "place their desires... over the best interests of the child".
constraint_indexing:constraint_classification(platonic_coparenting, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: THE LEGAL/MEDICAL AUDITOR (SCAFFOLD)
% The arrangement is often viewed as a "support structure" for early childhood.
constraint_indexing:constraint_classification(platonic_coparenting, scaffold,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coparenting_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Rope for the subject but a Snare for the critic.
    constraint_indexing:constraint_classification(platonic_coparenting, rope, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(platonic_coparenting, snare, context(agent_power(organized), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(platonic_coparenting, E),
    E >= 0.46. % Triggers high-extraction status.

:- end_tests(coparenting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The classification of 'Rope' for the Subject is justified by the relief of
 * the "all-in-one" partner pressure. The high extraction (0.48)
 * reflects the financial cost of modularizing a family (legal fees, IVF,
 * app subscriptions).
 * * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * Extraction (0.48) is high but not purely predatory. It is 'Tangled Rope'
 * because the platform provides a genuine coordination function that
 * offsets the "natural law" of the biological clock.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_child_outcome,
    'Will long-term longitudinal data confirm or refute the Heritage Foundation claim?',
    'Study of educational and emotional stability metrics for kids in multi-household platonic units.',
    'Confirmation = Shift to Snare; Refutation = Hardening as Mountain (Social Law).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(platonic_coparenting, 2020, 2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
