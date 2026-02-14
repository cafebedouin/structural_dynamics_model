% ============================================================================
% CONSTRAINT STORY: kjv_linguistic_residue
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_kjv_linguistic_residue, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kjv_linguistic_residue
 *   human_readable: The KJV Aesthetic as a Cognitive Constraint
 *   domain: linguistic/cultural
 *
 * SUMMARY:
 *   Even in secular contexts, the King James Version's cadence (Early Modern
 *   English) acts as a "signal of gravity." When a politician, writer, or AI
 *   wants to sound "weighty" or "authoritative," they often revert to KJV-era
 *   syntax. This is a constraint on how authority is perceived: if it doesn't
 *   "sound" biblical, it can feel less "true" or "ancient." The original
 *   function (religious text) has atrophied in secular use, but the aesthetic
 *   remains due to cultural inertia and theatrical maintenance.
 *
 * KEY AGENTS (by structural relationship):
 *   - Speakers of non-standard dialects: Primary target (powerless/trapped) — their speech is perceived as less authoritative.
 *   - Rhetoricians and orators: Primary beneficiary (powerful/mobile) — leverage the aesthetic for rhetorical effect.
 *   - Academic institutions: Custodial agent (institutional/analytical) — curates the linguistic artifact.
 *   - Analytical observer: Sees the full structure of inertia and theatrical use.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is low; it extracts cognitive effort, not wealth. Nudged >0.10 for Piton gate.
domain_priors:base_extractiveness(kjv_linguistic_residue, 0.11).
% Suppression is low; modern English is the default, but lacks the "authority weight."
domain_priors:suppression_score(kjv_linguistic_residue, 0.2).
% Theater is high; its modern secular function is almost entirely performative.
domain_priors:theater_ratio(kjv_linguistic_residue, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_linguistic_residue, extractiveness, 0.11).
narrative_ontology:constraint_metric(kjv_linguistic_residue, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(kjv_linguistic_residue, theater_ratio, 0.85).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(kjv_linguistic_residue, piton).

% --- Binary flags ---
% No active enforcement; the constraint persists through cultural inertia.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(kjv_linguistic_residue, rhetoricians_and_orators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(kjv_linguistic_residue, speakers_of_non_standard_dialects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PASSIVE CONSUMER (PITON)
% For a passive consumer of culture, the KJV's influence is an inertial artifact.
% They use its idioms without knowing their source. It's a non-functional but
% persistent feature of the linguistic landscape. High theater makes it a Piton.
constraint_indexing:constraint_classification(kjv_linguistic_residue, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SPEECHWRITER / ORATOR (ROPE)
% For the professional communicator, the KJV aesthetic is a functional tool.
% It's a 'Rope' used to coordinate an audience's perception towards "seriousness"
% and "authority," leveraging the theatricality for a specific rhetorical purpose.
constraint_indexing:constraint_classification(kjv_linguistic_residue, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ACADEMIC INSTITUTION (PITON)
% For an academic institution or the literary canon, the KJV's residue is a
% curated historical artifact. Its primary function has atrophied, but it is
% maintained by institutional inertia. It is a classic Piton.
constraint_indexing:constraint_classification(kjv_linguistic_residue, piton,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (PITON)
% The analytical view sees a constraint whose original function is largely inert
% in secular contexts, maintained primarily by theatricality and inertia.
% The high theater ratio (0.85) is decisive, classifying it as a Piton.
constraint_indexing:constraint_classification(kjv_linguistic_residue, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_linguistic_residue_tests).

test(perspectival_gap_passive_vs_active_use) :-
    % Verify the gap between those who experience the constraint passively (Piton)
    % and those who actively leverage it as a tool (Rope).
    constraint_indexing:constraint_classification(kjv_linguistic_residue, piton, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_linguistic_residue, rope, context(agent_power(powerful), _, _, _)),
    constraint_indexing:constraint_classification(kjv_linguistic_residue, piton, context(agent_power(institutional), _, _, _)).

test(piton_threshold_validation) :-
    % Verify that the metrics support the Piton classification.
    narrative_ontology:constraint_metric(kjv_linguistic_residue, theater_ratio, TR),
    TR >= 0.70,
    narrative_ontology:constraint_metric(kjv_linguistic_residue, extractiveness, E),
    E > 0.10.

:- end_tests(kjv_linguistic_residue_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The key to this constraint is its high theater_ratio (0.85). In modern
 *   secular contexts, the KJV's linguistic style is almost entirely performative,
 *   used to evoke a feeling of authority rather than for its original religious
 *   function. This high theatricality, combined with low extraction (0.11),
 *   firmly places it in the Piton category for most observers. The base
 *   extractiveness was set to 0.11 to cleanly pass the Piton gate (ε > 0.10).
 *
 * PERSPECTIVAL GAP:
 *   The gap is between passive observers and active users. For most people
 *   (powerless, institutional), the KJV's linguistic residue is a Piton—an
 *   inert but persistent cultural artifact. For a skilled rhetorician
 *   (powerful), this same artifact becomes a Rope—a functional tool to
 *   coordinate audience perception and emotion. They are leveraging the
 *   Piton's theatricality to create a coordination effect.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'rhetoricians_and_orators' benefit by having a shared
 *     cultural symbol they can deploy to signal authority.
 *   - Victims: 'speakers_of_non-standard_dialects' are harmed because their
 *     speech patterns are implicitly judged against this archaic, high-status
 *     standard and found wanting in "gravity."
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the SCAFFOLD_DANGER_ZONE linter warning by correctly
 *   identifying the high theatricality of the constraint. With a theater_ratio
 *   of 0.85, the engine's scaffold gate (which requires theater < 0.70) will
 *   not fire. The constraint is correctly identified as a Piton (an inertial
 *   artifact) rather than a Scaffold (a temporary support structure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_kjv_linguistic_residue,
    'Will the "signal of gravity" from KJV-era syntax diminish as cultural literacy of the text fades, or is the aesthetic now fully decoupled and self-sustaining?',
    'Longitudinal analysis of linguistic trends in political and formal speech; psycholinguistic studies on perceived authority of archaic language.',
    'If it diminishes, the Piton erodes and the Rope for rhetoricians frays. If it is self-sustaining, it remains a durable feature of the linguistic landscape.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(kjv_linguistic_residue, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */