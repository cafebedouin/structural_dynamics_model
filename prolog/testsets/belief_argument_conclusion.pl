% ============================================================================
% CONSTRAINT STORY: belief_argument_conclusion
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_belief_argument_conclusion, []).

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
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: belief_argument_conclusion
 *   human_readable: The Futility of Arguing Against Instinctive Belief
 *   domain: social/philosophical
 *
 * SUMMARY:
 *   This constraint models the process of arguing as a futile exercise when
 *   core beliefs are instinctive. It posits that justifications are secondary
 *   "excuses" and that the argument itself becomes a trap, extracting cognitive
 *   energy without changing minds. When coordination fails, the only perceived
 *   alternatives are disengagement or conflict ("to fight").
 *
 * KEY AGENTS (by structural relationship):
 *   - The Arguer (powerless/trapped): Primary target — bears the extraction of
 *     wasted cognitive labor and emotional energy.
 *   - Instinctive Certainty (beneficiary): The underlying, unexamined belief
 *     system that is reinforced by the failure of argument.
 *   - The Negotiator (institutional/mobile): Uses the structure of argument as
 *     a coordination tool to achieve a conclusion, regardless of belief change.
 *   - The Professor (analytical/analytical): Analytical observer — sees the
 *     entire process as an extractive snare built around an immutable core.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction represents the wasted cognitive and emotional labor spent
% defending immutable beliefs with post-hoc justifications ("excuses").
domain_priors:base_extractiveness(belief_argument_conclusion, 0.70).
% Suppression is high because genuine belief change through argument is
% framed as structurally impossible; the core belief is inaccessible.
domain_priors:suppression_score(belief_argument_conclusion, 0.80).
% Theater is high as the argument is performative ego-maintenance rather
% than a functional process of discovery.
domain_priors:theater_ratio(belief_argument_conclusion, 0.80).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(belief_argument_conclusion, extractiveness, 0.70).
narrative_ontology:constraint_metric(belief_argument_conclusion, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(belief_argument_conclusion, theater_ratio, 0.80).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(belief_argument_conclusion, snare).
narrative_ontology:human_readable(belief_argument_conclusion, "The Futility of Arguing Against Instinctive Belief").
narrative_ontology:topic_domain(belief_argument_conclusion, "social/philosophical").

% --- Binary flags ---
% The process requires active engagement (enforcement) from the participants.
domain_priors:requires_active_enforcement(belief_argument_conclusion).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
% The unexamined, instinctive belief system is reinforced and protected.
narrative_ontology:constraint_beneficiary(belief_argument_conclusion, instinctive_certainty).
%
% Who bears disproportionate cost?
% The individual caught in the argument, expending energy for no result.
narrative_ontology:constraint_victim(belief_argument_conclusion, the_arguer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ANALYTICAL OBSERVER (THE PROFESSOR)
% From an analytical view, the process is a Snare. While the underlying
% belief may be an immutable "Mountain", the *act of arguing* about it is a
% trap that extracts energy and suppresses the alternative of addressing
% the core instinct directly. The high extraction and futility define it as
% a Snare, not a natural law.
constraint_indexing:constraint_classification(belief_argument_conclusion, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY TARGET (THE ARGUER)
% For the participant, the argument is a Snare. It tightens as they "trot out
% their excuses," consuming their energy and triggering anger when they
% realize they cannot persuade the other, trapping them in a cycle of
% performative justification.
constraint_indexing:constraint_classification(belief_argument_conclusion, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (THE NEGOTIATOR)
% For an institution seeking a conclusion, the formal structure of argument
% and negotiation is a Rope—a functional coordination mechanism to achieve
% a stable outcome (e.g., a treaty, a contract), even if underlying beliefs
% remain untouched. The process serves a coordinating function.
constraint_indexing:constraint_classification(belief_argument_conclusion, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(belief_argument_conclusion_tests).

test(perspectival_gap) :-
    % Verify the gap between the arguer (Snare) and negotiator (Rope).
    constraint_indexing:constraint_classification(belief_argument_conclusion, TypeTarget,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(belief_argument_conclusion, TypeBeneficiary,
        context(agent_power(institutional), _, _, _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_matches_claim) :-
    % The analytical perspective should align with the constraint's claim.
    narrative_ontology:constraint_claim(belief_argument_conclusion, ClaimedType),
    constraint_indexing:constraint_classification(belief_argument_conclusion, AnalyticalType,
        context(agent_power(analytical), _, _, _)),
    assertion(ClaimedType == AnalyticalType).

:- end_tests(belief_argument_conclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high scores for extraction (0.70) and suppression (0.80) reflect the
 *   narrative's core assertion: arguing against instinct is a futile, energy-
 *   draining activity where the real root of belief is inaccessible. The
 *   constraint being modeled is the *process of argument*, not the belief
 *   itself. While the belief might be a Mountain to the individual, the
 *   social process constructed around it is a Snare that traps participants.
 *
 * PERSPECTIVAL GAP:
 *   The Arguer (powerless) experiences the argument as a Snare—a frustrating
 *   trap that consumes energy and leads to anger. The Negotiator
 *   (institutional), however, sees the exact same process as a Rope—a useful
 *   tool for achieving coordination and reaching a conclusion, abstracting
 *   away the question of genuine belief change. This gap highlights how a
 *   single process can be extractive for participants but functional for
 *   institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is 'instinctive_certainty'—the unexamined belief itself,
 *   which is protected from scrutiny by the futility of the argument. The
 *   victim is 'the_arguer', who bears the cognitive and emotional costs of
 *   participating in this futile process. This maps directly to the structure
 *   where an unchangeable core extracts energy from those who try to engage it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the process as a Snare from the
 *   analytical perspective, preventing the mislabeling of a highly extractive
 *   and performative activity as a mere fact of nature (Mountain). It
 *   distinguishes between the immutable belief and the costly social theater
 *   built around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_belief_erosion,
    "Are beliefs truly 100% instinctive, or can attacking justifications eventually erode the core belief?",
    "Long-term cognitive science studies on belief-erosion via justification-denial.",
    "If justifications matter, argument could be a Scaffold or Rope. If not, it remains a Snare.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_belief_intent,
    "Is the futility of argument a biological necessity or an intentional ego-strategy to maintain social dominance?",
    "Comparison of conflict outcomes in environments with vs. without 'high-rationality' training.",
    "If necessity, it has Mountain-like properties. If strategy, it is a pure Snare.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_belief_erosion, empirical,
    "Whether persistent logical argument can erode instinctive core beliefs over time.").
narrative_ontology:omega_variable(omega_belief_intent, empirical,
    "Whether the futility of argument is a biological feature or a learned social strategy.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(belief_argument_conclusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is required for high-extraction constraints (E > 0.46) to
% enable lifecycle drift detection.

% Theater ratio: Rising from functional inquiry (0.20) toward the
% performative "Theater of Excuses" (0.80) as the argument reaches impasse.
narrative_ontology:measurement(belief_tr_t0, belief_argument_conclusion, theater_ratio, 0, 0.20).
narrative_ontology:measurement(belief_tr_t5, belief_argument_conclusion, theater_ratio, 5, 0.55).
narrative_ontology:measurement(belief_tr_t10, belief_argument_conclusion, theater_ratio, 10, 0.80).

% Extraction: Tracking the intensification of cognitive labor extraction
% as the subject moves from discussion to the binary of "agree or fight."
narrative_ontology:measurement(belief_ex_t0, belief_argument_conclusion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(belief_ex_t5, belief_argument_conclusion, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(belief_ex_t10, belief_argument_conclusion, base_extractiveness, 10, 0.70).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */