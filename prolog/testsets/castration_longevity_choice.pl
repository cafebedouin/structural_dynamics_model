% ============================================================================
% CONSTRAINT STORY: castration_longevity_choice
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_castration_longevity_choice, []).

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
 * * constraint_id: castration_longevity_choice
 * human_readable: The Castration-Longevity Trade-off
 * domain: technological/social/biological
 * * SUMMARY:
 * This constraint explores the hypothesis that male sex hormones decrease lifespan,
 * positioning castration as a potential biological lever for longevity. While
 * supported by some historical data and animal studies, it is heavily
 * constrained by modern medical ethics, institutional disapproval, and
 * social pathologization, which actively suppress it as a valid choice.
 * * KEY AGENTS:
 * - Longevity Seeker: The subject, a male individual considering a radical
 *   biological sacrifice for increased lifespan.
 * - Institutional Review Board (IRB): The beneficiary of the status quo,
 *   enforcing current ethical norms and preventing such research.
 * - Biological Researcher: The analytical observer, evaluating the trade-off
 *   based on available data.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(castration_longevity_choice, 0.80). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(castration_longevity_choice, 0.50).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(castration_longevity_choice, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(castration_longevity_choice, extractiveness, 0.80).
narrative_ontology:constraint_metric(castration_longevity_choice, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(castration_longevity_choice, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
% The ethical barrier claims to be a natural law of beneficence.
narrative_ontology:constraint_claim(castration_longevity_choice, tangled_rope).
narrative_ontology:human_readable(castration_longevity_choice, "The Castration-Longevity Trade-off").
narrative_ontology:topic_domain(castration_longevity_choice, "technological/social/biological").

% Binary flags
domain_priors:requires_active_enforcement(castration_longevity_choice). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(castration_longevity_choice, medical_ethical_establishment).
narrative_ontology:constraint_victim(castration_longevity_choice, longevity_seeker).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The longevity seeker perceives a high-cost, high-reward option that is
% actively blocked by social and institutional forces, trapping them between
% aging and pathologization. The choice is extractive and coercive.
constraint_indexing:constraint_classification(castration_longevity_choice, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (MOUNTAIN)
% The medical institution (IRB) perceives its ethical guidelines against such
% a procedure not as a choice but as an immutable "Mountain" of medical ethics
% ("do no harm"). This is a false natural law claim, as the ethics are constructed.
constraint_indexing:constraint_classification(castration_longevity_choice, mountain,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a system with a genuine coordination function (trading
% reproductive fitness for longevity) but also asymmetric extraction (the
% seeker bears all risk/cost) and active enforcement (by the medical
% establishment). This combination is a Tangled Rope.
constraint_indexing:constraint_classification(castration_longevity_choice, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(castration_longevity_choice_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(castration_longevity_choice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(castration_longevity_choice, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = mountain.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical view correctly identifies the Tangled Rope structure.
    constraint_indexing:constraint_classification(castration_longevity_choice, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify the base extraction meets the threshold for a Snare/Tangled Rope.
    narrative_ontology:constraint_metric(castration_longevity_choice, extractiveness, E),
    E >= 0.46.

:- end_tests(castration_longevity_choice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect a biologically potent but socially suppressed trade-off.
 * Extraction (0.80) is extremely high, representing the permanent loss of
 * reproductive and hormonal function. Suppression (0.50) is moderate but
 * decisive, coming from institutional gatekeepers who enforce ethical norms.
 *
 * The Perspectival Gap is significant:
 * - The Seeker sees a Snare: a desirable outcome (longevity) is tied to a
 *   brutal cost and is actively blocked by an external power.
 * - The Institution sees a Mountain: it frames its ethical stance as an
 *   unquestionable, natural law of medicine, hiding the constructed nature of
 *   the barrier.
 * - The Analyst sees a Tangled Rope: the metrics (high extraction, moderate
 *   suppression, active enforcement, and a clear victim/beneficiary structure)
 *   perfectly align with a Tangled Rope, where a potential coordination
 *   benefit is entangled with coercive extraction and control.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This constraint is a classic case of a potential
 * Rope (a biological trade-off) being turned into a Snare/Tangled Rope by
 * social and institutional enforcement. The system avoids mislabeling it as
 * a pure Snare by recognizing the underlying (though inaccessible) coordination
 * function via the Tangled Rope classification from the analytical perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_castration_longevity_choice_1,
    'Does the longevity benefit seen in historical eunuchs translate to modern males with high-quality healthcare?',
    'Long-term cohort study of males castrated for medical reasons vs. matched controls.',
    'If yes, the Rope aspect is validated. If no, the sacrifice is a pure Snare with no payout.',
    confidence_without_resolution(low)
).

omega_variable(
    omega_castration_longevity_choice_2,
    'Is the social redirection to psychiatry a functional protection of the subject (Mountain) or an extractive suppression of life-extension agency (Snare)?',
    'Audit of IRB decisions regarding life-extending biological modifications.',
    'If protection, the institutional Mountain claim is stronger. If suppression, it confirms the Snare/Tangled Rope structure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(castration_longevity_choice, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models the constraint as an idea
% that became more concrete and extractive as longevity science advanced,
% solidifying the trade-off.
%
% Theater ratio over time (low and stable):
narrative_ontology:measurement(clc_tr_t0, castration_longevity_choice, theater_ratio, 0, 0.05).
narrative_ontology:measurement(clc_tr_t5, castration_longevity_choice, theater_ratio, 5, 0.08).
narrative_ontology:measurement(clc_tr_t10, castration_longevity_choice, theater_ratio, 10, 0.10).

% Extraction over time (increasing as the potential benefit becomes clearer):
narrative_ontology:measurement(clc_ex_t0, castration_longevity_choice, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(clc_ex_t5, castration_longevity_choice, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(clc_ex_t10, castration_longevity_choice, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint is a trade-off enforced by a social/ethical system.
narrative_ontology:coordination_type(castration_longevity_choice, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */