% ============================================================================
% CONSTRAINT STORY: advice_as_dangerous_gift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_advice_as_dangerous_gift, []).

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
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: advice_as_dangerous_gift
 *   human_readable: The Hazard of Counsel
 *   domain: social/philosophical
 *
 * SUMMARY:
 *   From J.R.R. Tolkien's Lord of the Rings, the giving and receiving of
 *   advice can be fraught with peril. While good counsel can guide and
 *   protect, poor or malicious advice can lead to ruin. This constraint
 *   explores the dual nature of advice as both a potential benefit and a
 *   significant hazard. The extractiveness arises from the advisor
 *   potentially benefiting from the advisee's actions, regardless of the
 *   outcome for the advisee.
 *
 * KEY AGENTS:
 *   - advice_givers: Those offering guidance (institutional/arbitrage)
 *   - advice_receivers: Those following guidance (powerless/trapped)
 *   - tradition_wisdom: established practices (institutional/arbitrage)
 *   - analytical_observer: evaluates outcome (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(advice_as_dangerous_gift, 0.55).
domain_priors:suppression_score(advice_as_dangerous_gift, 0.4).
domain_priors:theater_ratio(advice_as_dangerous_gift, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(advice_as_dangerous_gift, extractiveness, 0.55).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(advice_as_dangerous_gift, tangled_rope).
narrative_ontology:human_readable(advice_as_dangerous_gift, "The Hazard of Counsel").
narrative_ontology:topic_domain(advice_as_dangerous_gift, "social/philosophical").

domain_priors:requires_active_enforcement(advice_as_dangerous_gift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(advice_as_dangerous_gift, advice_givers).
narrative_ontology:constraint_victim(advice_as_dangerous_gift, advice_receivers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The advice receiver, lacking experience or knowledge, may be trapped by the advice, suffering negative consequences. The advice may be self-serving for the giver or simply incorrect, but the receiver is now committed.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Established traditions and cultural wisdom provide a coordinating function, guiding individuals through common life challenges. The advice is generally beneficial due to its proven track record and widespread acceptance.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Advice can be both helpful and harmful. It provides guidance and coordination, but also introduces the risk of manipulation, bias, and unintended consequences. The net effect is a complex interplay of extraction and coordination.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(advice_as_dangerous_gift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(advice_as_dangerous_gift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(advice_as_dangerous_gift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The advice giver may gain status or influence from giving advice, even if the advice harms the receiver. The receiver is extracted from because they lose autonomy and potentially suffer negative consequences. Suppression (0.40): Moderate. The receiver's options are limited by the advice; they may feel compelled to follow it, reducing their freedom of choice. The advice may also suppress other, potentially better options.
 *
 * PERSPECTIVAL GAP:
 *   The receiver, often in a vulnerable position, experiences the advice as a snare if it leads to negative outcomes. The giver, on the other hand, may see it as a helpful act of coordination, even if it's flawed. The analytical observer recognizes the complex interplay of potential benefits and risks.
 *
 * DIRECTIONALITY LOGIC:
 *   Advice givers benefit through increased status, influence, and sometimes tangible resources if the advice is followed successfully. Receivers are vulnerable and thus 'trapped' by the advice if it leads to adverse consequences. The directionality derives from this power asymmetry and potential for self-serving counsel.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advice_validity,
    'Is the advice valid and applicable to the specific situation of the receiver?',
    'Empirical testing of the advice''s effectiveness; careful consideration of the receiver''s context.',
    'If valid, advice is a rope; if invalid, it''s a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advice_validity, empirical, 'The validity of the advice being given.').

omega_variable(
    advisor_intent,
    'Does the advisor have benevolent or self-serving intentions?',
    'Assessing the advisor''s motives and potential conflicts of interest.',
    'Benevolent intent makes the advice more likely to be a rope; self-serving intent increases the likelihood of a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisor_intent, conceptual, 'The intent behind the advice giving.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(advice_as_dangerous_gift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(advi_tr_t0, advice_as_dangerous_gift, theater_ratio, 0, 0.1).
narrative_ontology:measurement(advi_tr_t5, advice_as_dangerous_gift, theater_ratio, 5, 0.2).
narrative_ontology:measurement(advi_tr_t10, advice_as_dangerous_gift, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(advi_be_t0, advice_as_dangerous_gift, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(advi_be_t5, advice_as_dangerous_gift, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(advi_be_t10, advice_as_dangerous_gift, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(advice_as_dangerous_gift, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
