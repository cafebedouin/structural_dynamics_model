% ============================================================================
% CONSTRAINT STORY: sadhu_integrity_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sadhu_integrity_protocol, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: sadhu_integrity_protocol
 *   human_readable: The Integrity Requirement (Sadhu's Sugar)
 *   domain: social/ethical
 *
 * SUMMARY:
 *   The integrity requirement is a social and ethical constraint that
 *   mandates advisors and teachers embody their own counsel before delivering
 *   it to others. It influences the relationship between advisors and
 *   advice-seekers, as well as the credibility and trustworthiness of
 *   individuals in positions of authority. The extraction reflects the costs
 *   borne by those who fail to meet the integrity standard, as well as
 *   society at large when hypocrisy is tolerated.
 *
 * KEY AGENTS:
 *   - Seekers of advice: Beneficiaries (organized/constrained) - gain from more reliable guidance.
 *   - Society at large: Beneficiary (institutional/mobile) - benefits from a more ethical environment.
 *   - Dishonest advisors: Victims (powerless/trapped) - face penalties when integrity requirements are upheld.
 *   - Ethical Teachers: those who are embodying the principles they preach (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sadhu_integrity_protocol, 0.6).
domain_priors:suppression_score(sadhu_integrity_protocol, 0.7).
domain_priors:theater_ratio(sadhu_integrity_protocol, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sadhu_integrity_protocol, extractiveness, 0.6).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sadhu_integrity_protocol, tangled_rope).
narrative_ontology:human_readable(sadhu_integrity_protocol, "The Integrity Requirement (Sadhu's Sugar)").
narrative_ontology:topic_domain(sadhu_integrity_protocol, "social/ethical").

domain_priors:requires_active_enforcement(sadhu_integrity_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sadhu_integrity_protocol, seekers_of_advice).
narrative_ontology:constraint_beneficiary(sadhu_integrity_protocol, society_at_large).
narrative_ontology:constraint_victim(sadhu_integrity_protocol, dishonest_advisors).
narrative_ontology:constraint_victim(sadhu_integrity_protocol, hypocrisy_tolerance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The advisor who does not embody their counsel experiences significant extraction. They face social stigma, loss of credibility, and potential ostracization.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Those embodying the principle benefit. Their standing is improved within society, and they are considered reliable counselors.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the integrity requirement presents a mixed bag: it promotes genuine ethical behavior but can also lead to hypocrisy-shaming and unrealistic expectations.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Those seeking advice both benefit from more reliable counsel, but are also constrained by more limited access. They are extracting information, however. The constraint is designed to benefit the seekers of advice, rather than punish advisors directly.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sadhu_integrity_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sadhu_integrity_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sadhu_integrity_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): Reflects the social costs, reputational damage, and potential career consequences faced by advisors who do not embody their own teachings. The suppression (0.7) stems from the social pressures to conform to ethical standards, potential legal or professional repercussions, and ostracization.
 *
 * PERSPECTIVAL GAP:
 *   The snare-rope gap highlights the inherent tension: advisors face penalties when their conduct deviates, while ethical teachers benefit. The analytical perspective highlights the value in promoting ethical behavior, while recognizing challenges. Seekers both benefit from ethical, reliable advice but are limited by the ethical teachers out there.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of each directionality value (d) depends on each actors structural position. Arbitrageurs gain a lot from the advice, so their d value is close to zero, resulting in a negative χ. Hypocritical advisors face social stigma, loss of credibility, so their d value is close to one, resulting in χ near 1.0
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measuring_integrity,
    'How can integrity be reliably assessed and measured, given that it often involves internal states and complex behaviors?',
    'Empirical studies correlating self-reported values with observed actions; development of behavioral metrics for integrity.',
    'A reliable measure would reduce the ambiguity surrounding hypocrisy accusations, shifting the classification towards rope for honest advisors and snare for dishonest ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measuring_integrity, empirical, 'Challenges in reliably assessing integrity.').

omega_variable(
    contextual_relativity,
    'To what degree is integrity relative to cultural norms, personal values, and specific circumstances?',
    'Cross-cultural comparisons of ethical codes; philosophical analyses of moral relativism; case studies examining moral dilemmas.',
    'Recognizing the role of context would mitigate the risk of excessive moral judgment, making the constraint more scaffold-like in specific situations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contextual_relativity, conceptual, 'Impact of context and cultural norms on ethical standards.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sadhu_integrity_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sadh_tr_t0, sadhu_integrity_protocol, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sadh_tr_t5, sadhu_integrity_protocol, theater_ratio, 5, 0.2).
narrative_ontology:measurement(sadh_tr_t10, sadhu_integrity_protocol, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(sadh_be_t0, sadhu_integrity_protocol, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sadh_be_t5, sadhu_integrity_protocol, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(sadh_be_t10, sadhu_integrity_protocol, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sadhu_integrity_protocol, enforcement_mechanism).
narrative_ontology:affects_constraint(sadhu_integrity_protocol, trustworthiness_index).
narrative_ontology:affects_constraint(sadhu_integrity_protocol, ethical_leadership_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
