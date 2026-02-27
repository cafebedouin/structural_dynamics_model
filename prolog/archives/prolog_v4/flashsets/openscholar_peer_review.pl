% ============================================================================
% CONSTRAINT STORY: openscholar_peer_review
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openscholar_peer_review, []).

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
 *   constraint_id: openscholar_peer_review
 *   human_readable: Traditional Academic Peer Review
 *   domain: technological
 *
 * SUMMARY:
 *   Traditional academic peer review, where experts assess research before
 *   publication, acts as a quality control mechanism. However, it also
 *   creates a system where certain researchers and institutions wield
 *   significant power over what knowledge is disseminated. This system can
 *   unintentionally suppress novel ideas, create barriers for early-career
 *   researchers, and suffer from biases.
 *
 * KEY AGENTS:
 *   - Tenured Faculty: Benefits by controlling access.
 *   - Journal Publishers: Benefits from curating high-quality content.
 *   - Early Career Researchers: Victimized by gatekeeping and potential biases.
 *   - Independent Researchers: May be victimized as they lack institutional support.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openscholar_peer_review, 0.5).
domain_priors:suppression_score(openscholar_peer_review, 0.6).
domain_priors:theater_ratio(openscholar_peer_review, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openscholar_peer_review, extractiveness, 0.5).
narrative_ontology:constraint_metric(openscholar_peer_review, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(openscholar_peer_review, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openscholar_peer_review, tangled_rope).
narrative_ontology:human_readable(openscholar_peer_review, "Traditional Academic Peer Review").
narrative_ontology:topic_domain(openscholar_peer_review, "technological").

domain_priors:requires_active_enforcement(openscholar_peer_review).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openscholar_peer_review, tenured_faculty).
narrative_ontology:constraint_beneficiary(openscholar_peer_review, journal_publishers).
narrative_ontology:constraint_victim(openscholar_peer_review, early_career_researchers).
narrative_ontology:constraint_victim(openscholar_peer_review, independent_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Early career researchers often feel trapped by the peer review system, as publications are essential for career advancement. They may face biases or gatekeeping, limiting their ability to advance without conforming to established norms.
constraint_indexing:constraint_classification(openscholar_peer_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Established academics benefit from the peer review system's role in validating their work and maintaining standards. However, they are constrained by the time commitment and potential biases within the system, experiencing it as a mix of coordination and extraction.
constraint_indexing:constraint_classification(openscholar_peer_review, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Journal publishers benefit from the peer review system by using it to curate high-quality content and maintain the reputation of their publications, giving them the power to decide what research is disseminated. They can choose which papers to publish.
constraint_indexing:constraint_classification(openscholar_peer_review, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, traditional peer review is a tangled rope, combining genuine coordination (quality control) with asymmetric extraction (gatekeeping, bias, suppression of novel ideas). The system's benefits and drawbacks are visible from this broad perspective.
constraint_indexing:constraint_classification(openscholar_peer_review, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openscholar_peer_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openscholar_peer_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openscholar_peer_review, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openscholar_peer_review, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(openscholar_peer_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The peer review system is classified as a tangled rope because it combines coordination (quality control) with asymmetric extraction. Extractiveness: It extracts from early-career and independent researchers by creating barriers to publication. Suppression: It suppresses novel ideas that do not conform to established paradigms. Theater Ratio: The system is partially performative, with reviews often focusing on presentation rather than the underlying validity of the research.
 *
 * PERSPECTIVAL GAP:
 *   Early Career Researchers view the system as a snare because it restricts their access to publishing their work. Established Academics see it as a tangled rope because they benefit, but are also constrained by its limitations. Journal Publishers view it as a rope because it enables them to curate high-quality content and maintain their publication's reputation. The analytical observer sees the broader picture: both the good and bad.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty and journal publishers benefit and have exit options (arbitrage) by selecting what is published and maintaining control of what research is valued. Early career researchers and independent researchers are victims as they are judged. The analytical observer sees the net effect.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_degree,
    'To what extent does the peer review process reflect biases (e.g., confirmation bias, institutional bias, gender bias)?',
    'Statistical analysis of publication rates and citation patterns for different demographics and institutions.',
    'High bias would suggest it''s more of a snare. Low bias would suggest it''s a more effective rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_degree, empirical, 'Quantifying the degree of bias within the peer review system.').

omega_variable(
    innovation_suppression,
    'Does the peer review process inadvertently suppress innovative or unconventional research?',
    'Case studies of groundbreaking research that initially faced resistance from peer reviewers.',
    'If suppression is high, the system is more of a snare. If it''s low, then the system is a tangled rope that still allows for innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_suppression, empirical, 'Assessing whether the system suppresses novel research.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openscholar_peer_review, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(open_tr_t0, openscholar_peer_review, theater_ratio, 0, 0.5).
narrative_ontology:measurement(open_tr_t5, openscholar_peer_review, theater_ratio, 5, 0.6).
narrative_ontology:measurement(open_tr_t10, openscholar_peer_review, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(open_be_t0, openscholar_peer_review, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(open_be_t5, openscholar_peer_review, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(open_be_t10, openscholar_peer_review, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openscholar_peer_review, information_standard).
narrative_ontology:affects_constraint(openscholar_peer_review, academic_publishing_business_model).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
