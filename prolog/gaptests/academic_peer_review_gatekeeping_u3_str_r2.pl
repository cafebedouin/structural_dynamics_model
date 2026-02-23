% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u3_str_r2
% ============================================================================
% Version: 4.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u3_str_r2, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u3_str_r2
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system is a mechanism where researchers,
 *   typically funded by public or non-profit grants, produce research,
 *   provide free peer-review labor, and serve as editors. For-profit
 *   publishers consolidate this labor, package it into journals, and sell
 *   access back to the researchers' own institutions at extremely high profit
 *   margins. The system is maintained by a prestige economy, where
 *   publication in certain journals is a prerequisite for career advancement
 *   (tenure, grants).
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must publish to secure tenure, providing free labor.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture immense value from free labor and subscription fees.
 *   - University Libraries/Consortia: Institutional victims and auditors (analytical/constrained) - must pay escalating fees, driving them to analyze the system's value.
 *   - Tenured Faculty/Editors: Beneficiaries and enforcers (organized/mobile) - act as gatekeepers, upholding the prestige economy that benefits their own status.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_str_r2, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u3_str_r2, 0.72).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u3_str_r2, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_str_r2, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_str_r2, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_str_r2, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u3_str_r2, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u3_str_r2, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u3_str_r2, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u3_str_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_str_r2, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_str_r2, tenured_faculty_gatekeepers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_str_r2, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_str_r2, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_str_r2, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the junior academic, publishing in prestige journals is non-negotiable for career survival. The system extracts their labor (research, writing, reviewing) for free and coerces participation via the tenure system, making it a classic Snare.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, they are providing an essential coordination service: managing peer review, curating content, and certifying quality. The extraction is framed as a fee for this service, making the system appear as a Rope that organizes the chaos of academic output.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer (e.g., a library consortium) sees both the genuine coordination function and the exorbitant, asymmetric extraction. They understand the need for quality control but also see the budget-breaking subscription costs, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% As a national funding body, we see the system's dual nature. It provides a (flawed) mechanism for evaluating research output, which we rely on for grant allocation (coordination). However, we also recognize that it extracts immense public funds for private profit and stifles innovation by locking research behind paywalls (extraction).
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u3_str_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_str_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u3_str_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.78) is extremely high, reflecting the publishers' 30-40% profit margins built on unpaid labor and publicly funded research. Suppression (0.72) is also high; while alternatives like pre-print servers and open-access journals exist, the 'publish or perish' mandate enforced by tenure committees makes participation in the prestige system largely non-optional for career academics.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The publisher sees a Rope, a service coordinating and validating science. The junior academic sees a Snare, a coercive system they cannot escape without sacrificing their career. The analytical view of a library consortium, forced to pay the bills, recognizes both the claimed coordination function and the undeniable extraction, leading to the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: value flows from researchers (labor) and their institutions (subscription fees) to the publishers (profit). Junior faculty are the primary targets of extraction (d -> 1.0). Publishers are the primary beneficiaries (d -> 0.0). This asymmetry is the core of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case of potential mandatrophy. It is defended as a pure coordination mechanism (Rope) for ensuring scientific quality. The Tangled Rope classification is crucial because it acknowledges the coordination function is real but inextricably linked to a highly extractive, rent-seeking business model. This prevents the system from being mislabeled as either a pure public good or a simple scam, capturing its dual nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_signal,
    'Is the prestige conferred by high-impact journals a genuine signal of quality (a coordination good) or an artificial scarcity manufactured for extraction?',
    'Longitudinal analysis comparing citation impact and real-world applicability of research from prestige journals vs. high-quality open-access platforms, controlling for institutional funding.',
    'If prestige is a genuine, irreplaceable signal, the system is a legitimate (if inefficient) Tangled Rope. If it's a manufactured commodity, the system is a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_signal, empirical, 'Whether journal prestige is a real quality signal or manufactured scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u3_str_r2, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_peer_review_gatekeeping_u3_str_r2, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acad_tr_t1995, academic_peer_review_gatekeeping_u3_str_r2, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u3_str_r2, theater_ratio, 2020, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_peer_review_gatekeeping_u3_str_r2, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acad_be_t1995, academic_peer_review_gatekeeping_u3_str_r2, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u3_str_r2, base_extractiveness, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u3_str_r2, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_str_r2, university_tenure_system).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_str_r2, scientific_funding_allocation).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_str_r2, public_access_to_information).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
