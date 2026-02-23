% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u1_exp_r4
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u1_exp_r4, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u1_exp_r4
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system operates on a model where researchers,
 *   typically funded by public or university grants, produce research, write
 *   articles, and perform peer review for free. For-profit publishers then
 *   package this free labor and sell access to the resulting journals back to
 *   university libraries at extremely high subscription costs. This creates a
 *   circular flow where value is systematically extracted from the academic
 *   community and taxpayers and concentrated in the hands of a few publishing
 *   houses.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must publish in prestigious journals to secure tenure, providing free labor.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture the value of the free labor and sell it back to the system.
 *   - University Libraries/Consortia: Victims and auditors (organized/analytical) - bear the direct financial costs and are organized enough to analyze and resist the extraction.
 *   - The General Public: Indirect victims (powerless/trapped) - fund the research through taxes but are denied access without paying high fees.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r4, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u1_exp_r4, 0.8).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u1_exp_r4, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r4, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r4, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r4, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u1_exp_r4, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u1_exp_r4, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u1_exp_r4, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u1_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r4, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r4, tenured_faculty_as_editors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r4, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r4, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r4, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a junior academic, the 'publish or perish' mandate makes participation non-optional. The system extracts free labor (research, writing, reviewing) under threat of career failure, classifying it as a Snare.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's viewpoint, they are providing a valuable coordination service: organizing peer review, curating content, and conferring prestige. The immense profits are seen as a fair return for managing this complex system, making it appear as a Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% University administrators are caught in the middle. They must fund library subscriptions (as a victim) but also enforce the tenure system that feeds the publishers (as an enforcer). They see both the coordination and extraction, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r4, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view, often held by library consortia or open-access advocates, sees the full picture. A genuine coordination function (quality control) exists but has been captured by an extractive business model. This dual nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u1_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u1_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.78) is extremely high, reflecting the publishers' profit margins built on unpaid labor. Suppression (0.80) is also high because the academic prestige and tenure system is deeply intertwined with the established journals, making alternatives like pre-print servers or open-access journals career risks for junior faculty. The system requires active enforcement through university tenure committees and grant funding requirements.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Junior Professor (who sees a Snare due to the coercive 'publish or perish' dynamic) and the Publisher (who frames their role as a value-add coordination Rope). The analytical perspective of a Library Consortium correctly identifies the dual nature of the system—a legitimate coordination function (organizing review) that has been co-opted for asymmetric extraction, hence a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from researchers (labor), universities (library budgets), and the public (research funding) towards the publishers. Junior professors are the primary targets of extraction, as their careers depend on compliance. Publishers are the primary beneficiaries, leveraging their position as gatekeepers to monopolize access to knowledge produced by others.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case of mandatrophy. A system that may have started as a genuine coordination mechanism (a Rope for disseminating research) has become pathologically extractive. The Tangled Rope classification is crucial because it avoids mislabeling the system as a pure Snare (which would ignore the vestigial coordination function) or a Rope (which would ignore the massive extraction). It correctly identifies that a coordination claim is being used to justify a highly extractive reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_review_quality_vs_theater,
    'Is the peer review process a genuine coordination mechanism for scientific quality, or a theatrical performance to justify the extraction by publishers?',
    'Comparative studies of outcomes (e.g., retraction rates, citation impact, reproducibility) from high-prestige journals vs. open-access platforms with different review models.',
    'If the quality control function is proven to be minimal or purely theatrical, the constraint degrades from a Tangled Rope to a pure Snare. If it is proven essential, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_quality_vs_theater, empirical, 'Whether peer review provides real quality control or is merely theatrical justification for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u1_exp_r4, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u1_exp_r4, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u1_exp_r4, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u1_exp_r4, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u1_exp_r4, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u1_exp_r4, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u1_exp_r4, base_extractiveness, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u1_exp_r4, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r4, university_tenure_process).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r4, research_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
