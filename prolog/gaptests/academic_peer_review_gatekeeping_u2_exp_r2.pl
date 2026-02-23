% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r2, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r2
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system relies on researchers, editors, and
 *   reviewers providing free or low-cost labor to for-profit publishing
 *   houses. These publishers then sell access to the resulting research,
 *   often funded by public grants, back to the researchers' own institutions
 *   (via university libraries) at extremely high subscription costs. This
 *   creates a circular flow where value is extracted from the academic
 *   community and taxpayers and concentrated in the publishing industry.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must publish in high-impact journals to secure tenure, providing free labor in the process.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture the value of academic labor and public research funding.
 *   - University Libraries/Consortia: Secondary victims (organized/constrained) - Bear the direct financial cost of journal subscriptions.
 *   - Open Access Advocates: Analytical observers (analytical/analytical) - Analyze and critique the system's extractive nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r2, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r2, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r2, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r2, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r2, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r2, for_profit_journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r2, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r2, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r2, taxpayers_funding_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic, publishing in prestigious, paywalled journals is often a non-negotiable requirement for career advancement ('publish or perish'). They provide free labor (research, writing, reviewing) and are trapped by the system's credentialing function.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, this is a highly efficient coordination mechanism. They leverage academic norms to acquire high-value content and quality control for free, creating a product with high margins. The extraction is the business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view sees both the genuine coordination function (peer review as a quality filter) and the immense, asymmetric extraction. It recognizes the system's value while quantifying the costs imposed on the public and academia.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Libraries are victims of high subscription costs but have some organized power. They can negotiate as consortia and threaten cancellations (the 'big deal'), but are constrained by faculty demand for access to key journals.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the publishers' profit margins, built on unpaid labor. The high suppression (0.75) comes from the 'publish or perish' culture and the monopoly on prestige held by top-tier journals, which makes alternative publishing routes risky for career-focused academics. The system requires active enforcement through tenure committees and grant review panels that use journal prestige as a proxy for research quality.
 *
 * PERSPECTIVAL GAP:
 *   A vast gap exists between the publisher, who sees an efficient Rope for coordinating and monetizing research, and the junior professor, who experiences a Snare that coerces free labor for career survival. The analytical perspective of a librarian or open-access advocate sees the Tangled Rope: a system with a real coordination function (quality control) that has been captured for asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. For-profit publishers are the beneficiaries, extracting financial value. Researchers, their institutions, and the public (who fund the research and library budgets) are the victims, providing uncompensated labor and paying monopoly rents for access to knowledge they created.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is crucial. A pure Snare classification would ignore the genuine, if often imperfect, coordination function of peer review in maintaining scholarly standards. A pure Rope classification would ignore the massive, coercive extraction. The Tangled Rope correctly identifies that a valuable coordination mechanism has been fused with a highly extractive business model, preventing mischaracterization from either extreme.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_review_quality_vs_extraction,
    'Is the publisher's gatekeeping and high cost a necessary price for ensuring research quality, or is it primarily rent-seeking that adds little marginal value over non-profit or open models?',
    'Comparative studies of article quality, retraction rates, and scientific impact between high-cost commercial journals and high-quality open-access or society-run journals.',
    'If necessary for quality, the constraint is a costly but functional Tangled Rope. If primarily rent-seeking, it is functionally a Snare masquerading as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_quality_vs_extraction, empirical, 'Whether the publisher's extraction is a necessary cost for quality control or is primarily rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r2, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u2_exp_r2, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r2, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r2, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u2_exp_r2, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r2, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r2, university_tenure_process).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r2, public_access_to_research).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
