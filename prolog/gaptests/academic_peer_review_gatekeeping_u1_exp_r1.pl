% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u1_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u1_exp_r1, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u1_exp_r1
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model functions as a constraint where
 *   researchers, primarily at public institutions, provide free labor
 *   (writing, peer review, editing) to for-profit publishers. These
 *   publishers then sell access to the resulting research back to the same
 *   institutions at extremely high subscription rates. This creates a cycle
 *   of value extraction built upon the career necessities ('publish or
 *   perish') of academics.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must publish in prestigious journals to secure tenure, providing free labor.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free labor and monopolistic pricing.
 *   - University Libraries: Constrained institutional victims - Forced to pay exorbitant subscription fees.
 *   - Library Consortia: Analytical observers - Evaluate the cost-benefit and negotiate with publishers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r1, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u1_exp_r1, 0.8).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u1_exp_r1, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r1, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r1, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r1, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u1_exp_r1, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u1_exp_r1, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u1_exp_r1, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u1_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r1, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r1, university_tenure_committees).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r1, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r1, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r1, the_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic, 'publish or perish' in high-impact, paywalled journals is a career necessity. They provide free labor (writing, reviewing) to a system that extracts from them and their institutions, with few viable alternatives for career progression.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's view, they are coordinating a complex system of quality control and dissemination, providing a valuable service (prestige, indexing, archiving) that justifies their business model. The extraction is seen as revenue for services rendered.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer sees both the genuine coordination function (standardizing and distributing research) and the highly extractive, asymmetric relationship where value is captured from free academic labor and sold back at a premium.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Libraries are institutional actors but are constrained. They must subscribe to expensive journal bundles to serve their faculty, making them unwilling participants in an extractive system they understand well but cannot easily exit.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u1_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u1_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is extremely high, reflecting the profit margins of major publishers built on unpaid labor. Suppression (0.80) is also high due to the 'publish or perish' culture and the lock-in of journal prestige, which makes alternative publishing venues risky for early-career researchers.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: junior researchers experience a Snare, trapped by career requirements. Publishers view their role as a Rope, coordinating the complex process of scholarly communication. Analytical observers and libraries see the reality: a Tangled Rope, where a genuine coordination function has been co-opted for massive, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is unidirectional. Labor and content flow from academics to publishers for free. Money flows from university libraries (often publicly funded) to publishers. The publishers and the tenure committees who rely on journal prestige as a heuristic are the primary beneficiaries. The researchers, their institutions, and the public who cannot access the research they funded are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic example of mandatrophy. A system for coordinating scientific validation (Rope) has been captured by actors who maximize extraction (Snare). The Tangled Rope classification is essential because it acknowledges the system's dual function. Misclassifying it as a pure Snare would ignore the real coordination and prestige-granting functions that give the system its power. Misclassifying it as a Rope would ignore the crippling extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_review_quality_vs_rent_seeking,
    'Is the high cost of journal access a necessary price for ensuring high-quality peer review and curation, or is it primarily rent-seeking on a captured market?',
    'Comparative analysis of article quality and retraction rates between high-cost subscription journals and high-quality, low-cost open access journals or pre-print archives.',
    'If primarily for quality, the constraint has a stronger Rope component. If primarily rent-seeking, it is almost a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_quality_vs_rent_seeking, empirical, 'Whether journal costs reflect quality control or rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u1_exp_r1, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u1_exp_r1, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2002, academic_peer_review_gatekeeping_u1_exp_r1, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u1_exp_r1, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u1_exp_r1, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(acad_be_t2002, academic_peer_review_gatekeeping_u1_exp_r1, base_extractiveness, 2002, 0.6).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u1_exp_r1, base_extractiveness, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u1_exp_r1, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r1, university_tenure_process).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r1, public_access_to_research).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
