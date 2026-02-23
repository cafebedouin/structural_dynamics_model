% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_sed_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_sed_r2, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_sed_r2
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system leverages a coordination function—quality
 *   control via peer review—to enable a highly extractive business model.
 *   Researchers, particularly junior faculty, provide free labor (writing,
 *   reviewing, editing) to for-profit publishers. These publishers then erect
 *   paywalls and sell access to the research—often publicly funded—back to
 *   the researchers' own institutions at exorbitant prices. The 'publish or
 *   perish' culture of academia serves as the primary enforcement mechanism,
 *   creating a captive labor and consumer base.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must provide free labor and publish in high-prestige journals to secure tenure and career advancement.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free academic labor and institutional subscription fees.
 *   - University Libraries: Victims and Auditors (institutional/constrained) - Forced to pay escalating subscription costs, often at the expense of other acquisitions.
 *   - The General Public: Secondary victims (powerless/trapped) - Denied access to the results of research they funded through taxes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r2, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_sed_r2, 0.85).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_sed_r2, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r2, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r2, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r2, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_sed_r2, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_sed_r2, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_sed_r2, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_sed_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r2, journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r2, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r2, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r2, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r2, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic facing 'publish or perish', the system is a coercive trap demanding free labor for career survival.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, this is a highly efficient coordination mechanism for sourcing, vetting, and distributing research, creating immense value.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view sees both the genuine coordination function (quality control) and the severe, asymmetric extraction from university budgets and public funds.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Organized actors attempting to reform the system see its tangled nature but have some agency to build alternatives, though they remain constrained by the dominant prestige economy.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r2, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_sed_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_sed_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.78) is extremely high, reflecting the direct conversion of free, skilled labor and public funding into private profit. Suppression (0.85) is also severe; for an early-career academic, refusing to participate is career suicide, making alternative publishing models risky and slow to gain traction.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the publisher, who sees a valuable Rope that coordinates and validates scientific knowledge, and the junior academic, who experiences a Snare that extracts labor under duress. The analytical perspective of a library consortium, which must negotiate with publishers, correctly identifies the dual nature of the system as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is unidirectional. Labor and content flow from academics to publishers for free. Money flows from university/public funds to publishers. The publishers and the senior academics who control the prestige economy are the clear beneficiaries. Junior academics, universities, and the public are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case where a legitimate coordination goal (vetting research) is used to justify a system of immense extraction. Classifying it as a pure Snare would ignore the real, albeit co-opted, function that provides its legitimacy. The Tangled Rope classification correctly identifies that the coordination and extraction are intertwined, which is essential for understanding why the system is so resilient to reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_separability,
    'Can the quality control function of peer review be maintained without the for-profit, high-prestige journal model?',
    'Large-scale adoption and longitudinal study of alternative models like overlay journals, university-hosted platforms, and post-publication review.',
    'If quality is separable, the current system is a pure Snare. If it is intrinsically linked, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_separability, empirical, 'Whether the quality control function is intrinsically linked to the extractive for-profit model.').

omega_variable(
    prestige_economy_inertia,
    'How much of the system's stability is due to the inertia of the prestige economy (hiring, grants, tenure) versus the publishers' direct enforcement?',
    'Analysis of hiring and funding outcomes for researchers who exclusively use non-traditional publishing venues.',
    'If inertia is dominant, the constraint is a Piton layered on a Snare. If publisher action is key, it is a classic Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prestige_economy_inertia, conceptual, 'The degree to which the system is self-perpetuating versus actively enforced by publishers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_sed_r2, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t0, academic_peer_review_gatekeeping_u2_sed_r2, theater_ratio, 0, 0.15).
narrative_ontology:measurement(acad_tr_t20, academic_peer_review_gatekeeping_u2_sed_r2, theater_ratio, 20, 0.4).
narrative_ontology:measurement(acad_tr_t40, academic_peer_review_gatekeeping_u2_sed_r2, theater_ratio, 40, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t0, academic_peer_review_gatekeeping_u2_sed_r2, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(acad_be_t20, academic_peer_review_gatekeeping_u2_sed_r2, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(acad_be_t40, academic_peer_review_gatekeeping_u2_sed_r2, base_extractiveness, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_sed_r2, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r2, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r2, scientific_research_integrity).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r2, intellectual_property_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
