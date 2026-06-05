% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u3_str_r1
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u3_str_r1, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u3_str_r1
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic peer review system is a mechanism where researchers provide
 *   free labor (writing, peer review, editing) to for-profit publishers.
 *   These publishers then sell access to the resulting research, often funded
 *   by public grants, back to the researchers' own institutions at
 *   significant markups. This creates a value loop where the producers and
 *   primary consumers of the content subsidize the profits of third-party
 *   gatekeepers.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must publish to secure tenure, providing free labor.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture immense value from free labor and content.
 *   - University Libraries: Victims and auditors (institutional/constrained) - face budget crises due to exorbitant subscription fees.
 *   - Tenure Committees: Secondary beneficiaries (institutional/constrained) - use journal prestige as a proxy for evaluating research quality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_str_r1, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u3_str_r1, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u3_str_r1, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_str_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_str_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_str_r1, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u3_str_r1, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u3_str_r1, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u3_str_r1, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u3_str_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_str_r1, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_str_r1, university_tenure_committees).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_str_r1, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_str_r1, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_str_r1, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a junior academic, the 'publish or perish' mandate makes this system a coercive trap. They must provide free labor (writing, reviewing) to the very entities that their institutions must pay for access, with their career progression held hostage.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the publisher, the system is an elegant and highly efficient coordination mechanism. It secures a steady stream of high-quality, no-cost content and peer review labor, which is then packaged and sold at a high margin. The extraction is seen as a reward for coordinating the market for academic prestige.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view, often held by library consortia who must negotiate subscription bundles, recognizes the dual nature. The system does coordinate the dissemination and credentialing of research (a Rope function), but does so via a highly extractive and coercive model (a Snare function).
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% As an institutional actor with a long-term view and arbitrage options (e.g., investing in open-access infrastructure), the constraint is a Tangled Rope. It provides a legible (if flawed) system for evaluating faculty performance but imposes unsustainable financial costs and perverse incentives.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u3_str_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_str_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_str_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u3_str_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.68) is high due to the direct conversion of free academic labor into publisher profit and the sale of publicly funded research back to public institutions. The suppression score (0.75) is high because the 'publish or perish' culture, enforced by tenure committees, makes participation non-optional for career academics, strongly disincentivizing alternative publication models.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Publishers see an efficient Rope that coordinates the chaotic world of research into a prestige-ranked, profitable order. Junior academics experience a Snare where their career is contingent on participating in a system that extracts their labor for free. Analytical observers like library consortia see the whole picture: a Tangled Rope where a genuine coordination function has been captured for extractive purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from researchers (labor) and universities/public (funding) to the publishers (profit). Publishers and tenure committees are beneficiaries, as the system provides them with profit and a simple evaluative heuristic, respectively. Researchers, their institutions' libraries, and the general public are the victims, bearing the costs of labor, subscriptions, and access denial.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would miss the fact that the system *does* perform a genuine coordination function: it organizes vast amounts of research and provides a (flawed) signaling mechanism for quality and impact. The Tangled Rope classification is crucial because it correctly identifies this dual nature, preventing mandatrophy by acknowledging the coordination benefit that coexists with and is used to justify the severe extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_vs_access,
    'Is the prestige conferred by high-impact journals an irreducible coordination good, or a manufactured scarcity that primarily serves to justify extraction?',
    'Longitudinal analysis of career outcomes and citation impact for researchers who publish exclusively in open-access, non-prestige-ranked venues versus those in traditional high-impact journals.',
    'If prestige is a manufactured artifact of the system, the constraint is functionally a pure Snare. If it provides an essential, non-replicable coordination signal for quality, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_vs_access, empirical, 'Whether journal prestige is an essential coordination good or a manufactured scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u3_str_r1, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_peer_review_gatekeeping_u3_str_r1, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acad_tr_t1995, academic_peer_review_gatekeeping_u3_str_r1, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u3_str_r1, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_peer_review_gatekeeping_u3_str_r1, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acad_be_t1995, academic_peer_review_gatekeeping_u3_str_r1, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u3_str_r1, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u3_str_r1, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_str_r1, university_tenure_system).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_str_r1, public_research_funding_access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
