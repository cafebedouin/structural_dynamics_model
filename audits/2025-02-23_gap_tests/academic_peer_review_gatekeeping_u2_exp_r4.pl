% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r4, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r4
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system relies on a value transfer where
 *   researchers and their institutions provide free labor (writing, peer
 *   review, editing) to for-profit publishers. These publishers then package
 *   the research and sell it back to the same institutions at extremely high
 *   subscription costs, effectively monetizing a publicly-funded and
 *   voluntarily-staffed process. The system's persistence is ensured by the
 *   'prestige' economy, which is critical for academic career progression.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must publish in high-prestige journals to secure tenure and funding.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free labor and institutional subscriptions.
 *   - University Libraries/Consortia: Constrained victims and analytical observers (analytical/constrained) - Forced to pay exorbitant fees, they understand the extraction but cannot easily opt out.
 *   - The General Public: Secondary victims (powerless/trapped) - Fund the research via taxes but are denied access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r4, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r4, 0.72).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r4, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r4, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r4, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r4, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r4, for_profit_journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r4, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r4, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r4, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior professor, publishing in prestigious, paywalled journals is a non-negotiable requirement for career advancement (tenure). They provide free labor (writing, reviewing) and their institutions must pay to access the final product. Exit is career suicide.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's perspective, they are providing a valuable coordination service: managing peer review, producing, and hosting scientific literature. The model is highly profitable and they have the power to acquire competitors, lobby, and set terms.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Library consortia see the full picture. They acknowledge the coordination function of journals but are acutely aware of the hyper-extractive costs ('serials crisis') that drain university budgets. They analyze the system but cannot easily exit without harming their researchers.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% The public funds a significant portion of this research through government grants, but is then denied access to the results behind a paywall, representing a pure extraction of public good for private profit.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the publishers' profit margins, built on the unpaid labor of the academic community. The high suppression (0.72) represents the powerful role of journal prestige in tenure and grant decisions, which makes alternative publishing models (like pre-print servers or institutional repositories) appear risky for career-focused academics.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the publisher, who sees a Rope (coordinating and validating science), and the junior researcher, who experiences a Snare (a coercive, extractive system they cannot escape). The library consortium, as an analytical observer, correctly identifies it as a Tangled Rope, acknowledging both the (dwindling) coordination function and the (growing) extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Value flows from researchers (labor) and their institutions/public funders (money) to the for-profit publishers. Publishers are the clear beneficiaries, while all other listed agents are victims of the extraction, differing only in their power and ability to mitigate the costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical Tangled Rope. It performs a genuine coordination function—organizing peer review and signaling research quality—that is essential for the academic system to function. However, this function has been captured to enable a highly extractive business model. It is not a pure Snare because the prestige signal, and thus the coordination function, is still actively enforced and valued by tenure committees, preventing its collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_quality_signal,
    'Is the 'prestige' of top-tier journals a genuine, irreplaceable signal of research quality, or a socially constructed lock-in that primarily serves to justify the publisher's business model?',
    'Longitudinal analysis comparing the replicability, citation impact, and retraction rates of research in top-tier journals versus high-quality open-access platforms and pre-print archives.',
    'If prestige is a robust and necessary quality signal, the system remains a Tangled Rope. If it is largely theatrical and maintained by inertia, the system is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_quality_signal, empirical, 'Whether journal prestige is a real quality signal or a theatrical justification for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r4, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r4, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r4, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u2_exp_r4, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r4, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r4, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r4, scientific_replication_crisis).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r4, intellectual_property_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
