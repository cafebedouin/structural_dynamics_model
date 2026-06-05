% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping, []).

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
 *   constraint_id: academic_peer_review_gatekeeping
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Academic peer review and journal gatekeeping is a system where
 *   researchers provide free labor (peer review and editing) to for-profit
 *   publishers, who then sell access to that research back to the
 *   researchers' own institutions at significant markups. This creates a
 *   structural tension between the coordination function of peer review and
 *   the extraction of value by publishers.
 *
 * KEY AGENTS:
 *   - Journal Publishers: Primary beneficiary (institutional/arbitrage) - extract profit from the system
 *   - Early Career Researchers: Primary victim (powerless/trapped) - dependent on publications for career advancement
 *   - University Libraries: Secondary victim (moderate/constrained) - forced to pay high subscription fees
 *   - Tenured Faculty: Powerful, constrained by prestige and the need to publish.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping, 0.65).
domain_priors:suppression_score(academic_peer_review_gatekeeping, 0.7).
domain_priors:theater_ratio(academic_peer_review_gatekeeping, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, extractiveness, 0.65).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, tenured_faculty).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, early_career_researchers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, university_libraries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Early career researchers are heavily dependent on publications for career advancement and are thus trapped in the system. They bear a disproportionate cost in terms of time and effort spent on unpaid peer review and the financial burden of accessing research.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% University libraries are forced to pay exorbitant subscription fees to access journals, limiting their ability to provide resources to their patrons. They are constrained by budget limitations and the increasing cost of subscriptions.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Journal publishers benefit from the free labor of researchers and the captive market of university libraries. They can set high subscription fees and maintain their position as gatekeepers of academic knowledge.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Tenured faculty benefit from the prestige associated with publishing in high-impact journals, which reinforces their position in the academic hierarchy. They also contribute to the system by serving as editors and reviewers. While they have more job security than early career researchers, they are still somewhat constrained by the need to publish in established journals for maintaining reputation.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer sees a system that combines coordination (peer review) with asymmetric extraction (publisher profits, career advancement dependent on gatekeepers). The system requires active enforcement through tenure requirements and university library budget allocations.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Publishers extract significant profit from the system, while researchers provide free labor. Suppression (0.70): High. Researchers are heavily incentivized to publish in established journals, limiting their choices. Theater Ratio (0.60): Moderate-High. Peer review process is often seen as performative, with reviewers unable to fully verify the validity of the research.
 *
 * PERSPECTIVAL GAP:
 *   Early career researchers and university libraries experience the system as a snare, as they are trapped and extracted from. Journal publishers see it as a rope, a coordination mechanism that allows them to profit. Tenured faculty have a mixed experience, seeing it as both a tangled rope (prestige) and potentially a scaffold (if open access alternatives succeed). The analytical observer sees a tangled rope, with coordination and extraction functions intertwined.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the journal publishers, who profit from the free labor of researchers and the captive market of university libraries. The victims are the early career researchers and university libraries, who are dependent on the system and forced to pay high subscription fees. Tenured faculty benefit from prestige of publication, so their d is lower than ECRs, but their need to get promoted and reviewed constrains them still. The perspectives and directionality values derive from the real-world structural relationships between these agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Tangled Rope, not a pure Snare, because the peer review process does provide a coordination function: it helps to filter out flawed research and ensure the quality of published work. However, the system also extracts value from researchers and libraries, creating a structural imbalance. The correct analysis requires recognizing both the coordination and extraction aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_access_transition,
    'How will the transition to open access affect the power dynamics in academic publishing?',
    'Track the adoption rates of open access models, the impact on subscription fees, and the emergence of new publishing platforms.',
    'If open access becomes dominant: Publishers may lose their gatekeeping power, and researchers may have more control over their work. If subscription models persist: The extractive nature of the system may continue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_access_transition, empirical, 'Impact of open access transition on academic publishing').

omega_variable(
    alternative_metrics,
    'Can alternative metrics (altmetrics) replace traditional citation counts as a measure of research impact?',
    'Compare the correlation between altmetrics and traditional citation counts, and assess their predictive power for career advancement.',
    'If altmetrics become widely accepted: The reliance on high-impact journals may decrease, and researchers may be incentivized to share their work more broadly. If citation counts remain dominant: The gatekeeping role of journals may persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_metrics, empirical, 'Whether altmetrics can replace traditional citation counts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t0, academic_peer_review_gatekeeping, theater_ratio, 0, 0.4).
narrative_ontology:measurement(acad_tr_t10, academic_peer_review_gatekeeping, theater_ratio, 10, 0.5).
narrative_ontology:measurement(acad_tr_t20, academic_peer_review_gatekeeping, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t0, academic_peer_review_gatekeeping, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(acad_be_t10, academic_peer_review_gatekeeping, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(acad_be_t20, academic_peer_review_gatekeeping, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping, enforcement_mechanism).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, research_funding_allocation).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, tenure_promotion_metrics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
