% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r5, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r5
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system relies on researchers, funded largely by
 *   public money, to provide free labor in the form of writing, peer review,
 *   and editing for for-profit publishers. These publishers then sell access
 *   to the finished research back to the researchers' own institutions (via
 *   university libraries) at extremely high markups. The system is maintained
 *   by the academic career structure, where publication in high-prestige
 *   journals is essential for tenure and promotion.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must publish to secure tenure.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture value from free labor and control prestige.
 *   - University Libraries/Consortia: Victims and auditors (institutional/constrained) - bear the financial costs while analyzing the system's inefficiencies.
 *   - Tenured Senior Faculty: Secondary beneficiaries and enforcers (powerful/constrained) - uphold the system through their role on tenure committees.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r5, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r5, 0.72).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r5, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r5, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r5, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r5, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r5, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r5, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r5, tenured_senior_faculty).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r5, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r5, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r5, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic, publishing in prestigious journals is non-negotiable for career survival (tenure). They provide free labor (writing, reviewing) and their institution pays to access the final product. The lack of viable alternatives for career progression makes this a coercive trap.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's perspective, they are providing a valuable coordination service: managing peer review, ensuring quality, and conferring prestige. The fees are for the value added in curation and distribution. The extraction is viewed as a legitimate business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% This observer sees both sides. Peer review is a necessary coordination function for science (the Rope). However, this function has been captured by a small number of publishers who extract enormous rents by leveraging the free labor of the academic community against itself (the Snare). The result is a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% University administrators are caught. They must fund libraries to pay exorbitant subscription fees to support their researchers, who in turn must publish in those same journals to advance. They are both victims of the extraction and enforcers of the system through tenure committees.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high due to the model of free labor input and high-cost subscription output. Suppression (0.72) is high because the 'publish or perish' culture and the prestige economy tied to tenure make alternatives like pre-print servers or new open-access journals risky for career-conscious academics.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Publishers see a Rope, a service that coordinates and validates science for a fee. Junior academics experience a Snare, a coercive system they cannot escape without abandoning their careers. Analytical observers like library consortia see the full picture: a legitimate coordination function (the rope) that has been captured and weaponized for asymmetric extraction (the snare), making it a textbook Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Value flows from taxpayers (funding research) and universities (paying salaries and subscriptions) to for-profit publishers. The labor of academics is captured at zero cost. Publishers are the clear beneficiaries. Junior academics, university libraries, and the public who cannot access the research they funded are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is often defended as a pure Rope for 'quality control.' The Tangled Rope classification is crucial because it acknowledges the existence of a coordination function while correctly identifying the severe, asymmetric extraction layered on top. It prevents the system from being mislabeled as a purely beneficial coordination mechanism and highlights the coercive dynamics that trap its participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_vs_value,
    'Is the prestige conferred by high-impact journals a genuine signal of quality (coordination), or an artificial scarcity created by publishers to justify extraction (snare)?',
    'Large-scale analysis comparing long-term citation impact and reproducibility of papers from high-prestige journals versus well-regarded open-access platforms.',
    'If prestige is a robust proxy for quality, the system is a high-cost but functional Tangled Rope. If it is primarily artificial scarcity, the system is functionally a Snare masquerading as a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_vs_value, empirical, 'Whether journal prestige is a real quality signal or an artificial scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r5, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r5, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r5, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u2_exp_r5, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r5, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r5, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r5, scientific_reproducibility_crisis).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r5, intellectual_property_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
