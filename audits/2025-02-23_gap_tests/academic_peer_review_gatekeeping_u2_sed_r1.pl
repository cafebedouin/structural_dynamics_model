% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_sed_r1
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_sed_r1, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_sed_r1
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system is a mature constraint where researchers,
 *   motivated by career progression ('publish or perish'), provide free labor
 *   (writing, reviewing, editing) to for-profit publishers. These publishers
 *   then erect paywalls and sell access to the research—often publicly
 *   funded—back to the researchers' own institutions at exorbitant prices.
 *   The system's persistence relies on the conflation of journal prestige
 *   with scientific quality, a norm actively enforced by tenure and grant
 *   committees.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must participate to secure a career.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture immense value from the free labor and prestige economy.
 *   - University Libraries/Consortia: Constrained victims (organized/constrained) - must pay escalating fees to provide necessary resources for their faculty and students.
 *   - Tenured Senior Academics: Secondary beneficiaries/enforcers (powerful/mobile) - control the committees that uphold the system from which they benefited.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r1, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_sed_r1, 0.85).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_sed_r1, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r1, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r1, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r1, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_sed_r1, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_sed_r1, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_sed_r1, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_sed_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r1, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r1, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r1, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r1, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r1, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The 'publish or perish' system is a coercive trap with no viable exit for those seeking tenure within the established academic career path.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's perspective, this is a highly efficient coordination mechanism for curating and distributing high-quality research, creating value for which they are compensated.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Library consortia see both the coordination function (access to vetted research is necessary) and the severe extraction (unsustainable subscription costs), making it a tangled rope they are forced to negotiate.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r1, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the dual nature: a genuine coordination function (quality control) has been captured by an extractive business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% As a moderate actor, I see the system's necessity for career progression but also its deep flaws and extractive nature. I can navigate it or potentially leave the system, but changing it is difficult.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r1, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_sed_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_sed_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.78) is extremely high, reflecting the publishers' profit margins built on unpaid labor and public funding. Suppression (0.85) is also very high because alternatives like pre-print archives or open-access journals lack the 'prestige' required for career advancement, making the legacy system effectively mandatory for ambitious academics.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Junior Professor (who sees a Snare) and the Publisher (who sees a Rope). The professor is trapped in a system that extracts their labor for free under threat of career failure. The publisher sees an efficient market mechanism for coordinating, vetting, and distributing knowledge, for which they provide a valuable service. The analytical view of Tangled Rope reconciles these by acknowledging both the real coordination function and the parasitic extraction layered on top of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Value flows unidirectionally from researchers and their institutions (and by extension, taxpayers) to the shareholders of publishing houses. Junior professors and university libraries are the primary cost-bearers (victims). Publishers and the senior academics who act as gatekeepers are the primary beneficiaries, capturing both monetary value and prestige.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would ignore that peer review does, in principle, serve a vital coordination function of quality control. Classifying it as a Rope would ignore the grotesque level of asymmetric extraction. The Tangled Rope classification is essential for resolving this mandatrophy, correctly identifying a system where a legitimate coordination goal has been almost entirely subsumed by a rent-seeking, extractive enterprise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_vs_quality,
    'Does the peer review system primarily select for genuine scientific quality, or does it enforce conformity and select for prestige signals?',
    'Comparative analysis of citation impact and replicability between papers in high-prestige journals and those in open-access or lower-tier venues.',
    'If primarily quality, it's a high-cost Tangled Rope. If primarily prestige/conformity, it's a Snare with a thin veneer of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_vs_quality, empirical, 'Whether the system selects for quality or enforces conformity and prestige.').

omega_variable(
    open_access_viability,
    'Can alternative models like pre-print servers and open-access journals fully replace the prestige and signaling function of the legacy system?',
    'Tracking hiring and tenure decisions over a decade to see if publications in alternative venues gain equal weight to legacy journals.',
    'If yes, the suppression score of the legacy system drops, potentially reclassifying it as a Piton. If no, it remains a high-suppression Snare/Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_access_viability, empirical, 'The viability of open access models to replace the legacy system's prestige function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_sed_r1, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_sed_r1, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(acad_tr_t2002, academic_peer_review_gatekeeping_u2_sed_r1, theater_ratio, 2002, 0.45).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u2_sed_r1, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_sed_r1, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acad_be_t2002, academic_peer_review_gatekeeping_u2_sed_r1, base_extractiveness, 2002, 0.65).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u2_sed_r1, base_extractiveness, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_sed_r1, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r1, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r1, public_trust_in_science).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r1, intellectual_property_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
