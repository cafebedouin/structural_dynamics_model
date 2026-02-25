% ============================================================================
% CONSTRAINT STORY: algeria_france_colonial_legacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algeria_france_colonial_legacy, []).

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
 *   constraint_id: algeria_france_colonial_legacy
 *   human_readable: The persistent structural legacy of French colonization in Algeria
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint models the enduring, multi-generational impact of
 *   France's 132-year colonization of Algeria. After formal independence in
 *   1962, the deep structures of economic dependency, political influence,
 *   and cultural orientation remained. These structures were not accidental;
 *   they were the result of a colonial project designed for resource
 *   extraction and the suppression of indigenous sovereignty. The legacy
 *   persists through linguistic ties, established trade routes, monetary
 *   policy influence (historically via the Franc zone), and the vested
 *   interests of both French corporations and a co-opted Algerian elite.
 *
 * KEY AGENTS:
 *   - Algerian Populace: Primary target (powerless/trapped) — bears the costs of underdevelopment and limited sovereignty.
 *   - French State and Corporations: Primary beneficiary (institutional/arbitrage) — reaps economic and geopolitical benefits from the established relationship.
 *   - Algerian Comprador Elites: Secondary beneficiary/enforcer (powerful/mobile) — maintains power and wealth by managing the extractive system.
 *   - Post-Colonial Analyst: Analytical observer (analytical/analytical) — identifies the hybrid nature of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algeria_france_colonial_legacy, 0.68).
domain_priors:suppression_score(algeria_france_colonial_legacy, 0.75).
domain_priors:theater_ratio(algeria_france_colonial_legacy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algeria_france_colonial_legacy, tangled_rope).
narrative_ontology:human_readable(algeria_france_colonial_legacy, "The persistent structural legacy of French colonization in Algeria").
narrative_ontology:topic_domain(algeria_france_colonial_legacy, "geopolitical/economic").

domain_priors:requires_active_enforcement(algeria_france_colonial_legacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, french_state_and_corporations).
narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, algerian_comprador_elites).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, algerian_populace).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, algerian_sovereign_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGERIAN POPULACE (SNARE) — Experiences the legacy as a trap of limited economic opportunity, political disenfranchisement, and cultural dependency. Exit is impossible for the vast majority. High base extraction and trapped status yield a χ value deep in the snare category (χ ≈ 0.96), reflecting a system of pure coercive extraction.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRENCH STATE (ROPE) — Perceives the relationship as a beneficial coordination mechanism for managing trade, migration, and geopolitical interests. With arbitrage exit options, the directionality is inverted, resulting in negative effective extraction (χ ≈ -0.10). The constraint is a net subsidy, appearing as pure coordination.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ALGERIAN ELITE (TANGLED ROPE) — Benefits from the extractive system but is also constrained by it. Their power depends on maintaining the structure, which requires active management of both popular dissent and the relationship with France. They see both the coordination function (maintaining power) and the extraction they facilitate. High suppression and active enforcement are salient features.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's true nature. It possesses a genuine (though perverse) coordination function (language, trade routes, elite networks) layered onto a foundation of severe asymmetric extraction. The high suppression of alternatives and requirement for active enforcement confirm the Tangled Rope classification, distinguishing it from a natural economic outcome.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algeria_france_colonial_legacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algeria_france_colonial_legacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(algeria_france_colonial_legacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high, reflecting persistent unequal terms of trade, capital flight, and brain drain that systematically benefit the French economy at the expense of Algerian development. Suppression (0.75) is also high; the colonial project actively dismantled or co-opted pre-existing institutions, and post-colonial structures make it extremely difficult and costly for Algeria to pursue truly independent development paths. Theater Ratio (0.35) is moderate; while there is significant diplomatic and cultural performance around the 'special relationship,' the underlying economic extraction is highly functional and not merely inertial.
 *
 * PERSPECTIVAL GAP:
 *   The profound perspectival gap is central to this constraint's persistence. The French state experiences a beneficial Rope, a natural and efficient coordination of mutual interests. The Algerian populace experiences a coercive Snare, a system from which they cannot escape and which extracts their future. The Algerian elite navigates a Tangled Rope, benefiting from their position while being bound to the system's logic. The analytical view confirms the Tangled Rope, recognizing that the 'coordination' seen by the beneficiary is the very mechanism of extraction experienced by the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation correctly captures the structural positions. The French state (beneficiary + arbitrage) has a low 'd' value, leading to negative effective extraction (a subsidy). The Algerian populace (victim + trapped) has a very high 'd' value, leading to maximum effective extraction. The Algerian elite (beneficiary + mobile) has a low 'd', but their national scope and role as enforcers place them in a complex hybrid position. The analytical observer's default 'd' value, combined with the high base metrics, correctly identifies the severe extraction inherent in the system's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution of mandatrophy. A simplistic analysis might label the entire system a Snare, ignoring the agency and benefits accrued by local elites, or a Rope, echoing the neocolonial narrative of 'cooperation.' The Tangled Rope classification is essential, as it correctly identifies the simultaneous existence of a genuine coordination function (for the elites and France) and a deeply extractive function (for the populace). It forces an analysis of *who* benefits and *who* pays, revealing that the coordination *is* the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_factors,
    'Are Algeria''s developmental challenges primarily a direct result of the colonial legacy, or are they driven by internal governance failures and broader global economic pressures?',
    'Comparative analysis with non-colonized nations with similar resource profiles; econometric models isolating the causal impact of specific colonial-era institutions.',
    'If primarily legacy-driven, the Snare/Tangled Rope classifications are confirmed. If primarily internal/global, the constraint might be better modeled as a Rope with high friction or a Mountain of development economics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_factors, empirical, 'Distinguishing colonial legacy impact from other developmental factors').

omega_variable(
    elite_agency,
    'To what degree are Algerian elites independent actors pursuing their own interests versus agents structurally determined by the post-colonial system?',
    'Analysis of capital flight patterns, policy decisions that deviate from French interests, and elite network formation outside the Franco-Algerian sphere.',
    'High agency suggests their perspective is closer to a beneficiary of a Rope they help maintain. Low agency suggests they are also partially victims of a Tangled Rope, trapped in a system they must perpetuate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_agency, conceptual, 'Assessing the independent agency of Algerian elites').

omega_variable(
    pathway_to_exit,
    'Can alternative economic and political alliances (e.g., with China, Russia, or Pan-African blocs) provide a genuine exit from the constraint, or do they merely substitute one form of dependency for another?',
    'Longitudinal study of trade balance, debt structure, and policy sovereignty in nations that have attempted similar geopolitical pivots.',
    'If genuine exit is possible, the ''suppression'' score is too high and the system is less coercive than a Snare. If it''s dependency substitution, the high suppression score is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathway_to_exit, empirical, 'Viability of alternative alliances as an exit from dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algeria_france_colonial_legacy, 0, 62).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alge_tr_t0, algeria_france_colonial_legacy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(alge_tr_t31, algeria_france_colonial_legacy, theater_ratio, 31, 0.25).
narrative_ontology:measurement(alge_tr_t62, algeria_france_colonial_legacy, theater_ratio, 62, 0.35).

% Extraction over time
narrative_ontology:measurement(alge_be_t0, algeria_france_colonial_legacy, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(alge_be_t31, algeria_france_colonial_legacy, base_extractiveness, 31, 0.72).
narrative_ontology:measurement(alge_be_t62, algeria_france_colonial_legacy, base_extractiveness, 62, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algeria_france_colonial_legacy, resource_allocation).
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, francafrique_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
