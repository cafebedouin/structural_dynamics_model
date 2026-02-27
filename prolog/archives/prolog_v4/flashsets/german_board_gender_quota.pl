% ============================================================================
% CONSTRAINT STORY: german_board_gender_quota
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_board_gender_quota, []).

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
 *   constraint_id: german_board_gender_quota
 *   human_readable: German Gender Quota for Corporate Boards (FüPoG II)
 *   domain: economic
 *
 * SUMMARY:
 *   The German Gender Quota for Corporate Boards (FüPoG II) is a law
 *   mandating a minimum percentage of women on the supervisory boards of
 *   publicly listed and co-determined companies in Germany. The law aims to
 *   promote gender equality in leadership positions but may also impose
 *   constraints on companies. The law is a hybrid of coordination and
 *   extraction, creating a Tangled Rope.
 *
 * KEY AGENTS:
 *   - Women Seeking Board Positions: Beneficiaries, gaining access to positions they might not have otherwise obtained.
 *   - Companies with Limited Qualified Candidates: Victims, facing potential penalties and challenges in finding qualified women.
 *   - Gender Equality Advocacy Groups: Beneficiaries, seeing their goals advanced.
 *   - Existing Male Board Members: Victims, potentially losing their positions to make way for women.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_board_gender_quota, 0.35).
domain_priors:suppression_score(german_board_gender_quota, 0.45).
domain_priors:theater_ratio(german_board_gender_quota, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_board_gender_quota, extractiveness, 0.35).
narrative_ontology:constraint_metric(german_board_gender_quota, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(german_board_gender_quota, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_board_gender_quota, tangled_rope).
narrative_ontology:human_readable(german_board_gender_quota, "German Gender Quota for Corporate Boards (FüPoG II)").
narrative_ontology:topic_domain(german_board_gender_quota, "economic").

domain_priors:requires_active_enforcement(german_board_gender_quota).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_board_gender_quota, women_seeking_board_positions).
narrative_ontology:constraint_beneficiary(german_board_gender_quota, gender_equality_advocacy_groups).
narrative_ontology:constraint_victim(german_board_gender_quota, companies_with_limited_qualified_candidates).
narrative_ontology:constraint_victim(german_board_gender_quota, existing_male_board_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Companies that struggle to find qualified female candidates may see the quota as a Snare, forcing them to comply with potentially unqualified hires to avoid penalties. They are trapped within the German legal system and experience extraction with little benefit.
constraint_indexing:constraint_classification(german_board_gender_quota, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% These groups experience the quota as a rope, facilitating their goals of achieving gender equality in corporate leadership. They benefit from the increased representation of women on corporate boards.
constraint_indexing:constraint_classification(german_board_gender_quota, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Companies that can readily comply with the quota may experience it as a tangled rope: they benefit from improved public image and potentially better decision-making, but they also face constraints in their hiring practices and internal promotions.
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The Analytical Observer sees a Tangled Rope: a law that attempts to coordinate gender equality but also extracts from companies and individuals. The effectiveness and long-term consequences of the quota are subject to debate.
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_board_gender_quota_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_board_gender_quota, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_board_gender_quota, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(german_board_gender_quota_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.35 - The quota extracts from companies by limiting their pool of candidates and potentially forcing them to hire less qualified individuals. Suppression: 0.45 - The quota suppresses alternative hiring practices and diversity initiatives that might not involve quotas. Theater Ratio: 0.25 - The law is primarily functional with little theater. The main aim is to increase the number of women on boards, not just create the appearance of doing so.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different experiences of the stakeholders. Companies with limited qualified candidates see a Snare, while gender equality advocacy groups see a Rope. Companies that can readily comply experience a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Women seeking board positions and gender equality advocacy groups are beneficiaries, and thus have negative directionality. Companies with limited qualified candidates and existing male board members are victims, and thus have positive directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    availability_qualified_women,
    'Is there a sufficient pool of qualified women to fill the board positions mandated by the quota?',
    'Track the number of qualified women available for board positions and compare it to the number of positions needing to be filled.',
    'If there are not enough qualified women, the quota will be highly extractive. If there are sufficient qualified women, the quota will primarily function as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(availability_qualified_women, empirical, 'Availability of qualified women for board positions').

omega_variable(
    impact_company_performance,
    'Does the gender quota lead to improved or worsened company performance?',
    'Conduct empirical studies comparing the performance of companies that comply with the quota to those that don''t.',
    'If company performance improves, the quota may be reclassified as a Rope. If company performance worsens, the quota may be reclassified as a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_company_performance, empirical, 'Impact on company performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_board_gender_quota, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(germ_tr_t0, german_board_gender_quota, theater_ratio, 0, 0.15).
narrative_ontology:measurement(germ_tr_t2, german_board_gender_quota, theater_ratio, 2, 0.2).
narrative_ontology:measurement(germ_tr_t5, german_board_gender_quota, theater_ratio, 5, 0.25).

% Extraction over time
narrative_ontology:measurement(germ_be_t0, german_board_gender_quota, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(germ_be_t2, german_board_gender_quota, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(germ_be_t5, german_board_gender_quota, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_board_gender_quota, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
