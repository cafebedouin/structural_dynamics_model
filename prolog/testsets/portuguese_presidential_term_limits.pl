% ============================================================================
% CONSTRAINT STORY: portuguese_presidential_term_limits
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_portuguese_presidential_term_limits, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: portuguese_presidential_term_limits
 *   human_readable: Portuguese Constitutional Term Limits (Article 123)
 *   domain: political/legal
 *
 * SUMMARY:
 *   Article 123 of the Portuguese Constitution prohibits any President from
 *   serving more than two consecutive five-year terms. This constraint
 *   classifies as a Mountain from all observable positions because it
 *   represents an irreducible structural boundary on executive power grounded
 *   in the post-1974 democratic constitutional order. The constraint emerges
 *   from democratic theory (rotation prevents authoritarianism),
 *   constitutional law (1976 Carnation Revolution legacy), and political
 *   economy (executives with indefinite tenure accumulate state capture
 *   risk). The term limit has remained invariant across 50 years of
 *   Portuguese political history, constitutional amendments to other
 *   articles, and changes in incumbent party. No serious political actor has
 *   successfully challenged it, not because of suppressive coercion but
 *   because the constitutional amendment process (2/3 supermajority
 *   requirement) makes repeal structurally inaccessible. The constraint
 *   exhibits zero degrees of freedom for all indices: powerless citizens
 *   cannot vote around it; institutional actors cannot reinterpret it;
 *   powerful presidents cannot extend tenure; analytical observers cannot
 *   dissolve it as mere convention. Theater ratio is minimal (0.15) because
 *   the constraint is self-executing: once two terms are completed, the
 *   Constitutional Court's role is administrative, not performative.
 *   Extractiveness is minimal (0.08) because no agent extracts value from the
 *   constraint — it redistributes future executive power opportunities rather
 *   than concentrating them. Suppression is negligible (0.02) because the
 *   constraint achieves its structural function through inaccessibility, not
 *   coercion.
 *
 * KEY AGENTS:
 *   - Portuguese Electorate: Constrained by constitutional prohibition; cannot vote for third-term incumbents regardless of preference
 *   - Incumbent President (term-limited): Faces immutable institutional barrier to reelection after two consecutive terms
 *   - Portuguese Constitutional Court: Enforces Article 123 interpretation with zero discretion; appears as institutional conduit for constitutional law
 *   - Parliament (2/3 supermajority): Gatekeepers of constitutional amendment; supermajority requirement makes repeal effectively inaccessible
 *   - Successor Presidents: Benefit from rotation principle — constraint guarantees periodic power transfer
 *   - Analytical Observer: Sees constraint as natural law of democratic constitutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portuguese_presidential_term_limits, 0.08).
domain_priors:suppression_score(portuguese_presidential_term_limits, 0.02).
domain_priors:theater_ratio(portuguese_presidential_term_limits, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, extractiveness, 0.08).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portuguese_presidential_term_limits, mountain).
narrative_ontology:human_readable(portuguese_presidential_term_limits, "Portuguese Constitutional Term Limits (Article 123)").
narrative_ontology:topic_domain(portuguese_presidential_term_limits, "political/legal").

domain_priors:emerges_naturally(portuguese_presidential_term_limits).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PORTUGUESE ELECTORATE (MOUNTAIN) — The constitutional prohibition on three consecutive presidential terms is experienced as an immutable institutional law. Citizens cannot vote for a sitting president beyond two consecutive five-year terms, regardless of electoral preference or political context. The constraint is enforced by the Constitutional Court and institutional consensus. No exit option exists within the constitutional framework.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PORTUGUESE CONSTITUTIONAL COURT (MOUNTAIN) — The court interprets Article 123 as a fundamental structural constraint on executive power, derived from principles of democratic rotation and separation of powers encoded in the 1976 Constitution. The court has no discretion to modify or relax the constraint. It appears as natural law of the constitutional system, enforced through interpretation doctrine with zero ambiguity.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a comparative constitutionalism perspective, the two-consecutive-term limit reflects a universal democratic principle: rotation of executive power prevents accumulation of state apparatus control and reduces the structural risk of authoritarianism. This limit appears as a natural law of democratic constitutional design, with accessibility to alternatives (repealing the constitutional article) being structurally so costly that it functions as an immutable boundary. The constraint is justified by irreducible political economy: executives with indefinite tenure risk capturing state institutions.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INCUMBENT PRESIDENT (MOUNTAIN) — After two terms, the president cannot run again without amending the Constitution — a process requiring 2/3 supermajority in Parliament and faces strong constitutional resistance. The constraint is experienced as an immutable limit on individual political career trajectory. No realistic exit path exists within a single presidential cycle. The president can attempt constitutional amendment but this has extremely low success probability and high political cost.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portuguese_presidential_term_limits_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(portuguese_presidential_term_limits, ExtMetricName, E),
    domain_priors:suppression_score(portuguese_presidential_term_limits, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(portuguese_presidential_term_limits),
    narrative_ontology:constraint_metric(portuguese_presidential_term_limits, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(portuguese_presidential_term_limits, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(portuguese_presidential_term_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. No agent extracts concentrated value from the term limit. The constraint redistributes executive opportunity across time and individuals rather than extracting rent from a group. The incumbent president bears a cost (forced exit), but this is a structural fairness mechanism, not extraction in the DR sense. Suppression (0.02): Negligible. The constraint operates through constitutional inaccessibility, not through active suppression or lack of alternatives. Citizens have electoral choice within the permitted terms. The president can seek other offices. No coercive apparatus is required because the rule is self-executing. Theater ratio (0.15): Very low. Constitutional Court enforcement is straightforward legal interpretation, not performative. No ritual or symbolic content is required because the constraint is embedded in institutional structure. When a term limit is reached, the president simply cannot run — no ceremony or legitimation theater is needed.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Mountain classification, indicating a uniform-type constraint. The perspectival gap is minimal because the constraint operates identically across all observation contexts: citizens, courts, presidents, and analysts all experience the same immutable boundary. The only variation is in justification (democratic rotation principle vs constitutional law vs political economy risk) but the experienced constraint is identical. This uniformity is the defining characteristic of a Mountain — it appears as natural law across all indices.
 *
 * DIRECTIONALITY LOGIC:
 *   Article 123 creates no meaningful directionality because there is no extraction relationship. The constraint redistributes power opportunistically (ensuring no single president accumulates indefinite state control) rather than concentrating it. Therefore, standard beneficiary/victim analysis does not apply. All agents bear the same structural constraint with equal force. The incumbent president is the temporary 'loser' (forced exit), but the constraint benefits the democratic system as a whole by preventing authoritarianism. This non-extraction structure is the core reason for Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by being a uniform-type Mountain. There is no risk of mislabeling coordination as pure extraction because there is no coordination function and no extraction. The constraint is purely redistributive: it ensures power rotation rather than concentration. All six DR types would incorrectly classify this constraint if applied independently, which is why the unified Mountain classification across all perspectives is the correct structural reading. The constraint's legitimacy derives from a democratic principle (rotation prevents authoritarianism), not from any coordination benefit or extraction relationship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_feasibility_threshold,
    'What political conditions would make constitutional amendment to remove term limits feasible, and how does the answer change the constraint''s classification?',
    'Historical analysis of Portuguese constitutional amendments since 1976; comparison with term limit amendments in other European democracies (France 2000, Austria debates); assessment of supermajority coalition stability',
    'If amendment is theoretically possible but requires >80% consensus: constraint remains Mountain (accessibility collapse remains >0.85). If amendment has ever succeeded for other articles with similar thresholds: constraint may degrade to Rope with very high suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_feasibility_threshold, empirical, 'Feasibility threshold for constitutional amendment to remove term limits').

omega_variable(
    comparative_democratic_legitimacy,
    'Is the two-consecutive-term limit a universal principle of democratic governance or a contingent design choice specific to Portugal''s post-authoritarian context?',
    'Comparative analysis: how many democracies with similar institutional stability use identical limits vs variants (e.g., two terms non-consecutive, three-term absolute, no limit with supermajority requirements); analysis of academic democratic theory consensus on optimal term lengths',
    'If universal principle: Mountain classification confirmed from all perspectives. If contingent design choice: constraint may appear as Tangled Rope from analytical perspective (coordination principle + restricted agency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_democratic_legitimacy, conceptual, 'Whether term limits are universal democratic principle or contingent design').

omega_variable(
    informal_enforcement_mechanisms,
    'Does informal party/media pressure play a role comparable to constitutional enforcement, or is the constitutional rule self-executing?',
    'Analysis of instances where presidents approached or tested the boundary (e.g., political signals from incumbent toward third run); assessment of whether party gatekeeping or media framing prevented attempts before formal constitutional bar activated',
    'If informal enforcement is strong: suppression classification may be lower (active enforcement not needed because cultural acceptance is high). If constitutional rule alone prevents attempts: suppression remains minimal (structure, not coercion, prevents exit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_enforcement_mechanisms, empirical, 'Role of informal enforcement vs formal constitutional prohibition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portuguese_presidential_term_limits, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_term_tr_t0, portuguese_presidential_term_limits, theater_ratio, 0, 0.12).
narrative_ontology:measurement(pres_term_tr_t25, portuguese_presidential_term_limits, theater_ratio, 25, 0.15).
narrative_ontology:measurement(pres_term_tr_t50, portuguese_presidential_term_limits, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(pres_term_be_t0, portuguese_presidential_term_limits, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(pres_term_be_t25, portuguese_presidential_term_limits, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(pres_term_be_t50, portuguese_presidential_term_limits, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portuguese_presidential_term_limits, enforcement_mechanism).
narrative_ontology:affects_constraint(portuguese_presidential_term_limits, electoral_system_proportionality_threshold).
narrative_ontology:affects_constraint(portuguese_presidential_term_limits, parliamentary_supermajority_amendment_requirements).

% DUAL FORMULATION NOTE:
% Article 123 term limits interact structurally with electoral system design and constitutional amendment procedures. The electoral system's proportionality affects coalition formation for supermajority amendments; amendment procedures determine the feasibility of constitutional change. These are separate constraints but linked through the institutional ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
