% ============================================================================
% CONSTRAINT STORY: german_board_gender_quota
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   domain: economic/labor/governance
 *
 * SUMMARY:
 *   The German FüPoG II (Führungspositionen-Gesetz II), effective August
 *   2021, mandates 40% gender representation on corporate boards
 *   (Vorstandsräte) in listed companies and large cooperatives. This
 *   constraint exemplifies the Tangled Rope classification: it simultaneously
 *   functions as a coordination mechanism (solving centuries-old structural
 *   exclusion of women from leadership) and as an extraction mechanism
 *   (removing firms' discretion in board composition). The extractiveness
 *   score (0.35) reflects that the coordination benefit (solving a collective
 *   action problem where individual firms lack incentive to advance qualified
 *   women despite availability) is larger than the extraction cost (loss of
 *   autonomous seat allocation), but both are real. The suppression score
 *   (0.42) captures the institutional barriers to women's advancement that
 *   the quota overrides — pre-board gatekeeping, implicit bias in
 *   partner-track selection, and Old Boys Network exclusion. The theater
 *   ratio (0.55) reflects rising compliance theater: diversity training,
 *   chief diversity officer positions, public reporting on board composition,
 *   but slower change in actual decision-making power distribution or
 *   pre-board pipeline advancement. The constraint reveals the mandatrophy:
 *   framing the quota as 'pure extraction' (firms lose autonomy) misses the
 *   coordination function (society solves the collective action problem of
 *   systemic exclusion); framing it as 'pure coordination' misses the real
 *   cost to firm flexibility and the extraction of selection discretion.
 *
 * KEY AGENTS:
 *   - Underrepresented Women in Leadership: Primary beneficiary (powerful/arbitrage post-2021) — gain access to board seats previously unavailable; career pathway accelerated by quota. Pre-quota women face continued exclusion (trapped/powerless).
 *   - Board-Nominating Corporations: Primary victim of extraction (institutional/constrained) — lose autonomy over board composition; incur sourcing and training overhead; face penalties for non-compliance.
 *   - Male Incumbent Board Members: Secondary actor (moderate/constrained) — grandfather clause exemptions reduce personal impact; power dynamics among sitting directors largely unchanged.
 *   - EU Gender Equality Coalition: Organized agent (organized/constrained) — advocate groups, EU Parliament; see quota as temporary enforcement mechanism with sunset as norm internalization.
 *   - Mid-Level Female Managers: Structural beneficiary but not primary agent — quota creates pressure on firms to surface and advance qualified women from pipelines.
 *   - Analytical Observer: Systemically detects both coordination and extraction components; clarifies that constraint is not natural law but intentional hybrid mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_board_gender_quota, 0.35).
domain_priors:suppression_score(german_board_gender_quota, 0.42).
domain_priors:theater_ratio(german_board_gender_quota, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_board_gender_quota, extractiveness, 0.35).
narrative_ontology:constraint_metric(german_board_gender_quota, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(german_board_gender_quota, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_board_gender_quota, tangled_rope).
narrative_ontology:human_readable(german_board_gender_quota, "German Gender Quota for Corporate Boards (FüPoG II)").
narrative_ontology:topic_domain(german_board_gender_quota, "economic/labor/governance").

domain_priors:requires_active_enforcement(german_board_gender_quota).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_board_gender_quota, underrepresented_women_in_leadership).
narrative_ontology:constraint_beneficiary(german_board_gender_quota, corporate_legitimacy_institutions).
narrative_ontology:constraint_victim(german_board_gender_quota, male_incumbent_board_candidates).
narrative_ontology:constraint_victim(german_board_gender_quota, firm_selection_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: QUALIFIED FEMALE CANDIDATE EXCLUDED (SNARE) — Women who enter the labor market before quota implementation or in firms below thresholds face no guarantee of advancement despite qualification. The quota creates artificial scarcity of board seats for women without removing pre-board gatekeeping. d≈0.90, f(d)≈1.38, σ=1.0 → χ≈0.48.
constraint_indexing:constraint_classification(german_board_gender_quota, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN WITHIN QUOTA COHORT (ROPE) — Women hired into boards post-FüPoG II benefit from explicit coordination mechanism (reserved seats). Experience the constraint as solving a collective action problem: firms now have liability if they don't source and advance qualified women. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.004. Net coordination benefit.
constraint_indexing:constraint_classification(german_board_gender_quota, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: BOARD-NOMINATING CORPORATIONS (TANGLED ROPE) — Firms experience both coordination benefit (pressure removes internal political barriers to advancing qualified women; legitimacy gains with shareholders/stakeholders) and extraction cost (mandatory sourcing overhead, seat reservation reduces discretion, reputational risk if quota not met). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.26. Mixed structure: genuine coordination function (solve the collective action problem of systemic exclusion) plus asymmetric extraction (loss of board selection autonomy).
constraint_indexing:constraint_classification(german_board_gender_quota, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EU EQUALITY COALITION (SCAFFOLD) — Organized advocates (EU Parliament, equality commissions, women's organizations) see the quota as temporary enforcement mechanism during the transition from systemic exclusion to normative inclusion. Expects sunset: once women's representation stabilizes at 40%+ and becomes self-sustaining cultural norm, the legal mandate should be unnecessary. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.11. Low extraction because sunset is visible and coalition has agency in policy design.
constraint_indexing:constraint_classification(german_board_gender_quota, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MALE INCUMBENT BOARD COHORT (PITON) — Men already on boards see the quota as a ritual of apology and bureaucratic compliance rather than a functional constraint on their own position (most have grandfather clause exemptions or retire before quota enforcement). Theater ratio = 0.55: boards conduct diversity training, hire 'chief diversity officers', publish gender reports, but substantive power dynamics among sitting directors remain largely unchanged. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.35. Piton gate not fully satisfied (theater < 0.70), but classification captures the degraded status of the enforcement mechanism relative to the stated goal.
constraint_indexing:constraint_classification(german_board_gender_quota, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the quota simultaneously enforces inclusion (genuine coordination function: solves centuries-old structural exclusion) and extracts from firm autonomy (real loss of selection discretion). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.40. Classification remains Tangled Rope at analytical level because both functions are structurally real, not because of observable choice. The constraint is not a natural law; it is a hybrid coordination-extraction mechanism that will need either sunset or evolution toward norms-based inclusion.
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
    constraint_indexing:constraint_classification(german_board_gender_quota, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(german_board_gender_quota, TR),
    TR >= 0.70.

:- end_tests(german_board_gender_quota_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): The quota removes firm discretion over 40% of board seats — a real extraction of selection autonomy. However, this extraction solves a collective action problem: individual firms benefit from accessing qualified female talent (improved decision-making, legitimacy), but lack individual incentive to overcome internal biases and networking effects. The quota forces this incentive alignment. The value (0.35, not 0.60+) reflects that the coordination benefit outweighs the extraction cost — firms' net welfare may increase if they were artificially excluding qualified candidates. Suppression (0.42): Moderate. The quota overrides implicit gatekeeping (partner-track selection bias, Old Boys Network exclusion, credential devaluation for women) but does not eliminate it. Pre-board pipeline barriers remain; women must still navigate systemic discrimination to reach mid-level positions eligible for board appointment. Theater ratio (0.55): Rising over the interval. Firms initially had low theater (genuine sourcing challenge 2021-2022), but as quotas were met and norms adapted, compliance theater increased: diversity reports, training programs, diversity officer positions that perform inclusion without shifting power. The trajectory reflects Goodhart drift — optimizing for the metric (women on board) rather than the goal (inclusive decision-making culture).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary perspective (women in quota cohort) sees pure coordination: a barrier was removed, and they gained access. The firm perspective sees mixed: coordination (we now access broader talent) plus extraction (we lost discretion and face compliance cost). The male incumbent perspective sees theater: the quota appears as ritual and bureaucratic obligation, not as a real constraint on their own power (grandfather clauses, retirement timelines). The powerless perspective (qualified women excluded by pipeline barriers) sees snare: the quota bypasses them and leaves structural exclusion in place. The analytical perspective (civilizational) sees the hybrid structure clearly: both functions are real, and the question is whether the coordination function will eventually internalize as norm (sunset path) or require continued enforcement (persistent extraction path).
 *
 * DIRECTIONALITY LOGIC:
 *   Underrepresented women (beneficiary + arbitrage) → d≈0.15, f(d)≈-0.01. Net beneficiary; effective extraction is slightly negative (coordination benefit dominates). Corporations (victim + constrained) → d≈0.55, f(d)≈0.75. Moderate extraction; firms cannot freely exit but can adapt through compliance. Male incumbents (victim + constrained but with exemptions) → d≈0.65, f(d)≈1.00. Moderate extraction on the cohort, but grandfather clause effects reduce this to low on individuals. EU coalition (organized + constrained) → d≈0.35, f(d)≈0.32. Low extraction because coalition maintains agency in policy evolution and sunset design. Analytical observer (analytical + analytical) → d≈0.72, f(d)≈1.15. Hybrid structure is visible; observer does not naturalize the constraint as immutable law but as intentional mechanism with possible lifecycle paths.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pipeline_sufficiency,
    'Are there enough qualified women in mid-management pipelines to meet 40% board quota without artificial advancement or credential dilution?',
    'Longitudinal analysis of mid-manager demographics pre-2021 vs post-2021; correlation of board appointment dates with prior tenure and performance metrics',
    'If yes: quota is enforcing coordination to overcome institutional discrimination (Rope/Tangled Rope confirmed). If no: quota forces firms to either advance under-qualified candidates (extraction becomes visible as performance drag) or exhaust pipeline and freeze, revealing structural exclusion depth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pipeline_sufficiency, empirical, 'Sufficiency of female mid-manager pipeline for quota targets').

omega_variable(
    internalization_vs_compliance,
    'Does the quota catalyze genuine norm change toward gender-inclusive board culture, or does it remain a compliance theater with unchanged power dynamics?',
    'Ethnographic board study; measurement of influence/decision-making contribution for women appointed pre-quota vs post-quota; tracking of board diversity metric sustainability after quota removal scenarios',
    'If internalized: scaffold perspective confirmed, sunset is achievable. If theater: piton classification will dominate, and the constraint becomes inert institutional baggage requiring external pressure to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_compliance, empirical, 'Whether quota drives norm change vs compliance performance').

omega_variable(
    cross_border_regulatory_arbitrage,
    'Do multinational firms headquartered outside Germany with German subsidiaries or CJEU jurisdiction experience the quota as extractive constraint vs coordination signal?',
    'Survey of multinational board structure decisions; analysis of whether firms relocate board seats away from German jurisdiction to avoid quota; comparison of enforcement intensity by nationality of ultimate parent company',
    'If arbitrage prevalent: quota effectiveness depends on jurisdictional enforcement and credibility, revealing extractive component (victim = firm autonomy). If arbitrage rare: indicates internalization or that reputational costs exceed compliance costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_border_regulatory_arbitrage, empirical, 'Regulatory arbitrage by multinationals on board composition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_board_gender_quota, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gbgq_tr_t0, german_board_gender_quota, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gbgq_tr_t3, german_board_gender_quota, theater_ratio, 3, 0.48).
narrative_ontology:measurement(gbgq_tr_t5, german_board_gender_quota, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(gbgq_be_t0, german_board_gender_quota, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gbgq_be_t3, german_board_gender_quota, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(gbgq_be_t5, german_board_gender_quota, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_board_gender_quota, enforcement_mechanism).
narrative_ontology:affects_constraint(german_board_gender_quota, eu_gender_balance_directive).
narrative_ontology:affects_constraint(german_board_gender_quota, workplace_gender_bias_exclusion).

% DUAL FORMULATION NOTE:
% FüPoG II is downstream of broader EU Gender Balance Directive and upstream of individual workplace gender bias constraints. The quota itself is a coordination mechanism (solving collective action problem of systemic exclusion), but decomposes into separate constraints when observing the pre-board pipeline (where structural exclusion remains) vs post-quota board dynamics (where the coordination function operates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(german_board_gender_quota, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
