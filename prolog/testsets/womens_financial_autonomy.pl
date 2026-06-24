% ============================================================================
% CONSTRAINT STORY: womens_financial_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_womens_financial_autonomy, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: womens_financial_autonomy
 *   human_readable: Women's Financial Autonomy and Divorce Initiation Capacity
 *   domain: social/economic/demographic
 *
 * SUMMARY:
 *   The structural shift from economic dependency to financial autonomy for
 *   women in mid-20th to early-21st century developed economies. As women's
 *   labor force participation rose and independent income became achievable,
 *   the marriage institution shifted from economically coercive (exit
 *   prohibitively costly) to coordinative (exit costly but survivable). The
 *   constraint is women's capacity to earn independent income sufficient to
 *   survive divorce — a capacity that exists for some women and not others,
 *   producing divergent experiences of the same legal and social marriage
 *   structure. KEY AGENTS (by structural relationship): - Financially
 *   independent women: Primary beneficiaries (moderate/mobile) — autonomy
 *   converts trapped position to mobile - Economically dependent women:
 *   Payers (powerless/trapped) — remain in pre-autonomy structural position -
 *   Dual-income households: Beneficiaries (organized/mobile) — genuine
 *   partnership coordination - Divorced women post-separation: Payers
 *   (powerless/trapped) — face 41% income drop after exercising exit -
 *   Traditional marriage advocates: Excluded voices (organized/constrained) —
 *   frame autonomy as destabilizing - Family demographers: Analytical
 *   observers — document the structural divergence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(womens_financial_autonomy, 0.38).
domain_priors:suppression_score(womens_financial_autonomy, 0.42).
domain_priors:theater_ratio(womens_financial_autonomy, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(womens_financial_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(womens_financial_autonomy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(womens_financial_autonomy, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(womens_financial_autonomy, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(womens_financial_autonomy, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(womens_financial_autonomy, rope).
narrative_ontology:human_readable(womens_financial_autonomy, "Women's Financial Autonomy and Divorce Initiation Capacity").
narrative_ontology:topic_domain(womens_financial_autonomy, "social/economic/demographic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(womens_financial_autonomy, financially_independent_women).
narrative_ontology:constraint_beneficiary(womens_financial_autonomy, dual_income_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(womens_financial_autonomy, economically_dependent_women).
narrative_ontology:constraint_victim(womens_financial_autonomy, divorced_women_post_separation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women with sustained labor force participation and independent income streams who can initiate divorce without facing destitution. Their financial autonomy converts what was historically a trapped position into a mobile one — they can exit unsatisfactory marriages because they control resources. Post-divorce income drops substantially (41% on average) but autonomy means the drop is survivable rather than prohibitive.
narrative_ontology:constraint_stakeholder(womens_financial_autonomy, financially_independent_women, beneficiary,
    moderate, biographical, mobile, national).

% Women without independent income or with interrupted labor force participation who remain in the pre-autonomy structural position. For them the constraint has not shifted — divorce still means potential poverty, loss of health insurance, loss of housing stability. They experience the same marriage structure as coercive that autonomous women experience as voluntary coordination.
narrative_ontology:constraint_stakeholder(womens_financial_autonomy, economically_dependent_women, payer,
    powerless, biographical, trapped, national).

% Households where both partners maintain careers and financial independence. The autonomy constraint enables genuine partnership negotiation — either party can exit, so the relationship persists by mutual preference rather than economic necessity. They benefit from the coordination function (stable household formation, childrearing, resource pooling) without the suppression that characterized single-earner marriages.
narrative_ontology:constraint_stakeholder(womens_financial_autonomy, dual_income_households, beneficiary,
    organized, biographical, mobile, national).

% Women who exercised the exit option and now face the 41% income drop, often with primary custody responsibilities and reduced earning capacity from career interruptions. Their autonomy was sufficient to initiate divorce but insufficient to prevent post-divorce economic vulnerability. They traded one constraint (unsatisfactory marriage) for another (economic precarity).
narrative_ontology:constraint_stakeholder(womens_financial_autonomy, divorced_women_post_separation, payer,
    powerless, immediate, trapped, local).

% Religious and cultural institutions that frame women's financial autonomy as destabilizing family formation and increasing divorce rates. They argue the constraint has dissolved a coordination mechanism (lifelong marriage) that provided stability, and that the autonomy reading ignores the costs borne by children and communities. They are structurally excluded from the labor market and legal frameworks that enable autonomy.
narrative_ontology:constraint_stakeholder(womens_financial_autonomy, traditional_marriage_advocates, excluded,
    organized, generational, constrained, national).

% Researchers tracking the structural shift from economic dependency to autonomy across cohorts. They measure labor force participation rates, divorce initiation patterns, post-divorce income trajectories, and the divergence between women with and without independent income. They document that the constraint operates differently across class and education lines.
narrative_ontology:constraint_stakeholder(womens_financial_autonomy, family_demographers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables household formation, resource pooling, childrearing, and long-term partnership on a voluntary basis rather than through economic coercion. Women with financial autonomy can choose partnership for its intrinsic benefits rather than economic necessity.
% TRANSFER_FUNCTION: Transfers decision-making power and exit capacity from the marriage structure itself to the individual woman, contingent on her labor market position. The transfer is not of money but of structural position — from trapped to mobile.
% ABSENT_VOICES: Economically dependent women without labor force participation are structurally present but functionally excluded from the autonomy the constraint enables. Traditional marriage advocates who frame autonomy as family destabilization are excluded from labor market and legal policy formation.
% DISAPPEARANCE_RATIONALE: If women's financial autonomy disappeared overnight — if labor force participation collapsed and independent income vanished — marriage would revert to an economically coercive institution for most women. Divorce rates would drop not because relationships improved but because exit became unaffordable. Household formation patterns, fertility decisions, and domestic violence dynamics would all shift as the exit option closed.
% FOUNDING_PROBLEM: Historical economic dependency trapped women in marriages regardless of quality, safety, or mutual satisfaction. Without independent income, exit meant destitution. The constraint emerged to solve the coordination problem of enabling voluntary partnership rather than economically coerced union.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live for economically dependent women, documented by family demographers and domestic violence researchers from outside the benefiting class. Labor economists and sociologists across ideological lines confirm that financial dependency still predicts relationship exit barriers, and that the autonomy constraint operates unevenly across class and education.
narrative_ontology:disappearance_verdict(womens_financial_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(womens_financial_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(womens_financial_autonomy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-24',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(womens_financial_autonomy, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(womens_financial_autonomy_tests).
:- end_tests(womens_financial_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38 at interval end, down from 0.62 at start) because financial autonomy genuinely solves a coordination problem for women who achieve it — voluntary partnership replaces economic coercion. But extraction remains non-zero because the constraint operates unevenly: economically dependent women still face the pre-autonomy trap, and even autonomous women face substantial post-divorce income drops (41% vs 23% for men). Suppression has declined substantially (0.72 to 0.42) as labor force participation rose and legal barriers to women's economic participation fell, but remains elevated because structural barriers (wage gaps, caregiving responsibilities, occupational segregation) still constrain full autonomy. Theater is low (0.18) because the autonomy is real — women with independent income genuinely can and do initiate divorce at higher rates than dependent women. Accessibility collapse is moderate (0.48) because alternatives to financial autonomy (economic dependency, traditional single-earner marriage) remain available but increasingly costly. Resistance is moderate (0.52) because traditional marriage advocates actively resist the autonomy norm, framing it as family destabilization.
 *
 * PERSPECTIVAL GAP:
 *   The financially independent and economically dependent seats should compute radically different types from the same legal and social marriage structure. From the autonomous position, marriage is voluntary coordination (rope) — exit is costly but achievable, so the relationship persists by preference. From the dependent position, the same structure operates as a snare — exit is prohibitively costly, so the relationship persists by economic necessity. The engine should detect this divergence from the power/exit differential: moderate/mobile vs powerless/trapped experiencing the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Financially independent women are structural beneficiaries — the constraint (their own earning capacity) enables exit from unsatisfactory marriages, converting a trapped position to mobile. Their directionality sits near the beneficiary end (d ≈ 0.2). Economically dependent women are targets — they lack the autonomy the constraint provides and remain trapped in the pre-shift structure; their directionality is near the target end (d ≈ 0.8). Dual-income households benefit from coordination without suppression (d ≈ 0.3). Divorced women post-separation are payers — they exercised autonomy but now bear its costs (income drop, economic precarity); their directionality shifts from beneficiary pre-divorce to target post-divorce (d ≈ 0.7 post-separation).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves a mandatrophy risk: without the autonomy framing, rising divorce rates initiated by women could be misread as pure extraction (women destabilizing families) rather than as the exercise of exit capacity that converts coercive coordination into voluntary coordination. The autonomy lens shows that what traditional advocates read as extraction (divorce) is actually the removal of suppression (economic coercion). The genuine coordination function (household formation, childrearing, partnership) persists for autonomous women, but on voluntary rather than coerced terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    post_divorce_income_asymmetry,
    'Is the 41% post-divorce income drop for women (vs 23% for men) a residual structural barrier that limits autonomy, or is it the unavoidable cost of exercising exit capacity?',
    'Longitudinal studies comparing post-divorce trajectories for women with continuous vs interrupted labor force participation, and policy experiments with stronger alimony/child support enforcement or subsidized childcare.',
    'If the income drop is structurally reducible (through policy or labor market changes), current autonomy is partial and extraction remains higher than measured. If it is unavoidable (reflecting genuine caregiving costs), the autonomy is real but costly, and the constraint is closer to pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_divorce_income_asymmetry, empirical, 'Whether post-divorce income asymmetry is structural barrier or unavoidable cost of exit.').

omega_variable(
    class_stratification_of_autonomy,
    'Does financial autonomy operate as a class-stratified constraint, where high-education/high-income women achieve genuine exit capacity while low-income women remain structurally trapped?',
    'Stratified analysis of divorce initiation and post-divorce outcomes by income quartile and education level. If autonomy concentrates in upper income/education brackets, the constraint is class-extractive.',
    'If autonomy is class-stratified, the constraint benefits high-SES women while leaving low-SES women in the pre-autonomy trap. The rope classification would hold only for the beneficiary class; for the excluded class it remains a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_stratification_of_autonomy, empirical, 'Whether autonomy is genuinely universal or class-stratified.').

omega_variable(
    coordination_vs_destabilization_framing,
    'Is the shift from economically coerced to voluntary partnership a net coordination gain (enabling better-quality relationships) or a destabilization of a functional institution (increasing family dissolution)?',
    'Comparison of relationship quality, domestic violence rates, and child outcomes in high-autonomy vs low-autonomy cohorts and regions. If autonomy correlates with better relationship quality and lower violence, it is coordination. If it correlates with worse child outcomes and community instability, it is destabilization.',
    'The framing determines whether the constraint is read as solving a coordination problem (enabling voluntary partnership) or creating an extraction problem (dissolving stable families). Traditional advocates hold the destabilization frame; autonomy advocates hold the coordination frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_destabilization_framing, preference, 'Whether autonomy is coordination gain or institutional destabilization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(womens_financial_autonomy, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wome_tr_t0, womens_financial_autonomy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wome_tr_t10, womens_financial_autonomy, theater_ratio, 10, 0.3).
narrative_ontology:measurement(wome_tr_t20, womens_financial_autonomy, theater_ratio, 20, 0.25).
narrative_ontology:measurement(wome_tr_t30, womens_financial_autonomy, theater_ratio, 30, 0.22).
narrative_ontology:measurement(wome_tr_t40, womens_financial_autonomy, theater_ratio, 40, 0.2).
narrative_ontology:measurement(wome_tr_t50, womens_financial_autonomy, theater_ratio, 50, 0.19).
narrative_ontology:measurement(wome_tr_t60, womens_financial_autonomy, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(wome_be_t0, womens_financial_autonomy, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(wome_be_t10, womens_financial_autonomy, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(wome_be_t20, womens_financial_autonomy, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(wome_be_t30, womens_financial_autonomy, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(wome_be_t40, womens_financial_autonomy, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(wome_be_t50, womens_financial_autonomy, base_extractiveness, 50, 0.39).
narrative_ontology:measurement(wome_be_t60, womens_financial_autonomy, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(wome_su_t0, womens_financial_autonomy, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(wome_su_t10, womens_financial_autonomy, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(wome_su_t20, womens_financial_autonomy, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(wome_su_t30, womens_financial_autonomy, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(wome_su_t40, womens_financial_autonomy, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(wome_su_t50, womens_financial_autonomy, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(wome_su_t60, womens_financial_autonomy, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(womens_financial_autonomy, resource_allocation).
narrative_ontology:affects_constraint(womens_financial_autonomy, no_fault_divorce_legal_framework).
narrative_ontology:affects_constraint(womens_financial_autonomy, gender_wage_gap).
narrative_ontology:affects_constraint(womens_financial_autonomy, childcare_cost_burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
