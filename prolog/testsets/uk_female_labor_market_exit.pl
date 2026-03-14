% ============================================================================
% CONSTRAINT STORY: uk_female_labor_market_exit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_female_labor_market_exit, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: uk_female_labor_market_exit
 *   human_readable: UK Female Labor Market Exit via Childcare and Career Penalty
 *   domain: economic/labor/gender
 *
 * SUMMARY:
 *   The UK female labor market exit is a structurally complex constraint that
 *   combines genuine coordination (household childcare logistics) with
 *   asymmetric extraction (wage penalties, career discontinuity, retirement
 *   savings gaps). Between 1990–2020, this constraint shifted from primarily
 *   cultural (19th-century gender roles persisting through institutional
 *   inertia) to increasingly structural (real childcare cost barriers,
 *   workplace inflexibility, fiscal incentives for single-earner households).
 *   The constraint exhibits a falling theater ratio (declining cultural
 *   performance, increasing structural reality) coupled with rising
 *   extractiveness (as structural barriers became more visible and
 *   quantifiable, the extraction became more measurable). Women exiting the
 *   workforce bear disproportionate costs: foregone income (£200k+ lifetime),
 *   permanent wage penalty (18–35% for 5+ years out), retirement savings gap
 *   (40–50% lower), and psychological burden of identity discontinuity.
 *   Employers benefit from reduced turnover costs and stable low-wage labor
 *   supply. Male household earners benefit from coordination of childcare
 *   burden (though lose household income resilience). Cultural norms around
 *   motherhood and work persist despite structural reforms (shared parental
 *   leave, workplace flexibility legislation) that enable alternatives. The
 *   constraint is tangled because genuine household coordination function
 *   (someone must handle childcare logistics) coexists with extractive
 *   asymmetry (that someone is disproportionately the woman).
 *
 * KEY AGENTS:
 *   - Women exiting workforce: Primary victim (powerless/trapped) — face childcare cost barriers, workplace inflexibility, and permanent career penalty with no structural exit
 *   - Employers and firms: Primary beneficiary (institutional/arbitrage) — benefit from reduced turnover, wage compression, and avoidance of childcare/flexibility infrastructure investment
 *   - Male household earners: Secondary beneficiary (moderate/constrained) — benefit from partner's exit (childcare burden transferred) but lose household financial resilience
 *   - Household income inequality system: Victim (powerless/trapped) — structural inequality persists across UK households; female earnings gap accumulates into retirement crisis
 *   - Work-family balance reformers: Organized agents (organized/mobile) — policy actors building alternative pathways (shared parental leave, flexible working rights, childcare subsidy) with sunset logic
 *   - Cultural gender role norms: Institutional persistence mechanism (institutional/arbitrage) — cultural expectations ('mothers should prioritize family') maintain the constraint through theater even as structural barriers decline
 *   - Analytical observer: Civilizational view (analytical/analytical) — recognizes the constraint as contingent institutional arrangement, not natural law; identifies that Nordic and Dutch models demonstrate exit rates can be substantially reduced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_female_labor_market_exit, 0.52).
domain_priors:suppression_score(uk_female_labor_market_exit, 0.58).
domain_priors:theater_ratio(uk_female_labor_market_exit, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_female_labor_market_exit, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_female_labor_market_exit, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(uk_female_labor_market_exit, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_female_labor_market_exit, tangled_rope).
narrative_ontology:human_readable(uk_female_labor_market_exit, "UK Female Labor Market Exit via Childcare and Career Penalty").
narrative_ontology:topic_domain(uk_female_labor_market_exit, "economic/labor/gender").

domain_priors:requires_active_enforcement(uk_female_labor_market_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_female_labor_market_exit, employers_avoiding_retention_costs).
narrative_ontology:constraint_beneficiary(uk_female_labor_market_exit, male_household_earners).
narrative_ontology:constraint_beneficiary(uk_female_labor_market_exit, childcare_gap_exploiters).
narrative_ontology:constraint_victim(uk_female_labor_market_exit, women_exiting_workforce).
narrative_ontology:constraint_victim(uk_female_labor_market_exit, household_income_inequality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXITING WOMAN WORKER (SNARE) — Trapped by childcare costs (£1000+ monthly for full-time nursery), inflexible workplace expectations, and career penalty for part-time work. Material barriers to continued labor force participation are severe. No arbitrage option exists; return to work after exit carries permanent wage penalty. Maximum experienced extraction — bears full burden of coordination failure on household childcare.
constraint_indexing:constraint_classification(uk_female_labor_market_exit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYERS AND FIRMS (ROPE) — Experience the constraint as coordination of labor supply through a de facto mechanism: women exit, workforce stabilizes at lower cost, firms avoid investing in flexible work infrastructure. Benefits from reduced turnover costs and wage compression. The extraction is real but benefits are real too — firms coordinate labor market expectations without explicit coercion. Effective extraction runs toward employers; they have arbitrage options (replace with cheaper labor, remain with smaller workforce).
constraint_indexing:constraint_classification(uk_female_labor_market_exit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: HOUSEHOLD MALE EARNER (TANGLED ROPE) — Constrained by implicit expectation that female partner bears childcare burden. Benefits from partner's exit (reduced childcare coordination burden falls to her, household specialization increases income efficiency in short term). Also bears cost (partner's forgone income, household financial vulnerability if he loses employment, relationship stress). Mixed extraction with genuine coordination function — household division of labor does solve daily childcare logistics, but the solution is asymmetric.
constraint_indexing:constraint_classification(uk_female_labor_market_exit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WORK-FAMILY BALANCE REFORMERS (SCAFFOLD) — Organized actors (unions, equality bodies, progressive employers) see the constraint as a temporary coordination failure with institutional sunset clause. Shared parental leave, employer-supported childcare, flexible working rights (post-2015 legislation) are building alternative pathways. The constraint classification as scaffold is contingent: these pathways must mature and enforcement must increase. Theater is lower in this perspective — reforms target the structural problem (childcare access, workplace inflexibility) not the performance of exit as natural choice.
constraint_indexing:constraint_classification(uk_female_labor_market_exit, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CULTURAL GENDER ROLE NORMS (PITON) — The primary function (organizing household labor allocation) has been substantially automated or outsourced but the institutional expectation persists through inertia. Modern childcare alternatives and employer flexibility make the traditional female exit unnecessary, yet the norm ('mothers should stay home,' 'fathers are breadwinners') continues through cultural performance. Theater ratio is moderate (0.45) because workplace inflexibility and childcare costs are real structural facts, not pure performance, but the cultural overlay is largely theatrical. The constraint is degraded — it persists not because it solves the problem it once did, but because alternative arrangements haven't fully replaced it.
constraint_indexing:constraint_classification(uk_female_labor_market_exit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scale, the UK female labor market exit is a hybrid: genuine coordination (household childcare, labor specialization) layered with extractive asymmetry (wage penalties for women, career discontinuity, retirement income inequality). The constraint is not immutable law but nor is it purely performative. It reflects real unresolved infrastructure gaps (childcare access) plus institutional path dependence (employer expectations) plus power asymmetry (who bears childcare burden). Globally, higher-income economies (Nordic, Netherlands) show exit rates can be reduced through policy without destroying household coordination, suggesting the extraction is contingent.
constraint_indexing:constraint_classification(uk_female_labor_market_exit, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_female_labor_market_exit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_female_labor_market_exit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_female_labor_market_exit, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_female_labor_market_exit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_female_labor_market_exit, TR),
    TR >= 0.70.

:- end_tests(uk_female_labor_market_exit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from exiting women through multiple channels: foregone lifetime income (~£200,000), permanent wage penalty (18–35% for 5+ years discontinuity), pension savings gap (40–50% lower retirement income), and opportunity cost of career advancement. This is not speculative — the empirical wage penalty literature in UK labor economics (Gutierrez et al., Budig & England) documents these losses. Extractiveness is not higher (0.70+) because some women benefit from exit (improved family time, reduced double-shift burden), and some exit is nominally voluntary (though voluntary choice under structural constraint is not freely chosen). Suppression (0.58): Moderate-high. Structural barriers include childcare costs (£1000–1200/month for full-time nursery), workplace inflexibility (UK has weak statutory part-time protections compared to EU), and fiscal incentives for single-earner households (tax breaks for couples on single income). But suppression is not total (0.70+) because: (1) policy alternatives exist (shared parental leave, flexible working legislation post-2015), (2) some women navigate around the constraint (higher education, high income, dual-earner households), and (3) external shock (COVID-19 forced remote work) demonstrated flexibility is technically possible. Theater ratio (0.45): Moderate. The constraint has declined in theatrical content over 1990–2020 as structural barriers became more visible. Early period (1990s): exit was largely performance of gender roles ('mothers belong at home'), theater was high (0.60). Later period: childcare costs and wage penalties are measurable structural facts, theater dropped (0.42–0.45) as the performance collapsed and the structure remained. Theater is not higher because the constraint does solve a real coordination problem (household childcare), not a purely performative one.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound. The exiting woman's Snare classification reflects her structural reality: she faces trapped conditions (childcare cost, workplace inflexibility, permanent wage penalty), experiences maximum extraction (all costs fall on her), and sees no exit path (reentry carries permanent penalty). The employer's Rope classification reflects genuine coordination benefit: the constraint solves the problem of labor supply coordination without explicit coercion. The male household earner's Tangled Rope reflects mixed experience: he benefits from coordination (childcare handled) but also bears cost (partner's income lost, household financial vulnerability, relationship stress from inequality). The work-family reformers' Scaffold classification is aspirational rather than descriptive: it reflects belief that policy alternatives (flexible work, parental leave, childcare subsidy) will create genuine exit paths. The cultural norms' Piton classification reveals the constraint's degradation: the norm ('mothers stay home') is performative in modern context because childcare alternatives exist, yet the norm persists through institutional inertia rather than functional necessity. The analytical observer's classification as Tangled Rope from civilizational perspective recognizes the genuine coordination function (childcare logistics must be solved) coexists with extractive asymmetry (the burden falls disproportionately on women). The gap between Snare and Rope is the widest: they perceive fundamentally different constraint types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and exit options. Women exiting face high barriers to re-entry (career penalty) and cannot arbitrage out of the constraint — their d value is high (0.92–0.95), producing high f(d) ≈ 1.38, amplifying experienced extraction. Employers have arbitrage options (replace with male workers, outsource childcare provision, adopt flexible work) — their d value is low (0.15–0.25), producing low f(d) ≈ 0.00, experiencing minimal extraction or net benefit. Male household earners are constrained but not trapped — they could theoretically take parental leave or bear childcare burden, but do so at career cost — d ≈ 0.55, f(d) ≈ 0.75, experiencing moderate asymmetry. Cultural norms have no individual d (they are institutional persistence, not agents), but their effect is to anchor beneficiary expectations (d toward 0) and victim expectations (d toward 1) in framing that makes exit appear inevitable or desirable. The directionality chain shows why the exiting woman perceives Snare (high d, high f(d), maximum χ) while the employer perceives Rope (low d, low f(d), minimal χ) — the same constraint produces opposite experienced extractiveness based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that extraction and coordination coexist at the same structural level. This is not 'which is it, extraction or coordination?' but 'extraction coexists with coordination — the coordination serves extractive function.' The household childcare coordination genuinely needs to happen (someone must arrange childcare), but the constraint ensures women bear this coordination burden disproportionately. This is the classic mandatrophy trap: defenders claim the constraint solves a real coordination problem (true — childcare must be coordinated), while critics identify extractive asymmetry (also true — burden falls disproportionately on women). The Tangled Rope classification preserves both truths: the constraint requires active enforcement (workplace inflexibility, cultural expectations) and has genuine beneficiaries (employers, male earners) AND genuine victims (exiting women, household inequality). The measured extractiveness (0.52) is below the snare threshold (0.66) not because the constraint is pure coordination, but because the coordination function is genuine even though it is asymmetrically distributed. Nordic and Dutch models demonstrate that the same coordination function can be delivered with lower asymmetry (higher female participation, shared parental leave, subsidized childcare), proving the UK extraction is policy-contingent, not coordination-necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    childcare_cost_mechanism,
    'Is the female exit driven primarily by childcare cost barriers (structural trap) or by internalized identity expectations (identity lock), or both in different proportions?',
    'Quasi-experimental analysis of exit rates pre/post childcare subsidy policies (e.g., Scotland''s expansion to 30 hours free childcare). If exit rates drop significantly: cost barrier is primary. If rates persist: identity-lock mechanism is substantial.',
    'If cost barrier primary: constraint is snare/tangled_rope (structural extraction). If identity-lock primary: constraint is rope (internalized coordination). If both: tangled_rope holds as classification, but omega variables for suppression mechanism must distinguish structural vs internalized components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(childcare_cost_mechanism, empirical, 'Cost barrier vs identity-lock mechanism in female labor market exit').

omega_variable(
    wage_penalty_permanence,
    'Is the wage penalty for career interruption permanent (structural snare property) or recoverable with career continuation (tangled rope property with high but transient extraction)?',
    'Longitudinal earnings analysis: women who exit and re-enter vs women with continuous employment, controlling for human capital and sector. If penalty persists 10+ years post-return: structural. If penalty decays with tenure: tangled rope with temporary asymmetry.',
    'If permanent: constraint is snare (extraction is path-dependent, inescapable). If recoverable: constraint is tangled rope (asymmetric but not irreversible). Classification may shift biographical→generational based on recovery rates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_penalty_permanence, empirical, 'Whether wage penalties for interruption are permanent or recoverable').

omega_variable(
    flexible_work_constraint_substitution,
    'Does flexible working (part-time, compressed hours) substitute for exit or create new extraction through underemployment and permanent wage suppression?',
    'Comparison of career trajectories: full-time exit vs flexible part-time continuance. Track wage growth, promotion rates, and long-term earnings for each group. If flexible workers show wage recovery vs ex-workers but still face suppression: substitution has occurred (theater rises, extraction becomes attenuated but persistent).',
    'If genuine substitution: the constraint becomes more visible (theater_ratio increases, classification may become piton rather than snare). If flexible work is extractive: constraint adapts but doesn''t dissolve — women trade overt exit for part-time trap with same lifetime earnings loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexible_work_constraint_substitution, empirical, 'Whether flexible work substitutes for exit or creates new extraction').

omega_variable(
    identity_lock_cultural_entrenchment,
    'To what extent is female labor market exit identity-locked through internalized gender roles vs structurally trapped by childcare/financial barriers?',
    'Comparative analysis across cohorts and education levels. If highly educated women (high arbitrage options) still exit at high rates despite financial capacity to pay for childcare: identity-lock mechanism is substantial. If rates vary inversely with childcare access: structural trap is primary.',
    'If identity-locked is substantial: constraint persists even after structural barriers removed (scaffold sunset is aspirational, not structural). If structural trap is primary: policy intervention (childcare, flexibility) creates real exit path. Informs whether piton classification is accurate (theater-driven persistence) or whether snare elements will persist post-reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_cultural_entrenchment, empirical, 'Extent of identity-lock vs structural barriers in female exit patterns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_female_labor_market_exit, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukfem_tr_t0, uk_female_labor_market_exit, theater_ratio, 0, 0.6).
narrative_ontology:measurement(ukfem_tr_t10, uk_female_labor_market_exit, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ukfem_tr_t20, uk_female_labor_market_exit, theater_ratio, 20, 0.45).
narrative_ontology:measurement(ukfem_tr_t30, uk_female_labor_market_exit, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(ukfem_be_t0, uk_female_labor_market_exit, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ukfem_be_t10, uk_female_labor_market_exit, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ukfem_be_t20, uk_female_labor_market_exit, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ukfem_be_t30, uk_female_labor_market_exit, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_female_labor_market_exit, attachment_coordination).
narrative_ontology:boltzmann_floor_override(uk_female_labor_market_exit, 0.12).
narrative_ontology:affects_constraint(uk_female_labor_market_exit, gender_pay_gap).
narrative_ontology:affects_constraint(uk_female_labor_market_exit, pension_savings_inequality).
narrative_ontology:affects_constraint(uk_female_labor_market_exit, childcare_access_scarcity).
narrative_ontology:affects_constraint(uk_female_labor_market_exit, workplace_flexibility_deficit).

% DUAL FORMULATION NOTE:
% The female labor market exit is downstream of and causally linked to multiple structural constraints: childcare access scarcity (infrastructure gap), workplace flexibility deficit (employer expectation mismatch), and gender pay gap (which is both upstream cause and downstream effect of exit patterns). The exit itself is a distinct constraint (identity-based coordination with extractive asymmetry), not reducible to any single upstream cause. Network family: female_labor_market_exit ← childcare_access_scarcity, workplace_flexibility_deficit; female_labor_market_exit → gender_pay_gap, pension_savings_inequality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_female_labor_market_exit, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
