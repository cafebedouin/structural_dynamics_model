% ============================================================================
% CONSTRAINT STORY: dual_earner_work_incentive_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_earner_work_incentive_structure, []).

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
 *   constraint_id: dual_earner_work_incentive_structure
 *   human_readable: Dual Earner Work Incentive Structure
 *   domain: labor/economic_policy/gender
 *
 * SUMMARY:
 *   The dual-earner work incentive structure emerged across OECD economies
 *   from the 1980s onward as women's labor force participation increased and
 *   breadwinner-wage model became politically untenable. Rather than
 *   maintaining household purchasing power through single-earner wages,
 *   employers and policymakers normalized the assumption that households
 *   would have two earners. This created a structural constraint where both
 *   partners must work to maintain pre-dual-earner household income levels,
 *   yet childcare costs, time poverty, and unpaid domestic labor intensified
 *   rather than decreased. The constraint exhibits asymmetric benefits
 *   (employers, upper-income households, childcare industry) and asymmetric
 *   costs (secondary earners, time-poverty burden, lower-income households,
 *   childcare workers). Unlike a pure coordination mechanism (rope), the
 *   dual-earner structure solves coordination problems (who works if not
 *   both?) but does so by extracting from secondary earners and displacing
 *   unpaid domestic labor onto families rather than onto employers or the
 *   state. The constraint is maintained by policy silence (lack of childcare
 *   subsidy, inflexible work norms, tax structures), workplace practice
 *   (flexibility offered unequally), and gender role naturalization
 *   (secondary earners disproportionately absorb care work). Theater has
 *   increased as work-life balance rhetoric and gender equity initiatives
 *   expand without corresponding institutional change to wage levels,
 *   childcare infrastructure, or parental accommodation.
 *
 * KEY AGENTS:
 *   - Secondary Earner with Dependent Children: Primary victim (powerless/trapped) — economic necessity combined with childcare cost and care work burden creates trapped exit. Both employment and domestic labor required; cannot exit either without crisis.
 *   - Lower-Income Dual-Earner Household: Secondary victim (moderate/constrained) — constrained by childcare costs consuming 20-35% of second earner income; genuine coordination problem (need both incomes) but extraction through wage indexing.
 *   - Childcare Industry and Service Sector Workers: Tertiary victim (organized/constrained) — expanded demand creates sector growth but not wage growth; extraction routed through low-wage work; organized exit options exist but constrained by cost structure.
 *   - Employers: Primary beneficiary (institutional/arbitrage) — perceive constraint as coordination solution; can retool pay structures but don't, maintaining surplus.
 *   - Upper-Income Dual-Earner Households: Secondary beneficiary (powerful/mobile) — have exit options (reduced hours, outsourced childcare, career flexibility) that lower-income households lack; benefit from dual-income norm without equivalent extraction burden.
 *   - State Policy Framework: Institutional actor (institutional/arbitrage) — maintains outdated policy structure; has arbitrage options (public childcare, wage adjustment, parental leave) but doesn't exercise them.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy as inevitable economic law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_earner_work_incentive_structure, 0.58).
domain_priors:suppression_score(dual_earner_work_incentive_structure, 0.62).
domain_priors:theater_ratio(dual_earner_work_incentive_structure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_earner_work_incentive_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(dual_earner_work_incentive_structure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dual_earner_work_incentive_structure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_earner_work_incentive_structure, tangled_rope).
narrative_ontology:human_readable(dual_earner_work_incentive_structure, "Dual Earner Work Incentive Structure").
narrative_ontology:topic_domain(dual_earner_work_incentive_structure, "labor/economic_policy/gender").

domain_priors:requires_active_enforcement(dual_earner_work_incentive_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_earner_work_incentive_structure, employers).
narrative_ontology:constraint_beneficiary(dual_earner_work_incentive_structure, dual_income_households_upper_bracket).
narrative_ontology:constraint_beneficiary(dual_earner_work_incentive_structure, childcare_industry).
narrative_ontology:constraint_beneficiary(dual_earner_work_incentive_structure, consumer_spending_economy).
narrative_ontology:constraint_victim(dual_earner_work_incentive_structure, secondary_earner_time_poverty).
narrative_ontology:constraint_victim(dual_earner_work_incentive_structure, childcare_burden_asymmetry).
narrative_ontology:constraint_victim(dual_earner_work_incentive_structure, lower_income_dual_earner_households).
narrative_ontology:constraint_victim(dual_earner_work_incentive_structure, unpaid_domestic_labor_displacement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECONDARY EARNER WITH DEPENDENT CHILDREN (SNARE) — Structurally trapped by childcare costs, healthcare dependency, and financial obligations. Both partners working is required for household survival, yet the second income is partially consumed by childcare, eldercare, and work-related expenses. Exit from employment brings immediate financial crisis; exit from unpaid domestic labor is socially penalized. The trapped secondary earner experiences pure extraction: generates income while bearing disproportionate time poverty and care work. No genuine coordination benefit is perceived — the constraint exists because employers have indexed wages to assume dual earners.
constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER-INCOME DUAL-EARNER HOUSEHOLD (TANGLED ROPE) — Both earners constrained by childcare costs that consume 20-35% of second earner's income. Genuine coordination problem: without both incomes, household drops below subsistence threshold. But asymmetric extraction: wages have been indexed to assume two earners, shifting childcare burden onto families rather than employers or the state. Perceives mixed benefit (economic survival requires dual income) and mixed extraction (childcare costs, time poverty, service sector degradation). Some exit options exist (relocation, career change) but at high cost.
constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYERS (ROPE) — Experience the dual-earner assumption as pure coordination: the constraint solves their collective action problem of downward wage pressure. Single-earner household wages were politically unsustainable (breadwinner norm); dual-earner indexing allows lower real wages per capita while maintaining household income. No meaningful extraction experienced from employer perspective — the constraint is perceived as efficient labor market coordination. Employers have arbitrage options (retool pay structures, adopt flexible work) but don't exercise them because current structure maximizes their surplus.
constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CHILDCARE AND SERVICE SECTOR WORKERS (TANGLED ROPE) — Organized around meeting dual-earner household demand, but caught in extraction dynamic: childcare work is low-wage, precarious, and highly gendered. The constraint creates massive coordination problem (who watches the children?) but routes extraction through service workers — expanding childcare industry doesn't improve conditions for childcare workers, because the industry expands to absorb the secondary earner's income as cost, not improve childcare quality. Organized exit options (unionization, wage demands) exist but are suppressed by the same cost structure that makes childcare expensive.
constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE SOCIAL POLICY FRAMEWORK (PITON) — The formal policy structure treats dual earning as individual choice, not structural fact. State provides minimal childcare subsidy, tax structures assume single earner (though increasingly outdated), and labor law does not mandate flexible work or parental accommodation. The institutional apparatus is performing its historical function (supporting male breadwinner model) despite structural conditions having completely changed. Theater is high: work-life balance initiatives, gender equity rhetoric, childcare 'access' programs — but real policy (wage floors, childcare infrastructure, parental leave) has not kept pace. The state sees its own role as degraded (institutional inertia).
constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER AT CIVILIZATIONAL SCALE (MOUNTAIN FALSE SUMMIT) — The constraint risks appearing as natural law: 'Dual earning is an economic necessity in modern capitalism; childcare costs are inevitable friction of market efficiency.' This naturalizes what is actually a contingent institutional arrangement — the indexing of wages to dual-earner assumption is a policy choice (made in the 1980s-90s across OECD), not a law of economics. The analytical observer at this scale mistakes normalization for immutability. The engine's false summit detector will flag this: the structural data shows this is contingent (suppression 0.62 = high, indicating enforcement required), not natural (would require suppression ≤0.05).
constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_earner_work_incentive_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dual_earner_work_incentive_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_earner_work_incentive_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dual_earner_work_incentive_structure, TR),
    TR >= 0.70.

:- end_tests(dual_earner_work_incentive_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from secondary earners and lower-income households in the form of time poverty, displaced unpaid labor, and reduced real wages per capita. However, extraction is not total (upper-income households have exit options; some flexibility exists) and some genuine coordination value exists (dual earning does enable household income in wage-constrained environment). The value reflects the mixed nature of tangled rope: asymmetric extraction layered over genuine coordination problem. Extractiveness has increased from 0.35 to 0.58 over the 30-year interval as childcare costs rose faster than wages and work-life balance rhetoric expanded without institutional change (theater increasing). Suppression (0.62): Moderate-high. Multiple suppression mechanisms: childcare costs create economic barrier; inflexible work norms restrict exit options; gender role naturalization makes the secondary-earner role seem inevitable; policy silence (lack of childcare infrastructure, inadequate parental leave) enforces the structure. Not total suppression (some employers offer flexibility, some households achieve reduced hours) but significant and multifaceted. Theater ratio (0.48): Moderate. Work-life balance initiatives, gender equity rhetoric, and childcare 'access' programs are performative without corresponding wage or infrastructure change. Theater has increased over time as gap widened between policy rhetoric and institutional reality. The constraint is not yet piton-level theater (which would suggest pure institutional inertia), but theater is substantial enough to flag degradation in the state policy perspective.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence. The secondary earner (trapped/powerless) experiences snare — pure extraction masked as economic necessity. The lower-income household (constrained/moderate) experiences tangled rope — genuine need for dual income but extraction through wage indexing and childcare costs. The employer (arbitrage/institutional) experiences rope — coordination solution that happens to benefit them. The upper-income household (mobile/powerful) experiences rope or near-positive — they have genuine flexibility and can arbitrage childcare (outsourcing, career flexibility). The childcare worker (organized/constrained) experiences tangled rope in a different form — benefits from industry expansion but extraction through low wages. The state policy framework (arbitrage/institutional) experiences piton — performing outdated functions (male breadwinner support) despite changed conditions. The analytical observer at civilizational scale risks mountain — naturalizing what is contingent policy. The perspectival gaps reveal the constraint's true structure: it solves coordination problems for some agents (employers, upper-income households) while extracting from others (secondary earners, childcare workers, lower-income households).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position and benefit/cost relationship. Secondary earners and childcare workers are victims with trapped/constrained exit — high d (0.85-0.95), experiencing maximum extraction. Lower-income households are victims with mixed benefits — moderate d (0.55-0.65), experiencing tangled rope. Employers are beneficiaries with arbitrage options — low d (0.15-0.25), experiencing coordination benefit with no extraction cost perceived. Upper-income households are partial beneficiaries with mobile exit options — moderate d (0.30-0.50), experiencing mixed benefit and extraction. The state policy framework has declared no victims (by policy silence) and no enforcement (appears neutral) — but the analytical view reveals it is an institutional beneficiary of the arrangement (lower wage floors, reduced public expenditure), giving it d (0.10-0.20). The analytical observer has no structural position in the extraction flow — d remains near canonical (0.72-0.73).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that this constraint simultaneously solves a real coordination problem (how to maintain household income as single-earner wages became politically unsustainable) and extracts through that solution (by indexing wages to assume dual earners, shifting childcare burden to families, and maintaining lower real wages per capita). This is the canonical tangled rope case: genuine coordination function cannot be removed without creating a different problem, but the coordination is achieved through asymmetric extraction. The snare perspective from the secondary earner is not wrong — from their position, the extraction is severe and unilateral. The rope perspective from the employer is not wrong either — from their position, the constraint solves their coordination problem and appears as pure coordination. The mandatrophy resolution is to recognize both perspectives as structurally accurate but observationally different. Removing the constraint would require addressing both the coordination problem (household income) and the extraction mechanism (wage indexing, childcare costs) simultaneously. A pure coordination solution would look like: maintaining household income through wage levels that assume single earner, with public childcare provision, flexible work options equally available, and equalized domestic labor. The current structure achieves coordination by extracting; a non-extractive solution would require a different set of institutions (public childcare, higher wages, flexible work, equal domestic labor distribution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    childcare_cost_structural_vs_artificial,
    'Are childcare costs structurally inevitable (reflecting genuine resource scarcity and labor intensity) or artificially elevated by market power concentration and undersupply of public provision?',
    'International comparison: childcare costs as % of household income in countries with public childcare provision (France, Nordic countries, ~5-8%) vs countries with market-based provision (US, UK, ~15-35%); cost trajectory analysis controlling for wage levels and real resource constraints',
    'If structural: childcare burden is unavoidable friction, secondary earner extraction is moderate. If artificial: the constraint is rent-extraction by childcare providers and employers both, and extraction value is higher. Classification shifts from Tangled Rope toward Snare for secondary earners.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(childcare_cost_structural_vs_artificial, empirical, 'Whether childcare costs reflect structural scarcity or market concentration').

omega_variable(
    wage_indexing_counterfactual,
    'Would single-earner household wages be politically sustainable at 2024 levels if dual-earner assumption had not been normalized, or is the dual-earner indexing genuinely necessary for labor market clearing?',
    'Historical wage analysis: real single-earner wage trajectory 1970-present vs dual-earner household income trajectory; cross-country comparison with different wage-setting norms; employer labor cost accounting under alternative assumptions',
    'If sustainable: employers are extracting via normalization rather than necessity, and the rope perspective is partially false. If necessary: the rope perspective is accurate — dual earning solves genuine coordination problem for employers and households both. Classification of employer perspective may shift toward legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_indexing_counterfactual, empirical, 'Whether wage indexing reflects necessity or extraction opportunity').

omega_variable(
    secondary_earner_identity_lock,
    'How much of the secondary earner''s remaining in employment despite negative net income reflects structural entrapment vs. internalized identity/role expectations (identity_locked)?',
    'Post-exit trajectory analysis: if a secondary earner exits employment and income is replaced by household income or policy transfer, do they report reduced stress/time poverty, or do they experience identity crisis and attempt re-entry despite financial loss? Qualitative research on identity dimensions of ''breadwinner'' role for both earners.',
    'If primarily structural entrapment: trapped exit option is correct. If significantly identity-locked: the exit_options should include identity_locked for some secondary earners, indicating cognitive/identity binding on top of material barriers. This would elevate the snare classification''s binding strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_earner_identity_lock, empirical, 'Degree of identity-lock vs. material entrapment in secondary earner role').

omega_variable(
    gender_role_naturalization,
    'Is the disproportionate domestic labor burden on secondary earners an accidental economic consequence or an intentional enforcement mechanism that benefits from gender role naturalization?',
    'Analysis of workplace flexibility policy adoption: do employers offer flexible work equally to all workers, or do they route flexibility offers toward women (thereby encoding secondary-earner assumption into policy)? Comparison of actual flexibility usage rates and advancement penalties by gender; examination of promotional pathways for reduced-hours workers.',
    'If accidental: the constraint is a coordination failure that could be solved by childcare infrastructure and wage restructuring. If intentional: the constraint is embedded in institutional practices that reproduce gender roles, and the suppression value should be higher (closer to 0.70, indicating enforcement mechanism rather than economic inevitability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_role_naturalization, empirical, 'Whether gender role naturalization enforces the dual-earner structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_earner_work_incentive_structure, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_earner_tr_t0, dual_earner_work_incentive_structure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dual_earner_tr_t15, dual_earner_work_incentive_structure, theater_ratio, 15, 0.38).
narrative_ontology:measurement(dual_earner_tr_t30, dual_earner_work_incentive_structure, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(dual_earner_be_t0, dual_earner_work_incentive_structure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dual_earner_be_t15, dual_earner_work_incentive_structure, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(dual_earner_be_t30, dual_earner_work_incentive_structure, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_earner_work_incentive_structure, resource_allocation).
narrative_ontology:affects_constraint(dual_earner_work_incentive_structure, gender_wage_gap).
narrative_ontology:affects_constraint(dual_earner_work_incentive_structure, childcare_affordability_crisis).
narrative_ontology:affects_constraint(dual_earner_work_incentive_structure, work_life_balance_theater).
narrative_ontology:affects_constraint(dual_earner_work_incentive_structure, unpaid_domestic_labor_displacement).

% DUAL FORMULATION NOTE:
% The dual-earner work incentive structure is upstream of multiple derivative constraints. Gender wage gap reflects the extraction mechanism's gendering. Childcare affordability crisis is the primary extraction vector. Work-life balance theater measures the performative response. Unpaid domestic labor displacement measures the constraint's secondary effect. All are downstream of this structural constraint — removing the dual-earner assumption would require addressing all of them simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_earner_work_incentive_structure, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
